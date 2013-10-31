#lang racket

(require
  pyret/lang/ast
  pyret/lang/load
  racket/list
  racket/match
  racket/set)

(provide (all-defined-out) parse-pyret (all-from-out pyret/lang/ast))

(struct assignment (steps parts delimiters) #:transparent)
(struct part (name type) #:transparent)
(struct step (name type) #:transparent)
(struct delimiter (type value) #:transparent)

(struct file-response (parts-json file-to-echo) #:transparent)

(struct parse-step (name type) #:transparent)

(define (get-parse-step step)
  (define the-step-name (step-name step))
  (define (chop str suffix)
    (substring str 0 (- (string-length str) (string-length suffix))))
  (define name (case (step-type step)
    [("body") (chop the-step-name "-body")]
    [("fun-checks") (chop the-step-name "-checks")]
    [("data-checks") (chop the-step-name "-data-definition")]
    [else (error (format "Invalid step type: ~a" (step-type step)))]))
  (parse-step name (step-type step)))

(define (steps-up-to current-step steps)
  (cond
    [(empty? steps) (error (format "Step not found: ~a" current-step))]
    [(cons? steps)
     (cond
      [(string=? current-step (step-name (first steps)))
       (cons (get-parse-step (first steps)) empty)]
      [else
       (cons (get-parse-step (first steps)) (steps-up-to current-step (rest steps)))])]))
  

(define (file-for-step assignment-structure current-step pyret-program)
  (define parsed (parse-pyret pyret-program "file-upload"))
  (define active-steps (steps-up-to current-step (assignment-steps assignment-structure)))
  '()

  )

(define (get-data-def program name)
  (match program
    [(s-prog _ _ (s-block _ stmts))
     (define datadef (findf (lambda (stmt)
        (match stmt
          [(s-data _ (? (lambda (n) (equal? n name))) _ _ _ _ _)
           stmt]
          [_ #f]))
        stmts))
     (when (not datadef)
       (error (format "Couldn't find data-def: ~a" name)))
     datadef]
    [_ (error "Bad arg to get-data-def: ~a" program)]))

(define (get-fun-def program name)
  (match program
    [(s-prog _ _ (s-block _ stmts))
     (define fundef (findf (lambda (stmt)
        (match stmt
          [(s-fun _ (? (lambda (n) (symbol=? n name))) _ _ _ _ _ _)
           stmt]
          [_ #f]))
        stmts))
     (when (not fundef)
       (error (format "Couldn't find fundef: ~a" name)))
     fundef]
    [_ (error "Bad arg to get-fun-def: ~a" program)]))

(define (empty-body s) (s-block s (list)))

(define (ids-for-step pstep program)
  (match pstep
    [(parse-step name "body")
     (define fun (get-fun-def program (string->symbol name)))
     (free-ids fun)]
    [(parse-step name "fun-checks")
     (define fun (get-fun-def program (string->symbol name)))
     (match-let ([(s-fun s name params args ann doc body check) fun])
       (free-ids (s-fun s name params args ann doc (s-str s "no-ids") check)))]
    [(parse-step name "data-checks")
     (define data (get-data-def program (string->symbol name)))
     (free-ids data)]
    [_ (error (format "Bad parse step: ~a" pstep))]))

(define (update-fun-def pstep fun-def original)
  (match (list pstep fun-def original)
    [(list (parse-step name "body")
           (s-fun s1 n1 p1 args1 ann1 doc1 _ check1)
           (s-fun _ _ _ _ _ _ body2 _))
     (s-fun s1 n1 p1 args1 ann1 doc1 body2 check1)]
    [(list (parse-step name "fun-checks")
           (s-fun s1 n1 p1 args1 ann1 doc1 body1 _)
           (s-fun _ _ _ _ _ _ _ check2))
     (s-fun s1 n1 p1 args1 ann1 doc1 body1 check2)]
    [_
     (error (format "Bad args to update-fun-def: ~a ~a ~a" pstep fun-def original))]))

(define (replace-fun-def program name new-fundef)
  (match program
    [(s-prog s imports (s-block s2 stmts))
     (define new-stmts
       (map (lambda (stmt)
          (match stmt
            [(s-fun _ (? (lambda (n) (equal? n name))) _ _ _ _ _ _)
             stmt]
            [_ stmt]))
          stmts))
     (s-prog s imports (s-block s2 new-stmts))]
    [_ (error "Bad arg to replace-fun-def: ~a" program)]))

(define (strip-step-functions program steps)
  (cond
    [(empty? steps) program]
    [(cons? steps)
     (match (first steps)
      [(parse-step name (or "fun-checks" "fun-body"))
       (match-let ([
             (s-fun s name params args ann doc body checks)
             (get-fun-def program (string->symbol name))
            ])
         (define replaced-once
          (replace-fun-def program name
           (s-fun s name params args ann "" (empty-body s) (empty-body s))))
         (strip-step-functions replaced-once (rest steps)))])]))

(define (initial-setup-for-steps original-program steps)
  (define stripped-program (strip-step-functions original-program steps))
  (define (help program steps)
    (cond
      [(empty? steps) program]
      [(cons? steps)
       (define step (first steps))
       (match step
        [(parse-step name (or "fun-checks" "body"))
         (define new-fd (get-fun-def program (string->symbol name)))
         (define original-fd (get-fun-def original-program (string->symbol name)))
         (replace-fun-def program name (update-fun-def step new-fd original-fd))])]))
  (help stripped-program steps))

(define (toplevel-ids-closure program start-ids)
  (match program
   [(s-prog s import (s-block s2 stmts))
    (define (needed-name? n) (set-member? start-ids n))
    (define (needed-data? stmt)
     (and
       (s-data? stmt)
       (not (set-empty?
         (set-intersect
           (list->set (binding-ids stmt))
           start-ids)))))
    (define (add-stmt-ids stmt ids)
      (define additional-ids
        (match stmt
          [(or
            (s-fun _ (? needed-name? name) _ _ _ _ _ _)
            (s-let _ (? needed-name? name) _)
            (s-var _ (? needed-name? name) _))
           (free-ids stmt)]
          [(? needed-data? data-expr)
           (free-ids stmt)]
          [_ (set)]))
      (set-union additional-ids ids))
     
    (define added-ids (foldr add-stmt-ids start-ids stmts))
    (cond
      [(set=? added-ids start-ids) start-ids]
      [else (toplevel-ids-closure program added-ids)])]
   [_
    (error (format "toplevel-ids-closure got non-prog: ~a ~a" start-ids program))]))

(define (keep-only program names)
  (match program
    [(s-prog s imports (s-block s2 stmts))
     (define (keep-name? n) (set-member? names n))
     (define new-stmts
       (filter (lambda (stmt)
          (match stmt
            [(or
              (s-fun _ name _ _ _ _ _ _)
              (s-let _ (s-bind _ name _) _)
              (s-var _ (s-bind _ name _) _))
             (keep-name? name)]
            [(? s-data? data-expr)
             (not (set-empty?
               (set-intersect
                 (list->set (binding-ids data-expr))
                 names)))]
            [_ #f]))
          stmts))
     (s-prog s imports (s-block s2 new-stmts))]
    [_ (error "Bad arg to remove-binding: ~a" program)]))
