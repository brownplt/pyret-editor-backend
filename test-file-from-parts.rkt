#lang racket/base

(require
  rackunit
  rackunit/text-ui
  racket/set
  pyret/lang/ast
  "file-from-parts.rkt")

(define-binary-check (check-equiv-ast equiv-ast actual expected))

(define length-prog
  "fun length(l :: List) -> Number:
     cases(List) l:
       | empty => help-used-by-body(0)
       | link(f, r) => length(r)
     end
   where:
     length([1,2,3]) is help(3)
   end
   
   fun help-not-used(): end
   fun help-used-by-body(x): x end
   fun help(x): help2(x) end
   fun help2(y): y end")

(define helpers (test-suite "helpers"
  (let ()
    (check-equal?
      (steps-up-to "is-valid-checks"
        (list
          (step "is-valid-body" "body")
          (step "is-valid-checks" "fun-checks")))
      (list
        (parse-step "is-valid" "body")
        (parse-step "is-valid" "fun-checks")))

    (define just-a-let
      (s-prog #f (list)
        (s-block #f (list
          (s-let #f (s-bind #f 'a (a-blank)) (s-num #f 5))))))

    (check-equiv-ast
      (keep-only just-a-let (set 'b))
      (s-prog #f (list)
        (s-block #f (list))))

    (check-equiv-ast (keep-only just-a-let (set 'a)) just-a-let)

    (check-equal?
      (ids-for-step (parse-step "length" "fun-checks") (parse-pyret length-prog))
      (set 'length 'help 'Number 'List))

    (print (initial-setup-for-steps (parse-pyret length-prog) (list (parse-step "length" "fun-checks"))))

    (check-equal?
      (toplevel-ids-closure
        (initial-setup-for-steps (parse-pyret length-prog) (list (parse-step "length" "fun-checks")))
        (ids-for-step (parse-step "length" "fun-checks") (parse-pyret length-prog)))
      (set 'length 'help 'help2 'Number 'List))

  )
))

(run-tests helpers 'normal)

(define simple-assign
  (assignment
    (list
      (step "length-checks" "fun-checks")
      (step "length-body" "body"))
    (list
      (part "length-body" "body")
      (part "length-checks" "fun-checks")
      (part "scratchpad" "scratch"))
    (list
      (delimiter "code" "\nfun length(l :: List) -> Number")
      (delimiter "code" "\nwhere:")
      (delimiter "code" "\nend"))))

#;(define ffs1 (file-for-step simple-assign "length-checks" length-prog))

#;(check-equal? (file-response-file-to-echo ffs1)
  "fun length(l :: List) -> Number:

   where:
     length([1,2,3]) is 3
   end")


#;(define mini-oracle
  (assignment
    (list
      (step "generate-input-checks" "fun-checks")
      (step "generate-input-body" "body")
      (step "is-valid-checks" "fun-checks")
      (step "is-valid-body" "body"))
    (list
      (part "generate-input-body" "body")
      (part "generate-input-checks" "fun-checks")
      (part "Oracle3" "scratch")
      (part "is-valid-body" "body")
      (part "is-valid-checks" "fun-checks")
      (part "Oracle2" "scratch"))
    (list
      (delimiter "instructions" "<p>Instructions</p>")
      (delimiter "code" "\nfun generate-input(n :: Number) -> List<List<Number>>")
      (delimiter "code" "\nwhere:")
      (delimiter "code" "end")
      (delimiter "instructions" "<p>More instructions</p>")
      (delimiter "code" "\nfun is-valid(women :: List<List<Number>>, men :: List<List<Number>>, m :: List<Marriage>) -> Bool:")
      (delimiter "code" "\nend"))))
          

#;(define ffs2 (file-for-step
    mini-oracle
    "is-valid-checks"
    "
import \"oracle-support.arr\" as oracle

fun generate-input(n :: Number) -> List<List<Number>>:
  some-implementation()
where:
  Marriage
end

fun helper-for-checks():
  42
end

fun is-valid(women :: List<List<Number>>, men :: List<List<Number>>, m :: List<Marriage>) -> Bool:

where:
  helper-for-checks() is 42

end
"))

#;(check-equal? (file-response-file-to-echo ffs2)
  "
import \"oracle-support.arr\" as oracle

fun generate-input(n :: Number) -> List<List<Number>>:
where:
end

fun helper-for-checks():
  42
end

fun is-valid(women :: List<List<Number>>, men :: List<List<Number>>, m :: List<Marriage>) -> Bool:

where:
  helper-for-checks() is 42
end
")

