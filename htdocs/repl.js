(function() {
    "use strict";    

    // options: { compilerUrl: string,,
    //            write: (dom-node -> void) 
    //            language: string }

    // makeRepl: options (Repl -> void) -> void
    var makeRepl = function(options, afterSetup) {
        new Repl(options, afterSetup);
        return;
    };

    var Repl = function(options, afterSetup) {
        this.M = plt.runtime.currentMachine;
        this.M.params['print-mode'] = 'constructor';
        this.compilerUrl = options.compilerUrl || 'rpc.html';
        this._xhr = undefined;
        if (options.write) { this.write = options.write; }
        if (options.prettyPrint) { this.prettyPrint = options.prettyPrint; }
        this.language = (options.language || 
                         'whalesong/wescheme/lang/semantics.rkt');
        setupMachine(this, afterSetup);
    };


    var getXhr = function(that) {
        if (! that._xhr) { 
            that._xhr = new easyXDM.Rpc(
                { remote: that.compilerUrl || 'rpc.html' },
                { remote: { replCompile: {}, moduleCompile: {} } });
        }
        return that._xhr;
    };
   

    // write: dom-node -> void
    // Expected to be overridden by others via options.write.
    Repl.prototype.write = function(dom) {
        jQuery(document.body).append(dom)
    };


    // Return true if the Repl is currently evaluating.
    Repl.prototype.isRunning = function() {
        return this.M.running;
    };


    // Request a break on the current evaluation, calling afterBreak
    // once the break succeeds.  If no evaluation is running, immediately
    // call afterBreak.
    Repl.prototype.requestBreak = function(afterBreak) {
        if (this.isRunning()) {
            interruptEvaluation(this, afterBreak); 
        } else {
            afterBreak();
        }
    };


    var interruptEvaluation = function(that, afterBreak) {
        if (! that.isRunning()) {
            throw new Error("internal error: trying to interrupt evaluation but nothing is running.");
        }
        that.M.scheduleBreak(afterBreak);
    };


    // Resets the evaluator, quietly interrupting evaluation if
    // necessary.
    Repl.prototype.reset = function(afterReset) {
        var that = this;
        if (this.isRunning()) {
            this.M.params.currentDisplayer = 
                function(MACHINE, domNode) {};
            this.M.params.currentErrorDisplayer =
                function(MACHINE, domNode) {};
            interruptEvaluation(
                that,
                function() {
                    setupMachine(that, afterReset);
                });
        } else {
            setupMachine(that, afterReset);
        }
    };

    var setupMachine = function(that, afterSetup) { 
        var M = that.M;
        M.reset();
        // We configure the machine's output to send it to the
        // "output" DOM node.
        M.params['print-mode'] = 'constructor';
        M.params.currentDisplayer = function(MACHINE, domNode) {
            that.write(domNode);
        };
        M.params.currentErrorDisplayer = function(MACHINE, domNode) {
            that.write(domNode);
        };
        // FIXME: add other parameter settings here.

        // We then want to initialize the language module.
        M.loadAndInvoke(
            that.language,
            function() {
                var semanticsModule = M.modules[that.language];
                // FIXME: this should be getting the namespace,
                // not the export dictionary...
                M.params.currentNamespace = semanticsModule.getExports();
                afterSetup(that);
            },
            function(err) {
                console.error("Failed to load language: ", err);
                console.log(err.message);
            });
    };


    Repl.prototype.compileAndExecuteProgram = function(programName, code, options,
					               onDone, onDoneError) {
        var that = this;
        this.compileProgram(
            programName, 
            code,
            options,
            function(compiledCode) {
                that.executeCompiledProgram(compiledCode,
                                            onDone,
                                            onDoneError);
            },
            onDoneError);
    };


    Repl.prototype.compileProgram = function(programName, code, options,
                                             onDone, onDoneError) {
        var that = this;
        getXhr(this).replCompile(
            programName, 
            code,
            options,
            onDone, 
            function(err) {
                // If we get a 503, try again.
                if (err.status == 503) {
                    that.compileProgram(programName, code,
                                        onDone, onDoneError);
                } else {
                    onDoneError(err);
                }
            });
    };


    Repl.prototype.executeCompiledProgram = function(compiledResult,
						     onDoneSuccess, onDoneFail) {
        var that = this;
        if (compiledResult.type === 'error') {
            return onDoneFail(compiledResult);
        } else if (compiledResult.type === 'repl') {
            // compiledResult.compiledCodes is an array of function chunks.
            // The evaluation leaves the value register of the machine
            // to contain the list of values from toplevel evaluation.
            var compiledCodes = compiledResult.compiledCodes;
            forEachK(compiledCodes,
                     function(code, k) {
                         // Indirect eval usage here is deliberate.
                         var codeFunction = (0,eval)(code);
                         var onGoodEvaluation = function() {
                             var resultCount = that.M.a;
                             var results = [];
                             var i;
                             for (i = 0; i < resultCount; i++) {
                                 results.push(that.M.e[that.M.e.length - 1 - i]);
                             }
                             for (i = 0; i < results.length; i++) {
                                if (typeof that.prettyPrint === 'function') {
                                  if (!(that.prettyPrint(results[i]))) {
                                    that.printlnIgnoringVoid(results[i]);
                                  }
                                }
                                else {
                                  that.printlnIgnoringVoid(results[i]);
                                }
                             };
                             k();
                         };
                         var onBadEvaluation = function(M, err) {
                             onDoneFail(err);
                         };
                         codeFunction(that.M, onGoodEvaluation, onBadEvaluation);
                     },
                     onDoneSuccess);
        } else if (compiledResult.type === 'module') {
            throw new Error("internal error: not yet implemented");
        } else {
            throw new Error("internal error: unexpected compilation result");
        }
    };


    // CPS'ed for-each.
    var forEachK = function(elts, f, after) {
        var n = elts.length;
        var loop = function(i) {
            if (i >= n) {
                return after();
            } else {
                return f(elts[i], function() { loop(i+1); });
            }
        }
        loop(0);
    };


    // printlnIgnoringVoid: racket-value -> void
    // Prints the racket value out, followed by a newline,
    // unless VOID is being printed.
    Repl.prototype.printlnIgnoringVoid = function(elt) {
        var that = this;
	var outputPort = that.M.params.currentOutputPort;
	if (elt !== plt.runtime.VOID) {
	    outputPort.writeDomNode(
                that.M,
                plt.runtime.toDomNode(elt, that.M.params['print-mode']));
	    outputPort.writeDomNode(that.M, 
                                    plt.runtime.toDomNode("\n", 'display'));
	}
    };

    // print: racket-value -> void
    Repl.prototype.print = function(elt) {
        var that = this;
	var outputPort = that.M.params.currentOutputPort;
	outputPort.writeDomNode(
            that.M,
            plt.runtime.toDomNode(elt, that.M.params['print-mode']));
    };

    // println: racket-value -> void
    // Displays the racket value out, followed by a newline,
    Repl.prototype.display = function(elt) {
        var that = this;
	var outputPort = that.M.params.currentOutputPort;
	outputPort.writeDomNode(
            that.M,
            plt.runtime.toDomNode(elt, 'display'));
    };



    //////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////
    // Expose to the outside world as plt.runtime.Repl.
    plt.runtime.makeRepl = makeRepl;
}());
