(function(global) {
  function callRacketFun(f, args, success, fail) {
    plt.runtime.currentMachine.trampoline(function(M) {
      plt.runtime.PAUSE(function(resume, internalCall) {
        var wrappedSuccess = function(v) {
          resume(function() { /* intentional no-op */ });
          success(v);
        };
        var wrappedFail = function(v) {
          resume(function() { /* intentional no-op */ });
          fail(v);
        };
        internalCall.apply(null, [f, wrappedSuccess, wrappedFail].concat(args));
      });
    });
  }

  function getPrimitive(name) {
    return plt.runtime.Primitives[name];
  }

  function makeSymbol(sym) {
     return plt.baselib.symbols.makeSymbol(sym)
  }

  function toArray(list) {
    return plt.baselib.lists.listToArray(list);
  }

  function getPyretLib(name) {
    var f = plt.runtime.currentMachine.modules["root/lang/pyret-lang-whalesong.rkt"].exports.get(name);

    if(!f) {
      throw "Couldn't get Pyret fun: " + name;
    } else {
      return f;
    }
  }

  function callPyretLibFun(name, args, success, fail) {
    callRacketFun(getPyretFun(name), args, success, fail);
  }

  function callPyretFun(pFun, args, success, fail) {
    var getApp = getPyretLib("p:p-base-app");
    callRacketFun(getApp, [pFun], function(racketFun) {
        callRacketFun(racketFun, args, success, fail);
      },
      function(e) {
        console.error("Couldn't get Pyret fun: ", pFun, e);
      }
    );
  }

  global.whalesongFFI = {
    getPrimitive: getPrimitive,
    callRacketFun: callRacketFun,
    getPyretLib: getPyretLib,
    callPyretFun: callPyretFun,
    makeSymbol: makeSymbol,
    toArray: toArray
  };
})(this);
