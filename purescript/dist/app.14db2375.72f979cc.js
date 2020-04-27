// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"app.14db2375.js":[function(require,module,exports) {
var define;
function _typeof(obj) { "@babel/helpers - typeof"; if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") { _typeof = function _typeof(obj) { return typeof obj; }; } else { _typeof = function _typeof(obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; }; } return _typeof(obj); }

parcelRequire = function (e, r, t, n) {
  var i,
      o = "function" == typeof parcelRequire && parcelRequire,
      u = "function" == typeof require && require;

  function f(t, n) {
    if (!r[t]) {
      if (!e[t]) {
        var i = "function" == typeof parcelRequire && parcelRequire;
        if (!n && i) return i(t, !0);
        if (o) return o(t, !0);
        if (u && "string" == typeof t) return u(t);
        var c = new Error("Cannot find module '" + t + "'");
        throw c.code = "MODULE_NOT_FOUND", c;
      }

      p.resolve = function (r) {
        return e[t][1][r] || r;
      }, p.cache = {};
      var l = r[t] = new f.Module(t);
      e[t][0].call(l.exports, p, l, l.exports, this);
    }

    return r[t].exports;

    function p(e) {
      return f(p.resolve(e));
    }
  }

  f.isParcelRequire = !0, f.Module = function (e) {
    this.id = e, this.bundle = f, this.exports = {};
  }, f.modules = e, f.cache = r, f.parent = o, f.register = function (r, t) {
    e[r] = [function (e, r) {
      r.exports = t;
    }, {}];
  };

  for (var c = 0; c < t.length; c++) {
    try {
      f(t[c]);
    } catch (e) {
      i || (i = e);
    }
  }

  if (t.length) {
    var l = f(t[t.length - 1]);
    "object" == (typeof exports === "undefined" ? "undefined" : _typeof(exports)) && "undefined" != typeof module ? module.exports = l : "function" == typeof define && define.amd ? define(function () {
      return l;
    }) : n && (this[n] = l);
  }

  if (parcelRequire = f, i) throw i;
  return f;
}({
  "EcpK": [function (require, module, exports) {
    function n(e) {
      return (n = "function" == typeof Symbol && "symbol" == _typeof(Symbol.iterator) ? function (n) {
        return _typeof(n);
      } : function (n) {
        return n && "function" == typeof Symbol && n.constructor === Symbol && n !== Symbol.prototype ? "symbol" : _typeof(n);
      })(e);
    }

    var e = {};
    !function (n) {
      "use strict";

      n["Control.Alt"] = n["Control.Alt"] || {};

      n["Control.Alt"].Alt = function (n, e) {
        this.Functor0 = n, this.alt = e;
      };
    }(e), function (n) {
      "use strict";

      n["Control.Semigroupoid"] = n["Control.Semigroupoid"] || {};
      var e = n["Control.Semigroupoid"],
          t = new function (n) {
        this.compose = n;
      }(function (n) {
        return function (e) {
          return function (t) {
            return n(e(t));
          };
        };
      });
      e.compose = function (n) {
        return n.compose;
      }, e.semigroupoidFn = t;
    }(e), function (n) {
      "use strict";

      n["Control.Category"] = n["Control.Category"] || {};
      var e = n["Control.Category"],
          t = n["Control.Semigroupoid"],
          r = new function (n, e) {
        this.Semigroupoid0 = n, this.identity = e;
      }(function () {
        return t.semigroupoidFn;
      }, function (n) {
        return n;
      });
      e.identity = function (n) {
        return n.identity;
      }, e.categoryFn = r;
    }(e), function (n) {
      "use strict";

      n["Data.Function"] = n["Data.Function"] || {};
      var e = n["Data.Function"];
      e.flip = function (n) {
        return function (e) {
          return function (t) {
            return n(t)(e);
          };
        };
      }, e.const = function (n) {
        return function (e) {
          return n;
        };
      };
    }(e), function (n) {
      "use strict";

      (e["Data.Functor"] = e["Data.Functor"] || {}).arrayMap = function (n) {
        return function (e) {
          for (var t = e.length, r = new Array(t), u = 0; u < t; u++) {
            r[u] = n(e[u]);
          }

          return r;
        };
      };
    }(), function (n) {
      "use strict";

      (e["Data.Unit"] = e["Data.Unit"] || {}).unit = {};
    }(), function (n) {
      "use strict";

      n["Data.Unit"] = n["Data.Unit"] || {};
      var e = n["Data.Unit"],
          t = n["Data.Unit"];
      e.unit = t.unit;
    }(e), function (n) {
      "use strict";

      n["Data.Functor"] = n["Data.Functor"] || {};

      var e = n["Data.Functor"],
          t = n["Data.Functor"],
          r = n["Control.Semigroupoid"],
          u = n["Data.Function"],
          a = n["Data.Unit"],
          i = function i(n) {
        this.map = n;
      },
          o = function o(n) {
        return n.map;
      },
          f = new i(r.compose(r.semigroupoidFn)),
          l = new i(t.arrayMap);

      e.Functor = i, e.map = o, e.void = function (n) {
        return o(n)(u.const(a.unit));
      }, e.voidLeft = function (n) {
        return function (e) {
          return function (t) {
            return o(n)(u.const(t))(e);
          };
        };
      }, e.functorFn = f, e.functorArray = l;
    }(e), function (n) {
      "use strict";

      n["Control.Apply"] = n["Control.Apply"] || {};

      var e = n["Control.Apply"],
          t = n["Control.Category"],
          r = n["Data.Function"],
          u = n["Data.Functor"],
          a = function a(n) {
        return n.apply;
      };

      e.Apply = function (n, e) {
        this.Functor0 = n, this.apply = e;
      }, e.apply = a, e.applySecond = function (n) {
        return function (e) {
          return function (i) {
            return a(n)(u.map(n.Functor0())(r.const(t.identity(t.categoryFn)))(e))(i);
          };
        };
      }, e.lift2 = function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return a(n)(u.map(n.Functor0())(e)(t))(r);
            };
          };
        };
      };
    }(e), function (n) {
      "use strict";

      n["Control.Applicative"] = n["Control.Applicative"] || {};

      var e = n["Control.Applicative"],
          t = n["Control.Apply"],
          r = n["Data.Unit"],
          u = function u(n) {
        return n.pure;
      };

      e.Applicative = function (n, e) {
        this.Apply0 = n, this.pure = e;
      }, e.pure = u, e.liftA1 = function (n) {
        return function (e) {
          return function (r) {
            return t.apply(n.Apply0())(u(n)(e))(r);
          };
        };
      }, e.unless = function (n) {
        return function (e) {
          return function (t) {
            if (!e) return t;
            if (e) return u(n)(r.unit);
            throw new Error("Failed pattern match at Control.Applicative (line 62, column 1 - line 62, column 65): " + [e.constructor.name, t.constructor.name]);
          };
        };
      }, e.when = function (n) {
        return function (e) {
          return function (t) {
            if (e) return t;
            if (!e) return u(n)(r.unit);
            throw new Error("Failed pattern match at Control.Applicative (line 57, column 1 - line 57, column 63): " + [e.constructor.name, t.constructor.name]);
          };
        };
      };
    }(e), function (n) {
      "use strict";

      n["Data.Bifunctor"] = n["Data.Bifunctor"] || {};

      var e = n["Data.Bifunctor"],
          t = n["Control.Category"],
          r = function r(n) {
        return n.bimap;
      };

      e.bimap = r, e.Bifunctor = function (n) {
        this.bimap = n;
      }, e.rmap = function (n) {
        return r(n)(t.identity(t.categoryFn));
      };
    }(e), function (n) {
      "use strict";

      n["Data.Either"] = n["Data.Either"] || {};

      var e = n["Data.Either"],
          t = n["Control.Apply"],
          r = n["Data.Bifunctor"],
          u = n["Data.Functor"],
          a = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          i = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          o = new u.Functor(function (n) {
        return function (e) {
          if (e instanceof a) return new a(e.value0);
          if (e instanceof i) return new i(n(e.value0));
          throw new Error("Failed pattern match at Data.Either (line 38, column 1 - line 38, column 52): " + [e.constructor.name]);
        };
      }),
          f = new r.Bifunctor(function (n) {
        return function (e) {
          return function (t) {
            if (t instanceof a) return new a(n(t.value0));
            if (t instanceof i) return new i(e(t.value0));
            throw new Error("Failed pattern match at Data.Either (line 46, column 1 - line 48, column 36): " + [n.constructor.name, e.constructor.name, t.constructor.name]);
          };
        };
      }),
          l = new t.Apply(function () {
        return o;
      }, function (n) {
        return function (e) {
          if (n instanceof a) return new a(n.value0);
          if (n instanceof i) return u.map(o)(n.value0)(e);
          throw new Error("Failed pattern match at Data.Either (line 82, column 1 - line 84, column 30): " + [n.constructor.name, e.constructor.name]);
        };
      });

      e.Left = a, e.Right = i, e.either = function (n) {
        return function (e) {
          return function (t) {
            if (t instanceof a) return n(t.value0);
            if (t instanceof i) return e(t.value0);
            throw new Error("Failed pattern match at Data.Either (line 238, column 1 - line 238, column 64): " + [n.constructor.name, e.constructor.name, t.constructor.name]);
          };
        };
      }, e.functorEither = o, e.bifunctorEither = f, e.applyEither = l;
    }(e), function (n) {
      "use strict";

      n["Control.Plus"] = n["Control.Plus"] || {};
      var e = n["Control.Plus"];
      e.Plus = function (n, e) {
        this.Alt0 = n, this.empty = e;
      }, e.empty = function (n) {
        return n.empty;
      };
    }(e), function (n) {
      "use strict";

      n["Control.Bind"] = n["Control.Bind"] || {};

      var e = n["Control.Bind"],
          t = n["Data.Function"],
          r = function r(n) {
        return n.bind;
      },
          u = function u(n) {
        return t.flip(r(n));
      },
          a = new function (n) {
        this.discard = n;
      }(function (n) {
        return r(n);
      });

      e.Bind = function (n, e) {
        this.Apply0 = n, this.bind = e;
      }, e.bind = r, e.bindFlipped = u, e.discard = function (n) {
        return n.discard;
      }, e.composeKleisliFlipped = function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return u(n)(e)(t(r));
            };
          };
        };
      }, e.discardUnit = a;
    }(e), function (n) {
      "use strict";

      var e = function e(n) {
        return function (e) {
          return n === e;
        };
      };

      n.eqBooleanImpl = e, n.eqIntImpl = e, n.eqStringImpl = e;
    }(e["Data.Eq"] = e["Data.Eq"] || {}), function (n) {
      "use strict";

      n["Data.Eq"] = n["Data.Eq"] || {};

      var e = n["Data.Eq"],
          t = n["Data.Eq"],
          r = function r(n) {
        this.eq = n;
      },
          u = new r(t.eqStringImpl),
          a = new r(t.eqIntImpl),
          i = new r(t.eqBooleanImpl);

      e.Eq = r, e.eq = function (n) {
        return n.eq;
      }, e.eqBoolean = i, e.eqInt = a, e.eqString = u;
    }(e), function (n) {
      "use strict";

      n["Data.Maybe"] = n["Data.Maybe"] || {};

      var e = n["Data.Maybe"],
          t = n["Control.Apply"],
          r = n["Control.Bind"],
          u = n["Control.Category"],
          a = n["Data.Eq"],
          i = n["Data.Function"],
          o = n["Data.Functor"],
          f = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          l = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          c = function c(n) {
        return function (e) {
          return function (t) {
            if (t instanceof f) return n;
            if (t instanceof l) return e(t.value0);
            throw new Error("Failed pattern match at Data.Maybe (line 217, column 1 - line 217, column 51): " + [n.constructor.name, e.constructor.name, t.constructor.name]);
          };
        };
      },
          v = c(!0)(i.const(!1)),
          s = c(!1)(i.const(!0)),
          d = new o.Functor(function (n) {
        return function (e) {
          return e instanceof l ? new l(n(e.value0)) : f.value;
        };
      }),
          p = new t.Apply(function () {
        return d;
      }, function (n) {
        return function (e) {
          if (n instanceof l) return o.map(d)(n.value0)(e);
          if (n instanceof f) return f.value;
          throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [n.constructor.name, e.constructor.name]);
        };
      }),
          m = new r.Bind(function () {
        return p;
      }, function (n) {
        return function (e) {
          if (n instanceof l) return e(n.value0);
          if (n instanceof f) return f.value;
          throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [n.constructor.name, e.constructor.name]);
        };
      });

      e.Nothing = f, e.Just = l, e.maybe = c, e.fromMaybe = function (n) {
        return c(n)(u.identity(u.categoryFn));
      }, e.isJust = s, e.isNothing = v, e.functorMaybe = d, e.bindMaybe = m, e.eqMaybe = function (n) {
        return new a.Eq(function (e) {
          return function (t) {
            return e instanceof f && t instanceof f || e instanceof l && t instanceof l && a.eq(n)(e.value0)(t.value0);
          };
        });
      };
    }(e), function (n) {
      "use strict";

      n["Data.Monoid"] = n["Data.Monoid"] || {};

      n["Data.Monoid"].mempty = function (n) {
        return n.mempty;
      };
    }(e), function (n) {
      "use strict";

      n["Data.Foldable"] = n["Data.Foldable"] || {};

      var e = n["Data.Foldable"],
          t = n["Control.Applicative"],
          r = n["Control.Apply"],
          u = n["Data.Function"],
          a = n["Data.Maybe"],
          i = n["Data.Monoid"],
          o = n["Data.Unit"],
          f = function f(n, e, t) {
        this.foldMap = n, this.foldl = e, this.foldr = t;
      },
          l = function l(n) {
        return n.foldr;
      },
          c = function c(n) {
        return function (e) {
          return function (u) {
            return l(e)((a = r.applySecond(n.Apply0()), function (n) {
              return a(u(n));
            }))(t.pure(n)(o.unit));
            var a;
          };
        };
      },
          v = new f(function (n) {
        return function (e) {
          return function (t) {
            if (t instanceof a.Nothing) return i.mempty(n);
            if (t instanceof a.Just) return e(t.value0);
            throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [e.constructor.name, t.constructor.name]);
          };
        };
      }, function (n) {
        return function (e) {
          return function (t) {
            if (t instanceof a.Nothing) return e;
            if (t instanceof a.Just) return n(e)(t.value0);
            throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [n.constructor.name, e.constructor.name, t.constructor.name]);
          };
        };
      }, function (n) {
        return function (e) {
          return function (t) {
            if (t instanceof a.Nothing) return e;
            if (t instanceof a.Just) return n(t.value0)(e);
            throw new Error("Failed pattern match at Data.Foldable (line 129, column 1 - line 135, column 27): " + [n.constructor.name, e.constructor.name, t.constructor.name]);
          };
        };
      });

      e.Foldable = f, e.foldr = l, e.foldl = function (n) {
        return n.foldl;
      }, e.foldMap = function (n) {
        return n.foldMap;
      }, e.traverse_ = c, e.for_ = function (n) {
        return function (e) {
          return u.flip(c(n)(e));
        };
      }, e.foldableMaybe = v;
    }(e), function (n) {
      "use strict";

      n["Data.Semigroup"] = n["Data.Semigroup"] || {};
      var e = n["Data.Semigroup"];
      e.Semigroup = function (n) {
        this.append = n;
      }, e.append = function (n) {
        return n.append;
      };
    }(e), function (n) {
      "use strict";

      n["Data.List.Types"] = n["Data.List.Types"] || {};

      var e = n["Data.List.Types"],
          t = n["Control.Alt"],
          r = n["Control.Applicative"],
          u = n["Control.Apply"],
          a = n["Control.Plus"],
          i = n["Data.Foldable"],
          o = n["Data.Function"],
          f = n["Data.Functor"],
          l = n["Data.Monoid"],
          c = n["Data.Semigroup"],
          v = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          s = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          d = new f.Functor(function (n) {
        var e;
        return e = v.value, function (t) {
          var r,
              u = e,
              a = !1;

          function i(e, r) {
            return r instanceof s && r.value1 instanceof s && r.value1.value1 instanceof s ? (u = new s(r, e), void (t = r.value1.value1.value1)) : (a = !0, (o = e, function (e) {
              for (var t, r, u, a = o, i = !1; !i;) {
                u = e, t = (r = a) instanceof s && r.value0 instanceof s && r.value0.value1 instanceof s && r.value0.value1.value1 instanceof s ? (a = r.value1, void (e = new s(n(r.value0.value0), new s(n(r.value0.value1.value0), new s(n(r.value0.value1.value1.value0), u))))) : (i = !0, u);
              }

              return t;
            })((i = r) instanceof s && i.value1 instanceof s && i.value1.value1 instanceof v ? new s(n(i.value0), new s(n(i.value1.value0), v.value)) : i instanceof s && i.value1 instanceof v ? new s(n(i.value0), v.value) : v.value));
            var i, o;
          }

          for (; !a;) {
            r = i(u, t);
          }

          return r;
        };
      }),
          p = new i.Foldable(function (n) {
        return function (e) {
          return i.foldl(p)(function (t) {
            var r = c.append(n.Semigroup0())(t);
            return function (n) {
              return r(e(n));
            };
          })(l.mempty(n));
        };
      }, function (n) {
        return function (e) {
          return function (t) {
            var r,
                u = e,
                a = !1;

            function i(e, r) {
              if (r instanceof v) return a = !0, e;
              if (r instanceof s) return u = n(e)(r.value0), void (t = r.value1);
              throw new Error("Failed pattern match at Data.List.Types (line 109, column 12 - line 111, column 30): " + [r.constructor.name]);
            }

            for (; !a;) {
              r = i(u, t);
            }

            return r;
          };
        };
      }, function (n) {
        return function (e) {
          var t = i.foldl(p)(o.flip(s.create))(v.value),
              r = i.foldl(p)(o.flip(n))(e);
          return function (n) {
            return r(t(n));
          };
        };
      }),
          m = new c.Semigroup(function (n) {
        return function (e) {
          return i.foldr(p)(s.create)(e)(n);
        };
      }),
          h = new u.Apply(function () {
        return d;
      }, function (n) {
        return function (e) {
          if (n instanceof v) return v.value;
          if (n instanceof s) return c.append(m)(f.map(d)(n.value0)(e))(u.apply(h)(n.value1)(e));
          throw new Error("Failed pattern match at Data.List.Types (line 155, column 1 - line 157, column 48): " + [n.constructor.name, e.constructor.name]);
        };
      }),
          w = new r.Applicative(function () {
        return h;
      }, function (n) {
        return new s(n, v.value);
      }),
          E = new t.Alt(function () {
        return d;
      }, c.append(m)),
          b = new a.Plus(function () {
        return E;
      }, v.value);

      e.Nil = v, e.Cons = s, e.NonEmptyList = function (n) {
        return n;
      }, e.semigroupList = m, e.foldableList = p, e.applicativeList = w, e.plusList = b;
    }(e), function (n) {
      "use strict";

      n["Data.NonEmpty"] = n["Data.NonEmpty"] || {};

      var e = n["Data.NonEmpty"],
          t = n["Control.Plus"],
          r = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }();

      e.NonEmpty = r, e.singleton = function (n) {
        return function (e) {
          return new r(e, t.empty(n));
        };
      };
    }(e), function (n) {
      "use strict";

      n["Data.List.NonEmpty"] = n["Data.List.NonEmpty"] || {};
      var e,
          t = n["Data.List.NonEmpty"],
          r = n["Data.List.Types"],
          u = n["Data.NonEmpty"],
          a = (e = u.singleton(r.plusList), function (n) {
        return r.NonEmptyList(e(n));
      });
      t.singleton = a, t.cons = function (n) {
        return function (e) {
          return new u.NonEmpty(n, new r.Cons(e.value0, e.value1));
        };
      };
    }(e), function (n) {
      "use strict";

      n["Data.Tuple"] = n["Data.Tuple"] || {};

      var e = n["Data.Tuple"],
          t = n["Data.Functor"],
          r = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          u = new t.Functor(function (n) {
        return function (e) {
          return new r(e.value0, n(e.value1));
        };
      });

      e.Tuple = r, e.fst = function (n) {
        return n.value0;
      }, e.snd = function (n) {
        return n.value1;
      }, e.functorTuple = u;
    }(e), function (n) {
      "use strict";

      n["Control.Applicative.Free"] = n["Control.Applicative.Free"] || {};

      var e = n["Control.Applicative.Free"],
          t = n["Control.Applicative"],
          r = n["Control.Apply"],
          u = n["Control.Category"],
          a = n["Data.Either"],
          i = n["Data.Functor"],
          o = n["Data.List.NonEmpty"],
          f = n["Data.List.Types"],
          l = n["Data.NonEmpty"],
          c = n["Data.Tuple"],
          v = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          s = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          d = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          p = function p(n) {
        return function (e) {
          return new d(n, e);
        };
      },
          m = s.create,
          h = function h(n) {
        return function (e) {
          return function (r) {
            return function (u) {
              return function (a) {
                return function (i) {
                  var l,
                      p = n,
                      m = e,
                      h = r,
                      w = u,
                      E = a,
                      b = !1;

                  function y(n, e, r, u, a, l) {
                    if (a instanceof v) return b = !0, new c.Tuple(new f.Cons({
                      func: t.pure(n)(a.value0),
                      count: l
                    }, e), r);
                    if (a instanceof s) return b = !0, new c.Tuple(new f.Cons({
                      func: u(a.value0),
                      count: l
                    }, e), r);
                    if (a instanceof d) return p = n, m = e, h = o.cons(a.value1)(r), w = u, E = a.value0, void (i = l + 1 | 0);
                    throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [a.constructor.name]);
                  }

                  for (; !b;) {
                    l = y(p, m, h, w, E, i);
                  }

                  return l;
                };
              };
            };
          };
        };
      },
          w = function w(n) {
        return function (e) {
          return function (t) {
            return function (u) {
              var i,
                  o = n,
                  v = e,
                  s = t,
                  d = !1;

              function p(n, e, t, i) {
                if (e instanceof f.Nil) return d = !0, new a.Left(i);

                if (e instanceof f.Cons) {
                  var p = r.apply(n.Apply0())(e.value0.func)(i);
                  if (1 === e.value0.count) return e.value1 instanceof f.Nil ? (d = !0, new a.Left(p)) : (o = n, v = e.value1, s = t, void (u = p));
                  if (t instanceof f.Nil) return d = !0, new a.Left(p);
                  if (t instanceof f.Cons) return d = !0, a.Right.create(new c.Tuple(new f.Cons({
                    func: p,
                    count: e.value0.count - 1 | 0
                  }, e.value1), new l.NonEmpty(t.value0, t.value1)));
                  throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [t.constructor.name]);
                }

                throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [e.constructor.name]);
              }

              for (; !d;) {
                i = p(o, v, s, u);
              }

              return i;
            };
          };
        };
      },
          E = new i.Functor(function (n) {
        return function (e) {
          return p(new v(n))(e);
        };
      }),
          b = function b(n) {
        return function (e) {
          return function (r) {
            return function (r) {
              var u,
                  i = !1;

              function o(u) {
                if (u.value1.value0 instanceof v) {
                  if ((o = w(n)(u.value0)(u.value1.value1)(t.pure(n)(u.value1.value0.value0))) instanceof a.Left) return i = !0, o.value0;
                  if (o instanceof a.Right) return void (r = o.value0);
                  throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [o.constructor.name]);
                }

                if (u.value1.value0 instanceof s) {
                  var o;
                  if ((o = w(n)(u.value0)(u.value1.value1)(e(u.value1.value0.value0))) instanceof a.Left) return i = !0, o.value0;
                  if (o instanceof a.Right) return void (r = o.value0);
                  throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [o.constructor.name]);
                }

                if (!(u.value1.value0 instanceof d)) throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [u.value1.value0.constructor.name]);
                var f = new l.NonEmpty(u.value1.value0.value1, u.value1.value1);
                r = h(n)(u.value0)(f)(e)(u.value1.value0.value0)(1);
              }

              for (; !i;) {
                u = o(r);
              }

              return u;
            }(new c.Tuple(f.Nil.value, o.singleton(r)));
          };
        };
      },
          y = new r.Apply(function () {
        return E;
      }, function (n) {
        return function (e) {
          return p(n)(e);
        };
      }),
          g = new t.Applicative(function () {
        return y;
      }, v.create);

      e.retractFreeAp = function (n) {
        return b(n)(u.identity(u.categoryFn));
      }, e.hoistFreeAp = function (n) {
        return b(g)(function (e) {
          return m(n(e));
        });
      };
    }(e), function (n) {
      "use strict";

      n["Control.Monad"] = n["Control.Monad"] || {};
      var e = n["Control.Monad"],
          t = n["Control.Applicative"],
          r = n["Control.Bind"];
      e.Monad = function (n, e) {
        this.Applicative0 = n, this.Bind1 = e;
      }, e.ap = function (n) {
        return function (e) {
          return function (u) {
            return r.bind(n.Bind1())(e)(function (e) {
              return r.bind(n.Bind1())(u)(function (r) {
                return t.pure(n.Applicative0())(e(r));
              });
            });
          };
        };
      }, e.unlessM = function (n) {
        return function (e) {
          return function (u) {
            return r.bind(n.Bind1())(e)(function (e) {
              return t.unless(n.Applicative0())(e)(u);
            });
          };
        };
      };
    }(e), function (n) {
      "use strict";

      n["Control.Monad.Except.Trans"] = n["Control.Monad.Except.Trans"] || {};

      var e = n["Control.Monad.Except.Trans"],
          t = n["Control.Applicative"],
          r = n["Control.Apply"],
          u = n["Control.Bind"],
          a = n["Control.Monad"],
          i = n["Data.Either"],
          o = n["Data.Functor"],
          f = function f(n) {
        return n;
      },
          l = function l(n) {
        return new u.Bind(function () {
          return c(n);
        }, function (e) {
          return function (r) {
            return u.bind(n.Bind1())(e)(i.either((a = t.pure(n.Applicative0()), function (n) {
              return a(i.Left.create(n));
            }))(function (n) {
              return r(n);
            }));
            var a;
          };
        });
      },
          c = function c(n) {
        return new r.Apply(function () {
          return e = n.Bind1().Apply0().Functor0(), new o.Functor(function (n) {
            return function (n) {
              return function (e) {
                return n(e);
              };
            }(o.map(e)(o.map(i.functorEither)(n)));
          });
          var e;
        }, a.ap(function (n) {
          return new a.Monad(function () {
            return v(n);
          }, function () {
            return l(n);
          });
        }(n)));
      },
          v = function v(n) {
        return new t.Applicative(function () {
          return c(n);
        }, (e = t.pure(n.Applicative0()), function (n) {
          return f(e(i.Right.create(n)));
        }));
        var e;
      };

      e.ExceptT = f, e.runExceptT = function (n) {
        return n;
      }, e.applicativeExceptT = v, e.bindExceptT = l;
    }(e), function (n) {
      "use strict";

      n.pureE = function (n) {
        return function () {
          return n;
        };
      }, n.bindE = function (n) {
        return function (e) {
          return function () {
            return e(n())();
          };
        };
      };
    }(e.Effect = e.Effect || {}), function (n) {
      "use strict";

      n.Effect = n.Effect || {};
      var e = n.Effect,
          t = n.Effect,
          r = n["Control.Applicative"],
          u = n["Control.Apply"],
          a = n["Control.Bind"],
          i = n["Control.Monad"],
          o = n["Data.Functor"],
          f = new i.Monad(function () {
        return v;
      }, function () {
        return l;
      }),
          l = new a.Bind(function () {
        return c;
      }, t.bindE),
          c = new u.Apply(function () {
        return s;
      }, i.ap(f)),
          v = new r.Applicative(function () {
        return c;
      }, t.pureE),
          s = new o.Functor(r.liftA1(v));
      e.functorEffect = s, e.applicativeEffect = v, e.bindEffect = l, e.monadEffect = f;
    }(e), function (n) {
      "use strict";

      n.new = function (n) {
        return function () {
          return {
            value: n
          };
        };
      }, n.read = function (n) {
        return function () {
          return n.value;
        };
      }, n["modify'"] = function (n) {
        return function (e) {
          return function () {
            var t = n(e.value);
            return e.value = t.state, t.value;
          };
        };
      }, n.write = function (n) {
        return function (e) {
          return function () {
            return e.value = n, {};
          };
        };
      };
    }(e["Effect.Ref"] = e["Effect.Ref"] || {}), function (n) {
      "use strict";

      n["Effect.Ref"] = n["Effect.Ref"] || {};
      var e = n["Effect.Ref"],
          t = n["Effect.Ref"],
          r = n["Data.Functor"],
          u = n.Effect;
      e.modify_ = function (n) {
        return function (e) {
          return r.void(u.functorEffect)(function (n) {
            return t["modify'"](function (e) {
              var t = n(e);
              return {
                state: t,
                value: t
              };
            });
          }(n)(e));
        };
      }, e.new = t.new, e.read = t.read, e["modify'"] = t["modify'"], e.write = t.write;
    }(e), function (n) {
      "use strict";

      n["Control.Monad.Rec.Class"] = n["Control.Monad.Rec.Class"] || {};

      var e = n["Control.Monad.Rec.Class"],
          t = n["Control.Bind"],
          r = n["Data.Functor"],
          u = n.Effect,
          a = n["Effect.Ref"],
          i = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          o = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          f = function f(n, e) {
        this.Monad0 = n, this.tailRecM = e;
      },
          l = new f(function () {
        return u.monadEffect;
      }, function (n) {
        return function (e) {
          var f = function f(n) {
            if (n instanceof o) return n.value0;
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): " + [n.constructor.name]);
          };

          return function () {
            var l = t.bindFlipped(u.bindEffect)(a.new)(n(e))();
            return function () {
              for (; !function () {
                var e = a.read(l)();

                if (e instanceof i) {
                  var t = n(e.value0)();
                  return a.write(t)(l)(), !1;
                }

                if (e instanceof o) return !0;
                throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): " + [e.constructor.name]);
              }();) {
                ;
              }
            }(), r.map(u.functorEffect)(f)(a.read(l))();
          };
        };
      });

      e.Loop = i, e.Done = o, e.MonadRec = f, e.tailRecM = function (n) {
        return n.tailRecM;
      }, e.monadRecEffect = l;
    }(e), function (n) {
      "use strict";

      n["Control.Monad.Trans.Class"] = n["Control.Monad.Trans.Class"] || {};
      var e = n["Control.Monad.Trans.Class"];
      e.lift = function (n) {
        return n.lift;
      }, e.MonadTrans = function (n) {
        this.lift = n;
      };
    }(e), function (n) {
      "use strict";

      (e["Unsafe.Coerce"] = e["Unsafe.Coerce"] || {}).unsafeCoerce = function (n) {
        return n;
      };
    }(), function (n) {
      "use strict";

      n["Unsafe.Coerce"] = n["Unsafe.Coerce"] || {};
      var e = n["Unsafe.Coerce"],
          t = n["Unsafe.Coerce"];
      e.unsafeCoerce = t.unsafeCoerce;
    }(e), function (n) {
      "use strict";

      n["Data.Exists"] = n["Data.Exists"] || {};
      var e = n["Data.Exists"],
          t = n["Unsafe.Coerce"],
          r = t.unsafeCoerce,
          u = t.unsafeCoerce;
      e.mkExists = u, e.runExists = r;
    }(e), function (n) {
      "use strict";

      n["Control.Monad.Free.Trans"] = n["Control.Monad.Free.Trans"] || {};

      var e = n["Control.Monad.Free.Trans"],
          t = n["Control.Applicative"],
          r = n["Control.Apply"],
          u = n["Control.Bind"],
          a = n["Control.Monad"],
          i = n["Control.Monad.Rec.Class"],
          o = n["Control.Monad.Trans.Class"],
          f = n["Data.Bifunctor"],
          l = n["Data.Either"],
          c = n["Data.Exists"],
          v = n["Data.Functor"],
          s = n["Data.Unit"],
          d = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          p = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          m = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          h = p.create,
          w = function w(n) {
        return function (e) {
          return new m(c.mkExists(new d(n, e)));
        };
      },
          E = function n(e) {
        return function (t) {
          return new v.Functor(function (r) {
            return function (u) {
              if (u instanceof p) return new p(function (a) {
                return v.map(t)(f.bimap(l.bifunctorEither)(r)(v.map(e)(v.map(n(e)(t))(r))))(u.value0(s.unit));
              });
              if (u instanceof m) return c.runExists(function (u) {
                return w(u.value0)((a = v.map(n(e)(t))(r), function (n) {
                  return a(u.value1(n));
                }));
                var a;
              })(u.value0);
              throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 59, column 1 - line 61, column 71): " + [r.constructor.name, u.constructor.name]);
            };
          });
        };
      },
          b = function b(n) {
        return function (e) {
          return new a.Monad(function () {
            return A(n)(e);
          }, function () {
            return y(n)(e);
          });
        };
      },
          y = function y(n) {
        return function (e) {
          return new u.Bind(function () {
            return g(n)(e);
          }, function (n) {
            return function (e) {
              return n instanceof m ? c.runExists(function (n) {
                return w(n.value0)(function (t) {
                  return w(function (e) {
                    return n.value1(t);
                  })(e);
                });
              })(n.value0) : w(function (e) {
                return n;
              })(e);
            };
          });
        };
      },
          g = function g(n) {
        return function (e) {
          return new r.Apply(function () {
            return E(n)(e.Bind1().Apply0().Functor0());
          }, a.ap(b(n)(e)));
        };
      },
          A = function A(n) {
        return function (e) {
          return new t.Applicative(function () {
            return g(n)(e);
          }, function (n) {
            return new p(function (r) {
              return t.pure(e.Applicative0())(new l.Left(n));
            });
          });
        };
      },
          D = function D(n) {
        return function (e) {
          return i.tailRecM(e)(function (r) {
            if (r instanceof p) return v.map(e.Monad0().Bind1().Apply0().Functor0())(i.Done.create)(r.value0(s.unit));
            if (r instanceof m) return c.runExists(function (r) {
              var a = r.value0(s.unit);
              if (a instanceof p) return u.bind(e.Monad0().Bind1())(a.value0(s.unit))(function (a) {
                if (a instanceof l.Left) return t.pure(e.Monad0().Applicative0())(new i.Loop(r.value1(a.value0)));
                if (a instanceof l.Right) return t.pure(e.Monad0().Applicative0())(new i.Done(new l.Right(v.map(n)(function (t) {
                  return u.bind(y(n)(e.Monad0()))(t)(r.value1);
                })(a.value0))));
                throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 54, column 20 - line 56, column 67): " + [a.constructor.name]);
              });
              if (a instanceof m) return c.runExists(function (a) {
                return t.pure(e.Monad0().Applicative0())(new i.Loop(u.bind(y(n)(e.Monad0()))(a.value0(s.unit))(function (t) {
                  return u.bind(y(n)(e.Monad0()))(a.value1(t))(r.value1);
                })));
              })(a.value0);
              throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 52, column 5 - line 57, column 98): " + [a.constructor.name]);
            })(r.value0);
            throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 49, column 3 - line 49, column 75): " + [r.constructor.name]);
          });
        };
      };

      e.freeT = h, e.liftFreeT = function (n) {
        return function (e) {
          return function (r) {
            return new p(function (u) {
              return t.pure(e.Applicative0())(new l.Right(v.map(n)(t.pure(A(n)(e)))(r)));
            });
          };
        };
      }, e.resume = D, e.runFreeT = function (n) {
        return function (e) {
          return function (r) {
            return i.tailRecM(e)(u.composeKleisliFlipped(e.Monad0().Bind1())(function (n) {
              if (n instanceof l.Left) return t.pure(e.Monad0().Applicative0())(new i.Done(n.value0));
              if (n instanceof l.Right) return v.map(e.Monad0().Bind1().Apply0().Functor0())(i.Loop.create)(r(n.value0));
              throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 141, column 3 - line 141, column 63): " + [n.constructor.name]);
            })(D(n)(e)));
          };
        };
      }, e.functorFreeT = E, e.applicativeFreeT = A, e.bindFreeT = y, e.monadTransFreeT = function (n) {
        return new o.MonadTrans(function (n) {
          return function (e) {
            return new p(function (t) {
              return v.map(n.Bind1().Apply0().Functor0())(l.Left.create)(e);
            });
          };
        });
      }, e.monadRecFreeT = function (n) {
        return function (e) {
          return new i.MonadRec(function () {
            return b(n)(e);
          }, function (r) {
            return function a(o) {
              return u.bind(y(n)(e))(r(o))(function (r) {
                if (r instanceof i.Loop) return a(r.value0);
                if (r instanceof i.Done) return t.pure(A(n)(e))(r.value0);
                throw new Error("Failed pattern match at Control.Monad.Free.Trans (line 82, column 15 - line 84, column 25): " + [r.constructor.name]);
              });
            };
          });
        };
      };
    }(e), function (n) {
      "use strict";

      n["Control.Parallel.Class"] = n["Control.Parallel.Class"] || {};
      var e = n["Control.Parallel.Class"];
      e.parallel = function (n) {
        return n.parallel;
      }, e.sequential = function (n) {
        return n.sequential;
      }, e.Parallel = function (n, e, t, r) {
        this.Applicative1 = n, this.Monad0 = e, this.parallel = t, this.sequential = r;
      };
    }(e), function (n) {
      "use strict";

      n["Data.Newtype"] = n["Data.Newtype"] || {};
      var e = n["Data.Newtype"];
      e.unwrap = function (n) {
        return n.unwrap;
      }, e.Newtype = function (n, e) {
        this.unwrap = n, this.wrap = e;
      };
    }(e), function (n) {
      "use strict";

      n["Data.Identity"] = n["Data.Identity"] || {};

      var e = n["Data.Identity"],
          t = n["Control.Applicative"],
          r = n["Control.Apply"],
          u = n["Data.Functor"],
          a = function a(n) {
        return n;
      },
          i = new n["Data.Newtype"].Newtype(function (n) {
        return n;
      }, a),
          o = new u.Functor(function (n) {
        return function (e) {
          return n(e);
        };
      }),
          f = new r.Apply(function () {
        return o;
      }, function (n) {
        return function (e) {
          return n(e);
        };
      }),
          l = new t.Applicative(function () {
        return f;
      }, a);

      e.newtypeIdentity = i, e.functorIdentity = o, e.applicativeIdentity = l;
    }(e), function (n) {
      "use strict";

      n["Data.Profunctor"] = n["Data.Profunctor"] || {};

      var e = n["Data.Profunctor"],
          t = n["Control.Category"],
          r = function r(n) {
        this.dimap = n;
      },
          u = new r(function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return e(t(n(r)));
            };
          };
        };
      }),
          a = function a(n) {
        return n.dimap;
      };

      e.dimap = a, e.Profunctor = r, e.rmap = function (n) {
        return function (e) {
          return a(n)(t.identity(t.categoryFn))(e);
        };
      }, e.profunctorFn = u;
    }(e), function (n) {
      "use strict";

      n["Control.Coroutine"] = n["Control.Coroutine"] || {};

      var e = n["Control.Coroutine"],
          t = n["Control.Applicative"],
          r = n["Control.Apply"],
          u = n["Control.Bind"],
          a = n["Control.Category"],
          i = n["Control.Monad.Except.Trans"],
          o = n["Control.Monad.Free.Trans"],
          f = n["Control.Monad.Rec.Class"],
          l = n["Control.Monad.Trans.Class"],
          c = n["Control.Parallel.Class"],
          v = n["Data.Bifunctor"],
          s = n["Data.Either"],
          d = n["Data.Functor"],
          p = n["Data.Identity"],
          m = n["Data.Maybe"],
          h = n["Data.Newtype"],
          w = n["Data.Profunctor"],
          E = n["Data.Tuple"],
          b = n["Data.Unit"],
          y = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          g = new w.Profunctor(function (n) {
        return function (e) {
          return function (t) {
            return w.dimap(w.profunctorFn)(n)(e)(t);
          };
        };
      }),
          A = function A(n) {
        return function (e) {
          return function (r) {
            return function (a) {
              return function (f) {
                return function (l) {
                  return function (c) {
                    return o.freeT(function (v) {
                      return function l(c) {
                        return i.runExceptT(u.bind(i.bindExceptT(a.Monad0()))(i.ExceptT(o.resume(n)(a)(c.value0)))(function (n) {
                          return u.bind(i.bindExceptT(a.Monad0()))(i.ExceptT(o.resume(e)(a)(c.value1)))(function (e) {
                            return t.pure(i.applicativeExceptT(a.Monad0()))(d.map(r)(function (n) {
                              return o.freeT(function (e) {
                                return l(n);
                              });
                            })(f(E.Tuple.create)(n)(e)));
                          });
                        }));
                      }(new E.Tuple(l, c));
                    });
                  };
                };
              };
            };
          };
        };
      },
          D = function D(n) {
        return function (e) {
          return function (a) {
            return function (i) {
              return function (f) {
                return function (l) {
                  return function (v) {
                    return function (p) {
                      return o.freeT(function (m) {
                        return function v(p) {
                          return u.bind(i.Monad0().Bind1())(c.sequential(f)(r.apply(f.Applicative1().Apply0())(d.map(f.Applicative1().Apply0().Functor0())(r.lift2(s.applyEither)(l(E.Tuple.create)))(c.parallel(f)(o.resume(n)(i)(p.value0))))(c.parallel(f)(o.resume(e)(i)(p.value1)))))(function (n) {
                            if (n instanceof s.Left) return t.pure(i.Monad0().Applicative0())(new s.Left(n.value0));
                            if (n instanceof s.Right) return t.pure(i.Monad0().Applicative0())(new s.Right(d.map(a)(function (n) {
                              return o.freeT(function (e) {
                                return v(n);
                              });
                            })(n.value0)));
                            throw new Error("Failed pattern match at Control.Coroutine (line 79, column 5 - line 81, column 63): " + [n.constructor.name]);
                          });
                        }(new E.Tuple(v, p));
                      });
                    };
                  };
                };
              };
            };
          };
        };
      },
          C = new d.Functor(w.rmap(g)),
          M = new v.Bifunctor(function (n) {
        return function (e) {
          return function (t) {
            return new y(n(t.value0), e(t.value1));
          };
        };
      }),
          F = new d.Functor(v.rmap(M));

      e.runProcess = function (n) {
        return o.runFreeT(p.functorIdentity)(n)((e = t.pure(n.Monad0().Applicative0()), r = h.unwrap(p.newtypeIdentity), function (n) {
          return e(r(n));
        }));
        var e, r;
      }, e.producer = function (n) {
        return function (e) {
          return (r = F, function (n) {
            return function (e) {
              return f.tailRecM(o.monadRecFreeT(r)(n))(function (t) {
                return d.map(o.functorFreeT(r)(n.Bind1().Apply0().Functor0()))(m.maybe(new f.Loop(b.unit))(f.Done.create))(e);
              })(b.unit);
            };
          })(n)(u.bind(o.bindFreeT(F)(n))(l.lift(o.monadTransFreeT(F))(n)(e))(function (e) {
            if (e instanceof s.Left) return d.voidLeft(o.functorFreeT(F)(n.Bind1().Apply0().Functor0()))(function (n) {
              return function (e) {
                return o.liftFreeT(F)(n)(new y(e, b.unit));
              };
            }(n)(e.value0))(m.Nothing.value);
            if (e instanceof s.Right) return t.pure(o.applicativeFreeT(F)(n))(new m.Just(e.value0));
            throw new Error("Failed pattern match at Control.Coroutine (line 125, column 3 - line 127, column 29): " + [e.constructor.name]);
          }));
          var r;
        };
      }, e.await = function (n) {
        return o.liftFreeT(C)(n)(a.identity(a.categoryFn));
      }, e.connect = function (n) {
        return function (e) {
          return D(F)(C)(p.functorIdentity)(n)(e)(function (n) {
            return function (e) {
              return function (t) {
                return n(e.value1)(t(e.value0));
              };
            };
          });
        };
      }, e.pullFrom = function (n) {
        return A(C)(F)(p.functorIdentity)(n)(function (n) {
          return function (e) {
            return function (r) {
              return t.pure(p.applicativeIdentity)(n(e(r.value0))(r.value1));
            };
          };
        });
      }, e.functorAwait = C;
    }(e), function (n) {
      "use strict";

      n["Control.Monad.Error.Class"] = n["Control.Monad.Error.Class"] || {};
      var e = n["Control.Monad.Error.Class"],
          t = n["Control.Applicative"],
          r = n["Data.Either"],
          u = n["Data.Functor"];
      e.throwError = function (n) {
        return n.throwError;
      }, e.MonadThrow = function (n, e) {
        this.Monad0 = n, this.throwError = e;
      }, e.MonadError = function (n, e) {
        this.MonadThrow0 = n, this.catchError = e;
      }, e.try = function (n) {
        return function (e) {
          return n.catchError(u.map(n.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(r.Right.create)(e))((a = t.pure(n.MonadThrow0().Monad0().Applicative0()), function (n) {
            return a(r.Left.create(n));
          }));
          var a;
        };
      };
    }(e), function (n) {
      "use strict";

      var e = function () {
        var n = {},
            e = "Pure",
            t = "Throw",
            r = "Catch",
            u = "Sync",
            a = "Async",
            i = "Bind",
            o = "Bracket",
            f = "Fork",
            l = "Sequential",
            c = "Map",
            v = "Apply",
            s = "Alt",
            d = "Cons",
            p = "Resume",
            m = "Release",
            h = "Finalizer",
            w = "Finalized",
            E = "Forked";

        function b(n, e, t, r) {
          this.tag = n, this._1 = e, this._2 = t, this._3 = r;
        }

        function y(n) {
          var e = function e(_e, t, r) {
            return new b(n, _e, t, r);
          };

          return e.tag = n, e;
        }

        function g(n) {
          return new b(e, void 0);
        }

        function A(n) {
          try {
            n();
          } catch (e) {
            setTimeout(function () {
              throw e;
            }, 0);
          }
        }

        function D(n, e, t) {
          try {
            return e(t());
          } catch (r) {
            return n(r);
          }
        }

        function C(n, e, t) {
          try {
            return e(t)();
          } catch (r) {
            return t(n(r))(), g;
          }
        }

        var M = function () {
          var n = 1024,
              e = 0,
              t = 0,
              r = new Array(n),
              u = !1;

          function a() {
            var a;

            for (u = !0; 0 !== e;) {
              e--, a = r[t], r[t] = void 0, t = (t + 1) % n, a();
            }

            u = !1;
          }

          return {
            isDraining: function isDraining() {
              return u;
            },
            enqueue: function enqueue(i) {
              var o;
              e === n && (o = u, a(), u = o), r[(t + e) % n] = i, e++, u || a();
            }
          };
        }();

        var F = 0,
            T = 1,
            H = 2,
            L = 3,
            S = 4,
            k = 5,
            _ = 6;

        function N(n, c, v) {
          var s = 0,
              E = F,
              y = v,
              g = null,
              R = null,
              x = null,
              U = null,
              P = null,
              O = 0,
              W = 0,
              B = null,
              q = !0;

          function Q(v) {
            for (var W, V, z;;) {
              switch (W = null, V = null, z = null, E) {
                case H:
                  E = T;

                  try {
                    y = x(y), null === U ? x = null : (x = U._1, U = U._2);
                  } catch (j) {
                    E = k, g = n.left(j), y = null;
                  }

                  break;

                case L:
                  n.isLeft(y) ? (E = k, g = y, y = null) : null === x ? E = k : (E = H, y = n.fromRight(y));
                  break;

                case T:
                  switch (y.tag) {
                    case i:
                      x && (U = new b(d, x, U)), x = y._2, E = T, y = y._1;
                      break;

                    case e:
                      null === x ? (E = k, y = n.right(y._1)) : (E = H, y = y._1);
                      break;

                    case u:
                      E = L, y = D(n.left, n.right, y._1);
                      break;

                    case a:
                      return E = S, void (y = C(n.left, y._1, function (n) {
                        return function () {
                          s === v && (s++, M.enqueue(function () {
                            s === v + 1 && (E = L, y = n, Q(s));
                          }));
                        };
                      }));

                    case t:
                      E = k, g = n.left(y._1), y = null;
                      break;

                    case r:
                      P = new b(d, y, null === x ? P : new b(d, new b(p, x, U), P, R), R), x = null, U = null, E = T, y = y._1;
                      break;

                    case o:
                      O++, P = new b(d, y, null === x ? P : new b(d, new b(p, x, U), P, R), R), x = null, U = null, E = T, y = y._1;
                      break;

                    case f:
                      E = L, W = N(n, c, y._2), c && c.register(W), y._1 && W.run(), y = n.right(W);
                      break;

                    case l:
                      E = T, y = I(n, c, y._1);
                  }

                  break;

                case k:
                  if (x = null, U = null, null === P) E = _, y = R || g || y;else switch (W = P._3, z = P._1, P = P._2, z.tag) {
                    case r:
                      R && R !== W && 0 === O ? E = k : g && (E = T, y = z._2(n.fromLeft(g)), g = null);
                      break;

                    case p:
                      R && R !== W && 0 === O || g ? E = k : (x = z._1, U = z._2, E = H, y = n.fromRight(y));
                      break;

                    case o:
                      O--, null === g && (V = n.fromRight(y), P = new b(d, new b(m, z._2, V), P, W), (R === W || O > 0) && (E = T, y = z._3(V)));
                      break;

                    case m:
                      P = new b(d, new b(w, y, g), P, R), E = T, y = R && R !== W && 0 === O ? z._1.killed(n.fromLeft(R))(z._2) : g ? z._1.failed(n.fromLeft(g))(z._2) : z._1.completed(n.fromRight(y))(z._2), g = null, O++;
                      break;

                    case h:
                      O++, P = new b(d, new b(w, y, g), P, R), E = T, y = z._1;
                      break;

                    case w:
                      O--, E = k, y = z._1, g = z._2;
                  }
                  break;

                case _:
                  for (var J in B) {
                    B.hasOwnProperty(J) && (q = q && B[J].rethrow, A(B[J].handler(y)));
                  }

                  return B = null, void (R && g ? setTimeout(function () {
                    throw n.fromLeft(g);
                  }, 0) : n.isLeft(y) && q && setTimeout(function () {
                    if (q) throw n.fromLeft(y);
                  }, 0));

                case F:
                  E = T;
                  break;

                case S:
                  return;
              }
            }
          }

          function V(n) {
            return function () {
              if (E === _) return q = q && n.rethrow, n.handler(y)(), function () {};
              var e = W++;
              return (B = B || {})[e] = n, function () {
                null !== B && delete B[e];
              };
            };
          }

          return {
            kill: function kill(e, t) {
              return function () {
                if (E === _) return t(n.right(void 0))(), function () {};
                var r = V({
                  rethrow: !1,
                  handler: function handler() {
                    return t(n.right(void 0));
                  }
                })();

                switch (E) {
                  case F:
                    R = n.left(e), E = _, y = R, Q(s);
                    break;

                  case S:
                    null === R && (R = n.left(e)), 0 === O && (E === S && (P = new b(d, new b(h, y(e)), P, R)), E = k, y = null, g = null, Q(++s));
                    break;

                  default:
                    null === R && (R = n.left(e)), 0 === O && (E = k, y = null, g = null);
                }

                return r;
              };
            },
            join: function join(n) {
              return function () {
                var e = V({
                  rethrow: !1,
                  handler: n
                })();
                return E === F && Q(s), e;
              };
            },
            onComplete: V,
            isSuspended: function isSuspended() {
              return E === F;
            },
            run: function run() {
              E === F && (M.isDraining() ? Q(s) : M.enqueue(function () {
                Q(s);
              }));
            }
          };
        }

        function R(e, t, r, u) {
          var i = 0,
              o = {},
              f = 0,
              l = {},
              p = new Error("[ParAff] Early exit"),
              m = null,
              h = n;

          function w(t, r, u) {
            var a,
                i,
                f = r,
                l = null,
                p = null,
                m = 0,
                h = {};

            n: for (;;) {
              switch (a = null, f.tag) {
                case E:
                  if (f._3 === n && (a = o[f._1], h[m++] = a.kill(t, function (n) {
                    return function () {
                      0 === --m && u(n)();
                    };
                  })), null === l) break n;
                  f = l._2, null === p ? l = null : (l = p._1, p = p._2);
                  break;

                case c:
                  f = f._2;
                  break;

                case v:
                case s:
                  l && (p = new b(d, l, p)), l = f, f = f._1;
              }
            }

            if (0 === m) u(e.right(void 0))();else for (i = 0, a = m; i < a; i++) {
              h[i] = h[i]();
            }
            return h;
          }

          function y(t) {
            return function (r) {
              return function () {
                delete o[t._1], t._3 = r, function t(r, a, i) {
                  var o, d, h, E, b, y;

                  for (e.isLeft(r) ? (o = r, d = null) : (d = r, o = null);;) {
                    if (h = null, E = null, b = null, y = null, null !== m) return;
                    if (null === a) return void u(o || d)();
                    if (a._3 !== n) return;

                    switch (a.tag) {
                      case c:
                        null === o ? (a._3 = e.right(a._1(e.fromRight(d))), d = a._3) : a._3 = o;
                        break;

                      case v:
                        if (h = a._1._3, E = a._2._3, o) {
                          if (a._3 = o, b = !0, y = f++, l[y] = w(p, o === h ? a._2 : a._1, function () {
                            return function () {
                              delete l[y], b ? b = !1 : null === i ? t(o, null, null) : t(o, i._1, i._2);
                            };
                          }), b) return void (b = !1);
                        } else {
                          if (h === n || E === n) return;
                          d = e.right(e.fromRight(h)(e.fromRight(E))), a._3 = d;
                        }

                        break;

                      case s:
                        if (h = a._1._3, E = a._2._3, h === n && e.isLeft(E) || E === n && e.isLeft(h)) return;
                        if (h !== n && e.isLeft(h) && E !== n && e.isLeft(E)) o = d === h ? E : h, d = null, a._3 = o;else if (a._3 = d, b = !0, y = f++, l[y] = w(p, d === h ? a._2 : a._1, function () {
                          return function () {
                            delete l[y], b ? b = !1 : null === i ? t(d, null, null) : t(d, i._1, i._2);
                          };
                        }), b) return void (b = !1);
                    }

                    null === i ? a = null : (a = i._1, i = i._2);
                  }
                }(r, t._2._1, t._2._2);
              };
            };
          }

          return function () {
            var u,
                a,
                f = T,
                l = r,
                p = null,
                m = null;

            n: for (;;) {
              switch (u = null, a = null, f) {
                case T:
                  switch (l.tag) {
                    case c:
                      p && (m = new b(d, p, m)), p = new b(c, l._1, n, n), l = l._2;
                      break;

                    case v:
                      p && (m = new b(d, p, m)), p = new b(v, n, l._2, n), l = l._1;
                      break;

                    case s:
                      p && (m = new b(d, p, m)), p = new b(s, n, l._2, n), l = l._1;
                      break;

                    default:
                      a = i++, f = k, u = l, l = new b(E, a, new b(d, p, m), n), (u = N(e, t, u)).onComplete({
                        rethrow: !1,
                        handler: y(l)
                      })(), o[a] = u, t && t.register(u);
                  }

                  break;

                case k:
                  if (null === p) break n;
                  p._1 === n ? (p._1 = l, f = T, l = p._2, p._2 = n) : (p._2 = l, l = p, null === m ? p = null : (p = m._1, m = m._2));
              }
            }

            for (h = l, a = 0; a < i; a++) {
              o[a].run();
            }
          }(), function (n) {
            return new b(a, function (t) {
              return function () {
                return function (n, t) {
                  var r;

                  for (var u in m = e.left(n), l) {
                    if (l.hasOwnProperty(u)) for (u in r = l[u]) {
                      r.hasOwnProperty(u) && r[u]();
                    }
                  }

                  l = null;
                  var i = w(n, h, t);
                  return function (n) {
                    return new b(a, function (n) {
                      return function () {
                        for (var n in i) {
                          i.hasOwnProperty(n) && i[n]();
                        }

                        return g;
                      };
                    });
                  };
                }(n, t);
              };
            });
          };
        }

        function I(n, e, t) {
          return new b(a, function (r) {
            return function () {
              return R(n, e, t, r);
            };
          });
        }

        return b.EMPTY = n, b.Pure = y(e), b.Throw = y(t), b.Catch = y(r), b.Sync = y(u), b.Async = y(a), b.Bind = y(i), b.Bracket = y(o), b.Fork = y(f), b.Seq = y(l), b.ParMap = y(c), b.ParApply = y(v), b.ParAlt = y(s), b.Fiber = N, b.Supervisor = function (n) {
          var e = {},
              t = 0,
              r = 0;
          return {
            register: function register(n) {
              var u = t++;
              n.onComplete({
                rethrow: !0,
                handler: function handler(n) {
                  return function () {
                    r--, delete e[u];
                  };
                }
              })(), e[u] = n, r++;
            },
            isEmpty: function isEmpty() {
              return 0 === r;
            },
            killAll: function killAll(a, i) {
              return function () {
                if (0 === r) return i();
                var o = 0,
                    f = {};

                function l(t) {
                  f[t] = e[t].kill(a, function (e) {
                    return function () {
                      delete f[t], o--, n.isLeft(e) && n.fromLeft(e) && setTimeout(function () {
                        throw n.fromLeft(e);
                      }, 0), 0 === o && i();
                    };
                  })();
                }

                for (var c in e) {
                  e.hasOwnProperty(c) && (o++, l(c));
                }

                return e = {}, t = 0, r = 0, function (n) {
                  return new b(u, function () {
                    for (var n in f) {
                      f.hasOwnProperty(n) && f[n]();
                    }
                  });
                };
              };
            }
          };
        }, b.Scheduler = M, b.nonCanceler = g, b;
      }();

      n._pure = e.Pure, n._throwError = e.Throw, n._catchError = function (n) {
        return function (t) {
          return e.Catch(n, t);
        };
      }, n._map = function (n) {
        return function (t) {
          return t.tag === e.Pure.tag ? e.Pure(n(t._1)) : e.Bind(t, function (t) {
            return e.Pure(n(t));
          });
        };
      }, n._bind = function (n) {
        return function (t) {
          return e.Bind(n, t);
        };
      }, n._fork = function (n) {
        return function (t) {
          return e.Fork(n, t);
        };
      }, n._liftEffect = e.Sync, n._parAffMap = function (n) {
        return function (t) {
          return e.ParMap(n, t);
        };
      }, n._parAffApply = function (n) {
        return function (t) {
          return e.ParApply(n, t);
        };
      }, n.makeAff = e.Async, n.generalBracket = function (n) {
        return function (t) {
          return function (r) {
            return e.Bracket(n, t, r);
          };
        };
      }, n._makeFiber = function (n, t) {
        return function () {
          return e.Fiber(n, null, t);
        };
      }, n._sequential = e.Seq;
    }(e["Effect.Aff"] = e["Effect.Aff"] || {}), function (n) {
      "use strict";

      n["Effect.Class"] = n["Effect.Class"] || {};
      var e = n["Effect.Class"];
      e.liftEffect = function (n) {
        return n.liftEffect;
      }, e.MonadEffect = function (n, e) {
        this.Monad0 = n, this.liftEffect = e;
      };
    }(e), function (n) {
      "use strict";

      (e["Effect.Unsafe"] = e["Effect.Unsafe"] || {}).unsafePerformEffect = function (n) {
        return n();
      };
    }(), function (n) {
      "use strict";

      n["Effect.Unsafe"] = n["Effect.Unsafe"] || {};
      var e = n["Effect.Unsafe"],
          t = n["Effect.Unsafe"];
      e.unsafePerformEffect = t.unsafePerformEffect;
    }(e), function (n) {
      "use strict";

      (e["Partial.Unsafe"] = e["Partial.Unsafe"] || {}).unsafePartial = function (n) {
        return n();
      };
    }(), function (n) {
      "use strict";

      (e["Partial"] = e["Partial"] || {}).crashWith = function () {
        return function (n) {
          throw new Error(n);
        };
      };
    }(), function (n) {
      "use strict";

      n.Partial = n.Partial || {};
      var e = n.Partial,
          t = n.Partial;
      e.crashWith = t.crashWith;
    }(e), function (n) {
      "use strict";

      n["Partial.Unsafe"] = n["Partial.Unsafe"] || {};
      var e = n["Partial.Unsafe"],
          t = n["Partial.Unsafe"],
          r = n.Partial;

      e.unsafeCrashWith = function (n) {
        return t.unsafePartial(function (e) {
          return r.crashWith()(n);
        });
      };
    }(e), function (n) {
      "use strict";

      n["Effect.Aff"] = n["Effect.Aff"] || {};

      var e,
          t,
          r,
          u,
          a = n["Effect.Aff"],
          i = n["Effect.Aff"],
          o = n["Control.Applicative"],
          f = n["Control.Apply"],
          l = n["Control.Bind"],
          c = n["Control.Monad"],
          v = n["Control.Monad.Error.Class"],
          s = n["Control.Monad.Rec.Class"],
          d = n["Control.Parallel.Class"],
          p = n["Data.Either"],
          m = n["Data.Function"],
          h = n["Data.Functor"],
          w = n["Data.Unit"],
          E = n.Effect,
          b = n["Effect.Class"],
          y = n["Effect.Unsafe"],
          g = n["Partial.Unsafe"],
          A = n["Unsafe.Coerce"],
          D = i._fork(!1),
          C = new h.Functor(i._parAffMap),
          M = new h.Functor(i._map),
          F = i._fork(!0),
          T = {
        isLeft: function isLeft(n) {
          if (n instanceof p.Left) return !0;
          if (n instanceof p.Right) return !1;
          throw new Error("Failed pattern match at Effect.Aff (line 390, column 12 - line 392, column 20): " + [n.constructor.name]);
        },
        fromLeft: function fromLeft(n) {
          if (n instanceof p.Left) return n.value0;
          if (n instanceof p.Right) return g.unsafeCrashWith("unsafeFromLeft: Right");
          throw new Error("Failed pattern match at Effect.Aff (line 395, column 20 - line 397, column 54): " + [n.constructor.name]);
        },
        fromRight: function fromRight(n) {
          if (n instanceof p.Right) return n.value0;
          if (n instanceof p.Left) return g.unsafeCrashWith("unsafeFromRight: Left");
          throw new Error("Failed pattern match at Effect.Aff (line 400, column 21 - line 402, column 54): " + [n.constructor.name]);
        },
        left: p.Left.create,
        right: p.Right.create
      },
          H = function H(n) {
        return i._makeFiber(T, n);
      },
          L = function L(n) {
        return function () {
          var e = H(n)();
          return e.run(), e;
        };
      },
          S = (e = h.void(E.functorEffect), function (n) {
        return e(L(n));
      }),
          k = new f.Apply(function () {
        return C;
      }, i._parAffApply),
          _ = new c.Monad(function () {
        return I;
      }, function () {
        return N;
      }),
          N = new l.Bind(function () {
        return R;
      }, i._bind),
          R = new f.Apply(function () {
        return M;
      }, c.ap(_)),
          I = new o.Applicative(function () {
        return R;
      }, i._pure),
          x = new b.MonadEffect(function () {
        return _;
      }, i._liftEffect),
          U = (t = b.liftEffect(x), function (n) {
        return m.const(t(n));
      }),
          P = function P(n) {
        return i.makeAff(function (e) {
          return h.map(E.functorEffect)(U)(n.join(e));
        });
      },
          O = new h.Functor(function (n) {
        return function (e) {
          return y.unsafePerformEffect(H(h.map(M)(n)(P(e))));
        };
      }),
          W = new v.MonadThrow(function () {
        return _;
      }, i._throwError),
          B = new v.MonadError(function () {
        return W;
      }, i._catchError),
          q = new d.Parallel(function () {
        return Q;
      }, function () {
        return _;
      }, A.unsafeCoerce, i._sequential),
          Q = new o.Applicative(function () {
        return k;
      }, (r = d.parallel(q), u = o.pure(I), function (n) {
        return r(u(n));
      })),
          V = new s.MonadRec(function () {
        return _;
      }, function (n) {
        return function e(t) {
          return l.bind(N)(n(t))(function (n) {
            if (n instanceof s.Done) return o.pure(I)(n.value0);
            if (n instanceof s.Loop) return e(n.value0);
            throw new Error("Failed pattern match at Effect.Aff (line 100, column 7 - line 102, column 22): " + [n.constructor.name]);
          });
        };
      }),
          z = m.const(o.pure(I)(w.unit));

      a.launchAff_ = S, a.runAff_ = function (n) {
        return function (e) {
          return h.void(E.functorEffect)(function (n) {
            return function (e) {
              return L(l.bindFlipped(N)((t = b.liftEffect(x), function (e) {
                return t(n(e));
              }))(v.try(B)(e)));
              var t;
            };
          }(n)(e));
        };
      }, a.forkAff = F, a.suspendAff = D, a.finally = function (n) {
        return function (e) {
          return (t = o.pure(I)(w.unit), function (n) {
            return i.generalBracket(t)({
              killed: m.const(n),
              failed: m.const(n),
              completed: m.const(n)
            });
          })(m.const(n))(m.const(e));
          var t;
        };
      }, a.killFiber = function (n) {
        return function (e) {
          return l.bind(N)(b.liftEffect(x)(e.isSuspended))(function (t) {
            return t ? b.liftEffect(x)(h.void(E.functorEffect)(e.kill(n, m.const(o.pure(E.applicativeEffect)(w.unit))))) : i.makeAff(function (t) {
              return h.map(E.functorEffect)(U)(e.kill(n, t));
            });
          });
        };
      }, a.joinFiber = P, a.nonCanceler = z, a.effectCanceler = U, a.functorAff = M, a.applicativeAff = I, a.bindAff = N, a.monadAff = _, a.monadRecAff = V, a.monadThrowAff = W, a.monadErrorAff = B, a.monadEffectAff = x, a.applicativeParAff = Q, a.parallelAff = q, a.functorFiber = O, a.makeAff = i.makeAff;
    }(e), function (n) {
      "use strict";

      n["Control.Monad.Fork.Class"] = n["Control.Monad.Fork.Class"] || {};
      var e = n["Control.Monad.Fork.Class"],
          t = n["Effect.Aff"],
          r = new function (n, e, t, r, u) {
        this.Functor1 = n, this.Monad0 = e, this.fork = t, this.join = r, this.suspend = u;
      }(function () {
        return t.functorFiber;
      }, function () {
        return t.monadAff;
      }, t.forkAff, t.joinFiber, t.suspendAff);
      e.fork = function (n) {
        return n.fork;
      }, e.monadForkAff = r;
    }(e), function (n) {
      "use strict";

      n["Data.List"] = n["Data.List"] || {};
      var e,
          t = n["Data.List"],
          r = n["Data.List.Types"],
          u = (e = r.Nil.value, function (n) {
        var t,
            u = e,
            a = !1;

        function i(e, t) {
          if (t instanceof r.Nil) return a = !0, e;
          if (t instanceof r.Cons) return u = new r.Cons(t.value0, e), void (n = t.value1);
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [e.constructor.name, t.constructor.name]);
        }

        for (; !a;) {
          t = i(u, n);
        }

        return t;
      });
      t.null = function (n) {
        return n instanceof r.Nil;
      }, t.reverse = u;
    }(e), function (n) {
      "use strict";

      n["Data.CatQueue"] = n["Data.CatQueue"] || {};

      var e = n["Data.CatQueue"],
          t = n["Data.List"],
          r = n["Data.List.Types"],
          u = n["Data.Maybe"],
          a = n["Data.Tuple"],
          i = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          o = new i(r.Nil.value, r.Nil.value);

      e.empty = o, e.null = function (n) {
        return n.value0 instanceof r.Nil && n.value1 instanceof r.Nil;
      }, e.snoc = function (n) {
        return function (e) {
          return new i(n.value0, new r.Cons(e, n.value1));
        };
      }, e.uncons = function (n) {
        var e,
            o = !1;

        function f(e) {
          if (e.value0 instanceof r.Nil && e.value1 instanceof r.Nil) return o = !0, u.Nothing.value;

          if (!(e.value0 instanceof r.Nil)) {
            if (e.value0 instanceof r.Cons) return o = !0, new u.Just(new a.Tuple(e.value0.value0, new i(e.value0.value1, e.value1)));
            throw new Error("Failed pattern match at Data.CatQueue (line 83, column 1 - line 83, column 63): " + [e.constructor.name]);
          }

          n = new i(t.reverse(e.value1), r.Nil.value);
        }

        for (; !o;) {
          e = f(n);
        }

        return e;
      };
    }(e), function (n) {
      "use strict";

      n["Data.CatList"] = n["Data.CatList"] || {};

      var e = n["Data.CatList"],
          t = n["Data.CatQueue"],
          r = n["Data.List.Types"],
          u = n["Data.Maybe"],
          a = n["Data.Semigroup"],
          i = n["Data.Tuple"],
          o = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          f = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          l = function l(n) {
        return function (e) {
          if (n instanceof o) return e;
          if (e instanceof o) return n;
          if (n instanceof f) return new f(n.value0, t.snoc(n.value1)(e));
          throw new Error("Failed pattern match at Data.CatList (line 109, column 1 - line 109, column 54): " + [n.constructor.name, e.constructor.name]);
        };
      },
          c = o.value,
          v = l,
          s = new a.Semigroup(v);

      e.empty = c, e.snoc = function (n) {
        return function (e) {
          return v(n)(new f(e, t.empty));
        };
      }, e.uncons = function (n) {
        if (n instanceof o) return u.Nothing.value;
        if (n instanceof f) return new u.Just(new i.Tuple(n.value0, t.null(n.value1) ? o.value : (e = l, function (n) {
          return function (a) {
            var i,
                o = function o(n) {
              return function (e) {
                return function (t) {
                  var u,
                      a = n,
                      i = e,
                      o = !1;

                  function f(n, e, u) {
                    if (u instanceof r.Nil) return o = !0, e;
                    if (u instanceof r.Cons) return a = n, i = n(e)(u.value0), void (t = u.value1);
                    throw new Error("Failed pattern match at Data.CatList (line 125, column 3 - line 125, column 59): " + [n.constructor.name, e.constructor.name, u.constructor.name]);
                  }

                  for (; !o;) {
                    u = f(a, i, t);
                  }

                  return u;
                };
              };
            };

            return (i = a, function (a) {
              var f,
                  l = i,
                  c = !1;

              function v(i, f) {
                var v = t.uncons(i);
                if (v instanceof u.Nothing) return c = !0, o(function (n) {
                  return function (e) {
                    return e(n);
                  };
                })(n)(f);
                if (v instanceof u.Just) return l = v.value0.value1, void (a = new r.Cons(e(v.value0.value0), f));
                throw new Error("Failed pattern match at Data.CatList (line 121, column 14 - line 123, column 67): " + [v.constructor.name]);
              }

              for (; !c;) {
                f = v(l, a);
              }

              return f;
            })(r.Nil.value);
          };
        })(o.value)(n.value1)));
        var e;
        throw new Error("Failed pattern match at Data.CatList (line 100, column 1 - line 100, column 61): " + [n.constructor.name]);
      }, e.semigroupCatList = s;
    }(e), function (n) {
      "use strict";

      n["Control.Monad.Free"] = n["Control.Monad.Free"] || {};

      var e = n["Control.Monad.Free"],
          t = n["Control.Applicative"],
          r = n["Control.Apply"],
          u = n["Control.Bind"],
          a = n["Control.Monad"],
          i = n["Control.Monad.Rec.Class"],
          o = n["Data.CatList"],
          f = n["Data.Functor"],
          l = n["Data.Maybe"],
          c = n["Data.Semigroup"],
          v = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          s = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          d = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          p = function p(n) {
        return new v(n, o.empty);
      },
          m = new a.Monad(function () {
        return b;
      }, function () {
        return w;
      }),
          h = new f.Functor(function (n) {
        return function (e) {
          return u.bindFlipped(w)((r = t.pure(b), function (e) {
            return r(n(e));
          }))(e);
          var r;
        };
      }),
          w = new u.Bind(function () {
        return E;
      }, function (n) {
        return function (e) {
          return new v(n.value0, o.snoc(n.value1)(e));
        };
      }),
          E = new r.Apply(function () {
        return h;
      }, a.ap(m)),
          b = new t.Applicative(function () {
        return E;
      }, function (n) {
        return p(s.create(n));
      });

      e.liftF = function (n) {
        return p(new d(n, (e = t.pure(b), function (n) {
          return e(n);
        })));
        var e;
      }, e.foldFree = function (n) {
        return function (e) {
          return i.tailRecM(n)(function (r) {
            var u = function (n) {
              var e,
                  t = !1;

              function r(e) {
                var r = function r(n) {
                  return function (e) {
                    return new v(n.value0, c.append(o.semigroupCatList)(n.value1)(e));
                  };
                };

                if (e.value0 instanceof s) {
                  var u = o.uncons(e.value1);
                  if (u instanceof l.Nothing) return t = !0, new s(e.value0.value0);
                  if (u instanceof l.Just) return void (n = r(function (n) {
                    return n;
                  }(u.value0.value0)(e.value0.value0))(u.value0.value1));
                  throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [u.constructor.name]);
                }

                if (e.value0 instanceof d) return t = !0, new d(e.value0.value0, function (n) {
                  return r(e.value0.value1(n))(e.value1);
                });
                throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [e.value0.constructor.name]);
              }

              for (; !t;) {
                e = r(n);
              }

              return e;
            }(r);

            if (u instanceof s) return f.map(n.Monad0().Bind1().Apply0().Functor0())(i.Done.create)(t.pure(n.Monad0().Applicative0())(u.value0));
            if (u instanceof d) return f.map(n.Monad0().Bind1().Apply0().Functor0())(function (n) {
              return i.Loop.create(u.value1(n));
            })(e(u.value0));
            throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [u.constructor.name]);
          });
        };
      }, e.freeFunctor = h, e.freeBind = w, e.freeApplicative = b, e.freeMonad = m;
    }(e), function (n) {
      "use strict";

      n["Control.Monad.State.Class"] = n["Control.Monad.State.Class"] || {};

      var e = n["Control.Monad.State.Class"],
          t = n["Data.Tuple"],
          r = n["Data.Unit"],
          u = function u(n) {
        return n.state;
      };

      e.MonadState = function (n, e) {
        this.Monad0 = n, this.state = e;
      }, e.get = function (n) {
        return u(n)(function (n) {
          return new t.Tuple(n, n);
        });
      }, e.put = function (n) {
        return function (e) {
          return u(n)(function (n) {
            return new t.Tuple(r.unit, e);
          });
        };
      };
    }(e), function (n) {
      "use strict";

      n["Control.Parallel"] = n["Control.Parallel"] || {};
      var e = n["Control.Parallel"],
          t = n["Control.Category"],
          r = n["Control.Parallel.Class"],
          u = n["Data.Foldable"];

      e.parSequence_ = function (n) {
        return function (e) {
          return function (n) {
            return function (e) {
              return function (t) {
                var a,
                    i = r.sequential(n),
                    o = u.traverse_(n.Applicative1())(e)((a = r.parallel(n), function (n) {
                  return a(t(n));
                }));
                return function (n) {
                  return i(o(n));
                };
              };
            };
          }(n)(e)(t.identity(t.categoryFn));
        };
      };
    }(e), function (n) {
      "use strict";

      n["DOM.HTML.Indexed.ButtonType"] = n["DOM.HTML.Indexed.ButtonType"] || {};

      var e = n["DOM.HTML.Indexed.ButtonType"],
          t = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          r = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          u = function () {
        function n() {}

        return n.value = new n(), n;
      }();

      e.ButtonButton = t, e.renderButtonType = function (n) {
        if (n instanceof t) return "button";
        if (n instanceof r) return "submit";
        if (n instanceof u) return "reset";
        throw new Error("Failed pattern match at DOM.HTML.Indexed.ButtonType (line 9, column 20 - line 12, column 25): " + [n.constructor.name]);
      };
    }(e), function (n) {
      "use strict";

      (e["Data.Array"] = e["Data.Array"] || {}).length = function (n) {
        return n.length;
      };
    }(), function (n) {
      "use strict";

      n["Data.Array"] = n["Data.Array"] || {};
      var e = n["Data.Array"],
          t = n["Data.Array"];
      e.length = t.length;
    }(e), function (n) {
      "use strict";

      n["Data.Boolean"] = n["Data.Boolean"] || {};
      n["Data.Boolean"].otherwise = !0;
    }(e), function (n) {
      "use strict";

      n["Data.Coyoneda"] = n["Data.Coyoneda"] || {};

      var e = n["Data.Coyoneda"],
          t = n["Control.Category"],
          r = n["Data.Exists"],
          u = n["Data.Functor"],
          a = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          i = function i(n) {
        return function (e) {
          return r.mkExists(new a(n, e));
        };
      },
          o = new u.Functor(function (n) {
        return function (e) {
          return r.runExists(function (e) {
            return i(function (t) {
              return n(e.value0(t));
            })(e.value1);
          })(e);
        };
      }),
          f = i(t.identity(t.categoryFn));

      e.unCoyoneda = function (n) {
        return function (e) {
          return r.runExists(function (e) {
            return n(e.value0)(e.value1);
          })(e);
        };
      }, e.liftCoyoneda = f, e.functorCoyoneda = o;
    }(e), function (n) {
      "use strict";

      (e["Data.Function.Uncurried"] = e["Data.Function.Uncurried"] || {}).runFn4 = function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return n(e, t, r, u);
              };
            };
          };
        };
      };
    }(), function (n) {
      "use strict";

      n["Data.Function.Uncurried"] = n["Data.Function.Uncurried"] || {};
      var e = n["Data.Function.Uncurried"],
          t = n["Data.Function.Uncurried"];
      e.runFn4 = t.runFn4;
    }(e), function (n) {
      "use strict";

      n.boolConj = function (n) {
        return function (e) {
          return n && e;
        };
      }, n.boolDisj = function (n) {
        return function (e) {
          return n || e;
        };
      }, n.boolNot = function (n) {
        return !n;
      };
    }(e["Data.HeytingAlgebra"] = e["Data.HeytingAlgebra"] || {}), function (n) {
      "use strict";

      n["Data.HeytingAlgebra"] = n["Data.HeytingAlgebra"] || {};

      var e = n["Data.HeytingAlgebra"],
          t = n["Data.HeytingAlgebra"],
          r = function r(n, e, t, _r, u, a) {
        this.conj = n, this.disj = e, this.ff = t, this.implies = _r, this.not = u, this.tt = a;
      },
          u = function u(n) {
        return n.not;
      },
          a = function a(n) {
        return n.disj;
      },
          i = new r(t.boolConj, t.boolDisj, !1, function (n) {
        return function (e) {
          return a(i)(u(i)(n))(e);
        };
      }, t.boolNot, !0);

      e.not = u, e.heytingAlgebraBoolean = i, e.heytingAlgebraFunction = function (n) {
        return new r(function (e) {
          return function (t) {
            return function (r) {
              return n.conj(e(r))(t(r));
            };
          };
        }, function (e) {
          return function (t) {
            return function (r) {
              return a(n)(e(r))(t(r));
            };
          };
        }, function (e) {
          return n.ff;
        }, function (e) {
          return function (t) {
            return function (r) {
              return n.implies(e(r))(t(r));
            };
          };
        }, function (e) {
          return function (t) {
            return u(n)(e(t));
          };
        }, function (e) {
          return n.tt;
        });
      };
    }(e), function (n) {
      "use strict";

      var e = function e(n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return r < u ? n : r === u ? e : t;
              };
            };
          };
        };
      };

      n.ordIntImpl = e, n.ordStringImpl = e;
    }(e["Data.Ord"] = e["Data.Ord"] || {}), function (n) {
      "use strict";

      n["Data.Ordering"] = n["Data.Ordering"] || {};

      var e = n["Data.Ordering"],
          t = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          r = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          u = function () {
        function n() {}

        return n.value = new n(), n;
      }();

      e.LT = t, e.GT = r, e.EQ = u;
    }(e), function (n) {
      "use strict";

      n["Data.Ord"] = n["Data.Ord"] || {};

      var e = n["Data.Ord"],
          t = n["Data.Ord"],
          r = n["Data.Eq"],
          u = n["Data.Ordering"],
          a = function a(n, e) {
        this.Eq0 = n, this.compare = e;
      },
          i = new a(function () {
        return r.eqString;
      }, t.ordStringImpl(u.LT.value)(u.EQ.value)(u.GT.value)),
          o = new a(function () {
        return r.eqInt;
      }, t.ordIntImpl(u.LT.value)(u.EQ.value)(u.GT.value));

      e.compare = function (n) {
        return n.compare;
      }, e.ordInt = o, e.ordString = i;
    }(e), function (n) {
      "use strict";

      n["Data.Map.Internal"] = n["Data.Map.Internal"] || {};

      var e = n["Data.Map.Internal"],
          t = n["Control.Applicative"],
          r = n["Data.Foldable"],
          u = n["Data.Functor"],
          a = n["Data.List.Types"],
          i = n["Data.Maybe"],
          o = n["Data.Ord"],
          f = n["Data.Ordering"],
          l = n["Data.Semigroup"],
          c = n["Data.Tuple"],
          v = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          s = function () {
        function n(n, e, t, r) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return new n(e, t, r, u);
              };
            };
          };
        }, n;
      }(),
          d = function () {
        function n(n, e, t, r, u, a, i) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r, this.value4 = u, this.value5 = a, this.value6 = i;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return function (a) {
                  return function (i) {
                    return function (o) {
                      return new n(e, t, r, u, a, i, o);
                    };
                  };
                };
              };
            };
          };
        }, n;
      }(),
          p = function () {
        function n(n, e, t) {
          this.value0 = n, this.value1 = e, this.value2 = t;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return new n(e, t, r);
            };
          };
        }, n;
      }(),
          m = function () {
        function n(n, e, t) {
          this.value0 = n, this.value1 = e, this.value2 = t;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return new n(e, t, r);
            };
          };
        }, n;
      }(),
          h = function () {
        function n(n, e, t, r, u, a) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r, this.value4 = u, this.value5 = a;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return function (a) {
                  return function (i) {
                    return new n(e, t, r, u, a, i);
                  };
                };
              };
            };
          };
        }, n;
      }(),
          w = function () {
        function n(n, e, t, r, u, a) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r, this.value4 = u, this.value5 = a;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return function (a) {
                  return function (i) {
                    return new n(e, t, r, u, a, i);
                  };
                };
              };
            };
          };
        }, n;
      }(),
          E = function () {
        function n(n, e, t, r, u, a) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r, this.value4 = u, this.value5 = a;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return function (a) {
                  return function (i) {
                    return new n(e, t, r, u, a, i);
                  };
                };
              };
            };
          };
        }, n;
      }(),
          b = function () {
        function n(n, e, t, r) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return new n(e, t, r, u);
              };
            };
          };
        }, n;
      }(),
          y = function n(e) {
        if (e instanceof v) return a.Nil.value;
        if (e instanceof s) return l.append(a.semigroupList)(n(e.value0))(l.append(a.semigroupList)(t.pure(a.applicativeList)(e.value2))(n(e.value3)));
        if (e instanceof d) return l.append(a.semigroupList)(n(e.value0))(l.append(a.semigroupList)(t.pure(a.applicativeList)(e.value2))(l.append(a.semigroupList)(n(e.value3))(l.append(a.semigroupList)(t.pure(a.applicativeList)(e.value5))(n(e.value6)))));
        throw new Error("Failed pattern match at Data.Map.Internal (line 612, column 1 - line 612, column 40): " + [e.constructor.name]);
      },
          g = function g(n) {
        return function (e) {
          var t = o.compare(n);
          return function (n) {
            var r,
                u = !1;

            function a(r) {
              if (r instanceof v) return u = !0, i.Nothing.value;

              if (r instanceof s) {
                var a = t(e)(r.value1);
                return a instanceof f.EQ ? (u = !0, new i.Just(r.value2)) : a instanceof f.LT ? void (n = r.value0) : void (n = r.value3);
              }

              if (r instanceof d) {
                var o = t(e)(r.value1);
                if (o instanceof f.EQ) return u = !0, new i.Just(r.value2);
                var l = t(e)(r.value4);
                return l instanceof f.EQ ? (u = !0, new i.Just(r.value5)) : o instanceof f.LT ? void (n = r.value0) : l instanceof f.GT ? void (n = r.value6) : void (n = r.value3);
              }

              throw new Error("Failed pattern match at Data.Map.Internal (line 200, column 5 - line 200, column 22): " + [r.constructor.name]);
            }

            for (; !u;) {
              r = a(n);
            }

            return r;
          };
        };
      },
          A = new u.Functor(function (n) {
        return function (e) {
          if (e instanceof v) return v.value;
          if (e instanceof s) return new s(u.map(A)(n)(e.value0), e.value1, n(e.value2), u.map(A)(n)(e.value3));
          if (e instanceof d) return new d(u.map(A)(n)(e.value0), e.value1, n(e.value2), u.map(A)(n)(e.value3), e.value4, n(e.value5), u.map(A)(n)(e.value6));
          throw new Error("Failed pattern match at Data.Map.Internal (line 96, column 1 - line 99, column 110): " + [n.constructor.name, e.constructor.name]);
        };
      }),
          D = function D(n) {
        return function (e) {
          return function (t) {
            var r,
                u = n,
                i = e,
                o = !1;

            function f(n, e, r) {
              if (e instanceof a.Nil) return o = !0, r;

              if (e instanceof a.Cons) {
                if (e.value0 instanceof p) return u = n, i = e.value1, void (t = new s(r, e.value0.value0, e.value0.value1, e.value0.value2));
                if (e.value0 instanceof m) return u = n, i = e.value1, void (t = new s(e.value0.value0, e.value0.value1, e.value0.value2, r));
                if (e.value0 instanceof h) return u = n, i = e.value1, void (t = new d(r, e.value0.value0, e.value0.value1, e.value0.value2, e.value0.value3, e.value0.value4, e.value0.value5));
                if (e.value0 instanceof w) return u = n, i = e.value1, void (t = new d(e.value0.value0, e.value0.value1, e.value0.value2, r, e.value0.value3, e.value0.value4, e.value0.value5));
                if (e.value0 instanceof E) return u = n, i = e.value1, void (t = new d(e.value0.value0, e.value0.value1, e.value0.value2, e.value0.value3, e.value0.value4, e.value0.value5, r));
                throw new Error("Failed pattern match at Data.Map.Internal (line 418, column 3 - line 423, column 88): " + [e.value0.constructor.name]);
              }

              throw new Error("Failed pattern match at Data.Map.Internal (line 415, column 1 - line 415, column 80): " + [e.constructor.name, r.constructor.name]);
            }

            for (; !o;) {
              r = f(u, i, t);
            }

            return r;
          };
        };
      },
          C = function C(n) {
        return function (e) {
          return function (t) {
            var r,
                u = function u(e) {
              return function (t) {
                var r,
                    u = e,
                    i = !1;

                function o(e, r) {
                  if (e instanceof a.Nil) return i = !0, new s(r.value0, r.value1, r.value2, r.value3);

                  if (e instanceof a.Cons) {
                    if (e.value0 instanceof p) return i = !0, D(n)(e.value1)(new d(r.value0, r.value1, r.value2, r.value3, e.value0.value0, e.value0.value1, e.value0.value2));
                    if (e.value0 instanceof m) return i = !0, D(n)(e.value1)(new d(e.value0.value0, e.value0.value1, e.value0.value2, r.value0, r.value1, r.value2, r.value3));
                    if (e.value0 instanceof h) return u = e.value1, void (t = new b(new s(r.value0, r.value1, r.value2, r.value3), e.value0.value0, e.value0.value1, new s(e.value0.value2, e.value0.value3, e.value0.value4, e.value0.value5)));
                    if (e.value0 instanceof w) return u = e.value1, void (t = new b(new s(e.value0.value0, e.value0.value1, e.value0.value2, r.value0), r.value1, r.value2, new s(r.value3, e.value0.value3, e.value0.value4, e.value0.value5)));
                    if (e.value0 instanceof E) return u = e.value1, void (t = new b(new s(e.value0.value0, e.value0.value1, e.value0.value2, e.value0.value3), e.value0.value4, e.value0.value5, new s(r.value0, r.value1, r.value2, r.value3)));
                    throw new Error("Failed pattern match at Data.Map.Internal (line 454, column 5 - line 459, column 108): " + [e.value0.constructor.name, r.constructor.name]);
                  }

                  throw new Error("Failed pattern match at Data.Map.Internal (line 451, column 3 - line 451, column 56): " + [e.constructor.name, r.constructor.name]);
                }

                for (; !i;) {
                  r = o(u, t);
                }

                return r;
              };
            },
                i = o.compare(n);

            return r = a.Nil.value, function (o) {
              var l,
                  c = r,
                  y = !1;

              function g(r, l) {
                if (l instanceof v) return y = !0, u(r)(new b(v.value, e, t, v.value));

                if (l instanceof s) {
                  var g = i(e)(l.value1);
                  return g instanceof f.EQ ? (y = !0, D(n)(r)(new s(l.value0, e, t, l.value3))) : g instanceof f.LT ? (c = new a.Cons(new p(l.value1, l.value2, l.value3), r), void (o = l.value0)) : (c = new a.Cons(new m(l.value0, l.value1, l.value2), r), void (o = l.value3));
                }

                if (l instanceof d) {
                  var A = i(e)(l.value1);
                  if (A instanceof f.EQ) return y = !0, D(n)(r)(new d(l.value0, e, t, l.value3, l.value4, l.value5, l.value6));
                  var C = i(e)(l.value4);
                  return C instanceof f.EQ ? (y = !0, D(n)(r)(new d(l.value0, l.value1, l.value2, l.value3, e, t, l.value6))) : A instanceof f.LT ? (c = new a.Cons(new h(l.value1, l.value2, l.value3, l.value4, l.value5, l.value6), r), void (o = l.value0)) : A instanceof f.GT && C instanceof f.LT ? (c = new a.Cons(new w(l.value0, l.value1, l.value2, l.value4, l.value5, l.value6), r), void (o = l.value3)) : (c = new a.Cons(new E(l.value0, l.value1, l.value2, l.value3, l.value4, l.value5), r), void (o = l.value6));
                }

                throw new Error("Failed pattern match at Data.Map.Internal (line 434, column 3 - line 434, column 55): " + [r.constructor.name, l.constructor.name]);
              }

              for (; !y;) {
                l = g(c, o);
              }

              return l;
            };
          };
        };
      },
          M = new r.Foldable(function (n) {
        return function (e) {
          return function (t) {
            return r.foldMap(a.foldableList)(n)(e)(y(t));
          };
        };
      }, function (n) {
        return function (e) {
          return function (t) {
            return r.foldl(a.foldableList)(n)(e)(y(t));
          };
        };
      }, function (n) {
        return function (e) {
          return function (t) {
            return r.foldr(a.foldableList)(n)(e)(y(t));
          };
        };
      }),
          F = v.value,
          T = function T(n) {
        return function (e) {
          return function (t) {
            return i.maybe(t)(c.snd)(function (n) {
              return function (e) {
                var t,
                    r = function r(e) {
                  return function (t) {
                    var r,
                        u = e,
                        i = !1;

                    function o(e, r) {
                      if (e instanceof a.Nil) return i = !0, r;

                      if (e instanceof a.Cons) {
                        if (e.value0 instanceof p && e.value0.value2 instanceof v && r instanceof v) return i = !0, D(n)(e.value1)(new s(v.value, e.value0.value0, e.value0.value1, v.value));
                        if (e.value0 instanceof m && e.value0.value0 instanceof v && r instanceof v) return i = !0, D(n)(e.value1)(new s(v.value, e.value0.value1, e.value0.value2, v.value));
                        if (e.value0 instanceof p && e.value0.value2 instanceof s) return u = e.value1, void (t = new d(r, e.value0.value0, e.value0.value1, e.value0.value2.value0, e.value0.value2.value1, e.value0.value2.value2, e.value0.value2.value3));
                        if (e.value0 instanceof m && e.value0.value0 instanceof s) return u = e.value1, void (t = new d(e.value0.value0.value0, e.value0.value0.value1, e.value0.value0.value2, e.value0.value0.value3, e.value0.value1, e.value0.value2, r));
                        if (e.value0 instanceof p && e.value0.value2 instanceof d) return i = !0, D(n)(e.value1)(new s(new s(r, e.value0.value0, e.value0.value1, e.value0.value2.value0), e.value0.value2.value1, e.value0.value2.value2, new s(e.value0.value2.value3, e.value0.value2.value4, e.value0.value2.value5, e.value0.value2.value6)));
                        if (e.value0 instanceof m && e.value0.value0 instanceof d) return i = !0, D(n)(e.value1)(new s(new s(e.value0.value0.value0, e.value0.value0.value1, e.value0.value0.value2, e.value0.value0.value3), e.value0.value0.value4, e.value0.value0.value5, new s(e.value0.value0.value6, e.value0.value1, e.value0.value2, r)));
                        if (e.value0 instanceof h && e.value0.value2 instanceof v && e.value0.value5 instanceof v && r instanceof v) return i = !0, D(n)(e.value1)(new d(v.value, e.value0.value0, e.value0.value1, v.value, e.value0.value3, e.value0.value4, v.value));
                        if (e.value0 instanceof w && e.value0.value0 instanceof v && e.value0.value5 instanceof v && r instanceof v) return i = !0, D(n)(e.value1)(new d(v.value, e.value0.value1, e.value0.value2, v.value, e.value0.value3, e.value0.value4, v.value));
                        if (e.value0 instanceof E && e.value0.value0 instanceof v && e.value0.value3 instanceof v && r instanceof v) return i = !0, D(n)(e.value1)(new d(v.value, e.value0.value1, e.value0.value2, v.value, e.value0.value4, e.value0.value5, v.value));
                        if (e.value0 instanceof h && e.value0.value2 instanceof s) return i = !0, D(n)(e.value1)(new s(new d(r, e.value0.value0, e.value0.value1, e.value0.value2.value0, e.value0.value2.value1, e.value0.value2.value2, e.value0.value2.value3), e.value0.value3, e.value0.value4, e.value0.value5));
                        if (e.value0 instanceof w && e.value0.value0 instanceof s) return i = !0, D(n)(e.value1)(new s(new d(e.value0.value0.value0, e.value0.value0.value1, e.value0.value0.value2, e.value0.value0.value3, e.value0.value1, e.value0.value2, r), e.value0.value3, e.value0.value4, e.value0.value5));
                        if (e.value0 instanceof w && e.value0.value5 instanceof s) return i = !0, D(n)(e.value1)(new s(e.value0.value0, e.value0.value1, e.value0.value2, new d(r, e.value0.value3, e.value0.value4, e.value0.value5.value0, e.value0.value5.value1, e.value0.value5.value2, e.value0.value5.value3)));
                        if (e.value0 instanceof E && e.value0.value3 instanceof s) return i = !0, D(n)(e.value1)(new s(e.value0.value0, e.value0.value1, e.value0.value2, new d(e.value0.value3.value0, e.value0.value3.value1, e.value0.value3.value2, e.value0.value3.value3, e.value0.value4, e.value0.value5, r)));
                        if (e.value0 instanceof h && e.value0.value2 instanceof d) return i = !0, D(n)(e.value1)(new d(new s(r, e.value0.value0, e.value0.value1, e.value0.value2.value0), e.value0.value2.value1, e.value0.value2.value2, new s(e.value0.value2.value3, e.value0.value2.value4, e.value0.value2.value5, e.value0.value2.value6), e.value0.value3, e.value0.value4, e.value0.value5));
                        if (e.value0 instanceof w && e.value0.value0 instanceof d) return i = !0, D(n)(e.value1)(new d(new s(e.value0.value0.value0, e.value0.value0.value1, e.value0.value0.value2, e.value0.value0.value3), e.value0.value0.value4, e.value0.value0.value5, new s(e.value0.value0.value6, e.value0.value1, e.value0.value2, r), e.value0.value3, e.value0.value4, e.value0.value5));
                        if (e.value0 instanceof w && e.value0.value5 instanceof d) return i = !0, D(n)(e.value1)(new d(e.value0.value0, e.value0.value1, e.value0.value2, new s(r, e.value0.value3, e.value0.value4, e.value0.value5.value0), e.value0.value5.value1, e.value0.value5.value2, new s(e.value0.value5.value3, e.value0.value5.value4, e.value0.value5.value5, e.value0.value5.value6)));
                        if (e.value0 instanceof E && e.value0.value3 instanceof d) return i = !0, D(n)(e.value1)(new d(e.value0.value0, e.value0.value1, e.value0.value2, new s(e.value0.value3.value0, e.value0.value3.value1, e.value0.value3.value2, e.value0.value3.value3), e.value0.value3.value4, e.value0.value3.value5, new s(e.value0.value3.value6, e.value0.value4, e.value0.value5, r)));
                        throw new Error("Failed pattern match at Data.Map.Internal (line 511, column 9 - line 528, column 136): " + [e.value0.constructor.name, r.constructor.name]);
                      }

                      throw new Error("Failed pattern match at Data.Map.Internal (line 508, column 5 - line 528, column 136): " + [e.constructor.name]);
                    }

                    for (; !i;) {
                      r = o(u, t);
                    }

                    return r;
                  };
                },
                    u = function u(n) {
                  return function (e) {
                    var t,
                        u = n,
                        i = !1;

                    function o(n, t) {
                      if (t instanceof s && t.value0 instanceof v && t.value3 instanceof v) return i = !0, r(n)(v.value);
                      if (t instanceof s) return u = new a.Cons(new m(t.value0, t.value1, t.value2), n), void (e = t.value3);
                      if (t instanceof d && t.value0 instanceof v && t.value3 instanceof v && t.value6 instanceof v) return i = !0, r(new a.Cons(new m(v.value, t.value1, t.value2), n))(v.value);
                      if (t instanceof d) return u = new a.Cons(new E(t.value0, t.value1, t.value2, t.value3, t.value4, t.value5), n), void (e = t.value6);
                      throw new Error("Failed pattern match at Data.Map.Internal (line 540, column 5 - line 544, column 107): " + [t.constructor.name]);
                    }

                    for (; !i;) {
                      t = o(u, e);
                    }

                    return t;
                  };
                },
                    l = function l(n) {
                  var e,
                      t = !1;

                  function r(e) {
                    if (e instanceof s && e.value3 instanceof v) return t = !0, {
                      key: e.value1,
                      value: e.value2
                    };
                    if (e instanceof s) n = e.value3;else {
                      if (e instanceof d && e.value6 instanceof v) return t = !0, {
                        key: e.value4,
                        value: e.value5
                      };
                      if (!(e instanceof d)) throw new Error("Failed pattern match at Data.Map.Internal (line 531, column 33 - line 535, column 45): " + [e.constructor.name]);
                      n = e.value6;
                    }
                  }

                  for (; !t;) {
                    e = r(n);
                  }

                  return e;
                },
                    b = o.compare(n);

                return t = a.Nil.value, function (o) {
                  var y,
                      g = t,
                      A = !1;

                  function C(t, y) {
                    if (y instanceof v) return A = !0, i.Nothing.value;

                    if (y instanceof s) {
                      var C = b(e)(y.value1);
                      if (y.value3 instanceof v && C instanceof f.EQ) return A = !0, new i.Just(new c.Tuple(y.value2, r(t)(v.value)));

                      if (C instanceof f.EQ) {
                        var M = l(y.value0);
                        return A = !0, new i.Just(new c.Tuple(y.value2, u(new a.Cons(new p(M.key, M.value, y.value3), t))(y.value0)));
                      }

                      return C instanceof f.LT ? (g = new a.Cons(new p(y.value1, y.value2, y.value3), t), void (o = y.value0)) : (g = new a.Cons(new m(y.value0, y.value1, y.value2), t), void (o = y.value3));
                    }

                    if (y instanceof d) {
                      var F = y.value0 instanceof v && y.value3 instanceof v && y.value6 instanceof v,
                          T = (C = b(e)(y.value4), b(e)(y.value1));
                      return F && T instanceof f.EQ ? (A = !0, new i.Just(new c.Tuple(y.value2, D(n)(t)(new s(v.value, y.value4, y.value5, v.value))))) : F && C instanceof f.EQ ? (A = !0, new i.Just(new c.Tuple(y.value5, D(n)(t)(new s(v.value, y.value1, y.value2, v.value))))) : T instanceof f.EQ ? (M = l(y.value0), A = !0, new i.Just(new c.Tuple(y.value2, u(new a.Cons(new h(M.key, M.value, y.value3, y.value4, y.value5, y.value6), t))(y.value0)))) : C instanceof f.EQ ? (M = l(y.value3), A = !0, new i.Just(new c.Tuple(y.value5, u(new a.Cons(new w(y.value0, y.value1, y.value2, M.key, M.value, y.value6), t))(y.value3)))) : T instanceof f.LT ? (g = new a.Cons(new h(y.value1, y.value2, y.value3, y.value4, y.value5, y.value6), t), void (o = y.value0)) : T instanceof f.GT && C instanceof f.LT ? (g = new a.Cons(new w(y.value0, y.value1, y.value2, y.value4, y.value5, y.value6), t), void (o = y.value3)) : (g = new a.Cons(new E(y.value0, y.value1, y.value2, y.value3, y.value4, y.value5), t), void (o = y.value6));
                    }

                    throw new Error("Failed pattern match at Data.Map.Internal (line 481, column 34 - line 504, column 80): " + [y.constructor.name]);
                  }

                  for (; !A;) {
                    y = C(g, o);
                  }

                  return y;
                };
              };
            }(n)(e)(t));
          };
        };
      };

      e.empty = F, e.insert = C, e.lookup = g, e.delete = T, e.member = function (n) {
        return function (e) {
          return function (t) {
            return i.isJust(g(n)(e)(t));
          };
        };
      }, e.alter = function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              var u = e(g(n)(t)(r));
              if (u instanceof i.Nothing) return T(n)(t)(r);
              if (u instanceof i.Just) return C(n)(t)(u.value0)(r);
              throw new Error("Failed pattern match at Data.Map.Internal (line 549, column 15 - line 551, column 25): " + [u.constructor.name]);
            };
          };
        };
      }, e.functorMap = A, e.foldableMap = M;
    }(e), function (n) {
      "use strict";

      n.null = null, n.nullable = function (n, e, t) {
        return null == n ? e : t(n);
      }, n.notNull = function (n) {
        return n;
      };
    }(e["Data.Nullable"] = e["Data.Nullable"] || {}), function (n) {
      "use strict";

      n["Data.Nullable"] = n["Data.Nullable"] || {};
      var e = n["Data.Nullable"],
          t = n["Data.Nullable"],
          r = n["Data.Maybe"],
          u = r.maybe(t.null)(t.notNull);
      e.toMaybe = function (n) {
        return t.nullable(n, r.Nothing.value, r.Just.create);
      }, e.toNullable = u, e.null = t.null;
    }(e), function (n) {
      "use strict";

      var e = function () {
        function n() {
          this.head = null, this.last = null, this.size = 0;
        }

        function e(n, e) {
          this.queue = n, this.value = e, this.next = null, this.prev = null;
        }

        function t(e) {
          this.draining = !1, this.error = null, this.value = e, this.takes = new n(), this.reads = new n(), this.puts = new n();
        }

        var r = {};

        function u(n) {
          try {
            n();
          } catch (e) {
            setTimeout(function () {
              throw e;
            }, 0);
          }
        }

        function a(n) {
          var e;

          switch (n.size) {
            case 0:
              return null;

            case 1:
              e = n.head, n.head = null;
              break;

            case 2:
              e = n.last, n.head.next = null, n.last = null;
              break;

            default:
              e = n.last, n.last = e.prev, n.last.next = null;
          }

          return e.prev = null, e.queue = null, n.size--, e.value;
        }

        function i(n) {
          var e;

          switch (n.size) {
            case 0:
              return null;

            case 1:
              e = n.head, n.head = null;
              break;

            case 2:
              e = n.head, n.last.prev = null, n.head = n.last, n.last = null;
              break;

            default:
              e = n.head, n.head = e.next, n.head.prev = null;
          }

          return e.next = null, e.queue = null, n.size--, e.value;
        }

        return t.EMPTY = r, t.putLast = function (n, t) {
          var r = new e(n, t);

          switch (n.size) {
            case 0:
              n.head = r;
              break;

            case 1:
              r.prev = n.head, n.head.next = r, n.last = r;
              break;

            default:
              r.prev = n.last, n.last.next = r, n.last = r;
          }

          return n.size++, r;
        }, t.takeLast = a, t.takeHead = i, t.deleteCell = function (n) {
          null !== n.queue && (n.queue.last !== n ? n.queue.head !== n ? (n.prev && (n.prev.next = n.next), n.next && (n.next.prev = n.prev), n.queue.size--, n.queue = null, n.value = null, n.next = null, n.prev = null) : i(n.queue) : a(n.queue));
        }, t.drainVar = function (n, e) {
          if (!e.draining) {
            var t,
                a,
                o,
                f,
                l,
                c = e.puts,
                v = e.takes,
                s = e.reads;

            for (e.draining = !0;;) {
              if (t = null, a = null, o = null, f = e.value, l = s.size, null !== e.error) {
                for (f = n.left(e.error); t = i(c);) {
                  u(t.cb(f));
                }

                for (; a = i(s);) {
                  u(a(f));
                }

                for (; o = i(v);) {
                  u(o(f));
                }

                break;
              }

              if (f === r && (t = i(c)) && (e.value = f = t.value), f !== r) {
                for (o = i(v); l-- && (a = i(s));) {
                  u(a(n.right(f)));
                }

                null !== o && (e.value = r, u(o(n.right(f))));
              }

              if (null !== t && u(t.cb(n.right(void 0))), e.value === r && 0 === c.size || e.value !== r && 0 === v.size) break;
            }

            e.draining = !1;
          }
        }, t;
      }();

      n.empty = function () {
        return new e(e.EMPTY);
      }, n._killVar = function (n, t, r) {
        return function () {
          null === r.error && (r.error = t, r.value = e.EMPTY, e.drainVar(n, r));
        };
      }, n._putVar = function (n, t, r, u) {
        return function () {
          var a = e.putLast(r.puts, {
            cb: u,
            value: t
          });
          return e.drainVar(n, r), function () {
            e.deleteCell(a);
          };
        };
      }, n._takeVar = function (n, t, r) {
        return function () {
          var u = e.putLast(t.takes, r);
          return e.drainVar(n, t), function () {
            e.deleteCell(u);
          };
        };
      };
    }(e["Effect.AVar"] = e["Effect.AVar"] || {}), function (n) {
      "use strict";

      n["Effect.AVar"] = n["Effect.AVar"] || {};

      var e = n["Effect.AVar"],
          t = n["Effect.AVar"],
          r = n["Data.Either"],
          u = n["Data.Maybe"],
          a = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          i = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          o = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          f = {
        left: r.Left.create,
        right: r.Right.create,
        nothing: u.Nothing.value,
        just: u.Just.create,
        killed: a.create,
        filled: i.create,
        empty: o.value
      };

      e.take = function (n) {
        return function (e) {
          return t._takeVar(f, n, e);
        };
      }, e.put = function (n) {
        return function (e) {
          return function (r) {
            return t._putVar(f, n, e, r);
          };
        };
      }, e.kill = function (n) {
        return function (e) {
          return t._killVar(f, n, e);
        };
      }, e.empty = t.empty;
    }(e), function (n) {
      "use strict";

      n["Effect.Aff.AVar"] = n["Effect.Aff.AVar"] || {};
      var e = n["Effect.Aff.AVar"],
          t = n["Effect.AVar"],
          r = n["Effect.Aff"],
          u = n["Effect.Class"],
          a = u.liftEffect(r.monadEffectAff)(t.empty);
      e.empty = a, e.take = function (n) {
        return r.makeAff(function (e) {
          return function () {
            var u = t.take(n)(e)();
            return r.effectCanceler(u);
          };
        });
      }, e.put = function (n) {
        return function (e) {
          return r.makeAff(function (u) {
            return function () {
              var a = t.put(n)(e)(u)();
              return r.effectCanceler(a);
            };
          });
        };
      }, e.kill = function (n) {
        var e = u.liftEffect(r.monadEffectAff),
            a = t.kill(n);
        return function (n) {
          return e(a(n));
        };
      };
    }(e), function (n) {
      "use strict";

      (e["Effect.Console"] = e["Effect.Console"] || {}).warn = function (n) {
        return function () {
          return console.warn(n), {};
        };
      };
    }(), function (n) {
      "use strict";

      n["Effect.Console"] = n["Effect.Console"] || {};
      var e = n["Effect.Console"],
          t = n["Effect.Console"];
      e.warn = t.warn;
    }(e), function (n) {
      "use strict";

      n.error = function (n) {
        return new Error(n);
      }, n.throwException = function (n) {
        return function () {
          throw n;
        };
      };
    }(e["Effect.Exception"] = e["Effect.Exception"] || {}), function (n) {
      "use strict";

      n["Effect.Exception"] = n["Effect.Exception"] || {};
      var e = n["Effect.Exception"],
          t = n["Effect.Exception"];
      e.throw = function (n) {
        return t.throwException(t.error(n));
      }, e.error = t.error, e.throwException = t.throwException;
    }(e), function (t) {
      "use strict";

      (e["Foreign"] = e["Foreign"] || {}).typeOf = function (e) {
        return n(e);
      };
    }(), function (n) {
      "use strict";

      n.Foreign = n.Foreign || {};
      var e = n.Foreign,
          t = n.Foreign;
      e.typeOf = t.typeOf;
    }(e), function (n) {
      "use strict";

      (e["Foreign.Object"] = e["Foreign.Object"] || {})._lookup = function (n, e, t, r) {
        return t in r ? e(r[t]) : n;
      };
    }(), function (n) {
      "use strict";

      n["Foreign.Object"] = n["Foreign.Object"] || {};
      var e = n["Foreign.Object"],
          t = n["Foreign.Object"],
          r = n["Data.Function.Uncurried"],
          u = n["Data.Maybe"],
          a = r.runFn4(t._lookup)(u.Nothing.value)(u.Just.create);
      e.lookup = a;
    }(e), function (n) {
      "use strict";

      (e["Foreign.Object.ST"] = e["Foreign.Object.ST"] || {}).new = function () {
        return {};
      };
    }(), function (n) {
      "use strict";

      n["Foreign.Object.ST"] = n["Foreign.Object.ST"] || {};
      var e = n["Foreign.Object.ST"],
          t = n["Foreign.Object.ST"];
      e.new = t.new;
    }(e), function (n) {
      "use strict";

      n["Halogen.Data.Slot"] = n["Halogen.Data.Slot"] || {};
      var e = n["Halogen.Data.Slot"],
          t = n["Data.Foldable"],
          r = n["Data.Map.Internal"],
          u = r.empty;
      e.empty = u, e.foreachSlot = function (n) {
        return function (e) {
          return function (u) {
            return t.traverse_(n)(r.foldableMap)(function (n) {
              return u(n);
            })(e);
          };
        };
      };
    }(e), function (n) {
      "use strict";

      n["Halogen.Aff.Driver.State"] = n["Halogen.Aff.Driver.State"] || {};
      var e = n["Halogen.Aff.Driver.State"],
          t = n["Data.Foldable"],
          r = n["Data.List.Types"],
          u = n["Data.Map.Internal"],
          a = n["Data.Maybe"],
          i = n["Effect.Ref"],
          o = n["Halogen.Data.Slot"],
          f = n["Unsafe.Coerce"],
          l = f.unsafeCoerce,
          c = f.unsafeCoerce,
          v = f.unsafeCoerce,
          s = f.unsafeCoerce;
      e.mapDriverState = function (n) {
        return function (e) {
          return n(e);
        };
      }, e.unDriverStateX = c, e.renderStateX = function (n) {
        return function (n) {
          return c(function (e) {
            return v(n(e.rendering));
          });
        };
      }, e.renderStateX_ = function (n) {
        return function (e) {
          return c(function (r) {
            return t.traverse_(n)(t.foldableMaybe)(e)(r.rendering);
          });
        };
      }, e.unRenderStateX = l, e.initDriverState = function (n) {
        return function (e) {
          return function (t) {
            return function (f) {
              return function () {
                var l = i.new({})(),
                    c = i.new(o.empty)(),
                    v = i.new(o.empty)(),
                    d = i.new(t)(),
                    p = i.new(new a.Just(r.Nil.value))(),
                    m = i.new(new a.Just(r.Nil.value))(),
                    h = i.new(a.Nothing.value)(),
                    w = i.new(1)(),
                    E = i.new(new a.Just(u.empty))(),
                    b = i.new(u.empty)(),
                    y = {
                  component: n,
                  state: n.initialState(e),
                  refs: u.empty,
                  children: o.empty,
                  childrenIn: c,
                  childrenOut: v,
                  selfRef: l,
                  handlerRef: d,
                  pendingQueries: p,
                  pendingOuts: m,
                  pendingHandlers: h,
                  rendering: a.Nothing.value,
                  fresh: w,
                  subscriptions: E,
                  forks: b,
                  lifecycleHandlers: f
                };
                return i.write(y)(l)(), s(l);
              };
            };
          };
        };
      };
    }(e), function (n) {
      "use strict";

      n["Halogen.Query.ChildQuery"] = n["Halogen.Query.ChildQuery"] || {};
      var e = n["Halogen.Query.ChildQuery"],
          t = n["Unsafe.Coerce"].unsafeCoerce;
      e.unChildQueryBox = t;
    }(e), function (n) {
      "use strict";

      n["Halogen.Query.EventSource"] = n["Halogen.Query.EventSource"] || {};

      n["Halogen.Query.EventSource"].finalize = function (n) {
        return n;
      };
    }(e), function (n) {
      "use strict";

      n["Halogen.Query.HalogenM"] = n["Halogen.Query.HalogenM"] || {};

      var e = n["Halogen.Query.HalogenM"],
          t = n["Control.Monad.Free"],
          r = n["Control.Monad.State.Class"],
          u = n["Data.Ord"],
          a = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          i = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          o = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          f = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          l = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          c = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          v = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          s = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          d = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          p = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          m = u.ordInt,
          h = u.ordInt,
          w = t.freeMonad,
          E = new r.MonadState(function () {
        return w;
      }, function (n) {
        return t.liftF(a.create(n));
      }),
          b = t.freeFunctor,
          y = t.freeBind,
          g = t.freeApplicative;

      e.State = a, e.Subscribe = i, e.Unsubscribe = o, e.Lift = f, e.ChildQuery = l, e.Raise = c, e.Par = v, e.Fork = s, e.Kill = d, e.GetRef = p, e.SubscriptionId = function (n) {
        return n;
      }, e.ForkId = function (n) {
        return n;
      }, e.functorHalogenM = b, e.applicativeHalogenM = g, e.bindHalogenM = y, e.monadStateHalogenM = E, e.ordSubscriptionId = m, e.ordForkId = h;
    }(e), function (n) {
      "use strict";

      n["Halogen.Query.HalogenQ"] = n["Halogen.Query.HalogenQ"] || {};

      var e = n["Halogen.Query.HalogenQ"],
          t = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          r = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          u = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          a = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          i = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }();

      e.Initialize = t, e.Finalize = r, e.Receive = u, e.Action = a, e.Query = i;
    }(e), function (n) {
      "use strict";

      n["Halogen.Query.Input"] = n["Halogen.Query.Input"] || {};

      var e = n["Halogen.Query.Input"],
          t = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          r = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }();

      e.RefUpdate = t, e.Action = r;
    }(e), function (n) {
      "use strict";

      (e["Unsafe.Reference"] = e["Unsafe.Reference"] || {}).reallyUnsafeRefEq = function (n) {
        return function (e) {
          return n === e;
        };
      };
    }(), function (n) {
      "use strict";

      n["Unsafe.Reference"] = n["Unsafe.Reference"] || {};
      var e = n["Unsafe.Reference"],
          t = n["Unsafe.Reference"].reallyUnsafeRefEq;
      e.unsafeRefEq = t;
    }(e), function (n) {
      "use strict";

      n["Halogen.Aff.Driver.Eval"] = n["Halogen.Aff.Driver.Eval"] || {};

      var e = n["Halogen.Aff.Driver.Eval"],
          t = n["Control.Applicative"],
          r = n["Control.Applicative.Free"],
          u = n["Control.Bind"],
          a = n["Control.Coroutine"],
          i = n["Control.Monad"],
          o = n["Control.Monad.Fork.Class"],
          f = n["Control.Monad.Free"],
          l = n["Control.Monad.Free.Trans"],
          c = n["Control.Monad.Trans.Class"],
          v = n["Control.Parallel"],
          s = n["Control.Parallel.Class"],
          d = n["Data.Boolean"],
          p = n["Data.Coyoneda"],
          m = n["Data.Eq"],
          h = n["Data.Foldable"],
          w = n["Data.Function"],
          E = n["Data.Functor"],
          b = n["Data.List.Types"],
          y = n["Data.Map.Internal"],
          g = n["Data.Maybe"],
          A = n["Data.Ord"],
          D = n["Data.Unit"],
          C = n.Effect,
          M = n["Effect.Aff"],
          F = n["Effect.Class"],
          T = n["Effect.Exception"],
          H = n["Effect.Ref"],
          L = n["Halogen.Aff.Driver.State"],
          S = n["Halogen.Query.ChildQuery"],
          k = n["Halogen.Query.EventSource"],
          _ = n["Halogen.Query.HalogenM"],
          N = n["Halogen.Query.HalogenQ"],
          R = n["Halogen.Query.Input"],
          I = n["Unsafe.Reference"],
          x = function x(n) {
        return function (e) {
          return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n)))(function (t) {
            if (t instanceof g.Nothing) return e;
            if (t instanceof g.Just) return F.liftEffect(M.monadEffectAff)(H.write(new g.Just(new b.Cons(e, t.value0)))(n));
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 195, column 33 - line 197, column 57): " + [t.constructor.name]);
          });
        };
      },
          U = function U(n) {
        return function (e) {
          return u.discard(u.discardUnit)(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.write({
            initializers: b.Nil.value,
            finalizers: b.Nil.value
          })(n)))(function () {
            return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(e))(function (e) {
              return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n)))(function (n) {
                return u.discard(u.discardUnit)(M.bindAff)(h.traverse_(M.applicativeAff)(b.foldableList)(o.fork(o.monadForkAff))(n.finalizers))(function () {
                  return u.discard(u.discardUnit)(M.bindAff)(v.parSequence_(M.parallelAff)(b.foldableList)(n.initializers))(function () {
                    return t.pure(M.applicativeAff)(e);
                  });
                });
              });
            });
          });
        };
      },
          P = function P(n) {
        return function (e) {
          return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(e)))(function (e) {
            return F.liftEffect(M.monadEffectAff)(H["modify'"](function (e) {
              return {
                state: e + 1 | 0,
                value: n(e)
              };
            })(e.fresh));
          });
        };
      },
          O = function O(n) {
        return function (e) {
          return function (t) {
            return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(e)))(function (r) {
              return W(n)(e)(r.component.eval(new N.Query(E.map(p.functorCoyoneda)(g.Just.create)(p.liftCoyoneda(t)), w.const(g.Nothing.value))));
            });
          };
        };
      },
          W = function n(e) {
        return function (v) {
          return function (p) {
            var w;
            return f.foldFree(M.monadRecAff)((w = v, function (f) {
              if (f instanceof _.State) return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(w)))(function (n) {
                var r = f.value0(n.state);
                if (I.unsafeRefEq(n.state)(r.value1)) return t.pure(M.applicativeAff)(r.value0);
                if (d.otherwise) return u.discard(u.discardUnit)(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.write({
                  component: n.component,
                  state: r.value1,
                  refs: n.refs,
                  children: n.children,
                  childrenIn: n.childrenIn,
                  childrenOut: n.childrenOut,
                  selfRef: n.selfRef,
                  handlerRef: n.handlerRef,
                  pendingQueries: n.pendingQueries,
                  pendingOuts: n.pendingOuts,
                  pendingHandlers: n.pendingHandlers,
                  rendering: n.rendering,
                  fresh: n.fresh,
                  subscriptions: n.subscriptions,
                  forks: n.forks,
                  lifecycleHandlers: n.lifecycleHandlers
                })(w)))(function () {
                  return u.discard(u.discardUnit)(M.bindAff)(U(n.lifecycleHandlers)(e(n.lifecycleHandlers)(w)))(function () {
                    return t.pure(M.applicativeAff)(r.value0);
                  });
                });
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 85, column 7 - line 91, column 21): " + [r.constructor.name]);
              });
              if (f instanceof _.Subscribe) return u.bind(M.bindAff)(P(_.SubscriptionId)(w))(function (n) {
                var r = f.value0(n);
                return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(w)))(function (i) {
                  return u.bind(M.bindAff)(o.fork(o.monadForkAff)(u.bind(M.bindAff)(r)(function (r) {
                    var f = u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(i.subscriptions)))(function (e) {
                      return u.discard(u.discardUnit)(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.modify_(E.map(g.functorMaybe)(y.delete(_.ordSubscriptionId)(n)))(i.subscriptions)))(function () {
                        return t.when(M.applicativeAff)(g.maybe(!1)(y.member(_.ordSubscriptionId)(n))(e))(k.finalize(r.finalizer));
                      });
                    }),
                        v = u.bind(l.bindFreeT(a.functorAwait)(M.monadAff))(a.await(M.monadAff))(function (r) {
                      return u.bind(l.bindFreeT(a.functorAwait)(M.monadAff))(c.lift(l.monadTransFreeT(a.functorAwait))(M.monadAff)(F.liftEffect(M.monadEffectAff)(H.read(i.subscriptions))))(function (i) {
                        return t.when(l.applicativeFreeT(a.functorAwait)(M.monadAff))(m.eq(g.eqMaybe(m.eqBoolean))(E.map(g.functorMaybe)(y.member(_.ordSubscriptionId)(n))(i))(new g.Just(!0)))(u.bind(l.bindFreeT(a.functorAwait)(M.monadAff))(c.lift(l.monadTransFreeT(a.functorAwait))(M.monadAff)(o.fork(o.monadForkAff)(B(e)(w)(new R.Action(r)))))(function () {
                          return v;
                        }));
                      });
                    });
                    return u.discard(u.discardUnit)(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.modify_(E.map(g.functorMaybe)(y.insert(_.ordSubscriptionId)(n)(f)))(i.subscriptions)))(function () {
                      return u.discard(u.discardUnit)(M.bindAff)(a.runProcess(M.monadRecAff)(a.pullFrom(M.monadRecAff)(v)(r.producer)))(function () {
                        return k.finalize(f);
                      });
                    });
                  })))(function () {
                    return t.pure(M.applicativeAff)(f.value1(n));
                  });
                });
              });
              if (f instanceof _.Unsubscribe) return u.discard(u.discardUnit)(M.bindAff)((v = f.value0, function (n) {
                return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n)))(function (n) {
                  return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n.subscriptions)))(function (n) {
                    return h.traverse_(M.applicativeAff)(h.foldableMaybe)(k.finalize)(u.bindFlipped(g.bindMaybe)(y.lookup(_.ordSubscriptionId)(v))(n));
                  });
                });
              })(w))(function () {
                return t.pure(M.applicativeAff)(f.value1);
              });
              var v, p, b;
              if (f instanceof _.Lift) return f.value0;
              if (f instanceof _.ChildQuery) return function (n) {
                return function (t) {
                  return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n)))(function (n) {
                    return S.unChildQueryBox(function (t) {
                      return E.map(M.functorAff)(t.value2)(s.sequential(M.parallelAff)(t.value0(M.applicativeParAff)(function (n) {
                        return s.parallel(M.parallelAff)(u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n)))(function (n) {
                          return L.unDriverStateX(function (n) {
                            return O(e)(n.selfRef)(t.value1);
                          })(n);
                        }));
                      })(n.children)));
                    })(t);
                  });
                };
              }(w)(f.value0);
              if (f instanceof _.Raise) return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(w)))(function (n) {
                return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n.handlerRef)))(function (e) {
                  return u.discard(u.discardUnit)(M.bindAff)(x(n.pendingOuts)(e(f.value0)))(function () {
                    return t.pure(M.applicativeAff)(f.value1);
                  });
                });
              });
              if (f instanceof _.Par) return s.sequential(M.parallelAff)(r.retractFreeAp(M.applicativeParAff)(r.hoistFreeAp((p = s.parallel(M.parallelAff), b = n(e)(w), function (n) {
                return p(b(n));
              }))(f.value0)));
              if (f instanceof _.Fork) return u.bind(M.bindAff)(P(_.ForkId)(w))(function (r) {
                return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(w)))(function (a) {
                  return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.new(!1)))(function (l) {
                    return u.bind(M.bindAff)(o.fork(o.monadForkAff)(M.finally(F.liftEffect(M.monadEffectAff)(function () {
                      return H.modify_(y.delete(_.ordForkId)(r))(a.forks)(), H.write(!0)(l)();
                    }))(n(e)(w)(f.value0))))(function (n) {
                      return u.discard(u.discardUnit)(M.bindAff)(F.liftEffect(M.monadEffectAff)(i.unlessM(C.monadEffect)(H.read(l))(H.modify_(y.insert(_.ordForkId)(r)(n))(a.forks))))(function () {
                        return t.pure(M.applicativeAff)(f.value1(r));
                      });
                    });
                  });
                });
              });
              if (f instanceof _.Kill) return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(w)))(function (n) {
                return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(n.forks)))(function (n) {
                  return u.discard(u.discardUnit)(M.bindAff)(h.traverse_(M.applicativeAff)(h.foldableMaybe)(M.killFiber(T.error("Cancelled")))(y.lookup(_.ordForkId)(f.value0)(n)))(function () {
                    return t.pure(M.applicativeAff)(f.value1);
                  });
                });
              });
              if (f instanceof _.GetRef) return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(w)))(function (n) {
                return t.pure(M.applicativeAff)(f.value1(y.lookup(A.ordString)(f.value0)(n.refs)));
              });
              throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 82, column 12 - line 146, column 33): " + [f.constructor.name]);
            }))(p);
          };
        };
      },
          B = function B(n) {
        return function (e) {
          return function (t) {
            if (t instanceof R.RefUpdate) return F.liftEffect(M.monadEffectAff)(w.flip(H.modify_)(e)(L.mapDriverState(function (n) {
              return {
                component: n.component,
                state: n.state,
                refs: y.alter(A.ordString)(w.const(t.value1))(t.value0)(n.refs),
                children: n.children,
                childrenIn: n.childrenIn,
                childrenOut: n.childrenOut,
                selfRef: n.selfRef,
                handlerRef: n.handlerRef,
                pendingQueries: n.pendingQueries,
                pendingOuts: n.pendingOuts,
                pendingHandlers: n.pendingHandlers,
                rendering: n.rendering,
                fresh: n.fresh,
                subscriptions: n.subscriptions,
                forks: n.forks,
                lifecycleHandlers: n.lifecycleHandlers
              };
            })));
            if (t instanceof R.Action) return u.bind(M.bindAff)(F.liftEffect(M.monadEffectAff)(H.read(e)))(function (r) {
              return W(n)(e)(r.component.eval(new N.Action(t.value0, D.unit)));
            });
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 51, column 20 - line 57, column 62): " + [t.constructor.name]);
          };
        };
      };

      e.evalF = B, e.evalQ = O, e.evalM = W, e.handleLifecycle = U, e.queueOrRun = x;
    }(e), function (n) {
      "use strict";

      n["Halogen.Component"] = n["Halogen.Component"] || {};

      var e = n["Halogen.Component"],
          t = n["Control.Applicative"],
          r = n["Data.Coyoneda"],
          u = n["Data.Foldable"],
          a = n["Data.Function"],
          i = n["Data.Functor"],
          o = n["Data.Maybe"],
          f = n["Data.Unit"],
          l = n["Halogen.Query.HalogenM"],
          c = n["Halogen.Query.HalogenQ"],
          v = n["Unsafe.Coerce"],
          s = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          d = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          p = v.unsafeCoerce,
          m = v.unsafeCoerce,
          h = v.unsafeCoerce,
          w = {
        handleAction: a.const(t.pure(l.applicativeHalogenM)(f.unit)),
        handleQuery: a.const(t.pure(l.applicativeHalogenM)(o.Nothing.value)),
        receive: a.const(o.Nothing.value),
        initialize: o.Nothing.value,
        finalize: o.Nothing.value
      };

      e.mkComponent = h, e.unComponent = m, e.mkEval = function (n) {
        return function (e) {
          if (e instanceof c.Initialize) return i.voidLeft(l.functorHalogenM)(u.traverse_(l.applicativeHalogenM)(u.foldableMaybe)(n.handleAction)(n.initialize))(e.value0);
          if (e instanceof c.Finalize) return i.voidLeft(l.functorHalogenM)(u.traverse_(l.applicativeHalogenM)(u.foldableMaybe)(n.handleAction)(n.finalize))(e.value0);
          if (e instanceof c.Receive) return i.voidLeft(l.functorHalogenM)(u.traverse_(l.applicativeHalogenM)(u.foldableMaybe)(n.handleAction)(n.receive(e.value0)))(e.value1);
          if (e instanceof c.Action) return i.voidLeft(l.functorHalogenM)(n.handleAction(e.value0))(e.value1);
          if (e instanceof c.Query) return r.unCoyoneda(function (t) {
            var r = i.map(l.functorHalogenM)(o.maybe(e.value1(f.unit))(t));
            return function (e) {
              return r(n.handleQuery(e));
            };
          })(e.value0);
          throw new Error("Failed pattern match at Halogen.Component (line 172, column 15 - line 182, column 70): " + [e.constructor.name]);
        };
      }, e.defaultEval = w, e.ComponentSlot = s, e.ThunkSlot = d, e.unComponentSlot = p;
    }(e), function (n) {
      "use strict";

      n["Halogen.Aff.Driver"] = n["Halogen.Aff.Driver"] || {};

      var e = n["Halogen.Aff.Driver"],
          t = n["Control.Applicative"],
          r = n["Control.Bind"],
          u = n["Control.Category"],
          a = n["Control.Coroutine"],
          i = n["Control.Monad.Error.Class"],
          o = n["Control.Monad.Fork.Class"],
          f = n["Control.Monad.Rec.Class"],
          l = n["Control.Parallel"],
          c = n["Data.Either"],
          v = n["Data.Foldable"],
          s = n["Data.Function"],
          d = n["Data.Functor"],
          p = n["Data.List"],
          m = n["Data.List.Types"],
          h = n["Data.Map.Internal"],
          w = n["Data.Maybe"],
          E = n["Data.Ord"],
          b = n["Data.Unit"],
          y = n.Effect,
          g = n["Effect.Aff"],
          A = n["Effect.Aff.AVar"],
          D = n["Effect.Class"],
          C = n["Effect.Console"],
          M = n["Effect.Exception"],
          F = n["Effect.Ref"],
          T = n["Halogen.Aff.Driver.Eval"],
          H = n["Halogen.Aff.Driver.State"],
          L = n["Halogen.Component"],
          S = n["Halogen.Data.Slot"],
          k = n["Halogen.Query.EventSource"],
          _ = n["Halogen.Query.HalogenQ"],
          N = n["Halogen.Query.Input"],
          R = F.new({
        initializers: m.Nil.value,
        finalizers: m.Nil.value
      }),
          I = g.runAff_(c.either(M.throwException)(s.const(t.pure(y.applicativeEffect)(b.unit)))),
          x = function x(n) {
        return function () {
          var e,
              t = F.read(n)();
          return F.write(w.Nothing.value)(n)(), v.for_(y.applicativeEffect)(v.foldableMaybe)(t)((e = v.traverse_(g.applicativeAff)(m.foldableList)(o.fork(o.monadForkAff)), function (n) {
            return I(e(p.reverse(n)));
          }))();
        };
      };

      e.runUI = function (n) {
        return function (e) {
          return function (U) {
            var P = function P(n) {
              return function (e) {
                return function (t) {
                  return r.bind(g.bindAff)(A.empty)(function (u) {
                    return r.bind(g.bindAff)(D.liftEffect(g.monadEffectAff)(function () {
                      var t = F.read(n)();
                      return F.modify_(function (n) {
                        return n + 1 | 0;
                      })(n)(), F.modify_(h.insert(E.ordInt)(t)(u))(e)(), t;
                    }))(function (n) {
                      var f = a.producer(g.monadAff)(d.map(g.functorAff)(c.either(s.const(new c.Right(b.unit)))(c.Left.create))(i.try(g.monadErrorAff)(A.take(u))));
                      return d.void(g.functorAff)(o.fork(o.monadForkAff)(r.discard(r.discardUnit)(g.bindAff)(a.runProcess(g.monadRecAff)(a.connect(g.monadRecAff)(g.parallelAff)(f)(t)))(function () {
                        return r.discard(r.discardUnit)(g.bindAff)(D.liftEffect(g.monadEffectAff)(F.modify_(h.delete(E.ordInt)(n))(e)))(function () {
                          return A.kill(M.error("ended"))(u);
                        });
                      })));
                    });
                  });
                };
              };
            },
                O = function O(n) {
              return function (e) {
                return function (t) {
                  return L.unComponent(function (u) {
                    return function () {
                      var a,
                          i = R(),
                          o = H.initDriverState(u)(t)(e)(i)(),
                          f = F.read(n)();
                      return F.write({
                        initializers: m.Nil.value,
                        finalizers: f.finalizers
                      })(n)(), r.bindFlipped(y.bindEffect)(H.unDriverStateX((a = B(n), function (n) {
                        return a(n.selfRef);
                      })))(F.read(o))(), r.bindFlipped(y.bindEffect)(function (n) {
                        return function (e) {
                          return H.unDriverStateX(function (t) {
                            var u = T.evalM(B)(t.selfRef)(t.component.eval(new _.Initialize(b.unit)));
                            return F.modify_(function (n) {
                              return {
                                initializers: new m.Cons(r.discard(r.discardUnit)(g.bindAff)(l.parSequence_(g.parallelAff)(m.foldableList)(p.reverse(n.initializers)))(function () {
                                  return r.discard(r.discardUnit)(g.bindAff)(u)(function () {
                                    return D.liftEffect(g.monadEffectAff)(function () {
                                      return x(t.pendingQueries)(), x(t.pendingOuts)();
                                    });
                                  });
                                }), e),
                                finalizers: n.finalizers
                              };
                            })(n);
                          });
                        };
                      }(n)(f.initializers))(F.read(o))(), o;
                    };
                  });
                };
              };
            },
                W = function W(e) {
              return function (u) {
                return function (a) {
                  return function (i) {
                    return L.unComponentSlot(function (o) {
                      return function () {
                        var f = d.map(y.functorEffect)(o.pop)(F.read(a))(),
                            l = function () {
                          if (f instanceof w.Just) {
                            F.write(f.value0.value1)(a)();
                            var n = F.read(f.value0.value0)();
                            return H.unDriverStateX(function (n) {
                              return function () {
                                var e;
                                return s.flip(F.write)(n.handlerRef)((e = w.maybe(t.pure(g.applicativeAff)(b.unit))(u), function (n) {
                                  return e(o.output(n));
                                }))(), I(T.evalM(B)(n.selfRef)(n.component.eval(o.input)))();
                              };
                            })(n)(), f.value0.value0;
                          }

                          if (f instanceof w.Nothing) return o.input instanceof _.Receive ? O(e)((r = w.maybe(t.pure(g.applicativeAff)(b.unit))(u), function (n) {
                            return r(o.output(n));
                          }))(o.input.value0)(o.component)() : M.throw("Halogen internal error: slot input was not a Receive query")();
                          var r;
                          throw new Error("Failed pattern match at Halogen.Aff.Driver (line 236, column 14 - line 249, column 81): " + [f.constructor.name]);
                        }(),
                            c = d.map(y.functorEffect)(function (n) {
                          return w.isJust(o.get(n));
                        })(F.read(i))();

                        return t.when(y.applicativeEffect)(c)(C.warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))(), F.modify_(o.set(l))(i)(), r.bind(y.bindEffect)(F.read(l))(H.renderStateX(y.functorEffect)(function (e) {
                          if (e instanceof w.Nothing) return M.throw("Halogen internal error: child was not initialized in renderChild");
                          if (e instanceof w.Just) return t.pure(y.applicativeEffect)(n.renderChild(e.value0));
                          throw new Error("Failed pattern match at Halogen.Aff.Driver (line 254, column 37 - line 256, column 50): " + [e.constructor.name]);
                        }))();
                      };
                    });
                  };
                };
              };
            },
                B = function e(r) {
              return function (a) {
                return function () {
                  var i = F.read(a)(),
                      l = d.map(y.functorEffect)(w.isNothing)(F.read(i.pendingHandlers))();
                  t.when(y.applicativeEffect)(l)(F.write(new w.Just(m.Nil.value))(i.pendingHandlers))(), F.write(S.empty)(i.childrenOut)(), F.write(i.children)(i.childrenIn)();

                  var c,
                      h,
                      E,
                      A,
                      D = u.identity(u.categoryFn)(i.selfRef),
                      C = u.identity(u.categoryFn)(i.pendingQueries),
                      M = u.identity(u.categoryFn)(i.pendingHandlers),
                      L = (c = T.queueOrRun(M), h = d.void(g.functorAff), E = T.evalF(e)(D), function (n) {
                    return c(h(E(n)));
                  }),
                      k = (A = T.queueOrRun(C), function (n) {
                    return A(L(N.Action.create(n)));
                  }),
                      _ = n.render(function (n) {
                    return I(L(n));
                  })(W(r)(k)(i.childrenIn)(i.childrenOut))(i.component.render(i.state))(i.rendering)(),
                      R = F.read(i.childrenOut)(),
                      x = F.read(i.childrenIn)();

                  return S.foreachSlot(y.applicativeEffect)(x)(function (e) {
                    return function () {
                      var t = F.read(e)();
                      return H.renderStateX_(y.applicativeEffect)(n.removeChild)(t)(), q(r)(t)();
                    };
                  })(), s.flip(F.modify_)(i.selfRef)(H.mapDriverState(function (n) {
                    return {
                      component: n.component,
                      state: n.state,
                      refs: n.refs,
                      children: R,
                      childrenIn: n.childrenIn,
                      childrenOut: n.childrenOut,
                      selfRef: n.selfRef,
                      handlerRef: n.handlerRef,
                      pendingQueries: n.pendingQueries,
                      pendingOuts: n.pendingOuts,
                      pendingHandlers: n.pendingHandlers,
                      rendering: new w.Just(_),
                      fresh: n.fresh,
                      subscriptions: n.subscriptions,
                      forks: n.forks,
                      lifecycleHandlers: n.lifecycleHandlers
                    };
                  }))(), t.when(y.applicativeEffect)(l)(s.flip(f.tailRecM(f.monadRecEffect))(b.unit)(function (n) {
                    return function () {
                      var n,
                          e = F.read(M)();
                      F.write(new w.Just(m.Nil.value))(M)(), v.traverse_(y.applicativeEffect)(v.foldableMaybe)((n = v.traverse_(g.applicativeAff)(m.foldableList)(o.fork(o.monadForkAff)), function (e) {
                        return I(n(p.reverse(e)));
                      }))(e)();
                      var t = F.read(M)();
                      return w.maybe(!1)(p.null)(t) ? d.voidLeft(y.functorEffect)(F.write(w.Nothing.value)(M))(new f.Done(b.unit))() : new f.Loop(b.unit);
                    };
                  }))();
                };
              };
            },
                q = function n(e) {
              return H.unDriverStateX(function (t) {
                return function () {
                  var u;
                  (u = t, function () {
                    var n, e, t;
                    return r.bindFlipped(y.bindEffect)(v.traverse_(y.applicativeEffect)(v.foldableMaybe)((e = v.traverse_(g.applicativeAff)(h.foldableMap)((n = o.fork(o.monadForkAff), function (e) {
                      return n(k.finalize(e));
                    })), function (n) {
                      return I(e(n));
                    })))(F.read(u.subscriptions))(), F.write(w.Nothing.value)(u.subscriptions)(), r.bindFlipped(y.bindEffect)(v.traverse_(y.applicativeEffect)(h.foldableMap)((t = g.killFiber(M.error("finalized")), function (n) {
                      return I(t(n));
                    })))(F.read(u.forks))(), F.write(h.empty)(u.forks)();
                  })();
                  var a = T.evalM(B)(t.selfRef)(t.component.eval(new _.Finalize(b.unit)));
                  return F.modify_(function (n) {
                    return {
                      initializers: n.initializers,
                      finalizers: new m.Cons(a, n.finalizers)
                    };
                  })(e)(), S.foreachSlot(y.applicativeEffect)(t.children)(function (t) {
                    return function () {
                      var r = F.read(t)();
                      return n(e)(r)();
                    };
                  })();
                };
              });
            },
                Q = function Q(n) {
              return function (e) {
                return function (u) {
                  return r.bind(g.bindAff)(D.liftEffect(g.monadEffectAff)(F.read(n)))(function (n) {
                    return n ? t.pure(g.applicativeAff)(w.Nothing.value) : T.evalQ(B)(e)(u);
                  });
                };
              };
            },
                V = function V(e) {
              return function (t) {
                return function (u) {
                  return function (a) {
                    return T.handleLifecycle(t)(function () {
                      var i, o;
                      return F.read(e)() ? b.unit : (F.write(!0)(e)(), r.bindFlipped(y.bindEffect)(v.traverse_(y.applicativeEffect)(h.foldableMap)((i = A.kill(M.error("disposed")), function (n) {
                        return g.launchAff_(i(n));
                      })))(F.read(a))(), q(t)(u)(), H.unDriverStateX((o = v.traverse_(y.applicativeEffect)(v.foldableMaybe)(n.dispose), function (n) {
                        return o(n.rendering);
                      }))(u)());
                    });
                  };
                };
              };
            };

            return r.bind(g.bindAff)(D.liftEffect(g.monadEffectAff)(R))(function (n) {
              return r.bind(g.bindAff)(D.liftEffect(g.monadEffectAff)(F.new(0)))(function (u) {
                return r.bind(g.bindAff)(D.liftEffect(g.monadEffectAff)(F.new(!1)))(function (a) {
                  return T.handleLifecycle(n)(function () {
                    var i,
                        f = F.new(h.empty)(),
                        l = r.bindFlipped(y.bindEffect)(F.read)(O(n)((i = f, function (n) {
                      return r.bind(g.bindAff)(D.liftEffect(g.monadEffectAff)(F.read(i)))(function (e) {
                        return v.traverse_(g.applicativeAff)(h.foldableMap)(o.fork(o.monadForkAff))(d.map(h.functorMap)(A.put(n))(e));
                      });
                    }))(U)(e))();
                    return H.unDriverStateX(function (e) {
                      return t.pure(y.applicativeEffect)({
                        query: Q(a)(e.selfRef),
                        subscribe: P(u)(f),
                        dispose: V(a)(n)(l)(f)
                      });
                    })(l)();
                  });
                });
              });
            });
          };
        };
      };
    }(e), function (n) {
      "use strict";

      (e["Web.DOM.ParentNode"] = e["Web.DOM.ParentNode"] || {})._querySelector = function (n) {
        return function (e) {
          return function () {
            return e.querySelector(n);
          };
        };
      };
    }(), function (n) {
      "use strict";

      n["Web.DOM.ParentNode"] = n["Web.DOM.ParentNode"] || {};
      var e = n["Web.DOM.ParentNode"],
          t = n["Web.DOM.ParentNode"],
          r = n["Data.Functor"],
          u = n["Data.Nullable"],
          a = n.Effect;

      e.querySelector = function (n) {
        var e = r.map(a.functorEffect)(u.toMaybe),
            i = t._querySelector(n);

        return function (n) {
          return e(i(n));
        };
      };
    }(e), function (n) {
      "use strict";

      n.eventListener = function (n) {
        return function () {
          return function (e) {
            return n(e)();
          };
        };
      }, n.addEventListener = function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return function () {
                return r.addEventListener(n, e, t);
              };
            };
          };
        };
      }, n.removeEventListener = function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return function () {
                return r.removeEventListener(n, e, t);
              };
            };
          };
        };
      };
    }(e["Web.Event.EventTarget"] = e["Web.Event.EventTarget"] || {}), function (n) {
      "use strict";

      n["Web.Event.EventTarget"] = n["Web.Event.EventTarget"] || {};
      var e = n["Web.Event.EventTarget"],
          t = n["Web.Event.EventTarget"];
      e.eventListener = t.eventListener, e.addEventListener = t.addEventListener, e.removeEventListener = t.removeEventListener;
    }(e), function (n) {
      "use strict";

      (e["Web.HTML"] = e["Web.HTML"] || {}).window = function () {
        return window;
      };
    }(), function (n) {
      "use strict";

      n["Web.HTML"] = n["Web.HTML"] || {};
      var e = n["Web.HTML"],
          t = n["Web.HTML"];
      e.window = t.window;
    }(e), function (n) {
      "use strict";

      n["Web.HTML.Event.EventTypes"] = n["Web.HTML.Event.EventTypes"] || {};
      n["Web.HTML.Event.EventTypes"].domcontentloaded = "DOMContentLoaded";
    }(e), function (n) {
      "use strict";

      (e["Web.HTML.HTMLDocument"] = e["Web.HTML.HTMLDocument"] || {})._readyState = function (n) {
        return function () {
          return n.readyState;
        };
      };
    }(), function (n) {
      "use strict";

      n["Web.HTML.HTMLDocument.ReadyState"] = n["Web.HTML.HTMLDocument.ReadyState"] || {};

      var e = n["Web.HTML.HTMLDocument.ReadyState"],
          t = n["Data.Maybe"],
          r = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          u = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          a = function () {
        function n() {}

        return n.value = new n(), n;
      }();

      e.Loading = r, e.parse = function (n) {
        return "loading" === n ? new t.Just(r.value) : "interactive" === n ? new t.Just(u.value) : "complete" === n ? new t.Just(a.value) : t.Nothing.value;
      };
    }(e), function (n) {
      "use strict";

      n["Web.HTML.HTMLDocument"] = n["Web.HTML.HTMLDocument"] || {};
      var e,
          t,
          r = n["Web.HTML.HTMLDocument"],
          u = n["Web.HTML.HTMLDocument"],
          a = n["Data.Functor"],
          i = n["Data.Maybe"],
          o = n.Effect,
          f = n["Unsafe.Coerce"],
          l = n["Web.HTML.HTMLDocument.ReadyState"],
          c = f.unsafeCoerce,
          v = f.unsafeCoerce,
          s = (t = a.map(o.functorEffect)((e = i.fromMaybe(l.Loading.value), function (n) {
        return e(l.parse(n));
      })), function (n) {
        return t(u._readyState(n));
      });
      r.toDocument = v, r.toParentNode = c, r.readyState = s;
    }(e), function (n) {
      "use strict";

      (e["Web.HTML.HTMLElement"] = e["Web.HTML.HTMLElement"] || {})._read = function (n, e, t) {
        var r = Object.prototype.toString.call(t);
        return 0 === r.indexOf("[object HTML") && r.indexOf("Element]") === r.length - 8 ? e(t) : n;
      };
    }(), function (n) {
      "use strict";

      n["Web.HTML.HTMLElement"] = n["Web.HTML.HTMLElement"] || {};
      var e = n["Web.HTML.HTMLElement"],
          t = n["Web.HTML.HTMLElement"],
          r = n["Data.Maybe"],
          u = n["Unsafe.Coerce"].unsafeCoerce;
      e.fromElement = function (n) {
        return t._read(r.Nothing.value, r.Just.create, n);
      }, e.toNode = u;
    }(e), function (n) {
      "use strict";

      (e["Web.HTML.Window"] = e["Web.HTML.Window"] || {}).document = function (n) {
        return function () {
          return n.document;
        };
      };
    }(), function (n) {
      "use strict";

      n["Web.HTML.Window"] = n["Web.HTML.Window"] || {};
      var e = n["Web.HTML.Window"],
          t = n["Web.HTML.Window"],
          r = n["Unsafe.Coerce"].unsafeCoerce;
      e.toEventTarget = r, e.document = t.document;
    }(e), function (n) {
      "use strict";

      n["Halogen.Aff.Util"] = n["Halogen.Aff.Util"] || {};
      var e = n["Halogen.Aff.Util"],
          t = n["Control.Applicative"],
          r = n["Control.Bind"],
          u = n["Control.Monad.Error.Class"],
          a = n["Data.Either"],
          i = n["Data.Function"],
          o = n["Data.Functor"],
          f = n["Data.Maybe"],
          l = n["Data.Unit"],
          c = n.Effect,
          v = n["Effect.Aff"],
          s = n["Effect.Class"],
          d = n["Effect.Exception"],
          p = n["Web.DOM.ParentNode"],
          m = n["Web.Event.EventTarget"],
          h = n["Web.HTML"],
          w = n["Web.HTML.Event.EventTypes"],
          E = n["Web.HTML.HTMLDocument"],
          b = n["Web.HTML.HTMLDocument.ReadyState"],
          y = n["Web.HTML.HTMLElement"],
          g = n["Web.HTML.Window"],
          A = v.runAff_(a.either(d.throwException)(i.const(t.pure(c.applicativeEffect)(l.unit)))),
          D = v.makeAff(function (n) {
        return function () {
          if (r.bindFlipped(c.bindEffect)(E.readyState)(r.bindFlipped(c.bindEffect)(g.document)(h.window))() instanceof b.Loading) {
            var e = o.map(c.functorEffect)(g.toEventTarget)(h.window)(),
                t = m.eventListener(function (e) {
              return n(new a.Right(l.unit));
            })();
            return m.addEventListener(w.domcontentloaded)(t)(!1)(e)(), v.effectCanceler(m.removeEventListener(w.domcontentloaded)(t)(!1)(e));
          }

          return n(new a.Right(l.unit))(), v.nonCanceler;
        };
      }),
          C = r.discard(r.discardUnit)(v.bindAff)(D)(function () {
        return r.bind(v.bindAff)((n = "body", r.bind(v.bindAff)(s.liftEffect(v.monadEffectAff)(r.bindFlipped(c.bindEffect)(r.composeKleisliFlipped(c.bindEffect)((e = p.querySelector(n), function (n) {
          return e(E.toParentNode(n));
        }))(g.document))(h.window)))(function (n) {
          return t.pure(v.applicativeAff)(r.bindFlipped(f.bindMaybe)(y.fromElement)(n));
        })))(function (n) {
          return f.maybe(u.throwError(v.monadThrowAff)(d.error("Could not find body")))(t.pure(v.applicativeAff))(n);
        });
        var n, e;
      });
      e.awaitBody = C, e.runHalogenAff = A;
    }(e), function (n) {
      "use strict";

      n["Halogen.VDom.Machine"] = n["Halogen.VDom.Machine"] || {};

      var e = n["Halogen.VDom.Machine"],
          t = n["Unsafe.Coerce"],
          r = function () {
        function n(n, e, t, r) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return new n(e, t, r, u);
              };
            };
          };
        }, n;
      }(),
          u = t.unsafeCoerce,
          a = t.unsafeCoerce,
          i = u(function (n) {
        return n.value0;
      });

      e.Step = r, e.mkStep = a, e.unStep = u, e.extract = i, e.step = function (n, e) {
        return n.value2(n.value1, e);
      }, e.halt = function (n) {
        return n.value3(n.value1);
      };
    }(e), function (n) {
      "use strict";

      n.unsafeGetAny = function (n, e) {
        return e[n];
      }, n.unsafeHasAny = function (n, e) {
        return e.hasOwnProperty(n);
      }, n.unsafeSetAny = function (n, e, t) {
        t[n] = e;
      }, n.forE = function (n, e) {
        for (var t = [], r = 0; r < n.length; r++) {
          t.push(e(r, n[r]));
        }

        return t;
      }, n.forEachE = function (n, e) {
        for (var t = 0; t < n.length; t++) {
          e(n[t]);
        }
      }, n.forInE = function (n, e) {
        for (var t = Object.keys(n), r = 0; r < t.length; r++) {
          var u = t[r];
          e(u, n[u]);
        }
      }, n.diffWithIxE = function (n, e, t, r, u) {
        for (var a = [], i = n.length, o = e.length, f = 0;;) {
          if (f < i) f < o ? a.push(t(f, n[f], e[f])) : r(f, n[f]);else {
            if (!(f < o)) break;
            a.push(u(f, e[f]));
          }
          f++;
        }

        return a;
      }, n.strMapWithIxE = function (n, e, t) {
        for (var r = {}, u = 0; u < n.length; u++) {
          var a = n[u],
              i = e(a);
          r[i] = t(i, u, a);
        }

        return r;
      }, n.diffWithKeyAndIxE = function (n, e, t, r, u, a) {
        for (var i = {}, o = 0; o < e.length; o++) {
          var f = e[o],
              l = t(f);
          n.hasOwnProperty(l) ? i[l] = r(l, o, n[l], f) : i[l] = a(l, o, f);
        }

        for (var l in n) {
          l in i || u(l, n[l]);
        }

        return i;
      }, n.refEq = function (n, e) {
        return n === e;
      }, n.createTextNode = function (n, e) {
        return e.createTextNode(n);
      }, n.setTextContent = function (n, e) {
        e.textContent = n;
      }, n.createElement = function (n, e, t) {
        return null != n ? t.createElementNS(n, e) : t.createElement(e);
      }, n.insertChildIx = function (n, e, t) {
        var r = t.childNodes.item(n) || null;
        r !== e && t.insertBefore(e, r);
      }, n.removeChild = function (n, e) {
        e && n.parentNode === e && e.removeChild(n);
      }, n.parentNode = function (n) {
        return n.parentNode;
      }, n.setAttribute = function (n, e, t, r) {
        null != n ? r.setAttributeNS(n, e, t) : r.setAttribute(e, t);
      }, n.removeAttribute = function (n, e, t) {
        null != n ? t.removeAttributeNS(n, e) : t.removeAttribute(e);
      }, n.hasAttribute = function (n, e, t) {
        return null != n ? t.hasAttributeNS(n, e) : t.hasAttribute(e);
      }, n.addEventListener = function (n, e, t) {
        t.addEventListener(n, e, !1);
      }, n.removeEventListener = function (n, e, t) {
        t.removeEventListener(n, e, !1);
      }, n.jsUndefined = void 0;
    }(e["Halogen.VDom.Util"] = e["Halogen.VDom.Util"] || {}), function (n) {
      "use strict";

      n["Halogen.VDom.Util"] = n["Halogen.VDom.Util"] || {};
      var e = n["Halogen.VDom.Util"],
          t = n["Halogen.VDom.Util"],
          r = n["Foreign.Object.ST"],
          u = n["Unsafe.Coerce"],
          a = t.unsafeGetAny,
          i = u.unsafeCoerce,
          o = t.unsafeSetAny,
          f = r.new;
      e.newMutMap = f, e.pokeMutMap = o, e.unsafeFreeze = i, e.unsafeLookup = a, e.unsafeGetAny = t.unsafeGetAny, e.unsafeHasAny = t.unsafeHasAny, e.unsafeSetAny = t.unsafeSetAny, e.forE = t.forE, e.forEachE = t.forEachE, e.forInE = t.forInE, e.diffWithIxE = t.diffWithIxE, e.diffWithKeyAndIxE = t.diffWithKeyAndIxE, e.strMapWithIxE = t.strMapWithIxE, e.refEq = t.refEq, e.createTextNode = t.createTextNode, e.setTextContent = t.setTextContent, e.createElement = t.createElement, e.insertChildIx = t.insertChildIx, e.removeChild = t.removeChild, e.parentNode = t.parentNode, e.setAttribute = t.setAttribute, e.removeAttribute = t.removeAttribute, e.hasAttribute = t.hasAttribute, e.addEventListener = t.addEventListener, e.removeEventListener = t.removeEventListener, e.jsUndefined = t.jsUndefined;
    }(e), function (n) {
      "use strict";

      n["Halogen.VDom.DOM.Prop"] = n["Halogen.VDom.DOM.Prop"] || {};

      var e = n["Halogen.VDom.DOM.Prop"],
          t = n["Data.Maybe"],
          r = n["Data.Nullable"],
          u = n["Data.Tuple"],
          a = n["Data.Unit"],
          i = n["Effect.Ref"],
          o = n.Foreign,
          f = n["Foreign.Object"],
          l = n["Halogen.VDom.Machine"],
          c = n["Halogen.VDom.Util"],
          v = n["Unsafe.Coerce"],
          s = n["Web.Event.EventTarget"],
          d = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          p = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          m = function () {
        function n(n, e, t) {
          this.value0 = n, this.value1 = e, this.value2 = t;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return new n(e, t, r);
            };
          };
        }, n;
      }(),
          h = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          w = function () {
        function n(n, e) {
          this.value0 = n, this.value1 = e;
        }

        return n.create = function (e) {
          return function (t) {
            return new n(e, t);
          };
        }, n;
      }(),
          E = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          b = c.unsafeGetAny,
          y = c.unsafeSetAny,
          g = function g(n) {
        if (n instanceof m && n.value0 instanceof t.Just) return "attr/" + n.value0.value0 + ":" + n.value1;
        if (n instanceof m) return "attr/:" + n.value1;
        if (n instanceof h) return "prop/" + n.value0;
        if (n instanceof w) return "handler/" + n.value0;
        if (n instanceof E) return "ref";
        throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [n.constructor.name]);
      },
          A = v.unsafeCoerce;

      e.Property = h, e.Handler = w, e.propFromString = A, e.buildProp = function (n) {
        return function (e) {
          var v = function v(n) {
            return function (t, i) {
              if (i instanceof m) return c.removeAttribute(r.toNullable(i.value0), i.value1, e);
              if (i instanceof h) return function (n, e) {
                return c.hasAttribute(r.null, n, e) ? c.removeAttribute(r.null, n, e) : "string" === o.typeOf(c.unsafeGetAny(n, e)) ? c.unsafeSetAny(n, "", e) : "rowSpan" === n ? c.unsafeSetAny(n, 1, e) : "colSpan" === n ? c.unsafeSetAny(n, 1, e) : c.unsafeSetAny(n, c.jsUndefined, e);
              }(i.value0, e);

              if (i instanceof w) {
                var f = c.unsafeLookup(i.value0, n);
                return c.removeEventListener(i.value0, u.fst(f), e);
              }

              if (i instanceof E) return a.unit;
              throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [i.constructor.name]);
            };
          },
              A = function A(e) {
            return e instanceof t.Just ? n(e.value0)() : a.unit;
          },
              D = function D(n) {
            var r = f.lookup("ref")(n.props);
            return r instanceof t.Just && r.value0 instanceof E ? A(r.value0.value0(new p(e))) : a.unit;
          },
              C = function C(n) {
            return function (t, a, o) {
              if (o instanceof m) return c.setAttribute(r.toNullable(o.value0), o.value1, o.value2, e), o;
              if (o instanceof h) return y(o.value0, o.value1, e), o;

              if (o instanceof w) {
                var f = c.unsafeGetAny(o.value0, n);
                if (c.unsafeHasAny(o.value0, n)) return i.write(o.value1)(u.snd(f))(), o;
                var l = i.new(o.value1)(),
                    v = s.eventListener(function (n) {
                  return function () {
                    var e = i.read(l)();
                    return A(e(n));
                  };
                })();
                return c.pokeMutMap(o.value0, new u.Tuple(v, l), n), c.addEventListener(o.value0, v, e), o;
              }

              if (o instanceof E) return A(o.value0(new d(e))), o;
              throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [o.constructor.name]);
            };
          },
              M = function n(t, o) {
            var f = c.newMutMap(),
                s = v(t.events),
                d = function (n, t) {
              return function (a, o, f, l) {
                if (f instanceof m && l instanceof m) return f.value2 === l.value2 ? l : (c.setAttribute(r.toNullable(l.value0), l.value1, l.value2, e), l);

                if (f instanceof h && l instanceof h) {
                  if (c.refEq(f.value1, l.value1)) return l;

                  if ("value" === l.value0) {
                    var v = b("value", e);
                    return c.refEq(v, l.value1) ? l : (y(l.value0, l.value1, e), l);
                  }

                  return y(l.value0, l.value1, e), l;
                }

                if (f instanceof w && l instanceof w) {
                  var s = c.unsafeLookup(l.value0, n);
                  return i.write(l.value1)(u.snd(s))(), c.pokeMutMap(l.value0, s, t), l;
                }

                return l;
              };
            }(t.events, f),
                p = C(f),
                E = c.diffWithKeyAndIxE(t.props, o, g, d, s, p),
                A = {
              events: c.unsafeFreeze(f),
              props: E
            };

            return l.mkStep(new l.Step(a.unit, A, n, D));
          };

          return function (n) {
            var e = c.newMutMap(),
                t = c.strMapWithIxE(n, g, C(e)),
                r = {
              events: c.unsafeFreeze(e),
              props: t
            };
            return l.mkStep(new l.Step(a.unit, r, M, D));
          };
        };
      };
    }(e), function (n) {
      "use strict";

      n["Halogen.VDom.Types"] = n["Halogen.VDom.Types"] || {};

      var e = n["Halogen.VDom.Types"],
          t = n["Data.Bifunctor"],
          r = n["Data.Functor"],
          u = n["Data.Tuple"],
          a = n["Unsafe.Coerce"],
          i = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          o = function () {
        function n(n, e, t, r) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return new n(e, t, r, u);
              };
            };
          };
        }, n;
      }(),
          f = function () {
        function n(n, e, t, r) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return new n(e, t, r, u);
              };
            };
          };
        }, n;
      }(),
          l = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          c = function () {
        function n(n) {
          this.value0 = n;
        }

        return n.create = function (e) {
          return new n(e);
        }, n;
      }(),
          v = function () {
        function n(n, e, t) {
          this.value0 = n, this.value1 = e, this.value2 = t;
        }

        return n.create = function (e) {
          return function (t) {
            return function (r) {
              return new n(e, t, r);
            };
          };
        }, n;
      }(),
          s = function s(n) {
        return function (e) {
          return n(e);
        };
      },
          d = a.unsafeCoerce,
          p = new t.Bifunctor(function (n) {
        return function (e) {
          return s(function (t) {
            return d(new v(function (e) {
              return n(t.value0(e));
            }, function (n) {
              return e(t.value1(n));
            }, t.value2));
          });
        };
      }),
          m = s(function (n) {
        return function e(a) {
          if (a instanceof i) return new i(a.value0);
          if (a instanceof o) return new o(a.value0, a.value1, n.value0(a.value2), r.map(r.functorArray)(e)(a.value3));
          if (a instanceof f) return new f(a.value0, a.value1, n.value0(a.value2), r.map(r.functorArray)(r.map(u.functorTuple)(e))(a.value3));
          if (a instanceof l) return new l(n.value1(a.value0));
          if (a instanceof c) return new c(t.bimap(p)(n.value0)(n.value1)(a.value0));
          throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [a.constructor.name]);
        }(n.value2);
      });

      e.Text = i, e.Elem = o, e.Keyed = f, e.Widget = l, e.Grafted = c, e.runGraft = m;
    }(e), function (n) {
      "use strict";

      n["Halogen.HTML.Core"] = n["Halogen.HTML.Core"] || {};

      var e = n["Halogen.HTML.Core"],
          t = n["DOM.HTML.Indexed.ButtonType"],
          r = n["Data.Newtype"],
          u = n["Halogen.VDom.DOM.Prop"],
          a = n["Halogen.VDom.Types"],
          i = function i(n) {
        return n;
      },
          o = function o(n) {
        this.toPropValue = n;
      },
          f = new r.Newtype(function (n) {
        return n;
      }, i),
          l = new r.Newtype(function (n) {
        return n;
      }, function (n) {
        return n;
      }),
          c = new o(u.propFromString),
          v = new o(function (n) {
        return u.propFromString(t.renderButtonType(n));
      }),
          s = u.Handler.create;

      e.text = function (n) {
        return i(a.Text.create(n));
      }, e.element = function (n) {
        return function (e) {
          return function (t) {
            return function (r) {
              return new a.Elem(n, e, t, r);
            };
          };
        };
      }, e.prop = function (n) {
        return function (e) {
          var t = u.Property.create(e),
              r = n.toPropValue;
          return function (n) {
            return t(r(n));
          };
        };
      }, e.handler = s, e.newtypeHTML = f, e.isPropString = c, e.isPropButtonType = v, e.newtypeClassName = l;
    }(e), function (n) {
      "use strict";

      n["Halogen.HTML.Elements"] = n["Halogen.HTML.Elements"] || {};
      var e = n["Halogen.HTML.Elements"],
          t = n["Data.Maybe"],
          r = n["Halogen.HTML.Core"].element(t.Nothing.value)("button");
      e.button = r;
    }(e), function (n) {
      "use strict";

      n["Web.UIEvent.MouseEvent.EventTypes"] = n["Web.UIEvent.MouseEvent.EventTypes"] || {};
      n["Web.UIEvent.MouseEvent.EventTypes"].click = "click";
    }(e), function (n) {
      "use strict";

      n["Halogen.HTML.Events"] = n["Halogen.HTML.Events"] || {};
      var e,
          t,
          r,
          u,
          a = n["Halogen.HTML.Events"],
          i = n["Data.Functor"],
          o = n["Data.Maybe"],
          f = n["Halogen.HTML.Core"],
          l = n["Halogen.Query.Input"],
          c = n["Unsafe.Coerce"],
          v = n["Web.UIEvent.MouseEvent.EventTypes"],
          s = c.unsafeCoerce,
          d = (e = v.click, t = f.handler(e), r = i.map(i.functorFn)(i.map(o.functorMaybe)(l.Action.create)), u = function u(n) {
        return t(r(n));
      }, function (n) {
        return u(s(n));
      });
      a.onClick = d;
    }(e), function (n) {
      "use strict";

      n["Halogen.HTML.Properties"] = n["Halogen.HTML.Properties"] || {};

      var e,
          t,
          r = n["Halogen.HTML.Properties"],
          u = n["Data.Newtype"],
          a = n["Halogen.HTML.Core"],
          i = function i(n) {
        return a.prop(n);
      },
          o = (e = i(a.isPropString)("className"), t = u.unwrap(a.newtypeClassName), function (n) {
        return e(t(n));
      });

      r.class_ = o, r.type_ = function (n) {
        return i(n)("type");
      };
    }(e), function (n) {
      "use strict";

      n["Web.DOM.Element"] = n["Web.DOM.Element"] || {};
      var e = n["Web.DOM.Element"],
          t = n["Unsafe.Coerce"].unsafeCoerce;
      e.toNode = t;
    }(e), function (n) {
      "use strict";

      n["Halogen.VDom.DOM"] = n["Halogen.VDom.DOM"] || {};

      var e = n["Halogen.VDom.DOM"],
          t = n["Data.Array"],
          r = n["Data.Boolean"],
          u = n["Data.Maybe"],
          a = n["Data.Nullable"],
          i = n["Data.Tuple"],
          o = n["Halogen.VDom.Machine"],
          f = n["Halogen.VDom.Types"],
          l = n["Halogen.VDom.Util"],
          c = n["Web.DOM.Element"],
          v = function v(n) {
        return o.halt(n.widget);
      },
          s = function n(e, t) {
        if (t instanceof f.Grafted) return n(e, f.runGraft(t.value0));

        if (t instanceof f.Widget) {
          var r = o.step(e.widget, t.value0);
          return o.unStep(function (t) {
            return o.mkStep(new o.Step(t.value0, {
              build: e.build,
              widget: r
            }, n, v));
          })(r);
        }

        return v(e), e.build(t);
      },
          d = function d(n) {
        var e = l.parentNode(n.node);
        return l.removeChild(n.node, e);
      },
          p = function n(e, t) {
        if (t instanceof f.Grafted) return n(e, f.runGraft(t.value0));

        if (t instanceof f.Text) {
          if (e.value === t.value0) return o.mkStep(new o.Step(e.node, e, n, d));

          if (r.otherwise) {
            var u = {
              build: e.build,
              node: e.node,
              value: t.value0
            };
            return l.setTextContent(t.value0, e.node), o.mkStep(new o.Step(e.node, u, n, d));
          }
        }

        return d(e), e.build(t);
      },
          m = function m(n) {
        var e = l.parentNode(n.node);
        return l.removeChild(n.node, e), l.forInE(n.children, function (n, e) {
          return o.halt(e);
        }), o.halt(n.attrs);
      },
          h = function h(n) {
        var e = l.parentNode(n.node);
        return l.removeChild(n.node, e), l.forEachE(n.children, o.halt), o.halt(n.attrs);
      },
          w = function w(n, e, t, r) {
        return !(e !== r) && (n instanceof u.Just && t instanceof u.Just && n.value0 === t.value0 || n instanceof u.Nothing && t instanceof u.Nothing);
      },
          E = function n(e, r) {
        if (r instanceof f.Grafted) return n(e, f.runGraft(r.value0));

        if (r instanceof f.Elem && w(e.ns, e.name, r.value0, r.value1)) {
          var u = t.length(r.value3);

          if (0 === t.length(e.children) && 0 === u) {
            var a = o.step(e.attrs, r.value2),
                i = {
              build: e.build,
              node: e.node,
              attrs: a,
              ns: r.value0,
              name: r.value1,
              children: e.children
            };
            return o.mkStep(new o.Step(e.node, i, n, h));
          }

          var c = l.diffWithIxE(e.children, r.value3, function (n, t, r) {
            var u = o.step(t, r);
            return l.insertChildIx(n, o.extract(u), e.node), u;
          }, function (n, e) {
            return o.halt(e);
          }, function (n, t) {
            var r = e.build(t);
            return l.insertChildIx(n, o.extract(r), e.node), r;
          });
          a = o.step(e.attrs, r.value2), i = {
            build: e.build,
            node: e.node,
            attrs: a,
            ns: r.value0,
            name: r.value1,
            children: c
          };
          return o.mkStep(new o.Step(e.node, i, n, h));
        }

        return h(e), e.build(r);
      },
          b = function n(e, r) {
        if (r instanceof f.Grafted) return n(e, f.runGraft(r.value0));

        if (r instanceof f.Keyed && w(e.ns, e.name, r.value0, r.value1)) {
          var u = t.length(r.value3);

          if (0 === e.length && 0 === u) {
            var a = o.step(e.attrs, r.value2),
                c = {
              build: e.build,
              node: e.node,
              attrs: a,
              ns: r.value0,
              name: r.value1,
              children: e.children,
              length: 0
            };
            return o.mkStep(new o.Step(e.node, c, n, m));
          }

          var v = l.diffWithKeyAndIxE(e.children, r.value3, i.fst, function (n, t, r, u) {
            var a = o.step(r, u.value1);
            return l.insertChildIx(t, o.extract(a), e.node), a;
          }, function (n, e) {
            return o.halt(e);
          }, function (n, t, r) {
            var u = e.build(r.value1);
            return l.insertChildIx(t, o.extract(u), e.node), u;
          });
          a = o.step(e.attrs, r.value2), c = {
            build: e.build,
            node: e.node,
            attrs: a,
            ns: r.value0,
            name: r.value1,
            children: v,
            length: u
          };
          return o.mkStep(new o.Step(e.node, c, n, m));
        }

        return m(e), e.build(r);
      };

      e.buildVDom = function (n) {
        return function e(r) {
          if (r instanceof f.Text) return function (n, e, t) {
            var r = l.createTextNode(t, n.document),
                u = {
              build: e,
              node: r,
              value: t
            };
            return o.mkStep(new o.Step(r, u, p, d));
          }(n, e, r.value0);
          if (r instanceof f.Elem) return function (n, e, t, r, u, i) {
            var f = l.createElement(a.toNullable(t), r, n.document),
                v = c.toNode(f),
                s = l.forE(i, function (n, t) {
              var r = e(t);
              return l.insertChildIx(n, o.extract(r), v), r;
            }),
                d = n.buildAttributes(f)(u),
                p = {
              build: e,
              node: v,
              attrs: d,
              ns: t,
              name: r,
              children: s
            };
            return o.mkStep(new o.Step(v, p, E, h));
          }(n, e, r.value0, r.value1, r.value2, r.value3);
          if (r instanceof f.Keyed) return function (n, e, r, u, f, v) {
            var s = l.createElement(a.toNullable(r), u, n.document),
                d = c.toNode(s),
                p = l.strMapWithIxE(v, i.fst, function (n, t, r) {
              var u = e(r.value1);
              return l.insertChildIx(t, o.extract(u), d), u;
            }),
                h = n.buildAttributes(s)(f),
                w = {
              build: e,
              node: d,
              attrs: h,
              ns: r,
              name: u,
              children: p,
              length: t.length(v)
            };
            return o.mkStep(new o.Step(d, w, b, m));
          }(n, e, r.value0, r.value1, r.value2, r.value3);
          if (r instanceof f.Widget) return function (n, e, t) {
            var r = n.buildWidget(n)(t);
            return o.unStep(function (n) {
              return o.mkStep(new o.Step(n.value0, {
                build: e,
                widget: r
              }, s, v));
            })(r);
          }(n, e, r.value0);
          if (r instanceof f.Grafted) return e(f.runGraft(r.value0));
          throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [r.constructor.name]);
        };
      };
    }(e), function (n) {
      "use strict";

      n["Halogen.VDom.Thunk"] = n["Halogen.VDom.Thunk"] || {};
      var e = n["Halogen.VDom.Thunk"],
          t = n["Halogen.VDom.DOM"],
          r = n["Halogen.VDom.Machine"],
          u = n["Halogen.VDom.Util"],
          a = (function () {
        function n(n, e, t, r) {
          this.value0 = n, this.value1 = e, this.value2 = t, this.value3 = r;
        }

        n.create = function (e) {
          return function (t) {
            return function (r) {
              return function (u) {
                return new n(e, t, r, u);
              };
            };
          };
        };
      }(), function (n) {
        return n.value2(n.value3);
      });

      e.buildThunk = function (n) {
        var e = function e(n) {
          return r.halt(n.vdom);
        },
            i = function t(i, o) {
          var f, l;
          if (f = i.thunk, l = o, u.refEq(f.value0, l.value0) && u.refEq(f.value1, l.value1) && u.refEq(f.value3, l.value3)) return r.mkStep(new r.Step(r.extract(i.vdom), i, t, e));
          var c = r.step(i.vdom, n(a(o)));
          return r.mkStep(new r.Step(r.extract(c), {
            vdom: c,
            thunk: o
          }, t, e));
        };

        return function (u) {
          return function (o) {
            var f = t.buildVDom(u)(n(a(o)));
            return r.mkStep(new r.Step(r.extract(f), {
              thunk: o,
              vdom: f
            }, i, e));
          };
        };
      };
    }(e), function (n) {
      "use strict";

      var e = function e(n) {
        return function (e) {
          return function () {
            return e[n];
          };
        };
      };

      n._parentNode = e("parentNode"), n._nextSibling = e("nextSibling"), n.insertBefore = function (n) {
        return function (e) {
          return function (t) {
            return function () {
              return t.insertBefore(n, e);
            };
          };
        };
      }, n.appendChild = function (n) {
        return function (e) {
          return function () {
            return e.appendChild(n);
          };
        };
      }, n.removeChild = function (n) {
        return function (e) {
          return function () {
            return e.removeChild(n);
          };
        };
      };
    }(e["Web.DOM.Node"] = e["Web.DOM.Node"] || {}), function (n) {
      "use strict";

      n["Web.DOM.Node"] = n["Web.DOM.Node"] || {};
      var e,
          t,
          r = n["Web.DOM.Node"],
          u = n["Web.DOM.Node"],
          a = n["Data.Functor"],
          i = n["Data.Nullable"],
          o = n.Effect,
          f = (e = a.map(o.functorEffect)(i.toMaybe), function (n) {
        return e(u._parentNode(n));
      }),
          l = (t = a.map(o.functorEffect)(i.toMaybe), function (n) {
        return t(u._nextSibling(n));
      });
      r.parentNode = f, r.nextSibling = l, r.insertBefore = u.insertBefore, r.appendChild = u.appendChild, r.removeChild = u.removeChild;
    }(e), function (n) {
      "use strict";

      n["Halogen.VDom.Driver"] = n["Halogen.VDom.Driver"] || {};

      var e = n["Halogen.VDom.Driver"],
          t = n["Control.Applicative"],
          r = n["Control.Bind"],
          u = n["Control.Category"],
          a = n["Data.Foldable"],
          i = n["Data.Functor"],
          o = n["Data.HeytingAlgebra"],
          f = n["Data.Maybe"],
          l = n["Data.Newtype"],
          c = n["Data.Unit"],
          v = n.Effect,
          s = n["Effect.Aff"],
          d = n["Effect.Class"],
          p = n["Effect.Ref"],
          m = n["Halogen.Aff.Driver"],
          h = n["Halogen.Aff.Driver.State"],
          w = n["Halogen.Component"],
          E = n["Halogen.HTML.Core"],
          b = n["Halogen.VDom.DOM"],
          y = n["Halogen.VDom.DOM.Prop"],
          g = n["Halogen.VDom.Machine"],
          A = n["Halogen.VDom.Thunk"],
          D = n["Unsafe.Reference"],
          C = n["Web.DOM.Node"],
          M = n["Web.HTML"],
          F = n["Web.HTML.HTMLDocument"],
          T = n["Web.HTML.HTMLElement"],
          H = n["Web.HTML.Window"],
          L = function L(n) {
        return function () {
          var e = C.parentNode(n.node)();
          return a.traverse_(v.applicativeEffect)(a.foldableMaybe)(function (e) {
            return C.removeChild(n.node)(e);
          })(e)();
        };
      },
          S = function S(n) {
        return function (e) {
          return {
            render: function render(r) {
              return function (u) {
                return function (a) {
                  return function (s) {
                    if (s instanceof f.Nothing) return function () {
                      var t = p.new(u)(),
                          o = function (n) {
                        return function (e) {
                          return function (t) {
                            var r = h.unRenderStateX(function (n) {
                              return n.node;
                            }),
                                u = function u(n) {
                              return n instanceof f.Just ? g.halt(n.value0) : c.unit;
                            };

                            return {
                              buildWidget: function buildWidget(n) {
                                var t = A.buildThunk(l.unwrap(E.newtypeHTML))(n),
                                    a = function a(n) {
                                  var t = p.read(e)()(n)(),
                                      a = r(t);
                                  return g.mkStep(new g.Step(a, f.Nothing.value, o, u));
                                },
                                    i = function i(n) {
                                  if (n instanceof w.ComponentSlot) return a(n.value0);

                                  if (n instanceof w.ThunkSlot) {
                                    var e = t(n.value0);
                                    return g.mkStep(new g.Step(g.extract(e), new f.Just(e), o, u));
                                  }

                                  throw new Error("Failed pattern match at Halogen.VDom.Driver (line 85, column 7 - line 90, column 75): " + [n.constructor.name]);
                                },
                                    o = function n(e, t) {
                                  if (e instanceof f.Just) {
                                    if (t instanceof w.ComponentSlot) return g.halt(e.value0), a(t.value0);

                                    if (t instanceof w.ThunkSlot) {
                                      var r = g.step(e.value0, t.value0);
                                      return g.mkStep(new g.Step(g.extract(r), new f.Just(r), n, u));
                                    }

                                    throw new Error("Failed pattern match at Halogen.VDom.Driver (line 98, column 22 - line 104, column 79): " + [t.constructor.name]);
                                  }

                                  return i(t);
                                };

                                return i;
                              },
                              buildAttributes: y.buildProp(n),
                              document: t
                            };
                          };
                        };
                      }(r)(t)(n),
                          s = b.buildVDom(o)(a),
                          d = g.extract(s);

                      return i.void(v.functorEffect)(C.appendChild(d)(T.toNode(e)))(), {
                        machine: s,
                        node: d,
                        renderChildRef: t
                      };
                    };
                    if (s instanceof f.Just) return function () {
                      p.write(u)(s.value0.renderChildRef)();
                      var n = C.parentNode(s.value0.node)(),
                          e = C.nextSibling(s.value0.node)(),
                          r = g.step(s.value0.machine, a),
                          l = g.extract(r);
                      return t.when(v.applicativeEffect)(o.not(o.heytingAlgebraFunction(o.heytingAlgebraFunction(o.heytingAlgebraBoolean)))(D.unsafeRefEq)(s.value0.node)(l))(function (n) {
                        return function (e) {
                          return function (r) {
                            return e instanceof f.Just && r instanceof f.Just ? i.void(v.functorEffect)(C.insertBefore(n)(e.value0)(r.value0)) : e instanceof f.Nothing && r instanceof f.Just ? i.void(v.functorEffect)(C.appendChild(n)(r.value0)) : t.pure(v.applicativeEffect)(c.unit);
                          };
                        };
                      }(l)(e)(n))(), {
                        machine: r,
                        node: l,
                        renderChildRef: s.value0.renderChildRef
                      };
                    };
                    throw new Error("Failed pattern match at Halogen.VDom.Driver (line 159, column 5 - line 175, column 80): " + [s.constructor.name]);
                  };
                };
              };
            },
            renderChild: u.identity(u.categoryFn),
            removeChild: L,
            dispose: L
          };
        };
      };

      e.runUI = function (n) {
        return function (e) {
          return function (t) {
            return r.bind(s.bindAff)(d.liftEffect(s.monadEffectAff)(i.map(v.functorEffect)(F.toDocument)(r.bindFlipped(v.bindEffect)(H.document)(M.window))))(function (r) {
              return m.runUI(S(r)(t))(n)(e);
            });
          };
        };
      };
    }(e), function (n) {
      "use strict";

      n.Main = n.Main || {};

      var e = n.Main,
          t = n["Control.Bind"],
          r = n["Control.Monad.State.Class"],
          u = n["DOM.HTML.Indexed.ButtonType"],
          a = n["Data.Function"],
          i = n["Data.Maybe"],
          o = n["Data.Unit"],
          f = n["Effect.Aff"],
          l = n["Halogen.Aff.Util"],
          c = n["Halogen.Component"],
          v = n["Halogen.HTML.Core"],
          s = n["Halogen.HTML.Elements"],
          d = n["Halogen.HTML.Events"],
          p = n["Halogen.HTML.Properties"],
          m = n["Halogen.Query.HalogenM"],
          h = n["Halogen.VDom.Driver"],
          w = function () {
        function n() {}

        return n.value = new n(), n;
      }(),
          E = function E(n) {
        var e = n ? "ON" : "OFF";
        return s.button([p.type_(v.isPropButtonType)(u.ButtonButton.value), p.class_("button"), d.onClick(function (n) {
          return new i.Just(w.value);
        })])([v.text("The button is " + e)]);
      },
          b = function b(n) {
        return c.mkComponent({
          initialState: a.const(n.initialState),
          render: n.render,
          eval: c.mkEval({
            handleAction: n.handleAction,
            handleQuery: c.defaultEval.handleQuery,
            receive: c.defaultEval.receive,
            initialize: c.defaultEval.initialize,
            finalize: c.defaultEval.finalize
          })
        });
      },
          y = function y(n) {
        return l.runHalogenAff(t.bind(f.bindAff)(l.awaitBody)(function (e) {
          return h.runUI(b(n))(o.unit)(e);
        }));
      },
          g = function g(n) {
        return t.bind(m.bindHalogenM)(r.get(m.monadStateHalogenM))(function (n) {
          var e = !n;
          return r.put(m.monadStateHalogenM)(e);
        });
      },
          A = y({
        initialState: !1,
        render: E,
        handleAction: g
      });

      e.Toggle = w, e.toggleButton = E, e.handleAction = g, e.main = A, e.runStateAndActionComponent = y, e.stateAndActionCompontent = b;
    }(e), e.Main.main();
  }, {}]
}, {}, ["EcpK"], null);
},{}],"../../../../../Users/pietr/AppData/Roaming/npm/node_modules/parcel/src/builtins/hmr-runtime.js":[function(require,module,exports) {
var global = arguments[3];
var OVERLAY_ID = '__parcel__error__overlay__';
var OldModule = module.bundle.Module;

function Module(moduleName) {
  OldModule.call(this, moduleName);
  this.hot = {
    data: module.bundle.hotData,
    _acceptCallbacks: [],
    _disposeCallbacks: [],
    accept: function (fn) {
      this._acceptCallbacks.push(fn || function () {});
    },
    dispose: function (fn) {
      this._disposeCallbacks.push(fn);
    }
  };
  module.bundle.hotData = null;
}

module.bundle.Module = Module;
var checkedAssets, assetsToAccept;
var parent = module.bundle.parent;

if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== 'undefined') {
  var hostname = "" || location.hostname;
  var protocol = location.protocol === 'https:' ? 'wss' : 'ws';
  var ws = new WebSocket(protocol + '://' + hostname + ':' + "62524" + '/');

  ws.onmessage = function (event) {
    checkedAssets = {};
    assetsToAccept = [];
    var data = JSON.parse(event.data);

    if (data.type === 'update') {
      var handled = false;
      data.assets.forEach(function (asset) {
        if (!asset.isNew) {
          var didAccept = hmrAcceptCheck(global.parcelRequire, asset.id);

          if (didAccept) {
            handled = true;
          }
        }
      }); // Enable HMR for CSS by default.

      handled = handled || data.assets.every(function (asset) {
        return asset.type === 'css' && asset.generated.js;
      });

      if (handled) {
        console.clear();
        data.assets.forEach(function (asset) {
          hmrApply(global.parcelRequire, asset);
        });
        assetsToAccept.forEach(function (v) {
          hmrAcceptRun(v[0], v[1]);
        });
      } else if (location.reload) {
        // `location` global exists in a web worker context but lacks `.reload()` function.
        location.reload();
      }
    }

    if (data.type === 'reload') {
      ws.close();

      ws.onclose = function () {
        location.reload();
      };
    }

    if (data.type === 'error-resolved') {
      console.log('[parcel] ✨ Error resolved');
      removeErrorOverlay();
    }

    if (data.type === 'error') {
      console.error('[parcel] 🚨  ' + data.error.message + '\n' + data.error.stack);
      removeErrorOverlay();
      var overlay = createErrorOverlay(data);
      document.body.appendChild(overlay);
    }
  };
}

function removeErrorOverlay() {
  var overlay = document.getElementById(OVERLAY_ID);

  if (overlay) {
    overlay.remove();
  }
}

function createErrorOverlay(data) {
  var overlay = document.createElement('div');
  overlay.id = OVERLAY_ID; // html encode message and stack trace

  var message = document.createElement('div');
  var stackTrace = document.createElement('pre');
  message.innerText = data.error.message;
  stackTrace.innerText = data.error.stack;
  overlay.innerHTML = '<div style="background: black; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; opacity: 0.85; font-family: Menlo, Consolas, monospace; z-index: 9999;">' + '<span style="background: red; padding: 2px 4px; border-radius: 2px;">ERROR</span>' + '<span style="top: 2px; margin-left: 5px; position: relative;">🚨</span>' + '<div style="font-size: 18px; font-weight: bold; margin-top: 20px;">' + message.innerHTML + '</div>' + '<pre>' + stackTrace.innerHTML + '</pre>' + '</div>';
  return overlay;
}

function getParents(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return [];
  }

  var parents = [];
  var k, d, dep;

  for (k in modules) {
    for (d in modules[k][1]) {
      dep = modules[k][1][d];

      if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) {
        parents.push(k);
      }
    }
  }

  if (bundle.parent) {
    parents = parents.concat(getParents(bundle.parent, id));
  }

  return parents;
}

function hmrApply(bundle, asset) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (modules[asset.id] || !bundle.parent) {
    var fn = new Function('require', 'module', 'exports', asset.generated.js);
    asset.isNew = !modules[asset.id];
    modules[asset.id] = [fn, asset.deps];
  } else if (bundle.parent) {
    hmrApply(bundle.parent, asset);
  }
}

function hmrAcceptCheck(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (!modules[id] && bundle.parent) {
    return hmrAcceptCheck(bundle.parent, id);
  }

  if (checkedAssets[id]) {
    return;
  }

  checkedAssets[id] = true;
  var cached = bundle.cache[id];
  assetsToAccept.push([bundle, id]);

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    return true;
  }

  return getParents(global.parcelRequire, id).some(function (id) {
    return hmrAcceptCheck(global.parcelRequire, id);
  });
}

function hmrAcceptRun(bundle, id) {
  var cached = bundle.cache[id];
  bundle.hotData = {};

  if (cached) {
    cached.hot.data = bundle.hotData;
  }

  if (cached && cached.hot && cached.hot._disposeCallbacks.length) {
    cached.hot._disposeCallbacks.forEach(function (cb) {
      cb(bundle.hotData);
    });
  }

  delete bundle.cache[id];
  bundle(id);
  cached = bundle.cache[id];

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    cached.hot._acceptCallbacks.forEach(function (cb) {
      cb();
    });

    return true;
  }
}
},{}]},{},["../../../../../Users/pietr/AppData/Roaming/npm/node_modules/parcel/src/builtins/hmr-runtime.js","app.14db2375.js"], null)
//# sourceMappingURL=/app.14db2375.72f979cc.js.map