var PS = PS || {};
PS.Prelude = (function () {
    "use strict";
    var Unit = {
        create: function (value) {
            return value;
        }
    };
    function LT() {

    };
    LT.value = new LT();
    function GT() {

    };
    GT.value = new GT();
    function EQ() {

    };
    EQ.value = new EQ();
    function Semigroupoid($less$less$less) {
        this["<<<"] = $less$less$less;
    };
    function Category(__superclass_Prelude$dotSemigroupoid_0, id) {
        this["__superclass_Prelude.Semigroupoid_0"] = __superclass_Prelude$dotSemigroupoid_0;
        this.id = id;
    };
    function Show(show) {
        this.show = show;
    };
    function Functor($less$dollar$greater) {
        this["<$>"] = $less$dollar$greater;
    };
    function Apply($less$times$greater, __superclass_Prelude$dotFunctor_0) {
        this["<*>"] = $less$times$greater;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
    };
    function Applicative(__superclass_Prelude$dotApply_0, pure) {
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
        this.pure = pure;
    };
    function Bind($greater$greater$eq, __superclass_Prelude$dotApply_0) {
        this[">>="] = $greater$greater$eq;
        this["__superclass_Prelude.Apply_0"] = __superclass_Prelude$dotApply_0;
    };
    function Monad(__superclass_Prelude$dotApplicative_0, __superclass_Prelude$dotBind_1) {
        this["__superclass_Prelude.Applicative_0"] = __superclass_Prelude$dotApplicative_0;
        this["__superclass_Prelude.Bind_1"] = __superclass_Prelude$dotBind_1;
    };
    function Num($percent, $times, $plus, $minus, $div, negate) {
        this["%"] = $percent;
        this["*"] = $times;
        this["+"] = $plus;
        this["-"] = $minus;
        this["/"] = $div;
        this.negate = negate;
    };
    function Eq($div$eq, $eq$eq) {
        this["/="] = $div$eq;
        this["=="] = $eq$eq;
    };
    function Ord(__superclass_Prelude$dotEq_0, compare) {
        this["__superclass_Prelude.Eq_0"] = __superclass_Prelude$dotEq_0;
        this.compare = compare;
    };
    function BoolLike($amp$amp, not, $bar$bar) {
        this["&&"] = $amp$amp;
        this.not = not;
        this["||"] = $bar$bar;
    };
    function Semigroup($less$greater) {
        this["<>"] = $less$greater;
    };
    function cons(e) {  return function (l) {    return [e].concat(l);  };};
    function showNumberImpl(n) {  return n.toString();};
    function numAdd(n1) {  return function(n2) {    return n1 + n2;  };};
    function numSub(n1) {  return function(n2) {    return n1 - n2;  };};
    function numMul(n1) {  return function(n2) {    return n1 * n2;  };};
    function numDiv(n1) {  return function(n2) {    return n1 / n2;  };};
    function numMod(n1) {  return function(n2) {    return n1 % n2;  };};
    function numNegate(n) {  return -n;};
    function refEq(r1) {  return function(r2) {    return r1 === r2;  };};
    function refIneq(r1) {  return function(r2) {    return r1 !== r2;  };};
    function unsafeCompareImpl(lt) {  return function (eq) {    return function (gt) {      return function (x) {        return function (y) {          return x < y ? lt : x > y ? gt : eq;        };      };    };  };};
    function boolAnd(b1) {  return function(b2) {    return b1 && b2;  };};
    function boolOr(b1) {  return function(b2) {    return b1 || b2;  };};
    function boolNot(b) {  return !b;};
    function concatString(s1) {  return function(s2) {    return s1 + s2;  };};
    var $greater$greater$eq = function (dict) {
        return dict[">>="];
    };
    var $eq$eq = function (dict) {
        return dict["=="];
    };
    var $less$greater = function (dict) {
        return dict["<>"];
    };
    var $less$less$less = function (dict) {
        return dict["<<<"];
    };
    var $less$times$greater = function (dict) {
        return dict["<*>"];
    };
    var $less$dollar$greater = function (dict) {
        return dict["<$>"];
    };
    var $colon = cons;
    var $plus$plus = function (__dict_Semigroup_1) {
        return $less$greater(__dict_Semigroup_1);
    };
    var $plus = function (dict) {
        return dict["+"];
    };
    var $amp$amp = function (dict) {
        return dict["&&"];
    };
    var $dollar = function (f) {
        return function (x) {
            return f(x);
        };
    };
    var unsafeCompare = unsafeCompareImpl(LT.value)(EQ.value)(GT.value);
    var unit = {};
    var showNumber = function (_) {
        return new Show(showNumberImpl);
    };
    var show = function (dict) {
        return dict.show;
    };
    var semigroupoidArr = function (_) {
        return new Semigroupoid(function (f) {
            return function (g) {
                return function (x) {
                    return f(g(x));
                };
            };
        });
    };
    var semigroupString = function (_) {
        return new Semigroup(concatString);
    };
    var pure = function (dict) {
        return dict.pure;
    };
    var $$return = function (__dict_Monad_4) {
        return pure(__dict_Monad_4["__superclass_Prelude.Applicative_0"]({}));
    };
    var numNumber = function (_) {
        return new Num(numMod, numMul, numAdd, numSub, numDiv, numNegate);
    };
    var liftA1 = function (__dict_Applicative_6) {
        return function (f) {
            return function (a) {
                return $less$times$greater(__dict_Applicative_6["__superclass_Prelude.Apply_0"]({}))(pure(__dict_Applicative_6)(f))(a);
            };
        };
    };
    var id = function (dict) {
        return dict.id;
    };
    var flip = function (f) {
        return function (b) {
            return function (a) {
                return f(a)(b);
            };
        };
    };
    var eqString = function (_) {
        return new Eq(refIneq, refEq);
    };
    var ordString = function (_) {
        return new Ord(function (__1) {
            return eqString({});
        }, unsafeCompare);
    };
    var eqNumber = function (_) {
        return new Eq(refIneq, refEq);
    };
    var ordNumber = function (_) {
        return new Ord(function (__1) {
            return eqNumber({});
        }, unsafeCompare);
    };
    var compare = function (dict) {
        return dict.compare;
    };
    var $less = function (__dict_Ord_10) {
        return function (a1) {
            return function (a2) {
                var _331 = compare(__dict_Ord_10)(a1)(a2);
                if (_331 instanceof LT) {
                    return true;
                };
                return false;
            };
        };
    };
    var $less$eq = function (__dict_Ord_11) {
        return function (a1) {
            return function (a2) {
                var _332 = compare(__dict_Ord_11)(a1)(a2);
                if (_332 instanceof GT) {
                    return false;
                };
                return true;
            };
        };
    };
    var $greater$eq = function (__dict_Ord_13) {
        return function (a1) {
            return function (a2) {
                var _333 = compare(__dict_Ord_13)(a1)(a2);
                if (_333 instanceof LT) {
                    return false;
                };
                return true;
            };
        };
    };
    var categoryArr = function (_) {
        return new Category(function (__1) {
            return semigroupoidArr({});
        }, function (x) {
            return x;
        });
    };
    var boolLikeBoolean = function (_) {
        return new BoolLike(boolAnd, boolNot, boolOr);
    };
    var ap = function (__dict_Monad_14) {
        return function (f) {
            return function (a) {
                return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]({}))(f)(function (_2) {
                    return $greater$greater$eq(__dict_Monad_14["__superclass_Prelude.Bind_1"]({}))(a)(function (_1) {
                        return $$return(__dict_Monad_14)(_2(_1));
                    });
                });
            };
        };
    };
    return {
        Unit: Unit, 
        LT: LT, 
        GT: GT, 
        EQ: EQ, 
        Semigroup: Semigroup, 
        BoolLike: BoolLike, 
        Ord: Ord, 
        Eq: Eq, 
        Num: Num, 
        Monad: Monad, 
        Bind: Bind, 
        Applicative: Applicative, 
        Apply: Apply, 
        Functor: Functor, 
        Show: Show, 
        Category: Category, 
        Semigroupoid: Semigroupoid, 
        unit: unit, 
        "++": $plus$plus, 
        "<>": $less$greater, 
        "&&": $amp$amp, 
        ">=": $greater$eq, 
        "<=": $less$eq, 
        "<": $less, 
        compare: compare, 
        refIneq: refIneq, 
        refEq: refEq, 
        "==": $eq$eq, 
        "+": $plus, 
        ap: ap, 
        "return": $$return, 
        ">>=": $greater$greater$eq, 
        liftA1: liftA1, 
        pure: pure, 
        "<*>": $less$times$greater, 
        "<$>": $less$dollar$greater, 
        show: show, 
        cons: cons, 
        ":": $colon, 
        "$": $dollar, 
        id: id, 
        "<<<": $less$less$less, 
        flip: flip, 
        semigroupoidArr: semigroupoidArr, 
        categoryArr: categoryArr, 
        showNumber: showNumber, 
        numNumber: numNumber, 
        eqString: eqString, 
        eqNumber: eqNumber, 
        ordNumber: ordNumber, 
        ordString: ordString, 
        boolLikeBoolean: boolLikeBoolean, 
        semigroupString: semigroupString
    };
})();
var PS = PS || {};
PS.Data_String = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function charAt(i) {  return function(s) {    return s.charAt(i);   };};
    function length(s) {  return s.length;};
    function toLower(s) {  return s.toLowerCase();};
    return {
        toLower: toLower, 
        length: length, 
        charAt: charAt
    };
})();
var PS = PS || {};
PS.Data_Maybe = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Nothing() {

    };
    Nothing.value = new Nothing();
    function Just(value0) {
        this.value0 = value0;
    };
    Just.create = function (value0) {
        return new Just(value0);
    };
    var maybe = function (_50) {
        return function (_51) {
            return function (_52) {
                if (_52 instanceof Nothing) {
                    return _50;
                };
                if (_52 instanceof Just) {
                    return _51(_52.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var functorMaybe = function (_) {
        return new Prelude.Functor(function (_53) {
            return function (_54) {
                if (_54 instanceof Just) {
                    return new Just(_53(_54.value0));
                };
                return Nothing.value;
            };
        });
    };
    var fromMaybe = function (a) {
        return maybe(a)(Prelude.id(Prelude.categoryArr({})));
    };
    var applyMaybe = function (_) {
        return new Prelude.Apply(function (_55) {
            return function (_56) {
                if (_55 instanceof Just) {
                    return Prelude["<$>"](functorMaybe({}))(_55.value0)(_56);
                };
                if (_55 instanceof Nothing) {
                    return Nothing.value;
                };
                throw new Error("Failed pattern match");
            };
        }, function (__1) {
            return functorMaybe({});
        });
    };
    var bindMaybe = function (_) {
        return new Prelude.Bind(function (_59) {
            return function (_60) {
                if (_59 instanceof Just) {
                    return _60(_59.value0);
                };
                if (_59 instanceof Nothing) {
                    return Nothing.value;
                };
                throw new Error("Failed pattern match");
            };
        }, function (__1) {
            return applyMaybe({});
        });
    };
    return {
        Nothing: Nothing, 
        Just: Just, 
        fromMaybe: fromMaybe, 
        maybe: maybe, 
        functorMaybe: functorMaybe, 
        applyMaybe: applyMaybe, 
        bindMaybe: bindMaybe
    };
})();
var PS = PS || {};
PS.Data_Function = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function runFn2(fn) {  return function(a) {    return function(b) {      return fn(a, b);    };  };};
    function runFn3(fn) {  return function(a) {    return function(b) {      return function(c) {        return fn(a, b, c);      };    };  };};
    function runFn4(fn) {  return function(a) {    return function(b) {      return function(c) {        return function(d) {          return fn(a, b, c, d);        };      };    };  };};
    return {
        runFn4: runFn4, 
        runFn3: runFn3, 
        runFn2: runFn2
    };
})();
var PS = PS || {};
PS.Data_Either = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function Left(value0) {
        this.value0 = value0;
    };
    Left.create = function (value0) {
        return new Left(value0);
    };
    function Right(value0) {
        this.value0 = value0;
    };
    Right.create = function (value0) {
        return new Right(value0);
    };
    var functorEither = function (_) {
        return new Prelude.Functor(function (_75) {
            return function (_76) {
                if (_76 instanceof Left) {
                    return new Left(_76.value0);
                };
                if (_76 instanceof Right) {
                    return new Right(_75(_76.value0));
                };
                throw new Error("Failed pattern match");
            };
        });
    };
    var either = function (_72) {
        return function (_73) {
            return function (_74) {
                if (_74 instanceof Left) {
                    return _72(_74.value0);
                };
                if (_74 instanceof Right) {
                    return _73(_74.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var applyEither = function (_) {
        return new Prelude.Apply(function (_77) {
            return function (_78) {
                if (_77 instanceof Left) {
                    return new Left(_77.value0);
                };
                if (_77 instanceof Right) {
                    return Prelude["<$>"](functorEither({}))(_77.value0)(_78);
                };
                throw new Error("Failed pattern match");
            };
        }, function (__1) {
            return functorEither({});
        });
    };
    var bindEither = function (_) {
        return new Prelude.Bind(either(function (e) {
            return function (__1) {
                return new Left(e);
            };
        })(function (a) {
            return function (f) {
                return f(a);
            };
        }), function (__1) {
            return applyEither({});
        });
    };
    var applicativeEither = function (_) {
        return new Prelude.Applicative(function (__1) {
            return applyEither({});
        }, Right.create);
    };
    return {
        Left: Left, 
        Right: Right, 
        either: either, 
        functorEither: functorEither, 
        applyEither: applyEither, 
        applicativeEither: applicativeEither, 
        bindEither: bindEither
    };
})();
var PS = PS || {};
PS.Data_Array = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function length (xs) {  return xs.length;};
    function append (l1) {  return function (l2) {    return l1.concat(l2);  };};
    function slice (s) {  return function (e) {    return function (l) {      return l.slice(s, e);    };  };};
    function concatMap (f) {  return function (arr) {    var result = [];    for (var i = 0, l = arr.length; i < l; i++) {      Array.prototype.push.apply(result, f(arr[i]));    }    return result;  };};
    function map (f) {  return function (arr) {    var l = arr.length;    var result = new Array(l);    for (var i = 0; i < l; i++) {      result[i] = f(arr[i]);    }    return result;  };};
    function range (start) {  return function (end) {    var i = ~~start, e = ~~end;    var step = i > e ? -1 : 1;    var result = [i], n = 1;    while (i !== e) {      i += step;      result[n++] = i;    }    return result;  };};
    function zipWith (f) {  return function (xs) {    return function (ys) {      var l = xs.length < ys.length ? xs.length : ys.length;      var result = new Array(l);      for (var i = 0; i < l; i++) {        result[i] = f(xs[i])(ys[i]);      }      return result;    };  };};
    var take = function (n) {
        return slice(0)(n);
    };
    var semigroupArray = function (_) {
        return new Prelude.Semigroup(append);
    };
    var functorArray = function (_) {
        return new Prelude.Functor(map);
    };
    return {
        zipWith: zipWith, 
        range: range, 
        concatMap: concatMap, 
        take: take, 
        append: append, 
        length: length, 
        map: map, 
        functorArray: functorArray, 
        semigroupArray: semigroupArray
    };
})();
var PS = PS || {};
PS.Data_Monoid = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var mempty = function (dict) {
        return dict.mempty;
    };
    return {
        mempty: mempty
    };
})();
var PS = PS || {};
PS.Data_Tuple = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    var zip = Data_Array.zipWith(Tuple.create);
    return {
        Tuple: Tuple, 
        zip: zip
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function returnE(a) {  return function() {    return a;  };};
    function bindE(a) {  return function(f) {    return function() {      return f(a())();    };  };};
    function foreachE(as) {  return function(f) {    return function() {      for (var i = 0; i < as.length; i++) {        f(as[i])();      }    };  };};
    var applicativeEff = function (_) {
        return new Prelude.Applicative(function (__1) {
            return applyEff({});
        }, returnE);
    };
    var applyEff = function (_) {
        return new Prelude.Apply(Prelude.ap(monadEff({})), function (__1) {
            return functorEff({});
        });
    };
    var monadEff = function (_) {
        return new Prelude.Monad(function (__1) {
            return applicativeEff({});
        }, function (__1) {
            return bindEff({});
        });
    };
    var bindEff = function (_) {
        return new Prelude.Bind(bindE, function (__1) {
            return applyEff({});
        });
    };
    var functorEff = function (_) {
        return new Prelude.Functor(Prelude.liftA1(applicativeEff({})));
    };
    return {
        foreachE: foreachE, 
        bindE: bindE, 
        returnE: returnE, 
        functorEff: functorEff, 
        applyEff: applyEff, 
        applicativeEff: applicativeEff, 
        bindEff: bindEff, 
        monadEff: monadEff
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff_AJAX = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function get(uri) {  return function(k) {    return function() {      var req = new XMLHttpRequest();      req.onreadystatechange = function() {        if (req.readyState === 4 && req.status === 200) {          k(req.responseText)();        }      };      req.open('GET', uri, true);      req.send();    };  };};
    return {
        get: get
    };
})();
var PS = PS || {};
PS.Data_Foldable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    function Foldable(foldMap, foldl, foldr) {
        this.foldMap = foldMap;
        this.foldl = foldl;
        this.foldr = foldr;
    };
    function foldrArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = xs.length - 1; i >= 0; --i) {        acc = f(xs[i])(acc);      }      return acc;    }  }};
    function foldlArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = 0, len = xs.length; i < len; ++i) {        acc = f(acc)(xs[i]);      }      return acc;    }  }};
    var foldr = function (dict) {
        return dict.foldr;
    };
    var foldl = function (dict) {
        return dict.foldl;
    };
    var foldableArray = function (_) {
        return new Foldable(function (__dict_Monoid_86) {
            return function (f) {
                return function (xs) {
                    return foldr(foldableArray({}))(function (x) {
                        return function (acc) {
                            return Prelude["<>"](__dict_Monoid_86["__superclass_Prelude.Semigroup_0"]({}))(f(x))(acc);
                        };
                    })(Data_Monoid.mempty(__dict_Monoid_86))(xs);
                };
            };
        }, function (f) {
            return function (z) {
                return function (xs) {
                    return foldlArray(f)(z)(xs);
                };
            };
        }, function (f) {
            return function (z) {
                return function (xs) {
                    return foldrArray(f)(z)(xs);
                };
            };
        });
    };
    return {
        Foldable: Foldable, 
        foldlArray: foldlArray, 
        foldrArray: foldrArray, 
        foldl: foldl, 
        foldr: foldr, 
        foldableArray: foldableArray
    };
})();
var PS = PS || {};
PS.Data_Map = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Maybe = PS.Data_Maybe;
    function Leaf() {

    };
    Leaf.value = new Leaf();
    function Two(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    Two.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new Two(value0, value1, value2, value3);
                };
            };
        };
    };
    function Three(value0, value1, value2, value3, value4, value5, value6) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
        this.value6 = value6;
    };
    Three.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return function (value6) {
                                return new Three(value0, value1, value2, value3, value4, value5, value6);
                            };
                        };
                    };
                };
            };
        };
    };
    function TwoLeft(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoLeft(value0, value1, value2);
            };
        };
    };
    function TwoRight(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TwoRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TwoRight(value0, value1, value2);
            };
        };
    };
    function ThreeLeft(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeLeft.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeLeft(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    function ThreeMiddle(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeMiddle.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeMiddle(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    function ThreeRight(value0, value1, value2, value3, value4, value5) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
        this.value4 = value4;
        this.value5 = value5;
    };
    ThreeRight.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return function (value4) {
                        return function (value5) {
                            return new ThreeRight(value0, value1, value2, value3, value4, value5);
                        };
                    };
                };
            };
        };
    };
    function KickUp(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    KickUp.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new KickUp(value0, value1, value2, value3);
                };
            };
        };
    };
    var toList = function (_243) {
        if (_243 instanceof Leaf) {
            return [  ];
        };
        if (_243 instanceof Two) {
            return Prelude["++"](Data_Array.semigroupArray({}))(toList(_243.value0))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_243.value1, _243.value2) ])(toList(_243.value3)));
        };
        if (_243 instanceof Three) {
            return Prelude["++"](Data_Array.semigroupArray({}))(toList(_243.value0))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_243.value1, _243.value2) ])(Prelude["++"](Data_Array.semigroupArray({}))(toList(_243.value3))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_243.value4, _243.value5) ])(toList(_243.value6)))));
        };
        throw new Error("Failed pattern match");
    };
    var lookup = function (__copy___dict_Ord_103) {
        return function (__copy__239) {
            return function (__copy__240) {
                var __dict_Ord_103 = __copy___dict_Ord_103;
                var _239 = __copy__239;
                var _240 = __copy__240;
                tco: while (true) {
                    if (_240 instanceof Leaf) {
                        return Data_Maybe.Nothing.value;
                    };
                    if (_240 instanceof Two && Prelude["=="](__dict_Ord_103["__superclass_Prelude.Eq_0"]({}))(_239)(_240.value1)) {
                        return new Data_Maybe.Just(_240.value2);
                    };
                    if (_240 instanceof Two && Prelude["<"](__dict_Ord_103)(_239)(_240.value1)) {
                        var __tco___dict_Ord_103 = __dict_Ord_103;
                        var __tco__239 = _239;
                        var __tco__240 = _240.value0;
                        __dict_Ord_103 = __tco___dict_Ord_103;
                        _239 = __tco__239;
                        _240 = __tco__240;
                        continue tco;
                    };
                    if (_240 instanceof Two) {
                        var __tco___dict_Ord_103 = __dict_Ord_103;
                        var __tco__239 = _239;
                        var __tco__240 = _240.value3;
                        __dict_Ord_103 = __tco___dict_Ord_103;
                        _239 = __tco__239;
                        _240 = __tco__240;
                        continue tco;
                    };
                    if (_240 instanceof Three && Prelude["=="](__dict_Ord_103["__superclass_Prelude.Eq_0"]({}))(_239)(_240.value1)) {
                        return new Data_Maybe.Just(_240.value2);
                    };
                    if (_240 instanceof Three && Prelude["=="](__dict_Ord_103["__superclass_Prelude.Eq_0"]({}))(_239)(_240.value4)) {
                        return new Data_Maybe.Just(_240.value5);
                    };
                    if (_240 instanceof Three && Prelude["<"](__dict_Ord_103)(_239)(_240.value1)) {
                        var __tco___dict_Ord_103 = __dict_Ord_103;
                        var __tco__239 = _239;
                        var __tco__240 = _240.value0;
                        __dict_Ord_103 = __tco___dict_Ord_103;
                        _239 = __tco__239;
                        _240 = __tco__240;
                        continue tco;
                    };
                    if (_240 instanceof Three && Prelude["<"](__dict_Ord_103)(_240.value1)(_239) && Prelude["<="](__dict_Ord_103)(_239)(_240.value4)) {
                        var __tco___dict_Ord_103 = __dict_Ord_103;
                        var __tco__239 = _239;
                        var __tco__240 = _240.value3;
                        __dict_Ord_103 = __tco___dict_Ord_103;
                        _239 = __tco__239;
                        _240 = __tco__240;
                        continue tco;
                    };
                    if (_240 instanceof Three) {
                        var __tco___dict_Ord_103 = __dict_Ord_103;
                        var __tco__239 = _239;
                        var __tco__240 = _240.value6;
                        __dict_Ord_103 = __tco___dict_Ord_103;
                        _239 = __tco__239;
                        _240 = __tco__240;
                        continue tco;
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
    };
    var fromZipper = function (__copy___dict_Ord_105) {
        return function (__copy__241) {
            return function (__copy__242) {
                var __dict_Ord_105 = __copy___dict_Ord_105;
                var _241 = __copy__241;
                var _242 = __copy__242;
                tco: while (true) {
                    if (_241.length === 0) {
                        return _242;
                    };
                    if (_241.length > 0) {
                        var _426 = _241.slice(1);
                        if (_241[0] instanceof TwoLeft) {
                            var __tco___dict_Ord_105 = __dict_Ord_105;
                            var __tco__242 = new Two(_242, (_241[0]).value0, (_241[0]).value1, (_241[0]).value2);
                            __dict_Ord_105 = __tco___dict_Ord_105;
                            _241 = _426;
                            _242 = __tco__242;
                            continue tco;
                        };
                    };
                    if (_241.length > 0) {
                        var _431 = _241.slice(1);
                        if (_241[0] instanceof TwoRight) {
                            var __tco___dict_Ord_105 = __dict_Ord_105;
                            var __tco__242 = new Two((_241[0]).value0, (_241[0]).value1, (_241[0]).value2, _242);
                            __dict_Ord_105 = __tco___dict_Ord_105;
                            _241 = _431;
                            _242 = __tco__242;
                            continue tco;
                        };
                    };
                    if (_241.length > 0) {
                        var _436 = _241.slice(1);
                        if (_241[0] instanceof ThreeLeft) {
                            var __tco___dict_Ord_105 = __dict_Ord_105;
                            var __tco__242 = new Three(_242, (_241[0]).value0, (_241[0]).value1, (_241[0]).value2, (_241[0]).value3, (_241[0]).value4, (_241[0]).value5);
                            __dict_Ord_105 = __tco___dict_Ord_105;
                            _241 = _436;
                            _242 = __tco__242;
                            continue tco;
                        };
                    };
                    if (_241.length > 0) {
                        var _444 = _241.slice(1);
                        if (_241[0] instanceof ThreeMiddle) {
                            var __tco___dict_Ord_105 = __dict_Ord_105;
                            var __tco__242 = new Three((_241[0]).value0, (_241[0]).value1, (_241[0]).value2, _242, (_241[0]).value3, (_241[0]).value4, (_241[0]).value5);
                            __dict_Ord_105 = __tco___dict_Ord_105;
                            _241 = _444;
                            _242 = __tco__242;
                            continue tco;
                        };
                    };
                    if (_241.length > 0) {
                        var _452 = _241.slice(1);
                        if (_241[0] instanceof ThreeRight) {
                            var __tco___dict_Ord_105 = __dict_Ord_105;
                            var __tco__242 = new Three((_241[0]).value0, (_241[0]).value1, (_241[0]).value2, (_241[0]).value3, (_241[0]).value4, (_241[0]).value5, _242);
                            __dict_Ord_105 = __tco___dict_Ord_105;
                            _241 = _452;
                            _242 = __tco__242;
                            continue tco;
                        };
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
    };
    var insert = function (__dict_Ord_106) {
        var up = function (__copy___dict_Ord_107) {
            return function (__copy__253) {
                return function (__copy__254) {
                    var __dict_Ord_107 = __copy___dict_Ord_107;
                    var _253 = __copy__253;
                    var _254 = __copy__254;
                    tco: while (true) {
                        if (_253.length === 0) {
                            return new Two(_254.value0, _254.value1, _254.value2, _254.value3);
                        };
                        if (_253.length > 0) {
                            var _470 = _253.slice(1);
                            if (_253[0] instanceof TwoLeft) {
                                return fromZipper(__dict_Ord_107)(_470)(new Three(_254.value0, _254.value1, _254.value2, _254.value3, (_253[0]).value0, (_253[0]).value1, (_253[0]).value2));
                            };
                        };
                        if (_253.length > 0) {
                            var _479 = _253.slice(1);
                            if (_253[0] instanceof TwoRight) {
                                return fromZipper(__dict_Ord_107)(_479)(new Three((_253[0]).value0, (_253[0]).value1, (_253[0]).value2, _254.value0, _254.value1, _254.value2, _254.value3));
                            };
                        };
                        if (_253.length > 0) {
                            var _488 = _253.slice(1);
                            if (_253[0] instanceof ThreeLeft) {
                                var __tco___dict_Ord_107 = __dict_Ord_107;
                                var __tco__254 = new KickUp(new Two(_254.value0, _254.value1, _254.value2, _254.value3), (_253[0]).value0, (_253[0]).value1, new Two((_253[0]).value2, (_253[0]).value3, (_253[0]).value4, (_253[0]).value5));
                                __dict_Ord_107 = __tco___dict_Ord_107;
                                _253 = _488;
                                _254 = __tco__254;
                                continue tco;
                            };
                        };
                        if (_253.length > 0) {
                            var _500 = _253.slice(1);
                            if (_253[0] instanceof ThreeMiddle) {
                                var __tco___dict_Ord_107 = __dict_Ord_107;
                                var __tco__254 = new KickUp(new Two((_253[0]).value0, (_253[0]).value1, (_253[0]).value2, _254.value0), _254.value1, _254.value2, new Two(_254.value3, (_253[0]).value3, (_253[0]).value4, (_253[0]).value5));
                                __dict_Ord_107 = __tco___dict_Ord_107;
                                _253 = _500;
                                _254 = __tco__254;
                                continue tco;
                            };
                        };
                        if (_253.length > 0) {
                            var _512 = _253.slice(1);
                            if (_253[0] instanceof ThreeRight) {
                                var __tco___dict_Ord_107 = __dict_Ord_107;
                                var __tco__254 = new KickUp(new Two((_253[0]).value0, (_253[0]).value1, (_253[0]).value2, (_253[0]).value3), (_253[0]).value4, (_253[0]).value5, new Two(_254.value0, _254.value1, _254.value2, _254.value3));
                                __dict_Ord_107 = __tco___dict_Ord_107;
                                _253 = _512;
                                _254 = __tco__254;
                                continue tco;
                            };
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var down = function (__copy___dict_Ord_108) {
            return function (__copy__249) {
                return function (__copy__250) {
                    return function (__copy__251) {
                        return function (__copy__252) {
                            var __dict_Ord_108 = __copy___dict_Ord_108;
                            var _249 = __copy__249;
                            var _250 = __copy__250;
                            var _251 = __copy__251;
                            var _252 = __copy__252;
                            tco: while (true) {
                                if (_252 instanceof Leaf) {
                                    return up(__dict_Ord_108)(_249)(new KickUp(Leaf.value, _250, _251, Leaf.value));
                                };
                                if (_252 instanceof Two && Prelude["=="](__dict_Ord_108["__superclass_Prelude.Eq_0"]({}))(_250)(_252.value1)) {
                                    return fromZipper(__dict_Ord_108)(_249)(new Two(_252.value0, _250, _251, _252.value3));
                                };
                                if (_252 instanceof Two && Prelude["<"](__dict_Ord_108)(_250)(_252.value1)) {
                                    var __tco___dict_Ord_108 = __dict_Ord_108;
                                    var __tco__249 = Prelude[":"](new TwoLeft(_252.value1, _252.value2, _252.value3))(_249);
                                    var __tco__250 = _250;
                                    var __tco__251 = _251;
                                    var __tco__252 = _252.value0;
                                    __dict_Ord_108 = __tco___dict_Ord_108;
                                    _249 = __tco__249;
                                    _250 = __tco__250;
                                    _251 = __tco__251;
                                    _252 = __tco__252;
                                    continue tco;
                                };
                                if (_252 instanceof Two) {
                                    var __tco___dict_Ord_108 = __dict_Ord_108;
                                    var __tco__249 = Prelude[":"](new TwoRight(_252.value0, _252.value1, _252.value2))(_249);
                                    var __tco__250 = _250;
                                    var __tco__251 = _251;
                                    var __tco__252 = _252.value3;
                                    __dict_Ord_108 = __tco___dict_Ord_108;
                                    _249 = __tco__249;
                                    _250 = __tco__250;
                                    _251 = __tco__251;
                                    _252 = __tco__252;
                                    continue tco;
                                };
                                if (_252 instanceof Three && Prelude["=="](__dict_Ord_108["__superclass_Prelude.Eq_0"]({}))(_250)(_252.value1)) {
                                    return fromZipper(__dict_Ord_108)(_249)(new Three(_252.value0, _250, _251, _252.value3, _252.value4, _252.value5, _252.value6));
                                };
                                if (_252 instanceof Three && Prelude["=="](__dict_Ord_108["__superclass_Prelude.Eq_0"]({}))(_250)(_252.value4)) {
                                    return fromZipper(__dict_Ord_108)(_249)(new Three(_252.value0, _252.value1, _252.value2, _252.value3, _250, _251, _252.value6));
                                };
                                if (_252 instanceof Three && Prelude["<"](__dict_Ord_108)(_250)(_252.value1)) {
                                    var __tco___dict_Ord_108 = __dict_Ord_108;
                                    var __tco__249 = Prelude[":"](new ThreeLeft(_252.value1, _252.value2, _252.value3, _252.value4, _252.value5, _252.value6))(_249);
                                    var __tco__250 = _250;
                                    var __tco__251 = _251;
                                    var __tco__252 = _252.value0;
                                    __dict_Ord_108 = __tco___dict_Ord_108;
                                    _249 = __tco__249;
                                    _250 = __tco__250;
                                    _251 = __tco__251;
                                    _252 = __tco__252;
                                    continue tco;
                                };
                                if (_252 instanceof Three && Prelude["<"](__dict_Ord_108)(_252.value1)(_250) && Prelude["<="](__dict_Ord_108)(_250)(_252.value4)) {
                                    var __tco___dict_Ord_108 = __dict_Ord_108;
                                    var __tco__249 = Prelude[":"](new ThreeMiddle(_252.value0, _252.value1, _252.value2, _252.value4, _252.value5, _252.value6))(_249);
                                    var __tco__250 = _250;
                                    var __tco__251 = _251;
                                    var __tco__252 = _252.value3;
                                    __dict_Ord_108 = __tco___dict_Ord_108;
                                    _249 = __tco__249;
                                    _250 = __tco__250;
                                    _251 = __tco__251;
                                    _252 = __tco__252;
                                    continue tco;
                                };
                                if (_252 instanceof Three) {
                                    var __tco___dict_Ord_108 = __dict_Ord_108;
                                    var __tco__249 = Prelude[":"](new ThreeRight(_252.value0, _252.value1, _252.value2, _252.value3, _252.value4, _252.value5))(_249);
                                    var __tco__250 = _250;
                                    var __tco__251 = _251;
                                    var __tco__252 = _252.value6;
                                    __dict_Ord_108 = __tco___dict_Ord_108;
                                    _249 = __tco__249;
                                    _250 = __tco__250;
                                    _251 = __tco__251;
                                    _252 = __tco__252;
                                    continue tco;
                                };
                                throw new Error("Failed pattern match");
                            };
                        };
                    };
                };
            };
        };
        return down(__dict_Ord_106)([  ]);
    };
    var empty = Leaf.value;
    var $$delete = function (__dict_Ord_114) {
        var up = function (__copy___dict_Ord_115) {
            return function (__copy__258) {
                return function (__copy__259) {
                    var __dict_Ord_115 = __copy___dict_Ord_115;
                    var _258 = __copy__258;
                    var _259 = __copy__259;
                    tco: while (true) {
                        if (_258.length === 0) {
                            return _259;
                        };
                        if (_258.length > 0) {
                            var _573 = _258.slice(1);
                            if (_258[0] instanceof TwoLeft && (_258[0]).value2 instanceof Leaf && _259 instanceof Leaf) {
                                return fromZipper(__dict_Ord_115)(_573)(new Two(Leaf.value, (_258[0]).value0, (_258[0]).value1, Leaf.value));
                            };
                        };
                        if (_258.length > 0) {
                            var _578 = _258.slice(1);
                            if (_258[0] instanceof TwoRight && (_258[0]).value0 instanceof Leaf && _259 instanceof Leaf) {
                                return fromZipper(__dict_Ord_115)(_578)(new Two(Leaf.value, (_258[0]).value1, (_258[0]).value2, Leaf.value));
                            };
                        };
                        if (_258.length > 0) {
                            var _583 = _258.slice(1);
                            if (_258[0] instanceof TwoLeft && (_258[0]).value2 instanceof Two) {
                                var __tco___dict_Ord_115 = __dict_Ord_115;
                                var __tco__259 = new Three(_259, (_258[0]).value0, (_258[0]).value1, (_258[0]).value2.value0, (_258[0]).value2.value1, (_258[0]).value2.value2, (_258[0]).value2.value3);
                                __dict_Ord_115 = __tco___dict_Ord_115;
                                _258 = _583;
                                _259 = __tco__259;
                                continue tco;
                            };
                        };
                        if (_258.length > 0) {
                            var _592 = _258.slice(1);
                            if (_258[0] instanceof TwoRight && (_258[0]).value0 instanceof Two) {
                                var __tco___dict_Ord_115 = __dict_Ord_115;
                                var __tco__259 = new Three((_258[0]).value0.value0, (_258[0]).value0.value1, (_258[0]).value0.value2, (_258[0]).value0.value3, (_258[0]).value1, (_258[0]).value2, _259);
                                __dict_Ord_115 = __tco___dict_Ord_115;
                                _258 = _592;
                                _259 = __tco__259;
                                continue tco;
                            };
                        };
                        if (_258.length > 0) {
                            var _601 = _258.slice(1);
                            if (_258[0] instanceof TwoLeft && (_258[0]).value2 instanceof Three) {
                                return fromZipper(__dict_Ord_115)(_601)(new Two(new Two(_259, (_258[0]).value0, (_258[0]).value1, (_258[0]).value2.value0), (_258[0]).value2.value1, (_258[0]).value2.value2, new Two((_258[0]).value2.value3, (_258[0]).value2.value4, (_258[0]).value2.value5, (_258[0]).value2.value6)));
                            };
                        };
                        if (_258.length > 0) {
                            var _613 = _258.slice(1);
                            if (_258[0] instanceof TwoRight && (_258[0]).value0 instanceof Three) {
                                return fromZipper(__dict_Ord_115)(_613)(new Two(new Two((_258[0]).value0.value0, (_258[0]).value0.value1, (_258[0]).value0.value2, (_258[0]).value0.value3), (_258[0]).value0.value4, (_258[0]).value0.value5, new Two((_258[0]).value0.value6, (_258[0]).value1, (_258[0]).value2, _259)));
                            };
                        };
                        if (_258.length > 0) {
                            var _625 = _258.slice(1);
                            if (_258[0] instanceof ThreeLeft && (_258[0]).value2 instanceof Leaf && (_258[0]).value5 instanceof Leaf && _259 instanceof Leaf) {
                                return fromZipper(__dict_Ord_115)(_625)(new Three(Leaf.value, (_258[0]).value0, (_258[0]).value1, Leaf.value, (_258[0]).value3, (_258[0]).value4, Leaf.value));
                            };
                        };
                        if (_258.length > 0) {
                            var _633 = _258.slice(1);
                            if (_258[0] instanceof ThreeMiddle && (_258[0]).value0 instanceof Leaf && (_258[0]).value5 instanceof Leaf && _259 instanceof Leaf) {
                                return fromZipper(__dict_Ord_115)(_633)(new Three(Leaf.value, (_258[0]).value1, (_258[0]).value2, Leaf.value, (_258[0]).value3, (_258[0]).value4, Leaf.value));
                            };
                        };
                        if (_258.length > 0) {
                            var _641 = _258.slice(1);
                            if (_258[0] instanceof ThreeRight && (_258[0]).value0 instanceof Leaf && (_258[0]).value3 instanceof Leaf && _259 instanceof Leaf) {
                                return fromZipper(__dict_Ord_115)(_641)(new Three(Leaf.value, (_258[0]).value1, (_258[0]).value2, Leaf.value, (_258[0]).value4, (_258[0]).value5, Leaf.value));
                            };
                        };
                        if (_258.length > 0) {
                            var _649 = _258.slice(1);
                            if (_258[0] instanceof ThreeLeft && (_258[0]).value2 instanceof Two) {
                                return fromZipper(__dict_Ord_115)(_649)(new Two(new Three(_259, (_258[0]).value0, (_258[0]).value1, (_258[0]).value2.value0, (_258[0]).value2.value1, (_258[0]).value2.value2, (_258[0]).value2.value3), (_258[0]).value3, (_258[0]).value4, (_258[0]).value5));
                            };
                        };
                        if (_258.length > 0) {
                            var _661 = _258.slice(1);
                            if (_258[0] instanceof ThreeMiddle && (_258[0]).value0 instanceof Two) {
                                return fromZipper(__dict_Ord_115)(_661)(new Two(new Three((_258[0]).value0.value0, (_258[0]).value0.value1, (_258[0]).value0.value2, (_258[0]).value0.value3, (_258[0]).value1, (_258[0]).value2, _259), (_258[0]).value3, (_258[0]).value4, (_258[0]).value5));
                            };
                        };
                        if (_258.length > 0) {
                            var _673 = _258.slice(1);
                            if (_258[0] instanceof ThreeMiddle && (_258[0]).value5 instanceof Two) {
                                return fromZipper(__dict_Ord_115)(_673)(new Two((_258[0]).value0, (_258[0]).value1, (_258[0]).value2, new Three(_259, (_258[0]).value3, (_258[0]).value4, (_258[0]).value5.value0, (_258[0]).value5.value1, (_258[0]).value5.value2, (_258[0]).value5.value3)));
                            };
                        };
                        if (_258.length > 0) {
                            var _685 = _258.slice(1);
                            if (_258[0] instanceof ThreeRight && (_258[0]).value3 instanceof Two) {
                                return fromZipper(__dict_Ord_115)(_685)(new Two((_258[0]).value0, (_258[0]).value1, (_258[0]).value2, new Three((_258[0]).value3.value0, (_258[0]).value3.value1, (_258[0]).value3.value2, (_258[0]).value3.value3, (_258[0]).value4, (_258[0]).value5, _259)));
                            };
                        };
                        if (_258.length > 0) {
                            var _697 = _258.slice(1);
                            if (_258[0] instanceof ThreeLeft && (_258[0]).value2 instanceof Three) {
                                return fromZipper(__dict_Ord_115)(_697)(new Three(new Two(_259, (_258[0]).value0, (_258[0]).value1, (_258[0]).value2.value0), (_258[0]).value2.value1, (_258[0]).value2.value2, new Two((_258[0]).value2.value3, (_258[0]).value2.value4, (_258[0]).value2.value5, (_258[0]).value2.value6), (_258[0]).value3, (_258[0]).value4, (_258[0]).value5));
                            };
                        };
                        if (_258.length > 0) {
                            var _712 = _258.slice(1);
                            if (_258[0] instanceof ThreeMiddle && (_258[0]).value0 instanceof Three) {
                                return fromZipper(__dict_Ord_115)(_712)(new Three(new Two((_258[0]).value0.value0, (_258[0]).value0.value1, (_258[0]).value0.value2, (_258[0]).value0.value3), (_258[0]).value0.value4, (_258[0]).value0.value5, new Two((_258[0]).value0.value6, (_258[0]).value1, (_258[0]).value2, _259), (_258[0]).value3, (_258[0]).value4, (_258[0]).value5));
                            };
                        };
                        if (_258.length > 0) {
                            var _727 = _258.slice(1);
                            if (_258[0] instanceof ThreeMiddle && (_258[0]).value5 instanceof Three) {
                                return fromZipper(__dict_Ord_115)(_727)(new Three((_258[0]).value0, (_258[0]).value1, (_258[0]).value2, new Two(_259, (_258[0]).value3, (_258[0]).value4, (_258[0]).value5.value0), (_258[0]).value5.value1, (_258[0]).value5.value2, new Two((_258[0]).value5.value3, (_258[0]).value5.value4, (_258[0]).value5.value5, (_258[0]).value5.value6)));
                            };
                        };
                        if (_258.length > 0) {
                            var _742 = _258.slice(1);
                            if (_258[0] instanceof ThreeRight && (_258[0]).value3 instanceof Three) {
                                return fromZipper(__dict_Ord_115)(_742)(new Three((_258[0]).value0, (_258[0]).value1, (_258[0]).value2, new Two((_258[0]).value3.value0, (_258[0]).value3.value1, (_258[0]).value3.value2, (_258[0]).value3.value3), (_258[0]).value3.value4, (_258[0]).value3.value5, new Two((_258[0]).value3.value6, (_258[0]).value4, (_258[0]).value5, _259)));
                            };
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var removeMaxNode = function (__copy___dict_Ord_116) {
            return function (__copy__261) {
                return function (__copy__262) {
                    var __dict_Ord_116 = __copy___dict_Ord_116;
                    var _261 = __copy__261;
                    var _262 = __copy__262;
                    tco: while (true) {
                        if (_262 instanceof Two && _262.value0 instanceof Leaf && _262.value3 instanceof Leaf) {
                            return up(__dict_Ord_116)(_261)(Leaf.value);
                        };
                        if (_262 instanceof Two) {
                            var __tco___dict_Ord_116 = __dict_Ord_116;
                            var __tco__261 = Prelude[":"](new TwoRight(_262.value0, _262.value1, _262.value2))(_261);
                            var __tco__262 = _262.value3;
                            __dict_Ord_116 = __tco___dict_Ord_116;
                            _261 = __tco__261;
                            _262 = __tco__262;
                            continue tco;
                        };
                        if (_262 instanceof Three && _262.value0 instanceof Leaf && _262.value3 instanceof Leaf && _262.value6 instanceof Leaf) {
                            return up(__dict_Ord_116)(Prelude[":"](new TwoRight(Leaf.value, _262.value1, _262.value2))(_261))(Leaf.value);
                        };
                        if (_262 instanceof Three) {
                            var __tco___dict_Ord_116 = __dict_Ord_116;
                            var __tco__261 = Prelude[":"](new ThreeRight(_262.value0, _262.value1, _262.value2, _262.value3, _262.value4, _262.value5))(_261);
                            var __tco__262 = _262.value6;
                            __dict_Ord_116 = __tco___dict_Ord_116;
                            _261 = __tco__261;
                            _262 = __tco__262;
                            continue tco;
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var maxNode = function (__copy___dict_Ord_117) {
            return function (__copy__260) {
                var __dict_Ord_117 = __copy___dict_Ord_117;
                var _260 = __copy__260;
                tco: while (true) {
                    if (_260 instanceof Two && _260.value3 instanceof Leaf) {
                        return {
                            key: _260.value1, 
                            value: _260.value2
                        };
                    };
                    if (_260 instanceof Two) {
                        var __tco___dict_Ord_117 = __dict_Ord_117;
                        var __tco__260 = _260.value3;
                        __dict_Ord_117 = __tco___dict_Ord_117;
                        _260 = __tco__260;
                        continue tco;
                    };
                    if (_260 instanceof Three && _260.value6 instanceof Leaf) {
                        return {
                            key: _260.value4, 
                            value: _260.value5
                        };
                    };
                    if (_260 instanceof Three) {
                        var __tco___dict_Ord_117 = __dict_Ord_117;
                        var __tco__260 = _260.value6;
                        __dict_Ord_117 = __tco___dict_Ord_117;
                        _260 = __tco__260;
                        continue tco;
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
        var down = function (__copy___dict_Ord_118) {
            return function (__copy__255) {
                return function (__copy__256) {
                    return function (__copy__257) {
                        var __dict_Ord_118 = __copy___dict_Ord_118;
                        var _255 = __copy__255;
                        var _256 = __copy__256;
                        var _257 = __copy__257;
                        tco: while (true) {
                            if (_257 instanceof Leaf) {
                                return fromZipper(__dict_Ord_118)(_255)(Leaf.value);
                            };
                            if (_257 instanceof Two && _257.value0 instanceof Leaf && _257.value3 instanceof Leaf && Prelude["=="](__dict_Ord_118["__superclass_Prelude.Eq_0"]({}))(_256)(_257.value1)) {
                                return up(__dict_Ord_118)(_255)(Leaf.value);
                            };
                            if (_257 instanceof Two && Prelude["=="](__dict_Ord_118["__superclass_Prelude.Eq_0"]({}))(_256)(_257.value1)) {
                                var max = maxNode(__dict_Ord_118)(_257.value0);
                                return removeMaxNode(__dict_Ord_118)(Prelude[":"](new TwoLeft(max.key, max.value, _257.value3))(_255))(_257.value0);
                            };
                            if (_257 instanceof Two && Prelude["<"](__dict_Ord_118)(_256)(_257.value1)) {
                                var __tco___dict_Ord_118 = __dict_Ord_118;
                                var __tco__255 = Prelude[":"](new TwoLeft(_257.value1, _257.value2, _257.value3))(_255);
                                var __tco__256 = _256;
                                var __tco__257 = _257.value0;
                                __dict_Ord_118 = __tco___dict_Ord_118;
                                _255 = __tco__255;
                                _256 = __tco__256;
                                _257 = __tco__257;
                                continue tco;
                            };
                            if (_257 instanceof Two) {
                                var __tco___dict_Ord_118 = __dict_Ord_118;
                                var __tco__255 = Prelude[":"](new TwoRight(_257.value0, _257.value1, _257.value2))(_255);
                                var __tco__256 = _256;
                                var __tco__257 = _257.value3;
                                __dict_Ord_118 = __tco___dict_Ord_118;
                                _255 = __tco__255;
                                _256 = __tco__256;
                                _257 = __tco__257;
                                continue tco;
                            };
                            if (_257 instanceof Three && _257.value0 instanceof Leaf && _257.value3 instanceof Leaf && _257.value6 instanceof Leaf && Prelude["=="](__dict_Ord_118["__superclass_Prelude.Eq_0"]({}))(_256)(_257.value1)) {
                                return fromZipper(__dict_Ord_118)(_255)(new Two(Leaf.value, _257.value4, _257.value5, Leaf.value));
                            };
                            if (_257 instanceof Three && _257.value0 instanceof Leaf && _257.value3 instanceof Leaf && _257.value6 instanceof Leaf && Prelude["=="](__dict_Ord_118["__superclass_Prelude.Eq_0"]({}))(_256)(_257.value4)) {
                                return fromZipper(__dict_Ord_118)(_255)(new Two(Leaf.value, _257.value1, _257.value2, Leaf.value));
                            };
                            if (_257 instanceof Three && Prelude["=="](__dict_Ord_118["__superclass_Prelude.Eq_0"]({}))(_256)(_257.value1)) {
                                var max = maxNode(__dict_Ord_118)(_257.value0);
                                return removeMaxNode(__dict_Ord_118)(Prelude[":"](new ThreeLeft(max.key, max.value, _257.value3, _257.value4, _257.value5, _257.value6))(_255))(_257.value0);
                            };
                            if (_257 instanceof Three && Prelude["=="](__dict_Ord_118["__superclass_Prelude.Eq_0"]({}))(_256)(_257.value4)) {
                                var max = maxNode(__dict_Ord_118)(_257.value3);
                                return removeMaxNode(__dict_Ord_118)(Prelude[":"](new ThreeMiddle(_257.value0, _257.value1, _257.value2, max.key, max.value, _257.value6))(_255))(_257.value3);
                            };
                            if (_257 instanceof Three && Prelude["<"](__dict_Ord_118)(_256)(_257.value1)) {
                                var __tco___dict_Ord_118 = __dict_Ord_118;
                                var __tco__255 = Prelude[":"](new ThreeLeft(_257.value1, _257.value2, _257.value3, _257.value4, _257.value5, _257.value6))(_255);
                                var __tco__256 = _256;
                                var __tco__257 = _257.value0;
                                __dict_Ord_118 = __tco___dict_Ord_118;
                                _255 = __tco__255;
                                _256 = __tco__256;
                                _257 = __tco__257;
                                continue tco;
                            };
                            if (_257 instanceof Three && Prelude["<"](__dict_Ord_118)(_257.value1)(_256) && Prelude["<"](__dict_Ord_118)(_256)(_257.value4)) {
                                var __tco___dict_Ord_118 = __dict_Ord_118;
                                var __tco__255 = Prelude[":"](new ThreeMiddle(_257.value0, _257.value1, _257.value2, _257.value4, _257.value5, _257.value6))(_255);
                                var __tco__256 = _256;
                                var __tco__257 = _257.value3;
                                __dict_Ord_118 = __tco___dict_Ord_118;
                                _255 = __tco__255;
                                _256 = __tco__256;
                                _257 = __tco__257;
                                continue tco;
                            };
                            if (_257 instanceof Three) {
                                var __tco___dict_Ord_118 = __dict_Ord_118;
                                var __tco__255 = Prelude[":"](new ThreeRight(_257.value0, _257.value1, _257.value2, _257.value3, _257.value4, _257.value5))(_255);
                                var __tco__256 = _256;
                                var __tco__257 = _257.value6;
                                __dict_Ord_118 = __tco___dict_Ord_118;
                                _255 = __tco__255;
                                _256 = __tco__256;
                                _257 = __tco__257;
                                continue tco;
                            };
                            throw new Error("Failed pattern match");
                        };
                    };
                };
            };
        };
        return down(__dict_Ord_114)([  ]);
    };
    var alter = function (__dict_Ord_119) {
        return function (f) {
            return function (k) {
                return function (m) {
                    var _871 = f(lookup(__dict_Ord_119)(k)(m));
                    if (_871 instanceof Data_Maybe.Nothing) {
                        return $$delete(__dict_Ord_119)(k)(m);
                    };
                    if (_871 instanceof Data_Maybe.Just) {
                        return insert(__dict_Ord_119)(k)(_871.value0)(m);
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
    };
    return {
        alter: alter, 
        "delete": $$delete, 
        toList: toList, 
        lookup: lookup, 
        insert: insert, 
        empty: empty
    };
})();
var PS = PS || {};
PS.Data_Trie = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Map = PS.Data_Map;
    var Data_String = PS.Data_String;
    function Trie(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Trie.create = function (value0) {
        return function (value1) {
            return new Trie(value0, value1);
        };
    };
    var toArray = (function () {
        var go = function (_265) {
            return function (_266) {
                return Prelude["<>"](Data_Array.semigroupArray({}))(Data_Maybe.maybe([  ])(function (a$prime) {
                    return [ new Data_Tuple.Tuple(_265, a$prime) ];
                })(_266.value0))(Data_Array.concatMap(function (_263) {
                    return go(_265 + _263.value0)(_263.value1);
                })(Data_Map.toList(_266.value1)));
            };
        };
        return go("");
    })();
    var lookupAll = function (s) {
        var go = function (_269) {
            return function (_270) {
                if (_269 >= Data_String.length(s)) {
                    return new Data_Maybe.Just(_270);
                };
                return Prelude[">>="](Data_Maybe.bindMaybe({}))(Data_Map.lookup(Prelude.ordString({}))(Data_String.charAt(_269)(s))(_270.value1))(function (_5) {
                    return go(_269 + 1)(_5);
                });
            };
        };
        return go(0);
    };
    var empty = new Trie(Data_Maybe.Nothing.value, Data_Map.empty);
    var insert = function (s) {
        return function (a) {
            var go = function (_267) {
                return function (_268) {
                    if (_267 >= Data_String.length(s)) {
                        return new Trie(new Data_Maybe.Just(a), _268.value1);
                    };
                    return new Trie(_268.value0, Data_Map.alter(Prelude.ordString({}))(Prelude["<<<"](Prelude.semigroupoidArr({}))(Data_Maybe.Just.create)(Prelude["<<<"](Prelude.semigroupoidArr({}))(go(_267 + 1))(Data_Maybe.fromMaybe(empty))))(Data_String.charAt(_267)(s))(_268.value1));
                };
            };
            return go(0);
        };
    };
    return {
        Trie: Trie, 
        lookupAll: lookupAll, 
        insert: insert, 
        empty: empty, 
        toArray: toArray
    };
})();
var PS = PS || {};
PS.Data_Traversable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var Data_Foldable = PS.Data_Foldable;
    function Traversable(__superclass_Data$dotFoldable$dotFoldable_1, __superclass_Prelude$dotFunctor_0, sequence, traverse) {
        this["__superclass_Data.Foldable.Foldable_1"] = __superclass_Data$dotFoldable$dotFoldable_1;
        this["__superclass_Prelude.Functor_0"] = __superclass_Prelude$dotFunctor_0;
        this.sequence = sequence;
        this.traverse = traverse;
    };
    var traverse = function (dict) {
        return dict.traverse;
    };
    var sequence = function (dict) {
        return dict.sequence;
    };
    var traversableArray = function (_) {
        return new Traversable(function (__1) {
            return Data_Foldable.foldableArray({});
        }, function (__1) {
            return Data_Array.functorArray({});
        }, function (__dict_Applicative_139) {
            return function (_288) {
                if (_288.length === 0) {
                    return Prelude.pure(__dict_Applicative_139)([  ]);
                };
                if (_288.length > 0) {
                    var _895 = _288.slice(1);
                    return Prelude["<*>"](__dict_Applicative_139["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_139["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_288[0]))(sequence(traversableArray({}))(__dict_Applicative_139)(_895));
                };
                throw new Error("Failed pattern match");
            };
        }, function (__dict_Applicative_138) {
            return function (_286) {
                return function (_287) {
                    if (_287.length === 0) {
                        return Prelude.pure(__dict_Applicative_138)([  ]);
                    };
                    if (_287.length > 0) {
                        var _899 = _287.slice(1);
                        return Prelude["<*>"](__dict_Applicative_138["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_138["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_286(_287[0])))(traverse(traversableArray({}))(__dict_Applicative_138)(_286)(_899));
                    };
                    throw new Error("Failed pattern match");
                };
            };
        });
    };
    var $$for = function (__dict_Applicative_141) {
        return function (__dict_Traversable_142) {
            return function (x) {
                return function (f) {
                    return traverse(__dict_Traversable_142)(__dict_Applicative_141)(f)(x);
                };
            };
        };
    };
    return {
        Traversable: Traversable, 
        "for": $$for, 
        sequence: sequence, 
        traverse: traverse, 
        traversableArray: traversableArray
    };
})();
var PS = PS || {};
PS.Data_Foreign = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Function = PS.Data_Function;
    var Data_Either = PS.Data_Either;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Traversable = PS.Data_Traversable;
    var Data_Array = PS.Data_Array;
    function ForeignParser(value0) {
        this.value0 = value0;
    };
    ForeignParser.create = function (value0) {
        return new ForeignParser(value0);
    };
    function ReadForeign(read) {
        this.read = read;
    };
    function fromStringImpl(left, right, str) {   try {     return right(JSON.parse(str));   } catch (e) {     return left(e.toString());   } };
    function readPrimTypeImpl(left, right, typeName, value) {   if (toString.call(value) == '[object ' + typeName + ']') {     return right(value);  }   return left('Value is not a ' + typeName + ''); };
    function readPropImpl(k, obj) {     return obj == undefined ? undefined : obj[k];};
    var readPrimType = function (ty) {
        return function (x) {
            return readPrimTypeImpl(Data_Either.Left.create, Data_Either.Right.create, ty, x);
        };
    };
    var readString = function (_) {
        return new ReadForeign(ForeignParser.create(readPrimType("String")));
    };
    var read = function (dict) {
        return dict.read;
    };
    var parseForeign = function (_301) {
        return function (_302) {
            return _301.value0(_302);
        };
    };
    var functorForeignParser = function (_) {
        return new Prelude.Functor(function (_303) {
            return function (_304) {
                return new ForeignParser(function (x) {
                    return Prelude["<$>"](Data_Either.functorEither({}))(_303)(_304.value0(x));
                });
            };
        });
    };
    var fromString = function (s) {
        return fromStringImpl(Data_Either.Left.create, Data_Either.Right.create, s);
    };
    var parseJSON = function (__dict_ReadForeign_147) {
        return function (json) {
            return Prelude[">>="](Data_Either.bindEither({}))(fromString(json))(parseForeign(read(__dict_ReadForeign_147)));
        };
    };
    var applyForeignParser = function (_) {
        return new Prelude.Apply(function (_307) {
            return function (_308) {
                return new ForeignParser(function (x) {
                    var _908 = _307.value0(x);
                    if (_908 instanceof Data_Either.Left) {
                        return new Data_Either.Left(_908.value0);
                    };
                    if (_908 instanceof Data_Either.Right) {
                        return Prelude["<$>"](Data_Either.functorEither({}))(_908.value0)(_308.value0(x));
                    };
                    throw new Error("Failed pattern match");
                });
            };
        }, function (__1) {
            return functorForeignParser({});
        });
    };
    var bindForeignParser = function (_) {
        return new Prelude.Bind(function (_305) {
            return function (_306) {
                return new ForeignParser(function (x) {
                    var _915 = _305.value0(x);
                    if (_915 instanceof Data_Either.Left) {
                        return new Data_Either.Left(_915.value0);
                    };
                    if (_915 instanceof Data_Either.Right) {
                        return parseForeign(_306(_915.value0))(x);
                    };
                    throw new Error("Failed pattern match");
                });
            };
        }, function (__1) {
            return applyForeignParser({});
        });
    };
    var prop = function (__dict_ReadForeign_143) {
        return function (p) {
            return Prelude[">>="](bindForeignParser({}))(new ForeignParser(function (x) {
                return Data_Either.Right.create(readPropImpl$prime(p)(x));
            }))(function (x) {
                return new ForeignParser(function (_) {
                    var _919 = parseForeign(read(__dict_ReadForeign_143))(x);
                    if (_919 instanceof Data_Either.Right) {
                        return new Data_Either.Right(_919.value0);
                    };
                    if (_919 instanceof Data_Either.Left) {
                        return Data_Either.Left.create("Error reading property '" + p + "':\n" + _919.value0);
                    };
                    throw new Error("Failed pattern match");
                });
            });
        };
    };
    var readPropImpl$prime = function (prop_1) {
        return function (x) {
            return readPropImpl(prop_1, x);
        };
    };
    var readArray = function (__dict_ReadForeign_144) {
        return new ReadForeign((function () {
            var arrayItem = function (_309) {
                var _923 = parseForeign(read(__dict_ReadForeign_144))(_309.value1);
                if (_923 instanceof Data_Either.Right) {
                    return new Data_Either.Right(_923.value0);
                };
                if (_923 instanceof Data_Either.Left) {
                    return Data_Either.Left.create("Error reading item at index " + Prelude.show(Prelude.showNumber({}))(_309.value0) + ":\n" + _923.value0);
                };
                throw new Error("Failed pattern match");
            };
            return Prelude[">>="](bindForeignParser({}))(ForeignParser.create(readPrimType("Array")))(function (xs) {
                return new ForeignParser(function (_) {
                    return Data_Traversable.traverse(Data_Traversable.traversableArray({}))(Data_Either.applicativeEither({}))(arrayItem)(Data_Tuple.zip(Data_Array.range(0)(Data_Array.length(xs)))(xs));
                });
            });
        })());
    };
    return {
        ForeignParser: ForeignParser, 
        ReadForeign: ReadForeign, 
        prop: prop, 
        read: read, 
        parseJSON: parseJSON, 
        parseForeign: parseForeign, 
        functorForeignParser: functorForeignParser, 
        bindForeignParser: bindForeignParser, 
        applyForeignParser: applyForeignParser, 
        readString: readString, 
        readArray: readArray
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff_DOM = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Function = PS.Data_Function;
    var Data_Maybe = PS.Data_Maybe;
    function createElement(name) {  return function() {    return document.createElement(name);  };};
    function querySelectorImpl(r, f, s) {  return function() {    var result = document.querySelector(s);    return result ? f(result) : r;  };};
    function appendChild(child) {  return function(node) {    return function() {      node.appendChild(child);      return node;    };  };};
    function setText(text) {  return function(node) {    return function() {      node.textContent = text;      return node;    };  };};
    function getValue(node) {  return function() {    return node.value;  };};
    function setInnerHTML(html) {  return function(node) {    return function() {      node.innerHTML = html;      return node;    };  };};
    function addEventListener(name) {  return function(handler) {    return function(node) {      return function() {        node.addEventListener(name, function(e) {          handler();        });      };    };  };};
    var querySelector = function (s) {
        return querySelectorImpl(Data_Maybe.Nothing.value, Data_Maybe.Just.create, s);
    };
    return {
        addEventListener: addEventListener, 
        setInnerHTML: setInnerHTML, 
        getValue: getValue, 
        setText: setText, 
        appendChild: appendChild, 
        querySelector: querySelector, 
        querySelectorImpl: querySelectorImpl, 
        createElement: createElement
    };
})();
var PS = PS || {};
PS.Main = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Trie = PS.Data_Trie;
    var Data_String = PS.Data_String;
    var Data_Foreign = PS.Data_Foreign;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_Eff_DOM = PS.Control_Monad_Eff_DOM;
    var Data_Either = PS.Data_Either;
    var Data_Array = PS.Data_Array;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Foldable = PS.Data_Foldable;
    var Control_Monad_Eff_AJAX = PS.Control_Monad_Eff_AJAX;
    var Data_Traversable = PS.Data_Traversable;
    function Entry(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Entry.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Entry(value0, value1, value2);
            };
        };
    };
    function error(msg) {  throw new Error(msg);};
    var runSearch = function (_329) {
        return function (_330) {
            if (_330 === "") {
                return Data_Maybe.Nothing.value;
            };
            return Prelude["<$>"](Data_Maybe.functorMaybe({}))(Data_Trie.toArray)(Data_Trie.lookupAll(Data_String.toLower(_330))(_329));
        };
    };
    var readForeignEntry = function (_) {
        return new Data_Foreign.ReadForeign(Prelude["<*>"](Data_Foreign.applyForeignParser({}))(Prelude["<*>"](Data_Foreign.applyForeignParser({}))(Prelude["<$>"](Data_Foreign.functorForeignParser({}))(Entry.create)(Data_Foreign.prop(Data_Foreign.readString({}))("module")))(Data_Foreign.prop(Data_Foreign.readString({}))("name")))(Data_Foreign.prop(Data_Foreign.readString({}))("detail")));
    };
    var getQuery = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.querySelector("#searchInput"))(function (_24) {
        if (_24 instanceof Data_Maybe.Just) {
            return function __do() {
                var _23 = Control_Monad_Eff_DOM.getValue(_24.value0)();
                var _932 = Data_Foreign.parseForeign(Data_Foreign.read(Data_Foreign.readString({})))(_23);
                if (_932 instanceof Data_Either.Right) {
                    return _932.value0;
                };
                if (_932 instanceof Data_Either.Left) {
                    return "";
                };
                throw new Error("Failed pattern match");
            };
        };
        throw new Error("Failed pattern match");
    });
    var search = function (trie) {
        return function __do() {
            var _27 = getQuery();
            var _26 = Control_Monad_Eff_DOM.querySelector("#searchResults")();
            return (function () {
                if (_26 instanceof Data_Maybe.Nothing) {
                    return error("#searchResults not found");
                };
                if (_26 instanceof Data_Maybe.Just) {
                    return Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.setInnerHTML("")(_26.value0))(function (_) {
                        var _939 = runSearch(trie)(_27);
                        if (_939 instanceof Data_Maybe.Nothing) {
                            return Prelude["return"](Control_Monad_Eff.monadEff({}))(Prelude.unit);
                        };
                        if (_939 instanceof Data_Maybe.Just) {
                            return Control_Monad_Eff.foreachE(Data_Array.take(20)(_939.value0))(function (_327) {
                                return function __do() {
                                    var _25 = Control_Monad_Eff_DOM.createElement("div")();
                                    var __1 = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("h2"))(Control_Monad_Eff_DOM.setText(_327.value1.value1)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    var __2 = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("div"))(Control_Monad_Eff_DOM.setText(_327.value1.value0)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    var __3 = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("pre"))(Control_Monad_Eff_DOM.setText(_327.value1.value2)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    var __4 = Control_Monad_Eff_DOM.appendChild(_25)(_26.value0)();
                                    return Prelude.unit;
                                };
                            });
                        };
                        throw new Error("Failed pattern match");
                    });
                };
                throw new Error("Failed pattern match");
            })()();
        };
    };
    var buildTrie = function (json) {
        var _949 = Data_Foreign.parseJSON(Data_Foreign.readArray(readForeignEntry({})))(json);
        if (_949 instanceof Data_Either.Left) {
            return error(_949.value0);
        };
        if (_949 instanceof Data_Either.Right) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray({}))(function (t) {
                return function (_328) {
                    return Data_Trie.insert(Data_String.toLower(_328.value1))(_328)(t);
                };
            })(Data_Trie.empty)(_949.value0);
        };
        throw new Error("Failed pattern match");
    };
    var main = Control_Monad_Eff_AJAX.get("data.json")(function (json) {
        return function __do() {
            var _28 = Control_Monad_Eff_DOM.querySelector("#searchInput")();
            return (function () {
                if (_28 instanceof Data_Maybe.Nothing) {
                    return error("#searchInput not found");
                };
                if (_28 instanceof Data_Maybe.Just) {
                    var trie = buildTrie(json);
                    return function __do() {
                        Data_Traversable["for"](Control_Monad_Eff.applicativeEff({}))(Data_Traversable.traversableArray({}))([ "keyup", "change" ])(function (evt) {
                            return Control_Monad_Eff_DOM.addEventListener(evt)(search(trie))(_28.value0);
                        })();
                        return Prelude.unit;
                    };
                };
                throw new Error("Failed pattern match");
            })()();
        };
    });
    return {
        Entry: Entry, 
        main: main, 
        buildTrie: buildTrie, 
        error: error, 
        search: search, 
        runSearch: runSearch, 
        getQuery: getQuery, 
        readForeignEntry: readForeignEntry
    };
})();
PS.Main.main();