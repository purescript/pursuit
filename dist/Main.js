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
    function showStringImpl(s) {  return JSON.stringify(s);};
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
    var $bar$bar = function (dict) {
        return dict["||"];
    };
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
    var showString = function (__unused) {
        return new Show(showStringImpl);
    };
    var showNumber = function (__unused) {
        return new Show(showNumberImpl);
    };
    var show = function (dict) {
        return dict.show;
    };
    var semigroupoidArr = function (__unused) {
        return new Semigroupoid(function (f) {
            return function (g) {
                return function (x) {
                    return f(g(x));
                };
            };
        });
    };
    var semigroupString = function (__unused) {
        return new Semigroup(concatString);
    };
    var pure = function (dict) {
        return dict.pure;
    };
    var $$return = function (__dict_Monad_4) {
        return pure(__dict_Monad_4["__superclass_Prelude.Applicative_0"]({}));
    };
    var numNumber = function (__unused) {
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
    var eqString = function (__unused) {
        return new Eq(refIneq, refEq);
    };
    var ordString = function (__unused) {
        return new Ord(eqString, unsafeCompare);
    };
    var eqNumber = function (__unused) {
        return new Eq(refIneq, refEq);
    };
    var ordNumber = function (__unused) {
        return new Ord(eqNumber, unsafeCompare);
    };
    var compare = function (dict) {
        return dict.compare;
    };
    var $less = function (__dict_Ord_10) {
        return function (a1) {
            return function (a2) {
                var _343 = compare(__dict_Ord_10)(a1)(a2);
                if (_343 instanceof LT) {
                    return true;
                };
                return false;
            };
        };
    };
    var $less$eq = function (__dict_Ord_11) {
        return function (a1) {
            return function (a2) {
                var _344 = compare(__dict_Ord_11)(a1)(a2);
                if (_344 instanceof GT) {
                    return false;
                };
                return true;
            };
        };
    };
    var $greater$eq = function (__dict_Ord_13) {
        return function (a1) {
            return function (a2) {
                var _345 = compare(__dict_Ord_13)(a1)(a2);
                if (_345 instanceof LT) {
                    return false;
                };
                return true;
            };
        };
    };
    var categoryArr = function (__unused) {
        return new Category(semigroupoidArr, function (x) {
            return x;
        });
    };
    var boolLikeBoolean = function (__unused) {
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
        "||": $bar$bar, 
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
        showString: showString, 
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
PS.Control_Monad_Eff = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function returnE(a) {  return function() {    return a;  };};
    function bindE(a) {  return function(f) {    return function() {      return f(a())();    };  };};
    function foreachE(as) {  return function(f) {    return function() {      for (var i = 0; i < as.length; i++) {        f(as[i])();      }    };  };};
    var applicativeEff = function (__unused) {
        return new Prelude.Applicative(applyEff, returnE);
    };
    var applyEff = function (__unused) {
        return new Prelude.Apply(Prelude.ap(monadEff({})), functorEff);
    };
    var monadEff = function (__unused) {
        return new Prelude.Monad(applicativeEff, bindEff);
    };
    var bindEff = function (__unused) {
        return new Prelude.Bind(bindE, applyEff);
    };
    var functorEff = function (__unused) {
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
    var functorEither = function (__unused) {
        return new Prelude.Functor(function (_68) {
            return function (_69) {
                if (_69 instanceof Left) {
                    return new Left(_69.value0);
                };
                if (_69 instanceof Right) {
                    return new Right(_68(_69.value0));
                };
                throw new Error("Failed pattern match");
            };
        });
    };
    var either = function (_65) {
        return function (_66) {
            return function (_67) {
                if (_67 instanceof Left) {
                    return _65(_67.value0);
                };
                if (_67 instanceof Right) {
                    return _66(_67.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var applyEither = function (__unused) {
        return new Prelude.Apply(function (_70) {
            return function (_71) {
                if (_70 instanceof Left) {
                    return new Left(_70.value0);
                };
                if (_70 instanceof Right) {
                    return Prelude["<$>"](functorEither({}))(_70.value0)(_71);
                };
                throw new Error("Failed pattern match");
            };
        }, functorEither);
    };
    var bindEither = function (__unused) {
        return new Prelude.Bind(either(function (e) {
            return function (_) {
                return new Left(e);
            };
        })(function (a) {
            return function (f) {
                return f(a);
            };
        }), applyEither);
    };
    var applicativeEither = function (__unused) {
        return new Prelude.Applicative(applyEither, Right.create);
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
    var maybe = function (_79) {
        return function (_80) {
            return function (_81) {
                if (_81 instanceof Nothing) {
                    return _79;
                };
                if (_81 instanceof Just) {
                    return _80(_81.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var functorMaybe = function (__unused) {
        return new Prelude.Functor(function (_82) {
            return function (_83) {
                if (_83 instanceof Just) {
                    return new Just(_82(_83.value0));
                };
                return Nothing.value;
            };
        });
    };
    var fromMaybe = function (a) {
        return maybe(a)(Prelude.id(Prelude.categoryArr({})));
    };
    var applyMaybe = function (__unused) {
        return new Prelude.Apply(function (_84) {
            return function (_85) {
                if (_84 instanceof Just) {
                    return Prelude["<$>"](functorMaybe({}))(_84.value0)(_85);
                };
                if (_84 instanceof Nothing) {
                    return Nothing.value;
                };
                throw new Error("Failed pattern match");
            };
        }, functorMaybe);
    };
    var bindMaybe = function (__unused) {
        return new Prelude.Bind(function (_88) {
            return function (_89) {
                if (_88 instanceof Just) {
                    return _89(_88.value0);
                };
                if (_88 instanceof Nothing) {
                    return Nothing.value;
                };
                throw new Error("Failed pattern match");
            };
        }, applyMaybe);
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
PS.Data_Array = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function append (l1) {  return function (l2) {    return l1.concat(l2);  };};
    function slice (s) {  return function (e) {    return function (l) {      return l.slice(s, e);    };  };};
    function concatMap (f) {  return function (arr) {    var result = [];    for (var i = 0, l = arr.length; i < l; i++) {      Array.prototype.push.apply(result, f(arr[i]));    }    return result;  };};
    function map (f) {  return function (arr) {    var l = arr.length;    var result = new Array(l);    for (var i = 0; i < l; i++) {      result[i] = f(arr[i]);    }    return result;  };};
    var take = function (n) {
        return slice(0)(n);
    };
    var semigroupArray = function (__unused) {
        return new Prelude.Semigroup(append);
    };
    var functorArray = function (__unused) {
        return new Prelude.Functor(map);
    };
    return {
        concatMap: concatMap, 
        take: take, 
        append: append, 
        map: map, 
        functorArray: functorArray, 
        semigroupArray: semigroupArray
    };
})();
var PS = PS || {};
PS.Data_Foreign = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Either = PS.Data_Either;
    var Data_Function = PS.Data_Function;
    function TypeMismatch(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    TypeMismatch.create = function (value0) {
        return function (value1) {
            return new TypeMismatch(value0, value1);
        };
    };
    function ErrorAtIndex(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtIndex.create = function (value0) {
        return function (value1) {
            return new ErrorAtIndex(value0, value1);
        };
    };
    function ErrorAtProperty(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ErrorAtProperty.create = function (value0) {
        return function (value1) {
            return new ErrorAtProperty(value0, value1);
        };
    };
    function JSONError(value0) {
        this.value0 = value0;
    };
    JSONError.create = function (value0) {
        return new JSONError(value0);
    };
    function parseJSONImpl(left, right, str) {  try {    return right(JSON.parse(str));  } catch (e) {    return left(e.toString());  } };
    function unsafeFromForeign(value) {  return value;};
    function typeOf(value) {  return typeof value;};
    function tagOf(value) {  return Object.prototype.toString.call(value).slice(8, -1);};
    function isNull(value) {  return value === null;};
    function isUndefined(value) {  return value === undefined;};
    function isArray(value) {  return Array.isArray(value);};
    var unsafeReadPrim = function (_116) {
        return function (_117) {
            if (tagOf(_117) === _116) {
                return Prelude.pure(Data_Either.applicativeEither({}))(unsafeFromForeign(_117));
            };
            return new Data_Either.Left(new TypeMismatch(_116, tagOf(_117)));
        };
    };
    var showForeignError = function (__unused) {
        return new Prelude.Show(function (_119) {
            if (_119 instanceof TypeMismatch) {
                return "Type mismatch: expected " + _119.value0 + ", found " + _119.value1;
            };
            if (_119 instanceof ErrorAtIndex) {
                return "Error at array index " + Prelude.show(Prelude.showNumber({}))(_119.value0) + ": " + Prelude.show(showForeignError({}))(_119.value1);
            };
            if (_119 instanceof ErrorAtProperty) {
                return "Error at property " + Prelude.show(Prelude.showString({}))(_119.value0) + ": " + Prelude.show(showForeignError({}))(_119.value1);
            };
            if (_119 instanceof JSONError) {
                return "JSON error: " + _119.value0;
            };
            throw new Error("Failed pattern match");
        });
    };
    var readString = unsafeReadPrim("String");
    var readArray = function (_118) {
        if (isArray(_118)) {
            return Prelude.pure(Data_Either.applicativeEither({}))(unsafeFromForeign(_118));
        };
        return new Data_Either.Left(new TypeMismatch("array", tagOf(_118)));
    };
    var parseJSON = function (json) {
        return parseJSONImpl(Prelude["<<<"](Prelude.semigroupoidArr({}))(Data_Either.Left.create)(JSONError.create), Data_Either.Right.create, json);
    };
    return {
        TypeMismatch: TypeMismatch, 
        ErrorAtIndex: ErrorAtIndex, 
        ErrorAtProperty: ErrorAtProperty, 
        JSONError: JSONError, 
        readArray: readArray, 
        readString: readString, 
        isArray: isArray, 
        isUndefined: isUndefined, 
        isNull: isNull, 
        tagOf: tagOf, 
        typeOf: typeOf, 
        unsafeFromForeign: unsafeFromForeign, 
        parseJSON: parseJSON, 
        showForeignError: showForeignError
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
PS.Data_Foreign_Index = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Function = PS.Data_Function;
    var Data_Either = PS.Data_Either;
    var Data_Foreign = PS.Data_Foreign;
    function Index($bang, errorAt, hasOwnProperty, hasProperty) {
        this["!"] = $bang;
        this.errorAt = errorAt;
        this.hasOwnProperty = hasOwnProperty;
        this.hasProperty = hasProperty;
    };
    function unsafeReadPropImpl(f, s, key, value) {   if (value && typeof value === 'object') {    return s(value[key]);  } else {    return f;  }};
    function unsafeHasOwnProperty(prop, value) {  return value.hasOwnProperty(prop);};
    function unsafeHasProperty(prop, value) {  return prop in value;};
    var $bang = function (dict) {
        return dict["!"];
    };
    var unsafeReadProp = function (k) {
        return function (value) {
            return unsafeReadPropImpl(new Data_Either.Left(new Data_Foreign.TypeMismatch("object", Data_Foreign.typeOf(value))), Prelude.pure(Data_Either.applicativeEither({})), k, value);
        };
    };
    var prop = unsafeReadProp;
    var hasPropertyImpl = function (_122) {
        return function (_123) {
            if (Data_Foreign.isNull(_123)) {
                return false;
            };
            if (Data_Foreign.isUndefined(_123)) {
                return false;
            };
            if (Data_Foreign.typeOf(_123) === "object" || Data_Foreign.typeOf(_123) === "function") {
                return unsafeHasProperty(_122, _123);
            };
            return false;
        };
    };
    var hasOwnPropertyImpl = function (_120) {
        return function (_121) {
            if (Data_Foreign.isNull(_121)) {
                return false;
            };
            if (Data_Foreign.isUndefined(_121)) {
                return false;
            };
            if (Data_Foreign.typeOf(_121) === "object" || Data_Foreign.typeOf(_121) === "function") {
                return unsafeHasOwnProperty(_120, _121);
            };
            return false;
        };
    };
    var indexString = function (__unused) {
        return new Index(Prelude.flip(prop), Data_Foreign.ErrorAtProperty.create, hasOwnPropertyImpl, hasPropertyImpl);
    };
    var errorAt = function (dict) {
        return dict.errorAt;
    };
    return {
        Index: Index, 
        errorAt: errorAt, 
        "!": $bang, 
        prop: prop, 
        indexString: indexString
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
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return {
        Tuple: Tuple
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
    var foldableArray = function (__unused) {
        return new Foldable(function (__dict_Monoid_102) {
            return function (f) {
                return function (xs) {
                    return foldr(foldableArray({}))(function (x) {
                        return function (acc) {
                            return Prelude["<>"](__dict_Monoid_102["__superclass_Prelude.Semigroup_0"]({}))(f(x))(acc);
                        };
                    })(Data_Monoid.mempty(__dict_Monoid_102))(xs);
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
    var toList = function (_254) {
        if (_254 instanceof Leaf) {
            return [  ];
        };
        if (_254 instanceof Two) {
            return Prelude["++"](Data_Array.semigroupArray({}))(toList(_254.value0))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_254.value1, _254.value2) ])(toList(_254.value3)));
        };
        if (_254 instanceof Three) {
            return Prelude["++"](Data_Array.semigroupArray({}))(toList(_254.value0))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_254.value1, _254.value2) ])(Prelude["++"](Data_Array.semigroupArray({}))(toList(_254.value3))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_254.value4, _254.value5) ])(toList(_254.value6)))));
        };
        throw new Error("Failed pattern match");
    };
    var lookup = function (__copy___dict_Ord_119) {
        return function (__copy__250) {
            return function (__copy__251) {
                var __dict_Ord_119 = __copy___dict_Ord_119;
                var _250 = __copy__250;
                var _251 = __copy__251;
                tco: while (true) {
                    if (_251 instanceof Leaf) {
                        return Data_Maybe.Nothing.value;
                    };
                    if (_251 instanceof Two && Prelude["=="](__dict_Ord_119["__superclass_Prelude.Eq_0"]({}))(_250)(_251.value1)) {
                        return new Data_Maybe.Just(_251.value2);
                    };
                    if (_251 instanceof Two && Prelude["<"](__dict_Ord_119)(_250)(_251.value1)) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__250 = _250;
                        var __tco__251 = _251.value0;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _250 = __tco__250;
                        _251 = __tco__251;
                        continue tco;
                    };
                    if (_251 instanceof Two) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__250 = _250;
                        var __tco__251 = _251.value3;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _250 = __tco__250;
                        _251 = __tco__251;
                        continue tco;
                    };
                    if (_251 instanceof Three && Prelude["=="](__dict_Ord_119["__superclass_Prelude.Eq_0"]({}))(_250)(_251.value1)) {
                        return new Data_Maybe.Just(_251.value2);
                    };
                    if (_251 instanceof Three && Prelude["=="](__dict_Ord_119["__superclass_Prelude.Eq_0"]({}))(_250)(_251.value4)) {
                        return new Data_Maybe.Just(_251.value5);
                    };
                    if (_251 instanceof Three && Prelude["<"](__dict_Ord_119)(_250)(_251.value1)) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__250 = _250;
                        var __tco__251 = _251.value0;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _250 = __tco__250;
                        _251 = __tco__251;
                        continue tco;
                    };
                    if (_251 instanceof Three && Prelude["<"](__dict_Ord_119)(_251.value1)(_250) && Prelude["<="](__dict_Ord_119)(_250)(_251.value4)) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__250 = _250;
                        var __tco__251 = _251.value3;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _250 = __tco__250;
                        _251 = __tco__251;
                        continue tco;
                    };
                    if (_251 instanceof Three) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__250 = _250;
                        var __tco__251 = _251.value6;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _250 = __tco__250;
                        _251 = __tco__251;
                        continue tco;
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
    };
    var fromZipper = function (__copy___dict_Ord_121) {
        return function (__copy__252) {
            return function (__copy__253) {
                var __dict_Ord_121 = __copy___dict_Ord_121;
                var _252 = __copy__252;
                var _253 = __copy__253;
                tco: while (true) {
                    if (_252.length === 0) {
                        return _253;
                    };
                    if (_252.length > 0) {
                        var _453 = _252.slice(1);
                        if (_252[0] instanceof TwoLeft) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__253 = new Two(_253, (_252[0]).value0, (_252[0]).value1, (_252[0]).value2);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _252 = _453;
                            _253 = __tco__253;
                            continue tco;
                        };
                    };
                    if (_252.length > 0) {
                        var _458 = _252.slice(1);
                        if (_252[0] instanceof TwoRight) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__253 = new Two((_252[0]).value0, (_252[0]).value1, (_252[0]).value2, _253);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _252 = _458;
                            _253 = __tco__253;
                            continue tco;
                        };
                    };
                    if (_252.length > 0) {
                        var _463 = _252.slice(1);
                        if (_252[0] instanceof ThreeLeft) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__253 = new Three(_253, (_252[0]).value0, (_252[0]).value1, (_252[0]).value2, (_252[0]).value3, (_252[0]).value4, (_252[0]).value5);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _252 = _463;
                            _253 = __tco__253;
                            continue tco;
                        };
                    };
                    if (_252.length > 0) {
                        var _471 = _252.slice(1);
                        if (_252[0] instanceof ThreeMiddle) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__253 = new Three((_252[0]).value0, (_252[0]).value1, (_252[0]).value2, _253, (_252[0]).value3, (_252[0]).value4, (_252[0]).value5);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _252 = _471;
                            _253 = __tco__253;
                            continue tco;
                        };
                    };
                    if (_252.length > 0) {
                        var _479 = _252.slice(1);
                        if (_252[0] instanceof ThreeRight) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__253 = new Three((_252[0]).value0, (_252[0]).value1, (_252[0]).value2, (_252[0]).value3, (_252[0]).value4, (_252[0]).value5, _253);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _252 = _479;
                            _253 = __tco__253;
                            continue tco;
                        };
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
    };
    var insert = function (__dict_Ord_122) {
        var up = function (__copy___dict_Ord_123) {
            return function (__copy__264) {
                return function (__copy__265) {
                    var __dict_Ord_123 = __copy___dict_Ord_123;
                    var _264 = __copy__264;
                    var _265 = __copy__265;
                    tco: while (true) {
                        if (_264.length === 0) {
                            return new Two(_265.value0, _265.value1, _265.value2, _265.value3);
                        };
                        if (_264.length > 0) {
                            var _497 = _264.slice(1);
                            if (_264[0] instanceof TwoLeft) {
                                return fromZipper(__dict_Ord_123)(_497)(new Three(_265.value0, _265.value1, _265.value2, _265.value3, (_264[0]).value0, (_264[0]).value1, (_264[0]).value2));
                            };
                        };
                        if (_264.length > 0) {
                            var _506 = _264.slice(1);
                            if (_264[0] instanceof TwoRight) {
                                return fromZipper(__dict_Ord_123)(_506)(new Three((_264[0]).value0, (_264[0]).value1, (_264[0]).value2, _265.value0, _265.value1, _265.value2, _265.value3));
                            };
                        };
                        if (_264.length > 0) {
                            var _515 = _264.slice(1);
                            if (_264[0] instanceof ThreeLeft) {
                                var __tco___dict_Ord_123 = __dict_Ord_123;
                                var __tco__265 = new KickUp(new Two(_265.value0, _265.value1, _265.value2, _265.value3), (_264[0]).value0, (_264[0]).value1, new Two((_264[0]).value2, (_264[0]).value3, (_264[0]).value4, (_264[0]).value5));
                                __dict_Ord_123 = __tco___dict_Ord_123;
                                _264 = _515;
                                _265 = __tco__265;
                                continue tco;
                            };
                        };
                        if (_264.length > 0) {
                            var _527 = _264.slice(1);
                            if (_264[0] instanceof ThreeMiddle) {
                                var __tco___dict_Ord_123 = __dict_Ord_123;
                                var __tco__265 = new KickUp(new Two((_264[0]).value0, (_264[0]).value1, (_264[0]).value2, _265.value0), _265.value1, _265.value2, new Two(_265.value3, (_264[0]).value3, (_264[0]).value4, (_264[0]).value5));
                                __dict_Ord_123 = __tco___dict_Ord_123;
                                _264 = _527;
                                _265 = __tco__265;
                                continue tco;
                            };
                        };
                        if (_264.length > 0) {
                            var _539 = _264.slice(1);
                            if (_264[0] instanceof ThreeRight) {
                                var __tco___dict_Ord_123 = __dict_Ord_123;
                                var __tco__265 = new KickUp(new Two((_264[0]).value0, (_264[0]).value1, (_264[0]).value2, (_264[0]).value3), (_264[0]).value4, (_264[0]).value5, new Two(_265.value0, _265.value1, _265.value2, _265.value3));
                                __dict_Ord_123 = __tco___dict_Ord_123;
                                _264 = _539;
                                _265 = __tco__265;
                                continue tco;
                            };
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var down = function (__copy___dict_Ord_124) {
            return function (__copy__260) {
                return function (__copy__261) {
                    return function (__copy__262) {
                        return function (__copy__263) {
                            var __dict_Ord_124 = __copy___dict_Ord_124;
                            var _260 = __copy__260;
                            var _261 = __copy__261;
                            var _262 = __copy__262;
                            var _263 = __copy__263;
                            tco: while (true) {
                                if (_263 instanceof Leaf) {
                                    return up(__dict_Ord_124)(_260)(new KickUp(Leaf.value, _261, _262, Leaf.value));
                                };
                                if (_263 instanceof Two && Prelude["=="](__dict_Ord_124["__superclass_Prelude.Eq_0"]({}))(_261)(_263.value1)) {
                                    return fromZipper(__dict_Ord_124)(_260)(new Two(_263.value0, _261, _262, _263.value3));
                                };
                                if (_263 instanceof Two && Prelude["<"](__dict_Ord_124)(_261)(_263.value1)) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__260 = Prelude[":"](new TwoLeft(_263.value1, _263.value2, _263.value3))(_260);
                                    var __tco__261 = _261;
                                    var __tco__262 = _262;
                                    var __tco__263 = _263.value0;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _260 = __tco__260;
                                    _261 = __tco__261;
                                    _262 = __tco__262;
                                    _263 = __tco__263;
                                    continue tco;
                                };
                                if (_263 instanceof Two) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__260 = Prelude[":"](new TwoRight(_263.value0, _263.value1, _263.value2))(_260);
                                    var __tco__261 = _261;
                                    var __tco__262 = _262;
                                    var __tco__263 = _263.value3;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _260 = __tco__260;
                                    _261 = __tco__261;
                                    _262 = __tco__262;
                                    _263 = __tco__263;
                                    continue tco;
                                };
                                if (_263 instanceof Three && Prelude["=="](__dict_Ord_124["__superclass_Prelude.Eq_0"]({}))(_261)(_263.value1)) {
                                    return fromZipper(__dict_Ord_124)(_260)(new Three(_263.value0, _261, _262, _263.value3, _263.value4, _263.value5, _263.value6));
                                };
                                if (_263 instanceof Three && Prelude["=="](__dict_Ord_124["__superclass_Prelude.Eq_0"]({}))(_261)(_263.value4)) {
                                    return fromZipper(__dict_Ord_124)(_260)(new Three(_263.value0, _263.value1, _263.value2, _263.value3, _261, _262, _263.value6));
                                };
                                if (_263 instanceof Three && Prelude["<"](__dict_Ord_124)(_261)(_263.value1)) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__260 = Prelude[":"](new ThreeLeft(_263.value1, _263.value2, _263.value3, _263.value4, _263.value5, _263.value6))(_260);
                                    var __tco__261 = _261;
                                    var __tco__262 = _262;
                                    var __tco__263 = _263.value0;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _260 = __tco__260;
                                    _261 = __tco__261;
                                    _262 = __tco__262;
                                    _263 = __tco__263;
                                    continue tco;
                                };
                                if (_263 instanceof Three && Prelude["<"](__dict_Ord_124)(_263.value1)(_261) && Prelude["<="](__dict_Ord_124)(_261)(_263.value4)) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__260 = Prelude[":"](new ThreeMiddle(_263.value0, _263.value1, _263.value2, _263.value4, _263.value5, _263.value6))(_260);
                                    var __tco__261 = _261;
                                    var __tco__262 = _262;
                                    var __tco__263 = _263.value3;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _260 = __tco__260;
                                    _261 = __tco__261;
                                    _262 = __tco__262;
                                    _263 = __tco__263;
                                    continue tco;
                                };
                                if (_263 instanceof Three) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__260 = Prelude[":"](new ThreeRight(_263.value0, _263.value1, _263.value2, _263.value3, _263.value4, _263.value5))(_260);
                                    var __tco__261 = _261;
                                    var __tco__262 = _262;
                                    var __tco__263 = _263.value6;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _260 = __tco__260;
                                    _261 = __tco__261;
                                    _262 = __tco__262;
                                    _263 = __tco__263;
                                    continue tco;
                                };
                                throw new Error("Failed pattern match");
                            };
                        };
                    };
                };
            };
        };
        return down(__dict_Ord_122)([  ]);
    };
    var empty = Leaf.value;
    var $$delete = function (__dict_Ord_130) {
        var up = function (__copy___dict_Ord_131) {
            return function (__copy__269) {
                return function (__copy__270) {
                    var __dict_Ord_131 = __copy___dict_Ord_131;
                    var _269 = __copy__269;
                    var _270 = __copy__270;
                    tco: while (true) {
                        if (_269.length === 0) {
                            return _270;
                        };
                        if (_269.length > 0) {
                            var _600 = _269.slice(1);
                            if (_269[0] instanceof TwoLeft && (_269[0]).value2 instanceof Leaf && _270 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_600)(new Two(Leaf.value, (_269[0]).value0, (_269[0]).value1, Leaf.value));
                            };
                        };
                        if (_269.length > 0) {
                            var _605 = _269.slice(1);
                            if (_269[0] instanceof TwoRight && (_269[0]).value0 instanceof Leaf && _270 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_605)(new Two(Leaf.value, (_269[0]).value1, (_269[0]).value2, Leaf.value));
                            };
                        };
                        if (_269.length > 0) {
                            var _610 = _269.slice(1);
                            if (_269[0] instanceof TwoLeft && (_269[0]).value2 instanceof Two) {
                                var __tco___dict_Ord_131 = __dict_Ord_131;
                                var __tco__270 = new Three(_270, (_269[0]).value0, (_269[0]).value1, (_269[0]).value2.value0, (_269[0]).value2.value1, (_269[0]).value2.value2, (_269[0]).value2.value3);
                                __dict_Ord_131 = __tco___dict_Ord_131;
                                _269 = _610;
                                _270 = __tco__270;
                                continue tco;
                            };
                        };
                        if (_269.length > 0) {
                            var _619 = _269.slice(1);
                            if (_269[0] instanceof TwoRight && (_269[0]).value0 instanceof Two) {
                                var __tco___dict_Ord_131 = __dict_Ord_131;
                                var __tco__270 = new Three((_269[0]).value0.value0, (_269[0]).value0.value1, (_269[0]).value0.value2, (_269[0]).value0.value3, (_269[0]).value1, (_269[0]).value2, _270);
                                __dict_Ord_131 = __tco___dict_Ord_131;
                                _269 = _619;
                                _270 = __tco__270;
                                continue tco;
                            };
                        };
                        if (_269.length > 0) {
                            var _628 = _269.slice(1);
                            if (_269[0] instanceof TwoLeft && (_269[0]).value2 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_628)(new Two(new Two(_270, (_269[0]).value0, (_269[0]).value1, (_269[0]).value2.value0), (_269[0]).value2.value1, (_269[0]).value2.value2, new Two((_269[0]).value2.value3, (_269[0]).value2.value4, (_269[0]).value2.value5, (_269[0]).value2.value6)));
                            };
                        };
                        if (_269.length > 0) {
                            var _640 = _269.slice(1);
                            if (_269[0] instanceof TwoRight && (_269[0]).value0 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_640)(new Two(new Two((_269[0]).value0.value0, (_269[0]).value0.value1, (_269[0]).value0.value2, (_269[0]).value0.value3), (_269[0]).value0.value4, (_269[0]).value0.value5, new Two((_269[0]).value0.value6, (_269[0]).value1, (_269[0]).value2, _270)));
                            };
                        };
                        if (_269.length > 0) {
                            var _652 = _269.slice(1);
                            if (_269[0] instanceof ThreeLeft && (_269[0]).value2 instanceof Leaf && (_269[0]).value5 instanceof Leaf && _270 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_652)(new Three(Leaf.value, (_269[0]).value0, (_269[0]).value1, Leaf.value, (_269[0]).value3, (_269[0]).value4, Leaf.value));
                            };
                        };
                        if (_269.length > 0) {
                            var _660 = _269.slice(1);
                            if (_269[0] instanceof ThreeMiddle && (_269[0]).value0 instanceof Leaf && (_269[0]).value5 instanceof Leaf && _270 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_660)(new Three(Leaf.value, (_269[0]).value1, (_269[0]).value2, Leaf.value, (_269[0]).value3, (_269[0]).value4, Leaf.value));
                            };
                        };
                        if (_269.length > 0) {
                            var _668 = _269.slice(1);
                            if (_269[0] instanceof ThreeRight && (_269[0]).value0 instanceof Leaf && (_269[0]).value3 instanceof Leaf && _270 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_668)(new Three(Leaf.value, (_269[0]).value1, (_269[0]).value2, Leaf.value, (_269[0]).value4, (_269[0]).value5, Leaf.value));
                            };
                        };
                        if (_269.length > 0) {
                            var _676 = _269.slice(1);
                            if (_269[0] instanceof ThreeLeft && (_269[0]).value2 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_676)(new Two(new Three(_270, (_269[0]).value0, (_269[0]).value1, (_269[0]).value2.value0, (_269[0]).value2.value1, (_269[0]).value2.value2, (_269[0]).value2.value3), (_269[0]).value3, (_269[0]).value4, (_269[0]).value5));
                            };
                        };
                        if (_269.length > 0) {
                            var _688 = _269.slice(1);
                            if (_269[0] instanceof ThreeMiddle && (_269[0]).value0 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_688)(new Two(new Three((_269[0]).value0.value0, (_269[0]).value0.value1, (_269[0]).value0.value2, (_269[0]).value0.value3, (_269[0]).value1, (_269[0]).value2, _270), (_269[0]).value3, (_269[0]).value4, (_269[0]).value5));
                            };
                        };
                        if (_269.length > 0) {
                            var _700 = _269.slice(1);
                            if (_269[0] instanceof ThreeMiddle && (_269[0]).value5 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_700)(new Two((_269[0]).value0, (_269[0]).value1, (_269[0]).value2, new Three(_270, (_269[0]).value3, (_269[0]).value4, (_269[0]).value5.value0, (_269[0]).value5.value1, (_269[0]).value5.value2, (_269[0]).value5.value3)));
                            };
                        };
                        if (_269.length > 0) {
                            var _712 = _269.slice(1);
                            if (_269[0] instanceof ThreeRight && (_269[0]).value3 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_712)(new Two((_269[0]).value0, (_269[0]).value1, (_269[0]).value2, new Three((_269[0]).value3.value0, (_269[0]).value3.value1, (_269[0]).value3.value2, (_269[0]).value3.value3, (_269[0]).value4, (_269[0]).value5, _270)));
                            };
                        };
                        if (_269.length > 0) {
                            var _724 = _269.slice(1);
                            if (_269[0] instanceof ThreeLeft && (_269[0]).value2 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_724)(new Three(new Two(_270, (_269[0]).value0, (_269[0]).value1, (_269[0]).value2.value0), (_269[0]).value2.value1, (_269[0]).value2.value2, new Two((_269[0]).value2.value3, (_269[0]).value2.value4, (_269[0]).value2.value5, (_269[0]).value2.value6), (_269[0]).value3, (_269[0]).value4, (_269[0]).value5));
                            };
                        };
                        if (_269.length > 0) {
                            var _739 = _269.slice(1);
                            if (_269[0] instanceof ThreeMiddle && (_269[0]).value0 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_739)(new Three(new Two((_269[0]).value0.value0, (_269[0]).value0.value1, (_269[0]).value0.value2, (_269[0]).value0.value3), (_269[0]).value0.value4, (_269[0]).value0.value5, new Two((_269[0]).value0.value6, (_269[0]).value1, (_269[0]).value2, _270), (_269[0]).value3, (_269[0]).value4, (_269[0]).value5));
                            };
                        };
                        if (_269.length > 0) {
                            var _754 = _269.slice(1);
                            if (_269[0] instanceof ThreeMiddle && (_269[0]).value5 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_754)(new Three((_269[0]).value0, (_269[0]).value1, (_269[0]).value2, new Two(_270, (_269[0]).value3, (_269[0]).value4, (_269[0]).value5.value0), (_269[0]).value5.value1, (_269[0]).value5.value2, new Two((_269[0]).value5.value3, (_269[0]).value5.value4, (_269[0]).value5.value5, (_269[0]).value5.value6)));
                            };
                        };
                        if (_269.length > 0) {
                            var _769 = _269.slice(1);
                            if (_269[0] instanceof ThreeRight && (_269[0]).value3 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_769)(new Three((_269[0]).value0, (_269[0]).value1, (_269[0]).value2, new Two((_269[0]).value3.value0, (_269[0]).value3.value1, (_269[0]).value3.value2, (_269[0]).value3.value3), (_269[0]).value3.value4, (_269[0]).value3.value5, new Two((_269[0]).value3.value6, (_269[0]).value4, (_269[0]).value5, _270)));
                            };
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var removeMaxNode = function (__copy___dict_Ord_132) {
            return function (__copy__272) {
                return function (__copy__273) {
                    var __dict_Ord_132 = __copy___dict_Ord_132;
                    var _272 = __copy__272;
                    var _273 = __copy__273;
                    tco: while (true) {
                        if (_273 instanceof Two && _273.value0 instanceof Leaf && _273.value3 instanceof Leaf) {
                            return up(__dict_Ord_132)(_272)(Leaf.value);
                        };
                        if (_273 instanceof Two) {
                            var __tco___dict_Ord_132 = __dict_Ord_132;
                            var __tco__272 = Prelude[":"](new TwoRight(_273.value0, _273.value1, _273.value2))(_272);
                            var __tco__273 = _273.value3;
                            __dict_Ord_132 = __tco___dict_Ord_132;
                            _272 = __tco__272;
                            _273 = __tco__273;
                            continue tco;
                        };
                        if (_273 instanceof Three && _273.value0 instanceof Leaf && _273.value3 instanceof Leaf && _273.value6 instanceof Leaf) {
                            return up(__dict_Ord_132)(Prelude[":"](new TwoRight(Leaf.value, _273.value1, _273.value2))(_272))(Leaf.value);
                        };
                        if (_273 instanceof Three) {
                            var __tco___dict_Ord_132 = __dict_Ord_132;
                            var __tco__272 = Prelude[":"](new ThreeRight(_273.value0, _273.value1, _273.value2, _273.value3, _273.value4, _273.value5))(_272);
                            var __tco__273 = _273.value6;
                            __dict_Ord_132 = __tco___dict_Ord_132;
                            _272 = __tco__272;
                            _273 = __tco__273;
                            continue tco;
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var maxNode = function (__copy___dict_Ord_133) {
            return function (__copy__271) {
                var __dict_Ord_133 = __copy___dict_Ord_133;
                var _271 = __copy__271;
                tco: while (true) {
                    if (_271 instanceof Two && _271.value3 instanceof Leaf) {
                        return {
                            key: _271.value1, 
                            value: _271.value2
                        };
                    };
                    if (_271 instanceof Two) {
                        var __tco___dict_Ord_133 = __dict_Ord_133;
                        var __tco__271 = _271.value3;
                        __dict_Ord_133 = __tco___dict_Ord_133;
                        _271 = __tco__271;
                        continue tco;
                    };
                    if (_271 instanceof Three && _271.value6 instanceof Leaf) {
                        return {
                            key: _271.value4, 
                            value: _271.value5
                        };
                    };
                    if (_271 instanceof Three) {
                        var __tco___dict_Ord_133 = __dict_Ord_133;
                        var __tco__271 = _271.value6;
                        __dict_Ord_133 = __tco___dict_Ord_133;
                        _271 = __tco__271;
                        continue tco;
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
        var down = function (__copy___dict_Ord_134) {
            return function (__copy__266) {
                return function (__copy__267) {
                    return function (__copy__268) {
                        var __dict_Ord_134 = __copy___dict_Ord_134;
                        var _266 = __copy__266;
                        var _267 = __copy__267;
                        var _268 = __copy__268;
                        tco: while (true) {
                            if (_268 instanceof Leaf) {
                                return fromZipper(__dict_Ord_134)(_266)(Leaf.value);
                            };
                            if (_268 instanceof Two && _268.value0 instanceof Leaf && _268.value3 instanceof Leaf && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_267)(_268.value1)) {
                                return up(__dict_Ord_134)(_266)(Leaf.value);
                            };
                            if (_268 instanceof Two && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_267)(_268.value1)) {
                                var max = maxNode(__dict_Ord_134)(_268.value0);
                                return removeMaxNode(__dict_Ord_134)(Prelude[":"](new TwoLeft(max.key, max.value, _268.value3))(_266))(_268.value0);
                            };
                            if (_268 instanceof Two && Prelude["<"](__dict_Ord_134)(_267)(_268.value1)) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__266 = Prelude[":"](new TwoLeft(_268.value1, _268.value2, _268.value3))(_266);
                                var __tco__267 = _267;
                                var __tco__268 = _268.value0;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _266 = __tco__266;
                                _267 = __tco__267;
                                _268 = __tco__268;
                                continue tco;
                            };
                            if (_268 instanceof Two) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__266 = Prelude[":"](new TwoRight(_268.value0, _268.value1, _268.value2))(_266);
                                var __tco__267 = _267;
                                var __tco__268 = _268.value3;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _266 = __tco__266;
                                _267 = __tco__267;
                                _268 = __tco__268;
                                continue tco;
                            };
                            if (_268 instanceof Three && _268.value0 instanceof Leaf && _268.value3 instanceof Leaf && _268.value6 instanceof Leaf && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_267)(_268.value1)) {
                                return fromZipper(__dict_Ord_134)(_266)(new Two(Leaf.value, _268.value4, _268.value5, Leaf.value));
                            };
                            if (_268 instanceof Three && _268.value0 instanceof Leaf && _268.value3 instanceof Leaf && _268.value6 instanceof Leaf && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_267)(_268.value4)) {
                                return fromZipper(__dict_Ord_134)(_266)(new Two(Leaf.value, _268.value1, _268.value2, Leaf.value));
                            };
                            if (_268 instanceof Three && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_267)(_268.value1)) {
                                var max = maxNode(__dict_Ord_134)(_268.value0);
                                return removeMaxNode(__dict_Ord_134)(Prelude[":"](new ThreeLeft(max.key, max.value, _268.value3, _268.value4, _268.value5, _268.value6))(_266))(_268.value0);
                            };
                            if (_268 instanceof Three && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_267)(_268.value4)) {
                                var max = maxNode(__dict_Ord_134)(_268.value3);
                                return removeMaxNode(__dict_Ord_134)(Prelude[":"](new ThreeMiddle(_268.value0, _268.value1, _268.value2, max.key, max.value, _268.value6))(_266))(_268.value3);
                            };
                            if (_268 instanceof Three && Prelude["<"](__dict_Ord_134)(_267)(_268.value1)) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__266 = Prelude[":"](new ThreeLeft(_268.value1, _268.value2, _268.value3, _268.value4, _268.value5, _268.value6))(_266);
                                var __tco__267 = _267;
                                var __tco__268 = _268.value0;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _266 = __tco__266;
                                _267 = __tco__267;
                                _268 = __tco__268;
                                continue tco;
                            };
                            if (_268 instanceof Three && Prelude["<"](__dict_Ord_134)(_268.value1)(_267) && Prelude["<"](__dict_Ord_134)(_267)(_268.value4)) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__266 = Prelude[":"](new ThreeMiddle(_268.value0, _268.value1, _268.value2, _268.value4, _268.value5, _268.value6))(_266);
                                var __tco__267 = _267;
                                var __tco__268 = _268.value3;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _266 = __tco__266;
                                _267 = __tco__267;
                                _268 = __tco__268;
                                continue tco;
                            };
                            if (_268 instanceof Three) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__266 = Prelude[":"](new ThreeRight(_268.value0, _268.value1, _268.value2, _268.value3, _268.value4, _268.value5))(_266);
                                var __tco__267 = _267;
                                var __tco__268 = _268.value6;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _266 = __tco__266;
                                _267 = __tco__267;
                                _268 = __tco__268;
                                continue tco;
                            };
                            throw new Error("Failed pattern match");
                        };
                    };
                };
            };
        };
        return down(__dict_Ord_130)([  ]);
    };
    var alter = function (__dict_Ord_135) {
        return function (f) {
            return function (k) {
                return function (m) {
                    var _898 = f(lookup(__dict_Ord_135)(k)(m));
                    if (_898 instanceof Data_Maybe.Nothing) {
                        return $$delete(__dict_Ord_135)(k)(m);
                    };
                    if (_898 instanceof Data_Maybe.Just) {
                        return insert(__dict_Ord_135)(k)(_898.value0)(m);
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
    var traversableArray = function (__unused) {
        return new Traversable(function (__unused) {
            return Data_Foldable.foldableArray({});
        }, function (__unused) {
            return Data_Array.functorArray({});
        }, function (__dict_Applicative_157) {
            return function (_301) {
                if (_301.length === 0) {
                    return Prelude.pure(__dict_Applicative_157)([  ]);
                };
                if (_301.length > 0) {
                    var _902 = _301.slice(1);
                    return Prelude["<*>"](__dict_Applicative_157["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_157["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_301[0]))(sequence(traversableArray({}))(__dict_Applicative_157)(_902));
                };
                throw new Error("Failed pattern match");
            };
        }, function (__dict_Applicative_156) {
            return function (_299) {
                return function (_300) {
                    if (_300.length === 0) {
                        return Prelude.pure(__dict_Applicative_156)([  ]);
                    };
                    if (_300.length > 0) {
                        var _906 = _300.slice(1);
                        return Prelude["<*>"](__dict_Applicative_156["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_156["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_299(_300[0])))(traverse(traversableArray({}))(__dict_Applicative_156)(_299)(_906));
                    };
                    throw new Error("Failed pattern match");
                };
            };
        });
    };
    var $$for = function (__dict_Applicative_159) {
        return function (__dict_Traversable_160) {
            return function (x) {
                return function (f) {
                    return traverse(__dict_Traversable_160)(__dict_Applicative_159)(f)(x);
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
PS.Data_Foreign_Class = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Foreign = PS.Data_Foreign;
    var Data_Either = PS.Data_Either;
    var Data_Foreign_Index = PS.Data_Foreign_Index;
    function IsForeign(read) {
        this.read = read;
    };
    var stringIsForeign = function (__unused) {
        return new IsForeign(Data_Foreign.readString);
    };
    var read = function (dict) {
        return dict.read;
    };
    var readWith = function (__dict_IsForeign_162) {
        return function (f) {
            return function (value) {
                return Data_Either.either(Prelude["<<<"](Prelude.semigroupoidArr({}))(Data_Either.Left.create)(f))(Data_Either.Right.create)(read(__dict_IsForeign_162)(value));
            };
        };
    };
    var readProp = function (__dict_IsForeign_163) {
        return function (__dict_Index_164) {
            return function (prop) {
                return function (value) {
                    return Prelude[">>="](Data_Either.bindEither({}))(Data_Foreign_Index["!"](__dict_Index_164)(value)(prop))(readWith(__dict_IsForeign_163)(Data_Foreign_Index.errorAt(__dict_Index_164)(prop)));
                };
            };
        };
    };
    return {
        IsForeign: IsForeign, 
        readProp: readProp, 
        readWith: readWith, 
        read: read, 
        stringIsForeign: stringIsForeign
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
        var go = function (_333) {
            return function (_334) {
                return Prelude["<>"](Data_Array.semigroupArray({}))(Data_Maybe.maybe([  ])(function (a$prime) {
                    return [ new Data_Tuple.Tuple(_333, a$prime) ];
                })(_334.value0))(Data_Array.concatMap(function (_331) {
                    return go(_333 + _331.value0)(_331.value1);
                })(Data_Map.toList(_334.value1)));
            };
        };
        return go("");
    })();
    var lookupAll = function (s) {
        var go = function (_337) {
            return function (_338) {
                if (_337 >= Data_String.length(s)) {
                    return new Data_Maybe.Just(_338);
                };
                return Prelude[">>="](Data_Maybe.bindMaybe({}))(Data_Map.lookup(Prelude.ordString({}))(Data_String.charAt(_337)(s))(_338.value1))(function (_21) {
                    return go(_337 + 1)(_21);
                });
            };
        };
        return go(0);
    };
    var empty = new Trie(Data_Maybe.Nothing.value, Data_Map.empty);
    var insert = function (s) {
        return function (a) {
            var go = function (_335) {
                return function (_336) {
                    if (_335 >= Data_String.length(s)) {
                        return new Trie(new Data_Maybe.Just(a), _336.value1);
                    };
                    return new Trie(_336.value0, Data_Map.alter(Prelude.ordString({}))(Prelude["<<<"](Prelude.semigroupoidArr({}))(Data_Maybe.Just.create)(Prelude["<<<"](Prelude.semigroupoidArr({}))(go(_335 + 1))(Data_Maybe.fromMaybe(empty))))(Data_String.charAt(_335)(s))(_336.value1));
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
PS.Main = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Trie = PS.Data_Trie;
    var Data_String = PS.Data_String;
    var Data_Foreign_Class = PS.Data_Foreign_Class;
    var Data_Either = PS.Data_Either;
    var Data_Foreign_Index = PS.Data_Foreign_Index;
    var Control_Monad_Eff = PS.Control_Monad_Eff;
    var Control_Monad_Eff_DOM = PS.Control_Monad_Eff_DOM;
    var Data_Foreign = PS.Data_Foreign;
    var Data_Array = PS.Data_Array;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Traversable = PS.Data_Traversable;
    var Data_Foldable = PS.Data_Foldable;
    var Control_Monad_Eff_AJAX = PS.Control_Monad_Eff_AJAX;
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
    var runSearch = function (_341) {
        return function (_342) {
            if (_342 === "") {
                return Data_Maybe.Nothing.value;
            };
            return Prelude["<$>"](Data_Maybe.functorMaybe({}))(Data_Trie.toArray)(Data_Trie.lookupAll(Data_String.toLower(_342))(_341));
        };
    };
    var isForeignEntry = function (__unused) {
        return new Data_Foreign_Class.IsForeign(function (entry) {
            return Prelude["<*>"](Data_Either.applyEither({}))(Prelude["<*>"](Data_Either.applyEither({}))(Prelude["<$>"](Data_Either.functorEither({}))(Entry.create)(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign({}))(Data_Foreign_Index.indexString({}))("module")(entry)))(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign({}))(Data_Foreign_Index.indexString({}))("name")(entry)))(Data_Foreign_Class.readProp(Data_Foreign_Class.stringIsForeign({}))(Data_Foreign_Index.indexString({}))("detail")(entry));
        });
    };
    var getQuery = Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.querySelector("#searchInput"))(function (_24) {
        if (_24 instanceof Data_Maybe.Just) {
            return function __do() {
                var _23 = Control_Monad_Eff_DOM.getValue(_24.value0)();
                var _931 = Data_Foreign.readString(_23);
                if (_931 instanceof Data_Either.Right) {
                    return _931.value0;
                };
                if (_931 instanceof Data_Either.Left) {
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
                    return Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.setInnerHTML("")(_26.value0))(function (__unused) {
                        var _938 = runSearch(trie)(_27);
                        if (_938 instanceof Data_Maybe.Nothing) {
                            return Prelude["return"](Control_Monad_Eff.monadEff({}))(Prelude.unit);
                        };
                        if (_938 instanceof Data_Maybe.Just) {
                            return Control_Monad_Eff.foreachE(Data_Array.take(20)(_938.value0))(function (_339) {
                                return function __do() {
                                    var _25 = Control_Monad_Eff_DOM.createElement("div")();
                                    Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("h2"))(Control_Monad_Eff_DOM.setText(_339.value1.value1)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("div"))(Control_Monad_Eff_DOM.setText(_339.value1.value0)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("pre"))(Control_Monad_Eff_DOM.setText(_339.value1.value2)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    Control_Monad_Eff_DOM.appendChild(_25)(_26.value0)();
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
        var _948 = Prelude[">>="](Data_Either.bindEither({}))(Prelude[">>="](Data_Either.bindEither({}))(Data_Foreign.parseJSON(json))(Data_Foreign.readArray))(Data_Traversable.traverse(Data_Traversable.traversableArray({}))(Data_Either.applicativeEither({}))(Data_Foreign_Class.read(isForeignEntry({}))));
        if (_948 instanceof Data_Either.Left) {
            return error(Prelude.show(Data_Foreign.showForeignError({}))(_948.value0));
        };
        if (_948 instanceof Data_Either.Right) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray({}))(function (t) {
                return function (_340) {
                    return Data_Trie.insert(Data_String.toLower(_340.value1))(_340)(t);
                };
            })(Data_Trie.empty)(_948.value0);
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
        isForeignEntry: isForeignEntry
    };
})();
PS.Main.main();