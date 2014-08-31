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
                var _351 = compare(__dict_Ord_10)(a1)(a2);
                if (_351 instanceof LT) {
                    return true;
                };
                return false;
            };
        };
    };
    var $less$eq = function (__dict_Ord_11) {
        return function (a1) {
            return function (a2) {
                var _352 = compare(__dict_Ord_11)(a1)(a2);
                if (_352 instanceof GT) {
                    return false;
                };
                return true;
            };
        };
    };
    var $greater$eq = function (__dict_Ord_13) {
        return function (a1) {
            return function (a2) {
                var _353 = compare(__dict_Ord_13)(a1)(a2);
                if (_353 instanceof LT) {
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
    function drop(n) {  return function(s) {    return s.substr(n);  };};
    function toLower(s) {  return s.toLowerCase();};
    return {
        toLower: toLower, 
        drop: drop, 
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
        return new Prelude.Functor(function (_76) {
            return function (_77) {
                if (_77 instanceof Left) {
                    return new Left(_77.value0);
                };
                if (_77 instanceof Right) {
                    return new Right(_76(_77.value0));
                };
                throw new Error("Failed pattern match");
            };
        });
    };
    var either = function (_73) {
        return function (_74) {
            return function (_75) {
                if (_75 instanceof Left) {
                    return _73(_75.value0);
                };
                if (_75 instanceof Right) {
                    return _74(_75.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var applyEither = function (__unused) {
        return new Prelude.Apply(function (_78) {
            return function (_79) {
                if (_78 instanceof Left) {
                    return new Left(_78.value0);
                };
                if (_78 instanceof Right) {
                    return Prelude["<$>"](functorEither({}))(_78.value0)(_79);
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
    var maybe = function (_87) {
        return function (_88) {
            return function (_89) {
                if (_89 instanceof Nothing) {
                    return _87;
                };
                if (_89 instanceof Just) {
                    return _88(_89.value0);
                };
                throw new Error("Failed pattern match");
            };
        };
    };
    var functorMaybe = function (__unused) {
        return new Prelude.Functor(function (_90) {
            return function (_91) {
                if (_91 instanceof Just) {
                    return new Just(_90(_91.value0));
                };
                return Nothing.value;
            };
        });
    };
    var fromMaybe = function (a) {
        return maybe(a)(Prelude.id(Prelude.categoryArr({})));
    };
    var applyMaybe = function (__unused) {
        return new Prelude.Apply(function (_92) {
            return function (_93) {
                if (_92 instanceof Just) {
                    return Prelude["<$>"](functorMaybe({}))(_92.value0)(_93);
                };
                if (_92 instanceof Nothing) {
                    return Nothing.value;
                };
                throw new Error("Failed pattern match");
            };
        }, functorMaybe);
    };
    var bindMaybe = function (__unused) {
        return new Prelude.Bind(function (_96) {
            return function (_97) {
                if (_96 instanceof Just) {
                    return _97(_96.value0);
                };
                if (_96 instanceof Nothing) {
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
    var unsafeReadPrim = function (_124) {
        return function (_125) {
            if (tagOf(_125) === _124) {
                return Prelude.pure(Data_Either.applicativeEither({}))(unsafeFromForeign(_125));
            };
            return new Data_Either.Left(new TypeMismatch(_124, tagOf(_125)));
        };
    };
    var showForeignError = function (__unused) {
        return new Prelude.Show(function (_127) {
            if (_127 instanceof TypeMismatch) {
                return "Type mismatch: expected " + _127.value0 + ", found " + _127.value1;
            };
            if (_127 instanceof ErrorAtIndex) {
                return "Error at array index " + Prelude.show(Prelude.showNumber({}))(_127.value0) + ": " + Prelude.show(showForeignError({}))(_127.value1);
            };
            if (_127 instanceof ErrorAtProperty) {
                return "Error at property " + Prelude.show(Prelude.showString({}))(_127.value0) + ": " + Prelude.show(showForeignError({}))(_127.value1);
            };
            if (_127 instanceof JSONError) {
                return "JSON error: " + _127.value0;
            };
            throw new Error("Failed pattern match");
        });
    };
    var readString = unsafeReadPrim("String");
    var readArray = function (_126) {
        if (isArray(_126)) {
            return Prelude.pure(Data_Either.applicativeEither({}))(unsafeFromForeign(_126));
        };
        return new Data_Either.Left(new TypeMismatch("array", tagOf(_126)));
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
    function setValue(text) {  return function(node) {    return function() {      node.value = text;      return node;    };  };};
    function getValue(node) {  return function() {    return node.value;  };};
    function setInnerHTML(html) {  return function(node) {    return function() {      node.innerHTML = html;      return node;    };  };};
    function addEventListener(name) {  return function(handler) {    return function(node) {      return function() {        node.addEventListener(name, function(e) {          handler();        });      };    };  };};
    function documentTitle() {  return document.title;};
    function locationProtocol() {  return window.location.protocol;};
    function locationHost() {  return window.location.host;};
    function locationPathname() {  return window.location.pathname;};
    function locationSearch() {  return window.location.search;};
    function historyState() {  return window.history.state;};
    function replaceHistoryState(data) {  return function(title) {    return function(url) {      return function() {        window.history.replaceState(data, title, url);      };    };  };};
    var querySelector = function (s) {
        return querySelectorImpl(Data_Maybe.Nothing.value, Data_Maybe.Just.create, s);
    };
    return {
        replaceHistoryState: replaceHistoryState, 
        historyState: historyState, 
        locationSearch: locationSearch, 
        locationPathname: locationPathname, 
        locationHost: locationHost, 
        locationProtocol: locationProtocol, 
        documentTitle: documentTitle, 
        addEventListener: addEventListener, 
        setInnerHTML: setInnerHTML, 
        getValue: getValue, 
        setValue: setValue, 
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
    var hasPropertyImpl = function (_130) {
        return function (_131) {
            if (Data_Foreign.isNull(_131)) {
                return false;
            };
            if (Data_Foreign.isUndefined(_131)) {
                return false;
            };
            if (Data_Foreign.typeOf(_131) === "object" || Data_Foreign.typeOf(_131) === "function") {
                return unsafeHasProperty(_130, _131);
            };
            return false;
        };
    };
    var hasOwnPropertyImpl = function (_128) {
        return function (_129) {
            if (Data_Foreign.isNull(_129)) {
                return false;
            };
            if (Data_Foreign.isUndefined(_129)) {
                return false;
            };
            if (Data_Foreign.typeOf(_129) === "object" || Data_Foreign.typeOf(_129) === "function") {
                return unsafeHasOwnProperty(_128, _129);
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
    var toList = function (_262) {
        if (_262 instanceof Leaf) {
            return [  ];
        };
        if (_262 instanceof Two) {
            return Prelude["++"](Data_Array.semigroupArray({}))(toList(_262.value0))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_262.value1, _262.value2) ])(toList(_262.value3)));
        };
        if (_262 instanceof Three) {
            return Prelude["++"](Data_Array.semigroupArray({}))(toList(_262.value0))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_262.value1, _262.value2) ])(Prelude["++"](Data_Array.semigroupArray({}))(toList(_262.value3))(Prelude["++"](Data_Array.semigroupArray({}))([ new Data_Tuple.Tuple(_262.value4, _262.value5) ])(toList(_262.value6)))));
        };
        throw new Error("Failed pattern match");
    };
    var lookup = function (__copy___dict_Ord_119) {
        return function (__copy__258) {
            return function (__copy__259) {
                var __dict_Ord_119 = __copy___dict_Ord_119;
                var _258 = __copy__258;
                var _259 = __copy__259;
                tco: while (true) {
                    if (_259 instanceof Leaf) {
                        return Data_Maybe.Nothing.value;
                    };
                    if (_259 instanceof Two && Prelude["=="](__dict_Ord_119["__superclass_Prelude.Eq_0"]({}))(_258)(_259.value1)) {
                        return new Data_Maybe.Just(_259.value2);
                    };
                    if (_259 instanceof Two && Prelude["<"](__dict_Ord_119)(_258)(_259.value1)) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__258 = _258;
                        var __tco__259 = _259.value0;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _258 = __tco__258;
                        _259 = __tco__259;
                        continue tco;
                    };
                    if (_259 instanceof Two) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__258 = _258;
                        var __tco__259 = _259.value3;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _258 = __tco__258;
                        _259 = __tco__259;
                        continue tco;
                    };
                    if (_259 instanceof Three && Prelude["=="](__dict_Ord_119["__superclass_Prelude.Eq_0"]({}))(_258)(_259.value1)) {
                        return new Data_Maybe.Just(_259.value2);
                    };
                    if (_259 instanceof Three && Prelude["=="](__dict_Ord_119["__superclass_Prelude.Eq_0"]({}))(_258)(_259.value4)) {
                        return new Data_Maybe.Just(_259.value5);
                    };
                    if (_259 instanceof Three && Prelude["<"](__dict_Ord_119)(_258)(_259.value1)) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__258 = _258;
                        var __tco__259 = _259.value0;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _258 = __tco__258;
                        _259 = __tco__259;
                        continue tco;
                    };
                    if (_259 instanceof Three && Prelude["<"](__dict_Ord_119)(_259.value1)(_258) && Prelude["<="](__dict_Ord_119)(_258)(_259.value4)) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__258 = _258;
                        var __tco__259 = _259.value3;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _258 = __tco__258;
                        _259 = __tco__259;
                        continue tco;
                    };
                    if (_259 instanceof Three) {
                        var __tco___dict_Ord_119 = __dict_Ord_119;
                        var __tco__258 = _258;
                        var __tco__259 = _259.value6;
                        __dict_Ord_119 = __tco___dict_Ord_119;
                        _258 = __tco__258;
                        _259 = __tco__259;
                        continue tco;
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
    };
    var fromZipper = function (__copy___dict_Ord_121) {
        return function (__copy__260) {
            return function (__copy__261) {
                var __dict_Ord_121 = __copy___dict_Ord_121;
                var _260 = __copy__260;
                var _261 = __copy__261;
                tco: while (true) {
                    if (_260.length === 0) {
                        return _261;
                    };
                    if (_260.length > 0) {
                        var _461 = _260.slice(1);
                        if (_260[0] instanceof TwoLeft) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__261 = new Two(_261, (_260[0]).value0, (_260[0]).value1, (_260[0]).value2);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _260 = _461;
                            _261 = __tco__261;
                            continue tco;
                        };
                    };
                    if (_260.length > 0) {
                        var _466 = _260.slice(1);
                        if (_260[0] instanceof TwoRight) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__261 = new Two((_260[0]).value0, (_260[0]).value1, (_260[0]).value2, _261);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _260 = _466;
                            _261 = __tco__261;
                            continue tco;
                        };
                    };
                    if (_260.length > 0) {
                        var _471 = _260.slice(1);
                        if (_260[0] instanceof ThreeLeft) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__261 = new Three(_261, (_260[0]).value0, (_260[0]).value1, (_260[0]).value2, (_260[0]).value3, (_260[0]).value4, (_260[0]).value5);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _260 = _471;
                            _261 = __tco__261;
                            continue tco;
                        };
                    };
                    if (_260.length > 0) {
                        var _479 = _260.slice(1);
                        if (_260[0] instanceof ThreeMiddle) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__261 = new Three((_260[0]).value0, (_260[0]).value1, (_260[0]).value2, _261, (_260[0]).value3, (_260[0]).value4, (_260[0]).value5);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _260 = _479;
                            _261 = __tco__261;
                            continue tco;
                        };
                    };
                    if (_260.length > 0) {
                        var _487 = _260.slice(1);
                        if (_260[0] instanceof ThreeRight) {
                            var __tco___dict_Ord_121 = __dict_Ord_121;
                            var __tco__261 = new Three((_260[0]).value0, (_260[0]).value1, (_260[0]).value2, (_260[0]).value3, (_260[0]).value4, (_260[0]).value5, _261);
                            __dict_Ord_121 = __tco___dict_Ord_121;
                            _260 = _487;
                            _261 = __tco__261;
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
            return function (__copy__272) {
                return function (__copy__273) {
                    var __dict_Ord_123 = __copy___dict_Ord_123;
                    var _272 = __copy__272;
                    var _273 = __copy__273;
                    tco: while (true) {
                        if (_272.length === 0) {
                            return new Two(_273.value0, _273.value1, _273.value2, _273.value3);
                        };
                        if (_272.length > 0) {
                            var _505 = _272.slice(1);
                            if (_272[0] instanceof TwoLeft) {
                                return fromZipper(__dict_Ord_123)(_505)(new Three(_273.value0, _273.value1, _273.value2, _273.value3, (_272[0]).value0, (_272[0]).value1, (_272[0]).value2));
                            };
                        };
                        if (_272.length > 0) {
                            var _514 = _272.slice(1);
                            if (_272[0] instanceof TwoRight) {
                                return fromZipper(__dict_Ord_123)(_514)(new Three((_272[0]).value0, (_272[0]).value1, (_272[0]).value2, _273.value0, _273.value1, _273.value2, _273.value3));
                            };
                        };
                        if (_272.length > 0) {
                            var _523 = _272.slice(1);
                            if (_272[0] instanceof ThreeLeft) {
                                var __tco___dict_Ord_123 = __dict_Ord_123;
                                var __tco__273 = new KickUp(new Two(_273.value0, _273.value1, _273.value2, _273.value3), (_272[0]).value0, (_272[0]).value1, new Two((_272[0]).value2, (_272[0]).value3, (_272[0]).value4, (_272[0]).value5));
                                __dict_Ord_123 = __tco___dict_Ord_123;
                                _272 = _523;
                                _273 = __tco__273;
                                continue tco;
                            };
                        };
                        if (_272.length > 0) {
                            var _535 = _272.slice(1);
                            if (_272[0] instanceof ThreeMiddle) {
                                var __tco___dict_Ord_123 = __dict_Ord_123;
                                var __tco__273 = new KickUp(new Two((_272[0]).value0, (_272[0]).value1, (_272[0]).value2, _273.value0), _273.value1, _273.value2, new Two(_273.value3, (_272[0]).value3, (_272[0]).value4, (_272[0]).value5));
                                __dict_Ord_123 = __tco___dict_Ord_123;
                                _272 = _535;
                                _273 = __tco__273;
                                continue tco;
                            };
                        };
                        if (_272.length > 0) {
                            var _547 = _272.slice(1);
                            if (_272[0] instanceof ThreeRight) {
                                var __tco___dict_Ord_123 = __dict_Ord_123;
                                var __tco__273 = new KickUp(new Two((_272[0]).value0, (_272[0]).value1, (_272[0]).value2, (_272[0]).value3), (_272[0]).value4, (_272[0]).value5, new Two(_273.value0, _273.value1, _273.value2, _273.value3));
                                __dict_Ord_123 = __tco___dict_Ord_123;
                                _272 = _547;
                                _273 = __tco__273;
                                continue tco;
                            };
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var down = function (__copy___dict_Ord_124) {
            return function (__copy__268) {
                return function (__copy__269) {
                    return function (__copy__270) {
                        return function (__copy__271) {
                            var __dict_Ord_124 = __copy___dict_Ord_124;
                            var _268 = __copy__268;
                            var _269 = __copy__269;
                            var _270 = __copy__270;
                            var _271 = __copy__271;
                            tco: while (true) {
                                if (_271 instanceof Leaf) {
                                    return up(__dict_Ord_124)(_268)(new KickUp(Leaf.value, _269, _270, Leaf.value));
                                };
                                if (_271 instanceof Two && Prelude["=="](__dict_Ord_124["__superclass_Prelude.Eq_0"]({}))(_269)(_271.value1)) {
                                    return fromZipper(__dict_Ord_124)(_268)(new Two(_271.value0, _269, _270, _271.value3));
                                };
                                if (_271 instanceof Two && Prelude["<"](__dict_Ord_124)(_269)(_271.value1)) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__268 = Prelude[":"](new TwoLeft(_271.value1, _271.value2, _271.value3))(_268);
                                    var __tco__269 = _269;
                                    var __tco__270 = _270;
                                    var __tco__271 = _271.value0;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _268 = __tco__268;
                                    _269 = __tco__269;
                                    _270 = __tco__270;
                                    _271 = __tco__271;
                                    continue tco;
                                };
                                if (_271 instanceof Two) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__268 = Prelude[":"](new TwoRight(_271.value0, _271.value1, _271.value2))(_268);
                                    var __tco__269 = _269;
                                    var __tco__270 = _270;
                                    var __tco__271 = _271.value3;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _268 = __tco__268;
                                    _269 = __tco__269;
                                    _270 = __tco__270;
                                    _271 = __tco__271;
                                    continue tco;
                                };
                                if (_271 instanceof Three && Prelude["=="](__dict_Ord_124["__superclass_Prelude.Eq_0"]({}))(_269)(_271.value1)) {
                                    return fromZipper(__dict_Ord_124)(_268)(new Three(_271.value0, _269, _270, _271.value3, _271.value4, _271.value5, _271.value6));
                                };
                                if (_271 instanceof Three && Prelude["=="](__dict_Ord_124["__superclass_Prelude.Eq_0"]({}))(_269)(_271.value4)) {
                                    return fromZipper(__dict_Ord_124)(_268)(new Three(_271.value0, _271.value1, _271.value2, _271.value3, _269, _270, _271.value6));
                                };
                                if (_271 instanceof Three && Prelude["<"](__dict_Ord_124)(_269)(_271.value1)) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__268 = Prelude[":"](new ThreeLeft(_271.value1, _271.value2, _271.value3, _271.value4, _271.value5, _271.value6))(_268);
                                    var __tco__269 = _269;
                                    var __tco__270 = _270;
                                    var __tco__271 = _271.value0;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _268 = __tco__268;
                                    _269 = __tco__269;
                                    _270 = __tco__270;
                                    _271 = __tco__271;
                                    continue tco;
                                };
                                if (_271 instanceof Three && Prelude["<"](__dict_Ord_124)(_271.value1)(_269) && Prelude["<="](__dict_Ord_124)(_269)(_271.value4)) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__268 = Prelude[":"](new ThreeMiddle(_271.value0, _271.value1, _271.value2, _271.value4, _271.value5, _271.value6))(_268);
                                    var __tco__269 = _269;
                                    var __tco__270 = _270;
                                    var __tco__271 = _271.value3;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _268 = __tco__268;
                                    _269 = __tco__269;
                                    _270 = __tco__270;
                                    _271 = __tco__271;
                                    continue tco;
                                };
                                if (_271 instanceof Three) {
                                    var __tco___dict_Ord_124 = __dict_Ord_124;
                                    var __tco__268 = Prelude[":"](new ThreeRight(_271.value0, _271.value1, _271.value2, _271.value3, _271.value4, _271.value5))(_268);
                                    var __tco__269 = _269;
                                    var __tco__270 = _270;
                                    var __tco__271 = _271.value6;
                                    __dict_Ord_124 = __tco___dict_Ord_124;
                                    _268 = __tco__268;
                                    _269 = __tco__269;
                                    _270 = __tco__270;
                                    _271 = __tco__271;
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
            return function (__copy__277) {
                return function (__copy__278) {
                    var __dict_Ord_131 = __copy___dict_Ord_131;
                    var _277 = __copy__277;
                    var _278 = __copy__278;
                    tco: while (true) {
                        if (_277.length === 0) {
                            return _278;
                        };
                        if (_277.length > 0) {
                            var _608 = _277.slice(1);
                            if (_277[0] instanceof TwoLeft && (_277[0]).value2 instanceof Leaf && _278 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_608)(new Two(Leaf.value, (_277[0]).value0, (_277[0]).value1, Leaf.value));
                            };
                        };
                        if (_277.length > 0) {
                            var _613 = _277.slice(1);
                            if (_277[0] instanceof TwoRight && (_277[0]).value0 instanceof Leaf && _278 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_613)(new Two(Leaf.value, (_277[0]).value1, (_277[0]).value2, Leaf.value));
                            };
                        };
                        if (_277.length > 0) {
                            var _618 = _277.slice(1);
                            if (_277[0] instanceof TwoLeft && (_277[0]).value2 instanceof Two) {
                                var __tco___dict_Ord_131 = __dict_Ord_131;
                                var __tco__278 = new Three(_278, (_277[0]).value0, (_277[0]).value1, (_277[0]).value2.value0, (_277[0]).value2.value1, (_277[0]).value2.value2, (_277[0]).value2.value3);
                                __dict_Ord_131 = __tco___dict_Ord_131;
                                _277 = _618;
                                _278 = __tco__278;
                                continue tco;
                            };
                        };
                        if (_277.length > 0) {
                            var _627 = _277.slice(1);
                            if (_277[0] instanceof TwoRight && (_277[0]).value0 instanceof Two) {
                                var __tco___dict_Ord_131 = __dict_Ord_131;
                                var __tco__278 = new Three((_277[0]).value0.value0, (_277[0]).value0.value1, (_277[0]).value0.value2, (_277[0]).value0.value3, (_277[0]).value1, (_277[0]).value2, _278);
                                __dict_Ord_131 = __tco___dict_Ord_131;
                                _277 = _627;
                                _278 = __tco__278;
                                continue tco;
                            };
                        };
                        if (_277.length > 0) {
                            var _636 = _277.slice(1);
                            if (_277[0] instanceof TwoLeft && (_277[0]).value2 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_636)(new Two(new Two(_278, (_277[0]).value0, (_277[0]).value1, (_277[0]).value2.value0), (_277[0]).value2.value1, (_277[0]).value2.value2, new Two((_277[0]).value2.value3, (_277[0]).value2.value4, (_277[0]).value2.value5, (_277[0]).value2.value6)));
                            };
                        };
                        if (_277.length > 0) {
                            var _648 = _277.slice(1);
                            if (_277[0] instanceof TwoRight && (_277[0]).value0 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_648)(new Two(new Two((_277[0]).value0.value0, (_277[0]).value0.value1, (_277[0]).value0.value2, (_277[0]).value0.value3), (_277[0]).value0.value4, (_277[0]).value0.value5, new Two((_277[0]).value0.value6, (_277[0]).value1, (_277[0]).value2, _278)));
                            };
                        };
                        if (_277.length > 0) {
                            var _660 = _277.slice(1);
                            if (_277[0] instanceof ThreeLeft && (_277[0]).value2 instanceof Leaf && (_277[0]).value5 instanceof Leaf && _278 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_660)(new Three(Leaf.value, (_277[0]).value0, (_277[0]).value1, Leaf.value, (_277[0]).value3, (_277[0]).value4, Leaf.value));
                            };
                        };
                        if (_277.length > 0) {
                            var _668 = _277.slice(1);
                            if (_277[0] instanceof ThreeMiddle && (_277[0]).value0 instanceof Leaf && (_277[0]).value5 instanceof Leaf && _278 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_668)(new Three(Leaf.value, (_277[0]).value1, (_277[0]).value2, Leaf.value, (_277[0]).value3, (_277[0]).value4, Leaf.value));
                            };
                        };
                        if (_277.length > 0) {
                            var _676 = _277.slice(1);
                            if (_277[0] instanceof ThreeRight && (_277[0]).value0 instanceof Leaf && (_277[0]).value3 instanceof Leaf && _278 instanceof Leaf) {
                                return fromZipper(__dict_Ord_131)(_676)(new Three(Leaf.value, (_277[0]).value1, (_277[0]).value2, Leaf.value, (_277[0]).value4, (_277[0]).value5, Leaf.value));
                            };
                        };
                        if (_277.length > 0) {
                            var _684 = _277.slice(1);
                            if (_277[0] instanceof ThreeLeft && (_277[0]).value2 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_684)(new Two(new Three(_278, (_277[0]).value0, (_277[0]).value1, (_277[0]).value2.value0, (_277[0]).value2.value1, (_277[0]).value2.value2, (_277[0]).value2.value3), (_277[0]).value3, (_277[0]).value4, (_277[0]).value5));
                            };
                        };
                        if (_277.length > 0) {
                            var _696 = _277.slice(1);
                            if (_277[0] instanceof ThreeMiddle && (_277[0]).value0 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_696)(new Two(new Three((_277[0]).value0.value0, (_277[0]).value0.value1, (_277[0]).value0.value2, (_277[0]).value0.value3, (_277[0]).value1, (_277[0]).value2, _278), (_277[0]).value3, (_277[0]).value4, (_277[0]).value5));
                            };
                        };
                        if (_277.length > 0) {
                            var _708 = _277.slice(1);
                            if (_277[0] instanceof ThreeMiddle && (_277[0]).value5 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_708)(new Two((_277[0]).value0, (_277[0]).value1, (_277[0]).value2, new Three(_278, (_277[0]).value3, (_277[0]).value4, (_277[0]).value5.value0, (_277[0]).value5.value1, (_277[0]).value5.value2, (_277[0]).value5.value3)));
                            };
                        };
                        if (_277.length > 0) {
                            var _720 = _277.slice(1);
                            if (_277[0] instanceof ThreeRight && (_277[0]).value3 instanceof Two) {
                                return fromZipper(__dict_Ord_131)(_720)(new Two((_277[0]).value0, (_277[0]).value1, (_277[0]).value2, new Three((_277[0]).value3.value0, (_277[0]).value3.value1, (_277[0]).value3.value2, (_277[0]).value3.value3, (_277[0]).value4, (_277[0]).value5, _278)));
                            };
                        };
                        if (_277.length > 0) {
                            var _732 = _277.slice(1);
                            if (_277[0] instanceof ThreeLeft && (_277[0]).value2 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_732)(new Three(new Two(_278, (_277[0]).value0, (_277[0]).value1, (_277[0]).value2.value0), (_277[0]).value2.value1, (_277[0]).value2.value2, new Two((_277[0]).value2.value3, (_277[0]).value2.value4, (_277[0]).value2.value5, (_277[0]).value2.value6), (_277[0]).value3, (_277[0]).value4, (_277[0]).value5));
                            };
                        };
                        if (_277.length > 0) {
                            var _747 = _277.slice(1);
                            if (_277[0] instanceof ThreeMiddle && (_277[0]).value0 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_747)(new Three(new Two((_277[0]).value0.value0, (_277[0]).value0.value1, (_277[0]).value0.value2, (_277[0]).value0.value3), (_277[0]).value0.value4, (_277[0]).value0.value5, new Two((_277[0]).value0.value6, (_277[0]).value1, (_277[0]).value2, _278), (_277[0]).value3, (_277[0]).value4, (_277[0]).value5));
                            };
                        };
                        if (_277.length > 0) {
                            var _762 = _277.slice(1);
                            if (_277[0] instanceof ThreeMiddle && (_277[0]).value5 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_762)(new Three((_277[0]).value0, (_277[0]).value1, (_277[0]).value2, new Two(_278, (_277[0]).value3, (_277[0]).value4, (_277[0]).value5.value0), (_277[0]).value5.value1, (_277[0]).value5.value2, new Two((_277[0]).value5.value3, (_277[0]).value5.value4, (_277[0]).value5.value5, (_277[0]).value5.value6)));
                            };
                        };
                        if (_277.length > 0) {
                            var _777 = _277.slice(1);
                            if (_277[0] instanceof ThreeRight && (_277[0]).value3 instanceof Three) {
                                return fromZipper(__dict_Ord_131)(_777)(new Three((_277[0]).value0, (_277[0]).value1, (_277[0]).value2, new Two((_277[0]).value3.value0, (_277[0]).value3.value1, (_277[0]).value3.value2, (_277[0]).value3.value3), (_277[0]).value3.value4, (_277[0]).value3.value5, new Two((_277[0]).value3.value6, (_277[0]).value4, (_277[0]).value5, _278)));
                            };
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var removeMaxNode = function (__copy___dict_Ord_132) {
            return function (__copy__280) {
                return function (__copy__281) {
                    var __dict_Ord_132 = __copy___dict_Ord_132;
                    var _280 = __copy__280;
                    var _281 = __copy__281;
                    tco: while (true) {
                        if (_281 instanceof Two && _281.value0 instanceof Leaf && _281.value3 instanceof Leaf) {
                            return up(__dict_Ord_132)(_280)(Leaf.value);
                        };
                        if (_281 instanceof Two) {
                            var __tco___dict_Ord_132 = __dict_Ord_132;
                            var __tco__280 = Prelude[":"](new TwoRight(_281.value0, _281.value1, _281.value2))(_280);
                            var __tco__281 = _281.value3;
                            __dict_Ord_132 = __tco___dict_Ord_132;
                            _280 = __tco__280;
                            _281 = __tco__281;
                            continue tco;
                        };
                        if (_281 instanceof Three && _281.value0 instanceof Leaf && _281.value3 instanceof Leaf && _281.value6 instanceof Leaf) {
                            return up(__dict_Ord_132)(Prelude[":"](new TwoRight(Leaf.value, _281.value1, _281.value2))(_280))(Leaf.value);
                        };
                        if (_281 instanceof Three) {
                            var __tco___dict_Ord_132 = __dict_Ord_132;
                            var __tco__280 = Prelude[":"](new ThreeRight(_281.value0, _281.value1, _281.value2, _281.value3, _281.value4, _281.value5))(_280);
                            var __tco__281 = _281.value6;
                            __dict_Ord_132 = __tco___dict_Ord_132;
                            _280 = __tco__280;
                            _281 = __tco__281;
                            continue tco;
                        };
                        throw new Error("Failed pattern match");
                    };
                };
            };
        };
        var maxNode = function (__copy___dict_Ord_133) {
            return function (__copy__279) {
                var __dict_Ord_133 = __copy___dict_Ord_133;
                var _279 = __copy__279;
                tco: while (true) {
                    if (_279 instanceof Two && _279.value3 instanceof Leaf) {
                        return {
                            key: _279.value1, 
                            value: _279.value2
                        };
                    };
                    if (_279 instanceof Two) {
                        var __tco___dict_Ord_133 = __dict_Ord_133;
                        var __tco__279 = _279.value3;
                        __dict_Ord_133 = __tco___dict_Ord_133;
                        _279 = __tco__279;
                        continue tco;
                    };
                    if (_279 instanceof Three && _279.value6 instanceof Leaf) {
                        return {
                            key: _279.value4, 
                            value: _279.value5
                        };
                    };
                    if (_279 instanceof Three) {
                        var __tco___dict_Ord_133 = __dict_Ord_133;
                        var __tco__279 = _279.value6;
                        __dict_Ord_133 = __tco___dict_Ord_133;
                        _279 = __tco__279;
                        continue tco;
                    };
                    throw new Error("Failed pattern match");
                };
            };
        };
        var down = function (__copy___dict_Ord_134) {
            return function (__copy__274) {
                return function (__copy__275) {
                    return function (__copy__276) {
                        var __dict_Ord_134 = __copy___dict_Ord_134;
                        var _274 = __copy__274;
                        var _275 = __copy__275;
                        var _276 = __copy__276;
                        tco: while (true) {
                            if (_276 instanceof Leaf) {
                                return fromZipper(__dict_Ord_134)(_274)(Leaf.value);
                            };
                            if (_276 instanceof Two && _276.value0 instanceof Leaf && _276.value3 instanceof Leaf && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_275)(_276.value1)) {
                                return up(__dict_Ord_134)(_274)(Leaf.value);
                            };
                            if (_276 instanceof Two && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_275)(_276.value1)) {
                                var max = maxNode(__dict_Ord_134)(_276.value0);
                                return removeMaxNode(__dict_Ord_134)(Prelude[":"](new TwoLeft(max.key, max.value, _276.value3))(_274))(_276.value0);
                            };
                            if (_276 instanceof Two && Prelude["<"](__dict_Ord_134)(_275)(_276.value1)) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__274 = Prelude[":"](new TwoLeft(_276.value1, _276.value2, _276.value3))(_274);
                                var __tco__275 = _275;
                                var __tco__276 = _276.value0;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _274 = __tco__274;
                                _275 = __tco__275;
                                _276 = __tco__276;
                                continue tco;
                            };
                            if (_276 instanceof Two) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__274 = Prelude[":"](new TwoRight(_276.value0, _276.value1, _276.value2))(_274);
                                var __tco__275 = _275;
                                var __tco__276 = _276.value3;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _274 = __tco__274;
                                _275 = __tco__275;
                                _276 = __tco__276;
                                continue tco;
                            };
                            if (_276 instanceof Three && _276.value0 instanceof Leaf && _276.value3 instanceof Leaf && _276.value6 instanceof Leaf && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_275)(_276.value1)) {
                                return fromZipper(__dict_Ord_134)(_274)(new Two(Leaf.value, _276.value4, _276.value5, Leaf.value));
                            };
                            if (_276 instanceof Three && _276.value0 instanceof Leaf && _276.value3 instanceof Leaf && _276.value6 instanceof Leaf && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_275)(_276.value4)) {
                                return fromZipper(__dict_Ord_134)(_274)(new Two(Leaf.value, _276.value1, _276.value2, Leaf.value));
                            };
                            if (_276 instanceof Three && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_275)(_276.value1)) {
                                var max = maxNode(__dict_Ord_134)(_276.value0);
                                return removeMaxNode(__dict_Ord_134)(Prelude[":"](new ThreeLeft(max.key, max.value, _276.value3, _276.value4, _276.value5, _276.value6))(_274))(_276.value0);
                            };
                            if (_276 instanceof Three && Prelude["=="](__dict_Ord_134["__superclass_Prelude.Eq_0"]({}))(_275)(_276.value4)) {
                                var max = maxNode(__dict_Ord_134)(_276.value3);
                                return removeMaxNode(__dict_Ord_134)(Prelude[":"](new ThreeMiddle(_276.value0, _276.value1, _276.value2, max.key, max.value, _276.value6))(_274))(_276.value3);
                            };
                            if (_276 instanceof Three && Prelude["<"](__dict_Ord_134)(_275)(_276.value1)) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__274 = Prelude[":"](new ThreeLeft(_276.value1, _276.value2, _276.value3, _276.value4, _276.value5, _276.value6))(_274);
                                var __tco__275 = _275;
                                var __tco__276 = _276.value0;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _274 = __tco__274;
                                _275 = __tco__275;
                                _276 = __tco__276;
                                continue tco;
                            };
                            if (_276 instanceof Three && Prelude["<"](__dict_Ord_134)(_276.value1)(_275) && Prelude["<"](__dict_Ord_134)(_275)(_276.value4)) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__274 = Prelude[":"](new ThreeMiddle(_276.value0, _276.value1, _276.value2, _276.value4, _276.value5, _276.value6))(_274);
                                var __tco__275 = _275;
                                var __tco__276 = _276.value3;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _274 = __tco__274;
                                _275 = __tco__275;
                                _276 = __tco__276;
                                continue tco;
                            };
                            if (_276 instanceof Three) {
                                var __tco___dict_Ord_134 = __dict_Ord_134;
                                var __tco__274 = Prelude[":"](new ThreeRight(_276.value0, _276.value1, _276.value2, _276.value3, _276.value4, _276.value5))(_274);
                                var __tco__275 = _275;
                                var __tco__276 = _276.value6;
                                __dict_Ord_134 = __tco___dict_Ord_134;
                                _274 = __tco__274;
                                _275 = __tco__275;
                                _276 = __tco__276;
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
                    var _906 = f(lookup(__dict_Ord_135)(k)(m));
                    if (_906 instanceof Data_Maybe.Nothing) {
                        return $$delete(__dict_Ord_135)(k)(m);
                    };
                    if (_906 instanceof Data_Maybe.Just) {
                        return insert(__dict_Ord_135)(k)(_906.value0)(m);
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
            return function (_309) {
                if (_309.length === 0) {
                    return Prelude.pure(__dict_Applicative_157)([  ]);
                };
                if (_309.length > 0) {
                    var _910 = _309.slice(1);
                    return Prelude["<*>"](__dict_Applicative_157["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_157["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_309[0]))(sequence(traversableArray({}))(__dict_Applicative_157)(_910));
                };
                throw new Error("Failed pattern match");
            };
        }, function (__dict_Applicative_156) {
            return function (_307) {
                return function (_308) {
                    if (_308.length === 0) {
                        return Prelude.pure(__dict_Applicative_156)([  ]);
                    };
                    if (_308.length > 0) {
                        var _914 = _308.slice(1);
                        return Prelude["<*>"](__dict_Applicative_156["__superclass_Prelude.Apply_0"]({}))(Prelude["<$>"]((__dict_Applicative_156["__superclass_Prelude.Apply_0"]({}))["__superclass_Prelude.Functor_0"]({}))(Prelude[":"])(_307(_308[0])))(traverse(traversableArray({}))(__dict_Applicative_156)(_307)(_914));
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
        var go = function (_341) {
            return function (_342) {
                return Prelude["<>"](Data_Array.semigroupArray({}))(Data_Maybe.maybe([  ])(function (a$prime) {
                    return [ new Data_Tuple.Tuple(_341, a$prime) ];
                })(_342.value0))(Data_Array.concatMap(function (_339) {
                    return go(_341 + _339.value0)(_339.value1);
                })(Data_Map.toList(_342.value1)));
            };
        };
        return go("");
    })();
    var lookupAll = function (s) {
        var go = function (_345) {
            return function (_346) {
                if (_345 >= Data_String.length(s)) {
                    return new Data_Maybe.Just(_346);
                };
                return Prelude[">>="](Data_Maybe.bindMaybe({}))(Data_Map.lookup(Prelude.ordString({}))(Data_String.charAt(_345)(s))(_346.value1))(function (_21) {
                    return go(_345 + 1)(_21);
                });
            };
        };
        return go(0);
    };
    var empty = new Trie(Data_Maybe.Nothing.value, Data_Map.empty);
    var insert = function (s) {
        return function (a) {
            var go = function (_343) {
                return function (_344) {
                    if (_343 >= Data_String.length(s)) {
                        return new Trie(new Data_Maybe.Just(a), _344.value1);
                    };
                    return new Trie(_344.value0, Data_Map.alter(Prelude.ordString({}))(Prelude["<<<"](Prelude.semigroupoidArr({}))(Data_Maybe.Just.create)(Prelude["<<<"](Prelude.semigroupoidArr({}))(go(_343 + 1))(Data_Maybe.fromMaybe(empty))))(Data_String.charAt(_343)(s))(_344.value1));
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
    var runSearch = function (_349) {
        return function (_350) {
            if (_350 === "") {
                return Data_Maybe.Nothing.value;
            };
            return Prelude["<$>"](Data_Maybe.functorMaybe({}))(Data_Trie.toArray)(Data_Trie.lookupAll(Data_String.toLower(_350))(_349));
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
                var _939 = Data_Foreign.readString(_23);
                if (_939 instanceof Data_Either.Right) {
                    return _939.value0;
                };
                if (_939 instanceof Data_Either.Left) {
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
                        var _946 = runSearch(trie)(_27);
                        if (_946 instanceof Data_Maybe.Nothing) {
                            return Prelude["return"](Control_Monad_Eff.monadEff({}))(Prelude.unit);
                        };
                        if (_946 instanceof Data_Maybe.Just) {
                            return Control_Monad_Eff.foreachE(Data_Array.take(20)(_946.value0))(function (_347) {
                                return function __do() {
                                    var _25 = Control_Monad_Eff_DOM.createElement("div")();
                                    Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("h2"))(Control_Monad_Eff_DOM.setText(_347.value1.value1)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("div"))(Control_Monad_Eff_DOM.setText(_347.value1.value0)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
                                    Prelude[">>="](Control_Monad_Eff.bindEff({}))(Prelude[">>="](Control_Monad_Eff.bindEff({}))(Control_Monad_Eff_DOM.createElement("pre"))(Control_Monad_Eff_DOM.setText(_347.value1.value2)))(Prelude.flip(Control_Monad_Eff_DOM.appendChild)(_25))();
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
        var _956 = Prelude[">>="](Data_Either.bindEither({}))(Prelude[">>="](Data_Either.bindEither({}))(Data_Foreign.parseJSON(json))(Data_Foreign.readArray))(Data_Traversable.traverse(Data_Traversable.traversableArray({}))(Data_Either.applicativeEither({}))(Data_Foreign_Class.read(isForeignEntry({}))));
        if (_956 instanceof Data_Either.Left) {
            return error(Prelude.show(Data_Foreign.showForeignError({}))(_956.value0));
        };
        if (_956 instanceof Data_Either.Right) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray({}))(function (t) {
                return function (_348) {
                    return Data_Trie.insert(Data_String.toLower(_348.value1))(_348)(t);
                };
            })(Data_Trie.empty)(_956.value0);
        };
        throw new Error("Failed pattern match");
    };
    var baseUrl = function __do() {
        var _30 = Control_Monad_Eff_DOM.locationProtocol();
        var _29 = Control_Monad_Eff_DOM.locationHost();
        var _28 = Control_Monad_Eff_DOM.locationPathname();
        return _30 + "//" + _29 + _28;
    };
    var updateHistorySearch = function __do() {
        var _34 = Control_Monad_Eff_DOM.historyState();
        var _33 = Control_Monad_Eff_DOM.documentTitle();
        var _32 = baseUrl();
        var _31 = getQuery();
        return Control_Monad_Eff_DOM.replaceHistoryState(_34)(_33)(_32 + "?" + _31)();
    };
    var main = Control_Monad_Eff_AJAX.get("data.json")(function (json) {
        return function __do() {
            var _36 = Control_Monad_Eff_DOM.querySelector("#searchInput")();
            return (function () {
                if (_36 instanceof Data_Maybe.Nothing) {
                    return error("#searchInput not found");
                };
                if (_36 instanceof Data_Maybe.Just) {
                    var trie = buildTrie(json);
                    return function __do() {
                        Data_Traversable["for"](Control_Monad_Eff.applicativeEff({}))(Data_Traversable.traversableArray({}))([ "keyup", "change" ])(function (evt) {
                            return function __do() {
                                Control_Monad_Eff_DOM.addEventListener(evt)(search(trie))(_36.value0)();
                                return Control_Monad_Eff_DOM.addEventListener(evt)(updateHistorySearch)(_36.value0)();
                            };
                        })();
                        var _35 = Prelude["<$>"](Control_Monad_Eff.functorEff({}))(Data_String.drop(1))(Control_Monad_Eff_DOM.locationSearch)();
                        Control_Monad_Eff_DOM.setValue(_35)(_36.value0)();
                        return search(trie)();
                    };
                };
                throw new Error("Failed pattern match");
            })()();
        };
    });
    return {
        Entry: Entry, 
        main: main, 
        updateHistorySearch: updateHistorySearch, 
        baseUrl: baseUrl, 
        buildTrie: buildTrie, 
        error: error, 
        search: search, 
        runSearch: runSearch, 
        getQuery: getQuery, 
        isForeignEntry: isForeignEntry
    };
})();
PS.Main.main();