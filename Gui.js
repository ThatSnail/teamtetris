// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.target.offsetLeft || 0),
	    posy - (e.target.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}

function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=unCStr("Prelude.(!!): index too large\n"),_1=new T(function(){return err(_0);}),_2=function(_3,_4){while(1){var _5=E(_3);if(!_5[0]){return E(_1);}else{var _6=E(_4);if(!_6){return E(_5[1]);}else{_3=_5[2];_4=_6-1|0;continue;}}}},_7=unCStr("Prelude.(!!): negative index\n"),_8=new T(function(){return err(_7);}),_9=function(_a,_b){return [0,E(_a)[1]+E(_b)[1]|0];},_c=function(_d,_e){var _f=E(_e);return _f[0]==0?[0]:[1,new T(function(){return A(_d,[_f[1]]);}),new T(function(){return _c(_d,_f[2]);})];},_g=[0,0],_h=[0,-1],_i=[0,_h,_g],_j=[0],_k=[0,1],_l=[0,_g,_k],_m=[1,_l,_j],_n=[0,_g,_g],_o=[1,_n,_m],_p=[0,_g,_h],_q=[1,_p,_o],_r=[1,_i,_q],_s=[0,_k,_h],_t=[1,_s,_j],_u=[0,_g,_h],_v=[1,_u,_t],_w=[0,_g,_g],_x=[1,_w,_v],_y=[0,_h,_g],_z=[1,_y,_x],_A=[0,_k,_g],_B=[1,_A,_j],_C=[0,_g,_k],_D=[1,_C,_B],_E=[0,_g,_g],_F=[1,_E,_D],_G=[0,_h,_g],_H=[1,_G,_F],_I=[0,_k,_h],_J=[1,_I,_j],_K=[0,_k,_g],_L=[1,_K,_J],_M=[0,_g,_g],_N=[1,_M,_L],_O=[0,_g,_k],_P=[1,_O,_N],_Q=[0,_k,_g],_R=[1,_Q,_j],_S=[0,_g,_g],_T=[1,_S,_R],_U=[0,_g,_h],_V=[1,_U,_T],_W=[0,_h,_h],_X=[1,_W,_V],_Y=[0,_g,_k],_Z=[1,_Y,_j],_10=[0,_g,_g],_11=[1,_10,_Z],_12=[0,_g,_h],_13=[1,_12,_11],_14=[0,-2],_15=[0,_g,_14],_16=[1,_15,_13],_17=[0,_k,_g],_18=[1,_17,_j],_19=[0,_g,_g],_1a=[1,_19,_18],_1b=[0,_h,_g],_1c=[1,_1b,_1a],_1d=[0,_14,_g],_1e=[1,_1d,_1c],_1f=[0,_h,_g],_1g=[1,_1f,_j],_1h=[0,_h,_h],_1i=[1,_1h,_1g],_1j=[0,_g,_h],_1k=[1,_1j,_1i],_1l=[0,_g,_g],_1m=[1,_1l,_1k],_1n=[0,_k,_g],_1o=[1,_1n,_j],_1p=[0,_g,_k],_1q=[1,_1p,_1o],_1r=[0,_g,_g],_1s=[1,_1r,_1q],_1t=[0,_g,_h],_1u=[1,_1t,_1s],_1v=[0,_k,_g],_1w=[1,_1v,_j],_1x=[0,_g,_h],_1y=[1,_1x,_1w],_1z=[0,_g,_g],_1A=[1,_1z,_1y],_1B=[0,_h,_g],_1C=[1,_1B,_1A],_1D=[0,_g,_k],_1E=[1,_1D,_j],_1F=[0,_g,_g],_1G=[1,_1F,_1E],_1H=[0,_g,_h],_1I=[1,_1H,_1G],_1J=[0,_h,_h],_1K=[1,_1J,_1I],_1L=[0,_k,_g],_1M=[1,_1L,_j],_1N=[0,_g,_g],_1O=[1,_1N,_1M],_1P=[0,_h,_g],_1Q=[1,_1P,_1O],_1R=[0,_h,_k],_1S=[1,_1R,_1Q],_1T=[0,_k,_k],_1U=[1,_1T,_j],_1V=[0,_g,_k],_1W=[1,_1V,_1U],_1X=[0,_g,_g],_1Y=[1,_1X,_1W],_1Z=[0,_g,_h],_20=[1,_1Z,_1Y],_21=[0,_k,_h],_22=[1,_21,_j],_23=[0,_k,_g],_24=[1,_23,_22],_25=[0,_g,_g],_26=[1,_25,_24],_27=[0,_h,_g],_28=[1,_27,_26],_29=[0,_g,_h],_2a=[1,_29,_j],_2b=[0,_g,_g],_2c=[1,_2b,_2a],_2d=[0,_g,_k],_2e=[1,_2d,_2c],_2f=[0,_h,_k],_2g=[1,_2f,_2e],_2h=[0,_k,_k],_2i=[1,_2h,_j],_2j=[0,_k,_g],_2k=[1,_2j,_2i],_2l=[0,_g,_g],_2m=[1,_2l,_2k],_2n=[0,_h,_g],_2o=[1,_2n,_2m],_2p=[0,_g,_k],_2q=[1,_2p,_j],_2r=[0,_g,_g],_2s=[1,_2r,_2q],_2t=[0,_g,_h],_2u=[1,_2t,_2s],_2v=[0,_k,_h],_2w=[1,_2v,_2u],_2x=[0,_k,_g],_2y=[1,_2x,_j],_2z=[0,_g,_g],_2A=[1,_2z,_2y],_2B=[0,_h,_g],_2C=[1,_2B,_2A],_2D=[0,_h,_h],_2E=[1,_2D,_2C],_2F=[0,_k,_k],_2G=[1,_2F,_j],_2H=[0,_k,_g],_2I=[1,_2H,_2G],_2J=[0,_g,_g],_2K=[1,_2J,_2I],_2L=[0,_g,_h],_2M=[1,_2L,_2K],_2N=function(_2O,_2P){switch(E(_2O)){case 0:return E(_1m);case 1:switch(E(_2P)){case 1:return E(_16);case 3:return E(_16);default:return E(_1e);}break;case 2:switch(E(_2P)){case 1:return E(_P);case 3:return E(_P);default:return E(_X);}break;case 3:switch(E(_2P)){case 1:return E(_2M);case 3:return E(_2M);default:return E(_z);}break;case 4:switch(E(_2P)){case 0:return E(_2E);case 1:return E(_2w);case 2:return E(_2o);default:return E(_2g);}break;case 5:switch(E(_2P)){case 0:return E(_28);case 1:return E(_20);case 2:return E(_1S);default:return E(_1K);}break;default:switch(E(_2P)){case 0:return E(_1C);case 1:return E(_1u);case 2:return E(_H);default:return E(_r);}}},_2Q=function(_2R,_2S,_2T){return _c(function(_2U){var _2V=E(_2R),_2W=E(_2U);return [0,new T(function(){return _9(_2V[1],_2W[1]);}),new T(function(){return _9(_2V[2],_2W[2]);})];},_2N(_2S,_2T));},_2X=function(_2Y){var _2Z=E(_2Y);return _2Q(_2Z[1],_2Z[2],_2Z[3]);},_30=function(_31){return E(E(_31)[1]);},_32=function(_33,_34,_35){while(1){var _36=E(_35);if(!_36[0]){return false;}else{if(!A(_30,[_33,_34,_36[1]])){_35=_36[2];continue;}else{return true;}}}},_37=function(_38,_39){while(1){var _3a=(function(_3b,_3c){var _3d=E(_3c);if(!_3d[0]){return [0];}else{var _3e=_3d[1],_3f=_3d[2];if(!A(_3b,[_3e])){var _3g=_3b;_39=_3f;_38=_3g;return null;}else{return [1,_3e,new T(function(){return _37(_3b,_3f);})];}}})(_38,_39);if(_3a!=null){return _3a;}}},_3h=function(_3i,_3j,_3k,_3l,_3m,_3n){return !A(_3i,[_3k,_3m])?true:!A(_30,[_3j,_3l,_3n])?true:false;},_3o=function(_3p,_3q,_3r,_3s){var _3t=E(_3r),_3u=E(_3s);return _3h(E(_3p)[1],_3q,_3t[1],_3t[2],_3u[1],_3u[2]);},_3v=function(_3w,_3x,_3y,_3z,_3A,_3B){return !A(_3w,[_3y,_3A])?false:A(_30,[_3x,_3z,_3B]);},_3C=function(_3D,_3E,_3F,_3G){var _3H=E(_3F),_3I=E(_3G);return _3v(E(_3D)[1],_3E,_3H[1],_3H[2],_3I[1],_3I[2]);},_3J=function(_3K,_3L){return [0,function(_3M,_3N){return _3C(_3K,_3L,_3M,_3N);},function(_3M,_3N){return _3o(_3K,_3L,_3M,_3N);}];},_3O=function(_3P,_3Q){return E(_3P)[1]==E(_3Q)[1];},_3R=function(_3S,_3T){return E(_3S)[1]!=E(_3T)[1];},_3U=[0,_3O,_3R],_3V=new T(function(){return _3J(_3U,_3U);}),_3W=function(_3X,_3Y,_3Z){var _40=_37(function(_41){return _32(_3V,[0,_3Y,_3Z],_2X(_41));},_3X);return _40[0]==0?[0]:[1,_40[1]];},_42=0,_43=function(_44,_){return _42;},_45=new T(function(){return [0,"strokeStyle"];}),_46=new T(function(){return [0,"fillStyle"];}),_47=[0,44],_48=[1,_47,_j],_49=new T(function(){return [0,toJSStr(_48)];}),_4a=[1,_47,_j],_4b=new T(function(){return [0,toJSStr(_4a)];}),_4c=new T(function(){return [0,"rgba("];}),_4d=new T(function(){return [0,toJSStr(_j)];}),_4e=[0,41],_4f=[1,_4e,_j],_4g=new T(function(){return [0,toJSStr(_4f)];}),_4h=[1,_4g,_j],_4i=[1,_47,_j],_4j=new T(function(){return [0,toJSStr(_4i)];}),_4k=[1,_47,_j],_4l=new T(function(){return [0,toJSStr(_4k)];}),_4m=new T(function(){return [0,"rgb("];}),_4n=[1,_4e,_j],_4o=new T(function(){return [0,toJSStr(_4n)];}),_4p=[1,_4o,_j],_4q=[1,_47,_j],_4r=new T(function(){return [0,toJSStr(_4q)];}),_4s=function(_4t){var _4u=String(E(_4t)[1]);return [0,_4u];},_4v=function(_4w){var _4x=E(_4w);if(!_4x[0]){var _4y=jsCat([1,_4m,[1,new T(function(){return _4s(_4x[1]);}),[1,_4l,[1,new T(function(){return _4s(_4x[2]);}),[1,_4j,[1,new T(function(){return _4s(_4x[3]);}),_4h]]]]]],E(_4d)[1]);return [0,_4y];}else{var _4z=jsCat([1,_4c,[1,new T(function(){return _4s(_4x[1]);}),[1,_4b,[1,new T(function(){return _4s(_4x[2]);}),[1,_49,[1,new T(function(){return _4s(_4x[3]);}),[1,_4r,[1,new T(function(){return _4s(_4x[4]);}),_4p]]]]]]]],E(_4d)[1]);return [0,_4z];}},_4A=function(_4B,_4C){var _4D=new T(function(){return _4v(_4B);});return function(_4E,_){var _4F=E(_4E),_4G=_4F[1],_4H=E(_46)[1],_4I=jsGet(_4G,_4H),_4J=E(_45)[1],_4K=jsGet(_4G,_4J),_4L=E(_4D)[1],_4M=jsSet(_4G,_4H,_4L),_4N=jsSet(_4G,_4J,_4L),_4O=A(_4C,[_4F,_]),_4P=jsSet(_4G,_4H,_4I),_4Q=jsSet(_4G,_4J,_4K);return _42;};},_4R=[0,255],_4S=[0,_4R,_g,_4R],_4T=[0,_4R,_4R,_g],_4U=[0,128],_4V=[0,_4R,_4U,_g],_4W=[0,_g,_g,_4R],_4X=[0,_4R,_g,_g],_4Y=[0,_g,_4R,_g],_4Z=[0,_g,_4R,_4R],_50=function(_51,_52,_){var _53=E(_52),_54=_53[1],_55=jsBeginPath(_54),_56=A(_51,[_53,_]),_57=jsFill(_54);return _42;},_58=[0,128],_59=[0,_58,_58,_58],_5a=[0,0],_5b=[0,_5a,_5a,_5a],_5c=function(_5d,_5e){return [0,_5d,_5e];},_5f=function(_5g,_5h){var _5i=E(_5g);return _5i[0]==0?E(_5h):[1,_5i[1],new T(function(){return _5f(_5i[2],_5h);})];},_5j=function(_5k,_5l,_5m){var _5n=function(_5o){while(1){var _5p=(function(_5q){var _5r=E(_5q);if(!_5r[0]){return [0];}else{var _5s=_5r[2],_5t=function(_5u){var _5v=E(_5u);return _5v[0]==0?[0]:[1,new T(function(){return A(_5r[1],[_5v[1]]);}),new T(function(){return _5f(_j,new T(function(){return _5t(_5v[2]);}));})];},_5w=_5t(_5m);if(!_5w[0]){_5o=_5s;return null;}else{return [1,_5w[1],new T(function(){return _5f(_5w[2],new T(function(){return _5n(_5s);}));})];}}})(_5o);if(_5p!=null){return _5p;}}};return _5n(_c(_5k,_5l));},_5x=function(_5y){return [1,[0,_5y],new T(function(){var _5z=E(_5y);return _5z==13?[0]:_5x(_5z+1|0);})];},_5A=new T(function(){return _5x(0);}),_5B=function(_5C){return [1,[0,_5C],new T(function(){var _5D=E(_5C);return _5D==21?[0]:_5B(_5D+1|0);})];},_5E=new T(function(){return _5B(0);}),_5F=new T(function(){return _5j(_5c,_5A,_5E);}),_5G=function(_5H,_){return _42;},_5I=function(_5J){var _5K=E(_5J);if(!_5K[0]){return E(_5G);}else{var _5L=E(_5K[1]);return function(_5M,_){var _5N=E(_5M)[1],_5O=jsMoveTo(_5N,E(_5L[1])[1],E(_5L[2])[1]);return (function(_5P,_){while(1){var _5Q=E(_5P);if(!_5Q[0]){return _42;}else{var _5R=E(_5Q[1]),_5S=jsLineTo(_5N,E(_5R[1])[1],E(_5R[2])[1]);_5P=_5Q[2];continue;}}})(_5K[2],_);};}},_5T=function(_5U,_5V,_5W){var _5X=new T(function(){return E(E(_5U)[1]);}),_5Y=function(_5Z){var _60=E(_5Z);if(!_60[0]){return E(_43);}else{var _61=new T(function(){return _5Y(_60[2]);});return function(_62,_){var _63=E(_62),_64=_63[1],_65=jsPushState(_64),_66=jsTranslate(_64,E(_5V)[1],E(_5W)[1]),_67=A(new T(function(){var _68=E(_60[1]),_69=_68[1],_6a=_68[2];return _4A(new T(function(){var _6b=E(_69),_6c=_6b[1],_6d=E(_6a),_6e=_6d[1];if(_6e>=0){if(_6c>=0){if(!_2(_2(_5X,_6c),_6e)){var _6f=_3W(E(_5U)[2],_6b,_6d);if(!_6f[0]){return E(_59);}else{switch(E(E(_6f[1])[2])){case 0:return E(_4T);case 1:return E(_4Z);case 2:return E(_4Y);case 3:return E(_4X);case 4:return E(_4W);case 5:return E(_4V);default:return E(_4S);}}}else{return E(_5b);}}else{return E(_8);}}else{return E(_8);}}),function(_6g,_){return _50(new T(function(){var _6h=new T(function(){return [0,((22-E(_6a)[1]|0)-1|0)*20];}),_6i=new T(function(){return [0,E(_6h)[1]+20-2];}),_6j=new T(function(){return [0,E(_69)[1]*20];}),_6k=new T(function(){return [0,E(_6j)[1]+20-2];}),_6l=new T(function(){return [0,E(_6h)[1]+2];}),_6m=new T(function(){return [0,E(_6j)[1]+2];});return _5I([1,[0,_6m,_6l],[1,[0,_6k,_6l],[1,[0,_6k,_6i],[1,[0,_6m,_6i],[1,[0,_6m,_6l],_j]]]]]);}),_6g,_);});}),[_63,_]),_6n=jsPopState(_64);return A(_61,[_63,_]);};}};return _5Y(_5F);},_6o=[0],_6p=function(_6q,_){var _6r=jsHasCtx2D(_6q);if(!E(_6r)){return _6o;}else{var _6s=jsGetCtx2D(_6q);return [1,[0,[0,_6s],[0,_6q]]];}},_6t=function(_6u,_){return _6p(E(_6u)[1],_);},_6v=function(_6w,_6x){return A(_6w,[function(_){var _6y=jsFind(toJSStr(E(_6x))),_6z=E(_6y);return _6z[0]==0?_6o:_6t(_6z[1],_);}]);},_6A=function(_6B,_6C,_6D){while(1){var _6E=(function(_6F,_6G,_6H){var _6I=E(_6H);if(!_6I[0]){return [0,_6F,_6G];}else{var _6J=_6I[1],_6K=[1,new T(function(){return E(A(_6J,[_6G])[1]);}),_6F];_6C=new T(function(){return E(A(_6J,[_6G])[2]);});_6D=_6I[2];_6B=_6K;return null;}})(_6B,_6C,_6D);if(_6E!=null){return _6E;}}},_6L=function(_6M,_6N){return _6M<=0?_6M>=0?quot(_6M,_6N):_6N<=0?quot(_6M,_6N):quot(_6M+1|0,_6N)-1|0:_6N>=0?_6M>=0?quot(_6M,_6N):_6N<=0?quot(_6M,_6N):quot(_6M+1|0,_6N)-1|0:quot(_6M-1|0,_6N)-1|0;},_6O=unCStr("ArithException"),_6P=unCStr("GHC.Exception"),_6Q=unCStr("base"),_6R=new T(function(){var _6S=hs_wordToWord64(4194982440),_6T=hs_wordToWord64(3110813675);return [0,_6S,_6T,[0,_6S,_6T,_6Q,_6P,_6O],_j];}),_6U=function(_6V){return E(_6R);},_6W=function(_6X){return E(E(_6X)[1]);},_6Y=unCStr("Maybe.fromJust: Nothing"),_6Z=new T(function(){return err(_6Y);}),_70=function(_71,_72,_73){var _74=new T(function(){var _75=A(_71,[_73]),_76=A(_72,[new T(function(){var _77=E(_74);return _77[0]==0?E(_6Z):E(_77[1]);})]),_78=hs_eqWord64(_75[1],_76[1]);if(!E(_78)){return [0];}else{var _79=hs_eqWord64(_75[2],_76[2]);return E(_79)==0?[0]:[1,_73];}});return E(_74);},_7a=function(_7b){var _7c=E(_7b);return _70(_6W(_7c[1]),_6U,_7c[2]);},_7d=unCStr("arithmetic underflow"),_7e=unCStr("arithmetic overflow"),_7f=unCStr("Ratio has zero denominator"),_7g=unCStr("denormal"),_7h=unCStr("divide by zero"),_7i=unCStr("loss of precision"),_7j=function(_7k){switch(E(_7k)){case 0:return E(_7e);case 1:return E(_7d);case 2:return E(_7i);case 3:return E(_7h);case 4:return E(_7g);default:return E(_7f);}},_7l=function(_7m){return _5f(_7d,_7m);},_7n=function(_7m){return _5f(_7e,_7m);},_7o=function(_7m){return _5f(_7f,_7m);},_7p=function(_7m){return _5f(_7g,_7m);},_7q=function(_7m){return _5f(_7h,_7m);},_7r=function(_7m){return _5f(_7i,_7m);},_7s=function(_7t){switch(E(_7t)){case 0:return E(_7n);case 1:return E(_7l);case 2:return E(_7r);case 3:return E(_7q);case 4:return E(_7p);default:return E(_7o);}},_7u=[0,44],_7v=[0,93],_7w=[0,91],_7x=function(_7y,_7z,_7A){var _7B=E(_7z);return _7B[0]==0?unAppCStr("[]",_7A):[1,_7w,new T(function(){return A(_7y,[_7B[1],new T(function(){var _7C=function(_7D){var _7E=E(_7D);return _7E[0]==0?E([1,_7v,_7A]):[1,_7u,new T(function(){return A(_7y,[_7E[1],new T(function(){return _7C(_7E[2]);})]);})];};return _7C(_7B[2]);})]);})];},_7F=function(_7G,_7H){return _7x(_7s,_7G,_7H);},_7I=function(_7J,_7K){switch(E(_7K)){case 0:return E(_7n);case 1:return E(_7l);case 2:return E(_7r);case 3:return E(_7q);case 4:return E(_7p);default:return E(_7o);}},_7L=[0,_7I,_7j,_7F],_7M=new T(function(){return [0,_6U,_7L,_7N,_7a];}),_7N=function(_7m){return [0,_7M,_7m];},_7O=3,_7P=function(_7Q,_7R){return die(new T(function(){return A(_7R,[_7Q]);}));},_7S=new T(function(){return _7P(_7O,_7N);}),_7T=[0,21],_7U=new T(function(){return [1,_7T,_7U];}),_7V=function(_7W){if(1<=_7W){var _7X=function(_7Y,_7Z){var _80=E(_7Z);return _80[0]==0?[0]:[1,[0,new T(function(){return [0,imul(_7Y,E(new T(function(){var _81=_7W+1|0;switch(_81){case -1:return [0,_6L(14,-1)];case 0:return E(_7S);default:return [0,_6L(14,_81)];}}))[1])|0];}),_80[1]],new T(function(){return _7Y!=_7W?_7X(_7Y+1|0,_80[2]):[0];})];};return _7X(1,_7U);}else{return [0];}},_82=0,_83=[1,_82,_j],_84=function(_85){return _85>1?[1,_82,new T(function(){return _84(_85-1|0);})]:E(_83);},_86=new T(function(){return _84(22);}),_87=[1,_86,_j],_88=function(_89){return _89>1?[1,_86,new T(function(){return _88(_89-1|0);})]:E(_87);},_8a=new T(function(){return _88(14);}),_8b=[0,_j],_8c=[1,_8b,_j],_8d=function(_8e){return _8e>1?[1,_8b,new T(function(){return _8d(_8e-1|0);})]:E(_8c);},_8f=function(_8g){var _8h=jsRound(_8g);return [0,_8h];},_8i=new T(function(){return [0,"(function(s){return s[0];})"];}),_8j=function(_8k){var _8l=A(_8k,[_]);return E(_8l);},_8m=function(_8n){return _8j(function(_){var _=0;return eval(E(_8n)[1]);});},_8o=new T(function(){return _8m(_8i);}),_8p=function(_8q,_){var _8r=A(_8o,[E(_8q),_]);return new T(function(){return _8f(_8r);});},_8s=function(_8t,_){return _8p(_8t,_);},_8u=function(_8v,_8w){var _8x=_8v%_8w;if(_8v<=0){if(_8v>=0){return E(_8x);}else{if(_8w<=0){return E(_8x);}else{var _8y=E(_8x);return _8y==0?0:_8y+_8w|0;}}}else{if(_8w>=0){if(_8v>=0){return E(_8x);}else{if(_8w<=0){return E(_8x);}else{var _8z=E(_8x);return _8z==0?0:_8z+_8w|0;}}}else{var _8A=E(_8x);return _8A==0?0:_8A+_8w|0;}}},_8B=new T(function(){return [0,"(function(s){return md51(s.join(\',\'));})"];}),_8C=new T(function(){return _8m(_8B);}),_8D=function(_8E,_){return A(_8C,[E(_8E),_]);},_8F=function(_8t,_){return _8D(_8t,_);},_8G=function(_8H){return _8j(function(_){var _=0;return _8F(_8H,_);});},_8I=function(_8J,_8K,_8L){while(1){var _8M=(function(_8N,_8O,_8P){if(_8N>_8O){var _8Q=_8O,_8R=_8N,_8S=_8P;_8J=_8Q;_8K=_8R;_8L=_8S;return null;}else{return [0,new T(function(){var _8T=(_8O-_8N|0)+1|0;switch(_8T){case -1:return [0,_8N];case 0:return E(_7S);default:return [0,_8u(_8j(function(_){var _=0;return _8s(_8P,_);})[1],_8T)+_8N|0];}}),new T(function(){return _8G(_8P);})];}})(_8J,_8K,_8L);if(_8M!=null){return _8M;}}},_8U=function(_8V,_8W){while(1){var _8X=E(_8V);if(!_8X[0]){return E(_8W);}else{_8V=_8X[2];var _8Y=_8W+1|0;_8W=_8Y;continue;}}},_8Z=function(_90){return [1,new T(function(){return _90;}),new T(function(){var _91=E(_90);return _91==6?[0]:_8Z(_91+1|0);})];},_92=new T(function(){return _8Z(0);}),_93=new T(function(){return [0,_8U(_92,0)-1|0];}),_94=function(_95){var _96=new T(function(){var _97=_8I(0,E(_93)[1],_95);return [0,_97[1],_97[2]];});return [0,new T(function(){var _98=E(E(_96)[1])[1];return _98>=0?_2(_92,_98):E(_8);}),new T(function(){return E(E(_96)[2]);})];},_99=0,_9a=function(_9b,_9c){var _9d=new T(function(){var _9e=_94(_9c);return [0,_9e[1],_9e[2]];});return [0,[0,_9b,new T(function(){return E(E(_9d)[1]);}),_99],new T(function(){return E(E(_9d)[2]);})];},_9f=function(_9g,_9h,_9i){return [0,new T(function(){var _9j=E(_9h)[1];if(_9j>0){var _9k=new T(function(){return _7V(E(_9i)[1]);}),_9l=new T(function(){var _9m=_6A(_j,_9g,_c(_9a,_9k));return [0,_9m[1],_9m[2]];}),_9n=[0,_8a,new T(function(){return E(E(_9l)[1]);}),_9k,new T(function(){return E(E(_9l)[2]);})],_9o=function(_9p){return _9p>1?[1,_9n,new T(function(){return _9o(_9p-1|0);})]:E([1,_9n,_j]);};return _9o(_9j);}else{return [0];}}),new T(function(){var _9q=E(_9h)[1];return _9q>0?_8d(_9q):[0];})];},_9r=new T(function(){return [0,"(function(){return md51(jsRand().toString());})"];}),_9s=function(_){return A(_8m,[_9r,_]);},_9t=function(_){return _9s(_);},_9u=unCStr("GHC.IO.Exception"),_9v=unCStr("base"),_9w=unCStr("IOException"),_9x=new T(function(){var _9y=hs_wordToWord64(4053623282),_9z=hs_wordToWord64(3693590983);return [0,_9y,_9z,[0,_9y,_9z,_9v,_9u,_9w],_j];}),_9A=function(_9B){return E(_9x);},_9C=function(_9D){var _9E=E(_9D);return _70(_6W(_9E[1]),_9A,_9E[2]);},_9F=unCStr(": "),_9G=[0,41],_9H=unCStr(" ("),_9I=unCStr("already exists"),_9J=unCStr("does not exist"),_9K=unCStr("protocol error"),_9L=unCStr("failed"),_9M=unCStr("invalid argument"),_9N=unCStr("inappropriate type"),_9O=unCStr("hardware fault"),_9P=unCStr("unsupported operation"),_9Q=unCStr("timeout"),_9R=unCStr("resource vanished"),_9S=unCStr("interrupted"),_9T=unCStr("resource busy"),_9U=unCStr("resource exhausted"),_9V=unCStr("end of file"),_9W=unCStr("illegal operation"),_9X=unCStr("permission denied"),_9Y=unCStr("user error"),_9Z=unCStr("unsatisified constraints"),_a0=unCStr("system error"),_a1=function(_a2,_a3){switch(E(_a2)){case 0:return _5f(_9I,_a3);case 1:return _5f(_9J,_a3);case 2:return _5f(_9T,_a3);case 3:return _5f(_9U,_a3);case 4:return _5f(_9V,_a3);case 5:return _5f(_9W,_a3);case 6:return _5f(_9X,_a3);case 7:return _5f(_9Y,_a3);case 8:return _5f(_9Z,_a3);case 9:return _5f(_a0,_a3);case 10:return _5f(_9K,_a3);case 11:return _5f(_9L,_a3);case 12:return _5f(_9M,_a3);case 13:return _5f(_9N,_a3);case 14:return _5f(_9O,_a3);case 15:return _5f(_9P,_a3);case 16:return _5f(_9Q,_a3);case 17:return _5f(_9R,_a3);default:return _5f(_9S,_a3);}},_a4=[0,125],_a5=unCStr("{handle: "),_a6=function(_a7,_a8,_a9,_aa,_ab,_ac){var _ad=new T(function(){var _ae=new T(function(){return _a1(_a8,new T(function(){var _af=E(_aa);return _af[0]==0?E(_ac):_5f(_9H,new T(function(){return _5f(_af,[1,_9G,_ac]);}));}));}),_ag=E(_a9);return _ag[0]==0?E(_ae):_5f(_ag,new T(function(){return _5f(_9F,_ae);}));}),_ah=E(_ab);if(!_ah[0]){var _ai=E(_a7);if(!_ai[0]){return E(_ad);}else{var _aj=E(_ai[1]);return _aj[0]==0?_5f(_a5,new T(function(){return _5f(_aj[1],[1,_a4,new T(function(){return _5f(_9F,_ad);})]);})):_5f(_a5,new T(function(){return _5f(_aj[1],[1,_a4,new T(function(){return _5f(_9F,_ad);})]);}));}}else{return _5f(_ah[1],new T(function(){return _5f(_9F,_ad);}));}},_ak=function(_al){var _am=E(_al);return _a6(_am[1],_am[2],_am[3],_am[4],_am[6],_j);},_an=function(_ao,_ap){var _aq=E(_ao);return _a6(_aq[1],_aq[2],_aq[3],_aq[4],_aq[6],_ap);},_ar=function(_as,_at){return _7x(_an,_as,_at);},_au=function(_av,_aw,_ax){var _ay=E(_aw);return _a6(_ay[1],_ay[2],_ay[3],_ay[4],_ay[6],_ax);},_az=[0,_au,_ak,_ar],_aA=new T(function(){return [0,_9A,_az,_aB,_9C];}),_aB=function(_aC){return [0,_aA,_aC];},_aD=7,_aE=function(_aF){return [0,_6o,_aD,_j,_aF,_6o,_6o];},_aG=function(_aH,_){return die(new T(function(){return _aB(new T(function(){return _aE(_aH);}));}));},_aI=function(_aJ,_){return _aG(_aJ,_);},_aK=function(_aL){return E(_aL);},_aM=[0,2],_aN=[0,300],_aO=[0,0],_aP=unCStr("Pattern match failure in do expression at Gui.hs:50:5-15"),_aQ=unCStr("canvas"),_aR=function(_aS,_aT,_aU,_aV){var _aW=E(_aU);if(!_aW[0]){return E(_aT);}else{var _aX=E(_aV);return _aX[0]==0?E(_aT):A(_aS,[_aW[1],_aX[1],new T(function(){return _aR(_aS,_aT,_aW[2],_aX[2]);})]);}},_aY=function(_aZ,_b0){var _b1=E(_aZ);if(!_b1){return [0,_j,_b0];}else{var _b2=E(_b0);if(!_b2[0]){return [0,_j,_j];}else{var _b3=new T(function(){var _b4=_aY(_b1-1|0,_b2[2]);return [0,_b4[1],_b4[2]];});return [0,[1,_b2[1],new T(function(){return E(E(_b3)[1]);})],new T(function(){return E(E(_b3)[2]);})];}}},_b5=unCStr("Control.Exception.Base"),_b6=unCStr("base"),_b7=unCStr("PatternMatchFail"),_b8=new T(function(){var _b9=hs_wordToWord64(18445595),_ba=hs_wordToWord64(52003073);return [0,_b9,_ba,[0,_b9,_ba,_b6,_b5,_b7],_j];}),_bb=function(_bc){return E(_b8);},_bd=function(_be){var _bf=E(_be);return _70(_6W(_bf[1]),_bb,_bf[2]);},_bg=function(_bh){return E(E(_bh)[1]);},_bi=function(_bj,_bk){return _5f(E(_bj)[1],_bk);},_bl=function(_bm,_bn){return _7x(_bi,_bm,_bn);},_bo=function(_bp,_bq,_br){return _5f(E(_bq)[1],_br);},_bs=[0,_bo,_bg,_bl],_bt=new T(function(){return [0,_bb,_bs,_bu,_bd];}),_bu=function(_bv){return [0,_bt,_bv];},_bw=unCStr("Irrefutable pattern failed for pattern"),_bx=function(_by,_bz){var _bA=E(_bz);if(!_bA[0]){return [0,_j,_j];}else{var _bB=_bA[1];if(!A(_by,[_bB])){return [0,_j,_bA];}else{var _bC=new T(function(){var _bD=_bx(_by,_bA[2]);return [0,_bD[1],_bD[2]];});return [0,[1,_bB,new T(function(){return E(E(_bC)[1]);})],new T(function(){return E(E(_bC)[2]);})];}}},_bE=[0,32],_bF=[0,10],_bG=[1,_bF,_j],_bH=function(_bI){return E(E(_bI)[1])==124?false:true;},_bJ=function(_bK,_bL){var _bM=_bx(_bH,unCStr(_bK)),_bN=_bM[1],_bO=function(_bP,_bQ){return _5f(_bP,new T(function(){return unAppCStr(": ",new T(function(){return _5f(_bL,new T(function(){return _5f(_bQ,_bG);}));}));}));},_bR=E(_bM[2]);return _bR[0]==0?_bO(_bN,_j):E(E(_bR[1])[1])==124?_bO(_bN,[1,_bE,_bR[2]]):_bO(_bN,_j);},_bS=function(_bT){return _7P([0,new T(function(){return _bJ(_bT,_bw);})],_bu);},_bU=new T(function(){return _bS("Utils.hs:31:9-31|(c, d : ds)");}),_bV=new T(function(){return _bS("Utils.hs:26:9-31|(c, _ : ds)");}),_bW=function(_bX,_bY,_bZ,_c0){var _c1=function(_c2,_c3,_c4){return _5f(_c2,[1,new T(function(){var _c5=E(_bY)[1];if(_c5>=0){var _c6=_aY(_c5,_c3),_c7=E(_c6[2]);return _c7[0]==0?E(_bV):_5f(_c6[1],[1,_bZ,_c7[2]]);}else{var _c8=E(_c3);return _c8[0]==0?E(_bV):_5f(_j,[1,_bZ,_c8[2]]);}}),_c4]);};if(_bX>=0){var _c9=_aY(_bX,_c0),_ca=E(_c9[2]);return _ca[0]==0?E(_bU):_c1(_c9[1],_ca[1],_ca[2]);}else{var _cb=E(_c0);return _cb[0]==0?E(_bU):_c1(_j,_cb[1],_cb[2]);}},_cc=false,_cd=true,_ce=function(_cf,_cg){while(1){var _ch=E(_cg);if(!_ch[0]){return true;}else{if(!A(_cf,[_ch[1]])){return false;}else{_cg=_ch[2];continue;}}}},_ci=function(_cj,_ck,_cl,_cm){var _cn=new T(function(){var _co=E(_cj);return [0,_co[1],new T(function(){return [0,E(_co[2])[1]-1|0];})];});return !_ce(function(_cp){var _cq=E(_cn),_cr=E(_cp),_cs=E(_cq[1])[1]+E(_cr[1])[1]|0;if(0>_cs){return false;}else{if(_cs>=14){return false;}else{var _ct=E(_cq[2])[1]+E(_cr[2])[1]|0;return 0>_ct?false:_ct>=22?false:_ct>=0?_cs>=0?_2(_2(_cm,_cs),_ct)==0?true:false:E(_8):E(_8);}}},_2N(_ck,_cl))?[0,[0,_cj,_ck,_cl],_cd]:[0,[0,_cn,_ck,_cl],_cc];},_cu=1,_cv=function(_cw,_cx,_cy){var _cz=E(_cw),_cA=_cz[1],_cB=_cz[2],_cC=_cz[3],_cD=_ci(_cA,_cB,_cC,new T(function(){return E(E(_cy)[1]);})),_cE=_cD[1];if(!E(_cD[2])){var _cF=E(_cy);return [0,_cF[1],[1,_cE,_cF[2]],_cF[3],_cF[4]];}else{var _cG=E(_cy),_cH=_cG[4];return [0,new T(function(){var _cI=function(_cJ){var _cK=E(_cJ);if(!_cK[0]){return E(_cG[1]);}else{var _cL=E(_cK[1]);return _bW(E(_cL[1])[1],_cL[2],_cu,new T(function(){return _cI(_cK[2]);}));}};return _cI(_2Q(_cA,_cB,_cC));}),[1,new T(function(){var _cM=E(_cE);return [0,_cx,new T(function(){return E(_94(_cH)[1]);}),_99];}),_cG[2]],_cG[3],new T(function(){return E(_94(_cH)[2]);})];}},_cN=function(_cO,_cP,_cQ,_cR){return _aR(_cv,[0,_cO,_j,_cQ,_cR],_cP,_cQ);},_cS=function(_cT){var _cU=E(_cT);return _cN(_cU[1],_cU[2],_cU[3],_cU[4]);},_cV=function(_cW){var _cX=E(_cW);return [0,new T(function(){return _c(_cS,_cX[1]);}),_cX[2]];},_cY=function(_){var _cZ=_9t(_),_d0=A(_6v,[_aK,_aQ,_]),_d1=E(_d0);if(!_d1[0]){return _aI(_aP,_);}else{var _d2=function(_d3,_){var _d4=E(_d1[1]),_d5=_d4[1],_d6=jsResetCanvas(E(_d4[2])[1]),_d7=new T(function(){var _d8=E(_d3);return [0,new T(function(){return _c(_cS,_d8[1]);}),_d8[2]];}),_d9=A(_5T,[new T(function(){return _2(E(_d7)[1],0);}),_aO,_aO,_d5,_]),_da=A(_5T,[new T(function(){return _2(E(_d7)[1],1);}),_aN,_aO,_d5,_]),_db=jsSetTimeout(100,function(_){return _d2(new T(function(){return _cV(_d3);}),_);});return _42;};return _d2(new T(function(){var _dc=_9f(_cZ,_aM,_aM);return [0,_dc[1],_dc[2]];}),_);}},_dd=function(_){return _cY(_);};
var hasteMain = function() {A(_dd, [0]);};window.onload = hasteMain;