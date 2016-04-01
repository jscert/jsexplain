var JsNumber = {
  /* Alternative approach to the int32 and uint32 conversions
     source: http://www.2ality.com/2012/02/js-integers.html
     function modulo(a, b) {
       return a - Math.floor(a/b)*b;
     }
     function ToUint32(x) {
       return modulo(ToInteger(x), Math.pow(2, 32));
     }

     function ToInt32(x) {
       var uint32 = ToUint32(x);
       if (uint32 >= Math.pow(2, 31)) {
         return uint32 - Math.pow(2, 32)
       } else {
         return uint32;
       }
     }
   */

  // this works because the >>> operator first converts its first argument to uint32
  to_uint32 : function (x) {
    return x >>> 0;
  },

  // this works because the >> operator first converts its first argument to int32
  to_int32 : function (x) {
    return x >> 0;
  },

  from_string : function (x) {
    return Number(x);
  },

  to_string : function (x) {
    return "" + x;
  },

  int32_left_shift : function (x, y) { return x << y; },
  int32_right_shift : function (x, y) { return x >> y; },
  uint32_right_shift : function (x, y) { return x >>> y; },

  int32_bitwise_and : function (x, y) { return x & y; },
  int32_bitwise_or : function (x, y) { return x | y; },
  int32_bitwise_xor : function (x, y) { return x ^ y; },
  int32_bitwise_not : function (x) { return ~ x; },

  floor : function (x) { return Math.floor(x); },
  neg : function (x) { return - x; },
  sign : function (x) { return Math.sign(x); },
  absolute : function (x) { return Math.abs(x); },
  fmod : function (x, y) { return x % y; },

  modulo_32 : function (x) { return x & 0x1F; },

  zero : 0.0,
  neg_zero : -0.0,
  one : 1.0,
  infinity : Number.POSITIVE_INFINITY,
  neg_infinity : Number.NEGATIVE_INFINITY,
  max_value : Number.MAX_VALUE,
  min_value : Number.MIN_VALUE,
  nan : Number.NaN,
  pi : Math.PI,
  e : Math.E,
  ln2 : Math.LN2,

  /* TODO: what about other functions from Math? */
};
