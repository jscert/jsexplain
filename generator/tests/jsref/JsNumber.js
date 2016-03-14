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
  }
};

