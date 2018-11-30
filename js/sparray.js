
/* Semi-persistent arrays

   Following "Semi-Persistent Data Structures"
   Sylvain Conchon and Jean-Christophe Filliâtre
   https://www.lri.fr/~filliatr/ftp/publis/spds-rr.pdf

   Ported to JavaScript, and recursion eliminated,
   by Arthur Charguéraud.
*/

/*
  type PersistentArray :
  - data : an array, or null
  - key : index of update
  - val : value of update
  - base : pointer to another PersistentArray

  If data is not null, then data represents the array
  (and the other fields are undefined);
  Otherwise, the array represented corresponds to the array
  represented by base, with the index key modified with the value val.
*/

// data should be undefined or an array or an object (but not null)
var PersistentArray = function(data) {
  if (data === undefined)
    data = [];
  // assert isArray(data)
  this.data = data;
  this.key = undefined;
  this.val = undefined;
  this.base = undefined;
};

// compress path then read
PersistentArray.prototype.get = function(i) {
  this.reroot();
  return this.data[i];
};

// returns the underlying array or object, which should not be modified
PersistentArray.prototype.asReadOnlyArray = function() {
  this.reroot();
  return this.data;
};

// returns a copy of the array; does not work for objects
PersistentArray.prototype.asArray = function() {
  return this.asReadOnlyArray().slice();
};

// compress path then read
// returns something bigger than the real length
PersistentArray.prototype.length = function() {
  this.reroot();
  return this.data.length;
};

// returns a new array that contains an update
PersistentArray.prototype.copyAndSet = function(i, v) {
  this.reroot();
  var oldv = this.data[i];
  this.data[i] = v;
  var arr = new PersistentArray(this.data);
  // assert this.key, this.val, and this.base are undefined
  this.data = null;
  this.key = i;
  this.val = oldv;
  this.base = arr;
  return arr;
};

// compress path, at rate of about 5 million nodes per seconds.
// ensures this.data is not null after the call.
PersistentArray.prototype.reroot = function() {
  // var cost = 0;
  if (this.data !== null)
    return;
  var stack = [];
  var arr = this;
  while (arr.data === null) {
    // console.log("walk  key=" + arr.key + " / val=" + arr.val);
    // cost++;
    stack.push(arr);
    arr = arr.base;
  }
  // console.log("cost = " + cost);
  var data = arr.data;
  arr.data = null;
  // assert (stack.length > 0)
  // assert (data !== null)
  while (stack.length > 0) {
    var prev = stack.pop();
    // assert (prev.data === null)
    arr.key = prev.key;
    arr.val = data[prev.key];
    arr.base = prev;
    data[prev.key] = prev.val;
    arr = prev;
  }
  // assert (arr === this)
  arr.data = data;
  arr.key = undefined;
  arr.val = undefined;
  arr.base = undefined;
};



//--------------------------------------------------------------
// TESTS

/* auxiliary function for demos
 function show(n, a) {
 var s = "";
 for (var i = 0; i < n; i++) {
 s += a.get(i) + ", ";
 }
 console.log(s)
 }
 */

/* efficiency demo:

 var n = 5000000;
 var d = [];
 for (var i = 0; i < n; i++)
 d[i] = i;

 var ts = [];
 var t = new PersistentArray(d);
 for (var i = 0; i < n; i++) {
 ts.push(t);
 t = t.copyAndSet(i, i+1);
 }

 console.log(ts[0].get(0))
 console.log(ts[n-1].get(0))
 console.log(ts[0].get(0))
 console.log(ts[n-1].get(0))

 */

/* bonus for testing above with small values:
 for (var k = 0; k < ts.length; k++) {
 // console.log(" k = " + k);
 show(n, ts[k])
 }
 */

/* basic demo:
 var n = 8;
 var d = [];
 for (var i = 0; i < n; i++)
 d[i] = i;

 var t0 = new PersistentArray(d);
 var t1 = t0.copyAndSet(4,8);
 var t2 = t1.copyAndSet(3,9);
 var t3 = t2.copyAndSet(6,0);
 var t4 = t3.copyAndSet(4,1);

 show(n, t4)
 show(n, t3)
 show(n, t4)
 show(n, t2)
 show(n, t0)
 show(n, t3)
 show(n, t3)
 show(n, t4)

 // t0: 0 1 2 3 4 5 6 7
 // t1: 0 1 2 3 8 5 6 7
 // t2: 0 1 2 9 8 5 6 7
 // t3: 0 1 2 9 8 5 0 7
 // t4: 0 1 2 9 1 5 0 7
 */
