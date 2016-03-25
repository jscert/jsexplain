
// ----------- Lineof ----------------

// see "generator/lineof.ml" and "lineof.js" 
function lineof(filename, token) {
   var f = lineof_data[filename];
   if (f == undefined) {
     console.log("could not find lineof for " + filename);
     return;
   }
   var d = f[token];
   if (d == undefined) {
     console.log("could not find token " + token + " for " + filename);
     return;
   }
   return { file: filename,
            start: {line: d[0], column: d[1]}, 
            end: {line: d[2], column: d[3]} };
};


// ----------- Auxiliary --------------

String.prototype.replaceAt=function(index, character) {
    return this.substr(0, index) + character + this.substr(index+character.length);
}

// ----------- Datalog ----------------

var datalog = [];

function reset_datalog() {
  datalog = [];
}

// filename assumed to be of js extension
function log_event(filename, token, ctx, type) {
  // TODO populate state with object_heap, env_record_heap, fresh_locations, and populate env

  // compute "foo.ml" from "foo.js"
  var len = filename.length;
  var mlfilename = filename.replaceAt(len-2, "m");
  mlfilename = mlfilename.replaceAt(len-1, "l");

  var jsloc = lineof(filename, token);
  var mlloc = lineof(mlfilename, token);

  var event = { token: token, locByExt: { "ml": mlloc, "js": jsloc },
                ctx : ctx, type : type, state: {}, env: {}};
  datalog.push(event);

  // for debug
  if (event.type == "return" && 
     // event.ctx.bindings[0].key == "#RETURN_VALUE#"   must be true
     event.ctx.bindings[0].val === undefined) {
    console.log("Return event carrying undefined");
    console.log(event);
   }
}



// ----------- Context ----------------


function ctx_empty() {
  return {tag: "ctx_nil"};
}

function ctx_push(ctx, bindings) {
  // for debug
  for (var i = 0; i < bindings.length; i++) {
    if (bindings[i].val === undefined) {
      console.log("ctx_push with undefined value");
      console.log(ctx);
      console.log(bindings);
    }
  }

  return {tag: "ctx_cons", next: ctx, bindings: bindings};
}

function ctx_to_array(ctx) {
  var a = [];

  while (ctx.tag === "ctx_cons") {
    var b = ctx.bindings;
    for (var i = b.length - 1; i >= 0; i--) {
      a.push(b[i]);
    }
    ctx = ctx.next;
  }

  return a;
}

