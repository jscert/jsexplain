
// ----------- Lineof ----------------

// see "generator/lineof.ml" and "lineof.js" 
function lineof(filename, token) {
   var d = lineof_data[filename][token];
   return { file: filename,
            start: {line: d[0], column: d[1]}, 
            end: {line: d[2], column: d[3]} };
};


// ----------- Datalog ----------------

var datalog = [];

function reset_datalog() {
  datalog = [];
}

function log_event(loc, ctx, type) {
  // TODO populate state with object_heap, env_record_heap, fresh_locations, and populate env
  var event = {loc : loc, ctx : ctx, type : type, state: {}, env: {}};
  datalog.push(event);
}

// ----------- Context ----------------


function ctx_empty() {
  return {tag: "ctx_nil"};
}

function ctx_push(ctx, bindings) {
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

