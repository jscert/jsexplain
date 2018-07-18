var Debug = {
  not_yet_implemented_because : function(_, s) { console.log("not_yet_implemented_because: "+s) },
  impossible_because : function(_, s) { console.log("impossible_because: "+s) },
  impossible_with_heap_because : function(_, s) { console.log("impossible_with_heap_because: "+s) },
  impossible_resvalue : function(_, _, s) { console.log(s); },
  ref_get_value : function() {},
  ref_get_value_2 : function() {},
  run_object_method : function() {},
  run_object_heap_set_extensible : function() {},
  lexical_env_get_identifier_ref : function() {}
};
