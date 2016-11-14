var Debug = {
  not_yet_implemented_because : function(_, s) { throw new Error("not_yet_implemented_because: "+s) },
  impossible_because : function(_, s) { throw new Error("impossible_because: "+s) },
  impossible_with_heap_because : function(_, s) { throw new Error("impossible_with_heap_because: "+s) },
  ref_get_value : function() {},
  ref_get_value_2 : function() {},
  run_object_method : function() {},
  run_object_heap_set_extensible : function() {},
  lexical_env_get_identifier_ref : function() {}
};
