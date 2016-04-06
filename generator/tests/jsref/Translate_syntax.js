var Translate_syntax = {
  eval_counter: 0,
  parse_esprima: function (strictness, src) {
    try {
      // EVAL: Uncomment line below to enable multiple eval tabs
      //return Some(parseSource(src, "_eval_" + Translate_syntax.eval_counter++, true));
      return Some(parseSource(src, "_eval_", true));
    } catch (e) {
      return None();
    }
  }
};
