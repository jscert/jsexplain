var Translate_syntax = {
  eval_counter: 0,
  parse_esprima: function (strictness, src) {
    try {
      // TODO Fixup line numbers for eval context
      return Some(parseSource(src, "_eval_" + Translate_syntax.eval_counter++, true));
    } catch (e) {
      return None();
    }
  }
};
