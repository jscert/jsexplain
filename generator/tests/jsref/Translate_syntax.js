var Translate_syntax = {
  parse_esprima: function (strictness, src) {
    try {
      // TODO Fixup line numbers for eval context
      return Some(esprimaToAST(esprima.parse(src, {loc: true, range: true}), src));
    } catch (e) {
      return None();
    }
  }
};
