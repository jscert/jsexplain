// Export the JsInterpreter ML module as a NodeJS module
try {
  module.exports = {
    JsInterpreter: JsInterpreter,
    JsInterpreterMonads: JsInterpreterMonads,
    JsInterpreterUtils: JsInterpreterUtils,
    JsCommonAux: JsCommonAux,
    HeapStr: HeapStr
  }
} catch (e) {}
