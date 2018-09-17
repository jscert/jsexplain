const __jsexplain_assert = require('assert');
const _compare_generic = function (val1, val2) {
  try {
    __jsexplain_assert.deepStrictEqual(val1, val2);
  } catch (_) {
    return false;
  }
  return true;
};
const _compare_stack = _compare_generic;
