exports.staticPtrTable = {};

exports._deref = function(table, ptr) {
  return table[ptr];
};
