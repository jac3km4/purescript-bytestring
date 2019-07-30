'use strict';

exports.allocUnsafeImpl = Buffer.allocUnsafe;
exports.equalsImpl = function(a, b) { return a.equals(b); }
