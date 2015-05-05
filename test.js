{
  console.alert("THIS IS A TEST");
  var a = 4;
  var b = a;
  b;
  var f = function() {
    print("print");
  };
  return a + 1;
}

/*
// from underscore.js

_.map = _.collect = function(obj, iteratee, context) {
    iteratee = cb(iteratee, context);
    var keys = !isArrayLike(obj) && _.keys(obj),
        length = (keys || obj).length,
        results = Array(length);
    for (var index = 0; index < length; index++) {
      var currentKey = keys ? keys[index] : index;
      results[index] = iteratee(obj[currentKey], currentKey, obj);
    }
    return results;
  };
*/
