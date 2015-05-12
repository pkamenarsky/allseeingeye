{
  /*
  console.alert("THIS IS A TEST");
  var a = 4;
  var b = a;
  b = 9;
  b = 10;
  a = 6;
  var c = a + b;
  f(c);
  f(c);
  f(a, b, c);
  return (a + b + c);
  */
  var a = 5;
  var c = a;
  var b = c;
  h(a);
  g(b);
  return (c + a + b + c);
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
