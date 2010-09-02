var foo = 1;
var bar = "2";

var x = function (a, b) {
    var c = 3;

    return function () {
        return a * c;
    }
};

with ({}) {
    var y = foo;
}