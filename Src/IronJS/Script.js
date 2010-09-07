/*
var fun = function (a) {
    var y = 2;
    var x = "lol";
}

fun();
*/

var outer = function (a, b) {
    var y = 2;

    var inner3 = function () {
        return eval("y");
    }

    return inner3();
}

var x = outer(3, 4);