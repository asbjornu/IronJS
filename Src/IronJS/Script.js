/*
var fun = function (a) {
    var y = 2;
    var x = "lol";
}

fun();
*/


var outer = function (a, b) {
    eval("var y = 1");
    return y;
}

var x = outer(3, 4);