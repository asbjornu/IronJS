/*
var fun = function (a) {
    var y = 2;
    var x = "lol";
}

fun();
*/


var outer = function (a, b) {
    eval("var y = 'lol'");
    return eval("var z = 'asdasd'");
}

var x = outer(3, 4);