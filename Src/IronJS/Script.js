/*
var fun = function (a) {
    var y = 2;
    var x = "lol";
}

fun();
*/

fun = function () {
    var closure = function () {
        var y = eval("x");
        return y;
    };

    eval("var x = 'oh yes'");

    closure();
};

fun();