/*
var fun = function (a) {
    var y = 2;
    var x = "lol";
}

fun();
*/

fun = function () {
    var y = 1;
    eval("var x = 'oh yes'");
    /*
    var closure = function () {
    var y = eval("x");
    return y;
    };

    eval("var x = 'oh yes'");
    return closure();
    */
};

fun();