/*
var fun = function (a) {
    var y = 2;
    var x = "lol";
}

fun();
*/


var outer = function (a, b) {

    var inner_1 = function () {
        var inner_1_1 = function (c) {
            var y = c;
            return a;
        }
    }

}