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

        return inner_1_1(5);
    }

    return inner_1();

}

var x = outer(3, 4);