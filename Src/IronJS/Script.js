var y = 1;
var z = "lol";
var x = z;
x = x;


var scope1 = function (a) {
    var b = 2;
        var scope2 = function (b) {
            var x = b;
        };
    }
};