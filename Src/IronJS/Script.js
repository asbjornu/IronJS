var x = 1;
var y = 2;
var scope1 = function (a) {
    var scope2 = function (b) {
        var scope3_1 = function (c) {
            var x = 1;
        };
        var scope3_2 = function (d) {
            var y = 2;
        };
    };
};