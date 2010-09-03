var z = 1;
var scope1 = function (a) {
    with (a) {
        var scope2 = function (b) {
            var scope3 = function (c) {
                var x = b;
            };
        };
    }
};