var memoize = (function() {
    var memo = [];

	function m(f) {
		return function(n) {
			if (memo[n]) {
				return memo[n];
			}

			memo[n] = f(n);

			return memo[n];
		};
    }

	return m;
})();

var cool_fibonacci = memoize(function(n) {
	return (n === 0 || n === 1) ? n : cool_fibonacci(n - 1) + cool_fibonacci(n - 2);
});

console.log(cool_fibonacci(100) + " wow, this was fast!");