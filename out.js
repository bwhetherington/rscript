log = console.log;
rsc_module = new (function() {
  this.test = new (function() {
    this.functions = new (function() {
      const add = (x, y) => eval(x) + eval(y);
      this.add = add;
      const fibonacci = x =>
        eval(x) < 2
          ? eval(x)
          : eval(fibonacci)(eval(x) - 1) + eval(fibonacci)(eval(x) - 2);
      this.fibonacci = fibonacci;
    })();
  })();
  this.factorial = new (function() {
    const factorial = n =>
      eval(n) === 0 ? 1 : eval(n) * eval(factorial)(eval(n) - 1);
    this.factorial = factorial;
  })();
  this.main = new (function() {
    const foo = "rsc_module.factorial.factorial";
    const main = () =>
      (() => {
        const val = eval(rsc_module.test.functions.fibonacci)(30);
        eval(log)(eval(val));
        return undefined;
      })();
    this.main = main;
  })();
})();
rsc_module.main.main();
