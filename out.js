const assert = condition => {
    if (!condition) {
        throw new Error("AssertionError");
    }
};
rsc_module = (new(function() {
    this.main = (new(function() {
        const add = (x, y) => (x + y);
        const main = () => (() => {
            const sum = (add)(4, (add)(9, 3));
            ((console["log"]))((rsc_module.baz.id)(sum));
            return undefined;
        })();
        this.main = main;
    })());
    this.foo = (new(function() {
        this.foo = (new(function() {
            const id = (x) => x;
            this.id = id;
        })());
    })());
})());
rsc_module.main.main();