describe("let global scope", () => {
    it("in immediate function", () => {
        (() => {
            let x = (y = 10);
        })();

        expect(typeof x).toEqual("undefined");
        expect(typeof y).toEqual("number"); // global
    });
});
