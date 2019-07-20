describe("immediately execution", () => {
    it("basic usage", () => {
        function sayHi() {
            return (() => 0)();
        }

        expect(typeof sayHi()).toEqual("number");
    });
});
