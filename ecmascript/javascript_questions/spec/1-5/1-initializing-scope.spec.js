describe("initialize scope", () => {
    it("var and let", () => {
        function sayHi() {
            expect(name).toBe(undefined);
            expect(() => {
                console.log(age);
            }).toThrowError(ReferenceError);
            var name = "Lydia";
            let age = 21;
        }

        sayHi();
    });
});
