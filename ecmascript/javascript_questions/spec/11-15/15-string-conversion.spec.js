describe("string conversion", () => {
    it("string convert with plus binary operator", () => {
        function sum(a, b) {
            return a + b;
        }

        expect(sum(1, "2")).toEqual("12");
    });
});
