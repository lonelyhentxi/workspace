describe("another falsy", () => {
    it("three example", () => {
        expect(!!null).toBe(false);
        expect(!!"").toBe(false);
        expect(!!1).toBe(true);
    });
});
