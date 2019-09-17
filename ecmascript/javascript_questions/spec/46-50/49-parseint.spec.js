describe("parse int", () => {
    it("parse first digits part", () => {
        const num = parseInt("7*6", 10);
        expect(num).toEqual(7);
    });
});
