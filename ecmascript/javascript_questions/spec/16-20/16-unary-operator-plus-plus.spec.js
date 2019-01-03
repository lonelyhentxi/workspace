describe("unary operator plus plus", () => {
    it("basic usage", () => {
        let number = 0;
        expect(number++).toBe(0);
        expect(++number).toBe(2);
        expect(number).toBe(2);
    });
});
