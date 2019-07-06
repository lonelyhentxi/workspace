describe("basic set", () => {
    it("basic set", () => {
        const set = new Set([1, 1, 2, 3, 4]);
        expect([...set]).toEqual([1, 2, 3, 4]);
    });
});
