describe("continue keywords", () => {
    it("basic usage", () => {
        const res = [];
        for (let i = 1; i < 5; i++) {
            if (i === 3) continue;
            res.push(i);
        }
        expect(res).toEqual([1,2,4]);
    });
});
