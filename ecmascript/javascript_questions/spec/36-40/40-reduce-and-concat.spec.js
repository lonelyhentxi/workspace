describe("reduce and concat", () => {
    it("combine", () => {
        expect([[0, 1], [2, 3]].reduce((acc, cur) => {
            return acc.concat(cur);
        }, [1, 2])).toEqual([1, 2, 0, 1, 2, 3]);
    });
});
