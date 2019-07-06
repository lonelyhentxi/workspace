describe("map undefined", () => {
    it("map undefined", () => {
        expect([1, 2, 3].map(num => {
            if (typeof num === "number") return;
            return num * 2;
        })).toEqual([undefined, undefined, undefined]);
    });
});
