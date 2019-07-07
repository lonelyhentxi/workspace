describe("object and set", () => {
    it("object can only use string as key",()=>{
        const obj = { 1: "a", 2: "b", 3: "c" };
        const set = new Set([1, 2, 3, 4, 5]);

        expect(obj.hasOwnProperty("1")).toBe(true);
        expect(obj.hasOwnProperty(1)).toBe(true);
        expect(set.has("1")).toBe(false);
        expect(set.has(1)).toBe(true);
    });
});
