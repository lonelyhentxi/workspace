describe("falsy values",()=>{
    it("basic usage",()=>{
        expect(!0).toBe(true);
        expect(!new Number(0)).toBe(false);
        expect(!"").toBe(true);
        expect(!" ").toBe(false);
        expect(!new Boolean(false)).toBe(false);
        expect(!undefined).toBe(true);
    });
});
