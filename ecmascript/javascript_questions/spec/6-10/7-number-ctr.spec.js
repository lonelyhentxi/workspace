describe("number ctr", ()=>{
    it("basic usage",()=>{
        let a = 3;
        let b = new Number(3);
        let c = 3;
        expect(a==b).toBe(true);
        expect(a===b).toBe(false);
        expect(b===c).toBe(false);
    });
});
