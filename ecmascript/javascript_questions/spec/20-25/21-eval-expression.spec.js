describe("eval expression",()=>{
    it("basic usage",()=>{
        const sum = eval("10*10+5");
        expect(sum).toEqual(105);
    });
});
