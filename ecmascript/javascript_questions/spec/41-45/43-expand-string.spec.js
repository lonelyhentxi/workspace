describe("expand string",()=>{
    it("basic usage",()=>{
        expect([..."Lydia"]).toEqual(["L", "y", "d", "i", "a"]);
    });
});
