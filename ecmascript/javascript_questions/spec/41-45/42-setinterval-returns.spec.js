describe("set interval returns",()=>{
    it("return id",()=>{
        expect(typeof setInterval(()=>{
        },1000)).toEqual("object");
        // typename Timeout
    });
});
