describe("try catch scope", () => {
    it("let scope", () => {
        (()=>{
            let x,y;
            try {
                throw new Error();
            }catch (x) {
                (x=1), (y=2);
                expect(x).toBe(1);
            }
            expect(x).toBe(undefined);
            expect(y).toBe(2);
        })();
    });
});
