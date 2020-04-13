describe("several empty",()=>{
    it("before number",()=>{
        const numbers = [1,2,3];
        numbers[4] = 5;
        expect(numbers).toEqual([1,2,3,,5]);
    });
});
