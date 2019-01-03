describe("const assign details",()=>{
    it("basic usage",()=>{
        const obj = { a: "one", b: "two", a: "three" };
        expect(JSON.stringify(obj)).toEqual(JSON.stringify({ a: "three", b: "two" }));
    });
});
