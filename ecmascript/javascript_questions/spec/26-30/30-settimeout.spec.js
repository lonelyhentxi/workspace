describe("set timeout", () => {
    it("execution time", () => {
        const res = [];
        const foo = () => res.push("First");
        const bar = () => setTimeout(() => res.push("Second"));
        const baz = () => res.push("Third");
        bar();
        foo();
        baz();
        expect(res).toEqual(["First", "Third"]);
        setTimeout(() => {
            expect(res).toEqual(["First", "Third", "Second"]);
        });
    });
});
