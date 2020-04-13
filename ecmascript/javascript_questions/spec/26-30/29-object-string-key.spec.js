describe("object string key", () => {
    it("object will be convert to [object Object]", () => {
        const a = {};
        const b = {key: "b"};
        a[b] = 123;
        expect(a[b]).toBe(123);
        expect(a["[object Object]"]).toBe(123);
    });
});
