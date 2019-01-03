describe("multiple var", () => {
    it("overlap", () => {
        var num = 8;
        // eslint-disable-next-line no-redeclare
        var num = 10;
        expect(num).toEqual(10);
    });
});
