describe("iterable closure scope", () => {
    it("let var", () => {
        const variables = {
            init: 0,
            cal: 0
        };
        for (var i = 0; i < 3; i++) {
            setTimeout(() => expect(i).toBe(variables.init + 3), 1);
        }
        for (let i = 0; i < 3; i++) {
            setTimeout(() => {
                const tmp = variables.cal;
                expect(i).toBe(tmp);
            }, 1);
            variables.cal++;
        }
    });
});
