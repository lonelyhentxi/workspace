describe("reference reset", () => {
    it("basic usage", () => {
        let person = {name: "Lydia"};
        const members = [person];
        person = null;

        expect(members).toEqual([{name: "Lydia"}]);
    });
});
