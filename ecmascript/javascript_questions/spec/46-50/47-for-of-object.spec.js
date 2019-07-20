describe("for of object", () => {
    it("should iterate key", () => {
        const person = {
            name: "Lydia",
            age: 21
        };

        const res = [];
        for (const item in person) {
            res.push(item);
        }
        expect(res).toEqual(["name", "age"]);
    });
});
