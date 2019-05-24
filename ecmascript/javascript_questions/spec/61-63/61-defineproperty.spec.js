describe("", () => {
    it("", () => {
        const person = {name: "Lydia"};
        Object.defineProperty(person, "age", {value: 21});
        Object.defineProperty(person, "sex", {value: "female", enumerable: true});
        expect(person).toEqual({name: "Lydia", age: 21});
        expect(Object.keys(person)).toEqual(["name", "sex"]);
    });
});
