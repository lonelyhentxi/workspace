describe("call or bind", () => {
    it("basic usage", () => {
        const person = {name: "Lydia"};

        function sayHi(age) {
            return `${this.name} is ${age}`;
        }

        expect(sayHi.call(person, 21)).toEqual("Lydia is 21");
        expect(sayHi.bind(person, 21)).toBeInstanceOf(Function);
    });
});
