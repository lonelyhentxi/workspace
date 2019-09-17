describe("new and ctr", () => {
    it("new with new object, or with global object", function () {
        function Person(firstName, lastName) {
            this.firstName = firstName;
            this.lastName = lastName;
        }

        const lydia = new Person("Lydia", "Hallie");
        const sarah = Person("Sarah", "Smith");

        expect(lydia).toEqual({firstName: "Lydia", lastName: "Hallie"});
        expect(sarah).toBe(undefined);
    });
});
