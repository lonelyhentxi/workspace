describe("prototype property", () => {
    it("must use prototype to add property", () => {
        function Person(firstName, lastName) {
            this.firstName = firstName;
            this.lastName = lastName;
        }

        const member = new Person("Lydia", "Hallie");
        Person.getFullName = function () {
            return `${this.firstName} ${this.lastName}`;
        };
        expect(() => {
            return member.getFullName();
        }).toThrowError(TypeError);
    });
});
