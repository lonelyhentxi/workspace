describe("class constructor", () => {
    it("basic usage", () => {
        class Dog {
            constructor(name) {
                this.name = name;
            }
        }

        Dog.prototype.bark = function () {
            return `Woof I am ${this.name}`;
        };

        const pet = new Dog("Mara");

        expect(pet.bark()).toEqual("Woof I am Mara");

        delete Dog.prototype.bark;

        expect(() => pet.bark()).toThrowError(TypeError);
    });
});
