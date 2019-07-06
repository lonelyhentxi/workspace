describe("constructor func", () => {
    it("strange usage", () => {
        function Car() {
            this.make = "Lamborghini";
            return {make: "Maserati"};
        }

        const myCar = new Car();
        expect(myCar.make).toEqual("Maserati");
    });
});
