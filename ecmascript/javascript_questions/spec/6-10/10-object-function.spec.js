describe("object function", () => {
    it("can add field like object", () => {
        function bark() {
            console.log("Woof!");
        }

        bark.animal = "dog";
        bark();
        expect(bark.animal).toEqual("dog");
    });
});
