describe("typeof spread operator returns", () => {
    it("should return an array", () => {
        function getAge(...args) {
            return (typeof args);
        }

        expect(getAge(21)).toEqual("object");
    });
});
