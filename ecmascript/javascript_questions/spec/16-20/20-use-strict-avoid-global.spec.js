describe("use strict avoid global", () => {
    it("should avoid accidentally declare global variables", () => {
        expect(() => {
            function getAge() {
                "use strict";
                age = 21;
                console.log(age);
            }
            getAge();
        }).toThrowError(ReferenceError);
    });
});
