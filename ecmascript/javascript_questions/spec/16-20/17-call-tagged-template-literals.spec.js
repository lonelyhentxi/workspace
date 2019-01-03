describe("call tagged template literals", () => {
    it("basic usage", () => {
        function getPersonInfo(one, two, three) {
            return [one, two, three];
        }

        const person = "Lydia";
        const age = 21;
        expect(getPersonInfo`${person} is ${age} years old`).toEqual([["", " is ", " years old"],"Lydia",21]);
    });
});
