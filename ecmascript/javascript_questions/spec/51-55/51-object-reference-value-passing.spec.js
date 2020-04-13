describe("object reference value passing", () => {
    it("basic usage", () => {
        function getInfo(member, year) {
            member.name = "Lydia";
            year = "1998";
        }

        const person = {name: "Sarah"};
        const birthYear = "1997";

        getInfo(person, birthYear);
        expect(person).toEqual({name: "Lydia"});
        expect(birthYear).toEqual("1997");
    });
});
