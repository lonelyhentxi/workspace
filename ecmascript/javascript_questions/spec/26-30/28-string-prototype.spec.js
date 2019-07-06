describe("string prototype", () => {
    it("basic usage", () => {
        String.prototype.giveLydiaPizza = () => {
            return "Just give Lydia pizza already!";
        };
        const name = "Lydia";
        expect(name.giveLydiaPizza()).toEqual(String.prototype.giveLydiaPizza());
    });
});
