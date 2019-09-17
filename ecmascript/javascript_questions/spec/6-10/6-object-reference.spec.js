describe("object reference", () => {
    it("change inner variable", () => {
        let c = {greeting: "Hey!"};
        let d;

        d = c;
        c.greeting = "Hello";
        expect(d.greeting).toEqual("Hello");
    });
});
