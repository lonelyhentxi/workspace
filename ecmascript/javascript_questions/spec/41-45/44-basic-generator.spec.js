describe("basic generator", () => {
    it("basic usage", () => {
        function* generator(i) {
            yield i;
            yield i * 2;
        }

        const gen = generator(10);
        expect(gen.next().value).toEqual(10);
        expect(gen.next().value).toEqual(20);
    });
});
