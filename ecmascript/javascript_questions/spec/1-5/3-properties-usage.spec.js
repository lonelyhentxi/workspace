describe("properties usage", () => {
    it("field, method, lambda", () => {
        const shape = {
            radius: 10,
            diameter() {
                return this.radius * 2;
            },
            perimeter: () => 2 * Math.PI * this.radius
        };

        expect(shape.diameter()).toBe(20);
        expect(shape.perimeter()).toBe(NaN);
    });
});
