describe("change variables", () => {
    it("basic usage", () => {
        let num = 10;

        const increaseNumber = () => num++;
        const increasePassedNumber = number => number++;

        const num1 = increaseNumber();
        const num2 = increasePassedNumber(num1);

        expect(num1).toEqual(10);
        expect(num2).toEqual(10);
    });
});
