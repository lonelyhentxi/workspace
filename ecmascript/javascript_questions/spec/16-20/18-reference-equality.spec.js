describe("reference equality",()=>{
    it("only check reference and its type",()=>{
        function checkAge(data) {
            if (data === { age: 18 }) {
                return 0;
            } else if (data == { age: 18 }) {
                return 1;
            } else {
                return 2;
            }
        }

        expect(checkAge({ age: 18 })).toBe(2);
    });
});
