describe("control flow",()=>{
    it("error control flow",()=>{
        function greeting() {
            throw "Hello world!";
        }

        function sayHi() {
            try {
                greeting();
                return "It worked!";
            } catch (e) {
                return "Oh no an error!";
            }
        }

        expect(sayHi()).toEqual("Oh no an error!");
    });
});
