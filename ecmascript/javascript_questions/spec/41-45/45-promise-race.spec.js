describe("promise race",()=>{
    it("basic usage",()=>{
        const firstPromise = new Promise((res) => {
            setTimeout(res, 500, "one");
        });

        const secondPromise = new Promise((res) => {
            setTimeout(res, 100, "two");
        });

        Promise.race([firstPromise, secondPromise]).then(res => expect(res).toEqual("two"));
    });
});
