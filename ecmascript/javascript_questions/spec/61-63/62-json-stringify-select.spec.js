describe("json stringify select",()=>{
    it("select usage",()=>{
        const settings = {
            username: "lydiahallie",
            level: 19,
            health: 90
        };

        const data = JSON.stringify(settings, ["level", "health"]);
        expect(data).toEqual(JSON.stringify({level: 19, health: 90}));
    });
});
