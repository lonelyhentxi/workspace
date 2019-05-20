describe("object expand assign", () => {
    it("basic usage", () => {
        const user = {name: "Lydia", age: 21};
        const admin = {admin: true, ...user};
        expect(admin).toEqual({admin: true, name: "Lydia", age: 21});
    });
});
