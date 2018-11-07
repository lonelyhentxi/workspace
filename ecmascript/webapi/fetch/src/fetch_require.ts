const main = async () => {
    const response = await fetch("./resource/fetch_init.txt", {method: "get"});
    const innerText = await response.text();
    window.alert(`load text fetch file successfully. There is "${innerText}"`);
};

main()
    .then(() => {
    })
    .catch(err => {
        window.alert('load text fetch file failed.');
        console.log(err);
    });