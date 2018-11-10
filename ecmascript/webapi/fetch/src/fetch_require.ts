const basicUse = async () => {
    const response = await fetch("./resource/fetch_init.txt", {method: "get"});
    const innerText = await response.text();
    window.alert(`load text fetch file successfully. There is "${innerText}"`);
};

const postData = async (url: string, data: any) => {
    console.log(data);
    await fetch(url, {
        body: JSON.stringify({"my-content": "my-content"}),
        cache: "no-cache",
        credentials: "same-origin",
        headers: {
            'user-agent': 'Mozilla/4.0 MDN Example',
            'content-type': 'application/json'
        },
        method: "POST",
        mode: "no-cors",
        redirect: "follow",
        referrer: "no-referrer"
    }).then((_) => {
        window.alert("post succeed.");
    });
};

const callAsync = (callee: Promise<any>) => {
    callee.then(() => {
        console.log("fetch succeed.")
    }).catch((e) => {
        console.log(e);
        window.alert("fetch error.");
    })
};
