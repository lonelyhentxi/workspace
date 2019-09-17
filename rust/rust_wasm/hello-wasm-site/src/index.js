const js = import("../node_modules/@lonelyhentai/hello-wasm/hello_wasm.js");
js.then(js => {
    js.greet("WebAssembly");
});
