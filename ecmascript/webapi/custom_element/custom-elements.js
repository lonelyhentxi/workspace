function customTag(tagName, fn) {
    Array.from(document.getElementsByTagName(tagName))
        .forEach(fn)
}

function greetingHandler(element) {
    element.innerHTML = "你好，世界";
}

function showCustom(element) {
    const target = document.getElementsByTagName(element).item(0);
    alert(`target is HTMLUnknownElement: ${target instanceof HTMLUnknownElement}, target is HTMLElement: ${target instanceof HTMLElement}`);
}

customTag("greeting", greetingHandler);
