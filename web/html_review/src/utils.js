function appendWindowEvent(eventName, event) {
    if (typeof window[eventName] === 'function') {
        const currentOnload = window[eventName];
        window[eventName] = () => {
            currentOnload();
            event();
        }
    } else {
        window[eventName] = event;
    }
}

function appendScrollToDownOnLoad() {
    const scrollToDown = () => {
        setTimeout(() => {
            window.scrollTo(0, window.innerHeight);
        }, 1000);
    };
    appendWindowEvent('onload', scrollToDown);
}
