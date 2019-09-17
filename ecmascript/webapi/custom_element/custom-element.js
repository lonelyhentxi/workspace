class XGreeting extends HTMLElement {
    get content() {
        return this.getAttribute('content');
    }

    set content(val) {
        this.setAttribute('content', val);
    }
}

customElements.define('x-greeting', XGreeting);

export {
    XGreeting
};
