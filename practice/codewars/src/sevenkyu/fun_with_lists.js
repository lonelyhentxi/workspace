exports.FunWithListsNode = class FunWithListsNode {
    constructor(data, next = null) {
        this.data = data;
        this.next = next;
    }
}

function forEach (head, callback) {
    let index = -1;
    let current = head;
    while (current) {
        index += 1;
        let res = callback(current.data, index);
        if (res !== undefined) {
            return res;
        }
        current = current.next;
    };
    return;
}

exports.forEach = forEach;

exports.indexOf = function indexOf(head, value) {
    const index = forEach(head, (item, i) => {
        if (item === value) {
            return i;
        }
    })
    return index === undefined ? -1 : index;
}

exports.length = function length(head) {
    let index = 0;
    forEach(head, (item, i) => {
        index = i + 1;
    })
    return index;
}

exports.lastIndexOf = function lastIndexOf(head, value) {
    let index = -1;
    forEach(head, (item, i) => {
        if(item===value) {
            index = i;
        }
    })
    return index;
}

exports.countIf = function countIf(head, p) {
    let count = 0;
    forEach(head, (item,i) => {
        if(p(item)) {
            count+=1;
        }
    })
    return count;
}