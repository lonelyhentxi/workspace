class FunWithListsNode {
    constructor(data, next = null) {
        this.data = data;
        this.next = next == null || next === undefined ? null : next;
    }
}

exports.FunWithListsNode = FunWithListsNode;

function forEach(head, callback) {
    let index = -1;
    let current = head;
    while (current) {
        index += 1;
        let res = callback(current.data, index);
        if (res !== undefined) {
            return res;
        }
        current = current.next;
    }
}

exports.forEach = forEach;

function nodeEqual(lhs, rhs) {
    if (lhs === rhs) {
        if (lhs == null) {
            return true;
        }
        if (!(lhs instanceof FunWithListsNode)) {
            return false;
        } else {
            return true;
        }
    } else if (lhs instanceof FunWithListsNode && rhs instanceof FunWithListsNode) {
        if (lhs.data !== rhs.data) {
            return false;
        } else {
            return nodeEqual(lhs.next, rhs.next);
        }
    } else {
        return false;
    }
}

exports.nodeEqual = nodeEqual;

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
        if (item === value) {
            index = i;
        }
    })
    return index;
}

exports.countIf = function countIf(head, p) {
    let count = 0;
    forEach(head, (item, i) => {
        if (p(item)) {
            count += 1;
        }
    })
    return count;
}

exports.anyMatch = function anyMatch(head, p) {
    return forEach(head, (item, i) => {
        if (p(item)) {
            return true;
        }
    }) === true;
}

exports.allMatch = function allMatch(head, p) {
    return forEach(head, (item, i) => {
        if (!p(item)) {
            return false;
        }
    })!==false;
}

function listFromArray(array) {
    const mapedArray = array.map(item => new FunWithListsNode(item, null));
    const length = mapedArray.length;
    mapedArray.forEach((node, index) => {
        node.next = index + 1 < length ? mapedArray[index + 1] : null;
    });
    return mapedArray.length > 0 ? mapedArray[0] : null;
}

exports.listFromArray = listFromArray;

exports.filter = function filter(head, p) {
    const res = [];
    forEach(head, (item, i) => {
        if (p(item)) {
            res.push(item);
        }
    })
    return listFromArray(res);
}

exports.map = function map(head, f) {
    const res = [];
    forEach(head, (item, i) => {
        res.push(f(item));
    })
    return listFromArray(res);
}

exports.reduce = function reduce(head, f, init) {
    let acc = init;
    forEach(head, (item, i) => {
        acc = f(acc, item)
    })
    return acc;
}