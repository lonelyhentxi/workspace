function maxSequence(arr) {
    let sum = 0;
    let max = 0;
    for (const i of arr) {
        sum = Math.max(i,sum+i);
        max = Math.max(max,sum)
    }
    return max;
}

exports.maxSequence = maxSequence;