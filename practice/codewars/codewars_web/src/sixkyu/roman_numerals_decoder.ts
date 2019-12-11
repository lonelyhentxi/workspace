const singleElementMap: { [key: string]:number } = {
    I: 1,
    V: 5,
    X: 10,
    L: 50,
    C: 100,
    D: 500,
    M: 1000,
};

interface RomanWindow {
    startIndex: number;
    roman: string;
}

function windowCalculate(window: RomanWindow): number {
    const startIndex = window.startIndex;
    const roman = window.roman;
    const first = singleElementMap[roman[startIndex]];
    const secondIndex = startIndex + 1;
    if (secondIndex < roman.length) {
        const second = singleElementMap[roman[secondIndex]];
        if (first < second) {
            window.startIndex = startIndex + 2;
            return second - first;
        } else {
            window.startIndex = secondIndex;
            return first;
        }
    } else {
        window.startIndex = secondIndex;
        return first;
    }
}


/**
 * 这里使用的是窗口检查前后大小，如果前面小于后面，则后面减前面
 * 否则视作窗口为1
 * ！还可以使用字符串替换的方法，将字符串替换成等价的单调字符串
 */
export function solution(roman: string): number {
    // complete the solution by transforming the
    // string roman numeral into an integer
    let sum = 0;
    const window: RomanWindow = {
        startIndex: 0,
        roman,
    };
    while (window.startIndex < roman.length) {
        sum += windowCalculate(window);
    }
    return sum;
}