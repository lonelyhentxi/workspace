const assert = require('assert');

/*
 * @lc app=leetcode.cn id=240 lang=javascript
 *
 * [240] 搜索二维矩阵 II
 */

// @lc code=start
/**
 * @param {number[][]} matrix
 * @param {number} target
 * @return {boolean}
 */
var searchMatrix = function(matrix, target) {
    if (matrix.length==0||matrix[0].length==0) {
        return false;
    }
    const n = matrix.length;
    const m = matrix[0].length;
    if(target < matrix[0][0] || target > matrix[n-1][m-1]) {
        return false;
    }
    let x = n-1;
    let y = 0;
    while(true) {
        if (target > matrix[x][y]) { y+=1; }
        else if(target < matrix[x][y]) { x-=1; }
        else {
            return true;
        }
        if(x<0||y>=m) {
            return false;
        }
    }
};
// @lc code=end

function testSearchMatrix() {
    const matrix = [
        [1,   4,  7, 11, 15],
        [2,   5,  8, 12, 19],
        [3,   6,  9, 16, 22],
        [10, 13, 14, 17, 24],
        [18, 21, 23, 26, 30]
      ];
    assert(searchMatrix(matrix, 5)===true);
    assert(searchMatrix(matrix, 20)===false);
}


if(!module.parent) {
    testSearchMatrix()
}
