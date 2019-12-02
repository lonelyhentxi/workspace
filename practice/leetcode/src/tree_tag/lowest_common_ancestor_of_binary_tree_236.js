const assert = require('assert');

/*
 * @lc app=leetcode.cn id=236 lang=javascript
 *
 * [236] 二叉树的最近公共祖先
 */

// @lc code=start
/**
 * Definition for a binary tree node.
 * function TreeNode(val) {
 *     this.val = val;
 *     this.left = this.right = null;
 * }
 */
/**
 * @param {TreeNode} root
 * @param {TreeNode} p
 * @param {TreeNode} q
 * @return {TreeNode}
 */
function find(root, p,  id) {
    if (root===null) {
        return -1;
    }
    if (root===p) {
        return id;
    }
    else {
        let leftId;
        let rightId;
        if(typeof id === 'bigint') {
            leftId = id * 2n + 1n
            rightId = leftId + 1n;
        }
        else if(id * 2 + 2 >= Number.MAX_SAFE_INTEGER) {
            leftId = BigInt(id) * 2n + 1n
            rightId = leftId + 1n;
        }
        else {
            leftId = id * 2 + 1;
            rightId = leftId + 1;
        }
        let res1 = find(root.left, p, leftId);
        if(res1!==-1) {
            return res1;
        }
        return find(root.right, p, rightId);
    }
}

function trace(id) {
    let temp = typeof id === 'bigint'? id + 1n: Number(id+1);
    return temp.toString(2);
}

var lowestCommonAncestor = function(root, p, q) {
    let pId = find(root, p, 0);
    let qId = find(root, q, 0);
    let pTrace = trace(pId);
    let qTrace = trace(qId);
    let i = 0;
    let j = 0;
    while (i<pTrace.length && j<qTrace.length && pTrace[i]===qTrace[j]) {
        i+=1;
        j+=1;
    }
    let current = root;
    for(let k=1;k<i;k++) {
        if(pTrace[k]==='1') {
            current = current.right;
        }
        else {
            current = current.left;
        }
    }
    return current;
};
// @lc code=end

function TreeNode(val) {
    this.val = val;
    this.left = this.right = null;
}

module.TreeNode = TreeNode;

function testLowestCommonAncestor1() {
    let tree = {
        val: 3,
        left: {
            val: 5,
            left: {
                val: 6,
                left: null, 
                right: null
            }, 
            right: {
                val: 2,
                left: {
                    val: 7,
                    left: null, 
                    right: null
                }, 
                right: {
                    val: 4,
                    left: null, 
                    right: null
                }
            }
        }, 
        right: {
            val: 1,
            left: {
                val: 0,
                left: null, 
                right: null
            }, 
            right: {
                val: 8,
                left: null, 
                right: null
            }
        }
    };
    assert(lowestCommonAncestor(tree, tree.left, tree.right)===tree);
    assert(lowestCommonAncestor(tree, tree.left, tree.left.right.right)===tree.left);
}

function testLowestCommonAncestor2() {
    let tree = {
        val: -1,
        left: {
            val: 0,
            left: {
                val: 1,
                left: {
                    val: 2,
                    left: {
                        val: 3,
                        left: null, 
                        right: null
                    }, 
                    right: null
                }, 
                right: null
            }, 
            right: null
        }, 
        right:null,
    };
    assert(
        lowestCommonAncestor(
            tree, 
            tree.left.left.left, 
            tree.left.left.left.left)
            ===tree.left.left.left);
}

if(module.parent==null) {
    testLowestCommonAncestor1()
    testLowestCommonAncestor2()
}