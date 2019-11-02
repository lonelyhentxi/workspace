const assert = require('assert');

/*
 * @lc app=leetcode.cn id=141 lang=javascript
 *
 * [141] 环形链表
 */

// @lc code=start
/**
 * Definition for singly-linked list.
 * function ListNode(val) {
 *     this.val = val;
 *     this.next = null;
 * }
 */

/**
 * @param {ListNode} head
 * @return {boolean}
 */
var hasCycle = function(head) {
    let fast_iter = head;
    let slow_iter = head;
    while (fast_iter!=null) {
        fast_iter = fast_iter.next;
        if(fast_iter==null) {
            return false;
        }
        fast_iter = fast_iter.next;
        slow_iter = slow_iter.next;
        if (fast_iter===slow_iter) {
            return true;
        }
    }
    return false;
};
// @lc code=end


function ListNode(val) {
     this.val = val;
     this.next = null;
}

exports.ListNode = ListNode;

function buildList(array, cycleIndex) {
    let head = new ListNode(-1);
    let current = head;
    for(const i of array) {
        current.next = new ListNode(i);
        current = current.next;
    }
    let end = current;
    if(cycleIndex >= 0) {
        current = head;
        for(let i=0;i<=cycleIndex;i++) {
            current = current.next;
        }
        end.next = current;
    }
    return head.next;
}

exports.buildList = buildList;

function testHasCycle() {
    let node1 = buildList([3,2,0,-4],1);
    assert(hasCycle(node1)==true);
    let node2 = buildList([1,2],0);
    assert(hasCycle(node2)==true);
    let node3 = buildList([1],-1);
    assert(hasCycle(node3)==false);
}



if (!module.parent) {
    testHasCycle();
} 
