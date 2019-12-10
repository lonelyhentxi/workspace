const assert = require('assert');
const { ListNode, buildList } = require('./linked_list_cycle_141');

/*
 * @lc app=leetcode.cn id=142 lang=javascript
 *
 * [142] 环形链表 II
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
 * @return {ListNode}
 */
var detectCycle = function(head) {
    let fast_iter = head;
    let slow_iter = head;
    let step = 0;
    while (fast_iter!=null) {
        fast_iter = fast_iter.next;
        if(fast_iter==null) {
            return null;
        }
        fast_iter = fast_iter.next;
        slow_iter = slow_iter.next;
        step += 1;
        if (fast_iter===slow_iter) {
            let slow_iter = head;
            while(fast_iter!=slow_iter) {
                fast_iter = fast_iter.next;
                slow_iter = slow_iter.next;
            }
            return fast_iter;
        }
    }
    return null;
};
// @lc code=end

function testDetectCycle() {
    let node1 = buildList([3,2,0,-4],1);
    assert(detectCycle(node1)==node1.next);
    let node2 = buildList([1,2],0);
    assert(detectCycle(node2)==node2);
    let node3 = buildList([1],-1);
    assert(detectCycle(node3)==null);
}

if (!module.parent) {
    testDetectCycle();
} 