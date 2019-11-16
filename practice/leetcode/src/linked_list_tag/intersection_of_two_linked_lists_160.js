const assert = require('assert');
const { ListNode, buildList } = require('./linked_list_cycle_141');

/*
 * @lc app=leetcode.cn id=160 lang=javascript
 *
 * [160] 相交链表
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
 * @param {ListNode} headA
 * @param {ListNode} headB
 * @return {ListNode}
 */
var getIntersectionNode = function(headA, headB) {
    if (headA==null||headB==null) {
        return null;
    }
    let currentA = headA;
    let currentB = headB;
    let countA = 1;
    let countB = 1;
    while (currentA.next!=null) {
        countA += 1;
        currentA = currentA.next;
    }
    while (currentB.next!=null) {
        countB += 1;
        currentB = currentB.next;
    }
    if(currentA!=currentB) {
        return null;
    }
    let countSub = Math.abs(countA-countB);
    currentA = headA;
    currentB = headB;
    if (countA > countB) {
        while (--countSub>=0) {
            currentA = currentA.next;
        }
    } else if(countA < countB) {
        while (--countSub>=0) {
            currentB = currentB.next;
        }
    }
    while(currentA!=currentB) {
        currentA = currentA.next;
        currentB = currentB.next;
    }
    return currentA;
};
// @lc code=end

function buildInterselectionList(head1, head2, skip1, skip2) {
    let feek1 = new ListNode(-1);
    let feek2 = new ListNode(-1);
    let current1 = feek1;
    let current2 = feek2;
    for(let i=0;i<skip1;i++) {
        let h1 = head1[i];
        current1.next = new ListNode(h1);
        current1 = current1.next;
    }
    for(let j=0;j<skip2;j++) {
        let h2 = head2[j];
        current2.next = new ListNode(h2);
        current2 = current2.next;
    }
    for(let k=skip1;k<head1.length;k++) {
        let h = head1[k];
        let new_node = new ListNode(h);
        if(current2.next==null) {
            current2.next = new_node;
        }
        current1.next = new_node;
        current1 = current1.next;
    }
    return [feek1.next,feek2.next,current2.next];
}

function testGetIntersectionNode() {
    let [head1, head2, intersection1] = buildInterselectionList([4,1,8,4,5],[5,0,1,8,4,5],2,3);
    assert(getIntersectionNode(head1,head2)==intersection1);
    let [head3, head4, intersection2] = buildInterselectionList([0,9,1,2,4],[3,2,4],3,1);
    assert(getIntersectionNode(head3,head4)==intersection2);
    let [head5, head6, intersection3] = buildInterselectionList([2,6,4],[2,6,4],3,2);
    assert(getIntersectionNode(head5,head6)==intersection3);
}

if (!module.parent) {
    testGetIntersectionNode()
} 