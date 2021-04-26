require 'test/unit'

#
# @lc app=leetcode.cn id=297 lang=ruby
#
# [297] 二叉树的序列化与反序列化
#

# @lc code=start

# Encodes a tree to a single string.
#
# @param {TreeNode} root
# @return {string}
def serialize(root)
    queue = Queue.new
    queue.push root
    res = []
    while not queue.empty?
      front = queue.shift
      if not front.nil? then
        res.push front.val
        queue.push front.left
        queue.push front.right
      else
        res.push nil
      end
    end
    while not res.empty? and res[-1].nil?
      res.pop
    end
    res.to_s
end

# Decodes your encoded data to tree.
#
# @param {string} data
# @return {TreeNode}
def deserialize(data)
    items = ((data[1...-1].split ',').map { |item|
      item = item.strip
      if item == 'nil' then
        nil
      else
        Integer item
      end
    })
    queue = Queue.new
    i = 0
    size = items.size
    root = if items.empty? or items[i].nil? then nil else TreeNode.new items[i] end
    queue.push root
    while not queue.empty?
      front = queue.shift
      if front.nil? then 
        next
      else
        left_index = 2 * i + 1
        if left_index < size and not items[left_index].nil? then
          front.left = TreeNode.new items[left_index]
        end
        right_index = 2 * i + 2
        if right_index < size and not items[right_index].nil? then
          front.right = TreeNode.new items[right_index]
        end
      end
      queue.push front.left
      queue.push front.right
      i+=1
    end
    root
end


# Your functions will be called as such:
# deserialize(serialize(data))
# @lc code=end

def tree_equal?(tree1, tree2)
    if tree1.nil? and tree2.nil? then
        true
    elsif tree1.nil? or tree2.nil? then
        false
    elsif tree1.val != tree2.val then
        false
    else
       tree_equal?(tree1.left, tree2.left) and tree_equal?(tree1.right, tree2.right)
    end
end

class TreeNode
    attr_accessor :val, :left, :right
    def initialize(val)
        @val = val
        @left, @right = nil, nil
    end

    def ==(other)
        self.class == other.class and tree_equal?(self, other)
    end
end

class TestSerializeAndDeserialize < Test::Unit::TestCase
    def test_serialize_deserialize
        tree_node = TreeNode.new 1
        tree_node.left = TreeNode.new 2
        tree_node.right = TreeNode.new 3
        tree_node.right.left = TreeNode.new 4
        tree_node.right.right = TreeNode.new 5
        assert_equal(tree_node, deserialize(serialize tree_node))
    end
end