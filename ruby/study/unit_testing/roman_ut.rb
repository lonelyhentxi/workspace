require './roman'
require 'test/unit'

class TestRoman < Test::Unit::TestCase
  def test_simple
    assert_equal('i',Roman.new(1).to_s)
    assert_equal('ix',Roman.new(9).to_s)
  end
end

# 常用的单元测试方法
# assert_raise(RuntimeError) {Roman.new(0)}
# assert_nothing_raised() {Roman.new(1)}


