# 对于迭代器，就是实现相应的 trait/interface

class VowelFinder
  include Enumerable

  def initialize(string)
    @string = string
  end

  def each
    @string.scan(/[aeiou]/) do |vowel|
      yield vowel
    end
  end
end

vf = VowelFinder.new('the quick brown fox jumped')
puts vf.inject {|v, n| v + n}

module Summable
  def sum
    inject {|v, n| v + n}
  end
end

class Array
  include Summable
end

class Range
  include Summable
end

class VowelFinder
  include Summable
end

puts [1, 2, 3, 4, 5].sum
puts ('a'..'m').sum

module Observable
  def observers
    @observer_list ||= []
  end

  def add_observer(obj)
    observers << obj
  end

  def notify_observers
    observers.each {|o| o.update}
  end
end

# 在如下的这种境况下，一些mixin的冲突难以被发现
class TelescopeScheduler
  # other classes can register to get notifications
  # when the schedule changes
  include Observable

  def initialize
    @observer_list = [] # folks with telescope time
  end

  def add_viewer(viewer)
    @observer_list << viewer
  end
end

# 多数情况下，mixin 一般不通过实例方法访问，而使用访问器
# 模块可以使用模块一级的散列表，使用当前对象的id作为索引来保存相对于特定实例的数据
module Test
  State = {}

  def state=(value)
    State[object_id] = value
  end

  def state
    State[object_id]
  end
end

class Client
  include Test
end

c1 = Client.new
c2 = Client.new
c1.state = 'cat'
c2.state = 'dog'
puts c1.state
puts c2.state

# 解析有歧义的方法名，按照原型链理解，直属类->mixin->超类->超类的mixin

