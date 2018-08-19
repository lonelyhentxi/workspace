class IncorrectAccessor
  attr_accessor :one, :two
  def initialize
    one=1 # incorrect - sets local variables
    self.two=2
  end
end

obj = IncorrectAccessor.new
puts obj.one.nil?
puts obj.two.nil?

class IncorrectInitialize
  attr_reader :answer
  def initialise
    @answer = 42
  end
end

ultimate = IncorrectInitialize.new
puts ultimate.answer.nil?

# ruby 的作用域规则已经被修改，该情况下 i 的内容已经不存在问题
c = 'carbon'
i = 'iodine'
elements = [c,i]
elements.each_with_index do |element,i|
  i = 1
end
puts c
puts i

# 优先级问题

def one(arg)
  if block_given?
    "block given to 'one' returns #{yield}"
  else
    arg
  end
end

def two
  if block_given?
    "block given to 'two' returns #{yield}"
  end
end

result1 = one two {
  'three'
}

result2 = one two do
  "three"
end

puts "with brases, result = #{result1}"
puts "with do/end, result = #{result2}"