def my_new_method(arg1 = "Miles", arg2 = "Coltrane", arg3 = "Roach")
  # Code for the method would go here
  puts "#{arg1}, #{arg2}, #{arg3}"
end

def my_other_new_method
  # Code for the method would go here
end

def varargs(arg1, *rest)
  puts "Got #{arg1} and #{rest.join('. ')}"
end

varargs("ones")
varargs("ones", "two")
varargs "ones", "two", "three"

def take_block(p1)
  if block_given?
    yield(p1)
  else
    p1
  end
end

class TaxCalculator
  def initialize(name, &block)
    @name = name
    @block = block
  end

  def get_tax(amount)
    puts "#{@name} on #{amount} = #{@block.call(amount)}"
  end
end

tc = TaxCalculator.new("Sales tax") {|amt| amt * 0.075}

tc.get_tax(100)
tc.get_tax(250)

puts take_block("no block")
puts take_block("no block") {|s| s.sub(/no /, '')}

