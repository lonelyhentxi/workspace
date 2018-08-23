=begin
if don't use it, because of trash collection, a and b will not be ouput
=end
a = 102.7
b = 95.1
ObjectSpace.each_object(Numeric) {|x| p x}
puts a
puts b

# show fixnum's prototype chain
klass = Fixnum
begin
  print klass
  klass = klass.superclass
  print '<' if klass
end while klass
puts
p Fixnum.ancestors

# Looking inside classes
class Demo
  @@var = 99
  CONST = 1.23
  private
  def private_method
  end
  protected
  def protected_method
  end
  public
  def public_method
    @inst=1
    i = 1
    j = 2
    local_variables
  end
  def self.class_method
  end
end

puts Demo.private_instance_methods(false).to_s
puts Demo.protected_instance_methods(false).to_s
puts Demo.public_instance_methods(false).to_s
puts Demo.singleton_methods(false).to_s
puts Demo.class_variables.to_s
puts Demo.constants - Demo.superclass.constants.to_s

demo = Demo.new
puts demo.instance_variables.to_s
puts demo.public_method.to_s
puts demo.instance_variables.to_s
