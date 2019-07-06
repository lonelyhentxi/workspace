# 如果没有指定接受者，则默认接收对象是 self
# 在方法调用中的数组展开
def five(a, b, c, d, e)
  "I was passed #{a}, #{b}, #{c}, #{d}, #{e}"
end

puts five(1, 2, 3, 4, 5)
puts five(1, 2, 3, *['a', 'b'])
puts five(*(10..14).to_a)

print "(t)imes of (p)lus: "
times = gets
print "number: "
number = Integer(gets)
if times =~ /^t/
  calc = lambda {|n| n * number}
else
  calc = lambda {|n| n + number}
end
puts((1..10).collect(&calc).join(', '))