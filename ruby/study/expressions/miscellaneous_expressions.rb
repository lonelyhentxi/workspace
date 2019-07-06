a = 1
b = 2
c = 3
puts (a.*(b)).+(c)

# bad idea
%Q(
class Fixnum
  alias old_plus +
  def +(other)
    old_plus(other).succ
  end
end
)
# 反引号和 %x 为前缀的分界形式可以执行操作系统的底层命令，但在某些环境下容易失败，需要谨慎使用，如 windows 下的 cmder

# 赋值 Assignment, 有不可变量赋值和指针赋值
# ruby 的赋值是以并行(假并行)的形式执行的, 多余的左值会被忽略，但是并行并不建议使用
# 赋值过程和 python 元祖并行赋值类似
x = 0
a, b, c = x, (x += 1), (x += 1)
puts [a, b, c].to_s

class BrokenAmplifier
  attr_accessor :left_channel, :right_channel

  def volume=(vol)
    left_channel = self.right_channel = vol
  end
end

ba = BrokenAmplifier::new
ba.left_channel = ba.right_channel = 99
ba.volume = 5
puts ba.left_channel
puts ba.right_channel
# * 和 javascript 中的 ... 类似
# 嵌套赋值
# ruby 不支持自增、自减操作符，但是支持 +=，-= 操作符

# 条件执行，任何不是 nil 和 false 都会被判定为真值
# defined? 返回类型

puts defined? 42.abs
# == 比较值是否相等
# === 用来 case 语句目标和每个 when 从句的项
# <=> 通用比较操作符
# < <= => > 小于、小于等于、大于、大于等于
# =~ 正则表达式模式匹配操作符
# eql? 接受者和参数是否有相同的类型和值
# equal? 接受者和参数有相同的对象 id

if 1 == 1 then
  handle = 'Dizzy'
end

# 如果是多行 if， 还可以写成以下这种形式

if 1 == 1
  handle = 'Dizzy'
end

# 或者类似 python，使用：分隔以保持形式紧凑
# unless 和 if 反义并且用法相反
# 也支持 c 风格三目运算符
a = 1 if 1
puts a

# case 表达式
year = 2018
leap = case
       when year % 400 == 0 then
         true
       when year % 100 == 0 then
         false
       else
         year % 4 == 0
       end
puts leap

# 或者使用 c 风格 case
input_line = 'debug'
case input_line
when 'debug'
  puts input_line
when /[w]+/
when 'quit', 'exit'
  exit
end

# 在 case 中可以使用 === 比较
# 循环
a = 100
while a == 0
  a -= 1
end
until a
  a += 1
end
# 执行块，类似于 do while
print "Hello\n" while false
begin
  print "Goodbye\n"
end while false
# 迭代器

3.times do
  print "Ho!"
end

# for in 类似其它语言中的 for in, 需要实现 each trait
for a in [1, 2, 3]
  puts a
end

class Periods
  def each
    yield 'Classical'
    yield 'Jazz'
    yield 'Rock'
  end
end

ps = Periods::new
for p in ps
  puts p
end

# break, redo, next
# break 结束迭代
# redo 重新执行当前循环
# next 跳到本次循环末尾
# break 和 next 可以传入参数
# retry 从头执行的迭代, 高版本 ruby 中这种用法被严格限制，只能用在 rescue 中

for i in 1..2
  print "Now at #{i}. Restart?"
end

# ruby 中的 while、until、for 没有引入新的作用域，前面存在的局部变量可以在循环中使用，循环中新创建的局部变量可以在循环后
# 使用
# 被迭代器创建的是新的作用域

