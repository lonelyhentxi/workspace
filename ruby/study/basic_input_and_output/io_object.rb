# io 对象是 ruby 程序和某些外部资源之间的一个双向通道

file = File.new('testfile', 'r')
file.close

%Q(
class File
  def self.open(*args)
    result = f = File.new(*args)
    if block_given?
      begin
        result = yield f
      ensure
        f.close
      end
    end
    return result
  end
end
)

# 简单读写
# 用于简单读写的所有方法都适用于文件对象

File.open('testfile') do |file|
  while line = file.gets
    puts line
  end
end

# 读取迭代器
File.open('testfile') do |file|
  file.each_byte {|ch| putc ch; print "."}
end

File.open('testfile') do |file|
  file.each_line('e') {|line| puts line}
end

# 写文件
# 除了少数几个对象外，简单输出会调用 to_s 方法
str1 = "\001\002\003"
puts str1
str2 = ""
puts str2
str2 << 1 << 2 << 3
puts str2
puts [1, 2, 3].pack("c*")
# 字符串 io
require 'stringio'

ip = StringIO.new('now is\nthe time\nto learn\nRuby!')
op = StringIO.new("",'w')

ip.each_line do |line|
  op.puts line.reverse
end
op.string