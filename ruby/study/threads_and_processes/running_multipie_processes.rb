system("tar xzf test.tgz")
puts "tar finished #{$?}"
# only for linux, do not run in windows
# result = `date`
# puts result

pig = IO.popen('ruby -v','w+')
pig.puts "ice cream after they go to bed"
pig.close_write
puts pig.gets

# windows 没有 fork 方法 popen - 由于是通过 fork 实现因此失败
# 并且这种错误是一种编译期错误
def popen_minus
  begin
    pipe = IO.popen('-','w+')
    if pipe
      pipe.puts "Get a job!"
      STDERR.puts "Child says '#{pipe.gets.chomp}'"
    else
      STDERR.puts "Dad says '#{gets.chomp}'"
      puts "OK"
    end
  rescue RuntimeError => e then
    puts e.message
  end
end

# 独立子进程
# 同理，windows 无 date
def independent_children
  exec('sort testfile > output.txt') if fork.nil?
  process.wait
  trap('CLD') do
    pid = Process.wait
    puts "Child pid #{pid}: terminated"
  end
  exec("sort testfile > output.txt") if fork.nil?
end

def blocks_and_subprocesses
  IO.popen("date") {|f| puts "Date is #{f.gets}"}
  fork do
    puts "In child, pid = #{$$}"
    exit 99
  end
  pid = Process.wait
  puts "Child terminal, pid = #{pid}, status = #{$?.exitstatus}"
end

