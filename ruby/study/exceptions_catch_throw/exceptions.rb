# 可以使用内建的 Exception 或者自己继承自 StandardError 类
#
# 处理异常

%Q(
opfile_name = 'aa'
op_file = File::open(opfile_name, 'w')
begin
  while data = socket.read(512)
    op_file.write(data)
  end

rescue SystemCallError
  $stderr.print 'IO failed: ' + $!
  op_file.close
  File::delete(op_file)
  raise
end
)

# ensure 用于 rescue 之后，相当于 finally
# 如前所述，retry 会重新执行整个 begin end 区块

# 引发异常
#
# raise 引发当前错误
raise "bad_error" # 引发 runtimeError，消息指定为指定的字符串
raise ArgumentError, "Name too big", caller
# 第三种形式使用第一个参数创建异常，然后把相关联的消息设置给第二个参数，同时把栈信息给第三个参数

class RetryException < RuntimeError
  attr :ok_to_retry
  def initialize(ok_or_retry)
    @ok_to_retry = ok_or_retry
  end
end

# 捕获和抛出，catch 提供从深度嵌套的结构中跳转出来

catch (:done) do
  while line = gets
    throw :done unless fields = line.split(/\t/)
  end
end