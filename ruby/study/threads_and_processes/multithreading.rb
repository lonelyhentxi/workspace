require 'net/http'

def create_thread
  pages = %w(www.sina.com www.baidu.com)
  threads = []
  for page_to_fetch in pages
    threads << Thread.new(page_to_fetch) do |url|
      h = Net::HTTP.new(url, 80)
      puts "Fetching: #{url}"
      resp = h.get("/", nil)
      puts "Got #{url}: #{resp.message}"
    end
  end
  threads.each {|thr| thr.join}
end

create_thread

# 操纵线程
# Thread#join 调用 thread 的线程会阻塞，可以指定 join 的超时参数
# Thread#join 返回 nil；Thread#value 返回 最后一条语句的值
# Thread#current 返回当前线程，Thread#list 返回所有线程
# Thread#status，Thread#alive？返回特定线程的状态
# Thread#priority= 调整优先级

def handle_thread
  count = 0
  threads = []
  10.times do |i|
    threads[i] = Thread.new do
      sleep(rand(0.1))
      Thread.current['mycount'] = count
      count += 1
    end
  end
  threads.each {|t| t.join; print t["mycount"], ", "}
  puts "count = #{count}"
end

handle_thread

# 线程和异常

def try_threads_and_exceptions
  threads = []
  4.times do |number|
    threads << Thread.new(number) do |i|
      raise "Boom!" if i == 2
      print "#{i}\n"
    end
  end
  threads.each do |t|
    begin
      t.join
    rescue RuntimeError => e then
      puts "Failed: #{e.message}"
    end
  end
end

try_threads_and_exceptions

