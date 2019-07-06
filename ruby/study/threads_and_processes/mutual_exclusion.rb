# thread.critical= 方法设置线程关键，当为 true 是，调度器将不会调度现有的线程去执行
# 不建议使用之，因为人类无法安全且良好的使用

# 监视器

class Counter
  attr_reader :count
  def initialize
    @count = 0
  end
  def tick
    @count += 1
  end
end


def try_mess_async(target_class)
  c = target_class.new
  t1 = Thread.new {100_000.times {c.tick}}
  t2 = Thread.new {100_000.times {c.tick}}

  t2.join
  t1.join

  puts c.count
end

try_mess_async(Counter)
# 指导书中说明此处会因为同步导致非 20000 的结果，然而实际运行并不是

# 监视器

require 'monitor'
class MonitorCounter < Monitor
  attr_reader :count
  def initialize
    @count = 0
    super
  end
  def tick
    synchronize do
      @count += 1
    end
  end
end

%Q(
class Monitor
  include MonitorMixin
end
)

try_mess_async(MonitorCounter)

# 外部监视器

class OutMonitorCounter
  attr_reader :count
  def initialize
    @count = 0
  end
  def tick
    @count += 1
  end
end

def try_out_monitor_counter
  c = OutMonitorCounter.new
  lock = Monitor.new
  t1 = Thread.new { 100_000.times { lock.synchronize { c.tick } } }
  t2 = Thread.new { 100_000.times { lock.synchronize { c.tick } } }
  t1.join; t2.join
  puts c.count
end

try_out_monitor_counter

%Q(
playlist = []
playlist.extend(MonitorMixin)

Thread.new do
  record = nil
  loop do
    playlist.synchronize do
      sleep 0.1 while playlist.empty?
      record = playlist.shift
    end
    play(record)
  end
end

Thread.new do
  loop do
    req = get_customer_request
    playlist.synchronize do
      playlist << req
    end
  end
end
)

# 可以使用条件变量来解决

require 'monitor'
SONGS = [
    'Blue Suede Shoes',
    'Take Five',
    'Bye Bye Love',
    'Rock Around The Clock',
    'Ruby Tuesday'
]
START_TIME = Time.now
def timestamp
  (Time.now-START_TIME).to_i
end

def get_customer_request
  sleep(6*rand)
  song = SONGS.shift
  puts "#{timestamp}: Requesting #{song}" if song
  song
end

def play(song)
  puts "#{timestamp}: Playing #{song}"
  sleep(rand)
end

ok_to_shutdown = false
playlist = []
playlist.extend(MonitorMixin)

plays_pending = playlist.new_cond
customer = Thread.new do
  loop do
    req = get_customer_request
    break unless req
    playlist.synchronize do
      playlist << req
      plays_pending.signal
    end
  end
end
player = Thread.new do
  loop do
    song = nil
    playlist.synchronize do
      break if ok_to_shutdown && playlist.empty?
      plays_pending.wait_while {playlist.empty?}
      song = playlist.shift
    end
    break unless song
    play(song)
  end
end

customer.join
ok_to_shutdown = true
player.join