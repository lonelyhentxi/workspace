require 'socket'

def socket_try
  client = TCPSocket.open('127.0.0.1', 'finger')
  client.send('mysql\n', 0)
  puts client.readlines
  client.close
end

begin
  socket_try
rescue
  puts $ERROR_INFO
end

require 'net/http'

def http_try
  h = Net::HTTP.new('www.pragmaticprogrammer.com', 80)
  response = h.get('/index.html', nil)
  if response.message == 'OK' then
    puts response.body.scan(/<img src="(.*?)">/m).uniq
  else
    puts response.body
  end
end

http_try


require 'open-uri'

def try_openuri
  open('http://www.pragmaticprogrammer.com') do |f|
    puts f.read.scan(/<img src="(.*?)">/m).uniq
  end
end

try_openuri
