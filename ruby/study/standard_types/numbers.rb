num = 81
6.times do
  puts "#{num.class}: #{num}"
  num *= num
end
num_list = [123456, 0d123456, 123_456, -543, 0xaabb, 0377, -0b10_1010, 123_456_789_123_456_789]
puts num_list.to_s
puts [?a,?\n,?\C-a,?\M-a,?\M-\C-a,?\C-?].to_s
3.times {print "X "}
1.upto(5) {|i| print i, " "}
99.downto(95) {|i| print i, " "}
50.step(80,5) {|i| print i, " "}