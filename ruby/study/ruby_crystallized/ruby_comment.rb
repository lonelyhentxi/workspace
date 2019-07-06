# old version ruby only use 7-bit ascii, if want to use utf-8, must set the KCODE config
# ruby will see '=begin' '=end' as comment
=begin
this is comment
=end
puts 'this is code'

=begin
这段代码将在主体代码执行之前执行
顺序按照 BEGIN 的顺序
=end
BEGIN {
  puts 'this is start'
}

BEGIN {
  puts 'again this is start'
}

=begin
这段代码在主体代码执行之后执行
顺序按照反 END 的顺序
=end
END {
  puts 'again this is start'
}

END {
  puts 'this is start'
}