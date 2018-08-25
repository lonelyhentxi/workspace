# ARGV for ruby, there is a gotcha, ARGV[0] is the first params rather than program name

ARGV.each {|arg| puts arg}

# pragram exit
# @return Nil
def exit
  exit
end

# we can access environment variable via ENV
puts ENV['path']
# write access to ENV via ENV['xxx']=xxx
puts "In parent, term = #{ENV['TERM']}"



