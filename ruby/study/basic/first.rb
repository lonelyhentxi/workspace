def say_good_night(name)
  'Good night, ' + name
end

puts say_good_night('John-Boy')
puts say_good_night('Mary-Ellen')

puts "Good night, version #{1}"

a = ['ant', 'bee', 'cat', 'dog', 'elk']
puts a[0]
puts a[3]
b = %w[ant bee cat dog elk]
puts b[0]
puts b[3]
cello_key = 'cello'.to_sym
inst_section = {cello_key => 'string'}
puts inst_section.key? cello_key

class Song
  @@play_times = 0

  def initialize(name, artist, duration)
    @name = name
    @artist = artist
    @duration = duration
  end

  def play
    @@play_times += 1
  end

  def self.count
    @@play_times
  end

  attr_reader :name, :artist, :duration
  attr_writer :name, :artist, :duration

  private

  def haaa
    puts 'haaa'
  end
end

song = Song.new('Bicyclops', 'Fleck', 260)
puts song.inspect
puts song.name
song.play
# song.haaa

class KaraokeSong < Song
  def initialize(name, artist, duration, lyrics)
    super(name, artist, duration)
    @lyrics = lyrics
  end

  def lyrics
    @lyrics
  end

  def lyrics=(lyrics)
    @lyrics = lyrics
  end
end

puts Song.count

class MyLogger
  private_class_method :new
  @@logger = nil

  def self.create
    @@logger = new unless @@logger
    @@logger
  end
end

a = [1, 3, 5, 7, 9]
puts a[1]
puts a[-1]
puts a[3, 1]
puts a[1..3]

[1, 3, 5, 7].inject {|sum, element| sum + element}
[1, 3, 5, 7].inject {|product, element| product * element}

class File
  def self.my_open(*args)
    result = file = File.new(*args)
    if block_given?
      result = yield file
      file.close
    end
  end
end

def n_times(thing)
  return lambda {|n| thing*n}
end

p1 = n_times(23)
puts p1.call(3)
puts p1.call(4)