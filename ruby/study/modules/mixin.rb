module Debug
  def who_am_i?
    "#{self.class.name} {\##{self.object_id}}: #{self.to_s}"
  end

  def initialize(name)
    @name = name
  end
end

# 使用 include 需要被引入，例如 require，并且这种 include 是建立到模块的引用
# 按照原型链的方式思考这种 traits/interface 很容易理解
class Phonograph
  include Debug
  # ...
end
class EightTrack
  include Debug
  # ...
end
ph = Phonograph.new('West End Blues')
et = EightTrack.new('Surrealistic Pillow')
puts ph.who_am_i?
puts et.who_am_i?

class Song
  include Comparable

  def initialize(name, artist, duration)
    @name = name
    @artist = artist
    @duration = duration
  end

  def <=>(other)
    self.duration <=> other.duration
  end

  attr_reader :duration
end

song1 = Song.new('My Way', 'Sinatra', 225)
song2 = Song.new('Bicyclops', 'Fleck', 260)
puts song1 <=> song2
puts song1 < song2
puts song1 == song2
puts song1 > song2