puts 'escape using  "\\"'
puts 'That\'s right'
puts "Seconds/day: #{24 * 60 * 60}"
puts "#{'Ho! ' * 3}Merry Christmas!"
puts "This is line #{$INPUT_LINE_NUMBER}"
puts %q/general single-quoted stringQ%q/
puts %Q!general double-quoted string%Q!
puts %Q(Seconds/day:' ' #{24 * 60 * 60}%Q)
string = <<END_OF_STRING
  The body of the string
  is the input lines up to
  one ending with the same
  text that followed the '<<'
END_OF_STRING

print <<-STRING1, <<-STRING2
  Concat
STRING1
    enate
STRING2

File.open('songdata') do |song_file|
  songs = Array.new
  song_file.each do |line|
    file, length, name, title = line.chomp.split(/\s*\|\s*/)
    name.squeeze!(" ")
    mins, secs = length.scan(/\d+/)
    songs.append({'title' => title, 'name' => name, 'length' => mins.to_i * 60 + secs.to_i})
  end
  puts songs[1]
end

class WordIndex
  def initialize
    @index = {}
  end

  def add_to_index(obj, *phrases)
    phrases.each do |phrase|
      phrase.scan(/\w[-\w']+/) do |word|
        word.downcase!
        @index[word] = [] if @index[word].nil?
        @index[word].push(obj)
      end
    end

    def lookup(word)
      @index[word.downcase]
    end
  end
end

reg =  Regexp.new('^\s*[a-z]')
b = /^s*[a-z]/
c = %r(^\s*[a-z])
name = "Fats Waller"
puts name =~ /a/
puts name =~ /z/