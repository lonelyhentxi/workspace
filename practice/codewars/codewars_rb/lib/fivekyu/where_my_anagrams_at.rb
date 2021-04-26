# frozen_string_literal: true

public def chars_count(word)
  counter = {}
  counter.default = 0
  word.chars.each { |x| counter[x] = counter[x] + 1 }
  counter
end

public def anagrams(word, words)
  word_hash = chars_count(word)
  word_length = word.length
  words.select { |w| word_length == w.length && word_hash == chars_count(w) }
end
