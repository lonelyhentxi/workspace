# frozen_string_literal: true

def duplicate_count(text)
  dict = {}
  dict.default = 0
  text.upcase.chars.each do |ch|
    dict[ch] += 1
  end
  dict.each_value.reduce(0) do |acc, value|
    acc + if value > 1
            1
          else
            0
          end
  end
end
