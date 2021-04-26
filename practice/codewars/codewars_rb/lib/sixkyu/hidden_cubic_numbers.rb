# frozen_string_literal: true

def isSumOfCubes(s)
  isolated_positive_integer_strs = s.scan(/(?!<=\d-)(\d+)(?!=\d)/).flatten
  filtered_with_length_strs = isolated_positive_integer_strs.join(' ').scan(/(\d{1,3})/).flatten
  hidden_cubes = (filtered_with_length_strs.select do |num_str|
    num_str.split('').map(&:to_i).inject(0) { |sum, x| sum + x.pow(3) } == num_str.to_i
  end).map(&:to_i)
  !hidden_cubes.empty? ? hidden_cubes.push(hidden_cubes.sum).join(' ') + ' Lucky' : 'Unlucky'
end