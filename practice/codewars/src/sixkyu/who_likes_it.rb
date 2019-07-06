def likes(names)
  size = names.length
  if size == 0
    'no one likes this'
  elsif size == 1
    "#{names[0]} likes this"
  elsif size == 2
    "#{names[0]} and #{names[1]} like this"
  elsif size == 3
    "#{names[0]}, #{names[1]} and #{names[2]} like this"
  else
    "#{names[0]}, #{names[1]} and #{size - 2} others like this"
  end
end