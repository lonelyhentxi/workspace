def search_substr(full_text, search_text, allow_over_tapping = true)
  search_list = search_text.split('')
  search_length = search_text.length
  match_list = []
  counter = 0
  if search_length > full_text.length || search_length.zero?
    return counter
  else
    (0...search_length).each do |i|
      match_list.push full_text[i]
    end
  end
  i = search_length - 1
  until i >= full_text.length
    add_counter = match_list == search_list
    counter += add_counter ? 1 : 0
    times = !allow_over_tapping && add_counter ? search_length : 1
    (1..times).each do |j|
      match_list.shift
      match_list.push full_text[i + j]
    end
    i += times
  end
  counter + (match_list == search_list ? 1 : 0)
end

def search_substr_better( full_text, search_text, allow_overlap = true )
  return 0 if search_text.empty?
  full_text.scan(allow_overlap ? /(?=#{search_text})/ : search_text).length
end