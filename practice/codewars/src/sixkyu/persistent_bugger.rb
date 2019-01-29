def persistence(n)
  number_s = n.to_s
  times = 0
  while number_s.length != 1
    number_s = number_s
                   .chars
                   .map {|i| i.to_i(10)}
                   .reduce(1, &:*)
                   .to_s(10)
    times += 1
  end
  times
end
