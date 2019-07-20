def solution(number)
  (3...number).reduce(0) do |acc, x|
    acc + if [3, 5].any? {|multi| x % multi == 0}
            x
          else
            0
          end
  end
end