# frozen_string_literal: true

def solution(number)
  (3...number).reduce(0) do |acc, x|
    acc + if [3, 5].any? { |multi| (x % multi).zero? }
            x
          else
            0
          end
  end
end
