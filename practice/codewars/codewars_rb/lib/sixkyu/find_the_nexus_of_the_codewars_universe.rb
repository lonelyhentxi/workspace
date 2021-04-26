# frozen_string_literal: true

public def nexus(users)
  (users.to_a.map { |first, second| [(first - second).abs, first] }
    .reduce do |acc, curr|
      res = if (curr[0] < acc[0]) || ((curr[0] == acc[0]) && (curr[1] < acc[1]))
              curr
            else
              acc
            end
      res
    end)[1]
end
