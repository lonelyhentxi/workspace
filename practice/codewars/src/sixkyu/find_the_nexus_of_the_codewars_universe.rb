public def nexus(users)
  (users.to_a.map { |first, second| [(first - second).abs,first] }
    .reduce do |acc,curr| 
      if curr[0]<acc[0] or ( curr[0]==acc[0] and curr[1]<acc[1] )
        res = curr
      else
        res = acc
      end
      res
    end)[1]
end