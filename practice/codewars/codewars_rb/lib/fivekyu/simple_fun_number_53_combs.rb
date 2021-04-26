# frozen_string_literal: true

def match_one(comb1, comb2, index1, index2)
  type1 = index1 >= 0 && index1 < comb1.length ? comb1[index1] : ' '
  type2 = index2 >= 0 && index2 < comb2.length ? comb2[index2] : ' '
  !(type1 == '*' && type2 == '*')
end

def combs(comb1, comb2)
  min_len = Float::INFINITY
  i = -comb2.length
  while i <= comb1.length
    j = 0
    while j < comb2.length && match_one(comb1, comb2, j + i, j); j += 1 end

    if j == comb2.length
      len = [comb1.length, i + comb2.length].max - [0, i].min
      min_len = [min_len, len].min
    end

    i += 1
  end
  min_len
end
