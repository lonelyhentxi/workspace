def find_nb(m)
  if m == 0
    -1
  else
    med = (m ** 0.5).floor
    if med ** 2 == m
      med = 2 * med
    else
      return -1
    end
    upper = ((med + 1) ** 0.5).ceil
    lower = ((med - 1) ** 0.5).floor
    if lower * upper == med
      lower
    else
      -1
    end
  end
end