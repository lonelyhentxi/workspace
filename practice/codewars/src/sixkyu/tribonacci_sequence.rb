public def tribonacci(signature,n)
  if n < 0
    signature[0..0]
  elsif n < 3
    signature[0...n]
  else
    res = signature.clone
    (3...n).each { |i| res.push(res[i-3] + res[i-2] + res[i-1]) }
    res
  end
end