class RGB
  class << self
    def validate(value)
      if value < 0
        0
      elsif value > 255
        255
      else
        value
      end
    end

    def rgb(r, g, b)
      r = validate r
      g = validate g
      b = validate b
      lower = (r * 256 * 256 + g * 256 + b).to_s(16).upcase
      '0' * (6 - lower.length) + lower
    end
  end
end
