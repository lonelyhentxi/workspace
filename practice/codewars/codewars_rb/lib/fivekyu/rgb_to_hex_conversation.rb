# frozen_string_literal: true

class RGB
  class << self
    def validate(value)
      if value.negative?
        0
      elsif value > 255
        255
      else
        value
      end
    end

    def rgb(red, green, blue)
      red = validate red
      green = validate green
      blue = validate blue
      lower = (red * 256 * 256 + green * 256 + blue).to_s(16).upcase
      '0' * (6 - lower.length) + lower
    end
  end
end
