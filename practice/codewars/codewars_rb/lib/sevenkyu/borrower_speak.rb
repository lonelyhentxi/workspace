# frozen_string_literal: true

def borrow(s)
  (s.split('').select { |c| c.match(/[a-zA-Z\d]/) }).map(&:downcase).join('')
end
