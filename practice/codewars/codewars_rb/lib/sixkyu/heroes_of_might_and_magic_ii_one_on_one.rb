# frozen_string_literal: true

def who_would_win(monster1, monster2) # rubocop:disable Metrics/AbcSize
  all_points = [(monster1.hitpoints * monster1.number).to_f, (monster2.hitpoints * monster2.number).to_f]
  numbers = [monster1.number, monster2.number]
  monsters = [monster1, monster2]
  current = 0
  while numbers[current].positive?
    another = (current + 1) % 2
    all_points[another] -= numbers[current] * monsters[current].damage # start attack
    numbers[another] = (all_points[another] / monsters[another].hitpoints).ceil # attack end
    current = another
  end
  winner = (current + 1) % 2
  "#{numbers[winner]} #{monsters[winner].type}(s) won"
end
