# frozen_string_literal: true

def who_would_win(monster1, monster2) # rubocop:disable Metrics/AbcSize
  all_points1 = monster1.hitpoints * monster1.number
  all_points2 = monster2.hitpoints * monster2.number
  while monster1.number.postive? || monster2.number.postive?
    # monster 1 attack
    all_points2 -= monster1.number * monster1.damage
    # attack 1 end
    monster2.number = (all_points2 / monster2.hitpoints).ceil
    # monster 2 attack
    all_points1 -= monster2.number * monster2.damage
    # attack 2 end
    monster2.number = (all_points1 / monster1.hitpoints).ceil
  end
  winner = if monster1.number <= 0
             monster2
           else
             monster1
           end
  "#{winner.number} #{winner.type}(s) won"
end
