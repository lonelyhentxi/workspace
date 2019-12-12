# frozen_string_literal: true

require 'rspec'
require 'sixkyu/heroes_of_might_and_magic_ii_one_on_one'

class Monster
  attr_accessor :type, :hitpoints, :number, :damage
  def initialize(type:, hitpoints:, number:, damage:)
    @type = type
    @hitpoints = hitpoints
    @number = number
    @damage = damage
  end
end

# rubocop:disable Lint/UselessAssignment
monster_pairs = [
  [Monster.new(type = 'Roc', hitpoints = 40, number = 6, damage = 8),
   Monster.new(type = 'Unicorn', hitpoints = 40, number = 4, damage = 13)],
  [Monster.new(type = 'Titan',       hitpoints = 300, number = 1,   damage = 50),
   Monster.new(type = 'Battle Dwarf', hitpoints = 20, number = 20, damage = 4)],
  [Monster.new(type = 'Paladin',     hitpoints = 50,  number = 8,   damage = 20),
   Monster.new(type = 'Skeleton',    hitpoints = 4,   number = 100, damage = 3)],
  [Monster.new(type = 'Bone Dragon', hitpoints = 150, number = 5,   damage = 45),
   Monster.new(type = 'Cyclops',     hitpoints = 80,  number = 11,  damage = 24)]
]
# rubocop:enable Lint/UselessAssignment

winners = [
  '5 Roc(s) won',
  '1 Titan(s) won',
  '1 Paladin(s) won',
  '1 Bone Dragon(s) won'
]

describe 'Who wins?' do
  it 'should pass basic tests' do
    monster_pairs.each_with_index do |pair, idx|
      expect(who_would_win(pair[0], pair[1])).to eq(winners[idx])
    end
  end
end
