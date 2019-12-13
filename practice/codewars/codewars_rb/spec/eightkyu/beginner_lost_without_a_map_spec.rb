# frozen_string_literal: true

require 'rspec'
require 'eightkyu/beginner_lost_without_a_map'

describe 'Test map with double' do
  it 'Should double map' do
    expect(maps([1, 2, 3])).to eq([2, 4, 6])
    expect(maps([4, 1, 1, 1, 4])).to eq([8, 2, 2, 2, 8])
    expect(maps([2, 2, 2, 2, 2, 2])).to eq([4, 4, 4, 4, 4, 4])
  end
end