# frozen_string_literal: true

require 'rspec'
require 'sixkyu/steve_wants_to_build_a_beacon_pyramid'

describe 'Test build a beacon pyramid' do
  it 'Should calculate the required number of each material' do
    expect(blocks_to_collect(1)).to eq(total: 9, gold: 9, diamond: 0, emerald: 0, iron: 0)
    expect(blocks_to_collect(2)).to eq(total: 34, gold: 9, diamond: 25, emerald: 0, iron: 0)
    expect(blocks_to_collect(3)).to eq(total: 83, gold: 9, diamond: 25, emerald: 49, iron: 0)
  end
end
