# frozen_string_literal: true

require 'rspec'
require 'sixkyu/minecraft_series_number_2_minimum_amount_of_fuel_needed_to_get_some_iron_ingots'

describe 'Test minecraft min fuel to get ingots' do
  it 'Should get ingots with min fuel' do
    expect(calc_fuel(37)).to eq(:lava => 0, :blaze_rod => 3, :coal => 0, :wood => 3, :stick => 2)
    expect(calc_fuel(21)).to eq(:lava => 0, :blaze_rod => 1, :coal => 1, :wood => 2, :stick => 1)
    expect(calc_fuel(123)).to eq(:lava => 1, :blaze_rod => 4, :coal => 0, :wood => 4, :stick => 13)
  end
end
