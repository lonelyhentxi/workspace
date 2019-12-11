# frozen_string_literal: true

require 'rspec'
require 'sixkyu/build_a_pile_of_cubes'

describe 'build a pile of cubes spec' do
  it 'should return correct value' do
    expect(find_nb(4_183_059_834_009)).to eq(2022)
    expect(find_nb(135_440_716_410_000)).to eq(4824)
    expect(find_nb(40_539_911_473_216)).to eq(3568)
  end
  it 'should return incorrect value' do
    expect(find_nb(24_723_578_342_962)).to eq(-1)
  end
end
