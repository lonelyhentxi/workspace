require 'rspec'
require 'sixkyu/build_a_pile_of_cubes'

describe 'build a pile of cubes spec' do
  it 'should return correct value' do
    expect(find_nb(4183059834009)).to eq(2022)
    expect(find_nb(135440716410000)).to eq(4824)
    expect(find_nb(40539911473216)).to eq(3568)
  end
  it 'should return incorrect value' do
    expect(find_nb(24723578342962)).to eq(-1)
  end
end