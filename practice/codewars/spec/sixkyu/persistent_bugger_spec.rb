require 'rspec'
require 'sixkyu/persistent_bugger'

describe 'persistent_bugger' do

  it 'should persistent' do
    expect(persistence(39)).to eq(3)
    expect(persistence(4)).to eq(0)
    expect(persistence(25)).to eq(2)
    expect(persistence(999)).to eq(4)
  end
end