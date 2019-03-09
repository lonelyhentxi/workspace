require 'rspec'
require 'fivekyu/rgb_to_hex_conversation'

describe 'Solution' do
  it 'should test for something' do
    expect(RGB.rgb(255, 255, 255)).to eq('FFFFFF')
    expect(RGB.rgb(255, 255, 300)).to eq('FFFFFF')
    expect(RGB.rgb(0, 0, 0)).to eq('000000')
    expect(RGB.rgb(148, 0, 211)).to eq('9400D3')
  end
end