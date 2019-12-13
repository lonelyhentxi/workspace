# frozen_string_literal: true

require 'rspec'
require 'sixkyu/hidden_cubic_numbers'

describe 'Test hidden cubic numbers' do
  it 'should find all cubic numbers' do
    expect(isSumOfCubes('0 9026315 -827&()')).to eq('0 0 Lucky')
    expect(isSumOfCubes('Once upon a midnight dreary, while100 I pondered, 9026315weak and weary -827&()'))
        .to(eq('Unlucky'))
  end
end