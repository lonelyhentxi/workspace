# frozen_string_literal: true

require 'rspec'
require 'sixkyu/multiples_of_3_or_5'

describe 'multiples of 3 or 5' do
  it 'should pass example tests' do
    expect(solution(10)).to eq(23)
    expect(solution(20)).to eq(78)
    expect(solution(200)).to eq(9168)
  end
end
