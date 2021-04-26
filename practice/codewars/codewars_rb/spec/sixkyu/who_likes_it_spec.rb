# frozen_string_literal: true

require 'rspec'
require 'sixkyu/who_likes_it'

describe 'who likes it' do
  it 'should return value' do
    expect(likes([])).to eq('no one likes this')
    expect(likes(['Peter'])).to eq('Peter likes this')
    expect(likes(%w[Jacob Alex])).to eq('Jacob and Alex like this')
    expect(likes(%w[Max John Mark])).to eq('Max, John and Mark like this')
    expect(likes(%w[Alex Jacob Mark Max])).to eq('Alex, Jacob and 2 others like this')
  end
end
