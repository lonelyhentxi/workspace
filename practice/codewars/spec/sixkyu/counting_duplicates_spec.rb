require 'rspec'
require 'sixkyu/counting_duplicates'

describe 'duplicate count' do

  it 'should count duplicates' do
    expect(duplicate_count("")).to eq(0)
    expect(duplicate_count("abcde")).to eq(0)
    expect(duplicate_count("abcdeaa")).to eq(1)
    expect(duplicate_count("abcdeaB")).to eq(2)
    expect(duplicate_count("Indivisibilities")).to eq(2)
  end
end