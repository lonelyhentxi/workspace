# frozen_string_literal: true

require 'rspec'
require 'fivekyu/return_substring_instance_count_2'

describe 'Solution' do
  it 'should test for something' do
    expect(search_substr('aa_bb_cc_dd_bb_e', 'bb')).to eq(2)
    expect(search_substr('aaabbbcccc', 'bbb')).to eq(1)
    expect(search_substr('aaacccbbbcccc', 'cc')).to eq(5)
    expect(search_substr('aaa', 'aa')).to eq(2)
    expect(search_substr('aaa', 'aa', false)).to eq(1)
    expect(search_substr('aaabbbaaa', 'bb', false)).to eq(1)
    expect(search_substr('a', '')).to eq(0)
    expect(search_substr('', 'a')).to eq(0)
    expect(search_substr('', '')).to eq(0)
    expect(search_substr('', '', false)).to eq(0)
  end
end
