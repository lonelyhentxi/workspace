# frozen_string_literal: true

require 'rspec'
require 'fivekyu/simple_fun_number_53_combs'

describe 'Combs test' do
  it 'should works with basic test' do
    expect(combs('*..*', '*.*')).to eq(5)
    expect(combs('*...*', '*.*')).to eq(5)
    expect(combs('*..*.*', '*.***')).to eq(9)
    expect(combs('*.*', '*.*')).to eq(4)
    expect(combs('*.**', '*.*')).to eq(5)
    expect(combs('*.*.*', '***.*..*.*')).to eq(11)
  end
end
