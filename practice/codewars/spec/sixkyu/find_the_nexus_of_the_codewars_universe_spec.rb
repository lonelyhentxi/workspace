require "rspec"
require 'find_the_nexus_of_the_codewars_universe'

describe "finding the nexus" do
    it "basic test" do
        users = {1 => 3, 3 => 3, 5 => 1}
        expect(nexus(users)).to eq(3)
    
        users = {1 => 10, 2 => 6, 3 => 4, 5 => 1}
        expect(nexus(users)).to eq(3)
    end
  end