require 'rspec'
require 'fivekyu/where_my_anagrams_at'

describe 'where my anagrams at' do

  def test_anagrams(word, result, wrong)
    actual = anagrams(word, result + wrong)
    expect(actual.to_set).to eq(result.to_set)
  end

  it 'basic test' do
    word0 = "a"
    result0 = ["a"]
    wrong0 = ["b", "c", "d"]
    test_anagrams(word0, result0, wrong0)

    word1 = "ab"
    result1 = ["ab", "ba"]
    wrong1 = ["aa", "bb", "cc", "ac", "bc", "cd"]
    test_anagrams(word1, result1, wrong1)

    word2 = "abba"
    result2 = ["aabb", "bbaa", "abab", "baba", "baab"]
    wrong2 = ["abcd", "abbba", "baaab", "abbab", "abbaa", "babaa"]
    test_anagrams(word2, result2, wrong2)
  end
end