WORDS = ARGF.read.downcase.scan(/[a-z]+/)

DICTIONARY.inject(Hash.new(0)) { |dic, word|
  dic[word] += 1
  dic
}

p DICTIONARY.top_by_value(30)

module Enumerable
  def take_by(n, &black)
    sort_by(&black).take n
  end
end

class Hash
  def top_by_value(n)
    take_by(n) { |key, val| -val }
  end

  def bottom_by_value(n)
    take_by(n) { |key, val| val }
  end
end
