class Counter
  def initialize(n=0)
    @value = n
  end

  def inc
    @value += 1
  end

  def value
    @value
  end
# or attr_reader :value
end
