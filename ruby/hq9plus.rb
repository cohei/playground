require '99.rb'

class HQ9Plus
  def initialize(src)
    @src = src
    @count = 0
  end

  def run
    @src.each_char do |c|
      case c
        when "H"
          hello
        when "Q"
          quine
        when "9"
          bot
        when "+"
          inc
      end
    end
  end

  private

  def hello
    puts "Hello, world!"
  end

  def quine
    print @src
  end

  def bot
    bottles
  end

  def inc
    @count += 1
  end

end

HQ9Plus.new(ARGF.read).run
