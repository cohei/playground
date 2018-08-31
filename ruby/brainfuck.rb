class BF
  class BrainfuckError < StandardError
  end

  def initialize(src)
    @tokens = src.chars.to_a
    @jumps = jumpto(@tokens)
  end

  def jumpto(tks)
    jumps = {}
    starts = []

    tks.each_with_index do |c,i|
      if c == "["
        starts.push(i)
      elsif c == "]"
        raise BrainfuckError, "too many ']'s" if starts.empty?
        from = starts.pop
        to = i
        jumps[from] = to
        jumps[to] = from
      end
    end

    raise BrainfuckError, "too many '['s" unless starts.empty?

    jumps
  end

  def run
    tape = []
    pointer = 0 # point processing code
    current = 0 # current memory number

    while pointer < @tokens.size
      case @tokens[pointer]
        when "+"
          tape[current] ||= 0
          tape[current] += 1
        when "-"
          tape[current] ||= 0
          tape[current] -= 1
        when ">"
          current += 1
        when "<"
          current -= 1
          raise BrainfuckError, "no more tape on the left" if current < 0
        when "."
          n = (tape[current] || 0)
          print n.chr
        when ","
          tape[current] = $stdin.getc.ord
        when "["
          if tape[current] == 0
            pointer = @jumps[pointer]
          end
        when "]"
          if tape[current] != 0
            pointer = @jumps[pointer]
          end
      end
      pointer += 1
    end
  end
end

if ARGV.size == 0
  BF.new($stdin.read).run
else
  ARGV.each do |file|
    BF.new(File.read(file)).run
  end
end
