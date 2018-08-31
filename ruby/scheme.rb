class Cons
  def initialize(a,d)
    @car = a
    @cdr = d
  end
  attr_accessor :car
  attr_accessor :cdr

  def to_a
    return [car] if cdr.nil?
    return [car] + cdr.to_a if cons? cdr
    raise 'not proper list'
  end

  def length
    return 1 if cdr.nil?
    return 1 + cdr.length
    raise 'not proper list'
  end

  def to_s
    str = "("
    cur = self
    while cons? cur.cdr
      str += cur.car.to_s + " "
      cur = cur.cdr
    end
    str += cur.car.to_s
    str += " . " + cur.cdr.to_s unless cur.cdr.nil?
    str += ")"
    return str
  end
end

def nil.to_s
  "()"
end

def list(*ss)
  return nil if ss.length == 0
  return cons(ss.first, list(*ss[1...ss.length]))
end

def cons?(s)
  s.kind_of?(Cons)
end

def cons(car,cdr)
  return Cons.new(car,cdr)
end

class Environment
  def initialize(parent)
    @parent = parent
    @hash = Hash.new
  end
  attr_reader :parent

  def key?(name)
    return current_key?(name) || (@parent && @parent.key?(name))
  end

  def current_key?(name)
    return @hash.key?(name)
  end

  def define(name,value)
    @hash.store(name,value)
  end

  def get(name)
    return @hash[name] if current_key? name
    return @parent.get(name) unless @parent.nil?
    raise "not defined: #{name}"
  end

  def set(name,value)
    return @hash.store(name,value) if current_key?(name)
    return @parent.set(name,value) unless @parent.nil?
    raise "not defined: #{name}"
  end
end

def parse_s(src)
  src = src.gsub(/\s+/,",")
           .gsub(/[a-zA-Z+\-*\/][a-zA-Z0-9?!]*/,":\\0")
           .gsub("(","list(")
  eval(src)
end

Frame = Struct.new(:parent,:env,:nxt,:rib)
Closure = Struct.new(:env,:params,:body)

def compile(sexpr,nxt)
  #p "compile #{sexpr.to_s},#{nxt.to_s}"
  return compile_list(sexpr,nxt) if cons? sexpr
  return [:refer, sexpr, nxt] if sexpr.kind_of? Symbol
  return [:const, sexpr, nxt]
end

def compile_list(sexp,nxt)
  case sexpr.car
  when :if
      th = campile(sexpr.caddr,nxt)
