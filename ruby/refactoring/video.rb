class Movie
  REGULAR = 0
  NEW_RELEASE = 1
  CHILDRENS = 2

  attr_writer :price

  def initialize(title, price)
    @title, @price = title, price
  end

  # 各行の金額を計算
  def charge(days_rented)
    @price.charge(days_rented)
  end

  def frequent_renter_points(days_rented)
    @price.frequent_renter_points(days_rented)
  end
end

module DefaultPrice
  def frequent_renter_points(days_rented)
    1
  end
end

class RegularPrice
  include DefaultPrice

  def charge(days_rented)
    result = 2
    result += (days_rented - 2) * 1.5 if days_rented > 2
  end
end

class NewReleasePrice
  def frequent_renter_points(days_rented)
    # 新作2日間レンタルでボーナス点
    days_rented > 1 ? 2 : 1
  end

  def charge(days_rented)
    days_rented * 3
  end
end

class ChildrensPrice
  include DefaultPrice

  def charge(days_rented)
    result = 1.5
    result += (days_rented - 3) * 1.5 if days_rented > 3
  end
end

class Rental
  attr_reader :movie, :days_rented

  def initialize(movie, days_rented)
    @movie, @days_rented = movie, days_rented
  end

  # 各行の金額を計算
  def charge
    @movie.charge(days_rented)
  end

  # レンタルポイントを加算
  def frequent_renter_points
    @movie.frequent_renter_points(days_rented)
  end
end

class Customer
  attr_reader :name

  def initialize(name)
    @name = name
    @rentals = []
  end

  def add_rental(arg)
    @rentals << arg
  end

  def statement
    result = "Rental Record for #{name}\n"
    @rentals.each do |element|
      result += "\t" + element.movie.title + "\t" + element.charge.to_s + "\n"
    end

    # フッター行を追加
    result += "Amount owed is #{total_charge}\n"
    result += "You earned #{total_frequent_renter_points} frequent renter points"
  end

  def html_statement
    result = "<h1>Rental Record for <em>#{name}</em></h1><<p>\n"
    @rentals.each do |element|
      result += "\t" + element.movie.title + "\t" + element.charge.to_s + "\n"
    end

    # フッター行を追加
    result += "<p>Amount owed is <em>#{total_charge}</em><p>\n"
    result += "You earned #{total_frequent_renter_points} frequent renter points"
  end

  def total_charge
    @rentals.inject(0) { |sum, rental| sum + rental.charge }
  end

  private
  def total_frequent_renter_points
    @rentals.inject(0) { |sum, rental| sum + rental.frequent_renter_points }
  end
end

m = Movie.new "rashomon", RegularPrice.new
r = Rental.new m, 5
c = Customer.new "kodei"
c.add_rental(r)
puts c.statement
c.add_rental(r)
puts c.statement
