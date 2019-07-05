// Open Recursion in scala もしくは関数型もオブジェクト指向も仲良くしようよぉのお話
// https://lyrical-logical.hatenadiary.org/entry/20111107/1320671610

import collection.mutable.Map
import scala.util.chaining._

package objectoriented {
  class Fib {
    def fib(n: Int): Int =
      if(n <= 1) n else fib(n - 1) + fib(n - 2)
  }

  class FibMemo extends Fib {
    private val table: Map[Int, Int] = Map()

    override def fib(n: Int): Int =
      table.getOrElse(n, super.fib(n).tap(table(n) = _))
  }

  class FibMemoTrace extends FibMemo {
    override def fib(n: Int): Int = {
      println(s"Enter fib($n)")
      val result = super.fib(n)
      println(s"Exit fib($n)")
      result
    }
  }
}

package functional {
  object Fib {
    def fix[A](f: (A => A) => A => A): A => A =
      f((x: A) => fix(f)(x))

    def fib_(next: Int => Int)(n: Int): Int =
      if(n <= 1) n else next(n - 1) + next(n - 2)

    val fib = fix(fib_)

    private val table: Map[Int, Int] = Map()

    def memo(table: Map[Int, Int])(f: Int => Int)(n: Int): Int =
      table.getOrElse(n, f(n).tap(table(n) = _))

    val fibMemo = fix((memo(table) _) compose fib_)

    def trace(f: Int => Int)(n: Int): Int = {
      println(s"Enter fib($n)")
      val result = f(n)
      println(s"Exit fib($n)")
      result
    }

    val fibMemoTrace = fix((trace _) compose (memo(table) _) compose fib_)
  }
}
