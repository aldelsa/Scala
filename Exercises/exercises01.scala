object exercises01{
  def main(args: Array[String]):Unit = {

    //println(sum(10))
    //println(factorial(5))
    //println(digits(5634))
    //println(sumDigits(566))
    //println(gcd(10,55))
    //println(fibonacci(7))
    println(pascal(3,2))

  }

  ///////////////////////////////////////////
  def sum(num: Integer) : Integer = {
    if (num <= 0 ) 0
    else num + sum(num - 1)
  }

  ///////////////////////////////////////////
  def factorial(num: Integer) : Integer = {
    if (num <= 1 ) 1
    else num * factorial(num - 1)
  }

  ///////////////////////////////////////////
  def digits(num: Integer) : Integer = {
    def digits_2(num: Integer, accu: Integer) : Integer = {
      if (num <= 0 ) accu
      else digits_2(num / 10,accu + 1)
    }
    digits_2(num,0)
  }

  ///////////////////////////////////////////
  def sumDigits(num: Integer ) : Integer = {
    def sumDigits_2(num: Integer, accu: Integer) : Integer = {
      if (num <= 0 ) accu
      else sumDigits_2(num / 10, accu + num%10)
    }
    sumDigits_2(num,0)
  }

  def gcd(num1: Integer , num2: Integer) = {

  }

  ///////////////////////////////////////////

  def fibonacci(pos: Int) = {
    def fibonacci_2(n: Int, curr: Int, next: Int): Int = {
      if (n == 0) curr
      else fibonacci_2(n-1, next, curr+next)
    }
    fibonacci_2(pos, 0, 1)
  }

  ///////////////////////////////////////////
  def pascal (x: Int, y: Int): Int = {
    if (y == 1 || x == y) 1
    else pascal( x-1, y-1 ) + pascal ( x-1, y)
  }
}
