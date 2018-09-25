object exercises01{
  def main(args: Array[String]):Unit = {

    //2.1 Sum
    //println(sum(10))

    //2.2 Factorial
    //println(factorial(5))

    //2.3 Digits
    //println(digits(5634))

    //2.4 Sum of digits
    //println(sumDigits(566))

    //2.5 Greatest Common Divisor ####### NOT
    //println(gcd(10,55))

    //2.6 Fibonacci
    //println(fibonacci(7))

    //2.7 Pascal triangle
    println(pascal(3,2))

  }

  ///////////////////////////////////////////
  //2.1 Sum
  ///////////////////////////////////////////
  def sum(num: Integer) : Integer = {
    if (num <= 0 ) 0
    else num + sum(num - 1)
  }

  ///////////////////////////////////////////
  //2.2 Factorial
  ///////////////////////////////////////////
  def factorial(num: Integer) : Integer = {
    if (num <= 1 ) 1
    else num * factorial(num - 1)
  }

  ///////////////////////////////////////////
  //2.3 Digits
  ///////////////////////////////////////////
  def digits(num: Integer) : Integer = {
    def digits_2(num: Integer, accu: Integer) : Integer = {
      if (num <= 0 ) accu
      else digits_2(num / 10,accu + 1)
    }
    digits_2(num,0)
  }

  ///////////////////////////////////////////
  //2.4 Sum of digits
  ///////////////////////////////////////////
  def sumDigits(num: Integer ) : Integer = {
    def sumDigits_2(num: Integer, accu: Integer) : Integer = {
      if (num <= 0 ) accu
      else sumDigits_2(num / 10, accu + num%10)
    }
    sumDigits_2(num,0)
  }

  ///////////////////////////////////////////
  //2.5 Greatest Common Divisor
  ///////////////////////////////////////////
  def gcd(num1: Integer , num2: Integer) = {

  }

  ///////////////////////////////////////////
  //2.6 Fibonacci
  ///////////////////////////////////////////
  def fibonacci(pos: Int) = {
    def fibonacci_2(n: Int, curr: Int, next: Int): Int = {
      if (n == 0) curr
      else fibonacci_2(n-1, next, curr+next)
    }
    fibonacci_2(pos, 0, 1)
  }

  ///////////////////////////////////////////
  //2.7 Pascal triangle
  ///////////////////////////////////////////
  def pascal (x: Int, y: Int): Int = {
    if (y == 1 || x == y) 1
    else pascal( x-1, y-1 ) + pascal ( x-1, y)
  }
}
