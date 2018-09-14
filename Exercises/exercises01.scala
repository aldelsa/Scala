object exercises01{
  def main(args: Array[String]):Unit = {

    //println(sum(10))
    //println(factorial(5))
    //println(digits(5634))
    //println(sumDigits(566))
    //println(gcd(10,55))
    println(fibonacci(4))

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

  ///////////////////////////////////////////
  def fibonacci(num: Integer ) : Integer  = {
    def fibonacci_2(num: Integer, accu: Integer) : Integer = {
      if (num <= 1 ) accu
      else {
          println(accu)
          fibonacci_2(num, accu + fibonacci_2(num-1, accu) + fibonacci_2(num-2, accu))
        }
    }
    fibonacci_2(num,0)
  }

}
