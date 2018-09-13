object exercises01{
  def main(args: Array[String]):Unit = {

    //println(sum(10))
    //println(factorial(5))
    //println(digits(5634))
    println(sumDigits(55).toInt)

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

}
