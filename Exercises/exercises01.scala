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

    //2.5 Greatest Common Divisor ####### NOT ######
    //println(gcd(10,55))

    //2.6 Fibonacci
    //println(fibonacci(7))

    //2.7 Pascal triangle
    //println(pascal(3,2))

    //2.8 Currified function  ####### NOT ######
    //println(currified(3,2))

    //2.9 Sum elements
    //println(sumEle(List(1,2,3,4,5)))

    //2.10 Square elements
    //println(square(List(1,2,3,4,5)))

    //2.11 Largest element
    //println(largest(List(1,2,7,4,5)))

    //2.12 Reverse
    //println(reverse(List(1,2,7,4,5)))

    //2.13 Map
    //println(map(List(1, 2, 3, 4, 5), x => x * x))

    //2.14 Sorted
    //println(sorted(List(5, 4, 3, 2, 1), (a, b) => a > b))

    //2.15 Concatenation
    //println(concatenate(List(1,2,3), List(4, 5))

    //2.16 Zip
    //println(zip(List(1,2,3), List(4,5,6))) ######## REPASS #######

    //2.17 Dot product
    //println(dot(List(1,2,3,4), List(9,8,7,6)))

    //2.18 Scale  ####### NOT ######

    //2.19 Filter
    println(filter(List(9, 8, 7, 6, 5, 4), x => x % 3 == 0))


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

  ///////////////////////////////////////////
  //2.8 Currified function
  ///////////////////////////////////////////
  def currified(num1: Integer , num2: Integer) = {

  }

  ///////////////////////////////////////////
  //2.9 Sum elements
  ///////////////////////////////////////////
  def sumEle(l: List[Int]): Int = {
    if (l.isEmpty) 0
    else l.head + sumEle(l.tail)
  }

  ///////////////////////////////////////////
  //2.10 Square elements
  ///////////////////////////////////////////
  def square(l: List[Int]): List[Int] = {
    if (l.isEmpty) Nil
    else l.head * l.head :: square(l.tail)
  }

  ///////////////////////////////////////////
  //2.11 Largest element
  ///////////////////////////////////////////
  def largest(l: List[Int]): Int = {
    def largest_1(l: List[Int], max: Int): Int = {
      if (l.isEmpty) max
      else if (l.head > max) largest_1(l.tail,l.head)
      else largest_1(l.tail,max)
    }
    largest_1(l,0)
  }

  ///////////////////////////////////////////
  //2.12 Reverse
  ///////////////////////////////////////////
  def reverse(l: List[Int]): List[Int] = l match {
    case h :: tail => reverse(tail) ::: List(h)
    case Nil => Nil
  }

  ///////////////////////////////////////////
  //2.13 Map
  ///////////////////////////////////////////
  def map(l: List[Int], f: Int => Int): List[Int] = {
    if (l.isEmpty) Nil
    else f(l.head) :: map(l.tail,f)
  }

  ///////////////////////////////////////////
  //2.14 Sorted
  ///////////////////////////////////////////
  def sorted (l: List[Int], f: (Int,Int) => Boolean): Boolean = {
    def sorted_1 (l: List[Int], f: (Int,Int) => Boolean, h: Integer): Boolean = {
      if (l.isEmpty) true
      else if (f(h,l.head)) sorted_1(l.tail,f,l.head)
      else false
    }
    sorted_1(l.tail,f,l.head)
  }

  ///////////////////////////////////////////
  //2.15 Concatenation
  ///////////////////////////////////////////
  //def concatenate(l1: List[Int],l2: List[Int]): List[Int] = (l1,l2) match {

  //}

  ///////////////////////////////////////////
  //2.16 Zip
  ///////////////////////////////////////////
  def zip(l1: List[Int], l2: List[Int]): List[(Int,Int)] = (l1,l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x :: l1, y :: l2) => (x, y) :: zip(l1, l2)
  }

  ///////////////////////////////////////////
  //2.17 Dot product
  ///////////////////////////////////////////
  def dot(l1: List[Int],l2: List[Int]): Int = {
    if (l1.isEmpty || l2.isEmpty) 0
    else l1.head * l2.head + dot(l1.tail,l2.tail)
  }
  ///////////////////////////////////////////
  //2.18 Scale
  ///////////////////////////////////////////



  ///////////////////////////////////////////
  //2.19 Filter
  ///////////////////////////////////////////
  def filter (l: List[Int], f: Int => Boolean): List[Int] = {
    if (l.isEmpty) Nil
    else if (f(l.head)) l.head :: filter(l.tail,f)
    else filter(l.tail,f)
  }







}
