object exercises01{
  def main(args: Array[String]):Unit = {
    println(sum(3))
  }
 def sum(num: Integer) : Integer = {
  if (num <= 0 ) 0
  else num + sum(num - 1)
  }
}
