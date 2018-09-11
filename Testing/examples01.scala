object example01 {
  def main (args: Array[String]): Unit = {
    println(hello(args(0)))
  }

  def hello(a: String): String = {
    return "Hola " + a
  }
}
