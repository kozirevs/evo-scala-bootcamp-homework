package basics

object Basics {

  def main(args: Array[String]): Unit = {
    println("lcm = " + lcm(35, 15))

    println("gcd = " + gcd(70, 30))
  }

  def lcm(a: Int, b: Int): Int = {
    if ((a == 0) || (b == 0)) 0
    else {
      val absA = Math.abs(a)
      val absB = Math.abs(b)
      val absHigherNumber = Math.max(absA, absB)
      val absLowerNumber = Math.min(absA, absB)
      var lcm = absHigherNumber
      while (lcm % absLowerNumber != 0)
        lcm += absHigherNumber
      lcm
    }
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
