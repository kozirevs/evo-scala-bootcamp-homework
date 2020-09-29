package homework1

import scala.annotation.tailrec

object LcmAndGcd {

  def lcm(a: Int, b: Int): Int = if ((a == 0) || (b == 0)) 0 else Math.abs(a * b) / gcd(a, b)

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
