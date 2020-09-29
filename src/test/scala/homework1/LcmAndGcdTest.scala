package homework1

import homework1.LcmAndGcd.{gcd, lcm}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class LcmAndGcdTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "lcm method" should "return lowest common multiple for two integers" in {
    lcm(35, 15) shouldEqual 105
    lcm(3, 7) shouldEqual 21
    lcm(25, 75) shouldEqual 75
    lcm(18, 15) shouldEqual 90
    lcm(21, 15) shouldEqual 105
  }

  "gcd method" should "return greatest common divisor for two integers" in {
    gcd(70, 30) shouldEqual 10
    gcd(12, 36) shouldEqual 12
    gcd(7, 9) shouldEqual 1
    gcd(28, 64) shouldEqual 4
    gcd(48, 36) shouldEqual 12
  }

}
