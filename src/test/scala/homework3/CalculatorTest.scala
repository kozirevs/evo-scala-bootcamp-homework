package homework3

import homework3.Calculator.process
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class CalculatorTest extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {

  "process method" should "calculate correctly" in {
    "4.0 divided by 5.0 is 0.80" shouldEqual process("divide 4 5")
    "the sum of 5.0 5.0 6.0 8.5 is 24.5" shouldEqual process("sum 5 5 6 8.5")
    "the average of 4.0 3.0 8.5 4.0 is 4.875" shouldEqual process("average 4 3 8.5 4")
    "the minimum of 4.0 -3.0 -17.0 is -17.0" shouldEqual process("min 4 -3 -17")
    "the maximum of 4.0 -3.0 -17.0 is 4.0" shouldEqual process("max 4 -3 -17")
  }
}
