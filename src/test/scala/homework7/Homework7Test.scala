package homework7

import java.time.YearMonth

import cats.syntax.all._
import homework7.Homework7._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks


class Homework7Test extends AnyFlatSpec with Matchers
  with ScalaCheckDrivenPropertyChecks {

  "PaymentCardValidator" should "handle valid and invalid payment cards" in {
    import ValidationError._

    PaymentCardValidator.validate("Aleksandrs Kozirevs", "4567 0098 6543 2111",
      "07/22", "123") shouldBe PaymentCard("Aleksandrs Kozirevs",
      "4567009865432111", YearMonth.parse("2022-07"), "123").validNec

    PaymentCardValidator.validate("Aleksandrs de Kozirevs", "4567009865432111",
      "07/22", "123") shouldBe PaymentCard("Aleksandrs de Kozirevs",
      "4567009865432111", YearMonth.parse("2022-07"), "123").validNec

    PaymentCardValidator.validate("Aleksandrs", " 4 5670098 654321 11   ",
      "07/22", "123") shouldBe PaymentCard("Aleksandrs",
      "4567009865432111", YearMonth.parse("2022-07"), "123").validNec

    def checkInvalid(name: String, number: String, expirationDate: String, securityCode: String,
                     errors: Set[ValidationError]): Assertion = PaymentCardValidator
      .validate(name, number, expirationDate, securityCode).leftMap(_.toList.toSet) shouldBe errors.invalid

    checkInvalid(
      "Alek",
      "4567 0098 6543 2111",
      "07/22",
      "123",
      Set(NameAndSurnameLengthIsInvalid))

    checkInvalid(
      "Ale!",
      "4567 0098 6543 2111",
      "07/22",
      "123",
      Set(NameAndSurnameLengthIsInvalid, NameAndSurnameHaveSpecialCharacters))

    checkInvalid(
      "Ale$",
      "4567 0098 6543 21o1",
      "07/22",
      "123",
      Set(NameAndSurnameLengthIsInvalid, NameAndSurnameHaveSpecialCharacters, NumberHasNotOnlyDigits))

    checkInvalid(
      "Aleksandrs&",
      "4567 0098 6543 2111 1",
      "07/22",
      "123",
      Set(NameAndSurnameHaveSpecialCharacters, NumberLengthIsInvalid))

    checkInvalid(
      "Aleksandrs@",
      "4567 0098 6543 211",
      "07/2022",
      "123",
      Set(NameAndSurnameHaveSpecialCharacters, NumberLengthIsInvalid, DateFormatIsInvalid))

    checkInvalid(
      "Aleksandrs#",
      "4567 0098 6543 2111 123",
      "09/20",
      "123",
      Set(NameAndSurnameHaveSpecialCharacters, NumberLengthIsInvalid, ExpirationDateIsInvalid))

    checkInvalid(
      "Aleksandrs Kozirevs",
      "4567 0098 6543 2111 00",
      "12/25",
      "123a",
      Set(NumberLengthIsInvalid, ExpirationDateIsInvalid, SecurityCodeIsInvalid))

    checkInvalid(
      "Aleksandrs Kozirevs",
      "4567 0098 6543 2111 00",
      "12/25",
      "12345",
      Set(NumberLengthIsInvalid, ExpirationDateIsInvalid, SecurityCodeIsInvalid))
  }
}
