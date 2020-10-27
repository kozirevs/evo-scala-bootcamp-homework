package homework7

import java.text.SimpleDateFormat
import java.time.YearMonth
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import cats.data.ValidatedNec
import cats.syntax.all._
import homework7.Homework7.ValidationError._

object Homework7 {

  case class PaymentCard(name: String, number: String, expirationDate: YearMonth, securityCode: String)

  sealed trait ValidationError
  object ValidationError {
    final case object NameAndSurnameLengthIsInvalid extends ValidationError {
      override def toString: String = "Name and surname must be between 5 and 255 characters"
    }
    final case object NameAndSurnameHaveSpecialCharacters extends ValidationError {
      override def toString: String = "Name and surname cannot contain special characters"
    }
    final case object NumberHasNotOnlyDigits extends ValidationError {
      override def toString: String = "Number should contain only digits"
    }
    final case object NumberLengthIsInvalid extends ValidationError {
      override def toString: String = "Number must be 16 digits"
    }
    final case object DateFormatIsInvalid extends ValidationError {
      override def toString: String = "Date must be as in the example(12/20)"
    }
    final case object ExpirationDateIsInvalid extends ValidationError {
      override def toString: String = "Expiration date is expired or more than 5 years from now"
    }
    final case object SecurityCodeIsInvalid extends ValidationError {
      override def toString: String = "Security code must be 3 or 4 digits"
    }
  }

  object PaymentCardValidator {

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateNameAndSurname(nameAndSurname: String): AllErrorsOr[String] = {

      def validateNameAndSurnameLength: AllErrorsOr[String] =
        if (nameAndSurname.length >= 5 && nameAndSurname.length <= 255) nameAndSurname.validNec
        else NameAndSurnameLengthIsInvalid.invalidNec

      def validateNameAndSurnameContents: AllErrorsOr[String] =
        if (nameAndSurname.matches("^[a-zA-Z]+(([' ][a-zA-Z ])?[a-zA-Z]*)*$")) nameAndSurname.validNec
        else NameAndSurnameHaveSpecialCharacters.invalidNec

      validateNameAndSurnameLength.productR(validateNameAndSurnameContents)
    }

    private def validateNumber(number: String): AllErrorsOr[String] = {

      def validateNumberContent: AllErrorsOr[String] = {
        val numberWithoutSpaces = number.replaceAll(" ", "")
        if (numberWithoutSpaces.toLongOption.isDefined) numberWithoutSpaces.validNec
        else NumberHasNotOnlyDigits.invalidNec
      }
      def validateNumberLength(number: String): AllErrorsOr[String] =
        if (number.length == 16) number.validNec
        else NumberLengthIsInvalid.invalidNec

      validateNumberContent.andThen(number => validateNumberLength(number))
    }

    private def validateExpirationDate(expirationDate: String): AllErrorsOr[YearMonth] = {

      def validateDateFormat: AllErrorsOr[YearMonth] = {
        val format = DateTimeFormatter.ofPattern("MM/yy")
        if (expirationDate.matches("^((0[1-9])|(1[0-2]))\\/(\\d{2})$")) {
          YearMonth.parse(expirationDate, format).validNec
        } else DateFormatIsInvalid.invalidNec
      }

      def validateExpiration(date: YearMonth): AllErrorsOr[YearMonth] =
        if (date.isAfter(YearMonth.now()) && date.isBefore(YearMonth.now().plusYears(5)))
          date.validNec
        else ExpirationDateIsInvalid.invalidNec

      validateDateFormat.andThen(date => validateExpiration(date))
    }

    private def validateSecurityCode(securityCode: String): AllErrorsOr[String] = {
      if (securityCode.matches("^(\\d{3,4})$")) securityCode.validNec
      else SecurityCodeIsInvalid.invalidNec
    }

    def validate(name: String, number: String, expirationDate: String,
                 securityCode: String): AllErrorsOr[PaymentCard] = (validateNameAndSurname(name),
      validateNumber(number), validateExpirationDate(expirationDate), validateSecurityCode(securityCode))
      .mapN(PaymentCard)
  }
}
