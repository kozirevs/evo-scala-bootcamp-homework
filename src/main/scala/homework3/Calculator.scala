package homework3

import homework3.Calculator.Command.{Average, Divide, Max, Min, Sum}
import homework3.Calculator.Result.{AverageResult, DivideResult, MaxResult, MinResult, SumResult}

import scala.io.Source

object Calculator {

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result
  object Result {
    final case class DivideResult(dividend: Double, divisor: Double, value: Double) extends Result
    final case class SumResult(numbers: List[Double], value: Double) extends Result
    final case class AverageResult(numbers: List[Double], value: Double) extends Result
    final case class MinResult(numbers: List[Double], value: Double) extends Result
    final case class MaxResult(numbers: List[Double], value: Double) extends Result
    final case class ChangeMe(value: String) extends Result // adjust Result as required to match requirements
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val input = x.split(" ").toList.filterNot(_.isEmpty)
    input match {
      case x if x.size < 2 => Left(ErrorMessage("Not enough parameters"))
      case _ :: x if x.map(_.toDoubleOption).contains(None) => Left(ErrorMessage("Couldn't parse"))
      case x :: xs if x == "divide" => xs match {
        case _ if xs.size != 2 => Left(ErrorMessage("Parameters should be two"))
        case _ => Right(Divide(xs.head.toDouble, xs.tail.head.toDouble))
      }
      case x :: xs if x == "sum" => Right(Sum(xs.map(_.toDouble)))
      case x :: xs if x == "average" => Right(Average(xs.map(_.toDouble)))
      case x :: xs if x == "min" => Right(Min(xs.map(_.toDouble)))
      case x :: xs if x == "max" => Right(Max(xs.map(_.toDouble)))
      case _ => Left(ErrorMessage("No such a command"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(dividend, divisor) => (dividend, dividend) match {
        case (_, 0) => Left(ErrorMessage("Division by zero"))
        case _ => Right(DivideResult(dividend, divisor, dividend / divisor))
      }
      case Sum(numbers) => Right(SumResult(numbers, numbers.sum))
      case Average(numbers) => Right(AverageResult(numbers, numbers.sum / numbers.size))
      case Min(numbers) => Right(MinResult(numbers, numbers.min))
      case Max(numbers) => Right(MaxResult(numbers, numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    x match {
      case DivideResult(dividend, divisor, value) => s"$dividend divided by $divisor is ${value.formatted("%.2f")}"
      case SumResult(numbers, value) => s"the sum of ${numbers.mkString(" ")} is $value"
      case AverageResult(numbers, value) => s"the average of ${numbers.mkString(" ")} is $value"
      case MinResult(numbers, value) => s"the min of ${numbers.mkString(" ")} is $value"
      case MaxResult(numbers, value) => s"the max of ${numbers.mkString(" ")} is $value"
    }
  }

  def process(x: String): String = {
    val res: Either[ErrorMessage, Result] = for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield result

    res match {
      case Left(x) => s"Error: ${x.value}"
      case Right(x) => renderResult(x)
    }
  }

  def main(args: Array[String]): Unit = {
    Source.stdin.getLines() map process foreach println
  }
}
