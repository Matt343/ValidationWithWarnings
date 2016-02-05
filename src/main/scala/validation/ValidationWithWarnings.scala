package pl.touk.util.validation

import scalaz.Scalaz._
import scalaz._

case class ValidationWithWarnings[E, +A] private[validation](private val validation: ValidationNel[E, A],
                                                              warnings: Seq[E]) {
  import pl.touk.util.validation.Implicits._

  def flatMap[U](function: A => ValidationWithWarnings[E, U]): ValidationWithWarnings[E, U] = {
    validation match {
      case Success(value) =>
        val newResult = function(value)
        new ValidationWithWarnings[E, U](newResult.validation, warnings ++ newResult.warnings)
      case Failure(errors) => new ValidationWithWarnings[E, U](errors.failure[U], warnings)
    }
  }

  def map[U](function: A => U): ValidationWithWarnings[E, U] = {
    val newValidation = validation.map(function)
    ValidationWithWarnings(newValidation, warnings)
  }

  def foreach[U](function: A => U): Unit = {
    validation.foreach(function)
  }

  def exists(f: A => Boolean): Boolean = validation.exists(f)

  def mapWarnings(function: E => E) = {
    new ValidationWithWarnings(validation, warnings.map(function))
  }

  def mapErrors(function: E => E): ValidationWithWarnings[E, A] = {
    new ValidationWithWarnings(validation.leftMap(_.map(function)), warnings)
  }

  def mapErrorsAndWarnings[F](function: E => F): ValidationWithWarnings[F, A] = {
    new ValidationWithWarnings(validation.leftMap(_.map(function)), warnings.map(function))
  }
  
  def warningToErrors: ValidationWithWarnings[E, A] = {
    val warningsNelOpt = toNel(warnings.toList)
    (warningsNelOpt, validation) match {
      case (Some(warningsNel), Failure(errors)) => errors.append(warningsNel).failureWithoutWarnings
      case (Some(warningsNel), Success(_))      => warningsNel.failureWithoutWarnings
      case (None,              v)               => v.withWarnings()
    }
  }

  def ++[AA >: A : Semigroup](that: ValidationWithWarnings[E, AA]) = {
    new ValidationWithWarnings(validation +++ that.validation, warnings ++ that.warnings)
  }

  def toOption: Option[A] = validation.toOption

  def valueOr[AA >: A](function: NonEmptyList[E] => AA): AA = validation.valueOr(function)

  def errors: Seq[E] = validation match {
    case Failure(errors) => errors.list.toList
    case Success(_) => Seq()
  }

  def hasWarnings = warnings.nonEmpty
  
  def isSuccess = validation.isSuccess

  def isFailure = validation.isFailure

}
