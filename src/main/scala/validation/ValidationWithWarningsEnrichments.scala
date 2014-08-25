package pl.touk.util.validation

import scalaz.Scalaz._
import scalaz._

trait ValidationWithWarningsEnrichments {

  implicit class ValidationNelOps[E, A](validation: ValidationNel[E, A]) {
    def withWarnings(warnings: Seq[E] = Nil): ValidationWithWarnings[E, A] = ValidationWithWarnings(validation, warnings)
    def withWarning(warning: E): ValidationWithWarnings[E, A] = ValidationWithWarnings(validation, Seq(warning))
    def noWarnings: ValidationWithWarnings[E, A] = ValidationWithWarnings(validation, Nil)
  }


  implicit class ValidationOps[E, A](validation: Validation[E, A]) {
    def withWarnings(warnings: Seq[E] = Nil): ValidationWithWarnings[E, A] =
      new ValidationNelOps(validation.toValidationNel).withWarnings(warnings)

    def withWarning(warning: E): ValidationWithWarnings[E, A] =
      new ValidationNelOps(validation.toValidationNel).withWarning(warning)

    def noWarnings: ValidationWithWarnings[E, A] = new ValidationNelOps(validation.toValidationNel).noWarnings
  }

  implicit class ValidationWithWarningsSuccessHelper[A](value: A) {
    def successNelWithoutWarnings[E]: ValidationWithWarnings[E, A] = new ValidationWithWarnings(value.successNel[E], Seq())
  }

  implicit class ValidationWithWarningsFailureNelHelper[E](error: NonEmptyList[E]) {
    def failureWithoutWarnings[A]: ValidationWithWarnings[E, A] = new ValidationWithWarnings(error.failure[A], Seq())
  }

  implicit class ValidationWithWarningsFailureHelper[E](error: E) {
    def failureNelWithoutWarnings[A]: ValidationWithWarnings[E, A] = new ValidationWithWarnings(error.failureNel[A], Seq())
  }

  implicit class SeqOfValidationWithWarnings[E, A](validationSeq: Seq[ValidationWithWarnings[E, A]]) {

    def accumulateValidationsIgnoringValue: ValidationWithWarnings[E, Unit] = accumulateValidations.map(_ => ())

    /**
     * Converts Seq[VWW[E,A]] to VWW[E,[Seq[A]]
     */
    def accumulateValidations: ValidationWithWarnings[E, Seq[A]] = {
      if (validationSeq.isEmpty)
        Seq().successNelWithoutWarnings
      else
        accumulateValidationsNotEmpty
    }

    private def accumulateValidationsNotEmpty: ValidationWithWarnings[E, Seq[A]] = {
      validationSeq.map { singleValidation =>
        val validationOfLists = singleValidation.map(singletonValue => List(singletonValue))
        validationOfLists
      }.reduce(_ ++ _)
    }
  }

}
