package pl.touk.util.validation

import scalaz._

object Implicits extends ValidationWithWarningsEnrichments {

  implicit def ValidationWithWarningsApplicative[L]: Applicative[({type l[a] = ValidationWithWarnings[L, a]})#l] =
    new Applicative[({type l[a] = ValidationWithWarnings[L, a]})#l] {
      override def map[A, B](fa: ValidationWithWarnings[L, A])(f: A => B) =
        fa map f

      def point[A](a: => A) =
        a.successNelWithoutWarnings

      def ap[A, B](va: => ValidationWithWarnings[L, A])(vfa: => ValidationWithWarnings[L, A => B]) =
        va.flatMap(a => vfa.map(f => f(a)))
    }

}
