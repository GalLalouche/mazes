package mazes.utils.func

import cats.Monoid

trait Default[A]:
  def empty: A

given monoidDefault[A] (using monoid: Monoid[A]): Default[A] with {
  override def empty: A = monoid.empty
}