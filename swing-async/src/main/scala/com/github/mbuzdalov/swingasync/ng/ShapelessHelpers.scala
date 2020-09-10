package com.github.mbuzdalov.swingasync.ng

import shapeless._

object ShapelessHelpers {
  object DoneOrNoneFold extends Poly2 {
    // This is for the case when the type of the tail call is not known at compile-time.
    implicit def foldOption[H, T <: HList]: Case.Aux[UpdatableValue[H], Option[T], Option[H :: T]] =
      at((h, t) => t match {
        case None => None
        case Some(tail) => h.state match {
          case UpdatableValue.Done(value) => Some(value :: tail)
          case _ => None
        }
      })

    // This is for the case when the type of the tail call is known to be Some(t) at compile-time.
    implicit def foldSome[H, T <: HList]: Case.Aux[UpdatableValue[H], Some[T], Option[H :: T]] =
      at((h, t) => h.state match {
        case UpdatableValue.Done(value) => Some(value :: t.get)
        case _ => None
      })

    // This is for the case when the type of the tail call is known to be None at compile-time.
    implicit def foldNone[H]: Case.Aux[UpdatableValue[H], None.type, None.type] = at((_, _) => None)
  }
}
