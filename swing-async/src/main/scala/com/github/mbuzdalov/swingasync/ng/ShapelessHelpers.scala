package com.github.mbuzdalov.swingasync.ng

import shapeless._

object ShapelessHelpers {
  /* This converts a HList of UpdatableValue's into an Option of HList of their contained types */

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

  /* This converts a HList of UpdatableValue's into a HList of their contained types, assuming all are Done */

  trait UpdatableValueDoneExtractor[U <: HList, W <: HList] {
    def unwrap(wrapped: W): U
    def addListener(wrapped: W, listener: UpdatableValue.Listener[Any]): Unit
  }

  object UpdatableValueDoneExtractor {
    implicit val empty: UpdatableValueDoneExtractor[HNil, HNil] = new UpdatableValueDoneExtractor[HNil, HNil] {
      override def unwrap(wrapped: HNil): HNil = HNil
      override def addListener(wrapped: HNil, listener: UpdatableValue.Listener[Any]): Unit = {}
    }

    implicit def nonEmpty[H, TU <: HList, TW <: HList]
      (implicit tail: UpdatableValueDoneExtractor[TU, TW]): UpdatableValueDoneExtractor[H :: TU, UpdatableValue[H] :: TW] =
    new UpdatableValueDoneExtractor[H :: TU, UpdatableValue[H] :: TW] {

      override def unwrap(wrapped: UpdatableValue[H] :: TW): H :: TU = wrapped.head.state match {
        case UpdatableValue.Done(value) => value :: tail.unwrap(wrapped.tail)
        case _ => throw new IllegalStateException("A required UpdatableValue is not in the Done state")
      }

      override def addListener(wrapped: UpdatableValue[H] :: TW, listener: UpdatableValue.Listener[Any]): Unit = {
        wrapped.head.addListener(listener)
        tail.addListener(wrapped.tail, listener)
      }
    }
  }
}
