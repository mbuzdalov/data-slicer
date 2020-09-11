package com.github.mbuzdalov.swingasync.ng

import shapeless._

import com.github.mbuzdalov.swingasync.ng.ShapelessHelpers._

class ShapelessHelpersTests extends CommonTesting {

  "DoneOrNoneFolder" should "map Option(HNil) to itself" in {
    val input: HNil = HNil
    val actual: Option[HNil] = input.foldRight(Option(HNil))(DoneOrNoneFold)
    actual shouldEqual Some(HNil)
  }

  it should "map Some(HNil) to itself" in {
    val input: HNil = HNil
    val actual: Option[HNil] = input.foldRight(Some(HNil))(DoneOrNoneFold)
    actual shouldEqual Some(HNil)
  }

  it should "map None to itself" in {
    val input: HNil = HNil
    val actual: Option[HNil] = input.foldRight(None)(DoneOrNoneFold)
    actual shouldEqual None
    // it shall infer the None.type statically
    val actual2: None.type = input.foldRight(None)(DoneOrNoneFold)
    actual2 shouldEqual None
  }

  it should "map the HList of one UpdatableValue if it is done" in {
    val input = done(1) :: HNil
    val actual: Option[Int :: HNil] = input.foldRight(Option(HNil))(DoneOrNoneFold)
    actual shouldEqual Some(1 :: HNil)
  }

  it should "map a fragile call to the HList of one UpdatableValue if it is done" in {
    val input = done(1) :: HNil
    val actual: Option[Int :: HNil] = input.foldRight(Some(HNil))(DoneOrNoneFold)
    actual shouldEqual Some(1 :: HNil)
  }

  it should "map the HList of one UpdatableValue to None if it is not done" in {
    val input = waiting[Int] :: HNil
    val actual: Option[Int :: HNil] = input.foldRight(Option(HNil))(DoneOrNoneFold)
    actual shouldEqual None
  }

  it should "map a fragile call to the HList with None as a fold base, resulting in None" in {
    val input = done(1) :: HNil
    val actual: Option[Int :: HNil] = input.foldRight(None)(DoneOrNoneFold)
    actual shouldEqual None
    // it shall infer the None.type statically
    val actual2: None.type = input.foldRight(None)(DoneOrNoneFold)
    actual2 shouldEqual None
  }

  it should "map the HList of three UpdatableValues if all of them are done" in {
    val input = done(1) :: done("2") :: done(3.0) :: HNil
    val actual: Option[Int ::String :: Double :: HNil] = input.foldRight(Option(HNil))(DoneOrNoneFold)
    actual shouldEqual Some(1 :: "2" :: 3.0 :: HNil)
  }

  it should "map the HList of three UpdatableValues to None if the first is not done" in {
    val input = waiting[Int] :: done("2") :: done(3.0) :: HNil
    val actual: Option[Int ::String :: Double :: HNil] = input.foldRight(Option(HNil))(DoneOrNoneFold)
    actual shouldEqual None
  }

  it should "map the HList of three UpdatableValues to None if the second is not done" in {
    val input = done(1) :: waiting[String] :: done(3.0) :: HNil
    val actual: Option[Int ::String :: Double :: HNil] = input.foldRight(Option(HNil))(DoneOrNoneFold)
    actual shouldEqual None
  }

  it should "map the HList of three UpdatableValues to None if the last is not done" in {
    val input = done(1) :: done("2") :: waiting[Double] :: HNil
    val actual: Option[Int ::String :: Double :: HNil] = input.foldRight(Option(HNil))(DoneOrNoneFold)
    actual shouldEqual None
  }

  it should "map a fragile call to the HList of three UpdatableValues if all of them are done" in {
    val input = done(1) :: done("2") :: done(3.0) :: HNil
    val actual: Option[Int ::String :: Double :: HNil] = input.foldRight(Some(HNil))(DoneOrNoneFold)
    actual shouldEqual Some(1 :: "2" :: 3.0 :: HNil)
  }

  it should "map a fragile call to the HList of three UpdatableValues with None as a fold base, resulting in None even if all of them are done" in {
    val input = done(1) :: done("2") :: done(3.0) :: HNil
    val actual: Option[Int ::String :: Double :: HNil] = input.foldRight(None)(DoneOrNoneFold)
    actual shouldEqual None
    // it shall infer the None.type statically
    val actual2: None.type = input.foldRight(None)(DoneOrNoneFold)
    actual2 shouldEqual None
  }
}
