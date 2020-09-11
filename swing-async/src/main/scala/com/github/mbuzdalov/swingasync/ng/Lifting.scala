package com.github.mbuzdalov.swingasync.ng

import shapeless._
import shapeless.ops.function.{FnFromProduct, FnToProduct}

import com.github.mbuzdalov.swingasync.ng.impl.InSwingFunctionalValue

object Lifting {
  def liftInSwing[F, LF <: HList, R,
                  G, LG <: HList](function: F)
                                 (implicit fun2gen: FnToProduct.Aux[F, LF => R],
                                           extractor: ShapelessHelpers.UpdatableValueDoneExtractor[LF, LG],
                                           gen2fun: FnFromProduct.Aux[LG => UpdatableValue[R], G]): G = {
    val unwrappedFunction = fun2gen(function)
    def internalLift(wArgs: LG) = new InSwingFunctionalValue(wArgs, extractor, unwrappedFunction)
    gen2fun(internalLift)
  }
}
