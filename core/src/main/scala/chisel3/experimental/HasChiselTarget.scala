// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental
import firrtl.annotations.{CompleteTarget, SingleTargetAnnotation}

/** An Annotation that contains original */
trait HasChiselTarget[T <: CompleteTarget] { x: SingleTargetAnnotation[T] =>
  val chiselTarget: T
}
