package chisel3.experimental
import chisel3.internal.{ChiselException, HasId}
import chisel3.internal.firrtl.Width
import chisel3.{ActualDirection, Aggregate, Data, Element, Module, Record, SpecifiedDirection, Vec, dontTouch}
import firrtl.AnnotationSeq
import firrtl.annotations.{Annotation, CompleteTarget, IsMember, ReferenceTarget, SingleTargetAnnotation, Target}

/** Experimental hardware construction reflection API
  */
object DataMirror {
  def widthOf(target:              Data): Width = target.width
  def specifiedDirectionOf(target: Data): SpecifiedDirection = target.specifiedDirection
  def directionOf(target: Data): ActualDirection = {
    chisel3.internal.requireIsHardware(target, "node requested directionality on")
    target.direction
  }

  /** Check if two Chisel types are the same type.
    * Internally, this is dispatched to each Chisel type's
    * `typeEquivalent` function for each type to determine
    * if the types are intended to be equal.
    *
    * For most types, different parameters should ensure
    * that the types are different.
    * For example, `UInt(8.W)` and `UInt(16.W)` are different.
    * Likewise, Records check that both Records have the same
    * elements with the same types.
    *
    * @param x First Chisel type
    * @param y Second Chisel type
    * @return true if the two Chisel types are equal.
    */
  def checkTypeEquivalence(x: Data, y: Data): Boolean = x.typeEquivalent(y)

  // Returns the top-level module ports
  // TODO: maybe move to something like Driver or DriverUtils, since this is mainly for interacting
  // with compiled artifacts (vs. elaboration-time reflection)?
  def modulePorts(target: BaseModule): Seq[(String, Data)] = target.getChiselPorts

  /** Returns all module ports with underscore-qualified names
    * return includes [[chisel3.Module.clock]] and [[chisel3.Module.reset]]
    */
  def fullModulePorts(target: BaseModule): Seq[(String, Data)] = {
    def getPortNames(name: String, data: Data): Seq[(String, Data)] = Seq(name -> data) ++ (data match {
      case _: Element => Seq()
      case r: Record  => r.elements.toSeq.flatMap { case (eltName, elt) => getPortNames(s"${name}_${eltName}", elt) }
      case v: Vec[_]  => v.zipWithIndex.flatMap { case (elt, index) => getPortNames(s"${name}_${index}", elt) }
    })
    modulePorts(target).flatMap {
      case (name, data) =>
        getPortNames(name, data).toList
    }
  }

  // Internal reflection-style APIs, subject to change and removal whenever.
  object internal {
    def isSynthesizable(target: Data): Boolean = target.isSynthesizable
    // For those odd cases where you need to care about object reference and uniqueness
    def chiselTypeClone[T <: Data](target: Data): T = {
      target.cloneTypeFull.asInstanceOf[T]
    }
  }

  // This API provides a util to trace name.
  object trace {
    case class TraceNameAnnotation[T <: CompleteTarget](target: T, hash: T) extends SingleTargetAnnotation[T] {
      // rename map only update target, hash is to used to match via the view API.
      def duplicate(n: T): Annotation = this.copy(target = n)
    }
    def tap(x: Module): Unit = {
      new ChiselAnnotation { def toFirrtl: Annotation = TraceNameAnnotation(x.toTarget, x.toTarget) }
    }
    def tap(x: Data): Unit = {
      x match {
        case aggregate: Aggregate =>
          annotate(new ChiselAnnotation {
            def toFirrtl: Annotation = TraceNameAnnotation(aggregate.toTarget, aggregate.toTarget)
          })
          aggregate.getElements.foreach(tap)
        case element: Element =>
          annotate(new ChiselAnnotation { def toFirrtl: Annotation = TraceNameAnnotation(element.toTarget, element.toTarget) })
      }
     }
    /** API to view final target of a [[Data]] */
    def view(x: HasId, annos: AnnotationSeq): Seq[Target] =
      annos.collect {
        case TraceNameAnnotation(t, hash) if x.toTarget.toString == hash.toString => t
      }
  }
}
