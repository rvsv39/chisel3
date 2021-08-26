// SPDX-License-Identifier: Apache-2.0

package chiselTests

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage, DesignAnnotation}
import firrtl.AnnotationSeq
import firrtl.annotations.TargetToken.Ref
import org.scalatest.matchers.should.Matchers
import firrtl.annotations.{CircuitTarget, CompleteTarget}

class DataMirrorSpec extends ChiselFlatSpec with Matchers {
  class Bundle0 extends Bundle {
    val a = UInt(8.W)
    val b = Bool()
    val c = Enum0.Type
  }

  class Bundle1 extends Bundle {
    val a = new Bundle0
    val b = Vec(4, Vec(4, Bool()))
  }

  class Module0 extends Module {
    val i = IO(Input(new Bundle1))
    val o = IO(Output(new Bundle1))
    val r = Reg(new Bundle1)
    chisel3.experimental.DataMirror.trace.traceName(r)
    chisel3.experimental.DataMirror.trace.traceName(i)
    o := r
    r := i
    dontTouch(r)
    dontTouch(o)
    dontTouch(i)
  }

  class Module1 extends Module {
    val m0 = Module(new Module0)
    m0.i := DontCare
  }

  object Enum0 extends ChiselEnum {
    val s0, s1, s2 = Value
  }

  "TraceFromAnnotations" should "be able to get nested name." in {
    val annos = (new ChiselStage).run(
      Seq(
        ChiselGeneratorAnnotation(() => new Module1)
      )
    )
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module1]
    // out of Builder.
    import chisel3.experimental.DataMirror.trace.TraceFromAnnotations

    val oneTarget = annos.finalTarget(dut.m0.r.a.a)
    val ioTarget = annos.finalTarget(dut.m0.i.b(1)(2))
    oneTarget.head should be (CircuitTarget("Module1").module("Module0").ref("r_a_a"))
    ioTarget.head should be (CircuitTarget("Module1").module("Module0").ref("i_b_1_2"))

    // Below codes doesn't needs to be a FIRRTL Transfom.
    def generateVerilatorConfigFile(data: Seq[Data], annos: AnnotationSeq): String = {
      data.flatMap(annos.finalTarget).toSet.map { target: CompleteTarget =>
        s"""public [-module "${target.moduleOpt.get}"] -var "${target.tokens.map{
          case Ref(r) => r
        }.mkString(".")}" """
      }.mkString("\n")
    }
    println(generateVerilatorConfigFile(Seq(dut.m0.r.a, dut.m0.i.b), annos))
  }

}
