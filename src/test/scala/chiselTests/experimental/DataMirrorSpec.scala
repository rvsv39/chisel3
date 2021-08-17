// SPDX-License-Identifier: Apache-2.0

package chiselTests

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage, DesignAnnotation}
import org.scalatest.matchers.should.Matchers

class DataMirrorSpec extends ChiselFlatSpec with Matchers {
  class Bundle0 extends Bundle {
    val a = UInt(8.W)
    val b = Bool()
  }

  class Bundle1 extends Bundle {
    val a = new Bundle0
    val b = Vec(4, Vec(4, Bool()))
  }

  class Module0 extends Module {
    val i = IO(Input(new Bundle1))
    val o = IO(Output(new Bundle1))
    val r = Reg(new Bundle1)
    chisel3.experimental.DataMirror.trace.tap(r)
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

  "tap" should "be able to get nested name." in {
    val annos = (new ChiselStage).run(
      Seq(
        ChiselGeneratorAnnotation(() => new Module1)
      )
    )
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module1]
    // out of Build.
    val rTarget = chisel3.experimental.DataMirror.trace.view(dut.m0.r.a.a, annos)
    rTarget.toString should be ("~Module1|Module0>r_a_a")
  }

}
