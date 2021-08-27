// SPDX-License-Identifier: Apache-2.0

package chiselTests

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage, DesignAnnotation}
import firrtl.AnnotationSeq
import firrtl.annotations.TargetToken.Ref
import org.scalatest.matchers.should.Matchers
import firrtl.annotations.{CircuitTarget, CompleteTarget}

import scala.annotation.nowarn

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
    chisel3.experimental.DataMirror.trace.traceName(o)
    o := r
    r := i
    dontTouch(r)
    dontTouch(o)
    dontTouch(i)
  }

  class Module1 extends Module {
    val i = IO(Input(new Bundle1))
    val m0 = Module(new Module0)
    m0.i := i
    m0.o := DontCare
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
    @nowarn("msg=match may not be exhaustive")
    def generateVerilatorConfigFile(data: Seq[Data], annos: AnnotationSeq): String =
      """`verilator_config
        |lint_off -rule unused
        |lint_off -rule declfilename
        |""".stripMargin +
        data.flatMap(annos.finalTarget).toSet.map { target: CompleteTarget =>
          s"""public_flat_rd -module "${target.moduleOpt.get}" -var "${target.tokens.map { case Ref(r) => r }.mkString(".")}""""
        }.mkString("\n") + "\n"

    val config = os.pwd / "verilator_config"
    val verilog = os.pwd / "Module1.v"
    val cpp = os.pwd / "sim_main.cpp"
    val exe = os.pwd / "obj_dir" / "VModule1"
    os.write.over(config, generateVerilatorConfigFile(Seq(dut.m0.o.a.b), annos))
    os.proc("verilator", "-Wall", "--cc", "--exe", "--build", "--vpi", s"$cpp", s"$verilog", s"$config").call(stdout = os.Inherit, stderr = os.Inherit)
    assert(os.proc(s"$exe").call(stdout = os.Inherit, stderr = os.Inherit).exitCode == 0, "verilator should exit peacefully")
  }

}
