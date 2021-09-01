// SPDX-License-Identifier: Apache-2.0

package chiselTests

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.DataMirror.trace.traceName
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage, DesignAnnotation}
import chisel3.util.experimental.InlineInstance
import firrtl.AnnotationSeq
import firrtl.analyses.InstanceKeyGraph
import firrtl.annotations.TargetToken.{Instance, OfModule, Ref}
import firrtl.annotations.{CircuitTarget, CompleteTarget, ReferenceTarget}
import firrtl.stage.FirrtlCircuitAnnotation
import org.scalatest.matchers.should.Matchers

import scala.annotation.nowarn

class DataMirrorSpec extends ChiselFlatSpec with Matchers {

  "TraceFromAnnotations" should "be able to get nested name." in {
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
      val testDir = os.Path(createTestDirectory("TraceFromAnnotations").getAbsolutePath)
      val annos = (new ChiselStage).execute(
        Array("--target-dir", s"$testDir"),
        Seq(
          ChiselGeneratorAnnotation(() => new Module1)
        )
      )
      val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module1]
      // out of Builder.
      import chisel3.experimental.DataMirror.trace.TraceFromAnnotations

      val oneTarget = annos.finalTarget(dut.m0.r.a.a)
      val ioTarget = annos.finalTarget(dut.m0.i.b(1)(2))

      val topName = "Module1"
      oneTarget.head should be(
        ReferenceTarget(
          topName,
          topName,
          List((Instance("m0"), OfModule("Module0"))),
          "r_a_a",
          List()
        ))

      ioTarget.head should be(
        ReferenceTarget(
          topName,
          topName,
          List((Instance("m0"), OfModule("Module0"))),
          "i_b_1_2",
          List()
        )
      )

      // Below codes doesn't needs to be a FIRRTL Transform.
      def generateVerilatorConfigFile(data: Seq[Data], annos: AnnotationSeq): String =
        """`verilator_config
          |lint_off -rule unused
          |lint_off -rule declfilename
          |""".stripMargin +
          data
            .flatMap(annos.finalTarget)
            .toSet
            .map { target: CompleteTarget =>
              s"""public_flat_rd -module "${target.tokens.collectFirst { case OfModule(m) => m }.get}" -var "${target.tokens.collectFirst { case Ref(r) => r }.get}""""
            }
            .mkString("\n") + "\n"

      def verilatorTemplate(data: Seq[Data], annos: AnnotationSeq): String = {
        val vpiNames = data.flatMap(annos.finalTarget).map { ct =>
          s"""TOP.${ct.circuit}.${ct.path.map { case (Instance(i), _) => i }.mkString(".")}.${ct.tokens.collectFirst { case Ref(r) => r }.get}"""
        }
        s"""
           |#include "V${topName}.h"
           |#include "verilated_vpi.h"
           |#include <memory>
           |#include <verilated.h>
           |
           |int vpiGetInt(const char name[]) {
           |  // TODO Check name here.
           |  vpiHandle vh1 = vpi_handle_by_name((PLI_BYTE8 *)name, NULL);
           |  if (!vh1)
           |    vl_fatal(__FILE__, __LINE__, "sim_main", "No handle found");
           |  s_vpi_value v;
           |  v.format = vpiIntVal;
           |  vpi_get_value(vh1, &v);
           |  return v.value.integer;
           |}
           |
           |int main(int argc, char **argv) {
           |  const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
           |  contextp->commandArgs(argc, argv);
           |  const std::unique_ptr<V$topName> top{new V$topName{contextp.get(), "TOP"}};
           |  top->reset = 0;
           |  top->clock = 0;
           |  int a_b = 1;
           |  top->i_a_b = a_b;
           |  bool started = false;
           |  int ticks = 20;
           |  while (ticks--) {
           |    contextp->timeInc(1);
           |    top->clock = !top->clock;
           |    if (!top->clock) {
           |      if (contextp->time() > 1 && contextp->time() < 10) {
           |        top->reset = 1;
           |      } else {
           |        top->reset = 0;
           |        started = true;
           |      }
           |      a_b = a_b ? 0 : 1;
           |      top->i_a_b = a_b;
           |    }
           |    top->eval();
           |    VerilatedVpi::callValueCbs();
           |    if (started && !top->clock) {
           |      const int i = top->i_a_b;
           |      const int o = vpiGetInt("${vpiNames.head}");
           |      if (i == o)
           |        vl_fatal(__FILE__, __LINE__, "sim_main", "${vpiNames.head} should be the old value of Module1.i_a_b");
           |      printf("${vpiNames.head}=%d Module1.m0.o_a_b=%d\\n", i, o);
           |    }
           |  }
           |  top->final();
           |  return 0;
           |}
           |""".stripMargin
      }

      val config = os.temp(dir = testDir, contents = generateVerilatorConfigFile(Seq(dut.m0.o.a.b), annos))
      val verilog = testDir / s"$topName.v"
      val cpp = os.temp(dir = testDir, suffix = ".cpp", contents = verilatorTemplate(Seq(dut.m0.o.a.b), annos))
      val exe = testDir / "obj_dir" / s"V$topName"
      os.proc("verilator", "-Wall", "--cc", "--exe", "--build", "--vpi", s"$cpp", s"$verilog", s"$config").call(stdout = os.Inherit, stderr = os.Inherit, cwd = testDir)
      assert(os.proc(s"$exe").call(stdout = os.Inherit, stderr = os.Inherit).exitCode == 0, "verilator should exit peacefully")
    }

  "TraceFromCollideBundle" should "work" in {
    class CollideModule extends Module {
      val a = IO(Input(Vec(2, new Bundle {
        val b = Flipped(Bool())
        val c = Vec(2, new Bundle {
          val d = UInt(2.W)
          val e = Flipped(UInt(3.W))
        })
        val c_1_e = UInt(4.W)
      })))
      traceName(a)
      val a_0_c = IO(Output(UInt(5.W)))
      traceName(a_0_c)
      val a__0 = IO(Output(UInt(5.W)))
      dontTouch(a)
      dontTouch(a_0_c)
      dontTouch(a_0_c)
      a_0_c := DontCare
      a__0 := DontCare
    }
    val testDir = os.Path(createTestDirectory("TraceFromCollideBundle").getAbsolutePath)
    val annos = (new ChiselStage).execute(
      Array("--target-dir", s"$testDir"),
      Seq(
        ChiselGeneratorAnnotation(() => new CollideModule)
      )
    )
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[CollideModule]
    // out of Builder.
    import chisel3.experimental.DataMirror.trace.TraceFromAnnotations

    val t0 = annos.finalTarget(dut.a(0).c)
    val t1 = annos.finalTarget(dut.a_0_c)
    val t2 = annos.finalTarget(dut.a(0).c(1).e)
    val t3 = annos.finalTarget(dut.a(0).c_1_e)
  }

  "Inline should work" should "work" in {
    class Module0 extends Module {
      val i = IO(Input(Bool()))
      val o = IO(Output(Bool()))
      traceName(i)
      o := !i
    }

    class Module1 extends Module {
      val i = IO(Input(Bool()))
      val o = IO(Output(Bool()))
      val m0 = Module(new Module0 with InlineInstance)
      m0.i := i
      o := m0.o
    }

    val testDir = os.Path(createTestDirectory("Inline").getAbsolutePath)
    val annos = (new ChiselStage).execute(
      Array("--target-dir", s"$testDir"),
      Seq(
        ChiselGeneratorAnnotation(() => new Module1)
      )
    )
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module1]
    // out of Builder.
    import chisel3.experimental.DataMirror.trace.TraceFromAnnotations

    val t0 = annos.finalTarget(dut.m0.i)
  }

  "Constant Propagation" should "be crazy" in {
    class Module0 extends Module {
      val i = WireDefault(1.U)
      val i0 = i + 1.U
      val o = IO(Output(UInt(2.W)))
      traceName(i0)
      o := i0
    }

    val testDir = os.Path(createTestDirectory("ConstantProp").getAbsolutePath)
    val annos = (new ChiselStage).execute(
      Array("--target-dir", s"$testDir"),
      Seq(
        ChiselGeneratorAnnotation(() => new Module0)
      )
    )
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module0]
    // out of Builder.
    import chisel3.experimental.DataMirror.trace.TraceFromAnnotations

    val t0 = annos.finalTarget(dut.i0)
    println(t0)
  }

}
