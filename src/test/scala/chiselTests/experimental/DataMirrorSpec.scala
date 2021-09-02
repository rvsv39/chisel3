// SPDX-License-Identifier: Apache-2.0

package chiselTests

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.DataMirror.trace.{traceName, TraceFromAnnotations}
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage, DesignAnnotation}
import chisel3.util.experimental.InlineInstance
import firrtl.AnnotationSeq
import firrtl.annotations.TargetToken.{Instance, OfModule, Ref}
import firrtl.annotations.{CompleteTarget, ReferenceTarget}
import org.scalatest.matchers.should.Matchers

class DataMirrorSpec extends ChiselFlatSpec with Matchers {

  def target(topName: String, ref: String, path: Seq[(Instance, OfModule)] = Seq()) =
    ReferenceTarget(topName, topName, path, ref, Seq())

  def compile(testName: String, gen: () => Module): (os.Path, AnnotationSeq) = {
    val testDir = os.Path(createTestDirectory(testName).getAbsolutePath)
    val annos = (new ChiselStage).execute(
      Array("--target-dir", s"$testDir"),
      Seq(
        ChiselGeneratorAnnotation(gen)
      )
    )
    (testDir, annos)
  }

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
      o := r
      r := i

      traceName(r)
      traceName(i)
      traceName(o)
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

    val (testDir, annos) = compile("TraceFromAnnotaions", () => new Module1)
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module1]
    // out of Builder.

    val oneTarget = annos.finalTarget(dut.m0.r.a.a).head
    val ioTarget = annos.finalTarget(dut.m0.i.b(1)(2)).head

    val topName = "Module1"
    oneTarget should be(target(topName, "r_a_a", Seq(Instance("m0") -> OfModule("Module0"))))

    ioTarget should be(target(topName, "i_b_1_2", Seq(Instance("m0") -> OfModule("Module0"))))

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
      val a_0_c = IO(Output(UInt(5.W)))
      val a__0 = IO(Output(UInt(5.W)))
      a_0_c := DontCare
      a__0 := DontCare

      traceName(a)
      traceName(a_0_c)
      traceName(a__0)
    }

    val (_, annos) = compile("TraceFromCollideBundle", () => new CollideModule)
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[CollideModule]

    val topName = "CollideModule"

    val a0 = annos.finalTarget(dut.a(0))
    val a__0 = annos.finalTarget(dut.a__0).head
    val a__0_ref = target(topName, "a__0")
    a0.foreach(_ shouldNot be(a__0_ref))
    a__0 should be(a__0_ref)

    val a0_c = annos.finalTarget(dut.a(0).c)
    val a_0_c = annos.finalTarget(dut.a_0_c).head
    val a_0_c_ref = target(topName, "a_0_c")
    a0_c.foreach(_ shouldNot be(a_0_c_ref))
    a_0_c should be(a_0_c_ref)

    val a0_c1_e = annos.finalTarget(dut.a(0).c(1).e).head
    val a0_c_1_e = annos.finalTarget(dut.a(0).c_1_e).head
    a0_c1_e should be(target(topName, "a_0_c__1_e"))
    a0_c_1_e should be(target(topName, "a_0_c_1_e"))
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

    val (_, annos) = compile("Inline", () => new Module1)
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module1]

    val m0_i = annos.finalTarget(dut.m0.i).head
    m0_i should be(target("Module1", "m0_i"))
  }

  "Constant Propagation" should "be turned off by traceName" in {
    class Module0 extends Module {
      val i = WireDefault(1.U)
      val i0 = i + 1.U
      val o = IO(Output(UInt(2.W)))
      traceName(i0)
      o := i0
    }

    val (_, annos) = compile("ConstantProp", () => new Module0)
    val dut = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[Module0]

    val i0 = annos.finalTarget(dut.i0).head
    i0 should be(target("Module0", "i0"))
  }

  "Nested Module" should "work" in {
    class Io extends Bundle {
      val i = Input(Bool())
      val o = Output(Bool())
    }

    class Not extends Module {
      val io = IO(new Io)
      io.o := !io.i

      traceName(io.o)
    }

    class M1 extends Module {
      val io = IO(new Io)
      val not = Module(new Not)
      not.io <> io
    }

    class M2 extends Module {
      val io = IO(new Io)
      val m1 = Module(new M1 with InlineInstance)
      val not = Module(new Not)

      m1.io.i := io.i
      not.io.i := io.i

      io.o := m1.io.o && not.io.o
    }

    class M3 extends Module {
      val io = IO(new Io)
      val m2 = Module(new M2)
      io <> m2.io
    }

    val (_, annos) = compile("NestedModule", () => new M3)
    val m3 = annos.collectFirst { case DesignAnnotation(dut) => dut }.get.asInstanceOf[M3]

    val inlined = annos.finalTarget(m3.m2.m1.not.io.o).head
    val normal = annos.finalTarget(m3.m2.not.io.o).head

    inlined should be(target("M3", "io_o", Seq(Instance("m2") -> OfModule("M2"), Instance("m1_not") -> OfModule("Not"))))
    normal should be(target("M3", "io_o", Seq(Instance("m2") -> OfModule("M2"), Instance("not") -> OfModule("Not"))))
  }
}
