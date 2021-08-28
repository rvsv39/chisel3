// SPDX-License-Identifier: Apache-2.0

package chiselTests

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage, DesignAnnotation}
import firrtl.AnnotationSeq
import firrtl.analyses.InstanceKeyGraph
import firrtl.annotations.TargetToken.{Instance, OfModule, Ref}
import firrtl.annotations.{CircuitTarget, CompleteTarget, ReferenceTarget}
import firrtl.stage.FirrtlCircuitAnnotation
import org.scalatest.matchers.should.Matchers

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

    oneTarget.head should be(
      ReferenceTarget(
      "Module1",
      "Module1",
      List((Instance("m0"), OfModule("Module0"))),
      "r_a_a",
      List()
    ))
    pprint.pprintln(ioTarget.head)

    ioTarget.head should be(
      ReferenceTarget(
        "Module1",
        "Module1",
        List((Instance("m0"), OfModule("Module0"))),
        "i_b_1_2",
        List()
      )
    )

    val symbolTable = annos.finalTargetMap
    // Below codes doesn't needs to be a FIRRTL Transform.
    @nowarn("msg=match may not be exhaustive")
    def generateVerilatorConfigFile(data: Seq[Data], annos: AnnotationSeq): String =
      """`verilator_config
        |lint_off -rule unused
        |lint_off -rule declfilename
        |""".stripMargin +
        data
          .flatMap(annos.finalTarget)
          .toSet
          .map { target: CompleteTarget =>
            s"""public_flat_rd -module "${target.moduleOpt.get}" -var "${target.tokens.map { case Ref(r) => r }
              .mkString(".")}""""
          }
          .mkString("\n") + "\n"

    def verilatorTemplate(data: Seq[Data], annos: AnnotationSeq): String = {
      symbolTable.map(_._2.path).foreach(println)
      """
        |#include "VModule1.h"
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
        |int main(int argc, char **argv) {
        |  const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
        |  contextp->commandArgs(argc, argv);
        |  const std::unique_ptr<VModule1> top{new VModule1{contextp.get(), "TOP"}};
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
        |      const int o = vpiGetInt("TOP.Module1.m0.o_a_b");
        |      if (i == o)
        |        goto bad_end;
        |      printf("Module1.i_a_b=%d Module1.m0.o_a_b=%d\n", i, o);
        |    }
        |  }
        |  top->final();
        |  return 0;
        |bad_end:
        |  puts("Module1.m0.o_a_b should be the old value of Module1.i_a_b");
        |  top->final();
        |  return 1;
        |}
        |
        |
        |""".stripMargin
    }

    val config = os.temp()
    val verilog = os.pwd / "Module1.v"
    val cpp = os.temp(suffix = ".cpp")
    val exe = os.pwd / "obj_dir" / "VModule1"
    os.write.over(config, generateVerilatorConfigFile(Seq(dut.m0.o.a.b), annos))
    os.write.over(cpp, verilatorTemplate(Seq(dut.m0.o.a.b), annos))
    os.proc("verilator", "-Wall", "--cc", "--exe", "--build", "--vpi", s"$cpp", s"$verilog", s"$config").call(stdout = os.Inherit, stderr = os.Inherit)
    assert(os.proc(s"$exe").call(stdout = os.Inherit, stderr = os.Inherit).exitCode == 0, "verilator should exit peacefully")
  }

}
