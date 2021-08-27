#include "VModule1.h"
#include "verilated_vpi.h"
#include <memory>
#include <verilated.h>

int vpiGetInt(const char name[]) {
  vpiHandle vh1 = vpi_handle_by_name((PLI_BYTE8 *)name, NULL);
  if (!vh1)
    vl_fatal(__FILE__, __LINE__, "sim_main", "No handle found");

  s_vpi_value v;
  v.format = vpiIntVal;
  vpi_get_value(vh1, &v);
  return v.value.integer;
}

int main(int argc, char **argv) {
  const std::unique_ptr<VerilatedContext> contextp{new VerilatedContext};
  contextp->commandArgs(argc, argv);

  const std::unique_ptr<VModule1> top{new VModule1{contextp.get(), "TOP"}};

  top->reset = 0;
  top->clock = 0;

  int a_b = 1;
  top->i_a_b = a_b;

  bool started = false;
  int ticks = 20;
  while (ticks--) {
    contextp->timeInc(1);
    top->clock = !top->clock;

    if (!top->clock) {
      if (contextp->time() > 1 && contextp->time() < 10) {
        top->reset = 1;
      } else {
        top->reset = 0;
        started = true;
      }
      a_b = a_b ? 0 : 1;
      top->i_a_b = a_b;
    }
    top->eval();
    VerilatedVpi::callValueCbs();
    if (started && !top->clock) {
      const int i = top->i_a_b;
      const int o = vpiGetInt("TOP.Module1.m0.o_a_b");

      if (i == o)
        goto bad_end;
      printf("Module1.i_a_b=%d Module1.m0.o_a_b=%d\n", i, o);
    }
  }
  top->final();
  return 0;
bad_end:
  puts("Module1.m0.o_a_b should be the old value of Module1.i_a_b");
  top->final();
  return 1;
}