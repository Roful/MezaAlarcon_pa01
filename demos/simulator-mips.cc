// Copyright 2011 the V8 project authors. All rights reserved.
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
//       copyright notice, this list of conditions and the following
//       disclaimer in the documentation and/or other materials provided
//       with the distribution.
//     * Neither the name of Google Inc. nor the names of its
//       contributors may be used to endorse or promote products derived
//       from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <cstdarg>
#include "v8.h"

#if defined(V8_TARGET_ARCH_MIPS)

#include "disasm.h"
#include "assembler.h"
#include "globals.h"    // Need the BitCast.
#include "mips/constants-mips.h"
#include "mips/simulator-mips.h"


// Only build the simulator if not compiling for real MIPS hardware.
#if defined(USE_SIMULATOR)

namespace v8 {
namespace internal {

// Utils functions.
bool HaveSameSign(int32_t a, int32_t b) {
  return ((a ^ b) >= 0);
}


uint32_t get_fcsr_condition_bit(uint32_t cc) {
  if (cc == 0) {
    return 23;
  } else {
    return 24 + cc;
  }
}


// This macro provides a platform independent use of sscanf. The reason for
// SScanF not being implemented in a platform independent was through
// ::v8::internal::OS in the same way as SNPrintF is that the Windows C Run-Time
// Library does not provide vsscanf.
#define SScanF sscanf  // NOLINT

// The MipsDebugger class is used by the simulator while debugging simulated
// code.
class MipsDebugger {
 public:
  explicit MipsDebugger(Simulator* sim);
  ~MipsDebugger();

  void Stop(Instruction* instr);
  void Debug();
  // Print all registers with a nice formatting.
  void PrintAllRegs();
  void PrintAllRegsIncludingFPU();

 private:
  // We set the breakpoint code to 0xfffff to easily recognize it.
  static const Instr kBreakpointInstr = SPECIAL | BREAK | 0xfffff << 6;
  static const Instr kNopInstr =  0x0;

  Simulator* sim_;

  int32_t GetRegisterValue(int regnum);
  int32_t GetFPURegisterValueInt(int regnum);
  int64_t GetFPURegisterValueLong(int regnum);
  float GetFPURegisterValueFloat(int regnum);
  double GetFPURegisterValueDouble(int regnum);
  bool GetValue(const char* desc, int32_t* value);

  // Set or delete a breakpoint. Returns true if successful.
  bool SetBreakpoint(Instruction* breakpc);
  bool DeleteBreakpoint(Instruction* breakpc);

  // Undo and redo all breakpoints. This is needed to bracket disassembly and
  // execution to skip past breakpoints when run from the debugger.
  void UndoBreakpoints();
  void RedoBreakpoints();
};

MipsDebugger::MipsDebugger(Simulator* sim) {
  sim_ = sim;
}


MipsDebugger::~MipsDebugger() {
}


#ifdef GENERATED_CODE_COVERAGE
static FILE* coverage_log = NULL;


static void InitializeCoverage() {
  char* file_name = getenv("V8_GENERATED_CODE_COVERAGE_LOG");
  if (file_name != NULL) {
    coverage_log = fopen(file_name, "aw+");
  }
}


void MipsDebugger::Stop(Instruction* instr) {
  // Get the stop code.
  uint32_t code = instr->Bits(25, 6);
  // Retrieve the encoded address, which comes just after this stop.
  char** msg_address =
    reinterpret_cast<char**>(sim_->get_pc() + Instr::kInstrSize);
  char* msg = *msg_address;
  ASSERT(msg != NULL);

  // Update this stop description.
  if (!watched_stops[code].desc) {
    watched_stops[code].desc = msg;
  }

  if (strlen(msg) > 0) {
    if (coverage_log != NULL) {
      fprintf(coverage_log, "%s\n", str);
      fflush(coverage_log);
    }
    // Overwrite the instruction and address with nops.
    instr->SetInstructionBits(kNopInstr);
    reinterpret_cast<Instr*>(msg_address)->SetInstructionBits(kNopInstr);
  }
  sim_->set_pc(sim_->get_pc() + 2 * Instruction::kInstructionSize);
}


#else  // GENERATED_CODE_COVERAGE

#define UNSUPPORTED() printf("Unsupported instruction.\n");

static void InitializeCoverage() {}


void MipsDebugger::Stop(Instruction* instr) {
  // Get the stop code.
  uint32_t code = instr->Bits(25, 6);
  // Retrieve the encoded address, which comes just after this stop.
  char* msg = *reinterpret_cast<char**>(sim_->get_pc() +
      Instruction::kInstrSize);
  // Update this stop description.
  if (!sim_->watched_stops[code].desc) {
    sim_->watched_stops[code].desc = msg;
  }
  PrintF("Simulator hit %s (%u)\n", msg, code);
  sim_->set_pc(sim_->get_pc() + 2 * Instruction::kInstrSize);
  Debug();
}
#endif  // GENERATED_CODE_COVERAGE


int32_t MipsDebugger::GetRegisterValue(int regnum) {
  if (regnum == kNumSimuRegisters) {
    return sim_->get_pc();
  } else {
    return sim_->get_register(regnum);
  }
}


int32_t MipsDebugger::GetFPURegisterValueInt(int regnum) {
  if (regnum == kNumFPURegisters) {
    return sim_->get_pc();
  } else {
    return sim_->get_fpu_register(regnum);
  }
}


int64_t MipsDebugger::GetFPURegisterValueLong(int regnum) {
  if (regnum == kNumFPURegisters) {
    return sim_->get_pc();
  } else {
    return sim_->get_fpu_register_long(regnum);
  }
}


float MipsDebugger::GetFPURegisterValueFloat(int regnum) {
  if (regnum == kNumFPURegisters) {
    return sim_->get_pc();
  } else {
    return sim_->get_fpu_register_float(regnum);
  }
}


double MipsDebugger::GetFPURegisterValueDouble(int regnum) {
  if (regnum == kNumFPURegisters) {
    return sim_->get_pc();
  } else {
    return sim_->get_fpu_register_double(regnum);
  }
}


bool MipsDebugger::GetValue(const char* desc, int32_t* value) {
  int regnum = Registers::Number(desc);
  int fpuregnum = FPURegisters::Number(desc);

  if (regnum != kInvalidRegister) {
    *value = GetRegisterValue(regnum);
    return true;
  } else if (fpuregnum != kInvalidFPURegister) {
    *value = GetFPURegisterValueInt(fpuregnum);
    return true;
  } else if (strncmp(desc, "0x", 2) == 0) {
    return SScanF(desc, "%x", reinterpret_cast<uint32_t*>(value)) == 1;
  } else {
    return SScanF(desc, "%i", value) == 1;
  }
  return false;
}


bool MipsDebugger::SetBreakpoint(Instruction* breakpc) {
  // Check if a breakpoint can be set. If not return without any side-effects.
  if (sim_->break_pc_ != NULL) {
    return false;
  }

  // Set the breakpoint.
  sim_->break_pc_ = breakpc;
  sim_->break_instr_ = breakpc->InstructionBits();
  // Not setting the breakpoint instruction in the code itself. It will be set
  // when the debugger shell continues.
  return true;
}


bool MipsDebugger::DeleteBreakpoint(Instruction* breakpc) {
  if (sim_->break_pc_ != NULL) {
    sim_->break_pc_->SetInstructionBits(sim_->break_instr_);
  }

  sim_->break_pc_ = NULL;
  sim_->break_instr_ = 0;
  return true;
}


void MipsDebugger::UndoBreakpoints() {
  if (sim_->break_pc_ != NULL) {
    sim_->break_pc_->SetInstructionBits(sim_->break_instr_);
  }
}


void MipsDebugger::RedoBreakpoints() {
  if (sim_->break_pc_ != NULL) {
    sim_->break_pc_->SetInstructionBits(kBreakpointInstr);
  }
}


void MipsDebugger::PrintAllRegs() {
#define REG_INFO(n) Registers::Name(n), GetRegisterValue(n), GetRegisterValue(n)

  PrintF("\n");
  // at, v0, a0.
  PrintF("%3s: 0x%08x %10d\t%3s: 0x%08x %10d\t%3s: 0x%08x %10d\n",
         REG_INFO(1), REG_INFO(2), REG_INFO(4));
  // v1, a1.
  PrintF("%26s\t%3s: 0x%08x %10d\t%3s: 0x%08x %10d\n",
         "", REG_INFO(3), REG_INFO(5));
  // a2.
  PrintF("%26s\t%26s\t%3s: 0x%08x %10d\n", "", "", REG_INFO(6));
  // a3.
  PrintF("%26s\t%26s\t%3s: 0x%08x %10d\n", "", "", REG_INFO(7));
  PrintF("\n");
  // t0-t7, s0-s7
  for (int i = 0; i < 8; i++) {
    PrintF("%3s: 0x%08x %10d\t%3s: 0x%08x %10d\n",
           REG_INFO(8+i), REG_INFO(16+i));
  }
  PrintF("\n");
  // t8, k0, LO.
  PrintF("%3s: 0x%08x %10d\t%3s: 0x%08x %10d\t%3s: 0x%08x %10d\n",
         REG_INFO(24), REG_INFO(26), REG_INFO(32));
  // t9, k1, HI.
  PrintF("%3s: 0x%08x %10d\t%3s: 0x%08x %10d\t%3s: 0x%08x %10d\n",
         REG_INFO(25), REG_INFO(27), REG_INFO(33));
  // sp, fp, gp.
  PrintF("%3s: 0x%08x %10d\t%3s: 0x%08x %10d\t%3s: 0x%08x %10d\n",
         REG_INFO(29), REG_INFO(30), REG_INFO(28));
  // pc.
  PrintF("%3s: 0x%08x %10d\t%3s: 0x%08x %10d\n",
         REG_INFO(31), REG_INFO(34));

#undef REG_INFO
#undef FPU_REG_INFO
}


void MipsDebugger::PrintAllRegsIncludingFPU() {
#define FPU_REG_INFO(n) FPURegisters::Name(n), FPURegisters::Name(n+1), \
        GetFPURegisterValueInt(n+1), \
        GetFPURegisterValueInt(n), \
                        GetFPURegisterValueDouble(n)

  PrintAllRegs();

  PrintF("\n\n");
  // f0, f1, f2, ... f31.
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(0) );
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(2) );
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(4) );
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(6) );
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(8) );
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(10));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(12));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(14));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(16));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(18));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(20));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(22));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(24));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(26));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(28));
  PrintF("%3s,%3s: 0x%08x%08x %16.4e\n", FPU_REG_INFO(30));

#undef REG_INFO
#undef FPU_REG_INFO
}


void MipsDebugger::Debug() {
  intptr_t last_pc = -1;
  bool done = false;

#define COMMAND_SIZE 63
#define ARG_SIZE 255

#define STR(a) #a
#define XSTR(a) STR(a)

  char cmd[COMMAND_SIZE + 1];
  char arg1[ARG_SIZE + 1];
  char arg2[ARG_SIZE + 1];
  char* argv[3] = { cmd, arg1, arg2 };

  // Make sure to have a proper terminating character if reaching the limit.
  cmd[COMMAND_SIZE] = 0;
  arg1[ARG_SIZE] = 0;
  arg2[ARG_SIZE] = 0;

  // Undo all set breakpoints while running in the debugger shell. This will
  // make them invisible to all commands.
  UndoBreakpoints();

  while (!done && (sim_->get_pc() != Simulator::end_sim_pc)) {
    if (last_pc != sim_->get_pc()) {
      disasm::NameConverter converter;
      disasm::Disassembler dasm(converter);
      // Use a reasonably large buffer.
      v8::internal::EmbeddedVector<char, 256> buffer;
      dasm.InstructionDecode(buffer,
                             reinterpret_cast<byte*>(sim_->get_pc()));
      PrintF("  0x%08x  %s\n", sim_->get_pc(), buffer.start());
      last_pc = sim_->get_pc();
    }
    char* line = ReadLine("sim> ");
    if (line == NULL) {
      break;
    } else {
      // Use sscanf to parse the individual parts of the command line. At the
      // moment no command expects more than two parameters.
      int argc = SScanF(line,
                        "%" XSTR(COMMAND_SIZE) "s "
                        "%" XSTR(ARG_SIZE) "s "
                        "%" XSTR(ARG_SIZE) "s",
                        cmd, arg1, arg2);
      if ((strcmp(cmd, "si") == 0) || (strcmp(cmd, "stepi") == 0)) {
        Instruction* instr = reinterpret_cast<Instruction*>(sim_->get_pc());
        if (!(instr->IsTrap()) ||
            instr->InstructionBits() == rtCallRedirInstr) {
          sim_->InstructionDecode(
              reinterpret_cast<Instruction*>(sim_->get_pc()));
        } else {
          // Allow si to jump over generated breakpoints.
          PrintF("/!\\ Jumping over generated breakpoint.\n");
          sim_->set_pc(sim_->get_pc() + Instruction::kInstrSize);
        }
      } else if ((strcmp(cmd, "c") == 0) || (strcmp(cmd, "cont") == 0)) {
        // Execute the one instruction we broke at with breakpoints disabled.
        sim_->InstructionDecode(reinterpret_cast<Instruction*>(sim_->get_pc()));
        // Leave the debugger shell.
        done = true;
      } else if ((strcmp(cmd, "p") == 0) || (strcmp(cmd, "print") == 0)) {
        if (argc == 2) {
          int32_t value;
          float fvalue;
          if (strcmp(arg1, "all") == 0) {
            PrintAllRegs();
          } else if (strcmp(arg1, "allf") == 0) {
            PrintAllRegsIncludingFPU();
          } else {
            int regnum = Registers::Number(arg1);
            int fpuregnum = FPURegisters::Number(arg1);

            if (regnum != kInvalidRegister) {
              value = GetRegisterValue(regnum);
              PrintF("%s: 0x%08x %d \n", arg1, value, value);
            } else if (fpuregnum != kInvalidFPURegister) {
              if (fpuregnum % 2 == 1) {
                value = GetFPURegisterValueInt(fpuregnum);
                fvalue = GetFPURegisterValueFloat(fpuregnum);
                PrintF("%s: 0x%08x %11.4e\n", arg1, value, fvalue);
              } else {
                double dfvalue;
                int32_t lvalue1 = GetFPURegisterValueInt(fpuregnum);
                int32_t lvalue2 = GetFPURegisterValueInt(fpuregnum + 1);
                dfvalue = GetFPURegisterValueDouble(fpuregnum);
                PrintF("%3s,%3s: 0x%08x%08x %16.4e\n",
                       FPURegisters::Name(fpuregnum+1),
                       FPURegisters::Name(fpuregnum),
                       lvalue1,
                       lvalue2,
                       dfvalue);
              }
            } else {
              PrintF("%s unrecognized\n", arg1);
            }
          }
        } else {
          if (argc == 3) {
            if (strcmp(arg2, "single") == 0) {
              int32_t value;
              float fvalue;
              int fpuregnum = FPURegisters::Number(arg1);

              if (fpuregnum != kInvalidFPURegister) {
                value = GetFPURegisterValueInt(fpuregnum);
                fvalue = GetFPURegisterValueFloat(fpuregnum);
                PrintF("%s: 0x%08x %11.4e\n", arg1, value, fvalue);
              } else {
                PrintF("%s unrecognized\n", arg1);
              }
            } else {
              PrintF("print <fpu register> single\n");
            }
          } else {
            PrintF("print <register> or print <fpu register> single\n");
          }
        }
      } else if ((strcmp(cmd, "po") == 0)
                 || (strcmp(cmd, "printobject") == 0)) {
        if (argc == 2) {
          int32_t value;
          if (GetValue(arg1, &value)) {
            Object* obj = reinterpret_cast<Object*>(value);
            PrintF("%s: \n", arg1);
#ifdef DEBUG
            obj->PrintLn();
#else
            obj->ShortPrint();
            PrintF("\n");
#endif
          } else {
            PrintF("%s unrecognized\n", arg1);
          }
        } else {
          PrintF("printobject <value>\n");
        }
      } else if (strcmp(cmd, "stack") == 0 || strcmp(cmd, "mem") == 0) {
        int32_t* cur = NULL;
        int32_t* end = NULL;
        int next_arg = 1;

        if (strcmp(cmd, "stack") == 0) {
          cur = reinterpret_cast<int32_t*>(sim_->get_register(Simulator::sp));
        } else {  // Command "mem".
          int32_t value;
          if (!GetValue(arg1, &value)) {
            PrintF("%s unrecognized\n", arg1);
            continue;
          }
          cur = reinterpret_cast<int32_t*>(value);
          next_arg++;
        }

        int32_t words;
        if (argc == next_arg) {
          words = 10;
        } else if (argc == next_arg + 1) {
          if (!GetValue(argv[next_arg], &words)) {
            words = 10;
          }
        }
        end = cur + words;

        while (cur < end) {
          PrintF("  0x%08x:  0x%08x %10d",
                 reinterpret_cast<intptr_t>(cur), *cur, *cur);
          HeapObject* obj = reinterpret_cast<HeapObject*>(*cur);
          int value = *cur;
          Heap* current_heap = v8::internal::Isolate::Current()->heap();
          if (current_heap->Contains(obj) || ((value & 1) == 0)) {
            PrintF(" (");
            if ((value & 1) == 0) {
              PrintF("smi %d", value / 2);
            } else {
              obj->ShortPrint();
            }
            PrintF(")");
          }
          PrintF("\n");
          cur++;
        }

      } else if ((strcmp(cmd, "disasm") == 0) ||
                 (strcmp(cmd, "dpc") == 0) ||
                 (strcmp(cmd, "di") == 0)) {
        disasm::NameConverter converter;
        disasm::Disassembler dasm(converter);
        // Use a reasonably large buffer.
        v8::internal::EmbeddedVector<char, 256> buffer;

        byte* cur = NULL;
        byte* end = NULL;

        if (argc == 1) {
          cur = reinterpret_cast<byte*>(sim_->get_pc());
          end = cur + (10 * Instruction::kInstrSize);
        } else if (argc == 2) {
          int regnum = Registers::Number(arg1);
          if (regnum != kInvalidRegister || strncmp(arg1, "0x", 2) == 0) {
            // The argument is an address or a register name.
            int32_t value;
            if (GetValue(arg1, &value)) {
              cur = reinterpret_cast<byte*>(value);
              // Disassemble 10 instructions at <arg1>.
              end = cur + (10 * Instruction::kInstrSize);
            }
          } else {
            // The argument is the number of instructions.
            int32_t value;
            if (GetValue(arg1, &value)) {
              cur = reinterpret_cast<byte*>(sim_->get_pc());
              // Disassemble <arg1> instructions.
              end = cur + (value * Instruction::kInstrSize);
            }
          }
        } else {
          int32_t value1;
          int32_t value2;
          if (GetValue(arg1, &value1) && GetValue(arg2, &value2)) {
            cur = reinterpret_cast<byte*>(value1);
            end = cur + (value2 * Instruction::kInstrSize);
          }
        }

        while (cur < end) {
          dasm.InstructionDecode(buffer, cur);
          PrintF("  0x%08x  %s\n",
              reinterpret_cast<intptr_t>(cur), buffer.start());
          cur += Instruction::kInstrSize;
        }
      } else if (strcmp(cmd, "gdb") == 0) {
        PrintF("relinquishing control to gdb\n");
        v8::internal::OS::DebugBreak();
        PrintF("regaining control from gdb\n");
      } else if (strcmp(cmd, "break") == 0) {
        if (argc == 2) {
          int32_t value;
          if (GetValue(arg1, &value)) {
            if (!SetBreakpoint(reinterpret_cast<Instruction*>(value))) {
              PrintF("setting breakpoint failed\n");
            }
          } else {
            PrintF("%s unrecognized\n", arg1);
          }
        } else {
          PrintF("break <address>\n");
        }
      } else if (strcmp(cmd, "del") == 0) {
        if (!DeleteBreakpoint(NULL)) {
          PrintF("deleting breakpoint failed\n");
        }
      } else if (strcmp(cmd, "flags") == 0) {
        PrintF("No flags on MIPS !\n");
      } else if (strcmp(cmd, "stop") == 0) {
        int32_t value;
        intptr_t stop_pc = sim_->get_pc() -
            2 * Instruction::kInstrSize;
        Instruction* stop_instr = reinterpret_cast<Instruction*>(stop_pc);
        Instruction* msg_address =
          reinterpret_cast<Instruction*>(stop_pc +
              Instruction::kInstrSize);
        if ((argc == 2) && (strcmp(arg1, "unstop") == 0)) {
          // Remove the current stop.
          if (sim_->IsStopInstruction(stop_instr)) {
            stop_instr->SetInstructionBits(kNopInstr);
            msg_address->SetInstructionBits(kNopInstr);
          } else {
            PrintF("Not at debugger stop.\n");
          }
        } else if (argc == 3) {
          // Print information about all/the specified breakpoint(s).
          if (strcmp(arg1, "info") == 0) {
            if (strcmp(arg2, "all") == 0) {
              PrintF("Stop information:\n");
              for (uint32_t i = kMaxWatchpointCode + 1;
                   i <= kMaxStopCode;
                   i++) {
                sim_->PrintStopInfo(i);
              }
            } else if (GetValue(arg2, &value)) {
              sim_->PrintStopInfo(value);
            } else {
              PrintF("Unrecognized argument.\n");
            }
          } else if (strcmp(arg1, "enable") == 0) {
            // Enable all/the specified breakpoint(s).
            if (strcmp(arg2, "all") == 0) {
              for (uint32_t i = kMaxWatchpointCode + 1;
                   i <= kMaxStopCode;
                   i++) {
                sim_->EnableStop(i);
              }
            } else if (GetValue(arg2, &value)) {
              sim_->EnableStop(value);
            } else {
              PrintF("Unrecognized argument.\n");
            }
          } else if (strcmp(arg1, "disable") == 0) {
            // Disable all/the specified breakpoint(s).
            if (strcmp(arg2, "all") == 0) {
              for (uint32_t i = kMaxWatchpointCode + 1;
                   i <= kMaxStopCode;
                   i++) {
                sim_->DisableStop(i);
              }
            } else if (GetValue(arg2, &value)) {
              sim_->DisableStop(value);
            } else {
              PrintF("Unrecognized argument.\n");
            }
          }
        } else {
          PrintF("Wrong usage. Use help command for more information.\n");
        }
      } else if ((strcmp(cmd, "stat") == 0) || (strcmp(cmd, "st") == 0)) {
        // Print registers and disassemble.
        PrintAllRegs();
        PrintF("\n");

        disasm::NameConverter converter;
        disasm::Disassembler dasm(converter);
        // Use a reasonably large buffer.
        v8::internal::EmbeddedVector<char, 256> buffer;

        byte* cur = NULL;
        byte* end = NULL;

        if (argc == 1) {
          cur = reinterpret_cast<byte*>(sim_->get_pc());
          end = cur + (10 * Instruction::kInstrSize);
        } else if (argc == 2) {
          int32_t value;
          if (GetValue(arg1, &value)) {
            cur = reinterpret_cast<byte*>(value);
            // no length parameter passed, assume 10 instructions
            end = cur + (10 * Instruction::kInstrSize);
          }
        } else {
          int32_t value1;
          int32_t value2;
          if (GetValue(arg1, &value1) && GetValue(arg2, &value2)) {
            cur = reinterpret_cast<byte*>(value1);
            end = cur + (value2 * Instruction::kInstrSize);
          }
        }

        while (cur < end) {
          dasm.InstructionDecode(buffer, cur);
          PrintF("  0x%08x  %s\n",
                 reinterpret_cast<intptr_t>(cur), buffer.start());
          cur += Instruction::kInstrSize;
        }
      } else if ((strcmp(cmd, "h") == 0) || (strcmp(cmd, "help") == 0)) {
        PrintF("cont\n");
        PrintF("  continue execution (alias 'c')\n");
        PrintF("stepi\n");
        PrintF("  step one instruction (alias 'si')\n");
        PrintF("print <register>\n");
        PrintF("  print register content (alias 'p')\n");
        PrintF("  use register name 'all' to print all registers\n");
        PrintF("printobject <register>\n");
        PrintF("  print an object from a register (alias 'po')\n");
        PrintF("stack [<words>]\n");
        PrintF("  dump stack content, default dump 10 words)\n");
        PrintF("mem <address> [<words>]\n");
        PrintF("  dump memory content, default dump 10 words)\n");
        PrintF("flags\n");
        PrintF("  print flags\n");
        PrintF("disasm [<instructions>]\n");
        PrintF("disasm [<address/register>]\n");
        PrintF("disasm [[<address/register>] <instructions>]\n");
        PrintF("  disassemble code, default is 10 instructions\n");
        PrintF("  from pc (alias 'di')\n");
        PrintF("gdb\n");
        PrintF("  enter gdb\n");
        PrintF("break <address>\n");
        PrintF("  set a break point on the address\n");
        PrintF("del\n");
        PrintF("  delete the breakpoint\n");
        PrintF("stop feature:\n");
        PrintF("  Description:\n");
        PrintF("    Stops are debug instructions inserted by\n");
        PrintF("    the Assembler::stop() function.\n");
        PrintF("    When hitting a stop, the Simulator will\n");
        PrintF("    stop and and give control to the Debugger.\n");
        PrintF("    All stop codes are watched:\n");
        PrintF("    - They can be enabled / disabled: the Simulator\n");
        PrintF("       will / won't stop when hitting them.\n");
        PrintF("    - The Simulator keeps track of how many times they \n");
        PrintF("      are met. (See the info command.) Going over a\n");
        PrintF("      disabled stop still increases its counter. \n");
        PrintF("  Commands:\n");
        PrintF("    stop info all/<code> : print infos about number <code>\n");
        PrintF("      or all stop(s).\n");
        PrintF("    stop enable/disable all/<code> : enables / disables\n");
        PrintF("      all or number <code> stop(s)\n");
        PrintF("    stop unstop\n");
        PrintF("      ignore the stop instruction at the current location\n");
        PrintF("      from now on\n");
      } else {
        PrintF("Unknown command: %s\n", cmd);
      }
    }
    DeleteArray(line);
  }

  // Add all the breakpoints back to stop execution and enter the debugger
  // shell when hit.
  RedoBreakpoints();

#undef COMMAND_SIZE
#undef ARG_SIZE

#undef STR
#undef XSTR
}


static bool ICacheMatch(void* one, void* two) {
  ASSERT((reinterpret_cast<intptr_t>(one) & CachePage::kPageMask) == 0);
  ASSERT((reinterpret_cast<intptr_t>(two) & CachePage::kPageMask) == 0);
  return one == two;
}


static uint32_t ICacheHash(void* key) {
  return static_cast<uint32_t>(reinterpret_cast<uintptr_t>(key)) >> 2;
}


static bool AllOnOnePage(uintptr_t start, int size) {
  intptr_t start_page = (start & ~CachePage::kPageMask);
  intptr_t end_page = ((start + size) & ~CachePage::kPageMask);
  return start_page == end_page;
}


void Simulator::FlushICache(v8::internal::HashMap* i_cache,
                            void* start_addr,
                            size_t size) {
  intptr_t start = reinterpret_cast<intptr_t>(start_addr);
  int intra_line = (start & CachePage::kLineMask);
  start -= intra_line;
  size += intra_line;
  size = ((size - 1) | CachePage::kLineMask) + 1;
  int offset = (start & CachePage::kPageMask);
  while (!AllOnOnePage(start, size - 1)) {
    int bytes_to_flush = CachePage::kPageSize - offset;
    FlushOnePage(i_cache, start, bytes_to_flush);
    start += bytes_to_flush;
    size -= bytes_to_flush;
    ASSERT_EQ(0, start & CachePage::kPageMask);
    offset = 0;
  }
  if (size != 0) {
    FlushOnePage(i_cache, start, size);
  }
}


CachePage* Simulator::GetCachePage(v8::internal::HashMap* i_cache, void* page) {
  v8::internal::HashMap::Entry* entry = i_cache->Lookup(page,
                                                        ICacheHash(page),
                                                        true);
  if (entry->value == NULL) {
    CachePage* new_page = new CachePage();
    entry->value = new_page;
  }
  return reinterpret_cast<CachePage*>(entry->value);
}


// Flush from start up to and not including start + size.
void Simulator::FlushOnePage(v8::internal::HashMap* i_cache,
                             intptr_t start,
                             int size) {
  ASSERT(size <= CachePage::kPageSize);
  ASSERT(AllOnOnePage(start, size - 1));
  ASSERT((start & CachePage::kLineMask) == 0);
  ASSERT((size & CachePage::kLineMask) == 0);
  void* page = reinterpret_cast<void*>(start & (~CachePage::kPageMask));
  int offset = (start & CachePage::kPageMask);
  CachePage* cache_page = GetCachePage(i_cache, page);
  char* valid_bytemap = cache_page->ValidityByte(offset);
  memset(valid_bytemap, CachePage::LINE_INVALID, size >> CachePage::kLineShift);
}


void Simulator::CheckICache(v8::internal::HashMap* i_cache,
                            Instruction* instr) {
  intptr_t address = reinterpret_cast<intptr_t>(instr);
  void* page = reinterpret_cast<void*>(address & (~CachePage::kPageMask));
  void* line = reinterpret_cast<void*>(address & (~CachePage::kLineMask));
  int offset = (address & CachePage::kPageMask);
  CachePage* cache_page = GetCachePage(i_cache, page);
  char* cache_valid_byte = cache_page->ValidityByte(offset);
  bool cache_hit = (*cache_valid_byte == CachePage::LINE_VALID);
  char* cached_line = cache_page->CachedData(offset & ~CachePage::kLineMask);
  if (cache_hit) {
    // Check that the data in memory matches the contents of the I-cache.
    CHECK(memcmp(reinterpret_cast<void*>(instr),
                 cache_page->CachedData(offset),
                 Instruction::kInstrSize) == 0);
  } else {
    // Cache miss.  Load memory into the cache.
    memcpy(cached_line, line, CachePage::kLineLength);
    *cache_valid_byte = CachePage::LINE_VALID;
  }
}


void Simulator::Initialize(Isolate* isolate) {
  if (isolate->simulator_initialized()) return;
  isolate->set_simulator_initialized(true);
  ::v8::internal::ExternalReference::set_redirector(isolate,
                                                    &RedirectExternalReference);
}


Simulator::Simulator(Isolate* isolate) : isolate_(isolate) {
  i_cache_ = isolate_->simulator_i_cache();
  if (i_cache_ == NULL) {
    i_cache_ = new v8::internal::HashMap(&ICacheMatch);
    isolate_->set_simulator_i_cache(i_cache_);
  }
  Initialize(isolate);
  // Setup simulator support first. Some of this information is needed to
  // setup the architecture state.
  stack_ = reinterpret_cast<char*>(malloc(stack_size_));
  pc_modified_ = false;
  icount_ = 0;
  break_count_ = 0;
  break_pc_ = NULL;
  break_instr_ = 0;

  // Setup architecture state.
  // All registers are initialized to zero to start with.
  for (int i = 0; i < kNumSimuRegisters; i++) {
    registers_[i] = 0;
  }
  for (int i = 0; i < kNumFPURegisters; i++) {
    FPUregisters_[i] = 0;
  }
  FCSR_ = 0;

  // The sp is initialized to point to the bottom (high address) of the
  // allocated stack area. To be safe in potential stack underflows we leave
  // some buffer below.
  registers_[sp] = reinterpret_cast<int32_t>(stack_) + stack_size_ - 64;
  // The ra and pc are initialized to a known bad value that will cause an
  // access violation if the simulator ever tries to execute it.
  registers_[pc] = bad_ra;
  registers_[ra] = bad_ra;
  InitializeCoverage();
  for (int i = 0; i < kNumExceptions; i++) {
    exceptions[i] = 0;
  }
}


// When the generated code calls an external reference we need to catch that in
// the simulator.  The external reference will be a function compiled for the
// host architecture.  We need to call that function instead of trying to
// execute it with the simulator.  We do that by redirecting the external
// reference to a swi (software-interrupt) instruction that is handled by
// the simulator.  We write the original destination of the jump just at a known
// offset from the swi instruction so the simulator knows what to call.
class Redirection {
 public:
  Redirection(void* external_function, ExternalReference::Type type)
      : external_function_(external_function),
        swi_instruction_(rtCallRedirInstr),
        type_(type),
        next_(NULL) {
    Isolate* isolate = Isolate::Current();
    next_ = isolate->simulator_redirection();
    Simulator::current(isolate)->
        FlushICache(isolate->simulator_i_cache(),
                    reinterpret_cast<void*>(&swi_instruction_),
                    Instruction::kInstrSize);
    isolate->set_simulator_redirection(this);
  }

  void* address_of_swi_instruction() {
    return reinterpret_cast<void*>(&swi_instruction_);
  }

  void* external_function() { return external_function_; }
  ExternalReference::Type type() { return type_; }

  static Redirection* Get(void* external_function,
                          ExternalReference::Type type) {
    Isolate* isolate = Isolate::Current();
    Redirection* current = isolate->simulator_redirection();
    for (; current != NULL; current = current->next_) {
      if (current->external_function_ == external_function) return current;
    }
    return new Redirection(external_function, type);
  }

  static Redirection* FromSwiInstruction(Instruction* swi_instruction) {
    char* addr_of_swi = reinterpret_cast<char*>(swi_instruction);
    char* addr_of_redirection =
        addr_of_swi - OFFSET_OF(Redirection, swi_instruction_);
    return reinterpret_cast<Redirection*>(addr_of_redirection);
  }

 private:
  void* external_function_;
  uint32_t swi_instruction_;
  ExternalReference::Type type_;
  Redirection* next_;
};


void* Simulator::RedirectExternalReference(void* external_function,
                                           ExternalReference::Type type) {
  Redirection* redirection = Redirection::Get(external_function, type);
  return redirection->address_of_swi_instruction();
}


// Get the active Simulator for the current thread.
Simulator* Simulator::current(Isolate* isolate) {
  v8::internal::Isolate::PerIsolateThreadData* isolate_data =
       isolate->FindOrAllocatePerThreadDataForThisThread();
  ASSERT(isolate_data != NULL);
  ASSERT(isolate_data != NULL);

  Simulator* sim = isolate_data->simulator();
  if (sim == NULL) {
    // TODO(146): delete the simulator object when a thread/isolate goes away.
    sim = new Simulator(isolate);
    isolate_data->set_simulator(sim);
  }
  return sim;
}


// Sets the register in the architecture state. It will also deal with updating
// Simulator internal state for special registers such as PC.
void Simulator::set_register(int reg, int32_t value) {
  ASSERT((reg >= 0) && (reg < kNumSimuRegisters));
  if (reg == pc) {
    pc_modified_ = true;
  }

  // Zero register always holds 0.
  registers_[reg] = (reg == 0) ? 0 : value;
}


void Simulator::set_fpu_register(int fpureg, int32_t value) {
  ASSERT((fpureg >= 0) && (fpureg < kNumFPURegisters));
  FPUregisters_[fpureg] = value;
}


void Simulator::set_fpu_register_float(int fpureg, float value) {
  ASSERT((fpureg >= 0) && (fpureg < kNumFPURegisters));
  *BitCast<float*>(&FPUregisters_[fpureg]) = value;
}


void Simulator::set_fpu_register_double(int fpureg, double value) {
  ASSERT((fpureg >= 0) && (fpureg < kNumFPURegisters) && ((fpureg % 2) == 0));
  *BitCast<double*>(&FPUregisters_[fpureg]) = value;
}


// Get the register from the architecture state. This function does handle
// the special case of accessing the PC register.
int32_t Simulator::get_register(int reg) const {
  ASSERT((reg >= 0) && (reg < kNumSimuRegisters));
  if (reg == 0)
    return 0;
  else
    return registers_[reg] + ((reg == pc) ? Instruction::kPCReadOffset : 0);
}


int32_t Simulator::get_fpu_register(int fpureg) const {
  ASSERT((fpureg >= 0) && (fpureg < kNumFPURegisters));
  return FPUregisters_[fpureg];
}


int64_t Simulator::get_fpu_register_long(int fpureg) const {
  ASSERT((fpureg >= 0) && (fpureg < kNumFPURegisters) && ((fpureg % 2) == 0));
  return *BitCast<int64_t*>(
      const_cast<int32_t*>(&FPUregisters_[fpureg]));
}


float Simulator::get_fpu_register_float(int fpureg) const {
  ASSERT((fpureg >= 0) && (fpureg < kNumFPURegisters));
  return *BitCast<float*>(
      const_cast<int32_t*>(&FPUregisters_[fpureg]));
}


double Simulator::get_fpu_register_double(int fpureg) const {
  ASSERT((fpureg >= 0) && (fpureg < kNumFPURegisters) && ((fpureg % 2) == 0));
  return *BitCast<double*>(const_cast<int32_t*>(&FPUregisters_[fpureg]));
}


// For use in calls that take two double values, constructed either
// from a0-a3 or f12 and f14.
void Simulator::GetFpArgs(double* x, double* y) {
  if (!IsMipsSoftFloatABI) {
    *x = get_fpu_register_double(12);
    *y = get_fpu_register_double(14);
  } else {
    // We use a char buffer to get around the strict-aliasing rules which
    // otherwise allow the compiler to optimize away the copy.
    char buffer[sizeof(*x)];
    int32_t* reg_buffer = reinterpret_cast<int32_t*>(buffer);

    // Registers a0 and a1 -> x.
    reg_buffer[0] = get_register(a0);
    reg_buffer[1] = get_register(a1);
    memcpy(x, buffer, sizeof(buffer));

    // Registers a2 and a3 -> y.
    reg_buffer[0] = get_register(a2);
    reg_buffer[1] = get_register(a3);
    memcpy(y, buffer, sizeof(buffer));
  }
}


// For use in calls that take one double value, constructed either
// from a0 and a1 or f12.
void Simulator::GetFpArgs(double* x) {
  if (!IsMipsSoftFloatABI) {
    *x = get_fpu_register_double(12);
  } else {
    // We use a char buffer to get around the strict-aliasing rules which
    // otherwise allow the compiler to optimize away the copy.
    char buffer[sizeof(*x)];
    int32_t* reg_buffer = reinterpret_cast<int32_t*>(buffer);
    // Registers a0 and a1 -> x.
    reg_buffer[0] = get_register(a0);
    reg_buffer[1] = get_register(a1);
    memcpy(x, buffer, sizeof(buffer));
  }
}


// For use in calls that take one double value constructed either
// from a0 and a1 or f12 and one integer value.
void Simulator::GetFpArgs(double* x, int32_t* y) {
  if (!IsMipsSoftFloatABI) {
    *x = get_fpu_register_double(12);
    *y = get_register(a2);
  } else {
    // We use a char buffer to get around the strict-aliasing rules which
    // otherwise allow the compiler to optimize away the copy.
    char buffer[sizeof(*x)];
    int32_t* reg_buffer = reinterpret_cast<int32_t*>(buffer);
    // Registers 0 and 1 -> x.
    reg_buffer[0] = get_register(a0);
    reg_buffer[1] = get_register(a1);
    memcpy(x, buffer, sizeof(buffer));

    // Register 2 -> y.
    reg_buffer[0] = get_register(a2);
    memcpy(y, buffer, sizeof(*y));
  }
}


// The return value is either in v0/v1 or f0.
void Simulator::SetFpResult(const double& result) {
  if (!IsMipsSoftFloatABI) {
    set_fpu_register_double(0, result);
  } else {
    char buffer[2 * sizeof(registers_[0])];
    int32_t* reg_buffer = reinterpret_cast<int32_t*>(buffer);
    memcpy(buffer, &result, sizeof(buffer));
    // Copy result to v0 and v1.
    set_register(v0, reg_buffer[0]);
    set_register(v1, reg_buffer[1]);
  }
}


// Helper functions for setting and testing the FCSR register's bits.
void Simulator::set_fcsr_bit(uint32_t cc, bool value) {
  if (value) {
    FCSR_ |= (1 << cc);
  } else {
    FCSR_ &= ~(1 << cc);
  }
}


bool Simulator::test_fcsr_bit(uint32_t cc) {
  return FCSR_ & (1 << cc);
}


// Sets the rounding error codes in FCSR based on the result of the rounding.
// Returns true if the operation was invalid.
bool Simulator::set_fcsr_round_error(double original, double rounded) {
  bool ret = false;

  if (!isfinite(original) || !isfinite(rounded)) {
    set_fcsr_bit(kFCSRInvalidOpFlagBit, true);
    ret = true;
  }

  if (original != rounded) {
    set_fcsr_bit(kFCSRInexactFlagBit, true);
  }

  if (rounded < DBL_MIN && rounded > -DBL_MIN && rounded != 0) {
    set_fcsr_bit(kFCSRUnderflowFlagBit, true);
    ret = true;
  }

  if (rounded > INT_MAX || rounded < INT_MIN) {
    set_fcsr_bit(kFCSROverflowFlagBit, true);
    // The reference is not really clear but it seems this is required:
    set_fcsr_bit(kFCSRInvalidOpFlagBit, true);
    ret = true;
  }

  return ret;
}


// Raw access to the PC register.
void Simulator::set_pc(int32_t value) {
  pc_modified_ = true;
  registers_[pc] = value;
}


bool Simulator::has_bad_pc() const {
  return ((registers_[pc] == bad_ra) || (registers_[pc] == end_sim_pc));
}


// Raw access to the PC register without the special adjustment when reading.
int32_t Simulator::get_pc() const {
  return registers_[pc];
}


// The MIPS cannot do unaligned reads and writes.  On some MIPS platforms an
// interrupt is caused.  On others it does a funky rotation thing.  For now we
// simply disallow unaligned reads, but at some point we may want to move to
// emulating the rotate behaviour.  Note that simulator runs have the runtime
// system running directly on the host system and only generated code is
// executed in the simulator.  Since the host is typically IA32 we will not
// get the correct MIPS-like behaviour on unaligned accesses.

int Simulator::ReadW(int32_t addr, Instruction* instr) {
  if (addr >=0 && addr < 0x400) {
    // This has to be a NULL-dereference, drop into debugger.
    MipsDebugger dbg(this);
    dbg.Debug();
  }
  if ((addr & kPointerAlignmentMask) == 0) {
    intptr_t* ptr = reinterpret_cast<intptr_t*>(addr);
    return *ptr;
  }
  PrintF("Unaligned read at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  MipsDebugger dbg(this);
  dbg.Debug();
  return 0;
}


void Simulator::WriteW(int32_t addr, int value, Instruction* instr) {
  if (addr >= 0 && addr < 0x400) {
    // This has to be a NULL-dereference, drop into debugger.
    MipsDebugger dbg(this);
    dbg.Debug();
  }
  if ((addr & kPointerAlignmentMask) == 0) {
    intptr_t* ptr = reinterpret_cast<intptr_t*>(addr);
    *ptr = value;
    return;
  }
  PrintF("Unaligned write at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  MipsDebugger dbg(this);
  dbg.Debug();
}


double Simulator::ReadD(int32_t addr, Instruction* instr) {
  if ((addr & kDoubleAlignmentMask) == 0) {
    double* ptr = reinterpret_cast<double*>(addr);
    return *ptr;
  }
  PrintF("Unaligned (double) read at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  OS::Abort();
  return 0;
}


void Simulator::WriteD(int32_t addr, double value, Instruction* instr) {
  if ((addr & kDoubleAlignmentMask) == 0) {
    double* ptr = reinterpret_cast<double*>(addr);
    *ptr = value;
    return;
  }
  PrintF("Unaligned (double) write at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  OS::Abort();
}


uint16_t Simulator::ReadHU(int32_t addr, Instruction* instr) {
  if ((addr & 1) == 0) {
    uint16_t* ptr = reinterpret_cast<uint16_t*>(addr);
    return *ptr;
  }
  PrintF("Unaligned unsigned halfword read at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  OS::Abort();
  return 0;
}


int16_t Simulator::ReadH(int32_t addr, Instruction* instr) {
  if ((addr & 1) == 0) {
    int16_t* ptr = reinterpret_cast<int16_t*>(addr);
    return *ptr;
  }
  PrintF("Unaligned signed halfword read at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  OS::Abort();
  return 0;
}


void Simulator::WriteH(int32_t addr, uint16_t value, Instruction* instr) {
  if ((addr & 1) == 0) {
    uint16_t* ptr = reinterpret_cast<uint16_t*>(addr);
    *ptr = value;
    return;
  }
  PrintF("Unaligned unsigned halfword write at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  OS::Abort();
}


void Simulator::WriteH(int32_t addr, int16_t value, Instruction* instr) {
  if ((addr & 1) == 0) {
    int16_t* ptr = reinterpret_cast<int16_t*>(addr);
    *ptr = value;
    return;
  }
  PrintF("Unaligned halfword write at 0x%08x, pc=0x%08" V8PRIxPTR "\n",
         addr,
         reinterpret_cast<intptr_t>(instr));
  OS::Abort();
}


uint32_t Simulator::ReadBU(int32_t addr) {
  uint8_t* ptr = reinterpret_cast<uint8_t*>(addr);
  return *ptr & 0xff;
}


int32_t Simulator::ReadB(int32_t addr) {
  int8_t* ptr = reinterpret_cast<int8_t*>(addr);
  return *ptr;
}


void Simulator::WriteB(int32_t addr, uint8_t value) {
  uint8_t* ptr = reinterpret_cast<uint8_t*>(addr);
  *ptr = value;
}


void Simulator::WriteB(int32_t addr, int8_t value) {
  int8_t* ptr = reinterpret_cast<int8_t*>(addr);
  *ptr = value;
}


// Returns the limit of the stack area to enable checking for stack overflows.
uintptr_t Simulator::StackLimit() const {
  // Leave a safety margin of 256 bytes to prevent overrunning the stack when
  // pushing values.
  return reinterpret_cast<uintptr_t>(stack_) + 256;
}


// Unsupported instructions use Format to print an error and stop execution.
void Simulator::Format(Instruction* instr, const char* format) {
  PrintF("Simulator found unsupported instruction:\n 0x%08x: %s\n",
         reinterpret_cast<intptr_t>(instr), format);
  UNIMPLEMENTED_MIPS();
}


// Calls into the V8 runtime are based on this very simple interface.
// Note: To be able to return two values from some calls the code in runtime.cc
// uses the ObjectPair which is essentially two 32-bit values stuffed into a
// 64-bit value. With the code below we assume that all runtime calls return
// 64 bits of result. If they don't, the v1 result register contains a bogus
// value, which is fine because it is caller-saved.
typedef int64_t (*SimulatorRuntimeCall)(int32_t arg0,
                                        int32_t arg1,
                                        int32_t arg2,
                                        int32_t arg3,
                                        int32_t arg4,
                                        int32_t arg5);
typedef double (*SimulatorRuntimeFPCall)(int32_t arg0,
                                         int32_t arg1,
                                         int32_t arg2,
                                         int32_t arg3);

// This signature supports direct call in to API function native callback
// (refer to InvocationCallback in v8.h).
typedef v8::Handle<v8::Value> (*SimulatorRuntimeDirectApiCall)(int32_t arg0);

// This signature supports direct call to accessor getter callback.
typedef v8::Handle<v8::Value> (*SimulatorRuntimeDirectGetterCall)(int32_t arg0,
                                                                  int32_t arg1);

// Software interrupt instructions are used by the simulator to call into the
// C-based V8 runtime. They are also used for debugging with simulator.
void Simulator::SoftwareInterrupt(Instruction* instr) {
  // There are several instructions that could get us here,
  // the break_ instruction, or several variants of traps. All
  // Are "SPECIAL" class opcode, and are distinuished by function.
  int32_t func = instr->FunctionFieldRaw();
  uint32_t code = (func == BREAK) ? instr->Bits(25, 6) : -1;

  // We first check if we met a call_rt_redirected.
  if (instr->InstructionBits() == rtCallRedirInstr) {
    Redirection* redirection = Redirection::FromSwiInstruction(instr);
    int32_t arg0 = get_register(a0);
    int32_t arg1 = get_register(a1);
    int32_t arg2 = get_register(a2);
    int32_t arg3 = get_register(a3);
    int32_t arg4 = 0;
    int32_t arg5 = 0;

    // Need to check if sp is valid before assigning arg4, arg5.
    // This is a fix for cctest test-api/CatchStackOverflow which causes
    // the stack to overflow. For some reason arm doesn't need this
    // stack check here.
    int32_t* stack_pointer = reinterpret_cast<int32_t*>(get_register(sp));
    int32_t* stack = reinterpret_cast<int32_t*>(stack_);
    if (stack_pointer >= stack && stack_pointer < stack + stack_size_ - 5) {
      // Args 4 and 5 are on the stack after the reserved space for args 0..3.
      arg4 = stack_pointer[4];
      arg5 = stack_pointer[5];
    }

    bool fp_call =
         (redirection->type() == ExternalReference::BUILTIN_FP_FP_CALL) ||
         (redirection->type() == ExternalReference::BUILTIN_COMPARE_CALL) ||
         (redirection->type() == ExternalReference::BUILTIN_FP_CALL) ||
         (redirection->type() == ExternalReference::BUILTIN_FP_INT_CALL);

    if (!IsMipsSoftFloatABI) {
      // With the hard floating point calling convention, double
      // arguments are passed in FPU registers. Fetch the arguments
      // from there and call the builtin using soft floating point
      // convention.
      switch (redirection->type()) {
      case ExternalReference::BUILTIN_FP_FP_CALL:
      case ExternalReference::BUILTIN_COMPARE_CALL:
        arg0 = get_fpu_register(f12);
        arg1 = get_fpu_register(f13);
        arg2 = get_fpu_register(f14);
        arg3 = get_fpu_register(f15);
        break;
      case ExternalReference::BUILTIN_FP_CALL:
        arg0 = get_fpu_register(f12);
        arg1 = get_fpu_register(f13);
        break;
      case ExternalReference::BUILTIN_FP_INT_CALL:
        arg0 = get_fpu_register(f12);
        arg1 = get_fpu_register(f13);
        arg2 = get_register(a2);
        break;
      default:
        break;
      }
    }

    // This is dodgy but it works because the C entry stubs are never moved.
    // See comment in codegen-arm.cc and bug 1242173.
    int32_t saved_ra = get_register(ra);

    intptr_t external =
          reinterpret_cast<intptr_t>(redirection->external_function());

    // Based on CpuFeatures::IsSupported(FPU), Mips will use either hardware
    // FPU, or gcc soft-float routines. Hardware FPU is simulated in this
    // simulator. Soft-float has additional abstraction of ExternalReference,
    // to support serialization.
    if (fp_call) {
      SimulatorRuntimeFPCall target =
                  reinterpret_cast<SimulatorRuntimeFPCall>(external);
      if (::v8::internal::FLAG_trace_sim) {
        double dval0, dval1;
        int32_t ival;
        switch (redirection->type()) {
          case ExternalReference::BUILTIN_FP_FP_CALL:
          case ExternalReference::BUILTIN_COMPARE_CALL:
            GetFpArgs(&dval0, &dval1);
            PrintF("Call to host function at %p with args %f, %f",
                FUNCTION_ADDR(target), dval0, dval1);
            break;
          case ExternalReference::BUILTIN_FP_CALL:
            GetFpArgs(&dval0);
            PrintF("Call to host function at %p with arg %f",
                FUNCTION_ADDR(target), dval0);
            break;
          case ExternalReference::BUILTIN_FP_INT_CALL:
            GetFpArgs(&dval0, &ival);
            PrintF("Call to host function at %p with args %f, %d",
                FUNCTION_ADDR(target), dval0, ival);
            break;
          default:
            UNREACHABLE();
            break;
        }
      }
      double result = target(arg0, arg1, arg2, arg3);
      if (redirection->type() != ExternalReference::BUILTIN_COMPARE_CALL) {
          SetFpResult(result);
      } else {
        int32_t gpreg_pair[2];
        memcpy(&gpreg_pair[0], &result, 2 * sizeof(int32_t));
        set_register(v0, gpreg_pair[0]);
        set_register(v1, gpreg_pair[1]);
      }
    } else if (redirection->type() == ExternalReference::DIRECT_API_CALL) {
      // See DirectCEntryStub::GenerateCall for explanation of register usage.
      SimulatorRuntimeDirectApiCall target =
                  reinterpret_cast<SimulatorRuntimeDirectApiCall>(external);
      if (::v8::internal::FLAG_trace_sim) {
        PrintF("Call to host function at %p args %08x\n",
               FUNCTION_ADDR(target), arg1);
      }
      v8::Handle<v8::Value> result = target(arg1);
      *(reinterpret_cast<int*>(arg0)) = (int32_t) *result;
      set_register(v0, arg0);
    } else if (redirection->type() == ExternalReference::DIRECT_GETTER_CALL) {
      // See DirectCEntryStub::GenerateCall for explanation of register usage.
      SimulatorRuntimeDirectGetterCall target =
                  reinterpret_cast<SimulatorRuntimeDirectGetterCall>(external);
      if (::v8::internal::FLAG_trace_sim) {
        PrintF("Call to host function at %p args %08x %08x\n",
               FUNCTION_ADDR(target), arg1, arg2);
      }
      v8::Handle<v8::Value> result = target(arg1, arg2);
      *(reinterpret_cast<int*>(arg0)) = (int32_t) *result;
      set_register(v0, arg0);
    } else {
      SimulatorRuntimeCall target =
                  reinterpret_cast<SimulatorRuntimeCall>(external);
      if (::v8::internal::FLAG_trace_sim) {
        PrintF(
            "Call to host function at %p "
            "args %08x, %08x, %08x, %08x, %08x, %08x\n",
            FUNCTION_ADDR(target),
            arg0,
            arg1,
            arg2,
            arg3,
            arg4,
            arg5);
      }
      int64_t result = target(arg0, arg1, arg2, arg3, arg4, arg5);
      set_register(v0, static_cast<int32_t>(result));
      set_register(v1, static_cast<int32_t>(result >> 32));
    }
    if (::v8::internal::FLAG_trace_sim) {
      PrintF("Returned %08x : %08x\n", get_register(v1), get_register(v0));
    }
    set_register(ra, saved_ra);
    set_pc(get_register(ra));

  } else if (func == BREAK && code <= kMaxStopCode) {
    if (IsWatchpoint(code)) {
      PrintWatchpoint(code);
    } else {
      IncreaseStopCounter(code);
      HandleStop(code, instr);
    }
  } else {
    // All remaining break_ codes, and all traps are handled here.
    MipsDebugger dbg(this);
    dbg.Debug();
  }
}


// Stop helper functions.
bool Simulator::IsWatchpoint(uint32_t code) {
  return (code <= kMaxWatchpointCode);
}


void Simulator::PrintWatchpoint(uint32_t code) {
  MipsDebugger dbg(this);
  ++break_count_;
  PrintF("\n---- break %d marker: %3d  (instr count: %8d) ----------"
         "----------------------------------",
         code, break_count_, icount_);
  dbg.PrintAllRegs();  // Print registers and continue running.
}


void Simulator::HandleStop(uint32_t code, Instruction* instr) {
  // Stop if it is enabled, otherwise go on jumping over the stop
  // and the message address.
  if (IsEnabledStop(code)) {
    MipsDebugger dbg(this);
    dbg.Stop(instr);
  } else {
    set_pc(get_pc() + 2 * Instruction::kInstrSize);
  }
}


bool Simulator::IsStopInstruction(Instruction* instr) {
  int32_t func = instr->FunctionFieldRaw();
  uint32_t code = static_cast<uint32_t>(instr->Bits(25, 6));
  return (func == BREAK) && code > kMaxWatchpointCode && code <= kMaxStopCode;
}


bool Simulator::IsEnabledStop(uint32_t code) {
  ASSERT(code <= kMaxStopCode);
  ASSERT(code > kMaxWatchpointCode);
  return !(watched_stops[code].count & kStopDisabledBit);
}


void Simulator::EnableStop(uint32_t code) {
  if (!IsEnabledStop(code)) {
    watched_stops[code].count &= ~kStopDisabledBit;
  }
}


void Simulator::DisableStop(uint32_t code) {
  if (IsEnabledStop(code)) {
    watched_stops[code].count |= kStopDisabledBit;
  }
}


void Simulator::IncreaseStopCounter(uint32_t code) {
  ASSERT(code <= kMaxStopCode);
  if ((watched_stops[code].count & ~(1 << 31)) == 0x7fffffff) {
    PrintF("Stop counter for code %i has overflowed.\n"
           "Enabling this code and reseting the counter to 0.\n", code);
    watched_stops[code].count = 0;
    EnableStop(code);
  } else {
    watched_stops[code].count++;
  }
}


// Print a stop status.
void Simulator::PrintStopInfo(uint32_t code) {
  if (code <= kMaxWatchpointCode) {
    PrintF("That is a watchpoint, not a stop.\n");
    return;
  } else if (code > kMaxStopCode) {
    PrintF("Code too large, only %u stops can be used\n", kMaxStopCode + 1);
    return;
  }
  const char* state = IsEnabledStop(code) ? "Enabled" : "Disabled";
  int32_t count = watched_stops[code].count & ~kStopDisabledBit;
  // Don't print the state of unused breakpoints.
  if (count != 0) {
    if (watched_stops[code].desc) {
      PrintF("stop %i - 0x%x: \t%s, \tcounter = %i, \t%s\n",
             code, code, state, count, watched_stops[code].desc);
    } else {
      PrintF("stop %i - 0x%x: \t%s, \tcounter = %i\n",
             code, code, state, count);
    }
  }
}


void Simulator::SignalExceptions() {
  for (int i = 1; i < kNumExceptions; i++) {
    if (exceptions[i] != 0) {
      V8_Fatal(__FILE__, __LINE__, "Error: Exception %i raised.", i);
    }
  }
}


// Handle execution based on instruction types.

void Simulator::ConfigureTypeRegister(Instruction* instr,
                                      int32_t& alu_out,
                                      int64_t& i64hilo,
                                      uint64_t& u64hilo,
                                      int32_t& next_pc,
                                      bool& do_interrupt) {
  // Every local variable declared here needs to be const.
  // This is to make sure that changed values are sent back to
  // DecodeTypeRegister correctly.

  // Instruction fields.
  const Opcode   op     = instr->OpcodeFieldRaw();
  const int32_t  rs_reg = instr->RsValue();
  const int32_t  rs     = get_register(rs_reg);
  const uint32_t rs_u   = static_cast<uint32_t>(rs);
  const int32_t  rt_reg = instr->RtValue();
  const int32_t  rt     = get_register(rt_reg);
  const uint32_t rt_u   = static_cast<uint32_t>(rt);
  const int32_t  rd_reg = instr->RdValue();
  const uint32_t sa     = instr->SaValue();

  const int32_t  fs_reg = instr->FsValue();


  // ---------- Configuration.
  switch (op) {
    case COP1:    // Coprocessor instructions.
      switch (instr->RsFieldRaw()) {
        case BC1:   // Handled in DecodeTypeImmed, should never come here.
          UNREACHABLE();
          break;
        case CFC1:
          // At the moment only FCSR is supported.
          ASSERT(fs_reg == kFCSRRegister);
          alu_out = FCSR_;
          break;
        case MFC1:
          alu_out = get_fpu_register(fs_reg);
          break;
        case MFHC1:
          UNIMPLEMENTED_MIPS();
          break;
        case CTC1:
        case MTC1:
        case MTHC1:
          // Do the store in the execution step.
          break;
        case S:
        case D:
        case W:
        case L:
        case PS:
          // Do everything in the execution step.
          break;
        default:
          UNIMPLEMENTED_MIPS();
      };
      break;
    case SPECIAL:
      switch (instr->FunctionFieldRaw()) {
        case JR:
        case JALR:
          next_pc = get_register(instr->RsValue());
          break;
        case SLL:
          alu_out = rt << sa;
          break;
        case SRL:
          if (rs_reg == 0) {
            // Regular logical right shift of a word by a fixed number of
            // bits instruction. RS field is always equal to 0.
            alu_out = rt_u >> sa;
          } else {
            // Logical right-rotate of a word by a fixed number of bits. This
            // is special case of SRL instruction, added in MIPS32 Release 2.
            // RS field is equal to 00001.
            alu_out = (rt_u >> sa) | (rt_u << (32 - sa));
          }
          break;
        case SRA:
          alu_out = rt >> sa;
          break;
        case SLLV:
          alu_out = rt << rs;
          break;
        case SRLV:
          if (sa == 0) {
            // Regular logical right-shift of a word by a variable number of
            // bits instruction. SA field is always equal to 0.
            alu_out = rt_u >> rs;
          } else {
            // Logical right-rotate of a word by a variable number of bits.
            // This is special case od SRLV instruction, added in MIPS32
            // Release 2. SA field is equal to 00001.
            alu_out = (rt_u >> rs_u) | (rt_u << (32 - rs_u));
          }
          break;
        case SRAV:
          alu_out = rt >> rs;
          break;
        case MFHI:
          alu_out = get_register(HI);
          break;
        case MFLO:
          alu_out = get_register(LO);
          break;
        case MULT:
          i64hilo = static_cast<int64_t>(rs) * static_cast<int64_t>(rt);
          break;
        case MULTU:
          u64hilo = static_cast<uint64_t>(rs_u) * static_cast<uint64_t>(rt_u);
          break;
        case ADD:
          if (HaveSameSign(rs, rt)) {
            if (rs > 0) {
              exceptions[kIntegerOverflow] = rs > (Registers::kMaxValue - rt);
            } else if (rs < 0) {
              exceptions[kIntegerUnderflow] = rs < (Registers::kMinValue - rt);
            }
          }
          alu_out = rs + rt;
          break;
        case ADDU:
          alu_out = rs + rt;
          break;
        case SUB:
          if (!HaveSameSign(rs, rt)) {
            if (rs > 0) {
              exceptions[kIntegerOverflow] = rs > (Registers::kMaxValue + rt);
            } else if (rs < 0) {
              exceptions[kIntegerUnderflow] = rs < (Registers::kMinValue + rt);
            }
          }
          alu_out = rs - rt;
          break;
        case SUBU:
          alu_out = rs - rt;
          break;
        case AND:
          alu_out = rs & rt;
          break;
        case OR:
          alu_out = rs | rt;
          break;
        case XOR:
          alu_out = rs ^ rt;
          break;
        case NOR:
          alu_out = ~(rs | rt);
          break;
        case SLT:
          alu_out = rs < rt ? 1 : 0;
          break;
        case SLTU:
          alu_out = rs_u < rt_u ? 1 : 0;
          break;
        // Break and trap instructions.
        case BREAK:

          do_interrupt = true;
          break;
        case TGE:
          do_interrupt = rs >= rt;
          break;
        case TGEU:
          do_interrupt = rs_u >= rt_u;
          break;
        case TLT:
          do_interrupt = rs < rt;
          break;
        case TLTU:
          do_interrupt = rs_u < rt_u;
          break;
        case TEQ:
          do_interrupt = rs == rt;
          break;
        case TNE:
          do_interrupt = rs != rt;
          break;
        case MOVN:
        case MOVZ:
        case MOVCI:
          // No action taken on decode.
          break;
        case DIV:
        case DIVU:
          // div and divu never raise exceptions.
          break;
        default:
          UNREACHABLE();
      };
      break;
    case SPECIAL2:
      switch (instr->FunctionFieldRaw()) {
        case MUL:
          alu_out = rs_u * rt_u;  // Only the lower 32 bits are kept.
          break;
        case CLZ:
          alu_out = __builtin_clz(rs_u);
          break;
        default:
          UNREACHABLE();
      };
      break;
    case SPECIAL3:
      switch (instr->FunctionFieldRaw()) {
        case INS: {   // Mips32r2 instruction.
          // Interpret rd field as 5-bit msb of insert.
          uint16_t msb = rd_reg;
          // Interpret sa field as 5-bit lsb of insert.
          uint16_t lsb = sa;
          uint16_t size = msb - lsb + 1;
          uint32_t mask = (1 << size) - 1;
          alu_out = (rt_u & ~(mask << lsb)) | ((rs_u & mask) << lsb);
          break;
        }
        case EXT: {   // Mips32r2 instruction.
          // Interpret rd field as 5-bit msb of extract.
          uint16_t msb = rd_reg;
          // Interpret sa field as 5-bit lsb of extract.
          uint16_t lsb = sa;
          uint16_t size = msb + 1;
          uint32_t mask = (1 << size) - 1;
          alu_out = (rs_u & (mask << lsb)) >> lsb;
          break;
        }
        default:
          UNREACHABLE();
      };
      break;
    default:
      UNREACHABLE();
  };
}


void Simulator::DecodeTypeRegister(Instruction* instr) {
  // Instruction fields.
  const Opcode   op     = instr->OpcodeFieldRaw();
  const int32_t  rs_reg = instr->RsValue();
  const int32_t  rs     = get_register(rs_reg);
  const uint32_t rs_u   = static_cast<uint32_t>(rs);
  const int32_t  rt_reg = instr->RtValue();
  const int32_t  rt     = get_register(rt_reg);
  const uint32_t rt_u   = static_cast<uint32_t>(rt);
  const int32_t  rd_reg = instr->RdValue();

  const int32_t  fs_reg = instr->FsValue();
  const int32_t  ft_reg = instr->FtValue();
  const int32_t  fd_reg = instr->FdValue();
  int64_t  i64hilo = 0;
  uint64_t u64hilo = 0;

  // ALU output.
  // It should not be used as is. Instructions using it should always
  // initialize it first.
  int32_t alu_out = 0x12345678;

  // For break and trap instructions.
  bool do_interrupt = false;

  // For jr and jalr.
  // Get current pc.
  int32_t current_pc = get_pc();
  // Next pc
  int32_t next_pc = 0;

  // Setup the variables if needed before executing the instruction.
  ConfigureTypeRegister(instr,
                        alu_out,
                        i64hilo,
                        u64hilo,
                        next_pc,
                        do_interrupt);

  // ---------- Raise exceptions triggered.
  SignalExceptions();

  // ---------- Execution.
  switch (op) {
    case COP1:
      switch (instr->RsFieldRaw()) {
        case BC1:   // Branch on coprocessor condition.
          UNREACHABLE();
          break;
        case CFC1:
          set_register(rt_reg, alu_out);
        case MFC1:
          set_register(rt_reg, alu_out);
          break;
        case MFHC1:
          UNIMPLEMENTED_MIPS();
          break;
        case CTC1:
          // At the moment only FCSR is supported.
          ASSERT(fs_reg == kFCSRRegister);
          FCSR_ = registers_[rt_reg];
          break;
        case MTC1:
          FPUregisters_[fs_reg] = registers_[rt_reg];
          break;
        case MTHC1:
          UNIMPLEMENTED_MIPS();
          break;