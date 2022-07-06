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
  // f0, f1, f2, ... f3