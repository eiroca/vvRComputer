(* Copyright (C) 2020-2021 Enrico Croce - AGPL >= 3.0
*
* This program is free software: you can redistribute it and/or modify it under the terms of the
* GNU Affero General Public License as published by the Free Software Foundation, either version 3
* of the License, or (at your option) any later version.
*
* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
* even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License along with this program.
* If not, see <http://www.gnu.org/licenses/>.
*
*)
unit uCPU_8080;

{$mode ObjFPC}{$H+}

interface

uses
  uCPU,
  Classes, SysUtils;

type
  ReadIOCall = function(const address: byte): byte of object;
  WriteIOCall = procedure(const address: byte; const val: byte) of object;

const
  Flag_C = 0;
  Flag_P = 2;
  Flag_H = 4;
  Flag_Z = 6;
  Flag_S = 7;

type

  { TCPU_8080 }

  TCPU_8080 = class(TCPU_ClassA)
  protected
    // 8080 Registers
    A: iSize8;
    B: iSize8;
    C: iSize8;
    D: iSize8;
    E: iSize8;
    H: iSize8;
    L: iSize8;
  private
    // Emulator hooks
    FReadIO: ReadIOCall;
    FWriteIO: WriteIOCall;
  public
    property ReadIO: ReadIOCall read FReadIO write FReadIO;
    property WriteIO: WriteIOCall read FWriteIO write FWriteIO;
  protected
    procedure FlagsFromByte(const aByte: iSize8);
    function FlagsToByte: iSize8;
  protected
    procedure op_NotSup;
    procedure op_IlgJmp;
    procedure op_IlgCall;
    procedure op_IlgRet;
  protected
    procedure op_ACI;
    procedure op_ADC_A;
    procedure op_ADC_B;
    procedure op_ADC_C;
    procedure op_ADC_D;
    procedure op_ADC_E;
    procedure op_ADC_H;
    procedure op_ADC_L;
    procedure op_ADC_M;
    procedure op_ADD_A;
    procedure op_ADD_B;
    procedure op_ADD_C;
    procedure op_ADD_D;
    procedure op_ADD_E;
    procedure op_ADD_H;
    procedure op_ADD_L;
    procedure op_ADD_M;
    procedure op_ADI;
    procedure op_ANA_A;
    procedure op_ANA_B;
    procedure op_ANA_C;
    procedure op_ANA_D;
    procedure op_ANA_E;
    procedure op_ANA_H;
    procedure op_ANA_L;
    procedure op_ANA_M;
    procedure op_ANI;
    procedure op_CALL;
    procedure op_CC;
    procedure op_CNC;
    procedure op_CZ;
    procedure op_CNZ;
    procedure op_CP;
    procedure op_CPE;
    procedure op_CPO;
    procedure op_CM;
    procedure op_CMA;
    procedure op_CMC;
    procedure op_CMP_A;
    procedure op_CMP_B;
    procedure op_CMP_C;
    procedure op_CMP_D;
    procedure op_CMP_E;
    procedure op_CMP_H;
    procedure op_CMP_L;
    procedure op_CMP_M;
    procedure op_CPI;
    procedure op_DAA;
    procedure op_DAD_B;
    procedure op_DAD_D;
    procedure op_DAD_H;
    procedure op_DAD_SP;
    procedure op_DCR_A;
    procedure op_DCR_B;
    procedure op_DCR_C;
    procedure op_DCR_D;
    procedure op_DCR_E;
    procedure op_DCR_H;
    procedure op_DCR_L;
    procedure op_DCR_M;
    procedure op_DCX_B;
    procedure op_DCX_D;
    procedure op_DCX_H;
    procedure op_DCX_SP;
    procedure op_DI;
    procedure op_EI;
    procedure op_HLT;
    procedure op_IN;
    procedure op_OUT;
    procedure op_INR_A;
    procedure op_INR_B;
    procedure op_INR_C;
    procedure op_INR_D;
    procedure op_INR_E;
    procedure op_INR_H;
    procedure op_INR_L;
    procedure op_INR_M;
    procedure op_INX_B;
    procedure op_INX_D;
    procedure op_INX_H;
    procedure op_INX_SP;
    procedure op_JZ;
    procedure op_JC;
    procedure op_JM;
    procedure op_JP;
    procedure op_JMP;
    procedure op_JNC;
    procedure op_JNZ;
    procedure op_JPE;
    procedure op_JPO;
    procedure op_LDA;
    procedure op_LDAX_B;
    procedure op_LDAX_D;
    procedure op_LHLD;
    procedure op_LXI_B;
    procedure op_LXI_D;
    procedure op_LXI_H;
    procedure op_LXI_SP;
    procedure op_MOV;
    procedure op_MOV_AB;
    procedure op_MOV_AC;
    procedure op_MOV_AD;
    procedure op_MOV_AE;
    procedure op_MOV_AH;
    procedure op_MOV_AL;
    procedure op_MOV_AM;
    procedure op_MOV_BA;
    procedure op_MOV_BC;
    procedure op_MOV_BD;
    procedure op_MOV_BE;
    procedure op_MOV_BH;
    procedure op_MOV_BL;
    procedure op_MOV_BM;
    procedure op_MOV_CA;
    procedure op_MOV_CB;
    procedure op_MOV_CD;
    procedure op_MOV_CE;
    procedure op_MOV_CH;
    procedure op_MOV_CL;
    procedure op_MOV_CM;
    procedure op_MOV_DA;
    procedure op_MOV_DB;
    procedure op_MOV_DC;
    procedure op_MOV_DE;
    procedure op_MOV_DH;
    procedure op_MOV_DL;
    procedure op_MOV_DM;
    procedure op_MOV_EA;
    procedure op_MOV_EB;
    procedure op_MOV_EC;
    procedure op_MOV_ED;
    procedure op_MOV_EH;
    procedure op_MOV_EL;
    procedure op_MOV_EM;
    procedure op_MOV_HA;
    procedure op_MOV_HB;
    procedure op_MOV_HC;
    procedure op_MOV_HD;
    procedure op_MOV_HE;
    procedure op_MOV_HL;
    procedure op_MOV_HM;
    procedure op_MOV_LA;
    procedure op_MOV_LB;
    procedure op_MOV_LC;
    procedure op_MOV_LD;
    procedure op_MOV_LE;
    procedure op_MOV_LH;
    procedure op_MOV_LM;
    procedure op_MOV_MA;
    procedure op_MOV_MB;
    procedure op_MOV_MC;
    procedure op_MOV_MD;
    procedure op_MOV_ME;
    procedure op_MOV_MH;
    procedure op_MOV_ML;
    procedure op_MVI_A;
    procedure op_MVI_B;
    procedure op_MVI_C;
    procedure op_MVI_D;
    procedure op_MVI_E;
    procedure op_MVI_H;
    procedure op_MVI_L;
    procedure op_MVI_Mi;
    procedure op_NOP;
    procedure op_ORA_A;
    procedure op_ORA_B;
    procedure op_ORA_C;
    procedure op_ORA_D;
    procedure op_ORA_E;
    procedure op_ORA_H;
    procedure op_ORA_L;
    procedure op_ORA_M;
    procedure op_ORI;
    procedure op_PCHL;
    procedure op_POP_B;
    procedure op_POP_D;
    procedure op_POP_H;
    procedure op_POP_PSW;
    procedure op_PUSH_B;
    procedure op_PUSH_D;
    procedure op_PUSH_H;
    procedure op_PUSH_PSW;
    procedure op_RAL;
    procedure op_RAR;
    procedure op_RC;
    procedure op_RET;
    procedure op_RLC;
    procedure op_RM;
    procedure op_RNC;
    procedure op_RNZ;
    procedure op_RP;
    procedure op_RPE;
    procedure op_RPO;
    procedure op_RRC;
    procedure op_RST_0;
    procedure op_RST_1;
    procedure op_RST_2;
    procedure op_RST_3;
    procedure op_RST_4;
    procedure op_RST_5;
    procedure op_RST_6;
    procedure op_RST_7;
    procedure op_RZ;
    procedure op_SBB_A;
    procedure op_SBB_B;
    procedure op_SBB_C;
    procedure op_SBB_D;
    procedure op_SBB_E;
    procedure op_SBB_H;
    procedure op_SBB_L;
    procedure op_SBB_M;
    procedure op_SBI;
    procedure op_SHLD;
    procedure op_SPHL;
    procedure op_STA;
    procedure op_STAX_B;
    procedure op_STAX_D;
    procedure op_STC;
    procedure op_SUB_A;
    procedure op_SUB_B;
    procedure op_SUB_C;
    procedure op_SUB_D;
    procedure op_SUB_E;
    procedure op_SUB_H;
    procedure op_SUB_L;
    procedure op_SUB_M;
    procedure op_SUI;
    procedure op_XCHG;
    procedure op_XRA_A;
    procedure op_XRA_B;
    procedure op_XRA_C;
    procedure op_XRA_D;
    procedure op_XRA_E;
    procedure op_XRA_H;
    procedure op_XRA_L;
    procedure op_XRA_M;
    procedure op_XRI;
    procedure op_XTHL;
  public
    constructor Create;
  public
    procedure Reset(); override;
  protected
    procedure UpdateCPUInfo(); override;
    procedure UpdateCPUStatus(fillExtra: boolean = True); override;
    procedure DoIRQ(int: integer); override;
  end;

implementation

constructor TCPU_8080.Create;
begin
  inherited;
  SetLength(OpCodes, 256);
  {$include CPUTable_8080.inc}
end;

procedure TCPU_8080.Reset;
begin
  A := 0;
  B := 0;
  C := 0;
  D := 0;
  E := 0;
  H := 0;
  L := 0;
  SP := 0;
  PC := 0;
  FlagsFromByte(0);
  FHalted := False;
  FIRQAllowed := False;
  opers := 0;
  cycles := 0;
  states := 0;
end;

procedure TCPU_8080.UpdateCPUStatus(fillExtra: boolean = True);
var
  i: integer;
begin;
  with FStatus do begin
    regs[0] := A;
    regs[1] := B;
    regs[2] := C;
    regs[3] := D;
    regs[4] := E;
    regs[5] := H;
    regs[6] := L;
    regs[7] := PC;
    regs[8] := SP;
    for i := 0 to 7 do begin
      flags[i] := F[i];
    end;
    if (fillExtra) then begin
      extras[0] := ReadMem(PC);
      extras[1] := ReadMem((PC + 1) and $FFFF);
      extras[2] := ReadMem((PC + 2) and $FFFF);
      extras[3] := ReadMem(B shl 8 + C);
      extras[4] := ReadMem(D shl 8 + E);
      extras[5] := ReadMem(H shl 8 + L);
      extras[6] := ReadMem(SP) + ReadMem((SP + 1) and $FFFF) shl 8;
    end;
  end;
end;

procedure TCPU_8080.UpdateCPUInfo;
begin;
  with FInfo do begin
    dataSize := 8;
    addrSize := 16;
    PCreg := 7;
    numRegs := 9;
    regsName := ['A', 'B', 'C', 'D', 'E', 'H', 'L', 'PC', 'SP'];
    regsSize := [1, 1, 1, 1, 1, 1, 1, 2, 2];
    numFlags := 8;
    flagsName := ['C', '-', 'P', '-', 'H', '-', 'Z', 'S'];
    numExtras := 7;
    extrasName := ['(PC)', '(PC+1)', '(PC+2)', '(BC)', '(DE)', '(HL)', '(SP)'];
    extrasSize := [1, 1, 1, 1, 1, 1, 2];
    littleEndian := True;
    numIRQs := 8;
    IRQsName := ['Reset', 'RST_1', 'RST_2', 'RST_3', 'RST_4', 'RST_5', 'RST_6', 'RST_7'];
    IRQsNMI := [True, False, False, False, False, False, False, False];
  end;
end;

procedure TCPU_8080.FlagsFromByte(const aByte: iSize8);
var
  i: integer;
begin
  for i := 0 to 7 do begin
    F[i] := (aByte and bit_val[i]) <> 0;
  end;
end;

function TCPU_8080.FlagsToByte: iSize8;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 7 do begin
    if F[i] then Result := Result or bit_val[i];
  end;
end;

procedure TCPU_8080.DoIRQ(int: integer);
begin
  inherited;
  case (int) of
    0: op_RST_0;
    1: op_RST_1;
    2: op_RST_2;
    3: op_RST_3;
    4: op_RST_4;
    5: op_RST_5;
    6: op_RST_6;
    7: op_RST_7;
  end;
end;

procedure TCPU_8080.op_NotSup;
begin
  op_NOP;
end;

procedure TCPU_8080.op_IlgJmp;
begin
  op_JMP;
end;

procedure TCPU_8080.op_IlgCall;
begin
  op_CALL;
end;

procedure TCPU_8080.op_IlgRet;
begin
  op_RET;
end;

procedure TCPU_8080.op_NOP;
begin
  // No Operation
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_HLT;
begin
  PC := (PC - 1) and $FFFF;
  FHalted := True;
  Inc(cycles);
  Inc(states, 7);
  OnHalt;
end;

procedure TCPU_8080.op_IN;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  if Assigned(ReadIO) then  A := ReadIO(b1);
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_OUT;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  if Assigned(WriteIO) then  WriteIO(b1, A);
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_CMC;
begin
  F[Flag_C] := not F[Flag_C];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_STC;
begin
  F[Flag_C] := True;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_DI;
begin
  FIRQAllowed := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_EI;
begin
  FIRQAllowed := True;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_JMP;
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := b1 + b2 shl 8;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_CALL;
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := b1 + b2 shl 8;
  Inc(cycles, 5);
  Inc(states, 17);
end;

procedure TCPU_8080.op_RET;
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  b2 := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  PC := b1 + b2 shl 8;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_RST_0;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0000;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_RST_1;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0008;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_RST_2;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0010;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_RST_3;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0018;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_RST_4;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0020;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_RST_5;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0028;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_RST_6;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0030;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_RST_7;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0038;
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_JC;
var
  b1, b2: iSize8;
begin
  if (F[Flag_C]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_JNC;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_C]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_JZ;
var
  b1, b2: iSize8;
begin
  if (F[Flag_Z]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_JNZ;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_Z]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_JM;
var
  b1, b2: iSize8;
begin
  if (F[Flag_S]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_JP;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_S]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_JPE;
var
  b1, b2: iSize8;
begin
  if (F[Flag_P]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_JPO;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_P]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_CZ;
begin
  if (F[Flag_Z]) then begin
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := ReadMem((PC + 1) and $FFFF) shl 8 + ReadMem(PC);
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_CNZ;
begin
  if (not F[Flag_Z]) then begin
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := ReadMem((PC + 1) and $FFFF) shl 8 + ReadMem(PC);
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_CC;
var
  b1, b2: iSize8;
begin
  if (F[Flag_Z]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := b1 + b2 shl 8;
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_CNC;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_C]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := b1 + b2 shl 8;
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_CPE;
var
  b1, b2: iSize8;
begin
  if (F[Flag_P]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := b1 + b2 shl 8;
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_CPO;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_P]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := b1 + b2 shl 8;
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_CM;
var
  b1, b2: iSize8;
begin
  if (F[Flag_S]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := b1 + b2 shl 8;
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_CP;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_S]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := b1 + b2 shl 8;
    Inc(cycles, 5);
    Inc(states, 17);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 3);
    Inc(states, 11);
  end;
end;

procedure TCPU_8080.op_RM;
var
  b1, b2: iSize8;
begin
  if (F[Flag_S]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_RPE;
var
  b1, b2: iSize8;
begin
  if (F[Flag_P]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_RC;
var
  b1, b2: iSize8;
begin
  if (F[Flag_C]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_RNC;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_C]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_RZ;
var
  b1, b2: iSize8;
begin
  if (F[Flag_Z]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_RNZ;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_Z]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_RPO;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_P]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_RP;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_S]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 11);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 5);
  end;
end;

procedure TCPU_8080.op_ADC_A;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + A + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor A xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADC_B;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + B + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor B xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADC_C;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + C + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor C xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADC_D;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + D + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor D xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADC_E;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + E + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor E xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADC_H;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + H + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor H xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADC_L;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + L + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor L xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADC_M;
var
  HL: iSize16;
  t, b1, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A + b1 + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_ACI;
var
  t, b1, cy: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  cy := bool_bit[F[Flag_C]];
  t := A + b1 + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_ADD_A;
var
  t: iSize8;
begin
  // Add A to A
  t := A + A;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor A xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADD_B;
var
  t: iSize8;
begin
  // Add B to A
  t := A + B;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor B xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADD_C;
var
  t: iSize8;
begin
  // Add C to A
  t := A + C;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor C xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADD_D;
var
  t: iSize8;
begin
  // Add D to A
  t := A + D;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor D xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADD_E;
var
  t: iSize8;
begin
  // Add E to A
  t := A + E;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor E xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADD_H;
var
  t: iSize8;
begin
  // Add H to A
  t := A + H;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor H xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADD_L;
var
  t: iSize8;
begin
  // Add L to A
  t := A + L;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor L xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ADD_M;
var
  HL: iSize16;
  b1, t: iSize8;
begin
  // Add (HL) to A
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A + b1;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_ADI;
var
  b1: iSize8;
  t: iSize8;
begin
  // Add immediate to A
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  t := A + b1;
  F[Flag_C] := (t > $FF);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_DAD_B;
var
  BC: iSize16;
  HL: iSize16;
begin
  // Adds BC to HL
  BC := B shl 8 + C;
  HL := H shl 8 + L;
  HL := HL + BC;
  F[Flag_C] := HL > $FFFF;
  H := (HL shr 8) and $FF;
  L := HL and $FF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_DAD_D;
var
  DE: iSize16;
  HL: iSize16;
begin
  // Adds DE to HL
  DE := D shl 8 + E;
  HL := H shl 8 + L;
  HL := HL + DE;
  F[Flag_C] := (HL > $FFFF);
  H := (HL shr 8) and $FF;
  L := HL and $FF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_DAD_H;
var
  HL: iSize16;
begin
  // Adds HL to HL
  HL := H shl 8 + L;
  HL := HL + HL;
  F[Flag_C] := (HL > $FFFF);
  H := (HL shr 8) and $FF;
  L := HL and $FF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_DAD_SP;
var
  HL: iSize16;
begin
  // Adds SP to HL
  HL := H shl 8 + L;
  HL := HL + SP;
  F[Flag_C] := (HL > $FFFF);
  H := (HL shr 8) and $FF;
  L := HL and $FF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_INR_A;
begin
  // Increments A
  A := (A + 1) and $FF;
  F[Flag_H] := (A and $0F) = $00;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INR_B;
begin
  // Increments B
  B := (B + 1) and $FF;
  F[Flag_H] := (B and $0F) = $00;
  F[Flag_Z] := (B = $00);
  F[Flag_P] := parity_of_bits[B];
  F[Flag_S] := (B and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INR_C;
begin
  // Increments C
  C := (C + 1) and $FF;
  F[Flag_H] := (C and $0F) = $00;
  F[Flag_Z] := (C = $00);
  F[Flag_P] := parity_of_bits[C];
  F[Flag_S] := (C and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INR_D;
begin
  // Increments D
  D := (D + 1) and $FF;
  F[Flag_H] := (D and $0F) = $00;
  F[Flag_Z] := (D = $00);
  F[Flag_P] := parity_of_bits[D];
  F[Flag_S] := (D and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INR_E;
begin
  // Increments E
  E := (E + 1) and $FF;
  F[Flag_H] := (E and $0F) = $00;
  F[Flag_Z] := (E = $00);
  F[Flag_P] := parity_of_bits[E];
  F[Flag_S] := (E and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INR_H;
begin
  // Increments H
  H := (H + 1) and $FF;
  F[Flag_H] := (H and $0F) = $00;
  F[Flag_Z] := (H = $00);
  F[Flag_P] := parity_of_bits[H];
  F[Flag_S] := (H and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INR_L;
begin
  // Increments L
  L := (L + 1) and $FF;
  F[Flag_H] := (L and $0F) = $00;
  F[Flag_Z] := (L = $00);
  F[Flag_P] := parity_of_bits[L];
  F[Flag_S] := (L and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INR_M;
var
  HL: iSize16;
  t: iSize8;
begin
  // Increment Memory
  HL := H shl 8 + L;
  t := (ReadMem(HL) + 1) and $FF;
  WriteMem(HL, t);
  F[Flag_H] := (t and $0F) = $00;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_H] := (t and $0F) = $0;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_INX_B;
begin
  // Increments BC
  Inc(C);
  if (C > $FF) then begin
    C := $00;
    B := (B + 1) and $FF;
  end;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INX_D;
begin
  // Increments DE
  Inc(E);
  if (E > $FF) then begin
    E := $00;
    D := (D + 1) and $FF;
  end;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INX_H;
begin
  // Increments HL
  Inc(L);
  if (L > $FF) then begin
    L := $00;
    H := (H + 1) and $FF;
  end;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_INX_SP;
begin
  // Increments SP
  SP := ((SP + 1) and $FFFF);
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_SUB_A;
begin
  A := 0;
  F[Flag_Z] := True;
  F[Flag_C] := False;
  F[Flag_P] := False;
  F[Flag_S] := False;
  F[Flag_H] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SUB_B;
var
  t: iSize8;
begin
  t := A - B;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor B xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SUB_C;
var
  t: iSize8;
begin
  t := A - C;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor C xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SUB_D;
var
  t: iSize8;
begin
  t := A - D;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor D xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SUB_E;
var
  t: iSize8;
begin
  t := A - E;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor E xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SUB_H;
var
  t: iSize8;
begin
  t := A - H;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor H xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SUB_L;
var
  t: iSize8;
begin
  t := A - L;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SUB_M;
var
  HL: iSize16;
  b1, t: iSize8;
begin
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A - b1;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor b1 xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_SUI;
var
  b1, t: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  t := A - b1;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor b1 xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_SBB_A;
var
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  A := -CY;
  F[Flag_C] := (cy <> 0);
  F[Flag_S] := (cy <> 0);
  F[Flag_H] := (cy = 0);
  F[Flag_Z] := (cy = 0);
  F[Flag_P] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SBB_B;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - B - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor B xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SBB_C;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - C - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor C xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SBB_D;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - D - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor D xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SBB_E;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - E - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor E xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SBB_H;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - H - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor H xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SBB_L;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - L - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SBB_M;
var
  HL: iSize16;
  t, b1, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A - b1 - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_SBI;
var
  t, b1, cy: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  cy := bool_bit[F[Flag_C]];
  t := A - b1 - CY;
  F[Flag_C] := (t < $00);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_DCR_A;
begin
  // Decrement A
  A := (A - 1) and $FF;
  F[Flag_H] := (A and $0F) = $F;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCR_B;
begin
  // Decrement B
  B := (B - 1) and $FF;
  F[Flag_H] := (B and $0F) = $0F;
  F[Flag_Z] := (B = $00);
  F[Flag_P] := parity_of_bits[B];
  F[Flag_S] := (B and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCR_C;
begin
  // Decrement C
  C := (C - 1) and $FF;
  F[Flag_H] := (C and $0F) = $0F;
  F[Flag_Z] := (C = $00);
  F[Flag_P] := parity_of_bits[C];
  F[Flag_S] := (C and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCR_D;
begin
  // Decrement D
  D := (D - 1) and $FF;
  F[Flag_H] := (D and $0F) = $0F;
  F[Flag_Z] := (D = $00);
  F[Flag_P] := parity_of_bits[D];
  F[Flag_S] := (D and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCR_E;
begin
  // Decrement E
  E := (E - 1) and $FF;
  F[Flag_H] := (E and $0F) = $0F;
  F[Flag_Z] := (E = $00);
  F[Flag_P] := parity_of_bits[E];
  F[Flag_S] := (E and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCR_H;
begin
  // Decrement H
  H := (H - 1) and $FF;
  F[Flag_H] := (H and $0F) = $0F;
  F[Flag_Z] := (H = $00);
  F[Flag_P] := parity_of_bits[H];
  F[Flag_S] := (H and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCR_L;
begin
  // Decrement L
  L := (L - 1) and $FF;
  F[Flag_H] := (L and $0F) = $0F;
  F[Flag_Z] := (L = $00);
  F[Flag_P] := parity_of_bits[L];
  F[Flag_S] := (L and $80) = $80;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCR_M;
var
  HL: iSize16;
  t: iSize8;
begin
  // Decrement Memmory
  HL := H shl 8 + L;
  t := (ReadMem(HL) - 1) and $FF;
  WriteMem(HL, t);
  F[Flag_H] := (t and $0F) = $0F;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_DCX_B;
begin
  // Decrement BC
  if (C = 0) then begin
    B := (B - 1) and $FF;
    C := $FF;
  end
  else begin
    Dec(C);
  end;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCX_D;
begin
  // Decrement DE
  if (E = 0) then begin
    D := (D - 1) and $FF;
    E := $FF;
  end
  else begin
    Dec(E);
  end;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCX_H;
begin
  // Decrement HL
  if (L = 0) then begin
    H := (H - 1) and $FF;
    L := $FF;
  end
  else begin
    Dec(L);
  end;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_DCX_SP;
begin
  // Decrement SP
  SP := ((SP - 1) and $FFFF);
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_ANA_A;
begin
  // A = A and A
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (A and $10) = $10;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ANA_B;
var
  t: iSize8;
begin
  A := A and B;
  t := A or B;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ANA_C;
var
  t: iSize8;
begin
  A := A and C;
  t := A or C;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ANA_D;
var
  t: iSize8;
begin
  A := A and D;
  t := A or D;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ANA_E;
var
  t: iSize8;
begin
  A := A and E;
  t := A or E;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ANA_H;
var
  t: iSize8;
begin
  A := A and H;
  t := A or H;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ANA_L;
var
  t: iSize8;
begin
  A := A and L;
  t := A or L;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ANA_M;
var
  HL: iSize16;
  t, b1: iSize8;
begin
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  A := A and b1;
  t := A or b1;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_ANI;
var
  t, b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  A := A and b1;
  t := A or b1;
  F[Flag_C] := False;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (t and $10) = $10;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_ORA_A;
begin
  // A = A | A
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ORA_B;
begin
  A := A or B;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ORA_C;
begin
  A := A or C;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ORA_D;
begin
  A := A or D;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ORA_E;
begin
  A := A or E;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ORA_H;
begin
  A := A or H;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ORA_L;
begin
  A := A or L;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_ORA_M;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := A or ReadMem(HL);
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_ORI;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  A := A or b1;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_XRA_A;
begin
  A := 0;
  F[Flag_Z] := True;
  F[Flag_P] := True;
  F[Flag_S] := False;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_XRA_B;
begin
  A := A xor B;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_XRA_C;
begin
  A := A xor C;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_XRA_D;
begin
  A := A xor D;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_XRA_E;
begin
  A := A xor E;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_XRA_H;
begin
  A := A xor H;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_XRA_L;
begin
  A := A xor L;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_XRA_M;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := A xor ReadMem(HL);
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_XRI;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  A := A xor b1;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_CMP_A;
begin
  F[Flag_Z] := True;
  F[Flag_P] := False;
  F[Flag_S] := False;
  F[Flag_H] := False;
  F[Flag_C] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_CMP_B;
var
  t: iSize8;
begin
  t := A - B;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_CMP_C;
var
  t: iSize8;
begin
  t := A - C;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor C) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_CMP_D;
var
  t: iSize8;
begin
  t := A - D;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor D) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_CMP_E;
var
  t: iSize8;
begin
  t := A - E;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor E) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_CMP_H;
var
  t: iSize8;
begin
  t := A - H;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor H) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_CMP_L;
var
  t: iSize8;
begin
  t := A - L;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor L) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_CMP_M;
var
  HL: iSize16;
  t, b1: iSize8;
begin
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A - b1;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor b1) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_CPI;
var
  b1: iSize8;
  t: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  t := A - b1;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor b1) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV;
begin
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_AB;
begin
  A := B;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_AC;
begin
  A := C;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_AD;
begin
  A := D;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_AE;
begin
  A := E;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_AH;
begin
  A := H;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_AL;
begin
  A := L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_AM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := ReadMem(HL);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_BA;
begin
  B := A;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_BC;
begin
  B := C;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_BD;
begin
  B := D;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_BE;
begin
  B := E;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_BH;
begin
  B := H;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_BL;
begin
  B := L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_BM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  B := ReadMem(HL);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_CA;
begin
  C := A;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_CB;
begin
  C := B;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_CD;
begin
  C := D;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_CE;
begin
  C := E;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_CH;
begin
  C := H;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_CL;
begin
  C := L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_CM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  C := ReadMem(HL);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_DA;
begin
  D := A;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_DB;
begin
  D := B;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_DC;
begin
  D := C;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_DE;
begin
  D := E;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_DH;
begin
  D := H;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_DL;
begin
  D := L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_DM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  D := ReadMem(HL);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_EA;
begin
  E := A;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_EB;
begin
  E := B;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_EC;
begin
  E := C;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_ED;
begin
  E := D;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_EH;
begin
  E := H;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_EL;
begin
  E := L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_EM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  E := ReadMem(HL);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_HA;
begin
  H := A;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_HB;
begin
  H := B;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_HC;
begin
  H := C;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_HD;
begin
  H := D;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_HE;
begin
  H := E;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_HL;
begin
  H := L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_HM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  H := ReadMem(HL);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_LA;
begin
  L := A;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_LB;
begin
  L := B;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_LC;
begin
  L := C;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_LD;
begin
  L := D;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_LE;
begin
  L := E;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_LH;
begin
  L := H;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_MOV_LM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  L := ReadMem(HL);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_MA;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, A);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_MB;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, B);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_MC;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, C);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_MD;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, D);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_ME;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, E);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_MH;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, H);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MOV_ML;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, L);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_A;
begin
  // Load A with an immediate
  A := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_B;
begin
  // Load B with an immediate
  B := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_C;
begin
  // Load C with an immediate
  C := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_D;
begin
  // Load D with an immediate
  D := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_E;
begin
  // Load E with an immediate
  E := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_H;
begin
  // Load H with an immediate
  H := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_L;
begin
  // Load L with an immediate
  L := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_MVI_Mi;
var
  HL: iSize16;
  b1: iSize8;
begin
  // Write byte to memory pointed by HL
  HL := H shl 8 + L;
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  WriteMem(HL, b1);
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_POP_PSW;
begin
  FlagsFromByte(ReadMem(SP));
  SP := (SP + 1) and $FFFF;
  A := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_POP_B;
begin
  C := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  B := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_POP_D;
begin
  E := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  D := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_POP_H;
begin
  L := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  H := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_PUSH_PSW;
begin
  SP := (SP - 1) and $FFFF;
  F[1] := True;
  F[3] := False;
  F[5] := False;
  WriteMem(SP, FlagsToByte());
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, A);
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_PUSH_B;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, B);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, C);
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_PUSH_D;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, E);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, D);
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_PUSH_H;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, L);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, H);
  Inc(cycles, 3);
  Inc(states, 11);
end;

procedure TCPU_8080.op_CMA;
begin
  A := (not A) and $FF;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_DAA;
var
  cy: boolean;
  correction: iSize8;
  lsb, msb: iSize8;
begin
  // Decimal Adjust Accumulator
  cy := F[Flag_C];
  correction := 0;
  lsb := A and $0F;
  msb := A shr 4;
  if (F[Flag_H] or (lsb > 9)) then  correction += $06;
  if (F[Flag_C] or (msb > 9) or ((msb >= 9) and (lsb > 9))) then begin
    correction += $60;
    cy := True;
  end;
  A := (A + correction) and $FF;
  F[Flag_Z] := A = 0;
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (A and $0F) = $00;
  F[Flag_C] := cy;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_RAL;
var
  b0: iSize8;
begin
  // Rotates A left
  b0 := bool_bit[F[Flag_C]];
  F[Flag_C] := (A and $80) <> 0;
  A := ((A shl 1) and $FF) or b0;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_RAR;
var
  b0: iSize8;
  b7: iSize8;
begin
  // Arithmetic rotate right A
  b0 := A and $01;
  b7 := bool_bit[F[Flag_C]] shl 7;
  A := (A shr 1) or b7;
  F[Flag_C] := b0 <> 0;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_RLC;
var
  b0: iSize8;
begin
  // Rotate A left with carry
  b0 := A shr 7;
  A := ((A shl 1) and $FF) or b0;
  F[Flag_C] := b0 <> 0;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_RRC;
var
  b0: iSize8;
begin
  // Rotate A right with carry
  b0 := (A and $01);
  A := (A shr 1);
  if (b0 <> 0) then begin
    F[Flag_C] := True;
    A := A or $80;
  end
  else begin
    F[Flag_C] := False;
  end;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_LXI_B;
begin
  // Load BC with a 16 bit immediate
  C := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  B := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_LXI_D;
begin
  // Load DE with a 16 bit immediate
  E := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  D := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_LXI_H;
begin
  // Load HL with a 16 bit immediate
  L := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  H := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_LXI_SP;
var
  b1, b2: iSize8;
begin
  // Load HL with a 16 bit immediate
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  SP := b2 shl 8 + b1;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_LDA;
var
  b1, b2: iSize8;
begin
  // Load A from memory
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  A := ReadMem(b2 shl 8 + b1);
  Inc(cycles, 4);
  Inc(states, 13);
end;

procedure TCPU_8080.op_LDAX_B;
var
  BC: iSize16;
begin
  // Load A with memory pointed by BC
  BC := B shl 8 + C;
  A := ReadMem(BC);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_LDAX_D;
var
  DE: iSize16;
begin
  // Load A with memory pointed by DE
  DE := D shl 8 + E;
  A := ReadMem(DE);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_LHLD;
var
  b1, b2: iSize8;
  adr: iSize16;
begin
  // Load HL from immediate address
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  adr := b2 shl 8 + b1;
  L := ReadMem(adr);
  H := ReadMem((adr + 1) and $FFFF);
  Inc(cycles, 5);
  Inc(states, 16);
end;

procedure TCPU_8080.op_STA;
var
  b1, b2: iSize8;
begin
  // Store A in memory
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  WriteMem(b2 shl 8 + b1, A);
  Inc(cycles, 4);
  Inc(states, 13);
end;

procedure TCPU_8080.op_STAX_B;
var
  BC: iSize16;
begin
  // Store A in memory pointed by BC
  BC := B shl 8 + C;
  WriteMem(BC, A);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_STAX_D;
var
  DE: iSize16;
begin
  // Store A in memory pointed by DE
  DE := D shl 8 + E;
  WriteMem(DE, A);
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8080.op_SHLD;
var
  b1, b2: iSize8;
  adr: iSize16;
begin
  // Write HL in memory
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  adr := b2 shl 8 + b1;
  WriteMem(adr, L);
  WriteMem((adr + 1) and $FFFF, H);
  Inc(cycles, 5);
  Inc(states, 16);
end;

procedure TCPU_8080.op_XCHG;
var t: iSize8;
begin
  t := H;
  H := D;
  D := t;
  t := L;
  L := E;
  E := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_SPHL;
begin
  SP := H shl 8 + L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_PCHL;
begin
  PC := H shl 8 + L;
  Inc(cycles);
  Inc(states, 5);
end;

procedure TCPU_8080.op_XTHL;
var
  b1: iSize8;
  b2: iSize8;
begin
  b1 := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  b2 := ReadMem(SP);
  WriteMem(SP, H);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, L);
  L := b1;
  H := b2;
  Inc(cycles, 5);
  Inc(states, 18);
end;

end.
