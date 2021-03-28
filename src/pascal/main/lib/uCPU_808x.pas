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
unit uCPU_808x;

{$mode ObjFPC}{$H+}

interface

uses
  uCPU,
  Classes, SysUtils;

type
  ReadIOCall = function(const address: byte): byte of object;
  WriteIOCall = procedure(const address: byte; const val: byte) of object;

type
  // 8085 additional serial I/O line
  ReadSerialCall = function(): boolean of object;
  WriteSerialCall = procedure(d: boolean) of object;


const
  Flag_C = 0;
  Flag_V = 1; // 8085 only
  Flag_U1 = 1; // 8085 only
  Flag_P = 2;
  Flag_U2 = 3;
  Flag_A = 4;
  Flag_K = 5; // 8085 only
  Flag_U3 = 5; // 8085 only
  Flag_Z = 6;
  Flag_S = 7;

const
  IRQ_RST75 = 1;
  IRQ_RST65 = 2;
  IRQ_RST55 = 3;

type

  { TCPU_8080 }

  TCPU_8080 = class(TCPU_ClassA)
  protected
    procedure internal_call(const c: integer); inline;
    procedure push_PC(); inline;
    procedure pull_PC(); inline;
    procedure push(v: iSize8); inline;
    function pull(): iSize8; inline;
  protected
    function addr_abs(): iSize16; inline;
    function data_abs(): iSize8; inline;
  protected
    procedure UpdateZPS(const t: iSize8); inline;
    procedure UpdateFlag_INC(const t: iSize8); inline;
    procedure UpdateFlag_DCR(const t: iSize8); inline;
    procedure UpdateFlag_OR(const v1: iSize8); inline;
    procedure UpdateFlag_AND(const v1, v2, v3: iSize8); inline;
    procedure UpdateFlag_SUB(var v1: iSize8; const v2, v3: iSize8); inline;
    procedure UpdateFlag_ADD(var v1: iSize8; const v2, v3: iSize8); inline;
  protected
    // 8080 Registers
    A: iSize8;
    B: iSize8;
    C: iSize8;
    D: iSize8;
    E: iSize8;
    H: iSize8;
    L: iSize8;
    SP: iSize16;
  private
    // Emulator hooks
    FReadIO: ReadIOCall;
    FWriteIO: WriteIOCall;
    procedure writeBC(const BC: iSize16);
    procedure writeDE(const DE: iSize16);
    procedure writeHL(const HL: iSize16);
    function readBC: iSize16;
    function readDE: iSize16;
    function readHL: iSize16;
  public
    property rA: iSize8 read A write A;
    property rB: iSize8 read B write B;
    property rC: iSize8 read C write C;
    property rD: iSize8 read D write D;
    property rE: iSize8 read E write E;
    property rH: iSize8 read H write H;
    property rL: iSize8 read L write L;
    property rSP: iSize16 read SP write SP;
    property rBC: iSize16 read readBC write writeBC;
    property rDE: iSize16 read readDE write writeDE;
    property rHL: iSize16 read readHL write writeHL;
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
    procedure op_ADI;
    procedure op_ADD_A;
    procedure op_ADD_B;
    procedure op_ADD_C;
    procedure op_ADD_D;
    procedure op_ADD_E;
    procedure op_ADD_H;
    procedure op_ADD_L;
    procedure op_ADD_M;
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
    constructor Create(allowIllegal: boolean = True);
  public
    procedure Reset(); override;
    procedure StackPush(w: iSize16);
    function StackPull: iSize16;
  protected
    procedure UpdateCPUInfo(); override;
    procedure UpdateCPUStatus(fillExtra: boolean = True); override;
    procedure DoIRQ(int: integer); override;
  end;

  { TCPU_8085 }

  TCPU_8085 = class(TCPU_8080)
  private
    procedure UpdateFlag_INC(const v1: iSize8); inline;
    procedure UpdateFlag_DCR(const v1: iSize8); inline;
    procedure UpdateFlag_OR(const v1: iSize8); inline;
    procedure UpdateFlag_AND(const v1: iSize8); inline;
    procedure UpdateFlag_CMP(var v1: iSize8; const v2, v3: iSize8); inline;
    procedure UpdateFlag_SUB(var v1: iSize8; const v2, v3: iSize8); inline;
    procedure UpdateFlag_ADD(var v1: iSize8; const v2, v3: iSize8); inline;
  private
    // Emulator hooks
    FReadSerial: ReadSerialCall;
    FWriteSerial: WriteSerialCall;
  public
    property ReadSerial: ReadSerialCall read FReadSerial write FReadSerial;
    property WriteSerial: WriteSerialCall read FWriteSerial write FWriteSerial;
  protected
    procedure op_DSUB;
    procedure op_ARHL;
    procedure op_RDEL;
    procedure op_RIM;
    procedure op_SIM;
    procedure op_LDHI;
    procedure op_LDSI;
    procedure op_LHLX;
    procedure op_SHLX;
    procedure op_RSTV;
    procedure op_JNK;
    procedure op_JK;
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
    procedure op_CMA;
    procedure op_CMP_A;
    procedure op_CMP_B;
    procedure op_CMP_C;
    procedure op_CMP_D;
    procedure op_CMP_E;
    procedure op_CMP_H;
    procedure op_CMP_L;
    procedure op_CMP_M;
    procedure op_CPI;
    procedure op_CC;
    procedure op_CNC;
    procedure op_CZ;
    procedure op_CNZ;
    procedure op_CP;
    procedure op_CPE;
    procedure op_CPO;
    procedure op_CM;
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
    procedure op_HLT;
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
    procedure op_JC;
    procedure op_JM;
    procedure op_JNZ;
    procedure op_JP;
    procedure op_JPE;
    procedure op_JPO;
    procedure op_JZ;
    procedure op_JNC;
    procedure op_ORA_A;
    procedure op_ORA_B;
    procedure op_ORA_C;
    procedure op_ORA_D;
    procedure op_ORA_E;
    procedure op_ORA_H;
    procedure op_ORA_L;
    procedure op_ORA_M;
    procedure op_ORI;
    procedure op_RAL;
    procedure op_RAR;
    procedure op_RLC;
    procedure op_RM;
    procedure op_RC;
    procedure op_RNC;
    procedure op_RZ;
    procedure op_RNZ;
    procedure op_RP;
    procedure op_RPE;
    procedure op_RPO;
    procedure op_RRC;
    procedure op_RST_75;
    procedure op_RST_65;
    procedure op_RST_55;
    procedure op_RST_45;
    procedure op_SBB_A;
    procedure op_SBB_B;
    procedure op_SBB_C;
    procedure op_SBB_D;
    procedure op_SBB_E;
    procedure op_SBB_H;
    procedure op_SBB_L;
    procedure op_SBB_M;
    procedure op_SBI;
    procedure op_SUB_A;
    procedure op_SUB_B;
    procedure op_SUB_C;
    procedure op_SUB_D;
    procedure op_SUB_E;
    procedure op_SUB_H;
    procedure op_SUB_L;
    procedure op_SUB_M;
    procedure op_SUI;
    procedure op_XRA_A;
    procedure op_XRA_B;
    procedure op_XRA_C;
    procedure op_XRA_D;
    procedure op_XRA_E;
    procedure op_XRA_H;
    procedure op_XRA_L;
    procedure op_XRA_M;
    procedure op_XRI;
  public
    constructor Create(allowIllegal: boolean = True);
  protected
    procedure UpdateCPUInfo(); override;
    procedure DoIRQ(int: integer); override;
  end;

implementation

constructor TCPU_8080.Create(allowIllegal: boolean = True);
begin
  inherited Create;
  SetLength(OpCodes, 256);
  {$include CPUTable_8080.inc}
end;

procedure TCPU_8080.Reset();
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
  FlagsFromByte(%00000010);
  state := active;
  FIRQAllowed := False;
  _opers := 0;
  _cycles := 0;
end;

procedure TCPU_8080.StackPush(w: iSize16);
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, (w shr 8) and $FF);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, w and $FF);
end;

function TCPU_8080.StackPull: iSize16;
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  b2 := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  Result := b1 + b2 shl 8;
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

procedure TCPU_8080.UpdateCPUInfo();
begin;
  with FInfo do begin
    dataSize := 8;
    addrSize := 16;
    PCreg := 7;
    numRegs := 9;
    regsName := ['A', 'B', 'C', 'D', 'E', 'H', 'L', 'PC', 'SP'];
    regsSize := [1, 1, 1, 1, 1, 1, 1, 2, 2];
    numFlags := 8;
    flagsName := ['c', '-', 'p', '-', 'a', '-', 'z', 's'];
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

procedure TCPU_8080.push_PC();
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
end;

procedure TCPU_8080.pull_PC();
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  b2 := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
  PC := b1 + b2 shl 8;
end;

procedure TCPU_8080.push(v: iSize8);
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, v);
end;

function TCPU_8080.pull(): iSize8;
begin
  Result := ReadMem(SP);
  SP := (SP + 1) and $FFFF;
end;

function TCPU_8080.addr_abs(): iSize16;
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Result := b1 + b2 shl 8;
end;

function TCPU_8080.data_abs(): iSize8;
var
  addr: iSize16;
begin
  addr := addr_abs();
  Result := ReadMem(addr);
end;

procedure TCPU_8080.DoIRQ(int: integer);
begin
  if (state = wait) then state := active;
  FIRQs[int].active := False;
  push_PC();
  case (int) of
    0: PC := $0000;
    1: PC := $0008;
    2: PC := $0010;
    3: PC := $0018;
    4: PC := $0020;
    5: PC := $0028;
    6: PC := $0030;
    7: PC := $0038;
    else begin
      PC := $0000;
    end;
  end;
end;

procedure TCPU_8080.internal_call(const c: integer);
var
  addr: iSize16;
begin
  addr := addr_abs();
  push_PC();
  PC := addr;
  Inc(_cycles, c);
end;

procedure TCPU_8080.UpdateZPS(const t: iSize8);
begin
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_bit[t];
  F[Flag_S] := (t and $80) <> 0;
end;

procedure TCPU_8080.UpdateFlag_INC(const t: iSize8);
begin
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_bit[t];
  F[Flag_S] := (t and $80) <> 0;
  F[Flag_A] := (t and $0F) = $00;
end;

procedure TCPU_8080.UpdateFlag_DCR(const t: iSize8);
begin
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_bit[t];
  F[Flag_S] := (t and $80) <> 0;
  F[Flag_A] := (t and $0F) <> $0F;
end;

procedure TCPU_8080.UpdateFlag_OR(const v1: iSize8);
begin
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_A] := False;
  F[Flag_C] := False;
end;

procedure TCPU_8080.UpdateFlag_AND(const v1, v2, v3: iSize8);
begin
  F[Flag_C] := False;
  F[Flag_A] := ((v2 or v3) and $08) <> 0;
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
end;

procedure TCPU_8080.UpdateFlag_SUB(var v1: iSize8; const v2, v3: iSize8);
begin
  F[Flag_C] := (v1 < 0);
  v1 := v1 and $FF;
  F[Flag_A] := ((v1 xor v2 xor v3) and $10) = 0;
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
end;

procedure TCPU_8080.UpdateFlag_ADD(var v1: iSize8; const v2, v3: iSize8);
begin
  F[Flag_C] := (v1 > $FF);
  v1 := v1 and $FF;
  F[Flag_A] := ((v1 xor v2 xor v3) and $10) <> 0;
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
end;

procedure TCPU_8080.writeBC(const BC: iSize16);
begin
  B := (BC shr 8) and $FF;
  C := BC and $FF;
end;

procedure TCPU_8080.writeDE(const DE: iSize16);
begin
  D := (DE shr 8) and $FF;
  E := DE and $FF;
end;

procedure TCPU_8080.writeHL(const HL: iSize16);
begin
  H := (HL shr 8) and $FF;
  L := HL and $FF;
end;

function TCPU_8080.readBC: iSize16;
begin
  Result := B shl 8 + C;
end;

function TCPU_8080.readDE: iSize16;
begin
  Result := D shl 8 + E;
end;

function TCPU_8080.readHL: iSize16;
begin
  Result := H shl 8 + L;
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
end;

procedure TCPU_8080.op_HLT;
begin
  PC := (PC - 1) and $FFFF;
  state := stop;
  if assigned(OnHalt) then OnHalt;
end;

procedure TCPU_8080.op_IN;
var
  MV: uint8;
begin
  MV := imm8();
  if Assigned(ReadIO) then  A := ReadIO(MV);
end;

procedure TCPU_8080.op_OUT;
var
  MV: uint8;
begin
  MV := imm8();
  if Assigned(WriteIO) then  WriteIO(MV, A);
end;

procedure TCPU_8080.op_CMC;
begin
  F[Flag_C] := not F[Flag_C];
end;

procedure TCPU_8080.op_STC;
begin
  F[Flag_C] := True;
end;

procedure TCPU_8080.op_DI;
begin
  FIRQAllowed := False;
end;

procedure TCPU_8080.op_EI;
begin
  FIRQAllowed := True;
end;

procedure TCPU_8080.op_JMP;
begin
  imm_PC();
end;

procedure TCPU_8080.op_CALL;
var
  addr: uint16;
begin
  addr := addr_abs();
  push_PC();
  PC := addr;
end;

procedure TCPU_8080.op_RET;
begin
  pull_PC();
end;

procedure TCPU_8080.op_RST_0;
begin
  push_PC();
  PC := $0000;
end;

procedure TCPU_8080.op_RST_1;
begin
  push_PC();
  PC := $0008;
end;

procedure TCPU_8080.op_RST_2;
begin
  push_PC();
  PC := $0010;
end;

procedure TCPU_8080.op_RST_3;
begin
  push_PC();
  PC := $0018;
end;

procedure TCPU_8080.op_RST_4;
begin
  push_PC();
  PC := $0020;
end;

procedure TCPU_8080.op_RST_5;
begin
  push_PC();
  PC := $0028;
end;

procedure TCPU_8080.op_RST_6;
begin
  push_PC();
  PC := $0030;
end;

procedure TCPU_8080.op_RST_7;
begin
  push_PC();
  PC := $0038;
end;

procedure TCPU_8080.op_JC;
begin
  if (F[Flag_C]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_JNC;
begin
  if (not F[Flag_C]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_JZ;
begin
  if (F[Flag_Z]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_JNZ;
begin
  if (not F[Flag_Z]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_JM;
begin
  if (F[Flag_S]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_JP;
begin
  if (not F[Flag_S]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_JPE;
begin
  if (F[Flag_P]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_JPO;
begin
  if (not F[Flag_P]) then begin
    imm_PC();
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CZ;
begin
  if (F[Flag_Z]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CNZ;
begin
  if (not F[Flag_Z]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CC;
begin
  if (F[Flag_C]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CNC;
begin
  if (not F[Flag_C]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CPE;
begin
  if (F[Flag_P]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CPO;
begin
  if (not F[Flag_P]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CM;
begin
  if (F[Flag_S]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_CP;
begin
  if (not F[Flag_S]) then begin
    internal_call(2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8080.op_RM;
begin
  if (F[Flag_S]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_RPE;
begin
  if (F[Flag_P]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_RC;
begin
  if (F[Flag_C]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_RNC;
begin
  if (not F[Flag_C]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_RZ;
begin
  if (F[Flag_Z]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_RNZ;
begin
  if (not F[Flag_Z]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_RPO;
begin
  if (not F[Flag_P]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_RP;
begin
  if (not F[Flag_S]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8080.op_ACI;
var
  t, v, cy: iSize8;
begin
  v := imm8();
  cy := bool_bit[F[Flag_C]];
  t := A + v + cy;
  UpdateFlag_ADD(t, A, v);
  A := t;
end;

procedure TCPU_8080.op_ADC_A;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + A + cy;
  UpdateFlag_ADD(t, A, A);
  A := t;
end;

procedure TCPU_8080.op_ADC_B;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + B + cy;
  UpdateFlag_ADD(t, A, B);
  A := t;
end;

procedure TCPU_8080.op_ADC_C;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + C + cy;
  UpdateFlag_ADD(t, A, C);
  A := t;
end;

procedure TCPU_8080.op_ADC_D;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + D + cy;
  UpdateFlag_ADD(t, A, D);
  A := t;
end;

procedure TCPU_8080.op_ADC_E;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + E + cy;
  UpdateFlag_ADD(t, A, E);
  A := t;
end;

procedure TCPU_8080.op_ADC_H;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + H + cy;
  UpdateFlag_ADD(t, A, H);
  A := t;
end;

procedure TCPU_8080.op_ADC_L;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + L + cy;
  UpdateFlag_ADD(t, A, L);
  A := t;
end;

procedure TCPU_8080.op_ADC_M;
var
  HL: iSize16;
  t, v, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A + v + cy;
  UpdateFlag_ADD(t, A, v);
  A := t;
end;

procedure TCPU_8080.op_ADI;
var
  t, v: iSize8;
begin
  // Add immediate to A
  v := imm8();
  t := A + v;
  UpdateFlag_ADD(t, A, v);
  A := t;
end;

procedure TCPU_8080.op_ADD_A;
var
  t: iSize8;
begin
  // Add A to A
  t := A + A;
  UpdateFlag_ADD(t, A, A);
  A := t;
end;

procedure TCPU_8080.op_ADD_B;
var
  t: iSize8;
begin
  // Add B to A
  t := A + B;
  UpdateFlag_ADD(t, A, B);
  A := t;
end;

procedure TCPU_8080.op_ADD_C;
var
  t: iSize8;
begin
  // Add C to A
  t := A + C;
  UpdateFlag_ADD(t, A, C);
  A := t;
end;

procedure TCPU_8080.op_ADD_D;
var
  t: iSize8;
begin
  // Add D to A
  t := A + D;
  UpdateFlag_ADD(t, A, D);
  A := t;
end;

procedure TCPU_8080.op_ADD_E;
var
  t: iSize8;
begin
  // Add E to A
  t := A + E;
  UpdateFlag_ADD(t, A, E);
  A := t;
end;

procedure TCPU_8080.op_ADD_H;
var
  t: iSize8;
begin
  // Add H to A
  t := A + H;
  UpdateFlag_ADD(t, A, H);
  A := t;
end;

procedure TCPU_8080.op_ADD_L;
var
  t: iSize8;
begin
  // Add L to A
  t := A + L;
  UpdateFlag_ADD(t, A, L);
  A := t;
end;

procedure TCPU_8080.op_ADD_M;
var
  HL: iSize16;
  t, v: iSize8;
begin
  // Add (HL) to A
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A + v;
  UpdateFlag_ADD(t, A, v);
  A := t;
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
end;

procedure TCPU_8080.op_INR_A;
begin
  // Increments A
  A := (A + 1) and $FF;
  UpdateFlag_INC(A);
end;

procedure TCPU_8080.op_INR_B;
begin
  // Increments B
  B := (B + 1) and $FF;
  UpdateFlag_INC(B);
end;

procedure TCPU_8080.op_INR_C;
begin
  // Increments C
  C := (C + 1) and $FF;
  UpdateFlag_INC(C);
end;

procedure TCPU_8080.op_INR_D;
begin
  // Increments D
  D := (D + 1) and $FF;
  UpdateFlag_INC(D);
end;

procedure TCPU_8080.op_INR_E;
begin
  // Increments E
  E := (E + 1) and $FF;
  UpdateFlag_INC(E);
end;

procedure TCPU_8080.op_INR_H;
begin
  // Increments H
  H := (H + 1) and $FF;
  UpdateFlag_INC(H);
end;

procedure TCPU_8080.op_INR_L;
begin
  // Increments L
  L := (L + 1) and $FF;
  UpdateFlag_INC(L);
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
  UpdateFlag_INC(t);
end;

procedure TCPU_8080.op_INX_B;
begin
  // Increments BC
  Inc(C);
  if (C > $FF) then begin
    C := $00;
    B := (B + 1) and $FF;
  end;
end;

procedure TCPU_8080.op_INX_D;
begin
  // Increments DE
  Inc(E);
  if (E > $FF) then begin
    E := $00;
    D := (D + 1) and $FF;
  end;
end;

procedure TCPU_8080.op_INX_H;
begin
  // Increments HL
  Inc(L);
  if (L > $FF) then begin
    L := $00;
    H := (H + 1) and $FF;
  end;
end;

procedure TCPU_8080.op_INX_SP;
begin
  // Increments SP
  SP := ((SP + 1) and $FFFF);
end;

procedure TCPU_8080.op_SUI;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A - v;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8080.op_SUB_A;
var
  t: iSize8;
begin
  t := 0;
  UpdateFlag_SUB(t, A, A);
  A := t;
end;

procedure TCPU_8080.op_SUB_B;
var
  t: iSize8;
begin
  t := A - B;
  UpdateFlag_SUB(t, A, B);
  A := t;
end;

procedure TCPU_8080.op_SUB_C;
var
  t: iSize8;
begin
  t := A - C;
  UpdateFlag_SUB(t, A, C);
  A := t;
end;

procedure TCPU_8080.op_SUB_D;
var
  t: iSize8;
begin
  t := A - D;
  UpdateFlag_SUB(t, A, D);
  A := t;
end;

procedure TCPU_8080.op_SUB_E;
var
  t: iSize8;
begin
  t := A - E;
  UpdateFlag_SUB(t, A, E);
  A := t;
end;

procedure TCPU_8080.op_SUB_H;
var
  t: iSize8;
begin
  t := A - H;
  UpdateFlag_SUB(t, A, H);
  A := t;
end;

procedure TCPU_8080.op_SUB_L;
var
  t: iSize8;
begin
  t := A - L;
  UpdateFlag_SUB(t, A, L);
  A := t;
end;

procedure TCPU_8080.op_SUB_M;
var
  HL: iSize16;
  v, t: iSize8;
begin
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A - v;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8080.op_SBI;
var
  t, v, cy: iSize8;
begin
  v := imm8();
  cy := bool_bit[F[Flag_C]];
  t := A - v - cy;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8080.op_SBB_A;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := -cy;
  UpdateFlag_SUB(t, A, A);
  A := t;
end;

procedure TCPU_8080.op_SBB_B;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - B - cy;
  UpdateFlag_SUB(t, A, B);
  A := t;
end;

procedure TCPU_8080.op_SBB_C;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - C - cy;
  UpdateFlag_SUB(t, A, C);
  A := t;
end;

procedure TCPU_8080.op_SBB_D;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - D - CY;
  UpdateFlag_SUB(t, A, D);
  A := t;
end;

procedure TCPU_8080.op_SBB_E;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - E - CY;
  UpdateFlag_SUB(t, A, E);
  A := t;
end;

procedure TCPU_8080.op_SBB_H;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - H - CY;
  UpdateFlag_SUB(t, A, H);
  A := t;
end;

procedure TCPU_8080.op_SBB_L;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - L - CY;
  UpdateFlag_SUB(t, A, L);
  A := t;
end;

procedure TCPU_8080.op_SBB_M;
var
  HL: iSize16;
  t, v, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A - v - CY;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8080.op_DCR_A;
begin
  // Decrement A
  A := (A - 1) and $FF;
  UpdateFlag_DCR(A);
end;

procedure TCPU_8080.op_DCR_B;
begin
  // Decrement B
  B := (B - 1) and $FF;
  UpdateFlag_DCR(B);
end;

procedure TCPU_8080.op_DCR_C;
begin
  // Decrement C
  C := (C - 1) and $FF;
  UpdateFlag_DCR(C);
end;

procedure TCPU_8080.op_DCR_D;
begin
  // Decrement D
  D := (D - 1) and $FF;
  UpdateFlag_DCR(D);
end;

procedure TCPU_8080.op_DCR_E;
begin
  // Decrement E
  E := (E - 1) and $FF;
  UpdateFlag_DCR(E);
end;

procedure TCPU_8080.op_DCR_H;
begin
  // Decrement H
  H := (H - 1) and $FF;
  UpdateFlag_DCR(H);
end;

procedure TCPU_8080.op_DCR_L;
begin
  // Decrement L
  L := (L - 1) and $FF;
  UpdateFlag_DCR(L);
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
  UpdateFlag_DCR(t);
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
end;

procedure TCPU_8080.op_DCX_SP;
begin
  // Decrement SP
  SP := ((SP - 1) and $FFFF);
end;

procedure TCPU_8080.op_ANI;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A;
  A := A and v;
  UpdateFlag_AND(A, t, v);
end;

procedure TCPU_8080.op_ANA_A;
begin
  // A = A and A
  UpdateFlag_AND(A, A, A);
end;

procedure TCPU_8080.op_ANA_B;
var
  t: iSize8;
begin
  t := A;
  A := A and B;
  UpdateFlag_AND(A, t, B);
end;

procedure TCPU_8080.op_ANA_C;
var
  t: iSize8;
begin
  t := A;
  A := A and C;
  UpdateFlag_AND(A, t, C);
end;

procedure TCPU_8080.op_ANA_D;
var
  t: iSize8;
begin
  t := A;
  A := A and D;
  UpdateFlag_AND(A, t, D);
end;

procedure TCPU_8080.op_ANA_E;
var
  t: iSize8;
begin
  t := A;
  A := A and E;
  UpdateFlag_AND(A, t, E);
end;

procedure TCPU_8080.op_ANA_H;
var
  t: iSize8;
begin
  t := A;
  A := A and H;
  UpdateFlag_AND(A, t, H);
end;

procedure TCPU_8080.op_ANA_L;
var
  t: iSize8;
begin
  t := A;
  A := A and L;
  UpdateFlag_AND(A, t, L);
end;

procedure TCPU_8080.op_ANA_M;
var
  HL: iSize16;
  t, v: iSize8;
begin
  HL := H shl 8 + L;
  t := A;
  v := ReadMem(HL);
  A := A and v;
  UpdateFlag_AND(A, t, v);
end;

procedure TCPU_8080.op_ORA_A;
begin
  // A = A | A
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORA_B;
begin
  A := A or B;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORA_C;
begin
  A := A or C;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORA_D;
begin
  A := A or D;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORA_E;
begin
  A := A or E;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORA_H;
begin
  A := A or H;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORA_L;
begin
  A := A or L;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORA_M;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := A or ReadMem(HL);
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_ORI;
var
  v: iSize8;
begin
  v := imm8();
  A := A or v;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_A;
begin
  A := 0;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_B;
begin
  A := A xor B;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_C;
begin
  A := A xor C;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_D;
begin
  A := A xor D;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_E;
begin
  A := A xor E;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_H;
begin
  A := A xor H;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_L;
begin
  A := A xor L;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRA_M;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := A xor ReadMem(HL);
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_XRI;
var
  v: iSize8;
begin
  v := imm8();
  A := A xor v;
  UpdateFlag_OR(A);
end;

procedure TCPU_8080.op_CMP_A;
var
  t: iSize8;
begin
  t := 0;
  UpdateFlag_SUB(t, A, A);
end;

procedure TCPU_8080.op_CMP_B;
var
  t: iSize8;
begin
  t := A - B;
  UpdateFlag_SUB(t, A, B);
end;

procedure TCPU_8080.op_CMP_C;
var
  t: iSize8;
begin
  t := A - C;
  UpdateFlag_SUB(t, A, C);
end;

procedure TCPU_8080.op_CMP_D;
var
  t: iSize8;
begin
  t := A - D;
  UpdateFlag_SUB(t, A, D);
end;

procedure TCPU_8080.op_CMP_E;
var
  t: iSize8;
begin
  t := A - E;
  UpdateFlag_SUB(t, A, E);
end;

procedure TCPU_8080.op_CMP_H;
var
  t: iSize8;
begin
  t := A - H;
  UpdateFlag_SUB(t, A, H);
end;

procedure TCPU_8080.op_CMP_L;
var
  t: iSize8;
begin
  t := A - L;
  UpdateFlag_SUB(t, A, L);
end;

procedure TCPU_8080.op_CMP_M;
var
  HL: iSize16;
  t, v: iSize8;
begin
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A - v;
  UpdateFlag_SUB(t, A, v);
end;

procedure TCPU_8080.op_CPI;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A - v;
  UpdateFlag_SUB(t, A, v);
end;

procedure TCPU_8080.op_MOV;
begin
end;

procedure TCPU_8080.op_MOV_AB;
begin
  A := B;
end;

procedure TCPU_8080.op_MOV_AC;
begin
  A := C;
end;

procedure TCPU_8080.op_MOV_AD;
begin
  A := D;
end;

procedure TCPU_8080.op_MOV_AE;
begin
  A := E;
end;

procedure TCPU_8080.op_MOV_AH;
begin
  A := H;
end;

procedure TCPU_8080.op_MOV_AL;
begin
  A := L;
end;

procedure TCPU_8080.op_MOV_AM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := ReadMem(HL);
end;

procedure TCPU_8080.op_MOV_BA;
begin
  B := A;
end;

procedure TCPU_8080.op_MOV_BC;
begin
  B := C;
end;

procedure TCPU_8080.op_MOV_BD;
begin
  B := D;
end;

procedure TCPU_8080.op_MOV_BE;
begin
  B := E;
end;

procedure TCPU_8080.op_MOV_BH;
begin
  B := H;
end;

procedure TCPU_8080.op_MOV_BL;
begin
  B := L;
end;

procedure TCPU_8080.op_MOV_BM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  B := ReadMem(HL);
end;

procedure TCPU_8080.op_MOV_CA;
begin
  C := A;
end;

procedure TCPU_8080.op_MOV_CB;
begin
  C := B;
end;

procedure TCPU_8080.op_MOV_CD;
begin
  C := D;
end;

procedure TCPU_8080.op_MOV_CE;
begin
  C := E;
end;

procedure TCPU_8080.op_MOV_CH;
begin
  C := H;
end;

procedure TCPU_8080.op_MOV_CL;
begin
  C := L;
end;

procedure TCPU_8080.op_MOV_CM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  C := ReadMem(HL);
end;

procedure TCPU_8080.op_MOV_DA;
begin
  D := A;
end;

procedure TCPU_8080.op_MOV_DB;
begin
  D := B;
end;

procedure TCPU_8080.op_MOV_DC;
begin
  D := C;
end;

procedure TCPU_8080.op_MOV_DE;
begin
  D := E;
end;

procedure TCPU_8080.op_MOV_DH;
begin
  D := H;
end;

procedure TCPU_8080.op_MOV_DL;
begin
  D := L;
end;

procedure TCPU_8080.op_MOV_DM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  D := ReadMem(HL);
end;

procedure TCPU_8080.op_MOV_EA;
begin
  E := A;
end;

procedure TCPU_8080.op_MOV_EB;
begin
  E := B;
end;

procedure TCPU_8080.op_MOV_EC;
begin
  E := C;
end;

procedure TCPU_8080.op_MOV_ED;
begin
  E := D;
end;

procedure TCPU_8080.op_MOV_EH;
begin
  E := H;
end;

procedure TCPU_8080.op_MOV_EL;
begin
  E := L;
end;

procedure TCPU_8080.op_MOV_EM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  E := ReadMem(HL);
end;

procedure TCPU_8080.op_MOV_HA;
begin
  H := A;
end;

procedure TCPU_8080.op_MOV_HB;
begin
  H := B;
end;

procedure TCPU_8080.op_MOV_HC;
begin
  H := C;
end;

procedure TCPU_8080.op_MOV_HD;
begin
  H := D;
end;

procedure TCPU_8080.op_MOV_HE;
begin
  H := E;
end;

procedure TCPU_8080.op_MOV_HL;
begin
  H := L;
end;

procedure TCPU_8080.op_MOV_HM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  H := ReadMem(HL);
end;

procedure TCPU_8080.op_MOV_LA;
begin
  L := A;
end;

procedure TCPU_8080.op_MOV_LB;
begin
  L := B;
end;

procedure TCPU_8080.op_MOV_LC;
begin
  L := C;
end;

procedure TCPU_8080.op_MOV_LD;
begin
  L := D;
end;

procedure TCPU_8080.op_MOV_LE;
begin
  L := E;
end;

procedure TCPU_8080.op_MOV_LH;
begin
  L := H;
end;

procedure TCPU_8080.op_MOV_LM;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  L := ReadMem(HL);
end;

procedure TCPU_8080.op_MOV_MA;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, A);
end;

procedure TCPU_8080.op_MOV_MB;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, B);
end;

procedure TCPU_8080.op_MOV_MC;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, C);
end;

procedure TCPU_8080.op_MOV_MD;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, D);
end;

procedure TCPU_8080.op_MOV_ME;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, E);
end;

procedure TCPU_8080.op_MOV_MH;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, H);
end;

procedure TCPU_8080.op_MOV_ML;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  WriteMem(HL, L);
end;

procedure TCPU_8080.op_MVI_A;
begin
  // Load A with an immediate
  A := imm8();
end;

procedure TCPU_8080.op_MVI_B;
begin
  // Load B with an immediate
  B := imm8();
end;

procedure TCPU_8080.op_MVI_C;
begin
  // Load C with an immediate
  C := imm8();
end;

procedure TCPU_8080.op_MVI_D;
begin
  // Load D with an immediate
  D := imm8();
end;

procedure TCPU_8080.op_MVI_E;
begin
  // Load E with an immediate
  E := imm8();
end;

procedure TCPU_8080.op_MVI_H;
begin
  // Load H with an immediate
  H := imm8();
end;

procedure TCPU_8080.op_MVI_L;
begin
  // Load L with an immediate
  L := imm8();
end;

procedure TCPU_8080.op_MVI_Mi;
var
  HL: iSize16;
  b1: iSize8;
begin
  // Write byte to memory pointed by HL
  HL := H shl 8 + L;
  b1 := imm8();
  WriteMem(HL, b1);
end;

procedure TCPU_8080.op_POP_PSW;
begin
  FlagsFromByte(pull());
  A := pull();
end;

procedure TCPU_8080.op_POP_B;
begin
  C := pull();
  B := pull();
end;

procedure TCPU_8080.op_POP_D;
begin
  E := pull();
  D := pull();
end;

procedure TCPU_8080.op_POP_H;
begin
  L := pull();
  H := pull();
end;

procedure TCPU_8080.op_PUSH_PSW;
begin
  F[Flag_U1] := True;
  F[Flag_U2] := False;
  F[Flag_U3] := False;
  push(A);
  push(FlagsToByte());
end;

procedure TCPU_8080.op_PUSH_B;
begin
  push(B);
  push(C);
end;

procedure TCPU_8080.op_PUSH_D;
begin
  push(D);
  push(E);
end;

procedure TCPU_8080.op_PUSH_H;
begin
  push(H);
  push(L);
end;

procedure TCPU_8080.op_CMA;
begin
  A := (not A) and $FF;
end;

procedure TCPU_8080.op_DAA;
var
  cy: boolean;
  t, correction: iSize8;
  lsb, msb: iSize8;
begin
  // Decimal Adjust Accumulator
  cy := F[Flag_C];
  correction := 0;
  lsb := A and $0F;
  msb := A shr 4;
  if (F[Flag_A] or (lsb > 9)) then  correction += $06;
  if (F[Flag_C] or (msb > 9) or ((msb >= 9) and (lsb > 9))) then begin
    correction += $60;
    cy := True;
  end;
  t := A + correction;
  UpdateFlag_ADD(t, A, correction);
  A := t;
  F[Flag_C] := cy;
end;

procedure TCPU_8080.op_RAL;
var
  b0: iSize8;
begin
  // Rotates A left
  b0 := bool_bit[F[Flag_C]];
  F[Flag_C] := (A and $80) <> 0;
  A := ((A shl 1) and $FF) or b0;
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
end;

procedure TCPU_8080.op_RLC;
var
  b0: iSize8;
begin
  // Rotate A left
  b0 := A shr 7;
  A := ((A shl 1) and $FF) or b0;
  F[Flag_C] := b0 <> 0;
end;

procedure TCPU_8080.op_RRC;
var
  b0: iSize8;
begin
  // Rotate A right
  b0 := (A and $01) shl 7;
  A := (A shr 1) or b0;
  F[Flag_C] := b0 <> 0;
end;

procedure TCPU_8080.op_LXI_B;
begin
  // Load BC with a 16 bit immediate
  C := imm8();
  B := imm8();
end;

procedure TCPU_8080.op_LXI_D;
begin
  // Load DE with a 16 bit immediate
  E := imm8();
  D := imm8();
end;

procedure TCPU_8080.op_LXI_H;
begin
  // Load HL with a 16 bit immediate
  L := imm8();
  H := imm8();
end;

procedure TCPU_8080.op_LXI_SP;
begin
  // Load SP with a 16 bit immediate
  SP := imm16();
end;

procedure TCPU_8080.op_LDA;
begin
  // Load A from memory
  A := data_abs();
end;

procedure TCPU_8080.op_LDAX_B;
var
  BC: iSize16;
begin
  // Load A with memory pointed by BC
  BC := B shl 8 + C;
  A := ReadMem(BC);
end;

procedure TCPU_8080.op_LDAX_D;
var
  DE: iSize16;
begin
  // Load A with memory pointed by DE
  DE := D shl 8 + E;
  A := ReadMem(DE);
end;

procedure TCPU_8080.op_LHLD;
var
  MA: iSize16;
begin
  // Load HL from immediate address
  MA := addr_abs();
  L := ReadMem(MA);
  H := ReadMem((MA + 1) and $FFFF);
end;

procedure TCPU_8080.op_STA;
var
  MA: iSize16;
begin
  // Store A in memory
  MA := addr_abs();
  WriteMem(MA, A);
end;

procedure TCPU_8080.op_STAX_B;
var
  BC: iSize16;
begin
  // Store A in memory pointed by BC
  BC := B shl 8 + C;
  WriteMem(BC, A);
end;

procedure TCPU_8080.op_STAX_D;
var
  DE: iSize16;
begin
  // Store A in memory pointed by DE
  DE := D shl 8 + E;
  WriteMem(DE, A);
end;

procedure TCPU_8080.op_SHLD;
var
  MA: iSize16;
begin
  // Write HL in memory
  MA := addr_abs();
  WriteMem(MA, L);
  WriteMem((MA + 1) and $FFFF, H);
end;

procedure TCPU_8080.op_XCHG;
var
  t: iSize8;
begin
  t := H;
  H := D;
  D := t;
  t := L;
  L := E;
  E := t;
end;

procedure TCPU_8080.op_SPHL;
begin
  SP := H shl 8 + L;
end;

procedure TCPU_8080.op_PCHL;
begin
  PC := H shl 8 + L;
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
end;

constructor TCPU_8085.Create(allowIllegal: boolean = True);
begin
  inherited Create;
  SetLength(OpCodes, 256);
  {$include CPUTable_8085.inc}
end;

procedure TCPU_8085.UpdateCPUInfo();
begin;
  inherited;
  with FInfo do begin
    flagsName := ['c', 'v', 'p', '-', 'a', 'k', 'z', 's'];
    numIRQs := 12;
    IRQsName := ['Reset', 'RST7.5', 'RST6.5', 'RST5.5', 'TRAP', 'RST_1', 'RST_2', 'RST_3', 'RST_4', 'RST_5', 'RST_6', 'RST_7'];
    IRQsNMI := [True, False, False, False, True, False, False, False, False, False, False, False];
  end;
end;

procedure TCPU_8085.DoIRQ(int: integer);
begin
  if (state = wait) then state := active;
  FIRQs[int].active := False;
  push_PC();
  case (int) of
    0: PC := $0000;
    1: PC := $003C;
    2: PC := $0034;
    3: PC := $002C;
    4: PC := $0024;
    5: PC := $0008;
    6: PC := $0010;
    7: PC := $0018;
    8: PC := $0020;
    9: PC := $0028;
    10: PC := $0030;
    11: PC := $0038;
    else begin
      PC := $0000;
    end;
  end;
end;

procedure TCPU_8085.UpdateFlag_INC(const v1: iSize8);
begin
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_A] := (v1 and $0F) = $00;
  F[Flag_V] := (v1 = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
end;

procedure TCPU_8085.UpdateFlag_DCR(const v1: iSize8);
begin
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_A] := (v1 and $0F) <> $0F;
  F[Flag_V] := (v1 = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
end;

procedure TCPU_8085.UpdateFlag_OR(const v1: iSize8);
begin
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_A] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
end;

procedure TCPU_8085.UpdateFlag_AND(const v1: iSize8);
begin
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_A] := True;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
end;

procedure TCPU_8085.UpdateFlag_CMP(var v1: iSize8; const v2, v3: iSize8);
begin
  F[Flag_C] := (v1 < 0);
  v1 := v1 and $FF;
  F[Flag_A] := ((v1 xor v2 xor v3) and $10) = 0;
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
end;

procedure TCPU_8085.UpdateFlag_SUB(var v1: iSize8; const v2, v3: iSize8);
begin
  F[Flag_C] := (v1 < 0);
  F[Flag_V] := (v1 < -128);
  v1 := v1 and $FF;
  F[Flag_A] := ((v1 xor v2 xor v3) and $10) = 0;
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
end;

procedure TCPU_8085.UpdateFlag_ADD(var v1: iSize8; const v2, v3: iSize8);
begin
  F[Flag_C] := (v1 > $FF);
  F[Flag_V] := (v1 > $7F);
  v1 := v1 and $FF;
  F[Flag_A] := ((v1 xor v2 xor v3) and $10) <> 0;
  F[Flag_Z] := (v1 = $00);
  F[Flag_P] := parity_bit[v1];
  F[Flag_S] := (v1 and $80) <> 0;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
end;

procedure TCPU_8085.op_DSUB;
var
  BC: iSize16;
  HL: iSize16;
begin
  // Sutracts BC from HL
  BC := B shl 8 + C;
  HL := H shl 8 + L;
  HL := (HL - BC);
  F[Flag_C] := (HL < $0000);
  F[Flag_Z] := ((HL and $FFFF) = $0000);
  F[Flag_S] := (HL and $8000) = $8000;
  F[Flag_A] := (HL and $0F) = $F;
  H := (HL shr 8) and $FF;
  L := HL and $FF;
  F[Flag_P] := (count_of_bits[H] + count_of_bits[L]) and $01 = $01;
end;

procedure TCPU_8085.op_ARHL;
var
  HL: iSize16;
  cy: boolean;
begin
  // Aritmethic shifts right HL
  cy := (H and $80) <> 0;
  HL := H shl 8 + L;
  HL := HL shr 1;
  H := HL shr 8;
  L := HL and $FF;
  if (cy) then begin
    H := H or $80;
    F[Flag_C] := True;
  end
  else begin
    F[Flag_C] := False;
  end;
end;

procedure TCPU_8085.op_RDEL;
var
  DE: iSize16;
  cy: boolean;
begin
  // Shifts DE left into carry
  DE := D shl 8 + E;
  cy := (D and $80) <> 0;
  DE := ((DE shl 1) and $FFFF);
  D := (DE shr 8) and $FF;
  E := DE and $FF;
  if (cy) then begin
    F[Flag_C] := True;
    F[Flag_V] := False;
    E := E or 1;
  end
  else begin
    F[Flag_C] := False;
    F[Flag_V] := False;
  end;
end;

procedure TCPU_8085.op_RIM;
var
  r: byte;
  s: boolean;
begin
  // Read Interrupt Mask and serial data into A
  if Assigned(ReadSerial) then s := ReadSerial() else s := False;
  r := 0;
  if (s) then r := r or bit_val[7];
  if (FIRQs[IRQ_RST75].active) then r := r or bit_val[6];
  if (FIRQs[IRQ_RST65].active) then r := r or bit_val[5];
  if (FIRQs[IRQ_RST65].active) then r := r or bit_val[4];
  if FIRQAllowed then r := r or bit_val[3];
  if (FIRQs[IRQ_RST75].masked) then r := r or bit_val[2];
  if (FIRQs[IRQ_RST65].masked) then r := r or bit_val[1];
  if (FIRQs[IRQ_RST65].masked) then r := r or bit_val[0];
  A := r;
end;

procedure TCPU_8085.op_SIM;
var
  r: byte;
  s: boolean;
begin
  // Set Interrupt Mask and serial data from A;
  r := A;
  if (Assigned(WriteSerial)) and ((r and bit_val[6]) <> 0) then begin
    s := (r and bit_val[7]) <> 0;
    WriteSerial(s);
  end;
  if ((r and bit_val[4]) <> 0) then FIRQs[IRQ_RST75].active := False;
  if ((r and bit_val[3]) <> 0) then begin
    FIRQs[IRQ_RST75].masked := ((r and bit_val[2]) <> 0);
    FIRQs[IRQ_RST65].masked := ((r and bit_val[1]) <> 0);
    FIRQs[IRQ_RST55].masked := ((r and bit_val[0]) <> 0);
  end;
end;

procedure TCPU_8085.op_LDHI;
var
  DE: iSize16;
  HL: iSize16;
  b1: iSize8;
begin
  // DE = HL + offset
  HL := H shl 8 + L;
  b1 := imm8();
  DE := ((HL + b1) and $FFFF);
  D := (DE shr 8) and $FF;
  E := DE and $FF;
end;

procedure TCPU_8085.op_LDSI;
var
  DE: iSize16;
  b1: iSize8;
begin
  // Load DE with SP plus immediate value
  b1 := imm8();
  DE := ((SP + b1) and $FFFF);
  D := (DE shr 8) and $FF;
  E := DE and $FF;
end;

procedure TCPU_8085.op_LHLX;
var
  DE: iSize16;
begin
  DE := D shl 8 + E;
  L := ReadMem(DE);
  H := ReadMem((DE + 1) and $FFFF);
end;

procedure TCPU_8085.op_SHLX;
var
  DE: iSize16;
begin
  DE := D shl 8 + E;
  WriteMem(DE, L);
  WriteMem((DE + 1) and $FFFF, H);
end;

procedure TCPU_8085.op_RSTV;
begin
  if (F[Flag_V]) then begin
    push_PC();
    PC := $0040;
    Inc(_cycles, 2);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JNK;
begin
  if (not F[Flag_K]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JK;
begin
  if (F[Flag_K]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_HLT;
begin
  PC := (PC - 1) and $FFFF;
  state := stop;
  if assigned(OnHalt) then OnHalt;
end;

procedure TCPU_8085.op_RST_75;
begin
  push_PC();
  PC := $003C;
end;

procedure TCPU_8085.op_RST_65;
begin
  push_PC();
  PC := $0034;
end;

procedure TCPU_8085.op_RST_55;
begin
  push_PC();
  PC := $002C;
end;

procedure TCPU_8085.op_RST_45;
begin
  push_PC();
  PC := $0024;
end;

procedure TCPU_8085.op_JC;
begin
  if (F[Flag_C]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JNC;
begin
  if (not F[Flag_C]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JZ;
begin
  if (F[Flag_Z]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JNZ;
begin
  if (not F[Flag_Z]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JM;
begin
  if (F[Flag_S]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JP;
begin
  if (not F[Flag_S]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JPE;
begin
  if (F[Flag_P]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_JPO;
begin
  if (not F[Flag_P]) then begin
    imm_PC();
    Inc(_cycles, 1);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CZ;
begin
  if (F[Flag_Z]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CNZ;
begin
  if (not F[Flag_Z]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CC;
begin
  if (F[Flag_C]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CM;
begin
  if (F[Flag_S]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CPE;
begin
  if (F[Flag_P]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CNC;
begin
  if (not F[Flag_C]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CPO;
begin
  if (not F[Flag_P]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_CP;
begin
  if (not F[Flag_S]) then begin
    internal_call(3);
  end
  else begin
    PC := (PC + 2) and $FFFF;
  end;
end;

procedure TCPU_8085.op_RM;
begin
  if (F[Flag_S]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_RPE;
begin
  if (F[Flag_P]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_RC;
begin
  if (F[Flag_C]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_RNC;
begin
  if (not F[Flag_C]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_RZ;
begin
  if (F[Flag_Z]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_RNZ;
begin
  if (not F[Flag_Z]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_RPO;
begin
  if (not F[Flag_P]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_RP;
begin
  if (not F[Flag_S]) then begin
    pull_PC();
    Inc(_cycles, 2);
  end;
end;

procedure TCPU_8085.op_ADC_A;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + A + cy;
  UpdateFlag_ADD(t, A, A);
  A:= t;
end;

procedure TCPU_8085.op_ADC_B;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + B + cy;
  UpdateFlag_ADD(t, A, B);
  A:= t;
end;

procedure TCPU_8085.op_ADC_C;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + C + cy;
  UpdateFlag_ADD(t, A, C);
  A:= t;
end;

procedure TCPU_8085.op_ADC_D;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + D + cy;
  UpdateFlag_ADD(t, A, D);
  A:= t;
end;

procedure TCPU_8085.op_ADC_E;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + E + cy;
  UpdateFlag_ADD(t, A, E);
  A:= t;
end;

procedure TCPU_8085.op_ADC_H;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + H + cy;
  UpdateFlag_ADD(t, A, H);
  A:= t;
end;

procedure TCPU_8085.op_ADC_L;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + L + cy;
  UpdateFlag_ADD(t, A, L);
  A:= t;
end;

procedure TCPU_8085.op_ADC_M;
var
  HL: iSize16;
  t, v, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A + v + cy;
  UpdateFlag_ADD(t, A, v);
  A:= t;
end;

procedure TCPU_8085.op_ACI;
var
  t, v, cy: iSize8;
begin
  v := imm8();
  cy := bool_bit[F[Flag_C]];
  t := A + v + cy;
  UpdateFlag_ADD(t, A, v);
  A:= t;
end;

procedure TCPU_8085.op_ADD_A;
var
  t: iSize8;
begin
  // Add A to A
  t := A + A;
  UpdateFlag_ADD(t, A, A);
  A:= t;
end;

procedure TCPU_8085.op_ADD_B;
var
  t: iSize8;
begin
  // Add B to A
  t := A + B;
  UpdateFlag_ADD(t, A, B);
  A:= t;
end;

procedure TCPU_8085.op_ADD_C;
var
  t: iSize8;
begin
  // Add C to A
  t := A + C;
  UpdateFlag_ADD(t, A, C);
  A:= t;
end;

procedure TCPU_8085.op_ADD_D;
var
  t: iSize8;
begin
  // Add D to A
  t := A + D;
  UpdateFlag_ADD(t, A, D);
  A:= t;
end;

procedure TCPU_8085.op_ADD_E;
var
  t: iSize8;
begin
  // Add E to A
  t := A + E;
  UpdateFlag_ADD(t, A, E);
  A:= t;
end;

procedure TCPU_8085.op_ADD_H;
var
  t: iSize8;
begin
  // Add H to A
  t := A + H;
  UpdateFlag_ADD(t, A, H);
  A:= t;
end;

procedure TCPU_8085.op_ADD_L;
var
  t: iSize8;
begin
  // Add L to A
  t := A + L;
  UpdateFlag_ADD(t, A, L);
  A:= t;
end;

procedure TCPU_8085.op_ADD_M;
var
  HL: iSize16;
  t, v: iSize8;
begin
  // Add (HL) to A
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A + v;
  UpdateFlag_ADD(t, A, v);
  A:= t;
end;

procedure TCPU_8085.op_ADI;
var
  t, v: iSize8;
begin
  // Add immediate to A
  v := imm8();
  t := A + v;
  UpdateFlag_ADD(t, A, v);
  A:= t;
end;

procedure TCPU_8085.op_DAD_B;
var
  BC: iSize16;
  HL: iSize16;
begin
  // Adds BC to HL
  BC := B shl 8 + C;
  HL := H shl 8 + L;
  HL := HL + BC;
  F[Flag_C] := HL > $FFFF;
  F[Flag_V] := F[Flag_C];
  H := (HL shr 8) and $FF;
  L := HL and $FF;
end;

procedure TCPU_8085.op_DAD_D;
var
  DE: iSize16;
  HL: iSize16;
begin
  // Adds DE to HL
  DE := D shl 8 + E;
  HL := H shl 8 + L;
  HL := HL + DE;
  F[Flag_C] := (HL > $FFFF);
  F[Flag_V] := F[Flag_C];
  H := (HL shr 8) and $FF;
  L := HL and $FF;
end;

procedure TCPU_8085.op_DAD_H;
var
  HL: iSize16;
begin
  // Adds HL to HL
  HL := H shl 8 + L;
  HL := HL + HL;
  F[Flag_C] := (HL > $FFFF);
  F[Flag_V] := F[Flag_C];
  H := (HL shr 8) and $FF;
  L := HL and $FF;
end;

procedure TCPU_8085.op_DAD_SP;
var
  HL: iSize16;
begin
  // Adds SP to HL
  HL := H shl 8 + L;
  HL := HL + SP;
  F[Flag_C] := (HL > $FFFF);
  F[Flag_V] := F[Flag_C];
  H := (HL shr 8) and $FF;
  L := HL and $FF;
end;

procedure TCPU_8085.op_INR_A;
begin
  // Increments A
  A := (A + 1) and $FF;
  UpdateFlag_INC(A);
end;

procedure TCPU_8085.op_INR_B;
begin
  // Increments B
  B := (B + 1) and $FF;
  UpdateFlag_INC(B);
end;

procedure TCPU_8085.op_INR_C;
begin
  // Increments C
  C := (C + 1) and $FF;
  UpdateFlag_INC(C);
end;

procedure TCPU_8085.op_INR_D;
begin
  // Increments D
  D := (D + 1) and $FF;
  UpdateFlag_INC(D);
end;

procedure TCPU_8085.op_INR_E;
begin
  // Increments E
  E := (E + 1) and $FF;
  UpdateFlag_INC(E);
end;

procedure TCPU_8085.op_INR_H;
begin
  // Increments H
  H := (H + 1) and $FF;
  UpdateFlag_INC(H);
end;

procedure TCPU_8085.op_INR_L;
begin
  // Increments L
  L := (L + 1) and $FF;
  UpdateFlag_INC(L);
end;

procedure TCPU_8085.op_INR_M;
var
  HL: iSize16;
  t: iSize8;
begin
  // Increment Memory
  HL := H shl 8 + L;
  t := (ReadMem(HL) + 1) and $FF;
  WriteMem(HL, t);
  UpdateFlag_INC(t);
end;

procedure TCPU_8085.op_INX_B;
begin
  // Increments BC
  Inc(C);
  if (C > $FF) then begin
    C := $00;
    Inc(B);
    if (B > $FF) then begin
      B := 0;
      F[Flag_K] := True;
    end
    else begin
      F[Flag_K] := False;
    end;
  end;
end;

procedure TCPU_8085.op_INX_D;
begin
  // Increments DE
  Inc(E);
  if (E > $FF) then begin
    E := $00;
    Inc(D);
    if (D > $FF) then begin
      D := 0;
      F[Flag_K] := True;
    end
    else begin
      F[Flag_K] := False;
    end;
  end;
end;

procedure TCPU_8085.op_INX_H;
begin
  // Increments HL
  Inc(L);
  if (L > $FF) then begin
    L := $00;
    Inc(H);
    if (H > $FF) then begin
      H := 0;
      F[Flag_K] := True;
    end
    else begin
      F[Flag_K] := False;
    end;
  end;
end;

procedure TCPU_8085.op_INX_SP;
begin
  // Increments SP
  SP := (SP + 1) and $FFFF;
  F[Flag_K] := (SP = $0000);
end;

procedure TCPU_8085.op_SUB_A;
var
  t: iSize8;
begin
  t := 0;
  UpdateFlag_SUB(t, A, A);
  A := t;
end;

procedure TCPU_8085.op_SUB_B;
var
  t: iSize8;
begin
  t := A - B;
  UpdateFlag_SUB(t, A, B);
  A := t;
end;

procedure TCPU_8085.op_SUB_C;
var
  t: iSize8;
begin
  t := A - C;
  UpdateFlag_SUB(t, A, C);
  A := t;
end;

procedure TCPU_8085.op_SUB_D;
var
  t: iSize8;
begin
  t := A - D;
  UpdateFlag_SUB(t, A, D);
  A := t;
end;

procedure TCPU_8085.op_SUB_E;
var
  t: iSize8;
begin
  t := A - E;
  UpdateFlag_SUB(t, A, E);
  A := t;
end;

procedure TCPU_8085.op_SUB_H;
var
  t: iSize8;
begin
  t := A - H;
  UpdateFlag_SUB(t, A, H);
  A := t;
end;

procedure TCPU_8085.op_SUB_L;
var
  t: iSize8;
begin
  t := A - L;
  UpdateFlag_SUB(t, A, L);
  A := t;
end;

procedure TCPU_8085.op_SUB_M;
var
  HL: iSize16;
  v, t: iSize8;
begin
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A - v;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8085.op_SUI;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A - v;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8085.op_SBB_A;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t:= -cy;
  UpdateFlag_SUB(t, A, A);
  A := t;
end;

procedure TCPU_8085.op_SBB_B;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - B - cy;
  UpdateFlag_SUB(t, A, B);
  A := t;
end;

procedure TCPU_8085.op_SBB_C;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - C - cy;
  UpdateFlag_SUB(t, A, C);
  A := t;
end;

procedure TCPU_8085.op_SBB_D;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - D - cy;
  UpdateFlag_SUB(t, A, D);
  A := t;
end;

procedure TCPU_8085.op_SBB_E;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - E - cy;
  UpdateFlag_SUB(t, A, E);
  A := t;
end;

procedure TCPU_8085.op_SBB_H;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - H - cy;
  UpdateFlag_SUB(t, A, H);
  A := t;
end;

procedure TCPU_8085.op_SBB_L;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - L - cy;
  UpdateFlag_SUB(t, A, L);
  A := t;
end;

procedure TCPU_8085.op_SBB_M;
var
  HL: iSize16;
  t, v, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A - v - cy;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8085.op_SBI;
var
  t, v, cy: iSize8;
begin
  v := imm8();
  cy := bool_bit[F[Flag_C]];
  t := A - v - cy;
  UpdateFlag_SUB(t, A, v);
  A := t;
end;

procedure TCPU_8085.op_DCR_A;
begin
  // Decrement A
  A := (A - 1) and $FF;
  UpdateFlag_DCR(A);
end;

procedure TCPU_8085.op_DCR_B;
begin
  // Decrement B
  B := (B - 1) and $FF;
  UpdateFlag_DCR(B);
end;

procedure TCPU_8085.op_DCR_C;
begin
  // Decrement C
  C := (C - 1) and $FF;
  UpdateFlag_DCR(C);
end;

procedure TCPU_8085.op_DCR_D;
begin
  // Decrement D
  D := (D - 1) and $FF;
  UpdateFlag_DCR(D);
end;

procedure TCPU_8085.op_DCR_E;
begin
  // Decrement E
  E := (E - 1) and $FF;
  UpdateFlag_DCR(E);
end;

procedure TCPU_8085.op_DCR_H;
begin
  // Decrement H
  H := (H - 1) and $FF;
  UpdateFlag_DCR(H);
end;

procedure TCPU_8085.op_DCR_L;
begin
  // Decrement L
  L := (L - 1) and $FF;
  UpdateFlag_DCR(L);
end;

procedure TCPU_8085.op_DCR_M;
var
  HL: iSize16;
  t: iSize8;
begin
  // Decrement Memmory
  HL := H shl 8 + L;
  t := (ReadMem(HL) - 1) and $FF;
  WriteMem(HL, t);
  UpdateFlag_DCR(t);
end;

procedure TCPU_8085.op_DCX_B;
begin
  // Decrement BC
  if (C = 0) then begin
    F[Flag_K] := (B = 0);
    B := (B - 1) and $FF;
    C := $FF;
  end
  else begin
    F[Flag_K] := False;
    Dec(C);
  end;
end;

procedure TCPU_8085.op_DCX_D;
begin
  // Decrement DE
  if (E = 0) then begin
    F[Flag_K] := (D = 0);
    D := (D - 1) and $FF;
    E := $FF;
  end
  else begin
    F[Flag_K] := False;
    Dec(E);
  end;
end;

procedure TCPU_8085.op_DCX_H;
begin
  // Decrement HL
  if (L = 0) then begin
    F[Flag_K] := (H = 0);
    H := (H - 1) and $FF;
    L := $FF;
  end
  else begin
    F[Flag_K] := False;
    Dec(L);
  end;
end;

procedure TCPU_8085.op_DCX_SP;
begin
  // Decrement SP
  SP := (SP - 1) and $FFFF;
  F[Flag_K] := (SP = $FFFF);
end;

procedure TCPU_8085.op_ANA_A;
begin
  // A = A and A
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANA_B;
begin
  A := A and B;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANA_C;
begin
  A := A and C;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANA_D;
begin
  A := A and D;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANA_E;
begin
  A := A and E;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANA_H;
begin
  A := A and H;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANA_L;
begin
  A := A and L;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANA_M;
var
  HL: iSize16;
  b1: iSize8;
begin
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  A := A and b1;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ANI;
var
  v: iSize8;
begin
  v := imm8();
  A := A and v;
  UpdateFlag_AND(A);
end;

procedure TCPU_8085.op_ORA_A;
begin
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORA_B;
begin
  A := A or B;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORA_C;
begin
  A := A or C;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORA_D;
begin
  A := A or D;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORA_E;
begin
  A := A or E;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORA_H;
begin
  A := A or H;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORA_L;
begin
  A := A or L;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORA_M;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := A or ReadMem(HL);
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_ORI;
var
  v: iSize8;
begin
  v := imm8();
  A := A or v;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_A;
begin
  A := 0;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_B;
begin
  A := A xor B;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_C;
begin
  A := A xor C;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_D;
begin
  A := A xor D;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_E;
begin
  A := A xor E;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_H;
begin
  A := A xor H;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_L;
begin
  A := A xor L;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRA_M;
var
  HL: iSize16;
begin
  HL := H shl 8 + L;
  A := A xor ReadMem(HL);
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_XRI;
var
  v: iSize8;
begin
  v := imm8();
  A := A xor v;
  UpdateFlag_OR(A);
end;

procedure TCPU_8085.op_CMP_A;
var
  t: iSize8;
begin
  t := 0;
  UpdateFlag_CMP(t, A, A);
end;

procedure TCPU_8085.op_CMP_B;
var
  t: iSize8;
begin
  t := A - B;
  UpdateFlag_CMP(t, A, B);
end;

procedure TCPU_8085.op_CMP_C;
var
  t: iSize8;
begin
  t := A - C;
  UpdateFlag_CMP(t, A, C);
end;

procedure TCPU_8085.op_CMP_D;
var
  t: iSize8;
begin
  t := A - D;
  UpdateFlag_CMP(t, A, D);
end;

procedure TCPU_8085.op_CMP_E;
var
  t: iSize8;
begin
  t := A - E;
  UpdateFlag_CMP(t, A, E);
end;

procedure TCPU_8085.op_CMP_H;
var
  t: iSize8;
begin
  t := A - H;
  UpdateFlag_CMP(t, A, H);
end;

procedure TCPU_8085.op_CMP_L;
var
  t: iSize8;
begin
  t := A - L;
  UpdateFlag_CMP(t, A, L);
end;

procedure TCPU_8085.op_CMP_M;
var
  HL: iSize16;
  t, v: iSize8;
begin
  HL := H shl 8 + L;
  v := ReadMem(HL);
  t := A - v;
  UpdateFlag_CMP(t, A, v);
end;

procedure TCPU_8085.op_CPI;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A - v;
  UpdateFlag_CMP(t, A, v);
end;

procedure TCPU_8085.op_CMA;
begin
  A := (not A) and $FF;
  F[Flag_V] := True;
  F[Flag_K] := True;
end;

procedure TCPU_8085.op_DAA;
var
  cy: boolean;
  correction: iSize8;
  t, lsb, msb: iSize8;
begin
  // Decimal Adjust Accumulator
  cy := F[Flag_C];
  correction := 0;
  lsb := A and $0F;
  msb := A shr 4;
  if (F[Flag_A] or (lsb > 9)) then  correction += $06;
  if (F[Flag_C] or (msb > 9) or ((msb >= 9) and (lsb > 9))) then begin
    correction += $60;
    cy := True;
  end;
  t := A + correction;
  UpdateFlag_ADD(t, A, correction);
  A := t;
  F[Flag_C] := cy;
end;

procedure TCPU_8085.op_RAL;
var
  b0: iSize8;
begin
  // Rotates A left
  b0 := bool_bit[F[Flag_C]];
  F[Flag_C] := (A and $80) <> 0;
  F[Flag_V] := F[Flag_C];
  A := ((A shl 1) and $FF) or b0;
end;

procedure TCPU_8085.op_RAR;
var
  b0: iSize8;
  b7: iSize8;
begin
  // Arithmetic rotate right A
  b0 := A and $01;
  b7 := bool_bit[F[Flag_C]] shl 7;
  A := (A shr 1) or b7;
  F[Flag_C] := b0 <> 0;
  F[Flag_V] := False;
end;

procedure TCPU_8085.op_RLC;
var
  b0: iSize8;
begin
  // Rotate A left with carry
  b0 := A shr 7;
  A := (A shl 1) or b0;
  F[Flag_V] := (A > $FF);
  A := A and $FF;
  F[Flag_C] := b0 <> 0;
end;

procedure TCPU_8085.op_RRC;
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
  F[Flag_V] := False;
end;


end.

