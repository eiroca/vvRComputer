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
unit uCPU_8085;

{$mode ObjFPC}{$H+}

interface

uses
  uCPU, uCPU_8080,
  Classes, SysUtils;

type
  ReadSerialCall = function(): boolean of object;
  WriteSerialCall = procedure(d: boolean) of object;

const
  Flag_V = 1;
  Flag_K = 5;

  IRQ_RST75 = 1;
  IRQ_RST65 = 2;
  IRQ_RST55 = 3;

type

  { TCPU_8085 }

  TCPU_8085 = class(TCPU_8080)
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
    procedure op_CALL;
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
    procedure op_MOV;
    procedure op_MOV_AB;
    procedure op_MOV_AC;
    procedure op_MOV_AD;
    procedure op_MOV_AE;
    procedure op_MOV_AH;
    procedure op_MOV_AL;
    procedure op_MOV_BA;
    procedure op_MOV_BC;
    procedure op_MOV_BD;
    procedure op_MOV_BE;
    procedure op_MOV_BH;
    procedure op_MOV_BL;
    procedure op_MOV_CA;
    procedure op_MOV_CB;
    procedure op_MOV_CD;
    procedure op_MOV_CE;
    procedure op_MOV_CH;
    procedure op_MOV_CL;
    procedure op_MOV_DA;
    procedure op_MOV_DB;
    procedure op_MOV_DC;
    procedure op_MOV_DE;
    procedure op_MOV_DH;
    procedure op_MOV_DL;
    procedure op_MOV_EA;
    procedure op_MOV_EB;
    procedure op_MOV_EC;
    procedure op_MOV_ED;
    procedure op_MOV_EH;
    procedure op_MOV_EL;
    procedure op_MOV_HA;
    procedure op_MOV_HB;
    procedure op_MOV_HC;
    procedure op_MOV_HD;
    procedure op_MOV_HE;
    procedure op_MOV_HL;
    procedure op_MOV_LA;
    procedure op_MOV_LB;
    procedure op_MOV_LC;
    procedure op_MOV_LD;
    procedure op_MOV_LE;
    procedure op_MOV_LH;
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
    procedure op_PUSH_B;
    procedure op_PUSH_D;
    procedure op_PUSH_H;
    procedure op_PUSH_PSW;
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
    procedure op_RST_0;
    procedure op_RST_1;
    procedure op_RST_2;
    procedure op_RST_3;
    procedure op_RST_4;
    procedure op_RST_5;
    procedure op_RST_6;
    procedure op_RST_7;
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
    procedure op_SPHL;
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
    procedure op_XTHL;
  public
    constructor Create;
  protected
    procedure UpdateCPUInfo(); override;
    procedure DoIRQ(int: integer); override;
  end;

implementation

constructor TCPU_8085.Create;
begin
  inherited;
  SetLength(OpCodes, 256);
  {$include CPUTable_8085.inc}
end;

procedure TCPU_8085.UpdateCPUInfo();
begin;
  inherited;
  with FInfo do begin
    flagsName := ['C', 'V', 'P', '-', 'H', 'K', 'Z', 'S'];
    numIRQs := 12;
    IRQsName := ['Reset', 'RST7.5', 'RST6.5', 'RST5.5', 'TRAP', 'RST_1', 'RST_2', 'RST_3', 'RST_4', 'RST_5', 'RST_6', 'RST_7'];
    IRQsNMI := [True, False, False, False, True, False, False, False, False, False, False, False];
  end;
end;

procedure TCPU_8085.DoIRQ(int: integer);
begin
  inherited;
  case (int) of
    0: op_RST_0;
    1: op_RST_75;
    2: op_RST_65;
    3: op_RST_55;
    4: op_RST_45;
    5: op_RST_1;
    6: op_RST_2;
    7: op_RST_3;
    8: op_RST_4;
    9: op_RST_5;
    10: op_RST_6;
    11: op_RST_7;
  end;
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
  F[Flag_H] := (HL and $0F) = $F;
  H := (HL shr 8) and $FF;
  L := HL and $FF;
  F[Flag_P] := (count_of_bits[H] + count_of_bits[L]) and $01 = $01;
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles, 2);
  Inc(states, 7);
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
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_LDHI;
var
  DE: iSize16;
  HL: iSize16;
  b1: iSize8;
begin
  // DE = HL + offset
  DE := D shl 8 + E;
  HL := H shl 8 + L;
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  DE := ((HL + b1) and $FFFF);
  D := (DE shr 8) and $FF;
  E := DE and $FF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8085.op_LDSI;
var
  DE: iSize16;
  b1: iSize8;
begin
  // Load DE with SP plus immediate value
  DE := D shl 8 + E;
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  DE := ((SP + b1) and $FFFF);
  D := (DE shr 8) and $FF;
  E := DE and $FF;
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8085.op_LHLX;
var
  DE: iSize16;
begin
  DE := D shl 8 + E;
  L := ReadMem(DE);
  H := ReadMem((DE + 1) and $FFFF);
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8085.op_SHLX;
var
  DE: iSize16;
begin
  DE := D shl 8 + E;
  WriteMem(DE, L);
  WriteMem((DE + 1) and $FFFF, H);
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8085.op_RSTV;
begin
  if (F[Flag_V]) then begin
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := $0040;
    Inc(cycles, 3);
    Inc(states, 12);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_JNK;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_K]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JK;
var
  b1, b2: iSize8;
begin
  if (F[Flag_K]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_HLT;
begin
  PC := (PC - 1) and $FFFF;
  FHalted := True;
  Inc(cycles);
  Inc(states, 5);
  OnHalt;
end;

procedure TCPU_8085.op_CALL;
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
  Inc(states, 18);
end;

procedure TCPU_8085.op_RST_0;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0000;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_1;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0008;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_2;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0010;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_3;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0018;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_4;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0020;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_5;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0028;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_6;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0030;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_7;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0038;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_75;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $003C;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_65;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0034;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_55;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $002C;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_RST_45;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC shr 8);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, PC and $FF);
  PC := $0024;
  Inc(cycles, 3);
  Inc(states, 12);
end;

procedure TCPU_8085.op_JC;
var
  b1, b2: iSize8;
begin
  if (F[Flag_C]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JNC;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_C]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JZ;
var
  b1, b2: iSize8;
begin
  if (F[Flag_Z]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JNZ;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_Z]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JM;
var
  b1, b2: iSize8;
begin
  if (F[Flag_S]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JP;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_S]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JPE;
var
  b1, b2: iSize8;
begin
  if (F[Flag_P]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_JPO;
var
  b1, b2: iSize8;
begin
  if (not F[Flag_P]) then begin
    b1 := ReadMem(PC);
    PC := (PC + 1) and $FFFF;
    b2 := ReadMem(PC);
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 10);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 7);
  end;
end;

procedure TCPU_8085.op_CZ;
begin
  if (F[Flag_Z]) then begin
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := ReadMem((PC + 1) and $FFFF) shl 8 + ReadMem(PC);
    Inc(cycles, 5);
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_CNZ;
begin
  if (not F[Flag_Z]) then begin
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC shr 8);
    SP := (SP - 1) and $FFFF;
    WriteMem(SP, PC and $FF);
    PC := ReadMem((PC + 1) and $FFFF) shl 8 + ReadMem(PC);
    Inc(cycles, 5);
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_CC;
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
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_CM;
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
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_CPE;
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
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_CNC;
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
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_CPO;
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
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_CP;
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
    Inc(states, 18);
  end
  else begin
    PC := (PC + 2) and $FFFF;
    Inc(cycles, 2);
    Inc(states, 9);
  end;
end;

procedure TCPU_8085.op_RM;
var
  b1, b2: iSize8;
begin
  if (F[Flag_S]) then begin
    b1 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    b2 := ReadMem(SP);
    SP := (SP + 1) and $FFFF;
    PC := b1 + b2 shl 8;
    Inc(cycles, 3);
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_RPE;
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
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_RC;
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
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_RNC;
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
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_RZ;
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
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_RNZ;
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
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_RPO;
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
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_RP;
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
    Inc(states, 12);
  end
  else begin
    Inc(cycles, 1);
    Inc(states, 6);
  end;
end;

procedure TCPU_8085.op_ADC_A;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + A + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor A xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADC_B;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + B + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor B xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADC_C;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + C + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor C xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADC_D;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + D + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor D xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADC_E;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + E + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor E xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADC_H;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + H + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor H xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADC_L;
var
  t: iSize8;
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A + L + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor L xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADC_M;
var
  HL: iSize16;
  t, b1, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A + b1 + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_ACI;
var
  t, b1, cy: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  cy := bool_bit[F[Flag_C]];
  t := A + b1 + cy;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_ADD_A;
var
  t: iSize8;
begin
  // Add A to A
  t := A + A;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor A xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADD_B;
var
  t: iSize8;
begin
  // Add B to A
  t := A + B;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor B xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADD_C;
var
  t: iSize8;
begin
  // Add C to A
  t := A + C;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor C xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADD_D;
var
  t: iSize8;
begin
  // Add D to A
  t := A + D;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor D xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADD_E;
var
  t: iSize8;
begin
  // Add E to A
  t := A + E;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor E xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADD_H;
var
  t: iSize8;
begin
  // Add H to A
  t := A + H;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor H xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADD_L;
var
  t: iSize8;
begin
  // Add L to A
  t := A + L;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor L xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ADD_M;
var
  HL: iSize16;
  b1, t: iSize8;
begin
  // Add (HL) to A
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A + b1;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_ADI;
var
  b1: iSize8;
  t: iSize8;
begin
  // Add immediate to A
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  t := A + b1;
  F[Flag_C] := (t > $FF);
  F[Flag_V] := (t > $7F);
  F[Flag_H] := (A xor b1 xor t) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_S] := (t and $80) = $80;
  F[Flag_P] := parity_of_bits[t];
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  A := t;
  Inc(cycles, 2);
  Inc(states, 7);
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
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8085.op_INR_A;
begin
  // Increments A
  A := (A + 1) and $FF;
  F[Flag_H] := (A and $0F) = $00;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_V] := (A = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_INR_B;
begin
  // Increments B
  B := (B + 1) and $FF;
  F[Flag_H] := (B and $0F) = $00;
  F[Flag_Z] := (B = $00);
  F[Flag_P] := parity_of_bits[B];
  F[Flag_S] := (B and $80) = $80;
  F[Flag_V] := (B = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_INR_C;
begin
  // Increments C
  C := (C + 1) and $FF;
  F[Flag_H] := (C and $0F) = $00;
  F[Flag_Z] := (C = $00);
  F[Flag_P] := parity_of_bits[C];
  F[Flag_S] := (C and $80) = $80;
  F[Flag_V] := (C = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_INR_D;
begin
  // Increments D
  D := (D + 1) and $FF;
  F[Flag_H] := (D and $0F) = $00;
  F[Flag_Z] := (D = $00);
  F[Flag_P] := parity_of_bits[D];
  F[Flag_S] := (D and $80) = $80;
  F[Flag_V] := (D = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_INR_E;
begin
  // Increments E
  E := (E + 1) and $FF;
  F[Flag_H] := (E and $0F) = $00;
  F[Flag_Z] := (E = $00);
  F[Flag_P] := parity_of_bits[E];
  F[Flag_S] := (E and $80) = $80;
  F[Flag_V] := (E = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_INR_H;
begin
  // Increments H
  H := (H + 1) and $FF;
  F[Flag_H] := (H and $0F) = $00;
  F[Flag_Z] := (H = $00);
  F[Flag_P] := parity_of_bits[H];
  F[Flag_S] := (H and $80) = $80;
  F[Flag_V] := (H = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_INR_L;
begin
  // Increments L
  L := (L + 1) and $FF;
  F[Flag_H] := (L and $0F) = $00;
  F[Flag_Z] := (L = $00);
  F[Flag_P] := parity_of_bits[L];
  F[Flag_S] := (L and $80) = $80;
  F[Flag_V] := (L = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
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
  F[Flag_H] := (t and $0F) = $00;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := (t = $00);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles);
  Inc(states, 6);
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
  Inc(cycles);
  Inc(states, 6);
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
  Inc(cycles);
  Inc(states, 6);
end;

procedure TCPU_8085.op_INX_SP;
begin
  // Increments SP
  SP := ((SP + 1) and $FFFF);
  F[Flag_V] := (SP = $0000);
  Inc(cycles);
  Inc(states, 6);
end;

procedure TCPU_8085.op_SUB_A;
begin
  A := 0;
  F[Flag_Z] := True;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_P] := False;
  F[Flag_S] := False;
  F[Flag_H] := False;
  F[Flag_V] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SUB_B;
var
  t: iSize8;
begin
  t := A - B;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor B xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SUB_C;
var
  t: iSize8;
begin
  t := A - C;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor C xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SUB_D;
var
  t: iSize8;
begin
  t := A - D;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor D xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SUB_E;
var
  t: iSize8;
begin
  t := A - E;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor E xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SUB_H;
var
  t: iSize8;
begin
  t := A - H;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor H xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SUB_L;
var
  t: iSize8;
begin
  t := A - L;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SUB_M;
var
  HL: iSize16;
  b1, t: iSize8;
begin
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A - b1;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor b1 xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_SUI;
var
  b1, t: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  t := A - b1;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor b1 xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_SBB_A;
var
  cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  A := -CY;
  F[Flag_C] := (cy <> 0);
  F[Flag_S] := (cy <> 0);
  F[Flag_H] := (cy = 0);
  F[Flag_Z] := (cy = 0);
  F[Flag_V] := False;
  F[Flag_P] := False;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SBB_B;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - B - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor B xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SBB_C;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - C - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor C xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SBB_D;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - D - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor D xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SBB_E;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - E - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor E xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SBB_H;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - H - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor H xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SBB_L;
var
  t, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  t := A - L - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SBB_M;
var
  HL: iSize16;
  t, b1, cy: iSize8;
begin
  cy := bool_bit[F[Flag_C]];
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  t := A - b1 - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_SBI;
var
  t, b1, cy: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  cy := bool_bit[F[Flag_C]];
  t := A - b1 - CY;
  F[Flag_C] := (t < $00);
  F[Flag_V] := (t < -128);
  F[Flag_H] := (not (A xor L xor t)) <> 0;
  A := t and $FF;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_DCR_A;
begin
  // Decrement A
  A := (A - 1) and $FF;
  F[Flag_H] := (A and $0F) = $0F;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_V] := (A = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_DCR_B;
begin
  // Decrement B
  B := (B - 1) and $FF;
  F[Flag_H] := (B and $0F) = $0F;
  F[Flag_Z] := (B = $00);
  F[Flag_P] := parity_of_bits[B];
  F[Flag_S] := (B and $80) = $80;
  F[Flag_V] := (B = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_DCR_C;
begin
  // Decrement C
  C := (C - 1) and $FF;
  F[Flag_H] := (C and $0F) = $0F;
  F[Flag_Z] := (C = $00);
  F[Flag_P] := parity_of_bits[C];
  F[Flag_S] := (C and $80) = $80;
  F[Flag_V] := (C = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_DCR_D;
begin
  // Decrement D
  D := (D - 1) and $FF;
  F[Flag_H] := (D and $0F) = $0F;
  F[Flag_Z] := (D = $00);
  F[Flag_P] := parity_of_bits[D];
  F[Flag_S] := (D and $80) = $80;
  F[Flag_V] := (D = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_DCR_E;
begin
  // Decrement E
  E := (E - 1) and $FF;
  F[Flag_H] := (E and $0F) = $0F;
  F[Flag_Z] := (E = $00);
  F[Flag_P] := parity_of_bits[E];
  F[Flag_S] := (E and $80) = $80;
  F[Flag_V] := (E = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_DCR_H;
begin
  // Decrement H
  H := (H - 1) and $FF;
  F[Flag_H] := (H and $0F) = $0F;
  F[Flag_Z] := (H = $00);
  F[Flag_P] := parity_of_bits[H];
  F[Flag_S] := (H and $80) = $80;
  F[Flag_V] := (H = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_DCR_L;
begin
  // Decrement L
  L := (L - 1) and $FF;
  F[Flag_H] := (L and $0F) = $0F;
  F[Flag_Z] := (L = $00);
  F[Flag_P] := parity_of_bits[L];
  F[Flag_S] := (L and $80) = $80;
  F[Flag_V] := (L = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
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
  F[Flag_H] := (t and $0F) = $0F;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := (t = $FF);
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 3);
  Inc(states, 10);
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
  Inc(cycles);
  Inc(states, 6);
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
  Inc(cycles);
  Inc(states, 6);
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
  Inc(cycles);
  Inc(states, 6);
end;

procedure TCPU_8085.op_DCX_SP;
begin
  // Decrement SP
  SP := (SP - 1);
  F[Flag_K] := (SP < 0);
  SP := SP and $FFFF;
  Inc(cycles);
  Inc(states, 6);
end;

procedure TCPU_8085.op_ANA_A;
begin
  // A = A and A
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ANA_B;
begin
  A := A and B;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ANA_C;
begin
  A := A and C;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ANA_D;
begin
  A := A and D;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ANA_E;
begin
  A := A and E;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ANA_H;
begin
  A := A and H;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ANA_L;
begin
  A := A and L;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ANA_M;
var
  HL: iSize16;
  b1: iSize8;
begin
  HL := H shl 8 + L;
  b1 := ReadMem(HL);
  A := A and b1;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_ANI;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  A := A and b1;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_H] := True;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_K] := F[Flag_V] or F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_ORA_A;
begin
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ORA_B;
begin
  A := A or B;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ORA_C;
begin
  A := A or C;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ORA_D;
begin
  A := A or D;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ORA_E;
begin
  A := A or E;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ORA_H;
begin
  A := A or H;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ORA_L;
begin
  A := A or L;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_ORA_M;
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
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_ORI;
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
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_XRA_A;
begin
  A := 0;
  F[Flag_Z] := True;
  F[Flag_P] := True;
  F[Flag_S] := False;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_K] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_XRA_B;
begin
  A := A xor B;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_XRA_C;
begin
  A := A xor C;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_XRA_D;
begin
  A := A xor D;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_XRA_E;
begin
  A := A xor E;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_XRA_H;
begin
  A := A xor H;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_XRA_L;
begin
  A := A xor L;
  F[Flag_Z] := (A = $00);
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_XRA_M;
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
  F[Flag_K] := F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_XRI;
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
  F[Flag_K] := F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_CMP_A;
begin
  F[Flag_Z] := True;
  F[Flag_P] := False;
  F[Flag_S] := False;
  F[Flag_H] := False;
  F[Flag_C] := False;
  F[Flag_V] := False;
  F[Flag_K] := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_CMP_B;
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
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_CMP_C;
var
  t: iSize8;
begin
  t := A - C;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_CMP_D;
var
  t: iSize8;
begin
  t := A - D;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_CMP_E;
var
  t: iSize8;
begin
  t := A - E;
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  F[Flag_C] := (t < 0);
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_CMP_H;
var
  t: iSize8;
begin
  t := A - H;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_CMP_L;
var
  t: iSize8;
begin
  t := A - L;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_CMP_M;
var
  HL: iSize16;
  t: iSize8;
  b0: iSize8;
begin
  HL := H shl 8 + L;
  b0 := ReadMem(HL);
  t := A - b0;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_CPI;
var
  b1: iSize8;
  t: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  t := A - b1;
  F[Flag_C] := (t < 0);
  F[Flag_H] := (not (A xor t xor B) and $10) <> 0;
  t := t and $FF;
  F[Flag_Z] := (t = $00);
  F[Flag_P] := parity_of_bits[t];
  F[Flag_S] := (t and $80) = $80;
  F[Flag_V] := False;
  F[Flag_K] := F[Flag_S];
  Inc(cycles, 2);
  Inc(states, 7);
end;

procedure TCPU_8085.op_MOV;
begin
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_AB;
begin
  A := B;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_AC;
begin
  A := C;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_AD;
begin
  A := D;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_AE;
begin
  A := E;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_AH;
begin
  A := H;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_AL;
begin
  A := L;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_BA;
begin
  B := A;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_BC;
begin
  B := C;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_BD;
begin
  B := D;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_BE;
begin
  B := E;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_BH;
begin
  B := H;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_BL;
begin
  B := L;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_CA;
begin
  C := A;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_CB;
begin
  C := B;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_CD;
begin
  C := D;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_CE;
begin
  C := E;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_CH;
begin
  C := H;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_CL;
begin
  C := L;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_DA;
begin
  D := A;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_DB;
begin
  D := B;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_DC;
begin
  D := C;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_DE;
begin
  D := E;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_DH;
begin
  D := H;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_DL;
begin
  D := L;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_EA;
begin
  E := A;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_EB;
begin
  E := B;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_EC;
begin
  E := C;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_ED;
begin
  E := D;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_EH;
begin
  E := H;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_EL;
begin
  E := L;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_HA;
begin
  H := A;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_HB;
begin
  H := B;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_HC;
begin
  H := C;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_HD;
begin
  H := D;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_HE;
begin
  H := E;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_HL;
begin
  H := L;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_LA;
begin
  L := A;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_LB;
begin
  L := B;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_LC;
begin
  L := C;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_LD;
begin
  L := D;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_LE;
begin
  L := E;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_MOV_LH;
begin
  L := H;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_PUSH_PSW;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, FlagsToByte());
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, A);
  Inc(cycles, 3);
  Inc(states, 13);
end;

procedure TCPU_8085.op_PUSH_B;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, B);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, C);
  Inc(cycles, 3);
  Inc(states, 13);
end;

procedure TCPU_8085.op_PUSH_D;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, E);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, D);
  Inc(cycles, 3);
  Inc(states, 13);
end;

procedure TCPU_8085.op_PUSH_H;
begin
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, L);
  SP := (SP - 1) and $FFFF;
  WriteMem(SP, H);
  Inc(cycles, 3);
  Inc(states, 13);
end;

procedure TCPU_8085.op_CMA;
begin
  A := (not A) and $FF;
  F[Flag_V] := True;
  F[Flag_K] := True;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_DAA;
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
  if (F[Flag_V]) then begin
    if (F[Flag_H] or (lsb > 9)) then  correction -= $06;
    if (F[Flag_C] or (msb > 9) or ((msb >= 9) and (lsb > 9))) then begin
      correction -= $60;
      cy := True;
    end;
  end
  else begin
    if (F[Flag_H] or (lsb > 9)) then  correction += $06;
    if (F[Flag_C] or (msb > 9) or ((msb >= 9) and (lsb > 9))) then begin
      correction += $60;
      cy := True;
    end;
    A := (A + correction) and $FF;
  end;
  F[Flag_Z] := A = 0;
  F[Flag_P] := parity_of_bits[A];
  F[Flag_S] := (A and $80) = $80;
  F[Flag_H] := (A and $0F) = $00;
  F[Flag_C] := cy;
  Inc(cycles);
  Inc(states, 4);
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
  Inc(cycles);
  Inc(states, 4);
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
  Inc(cycles);
  Inc(states, 4);
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
  Inc(cycles);
  Inc(states, 4);
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
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8085.op_SPHL;
begin
  SP := H shl 8 + L;
  Inc(cycles);
  Inc(states, 6);
end;

procedure TCPU_8085.op_PCHL;
begin
  PC := H shl 8 + L;
  Inc(cycles);
  Inc(states, 6);
end;

procedure TCPU_8085.op_XTHL;
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
  Inc(states, 16);
end;

end.

