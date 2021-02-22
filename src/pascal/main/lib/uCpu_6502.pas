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
unit uCPU_6502;

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
  Flag_Z = 1;
  Flag_B = 2;
  Flag_I = 3;
  Flag_D = 5;
  Flag_V = 6;
  Flag_N = 7;

type

  { TCPU_6502 }

  TCPU_6502 = class(TCPU_ClassA)
  private
    StackBase: iSize16;
  protected
    // 6502 Registers
    A: iSize8;
    X: iSize8;
    Y: iSize8;
    S: iSize8;
  private
    // Emulator hooks
  protected
    procedure push_PC(); inline;
    procedure pull_PC(); inline;
    procedure push(v: iSize8); inline;
    function pull(): iSize8; inline;
  protected
    procedure FlagsFromByte(const aByte: iSize8);
    function FlagsToByte: iSize8;
  protected
    procedure op_Illegal;
  protected
    procedure op_ADC_abs;
    procedure op_ADC_abs_idX;
    procedure op_ADC_abs_idY;
    procedure op_ADC_dp;
    procedure op_ADC_dp_idX;
    procedure op_ADC_dp_idX_ind;
    procedure op_ADC_dp_ind;
    procedure op_ADC_dp_ind_idY;
    procedure op_ADC_imm;
    procedure op_AND_abs;
    procedure op_AND_abs_idX;
    procedure op_AND_abs_idY;
    procedure op_AND_dp;
    procedure op_AND_dp_idX;
    procedure op_AND_dp_idX_ind;
    procedure op_AND_dp_ind;
    procedure op_AND_dp_ind_idY;
    procedure op_AND_imm;
    procedure op_ASL;
    procedure op_ASL_abs;
    procedure op_ASL_abs_idX;
    procedure op_ASL_dp;
    procedure op_ASL_dp_idX;
    procedure op_BCC_rel8;
    procedure op_BCS_rel8;
    procedure op_BEQ_rel8;
    procedure op_BIT_abs;
    procedure op_BIT_dp;
    procedure op_BMI_rel8;
    procedure op_BNE_rel8;
    procedure op_BPL_rel8;
    procedure op_BRK;
    procedure op_BVC_rel8;
    procedure op_BVS_rel8;
    procedure op_CLC;
    procedure op_CLD;
    procedure op_CLI;
    procedure op_CLV;
    procedure op_CMP_abs;
    procedure op_CMP_abs_idX;
    procedure op_CMP_abs_idY;
    procedure op_CMP_dp;
    procedure op_CMP_dp_idX;
    procedure op_CMP_dp_idX_ind;
    procedure op_CMP_dp_ind;
    procedure op_CMP_dp_ind_idY;
    procedure op_CMP_imm;
    procedure op_CPX_abs;
    procedure op_CPX_dp;
    procedure op_CPX_imm;
    procedure op_CPY_abs;
    procedure op_CPY_dp;
    procedure op_CPY_imm;
    procedure op_DEC;
    procedure op_DEC_abs;
    procedure op_DEC_abs_idX;
    procedure op_DEC_dp;
    procedure op_DEC_dp_idX;
    procedure op_DEX;
    procedure op_DEY;
    procedure op_EOR_abs;
    procedure op_EOR_abs_idX;
    procedure op_EOR_abs_idY;
    procedure op_EOR_dp;
    procedure op_EOR_dp_idX;
    procedure op_EOR_dp_idX_ind;
    procedure op_EOR_dp_ind;
    procedure op_EOR_dp_ind_idY;
    procedure op_EOR_imm;
    procedure op_INC;
    procedure op_INC_abs;
    procedure op_INC_abs_idX;
    procedure op_INC_dp;
    procedure op_INC_dp_idX;
    procedure op_INX;
    procedure op_INY;
    procedure op_JMP_abs;
    procedure op_JMP_abs_idX_ind;
    procedure op_JMP_abs_ind;
    procedure op_JSR_abs;
    procedure op_JSR_abs_idX_ind;
    procedure op_LDA_abs;
    procedure op_LDA_abs_idX;
    procedure op_LDA_abs_idY;
    procedure op_LDA_dp;
    procedure op_LDA_dp_idX;
    procedure op_LDA_dp_idX_ind;
    procedure op_LDA_dp_ind;
    procedure op_LDA_dp_ind_idY;
    procedure op_LDA_imm;
    procedure op_LDX_abs;
    procedure op_LDX_abs_idY;
    procedure op_LDX_dp;
    procedure op_LDX_dp_idY;
    procedure op_LDX_imm;
    procedure op_LDY_abs;
    procedure op_LDY_abs_idX;
    procedure op_LDY_dp;
    procedure op_LDY_dp_idX;
    procedure op_LDY_imm;
    procedure op_LSR;
    procedure op_LSR_abs;
    procedure op_LSR_abs_idX;
    procedure op_LSR_dp;
    procedure op_LSR_dp_idX;
    procedure op_NOP;
    procedure op_ORA_abs;
    procedure op_ORA_abs_idX;
    procedure op_ORA_abs_idY;
    procedure op_ORA_dp;
    procedure op_ORA_dp_idX;
    procedure op_ORA_dp_idX_ind;
    procedure op_ORA_dp_ind;
    procedure op_ORA_dp_ind_idY;
    procedure op_ORA_imm;
    procedure op_PHA;
    procedure op_PHP;
    procedure op_PHX;
    procedure op_PHY;
    procedure op_PLA;
    procedure op_PLP;
    procedure op_PLX;
    procedure op_PLY;
    procedure op_ROL;
    procedure op_ROL_abs;
    procedure op_ROL_abs_idX;
    procedure op_ROL_dp;
    procedure op_ROL_dp_idX;
    procedure op_ROR;
    procedure op_ROR_abs;
    procedure op_ROR_abs_idX;
    procedure op_ROR_dp;
    procedure op_ROR_dp_idX;
    procedure op_RTI;
    procedure op_RTS;
    procedure op_SBC_abs;
    procedure op_SBC_abs_idX;
    procedure op_SBC_abs_idY;
    procedure op_SBC_dp;
    procedure op_SBC_dp_idX;
    procedure op_SBC_dp_idX_ind;
    procedure op_SBC_dp_ind;
    procedure op_SBC_dp_ind_idY;
    procedure op_SBC_imm;
    procedure op_SEC;
    procedure op_SED;
    procedure op_SEI;
    procedure op_STA_abs;
    procedure op_STA_abs_idX;
    procedure op_STA_abs_idY;
    procedure op_STA_dp;
    procedure op_STA_dp_idX;
    procedure op_STA_dp_idX_ind;
    procedure op_STA_dp_ind;
    procedure op_STA_dp_ind_idY;
    procedure op_STX_abs;
    procedure op_STX_dp;
    procedure op_STX_dp_idY;
    procedure op_STY_abs;
    procedure op_STY_dp;
    procedure op_STY_dp_idX;
    procedure op_TAX;
    procedure op_TAY;
    procedure op_TSX;
    procedure op_TXA;
    procedure op_TXS;
    procedure op_TYA;
  public
    constructor Create(allowIllegal: boolean = True);
  public
    procedure Reset(); override;
  protected
    procedure UpdateCPUInfo(); override;
    procedure UpdateCPUStatus(fillExtra: boolean = True); override;
    procedure DoIRQ(int: integer); override;
  end;

implementation

constructor TCPU_6502.Create(allowIllegal: boolean = True);
begin
  inherited Create;
  SetLength(OpCodes, 256);
  StackBase := $0100;
  {$include CPUTable_6502.inc}
end;

procedure TCPU_6502.Reset();
begin
  A := $00;
  X := $00;
  Y := $00;
  S := $00;
  PC := $0000;
  FlagsFromByte(0);
  FHalted := False;
  FIRQAllowed := False;
  opers := 0;
  cycles := 0;
end;

procedure TCPU_6502.UpdateCPUStatus(fillExtra: boolean = True);
var
  i: integer;
begin;
  with FStatus do begin
    regs[0] := A;
    regs[1] := X;
    regs[2] := Y;
    regs[3] := S;
    regs[4] := PC;
    for i := 0 to 7 do begin
      flags[i] := F[i];
    end;
    if (fillExtra) then begin
      extras[0] := ReadMem(PC);
      extras[1] := ReadMem((PC + 1) and $FFFF);
      extras[2] := ReadMem((PC + 2) and $FFFF);
      extras[3] := ReadMem(StackBase + S) + ReadMem(StackBase + ((S + 1) and $FF)) shl 8;
    end;
  end;
end;

procedure TCPU_6502.UpdateCPUInfo();
begin;
  with FInfo do begin
    dataSize := 8;
    addrSize := 16;
    PCreg := 7;
    numRegs := 5;
    regsName := ['A', 'X', 'Y', 'S', 'PC'];
    regsSize := [1, 1, 1, 1, 2];
    numFlags := 8;
    flagsName := ['C', '-', 'P', '-', 'H', '-', 'Z', 'S'];
    numExtras := 4;
    extrasName := ['(PC)', '(PC+1)', '(PC+2)', '(S)'];
    extrasSize := [1, 1, 1, 2];
    littleEndian := True;
    numIRQs := 3;
    IRQsName := ['Reset', 'NMI', 'IRQ'];
    IRQsNMI := [True, True, False];
  end;
end;

procedure TCPU_6502.FlagsFromByte(const aByte: iSize8);
var
  i: integer;
begin
  for i := 0 to 7 do begin
    F[i] := (aByte and bit_val[i]) <> 0;
  end;
end;

function TCPU_6502.FlagsToByte: iSize8;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 7 do begin
    if F[i] then Result := Result or bit_val[i];
  end;
end;

procedure TCPU_6502.DoIRQ(int: integer);
begin
  inherited;
  case (int) of
    0: ;
  end;
end;

procedure TCPU_6502.push_PC();
begin
  S := (S - 1) and $FF;
  WriteMem(StackBase + S, PC and $FF);
  S := (S - 1) and $FF;
  WriteMem(StackBase + S, PC shr 8);
end;

procedure TCPU_6502.pull_PC();
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(StackBase + S);
  S := (S + 1) and $FF;
  b2 := ReadMem(StackBase + S);
  S := (S + 1) and $FF;
  PC := b1 + b2 shr 8;
end;

procedure TCPU_6502.push(v: iSize8);
begin
  S := (S - 1) and $FF;
  WriteMem(StackBase + S, v and $FF);
end;

function TCPU_6502.pull(): iSize8;
begin
  Result := ReadMem(StackBase + S);
  S := (S + 1) and $FF;
end;

procedure TCPU_6502.op_Illegal;
begin

end;

procedure TCPU_6502.op_ADC_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_ADC_abs_idY;
var
  t, v: iSize8;
begin
  v := data_abs_idx(Y);
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_ADC_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_ADC_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_ADC_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx(X);
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_ADC_dp_idX_ind;
var
  t, v: iSize8;
begin
  v := data_dp_idx_ind(X);
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_ADC_dp_ind;
var
  t, v: iSize8;
begin
  v := data_dp_ind();
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_ADC_dp_ind_idY;
var
  t, v: iSize8;
begin
  v := data_dp_ind_idx(Y);
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_ADC_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A + v + bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_AND_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_AND_abs_idY;
var
  t, v: iSize8;
begin
  v := data_abs_idx(Y);
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_AND_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_AND_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_AND_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx(X);
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_AND_dp_idX_ind;
var
  v: iSize8;
begin
  v := data_dp_idx_ind(X);
  A := A and v;
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_AND_dp_ind;
var
  t, v: iSize8;
begin
  v := data_dp_ind();
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_AND_dp_ind_idY;
var
  t, v: iSize8;
begin
  v := data_dp_ind_idx(Y);
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_AND_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A and v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_ASL_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_ASL_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 9);
end;

procedure TCPU_6502.op_ASL_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_ASL_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx(X);
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_ASL;
var
  t: iSize8;
begin
  t := A shl 1;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_BCC_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_C] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_BCS_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_C] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_BEQ_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_Z] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_BIT_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A and v;
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_BIT_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A and v;
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_BMI_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_N] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_BNE_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_Z] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_BPL_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_N] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_BRK;
begin
  F[Flag_D] := False;
  F[Flag_I] := True;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_BVC_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_V] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_BVS_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_V] then begin
    PC := (PC + rel) and $FFFF;
    Inc(cycles, 3);
  end
  else begin
    Inc(cycles, 2);
  end;
end;

procedure TCPU_6502.op_CLC;
begin
  F[Flag_C] := False;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_CLD;
begin
  F[Flag_D] := False;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_CLI;
begin
  F[Flag_I] := False;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_CLV;
begin
  F[Flag_Z] := False;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_CMP_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_CMP_abs_idY;
var
  t, v: iSize8;
begin
  v := data_abs_idx(Y);
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_CMP_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_CMP_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_CMP_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx(X);
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_CMP_dp_idX_ind;
var
  t, v: iSize8;
begin
  v := data_dp_idx_ind(X);
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_CMP_dp_ind;
var
  t, v: iSize8;
begin
  v := data_dp_ind();
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_CMP_dp_ind_idY;
var
  t, v: iSize8;
begin
  v := data_dp_ind_idx(Y);
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_CMP_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_CPX_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := X - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_CPX_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := X - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_CPX_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := X - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_CPY_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := Y - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_CPY_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := Y - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_CPY_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := Y - v;
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_DEC_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := v - 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_DEC_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := v - 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 9);
end;

procedure TCPU_6502.op_DEC_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := v - 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_DEC_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx(X);
  v := ReadMem(addr);
  t := v - 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_DEC;
var
  t: iSize8;
begin
  t := A - 1;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_DEX;
var
  t: iSize8;
begin
  t := X - 1;
  X := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_DEY;
var
  t: iSize8;
begin
  t := Y - 1;
  Y := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_EOR_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_EOR_abs_idY;
var
  t, v: iSize8;
begin
  v := data_abs_idx(Y);
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_EOR_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_EOR_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_EOR_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx(X);
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_EOR_dp_idX_ind;
var
  t, v: iSize8;
begin
  v := data_dp_idx_ind(X);
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_EOR_dp_ind;
var
  t, v: iSize8;
begin
  v := data_dp_ind();
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_EOR_dp_ind_idY;
var
  t, v: iSize8;
begin
  v := data_dp_ind_idx(Y);
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_EOR_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A xor v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_INC_abs;
var
  t, v: iSize8;
  MA: iSize16;
begin
  MA := addr_abs();
  v := ReadMem(MA);
  t := v + 1;
  WriteMem(MA, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_INC_abs_idX;
var
  t, v: iSize8;
  MA: iSize16;
begin
  MA := addr_abs_idx(X);
  v := ReadMem(MA);
  t := v + 1;
  WriteMem(MA, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 9);
end;

procedure TCPU_6502.op_INC_dp;
var
  t, v: iSize8;
  MA: iSize16;
begin
  MA := addr_dp();
  v := ReadMem(MA);
  t := v + 1;
  WriteMem(MA, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_INC_dp_idX;
var
  t, MV: iSize8;
  MA: iSize16;
begin
  MA := addr_dp_idx(X);
  MV := ReadMem(MA);
  t := MV + 1;
  WriteMem(MA, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_INC;
var
  t: iSize8;
begin
  t := A + 1;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_INX;
var
  t: iSize8;
begin
  t := X + 1;
  X := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_INY;
var
  t: iSize8;
begin
  t := Y + 1;
  Y := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_JMP_abs;
begin
  PC := addr_abs();
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_JMP_abs_idX_ind;
begin
  PC := addr_abs_idx_ind(X);
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_JMP_abs_ind;
begin
  PC := addr_abs_ind();
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_JSR_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  push_PC();
  PC := addr;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_JSR_abs_idX_ind;
var
  addr: iSize16;
begin
  addr := addr_abs_idx_ind(X);
  push_PC();
  PC := addr;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_LDA_abs;
begin
  A := data_abs();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_LDA_abs_idY;
begin
  A := data_abs_idx(Y);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_LDA_abs_idX;
begin
  A := data_abs_idx(X);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_LDA_dp;
begin
  A := data_dp();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_LDA_dp_idX;
begin
  A := data_dp_idx(X);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_LDA_dp_idX_ind;
begin
  A := data_dp_idx_ind(X);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_LDA_dp_ind;
begin
  A := data_dp_ind;
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_LDA_dp_ind_idY;
begin
  A := data_dp_ind_idx(Y);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_LDA_imm;
begin
  A := imm8();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_LDX_abs;
begin
  X := data_abs();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_LDX_abs_idY;
begin
  X := data_abs_idx(Y);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_LDX_dp;
begin
  X := data_dp();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_LDX_dp_idY;
begin
  X := data_dp_idx(Y);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_LDX_imm;
begin
  X := imm8();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_LDY_abs;
begin
  Y := data_abs();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_LDY_abs_idX;
begin
  Y := data_abs_idx(X);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_LDY_dp;
begin
  Y := data_dp();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_LDY_dp_idX;
begin
  Y := data_dp_idx(X);
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_LDY_imm;
begin
  Y := imm8();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_LSR_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_C] := (t and $100) = $100;
  F[Flag_N] := False;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_LSR_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_C] := (t and $100) = $100;
  F[Flag_N] := False;
  Inc(cycles, 9);
end;

procedure TCPU_6502.op_LSR_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_C] := (t and $100) = $100;
  F[Flag_N] := False;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_LSR_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx(X);
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_C] := (t and $100) = $100;
  F[Flag_N] := False;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_LSR;
var
  t: iSize8;
begin
  t := A shr 1;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_C] := (t and $100) = $100;
  F[Flag_N] := False;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_NOP;
begin
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_ORA_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_ORA_abs_idY;
var
  t, v: iSize8;
begin
  v := data_abs_idx(Y);
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_ORA_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_ORA_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_ORA_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx(X);
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_ORA_dp_idX_ind;
var
  t, v: iSize8;
begin
  v := data_dp_idx_ind(X);
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_ORA_dp_ind;
var
  t, v: iSize8;
begin
  v := data_dp_ind();
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_ORA_dp_ind_idY;
var
  t, v: iSize8;
begin
  v := data_dp_idx(Y);
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_ORA_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A or v;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_PHA;
begin
  push(A);
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_PHP;
begin
  push(FlagsToByte());
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_PHX;
begin
  push(X);
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_PHY;
begin
  push(Y);
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_PLA;
begin
  A := pull();
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_PLP;
begin
  FlagsFromByte(pull());
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_PLX;
begin
  X := pull();
  F[Flag_Z] := (X = 0);
  F[Flag_N] := (X and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_PLY;
begin
  Y := pull();
  F[Flag_Z] := (Y = 0);
  F[Flag_N] := (Y and $80) = $80;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_ROL_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_ROL_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 9);
end;

procedure TCPU_6502.op_ROL_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_ROL_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx(X);
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_ROL;
var
  t: iSize8;
begin
  t := A shl 1;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_ROR_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_ROR_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 9);
end;

procedure TCPU_6502.op_ROR_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_ROR_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx(X);
  v := ReadMem(addr);
  t := v shr 1;
  WriteMem(addr, t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 8);
end;

procedure TCPU_6502.op_ROR;
var
  t: iSize8;
begin
  t := A shr 1;
  A := (t and $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_RTI;
begin
  FlagsFromByte(pull());
  pull_PC();
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_RTS;
begin
  pull_PC();
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_SBC_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_SBC_abs_idY;
var
  t, v: iSize8;
begin
  v := data_abs_idx(Y);
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_SBC_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_SBC_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_SBC_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx(X);
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_SBC_dp_idX_ind;
var
  t, v: iSize8;
begin
  v := data_dp_idx_ind(X);
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_SBC_dp_ind;
var
  t, v: iSize8;
begin
  v := data_dp_ind();
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_SBC_dp_ind_idY;
var
  t, v: iSize8;
begin
  v := data_dp_ind_idx(Y);
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_SBC_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A - v - bool_bit[F[Flag_C]];
  A := (t and $FF);
  F[Flag_V] := (t > $FF);
  F[Flag_Z] := (t = 0);
  F[Flag_N] := (t and $80) = $80;
  F[Flag_C] := (t and $100) = $100;
  Inc(cycles, 3);
end;

procedure TCPU_6502.op_SEC;
begin
  F[Flag_C] := True;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_SED;
begin
  F[Flag_D] := True;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_SEI;
begin
  F[Flag_I] := True;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_STA_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  WriteMem(addr, A and $FF);
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_STA_abs_idY;
var
  addr: iSize16;
begin
  addr := addr_abs_idx(Y);
  WriteMem(addr, A and $FF);
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_STA_abs_idX;
var
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  WriteMem(addr, A and $FF);
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_STA_dp;
var
  addr: iSize16;
begin
  addr := addr_dp();
  WriteMem(addr, A and $FF);
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_STA_dp_idX;
var
  addr: iSize16;
begin
  addr := addr_dp_idx(X);
  WriteMem(addr, A and $FF);
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_STA_dp_idX_ind;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_ind(X);
  WriteMem(addr, A and $FF);
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_STA_dp_ind;
var
  addr: iSize16;
begin
  addr := addr_dp_ind();
  WriteMem(addr, A and $FF);
  Inc(cycles, 6);
end;

procedure TCPU_6502.op_STA_dp_ind_idY;
var
  addr: iSize16;
begin
  addr := addr_dp_ind_idx(Y);
  WriteMem(addr, A and $FF);
  Inc(cycles, 7);
end;

procedure TCPU_6502.op_STX_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  WriteMem(addr, X and $FF);
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_STX_dp;
var
  addr: iSize16;
begin
  addr := addr_dp();
  WriteMem(addr, X and $FF);
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_STX_dp_idY;
var
  addr: iSize16;
begin
  addr := addr_dp_idx(Y);
  WriteMem(addr, X and $FF);
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_STY_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  WriteMem(addr, Y and $FF);
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_STY_dp;
var
  addr: iSize16;
begin
  addr := addr_dp();
  WriteMem(addr, Y and $FF);
  Inc(cycles, 4);
end;

procedure TCPU_6502.op_STY_dp_idX;
var
  addr: iSize16;
begin
  addr := addr_dp_idx(X);
  WriteMem(addr, Y and $FF);
  Inc(cycles, 5);
end;

procedure TCPU_6502.op_TAX;
begin
  X := A;
  F[Flag_Z] := (X = 0);
  F[Flag_N] := (X and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_TAY;
begin
  Y := A;
  F[Flag_Z] := (Y = 0);
  F[Flag_N] := (Y and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_TSX;
begin
  X := S;
  F[Flag_Z] := (X = 0);
  F[Flag_N] := (X and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_TXA;
begin
  A := X;
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_TXS;
begin
  S := X;
  Inc(cycles, 2);
end;

procedure TCPU_6502.op_TYA;
begin
  A := Y;
  F[Flag_Z] := (A = 0);
  F[Flag_N] := (A and $80) = $80;
  Inc(cycles, 2);
end;

end.

