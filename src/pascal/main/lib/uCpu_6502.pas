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
  Flag_I = 2;
  Flag_D = 3;
  Flag_B = 4;
  Flag_U = 5;
  Flag_V = 6;
  Flag_N = 7;

type

  E6502Family = (B6502, U6502, W65C02, R65C02);

  { TCPU_6502 }

  TCPU_6502 = class(TCPU_ClassA)
  private
    DP_Base: iSize16;
    SP_Base: iSize16;
    IRQVector: iSize16;
    ResetVector: iSize16;
    NMIVector: iSize16;
  protected
    // 6502 Registers
    A: iSize8;
    X: iSize8;
    Y: iSize8;
    S: iSize8;
    Z: iSize8;
  private
    // Emulator hooks
  protected
    function addr_abs(): iSize16; inline;
    function data_abs(): iSize8; inline;
    function addr_abs_ind_c(): iSize16; inline;
    function data_abs_ind_c(): iSize16; inline;
    function addr_abs_ind_w(): iSize16; inline;
    function data_abs_ind_w(): iSize16; inline;
    function addr_abs_idx(const idx: iSize8): iSize16; inline;
    function data_abs_idx(const idx: iSize8): iSize8; inline;
    function addr_dp(): iSize16; inline;
    function data_dp(): iSize8; inline;
    function addr_dp_ind_idx_w(const idx: iSize8): iSize16; inline;
    function data_dp_ind_idx_w(const idx: iSize8): iSize8; inline;
    function addr_dp_idx_w(const idx: iSize8): iSize16; inline;
    function data_dp_idx_w(const idx: iSize8): iSize8; inline;
    function addr_dp_idx_ind_w(const idx: iSize8): iSize16; inline;
    function data_dp_idx_ind_w(const idx: iSize8): iSize8; inline;
  protected
    function addr_abs_idx_ind(const idx: iSize8): iSize16; inline;
    function data_abs_idx_ind(const idx: iSize8): iSize16; inline;
    function addr_dp_ind(): iSize16; inline;
    function data_dp_ind(): iSize8; inline;
  protected
    procedure UpdateNZ(const r: iSize8); inline;
    procedure UpdateNZC(const r: iSize8); inline;
    procedure UpdateNZV(const v, r: iSize8); inline;
  protected
    procedure push_PC(); inline;
    procedure pull_PC(); inline;
    procedure push(v: iSize8); inline;
    function pull(): iSize8; inline;
  protected
    procedure int_ADC(const v: iSize8); inline;
    procedure int_SBC(const v: iSize8); inline;
    function int_LSR(const v: iSize8): iSize8; inline;
    function int_ROL(const v: iSize8): iSize8; inline;
    function int_ROR(const v: iSize8): iSize8; inline;
    procedure int_SMB(const t: iSize8); inline;
    procedure int_RMB(const t: iSize8); inline;
    procedure int_BBR(const t: iSize8); inline;
    procedure int_BBS(const t: iSize8); inline;
  protected
    procedure FlagsFromByte(const aByte: iSize8);
    function FlagsToByte: iSize8;
  protected
    procedure op_Hang;
  protected // 6502 base opcodes
    procedure op_ADC_abs;
    procedure op_ADC_abs_idX;
    procedure op_ADC_abs_idY;
    procedure op_ADC_dp;
    procedure op_ADC_dp_idX;
    procedure op_ADC_dp_idX_ind;
    procedure op_ADC_dp_ind_idY;
    procedure op_ADC_imm;
    procedure op_AND_abs;
    procedure op_AND_abs_idX;
    procedure op_AND_abs_idY;
    procedure op_AND_dp;
    procedure op_AND_dp_idX;
    procedure op_AND_dp_idX_ind;
    procedure op_AND_dp_ind_idY;
    procedure op_AND_imm;
    procedure op_ASL_A;
    procedure op_ASL_abs;
    procedure op_ASL_abs_idX;
    procedure op_ASL_dp;
    procedure op_ASL_dp_idX;
    procedure op_BIT_abs;
    procedure op_BIT_dp;
    procedure op_BCC_rel8;
    procedure op_BCS_rel8;
    procedure op_BEQ_rel8;
    procedure op_BMI_rel8;
    procedure op_BNE_rel8;
    procedure op_BPL_rel8;
    procedure op_BVC_rel8;
    procedure op_BVS_rel8;
    procedure op_BRK;
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
    procedure op_CMP_dp_ind_idY;
    procedure op_CMP_imm;
    procedure op_CPX_abs;
    procedure op_CPX_dp;
    procedure op_CPX_imm;
    procedure op_CPY_abs;
    procedure op_CPY_dp;
    procedure op_CPY_imm;
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
    procedure op_EOR_dp_ind_idY;
    procedure op_EOR_imm;
    procedure op_INC_abs;
    procedure op_INC_abs_idX;
    procedure op_INC_dp;
    procedure op_INC_dp_idX;
    procedure op_INX;
    procedure op_INY;
    procedure op_JMP_abs;
    procedure op_JMP_abs_ind_w;
    procedure op_JSR_abs;
    procedure op_LDA_abs;
    procedure op_LDA_abs_idX;
    procedure op_LDA_abs_idY;
    procedure op_LDA_dp;
    procedure op_LDA_dp_idX;
    procedure op_LDA_dp_idX_ind;
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
    procedure op_LSR_abs;
    procedure op_LSR_abs_idX;
    procedure op_LSR_dp;
    procedure op_LSR;
    procedure op_NOP;
    procedure op_NOP_imm;
    procedure op_ORA_abs;
    procedure op_ORA_abs_idX;
    procedure op_ORA_abs_idY;
    procedure op_ORA_dp;
    procedure op_ORA_dp_idX;
    procedure op_ORA_dp_idX_ind;
    procedure op_ORA_dp_ind_idY;
    procedure op_ORA_imm;
    procedure op_PHA;
    procedure op_PHP;
    procedure op_PLA;
    procedure op_PLP;
    procedure op_ROL_A;
    procedure op_ROL_abs;
    procedure op_ROL_abs_idX;
    procedure op_ROL_dp;
    procedure op_ROL_dp_idX;
    procedure op_ROR_A;
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
  protected // 65C02 base opcodes
    procedure op_ADC_dp_ind;
    procedure op_AND_dp_ind;
    procedure op_BIT_abs_idX;
    procedure op_BIT_dp_idX;
    procedure op_BIT_imm;
    procedure op_BRA_rel8;
    procedure op_CMP_dp_ind;
    procedure op_DEC_A;
    procedure op_EOR_dp_ind;
    procedure op_INC_A;
    procedure op_JMP_abs_ind_c;
    procedure op_JMP_abs_idX_ind;
    procedure op_JSR_abs_idX_ind;
    procedure op_LDA_dp_ind;
    procedure op_LSR_dp_idX;
    procedure op_ORA_dp_ind;
    procedure op_PHX;
    procedure op_PHY;
    procedure op_PLX;
    procedure op_PLY;
    procedure op_SBC_dp_ind;
    procedure op_STA_dp_ind;
    procedure op_STZ_abs;
    procedure op_STZ_abs_idX;
    procedure op_STZ_dp;
    procedure op_STZ_dp_idX;
    procedure op_TRB_abs;
    procedure op_TRB_dp;
    procedure op_TSB_abs;
    procedure op_TSB_dp;
  protected // WDC 65C02 base opcodes
    procedure op_STP;
    procedure op_WAI;
  protected // Rockwell 65C02 base opcodes
    procedure op_BBR0_dp_rel8;
    procedure op_BBR1_dp_rel8;
    procedure op_BBR2_dp_rel8;
    procedure op_BBR3_dp_rel8;
    procedure op_BBR4_dp_rel8;
    procedure op_BBR5_dp_rel8;
    procedure op_BBR6_dp_rel8;
    procedure op_BBR7_dp_rel8;
    procedure op_BBS0_dp_rel8;
    procedure op_BBS1_dp_rel8;
    procedure op_BBS2_dp_rel8;
    procedure op_BBS3_dp_rel8;
    procedure op_BBS4_dp_rel8;
    procedure op_BBS5_dp_rel8;
    procedure op_BBS6_dp_rel8;
    procedure op_BBS7_dp_rel8;
    procedure op_RMB0_dp;
    procedure op_RMB1_dp;
    procedure op_RMB2_dp;
    procedure op_RMB3_dp;
    procedure op_RMB4_dp;
    procedure op_RMB5_dp;
    procedure op_RMB6_dp;
    procedure op_RMB7_dp;
    procedure op_SMB0_dp;
    procedure op_SMB1_dp;
    procedure op_SMB2_dp;
    procedure op_SMB3_dp;
    procedure op_SMB4_dp;
    procedure op_SMB5_dp;
    procedure op_SMB6_dp;
    procedure op_SMB7_dp;
  private
    procedure InitJAM();
    procedure Init6502();
    procedure Init6502Undoc();
    procedure InitNOP();
    procedure Init65C02();
    procedure Init65C02WDC();
    procedure Init65C02Rockwell();
  public
    constructor Create(mode: E6502Family);
  public
    procedure Reset(); override;
  protected
    procedure UpdateCPUInfo(); override;
    procedure UpdateCPUStatus(fillExtra: boolean = True); override;
    procedure DoIRQ(int: integer); override;
  end;

implementation

constructor TCPU_6502.Create(mode: E6502Family);
begin
  inherited Create;
  SetLength(OpCodes, 256);
  SP_Base := $0100;
  DP_Base := $0000;
  IRQVector := $FFFE;
  ResetVector := $FFFC;
  NMIVector := $FFFA;
  case (mode) of
    B6502: begin
      InitJAM();
      Init6502();
    end;
    U6502: begin
      InitJAM();
      Init6502();
      Init6502Undoc;
    end;
    R65C02: begin
      InitNOP();
      Init65C02();
      Init65C02Rockwell();
    end;
    W65C02: begin
      InitNOP();
      Init65C02();
      Init65C02Rockwell();
    end;
  end;
  Reset;
end;

procedure TCPU_6502.InitJAM();
var
  i: integer;
  JAM: ROpcode;
begin
  with JAM do begin
    code := @op_Hang;
    inst := 'JAM';
    fmt := 'JAM';
    group := Egroup.control;
    mode := Emode.imp;
    len := 1;
    cycle := 1;
  end;
  for i := 0 to 255 do begin
    OpCodes[i] := JAM;
  end;
end;

procedure TCPU_6502.InitNOP();
var
  i: integer;
  NOP: ROpcode;
begin
  with NOP do begin
    code := @op_NOP;
    inst := 'NOP';
    fmt := 'NOP';
    group := Egroup.control;
    mode := Emode.imp;
    len := 1;
    cycle := 2;
  end;
  for i := 0 to 255 do begin
    OpCodes[i] := NOP;
  end;
end;

procedure TCPU_6502.Init6502();
begin
  {$include CPUTable_6502.inc}
end;

procedure TCPU_6502.Init6502Undoc();
begin
  {$include CPUTable_6502U.inc}
end;

procedure TCPU_6502.Init65C02();
begin
  {$include CPUTable_65C02.inc}
end;

procedure TCPU_6502.Init65C02Rockwell();
begin
  {$include CPUTable_65C02R.inc}
end;

procedure TCPU_6502.Init65C02WDC();
begin
  {$include CPUTable_65C02W.inc}
end;

procedure TCPU_6502.Reset();
begin
  A := $00;
  X := $00;
  Y := $00;
  S := $00;
  Z := $00;
  PC := $0000;
  F[Flag_C] := False;
  F[Flag_Z] := True;
  F[Flag_I] := True;
  F[Flag_D] := False;
  F[Flag_B] := True;
  F[Flag_U] := True;
  F[Flag_V] := False;
  F[Flag_N] := False;
  state := active;
  FIRQAllowed := False;
  FOpers := 0;
  FCycles := 0;
end;

procedure TCPU_6502.UpdateCPUStatus(fillExtra: boolean = True);
var
  i: integer;
begin
  ;
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
      extras[3] := ReadMem(SP_Base + S) + ReadMem(SP_Base + ((S + 1) and $FF)) shl 8;
    end;
  end;
end;

procedure TCPU_6502.UpdateCPUInfo();
begin
  ;
  with FInfo do begin
    dataSize := 8;
    addrSize := 16;
    PCreg := 7;
    numRegs := 5;
    regsName := ['A', 'X', 'Y', 'S', 'PC'];
    regsSize := [1, 1, 1, 1, 2];
    numFlags := 8;
    flagsName := ['C', 'Z', 'I', 'D', 'B', '-', 'V', 'N'];
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
    if F[i] then begin
      Result := Result or bit_val[i];
    end;
  end;
end;

procedure TCPU_6502.push_PC();
begin
  WriteMem(SP_Base + S, PC and $FF);
  S := (S - 1) and $FF;
  WriteMem(SP_Base + S, PC shr 8);
  S := (S - 1) and $FF;
end;

procedure TCPU_6502.pull_PC();
var
  b1, b2: iSize8;
begin
  S := (S + 1) and $FF;
  b1 := ReadMem(SP_Base + S);
  S := (S + 1) and $FF;
  b2 := ReadMem(SP_Base + S);
  PC := b1 + b2 shr 8;
end;

procedure TCPU_6502.push(v: iSize8);
begin
  WriteMem(SP_Base + S, v and $FF);
  S := (S - 1) and $FF;
end;

function TCPU_6502.pull(): iSize8;
begin
  S := (S + 1) and $FF;
  Result := ReadMem(SP_Base + S);
end;

procedure TCPU_6502.DoIRQ(int: integer);
var
  vec: iSize16;
begin
  if (state = wait) then begin
    state := active;
  end;
  FIRQs[int].active := False;
  push_PC();
  push(FlagsToByte());
  case (int) of
    0: begin
      vec := ResetVector;
    end;
    1: begin
      vec := NMIVector;
    end;
    2: begin
      vec := IRQVector;
    end;
  end;
  PC := ReadMem(vec) + ReadMem(vec + 1) shl 8;
end;

procedure TCPU_6502.UpdateNZ(const r: iSize8);
begin
  F[Flag_Z] := (r and $FF) = 0;
  F[Flag_N] := (r and $80) <> 0;
end;

procedure TCPU_6502.UpdateNZC(const r: iSize8);
begin
  F[Flag_Z] := (r and $FF) = 0;
  F[Flag_N] := (r and $80) <> 0;
  F[Flag_C] := (r and $100) <> 0;
end;

procedure TCPU_6502.UpdateNZV(const v, r: iSize8);
begin
  F[Flag_Z] := (r and $FF) = 0;
  F[Flag_N] := (v and $80) <> 0;
  F[Flag_V] := (v and $40) <> 0;
end;

function TCPU_6502.addr_abs(): iSize16;
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Result := b1 + b2 shl 8;
end;

function TCPU_6502.data_abs(): iSize8;
var
  addr: iSize16;
begin
  addr := addr_abs();
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_abs_ind_c(): iSize16;
var
  b1, b2: iSize8;
  add1, add2: iSize16;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  add1 := b2 shl 8 + b1;
  add2 := (add1 + 1) and $FFFF;
  Result := ReadMem(add1) + ReadMem(add2) shl 8;
end;

function TCPU_6502.data_abs_ind_c(): iSize16;
var
  addr: iSize16;
begin
  addr := addr_abs_ind_c();
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_abs_ind_w(): iSize16;
var
  b1, b2: iSize8;
  addr, add1, add2: iSize16;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  addr := b2 shl 8;
  add1 := addr + b1;
  add2 := addr + ((b1 + 1) and $FF);
  Result := ReadMem(add1) + ReadMem(add2) shl 8;
end;

function TCPU_6502.data_abs_ind_w(): iSize16;
var
  addr: iSize16;
begin
  addr := addr_abs_ind_w();
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_abs_idx(const idx: iSize8): iSize16;
var
  b1, b2: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Result := (b1 + b2 shl 8 + idx) and $FFFF;
end;

function TCPU_6502.data_abs_idx(const idx: iSize8): iSize8;
var
  addr: iSize16;
begin
  addr := addr_abs_idx(idx);
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_dp(): iSize16;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Result := DP_Base + b1;
end;

function TCPU_6502.data_dp(): iSize8;
var
  addr: iSize16;
begin
  addr := addr_dp();
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_dp_ind_idx_w(const idx: iSize8): iSize16;
var
  b1: iSize8;
  add1, add2: iSize16;

begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  add1 := DP_Base + b1;
  add2 := (add1 and $FF00) + ((add1 + 1) and $FF);
  Result := (ReadMem(add1) + ReadMem(add2) shl 8 + idx) and $FFFF;
end;

function TCPU_6502.data_dp_ind_idx_w(const idx: iSize8): iSize8;
var
  addr: iSize16;
begin
  addr := addr_dp_ind_idx_w(idx);
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_dp_idx_w(const idx: iSize8): iSize16;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  Result := DP_Base + (b1 + idx) and $FF;
end;

function TCPU_6502.data_dp_idx_w(const idx: iSize8): iSize8;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_w(idx);
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_dp_idx_ind_w(const idx: iSize8): iSize16;
var
  b1: iSize8;
  add1, add2: iSize16;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  add1 := DP_Base + (b1 + idx) and $FF;
  add2 := (add1 and $FF00) + ((add1 + 1) and $FF);
  Result := ReadMem(add1) + ReadMem(add2) shl 8;
end;

function TCPU_6502.data_dp_idx_ind_w(const idx: iSize8): iSize8;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_ind_w(idx);
  Result := ReadMem(addr);
end;

procedure TCPU_6502.int_ADC(const v: iSize8);
var
  t, c: iSize8;
begin
  c := bool_bit[F[Flag_C]];
  t := A + v + c;
  if not F[Flag_D] then begin
    F[Flag_V] := (((A xor v) and $80) = 0) and (((A xor t) and $80) <> 0);
    UpdateNZC(A);
  end
  else begin
    if (((A and $0F) + (v and $0F) + c) > 9) then begin
      t := t + $06;
    end;
    if t > $99 then begin
      t := t + $60;
    end;
    F[Flag_Z] := (t and $FF) = 0;
    F[Flag_N] := (t and $80) <> 0;
    F[Flag_V] := (((A xor v) and $80) = 0) and (((A xor t) and $80) <> 0);
    F[Flag_C] := t > $99;
  end;
  A := (t and $FF);
end;

procedure TCPU_6502.int_SBC(const v: iSize8);
var
  t, c: iSize8;
begin
  c := negbool_bit[F[Flag_C]];
  t := A - v - c;
  F[Flag_N] := (t and $80) <> 0;
  F[Flag_Z] := (t and $FF) = 0;
  F[Flag_V] := (((A xor t) and $80) <> 0) and (((A xor v) and $80) <> 0);
  if (F[Flag_D]) then begin
    if (((A and $0F) - c) < (v and $0F)) then begin
      t := t - $06;
    end;
    if t > $99 then begin
      t := t - $60;
    end;
  end;
  F[Flag_C] := (t < $100);
  A := (t and $FF);
end;

function TCPU_6502.int_LSR(const v: iSize8): iSize8;
var
  t: iSize8;
begin
  F[Flag_C] := (v and $01) <> 0;
  t := v shr 1;
  F[Flag_Z] := False;
  F[Flag_N] := (t = 0);
  Result := t;
end;

function TCPU_6502.int_ROL(const v: iSize8): iSize8;
var
  t, c: iSize8;
begin
  c := bool_bit[F[Flag_C]];
  t := (v shl 1) or c;
  F[Flag_Z] := (t and $FF) = 0;
  F[Flag_N] := (t and $80) <> 0;
  F[Flag_C] := (t and $100) <> 0;
  Result := t and $FF;
end;

function TCPU_6502.int_ROR(const v: iSize8): iSize8;
var
  t, c: iSize8;
begin
  c := bool_bit8[F[Flag_C]];
  F[Flag_C] := (v and $01) <> 0;
  t := (v shr 1) or c;
  F[Flag_Z] := (t and $FF) = 0;
  F[Flag_N] := (t and $80) = $80;
  Result := t;
end;

procedure TCPU_6502.int_SMB(const t: iSize8);
var
  v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  v := v or t;
  WriteMem(addr, v);
end;

procedure TCPU_6502.int_RMB(const t: iSize8);
var
  v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  v := v and (not t);
  WriteMem(addr, v);
end;

procedure TCPU_6502.int_BBR(const t: iSize8);
var
  dp, rel: iSize8;
  v: iSize8;
begin
  dp := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  rel := int8(ReadMem(PC));
  PC := (PC + 1) and $FFFF;
  v := ReadMem(dp);
  if (v and t) = 0 then begin
    PC := (PC + rel) and $FFFF;
  end;
end;

procedure TCPU_6502.int_BBS(const t: iSize8);
var
  dp, rel: iSize8;
  v: iSize8;
begin
  dp := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  rel := int8(ReadMem(PC));
  PC := (PC + 1) and $FFFF;
  v := ReadMem(dp);
  if (v and t) <> 0 then begin
    PC := (PC + rel) and $FFFF;
  end;
end;

procedure TCPU_6502.op_Hang;
begin
  state := stop;
  if assigned(OnHalt) then begin
    OnHalt;
  end;
end;

procedure TCPU_6502.op_ADC_abs;
var
  v: iSize8;
begin
  v := data_abs();
  int_ADC(v);
end;

procedure TCPU_6502.op_ADC_abs_idY;
var
  v: iSize8;
begin
  v := data_abs_idx(Y);
  int_ADC(v);
end;

procedure TCPU_6502.op_ADC_abs_idX;
var
  v: iSize8;
begin
  v := data_abs_idx(X);
  int_ADC(v);
end;

procedure TCPU_6502.op_ADC_dp;
var
  v: iSize8;
begin
  v := data_dp();
  int_ADC(v);
end;

procedure TCPU_6502.op_ADC_dp_idX;
var
  v: iSize8;
begin
  v := data_dp_idx_w(X);
  int_ADC(v);
end;

procedure TCPU_6502.op_ADC_dp_idX_ind;
var
  v: iSize8;
begin
  v := data_dp_idx_ind_w(X);
  int_ADC(v);
end;


procedure TCPU_6502.op_ADC_dp_ind_idY;
var
  v: iSize8;
begin
  v := data_dp_ind_idx_w(Y);
  int_ADC(v);
end;

procedure TCPU_6502.op_ADC_imm;
var
  v: iSize8;
begin
  v := imm8();
  int_ADC(v);
end;

procedure TCPU_6502.op_AND_abs;
var
  v: iSize8;
begin
  v := data_abs();
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_AND_abs_idY;
var
  v: iSize8;
begin
  v := data_abs_idx(Y);
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_AND_abs_idX;
var
  v: iSize8;
begin
  v := data_abs_idx(X);
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_AND_dp;
var
  v: iSize8;
begin
  v := data_dp();
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_AND_dp_idX;
var
  v: iSize8;
begin
  v := data_dp_idx_w(X);
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_AND_dp_idX_ind;
var
  v: iSize8;
begin
  v := data_dp_idx_ind_w(X);
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_AND_dp_ind_idY;
var
  v: iSize8;
begin
  v := data_dp_ind_idx_w(Y);
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_AND_imm;
var
  v: iSize8;
begin
  v := imm8();
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ASL_A;
var
  t: iSize8;
begin
  t := A shl 1;
  A := (t and $FF);
  UpdateNZC(t);
end;

procedure TCPU_6502.op_ASL_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  UpdateNZC(t);
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
  UpdateNZC(t);
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
  UpdateNZC(t);
end;

procedure TCPU_6502.op_ASL_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  v := ReadMem(addr);
  t := v shl 1;
  WriteMem(addr, t and $FF);
  UpdateNZC(t);
end;

procedure TCPU_6502.op_BIT_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A and v;
  UpdateNZV(v, t);
end;

procedure TCPU_6502.op_BIT_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A and v;
  UpdateNZV(v, t);
end;

procedure TCPU_6502.op_BCC_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_C] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BCS_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_C] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BEQ_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_Z] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BNE_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_Z] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BMI_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_N] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BPL_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_N] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BVC_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if not F[Flag_V] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BVS_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  if F[Flag_V] then begin
    PC := (PC + rel) and $FFFF;
    Inc(FCycles, 1);
  end;
end;

procedure TCPU_6502.op_BRK;
begin
  imm8();
  push_PC();
  F[Flag_B] := True;
  push(FlagsToByte());
  F[Flag_I] := True;
  Pc := (ReadMem(IRQVector + 1) shl 8) + ReadMem(IRQVector);
end;

procedure TCPU_6502.op_CLC;
begin
  F[Flag_C] := False;
end;

procedure TCPU_6502.op_CLD;
begin
  F[Flag_D] := False;
end;

procedure TCPU_6502.op_CLI;
begin
  F[Flag_I] := False;
end;

procedure TCPU_6502.op_CLV;
begin
  F[Flag_Z] := False;
end;

procedure TCPU_6502.op_CMP_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CMP_abs_idY;
var
  t, v: iSize8;
begin
  v := data_abs_idx(Y);
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CMP_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CMP_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CMP_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx_w(X);
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CMP_dp_idX_ind;
var
  t, v: iSize8;
begin
  v := data_dp_idx_ind_w(X);
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CMP_dp_ind_idY;
var
  t, v: iSize8;
begin
  v := data_dp_ind_idx_w(Y);
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CMP_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CPX_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := X - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CPX_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := X - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CPX_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := X - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CPY_abs;
var
  t, v: iSize8;
begin
  v := data_abs();
  t := Y - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CPY_dp;
var
  t, v: iSize8;
begin
  v := data_dp();
  t := Y - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_CPY_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := Y - v;
  UpdateNZC(t);
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
  UpdateNZ(t);
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
  UpdateNZ(t);
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
  UpdateNZ(t);
end;

procedure TCPU_6502.op_DEC_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  v := ReadMem(addr);
  t := v - 1;
  WriteMem(addr, t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_DEX;
var
  t: iSize8;
begin
  t := X - 1;
  X := (t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_DEY;
var
  t: iSize8;
begin
  t := Y - 1;
  Y := (t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_EOR_abs;
var
  v: iSize8;
begin
  v := data_abs();
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_abs_idY;
var
  v: iSize8;
begin
  v := data_abs_idx(Y);
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_abs_idX;
var
  v: iSize8;
begin
  v := data_abs_idx(X);
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_dp;
var
  v: iSize8;
begin
  v := data_dp();
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_dp_idX;
var
  v: iSize8;
begin
  v := data_dp_idx_w(X);
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_dp_idX_ind;
var
  v: iSize8;
begin
  v := data_dp_idx_ind_w(X);
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_dp_ind_idY;
var
  v: iSize8;
begin
  v := data_dp_ind_idx_w(Y);
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_imm;
var
  v: iSize8;
begin
  v := imm8();
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_INC_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := v + 1;
  WriteMem(addr, t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_INC_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := v + 1;
  WriteMem(addr, t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_INC_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := v + 1;
  WriteMem(addr, t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_INC_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  v := ReadMem(addr);
  t := v + 1;
  WriteMem(addr, t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_INX;
var
  t: iSize8;
begin
  t := X + 1;
  X := (t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_INY;
var
  t: iSize8;
begin
  t := Y + 1;
  Y := (t and $FF);
  UpdateNZ(t);
end;

procedure TCPU_6502.op_JMP_abs;
begin
  imm_PC();
end;

procedure TCPU_6502.op_JMP_abs_ind_w;
begin
  PC := addr_abs_ind_w();
end;

procedure TCPU_6502.op_JSR_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  push(PC - 1);
  PC := addr;
end;

procedure TCPU_6502.op_LDA_abs;
begin
  A := data_abs();
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDA_abs_idY;
begin
  A := data_abs_idx(Y);
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDA_abs_idX;
begin
  A := data_abs_idx(X);
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDA_dp;
begin
  A := data_dp();
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDA_dp_idX;
begin
  A := data_dp_idx_w(X);
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDA_dp_idX_ind;
begin
  A := data_dp_idx_ind_w(X);
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDA_dp_ind_idY;
begin
  A := data_dp_ind_idx_w(Y);
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDA_imm;
begin
  A := imm8();
  UpdateNZ(A);
end;

procedure TCPU_6502.op_LDX_abs;
begin
  X := data_abs();
  UpdateNZ(X);
end;

procedure TCPU_6502.op_LDX_abs_idY;
begin
  X := data_abs_idx(Y);
  UpdateNZ(X);
end;

procedure TCPU_6502.op_LDX_dp;
begin
  X := data_dp();
  UpdateNZ(X);
end;

procedure TCPU_6502.op_LDX_dp_idY;
begin
  X := data_dp_idx_w(Y);
  UpdateNZ(X);
end;

procedure TCPU_6502.op_LDX_imm;
begin
  X := imm8();
  UpdateNZ(X);
end;

procedure TCPU_6502.op_LDY_abs;
begin
  Y := data_abs();
  UpdateNZ(Y);
end;

procedure TCPU_6502.op_LDY_abs_idX;
begin
  Y := data_abs_idx(X);
  UpdateNZ(Y);
end;

procedure TCPU_6502.op_LDY_dp;
begin
  Y := data_dp();
  UpdateNZ(Y);
end;

procedure TCPU_6502.op_LDY_dp_idX;
begin
  Y := data_dp_idx_w(X);
  UpdateNZ(Y);
end;

procedure TCPU_6502.op_LDY_imm;
begin
  Y := imm8();
  UpdateNZ(Y);
end;

procedure TCPU_6502.op_LSR_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := int_LSR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_LSR_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := int_LSR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_LSR_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := int_LSR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_LSR;
begin
  A := int_LSR(A);
end;

procedure TCPU_6502.op_NOP;
begin
end;

procedure TCPU_6502.op_NOP_imm;
begin
  imm8();
end;

procedure TCPU_6502.op_ORA_abs;
var
  v: iSize8;
begin
  v := data_abs();
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_abs_idY;
var
  v: iSize8;
begin
  v := data_abs_idx(Y);
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_abs_idX;
var
  v: iSize8;
begin
  v := data_abs_idx(X);
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_dp;
var
  v: iSize8;
begin
  v := data_dp();
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_dp_idX;
var
  v: iSize8;
begin
  v := data_dp_idx_w(X);
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_dp_idX_ind;
var
  v: iSize8;
begin
  v := data_dp_idx_ind_w(X);
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_dp_ind_idY;
var
  v: iSize8;
begin
  v := data_dp_idx_w(Y);
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_imm;
var
  v: iSize8;
begin
  v := imm8();
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_PHA;
begin
  push(A);
end;

procedure TCPU_6502.op_PHP;
begin
  push(FlagsToByte());
end;

procedure TCPU_6502.op_PLA;
begin
  A := pull();
  UpdateNZ(A);
end;

procedure TCPU_6502.op_PLP;
begin
  FlagsFromByte(pull());
  F[Flag_U] := True;
end;

procedure TCPU_6502.op_ROL_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := int_ROL(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROL_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := int_ROL(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROL_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := int_ROL(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROL_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  v := ReadMem(addr);
  t := int_ROL(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROL_A;
begin
  A := int_ROL(A);
end;

procedure TCPU_6502.op_ROR_abs;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  t := int_ROR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROR_abs_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  v := ReadMem(addr);
  t := int_ROR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROR_dp;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  t := int_ROR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROR_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  v := ReadMem(addr);
  t := int_ROR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_ROR_A;
begin
  A := int_ROR(A);
end;

procedure TCPU_6502.op_RTI;
begin
  FlagsFromByte(pull());
  pull_PC();
end;

procedure TCPU_6502.op_RTS;
begin
  PC := pull() + 1;
end;

procedure TCPU_6502.op_SBC_abs;
var
  v: iSize8;
begin
  v := data_abs();
  int_SBC(v);
end;

procedure TCPU_6502.op_SBC_abs_idY;
var
  v: iSize8;
begin
  v := data_abs_idx(Y);
  int_SBC(v);
end;

procedure TCPU_6502.op_SBC_abs_idX;
var
  v: iSize8;
begin
  v := data_abs_idx(X);
  int_SBC(v);
end;

procedure TCPU_6502.op_SBC_dp;
var
  v: iSize8;
begin
  v := data_dp();
  int_SBC(v);
end;

procedure TCPU_6502.op_SBC_dp_idX;
var
  v: iSize8;
begin
  v := data_dp_idx_w(X);
  int_SBC(v);
end;

procedure TCPU_6502.op_SBC_dp_idX_ind;
var
  v: iSize8;
begin
  v := data_dp_idx_ind_w(X);
  int_SBC(v);
end;

procedure TCPU_6502.op_SBC_dp_ind_idY;
var
  v: iSize8;
begin
  v := data_dp_ind_idx_w(Y);
  int_SBC(v);
end;

procedure TCPU_6502.op_SBC_imm;
var
  v: iSize8;
begin
  v := imm8();
  int_SBC(v);
end;

procedure TCPU_6502.op_SEC;
begin
  F[Flag_C] := True;
end;

procedure TCPU_6502.op_SED;
begin
  F[Flag_D] := True;
end;

procedure TCPU_6502.op_SEI;
begin
  F[Flag_I] := True;
end;

procedure TCPU_6502.op_STA_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STA_abs_idY;
var
  addr: iSize16;
begin
  addr := addr_abs_idx(Y);
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STA_abs_idX;
var
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STA_dp;
var
  addr: iSize16;
begin
  addr := addr_dp();
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STA_dp_idX;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STA_dp_idX_ind;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_ind_w(X);
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STA_dp_ind_idY;
var
  addr: iSize16;
begin
  addr := addr_dp_ind_idx_w(Y);
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STX_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  WriteMem(addr, X);
end;

procedure TCPU_6502.op_STX_dp;
var
  addr: iSize16;
begin
  addr := addr_dp();
  WriteMem(addr, X);
end;

procedure TCPU_6502.op_STX_dp_idY;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_w(Y);
  WriteMem(addr, X);
end;

procedure TCPU_6502.op_STY_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  WriteMem(addr, Y);
end;

procedure TCPU_6502.op_STY_dp;
var
  addr: iSize16;
begin
  addr := addr_dp();
  WriteMem(addr, Y);
end;

procedure TCPU_6502.op_STY_dp_idX;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  WriteMem(addr, Y);
end;

procedure TCPU_6502.op_TAX;
begin
  X := A;
  UpdateNZ(X);
end;

procedure TCPU_6502.op_TAY;
begin
  Y := A;
  UpdateNZ(Y);
end;

procedure TCPU_6502.op_TSX;
begin
  X := S;
  UpdateNZ(X);
end;

procedure TCPU_6502.op_TXA;
begin
  A := X;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_TXS;
begin
  S := X;
  UpdateNZ(S);
end;

procedure TCPU_6502.op_TYA;
begin
  A := Y;
  UpdateNZ(A);
end;

function TCPU_6502.addr_abs_idx_ind(const idx: iSize8): iSize16;
var
  b1, b2: iSize8;
  addr: iSize16;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  b2 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  addr := (b1 + b2 shl 8 + idx) and $FFFF;
  Result := ReadMem(addr) + ReadMem((addr + 1) and $FFFF) shl 8;
end;

{ TCPU_65C02 }


function TCPU_6502.data_abs_idx_ind(const idx: iSize8): iSize16;
var
  addr: iSize16;
begin
  addr := addr_abs_idx_ind(idx);
  Result := ReadMem(addr);
end;

function TCPU_6502.addr_dp_ind(): iSize16;
var
  b1: iSize8;
  addr: iSize16;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  addr := b1;
  Result := ReadMem(addr) + ReadMem(addr + 1) shl 8;
end;

function TCPU_6502.data_dp_ind(): iSize8;
var
  addr: iSize16;
begin
  addr := addr_dp_ind();
  Result := ReadMem(addr);
end;

procedure TCPU_6502.op_ADC_dp_ind;
var
  v: iSize8;
begin
  v := data_dp_ind();
  int_ADC(v);
end;

procedure TCPU_6502.op_AND_dp_ind;
var
  v: iSize8;
begin
  v := data_dp_ind();
  A := A and v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_BIT_abs_idX;
var
  t, v: iSize8;
begin
  v := data_abs_idx(X);
  t := A and v;
  UpdateNZV(v, t);
end;

procedure TCPU_6502.op_BIT_dp_idX;
var
  t, v: iSize8;
begin
  v := data_dp_idx_w(X);
  t := A and v;
  UpdateNZV(v, t);
end;

procedure TCPU_6502.op_BIT_imm;
var
  t, v: iSize8;
begin
  v := imm8();
  t := A and v;
  UpdateNZV(v, t);
end;

procedure TCPU_6502.op_BRA_rel8;
var
  rel: iSize8;
begin
  rel := rel8();
  PC := (PC + rel) and $FFFF;
end;

procedure TCPU_6502.op_CMP_dp_ind;
var
  t, v: iSize8;
begin
  v := data_dp_ind();
  t := A - v;
  UpdateNZC(t);
end;

procedure TCPU_6502.op_DEC_A;
begin
  A := (A - 1) and $FF;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_EOR_dp_ind;
var
  v: iSize8;
begin
  v := data_dp_ind();
  A := A xor v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_INC_A;
begin
  A := (A + 1) and $FF;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_JMP_abs_ind_c;
begin
  PC := addr_abs_ind_c();
end;

procedure TCPU_6502.op_JMP_abs_idX_ind;
begin
  PC := addr_abs_idx_ind(X);
end;

procedure TCPU_6502.op_JSR_abs_idX_ind;
var
  addr: iSize16;
begin
  addr := addr_abs_idx_ind(X);
  push(PC - 1);
  PC := addr;
end;

procedure TCPU_6502.op_LDA_dp_ind;
begin
  A := data_dp_ind;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_ORA_dp_ind;
var
  v: iSize8;
begin
  v := data_dp_ind();
  A := A or v;
  UpdateNZ(A);
end;

procedure TCPU_6502.op_SBC_dp_ind;
var
  v: iSize8;
begin
  v := data_dp_ind();
  int_SBC(v);
end;

procedure TCPU_6502.op_STA_dp_ind;
var
  addr: iSize16;
begin
  addr := addr_dp_ind();
  WriteMem(addr, A);
end;

procedure TCPU_6502.op_STZ_abs;
var
  addr: iSize16;
begin
  addr := addr_abs();
  WriteMem(addr, Z);
end;

procedure TCPU_6502.op_STZ_abs_idX;
var
  addr: iSize16;
begin
  addr := addr_abs_idx(X);
  WriteMem(addr, Z);
end;

procedure TCPU_6502.op_STZ_dp;
var
  addr: iSize16;
begin
  addr := addr_dp();
  WriteMem(addr, Z);
end;

procedure TCPU_6502.op_STZ_dp_idX;
var
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  WriteMem(addr, Z);
end;

procedure TCPU_6502.op_TRB_abs;
var
  v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  F[Flag_Z] := (v and A) = 0;
  v := v and (not A);
  WriteMem(addr, v);
end;

procedure TCPU_6502.op_TRB_dp;
var
  v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  F[Flag_Z] := (v and A) = 0;
  v := v and (not A);
  WriteMem(addr, v);
end;

procedure TCPU_6502.op_TSB_abs;
var
  v: iSize8;
  addr: iSize16;
begin
  addr := addr_abs();
  v := ReadMem(addr);
  F[Flag_Z] := (v and A) = 0;
  v := v or A;
  WriteMem(addr, v);
end;

procedure TCPU_6502.op_TSB_dp;
var
  v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp();
  v := ReadMem(addr);
  F[Flag_Z] := (v and A) = 0;
  v := v or A;
  WriteMem(addr, v);
end;


procedure TCPU_6502.op_LSR_dp_idX;
var
  t, v: iSize8;
  addr: iSize16;
begin
  addr := addr_dp_idx_w(X);
  v := ReadMem(addr);
  t := int_LSR(v);
  WriteMem(addr, t);
end;

procedure TCPU_6502.op_PHX;
begin
  push(X);
end;

procedure TCPU_6502.op_PHY;
begin
  push(Y);
end;

procedure TCPU_6502.op_PLX;
begin
  X := pull();
  UpdateNZ(X);
end;

procedure TCPU_6502.op_PLY;
begin
  Y := pull();
  UpdateNZ(Y);
end;

procedure TCPU_6502.op_STP;
begin
  state := stop;
end;

procedure TCPU_6502.op_WAI;
begin
  state := wait;
end;

procedure TCPU_6502.op_BBR0_dp_rel8;
begin
  int_BBR($01);
end;

procedure TCPU_6502.op_BBR1_dp_rel8;
begin
  int_BBR($02);
end;

procedure TCPU_6502.op_BBR2_dp_rel8;
begin
  int_BBR($04);
end;

procedure TCPU_6502.op_BBR3_dp_rel8;
begin
  int_BBR($08);
end;

procedure TCPU_6502.op_BBR4_dp_rel8;
begin
  int_BBR($10);
end;

procedure TCPU_6502.op_BBR5_dp_rel8;
begin
  int_BBR($20);
end;

procedure TCPU_6502.op_BBR6_dp_rel8;
begin
  int_BBR($40);
end;

procedure TCPU_6502.op_BBR7_dp_rel8;
begin
  int_BBR($80);
end;

procedure TCPU_6502.op_BBS0_dp_rel8;
begin
  int_BBS($01);
end;

procedure TCPU_6502.op_BBS1_dp_rel8;
begin
  int_BBS($02);
end;

procedure TCPU_6502.op_BBS2_dp_rel8;
begin
  int_BBS($04);
end;

procedure TCPU_6502.op_BBS3_dp_rel8;
begin
  int_BBS($08);
end;

procedure TCPU_6502.op_BBS4_dp_rel8;
begin
  int_BBS($10);
end;

procedure TCPU_6502.op_BBS5_dp_rel8;
begin
  int_BBS($20);
end;

procedure TCPU_6502.op_BBS6_dp_rel8;
begin
  int_BBS($40);
end;

procedure TCPU_6502.op_BBS7_dp_rel8;
begin
  int_BBS($80);
end;

procedure TCPU_6502.op_RMB0_dp;
begin
  int_RMB(%00000001);
end;

procedure TCPU_6502.op_RMB1_dp;
begin
  int_RMB(%00000010);
end;

procedure TCPU_6502.op_RMB2_dp;
begin
  int_RMB(%00000100);
end;

procedure TCPU_6502.op_RMB3_dp;
begin
  int_RMB(%00001000);
end;

procedure TCPU_6502.op_RMB4_dp;
begin
  int_RMB(%00010000);
end;

procedure TCPU_6502.op_RMB5_dp;
begin
  int_RMB(%00100000);
end;

procedure TCPU_6502.op_RMB6_dp;
begin
  int_RMB(%01000000);
end;

procedure TCPU_6502.op_RMB7_dp;
begin
  int_RMB(%10000000);
end;

procedure TCPU_6502.op_SMB0_dp;
begin
  int_SMB(%00000001);
end;

procedure TCPU_6502.op_SMB1_dp;
begin
  int_SMB(%00000010);
end;

procedure TCPU_6502.op_SMB2_dp;
begin
  int_SMB(%00000100);
end;

procedure TCPU_6502.op_SMB3_dp;
begin
  int_SMB(%00001000);
end;

procedure TCPU_6502.op_SMB4_dp;
begin
  int_SMB(%00010000);
end;

procedure TCPU_6502.op_SMB5_dp;
begin
  int_SMB(%00100000);
end;

procedure TCPU_6502.op_SMB6_dp;
begin
  int_SMB(%01000000);
end;

procedure TCPU_6502.op_SMB7_dp;
begin
  int_SMB(%10000000);
end;

end.
