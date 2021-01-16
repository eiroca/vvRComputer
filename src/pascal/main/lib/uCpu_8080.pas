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
  TFlags = array [0..7] of boolean;

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
    SP: iSize16;
    F: TFlags;
  protected
    // 8080 Status
    interruptAllowed: boolean;
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
    procedure Reset; override;
  protected
    procedure GetCPUInfo(var info: RCPUInfo); override;
    procedure GetStatus(var status: RCPUStatus; fillExtra: boolean = True); override;
  end;

implementation

constructor TCPU_8080.Create;
begin
  inherited;
  SetLength(OpCodes, 256);
  with OpCodes[$00] do begin code := @op_NOP; inst := 'NOP'; fmt := 'NOP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$01] do begin code := @op_LXI_B; inst := 'LXI'; fmt := 'LXI' + TAB + 'B,$%.4x'; mode := Emode.imm; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$02] do begin code := @op_STAX_B; inst := 'STAX'; fmt := 'STAX' + TAB + 'B'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$03] do begin code := @op_INX_B; inst := 'INX'; fmt := 'INX' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$04] do begin code := @op_INR_B; inst := 'INR'; fmt := 'INR' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$05] do begin code := @op_DCR_B; inst := 'DCR'; fmt := 'DCR' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$06] do begin code := @op_MVI_B; inst := 'MVI'; fmt := 'MVI' + TAB + 'B,$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$07] do begin code := @op_RLC; inst := 'RLC'; fmt := 'RLC'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$08] do begin code := @op_NotSup; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$09] do begin code := @op_DAD_B; inst := 'DAD'; fmt := 'DAD' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$0A] do begin code := @op_LDAX_B; inst := 'LDAX'; fmt := 'LDAX' + TAB + 'B'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$0B] do begin code := @op_DCX_B; inst := 'DCX'; fmt := 'DCX' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$0C] do begin code := @op_INR_C; inst := 'INR'; fmt := 'INR' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$0D] do begin code := @op_DCR_C; inst := 'DCR'; fmt := 'DCR' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$0E] do begin code := @op_MVI_C; inst := 'MVI'; fmt := 'MVI' + TAB + 'C,$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$0F] do begin code := @op_RRC; inst := 'RRC'; fmt := 'RRC'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$10] do begin code := @op_NotSup; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$11] do begin code := @op_LXI_D; inst := 'LXI'; fmt := 'LXI' + TAB + 'D,$%.4x'; mode := Emode.imm; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$12] do begin code := @op_STAX_D; inst := 'STAX'; fmt := 'STAX' + TAB + 'D'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$13] do begin code := @op_INX_D; inst := 'INX'; fmt := 'INX' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$14] do begin code := @op_INR_D; inst := 'INR'; fmt := 'INR' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$15] do begin code := @op_DCR_D; inst := 'DCR'; fmt := 'DCR' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$16] do begin code := @op_MVI_D; inst := 'MVI'; fmt := 'MVI' + TAB + 'D,$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$17] do begin code := @op_RAL; inst := 'RAL'; fmt := 'RAL'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$18] do begin code := @op_NotSup; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$19] do begin code := @op_DAD_D; inst := 'DAD'; fmt := 'DAD' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$1A] do begin code := @op_LDAX_D; inst := 'LDAX'; fmt := 'LDAX' + TAB + 'D'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$1B] do begin code := @op_DCX_D; inst := 'DCX'; fmt := 'DCX' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$1C] do begin code := @op_INR_E; inst := 'INR'; fmt := 'INR' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$1D] do begin code := @op_DCR_E; inst := 'DCR'; fmt := 'DCR' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$1E] do begin code := @op_MVI_E; inst := 'MVI'; fmt := 'MVI' + TAB + 'E,$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$1F] do begin code := @op_RAR; inst := 'RAR'; fmt := 'RAR'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$20] do begin code := @op_NotSup; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$21] do begin code := @op_LXI_H; inst := 'LXI'; fmt := 'LXI' + TAB + 'H,$%.4x'; mode := Emode.imm; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$22] do begin code := @op_SHLD; inst := 'SHLD'; fmt := 'SHLD' + TAB + '$%.4x'; mode := Emode.ind_abs; len := 3; cycle := [5]; state := [16]; end;
  with OpCodes[$23] do begin code := @op_INX_H; inst := 'INX'; fmt := 'INX' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$24] do begin code := @op_INR_H; inst := 'INR'; fmt := 'INR' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$25] do begin code := @op_DCR_H; inst := 'DCR'; fmt := 'DCR' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$26] do begin code := @op_MVI_H; inst := 'MVI'; fmt := 'MVI' + TAB + 'H,$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$27] do begin code := @op_DAA; inst := 'DAA'; fmt := 'DAA'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$28] do begin code := @op_NotSup; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$29] do begin code := @op_DAD_H; inst := 'DAD'; fmt := 'DAD' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$2A] do begin code := @op_LHLD; inst := 'LHLD'; fmt := 'LHLD' + TAB + '$%.4x'; mode := Emode.ind_abs; len := 3; cycle := [5]; state := [16]; end;
  with OpCodes[$2B] do begin code := @op_DCX_H; inst := 'DCX'; fmt := 'DCX' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$2C] do begin code := @op_INR_L; inst := 'INR'; fmt := 'INR' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$2D] do begin code := @op_DCR_L; inst := 'DCR'; fmt := 'DCR' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$2E] do begin code := @op_MVI_L; inst := 'MVI'; fmt := 'MVI' + TAB + 'L,$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$2F] do begin code := @op_CMA; inst := 'CMA'; fmt := 'CMA'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$30] do begin code := @op_NotSup; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$31] do begin code := @op_LXI_SP; inst := 'LXI'; fmt := 'LXI' + TAB + 'SP,$%.4x'; mode := Emode.imm; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$32] do begin code := @op_STA; inst := 'STA'; fmt := 'STA' + TAB + '$%.4x'; mode := Emode.abs; len := 3; cycle := [4]; state := [13]; end;
  with OpCodes[$33] do begin code := @op_INX_SP; inst := 'INX'; fmt := 'INX' + TAB + 'SP'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$34] do begin code := @op_INR_M; inst := 'INR'; fmt := 'INR' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$35] do begin code := @op_DCR_M; inst := 'DCR'; fmt := 'DCR' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$36] do begin code := @op_MVI_Mi; inst := 'MVI'; fmt := 'MVI' + TAB + 'M,$%.2x'; mode := Emode.ind_reg; len := 2; cycle := [3]; state := [10]; end;
  with OpCodes[$37] do begin code := @op_STC; inst := 'STC'; fmt := 'STC'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$38] do begin code := @op_NotSup; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$39] do begin code := @op_DAD_SP; inst := 'DAD'; fmt := 'DAD' + TAB + 'SP'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$3A] do begin code := @op_LDA; inst := 'LDA'; fmt := 'LDA' + TAB + '$%.4x'; mode := Emode.abs; len := 3; cycle := [4]; state := [13]; end;
  with OpCodes[$3B] do begin code := @op_DCX_SP; inst := 'DCX'; fmt := 'DCX' + TAB + 'SP'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$3C] do begin code := @op_INR_A; inst := 'INR'; fmt := 'INR' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$3D] do begin code := @op_DCR_A; inst := 'DCR'; fmt := 'DCR' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$3E] do begin code := @op_MVI_A; inst := 'MVI'; fmt := 'MVI' + TAB + 'A,$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$3F] do begin code := @op_CMC; inst := 'CMC'; fmt := 'CMC'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$40] do begin code := @op_MOV; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$41] do begin code := @op_MOV_BC; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$42] do begin code := @op_MOV_BD; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$43] do begin code := @op_MOV_BE; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$44] do begin code := @op_MOV_BH; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$45] do begin code := @op_MOV_BL; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$46] do begin code := @op_MOV_BM; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$47] do begin code := @op_MOV_BA; inst := 'MOV'; fmt := 'MOV' + TAB + 'B,A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$48] do begin code := @op_MOV_CB; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$49] do begin code := @op_MOV; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$4A] do begin code := @op_MOV_CD; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$4B] do begin code := @op_MOV_CE; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$4C] do begin code := @op_MOV_CH; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$4D] do begin code := @op_MOV_CL; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$4E] do begin code := @op_MOV_CM; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$4F] do begin code := @op_MOV_CA; inst := 'MOV'; fmt := 'MOV' + TAB + 'C,A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$50] do begin code := @op_MOV_DB; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$51] do begin code := @op_MOV_DC; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$52] do begin code := @op_MOV; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$53] do begin code := @op_MOV_DE; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$54] do begin code := @op_MOV_DH; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$55] do begin code := @op_MOV_DL; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$56] do begin code := @op_MOV_DM; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$57] do begin code := @op_MOV_DA; inst := 'MOV'; fmt := 'MOV' + TAB + 'D,A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$58] do begin code := @op_MOV_EB; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$59] do begin code := @op_MOV_EC; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$5A] do begin code := @op_MOV_ED; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$5B] do begin code := @op_MOV; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$5C] do begin code := @op_MOV_EH; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$5D] do begin code := @op_MOV_EL; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$5E] do begin code := @op_MOV_EM; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$5F] do begin code := @op_MOV_EA; inst := 'MOV'; fmt := 'MOV' + TAB + 'E,A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$60] do begin code := @op_MOV_HB; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$61] do begin code := @op_MOV_HC; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$62] do begin code := @op_MOV_HD; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$63] do begin code := @op_MOV_HE; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$64] do begin code := @op_MOV; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$65] do begin code := @op_MOV_HL; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$66] do begin code := @op_MOV_HM; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$67] do begin code := @op_MOV_HA; inst := 'MOV'; fmt := 'MOV' + TAB + 'H,A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$68] do begin code := @op_MOV_LB; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$69] do begin code := @op_MOV_LC; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$6A] do begin code := @op_MOV_LD; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$6B] do begin code := @op_MOV_LE; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$6C] do begin code := @op_MOV_LH; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$6D] do begin code := @op_MOV; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$6E] do begin code := @op_MOV_LM; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$6F] do begin code := @op_MOV_LA; inst := 'MOV'; fmt := 'MOV' + TAB + 'L,A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$70] do begin code := @op_MOV_MB; inst := 'MOV'; fmt := 'MOV' + TAB + 'M,B'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$71] do begin code := @op_MOV_MC; inst := 'MOV'; fmt := 'MOV' + TAB + 'M,C'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$72] do begin code := @op_MOV_MD; inst := 'MOV'; fmt := 'MOV' + TAB + 'M,D'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$73] do begin code := @op_MOV_ME; inst := 'MOV'; fmt := 'MOV' + TAB + 'M,E'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$74] do begin code := @op_MOV_MH; inst := 'MOV'; fmt := 'MOV' + TAB + 'M,H'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$75] do begin code := @op_MOV_ML; inst := 'MOV'; fmt := 'MOV' + TAB + 'M,L'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$76] do begin code := @op_HLT; inst := 'HLT'; fmt := 'HLT'; mode := Emode.imp; len := 1; cycle := [1]; state := [7]; end;
  with OpCodes[$77] do begin code := @op_MOV_MA; inst := 'MOV'; fmt := 'MOV' + TAB + 'M,A'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$78] do begin code := @op_MOV_AB; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,B'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$79] do begin code := @op_MOV_AC; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,C'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$7A] do begin code := @op_MOV_AD; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,D'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$7B] do begin code := @op_MOV_AE; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,E'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$7C] do begin code := @op_MOV_AH; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,H'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$7D] do begin code := @op_MOV_AL; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,L'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$7E] do begin code := @op_MOV_AM; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$7F] do begin code := @op_MOV; inst := 'MOV'; fmt := 'MOV' + TAB + 'A,A'; mode := Emode.reg; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$80] do begin code := @op_ADD_B; inst := 'ADD'; fmt := 'ADD' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$81] do begin code := @op_ADD_C; inst := 'ADD'; fmt := 'ADD' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$82] do begin code := @op_ADD_D; inst := 'ADD'; fmt := 'ADD' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$83] do begin code := @op_ADD_E; inst := 'ADD'; fmt := 'ADD' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$84] do begin code := @op_ADD_H; inst := 'ADD'; fmt := 'ADD' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$85] do begin code := @op_ADD_L; inst := 'ADD'; fmt := 'ADD' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$86] do begin code := @op_ADD_M; inst := 'ADD'; fmt := 'ADD' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$87] do begin code := @op_ADD_A; inst := 'ADD'; fmt := 'ADD' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$88] do begin code := @op_ADC_B; inst := 'ADC'; fmt := 'ADC' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$89] do begin code := @op_ADC_C; inst := 'ADC'; fmt := 'ADC' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$8A] do begin code := @op_ADC_D; inst := 'ADC'; fmt := 'ADC' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$8B] do begin code := @op_ADC_E; inst := 'ADC'; fmt := 'ADC' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$8C] do begin code := @op_ADC_H; inst := 'ADC'; fmt := 'ADC' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$8D] do begin code := @op_ADC_L; inst := 'ADC'; fmt := 'ADC' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$8E] do begin code := @op_ADC_M; inst := 'ADC'; fmt := 'ADC' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$8F] do begin code := @op_ADC_A; inst := 'ADC'; fmt := 'ADC' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$90] do begin code := @op_SUB_B; inst := 'SUB'; fmt := 'SUB' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$91] do begin code := @op_SUB_C; inst := 'SUB'; fmt := 'SUB' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$92] do begin code := @op_SUB_D; inst := 'SUB'; fmt := 'SUB' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$93] do begin code := @op_SUB_E; inst := 'SUB'; fmt := 'SUB' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$94] do begin code := @op_SUB_H; inst := 'SUB'; fmt := 'SUB' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$95] do begin code := @op_SUB_L; inst := 'SUB'; fmt := 'SUB' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$96] do begin code := @op_SUB_M; inst := 'SUB'; fmt := 'SUB' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$97] do begin code := @op_SUB_A; inst := 'SUB'; fmt := 'SUB' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$98] do begin code := @op_SBB_B; inst := 'SBB'; fmt := 'SBB' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$99] do begin code := @op_SBB_C; inst := 'SBB'; fmt := 'SBB' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$9A] do begin code := @op_SBB_D; inst := 'SBB'; fmt := 'SBB' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$9B] do begin code := @op_SBB_E; inst := 'SBB'; fmt := 'SBB' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$9C] do begin code := @op_SBB_H; inst := 'SBB'; fmt := 'SBB' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$9D] do begin code := @op_SBB_L; inst := 'SBB'; fmt := 'SBB' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$9E] do begin code := @op_SBB_M; inst := 'SBB'; fmt := 'SBB' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$9F] do begin code := @op_SBB_A; inst := 'SBB'; fmt := 'SBB' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A0] do begin code := @op_ANA_B; inst := 'ANA'; fmt := 'ANA' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A1] do begin code := @op_ANA_C; inst := 'ANA'; fmt := 'ANA' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A2] do begin code := @op_ANA_D; inst := 'ANA'; fmt := 'ANA' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A3] do begin code := @op_ANA_E; inst := 'ANA'; fmt := 'ANA' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A4] do begin code := @op_ANA_H; inst := 'ANA'; fmt := 'ANA' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A5] do begin code := @op_ANA_L; inst := 'ANA'; fmt := 'ANA' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A6] do begin code := @op_ANA_M; inst := 'ANA'; fmt := 'ANA' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$A7] do begin code := @op_ANA_A; inst := 'ANA'; fmt := 'ANA' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A8] do begin code := @op_XRA_B; inst := 'XRA'; fmt := 'XRA' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$A9] do begin code := @op_XRA_C; inst := 'XRA'; fmt := 'XRA' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$AA] do begin code := @op_XRA_D; inst := 'XRA'; fmt := 'XRA' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$AB] do begin code := @op_XRA_E; inst := 'XRA'; fmt := 'XRA' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$AC] do begin code := @op_XRA_H; inst := 'XRA'; fmt := 'XRA' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$AD] do begin code := @op_XRA_L; inst := 'XRA'; fmt := 'XRA' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$AE] do begin code := @op_XRA_M; inst := 'XRA'; fmt := 'XRA' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$AF] do begin code := @op_XRA_A; inst := 'XRA'; fmt := 'XRA' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B0] do begin code := @op_ORA_B; inst := 'ORA'; fmt := 'ORA' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B1] do begin code := @op_ORA_C; inst := 'ORA'; fmt := 'ORA' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B2] do begin code := @op_ORA_D; inst := 'ORA'; fmt := 'ORA' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B3] do begin code := @op_ORA_E; inst := 'ORA'; fmt := 'ORA' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B4] do begin code := @op_ORA_H; inst := 'ORA'; fmt := 'ORA' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B5] do begin code := @op_ORA_L; inst := 'ORA'; fmt := 'ORA' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B6] do begin code := @op_ORA_M; inst := 'ORA'; fmt := 'ORA' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$B7] do begin code := @op_ORA_A; inst := 'ORA'; fmt := 'ORA' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B8] do begin code := @op_CMP_B; inst := 'CMP'; fmt := 'CMP' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$B9] do begin code := @op_CMP_C; inst := 'CMP'; fmt := 'CMP' + TAB + 'C'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$BA] do begin code := @op_CMP_D; inst := 'CMP'; fmt := 'CMP' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$BB] do begin code := @op_CMP_E; inst := 'CMP'; fmt := 'CMP' + TAB + 'E'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$BC] do begin code := @op_CMP_H; inst := 'CMP'; fmt := 'CMP' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$BD] do begin code := @op_CMP_L; inst := 'CMP'; fmt := 'CMP' + TAB + 'L'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$BE] do begin code := @op_CMP_M; inst := 'CMP'; fmt := 'CMP' + TAB + 'M'; mode := Emode.ind_reg; len := 1; cycle := [2]; state := [7]; end;
  with OpCodes[$BF] do begin code := @op_CMP_A; inst := 'CMP'; fmt := 'CMP' + TAB + 'A'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$C0] do begin code := @op_RNZ; inst := 'RNZ'; fmt := 'RNZ'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$C1] do begin code := @op_POP_B; inst := 'POP'; fmt := 'POP' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$C2] do begin code := @op_JNZ; inst := 'JNZ'; fmt := 'JNZ' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$C3] do begin code := @op_JMP; inst := 'JMP'; fmt := 'JMP' + TAB + '$%.4x'; mode := Emode.jmp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$C4] do begin code := @op_CNZ; inst := 'CNZ'; fmt := 'CNZ' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$C5] do begin code := @op_PUSH_B; inst := 'PUSH'; fmt := 'PUSH' + TAB + 'B'; mode := Emode.reg; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$C6] do begin code := @op_ADI; inst := 'ADI'; fmt := 'ADI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$C7] do begin code := @op_RST_0; inst := 'RST'; fmt := 'RST' + TAB + '0'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$C8] do begin code := @op_RZ; inst := 'RZ'; fmt := 'RZ'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$C9] do begin code := @op_RET; inst := 'RET'; fmt := 'RET'; mode := Emode.imp; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$CA] do begin code := @op_JZ; inst := 'JZ'; fmt := 'JZ' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$CB] do begin code := @op_IlgJmp; inst := 'NOT_SUP'; fmt := 'NOT_SUP' + TAB + '$%.4x'; mode := Emode.jmp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$CC] do begin code := @op_CZ; inst := 'CZ'; fmt := 'CZ' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$CD] do begin code := @op_CALL; inst := 'CALL'; fmt := 'CALL' + TAB + '$%.4x'; mode := Emode.jmp_abs; len := 3; cycle := [5]; state := [17]; end;
  with OpCodes[$CE] do begin code := @op_ACI; inst := 'ACI'; fmt := 'ACI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$CF] do begin code := @op_RST_1; inst := 'RST'; fmt := 'RST' + TAB + '1'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$D0] do begin code := @op_RNC; inst := 'RNC'; fmt := 'RNC'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$D1] do begin code := @op_POP_D; inst := 'POP'; fmt := 'POP' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$D2] do begin code := @op_JNC; inst := 'JNC'; fmt := 'JNC' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$D3] do begin code := @op_OUT; inst := 'OUT'; fmt := 'OUT' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [3]; state := [10]; end;
  with OpCodes[$D4] do begin code := @op_CNC; inst := 'CNC'; fmt := 'CNC' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$D5] do begin code := @op_PUSH_D; inst := 'PUSH'; fmt := 'PUSH' + TAB + 'D'; mode := Emode.reg; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$D6] do begin code := @op_SUI; inst := 'SUI'; fmt := 'SUI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$D7] do begin code := @op_RST_2; inst := 'RST'; fmt := 'RST' + TAB + '2'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$D8] do begin code := @op_RC; inst := 'RC'; fmt := 'RC'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$D9] do begin code := @op_IlgRet; inst := 'NOT_SUP'; fmt := 'NOT_SUP'; mode := Emode.imp; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$DA] do begin code := @op_JC; inst := 'JC'; fmt := 'JC' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$DB] do begin code := @op_IN; inst := 'IN'; fmt := 'IN' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [3]; state := [10]; end;
  with OpCodes[$DC] do begin code := @op_CC; inst := 'CC'; fmt := 'CC' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$DD] do begin code := @op_IlgCall; inst := 'NOT_SUP'; fmt := 'NOT_SUP' + TAB + '$%.4x'; mode := Emode.jmp_abs; len := 3; cycle := [5]; state := [17]; end;
  with OpCodes[$DE] do begin code := @op_SBI; inst := 'SBI'; fmt := 'SBI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$DF] do begin code := @op_RST_3; inst := 'RST'; fmt := 'RST' + TAB + '3'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$E0] do begin code := @op_RPO; inst := 'RPO'; fmt := 'RPO'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$E1] do begin code := @op_POP_H; inst := 'POP'; fmt := 'POP' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$E2] do begin code := @op_JPO; inst := 'JPO'; fmt := 'JPO' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$E3] do begin code := @op_XTHL; inst := 'XTHL'; fmt := 'XTHL'; mode := Emode.imp; len := 1; cycle := [5]; state := [18]; end;
  with OpCodes[$E4] do begin code := @op_CPO; inst := 'CPO'; fmt := 'CPO' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$E5] do begin code := @op_PUSH_H; inst := 'PUSH'; fmt := 'PUSH' + TAB + 'H'; mode := Emode.reg; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$E6] do begin code := @op_ANI; inst := 'ANI'; fmt := 'ANI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$E7] do begin code := @op_RST_4; inst := 'RST'; fmt := 'RST' + TAB + '4'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$E8] do begin code := @op_RPE; inst := 'RPE'; fmt := 'RPE'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$E9] do begin code := @op_PCHL; inst := 'PCHL'; fmt := 'PCHL'; mode := Emode.imp; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$EA] do begin code := @op_JPE; inst := 'JPE'; fmt := 'JPE' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$EB] do begin code := @op_XCHG; inst := 'XCHG'; fmt := 'XCHG'; mode := Emode.reg; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$EC] do begin code := @op_CPE; inst := 'CPE'; fmt := 'CPE' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$ED] do begin code := @op_IlgCall; inst := 'NOT_SUP'; fmt := 'NOT_SUP' + TAB + '$%.4x'; mode := Emode.jmp_abs; len := 3; cycle := [5]; state := [17]; end;
  with OpCodes[$EE] do begin code := @op_XRI; inst := 'XRI'; fmt := 'XRI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$EF] do begin code := @op_RST_5; inst := 'RST'; fmt := 'RST' + TAB + '5'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$F0] do begin code := @op_RP; inst := 'RP'; fmt := 'RP'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$F1] do begin code := @op_POP_PSW; inst := 'POP'; fmt := 'POP' + TAB + 'PSW'; mode := Emode.reg; len := 1; cycle := [3]; state := [10]; end;
  with OpCodes[$F2] do begin code := @op_JP; inst := 'JP'; fmt := 'JP' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$F3] do begin code := @op_DI; inst := 'DI'; fmt := 'DI'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$F4] do begin code := @op_CP; inst := 'CP'; fmt := 'CP' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$F5] do begin code := @op_PUSH_PSW; inst := 'PUSH'; fmt := 'PUSH' + TAB + 'PSW'; mode := Emode.reg; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$F6] do begin code := @op_ORI; inst := 'ORI'; fmt := 'ORI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$F7] do begin code := @op_RST_6; inst := 'RST'; fmt := 'RST' + TAB + '6'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
  with OpCodes[$F8] do begin code := @op_RM; inst := 'RM'; fmt := 'RM'; mode := Emode.cjp_abs; len := 1; cycle := [1, 3]; state := [5, 11]; end;
  with OpCodes[$F9] do begin code := @op_SPHL; inst := 'SPHL'; fmt := 'SPHL'; mode := Emode.imp; len := 1; cycle := [1]; state := [5]; end;
  with OpCodes[$FA] do begin code := @op_JM; inst := 'JM'; fmt := 'JM' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3]; state := [10]; end;
  with OpCodes[$FB] do begin code := @op_EI; inst := 'EI'; fmt := 'EI'; mode := Emode.imp; len := 1; cycle := [1]; state := [4]; end;
  with OpCodes[$FC] do begin code := @op_CM; inst := 'CM'; fmt := 'CM' + TAB + '$%.4x'; mode := Emode.cjp_abs; len := 3; cycle := [3, 5]; state := [11, 17]; end;
  with OpCodes[$FD] do begin code := @op_IlgCall; inst := 'NOT_SUP'; fmt := 'NOT_SUP' + TAB + '$%.4x'; mode := Emode.jmp_abs; len := 3; cycle := [5]; state := [17]; end;
  with OpCodes[$FE] do begin code := @op_CPI; inst := 'CPI'; fmt := 'CPI' + TAB + '$%.2x'; mode := Emode.imm; len := 2; cycle := [2]; state := [7]; end;
  with OpCodes[$FF] do begin code := @op_RST_7; inst := 'RST'; fmt := 'RST' + TAB + '7'; mode := Emode.imp; len := 1; cycle := [3]; state := [11]; end;
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
  isHalted := False;
  opers := 0;
  cycles := 0;
  states := 0;
end;

procedure TCPU_8080.GetStatus(var status: RCPUStatus; fillExtra: boolean = True);
var
  i: integer;
begin;
  with status do begin
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
      flags[i] := bool_bit[F[i]];
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

procedure TCPU_8080.GetCPUInfo(var info: RCPUInfo);
begin;
  with info do begin
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
  isHalted := True;
  Inc(cycles);
  Inc(states, 7);
  Halt;
end;

procedure TCPU_8080.op_IN;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  A := ReadIO(b1);
  Inc(cycles, 3);
  Inc(states, 10);
end;

procedure TCPU_8080.op_OUT;
var
  b1: iSize8;
begin
  b1 := ReadMem(PC);
  PC := (PC + 1) and $FFFF;
  WriteIO(b1, A);
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
  interruptAllowed := False;
  Inc(cycles);
  Inc(states, 4);
end;

procedure TCPU_8080.op_EI;
begin
  interruptAllowed := True;
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
