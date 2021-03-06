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
(* *) with OpCodes[$00] do begin code:= @op_BRK; inst:= 'BRK'; fmt:= 'BRK'; group:= Egroup.control; mode:= Emode.imm; len:= 2; cycle:= 7; end;
(* *) with OpCodes[$01] do begin code:= @op_ORA_dp_idX_ind; inst:= 'ORA'; fmt:= 'ORA'+#9+'($%.2x,X)'; group:= Egroup.logic; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$04] do begin code:= @op_TSB_dp; inst:= 'TSB'; fmt:= 'TSB'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$05] do begin code:= @op_ORA_dp; inst:= 'ORA'; fmt:= 'ORA'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$06] do begin code:= @op_ASL_dp; inst:= 'ASL'; fmt:= 'ASL'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$08] do begin code:= @op_PHP; inst:= 'PHP'; fmt:= 'PHP'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 3; end;
(* *) with OpCodes[$09] do begin code:= @op_ORA_imm; inst:= 'ORA'; fmt:= 'ORA'+#9+'#%.2x'; group:= Egroup.logic; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$0A] do begin code:= @op_ASL_A; inst:= 'ASL'; fmt:= 'ASL'+#9+'A'; group:= Egroup.logic; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$0C] do begin code:= @op_TSB_abs; inst:= 'TSB'; fmt:= 'TSB'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$0D] do begin code:= @op_ORA_abs; inst:= 'ORA'; fmt:= 'ORA'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$0E] do begin code:= @op_ASL_abs; inst:= 'ASL'; fmt:= 'ASL'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$10] do begin code:= @op_BPL_rel8; inst:= 'BPL'; fmt:= 'BPL'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$11] do begin code:= @op_ORA_dp_ind_idY; inst:= 'ORA'; fmt:= 'ORA'+#9+'($%.2x),Y'; group:= Egroup.logic; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$12] do begin code:= @op_ORA_dp_ind; inst:= 'ORA'; fmt:= 'ORA'+#9+'($%.2x)'; group:= Egroup.logic; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$14] do begin code:= @op_TRB_dp; inst:= 'TRB'; fmt:= 'TRB'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$15] do begin code:= @op_ORA_dp_idX; inst:= 'ORA'; fmt:= 'ORA'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$16] do begin code:= @op_ASL_dp_idX; inst:= 'ASL'; fmt:= 'ASL'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.dp_idx; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$18] do begin code:= @op_CLC; inst:= 'CLC'; fmt:= 'CLC'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$19] do begin code:= @op_ORA_abs_idY; inst:= 'ORA'; fmt:= 'ORA'+#9+'$%.4x,Y'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$1A] do begin code:= @op_INC_A; inst:= 'INC'; fmt:= 'INC'+#9+'A'; group:= Egroup.arith; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$1C] do begin code:= @op_TRB_abs; inst:= 'TRB'; fmt:= 'TRB'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$1D] do begin code:= @op_ORA_abs_idX; inst:= 'ORA'; fmt:= 'ORA'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$1E] do begin code:= @op_ASL_abs_idX; inst:= 'ASL'; fmt:= 'ASL'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 7; end;
(* *) with OpCodes[$20] do begin code:= @op_JSR_abs; inst:= 'JSR'; fmt:= 'JSR'+#9+'$%.4x'; group:= Egroup.branch; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$21] do begin code:= @op_AND_dp_idX_ind; inst:= 'AND'; fmt:= 'AND'+#9+'($%.2x,X)'; group:= Egroup.logic; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$24] do begin code:= @op_BIT_dp; inst:= 'BIT'; fmt:= 'BIT'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$25] do begin code:= @op_AND_dp; inst:= 'AND'; fmt:= 'AND'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$26] do begin code:= @op_ROL_dp; inst:= 'ROL'; fmt:= 'ROL'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$28] do begin code:= @op_PLP; inst:= 'PLP'; fmt:= 'PLP'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 4; end;
(* *) with OpCodes[$29] do begin code:= @op_AND_imm; inst:= 'AND'; fmt:= 'AND'+#9+'#%.2x'; group:= Egroup.logic; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$2A] do begin code:= @op_ROL_A; inst:= 'ROL'; fmt:= 'ROL'+#9+'A'; group:= Egroup.logic; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$2C] do begin code:= @op_BIT_abs; inst:= 'BIT'; fmt:= 'BIT'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$2D] do begin code:= @op_AND_abs; inst:= 'AND'; fmt:= 'AND'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$2E] do begin code:= @op_ROL_abs; inst:= 'ROL'; fmt:= 'ROL'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$30] do begin code:= @op_BMI_rel8; inst:= 'BMI'; fmt:= 'BMI'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$31] do begin code:= @op_AND_dp_ind_idY; inst:= 'AND'; fmt:= 'AND'+#9+'($%.2x),Y'; group:= Egroup.logic; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$32] do begin code:= @op_AND_dp_ind; inst:= 'AND'; fmt:= 'AND'+#9+'($%.2x)'; group:= Egroup.logic; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$34] do begin code:= @op_BIT_dp_idX; inst:= 'BIT'; fmt:= 'BIT'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.imp; len:= 1; cycle:= 4; end;
(* *) with OpCodes[$35] do begin code:= @op_AND_dp_idX; inst:= 'AND'; fmt:= 'AND'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$36] do begin code:= @op_ROL_dp_idX; inst:= 'ROL'; fmt:= 'ROL'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.dp_idx; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$38] do begin code:= @op_SEC; inst:= 'SEC'; fmt:= 'SEC'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$39] do begin code:= @op_AND_abs_idY; inst:= 'AND'; fmt:= 'AND'+#9+'$%.4x,Y'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$3A] do begin code:= @op_DEC_A; inst:= 'DEC'; fmt:= 'DEC'+#9+'A'; group:= Egroup.arith; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$3C] do begin code:= @op_BIT_abs_idX; inst:= 'BIT'; fmt:= 'BIT'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$3D] do begin code:= @op_AND_abs_idX; inst:= 'AND'; fmt:= 'AND'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$3E] do begin code:= @op_ROL_abs_idX; inst:= 'ROL'; fmt:= 'ROL'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 7; end;
(* *) with OpCodes[$40] do begin code:= @op_RTI; inst:= 'RTI'; fmt:= 'RTI'; group:= Egroup.branch; mode:= Emode.imp; len:= 1; cycle:= 6; end;
(* *) with OpCodes[$41] do begin code:= @op_EOR_dp_idX_ind; inst:= 'EOR'; fmt:= 'EOR'+#9+'($%.2x,X)'; group:= Egroup.logic; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$45] do begin code:= @op_EOR_dp; inst:= 'EOR'; fmt:= 'EOR'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$46] do begin code:= @op_LSR_dp; inst:= 'LSR'; fmt:= 'LSR'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$48] do begin code:= @op_PHA; inst:= 'PHA'; fmt:= 'PHA'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 3; end;
(* *) with OpCodes[$49] do begin code:= @op_EOR_imm; inst:= 'EOR'; fmt:= 'EOR'+#9+'#%.2x'; group:= Egroup.logic; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$4A] do begin code:= @op_LSR; inst:= 'LSR'; fmt:= 'LSR'; group:= Egroup.logic; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$4C] do begin code:= @op_JMP_abs; inst:= 'JMP'; fmt:= 'JMP'+#9+'$%.4x'; group:= Egroup.branch; mode:= Emode.abs; len:= 3; cycle:= 3; end;
(* *) with OpCodes[$4D] do begin code:= @op_EOR_abs; inst:= 'EOR'; fmt:= 'EOR'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$4E] do begin code:= @op_LSR_abs; inst:= 'LSR'; fmt:= 'LSR'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$50] do begin code:= @op_BVC_rel8; inst:= 'BVC'; fmt:= 'BVC'+#9+'%d'; group:= Egroup.branch; mode:= Emode.dp_ind; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$51] do begin code:= @op_EOR_dp_ind_idY; inst:= 'EOR'; fmt:= 'EOR'+#9+'($%.2x),Y'; group:= Egroup.logic; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$52] do begin code:= @op_EOR_dp_ind; inst:= 'EOR'; fmt:= 'EOR'+#9+'($%.2x)'; group:= Egroup.logic; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$55] do begin code:= @op_EOR_dp_idX; inst:= 'EOR'; fmt:= 'EOR'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$56] do begin code:= @op_LSR_dp_idX; inst:= 'LSR'; fmt:= 'LSR'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.dp_idx; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$58] do begin code:= @op_CLI; inst:= 'CLI'; fmt:= 'CLI'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$59] do begin code:= @op_EOR_abs_idY; inst:= 'EOR'; fmt:= 'EOR'+#9+'$%.4x,Y'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$5A] do begin code:= @op_PHY; inst:= 'PHY'; fmt:= 'PHY'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 3; end;
(* *) with OpCodes[$5D] do begin code:= @op_EOR_abs_idX; inst:= 'EOR'; fmt:= 'EOR'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$5E] do begin code:= @op_LSR_abs_idX; inst:= 'LSR'; fmt:= 'LSR'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 7; end;
(* *) with OpCodes[$60] do begin code:= @op_RTS; inst:= 'RTS'; fmt:= 'RTS'; group:= Egroup.branch; mode:= Emode.imp; len:= 1; cycle:= 6; end;
(* *) with OpCodes[$61] do begin code:= @op_ADC_dp_idX_ind; inst:= 'ADC'; fmt:= 'ADC'+#9+'($%.2x,X)'; group:= Egroup.arith; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$64] do begin code:= @op_STZ_dp; inst:= 'STZ'; fmt:= 'STZ'+#9+'$%.2x'; group:= Egroup.data; mode:= Emode.dp; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$65] do begin code:= @op_ADC_dp; inst:= 'ADC'; fmt:= 'ADC'+#9+'$%.2x'; group:= Egroup.arith; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$66] do begin code:= @op_ROR_dp; inst:= 'ROR'; fmt:= 'ROR'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$68] do begin code:= @op_PLA; inst:= 'PLA'; fmt:= 'PLA'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 4; end;
(* *) with OpCodes[$69] do begin code:= @op_ADC_imm; inst:= 'ADC'; fmt:= 'ADC'+#9+'#%.2x'; group:= Egroup.arith; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$6A] do begin code:= @op_ROR_A; inst:= 'ROR'; fmt:= 'ROR'+#9+'A'; group:= Egroup.logic; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$6C] do begin code:= @op_JMP_abs_ind_c; inst:= 'JMP'; fmt:= 'JMP'+#9+'($%.4x)'; group:= Egroup.branch; mode:= Emode.abs_ind; len:= 3; cycle:= 5; end;
(* *) with OpCodes[$6D] do begin code:= @op_ADC_abs; inst:= 'ADC'; fmt:= 'ADC'+#9+'$%.4x'; group:= Egroup.arith; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$6E] do begin code:= @op_ROR_abs; inst:= 'ROR'; fmt:= 'ROR'+#9+'$%.4x'; group:= Egroup.logic; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$70] do begin code:= @op_BVS_rel8; inst:= 'BVS'; fmt:= 'BVS'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$71] do begin code:= @op_ADC_dp_ind_idY; inst:= 'ADC'; fmt:= 'ADC'+#9+'($%.2x),Y'; group:= Egroup.arith; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$72] do begin code:= @op_ADC_dp_ind; inst:= 'ADC'; fmt:= 'ADC'+#9+'($%.2x)'; group:= Egroup.arith; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$74] do begin code:= @op_STZ_dp_idX; inst:= 'STZ'; fmt:= 'STZ'+#9+'$%.2x,X'; group:= Egroup.data; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$75] do begin code:= @op_ADC_dp_idX; inst:= 'ADC'; fmt:= 'ADC'+#9+'$%.2x,X'; group:= Egroup.arith; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$76] do begin code:= @op_ROR_dp_idX; inst:= 'ROR'; fmt:= 'ROR'+#9+'$%.2x,X'; group:= Egroup.logic; mode:= Emode.dp_idx; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$78] do begin code:= @op_SEI; inst:= 'SEI'; fmt:= 'SEI'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$79] do begin code:= @op_ADC_abs_idY; inst:= 'ADC'; fmt:= 'ADC'+#9+'$%.4x,Y'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$7A] do begin code:= @op_PLY; inst:= 'PLY'; fmt:= 'PLY'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 4; end;
(* *) with OpCodes[$7C] do begin code:= @op_JMP_abs_idX_ind; inst:= 'JMP'; fmt:= 'JMP'+#9+'($%.4x,X)'; group:= Egroup.branch; mode:= Emode.abs_idx_ind; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$7D] do begin code:= @op_ADC_abs_idX; inst:= 'ADC'; fmt:= 'ADC'+#9+'$%.4x,X'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$7E] do begin code:= @op_ROR_abs_idX; inst:= 'ROR'; fmt:= 'ROR'+#9+'$%.4x,X'; group:= Egroup.logic; mode:= Emode.abs_idx; len:= 3; cycle:= 7; end;
(* *) with OpCodes[$80] do begin code:= @op_BRA_rel8; inst:= 'BRA'; fmt:= 'BRA'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$81] do begin code:= @op_STA_dp_idX_ind; inst:= 'STA'; fmt:= 'STA'+#9+'($%.2x,X)'; group:= Egroup.data; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$84] do begin code:= @op_STY_dp; inst:= 'STY'; fmt:= 'STY'+#9+'$%.2x'; group:= Egroup.data; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$85] do begin code:= @op_STA_dp; inst:= 'STA'; fmt:= 'STA'+#9+'$%.2x'; group:= Egroup.data; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$86] do begin code:= @op_STX_dp; inst:= 'STX'; fmt:= 'STX'+#9+'$%.2x'; group:= Egroup.data; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$88] do begin code:= @op_DEY; inst:= 'DEY'; fmt:= 'DEY'; group:= Egroup.arith; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$89] do begin code:= @op_BIT_imm; inst:= 'BIT'; fmt:= 'BIT'+#9+'#%.2x'; group:= Egroup.logic; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$8A] do begin code:= @op_TXA; inst:= 'TXA'; fmt:= 'TXA'; group:= Egroup.data; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$8C] do begin code:= @op_STY_abs; inst:= 'STY'; fmt:= 'STY'+#9+'$%.4x'; group:= Egroup.data; mode:= Emode.abs; len:= 3; cycle:= 3; end;
(* *) with OpCodes[$8D] do begin code:= @op_STA_abs; inst:= 'STA'; fmt:= 'STA'+#9+'$%.4x'; group:= Egroup.data; mode:= Emode.abs; len:= 3; cycle:= 3; end;
(* *) with OpCodes[$8E] do begin code:= @op_STX_abs; inst:= 'STX'; fmt:= 'STX'+#9+'$%.4x'; group:= Egroup.data; mode:= Emode.abs; len:= 3; cycle:= 3; end;
(* *) with OpCodes[$90] do begin code:= @op_BCC_rel8; inst:= 'BCC'; fmt:= 'BCC'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$91] do begin code:= @op_STA_dp_ind_idY; inst:= 'STA'; fmt:= 'STA'+#9+'($%.2x),Y'; group:= Egroup.data; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$92] do begin code:= @op_STA_dp_ind; inst:= 'STA'; fmt:= 'STA'+#9+'($%.2x)'; group:= Egroup.data; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$94] do begin code:= @op_STY_dp_idX; inst:= 'STY'; fmt:= 'STY'+#9+'$%.2x,X'; group:= Egroup.data; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$95] do begin code:= @op_STA_dp_idX; inst:= 'STA'; fmt:= 'STA'+#9+'$%.2x,X'; group:= Egroup.data; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$96] do begin code:= @op_STX_dp_idY; inst:= 'STX'; fmt:= 'STX'+#9+'$%.2x,Y'; group:= Egroup.data; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$98] do begin code:= @op_TYA; inst:= 'TYA'; fmt:= 'TYA'; group:= Egroup.data; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$99] do begin code:= @op_STA_abs_idY; inst:= 'STA'; fmt:= 'STA'+#9+'$%.4x,Y'; group:= Egroup.data; mode:= Emode.abs_idx; len:= 3; cycle:= 5; end;
(* *) with OpCodes[$9A] do begin code:= @op_TXS; inst:= 'TXS'; fmt:= 'TXS'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$9C] do begin code:= @op_STZ_abs; inst:= 'STZ'; fmt:= 'STZ'+#9+'$%.4x'; group:= Egroup.data; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$9D] do begin code:= @op_STA_abs_idX; inst:= 'STA'; fmt:= 'STA'+#9+'$%.4x,X'; group:= Egroup.data; mode:= Emode.abs_idx; len:= 3; cycle:= 5; end;
(* *) with OpCodes[$9E] do begin code:= @op_STZ_abs_idX; inst:= 'STZ'; fmt:= 'STZ'+#9+'$%.4x,X'; group:= Egroup.data; mode:= Emode.abs_idx; len:= 3; cycle:= 5; end;
(* *) with OpCodes[$A0] do begin code:= @op_LDY_imm; inst:= 'LDY'; fmt:= 'LDY'+#9+'#%.2x'; group:= Egroup.data; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$A1] do begin code:= @op_LDA_dp_idX_ind; inst:= 'LDA'; fmt:= 'LDA'+#9+'($%.2x,X)'; group:= Egroup.data; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$A2] do begin code:= @op_LDX_imm; inst:= 'LDX'; fmt:= 'LDX'+#9+'#%.2x'; group:= Egroup.data; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$A4] do begin code:= @op_LDY_dp; inst:= 'LDY'; fmt:= 'LDY'+#9+'$%.2x'; group:= Egroup.data; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$A5] do begin code:= @op_LDA_dp; inst:= 'LDA'; fmt:= 'LDA'+#9+'$%.2x'; group:= Egroup.data; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$A6] do begin code:= @op_LDX_dp; inst:= 'LDX'; fmt:= 'LDX'+#9+'$%.2x'; group:= Egroup.data; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$A8] do begin code:= @op_TAY; inst:= 'TAY'; fmt:= 'TAY'; group:= Egroup.data; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$A9] do begin code:= @op_LDA_imm; inst:= 'LDA'; fmt:= 'LDA'+#9+'#%.2x'; group:= Egroup.data; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$AA] do begin code:= @op_TAX; inst:= 'TAX'; fmt:= 'TAX'; group:= Egroup.data; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$AC] do begin code:= @op_LDY_abs; inst:= 'LDY'; fmt:= 'LDY'+#9+'$%.4x'; group:= Egroup.data; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$AD] do begin code:= @op_LDA_abs; inst:= 'LDA'; fmt:= 'LDA'+#9+'$%.4x'; group:= Egroup.data; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$AE] do begin code:= @op_LDX_abs; inst:= 'LDX'; fmt:= 'LDX'+#9+'$%.4x'; group:= Egroup.data; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$B0] do begin code:= @op_BCS_rel8; inst:= 'BCS'; fmt:= 'BCS'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$B1] do begin code:= @op_LDA_dp_ind_idY; inst:= 'LDA'; fmt:= 'LDA'+#9+'($%.2x),Y'; group:= Egroup.data; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$B2] do begin code:= @op_LDA_dp_ind; inst:= 'LDA'; fmt:= 'LDA'+#9+'($%.2x)'; group:= Egroup.data; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$B4] do begin code:= @op_LDY_dp_idX; inst:= 'LDY'; fmt:= 'LDY'+#9+'$%.2x,X'; group:= Egroup.data; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$B5] do begin code:= @op_LDA_dp_idX; inst:= 'LDA'; fmt:= 'LDA'+#9+'$%.2x,X'; group:= Egroup.data; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$B6] do begin code:= @op_LDX_dp_idY; inst:= 'LDX'; fmt:= 'LDX'+#9+'$%.2x,Y'; group:= Egroup.data; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$B8] do begin code:= @op_CLV; inst:= 'CLV'; fmt:= 'CLV'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$B9] do begin code:= @op_LDA_abs_idY; inst:= 'LDA'; fmt:= 'LDA'+#9+'$%.4x,Y'; group:= Egroup.data; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$BA] do begin code:= @op_TSX; inst:= 'TSX'; fmt:= 'TSX'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$BC] do begin code:= @op_LDY_abs_idX; inst:= 'LDY'; fmt:= 'LDY'+#9+'$%.4x,X'; group:= Egroup.data; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$BD] do begin code:= @op_LDA_abs_idX; inst:= 'LDA'; fmt:= 'LDA'+#9+'$%.4x,X'; group:= Egroup.data; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$BE] do begin code:= @op_LDX_abs_idY; inst:= 'LDX'; fmt:= 'LDX'+#9+'$%.4x,Y'; group:= Egroup.data; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$C0] do begin code:= @op_CPY_imm; inst:= 'CPY'; fmt:= 'CPY'+#9+'#%.2x'; group:= Egroup.arith; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$C1] do begin code:= @op_CMP_dp_idX_ind; inst:= 'CMP'; fmt:= 'CMP'+#9+'($%.2x,X)'; group:= Egroup.arith; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$C4] do begin code:= @op_CPY_dp; inst:= 'CPY'; fmt:= 'CPY'+#9+'$%.2x'; group:= Egroup.arith; mode:= Emode.dp; len:= 3; cycle:= 2; end;
(* *) with OpCodes[$C5] do begin code:= @op_CMP_dp; inst:= 'CMP'; fmt:= 'CMP'+#9+'$%.2x'; group:= Egroup.arith; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$C6] do begin code:= @op_DEC_dp; inst:= 'DEC'; fmt:= 'DEC'+#9+'$%.2x'; group:= Egroup.arith; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$C8] do begin code:= @op_INY; inst:= 'INY'; fmt:= 'INY'; group:= Egroup.arith; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$C9] do begin code:= @op_CMP_imm; inst:= 'CMP'; fmt:= 'CMP'+#9+'#%.2x'; group:= Egroup.arith; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$CA] do begin code:= @op_DEX; inst:= 'DEX'; fmt:= 'DEX'; group:= Egroup.arith; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$CC] do begin code:= @op_CPY_abs; inst:= 'CPY'; fmt:= 'CPY'+#9+'$%.4x'; group:= Egroup.arith; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$CD] do begin code:= @op_CMP_abs; inst:= 'CMP'; fmt:= 'CMP'+#9+'$%.4x'; group:= Egroup.arith; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$CE] do begin code:= @op_DEC_abs; inst:= 'DEC'; fmt:= 'DEC'+#9+'$%.4x'; group:= Egroup.arith; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$D0] do begin code:= @op_BNE_rel8; inst:= 'BNE'; fmt:= 'BNE'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$D1] do begin code:= @op_CMP_dp_ind_idY; inst:= 'CMP'; fmt:= 'CMP'+#9+'($%.2x),Y'; group:= Egroup.arith; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$D2] do begin code:= @op_CMP_dp_ind; inst:= 'CMP'; fmt:= 'CMP'+#9+'($%.2x)'; group:= Egroup.arith; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$D5] do begin code:= @op_CMP_dp_idX; inst:= 'CMP'; fmt:= 'CMP'+#9+'$%.2x,X'; group:= Egroup.arith; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$D6] do begin code:= @op_DEC_dp_idX; inst:= 'DEC'; fmt:= 'DEC'+#9+'$%.2x,X'; group:= Egroup.arith; mode:= Emode.dp_idx; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$D8] do begin code:= @op_CLD; inst:= 'CLD'; fmt:= 'CLD'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$D9] do begin code:= @op_CMP_abs_idY; inst:= 'CMP'; fmt:= 'CMP'+#9+'$%.4x,Y'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$DA] do begin code:= @op_PHX; inst:= 'PHX'; fmt:= 'PHX'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 3; end;
(* *) with OpCodes[$DD] do begin code:= @op_CMP_abs_idX; inst:= 'CMP'; fmt:= 'CMP'+#9+'$%.4x,X'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$DE] do begin code:= @op_DEC_abs_idX; inst:= 'DEC'; fmt:= 'DEC'+#9+'$%.4x,X'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 7; end;
(* *) with OpCodes[$E0] do begin code:= @op_CPX_imm; inst:= 'CPX'; fmt:= 'CPX'+#9+'#%.2x'; group:= Egroup.arith; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$E1] do begin code:= @op_SBC_dp_idX_ind; inst:= 'SBC'; fmt:= 'SBC'+#9+'($%.2x,X)'; group:= Egroup.arith; mode:= Emode.dp_idx_ind; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$E4] do begin code:= @op_CPX_dp; inst:= 'CPX'; fmt:= 'CPX'+#9+'$%.2x'; group:= Egroup.arith; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$E5] do begin code:= @op_SBC_dp; inst:= 'SBC'; fmt:= 'SBC'+#9+'$%.2x'; group:= Egroup.arith; mode:= Emode.dp; len:= 2; cycle:= 3; end;
(* *) with OpCodes[$E6] do begin code:= @op_INC_dp; inst:= 'INC'; fmt:= 'INC'+#9+'$%.2x'; group:= Egroup.arith; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$E8] do begin code:= @op_INX; inst:= 'INX'; fmt:= 'INX'; group:= Egroup.arith; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$E9] do begin code:= @op_SBC_imm; inst:= 'SBC'; fmt:= 'SBC'+#9+'#%.2x'; group:= Egroup.arith; mode:= Emode.imm; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$EC] do begin code:= @op_CPX_abs; inst:= 'CPX'; fmt:= 'CPX'+#9+'$%.4x'; group:= Egroup.arith; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$ED] do begin code:= @op_SBC_abs; inst:= 'SBC'; fmt:= 'SBC'+#9+'$%.4x'; group:= Egroup.arith; mode:= Emode.abs; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$EE] do begin code:= @op_INC_abs; inst:= 'INC'; fmt:= 'INC'+#9+'$%.4x'; group:= Egroup.arith; mode:= Emode.abs; len:= 3; cycle:= 6; end;
(* *) with OpCodes[$F0] do begin code:= @op_BEQ_rel8; inst:= 'BEQ'; fmt:= 'BEQ'+#9+'%d'; group:= Egroup.branch; mode:= Emode.rel; len:= 2; cycle:= 2; end;
(* *) with OpCodes[$F1] do begin code:= @op_SBC_dp_ind_idY; inst:= 'SBC'; fmt:= 'SBC'+#9+'($%.2x),Y'; group:= Egroup.arith; mode:= Emode.dp_ind_idx; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$F5] do begin code:= @op_SBC_dp_idX; inst:= 'SBC'; fmt:= 'SBC'+#9+'$%.2x,X'; group:= Egroup.arith; mode:= Emode.dp_idx; len:= 2; cycle:= 4; end;
(* *) with OpCodes[$F6] do begin code:= @op_INC_dp_idX; inst:= 'INC'; fmt:= 'INC'+#9+'$%.2x,X'; group:= Egroup.arith; mode:= Emode.dp_idx; len:= 2; cycle:= 6; end;
(* *) with OpCodes[$F8] do begin code:= @op_SED; inst:= 'SED'; fmt:= 'SED'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 2; end;
(* *) with OpCodes[$F9] do begin code:= @op_SBC_abs_idY; inst:= 'SBC'; fmt:= 'SBC'+#9+'$%.4x,Y'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$FA] do begin code:= @op_PLX; inst:= 'PLX'; fmt:= 'PLX'; group:= Egroup.stack; mode:= Emode.imp; len:= 1; cycle:= 4; end;
(* *) with OpCodes[$FC] do begin code:= @op_SBC_dp_ind; inst:= 'SBC'; fmt:= 'SBC'+#9+'($%.2x)'; group:= Egroup.arith; mode:= Emode.dp_ind; len:= 2; cycle:= 5; end;
(* *) with OpCodes[$FD] do begin code:= @op_SBC_abs_idX; inst:= 'SBC'; fmt:= 'SBC'+#9+'$%.4x,X'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 4; end;
(* *) with OpCodes[$FE] do begin code:= @op_INC_abs_idX; inst:= 'INC'; fmt:= 'INC'+#9+'$%.4x,X'; group:= Egroup.arith; mode:= Emode.abs_idx; len:= 3; cycle:= 7; end;
