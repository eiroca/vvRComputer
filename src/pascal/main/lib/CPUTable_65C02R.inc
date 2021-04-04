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
(*R*) with OpCodes[$07] do begin code:= @op_RMB0_dp; inst:= 'RMB0'; fmt:= 'RMB0'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$0F] do begin code:= @op_BBR0_dp_rel8; inst:= 'BBR0'; fmt:= 'BBR0'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$17] do begin code:= @op_RMB1_dp; inst:= 'RMB1'; fmt:= 'RMB1'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$1F] do begin code:= @op_BBR1_dp_rel8; inst:= 'BBR1'; fmt:= 'BBR1'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$27] do begin code:= @op_RMB2_dp; inst:= 'RMB2'; fmt:= 'RMB2'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$2F] do begin code:= @op_BBR2_dp_rel8; inst:= 'BBR2'; fmt:= 'BBR2'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$37] do begin code:= @op_RMB3_dp; inst:= 'RMB3'; fmt:= 'RMB3'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$3F] do begin code:= @op_BBR3_dp_rel8; inst:= 'BBR3'; fmt:= 'BBR3'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$47] do begin code:= @op_RMB4_dp; inst:= 'RMB4'; fmt:= 'RMB4'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$4F] do begin code:= @op_BBR4_dp_rel8; inst:= 'BBR4'; fmt:= 'BBR4'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$57] do begin code:= @op_RMB5_dp; inst:= 'RMB5'; fmt:= 'RMB5'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$5F] do begin code:= @op_BBR5_dp_rel8; inst:= 'BBR5'; fmt:= 'BBR5'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$67] do begin code:= @op_RMB6_dp; inst:= 'RMB6'; fmt:= 'RMB6'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$6F] do begin code:= @op_BBR6_dp_rel8; inst:= 'BBR6'; fmt:= 'BBR6'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$77] do begin code:= @op_RMB7_dp; inst:= 'RMB7'; fmt:= 'RMB7'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$7F] do begin code:= @op_BBR7_dp_rel8; inst:= 'BBR7'; fmt:= 'BBR7'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$87] do begin code:= @op_SMB0_dp; inst:= 'SMB0'; fmt:= 'SMB0'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$8F] do begin code:= @op_BBS0_dp_rel8; inst:= 'BBS0'; fmt:= 'BBS0'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$97] do begin code:= @op_SMB1_dp; inst:= 'SMB1'; fmt:= 'SMB1'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$9F] do begin code:= @op_BBS1_dp_rel8; inst:= 'BBS1'; fmt:= 'BBS1'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$A7] do begin code:= @op_SMB2_dp; inst:= 'SMB2'; fmt:= 'SMB2'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$AF] do begin code:= @op_BBS2_dp_rel8; inst:= 'BBS2'; fmt:= 'BBS2'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$B7] do begin code:= @op_SMB3_dp; inst:= 'SMB3'; fmt:= 'SMB3'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$BF] do begin code:= @op_BBS3_dp_rel8; inst:= 'BBS3'; fmt:= 'BBS3'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$C7] do begin code:= @op_SMB4_dp; inst:= 'SMB4'; fmt:= 'SMB4'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$CF] do begin code:= @op_BBS4_dp_rel8; inst:= 'BBS4'; fmt:= 'BBS4'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$D7] do begin code:= @op_SMB5_dp; inst:= 'SMB5'; fmt:= 'SMB5'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$DF] do begin code:= @op_BBS5_dp_rel8; inst:= 'BBS5'; fmt:= 'BBS5'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$E7] do begin code:= @op_SMB6_dp; inst:= 'SMB6'; fmt:= 'SMB6'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$EF] do begin code:= @op_BBS6_dp_rel8; inst:= 'BBS6'; fmt:= 'BBS6'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
(*R*) with OpCodes[$F7] do begin code:= @op_SMB7_dp; inst:= 'SMB7'; fmt:= 'SMB7'+#9+'$%.2x'; group:= Egroup.logic; mode:= Emode.dp; len:= 2; cycle:= 5; end;
(*R*) with OpCodes[$FF] do begin code:= @op_BBS7_dp_rel8; inst:= 'BBS7'; fmt:= 'BBS7'+#9+'$%.2x,%d'; group:= Egroup.branch; mode:= Emode.dp_rel; len:= 3; cycle:= 2; end;
