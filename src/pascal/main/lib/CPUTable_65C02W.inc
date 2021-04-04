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
(*W*) with OpCodes[$CB] do begin code:= @op_WAI; inst:= 'WAI'; fmt:= 'WAI'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 3; end;
(*W*) with OpCodes[$DB] do begin code:= @op_STP; inst:= 'STP'; fmt:= 'STP'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 3; end;

