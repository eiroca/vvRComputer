(*W*) with OpCodes[$CB] do begin code:= @op_WAI; inst:= 'WAI'; fmt:= 'WAI'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 3; end;
(*W*) with OpCodes[$DB] do begin code:= @op_STP; inst:= 'STP'; fmt:= 'STP'; group:= Egroup.control; mode:= Emode.imp; len:= 1; cycle:= 3; end;

