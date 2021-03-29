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
unit uMisc;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils;

function getValHex(val: string): integer;
function getVal(val: string): integer;

implementation

uses
  StrUtils;

function getValHex(val: string): integer;
var
  r: integer;
begin
  Result := -1;
  try
    val := StringReplace(val, ' ', '', [rfReplaceAll]);
    if (val = '') then begin
      val := '0';
    end;
    r := Hex2Dec(val);
    if (r >= 0) and (r <= $FFFF) then begin
      Result := r;
    end;
  except
    on E: EConvertError do ;
  end;
end;

function getVal(val: string): integer;
begin
  Result := -1;
  try
    val := StringReplace(val, ' ', '', [rfReplaceAll]);
    if (val = '') then begin
      val := '0';
    end;
    Result := StrToInt(val);
  except
    on E: EConvertError do ;
  end;
end;

end.
