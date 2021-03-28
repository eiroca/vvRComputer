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
unit uEmuCPM;

{$mode objfpc}{$H+}

interface

uses
  uCPU, uCPU_808x,
  Classes, SysUtils;

const
  SCREEN_WIDTH = 80;
  SCREEN_HEIGHT = 25;

type


  { TToolkitCPM }
  BDOSMessage = record
    ID: cardinal;
    DE: word;
    done: boolean;
  end;

  TToolkitCPM = class
  private
    cpu: TCPU_8080;
    oCallOpcode: OpcodeCall;
    crt: TStream;
    procedure CallInterceptor();
  public
    screenChange: boolean;
    screen: array[1..SCREEN_HEIGHT, 1..SCREEN_WIDTH] of char;
  public
    charDelim: iSize8;
    tabSize: iSize8;
    curX, curY: iSize8;
    procedure InitTerminal();
    procedure ClearScreen();
    procedure ScrollUp();
    procedure WriteChar(E: iSize8);
  public //BDOS calls
    procedure C_WRITE(var message: BDOSMessage); message $02;
    procedure C_WRITESTR(var message: BDOSMessage); message $09;
  public
    constructor Create(aCPU: TCPU_8080);
    destructor Destroy; override;
  end;

implementation

const
  BDOS_ADDR = $0005;

{ TToolkitCPM }


constructor TToolkitCPM.Create(aCPU: TCPU_8080);
begin
  cpu := aCPU;
  oCallOpcode := aCPU.ReplaceOpCode($CD, @CallInterceptor);
  crt := TFileStream.Create('crt.out', fmCreate);
  InitTerminal();
end;

destructor TToolkitCPM.Destroy;
begin
  inherited Destroy;
  FreeAndNil(crt);
end;

procedure TToolkitCPM.CallInterceptor();
var
  msg: BDOSMessage;
begin
  oCallOpcode();
  case cpu.rPC of
    BDOS_ADDR:
    begin
      msg.ID := cpu.rC;
      msg.DE := cpu.rDE;
      msg.done := False;
      Dispatch(msg);
    end
  end;
end;

procedure TToolkitCPM.InitTerminal();
begin
  charDelim := Ord('$');
  tabSize := 8;
  ClearScreen();
end;

procedure TToolkitCPM.ClearScreen();
begin
  FillChar(screen, sizeOf(screen), ' ');
  curX := 1;
  curY := 1;
  screenChange := True;
end;

procedure TToolkitCPM.ScrollUp();
var
  y, x: iSize8;
begin
  for y := 1 to SCREEN_HEIGHT - 1 do
  begin
    for x := 1 to SCREEN_WIDTH do
    begin
      screen[y, x] := screen[y + 1, x];
    end;
  end;
  for x := 1 to SCREEN_WIDTH do
  begin
    screen[SCREEN_HEIGHT, x] := ' ';
  end;
  Dec(curY);
  if (curY < 1) then
    curY := 1;
  screenChange := True;
end;

procedure TToolkitCPM.WriteChar(E: iSize8);
begin
  crt.WriteByte(E);
  case E of
    9:
    begin
      curX := ((curX + tabSize) and $F8);
    end;
    10:
    begin
      Inc(curY);
    end;
    12:
    begin
      ClearScreen();
    end;
    13:
    begin
      curX := 1;
    end;
    32..127:
    begin
      screen[curY, curX] := chr(E);
      Inc(curX);
    end;
  end;
  if (curX > SCREEN_WIDTH) then
  begin
    curX := 1;
    Inc(curY);
  end;
  while (curY > SCREEN_HEIGHT) do
    ScrollUp();
  screenChange := True;
end;

procedure TToolkitCPM.C_WRITE(var message: BDOSMessage);
// Print character in E
var
  E: iSize8;
begin
  E := message.DE and $00FF;
  WriteChar(E);
  cpu.rPC := cpu.StackPull();
end;

procedure TToolkitCPM.C_WRITESTR(var message: BDOSMessage);
// Print string pointed by DE until $
var
  c: iSize8;
  addr: iSize16;
  cnt: integer;
begin
  addr := message.DE;
  cnt := 0;
  repeat
    c := cpu.ReadMem(addr);
    addr := (addr + 1) and $FFFF;
    if c = charDelim then
      break;
    WriteChar(c);
    Inc(cnt);
  until (cnt > $FFFF);
  cpu.rPC := cpu.StackPull();
end;

end.
