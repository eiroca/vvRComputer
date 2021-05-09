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

type

  { TToolkitCPM }
  BDOSMessage = record
    ID: cardinal;
    DE: word;
    done: boolean;
  end;

  ITerminal = interface
    procedure ClearScreen();
    procedure ScrollUp();
    procedure WriteChar(E: iSize8);
  end;

  { TScreenTerminal }

  TScreenTerminal = class(TInterfacedObject, ITerminal)
  public
    ScreenWidth: integer;
    ScreenHeight: integer;
    crt: TStream;
    screenChange: boolean;
    screen: array of array of char;
  public
    tabSize: iSize8;
    curX, curY: iSize8;
  public
    constructor Create(const sizeX, sizeY: integer);
    procedure ClearScreen();
    procedure ScrollUp();
    procedure WriteChar(E: iSize8);
    destructor Destroy; override;
  end;

  { TStringTerminal }

  TStringTerminal = class(TInterfacedObject, ITerminal)
  public
    screen: TStrings;
    screenChange: boolean;
  public
    tabSize: iSize8;
    curX, curY: iSize8;
    ScreenWidth: integer;
    ScreenHeight: integer;
  public
    constructor Create(const sOut: TStrings);
    procedure ClearScreen();
    procedure ScrollUp();
    procedure WriteChar(E: iSize8);
    destructor Destroy; override;
  end;

  TToolkitCPM = class
  private
    charDelim: iSize8;
    cpu: TCPU_8080;
    oCallOpcode: OpcodeCall;
    term: ITerminal;
    procedure CallInterceptor();
  public //BDOS calls
    procedure C_WRITE(var message: BDOSMessage); message $02;
    procedure C_WRITESTR(var message: BDOSMessage); message $09;
  public
    constructor Create(aCPU: TCPU_8080; aTerm: ITerminal);
    destructor Destroy; override;
  end;

implementation

const
  BDOS_ADDR = $0005;

{ TStringTerminal }

constructor TStringTerminal.Create(const sOut: TStrings);
begin
  ScreenWidth := 0;
  ScreenHeight := 0;
  screen := TStringList.Create();
  ClearScreen();
end;

procedure TStringTerminal.ClearScreen();
begin
  screen.Clear();
  curX := 1;
  curY := 1;
  screenChange := True;
end;

procedure TStringTerminal.ScrollUp();
begin
  if (screen.Count > 0) then begin
    screen.Delete(0);
  end;
  screenChange := True;
end;

procedure TStringTerminal.WriteChar(E: iSize8);
var
  s: string;
begin
  case E of
    9: begin
      curX := ((curX + tabSize) and $F8);
    end;
    10: begin
      Inc(curY);
    end;
    12: begin
      ClearScreen();
    end;
    13: begin
      curX := 1;
    end;
    32..127: begin
      while (screen.Count < curY) do begin
        screen.Add('');
      end;
      s := screen[curY - 1];
      while (Length(s) < curX) do begin
        s := s + ' ';
      end;
      s[curX] := chr(E);
      screen[curY - 1] := s;
      Inc(curX);
    end;
  end;
  if (ScreenWidth > 0) and (curX > ScreenWidth) then begin
    curX := 1;
    Inc(curY);
  end;
  if (ScreenHeight > 0) then begin
    while (curY > ScreenHeight) do begin
      ScrollUp();
    end;
  end;
  screenChange := True;
end;

destructor TStringTerminal.Destroy;
begin
  inherited Destroy;
end;

{ TScreenTerminal }

constructor TScreenTerminal.Create(const sizeX, sizeY: integer);
begin
  crt := TFileStream.Create('crt.out', fmCreate);
  ScreenHeight := SizeY;
  ScreenWidth := SizeX;
  SetLength(screen, ScreenHeight, ScreenWidth);
  tabSize := 8;
  ClearScreen();
end;

procedure TScreenTerminal.ClearScreen();
var
  y, x: iSize8;
begin
  for y := 0 to ScreenHeight - 1 do begin
    for x := 0 to ScreenWidth - 1 do begin
      screen[y, x] := ' ';
    end;
  end;
  curX := 1;
  curY := 1;
  screenChange := True;
end;

procedure TScreenTerminal.ScrollUp();
var
  y, x: iSize8;
begin
  for y := 0 to ScreenHeight - 2 do begin
    for x := 0 to ScreenWidth - 1 do begin
      screen[y, x] := screen[y + 1, x];
    end;
  end;
  for x := 0 to ScreenWidth - 1 do begin
    screen[ScreenHeight - 1, x] := ' ';
  end;
  Dec(curY);
  if (curY < 1) then begin
    curY := 1;
  end;
  screenChange := True;
end;

procedure TScreenTerminal.WriteChar(E: iSize8);
begin
  crt.WriteByte(E);
  case E of
    9: begin
      curX := ((curX + tabSize) and $F8);
    end;
    10: begin
      Inc(curY);
    end;
    12: begin
      ClearScreen();
    end;
    13: begin
      curX := 1;
    end;
    32..127: begin
      screen[curY - 1, curX - 1] := chr(E);
      Inc(curX);
    end;
  end;
  if (curX > ScreenWidth) then begin
    curX := 1;
    Inc(curY);
  end;
  while (curY > ScreenHeight) do begin
    ScrollUp();
  end;
  screenChange := True;
end;

destructor TScreenTerminal.Destroy;
begin
  inherited Destroy;
  FreeAndNil(crt);
end;

{ TToolkitCPM }

constructor TToolkitCPM.Create(aCPU: TCPU_8080; aTerm: ITerminal);
begin
  cpu := aCPU;
  oCallOpcode := aCPU.ReplaceOpCode($CD, @CallInterceptor);
  term := aTerm;
  charDelim := Ord('$');
end;

destructor TToolkitCPM.Destroy;
begin
  inherited Destroy;
end;

procedure TToolkitCPM.CallInterceptor();
var
  msg: BDOSMessage;
begin
  oCallOpcode();
  case cpu.rPC of
    BDOS_ADDR: begin
      msg.ID := cpu.rC;
      msg.DE := cpu.rDE;
      msg.done := False;
      Dispatch(msg);
    end
  end;
end;

procedure TToolkitCPM.C_WRITE(var message: BDOSMessage);
// Print character in E
var
  E: iSize8;
begin
  E := message.DE and $00FF;
  term.WriteChar(E);
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
    if c = charDelim then begin
      break;
    end;
    term.WriteChar(c);
    Inc(cnt);
  until (cnt > $FFFF);
  cpu.rPC := cpu.StackPull();
end;

end.
