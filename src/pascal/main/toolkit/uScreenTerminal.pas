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
unit uScreenTerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uEmuCPM;

type

  { FScreenTerminal }

  FScreenTerminal = class(TForm)
    tAutoRefresh: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure tAutoRefreshTimer(Sender: TObject);
  public
    term: TScreenTerminal;
    cW, cH: integer;
    curState: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RereshScreen();
  end;

implementation

{$R *.lfm}

{ FScreenTerminal }

const
  CUR_CHAR: array [boolean] of char = (' ', '|');

procedure FScreenTerminal.FormCreate(Sender: TObject);
begin
  cH := Canvas.TextHeight('m') + 2;
  cW := Canvas.TextWidth('f') + 1;
  if (term <> nil) then begin
    Width := cW * term.ScreenWidth + BorderWidth;
    Height := ch * term.ScreenHeight + BorderWidth;
  end
  else begin
    Width := cW * 80 + BorderWidth;
    Height := ch * 25 + BorderWidth;
  end;
  curState := True;
end;

procedure FScreenTerminal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
end;

procedure FScreenTerminal.tAutoRefreshTimer(Sender: TObject);
begin
  curState := not curState;
  RereshScreen();
end;

constructor FScreenTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  term := TScreenTerminal.Create(80, 25);
end;

procedure FScreenTerminal.RereshScreen();
var
  x, y: integer;
  c: char;
begin
  if (term = nil) then begin
    exit;
  end;
  for y := 0 to term.ScreenHeight - 1 do begin
    for x := 0 to term.ScreenWidth - 1 do begin
      c := term.screen[y, x];
      Canvas.TextOut(x * cW, y * cH, c);
    end;
    term.screenChange := False;
  end;
  Canvas.TextOut((term.curX - 1) * cW, (term.curY - 1) * cH, CUR_CHAR[curState]);
end;

end.
