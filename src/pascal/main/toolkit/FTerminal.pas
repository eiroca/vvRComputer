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
unit FTerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uEmuCPM;

type

  { TfmTerminal }

  TfmTerminal = class(TForm)
    tAutoRefresh: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure tAutoRefreshTimer(Sender: TObject);
  public
    tlk: TToolkitCPM;
    cW, cH: integer;
    curState: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RereshScreen();

  end;

implementation

{$R *.lfm}

{ TfmTerminal }

const
  CUR_CHAR: array [boolean] of char = (' ', '|');

procedure TfmTerminal.FormCreate(Sender: TObject);
begin
  cH := Canvas.TextHeight('m') + 2;
  cW := Canvas.TextWidth('f') + 1;
  Width := cW * SCREEN_WIDTH + BorderWidth;
  Height := ch * SCREEN_HEIGHT + BorderWidth;
  curState := True;
end;

procedure TfmTerminal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
end;

procedure TfmTerminal.tAutoRefreshTimer(Sender: TObject);
begin
  curState := not curState;
  RereshScreen();
end;

constructor TfmTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  tlk := nil;
end;

procedure TfmTerminal.RereshScreen();
var
  x, y: integer;
  c: char;
begin
  if (tlk = nil) then  exit;
  for y := 1 to SCREEN_HEIGHT do begin
    for x := 1 to SCREEN_WIDTH do begin
      c := tlk.screen[y, x];
      Canvas.TextOut((x - 1) * cW, (y - 1) * cH, c);
    end;
    tlk.screenChange := False;
  end;
  Canvas.TextOut((tlk.curX - 1) * cW, (tlk.curY - 1) * cH, CUR_CHAR[curState]);
end;

end.






