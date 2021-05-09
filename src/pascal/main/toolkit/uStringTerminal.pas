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
unit uStringTerminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, uEmuCPM;

type

  { FStringTerminal }

  FStringTerminal = class(TForm)
    mOut: TMemo;
    tAutoRefresh: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure tAutoRefreshTimer(Sender: TObject);
  public
    term: TStringTerminal;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RereshScreen();
  end;

implementation

{$R *.lfm}

{ FStringTerminal }

procedure FStringTerminal.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
end;

procedure FStringTerminal.tAutoRefreshTimer(Sender: TObject);
begin
  RereshScreen();
end;

constructor FStringTerminal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  mOut.Clear;
  term := TStringTerminal.Create(mOut.Lines);
end;

procedure FStringTerminal.RereshScreen();
begin
  if (term.screenChange) then begin
    term.screenChange := False;
    mOut.Clear();
    mOut.Lines.AddStrings(term.screen);
  end;
end;

end.
