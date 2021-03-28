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






