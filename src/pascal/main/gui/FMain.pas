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
unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, strutils,
  LMessages, LCLIntf, LCLType, LCLProc, ExtCtrls, ComCtrls, Grids, MaskEdit,
  uDevice, uCPU_8080, uCPU_8085, Types, TypInfo;

type

  { TfmMain }

  TfmMain = class(TForm)
    iDisAss: TButton;
    dgDevices: TDrawGrid;
    iAdrStart: TMaskEdit;
    iAdrEnd: TMaskEdit;
    iDump: TButton;
    iExecute: TButton;
    Label1: TLabel;
    iBlockID: TMaskEdit;
    Label2: TLabel;
    mMemory: TMemo;
    mCPU: TMemo;
    mOutput: TMemo;
    Panel2: TPanel;
    pLog: TPageControl;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure iExecuteClick(Sender: TObject);
    procedure iDisAssClick(Sender: TObject);
    procedure iDumpClick(Sender: TObject);
    procedure dgDevicesDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    DCU: TDeviceUnit;
    CPU: T8080ProcessorUnit;
    VPU: TVideoUnit;
    SPU: TSoundUnit;
    procedure HandleDebug(var Msg: TLMessage); message WM_DEVICE_MESSAGE;
    procedure drawLeft(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
    procedure drawCentered(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
    procedure drawRight(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
  private
    procedure logStaus(Status: string);
    procedure DumpMem(addr: word; size: word);
    procedure DumpBlock(block: BlockNum);
  public

  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

function getVal(val: string): integer;
begin
  try
    val := StringReplace(val, ' ', '', [rfReplaceAll]);
    if (val = '') then val := '0';
    Result := Hex2Dec(val);
  except
    on E: EConvertError do Result := -1;
  end;
end;

{ TfmMain }
const
  ROMBANK_01 = 'bank_FFFF.bin';

procedure TfmMain.FormCreate(Sender: TObject);
begin
  mOutput.Clear;
  DCU := TDeviceUnit.Create;
  DCU.MessageHandler := fmMain.Handle;
  if not DCU.LoadBank(ROM_END, 'ROM\' + ROMBANK_01) then begin
    mOutput.Lines.Add('ROM (' + ROMBANK_01 + ') MISSING!');
  end;
  CPU := T8080ProcessorUnit.Create(DCU);
  CPU.MessageHandler := fmMain.Handle;
  VPU := TVideoUnit.Create(DCU);
  VPU.MessageHandler := fmMain.Handle;
  SPU := TSoundUnit.Create(DCU);
  SPU.MessageHandler := fmMain.Handle;
  SPU.Start;
  VPU.Start;
  CPU.Start;
  DCU.Start;
end;

procedure TfmMain.iExecuteClick(Sender: TObject);
var
  adrStart: integer;
begin
  adrStart := getVal(iAdrStart.Text);
  CPU.cpu.Run(adrStart, 1000);
end;

procedure TfmMain.iDisAssClick(Sender: TObject);
var
  addr: uint16;
  adrStart, adrEnd: integer;
  instList, codePoint, dataPoint: TStringList;
  cnt: integer;
begin
  adrStart := getVal(iAdrStart.Text);
  adrEnd := getVal(iAdrEnd.Text);
  if (adrStart <> -1) then begin
    if (adrEnd = -1) then adrEnd := adrStart + 20;
    instList := TStringList.Create;
    codePoint := TStringList.Create;
    dataPoint := TStringList.Create;
    instList.Duplicates := dupIgnore;
    codePoint.Duplicates := dupIgnore;
    dataPoint.Duplicates := dupIgnore;
    try
      addr := adrStart;
      cnt := CPU.cpu.DisAss(addr, adrEnd, instList, codePoint, dataPoint);
      if (cnt > 0) then begin
        mCPU.Lines.AddStrings(instList, True);
        if (codePoint.Count > 0) then begin
          codePoint.Sort;
          mCPU.Lines.Add('');
          mCPU.Lines.Add('Entry Points:');
          mCPU.Lines.AddStrings(codePoint);
        end;
        if (dataPoint.Count > 0) then begin
          dataPoint.Sort;
          mCPU.Lines.Add('');
          mCPU.Lines.Add('Data points:');
          mCPU.Lines.AddStrings(dataPoint);
        end;
      end;
    finally
      FreeAndNil(instList);
      FreeAndNil(codePoint);
      FreeAndNil(dataPoint);
    end;
  end;
end;


procedure TfmMain.iDumpClick(Sender: TObject);
var
  block: integer;
begin
  block := getVal(iBlockID.Text);
  if (block <> -1) then  DumpBlock(block);
end;

procedure TfmMain.drawCentered(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
var
  ww, hh: integer;
begin
  ww := Canvas.TextWidth(msg);
  hh := Canvas.TextHeight(msg);
  aCanvas.TextOut(aRect.Left + (aRect.Width - ww) div 2, aRect.Top + (aRect.Height - hh) div 2, msg);
end;

procedure TfmMain.drawRight(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
var
  ww, hh: integer;
begin
  ww := Canvas.TextWidth(msg);
  hh := Canvas.TextHeight(msg);
  aCanvas.TextOut(aRect.Left - 3 + (aRect.Width - ww), aRect.Top + (aRect.Height - hh) div 2, msg);
end;

procedure TfmMain.drawLeft(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
var
  ww, hh: integer;
begin
  ww := Canvas.TextWidth(msg);
  hh := Canvas.TextHeight(msg);
  aCanvas.TextOut(aRect.Left + 3, aRect.Top + (aRect.Height - hh) div 2, msg);
end;


procedure TfmMain.dgDevicesDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
var
  d: TDevice;
  devType: DeviceIDType;
  devSubType: DeviceSubType;
begin
  if (aRow <> 0) then begin
    aRow := ARow - 1;
    if ((aRow >= 0) and (aRow <= 15)) then begin
      if (aCol = 0) then begin
        drawCentered(dgDevices.Canvas, aRect, IntToHex(aRow, 2));
      end
      else begin
        d := DCU.Device[aRow];
        if (d <> nil) then begin
          devType := d.Conf.devID;
          devSubType := d.Conf.devSubType;
          case (aCol) of
            1: begin
              if ((devType >= low(DeviceTypeName)) and (devType <= high(DeviceTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, DeviceTypeName[devType]);
              end
              else begin
                drawLeft(dgDevices.Canvas, aRect, IntToHex(devType, 2));
              end;
            end;
            2: begin
              if ((devType = DU_MAIN) and (devSubType >= low(DPUSubTypeName)) and (devSubType <= high(DPUSubTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, DPUSubTypeName[devSubType]);
              end
              else if ((devType = DU_CPU) and (devSubType >= low(CPUSubTypeName)) and (devSubType <= high(CPUSubTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, CPUSubTypeName[devSubType]);
              end
              else if ((devType = DU_VDU) and (devSubType >= low(VDUSubTypeName)) and (devSubType <= high(VDUSubTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, VDUSubTypeName[devSubType]);
              end
              else if ((devType = DU_SDU) and (devSubType >= low(SDUSubTypeName)) and (devSubType <= high(SDUSubTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, SDUSubTypeName[devSubType]);
              end
              else begin
                drawLeft(dgDevices.Canvas, aRect, IntToHex(devSubType, 2));
              end;
            end;
            3: drawRight(dgDevices.Canvas, aRect, IntToHex(d.Conf.ramProv, 4));
            4: drawRight(dgDevices.Canvas, aRect, IntToHex(d.Conf.romProv, 4));
            5: drawRight(dgDevices.Canvas, aRect, IntToHex(d.Conf.ramReq, 4));
            6: drawLeft(dgDevices.Canvas, aRect, GetEnumName(TypeInfo(d.Status), Ord(d.Status)));
          end;
        end;
      end;
    end;
  end;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  DCU.Terminate;
end;


procedure TfmMain.HandleDebug(var Msg: TLMessage);
var
  MsgStr: PChar;
  MsgPasStr: string;
begin
  MsgStr := PChar(Msg.wparam);
  MsgPasStr := StrPas(MsgStr);
  mOutput.Lines.Add(MsgPasStr);
  StrDispose(MsgStr);
end;

procedure TfmMain.logStaus(Status: string);
begin
  mOutput.Lines.Add(Status);
end;

procedure TfmMain.DumpMem(addr: word; size: word);
var
  B: TBLock;
  r, i: integer;
  bank, base: integer;
  sb: string;
begin
  size := size + addr and $000F - 1;
  addr := addr and $FFF0;
  size := (size div 16) + 1;
  for r := 1 to size do begin
    bank := (addr and $F000) shr 12;
    base := (addr and $0FF0);
    B := DCU.getBlock(bank, False);
    sb := IntToHex(addr, 4) + ':';
    for i := 0 to 15 do begin
      sb := sb + ' ' + IntToHex(b.Data[base], 2);
      Inc(base);
      Inc(addr);
    end;
    mMemory.Lines.Add(sb);
  end;
end;

procedure TfmMain.DumpBlock(block: BlockNum);
var
  B: TBLock;
  size: integer;
  base, addr: integer;
  r, i: integer;
  sb: string;
begin
  B := DCU.getBlock(block, False);
  if (B <> nil) then begin
    size := SizeOf(BlockDataType) div 32;
    addr := block shl 12;
    base := 0;
    for r := 1 to size do begin
      sb := IntToHex(addr, 8) + ':';
      for i := 0 to 31 do begin
        sb := sb + ' ' + IntToHex(b.Data[base], 2);
        Inc(addr);
        Inc(Base);
      end;
      mMemory.Lines.Add(sb);
    end;
  end;
end;

end.

