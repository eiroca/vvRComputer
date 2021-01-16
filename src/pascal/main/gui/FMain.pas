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
  uCPU, AvgLvlTree,
  uDevice, Types, TypInfo;

type

  { TfmMain }

  TfmMain = class(TForm)
    cbTrace: TCheckBox;
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
    pcComputer: TPageControl;
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
    procedure HandleCPUTrace(var Msg: TLMessage); message WM_CPU_TRACE;
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

function getVal(val: string): iSize16;
var
  r: integer;
begin
  Result := -1;
  try
    val := StringReplace(val, ' ', '', [rfReplaceAll]);
    if (val = '') then val := '0';
    r := Hex2Dec(val);
    if (r >= 0) and (r <= $FFFF) then Result := r;
  except
    on E: EConvertError do ;
  end;
end;

{ TfmMain }
const
  ROMBANK_01 = 'bank_FFFF.bin';

procedure TfmMain.FormCreate(Sender: TObject);
begin
  pcComputer.ActivePageIndex := 0;
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
  addr: uint16;
  adrStart: iSize16;
begin
  adrStart := getVal(iAdrStart.Text);
  if (adrStart <> -1) then begin
    addr := adrStart;
    CPU.cpu.Run(addr, cbTrace.Checked);
  end;
end;

procedure TfmMain.iDisAssClick(Sender: TObject);
var
  node: TAvgLvlTreeNode;
  addr: uint16;
  adrStart, adrEnd: iSize16;
  instList: TInstructionList;
  memMap: TMemoryMap;
  memInfo: PMemoryArea;
  mu: EMemoryUsage;
  i, cnt: integer;
  inst: PInstruction;
  s, cmt: string;
begin
  adrStart := getVal(iAdrStart.Text);
  if (adrStart <> -1) then begin
    adrEnd := getVal(iAdrEnd.Text);
    if (adrEnd = -1) then adrEnd := adrStart + 20;
    instList := TInstructionList.Create;
    memMap := TMemoryMap.Create;
    try
      mCPU.Lines.BeginUpdate;
      addr := adrStart;
      cnt := CPU.cpu.Disassemble(addr, adrEnd, instList, memMap);
      // Code Entries
      mCPU.Lines.Add(';');
      mCPU.Lines.Add('; Code references');
      mCPU.Lines.Add(';');
      for Node in memMap do begin
        memInfo := PMemoryArea(Node.Data);
        with memInfo^ do begin
          if (refCode in usage) then begin
            s := IntToHex(addrStart, 4);
            s := 'C' + s + TAB + '.equ' + TAB + '$' + s;
            mCPU.Lines.Add(s);
          end;
        end;
      end;
      // Data Entries
      mCPU.Lines.Add(';');
      mCPU.Lines.Add('; Data references');
      mCPU.Lines.Add(';');
      for Node in memMap do begin
        memInfo := PMemoryArea(Node.Data);
        with memInfo^ do begin
          if (refData in usage) then begin
            s := IntToHex(addrStart, 4);
            s := 'D' + s + TAB + '.equ' + TAB + '$' + s + TAB + TAB + '; ' + IntToHex(CPU.cpu.ReadMem(addrStart), 2);
            mCPU.Lines.Add(s);
          end;
        end;
      end;
      mCPU.Lines.Add(';');
      mCPU.Lines.Add('; Code');
      mCPU.Lines.Add(';');
      if (cnt > 0) then begin
        for i := 0 to cnt - 1 do begin
          inst := instList[i];
          with inst^ do begin
            memInfo := memMap.FindArea(addr);
            cmt := '; ' + IntToHex(opcode, 2);
            if (def^.len = 1) then begin
              if (Pos(TAB, def^.fmt) <> 0) then begin
                cmt := cmt;
              end
              else begin
                cmt := TAB + cmt;
              end;
            end
            else if (def^.len = 2) then begin
              cmt := cmt + ' ' + IntToHeX(param1, 2);
            end
            else if (def^.len = 3) then begin
              s := IntToHeX(param1, 4);
              cmt := cmt + ' ' + Copy(s, 3, 2) + ' ' + Copy(s, 1, 2);
            end;
            if (memInfo = nil) then begin
              s := 'L' + IntToHex(addr, 4) + ':';
            end
            else begin
              if (refCode in memInfo^.usage) then begin
                s := 'L' + IntToHex(addr, 4);
              end
              else begin
                s := '';
              end;
            end;
            s := s + TAB + Format(def^.fmt, [param1]) + TAB + TAB + cmt;
            mCPU.Lines.Add(s);
          end;
        end;
      end;
      // Memory Map
      memMap.Pack;
      mCPU.Lines.Add('');
      mCPU.Lines.Add('Memory Map:');
      for Node in memMap do begin
        memInfo := PMemoryArea(Node.Data);
        with memInfo^ do begin
          s := IntToHex(addrStart, 4);
          if (len > 1) then begin
            s := s + '-' + IntToHex(addrStart + len - 1, 4);
          end;
          for mu in (usage * [mData, mCode]) do begin
            s := s + ' ' + GetEnumName(TypeInfo(EMemoryUsage), Ord(mu));
          end;
          mCPU.Lines.Add(s);
        end;
      end;
    finally
      mCPU.Lines.EndUpdate;
      FreeAndNil(instList);
      FreeAndNil(memMap);
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
  hh: integer;
begin
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
              if (devType <= high(DeviceTypeName)) then begin
                drawLeft(dgDevices.Canvas, aRect, DeviceTypeName[devType]);
              end
              else begin
                drawLeft(dgDevices.Canvas, aRect, IntToHex(devType, 2));
              end;
            end;
            2: begin
              if ((devType = DU_MAIN) and (devSubType <= high(DPUSubTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, DPUSubTypeName[devSubType]);
              end
              else if ((devType = DU_CPU) and (devSubType <= high(CPUSubTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, CPUSubTypeName[devSubType]);
              end
              else if ((devType = DU_VDU) and (devSubType <= high(VDUSubTypeName))) then begin
                drawLeft(dgDevices.Canvas, aRect, VDUSubTypeName[devSubType]);
              end
              else if ((devType = DU_SDU) and (devSubType <= high(SDUSubTypeName))) then begin
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

procedure TfmMain.HandleCPUTrace(var Msg: TLMessage);
var
  MsgStr: PChar;
  MsgPasStr: string;
begin
  MsgStr := PChar(Msg.wparam);
  MsgPasStr := StrPas(MsgStr);
  mCPU.Lines.Add(MsgPasStr);
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
    size := SizeOf(BlockData) div 32;
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



