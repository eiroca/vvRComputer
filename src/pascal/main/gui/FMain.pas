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

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LMessages, LCLIntf, LCLType, LCLProc, ExtCtrls, ComCtrls, Grids, MaskEdit,
  FileUtil, DateUtils,
  AvgLvlTree, uStringTerminal, uScreenTerminal,
  uCPU, uCPU_808x,
  uDevice,
  uEmuCPM,
  Types, TypInfo;

type

  { TfmMain }

  TfmMain = class(TForm)
    bCPU_Resume: TButton;
    bCPU_HardReset: TButton;
    bCPU_Status: TButton;
    bCPU_Halt: TButton;
    bCPU_SoftReset: TButton;
    cbLog: TCheckBox;
    iRun: TButton;
    cbTrace: TCheckBox;
    dgDevices: TDrawGrid;
    iProgName: TEdit;
    iAdrEnd: TMaskEdit;
    iAdrStart: TMaskEdit;
    iBlockID: TMaskEdit;
    iDisAss: TButton;
    iDump: TButton;
    iExecute: TButton;
    iSteps: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mOutput: TMemo;
    mTools: TMemo;
    mCPU: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    pcComputer: TPageControl;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure bCPU_HardResetClick(Sender: TObject);
    procedure bCPU_ResumeClick(Sender: TObject);
    procedure bCPU_HaltClick(Sender: TObject);
    procedure bCPU_SoftResetClick(Sender: TObject);
    procedure bCPU_StatusClick(Sender: TObject);
    procedure iRunClick(Sender: TObject);
    procedure iExecuteClick(Sender: TObject);
    procedure iDisAssClick(Sender: TObject);
    procedure iDumpClick(Sender: TObject);
    procedure dgDevicesDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pcComputerEnter(Sender: TObject);
  private
    DCU: TDeviceUnit;
    CPU: T8080ProcessorUnit;
    VPU: TVideoUnit;
    SPU: TSoundUnit;
    tlkCPM: TToolkitCPM;
    //fmConsole: FScreenTerminal;
    fmConsole: FStringTerminal;
    procedure InitEmulator();
    function LoadROMs(const path: string): integer;
    procedure HandleDebug(var Msg: TLMessage); message WM_DEVICE_MESSAGE;
    procedure HandleCPUTrace(var Msg: TLMessage); message WM_CPU_TRACE;
    procedure drawLeft(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
    procedure drawCentered(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
    procedure drawRight(const aCanvas: TCanvas; const aRect: TRect; const msg: string);
    procedure UpdateCPU_GUI();
    function getUsage(usage: AMemoryUsage): string;
  private
    procedure logStaus(Status: string);
    procedure DumpMem(addr: word; size: word);
    procedure DumpBlock(block: BlockRange);
  public

  end;

var
  fmMain: TfmMain;

implementation

uses
  uMisc;

{$R *.lfm}

const
  MemoryUsageName: array [EMemoryUsage] of string = ('Code', 'Data', 'reference', 'reference');

{ TfmMain }

procedure TfmMain.InitEmulator();
begin
  //fmConsole := FScreenTerminal.Create(Self);
  fmConsole := FStringTerminal.Create(Self);
  fmConsole.Show;
  DCU := TDeviceUnit.Create;
  DCU.MessageHandler := fmMain.Handle;
  if LoadROMs('ROM\') = 0 then begin
    mOutput.Lines.Add('ROMs MISSING!');
  end;
  CPU := T8080ProcessorUnit.Create(DCU);
  CPU.MessageHandler := fmMain.Handle;
  tlkCPM := TToolkitCPM.Create(CPU.cpu, fmConsole.term);
  VPU := TVideoUnit.Create(DCU);
  VPU.MessageHandler := fmMain.Handle;
  SPU := TSoundUnit.Create(DCU);
  SPU.MessageHandler := fmMain.Handle;
  SPU.Start;
  VPU.Start;
  CPU.Start;
  DCU.Start;
end;

function TfmMain.LoadROMs(const path: string): integer;
var
  roms: TStrings;
  romName, bank: string;
  bankId: BlockRange;
  i: integer;
begin
  Result := 0;
  roms := FindAllFiles(path, 'bank_????.bin', False);
  for i := 0 to roms.Count - 1 do begin
    romName := roms[i];
    bank := copy(romName, Length(path) + 6, 4);
    bankId := getValHex(bank);
    if DCU.LoadBanks(bankId, romName) then begin
      Inc(Result);
    end;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  pcComputer.ActivePageIndex := 0;
  mOutput.Clear;
  InitEmulator();
end;

procedure TfmMain.iExecuteClick(Sender: TObject);
var
  addr: iSize16;
  adrStart: iSize16;
begin
  adrStart := getValHex(iAdrStart.Text);
  if (adrStart <> -1) then begin
    addr := adrStart;
    CPU.Trace(cbTrace.Checked);
    CPU.cpu.Run(addr, getVal(iSteps.Text));
  end;
end;

procedure TfmMain.bCPU_StatusClick(Sender: TObject);
var
  i: integer;
  s: string;
  status: RCPUStatus;
  info: RCPUInfo;
begin
  mCPU.Lines.Clear;
  status := CPU.cpu.Status;
  info := CPU.cpu.Info;
  mCPU.Lines.Add(CreateTrace(info, status, @CPU.ReadMemoryCall));
  with info, status do begin
    mCPU.Lines.Add('Interrupts are ' + BoolToStr(IntEnabled, 'enabled', 'disabled'));
    for i := 0 to numIRQs - 1 do begin
      s := IRQsName[i] + ' is ' + BoolToStr(IRQs[i].active, 'on', 'off');
      if not IRQsNMI[i] then begin
        s := s + ' and ' + BoolToStr(IRQs[i].masked, '', 'not ') + 'masked';
      end;
      mCPU.Lines.Add(s);
    end;
  end;
  UpdateCPU_GUI();
end;

procedure TfmMain.iRunClick(Sender: TObject);
var
  addr: integer;
  path, ext: string;
  tStart, tEnd: TDateTime;
  elapsed: double;
  trace: boolean;
  ope, cyc: int64;
  mhz, mip: double;
begin
  path := iProgName.Text;
  if (Pos('.', path) < 1) then begin
    path := path + '.PRG';
  end;
  ext := LowerCase(Copy(path, Length(path) - 3, 4));
  if (ext = '.prg') then begin
    addr := DCU.LoadPRG(path);
  end
  else if (ext = '.com') then begin
    addr := $0100;
    DCU.LoadBIN(path, addr);
  end
  else begin
    addr := -3;
  end;
  if (addr >= 0) then begin
    tStart := Now;
    trace := cbTrace.Checked;
    CPU.Trace(trace);
    ope := CPU.cpu.Opers;
    cyc := CPU.cpu.Cycles;
    CPU.cpu.Run(addr, getVal(iSteps.Caption));
    tEnd := Now;
    elapsed := MilliSecondsBetween(tEnd, tStart) / 1000;
    if (elapsed > 0) then begin
      ope := CPU.cpu.Opers - ope;
      cyc := CPU.cpu.Cycles - cyc;
      mhz := (cyc / elapsed) / 1000000;
      mip := (ope / elapsed) / 1000000;
    end
    else begin
      mhz := 0;
      mip := 0;
    end;
    mTools.Lines.Add(Format(ExtractFileName(path) + ' executed in %.3fs %.3fMhz %.3fMIPS', [elapsed, mhz, mip]));
  end
  else begin
    mTools.Lines.Add(Format('Error %d loading file %s', [addr, path]));
  end;
end;

procedure TfmMain.bCPU_HaltClick(Sender: TObject);
begin
  CPU.cpu.state := ECPUState.stop;
  UpdateCPU_GUI();
end;

procedure TfmMain.bCPU_ResumeClick(Sender: TObject);
begin
  CPU.cpu.state := ECPUState.Active;
  UpdateCPU_GUI();
end;

procedure TfmMain.bCPU_SoftResetClick(Sender: TObject);
begin
  CPU.cpu.SoftReset();
  UpdateCPU_GUI();
end;

procedure TfmMain.bCPU_HardResetClick(Sender: TObject);
begin
  CPU.cpu.Reset();
  UpdateCPU_GUI();
end;

procedure TfmMain.UpdateCPU_GUI();
var
  halted: boolean;
begin
  halted := CPU.cpu.state = ECPUState.stop;
  bCPU_Halt.Enabled := not halted;
  bCPU_Resume.Enabled := halted;
  bCPU_HardReset.Enabled := halted;
end;

function TfmMain.getUsage(usage: AMemoryUsage): string;
var
  mu: EMemoryUsage;
begin
  Result := '';
  for mu in (usage * [mData, mCode]) do begin
    if (Result <> '') then begin
      Result := Result + ' ';
    end;
    Result := Result + MemoryUsageName[mu];
  end;
end;

procedure TfmMain.iDisAssClick(Sender: TObject);
var
  node: TAvgLvlTreeNode;
  addr: iSize16;
  adrStart, adrEnd: iSize16;
  instList: TInstructionList;
  memMap: TMemoryMap;
  memInfo: PMemoryArea;
  i, cnt: integer;
  inst: PInstruction;
  s, cmt: string;
begin
  adrStart := getValHex(iAdrStart.Text);
  if (adrStart <> -1) then begin
    adrEnd := getValHex(iAdrEnd.Text);
    if (adrEnd = -1) then begin
      adrEnd := adrStart + 20;
    end;
    instList := TInstructionList.Create;
    memMap := TMemoryMap.Create;
    try
      mTools.Lines.BeginUpdate;
      addr := adrStart;
      cnt := CPU.cpu.Disassemble(addr, adrEnd, instList, memMap);
      // Code Entries
      mTools.Lines.Add(';');
      mTools.Lines.Add('; Code references');
      mTools.Lines.Add(';');
      for Node in memMap do begin
        memInfo := PMemoryArea(Node.Data);
        with memInfo^ do begin
          if (refCode in usage) then begin
            s := IntToHex(addrStart, 4);
            s := 'C' + s + TAB + '.equ' + TAB + '$' + s;
            mTools.Lines.Add(s);
          end;
        end;
      end;
      // Data Entries
      mTools.Lines.Add(';');
      mTools.Lines.Add('; Data references');
      mTools.Lines.Add(';');
      for Node in memMap do begin
        memInfo := PMemoryArea(Node.Data);
        with memInfo^ do begin
          if (refData in usage) then begin
            s := IntToHex(addrStart, 4);
            s := 'D' + s + TAB + '.equ' + TAB + '$' + s + TAB + TAB + '; ' + IntToHex(addrStart, 4) + ': ' + IntToHex(CPU.cpu.ReadMem(addrStart), 2);
            mTools.Lines.Add(s);
          end;
        end;
      end;
      mTools.Lines.Add(';');
      mTools.Lines.Add('; Code');
      mTools.Lines.Add(';');
      mTools.Lines.Add(#9 + '.org' + #9 + '$' + IntToHex(adrStart, 4));
      if (cnt > 0) then begin
        for i := 0 to cnt - 1 do begin
          inst := instList[i];
          with inst^ do begin
            memInfo := memMap.FindArea(addr);
            cmt := '; ' + IntToHex(addr, 4) + ': ' + IntToHex(opcode, 2);
            if (def^.len = 1) then begin
              if (Pos(TAB, def^.fmt) <> 0) then begin
                cmt := cmt;
              end
              else begin
                cmt := TAB + cmt;
              end;
            end
            else if (def^.len = 2) then begin
              cmt := cmt + ' ' + IntToHeX(operand, 2);
            end
            else if (def^.len = 3) then begin
              s := IntToHeX(operand, 4);
              cmt := cmt + ' ' + Copy(s, 3, 2) + ' ' + Copy(s, 1, 2);
            end;
            if (memInfo = nil) then begin
              s := 'L' + IntToHex(addr, 4) + ':';
            end
            else if (refCode in memInfo^.usage) then begin
              s := 'L' + IntToHex(addr, 4);
            end
            else begin
              s := '';
            end;
            s := s + TAB + Format(def^.fmt, [operand]) + TAB + TAB + cmt;
            mTools.Lines.Add(s);
          end;
        end;
      end;
      // Memory Map
      memMap.Pack;
      mTools.Lines.Add('');
      mTools.Lines.Add('Memory Map:');
      for Node in memMap do begin
        s := '';
        memInfo := PMemoryArea(Node.Data);
        with memInfo^ do begin
          if (len > 1) then begin
            s := Format('%.4x - %.4x %s', [addrStart, addrStart + len - 1, getUsage(usage)]);
          end
          else if (len > 0) then begin
            s := Format('%.4x %s', [addrStart, getUsage(usage)]);
          end;
          if (s <> '') then begin
            mTools.Lines.Add(s);
          end;
        end;
      end;
    finally
      mTools.Lines.EndUpdate;
      FreeAndNil(instList);
      FreeAndNil(memMap);
    end;
  end;
end;


procedure TfmMain.iDumpClick(Sender: TObject);
var
  block: integer;
begin
  block := getValHex(iBlockID.Text);
  if (block <> -1) then begin
    DumpBlock(block);
  end;
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
            3: begin
              drawRight(dgDevices.Canvas, aRect, IntToHex(d.Conf.ramProv, 4));
            end;
            4: begin
              drawRight(dgDevices.Canvas, aRect, IntToHex(d.Conf.romProv, 4));
            end;
            5: begin
              drawRight(dgDevices.Canvas, aRect, IntToHex(d.Conf.ramReq, 4));
            end;
            6: begin
              drawLeft(dgDevices.Canvas, aRect, GetEnumName(TypeInfo(d.Status), Ord(d.Status)));
            end;
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

procedure TfmMain.pcComputerEnter(Sender: TObject);
begin
  UpdateCPU_GUI();
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
  if not cbLog.Checked then begin
    mTools.Lines.Add(MsgPasStr);
  end
  else begin
    DbgOutThreadLog(MsgPasStr + chr(13));
  end;
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
    mTools.Lines.Add(sb);
  end;
end;

procedure TfmMain.DumpBlock(block: BlockRange);
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
      mTools.Lines.Add(sb);
    end;
  end;
end;

end.
