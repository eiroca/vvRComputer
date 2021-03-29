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
unit uDevice;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  cthreads,
  {$endif}
  uCPU, uCPU_808x, uCPU_6502,
  AvgLvlTree, typinfo,
  LMessages, LCLIntf, LCLType, LCLProc,
  Forms,
  Classes, SysUtils;

const
  WM_DEVICE_MESSAGE = LM_USER + 2020;
  WM_CPU_TRACE = LM_USER + 2021;

const
  BlockSize = 4096;
  MaxBlocks = 65536;
  MaxDevices = 16;

type

  DeviceRange = 0..(MaxDevices - 1);
  BlockRange = 0..(MaxBlocks - 1);
  DataBlockRange = 0..(BlockSize - 1);
  AddressRange = 0..(65536 * BlockSize);

  PDeviceRange = ^DeviceRange;
  PBlockRange = ^BlockRange;
  PDataBlockRange = ^DataBlockRange;
  PAddressRange = ^AddressRange;

  RegisterNum = 0..15;
  InterruptNum = 0..15;
  SignalNum = 0..15;

  RegisterValue = 0..65535;
  SignalValue = RegisterValue;

  DeviceIDType = 0..255;
  DeviceSubType = 0..255;
  VendorType = array [0..5] of byte;
  SpecType = array [0..1] of byte;

  SignalCall = function(p: SignalValue): SignalValue of object;
  InterruptCall = procedure(id: InterruptNum) of object;
  RegisterWriteCall = procedure(reg: RegisterNum; p: RegisterValue) of object;
  RegisterReadCall = function(reg: RegisterNum): RegisterValue of object;

const
  SIGNAL_RESET: SignalNum = 0;

  DU_MAIN: DeviceIDType = 0;
  DU_MAIN_Generic: DeviceSubType = 0;

  DU_CPU: DeviceIDType = 1;
  DU_CPU_Generic: DeviceSubType = 0;
  DU_CPU_8080: DeviceSubType = 1;
  DU_CPU_8085: DeviceSubType = 2;

  DU_VDU: DeviceIDType = 2;
  DU_VDU_Generic: DeviceSubType = 0;

  DU_SDU: DeviceIDType = 3;
  DU_SDU_Generic: DeviceSubType = 0;

  EIROCA: VendorType = (0, 0, 0, 0, 0, 0);
  NULLSPEC: SpecType = (0, 0);

  DCU_SIGNAL_SCAN: SignalNum = 2;
  DCU_SIGNAL_COPY: SignalNum = 3;

  DCU_REG_SRC: RegisterNum = 2;
  DCU_REG_DST: RegisterNum = 3;

  DCU_CMD_COPY_BANK: SignalValue = 1;

const
  DeviceTypeName: array[0..3] of string = ('DCU', 'CPU', 'VDU', 'SDU');
  DPUSubTypeName: array[0..0] of string = ('Generic');
  CPUSubTypeName: array[0..2] of string = ('Generic', '8080', '8085');
  VDUSubTypeName: array[0..0] of string = ('Generic');
  SDUSubTypeName: array[0..0] of string = ('Generic');

const
  RAM_START: BlockRange = low(BlockRange);
  ROM_END: BlockRange = high(BlockRange);

type
  DeviceConfType = packed record
    devID: DeviceIDType;
    devSubType: DeviceSubType;
    vendor: VendorType;
    ramReq: BlockRange;
    ramProv: BlockRange;
    romProv: BlockRange;
    spec: SpecType;
  end;

  DeviceType = record
    conf: DeviceConfType;
    interrupt: InterruptCall;
    regRead: RegisterReadCall;
    regWrite: RegisterWriteCall;
    signals: array [SignalNum] of SignalCall;
  end;

  DeviceCommandStatus = (cNew, cRunning, cDone);
  DeviceCommandType = (noop, interrupt, readReg, writeReg, signal);

  PDeviceCommand = ^DeviceCommand;

  DeviceCommand = record
    status: DeviceCommandStatus;
    RetVal: RegisterValue;
    case cmdType: DeviceCommandType of
      noop: ( // No Op
      );
      interrupt: ( // Interrupt
        int: InterruptNum;
      );
      readReg: ( // ReadReg
        regRead: RegisterNum;
      );
      writeReg: ( // WriteReg
        regWrite: RegisterNum;
        regValue: RegisterValue;
      );
      signal: ( // Signal
        signalNum: SignalNum;
        signalVal: SignalValue;
      );
  end;

type
  EBlockPerm = (pRead, pWrite);
  BlockData = array[DataBlockRange] of byte;
  BlockPerms = set of EBlockPerm;

  TBlock = class
    id: BlockRange;
    perm: BlockPerms;
    Data: BlockData;
  end;

type
  NumBank = 0..15;
  CPUAddress = 0..65535;

const
  MASK_BANK = %1111000000000000;
  MASK_ADDRESS = %0000111111111111;

  DCU_NUM: DeviceRange = 0;

type
  TStatusEvent = procedure(Status: string) of object;

const
  MAX_WAITS: integer = 100;
  WAIT_TIME: integer = 50;

type
  TDeviceUnit = class;

  DeviceStatus = (dIdle, dSleep, dWait, dCommand, dBusy);

  { TDevice }

  TDevice = class(TThread)
  private
    commands: TThreadList;
    maxCmd: integer;
  private
    FMessageHandler: HWND;
    FStatus: DeviceStatus;
  private
    function getDeviceConf: DeviceConfType;
    function GetIsBusy: boolean;
    procedure logDeviceStaus(Status: string);
  protected
    devNum: DeviceRange;
    def: DeviceType;
    FLocker: DeviceRange;
    FDCU: TDeviceUnit;
  public
    property DCU: TDeviceUnit read FDCU write FDCU;
    property MessageHandler: HWND read FMessageHandler write FMessageHandler;
    property isBusy: boolean read GetIsBusy;
    property locker: DeviceRange read FLocker;
    property Conf: DeviceConfType read getDeviceConf;
    property Status: DeviceStatus read FStatus;
  public
    constructor Create;
    procedure Execute; override;
    procedure DoTask; virtual;
    destructor Destroy; override;
  protected
    procedure logMessage(Msg: string);
  protected
    function Wait: RegisterValue; virtual;
    procedure DoInterrupt(id: InterruptNum); virtual;
    procedure DoRegisterWrite(reg: RegisterNum; val: RegisterValue); virtual;
    function DoRegisterRead(reg: RegisterNum): RegisterValue; virtual;
    procedure DoSignal(s: SignalNum; p: SignalValue); virtual;
  protected
    function SignalReset(p: SignalValue): SignalValue; virtual;
  end;

  { TDeviceUnit }

  TDeviceUnit = class(TDevice)
  private
    blocks: TAvgLvlTree;
    cache: array[0..15] of TBlock;
    devices: array[DeviceRange] of TDevice;
    regs: array[RegisterNum] of RegisterValue;
    function GetDevice(index: DeviceRange): TDevice;
  public
    constructor Create;
    procedure DoTask; override;
    function AddDevice(device: TDevice): DeviceRange;
  public
    function LoadBanks(const startBank: BlockRange; const filename: string): boolean;
    function LoadBIN(const filename: string; addr: integer): boolean;
    function LoadPRG(const filename: string): integer;
  public
    property Device[idex: DeviceRange]: TDevice read GetDevice;
  public
    function Signal(const Sender, dev: DeviceRange; const cmd: SignalNum; param: SignalValue): SignalValue;
    function ReadRegister(const Sender, dev: DeviceRange; const reg: RegisterNum): RegisterValue;
    procedure WriteRegister(const Sender, dev: DeviceRange; const reg: RegisterNum; const val: RegisterValue);
  public
    function createBlock(id: BlockRange; Clear: boolean): TBlock;
    function getBlock(id: BlockRange; createIsMissing: boolean): TBlock;
    function readMemory(const block: BlockRange; const offset: DataBlockRange): byte; inline;
    function readMemory(const addr: AddressRange): byte; inline;
    procedure writeMemory(const block: BlockRange; const offset: DataBlockRange; const v: byte); inline;
    procedure writeMemory(const addr: AddressRange; const v: byte); inline;
  protected
    procedure Interrupt(id: InterruptNum);
    procedure RegisterWrite(reg: RegisterNum; p: RegisterValue); virtual;
    function RegisterRead(reg: RegisterNum): RegisterValue; virtual;
    function SignalReset(p: SignalValue): SignalValue; override;
    function SignalScan(p: SignalValue): SignalValue; virtual;
    function SignalCopy(p: SignalValue): SignalValue; virtual;
  end;

  { TProcessorUnit }

  generic TProcessorUnit<CPUType> = class(TDevice)
  public
    cpu: CPUType;
  private
    banks: array[NumBank] of BlockRange;
  public
    halt: boolean;
  public
    constructor Create(aDCU: TDeviceUnit);
    procedure Trace(const traceOn: boolean); virtual;
  public
    function ReadMemoryCall(const address: iSize16): iSize8;
    procedure WriteMemoryCall(const address: iSize16; const val: iSize8);
  protected
    procedure logTrace(Msg: string);
    procedure Reset; virtual;
    procedure CopyBlock(const srcBank, dstBank: BlockRange);
    procedure RegisterWrite(reg: RegisterNum; p: RegisterValue); virtual;
    function RegisterRead(reg: RegisterNum): RegisterValue; virtual;
  protected
    function SignalReset(p: SignalValue): SignalValue; override;
  end;

  { T8080ProcessorUnit }

  T8080ProcessorUnit = class(specialize TProcessorUnit<TCPU_8080>)
  public
    constructor Create(aDCU: TDeviceUnit);
    function ReadIOCall(const address: byte): byte;
    procedure WriteIOCall(const address: byte; const val: byte);
    procedure HaltCall;
    procedure TraceCPU(const aTrace: RCPUStatus);
    procedure Trace(const traceOn: boolean); override;
  protected
    procedure Reset; override;
  end;

  { T8085ProcessorUnit }

  T8085ProcessorUnit = class(T8080ProcessorUnit)
  public
    constructor Create(aDCU: TDeviceUnit);
  end;

  { TVideoUnit }

  TVideoUnit = class(TDevice)
  public
    constructor Create(aDCU: TDeviceUnit);
  end;

  { TSoundUnit }

  TSoundUnit = class(TDevice)
  public
    constructor Create(aDCU: TDeviceUnit);
  end;

implementation

{ TDeviceUnit }
function BlockSorter(d1, d2: Pointer): integer;
var
  id1, id2: BlockRange;
begin
  id1 := TBlock(d1).id;
  id2 := TBlock(d2).id;
  Result := id1 - id2;
end;

function BlockFinder(d1, d2: Pointer): integer;
var
  id1, id2: BlockRange;
begin
  id1 := PBlockRange(d1)^;
  id2 := TBlock(d2).id;
  Result := id1 - id2;
end;

constructor TDevice.Create;
var
  i: SignalNum;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  commands := TThreadList.Create;
  devNum := 0;
  FillByte(def, SizeOf(def), 0);
  def.regRead := nil;
  def.regWrite := nil;
  def.interrupt := nil;
  for i := low(SignalNum) to High(SignalNum) do begin
    def.signals[i] := nil;
  end;
  def.signals[SIGNAL_RESET] := @SignalReset;
  FStatus := dSleep;
  maxCmd := 1;
end;

procedure TDevice.Execute;
var
  cmds: TList;
  i, cmdDone: integer;
  cmd: PDeviceCommand;
begin
  logMessage(ClassName + ' starting...');
  try
    while (not Terminated) do begin
      cmds := commands.LockList;
      try
        cmdDone := 0;
        for i := 0 to cmds.Count - 1 do begin
          cmd := PDeviceCommand(cmds.Items[i]);
          if (cmd <> nil) and (cmd^.status = cNew) then begin
            with cmd^ do begin
              status := cRunning;
              FStatus := dCommand;
              if (cmdType <> noop) then begin
                case (cmdType) of
                  interrupt: begin
                    if Assigned(def.interrupt) then begin
                      def.interrupt(int);
                    end
                    else begin
                      logMessage(ClassName + ' Interrupt ' + IntToStr(int));
                    end;
                  end;
                  readReg: begin
                    if Assigned(def.regRead) then begin
                      RetVal := def.regRead(regRead);
                    end
                    else begin
                      logMessage(ClassName + ' RegisterRead ' + IntToStr(regRead));
                    end;
                  end;
                  writeReg: begin
                    if Assigned(def.regWrite) then begin
                      def.regWrite(regWrite, regValue);
                    end
                    else begin
                      logMessage(ClassName + ' RegisterWrite ' + IntToStr(regWrite) + ' <- $' + IntToHex(regValue, 4));
                    end;
                  end;
                  signal: begin
                    if Assigned(def.signals[signalNum]) then begin
                      RetVal := def.signals[signalNum](signalVal);
                    end
                    else begin
                      logMessage(ClassName + ' Signal ' + IntToStr(signalNum) + ' $' + IntToHex(signalVal, 4));
                    end;
                  end;
                end;
              end;
              status := cDone;
            end;
            Inc(cmdDone);
            if (cmdDone >= maxCmd) then begin
              break;
            end;
          end;
        end;
      finally
        commands.UnlockList;
      end;
      FStatus := dBusy;
      try
        DoTask;
      except
        on E: Exception do logMessage(ClassName + ' exception: ' + e.Message);
      end;
      FStatus := dIdle;
    end;
  except
    on E: Exception do logMessage(ClassName + ' fatal: ' + E.Message);
  end;
  logMessage(ClassName + ' stopping...');
end;

procedure TDevice.DoTask;
begin
  sleep(100);
end;

destructor TDevice.Destroy;
begin
  Wait;
  commands.Free;
end;

procedure TDevice.logMessage(Msg: string);
var
  PMessage: PChar;
begin
  if MessageHandler = 0 then begin
    Exit;
  end;
  Msg := IntToStr(GetThreadID) + ' - ' + Msg;
  PMessage := StrAlloc(Length(Msg) + 1);
  StrCopy(PMessage, PChar(Msg));
  PostMessage(MessageHandler, WM_DEVICE_MESSAGE, WParam(PMessage), 0);
end;

function TDevice.Wait: RegisterValue;
var
  cnt: integer;
  cmds: TList;
  cmd: PDeviceCommand;
  i: integer;
  cmdPending: integer;
begin
  Result := 0;
  cnt := 0;
  cmdPending := 1;
  while (cnt < MAX_WAITS) and (cmdPending > 0) do begin
    logMessage('Waiting... ' + IntToStr(cnt));
    Inc(cnt);
    cmds := commands.LockList;
    try
      i := 0;
      while (i < cmds.Count) do begin
        cmd := PDeviceCommand(cmds.Items[i]);
        Inc(i);
        if (cmd <> nil) then begin
          with cmd^ do begin
            if (status = cDone) then begin
              Result := RetVal;
              cmds.Remove(cmd);
              Dispose(cmd);
              i := 0;
            end;
          end;
        end;
      end;
      cmdPending := cmds.Count;
    finally
      commands.UnlockList;
    end;
    if (cmdPending > 0) and (WAIT_TIME > 0) then begin
      sleep(WAIT_TIME);
    end;
  end;
  if (cnt >= MAX_WAITS) and (cmdPending > 0) then begin
    raise  EBusError.Create(ClassName + ' waiting timeout');

  end;
end;

function TDevice.getDeviceConf: DeviceConfType;
begin
  Result := def.conf;
end;

function TDevice.GetIsBusy: boolean;
begin
  Result := (FStatus = dBusy) or (FStatus = dCommand);
end;

procedure TDevice.logDeviceStaus(Status: string);
begin
  logMessage(status);
end;

procedure TDevice.DoInterrupt(id: InterruptNum);
var
  cmd: PDeviceCommand;
begin
  new(cmd);
  with cmd^ do begin
    status := cNew;
    cmdType := DeviceCommandType.interrupt;
    int := id;
  end;
  commands.Add(cmd);
end;

procedure TDevice.DoRegisterWrite(reg: RegisterNum; val: RegisterValue);
var
  cmd: PDeviceCommand;
begin
  new(cmd);
  with cmd^ do begin
    status := cNew;
    cmdType := DeviceCommandType.writeReg;
    regWrite := reg;
    regValue := val;
  end;
  commands.Add(cmd);
end;

function TDevice.DoRegisterRead(reg: RegisterNum): RegisterValue;
var
  cmd: PDeviceCommand;
begin
  new(cmd);
  with cmd^ do begin
    status := cNew;
    cmdType := DeviceCommandType.readReg;
    regRead := reg;
  end;
  commands.Add(cmd);
  //  Result :=  Wait;
  Result := 0;
end;

procedure TDevice.DoSignal(s: SignalNum; p: SignalValue);
var
  cmd: PDeviceCommand;
begin
  new(cmd);
  with cmd^ do begin
    status := cNew;
    cmdType := DeviceCommandType.signal;
    signalNum := s;
    signalVal := p;
  end;
  commands.Add(cmd);
end;

function TDevice.SignalReset(p: SignalValue): SignalValue;
begin
  logMessage(ClassName + ' SignalReset');
  Result := 0;
end;

function TDeviceUnit.GetDevice(index: DeviceRange): TDevice;
begin
  Result := devices[index];
end;

constructor TDeviceUnit.Create;
var
  i: integer;
  d: DeviceRange;
begin
  inherited Create;
  DCU := Self;
  devNum := DCU_NUM;
  blocks := TAvgLvlTree.Create(@BlockSorter);
  // Create RAM blocks
  for i := 0 to 23 do begin
    createBlock(RAM_START + i, True);
  end;
  // Create ROM blocks
  for i := 0 to 7 do begin
    createBlock(ROM_END - i, False);
  end;
  with def.conf do begin
    devID := DU_MAIN;
    devSubType := DU_MAIN_Generic;
    vendor := EIROCA;
    ramReq := 0;
    ramProv := 24; // 96KiB
    romProv := 8; // 32KiB
    spec := NULLSPEC;
  end;
  def.regRead := @RegisterRead;
  def.regWrite := @RegisterWrite;
  def.interrupt := @interrupt;
  def.signals[DCU_SIGNAL_SCAN] := @SignalScan;
  def.signals[DCU_SIGNAL_COPY] := @SignalCopy;
  for d := low(DeviceRange) to High(DeviceRange) do begin
    devices[d] := nil;
  end;
  devices[DCU_NUM] := Self;
  DoSignal(SIGNAL_RESET, 0);
  for i := 0 to Length(cache) - 1 do begin
    cache[i] := getBlock(i, True);
  end;
end;

function TDeviceUnit.AddDevice(device: TDevice): DeviceRange;
var
  d: DeviceRange;
begin
  for d := low(DeviceRange) to High(DeviceRange) do begin
    if (devices[d] = nil) then begin
      devices[d] := device;
      Result := d;
      exit;
    end;
  end;
end;

function TDeviceUnit.LoadBanks(const startBank: BlockRange; const filename: string): boolean;
var
  b: TBlock;
  DataStream: TFilestream;
  i, lenRead: integer;
  block, numBlocks: integer;
begin
  Result := FileExists(filename);
  if (Result) then begin
    try
      block := startBank;
      dataStream := TFileStream.Create(filename, fmOpenRead);
      if (DataStream.Size mod BlockSize) = 0 then begin
        numBlocks := DataStream.Size div BlockSize;
        for i := 0 to numBlocks - 1 do begin
          b := getBlock(block, True);
          lenRead := DataStream.Read(b.Data, BlockSize);
          Result := (lenRead = BlockSize);
          if (not Result) then begin
            break;
          end;
          Inc(block);
          if (block > MaxBlocks) then begin
            Result := False;
            break;
          end;
        end;
      end
      else begin
        Result := False;
      end;
    finally
      dataStream.Free;
    end;
  end;
end;

function TDeviceUnit.LoadBIN(const filename: string; addr: integer): boolean;
var
  DataStream: TFilestream;
  i, len: integer;
  v: byte;
begin
  Result := FileExists(filename);
  if (Result) then begin
    dataStream := TFileStream.Create(filename, fmOpenRead);
    try
      len := DataStream.Size;
      for i := 0 to len - 1 do begin
        v := DataStream.ReadByte;
        writeMemory(addr, v);
        Inc(addr);
      end;
    finally
      dataStream.Free;
    end;
  end;
end;

function TDeviceUnit.LoadPRG(const filename: string): integer;
var
  DataStream: TFilestream;
  addr, i, len: integer;
  v: byte;
begin
  if (FileExists(filename)) then begin
    dataStream := TFileStream.Create(filename, fmOpenRead);
    len := DataStream.Size;
    if (len > 2) then begin
      addr := DataStream.ReadByte + 256 * DataStream.ReadByte;
      Result := addr;
      try
        for i := 0 to len - 3 do begin
          v := DataStream.ReadByte;
          writeMemory(addr, v);
          Inc(addr);
        end;
      finally
        dataStream.Free;
      end;
    end
    else begin
      Result := -2;
    end;
  end
  else begin
    Result := -1;
  end;
end;

procedure TDeviceUnit.DoTask;
begin
end;

function TDeviceUnit.Signal(const Sender, dev: DeviceRange; const cmd: SignalNum; param: SignalValue): SignalValue;
begin
  with devices[dev].def do begin
    Result := signals[cmd](param);
  end;
end;

function TDeviceUnit.ReadRegister(const Sender, dev: DeviceRange; const reg: RegisterNum): RegisterValue;
begin
  with devices[dev].def do begin
    Result := def.regRead(reg);
  end;
end;

procedure TDeviceUnit.WriteRegister(const Sender, dev: DeviceRange; const reg: RegisterNum; const val: RegisterValue);
begin
  with devices[dev].def do begin
    def.regWrite(reg, val);
  end;
end;

function TDeviceUnit.createBlock(id: BlockRange; Clear: boolean): TBlock;
var
  b: TBlock;
begin
  b := TBlock.Create;
  b.id := id;
  b.perm := [pRead, pWrite];
  if (Clear) then begin
    FillByte(b.Data, sizeOf(b.Data), 0);
  end;
  blocks.Add(b);
  Result := b;
end;

function TDeviceUnit.getBlock(id: BlockRange; createIsMissing: boolean): TBlock;
var
  blk: TAvgLvlTreeNode;
begin
  Result := nil;
  blk := blocks.FindKey(@id, @BlockFinder);
  if ((blk = nil) and createIsMissing) then begin
    Result := createBlock(id, True);
  end
  else begin
    Result := TBlock(blk.Data);
  end;
end;

function TDeviceUnit.readMemory(const block: BlockRange; const offset: DataBlockRange): byte;
var
  b: TBlock;
begin
  if (block < Length(cache)) then begin
    b := cache[block];
  end
  else begin
    b := getBlock(block, False);
  end;
  Result := b.Data[offset];
end;

function TDeviceUnit.readMemory(const addr: AddressRange): byte;
var
  block: BlockRange;
  offset: DataBlockRange;
begin
  block := (addr shr 12) and $FFFF;
  offset := addr and $0FFF;
  Result := readMemory(block, offset);
end;

procedure TDeviceUnit.writeMemory(const block: BlockRange; const offset: DataBlockRange; const v: byte);
var
  b: TBlock;
begin
  if (block < Length(cache)) then begin
    b := cache[block];
  end
  else begin
    b := getBlock(block, False);
  end;
  b.Data[offset] := v;
end;

procedure TDeviceUnit.writeMemory(const addr: AddressRange; const v: byte);
var
  block: BlockRange;
  offset: DataBlockRange;
begin
  block := (addr shr 12) and $FFFF;
  offset := addr and $0FFF;
  writeMemory(block, offset, v);
end;

procedure TDeviceUnit.Interrupt(id: InterruptNum);
begin

end;

procedure TDeviceUnit.RegisterWrite(reg: RegisterNum; p: RegisterValue);
begin
  logMessage(ClassName + ' WREG ' + IntToStr(reg) + ' <- ' + IntToStr(p));
  regs[reg] := p;
end;

function TDeviceUnit.RegisterRead(reg: RegisterNum): RegisterValue;
begin
  Result := regs[reg];
end;

function TDeviceUnit.SignalReset(p: SignalValue): SignalValue;
var
  d: DeviceRange;
begin
  logMessage(ClassName + ' SignalReset');
  Result := 0;
  for d := low(DeviceRange) + 1 to High(DeviceRange) do begin
    if Assigned(devices[d]) then begin
      devices[d].DoSignal(SIGNAL_RESET, 0);
      Inc(Result);
    end;
  end;
end;

function TDeviceUnit.SignalScan(p: SignalValue): SignalValue;
begin
  logMessage(ClassName + ' SignalScan');
  Result := p;
end;

function TDeviceUnit.SignalCopy(p: SignalValue): SignalValue;
var
  blkSrc, blkDst: TAvgLvlTreeNode;
  srcBlk, dstBlk: BlockRange;
begin
  Result := 0;
  case (p) of
    1: begin
      srcBlk := regs[DCU_REG_SRC];
      dstBlk := regs[DCU_REG_DST];
      blkSrc := blocks.FindKey(@srcBlk, @BlockFinder);
      blkDst := blocks.FindKey(@dstBlk, @BlockFinder);
      if (blkSrc <> nil) and (blkDst <> nil) then begin
        logMessage(ClassName + ' BlockCopy ' + IntToHex(srcBlk, 4) + ' -> ' + IntToHex(dstBlk, 4));
        TBlock(blkDst.Data).Data := TBlock(blkSrc.Data).Data;
        Result := SizeOf(TBlock(blkDst.Data).Data);
      end
      else begin
        logMessage(ClassName + ' Failed BlockCopy ' + IntToHex(srcBlk, 4) + ' -> ' + IntToHex(dstBlk, 4));
        Result := 0;
      end;
    end;
  end;
end;

{ T8080ProcessorUnit }

constructor T8080ProcessorUnit.Create(aDCU: TDeviceUnit);
begin
  inherited Create(aDCU);
  def.conf.devSubType := DU_CPU_8080;
  cpu := TCPU_8080.Create;
  with TCPU_8080(cpu) do begin
    ReadMem := @ReadMemoryCall;
    WriteMem := @WriteMemoryCall;
    ReadIO := @ReadIOCall;
    WriteIO := @WriteIOCall;
    OnHalt := @HaltCall;
  end;
  Trace(False);
end;


function T8080ProcessorUnit.ReadIOCall(const address: byte): byte;
begin
  Result := 0;
end;

procedure T8080ProcessorUnit.WriteIOCall(const address: byte; const val: byte);
begin

end;

procedure T8080ProcessorUnit.HaltCall;
begin
  halt := True;
end;

procedure T8080ProcessorUnit.TraceCPU(const aTrace: RCPUStatus);
var
  s: string;
begin
  s := CreateTrace(cpu.Info, aTrace, nil);
  logTrace(s);
end;

procedure T8080ProcessorUnit.Trace(const traceOn: boolean);
begin
  if (traceOn) then begin
    cpu.OnTrace := @TraceCPU;
  end
  else begin
    cpu.OnTrace := nil;
  end;
end;

procedure T8080ProcessorUnit.Reset;
begin
  inherited Reset;
  cpu.Reset;
end;

{ T8085ProcessorUnit }

constructor T8085ProcessorUnit.Create(aDCU: TDeviceUnit);
begin
  inherited Create(aDCU);
  def.conf.devSubType := DU_CPU_8085;
  cpu := TCPU_8085.Create;
  with TCPU_8085(cpu) do begin
    ReadMem := @ReadMemoryCall;
    WriteMem := @WriteMemoryCall;
    ReadIO := @ReadIOCall;
    WriteIO := @WriteIOCall;
    OnHalt := @HaltCall;
    OnTrace := @TraceCPU;
  end;
end;

{ TProcessingUnit }

constructor TProcessorUnit.Create(aDCU: TDeviceUnit);
begin
  inherited Create;
  DCU := aDCU;
  def.conf.devID := DU_CPU;
  devNum := DCU.AddDevice(Self);
  def.regRead := @RegisterRead;
  def.regWrite := @RegisterWrite;
  halt := False;
end;

procedure TProcessorUnit.Trace(const traceOn: boolean);
begin

end;

procedure TProcessorUnit.logTrace(Msg: string);
var
  PMessage: PChar;
begin
  if MessageHandler = 0 then begin
    Exit;
  end;
  PMessage := StrAlloc(Length(Msg) + 1);
  StrCopy(PMessage, PChar(Msg));
  PostMessage(MessageHandler, WM_CPU_TRACE, WParam(PMessage), 0);
  DbgOutThreadLog(Msg + chr(13));
end;

function TProcessorUnit.ReadMemoryCall(const address: iSize16): iSize8;
var
  block: BlockRange;
  bank: integer;
  offset: integer;
begin
  bank := (address shr 12) and $000F;
  offset := address and $0FFF;
  block := banks[bank];
  Result := DCU.readMemory(block, offset);
end;

procedure TProcessorUnit.WriteMemoryCall(const address: iSize16; const val: iSize8);
var
  block: BlockRange;
  bank: integer;
  offset: integer;
begin
  bank := (address shr 12) and $000F;
  offset := address and $0FFF;
  block := banks[bank];
  DCU.writeMemory(block, offset, val);
end;

procedure TProcessorUnit.Reset;
var
  i: NumBank;
begin
  logMessage(ClassName + ' SignalReset');
  // Reset bank
  for i := 0 to 15 do begin
    banks[i] := i;
  end;
  CopyBlock(ROM_END, RAM_START);
end;

procedure TProcessorUnit.CopyBlock(const srcBank, dstBank: BlockRange);
var
  res: RegisterValue;
begin
  DCU.DoRegisterWrite(DCU_REG_SRC, srcBank);
  DCU.DoRegisterWrite(DCU_REG_DST, dstBank);
  DCU.DoSignal(DCU_SIGNAL_COPY, DCU_CMD_COPY_BANK);
  res := DCU.Wait;
  logMessage('DCU copied ' + IntToStr(res) + ' byte(s)');
end;

procedure TProcessorUnit.RegisterWrite(reg: RegisterNum; p: RegisterValue);
begin
  if (reg > 0) then begin
    banks[reg] := p;
  end;
end;

function TProcessorUnit.RegisterRead(reg: RegisterNum): RegisterValue;
begin
  if (reg > 0) then begin
    Result := banks[reg];
  end
  else begin
    Result := 0;
  end;
end;


function TProcessorUnit.SignalReset(p: SignalValue): SignalValue;
begin
  Reset;
  Result := 0;
end;

{ TVideoUnit }

constructor TVideoUnit.Create(aDCU: TDeviceUnit);
begin
  inherited Create;
  DCU := aDCU;
  def.conf.devID := DU_VDU;
  def.conf.devSubType := DU_VDU_Generic;
  def.conf.ramReq := 8;
  devNum := DCU.AddDevice(Self);
end;

{ TSoundUnit }

constructor TSoundUnit.Create(aDCU: TDeviceUnit);
begin
  inherited Create;
  DCU := aDCU;
  def.conf.devID := DU_SDU;
  def.conf.devSubType := DU_SDU_Generic;
  def.conf.ramReq := 1;
  devNum := DCU.AddDevice(Self);
end;

end.

