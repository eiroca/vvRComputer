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
unit uCPU;

{$mode ObjFPC}{$H+}

interface

uses
  AvgLvlTree,
  Classes, SysUtils;

type
  bit = 0..1;
  nibble = 0..15;

  _8bit = bitpacked array [0..7] of bit;
  _16bit = bitpacked array [0..15] of bit;

  iSize8 = int32; // can hold a 8 bits register + overflow (9 bits min)
  iSize16 = int32; // can hold a 16 bits register + overflow (17 bits min)
  iSize32 = uint32;
  PSize32 = ^iSize32;

{$include CPU_const.inc}

type
  OpcodeCall = procedure() of object;

  EGroup = (arith, branch, control, Data, illegal, logic, stack);
  EMode = (imp, imm, ind_reg, reg, reg_i, abs);

  TFlags = array of boolean;

  PCPUInfo = ^RCPUInfo;
  RCPUInfo = record
    dataSize: integer;
    addrSize: integer;
    PCreg: integer;
    numRegs: integer;
    regsName: array of string;
    regsSize: array of integer;
    numFlags: integer;
    flagsName: array of string;
    numExtras: integer;
    extrasName: array of string;
    extrasSize: array of integer;
    littleEndian: boolean;
    numIRQs: integer;
    IRQsName: array of string;
    IRQsNMI: array of boolean;
  end;

  POpcode = ^ROpcode;
  ROpcode = record
    code: OpcodeCall;
    inst: string;
    fmt: string;
    group: EGroup;
    mode: EMode;
    len: integer;
    cycle: array of integer;
    state: array of integer;
  end;

  PIRQ = ^RIRQ;
  RIRQ = record
    masked: boolean;
    active: boolean;
  end;
  TIRQs = array of RIRQ;

  PCPUStatus = ^RCPUStatus;
  RCPUStatus = record
    opcode: POpcode;
    regs: array of iSize32;
    extras: array of iSize32;
    flags: TFlags;
    IntEnabled: boolean;
    irqs: TIRQs;
  end;

type

  { Instructions }

  PInstruction = ^RInstuction;
  RInstuction = record
    def: POpcode;
    addr: iSize32;
    opcode: iSize32;
    param1: iSize32;
    param2: iSize32;
  end;

  TInstructionList = class(TList, IFPObserver)
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  end;

type
  { Memory Map }

  EMemoryUsage = (mCode, mData, refCode, refData);
  AMemoryUsage = set of EMemoryUsage;

  PMemoryArea = ^RMemoryArea;
  RMemoryArea = record
    addrStart: iSize32;
    len: iSize32;
    usage: AMemoryUsage;
  end;

  TMemoryMap = class(TAvgLvlTree)
  public
    constructor Create;
  public
    procedure DisposeNode(ANode: TAvgLvlTreeNode); override;
  public
    procedure Pack();
    function FindArea(const addr: iSize32): PMemoryArea;
    procedure AddCode(const addr: iSize32; const aLen: integer);
    procedure AddRef(const addr: iSize32; const aType: AMemoryUsage; const aLen: integer = 0);
  end;

type

  { TCPU }

  HaltCall = procedure() of object;
  TraceCall = procedure(const trace: RCPUStatus) of object;

  generic TCPU<AddrType> = class
  private
    // Emulator hooks
    FHaltEvent: HaltCall;
    FTraceEvent: TraceCall;
  protected
    // Emulator status
    opers: int64;
    cycles: int64;
  protected
    FHalted: boolean;
    FIRQAllowed: boolean;
    FInfo: RCPUInfo;
    FStatus: RCPUStatus;
    FIRQs: TIRQs;
  protected
    // CPU status & Regs
    PC: AddrType;
    SP: AddrType;
    F: array of boolean;
  public
    constructor Create;
  protected
    function CheckIRQs(): boolean; virtual;
    procedure DoIRQ(int: integer); virtual;
  protected // Abstract
    procedure UpdateCPUInfo; virtual; abstract;
    procedure UpdateCPUStatus(fillExtra: boolean = True); virtual; abstract;
    function Step: POpcode; virtual; abstract;
    procedure DecodeInst(var addr: AddrType; var inst: RInstuction; incAddr: boolean = True); virtual; abstract;
  protected // property Getter/Setter
    procedure SetIRQ(int: integer; a: boolean);
    function GetCPUStatus: RCPUStatus;
  public // events
    property OnHalt: HaltCall read FHaltEvent write FHaltEvent;
    property OnTrace: TraceCall read FTraceEvent write FTraceEvent;
  public // properties
    property Halted: boolean read FHalted write FHalted;
    property IRQAllowed: boolean read FIRQAllowed write FIRQAllowed;
    property Info: RCPUInfo read FInfo;
    property Status: RCPUStatus read GetCPUStatus;
  public
    property IRQ[i: integer]: boolean write setIRQ;
  public // Commands
    procedure Reset(); virtual;
    procedure SoftReset(); virtual;
    function Run(var addr: AddrType; aTrace: boolean = False; steps: integer = -1): integer; virtual;
    function Disassemble(var addr: AddrType; const endAddr: AddrType; instList: TInstructionList; memMap: TMemoryMap; steps: integer = -1): integer;
  end;


type
  { TCPU_ClassA - 16 bits data address}

  Read8Memory16Call = function(const address: uint16): uint8 of object;
  Write8Memory16Call = procedure(const address: uint16; const val: uint8) of object;

  TCPU_ClassA = class(specialize TCPU<uint16>)
    // 8 bits data bus
    // 16 bits address bus
  protected
    // Emulator status
    states: int64;
  private
    // Emulator hooks
    FReadMem: Read8Memory16Call;
    FWriteMem: Write8Memory16Call;
  protected
    OpCodes: array of ROpcode;
  public
    property ReadMem: Read8Memory16Call read FReadMem write FReadMem;
    property WriteMem: Write8Memory16Call read FWriteMem write FWriteMem;
  public
    function Step: POpcode; override;
    procedure DecodeInst(var addr: uint16; var inst: RInstuction; incAddr: boolean = True); override;
  end;

implementation

{ TMemoryMap }
function AddressSorter(d1, d2: Pointer): integer;
var
  m1, m2: PMemoryArea;
  r: integer;
begin
  m1 := PMemoryArea(d1);
  m2 := PMemoryArea(d2);
  r := m1^.addrStart - m2^.addrStart;
  if (r = 0) then begin
    r := m1^.len - m2^.len;
  end;
  Result := r;
end;

function AddressRangeFinder(d1, d2: Pointer): integer;
var
  a1: iSize32;
  m2: PMemoryArea;
begin
  a1 := PSize32(d1)^;
  m2 := PMemoryArea(d2);
  if ((a1 = m2^.addrStart) or ((a1 > m2^.addrStart) and (a1 < (m2^.addrStart + m2^.len)))) then begin
    Result := 0;
  end
  else begin
    Result := a1 - m2^.addrStart;
  end;
end;

function AddressExactFinder(d1, d2: Pointer): integer;
var
  a1: iSize32;
  m2: PMemoryArea;
begin
  a1 := PSize32(d1)^;
  m2 := PMemoryArea(d2);
  if (a1 = m2^.addrStart) then begin
    Result := 0;
  end
  else begin
    Result := a1 - m2^.addrStart;
  end;
end;

constructor TMemoryMap.Create;
begin
  inherited Create(@AddressSorter);
end;

procedure TMemoryMap.DisposeNode(ANode: TAvgLvlTreeNode);
begin
  Dispose(PMemoryArea(ANode.Data));
  inherited DisposeNode(ANode);
end;

procedure TMemoryMap.Pack();
var
  node: TAvgLvlTreeNode;
  nodeOld: TAvgLvlTreeNode;
  memoryArea: PMemoryArea;
  memoryAreaOld: PMemoryArea;
  usage: AMemoryUsage;
  usageOld: AMemoryUsage;
  unused: TList;
  i: integer;
  newBlock: boolean;
begin
  nodeOld := nil;
  memoryAreaOld := nil;
  usageOld := [];
  unused := TList.Create;
  for Node in GetEnumerator do begin
    memoryArea := PMemoryArea(Node.Data);
    usage := memoryArea^.usage;
    newBlock := (memoryAreaOld = nil);
    if not newBlock and ((usageOld * [mCode, mData]) <> (usage * [mCode, mData])) then newBlock := True;
    if not newBlock and ((memoryAreaOld^.addrStart + memoryAreaOld^.len) <> memoryArea^.addrStart) then newBlock := True;
    if newBlock then begin
      nodeOld := node;
      memoryAreaOld := memoryArea;
      usageOld := usage;
    end
    else begin
      memoryAreaOld^.len := memoryAreaOld^.len + memoryArea^.len;
      if (usage * [refCode, refData] = []) then begin
        unused.Add(node);
      end
      else begin
        memoryArea^.len := 0;
        memoryArea^.usage := memoryArea^.usage - [mCode, mData];
      end;
    end;
  end;
  for i := 0 to unused.Count - 1 do begin
    Delete(TAvgLvlTreeNode(unused[i]));
  end;
  unused.Free;
end;

function TMemoryMap.FindArea(const addr: iSize32): PMemoryArea;
var
  node: TAvgLvlTreeNode;
begin
  node := FindKey(@addr, @AddressRangeFinder);
  Result := nil;
  if (node <> nil) then Result := PMemoryArea(node.Data);
end;

procedure TMemoryMap.AddCode(const addr: iSize32; const aLen: integer);
var
  node: TAvgLvlTreeNode;
  memoryArea: PMemoryArea;
  oldArea: PMemoryArea;
begin
  new(memoryArea);
  with memoryArea^ do begin
    addrStart := addr;
    len := aLen;
    usage := [mCode];
  end;
  node := FindKey(@addr, @AddressExactFinder);
  if (node = nil) then begin
    Add(memoryArea);
  end
  else begin
    oldArea := PMemoryArea(node.Data);
    with  oldArea^ do begin
      if (addrStart = addr) and ((len = 0) or (len = aLen)) then begin
        len := aLen;
        include(usage, mCode);
        dispose(memoryArea);
      end
      else begin
        Add(memoryArea);
      end;
    end;
  end;
end;

procedure TMemoryMap.AddRef(const addr: iSize32; const aType: AMemoryUsage; const aLen: integer = 0);
var
  node: TAvgLvlTreeNode;
  memoryArea: PMemoryArea;
begin
  node := FindKey(@addr, @AddressRangeFinder);
  if (node = nil) then begin
    new(memoryArea);
    with memoryArea^ do begin
      addrStart := addr;
      len := aLen;
      usage := aType;
    end;
    Add(memoryArea);
  end
  else begin
    memoryArea := PMemoryArea(node.Data);
    with memoryArea^ do begin
      usage := usage + aType;
    end;
  end;
end;

{ TInstructionList }

constructor TInstructionList.Create;
begin
  inherited;
  FPOAttachObserver(Self);
end;

destructor TInstructionList.Destroy;
begin
  inherited Destroy;
end;

procedure TInstructionList.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  if (Operation = ooDeleteItem) then begin
    dispose(PInstruction(Data));
  end;
end;

{ TCPU }

function TCPU.GetCPUStatus: RCPUStatus;
begin
  UpdateCPUStatus();
  FStatus.irqs := FIRQs;
  FStatus.IntEnabled := IRQAllowed;
  Result := FStatus;
end;

constructor TCPU.Create;
var
  i: integer;
begin
  UpdateCPUInfo();
  with FInfo do begin
    setLength(FStatus.regs, FInfo.numRegs);
    setLength(FStatus.flags, FInfo.numFlags);
    setLength(FStatus.extras, FInfo.numExtras);
    SetLength(F, numFlags);
    SetLength(FIRQs, numIRQs);
    for i := 0 to numIRQs - 1 do begin
      with FIRQs[i] do begin
        active := False;
        masked := False;
      end;
    end;
  end;
end;

procedure TCPU.SetIRQ(int: integer; a: boolean);
begin
  with FInfo do begin
    if (int >= 0) and (int < numIRQs) then begin
      FIRQs[int].active := a;
    end;
  end;
end;

procedure TCPU.Reset();
begin
  FHalted := True;
end;

function TCPU.CheckIRQs(): boolean;
var
  NMI: boolean;
  i: integer;
begin
  Result := False;
  with FInfo do begin
    for i := 0 to numIRQs - 1 do begin
      with FIRQs[i] do begin
        if FIRQs[i].active then begin
          NMI := IRQsNMI[i];
          if (NMI) or ((masked = False) and FIRQAllowed) then begin
            Result := True;
            DoIRQ(i);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCPU.DoIRQ(int: integer);
begin
  FIRQAllowed := False;
  FIRQs[int].active := False;
end;

function TCPU.Run(var addr: AddrType; aTrace: boolean = False; steps: integer = -1): integer;
var
  opcode: POpcode;
begin
  Result := 0;
  PC := addr;
  if not Assigned(FTraceEvent) then aTrace := False;
  while (not FHalted) and ((steps < 0) or (Result < steps)) do begin
    Result := (Result + 1) and $8FFFFFFF;
    if (aTrace) then begin
      UpdateCPUStatus();
    end;
    if CheckIRQs() = False then begin
      // No IRQs execute a opcode
      opcode := Step;
    end
    else begin
      opcode := nil;
    end;
    if (aTrace) then begin
      FStatus.opcode := opcode;
      OnTrace(FStatus);
    end;
  end;
  addr := PC;
end;

function TCPU_ClassA.Step: POpcode;
begin
  Inc(opers);
  Result := @OpCodes[ReadMem(PC)];
  PC := (PC + 1) and $FFFF;
  Result^.code();
end;

function TCPU.Disassemble(var addr: AddrType; const endAddr: AddrType; instList: TInstructionList; memMap: TMemoryMap; steps: integer = -1): integer;
var
  inst: PInstruction;
begin
  Result := 0;
  while (addr <= endAddr) and ((steps < 0) or (Result < steps)) do begin
    Inc(Result);
    new(inst);
    DecodeInst(addr, inst^);
    instList.Add(inst);
    with inst^, def^ do begin
      memMap.AddCode(addr, len);
      if (Result = 1) then begin
        memMap.AddRef(addr, [refCode]);
      end;
      if (def^.len = 3) then begin
        if (def^.mode = EMode.abs) then begin
          if (def^.group = EGroup.branch) then begin
            memMap.AddRef(param1, [refCode]);
          end
          else if (def^.group = EGroup.Data) then begin
            memMap.AddRef(param1, [mData, refData], 1);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCPU.SoftReset();
begin
  DoIRQ(0);
end;

procedure TCPU_ClassA.DecodeInst(var addr: uint16; var inst: RInstuction; incAddr: boolean);
var
  b1: iSize16;
  b2: iSize16;
  prm: iSize16;
  len: integer;
  opcode: iSize8;
begin
  prm := 0;
  opcode := ReadMem(addr);
  inst.addr := addr;
  inst.opcode := opcode;
  inst.def := @OpCodes[opcode];
  len := inst.def^.len;
  if (len = 2) then begin
    prm := ReadMem(addr + 1);
  end
  else if (len = 3) then begin
    b1 := ReadMem(addr + 1 + endian_offset[Info.littleEndian, 0]);
    b2 := ReadMem(addr + 1 + endian_offset[Info.littleEndian, 1]);
    prm := b2 shl 8 + b1;
  end;
  if (incAddr) then addr := addr + len;
  inst.param1 := prm;
  inst.param2 := 0;
end;


initialization
  assert(Sizeof(byte) = 1);
  assert(Sizeof(word) = 2);
  assert(Sizeof(cardinal) = 4);
  assert(Sizeof(integer) >= 4);
  assert(Sizeof(iSize8) > 1);
  assert(Sizeof(iSize16) > 2);
  assert(Sizeof(iSize32) >= 4);
end.
