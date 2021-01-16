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

const

  TAB = #9;

  bit_val: array[0..31] of iSize32 = (
    %00000000000000000000000000000001, %00000000000000000000000000000010,
    %00000000000000000000000000000100, %00000000000000000000000000001000,
    %00000000000000000000000000010000, %00000000000000000000000000100000,
    %00000000000000000000000001000000, %00000000000000000000000010000000,
    %00000000000000000000000100000000, %00000000000000000000001000000000,
    %00000000000000000000010000000000, %00000000000000000000100000000000,
    %00000000000000000001000000000000, %00000000000000000010000000000000,
    %00000000000000000100000000000000, %00000000000000001000000000000000,
    %00000000000000010000000000000000, %00000000000000100000000000000000,
    %00000000000001000000000000000000, %00000000000010000000000000000000,
    %00000000000100000000000000000000, %00000000001000000000000000000000,
    %00000000010000000000000000000000, %00000000100000000000000000000000,
    %00000001000000000000000000000000, %00000010000000000000000000000000,
    %00000100000000000000000000000000, %00001000000000000000000000000000,
    %00010000000000000000000000000000, %00100000000000000000000000000000,
    %01000000000000000000000000000000, %10000000000000000000000000000000
    );

  bit_mask: array[0..31] of iSize32 = (
    %11111111111111111111111111111110, %11111111111111111111111111111101,
    %11111111111111111111111111111011, %11111111111111111111111111110111,
    %11111111111111111111111111101111, %11111111111111111111111111011111,
    %11111111111111111111111110111111, %11111111111111111111111101111111,
    %11111111111111111111111011111111, %11111111111111111111110111111111,
    %11111111111111111111101111111111, %11111111111111111111011111111111,
    %11111111111111111110111111111111, %11111111111111111101111111111111,
    %11111111111111111011111111111111, %11111111111111110111111111111111,
    %11111111111111101111111111111111, %11111111111111011111111111111111,
    %11111111111110111111111111111111, %11111111111101111111111111111111,
    %11111111111011111111111111111111, %11111111110111111111111111111111,
    %11111111101111111111111111111111, %11111111011111111111111111111111,
    %11111110111111111111111111111111, %11111101111111111111111111111111,
    %11111011111111111111111111111111, %11110111111111111111111111111111,
    %11101111111111111111111111111111, %11011111111111111111111111111111,
    %10111111111111111111111111111111, %01111111111111111111111111111111
    );

  count_of_bits: array[0..255] of iSize8 = (
    0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
    );

  parity_of_bits: array[0..255] of boolean = (
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    True, False, False, True, False, True, True, False, False, True, True, False, True, False, False, True,
    False, True, True, False, True, False, False, True, True, False, False, True, False, True, True, False
    );

  bool_bit: array[boolean] of iSize8 = (0, 1);
  endian_offset: array[boolean, 0..1] of iSize8 = ((1, 0), (0, 1));

type
  OpcodeCall = procedure() of object;

  EMode = (imp, imm, ind_reg, ind_abs, reg, reg_i, cjp_abs, jmp_abs, abs);

  TFlags = array of boolean;

  PIRQ = ^RIRQ;
  RIRQ = record
    masked: boolean;
    active: boolean;
  end;
  TIRQs = array of RIRQ;

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
    mode: EMode;
    len: integer;
    cycle: array of integer;
    state: array of integer;
  end;

  PInstruction = ^RInstuction;
  RInstuction = record
    def: POpcode;
    addr: iSize32;
    opcode: iSize32;
    param1: iSize32;
    param2: iSize32;
  end;

  EMemoryUsage = (mCode, mData, refCode, refData);
  MemoryUsage = set of EMemoryUsage;

  PMemoryArea = ^RMemoryArea;
  RMemoryArea = record
    addrStart: iSize32;
    len: iSize32;
    usage: MemoryUsage;
  end;

  PCPUStatus = ^RCPUStatus;
  RCPUStatus = record
    opcode: POpcode;
    regs: array of iSize32;
    extras: array of iSize32;
    flags: TFlags;
  end;

type

  { TInstructionList }

  TInstructionList = class(TList, IFPObserver)
  public
    constructor Create;
    destructor Destroy; override;
  private
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  end;

  { TMemoryMap }

  TMemoryMap = class(TAvgLvlTree)
  public
    constructor Create;
  public
    procedure DisposeNode(ANode: TAvgLvlTreeNode); override;
  public
    procedure Pack();
    function FindArea(const addr: iSize32): PMemoryArea;
    procedure AddCode(const addr: iSize32; const aLen: integer);
    procedure AddRef(const addr: iSize32; const aType: MemoryUsage);
  end;

type
  HaltCall = procedure() of object;
  TraceCall = procedure(const trace: RCPUStatus) of object;

type
  Read8Memory16Call = function(const address: uint16): uint8 of object;
  Write8Memory16Call = procedure(const address: uint16; const val: uint8) of object;

type

  { TCPU }

  generic TCPU<AddrType> = class
  private
    // Emulator hooks
    FHalt: HaltCall;
    FTrace: TraceCall;
    FCPUInfo: RCPUInfo;
  protected
    // Emulator status
    opers: int64;
    cycles: int64;
    isHalted: boolean;
  protected
    // CPU status & Regs
    PC: AddrType;
    SP: AddrType;
    F: array of boolean;
    IRQs: TIRQs;
    interruptAllowed: boolean;
  public
    constructor Create;
  protected
    procedure SetIRQ(int: integer; a: boolean);
  public
    property Halt: HaltCall read FHalt write FHalt;
    property Trace: TraceCall read FTrace write FTrace;
    property CPUInfo: RCPUInfo read FCPUInfo;
    property IRQ[i: integer]: boolean write setIRQ;
  protected
    procedure CheckIRQs(); virtual;
    procedure DoIRQ(int: integer); virtual;
  protected // Abstract
    procedure GetCPUInfo(var info: RCPUInfo); virtual; abstract;
    procedure Reset; virtual; abstract;
    function Step: POpcode; virtual; abstract;
    procedure DecodeInst(var addr: AddrType; var inst: RInstuction; incAddr: boolean = True); virtual; abstract;
  public
    function Run(var addr: AddrType; aTrace: boolean = False; steps: integer = -1): integer; virtual;
    function Disassemble(var addr: AddrType; const endAddr: AddrType; instList: TInstructionList; memMap: TMemoryMap; steps: integer = -1): integer;
  public
    function AllocCPUStatus: PCPUStatus;
    procedure GetStatus(var status: RCPUStatus; fillExtra: boolean = True); virtual; abstract;
    function GetIRQs: TIRQs;
    function GetInterruptAllowed: boolean;
  end;


type

  { TCPU_ClassA }

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
  usage: MemoryUsage;
  usageOld: MemoryUsage;
  unused: TList;
  i: integer;
begin
  nodeOld := nil;
  memoryAreaOld := nil;
  usageOld := [];
  unused := TList.Create;
  for Node in GetEnumeratorHighToLow do begin
    memoryArea := PMemoryArea(Node.Data);
    usage := memoryArea^.usage;
    if (nodeOld = nil) or (usageOld <> (usage * [mCode, mData])) then begin
      nodeOld := node;
      memoryAreaOld := memoryArea;
      usageOld := usage;
    end
    else begin
      memoryArea^.len := memoryArea^.len + memoryAreaOld^.len;
      unused.Add(nodeOld);
      nodeOld := node;
      memoryAreaOld := memoryArea;
      usageOld := memoryAreaOld^.usage;
    end;
    if (usageOld * [refData, refCode] <> []) then begin
      nodeOld := nil;
      memoryAreaOld := nil;
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

procedure TMemoryMap.AddRef(const addr: iSize32; const aType: MemoryUsage);
var
  node: TAvgLvlTreeNode;
  memoryArea: PMemoryArea;
begin
  node := FindKey(@addr, @AddressRangeFinder);
  if (node = nil) then begin
    new(memoryArea);
    with memoryArea^ do begin
      addrStart := addr;
      len := 0;
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

constructor TCPU.Create;
var
  i: integer;
begin
  GetCPUInfo(FCPUInfo);
  with FCPUInfo do begin
    SetLength(F, numFlags);
    SetLength(IRQs, numIRQs);
    for i := 0 to numIRQs - 1 do begin
      with IRQs[i] do begin
        active := False;
        masked := False;
      end;
    end;
  end;
end;

procedure TCPU.SetIRQ(int: integer; a: boolean);
begin
  with FCPUInfo do begin
    if (int >= 0) and (int < numIRQs) then begin
      IRQs[int].active := a;
    end;
  end;
end;

procedure TCPU.CheckIRQs();
var
  NMI: boolean;
  i: integer;
begin
  with FCPUInfo do begin
    for i := 0 to numIRQs - 1 do begin
      with IRQs[i] do begin
        if IRQs[i].active then begin
          NMI := IRQsNMI[i];
          if (NMI) or ((masked = False) and interruptAllowed) then begin
            DoIRQ(i);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCPU.DoIRQ(int: integer);
begin
  interruptAllowed := False;
  IRQs[int].active := False;
end;

function TCPU.Run(var addr: AddrType; aTrace: boolean = False; steps: integer = -1): integer;
var
  status: PCPUStatus;
  opcode: POpcode;
begin
  Result := 0;
  PC := addr;
  if not Assigned(FTrace) then aTrace := False;
  if (aTrace) then begin
    status := AllocCPUStatus;
  end;
  while (not isHalted) and ((steps < 0) or (Result < steps)) do begin
    Result := (Result + 1) and $8FFFFFFF;
    if (aTrace) then begin
      GetStatus(status^);
    end;
    opcode := Step;
    if (aTrace) then begin
      status^.opcode := opcode;
      Trace(status^);
    end;
  end;
  if (aTrace) then begin
    Dispose(status);
  end;
  addr := PC;
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
        if (def^.mode = jmp_abs) or (def^.mode = cjp_abs) then begin
          memMap.AddRef(param1, [refCode]);
        end
        else if (def^.mode = imm) or (def^.mode = abs) then begin
          memMap.AddRef(param1, [mData, refData]);
        end;
      end;
    end;
  end;
end;

function TCPU.AllocCPUStatus: PCPUStatus;
begin
  new(Result);
  with Result^ do begin
    setLength(regs, FCPUInfo.numRegs);
    setLength(flags, FCPUInfo.numFlags);
    setLength(extras, FCPUInfo.numExtras);
  end;
end;

function TCPU.GetIRQs: TIRQs;
begin
  Result := IRQs;
end;

function TCPU.GetInterruptAllowed: boolean;
begin
  Result := interruptAllowed;
end;

function TCPU_ClassA.Step: POpcode;
begin
  Inc(opers);
  Result := @OpCodes[ReadMem(PC)];
  PC := (PC + 1) and $FFFF;
  Result^.code();
  CheckIRQs();
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
    b1 := ReadMem(addr + 1 + endian_offset[CPUInfo.littleEndian, 0]);
    b2 := ReadMem(addr + 1 + endian_offset[CPUInfo.littleEndian, 1]);
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
