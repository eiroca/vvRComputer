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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  bit = 0..1;
  nible = 0..15;
  _8bit = bitpacked array [0..7] of bit;
  _16bit = bitpacked array [0..15] of bit;

  iSize8 = int32; // can hold a 8 bits register + overflow (9 bits min)
  iSize16 = int32; // can hold a 16 bits register + overflow (17 bits min)
  iSize32 = uint32;

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

type
  OpcodeCall = procedure() of object;

  EMode = (imp, imm, ind_reg, ind_abs, reg, reg_i, cjmp_abs, jmp_abs, abs);

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

  PInst = ^RInst;
  RInst = record
    def: POpcode;
    addr: iSize32;
    opcode: iSize32;
    param1: iSize32;
    param2: iSize32;
  end;

type
  HaltCall = procedure() of object;

type
  TCPU = class
  protected
    // Emulator status
    opers: int64;
    cycles: int64;
    isHalted: boolean;
  private
    // Emulator hooks
    FHalt: HaltCall;
  public
    property Halt: HaltCall read FHalt write FHalt;
  end;

type
  ReadMemoryCall = function(const address: uint16): byte of object;
  WriteMemoryCall = procedure(const address: uint16; const val: byte) of object;

type

  { TCPU_Class8 }

  TCPU_Class8 = class(TCPU)
  protected
    PC: iSize16;
  protected
    // Emulator status
    states: int64;
  private
    // Emulator hooks
    FReadMem: ReadMemoryCall;
    FWriteMem: WriteMemoryCall;
  protected
    OpCodes: array of ROpcode;
  public
    property ReadMem: ReadMemoryCall read FReadMem write FReadMem;
    property WriteMem: WriteMemoryCall read FWriteMem write FWriteMem;
  public
    procedure Run(const addr: uint16; steps: integer);
    function Step: POpcode;
  public
    procedure DecodeInst(var addr: uint16; var inst: RInst; incAddr: boolean = True);
    function DisAss(var addr: uint16; const endAddr: uint16; instList, CodePoint, DataPoint: TStrings): integer;

  end;

implementation

procedure TCPU_Class8.Run(const addr: uint16; steps: integer);
var
  i: integer;
begin
  PC := addr;
  if (steps <= 0) then steps := MaxInt;
  for i := 1 to steps do begin
    if (isHalted) then break;
    Step;
  end;
end;

function TCPU_Class8.Step: POpcode;
var
  opcode: POpcode;
  call: OpcodeCall;
begin
  Inc(opers);
  opcode := @OpCodes[ReadMem(PC)];
  PC := (PC + 1) and $FFFF;
  call := opcode^.code;
  call();
  Result := opcode;
end;

procedure TCPU_Class8.DecodeInst(var addr: uint16; var inst: RInst; incAddr: boolean);
var
  b1: iSize16;
  b2: iSize16;
  prm: iSize16;
  len: integer;
  opcode: iSize8;
begin
  opcode := ReadMem(addr);
  prm := 0;
  inst.addr := addr;
  inst.opcode := opcode;
  inst.def := @OpCodes[opcode];
  len := inst.def^.len;
  if (len = 1) then begin
  end
  else if (len = 2) then begin
    prm := ReadMem(addr + 1);
  end
  else if (len = 3) then begin
    b1 := ReadMem(addr + 1);
    b2 := ReadMem(addr + 2);
    prm := b2 shl 8 + b1;
  end;
  if (incAddr) then addr := addr + len;
  inst.param1 := prm;
  inst.param2 := 0;
end;

function TCPU_Class8.DisAss(var addr: uint16; const endAddr: uint16; instList, CodePoint, DataPoint: TStrings): integer;
var
  inst: RInst;
  s, cmt: string;
begin
  codePoint.Add(IntToHex(addr, 4));
  inst.opcode := 0;
  Result := 0;
  while (addr <= endAddr) do begin
    Inc(Result);
    DecodeInst(addr, inst);
    with inst do begin
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
        if (def^.mode = jmp_abs) or (def^.mode = cjmp_abs) then begin
          codePoint.Add(s);
        end
        else if (def^.mode = imm) or (def^.mode = abs) then begin
          dataPoint.Add(IntToHeX(param1, 4));
        end;
      end;
      s := 'L' + IntToHex(addr, 4) + ':' + TAB + Format(def^.fmt, [param1]) + TAB + TAB + cmt;
      instList.Add(s);
    end;
  end;
end;


initialization
  assert(Sizeof(iSize8) > 1);
  assert(Sizeof(iSize16) > 2);
  assert(Sizeof(iSize32) >= 4);
end.

