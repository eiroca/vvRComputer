program vvComputer;

{$mode objfpc}{$H+}

uses // -
 {$IFDEF UNIX} {$IFDEF UseCThreads} cthreads, {$ENDIF} {$ENDIF}
  Interfaces, Forms, // -
  uCPU, ucpu_808x, uCPU_6502, uDevice, // -
  FMain, FTerminal;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

