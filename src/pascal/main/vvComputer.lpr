program vvComputer;

{$mode objfpc}{$H+}

uses // -
 {$IFDEF UNIX} {$IFDEF UseCThreads} cthreads, {$ENDIF} {$ENDIF}
  Interfaces, Forms, // -
  FMain;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

