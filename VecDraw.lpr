program VecDraw;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UDrawBox, UAbout, UFigures, UTransform
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TDrawForm, DrawForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.Run;
end.

