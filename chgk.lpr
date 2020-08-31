program chgk;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formviewpanel, formcontrol
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Табло "Что Где Когда"';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFrmControl, FrmControl);
  Application.CreateForm(TFrmView, FrmView);
  Application.Run;
end.

