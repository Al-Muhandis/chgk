program chgk;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formcontrol, formviewpanel, formhandout
  ;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Табло "Что Где Когда"';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFrmControl, FrmControl);
  Application.CreateForm(TFrmView, FrmView);
  Application.CreateForm(TFrmHandout, FrmHandout);
  Application.Run;
end.

