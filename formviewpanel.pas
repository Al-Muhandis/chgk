unit formviewpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  ExtCtrls
  ;

type

  { TFrmView }

  TFrmView = class(TForm)
    LblNameLeft: TLabel;
    LblNameRight: TLabel;
    PnlRight: TPanel;
    PnlLeft: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure FormResize(Sender: TObject);
  private
    FOriginalBounds: TRect;
    FOriginalWindowState: TWindowState;
    procedure ChangeFontSize(aPanel: TPanel);
    procedure ChangeFontSizeName(aLabel: TLabel);
  public
    procedure SwitchFullScreen(aMonitor: SmallInt = -1);
  end;

var
  FrmView: TFrmView;

implementation

{$R *.lfm}

{ TFrmView }

procedure TFrmView.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if Key = VK_F11 then
  begin
    SwitchFullScreen;
    Key := 0;
  end;
end;

procedure TFrmView.FormResize(Sender: TObject);
begin
  PnlRight.Width:=Width div 2;
  ChangeFontSize(PnlRight);
  ChangeFontSize(PnlLeft);
  ChangeFontSizeName(LblNameLeft);
  ChangeFontSizeName(LblNameRight);
end;

procedure TFrmView.ChangeFontSize(aPanel: TPanel);
begin
  aPanel.Font.Height:=trunc(aPanel.Height*1.2);
end;

procedure TFrmView.ChangeFontSizeName(aLabel: TLabel);
begin
  aLabel.Font.Height:=Height div 10;
end;

procedure TFrmView.SwitchFullScreen(aMonitor: SmallInt);
begin
  if Visible=False then
    Show;
  if BorderStyle <> bsNone then begin
    // To full screen
    FOriginalWindowState := WindowState;
    FOriginalBounds := BoundsRect;

    BorderStyle := bsNone;
    if (Screen.MonitorCount=1) or (aMonitor=-1) then
      BoundsRect := Screen.MonitorFromWindow(Handle, mdNearest).BoundsRect
    else
      BoundsRect:=Screen.Monitors[aMonitor].BoundsRect;
  end else begin
    // From full screen
    BorderStyle := bsSizeable;
    if FOriginalWindowState = wsMaximized then
      WindowState := wsMaximized
    else
      BoundsRect := FOriginalBounds;
  end;
end;

end.

