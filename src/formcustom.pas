unit formcustom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls
  ;

type

  { TFrmMonitorControl }

  TFrmMonitorControl = class(TForm)
    procedure FormKeyDown({%H-}Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
  private
    FOnMaximizeWindow: TNotifyEvent;
    FOriginalBounds: TRect;
    FOriginalWindowState: TWindowState;
  protected
    procedure ChangeFontSize(aPanel: TPanel);
    procedure ChangeFontSizeName(aLabel: TLabel);
  public    
    procedure SwitchFullScreen(aMonitor: SmallInt = -1);
    property OnMaximizeWindow: TNotifyEvent read FOnMaximizeWindow write FOnMaximizeWindow;
  end;

implementation

uses
  LCLType
  ;

{$R *.lfm}

{ TFrmMonitorControl }

procedure TFrmMonitorControl.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F11 then
  begin
    SwitchFullScreen;
    Key := 0;
  end;
end;

procedure TFrmMonitorControl.ChangeFontSize(aPanel: TPanel);
begin
  aPanel.Font.Height:=trunc(aPanel.Height*1.2);
end;

procedure TFrmMonitorControl.ChangeFontSizeName(aLabel: TLabel);
begin
  aLabel.Font.Height:=Height div 10;
end;

procedure TFrmMonitorControl.SwitchFullScreen(aMonitor: SmallInt);
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
    if Assigned(FOnMaximizeWindow) then
      FOnMaximizeWindow(Self);
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

