unit formhandout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls, formcustom
  ;

type

  { TFrmHandout }

  TFrmHandout = class(TFrmMonitorControl)
    Img: TImage;
    OpenDialog1: TOpenDialog;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure FormWindowStateChange(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    FFileName: String;
  public
    procedure LoadFromFile(aFileName: String = '');
    property FileName: String read FFileName;
  end;

var
  FrmHandout: TFrmHandout;

implementation

{$R *.lfm}

{ TFrmHandout }

procedure TFrmHandout.ToolButton1Click(Sender: TObject);
begin
  Img.Picture.Clear;
end;

procedure TFrmHandout.FormWindowStateChange(Sender: TObject);
begin
  if (WindowState=wsFullScreen) or (WindowState=wsMaximized) then
    ToolBar1.Hide
  else
    ToolBar1.Show;
end;

procedure TFrmHandout.ToolButton2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadFromFile(OpenDialog1.FileName);
end;

procedure TFrmHandout.LoadFromFile(aFileName: String);
begin
  if aFileName.IsEmpty then
    aFileName:=FFileName;
  if not aFileName.IsEmpty then
  begin
    Img.Picture.LoadFromFile(aFileName);
    FFileName:=aFileName;
  end
end;

end.

