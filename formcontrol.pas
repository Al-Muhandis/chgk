unit formcontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  IniPropStorage, ExtCtrls;

type

  { TFrmControl }

  TFrmControl = class(TForm)
    BtnFullScreen: TButton;
    BtnScoreAddLeft: TButton;
    BtnScoreAddRight: TButton;
    EdtNameRight: TLabeledEdit;
    InPrpStrg: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    EdtNameLeft: TLabeledEdit;
    RdGrpMonitors: TRadioGroup;
    SpnEdtScoreLeft: TSpinEdit;
    SpnEdtScoreRight: TSpinEdit;
    procedure BtnFullScreenClick(Sender: TObject);
    procedure BtnScoreAddLeftClick(Sender: TObject);
    procedure BtnScoreAddRightClick(Sender: TObject);
    procedure EdtNameLeftChange(Sender: TObject);
    procedure EdtNameRightChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpnEdtScoreLeftChange(Sender: TObject);
    procedure SpnEdtScoreRightChange(Sender: TObject);
  private
  public

  end;

var
  FrmControl: TFrmControl;

implementation

uses
  formviewpanel
  ;

{$R *.lfm}

{ TFrmControl }

procedure TFrmControl.BtnFullScreenClick(Sender: TObject);
begin
  FrmView.SwitchFullScreen(RdGrpMonitors.ItemIndex);
end;

procedure TFrmControl.BtnScoreAddLeftClick(Sender: TObject);
begin
  SpnEdtScoreLeft.Value:=SpnEdtScoreLeft.Value+1;
end;

procedure TFrmControl.BtnScoreAddRightClick(Sender: TObject);
begin
  SpnEdtScoreRight.Value:=SpnEdtScoreRight.Value+1;
end;

procedure TFrmControl.EdtNameLeftChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.LblNameLeft.Caption:=(Sender as TLabeledEdit).Text;
end;

procedure TFrmControl.EdtNameRightChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.LblNameRight.Caption:=(Sender as TLabeledEdit).Text;
end;

procedure TFrmControl.FormShow(Sender: TObject);
begin
  FrmView.Visible:=True;
  SpnEdtScoreLeftChange(SpnEdtScoreLeft);
  SpnEdtScoreRightChange(SpnEdtScoreRight);
  EdtNameLeftChange(EdtNameLeft);
  EdtNameRightChange(EdtNameRight);
  RdGrpMonitors.Enabled:=Screen.MonitorCount>1;
end;

procedure TFrmControl.SpnEdtScoreLeftChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.PnlLeft.Caption:=(Sender as TSpinEdit).Value.ToString;
end;

procedure TFrmControl.SpnEdtScoreRightChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.PnlRight.Caption:=(Sender as TSpinEdit).Value.ToString;
end;

end.

