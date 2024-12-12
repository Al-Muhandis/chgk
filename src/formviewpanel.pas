unit formviewpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  ExtCtrls, formcustom
  ;

type

  { TFrmView }

  TFrmView = class(TFrmMonitorControl)
    LblNameLeft: TLabel;
    LblNameRight: TLabel;
    PnlRight: TPanel;
    PnlLeft: TPanel;
    procedure FormResize({%H-}Sender: TObject);
  private
    procedure SetLeftScore(AValue: String);
    procedure SetLeftTitle(AValue: String);
    procedure SetRightScore(AValue: String);
    procedure SetRightTitle(AValue: String);
  public
    property LeftTitle: String write SetLeftTitle;
    property RightTitle: String write SetRightTitle;
    property LeftScore: String write SetLeftScore;
    property RightScore: String write SetRightScore;
  end;

var
  FrmView: TFrmView;

implementation

{$R *.lfm}

{ TFrmView }

procedure TFrmView.FormResize(Sender: TObject);
begin
  PnlRight.Width:=Width div 2;
  ChangeFontSize(PnlRight);
  ChangeFontSize(PnlLeft);
  ChangeFontSizeName(LblNameLeft);
  ChangeFontSizeName(LblNameRight);
end;

procedure TFrmView.SetLeftScore(AValue: String);
begin
  PnlLeft.Caption:=AValue;
end;

procedure TFrmView.SetLeftTitle(AValue: String);
begin
  LblNameLeft.Caption:=AValue;
end;

procedure TFrmView.SetRightScore(AValue: String);
begin
  PnlRight.Caption:=AValue;
end;

procedure TFrmView.SetRightTitle(AValue: String);
begin
  LblNameRight.Caption:=AValue;
end;

end.

