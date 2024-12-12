unit formcontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  IniPropStorage, ExtCtrls, ComCtrls, EditBtn, Telegram, tgtypes
  ;

type

  { TFrmControl }

  TFrmControl = class(TForm)
    BtnFullScreen: TButton;
    BtnHandout: TButton;
    BtnScoreAddLeft: TButton;
    BtnScoreAddRight: TButton;
    ChckBxTelegram: TCheckBox;
    ChckBxTelegramHandout: TCheckBox;
    ChckBxTelegramScoreWhileMaximize: TCheckBox;
    EdtNameRight: TLabeledEdit;
    EdtHandoutFile: TFileNameEdit;
    InPrpStrg: TIniPropStorage;
    LblHandoutFile: TLabel;
    LblLeftScore: TLabel;
    LblRightScore: TLabel;
    EdtNameLeft: TLabeledEdit;
    EdtToken: TLabeledEdit;
    EdtChat: TLabeledEdit;
    Memo1: TMemo;
    PgCntrlControl: TPageControl;
    PgCntrlMain: TPageControl;
    PgCntrlOptions: TPageControl;
    RdGrpMonitors: TRadioGroup;
    SpnEdtScoreLeft: TSpinEdit;
    SpnEdtScoreRight: TSpinEdit;
    TbShtAnswers: TTabSheet;
    TbShtControlScore: TTabSheet;
    TbShtTelegram: TTabSheet;
    TbShtMonitors: TTabSheet;
    TbShtHandout: TTabSheet;
    TbShtHome: TTabSheet;
    TbShtOptions: TTabSheet;
    TglBxReceive: TToggleBox;
    ToolBar1: TToolBar;
    procedure BtnFullScreenClick({%H-}Sender: TObject);
    procedure BtnHandoutClick({%H-}Sender: TObject);
    procedure BtnScoreAddLeftClick({%H-}Sender: TObject);
    procedure BtnScoreAddRightClick({%H-}Sender: TObject);
    procedure ChckBxTelegramChange(Sender: TObject);
    procedure EdtHandoutFileAcceptFileName({%H-}Sender: TObject; var Value: String);
    procedure EdtNameLeftChange(Sender: TObject);
    procedure EdtNameRightChange(Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormDestroy({%H-}Sender: TObject);
    procedure FormShow({%H-}Sender: TObject);
    procedure SpnEdtScoreLeftChange(Sender: TObject);
    procedure SpnEdtScoreRightChange(Sender: TObject);
    procedure TglBxReceiveChange(Sender: TObject);
  private
    FTelegramReceiver: TReceiverThread;
    FTelegramScore: TTelegramFace;
    FTelegramHandout: TTelegramFace;
    FDoUpdateTelegram: Boolean;
    procedure DoChangeScore;
    procedure DoMaximizeScore(aForm: TForm);
    procedure DoMaximizeHandout(aForm: TForm);
    procedure DoUpdateScore;    
    procedure DoUpdateHandout;
    procedure FormAppendMessage(aMsg: TTelegramMessageObj);
    procedure FormHandoutMaximizeWindow(Sender: TObject);
    procedure FormScoreMaximizeWindow(Sender: TObject);
  public

  end;

var
  FrmControl: TFrmControl;

implementation

uses
  formviewpanel, formhandout, eventlog, tgutils, DateUtils
  ;

{$R *.lfm}

function ScoreCaption(const aLeftTitle, aRightTitle: String; aLeftScore, aRightScore: Byte): String;
begin
  Result:='Счёт ('+aLeftScore.ToString+' : '+aRightScore.ToString+'). '+aLeftTitle+' - '+aLeftScore.ToString+', '+
    aRightTitle+' - '+aRightScore.ToString;
end;

{ TFrmControl }

procedure TFrmControl.BtnFullScreenClick(Sender: TObject);
begin
  FrmView.SwitchFullScreen(RdGrpMonitors.ItemIndex);
end;

procedure TFrmControl.BtnHandoutClick(Sender: TObject);
begin
  FrmHandout.SwitchFullScreen(RdGrpMonitors.ItemIndex);
end;

procedure TFrmControl.BtnScoreAddLeftClick(Sender: TObject);
begin
  SpnEdtScoreLeft.Value:=SpnEdtScoreLeft.Value+1;
end;

procedure TFrmControl.BtnScoreAddRightClick(Sender: TObject);
begin
  SpnEdtScoreRight.Value:=SpnEdtScoreRight.Value+1;
end;

procedure TFrmControl.ChckBxTelegramChange(Sender: TObject);
begin
  ChckBxTelegramScoreWhileMaximize.Enabled:=(Sender as TCheckBox).Checked;
end;

procedure TFrmControl.EdtHandoutFileAcceptFileName(Sender: TObject; var Value: String);
begin
  FrmHandout.LoadFromFile(Value);
end;

procedure TFrmControl.EdtNameLeftChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.LeftTitle:=(Sender as TLabeledEdit).Text;
  DoChangeScore;
end;

procedure TFrmControl.EdtNameRightChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.RightTitle:=(Sender as TLabeledEdit).Text;
  DoChangeScore;
end;

procedure TFrmControl.FormCreate(Sender: TObject);
var
  aLogger: TEventLog;
begin
  FTelegramScore:=TTelegramFace.Create;
  FTelegramHandout:=TTelegramFace.Create;
  aLogger:=TEventLog.Create(nil);
  aLogger.LogType:=ltFile;
  aLogger.Active:=True;
  FTelegramScore.Bot.Logger:=aLogger;
  FTelegramScore.Bot.LogDebug:=True;
  FTelegramHandout.Bot.Logger:=aLogger;
  FTelegramHandout.Bot.LogDebug:=True;
  FDoUpdateTelegram:=False;
  ChckBxTelegramChange(ChckBxTelegram);
end;

procedure TFrmControl.FormDestroy(Sender: TObject);
begin
  if Assigned(FTelegramReceiver) then
  begin
    FTelegramReceiver.Terminate;
    FTelegramReceiver.WaitFor;
  end;
  FTelegramScore.Bot.Logger.Free;
  FTelegramHandout.Free;
  FTelegramScore.Free;
end;

procedure TFrmControl.FormShow(Sender: TObject);
begin
  FrmView.Visible:=True;
  FDoUpdateTelegram:=False;
  SpnEdtScoreLeftChange(SpnEdtScoreLeft);
  SpnEdtScoreRightChange(SpnEdtScoreRight);
  EdtNameLeftChange(EdtNameLeft);
  EdtNameRightChange(EdtNameRight);
  RdGrpMonitors.Enabled:=Screen.MonitorCount>1;
  FDoUpdateTelegram:=True;
  DoChangeScore;
  FrmHandout.LoadFromFile();
  FrmView.OnMaximizeWindow:=@FormScoreMaximizeWindow;
  FrmHandout.OnMaximizeWindow:=@FormHandoutMaximizeWindow;
end;

procedure TFrmControl.SpnEdtScoreLeftChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.LeftScore:=(Sender as TSpinEdit).Value.ToString;
  DoChangeScore;
end;

procedure TFrmControl.SpnEdtScoreRightChange(Sender: TObject);
begin
  if not Assigned(FrmView) then
    Exit;
  FrmView.RightScore:=(Sender as TSpinEdit).Value.ToString;
  DoChangeScore;
end;

procedure TFrmControl.TglBxReceiveChange(Sender: TObject);
begin
  TglBxReceive.Enabled:=False;
  try
    if (Sender as TToggleBox).Checked then
    begin
      FTelegramReceiver:=TReceiverThread.Create(EmptyStr);
      FTelegramReceiver.FreeOnTerminate:=True;
      FTelegramReceiver.OnAppendMessage:=@FormAppendMessage;
      FTelegramReceiver.Bot.Token:=EdtToken.Text;
      FTelegramReceiver.Start;
    end
    else begin
      FTelegramReceiver.Terminate;
      FTelegramReceiver.WaitFor;  
      FTelegramReceiver:=nil;
    end;
  finally
    TglBxReceive.Enabled:=True;
  end;
end;

procedure TFrmControl.DoChangeScore;
begin
  if ChckBxTelegram.Checked and not ChckBxTelegramScoreWhileMaximize.Checked then
    DoUpdateScore;
end;

procedure TFrmControl.DoMaximizeScore(aForm: TForm);
begin
  if (aForm.WindowState<>wsMaximized) and  ChckBxTelegram.Checked and ChckBxTelegramScoreWhileMaximize.Checked then
    DoUpdateScore;
end;

procedure TFrmControl.DoMaximizeHandout(aForm: TForm);
begin
  if (aForm.WindowState<>wsMaximized) and ChckBxTelegramHandout.Checked then
    DoUpdateHandout;
end;

procedure TFrmControl.DoUpdateScore;
var
  aBitMap: TBitmap;
  aStream: TMemoryStream;
  aChatID: int64;
begin
  if not FDoUpdateTelegram then
    Exit;
  aBitMap:=FrmView.GetFormImage;
  aStream:=TMemoryStream.Create;
  Cursor:=crHourGlass;
  try
    aBitMap.SaveToStream(aStream);
    if EdtToken.Text<>EmptyStr then
      if TryStrToInt64(Trim(EdtChat.Text), aChatID) then
      begin
        FTelegramScore.Chat:=aChatID;
        FTelegramScore.Bot.Token:=EdtToken.Text;
        FTelegramScore.UpdatePicture(aStream, ScoreCaption(EdtNameLeft.Text, EdtNameRight.Text,
          SpnEdtScoreLeft.Value, SpnEdtScoreRight.Value));
      end;
  finally
    Cursor:=crDefault;
    aStream.Free;
    aBitMap.Free;
  end;
end;

procedure TFrmControl.DoUpdateHandout;
var
  aStream: TMemoryStream;
  aChatID: int64;
begin
  if not FDoUpdateTelegram then
    Exit;
  aStream:=TMemoryStream.Create;
  Cursor:=crHourGlass;
  try
    aStream.LoadFromFile(FrmHandout.FileName);
    if EdtToken.Text<>EmptyStr then
      if TryStrToInt64(Trim(EdtChat.Text), aChatID) then
      begin
        FTelegramHandout.Chat:=aChatID;
        FTelegramHandout.Bot.Token:=EdtToken.Text;
        FTelegramHandout.UpdatePicture(aStream, EmptyStr);
      end;
  finally
    Cursor:=crDefault;
    aStream.Free;
  end;
end;

procedure TFrmControl.FormAppendMessage(aMsg: TTelegramMessageObj);
begin
  Memo1.Lines.Add(aMsg.Text+' / '+CaptionFromUser(aMsg.From)+' ['+DateTimeToStr(UnixToDateTime(aMsg.Date, False))+']');
end;

procedure TFrmControl.FormHandoutMaximizeWindow(Sender: TObject);
begin
  DoMaximizeHandout(Sender as TForm);
end;

procedure TFrmControl.FormScoreMaximizeWindow(Sender: TObject);
begin
  DoMaximizeScore(Sender as TForm);
end;

end.

