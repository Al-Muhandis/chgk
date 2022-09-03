unit telegram;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgsendertypes, taskworker, tgtypes
  ;

type

  { TTelegramTask }

  TTelegramTask = class(TPersistent)
  private
    FCaption: String;
    FStream: TMemoryStream;
    function GetStream: TStream;
    procedure SetStream(AValue: TStream);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
    destructor Destroy; override;
  published
    property Caption: String read FCaption write FCaption;
    property Stream: TStream read GetStream write SetStream;
  end;

  TCustomTelegramThread = specialize TgTaskWorkerThread<TTelegramTask>;

  { TTelegramThread }

  TTelegramThread = class(TCustomTelegramThread)
  private
    FBot: TTelegramSender;
    FChat: Int64;  
    FMessageID: Int64;    
    FReSendInsteadEdit: Boolean;
    procedure EditMessageMediaStream(aPhotoStream: TStream; const aCaption: String);
    procedure SendPhotoStream(aPhotoStream: TStream; const aCaption: String);
  protected
    procedure ProcessTask(ATask: TTelegramTask); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Chat: Int64 read FChat write FChat;
  end;


  { TTelegramFace }

  TTelegramFace = class
  private
    FThread: TTelegramThread;
    function GetBot: TTelegramSender;
    function GetChat: Int64;
    function GetReSendInsteadEdit: Boolean;
    procedure SetChat(AValue: Int64);
    procedure SetReSendInsteadEdit(AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdatePicture(aPhotoStream: TStream; const aCaption: String);
    property Bot: TTelegramSender read GetBot;
    property Chat: Int64 read GetChat write SetChat;
    property ReSendInsteadEdit: Boolean read GetReSendInsteadEdit write SetReSendInsteadEdit;
  end;

  TOnAppendMessage = procedure (aMsg: TTelegramMessageObj) of object;

  { TReceiverThread }

  TReceiverThread=class(TThread)
  private
    FBot: TTelegramSender;
    FOnAppendMessage: TOnAppendMessage;
    FLPTimeout: Integer;
    procedure BotReceiveMessage({%H-}ASender: TObject; {%H-}AMessage: TTelegramMessageObj);
    procedure BotStartCommandHandler({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure SendMsgToMainThread;
  public
    constructor Create(const AToken: String);
    destructor Destroy; override;
    procedure Execute; override;
    property Bot: TTelegramSender read FBot write FBot;
    property OnAppendMessage: TOnAppendMessage read FOnAppendMessage write FOnAppendMessage;
  end;

implementation

uses
  Graphics, fpjson, eventlog
  ;

{ TReceiverThread }

procedure TReceiverThread.BotReceiveMessage(ASender: TObject; AMessage: TTelegramMessageObj);
begin
  if AMessage.Chat.ChatType=ctPrivate then
    Synchronize(@SendMsgToMainThread);
end;

procedure TReceiverThread.BotStartCommandHandler(ASender: TObject; const ACommand: String;
  AMessage: TTelegramMessageObj);
begin
  FBot.sendMessage('Hi! This is HelloWorld bot (Long Polling) developed in Lazarus');
end;

procedure TReceiverThread.SendMsgToMainThread;
begin
  if Assigned(FOnAppendMessage) then
    FOnAppendMessage(FBot.CurrentUpdate.Message);
end;

constructor TReceiverThread.Create(const AToken: String);
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  FBot:=TTelegramSender.Create(AToken);
  FBot.Logger:=TEventLog.Create(nil);
  FBot.Logger.LogType:=ltFile;
  FBot.Logger.FileName:='receiver.log';
  FBot.LogDebug:=True;
  FBot.CommandHandlers['/start']:=@BotStartCommandHandler;
  FBot.OnReceiveMessage:=@BotReceiveMessage;
  FLPTimeout:=6;
end;

destructor TReceiverThread.Destroy;
begin
  FBot.Logger.Free;
  FreeAndNil(FBot);
  inherited Destroy;
end;

procedure TReceiverThread.Execute;
begin
  while not Terminated do
    FBot.getUpdatesEx(0, FLPTimeout);
end;

{ TTelegramTask }

function TTelegramTask.GetStream: TStream;
begin
  Result:=FStream;
end;

procedure TTelegramTask.SetStream(AValue: TStream);
begin
  FStream.Clear;
  FStream.LoadFromStream(AValue);
end;

procedure TTelegramTask.Assign(Source: TPersistent);
var
  aSource: TTelegramTask;
begin
  if Source is TTelegramTask then
  begin
    aSource:=TTelegramTask(Source);
    FCaption:=aSource.Caption;
    FStream.LoadFromStream(aSource.Stream);
  end else
    inherited Assign(Source);
end;

constructor TTelegramTask.Create;
begin
  FStream:=TMemoryStream.Create;
end;

destructor TTelegramTask.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

{ TTelegramThread }

procedure TTelegramThread.EditMessageMediaStream(aPhotoStream: TStream; const aCaption: String);
var
  aMedia: TInputMediaPhoto;
begin
  aMedia:=TInputMediaPhoto.Create;
  try
    aMedia.Caption:=aCaption;
    try
      FBot.editMessageMediaStream(aPhotoStream, aMedia, FChat, FMessageID);
    except
    // do nothing
    end;
  finally
    aMedia.Free;
  end;
end;

procedure TTelegramThread.SendPhotoStream(aPhotoStream: TStream; const aCaption: String);
begin
  try
    FBot.sendPhotoStream(FChat, 'picture.bmp', aPhotoStream, aCaption);
    FMessageID:=(FBot.JSONResponse as TJSONObject).Int64s['message_id'];
  except
    FMessageID:=0;
  end;
end;

procedure TTelegramThread.ProcessTask(ATask: TTelegramTask);
begin
  try
    try
      if Count>0 then
        Exit;
      if FMessageID=0 then
        SendPhotoStream(ATask.Stream, ATask.Caption)
      else
        if FReSendInsteadEdit then
        begin
          FBot.deleteMessage(FChat, FMessageID);
          sendPhotoStream(ATask.Stream, ATask.Caption);
        end
        else
          editMessageMediaStream(ATask.Stream, ATask.Caption);
    finally
      ATask.Free;
    end;
  except        { #todo : create thread logger }
    //on E: Exception do Log(etError, E.Classname+': '+E.message);
  end;
end;

constructor TTelegramThread.Create;
begin
  inherited Create;
  FBot:=TTelegramSender.Create(EmptyStr);
  FReSendInsteadEdit:=True;
end;

destructor TTelegramThread.Destroy;
begin
  FreeAndNil(FBot);
  inherited Destroy;
end;

{ TTelegramFace }

function TTelegramFace.GetChat: Int64;
begin
  Result:=FThread.Chat;
end;

function TTelegramFace.GetBot: TTelegramSender;
begin
  Result:=FThread.FBot;
end;

function TTelegramFace.GetReSendInsteadEdit: Boolean;
begin
  Result:=FThread.FReSendInsteadEdit;
end;

procedure TTelegramFace.SetChat(AValue: Int64);
begin
  FThread.Chat:=AValue;
end;

procedure TTelegramFace.SetReSendInsteadEdit(AValue: Boolean);
begin
  FThread.FReSendInsteadEdit:=AValue;
end;

constructor TTelegramFace.Create;
begin
  FThread:=TTelegramThread.Create;
  FThread.FReSendInsteadEdit:=True;
  FThread.Start;
end;

destructor TTelegramFace.Destroy;
begin
  FThread.TerminateWorker;
  FThread.Free;
  inherited Destroy;
end;

procedure TTelegramFace.UpdatePicture(aPhotoStream: TStream; const aCaption: String);
var
  aTask: TTelegramTask;
begin
  aTask:=TTelegramTask.Create;
  try
    aTask.Caption:=aCaption;
    aTask.Stream:=aPhotoStream;
  except
    aTask.Free;
    Exit;
  end;
  FThread.PushTask(aTask);
end;

end.

