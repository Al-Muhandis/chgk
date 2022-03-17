unit telegram;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, tgsendertypes
  ;

type

  { TTelegramFace }

  TTelegramFace = class
  private
    FBot: TTelegramSender;
    FChat: Int64;
    FMessageID: Int64;
    FReSendInsteadEdit: Boolean;
    procedure EditMessageMediaStream(aPhotoStream: TStream; const aCaption: String);
    function GetToken: String;
    procedure SendPhotoStream(aPhotoStream: TStream; const aCaption: String);
    procedure SetToken(AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdatePicture(aPhotoStream: TStream; const aCaption: String);
    property Bot: TTelegramSender read FBot;
    property Chat: Int64 read FChat write FChat;
    property ReSendInsteadEdit: Boolean read FReSendInsteadEdit write FReSendInsteadEdit;
  end;

implementation

uses
  Graphics, fpjson
  ;



{ TTelegramFace }

procedure TTelegramFace.EditMessageMediaStream(aPhotoStream: TStream; const aCaption: String);
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

function TTelegramFace.GetToken: String;
begin
  Result:=FBot.Token;
end;

procedure TTelegramFace.SendPhotoStream(aPhotoStream: TStream; const aCaption: String);
begin
  try
    FBot.sendPhotoStream(FChat, 'picture.bmp', aPhotoStream, aCaption);
    FMessageID:=(FBot.JSONResponse as TJSONObject).Int64s['message_id'];
  except
    FMessageID:=0;
  end;
end;

procedure TTelegramFace.SetToken(AValue: String);
begin
  FBot.Token:=AValue;
end;

constructor TTelegramFace.Create;
begin
  FBot:=TTelegramSender.Create(EmptyStr);
  FReSendInsteadEdit:=True;
end;

destructor TTelegramFace.Destroy;
begin
  FBot.Free;
  inherited Destroy;
end;

procedure TTelegramFace.UpdatePicture(aPhotoStream: TStream; const aCaption: String);
begin
  if FMessageID=0 then
    SendPhotoStream(aPhotoStream, aCaption)
  else
    if FReSendInsteadEdit then
    begin
      FBot.deleteMessage(FChat, FMessageID);
      sendPhotoStream(aPhotoStream, aCaption);
    end
    else
      editMessageMediaStream(aPhotoStream, aCaption);
end;

end.

