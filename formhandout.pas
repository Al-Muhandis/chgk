unit formhandout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, formcustom
  ;

type

  { TFrmHandout }

  TFrmHandout = class(TFrmMonitorControl)
    Img: TImage;
  private
  public
    procedure LoadFromFile(const aFileName: String);
  end;

var
  FrmHandout: TFrmHandout;

implementation

{$R *.lfm}

{ TFrmHandout }

procedure TFrmHandout.LoadFromFile(const aFileName: String);
begin
  if not aFileName.IsEmpty then
    Img.Picture.LoadFromFile(aFileName)
end;

end.

