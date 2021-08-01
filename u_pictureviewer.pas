unit U_PictureViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TF__PICTUREVIEWER }

  TF__PICTUREVIEWER = class(TForm)
    B__SAVEAS: TButton;
    BT__CLOSE: TBitBtn;
    IMG__PIC: TImage;
    P__BUTTONS: TPanel;
    SDLG: TSaveDialog;
    procedure B__SAVEASClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
    msLANG_ID: string;
  end;

var
  F__PICTUREVIEWER: TF__PICTUREVIEWER;

implementation

{$R *.lfm}

{ TF__PICTUREVIEWER }

procedure TF__PICTUREVIEWER.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #27) then Close;
end;

procedure TF__PICTUREVIEWER.B__SAVEASClick(Sender: TObject);
begin
  if(SDLG.Execute) and (SDLG.FileName <> '') then
    IMG__PIC.Picture.SaveToFile(SDLG.FileName);
end;

end.

