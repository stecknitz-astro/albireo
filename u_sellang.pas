unit U_SelLang;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TF__SEL_LANG }

  TF__SEL_LANG = class(TForm)
    B__DE: TButton;
    B__EN: TButton;
    IMG_DE: TImage;
    IMG__EN: TImage;
    MM__INFO_DE: TMemo;
    MM__INFO_EN: TMemo;
    procedure B__DEClick(Sender: TObject);
    procedure B__ENClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    msLANG_ID: string;
  end;

var
  F__SEL_LANG: TF__SEL_LANG;

implementation

{$R *.lfm}

{ TF__SEL_LANG }

procedure TF__SEL_LANG.FormCreate(Sender: TObject);
begin
  msLANG_ID := 'DE';
  //B__DE.SetFocus;
end;

procedure TF__SEL_LANG.B__DEClick(Sender: TObject);
begin
  msLANG_ID := 'DE';
end;

procedure TF__SEL_LANG.B__ENClick(Sender: TObject);
begin
  msLANG_ID := 'EN';
end;

end.

