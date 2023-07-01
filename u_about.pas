unit U_About;

{
Copyright (C) 2012,2021 by Frank Szemkus
 website: https://www.stecknitz-astronomie.de
 email: albireo@stecknitz-astronomie.de

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons,
  U_AConst, U_ALib;

type

  { TF__ABOUT }

  TF__ABOUT = class(TForm)
    IMG__HEADER: TImage;
    L__AUTHOR: TLabel;
    L__AUTHOR_TITLE: TLabel;
    L__CONTACT: TLabel;
    L__DOWNLOAD: TLabel;
    L__DOWNLOAD_TITLE: TLabel;
    L__SUPPORT: TLabel;
    L__SUPPORT_TITLE1: TLabel;
    L__THANKS: TLabel;
    L__VERSION: TLabel;
    L__VERSION_TITLE: TLabel;
    L__VERSION_TITLE1: TLabel;
    L__VERSION_TITLE2: TLabel;
    L__WEB: TLabel;
    L__WEB_TITLE: TLabel;
    P__DONATE: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure L__DOWNLOADClick(Sender: TObject);
    procedure L__SUPPORTClick(Sender: TObject);
    procedure L__WEBClick(Sender: TObject);
    procedure P__DONATEClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    msLANG_ID: string;
  end;

var
  F__ABOUT: TF__ABOUT;

implementation

{$R *.lfm}

{ TF__ABOUT }

procedure TF__ABOUT.FormActivate(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
  begin
    L__AUTHOR_TITLE.Caption:='Autor:';
    L__THANKS.Caption := 'Danke, dass Sie die Albireo Astronomy Toolbox verwenden.';
    L__CONTACT.Caption:='Zu Fragen und Anregungen benutzen Sie bitte den Support-Link.';
  end;
  L__VERSION.Caption:= gcsAlbireoVersion;
  L__WEB.Caption:=gcsWebInfo;
  L__DOWNLOAD.Caption:=gcsWebAlbireoInfoDE;
  L__SUPPORT.Caption:=gcsWebAlbireoMail;

end;

procedure TF__ABOUT.L__DOWNLOADClick(Sender: TObject);
begin
 ExecOpen(gcsWebAlbireoInfoDE);
end;

procedure TF__ABOUT.L__SUPPORTClick(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
    ExecOpen('mailto:' + gcsWebAlbireoMail + '?subject=Supportanfrage&body=Bitte tragen Sie hier ihren Text ein')
  else
    ExecOpen('mailto:' + gcsWebAlbireoMail + '?subject=Support Request&body=Please put in your text here');

end;

procedure TF__ABOUT.L__WEBClick(Sender: TObject);
begin
  ExecOpen(gcsWebInfo);
end;

procedure TF__ABOUT.P__DONATEClick(Sender: TObject);
begin
  ExecOpen('https://www.tipeeestream.com/stecknitzastro/donation');
end;

end.

