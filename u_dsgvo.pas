unit U_DSGVO;

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

//{$mode delphiunicode}{$H+}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TF__DSGVO }

  TF__DSGVO = class(TForm)
    MM__EN: TMemo;
    MM__DE: TMemo;
    procedure FormShow(Sender: TObject);
  private

  public
    msLANG_ID: string;

  end;

var
  F__DSGVO: TF__DSGVO;

implementation

{$R *.lfm}

{ TF__DSGVO }


procedure TF__DSGVO.FormShow(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
  begin
    Caption := 'Datenschutz';
    MM__EN.Visible := false;
    MM__DE.Visible := true;
    MM__DE.Align := alClient;
  end
  else
  begin
    Caption := 'Privacy Statement';
    MM__DE.Visible := false;
    MM__EN.Visible := true;
    MM__EN.Align := alClient;
  end;
end;

end.

