unit U_SetPrefsInfo;

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
  StdCtrls, Buttons;

type

  { TF__SETPREFSINFO }

  TF__SETPREFSINFO = class(TForm)
    B__DE: TBitBtn;
    B__EN: TBitBtn;
    L__INFO_EN: TLabel;
    L__INFO_DE: TLabel;
    P__FIRSTSTART_HEADER: TPanel;
    procedure B__DEClick(Sender: TObject);
    procedure B__ENClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    msLANG_ID: string;
  end;

var
  F__SETPREFSINFO: TF__SETPREFSINFO;

implementation

{$R *.lfm}

{ TF__SETPREFSINFO }

procedure TF__SETPREFSINFO.B__DEClick(Sender: TObject);
begin
  msLANG_ID := 'DE';
  Close;
end;

procedure TF__SETPREFSINFO.B__ENClick(Sender: TObject);
begin
  msLANG_ID := 'EN';
  Close;
end;

end.

