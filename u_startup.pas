unit U_STARTUP;
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, U_AConst;

type

  { TF__STARTUP }

  TF__STARTUP = class(TForm)
    IMG__BACKGROUND: TImage;
    L__INFO: TLabel;
    L__PROGRESS: TLabel;
    L__PROGRESSBAR: TLabel;
    L__TITLE: TLabel;
    L__VERSION: TLabel;
    P__BOTTOM: TPanel;
    P__BACKGROUND: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  F__STARTUP: TF__STARTUP;

implementation

{$R *.lfm}

{ TF__STARTUP }

procedure TF__STARTUP.FormCreate(Sender: TObject);
begin
  Caption := 'Startup...';

  L__VERSION.Caption:= gcsAlbireoVersion;
end;

end.

