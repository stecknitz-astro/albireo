unit U_MShower;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  Buttons;

type

  { TF__MSHOWER }

  TF__MSHOWER = class(TForm)
    IMG__MSHOWER: TImage;
    P__MS_TITLE: TPanel;
    GRD__MSHOWER: TStringGrid;
    procedure FormShow(Sender: TObject);
  private

  public
    msLANG_ID: string;

  end;

var
  F__MSHOWER: TF__MSHOWER;

implementation

{$R *.lfm}

{ TF__MSHOWER }

procedure TF__MSHOWER.FormShow(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
  begin
    GRD__MSHOWER.Cells[0,0] := 'Sternzeichen';
    GRD__MSHOWER.Cells[0,1] := 'Maximum-Tag';
    GRD__MSHOWER.Cells[0,2] := 'Meteore pro h';
    GRD__MSHOWER.Cells[0,3] := 'Geschwindigkeit km/s';
    GRD__MSHOWER.Cells[0,4] := 'Zeit von';
    GRD__MSHOWER.Cells[0,5] := 'Zeit bis';
    GRD__MSHOWER.Cells[0,6] := 'Ursprungskomet';
  end
  else
  begin
    GRD__MSHOWER.Cells[0,0] := 'Constellation';
    GRD__MSHOWER.Cells[0,1] := 'Maximum-Day';
    GRD__MSHOWER.Cells[0,2] := 'Meteors per h';
    GRD__MSHOWER.Cells[0,3] := 'Speed m/s';
    GRD__MSHOWER.Cells[0,4] := 'Time from';
    GRD__MSHOWER.Cells[0,5] := 'Time until';
    GRD__MSHOWER.Cells[0,6] := 'Source comet';
  end;

end;

end.

