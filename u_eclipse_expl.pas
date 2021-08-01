unit U_ECLIPSE_EXPL;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TF__ECLIPSE_EXPL }

  TF__ECLIPSE_EXPL = class(TForm)
    IMG: TImage;
    L__EARTH1: TLabel;
    L__RIGHT: TLabel;
    L__SUN: TLabel;
    L__MID: TLabel;
    P__IMG: TPanel;
    P__LEGEND: TPanel;
  private

  public

  end;

var
  F__ECLIPSE_EXPL: TF__ECLIPSE_EXPL;

implementation

{$R *.lfm}

end.

