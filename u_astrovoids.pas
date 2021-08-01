unit U_AstroVoids;
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

  { TF__ASTROVOIDS }

  TF__ASTROVOIDS = class(TForm)
    IMG__VOID: TImage;
    L__CONST: TLabel;
    L__DIST_TITLE: TLabel;
    L__DIST: TLabel;
    L__CONST_TITLE: TLabel;
    L__SIZE_TITLE: TLabel;
    L__SIZE: TLabel;
    L__EXPLANATION: TLabel;
    P__VOIDNAME: TPanel;
    procedure FormShow(Sender: TObject);
  private

  public
    msLANG_ID: string;

  end;

var
  F__ASTROVOIDS: TF__ASTROVOIDS;

implementation

{$R *.lfm}

{ TF__ASTROVOIDS }

procedure TF__ASTROVOIDS.FormShow(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
  begin
    L__DIST_TITLE.Caption:='Entfernung zum Voidzentrum (Mio Lj):';
    L__CONST_TITLE.Caption:='Zentrum im Sternbild:';
    L__SIZE_TITLE.Caption:='Ausdehnung des Voids (Mio Lj):';
    L__EXPLANATION.Caption:='Voids zählen zusammen mit den Galaxien-Superclustern zu den größten Strukturen im Universum. ' +
      'Sie sind definiert durch Zonen im Universum, die keine bis wenige Galaxien enthalten. ' +
      'Die Ausmaße dieser Regionen reichen von mehreren Zehnmillionen Lichtjahren bis hin zu mehrere hundert Millionen Lichtjahre.';
  end
  else
  begin
    L__DIST_TITLE.Caption:='Distance to the void center (Mio Lj):';
    L__CONST_TITLE.Caption:='Center located in constellation:';
    L__SIZE_TITLE.Caption:='Void extension (Mio Lj):';
    L__EXPLANATION.Caption:='Voids count together with galaxy-supercluster to the largest structures of the universe. ' +
      'They are defnied by regions of the universe without a galaxy or a modicum of galaxies. ' +
      'The extension of this regions ranges from tens of millions of lightyears up to tens of hundreds millions lightyears.';
  end;

end;

end.

