program AstroToolBox;

{07.09.2012

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

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazcontrols, U_ALib,
  //turbopoweripro,
  U_AstroToolBox, U_Prefs, U_AConst, U_TimeStat, U_Translation, U_AOVis,
  U_Devices, U_About, U_SetPrefsInfo, U_PictureViewer, U_AlbireoLib, U_DSGVO,
  u_astrocalc, U_STARTUP, U_ABase, U_MShower, U_ADM, U_NewCam,
  U_AstroVoids, U_ECLIPSE_EXPL, U_SelLang, U_StrmDlg, U_HorCust, u_adbinfo;

{$R *.res}

begin
  Application.Title:='Albireo - Astronomy Toolbox';
  Application.Initialize;
  Application.CreateForm(TF__ASTROTOOLBOX, F__ASTROTOOLBOX);
  Application.CreateForm(TF__ADBINFO, F__ADBINFO);
  //Application.CreateForm(TF__AOVIS, F__AOVIS);
  Application.Run;
end.

