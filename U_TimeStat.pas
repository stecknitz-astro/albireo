unit U_TimeStat;

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Grids, Menus,
  StrUtils,
  U_AConst, U_ABase, U_ALib, U_AlbireoLib, U_Eclipse_Expl;

type

  { TF__TIMESTAT }

  TF__TIMESTAT = class(TForm)
    B__EXP_MECL: TButton;
    B__EXP_SECL: TButton;
    CHART__SM: TChart;
    CHART__SMConstantLine1: TConstantLine;
    CHART__SMLineSeries1: TLineSeries;
    CHART__SMLineSeries2: TLineSeries;
    CHART__SMLineSeries3: TLineSeries;
    ED__DWN1_A_HH: TEdit;
    ED__DWN1_A_MM: TEdit;
    ED__DWN1_C_HH: TEdit;
    ED__DWN1_C_MM: TEdit;
    ED__DWN1_N_HH: TEdit;
    ED__DWN1_N_MM: TEdit;
    ED__DWN2_A_HH: TEdit;
    ED__DWN2_A_MM: TEdit;
    ED__DWN2_C_HH: TEdit;
    ED__DWN2_C_MM: TEdit;
    ED__DWN2_N_HH: TEdit;
    ED__DWN2_N_MM: TEdit;
    ED__MOON_CUL_HH: TEdit;
    ED__MOON_CUL_MM: TEdit;
    ED__MOON_RISE_HH: TEdit;
    ED__MOON_RISE_MM: TEdit;
    ED__MOON_SET_HH: TEdit;
    ED__MOON_SET_MM: TEdit;
    ED__SUNRISE_HH: TEdit;
    ED__SUNRISE_MM: TEdit;
    ED__SUNSET_HH: TEdit;
    ED__SUNSET_MM: TEdit;
    GBX__SUNRISE: TGroupBox;
    GBX__SUNSET: TGroupBox;
    GMX__MOON: TGroupBox;
    GRD__ECLIPSE_MOON: TStringGrid;
    IMG__MOONECL: TImage;
    IMG__SUN: TImage;
    IMG__SUNECL: TImage;
    IMG__MOON: TImage;
    L__COLON: TLabel;
    L__COLON1: TLabel;
    L__COLON2: TLabel;
    L__COLON3: TLabel;
    L__COLON4: TLabel;
    L__COLON5: TLabel;
    L__COLON6: TLabel;
    L__COLON7: TLabel;
    L__COLON_MC: TLabel;
    L__COLON_MR: TLabel;
    L__COLON_MS: TLabel;
    L__DWN1_A: TLabel;
    L__DWN1_C: TLabel;
    L__DWN1_N: TLabel;
    L__DWN2_A: TLabel;
    L__DWN2_C: TLabel;
    L__DWN2_N: TLabel;
    L__DATETIME: TLabel;
    L__MOONPHASE: TLabel;
    L__MOON_CUL: TLabel;
    L__MOON_RISE: TLabel;
    L__MOON_SET: TLabel;
    L__SUNRISE: TLabel;
    L__SUNSET: TLabel;
    MENU__ECL_EXPL: TMenuItem;
    MENU_ECL_SETTIME: TMenuItem;
    PMENU__ECLIPSE: TPopupMenu;
    P__SUNANDMOON: TPanel;
    P__DYNIMG: TPanel;
    P__TIMEINT: TPanel;
    P__DCONTROL: TPanel;
    PC__SUNANDMOON: TPageControl;
    P__MOON: TPanel;
    P__SUN: TPanel;
    GRD__RISE_SET: TStringGrid;
    RB__MONTH: TRadioButton;
    RB__YEAR: TRadioButton;
    RB__MOON_CUL: TRadioButton;
    RB__MOON_RISE: TRadioButton;
    RB__MOON_SET: TRadioButton;
    GRD__ECLIPSE_SUN: TStringGrid;
    TS__ECLIPSE_MOON: TTabSheet;
    TS__ECLIPSE_SUN: TTabSheet;
    TS__CHART: TTabSheet;
    TS__TIMETABLE: TTabSheet;
    TS__CURR: TTabSheet;
    procedure B__EXP_MECLClick(Sender: TObject);
    procedure B__EXP_SECLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure MENU_ECL_SETTIMEClick(Sender: TObject);
    procedure MENU__ECL_EXPLClick(Sender: TObject);
    procedure PC__SUNANDMOONChange(Sender: TObject);
    procedure RB__MONTHChange(Sender: TObject);
    procedure RB__MOON_CULClick(Sender: TObject);
    procedure RB__MOON_RISEClick(Sender: TObject);
    procedure RB__MOON_SETClick(Sender: TObject);
    procedure RB__YEARChange(Sender: TObject);
  private
    { private declarations }
    miMoonLineIndex: Integer;

    procedure EvalTimeTable();
    procedure ShowEclipseExpl(iSunMoon: SmallInt; sType: string);

  public
    { public declarations }
    miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN: Integer;
    miDST_HH_DEF: Integer;
    msDST: string;
    mrSin_fGLat, mrCos_fGLat: Real;
    miGLat_DEG, miGLat_MIN: Integer;

    msLANG_ID: string;
    msAlbireoLocalDir: string;

    // Sun
    mdtWTime, mdtWTimeNew: TDateTime;
    mrDwnA_HH, mrDwnB_HH, mrDwn1_HH, mrDwn2_HH, mrRise_HH, mrSet_HH: Real;
    mrDwn1_HH_C, mrDwn2_HH_C, mrDwn1_HH_N, mrDwn2_HH_N: Real;

    // Moon
    mdtTimeMR: TDateTime;
    mdtTimeMS: TDateTime;
    mdtTimeCul: TDateTime;
    mrHgt_Max: Real;

    procedure PrepareSunEclipses(slEclipseList: TStringList);
    procedure PrepareMoonEclipses(slEclipseList: TStringList);
  end;

var
  F__TIMESTAT: TF__TIMESTAT;

implementation

{$R *.lfm}

{ TF__TIMESTAT }

procedure TF__TIMESTAT.ShowEclipseExpl(iSunMoon: SmallInt; sType: string);
var
  sMoon, sEarth, sImgPath, sCaption: string;
begin
  if(msLANG_ID = 'DE') then
  begin
    sEarth := 'Erde';
    sMoon := 'Mond';
  end
  else
  begin
    sEarth := 'Earth';
    sMoon := 'Moon';
  end;

  sType := Uppercase(sType);

  sCaption := '';

  F__ECLIPSE_EXPL := TF__ECLIPSE_EXPL.Create(nil);

  sImgPath := '';

  if(iSunMoon = 0) then // Solar eclipses
  begin
    if(sType = 'TOTAL') then
    begin
      if(msLANG_ID = 'DE') then
        sCaption := 'Totale Sonnenfinsternis'
      else
        sCaption := 'Total Solar Eclipse';

      sImgPath := ConvertWinPath(msAlbireoLocalDir + 'img\Eclipses\SE_TOTAL.png');
    end;
    if(sType = 'HYBRID') then
    begin
      if(msLANG_ID = 'DE') then
        sCaption := 'Hybride Sonnenfinsternis'
      else
        sCaption := 'Hybrid Solar Eclipse';

      sImgPath := ConvertWinPath(msAlbireoLocalDir + 'img\Eclipses\SE_HYBRID.png');
    end;
    if(LeftStr(sType,4) = 'PART') then
    begin
      if(msLANG_ID = 'DE') then
        sCaption := 'Partielle Sonnenfinsternis'
      else
        sCaption := 'Partial Solar Eclipse';

      sImgPath := ConvertWinPath(msAlbireoLocalDir + 'img\Eclipses\SE_PARTIAL.png');
    end;
    if(LeftStr(sType,4) = 'ANNU') or (LeftStr(sType,4) = 'RING') then
    begin
      if(msLANG_ID = 'DE') then
        sCaption := 'Ringförmige Sonnenfinsternis'
      else
        sCaption := 'Annular Solar Eclipse';

      sImgPath := ConvertWinPath(msAlbireoLocalDir + 'img\Eclipses\SE_ANNULAR.png');
    end;

    if(FileExists(sImgPath)) then
      F__ECLIPSE_EXPL.IMG.Picture.LoadFromFile(sImgPath)
    else
      F__ECLIPSE_EXPL.IMG.Picture := nil;

    F__ECLIPSE_EXPL.L__MID.Caption := sMoon;
    F__ECLIPSE_EXPL.L__MID.Font.Color := clSilver;
    F__ECLIPSE_EXPL.L__RIGHT.Caption := sEarth;
    F__ECLIPSE_EXPL.L__RIGHT.Font.Color := clBlue;
  end
  else
  begin
    if(LeftStr(sType,5) = 'TOTAL') then
    begin
      if(msLANG_ID = 'DE') then
        sCaption := 'Totale Mondfinsternis'
      else
        sCaption := 'Total Lunar Eclipse';

      sImgPath := ConvertWinPath(msAlbireoLocalDir + 'img\Eclipses\LE_TOTAL.png');
    end;
    if(LeftStr(sType,4) = 'PART') then
    begin
      if(msLANG_ID = 'DE') then
        sCaption := 'Partielle Mondfinsternis'
      else
        sCaption := 'Partial Lunar Eclipse';

      sImgPath := ConvertWinPath(msAlbireoLocalDir + 'img\Eclipses\LE_PARTIAL.png');
    end;
    if(sType = 'PENUMBRAL') or (sType = 'HALBSCHATTEN') then
    begin
      if(msLANG_ID = 'DE') then
        sCaption := 'Halbschatten-Mondfinsternis'
      else
        sCaption := 'Penumbral Lunar Eclipse';

      sImgPath := ConvertWinPath(msAlbireoLocalDir + 'img\Eclipses\LE_PENUMBRAL.png');
    end;

    if(FileExists(sImgPath)) then
      F__ECLIPSE_EXPL.IMG.Picture.LoadFromFile(sImgPath)
    else
      F__ECLIPSE_EXPL.IMG.Picture := nil;

    F__ECLIPSE_EXPL.L__MID.Caption := sEarth;
    F__ECLIPSE_EXPL.L__MID.Font.Color := clBlue;
    F__ECLIPSE_EXPL.L__RIGHT.Caption := sMoon;
    F__ECLIPSE_EXPL.L__RIGHT.Font.Color := clSilver;
  end;

  if(sCaption <> '') then
    F__ECLIPSE_EXPL.Caption := sCaption;

  F__ECLIPSE_EXPL.ShowModal;

  F__ECLIPSE_EXPL.Destroy;
end;

procedure TF__TIMESTAT.PrepareMoonEclipses(slEclipseList: TStringList);
var
  i: Integer;
  ME: TMoonEclipse;
begin
  GRD__ECLIPSE_MOON.ColWidths[5] := 300;

  if(msLANG_ID = 'DE') then
  begin
    GRD__ECLIPSE_MOON.Cells[0,0] := 'Zeit';
    GRD__ECLIPSE_MOON.Cells[1,0] := 'Typ';
    GRD__ECLIPSE_MOON.Cells[2,0] := 'Dauer HS';
    GRD__ECLIPSE_MOON.Cells[3,0] := 'Dauer Part';
    GRD__ECLIPSE_MOON.Cells[4,0] := 'Dauer Total';
    GRD__ECLIPSE_MOON.Cells[5,0] := 'Ort';
  end
  else
  begin
    GRD__ECLIPSE_MOON.Cells[0,0] := 'Time';
    GRD__ECLIPSE_MOON.Cells[1,0] := 'Type';
    GRD__ECLIPSE_MOON.Cells[2,0] := 'Duration PU';
    GRD__ECLIPSE_MOON.Cells[3,0] := 'Duration Part';
    GRD__ECLIPSE_MOON.Cells[4,0] := 'Duration Total';
    GRD__ECLIPSE_MOON.Cells[5,0] := 'Location';
  end;

  i:=0;
  while (i < slEclipseList.Count) do
  begin
    if((slEclipseList.Objects[i] as TEclipse).sEclType = 'M') then
    begin
      ME := (slEclipseList.Objects[i] as TMoonEclipse);
      GRD__ECLIPSE_MOON.Cells[0,GRD__ECLIPSE_MOON.RowCount-1] := FormatDateTime('dd.mm.yy hh:mm',ME.dtDateTime);

      GRD__ECLIPSE_MOON.Cells[1,GRD__ECLIPSE_MOON.RowCount-1] := TranslateEclStr(msLANG_ID,ME.sType,'M','TYPE');
      if(ME.dtDurationPE > 0) then
        GRD__ECLIPSE_MOON.Cells[2,GRD__ECLIPSE_MOON.RowCount-1] := FormatDateTime('hh:mm',ME.dtDurationPE);
      if(ME.dtDurationPA > 0) then
        GRD__ECLIPSE_MOON.Cells[3,GRD__ECLIPSE_MOON.RowCount-1] := FormatDateTime('hh:mm',ME.dtDurationPA);
      if(ME.dtDurationT > 0) then
        GRD__ECLIPSE_MOON.Cells[4,GRD__ECLIPSE_MOON.RowCount-1] := FormatDateTime('hh:mm',ME.dtDurationT);

      GRD__ECLIPSE_MOON.Cells[5,GRD__ECLIPSE_MOON.RowCount-1] := TranslateEclStr(msLANG_ID,ME.sLoc,'M','LOC');

      GRD__ECLIPSE_MOON.RowCount := GRD__ECLIPSE_MOON.RowCount + 1;
    end;
    Inc(i);
  end;

  GRD__ECLIPSE_MOON.RowCount := GRD__ECLIPSE_MOON.RowCount - 1;

    (*
    Inc(iRow);
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,1,31) + EncodeTime(13,31,0,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,17,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,23,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,16,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Asien, Australien, Pazifik, westl. Nordamerika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,7,27) + EncodeTime(20,22,54,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (z)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(6,14,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,54,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,43,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Südamerika, Europa, Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,1,21) + EncodeTime(5,13,27,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,12,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,17,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,2,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Zentralpazifik, Nord- und Südamerika, Europa, Afrika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,7,16) + EncodeTime(21,31,55,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partiell';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,34,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(2,58,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Südamerika, Europa, Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,1,10) + EncodeTime(19,11,11,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,5,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europa, Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,6,5) + EncodeTime(19,26,14,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(3,18,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europa, Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,7,5) + EncodeTime(4,31,12,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(2,45,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Südwesteuropa, Afrika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,11,30) + EncodeTime(9,44,01,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,21,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Asien, Australien, Pazifik, Nord- und Südamerika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,5,26) + EncodeTime(11,19,53,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,2,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,7,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(0,14,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Ostasien, Australien, Pazifik, Nord- und Südamerika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,11,19) + EncodeTime(9,4,6,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partiell';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(6,2,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,28,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Nordeuropa, Ostasien, Australien, Pazifik';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,5,16) + EncodeTime(4,12,42,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (z)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,19,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,27,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,25,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Europa, Afrika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,11,8) + EncodeTime(11,0,22,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (z)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,54,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,40,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,25,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Asien, Australien, Pazifik, Nord- und Südamerika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,5,5) + EncodeTime(17,24,5,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,18,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,10,28) + EncodeTime(20,15,18,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partiell';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,25,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(1,17,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Östl. Nord- und Südamerika, Europa, Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,3,25) + EncodeTime(7,13,59,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,25,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,9,18) + EncodeTime(2,45,25,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partiell';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,6,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(1,3,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Europa, Afrika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,3,14) + EncodeTime(6,59,56,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(6,3,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,38,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,5,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Pazifik, Nord- und Südamerika, Westeuropa, Westafrika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,9,7) + EncodeTime(18,12,58,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,27,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,29,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,22,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europa, Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,3,3) + EncodeTime(11,34,52,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,39,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,27,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(0,58,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Ostasien, Australien, Pazifik, Nord- und Südamerika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,8,28) + EncodeTime(4,14,4,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partiell';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,38,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,18,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Ostpazifik, Nord- und Südamerika, Europa, Afrika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,2,20) + EncodeTime(23,14,6,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,1,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Europa, Afrika, Asien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,7,18) + EncodeTime(16,4,9,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(0,12,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Ostafrika, Asien, Australien, Pazifik';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,8,17) + EncodeTime(7,14,59,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Halbschatten';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(3,39,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Pazifik, Nord- und Südamerika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,1,12) + EncodeTime(4,14,13,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partiell';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,11,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(0,56,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Europa, Afrika';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,7,6) + EncodeTime(18,20,57,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partiell';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,11,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(2,22,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europa, Afrika, Asien, Australien';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,12,31) + EncodeTime(16,53,15,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,36,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,29,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,11,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europa, Afrika, Asien, Australien, Pazifik';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,6,26) + EncodeTime(3,23,22,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (z)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,35,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,40,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,42,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Europa, Afrika, Naher Osten';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,12,20) + EncodeTime(22,43,12,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,58,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,33,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(0,54,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Nord- und Südamerika, Europa, Afrika, Asien';

  end
  else
  begin
    GRD__ECLIPSE_MOON.Cells[0,iRow] := 'Time';
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Type';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := 'Duration PU';
    GRD__ECLIPSE_MOON.Cells[3,iRow] := 'Duration Part';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := 'Duration Total';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Location';

    Inc(iRow);
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,1,31) + EncodeTime(13,31,0,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,17,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,23,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,16,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Asia, Australia, Pacific, Northwest America';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,7,27) + EncodeTime(20,22,54,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (c)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(6,14,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,54,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,43,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Southern America, Europe, Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,1,21) + EncodeTime(5,13,27,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,12,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,17,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,2,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Central Pacific, Northern- / Southern America, Europe, Africa';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,7,16) + EncodeTime(21,31,55,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partial';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,34,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(2,58,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Southern America, Europe, Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,1,10) + EncodeTime(19,11,11,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,5,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europe, Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,6,5) + EncodeTime(19,26,14,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(3,18,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europe, Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,7,5) + EncodeTime(4,31,12,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(2,45,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- / Southern America, Southwest Europe, Africa';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,11,30) + EncodeTime(9,44,01,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,21,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Asia, Australia, Pacific, Northern- / Southern America';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,5,26) + EncodeTime(11,19,53,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,2,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,7,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(0,14,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'East Asia, Australia, Pacific, Northern- /Southern America';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,11,19) + EncodeTime(9,4,6,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partial';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(6,2,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,28,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- / Southern America, Northern Europe, East Asia, Australia, Pacific';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,5,16) + EncodeTime(4,12,42,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (c)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,19,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,27,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,25,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- / Southern America, Europe, Africa';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,11,8) + EncodeTime(11,0,22,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (c)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,54,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,40,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,25,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Asia, Australia, Pacific, Northern- / Southern America';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,5,5) + EncodeTime(17,24,5,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,18,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,10,28) + EncodeTime(20,15,18,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partial';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,25,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(1,17,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Eastern Northern- / Southern America, Europe, Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,3,25) + EncodeTime(7,13,59,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,25,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- and Southern America';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,9,18) + EncodeTime(2,45,25,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partial';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,6,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(1,3,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- and Southern America, Europe, Africa';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,3,14) + EncodeTime(6,59,56,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(6,3,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,38,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,5,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Pacific, Northern- and Southern America, Western Europe, Western Africa';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,9,7) + EncodeTime(18,12,58,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,27,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,29,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,22,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europe, Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,3,3) + EncodeTime(11,34,52,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,39,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,27,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(0,58,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Eastern Asia, Australia, Pacific, Northern- and Southern America';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,8,28) + EncodeTime(4,14,4,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partial';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,38,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,18,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Eastern Pacific, Northern- and Southern America, Europe, Africa';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,2,20) + EncodeTime(23,14,6,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,1,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- and Southern America, Europe, Africa, Asia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,7,18) + EncodeTime(16,4,9,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(0,12,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Eastern Africa, Asia, Australia, Pacific';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,8,17) + EncodeTime(7,14,59,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Penumbral';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(3,39,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Pacific, Northern- and Southern America';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,1,12) + EncodeTime(4,14,13,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partial';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(4,11,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(0,56,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- and Southern America, Europe, Africa';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,7,6) + EncodeTime(18,20,57,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Partial';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,11,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(2,22,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := '-';
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europe, Africa, Asia, Australia';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,12,31) + EncodeTime(16,53,15,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,36,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,29,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,11,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Europe, Africa, Asia, Australia, Pacific';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,6,26) + EncodeTime(3,23,22,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total (c)';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,35,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,40,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,42,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- and Southern America, Europe, Africa, Middle East';

    GRD__ECLIPSE_MOON.RowCount :=  GRD__ECLIPSE_MOON.RowCount + 1; iRow := GRD__ECLIPSE_MOON.RowCount-1;
    GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,12,20) + EncodeTime(22,43,12,0));
    GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
    GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,58,0,0));
    GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,33,0,0));
    GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(0,54,0,0));
    GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Northern- and Southern America, Europe, Africa, Asia';

  end;
  *)
end;
procedure TF__TIMESTAT.PrepareSunEclipses(slEclipseList: TStringList);
var
  i: Integer;
  SE: TSunEclipse;
begin
  GRD__ECLIPSE_SUN.ColWidths[4] := 300;

  if(msLANG_ID = 'DE') then
  begin
    GRD__ECLIPSE_SUN.Cells[0,0] := 'Zeit';
    GRD__ECLIPSE_SUN.Cells[1,0] := 'Dauer';
    GRD__ECLIPSE_SUN.Cells[2,0] := 'Typ';
    GRD__ECLIPSE_SUN.Cells[3,0] := 'Größe';
    GRD__ECLIPSE_SUN.Cells[4,0] := 'Ort';
  end
  else
  begin
    GRD__ECLIPSE_SUN.Cells[0,0] := 'Time';
    GRD__ECLIPSE_SUN.Cells[1,0] := 'Duration';
    GRD__ECLIPSE_SUN.Cells[2,0] := 'Type';
    GRD__ECLIPSE_SUN.Cells[3,0] := 'Magnitude';
    GRD__ECLIPSE_SUN.Cells[4,0] := 'Location';
  end;

  i:=0;
  while (i < slEclipseList.Count) do
  begin
    if((slEclipseList.Objects[i] as TEclipse).sEclType = 'S') then
    begin
      SE := (slEclipseList.Objects[i] as TSunEclipse);

      GRD__ECLIPSE_SUN.Cells[0,GRD__ECLIPSE_SUN.RowCount-1] := FormatDateTime('dd.mm.yy hh:mm',SE.dtDateTime);

      if(SE.dtDurationT > 0) then
        GRD__ECLIPSE_SUN.Cells[1,GRD__ECLIPSE_SUN.RowCount-1] := FormatDateTime('hh:mm:ss',SE.dtDurationT);

      GRD__ECLIPSE_SUN.Cells[2,GRD__ECLIPSE_SUN.RowCount-1] := TranslateEclStr(msLANG_ID,SE.sType,'S','TYPE');

      GRD__ECLIPSE_SUN.Cells[3,GRD__ECLIPSE_SUN.RowCount-1] := FloatToStrF(SE.rSize,ffFixed,8,3);

      GRD__ECLIPSE_SUN.Cells[4,GRD__ECLIPSE_SUN.RowCount-1] := TranslateEclStr(msLANG_ID,SE.sLoc,'S','LOC');

      GRD__ECLIPSE_SUN.RowCount := GRD__ECLIPSE_SUN.RowCount + 1;
    end;
    Inc(i);
  end;

  GRD__ECLIPSE_SUN.RowCount := GRD__ECLIPSE_SUN.RowCount - 1;

  (*
  iRow := 0;
  if(msLANG_ID = 'DE') then
  begin
    GRD__ECLIPSE_SUN.Cells[0,iRow] := 'Zeit';
    GRD__ECLIPSE_SUN.Cells[1,iRow] := 'Dauer';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Typ';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := 'Größe';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Ort';

    Inc(iRow);
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,2,15) + EncodeTime(20,52,33,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.599';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Antarktis, südliches Südamerika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,7,13) + EncodeTime(3,2,16,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.337';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südaustralien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,8,11) + EncodeTime(9,27,28,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.737';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nordeuropa, nordöstliches Asien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,1,6) + EncodeTime(1,42,38,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.715';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nordöstliches Asien, Nordpazifik';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,7,2) + EncodeTime(19,24,7,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,4,33,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.046';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südlicher Pazifik, Südamerika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,12,26) + EncodeTime(5,18,53,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,3,40,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.97';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Asien, Australien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,6,21) + EncodeTime(6,41,15,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,0,38,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.994';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Afrika, südöstliches Europa, Asien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,12,14) + EncodeTime(6,14,39,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,2,10,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.025';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Pazifik, südliches Südamerika, Antarktis';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,06,10) + EncodeTime(10,43,7,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,3,51,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.943';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nördliches Nordamerika, Europa, Asien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,12,4) + EncodeTime(7,34,38,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,1,54,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.037';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Antarktis, südliches Afrika, Südatlantik';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,4,30) + EncodeTime(20,42,36,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.640';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südöstlicher Pazifik, südliches Südamerika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,10,25) + EncodeTime(11,1,20,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.862';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Europa, nordöstliches Afrika, Naher Osten, westliches Asien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,4,20) + EncodeTime(4,17,56,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,1,16,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Hybrid';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.013';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südostasien, Australien, Philippinen, Neuseeland';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,10,14) + EncodeTime(18,46,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,5,17,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.952';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nordamerika, Mittelamerika, Südamerika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,04,8) + EncodeTime(18,18,29,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,4,28,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.057';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nordamerika, Mittelamerika, nordwestlichstes Europa';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,10,2) + EncodeTime(18,46,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,7,25,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.933';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Pazifik, südliches Südamerika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,3,29) + EncodeTime(10,48,36,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.933';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nordwestliches Afrika, Europa, Nordrussland';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,9,21) + EncodeTime(19,43,4,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.855';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südlicher Pazifik, Neuseeland, Antarktis';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,2,17) + EncodeTime(19,43,4,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,2,20,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.963';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südargentinien und Chile, südliches Afrika, Antarktis';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,8,12) + EncodeTime(17,47,6,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,2,18,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.039';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nördliches Nordamerika, Westafrika, Europa (östl. Portugal)';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,2,6) + EncodeTime(16,0,48,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,7,51,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.928';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südamerika, Antarktis, Westafrika, südliches Afrika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,8,2) + EncodeTime(10,7,50,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,6,23,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.079';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Afrika, Europa (Spanien), Naher Osten, westliches Asien, Südasien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,1,26) + EncodeTime(15,8,59,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,10,27,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.921';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Östliches Nordamerika, Mittel- und Südamerika, Westeuropa, nordwestliches Afrika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,7,22) + EncodeTime(2,56,40,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,5,10,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.056';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südostasien, Australien, Neuseeland';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,1,14) + EncodeTime(17,13,48,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.871';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Nordamerika, Mittelamerika';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,6,12) + EncodeTime(4,6,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.458';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Arktis, Nordeuropa, Alaska, Nordasien, Nordkanada';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,7,11) + EncodeTime(15,37,19,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.230';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südchile, Südargentinien';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,12,5) + EncodeTime(15,3,58,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.891';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südargentinien, Südchile, Antarktis';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2030,6,1) + EncodeTime(6,29,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,5,21,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Ringförmig';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.944';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Europa (Griechenland), Nordafrika, Naher Osten, Asien, Arktis, Alaska';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2030,11,25) + EncodeTime(6,51,37,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,3,44,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.047';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Südliches Afrika, südlicher Indischer Ozean, Südostasien, Australien, Antarktis';

  end
  else
  begin
    GRD__ECLIPSE_SUN.Cells[0,iRow] := 'Time';
    GRD__ECLIPSE_SUN.Cells[1,iRow] := 'Duration';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Type';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := 'Magnitude';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Location';

    Inc(iRow);
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,2,15) + EncodeTime(20,52,33,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.599';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Antarctica, Southern South America ';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,7,13) + EncodeTime(3,2,16,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.337';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'South Australia, Victoria, Tasmania, Indian Ocean, Budd Coast';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,8,11) + EncodeTime(9,27,28,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.737';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Northeastern Canada, Greenland, Iceland, Arctic Ocean, Scandinavia, northern British Isles, Russia, northern Asia';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,1,6) + EncodeTime(1,42,38,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.715';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Northeastern Asia, Southwestern Alaska, Aleutian Islands';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,7,2) + EncodeTime(19,24,7,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,4,33,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.046';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Pitcairn Islands, central Argentina and Chile, Tuamotu Archipelago';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2019,12,26) + EncodeTime(5,18,53,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,3,40,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.97';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Saudi Arabia, Bahrain, Qatar, United Arab Emirates, Oman, Lakshadweep, Southern India, Sri Lanka, northern Sumatra, southern Malaysia, Singapore, Borneo, central Indonesia, Palau, Micronesia, Guam';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,6,21) + EncodeTime(6,41,15,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,0,38,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.994';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Democratic Republic of the Congo, Sudan, Ethiopia, Eritrea, Yemen, Empty Quarter, Oman, southern Pakistan, northern India, New Delhi, Tibet, southern China, Chongqing, Taiwan';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2020,12,14) + EncodeTime(6,14,39,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,2,10,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.025';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Southern Chile and Argentina, Kiribati, Polynesia';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,06,10) + EncodeTime(10,43,7,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,3,51,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.943';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Northern Canada, Greenland, Russia';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2021,12,4) + EncodeTime(7,34,38,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,1,54,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.037';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Antarctica';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,4,30) + EncodeTime(20,42,36,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.640';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Southeast Pacific, Southern South America';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2022,10,25) + EncodeTime(11,1,20,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.862';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Europe, northeast Africa, Mid East, West Asia';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,4,20) + EncodeTime(4,17,56,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,1,16,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Hybrid';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.013';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Indonesia, Australia, Papua New Guinea';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2023,10,14) + EncodeTime(18,46,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,5,17,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.952';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Western United States, Central America, Colombia, Brazil';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,04,8) + EncodeTime(18,18,29,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,4,28,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.057';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Mexico, central and northeastern United States, East Canada';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2024,10,2) + EncodeTime(18,46,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,7,25,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.933';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Southern Chile, Southern Argentina';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,3,29) + EncodeTime(10,48,36,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.938';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Northwest Africa, Europe, northern Russia';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2025,9,21) + EncodeTime(19,43,4,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.855';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'South Pacific, New Zealand, Antarctica';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,2,17) + EncodeTime(19,43,4,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,2,20,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.963';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Antarctica';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2026,8,12) + EncodeTime(17,47,6,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,2,18,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.039';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Arctic, Greenland, Iceland, Spain, Northeastern Portugal';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,2,6) + EncodeTime(16,0,48,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,7,51,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.928';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Chile, Argentina, Atlantic';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2027,8,2) + EncodeTime(10,7,50,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,6,23,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.079';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Morocco, Spain, Algeria, Tunisia, Libya, Egypt, Saudi Arabia, Yemen, Somalia';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,1,26) + EncodeTime(15,8,59,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,10,27,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.921';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Ecuador, Peru, Brazil, Suriname, Spain, Portugal';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2028,7,22) + EncodeTime(2,56,40,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,5,10,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.056';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Australia, New Zealand';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,1,14) + EncodeTime(17,13,48,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.871';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'North America, Central America';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,6,12) + EncodeTime(4,6,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.458';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Arctic, Scandinavia, Alaska, northern Asia, northern Canada';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,7,11) + EncodeTime(15,37,19,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.230';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Southern Chile, Southern Argentina';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2029,12,5) + EncodeTime(15,3,58,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partial';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.891';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Southern Argentina, Southern Chile, Antarctica';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2030,6,1) + EncodeTime(6,29,13,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,5,21,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Annular';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.944';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Algeria, Tunisia, Greece, Turkey, Russia, northern China, Japan';

    GRD__ECLIPSE_SUN.RowCount :=  GRD__ECLIPSE_SUN.RowCount + 1; iRow := GRD__ECLIPSE_SUN.RowCount-1;
    GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2030,11,25) + EncodeTime(6,51,37,0));
    GRD__ECLIPSE_SUN.Cells[1,iRow] := FormatDateTime('hh:mm:ss',EncodeTime(0,3,44,0));
    GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Total';
    GRD__ECLIPSE_SUN.Cells[3,iRow] := '1.047';
    GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Botswana, South Africa, Australia';

  end;
  *)

end;

procedure TF__TIMESTAT.EvalTimeTable();
var
  i,iYear,iMonth,iDay,iYearSel,iMonthSel,iDaySel: Word;
  iStartDay, iEndDay: Integer;
  dtTimeMR,dtTimeMS,dtTimeCul: TDateTime;
  rHgt_Max, rRise_HH, rSet_HH: Real;
  iRow, iNowRow: Integer;
  iDayOfYear: Integer;
  iDST_HH_Sel: Integer;
  sDST: string;
  rNight, rVal, rValPrev: Real;
  iFullMoon,iNewMoon,iBlueMoon: Word;
  sMoonInfo: string;
  bWriteBM, bBMLastFM: Boolean;
begin
  rVal := -1; rValPrev := -1;
  iFullMoon := 0; iNewMoon := 0; iBlueMoon := 0;
  sMoonInfo := '';
  bWriteBM := true;
  bBMLastFM := false;

  DecodeDate(mdtWTime,iYear,iMonth,iDay);

  if(RB__MONTH.Checked) then
  begin
    iEndDay := DaysOfMonth(mdtWTime) - 1;
    iStartDay := Trunc(EncodeDate(iYear,iMonth,1));
  end
  else
  begin
    iEndDay := Trunc(EncodeDate(iYear,12,31)) - Trunc(EncodeDate(iYear,1,1));
    iStartDay := Trunc(EncodeDate(iYear,1,1));
  end;

  iNowRow := Trunc(mdtWTime) - iStartDay + 1;

  for i:=0 to CHART__SM.SeriesCount-1 do // except index 3. This is the TODAY mark
  begin
    if(i <> 3) then
      (CHART__SM.Series[i] as TLineSeries).Clear;
  end;

  if(CHART__SM.SeriesCount < miMoonLineIndex+1) then
  begin
    CHART__SM.AddSeries(TLineSeries.Create(nil));
    (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Clear;
    (CHART__SM.Series[miMoonLineIndex] as TLineSeries).LinePen.Color:=clYellow;
    (CHART__SM.Series[miMoonLineIndex] as TLineSeries).LinePen.Width:=3;
  end;

  if(msLANG_ID = 'DE') then
  begin
    if(RB__MOON_RISE.Checked) then
      (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Title := 'Mondaufgang';

    if(RB__MOON_CUL.Checked) then
      (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Title := 'Mondkulmination';

    if(RB__MOON_SET.Checked) then
      (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Title := 'Monduntergang';
  end
  else
  begin
    if(RB__MOON_RISE.Checked) then
      (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Title := 'Moonrise';

    if(RB__MOON_CUL.Checked) then
      (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Title := 'Moon culmination';

    if(RB__MOON_SET.Checked) then
      (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Title := 'Moonset';
  end;

  GRD__RISE_SET.RowCount:=2;
  if(msLANG_ID = 'DE') then
  begin
    GRD__RISE_SET.Cells[0,0] := 'Tag';
    GRD__RISE_SET.Cells[1,0] := 'Sonnenaufgang';
    GRD__RISE_SET.Cells[2,0] := 'Kulmination';
    GRD__RISE_SET.Cells[3,0] := 'Sonnenuntergang';
    GRD__RISE_SET.Cells[4,0] := 'Mondaufgang';
    GRD__RISE_SET.Cells[5,0] := 'Mondkulmination';
    GRD__RISE_SET.Cells[6,0] := 'Monduntergang';
  end
  else
  begin
    GRD__RISE_SET.Cells[0,0] := 'Day';
    GRD__RISE_SET.Cells[1,0] := 'Sunrise';
    GRD__RISE_SET.Cells[2,0] := 'Culmination';
    GRD__RISE_SET.Cells[3,0] := 'Sunset';
    GRD__RISE_SET.Cells[4,0] := 'Moonrise';
    GRD__RISE_SET.Cells[5,0] := 'Moon Culmination';
    GRD__RISE_SET.Cells[6,0] := 'Moonset';
  end;

  iRow := 0;

  for i:=0 to iEndDay do
  begin
    iDay := iStartDay + i;

    DecodeDate(iDay,iYearSel,iMonthSel,iDaySel);

    if(iDaySel = 1) then
      CalcMoonOfMonth(iDay,iFullMoon,iNewMoon,iBlueMoon);

    if(bBMLastFM) then
    begin
      sMoonInfo := '';
      bBMLastFM := false;
    end
    else
    begin
      if(msLANG_ID = 'DE') then
      begin
        if (iDaySel = iFullMoon) then begin sMoonInfo := 'VM'; bWriteBM := true;  end
        else if (iDaySel =  iNewMoon) then sMoonInfo := 'NM'
        else if (iDaySel =  iBlueMoon) then
        begin
          if(bWriteBM) then
          begin
            sMoonInfo := 'BM';
            bWriteBM := false;
          end
          else
            sMoonInfo := 'VM';

          bBMLastFM := true;
        end
        else sMoonInfo := '';
      end
      else
      begin
        if (iDaySel = iFullMoon) then begin sMoonInfo := 'FM'; bWriteBM := true;  end
        else if (iDaySel =  iNewMoon) then sMoonInfo := 'NM'
        else if (iDaySel =  iBlueMoon) then
        begin
          if(bWriteBM) then
          begin
            sMoonInfo := 'BM';
            bWriteBM := false;
          end
          else
            sMoonInfo := 'FM';

          bBMLastFM := true;
        end
        else sMoonInfo := '';
      end;
    end;

    dtTimeMR:=0; dtTimeMS:=0; dtTimeCul:=0; rHgt_Max:=0;
    rRise_HH:=0; rSet_HH:=0;

    CalcMoonRiseAndMoonSet(iDay,
      miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,
      mrSin_fGLat, mrCos_fGLat,
      dtTimeMR,dtTimeMS,dtTimeCul,rHgt_Max);

    iDayOfYear := DayOfYear(iDay);
    CalcSunRiseAndSet(
      miGLng_DEG, miGLng_MIN,
      miGLat_DEG, miGLat_MIN,
      miUTC_HH,
      iDayOfYear,miDST_HH,-50,rRise_HH, rSet_HH);

    Inc(iRow);
    if(iRow > 1) then
      GRD__RISE_SET.RowCount := GRD__RISE_SET.RowCount + 1;

    iDST_HH_Sel := GetDST(iDay,msDST,miDST_HH_DEF); // Check DST of iterated day

    if(miDST_HH > 0) then
      sDST := msDST
    else
      sDST := '';

    // DST Time correttions while active DST
    if(miDST_HH > 0) and (iDST_HH_Sel = 0) then
    begin
      // Iterated day is non-DST
      sDST := '';
      rRise_HH := rRise_HH - miDST_HH_DEF;
      rSet_HH := rSet_HH - miDST_HH_DEF;
      dtTimeMR := dtTimeMR - miDST_HH_DEF/24;
      dtTimeCul := dtTimeCul - miDST_HH_DEF/24;
      dtTimeMS := dtTimeMS - miDST_HH_DEF/24;
    end;

    // DST Time correttions while inactive DST
    if(miDST_HH = 0) and (iDST_HH_SEL > 0) then
    begin
      // Iterated day is DST
      sDST := msDST;
      rRise_HH := rRise_HH + miDST_HH_DEF;
      rSet_HH := rSet_HH + miDST_HH_DEF;
      dtTimeMR := dtTimeMR + miDST_HH_DEF/24;
      dtTimeCul := dtTimeCul + miDST_HH_DEF/24;
      dtTimeMS := dtTimeMS + miDST_HH_DEF/24;
    end;

    GRD__RISE_SET.Cells[0,iRow] := Trim(FormatDateTime('ddd',iDay) + ' ' + DateToStr(iDay) + ' ' + sDST);
    GRD__RISE_SET.Cells[1,iRow] := HourToHHMMStr(rRise_HH);
    GRD__RISE_SET.Cells[2,iRow] := HourToHHMMStr(rRise_HH + 0.5*(rSet_HH - rRise_HH));
    GRD__RISE_SET.Cells[3,iRow] := HourToHHMMStr(rSet_HH);
    GRD__RISE_SET.Cells[4,iRow] := FormatDateTime('hh:mm',dtTimeMR);// TimeToStr(dtTimeMR);
    if(sMoonInfo = '') then
      GRD__RISE_SET.Cells[5,iRow] := FormatDateTime('hh:mm',dtTimeCul)
    else
      GRD__RISE_SET.Cells[5,iRow] := FormatDateTime('hh:mm',dtTimeCul) + ' ' + sMoonInfo;

    GRD__RISE_SET.Cells[6,iRow] := FormatDateTime('hh:mm',dtTimeMS);

    rNight := (rRise_HH - 2) + (24 - rSet_HH - 2);
    if(rNight < 0) then rNight := 0;

    (CHART__SM.Series[0] as TLineSeries).AddXY(i+1,rRise_HH);
    (CHART__SM.Series[1] as TLineSeries).AddXY(i+1,rSet_HH);
    (CHART__SM.Series[2] as TLineSeries).AddXY(i+1,rNight);

    if(RB__MOON_RISE.Checked) then
      rVal := (dtTimeMR-Trunc(dtTimeMR))*24.0
    else if(RB__MOON_CUL.Checked) then
      rVal := (dtTimeCul-Trunc(dtTimeCul))*24.0
    else
      rVal := (dtTimeMS-Trunc(dtTimeMS))*24.0;

    // Change to next lineseries of time is switched from 24:00 to 00:00
    if(rValPrev > -1) and (abs(rVal - rValPrev) > 10) then
    begin
      Inc(miMoonLineIndex);
      if(CHART__SM.SeriesCount < miMoonLineIndex+1) then
      begin
        CHART__SM.AddSeries(TLineSeries.Create(nil));
        (CHART__SM.Series[miMoonLineIndex] as TLineSeries).Clear;
        (CHART__SM.Series[miMoonLineIndex] as TLineSeries).LinePen.Color:=clYellow;
        (CHART__SM.Series[miMoonLineIndex] as TLineSeries).LinePen.Width:=3;
        (CHART__SM.Series[miMoonLineIndex] as TLineSeries).ShowInLegend:=false;
      end;

    end;

    (CHART__SM.Series[miMoonLineIndex] as TLineSeries).AddXY(i+1,rVal);

    rValPrev := rVal;

  end;

  GRD__RISE_SET.Row := iNowRow;

end;

procedure TF__TIMESTAT.FormShow(Sender: TObject);
var
  iHH, iMM, iSS, iMS: Word;
  fMoonPhaseProz: Real;
  sCaption: string;
  //sPicFile: string;
begin
  L__DATETIME.Caption := DateToStr(mdtWTime);
  TS__CURR.Caption:=DateToStr(mdtWTime);

  PC__SUNANDMOON.ActivePageIndex:=0;

  sCaption := FormatDateTime('mmmm yyyy',mdtWTime);
  sCaption := SetDE_ENMonthNames(sCaption,msLANG_ID);
  TS__TIMETABLE.Caption:= sCaption;

  CHART__SM.Title.Text.Clear;
  CHART__SM.Title.Text.Add(sCaption);//  FormatDateTime('mmmm',mdtWTime));

  (CHART__SM.Series[3] as TConstantLine).Position:=StrToInt(FormatDateTime('d',mdtWTime));

  if(msLANG_ID = 'DE') then
  begin
    (CHART__SM.Series[0] as TLineSeries).Title := 'Sonnenaufgang';
    (CHART__SM.Series[1] as TLineSeries).Title := 'Sonnenuntergang';
    (CHART__SM.Series[2] as TLineSeries).Title := 'Dunkle Nachtstunden';
    (CHART__SM.Series[3] as TConstantLine).Title := FormatDateTime('d.m.yy',mdtWTime);
  end
  else
  begin
    (CHART__SM.Series[0] as TLineSeries).Title := 'Sunrise';
    (CHART__SM.Series[1] as TLineSeries).Title := 'Sunset';
    (CHART__SM.Series[2] as TLineSeries).Title := 'Dark Nighthours';
    (CHART__SM.Series[3] as TConstantLine).Title := FormatDateTime('d.m.yy',mdtWTime);
  end;

  miMoonLineIndex := CHART__SM.SeriesCount;

  GRD__RISE_SET.ColWidths[0] := 130;

  if(mrDwn1_HH >= 24) then mrDwn1_HH := mrDwn1_HH - 24;
  if(mrDwn1_HH_N >= 24) then mrDwn1_HH_N := mrDwn1_HH_N - 24;
  if(mrDwn1_HH_C >= 24) then mrDwn1_HH_C := mrDwn1_HH_C - 24;
  if(mrRise_HH >= 24) then mrRise_HH := mrRise_HH - 24;
  if(mrDwn2_HH >= 24) then mrDwn2_HH := mrDwn2_HH - 24;
  if(mrDwn2_HH_N >= 24) then mrDwn2_HH_N := mrDwn2_HH_N - 24;
  if(mrDwn2_HH_C >= 24) then mrDwn2_HH_C := mrDwn2_HH_C - 24;
  if(mrSet_HH >= 24) then mrSet_HH := mrSet_HH - 24;

  // Sun
  if(mrDwn1_HH >= 0) then
  begin
    ED__DWN1_A_HH.Text := format('%.2d',[Trunc(mrDwn1_HH)]);
    ED__DWN1_A_MM.Text := format('%.2d',[Trunc((mrDwn1_HH-Trunc(mrDwn1_HH))*60)]);
  end;

  if(mrDwn1_HH_N >= 0) then
  begin
    ED__DWN1_N_HH.Text := format('%.2d',[Trunc(mrDwn1_HH_N)]);
    ED__DWN1_N_MM.Text := format('%.2d',[Trunc((mrDwn1_HH_N-Trunc(mrDwn1_HH_N))*60)]);
  end;

  if(mrDwn1_HH_C >= 0) then
  begin
    ED__DWN1_C_HH.Text := format('%.2d',[Trunc(mrDwn1_HH_C)]);
    ED__DWN1_C_MM.Text := format('%.2d',[Trunc((mrDwn1_HH_C-Trunc(mrDwn1_HH_C))*60)]);
  end;

  if(mrRise_HH >= 0) then
  begin
    ED__SUNRISE_HH.Text := format('%.2d',[Trunc(mrRise_HH)]);
    ED__SUNRISE_MM.Text := format('%.2d',[Trunc((mrRise_HH-Trunc(mrRise_HH))*60)]);
  end;

  if(mrDwn2_HH >= 0) then
  begin
    ED__DWN2_A_HH.Text := format('%.2d',[Trunc(mrDwn2_HH)]);
    ED__DWN2_A_MM.Text := format('%.2d',[Trunc((mrDwn2_HH-Trunc(mrDwn2_HH))*60)]);
  end;

  if(mrDwn2_HH_N >= 0) then
  begin
    ED__DWN2_N_HH.Text := format('%.2d',[Trunc(mrDwn2_HH_N)]);
    ED__DWN2_N_MM.Text := format('%.2d',[Trunc((mrDwn2_HH_N-Trunc(mrDwn2_HH_N))*60)]);
  end;

  if(mrDwn2_HH_C >= 0) then
  begin
    ED__DWN2_C_HH.Text := format('%.2d',[Trunc(mrDwn2_HH_C)]);
    ED__DWN2_C_MM.Text := format('%.2d',[Trunc((mrDwn2_HH_C-Trunc(mrDwn2_HH_C))*60)]);
  end;

  if(mrSet_HH >= 0) then
  begin
    ED__SUNSET_HH.Text := format('%.2d',[Trunc(mrSet_HH)]);
    ED__SUNSET_MM.Text := format('%.2d',[Trunc((mrSet_HH-Trunc(mrSet_HH))*60)]);
  end;

  // Moon
  if(mdtTimeMR > 0) then
  begin
    DeCodeTime(mdtTimeMR,iHH, iMM, iSS, iMS);
    ED__MOON_RISE_HH.Text := format('%.2d',[iHH]);
    ED__MOON_RISE_MM.Text := format('%.2d',[iMM]);
  end;

  if(mdtTimeMS > 0) then
  begin
    DeCodeTime(mdtTimeMS,iHH, iMM, iSS, iMS);
    ED__MOON_SET_HH.Text := format('%.2d',[iHH]);
    ED__MOON_SET_MM.Text := format('%.2d',[iMM]);
  end;

  if(mdtTimeCul > 0) then
  begin
    DeCodeTime(mdtTimeCul,iHH, iMM, iSS, iMS);
    ED__MOON_CUL_HH.Text := format('%.2d',[iHH]);
    ED__MOON_CUL_MM.Text :=  format('%.2d',[iMM]);
  end;

  fMoonPhaseProz := DisplayMoonPhase(mdtWTime,IMG__MOON);
  (*
  // iMoonPhaseProz = 0: Vollmond
  // iMoonPhaseProz = 50: Neumond
  // iMoonPhaseProz = 100: Vollmnond

  fMoonPhaseProz :=CalcMoonPhase(mdtWTime);
  if(fMoonPhaseProz <= 50) then
    iMoonPicNo := 15 + Round(15*(fMoonPhaseProz/50))
  else
    iMoonPicNo := Round((fMoonPhaseProz-50.0)/50 * 15);

  LoadMoonRes(IMG__MOON,'Moon-Mini-' + format('%.2d',[iMoonPicNo]));// + '.JPG');
  *)
  L__MOONPHASE.Caption := '';
  L__MOONPHASE.Font.Color := clRed;

  if(msLANG_ID = 'DE') then
  begin
    if((fMoonPhaseProz > 22) and (fMoonPhaseProz < 28)) then // 25
      L__MOONPHASE.Caption := '3. Viertel';

      if((fMoonPhaseProz > 72) and (fMoonPhaseProz < 78)) then // 75
        L__MOONPHASE.Caption := '1. Viertel';

      if((fMoonPhaseProz >= 85) and (fMoonPhaseProz <= 86)) then
      begin
        L__MOONPHASE.Font.Color := clYellow;
        L__MOONPHASE.Caption := 'Goldener Henkel';
      end;

      if((fMoonPhaseProz >= 0) and (fMoonPhaseProz < 10)) or
      ((fMoonPhaseProz > 90) and (fMoonPhaseProz <= 100)) then
      L__MOONPHASE.Caption := 'Vollmond';

    if((fMoonPhaseProz > 45) and (fMoonPhaseProz < 55)) then
      L__MOONPHASE.Caption := 'Neumond';

    if(L__MOONPHASE.Caption = '') then
    begin
      if(fMoonPhaseProz > 50) then
        L__MOONPHASE.Caption := 'Zunehmend'
      else
        L__MOONPHASE.Caption := 'Abnehmend';
    end;

  end
  else
  begin
    if((fMoonPhaseProz > 22) and (fMoonPhaseProz < 28)) then // 25
      L__MOONPHASE.Caption := '3. Quarter';

      if((fMoonPhaseProz > 72) and (fMoonPhaseProz < 78)) then // 75
        L__MOONPHASE.Caption := '1. Quarter';

      if((fMoonPhaseProz >= 85) and (fMoonPhaseProz <= 86)) then
      begin
        L__MOONPHASE.Font.Color := clYellow;
        L__MOONPHASE.Caption := 'Golden Handle';
      end;

      if((fMoonPhaseProz >= 0) and (fMoonPhaseProz < 10)) or
      ((fMoonPhaseProz > 90) and (fMoonPhaseProz <= 100)) then
      L__MOONPHASE.Caption := 'Full Moon';

    if((fMoonPhaseProz > 45) and (fMoonPhaseProz < 55)) then
      L__MOONPHASE.Caption := 'New Moon';

    if(L__MOONPHASE.Caption = '') then
    begin
      if(fMoonPhaseProz > 50) then
      begin
        if (fMoonPhaseProz <= 75) then
          L__MOONPHASE.Caption := 'Waxing Crescent'
        else
          L__MOONPHASE.Caption := 'Waxing Gibbous';

      end
      else
      begin
        if (fMoonPhaseProz <= 25) then
          L__MOONPHASE.Caption := 'Waning Gibbous'
        else
          L__MOONPHASE.Caption := 'Waning Crescent';

      end;
    end;
  end;

  EvalTimeTable();
  //PrepareSunEclipses();
  //PrepareMoonEclipses();
end;

procedure TF__TIMESTAT.MENU_ECL_SETTIMEClick(Sender: TObject);
var
  sTime, sMsg: string;
begin
  if(msLANG_ID = 'DE') then
  begin
    sTime := 'Zeit';
    sMsg := 'JETZT die Zeit setzen und auf die Sternkarte wechseln?'
  end
  else
  begin
    sTime := 'Time';
    sMsg := 'Select time and move to starmap NOW?';
  end;

  if(PC__SUNANDMOON.ActivePageIndex = 3) then
  begin
    if(GRD__ECLIPSE_SUN.Row > 0) then
    begin
      if(MessageDlg(sTime + ': ' + GRD__ECLIPSE_SUN.Cells[0,GRD__ECLIPSE_SUN.Row],sMsg,mtConfirmation,[mbYes,mbNo],0) = mrYes) then
      begin
        mdtWTimeNew := StrToDateTime(GRD__ECLIPSE_SUN.Cells[0,GRD__ECLIPSE_SUN.Row]);

        Close;
      end;
    end;
  end
  else if (PC__SUNANDMOON.ActivePageIndex = 4) then
  begin
    if(GRD__ECLIPSE_MOON.Row > 0) then
    begin
      if(MessageDlg(sTime + ': ' + GRD__ECLIPSE_MOON.Cells[0,GRD__ECLIPSE_MOON.Row],sMsg,mtConfirmation,[mbYes,mbNo],0) = mrYes) then
      begin
        mdtWTimeNew := StrToDateTime(GRD__ECLIPSE_MOON.Cells[0,GRD__ECLIPSE_MOON.Row]);

        Close;
      end;
    end;
  end;

end;

procedure TF__TIMESTAT.MENU__ECL_EXPLClick(Sender: TObject);
var
  sType: string;
  iRow: Integer;
begin
  case PC__SUNANDMOON.ActivePageIndex of
    3: // Solar Eclipses
    begin
      iRow := GRD__ECLIPSE_SUN.Row;
      if(iRow > 0) then
      begin
        sType := GRD__ECLIPSE_SUN.Cells[2,iRow];
        ShowEclipseExpl(0,sType);
      end;
    end;
    4: // Lunar Eclipses
    begin
      iRow := GRD__ECLIPSE_MOON.Row;
      if(iRow > 0) then
      begin
        sType := GRD__ECLIPSE_MOON.Cells[1,iRow];
        ShowEclipseExpl(1,sType);
      end;
    end;
  end; // case

end;

procedure TF__TIMESTAT.PC__SUNANDMOONChange(Sender: TObject);
begin
  case PC__SUNANDMOON.ActivePageIndex of
    3:
    begin
      IMG__MOONECL.Visible:=false;
      IMG__MOONECL.Align:=alNone;
      IMG__SUNECL.Visible:=true;
      IMG__SUNECL.Align:=alClient;
      P__DYNIMG.Visible:=true;
      P__SUNANDMOON.Visible:=false;
    end;
    4:
    begin
      IMG__SUNECL.Visible:=false;
      IMG__SUNECL.Align:=alNone;
      IMG__MOONECL.Visible:=true;
      IMG__MOONECL.Align:=alClient;
      P__DYNIMG.Visible:=true;
      P__SUNANDMOON.Visible:=false;
    end;
    else
    begin
      P__SUNANDMOON.Visible:=true;
      P__DYNIMG.Visible:=false;
    end
  end; // case

end;

procedure TF__TIMESTAT.RB__MONTHChange(Sender: TObject);
var
  sCaption: string;
begin
  if(RB__MONTH.Checked) then
  begin
    sCaption := FormatDateTime('mmmm yyyy',mdtWTime);
    sCaption := SetDE_ENMonthNames(sCaption,msLANG_ID);
    TS__TIMETABLE.Caption:= sCaption;
    CHART__SM.Title.Text.Text := sCaption;
    (CHART__SM.Series[3] as TConstantLine).Position:=StrToInt(FormatDateTime('d',mdtWTime));
  end;

  EvalTimeTable();
end;

procedure TF__TIMESTAT.RB__MOON_CULClick(Sender: TObject);
begin
  EvalTimeTable();
end;

procedure TF__TIMESTAT.RB__MOON_RISEClick(Sender: TObject);
begin
  EvalTimeTable();
end;

procedure TF__TIMESTAT.RB__MOON_SETClick(Sender: TObject);
begin
  EvalTimeTable();
end;

procedure TF__TIMESTAT.RB__YEARChange(Sender: TObject);
var
  sCaption: string;
begin
  if(RB__YEAR.Checked) then
  begin
    sCaption := FormatDateTime('yyyy',mdtWTime);
    TS__TIMETABLE.Caption:= sCaption;
    CHART__SM.Title.Text.Text := sCaption;
    (CHART__SM.Series[3] as TConstantLine).Position:=DayOfYear(mdtWTime);
  end;

  EvalTimeTable();
end;

procedure TF__TIMESTAT.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then Close;
end;

procedure TF__TIMESTAT.FormCreate(Sender: TObject);
begin
  mdtWTimeNew := 0;
end;

procedure TF__TIMESTAT.B__EXP_SECLClick(Sender: TObject);
var
  tfFile: TextFile;
  iRow: Integer;
  dtTime, dtTimeDUR: TDateTime;
  iYear,iMonth,iDay,iHH,iMM,iSS,iMS,iHH_DUR,iMM_DUR: Word;
  sLine, sDur, sType, sLoc: string;
begin
  AssignFile(tfFile,'E:\Temp\AO-SEcl.dat');
  ReWrite(tfFile);
  WriteLn(tfFile,'YEAR;MONTH;DAY;HOUR;MINUTE;DURATION_HH;DURATION_SS;TYPE;SIZE;LOCATION');
  (*
  GRD__ECLIPSE_SUN.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,2,15) + EncodeTime(20,52,33,0));
  GRD__ECLIPSE_SUN.Cells[1,iRow] := '-';
  GRD__ECLIPSE_SUN.Cells[2,iRow] := 'Partiell';
  GRD__ECLIPSE_SUN.Cells[3,iRow] := '0.599';
  GRD__ECLIPSE_SUN.Cells[4,iRow] := 'Antarktis, südliches Südamerika';
  *)
  for iRow:= 1 to GRD__ECLIPSE_SUN.RowCount-1 do
  begin
    dtTime := StrToDateTime(GRD__ECLIPSE_SUN.Cells[0,iRow]);
    DecodeDate(dtTime,iYear,iMonth,iDay);
    DecodeTime(dtTime,iHH,iMM,iSS,iMS);

    if(GRD__ECLIPSE_SUN.Cells[1,iRow] <> '-') and (Trim(GRD__ECLIPSE_SUN.Cells[1,iRow]) <> '') then
    begin
      dtTimeDUR := StrToTime(GRD__ECLIPSE_SUN.Cells[1,iRow]);
      //dtTimeDUR := GetTimeFromMM_SS(GRD__ECLIPSE_SUN.Cells[1,iRow]);
      DecodeTime(dtTimeDUR,iHH_DUR,iMM_DUR,iSS,iMS);
      iMM_DUR := iMM_DUR + iHH_DUR*60;
      sDur :=  IntToStr(iMM_DUR) + ';' + IntToStr(iSS);
    end
    else
    begin
      sDur := '-;-';
    end;

    if(GRD__ECLIPSE_SUN.Cells[2,iRow] = 'Partiell') then
      sType := 'P'
    else if(GRD__ECLIPSE_SUN.Cells[2,iRow] = 'Ringförmig') then
      sType := 'A'
    else if(GRD__ECLIPSE_SUN.Cells[2,iRow] = 'Hybrid') then
      sType := 'H'
    else if(GRD__ECLIPSE_SUN.Cells[2,iRow] = 'Total') then
      sType := 'T'
    else
      sType := GRD__ECLIPSE_SUN.Cells[2,iRow];

    sLoc := GRD__ECLIPSE_SUN.Cells[4,iRow];

    sLoc := AnsiReplaceStr(sLoc,'Alaska','ALA');
    sLoc := AnsiReplaceStr(sLoc,'Arktis','ARK');
    sLoc := AnsiReplaceStr(sLoc,'Antarktis','ANT');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Asien','NE-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Südostasien','SOASIA');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Asien','N-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'nordöstliches Asien','N-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Westliches Asien','W-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'westliches Asien','W-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Nordasien','N-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Südasien','S-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Ostasien','E-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Asien','ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Südaustralien','S-AUSTRALIA');
    sLoc := AnsiReplaceStr(sLoc,'Australien','AUSTRALIA');
    sLoc := AnsiReplaceStr(sLoc,'Osteuropa','E-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Nordeuropa','N-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Nordwestlichstes Europa','NWST-EUR');
    sLoc := AnsiReplaceStr(sLoc,'nordwestlichstes Europa','NWST-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Nordwestliches Europa','NW-EUR');
    sLoc := AnsiReplaceStr(sLoc,'nordwestliches Europa','NW-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Westeuropa','W-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Südwesteuropa','SW-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Südeuropa','S-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Südöstliches Europa','SE-EUR');
    sLoc := AnsiReplaceStr(sLoc,'südöstliches Europa','SE-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa (östl. Portugal)','EPO-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa (Griechenland)','GRE-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa (Spanien)','ESP-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa','EUR');
    sLoc := AnsiReplaceStr(sLoc,'Mittelamerika','MAM');
    sLoc := AnsiReplaceStr(sLoc,'Mittel- und Südamerika','MSAM');
    sLoc := AnsiReplaceStr(sLoc,'Östl. Nord- und Südamerika','E-NSAM');
    sLoc := AnsiReplaceStr(sLoc,'Südchile','S-CHIL');
    sLoc := AnsiReplaceStr(sLoc,'Südargentinien und Chile','SARG-CHIL');
    sLoc := AnsiReplaceStr(sLoc,'Südargentinien','S-ARG');
    sLoc := AnsiReplaceStr(sLoc,'Nord- und Südamerika','NSAM');
    sLoc := AnsiReplaceStr(sLoc,'Nördliches Nordamerika','NN-NAM');
    sLoc := AnsiReplaceStr(sLoc,'nördliches Nordamerika','NN-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Östliches Nordamerika','E-NAM');
    sLoc := AnsiReplaceStr(sLoc,'östliches Nordamerika','E-NAM');
    sLoc := AnsiReplaceStr(sLoc,'westliches Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'westl. Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Westliches Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Westl. Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Nordamerika','NAM');
    sLoc := AnsiReplaceStr(sLoc,'Südliches Südamerika','S-SAM');
    sLoc := AnsiReplaceStr(sLoc,'südliches Südamerika','S-SAM');
    sLoc := AnsiReplaceStr(sLoc,'Südamerika','SAM');
    sLoc := AnsiReplaceStr(sLoc,'Nordpazifik','N-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Ostpazifik','E-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Zentralpazifik','C-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Südöstlicher Pazifik','SE-PAC');
    sLoc := AnsiReplaceStr(sLoc,'südöstlicher Pazifik','SE-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Südlicher Pazifik','S-PAC');
    sLoc := AnsiReplaceStr(sLoc,'südlicher Pazifik','S-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Pazifik','PAC');
    sLoc := AnsiReplaceStr(sLoc,'Philippinen','PHIL');
    sLoc := AnsiReplaceStr(sLoc,'Nordafrika','N-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Südliches Afrika','S-AFR');
    sLoc := AnsiReplaceStr(sLoc,'südliches Afrika','S-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Westafrika','W-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Ostafrika','O-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Nordwestliches Afrika','NW-AFR');
    sLoc := AnsiReplaceStr(sLoc,'nordwestliches Afrika','NW-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Afrika','AFR');
    sLoc := AnsiReplaceStr(sLoc,'Südatlantik','S-ANT');
    sLoc := AnsiReplaceStr(sLoc,'Nordatlantik','N-ANT');
    sLoc := AnsiReplaceStr(sLoc,'Naher Osten','NEAST');
    sLoc := AnsiReplaceStr(sLoc,'Nordrussland','N-RUS');
    sLoc := AnsiReplaceStr(sLoc,'Neuseeland','NSL');
    sLoc := AnsiReplaceStr(sLoc,'Nordkanada','N-CAN');
    sLoc := AnsiReplaceStr(sLoc,'Südlicher Indischer Ozean','S-INO');
    sLoc := AnsiReplaceStr(sLoc,'südlicher Indischer Ozean','S-INO');

    sLoc := AnsiReplaceStr(sLoc,', ','|');
    sLoc := AnsiReplaceStr(sLoc,',','|');
    sLoc := Trim(sLoc);

    sLine := IntToStr(iYear) + ';' +
      IntToStr(iMonth) + ';' +
      IntToStr(iDay) + ';' +
      IntToStr(iHH) + ';' +
      IntToStr(iMM) + ';' +
      sDur + ';' +
      sType + ';' +
      GRD__ECLIPSE_SUN.Cells[3,iRow] + ';' + // Size
      sLoc;

    WriteLn(tfFile,sLine);

  end;

  CloseFile(tfFile);

end;

procedure TF__TIMESTAT.B__EXP_MECLClick(Sender: TObject);
var
  tfFile: TextFile;
  iRow: Integer;
  dtTime, dtTimeDUR: TDateTime;
  iYear,iMonth,iDay,iHH,iMM,iSS,iMS,iHH_DUR,iMM_DUR: Word;
  sLine, sDur1, sDur2, sDur3, sType, sLoc: string;
begin
  AssignFile(tfFile,'E:\Temp\AO-MEcl.dat');
  ReWrite(tfFile);
  WriteLn(tfFile,'YEAR;MONTH;DAY;HOUR;MINUTE;TYPE;DURATION1_HH;DURATION1_SS;DURATION2_HH;DURATION2_SS;DURATION3_HH;DURATION3_SS;LOCATION');
  (*
  GRD__ECLIPSE_MOON.Cells[0,iRow] := FormatDateTime('dd.mm.yy hh:mm',EncodeDate(2018,1,31) + EncodeTime(13,31,0,0));
  GRD__ECLIPSE_MOON.Cells[1,iRow] := 'Total';
  GRD__ECLIPSE_MOON.Cells[2,iRow] := FormatDateTime('hh:mm',EncodeTime(5,17,0,0));
  GRD__ECLIPSE_MOON.Cells[3,iRow] := FormatDateTime('hh:mm',EncodeTime(3,23,0,0));
  GRD__ECLIPSE_MOON.Cells[4,iRow] := FormatDateTime('hh:mm',EncodeTime(1,16,0,0));
  GRD__ECLIPSE_MOON.Cells[5,iRow] := 'Asien, Australien, Pazifik, westl. Nordamerika';
  *)
  for iRow:= 1 to GRD__ECLIPSE_MOON.RowCount-1 do
  begin
    dtTime := StrToDateTime(GRD__ECLIPSE_MOON.Cells[0,iRow]);
    DecodeDate(dtTime,iYear,iMonth,iDay);
    DecodeTime(dtTime,iHH,iMM,iSS,iMS);

    if(GRD__ECLIPSE_MOON.Cells[1,iRow] = 'Partiell') then
      sType := 'PA'
    else if(GRD__ECLIPSE_MOON.Cells[1,iRow] = 'Halbschatten') then
      sType := 'PN'
    else if(GRD__ECLIPSE_MOON.Cells[1,iRow] = 'Total') then
      sType := 'T'
    else if(GRD__ECLIPSE_MOON.Cells[1,iRow] = 'Total (z)') then
      sType := 'TZ'
    else
      sType := GRD__ECLIPSE_MOON.Cells[1,iRow];

    if(GRD__ECLIPSE_MOON.Cells[2,iRow] <> '-') and (Trim(GRD__ECLIPSE_MOON.Cells[2,iRow]) <> '') then
    begin
      dtTimeDUR := StrToTime(GRD__ECLIPSE_MOON.Cells[2,iRow]);
      DecodeTime(dtTimeDUR,iHH_DUR,iMM_DUR,iSS,iMS);
      sDur1 := IntToStr(iHH_DUR) + ';' + IntToStr(iMM_DUR);
    end
    else
    begin
      sDur1 := '-;-';
    end;

    if(GRD__ECLIPSE_MOON.Cells[3,iRow] <> '-') and (Trim(GRD__ECLIPSE_MOON.Cells[3,iRow]) <> '') then
    begin
      dtTimeDUR := StrToTime(GRD__ECLIPSE_MOON.Cells[3,iRow]);
      DecodeTime(dtTimeDUR,iHH_DUR,iMM_DUR,iSS,iMS);
      sDur2 := IntToStr(iHH_DUR) + ';' + IntToStr(iMM_DUR);
    end
    else
    begin
      sDur2 := '-;-';
    end;

    if(GRD__ECLIPSE_MOON.Cells[4,iRow] <> '-') and (Trim(GRD__ECLIPSE_MOON.Cells[4,iRow]) <> '') then
    begin
      dtTimeDUR := StrToTime(GRD__ECLIPSE_MOON.Cells[4,iRow]);
      DecodeTime(dtTimeDUR,iHH_DUR,iMM_DUR,iSS,iMS);
      sDur3 := IntToStr(iHH_DUR) + ';' + IntToStr(iMM_DUR);
    end
    else
    begin
      sDur3 := '-;-';
    end;

    sLoc := GRD__ECLIPSE_MOON.Cells[5,iRow];

    sLoc := AnsiReplaceStr(sLoc,'Alaska','ALA');
    sLoc := AnsiReplaceStr(sLoc,'Arktis','ARK');
    sLoc := AnsiReplaceStr(sLoc,'Antarktis','ANT');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Asien','NE-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Südostasien','SOASIA');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Asien','N-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'nordöstliches Asien','N-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Westliches Asien','W-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'westliches Asien','W-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Nordasien','N-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Südasien','S-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Ostasien','E-ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Asien','ASIA');
    sLoc := AnsiReplaceStr(sLoc,'Südaustralien','S-AUSTRALIA');
    sLoc := AnsiReplaceStr(sLoc,'Australien','AUSTRALIA');
    sLoc := AnsiReplaceStr(sLoc,'Osteuropa','E-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Nordeuropa','N-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Nordwestlichstes Europa','NWST-EUR');
    sLoc := AnsiReplaceStr(sLoc,'nordwestlichstes Europa','NWST-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Nordwestliches Europa','NW-EUR');
    sLoc := AnsiReplaceStr(sLoc,'nordwestliches Europa','NW-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Westeuropa','W-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Südwesteuropa','SW-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Südeuropa','S-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Südöstliches Europa','SE-EUR');
    sLoc := AnsiReplaceStr(sLoc,'südöstliches Europa','SE-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa (östl. Portugal)','EPO-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa (Griechenland)','GRE-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa (Spanien)','ESP-EUR');
    sLoc := AnsiReplaceStr(sLoc,'Europa','EUR');
    sLoc := AnsiReplaceStr(sLoc,'Mittelamerika','MAM');
    sLoc := AnsiReplaceStr(sLoc,'Mittel- und Südamerika','MSAM');
    sLoc := AnsiReplaceStr(sLoc,'Östl. Nord- und Südamerika','E-NSAM');
    sLoc := AnsiReplaceStr(sLoc,'Südchile','S-CHIL');
    sLoc := AnsiReplaceStr(sLoc,'Südargentinien und Chile','SARG-CHIL');
    sLoc := AnsiReplaceStr(sLoc,'Südargentinien','S-ARG');
    sLoc := AnsiReplaceStr(sLoc,'Nord- und Südamerika','NSAM');
    sLoc := AnsiReplaceStr(sLoc,'Nördliches Nordamerika','NN-NAM');
    sLoc := AnsiReplaceStr(sLoc,'nördliches Nordamerika','NN-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Östliches Nordamerika','E-NAM');
    sLoc := AnsiReplaceStr(sLoc,'östliches Nordamerika','E-NAM');
    sLoc := AnsiReplaceStr(sLoc,'westliches Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'westl. Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Westliches Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Westl. Nordamerika','W-NAM');
    sLoc := AnsiReplaceStr(sLoc,'Nordamerika','NAM');
    sLoc := AnsiReplaceStr(sLoc,'Südliches Südamerika','S-SAM');
    sLoc := AnsiReplaceStr(sLoc,'südliches Südamerika','S-SAM');
    sLoc := AnsiReplaceStr(sLoc,'Südamerika','SAM');
    sLoc := AnsiReplaceStr(sLoc,'Nordpazifik','N-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Ostpazifik','E-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Zentralpazifik','C-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Südöstlicher Pazifik','SE-PAC');
    sLoc := AnsiReplaceStr(sLoc,'südöstlicher Pazifik','SE-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Südlicher Pazifik','S-PAC');
    sLoc := AnsiReplaceStr(sLoc,'südlicher Pazifik','S-PAC');
    sLoc := AnsiReplaceStr(sLoc,'Pazifik','PAC');
    sLoc := AnsiReplaceStr(sLoc,'Philippinen','PHIL');
    sLoc := AnsiReplaceStr(sLoc,'Nordafrika','N-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Südliches Afrika','S-AFR');
    sLoc := AnsiReplaceStr(sLoc,'südliches Afrika','S-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Westafrika','W-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Ostafrika','O-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Nordwestliches Afrika','NW-AFR');
    sLoc := AnsiReplaceStr(sLoc,'nordwestliches Afrika','NW-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'nordöstliches Afrika','NE-AFR');
    sLoc := AnsiReplaceStr(sLoc,'Afrika','AFR');
    sLoc := AnsiReplaceStr(sLoc,'Südatlantik','S-ANT');
    sLoc := AnsiReplaceStr(sLoc,'Nordatlantik','N-ANT');
    sLoc := AnsiReplaceStr(sLoc,'Naher Osten','NEAST');
    sLoc := AnsiReplaceStr(sLoc,'Nordrussland','N-RUS');
    sLoc := AnsiReplaceStr(sLoc,'Neuseeland','NSL');
    sLoc := AnsiReplaceStr(sLoc,'Nordkanada','N-CAN');
    sLoc := AnsiReplaceStr(sLoc,'Südlicher Indischer Ozean','S-INO');
    sLoc := AnsiReplaceStr(sLoc,'südlicher Indischer Ozean','S-INO');

    sLoc := AnsiReplaceStr(sLoc,', ','|');
    sLoc := AnsiReplaceStr(sLoc,',','|');
    sLoc := Trim(sLoc);

    sLine := IntToStr(iYear) + ';' +
      IntToStr(iMonth) + ';' +
      IntToStr(iDay) + ';' +
      IntToStr(iHH) + ';' +
      IntToStr(iMM) + ';' +
      sType + ';' +
      sDur1 + ';' +
      sDur2 + ';' +
      sDur3 + ';' +
      sLoc;

    WriteLn(tfFile,sLine);

  end;


  CloseFile(tfFile);
end;


end.

