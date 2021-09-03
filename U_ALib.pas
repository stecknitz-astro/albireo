unit U_ALib;

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

//{$DEFINE DELPHI10} // Define for Development under Delphi 10+
{$DEFINE LAZARUS} // Define for Development under Lazarus
//{$DEFINE WINDOWS} // Define for Development under Lazarus
//{$DEFINE MACOS} // Define for Development under Lazarus
//{$DEFINE LINUX} // Define for Development under Lazarus


{$IFDEF LAZARUS}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$IFDEF LAZARUS}
uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, StrUtils, ShlObj,// ContNrs,
  Menus, DateUtils, Math, LCLIntf,
  U_AConst, U_Translation,
  {$IFDEF Windows}
  Windows, ShellAPI,
  {$ENDIF WIndows}
  {$IFDEF Darwin}
  MacOSAll, CocoaAll,
  {$ENDIF Darwin}
  {$IFDEF LINUX}
  Unix,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}
  LResources;

{$ENDIF}

{$IFDEF DELPHI10}
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,StrUtils,
  Math, System.DateUtils;
{$ENDIF}

(*
{$IFDEF LAZARUS}
  procedure ExecuteProcess();
{$ENDIF}
*)

{$IFDEF DELPHI10}
  function GetTimeZone(DateTime: TDateTime): Integer;
{$ENDIF}
  function StrIsNum(bFloatCheck: Boolean; var sVar: string): Boolean;
  procedure HoursToHH_MM_SS(fHours: Real; var iHH: Word; var iMM: Word; var rSS: Real);
  function HourToHHMMStr(rHour: Real): string;
  procedure DegToDEG_MM_SS(fAngle: Real; var iDEG: SmallInt; var iMM: SmallInt; var rSS: Real; IsCircleAngle: Boolean);
  procedure DegToDEG_MM_SS2(fAngle: Real; var iDEG: SmallInt; var iMM: SmallInt; var rSS: Real);
  function HH_MM_SSToDeg(iHH, iMM: Word; rSS: Real): Real;
  function DEG_MM_SSToDeg(iDEG, iMM: SmallInt; rSS: Real): Real;
  function DayOfYear(dtDate: TDateTime): Integer;
  function DaysOfMonth(dtDate: TDateTime): Word;
  function GetDST(dtDate: TDateTime; sDST: string; iDST_DIFF: Integer): Integer;
  function FindLastDayOfWeek(iYYYY, iMonth, iSearchDay: Integer): TDateTime;
  function CalcMoonPhase(dtTime: TDateTime): Integer;
  function CountChar(sInstr: string; cSym: Char): Integer;
  function GetNormVal(rVal, rNormVal: Real): Real;
  function ArcTan360(rZ,rN: Real): Real;
  function EclipticObl(dJD: Double): Real;
  procedure EclipticToEquatorial(dJD: Double; rLambda,rBeta: Real;
    var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
    var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real);
  procedure EclipticToEquatorialV2(dJD: Double; rLambda,rBeta: Real;
    var rRA: Real; var rDEC: Real);
  procedure EquatorialToEcliptic(dJD: Double; rRA, rDEC: Real; var rLambda: Real; var rBeta: Real);
  function HorizonToEquatorial(rAlt,rAz,rPhi: Real;
    var iHH_HH: Word; var iHH_MM: Word; var rHH_SS: Real;
    var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real): Boolean;
  function GetSIDTime(dtTimeIn: TDateTime;
    iDST_HH, iUTC_HH: SmallInt;
    iGLng_DEG: SmallInt; iGLng_MIN: Word;
    var dJulDat: Double): TDateTime;
  function GetJulTime(dtTimeIn: TDateTime; iDST_HH,iUTC_HH: SmallInt): Double;
  procedure GetEcpliticCoo(rLambda: Real; dtTime: TDateTime; iDST_HH, iUTC_HH: SmallInt; var rRA: Real; var rDEC: Real);
  procedure GetGalacticPlaneCoo(rLambda: Real; var rRA: Real; var rDEC: Real);
  procedure GetGalacticPlaneCooExt(rLambda, rBeta: Real; var rRA: Real; var rDEC: Real);
  procedure GetSunCoo(dtDateTime: TDateTime;
    iDST_HH,iUTC_HH: SmallInt; var rLambda: Real; var rM: Real;
    var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
    var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real);
  function DayDiffEpoch2010(iYear: Integer): Integer;
  procedure GetMoonCoo(dtDateTime: TDateTime;
    iDST_HH,iUTC_HH: SmallInt; rLambdaSun, rMSun: Real;
    var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
    var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real);
  function StrToTimeMS(sTime, sMSSep: string): TDateTime;
  function StrToFloatExt(sVal: string): Real;
  function GetEyePupil(iYearOfBirth: Integer): Real;
  function KeplerSolve(rEx, rM, rEpsilon: Real): Real;
  function GetDeltaKepler(rEx, rM: Real): Real;
  procedure CalcSunRiseAndSet(
    iGLng_DEG, iGLng_MIN,
    iGLat_DEG, iGLat_MIN,
    iUTC_HH,
    iDay, iDST_HH, iDawn_MIN: Integer; var rSunRise_HH: Real; var rSunSet_HH: Real);
  function GetMonth(dtDateTime: TDateTime): Word;
  //function GetYearVal(dtDate: TDateTime): Real;

  function StrToFloatExt2(sValue: string): Real;
  function StrToFloatExt3(sValue: string; var bIsAlphaNum: Boolean): Real;
  function StrToFloatExt4(sValue: string): Real;
  function IsNumeric(sValue: string): Boolean;

  procedure HMouseCooToHorizon(iWidth, iHeight: Integer; sHMode: string; rMouseX, rMouseY, rEyeFacH: Real; var rAz: Real; var rAlt: Real);
  procedure MouseCooToHorizon(iX0, iY0, iR0: Integer; rMouseX, rMouseY: Real; var rAz: Real; var rAlt: Real);
  function GetHA(dtST,dtRA: TDateTime): TDateTime;
  procedure CalcAZCoo(iDEC_DEG, iDEC_MIN: SmallInt; rDEC_SS: Real; iHA_HH, iHA_MIN, iHA_SS: SmallInt;
    rSin_fGLat, rCos_fGLat: Real;
    var fAz: Real; var fHgt: Real);
  procedure CalcMoonOfMonth(dtDate: TDateTime;
    var iFullMoon: Word;
    var iNewMoon: Word;
    var iBlueMoon: Word);

  function CalcMoonRiseAndMoonSet(
    dtWT: TDateTime;
    iDST_HH,iUTC_HH,iGLng_DEG,iGLng_MIN: Integer;
    rSin_fGLat, rCos_fGLat: Real;
    var dtTimeMR: TDateTime;
    var dtTimeMS: TDateTime;
    var dtTimeCul: TDateTime;
    var rHgt_Max: Real): string;

  function GetTimeZoneName(iLat_DEG, iLng_DEG: Integer): string;

  Procedure DrawEllipse(Dest: TCanvas;
                        xCenter, yCenter,         // mittelpunkt x,y
                        xRadius, yRadius: Single; // breite, höhe
                        RotateAngle: Single = 0;  // winkel (0° bis 360°) drehung im uhrzeigersinn
                        sAngle: Single = 0;       // ARC start winkel
                        eAngle: Single = 360;     // ARC end winkel
                        Precision: Byte = 60);    // genauigkeit

  Procedure DrawPlanetEllipse(Image: TImage; SHP: TShape; rLAngle: Single; sLabel: string;
                        Dest: TCanvas;
                        xCenter, yCenter: Integer;         // mittelpunkt x,y
                        xRadius, yRadius: Integer; // breite, höhe
                        Inclination: Single; /// Orbit inclination
                        sType: string;
                        RotateAngle: Single = 0;  // winkel (0° bis 360°) drehung im uhrzeigersinn
                        sAngle: Single = 0;       // ARC start winkel
                        eAngle: Single = 360;     // ARC end winkel
                        Precision: Integer = 2500);    // genauigkeit

  function SetDE_ENMonthNames(sText,sLANG_ID: string): string;

  procedure ExecOpen(sURL: string);

  function GetRVal(rgb: LongInt): BYTE;
  function GetGVal(rgb: LongInt): BYTE;
  function GetBVal(rgb: LongInt): BYTE;
  function GetRGB(R,G,B: BYTE): TColor;

  (*
  function GetVK_F5(): Word;
  function GetVK_ESCAPE(): Word;
  *)

  function GetDECYear(dtDateTime: TDateTime): Real;
  function GetCountryStringList(sLANG_ID: string; var slCountries: TStringList; sAOFileName: string): Boolean;
  function GetCitiesList(sCountrySel, sLANG_ID: string; var slCities: TStringList; sAOFileName: string): Boolean;
  function CycleDiff(fVal1,fVal2,fCycleVal: Real): Real;
  function GetLocalUserAppDataPath(): string;
  function GetVolumeLabel(DriveChar: Char): string;
  function GetVolumeID(cDriveChar: Char): string;
  function GetDriveChar(): Char;
  function GetStarBaseSignal(iAperture_mm: Integer; sTelType: string; var iOptArea_qcm: Integer; var fTransmission: Real): Real;
  function GetStarBaseSignalCompact(iAperture_mm: Integer; sTelType: string): Real;
  procedure EquToAZCoo(dtDateTime: TDateTime;
    iDST_HH,iUTC_HH,
    iGLng_DEG,iGLng_MIN: SmallInt;
    rSin_fGLat, rCos_fGLat: Real;
    iDEC_DEG, iDEC_MM: SmallInt; rDEC_SS: Real;
    iRA_HH, iRA_MM: SmallInt; rRA_SS: Real;
    var rAz: Real; var rHgt: Real
    );
  function GetMessierNum(sMessier: string): string;
  function GetApproxEarthRA(dtDateTime: TDateTime): Real;

  function GetTimeFromMM_SS(sMM_SS: string): TDateTime;

  function TranslateLocStr(sLANG_ID,sIn,sEN,sDE,sKey: string): string;
  function TranslateEclStr(sLANG_ID,sIn,sEclType,sField: string): string;

  function IsInteger(sValue: string): Boolean; // Exponential E NOT allowed.

  function WriteTextFile(sLine, sFileName: string): Boolean;
  function WriteTextFile2(sLine1, sLine2, sFileName: string): Boolean;
  function ReadFilesFromDir(slFileList: TStringList; sPathWithSlash, sFileExt: string): Integer;

  function MinFloatValue(const data : array of Real): Real;
  function MaxFloatValue(const data : array of Real): Real;

  function Get_arcsec(fPlanetRatio,fDist_AU: Real): Real;

implementation

function Get_arcsec(fPlanetRatio,fDist_AU: Real): Real;
begin
  Result := arctan(fPlanetRatio * 12735/(fDist_AU*149.598e6))*180/Pi*3600;
end;

function MaxFloatValue(const data : array of Real): Real;
var
  i,iLow,iHigh: Integer;
begin
  Result := -9999999999999999;

  iLow := Low(data);
  iHigh := High(data);

  for i:=iLow to iHigh do
  begin
    if(i = iLow) then
      Result := data[i]
    else
    begin
      if(data[i] > Result) then
        Result := data[i];
    end;
  end;
end;

function MinFloatValue(const data : array of Real): Real;
var
  i,iLow,iHigh: Integer;
begin
  Result := 9999999999999999;

  iLow := Low(data);
  iHigh := High(data);

  for i:=iLow to iHigh do
  begin
    if(i = iLow) then
      Result := data[i]
    else
    begin
      if(data[i] < Result) then
        Result := data[i];
    end;
  end;
end;

function ReadFilesFromDir(slFileList: TStringList; sPathWithSlash, sFileExt: string): Integer;
var
  Info : TSearchRec;
  sFileName: string;
begin
  Result := -1;

  if(slFileList = nil) then
    exit;

  slFileList.Clear;

  If (SysUtils.FindFirst (sPathWithSlash + '*' + sFileExt,faAnyFile,Info) = 0) then
  begin
    repeat
      sFileName := ExtractFileName(Info.Name);
      slFileList.Add(sFileName);
    until (SysUtils.FindNext(info)<>0);
    SysUtils.FindClose(Info);
  end;

  Result := slFileList.Count;

end;

function WriteTextFile(sLine,sFileName: string): Boolean;
var
  tfTextFile: TextFile;
begin
  Result := true;

  try
    Assign(tfTextFile,sFileName);
    ReWrite(tfTextFile);
    WriteLn(tfTextFile,sLine);
    Flush(tfTextFile);
    CloseFile(tfTextFile);
  except
    Result := false;
  end;

end;

function WriteTextFile2(sLine1, sLine2,sFileName: string): Boolean;
var
  tfTextFile: TextFile;
begin
  Result := true;

  try
    Assign(tfTextFile,sFileName);
    ReWrite(tfTextFile);
    WriteLn(tfTextFile,sLine1);
    WriteLn(tfTextFile,sLine2);
    Flush(tfTextFile);
    CloseFile(tfTextFile);
  except
    Result := false;
  end;

end;

function IsInteger(sValue: string): Boolean; // Exponential E NOT allowed.
var
  i: Integer;
  bIsNonInteger: Boolean;
begin
  Result := false;
  bIsNonInteger := false;

  i:=1;
  while (not  bIsNonInteger) and (i <= Length(sValue)) do
  begin
    bIsNonInteger := (sValue[i] in ['a'..'z','A'..'Z','.',',',
        '*','#','~','?','=','!','"','%','&','/','(',')','{','}','[',']',
        '-','_',';',':']);
    Inc(i);
  end;

  Result := not bIsNonInteger;

end;

function TranslateLocStr(sLANG_ID,sIn,sEN,sDE,sKey: string): string;
begin
  if(sLANG_ID = 'DE') then
    Result := AnsiReplaceStr(sIn,sKey,sDE)
  else
    Result := AnsiReplaceStr(sIn,sKey,sEN);

end;

function TranslateEclStr(sLANG_ID,sIn,sEclType,sField: string): string;
var
  sLoc: string;
begin
  sIn := Uppercase(sIn);
  sEclType := Uppercase(sEclType);
  sField := Uppercase(sField);

  Result := sIn;
  if(sField = 'LOC') then
  begin
    sLoc := sIn;
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Alaska','Alaska','ALA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Artica','Arktis','ARK');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Antartica','Antarktis','ANT');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Northeast Asia','Nordöstliches Asien','NE-ASIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Southeast Asia','Südostasien','SOASIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'West Asia','Westliches Asien','W-ASIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North Asia','Nordasien','N-ASIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Southern','Südasien','S-ASIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Eastern Asia','Ostasien','E-ASIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Asia','Asien','ASIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Australia','Südaustralien','S-AUSTRALIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Australia','Australien','AUSTRALIA');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Southeastern Europe','Südöstliches Europa','SE-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'East Europe','Osteuropa','E-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North Europe','Nordeuropa','N-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Most northwest Europe','Nordwestlichstes Europa','NWST-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Northwest Europe','Nordwestliches Europa','NW-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'West Europe','Westeuropa','W-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Southwest Europe','Südwesteuropa','SW-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Europe','Südeuropa','S-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Europe (eastern Portugal)','Europa (östl. Portugal)','EPO-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Europe (Greece)','Europa (Griechenland)','GRE-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Europe (Spain)','Europa (Spanien)','ESP-EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Europe','Europa','EUR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Central Ameriaca','Mittelamerika','MAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Central and Southern America','Mittel- und Südamerika','MSAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Eastern North and Southamerica','Östl. Nord- und Südamerika','E-NSAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Chile','Südchile','S-CHIL');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Argentina and Chile','Südargentinien und Chile','SARG-CHIL');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Argentina','Südargentinien','S-ARG');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North and South America','Nord- und Südamerika','NSAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Northern North America','Nördliches Nordamerika','NN-NAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Eastern North America','Östliches Nordamerika','E-NAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Western North America','westliches Nordamerika','W-NAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North America','Nordamerika','NAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Southern South America','Südliches Südamerika','S-SAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South America','Südamerika','SAM');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North Pacific','Nordpazifik','N-PAC');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'East Pacific','Ostpazifik','E-PAC');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Central Pacific','Zentralpazifik','C-PAC');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Southeast Pacific','Südöstlicher Pazifik','SE-PAC');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Pacific','Südlicher Pazifik','S-PAC');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Pacific','Pazifik','PAC');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Philippines','Philippinen','PHIL');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North Africa','Nordafrika','N-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Northeast Africa','Nordöstliches Afrika','NE-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Africa','Südliches Afrika','S-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Soutern Africa','südliches Afrika','S-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'West Africa','Westafrika','W-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'East Africa','Ostafrika','O-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Northwest Africa','Nordwestliches Afrika','NW-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Northeast','Nordöstliches Afrika','NE-AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Africa','Afrika','AFR');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'South Atlantic','Südatlantik','S-ANT');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North Atlantic','Nordatlantik','N-ANT');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Middle East','Naher Osten','NEAST');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North Russia','Nordrussland','N-RUS');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'New Zealand','Neuseeland','NSL');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'North Canada','Nordkanada','N-CAN');
    sLoc := TranslateLocStr(sLANG_ID,sLoc,'Souterh Indian Ocean','Südlicher Indischer Ozean','S-INO');

    Result := AnsiReplaceStr(sLoc,'|',', ');

  end
  else if(sField = 'TYPE') then
  begin
    if(sEclType = 'S') then
    begin
      if(sIn = 'P') then
      begin
        if(sLANG_ID = 'DE') then Result := 'Partiell'
        else Result := 'Partial';
      end
      else if(sIn = 'A') then
      begin
        if(sLANG_ID = 'DE') then Result := 'Ringförmig'
        else Result := 'Annular';
      end
      else if(sIn = 'T') then
      begin
        Result := 'Total';
      end
      else if(sIn = 'H') then
      begin
        Result := 'Hybrid';
      end;
    end
    else if(sEclType = 'M') then
    begin
      if(sIn = 'PA') then
      begin
        if(sLANG_ID = 'DE') then Result := 'Partiell'
        else Result := 'Partial';
      end
      else if(sIn = 'PN') then
      begin
        if(sLANG_ID = 'DE') then Result := 'Halbschatten'
        else Result := 'Penumbral';
      end
      else if(sIn = 'T') then
      begin
        Result := 'Total'
      end
      else if(sIn = 'TZ') then
      begin
        if(sLANG_ID = 'DE') then Result := 'Total (z)'
        else Result := 'Total (c)';
      end;
    end;
  end;

end;

function GetTimeFromMM_SS(sMM_SS: string): TDateTime;
{09.10.2020/fs
Returns Time (Minutes-Seconds) from MM:SS string
}
var
  iMM, iSS: Word;
  iPos, iLen: Integer;
begin
  Result := 0;
  iMM := 0; iSS := 0;

  iPos := Pos(':',sMM_SS);
  if(iPos > 1) then
  begin
    iMM := StrToInt(LeftStr(sMM_SS,iPos-1));
    iLen := Length(sMM_SS) - iPos;
    iSS := StrToInt(RightStr(sMM_SS,iLen));
    Result := EncodeTime(0,iMM,iSS,0);
  end;
end;

function GetApproxEarthRA(dtDateTime: TDateTime): Real;
{22.09.2020/fs
Return the approximated Earth RA-Direction
}
var
  iDay, iDayRef, iDiffDays: Integer;
  dtDayRef: TDateTime;
begin
  iDay := DayOfYear(dtDateTime);
  dtDayRef := EncodeDate(2020,9,21);
  iDayRef := DayOfYear(dtDayRef);
  iDiffDays := iDay - iDayRef;

  if(iDiffDays >= 0) then
  begin
    Result := 24.0/365.0 * iDiffDays;
  end
  else
  begin
    Result := 24.0 - 24.0/365.0 * iDiffDays;
  end;


end;

function GetMessierNum(sMessier: string): string;
begin
  sMessier := Trim(sMessier);

  case Length(sMessier) of
    2: Result := AnsiReplaceStr(sMessier,'M','M00');
    3: Result := AnsiReplaceStr(sMessier,'M','M0');
    else
      Result := sMessier;
  end;  //case Length()

end;

function GetStarBaseSignalCompact(iAperture_mm: Integer; sTelType: string): Real;
var
  iOptArea_qcm: Integer;
  fTransmission: Real;
begin
  iOptArea_qcm := 0; fTransmission := 0;
  Result := GetStarBAseSignal(iAperture_mm,sTelType,iOptArea_qcm,fTransmission);
end;

function GetStarBaseSignal(iAperture_mm: Integer; sTelType: string; var iOptArea_qcm: Integer; var fTransmission: Real): Real;
// Returns Standard Star signal strehngth
var
  rM2Estimated: Real;
  iTelType: SmallInt;
begin
  iOptArea_qcm := 0;
  rM2Estimated := 0;
  sTelType := Uppercase(sTelType);
  if(AnsiContainsStr(sTelType,'REFRA')) then
    iTelType := 0
  else if(AnsiContainsStr(sTelType,'NEWTON')) then
    iTelType := 1
  else
    iTelType := 2;

  case iTelType of
    0: // Refraktor
    begin
      iOptArea_qcm := Round(Pi/4*(iAperture_mm*iAperture_mm)* 0.01);
      fTransmission := 0.72; // = 0.96^8 // Handbuch der Astrofotografie S. 255
    end;
    1: // (Newton-) Reflektor mit Fangspiegel
    begin
      rM2Estimated := 0.094/0.254 * iAperture_mm;
      iOptArea_qcm := Round(Pi/4*(iAperture_mm*iAperture_mm - rM2Estimated*rM2Estimated)* 0.01);
      //fTransmission := 0.61; // = 0.96^8 * 0.92^2 // Handbuch der Astrofotografie S. 255
      fTransmission := 0.663; // = 0.96^6 * 0.92^2 // Handbuch der Astrofotografie S. 255
    end;
    2: // SC-Teleskop
    begin
      rM2Estimated := 0.094/0.254 * iAperture_mm;
      iOptArea_qcm := Round(Pi/4*(iAperture_mm*iAperture_mm - rM2Estimated*rM2Estimated)* 0.01);
      fTransmission := 0.56; // = 0.96^10 * 0.92^2 // Handbuch der Astrofotografie S. 255
    end;
  end; // case

  Result := fTransmission*0.6*iOptArea_qcm/10000 *100 *3.63e-11*
        550e-9 /6.626e-34/300000000; //Handbuch Astrofotografie S. 256

  Result := 2.8 * Result; // Estimated total value over all other spectal bands (U+B(+V)+R+I)
end;

function GetDriveChar(): Char;
begin
  Result := 'X';

  if(DirectoryExists('C:\')) then
    Result := 'C'
  else if(DirectoryExists('D:\')) then
    Result := 'D'
  else if(DirectoryExists('E:\')) then
    Result := 'E'
  else if(DirectoryExists('F:\')) then
    Result := 'F';

end;

function GetVolumeID(cDriveChar: Char): string;
//Para obtener el # de serie de un disco
var
  MaxFileNameLength:dWord=0;
  VolFlags:dWord=0;
  sSerNum: DWord;
  sDrivePath : String;
begin
  sDrivePath :=  cDriveChar + ':\';
  if GetVolumeInformation(PChar(sDrivePath), nil, 0,
     @sSerNum, MaxFileNameLength, VolFlags, nil, 0)
  then
  begin
      Result := IntToStr(sSerNum);
  end
  else
      Result := '';

  Result := Trim(Result);
end;

function GetVolumeLabel(DriveChar: Char): string;
var
  NotUsed:     DWORD;
  VolumeFlags: DWORD;
  VolumeInfo:  array[0..MAX_PATH] of Char;
  VolumeSerialNumber: DWORD;
  Buf: array [0..MAX_PATH] of Char;
begin
    Buf[0] := '0';
    VolumeFlags := 0;
    NotUsed := 0;

    GetVolumeInformation(PChar(DriveChar + ':\'),
    Buf, SizeOf(VolumeInfo), @VolumeSerialNumber, NotUsed,
    VolumeFlags, nil, 0);

    SetString(Result, Buf, StrLen(Buf));   { Set return result }
    Result:=AnsiUpperCase(Result)
end;

function GetLocalUserAppDataPath(): string;
// works at least with IE4 and Win 95 or later
var
   bSuccess: Bool;
   sPath: array[0..Max_Path] of Char;
begin
   sPath := '';

   bSuccess := ShGetSpecialFolderPath(0, sPath, CSIDL_LOCAL_APPDATA, False) ;
   if bSuccess then Result := sPath
   else Result := '';
end;

function CycleDiff(fVal1,fVal2,fCycleVal: Real): Real;
var
  fValL, fValS: Real;
begin
  Result := 0;

  if(fVal1 = fVal2) or (fCycleVal <= 0) then
    exit;

  if(fVal1 > fVal2) then
  begin
    fValL := fVal1;
    fValS := fVal2;
  end
  else
  begin
    fValL := fVal2;
    fValS := fVal1;
  end;

  if(fValS > fValL-fCycleVal/2.0) then
    Result := fValL - fValS
  else
    Result := fCycleVal-fValL + fValS;

end;

function GetCitiesList(sCountrySel, sLANG_ID: string; var slCities: TStringList; sAOFileName: string): Boolean;
var
  sLine: string;
  tfCities: TextFile;
  iFieldCnt: Integer;
  iPos, iLen: Integer;
  sCountry, sCity, sVar1, sVar2, sVar3: string;
  rLatDEG, rLatMIN, rLongDEG, rLongMIN: Real;
  slBuffer: TStringList;
  iTimeZone: Integer;
  bMatched: Boolean;
begin
  Result := false;
  iFieldCnt := -1;
  sCountry := '';
  sCity := '';
  rLatDEG := 0;
  rLatMIN := 0;
  rLongDEG := 0;
  rLongMIN := 0;

  if(slCities = nil) then
    exit;

  slBuffer := TStringList.Create;
  slBuffer.Delimiter := ' ';
  bMatched := false;

  AssignFile(tfCities,sAOFileName);

  (*
  if(sLANG_ID = 'DE') then
    AssignFile(tfCities,'CityCoo.txt')
  else
    AssignFile(tfCities,'CityCoo_EN.txt');
  *)

  Reset(tfCities);

  iFieldCnt := 0;
  while not eof(tfCities) do
  begin
    if(iFieldCnt >= 4) then
    begin
      iFieldCnt := 0;
      bMatched := false;
    end;

    Inc(iFieldCnt);

    ReadLn(tfCities,sLine);
    sLine := Trim(sLine);

    case iFieldCnt of
      1: // City, Country
      begin
        iPos := Pos(',',sLine);
        iLen := length(sLine);
        sCountry := Trim(RightStr(sLine,iLen-iPos));
        sCity := Trim(LeftStr(sLine,iPos-1));
        if(sLANG_ID = 'DE') then
        begin
          sCountry := GermanChars(sCountry);
          sCity := GermanChars(sCity);
        end;

        if(sCountrySel = sCountry) then
        begin
          //LB__CITY.Items.Add(sCity);
          bMatched := true;
        end;
      end;
      2: // Latitude
      begin
        if(bMatched) then
        begin
          slBuffer.DelimitedText := sLine;
          sVar1 := AnsiReplaceStr(slBuffer[0],'D','');
          sVar2 := AnsiReplaceStr(slBuffer[1],'M','');
          sVar3 := slBuffer[2];
          if(StrIsNum(true,sVar1)) then rLatDEG := StrToFloat(sVar1);
          if(StrIsNum(true,sVar2)) then rLatMIN := StrToFloat(sVar2);
          if(sVar3 = 'S') then rLatDEG := -1*rLatDEG;
        end;
      end;
      3: // Longitude
      begin
        if(bMatched) then
        begin
          slBuffer.DelimitedText := sLine;
          sVar1 := AnsiReplaceStr(slBuffer[0],'D','');
          sVar2 := AnsiReplaceStr(slBuffer[1],'M','');
          sVar3 := slBuffer[2];
          if(StrIsNum(true,sVar1)) then rLongDEG := StrToFloat(sVar1);
          if(StrIsNum(true,sVar2)) then rLongMIN := StrToFloat(sVar2);
          if(sVar3 = 'W') then rLongDEG := -1*rLongDEG;
        end;
      end;
      4: // Timezone
      begin
        if(bMatched) then
        begin

          if(sLine = 'GMT') then iTimeZone := 0
          else if(sLine = 'MEZ') then iTimeZone := 1
          else if(sLine = 'EST') then iTimeZone := -5
          else if(sLine = 'CST') then iTimeZone := -6
          else if(sLine = 'MST') then iTimeZone := -7
          else if(sLine = 'PST') then iTimeZone := -8
          else if(sLine = 'GMT+1') then iTimeZone := 1
          else if(sLine = 'GMT+2') then iTimeZone := 2
          else if(sLine = 'GMT+3') then iTimeZone := 3
          else if(sLine = 'GMT+4') then iTimeZone := 4
          else if(sLine = 'GMT+5') then iTimeZone := 5
          else if(sLine = 'GMT+6') then iTimeZone := 6
          else if(sLine = 'GMT+7') then iTimeZone := 7
          else if(sLine = 'GMT+8') then iTimeZone := 8
          else if(sLine = 'GMT+9') then iTimeZone := 9
          else if(sLine = 'GMT+10') then iTimeZone := 10
          else if(sLine = 'GMT+11') then iTimeZone := 11

          else if(sLine = 'GMT-1') then iTimeZone := -1
          else if(sLine = 'GMT-2') then iTimeZone := -2
          else if(sLine = 'GMT-3') then iTimeZone := -3
          else if(sLine = 'GMT-4') then iTimeZone := -4
          else if(sLine = 'GMT-5') then iTimeZone := -5
          else if(sLine = 'GMT-6') then iTimeZone := -6
          else if(sLine = 'GMT-7') then iTimeZone := -7
          else if(sLine = 'GMT-8') then iTimeZone := -8
          else if(sLine = 'GMT-9') then iTimeZone := -9
          else if(sLine = 'GMT-10') then iTimeZone := -10
          else if(sLine = 'GMT-11') then iTimeZone := -11

          else
            iTimeZone := 0;

          //if(sCity = 'Augsburg') then ShowMessage('Stop!');

          //if(LB__CITY.Items.IndexOf(sCity) = -1) then
          //  LB__CITY.AddItem(sCity,TEarthLocation.Create(sCountry,sCity,rLatDEG, rLatMIN,rLongDEG,rLongMIN,iTimeZone));
          if (slCities.IndexOf(sCity) = -1) then
            slCities.AddObject(sCity,TEarthLocation.Create(sCountry,sCity,rLatDEG, rLatMIN,rLongDEG,rLongMIN,iTimeZone));

        end;
      end;

    end;

  end;

  slBuffer.Destroy;
  CloseFile(tfCities);

  Result := bMatched;

end;

function GetCountryStringList(sLANG_ID: string; var slCountries: TStringList; sAOFileName: string): Boolean;
var
  iPos, iLen, iFieldCnt: SmallInt;
  sLine: string;
  sCountry: string;
  tfCities: TextFile;
begin
  iFieldCnt := -1;
  Result := false;

  if(slCountries = nil) then
    exit;

  AssignFile(tfCities,sAOFileName);

  (*
  if(sLANG_ID = 'DE') then
  else
    AssignFile(tfCities,'CityCoo_EN.txt');
  *)
  Reset(tfCities);
  iFieldCnt := 0;
  while not eof(tfCities) do
  begin
    if(iFieldCnt >= 4) then
    begin
      iFieldCnt := 0;
    end;

    Inc(iFieldCnt);

    ReadLn(tfCities,sLine);
    if(iFieldCnt = 1) then
    begin
      iPos := Pos(',',sLine);
      iLen := length(sLine);
      sCountry := Trim(RightStr(sLine,iLen-iPos));
      if(sLANG_ID = 'DE') then
        sCountry := GermanChars(sCountry);

      if(slCountries.IndexOf(sCountry) = -1) then
        slCountries.Add(sCountry);
    end;

  end; // while

  CloseFile(tfCities);

  Result := (iFieldCnt > -1);

end;

function GetDECYear(dtDateTime: TDateTime): Real;
var
  iYear: Integer;
  fYearDays, fDays: Real;
begin
  iYear := YearOf(dtDateTime);
  fYearDays := EncodeDate(iYear+1, 1, 1) - EncodeDate(iYear, 1, 1);
  //fYearDays := EncodeDate(iYear, 12, 31) - EncodeDate(iYear, 1, 1);
  fDays := dtDateTime - EncodeDate(iYear, 1, 1);

  Result := iYear + 1.0*fDays/fYearDays;

end;

(*
function GetVK_ESCAPE(): Word;
begin
  {$IFDEF Windows}
  Result := VK_ESCAPE;
  {$ENDIF}
  {$IFDEF Darwin}
  Result := 0; // VK_ESCAPE???; // https://www.delphipraxis.net/6963-getrvalue-getbvalue-und-getgvalue.html
  {$ENDIF}
  {$IFDEF UNIX}
  Result := 0; // VK_ESCAPE???;
  {$ENDIF}
end;

function GetVK_F5(): Word;
begin
  {$IFDEF Windows}
  Result := VK_F5;
  {$ENDIF}
  {$IFDEF Darwin}
  Result := 0; // VK_F5???; // https://www.delphipraxis.net/6963-getrvalue-getbvalue-und-getgvalue.html
  {$ENDIF}
  {$IFDEF UNIX}
  Result := 0; // VK_F5???;
  {$ENDIF}
end;
*)

procedure ExecOpen(sURL: string);
begin
 {$IFDEF Windows}
 ShellExecute(0, 'open', PChar(sURL), nil, nil, 0); //SW_SHOW);
 {$ENDIF Windows}
 {$IFDEF Darwin}
 LCLIntf.OpenDocument(sURL);
 {$ENDIF Darwin}
 {$IFDEF LINUX}
 LCLIntf.OpenDocument(sURL);
 {$ENDIF LINUX}

end;

function GetRGB(R,G,B: BYTE): TColor;
begin
  {$IFDEF Windows}
  Result := RGB(R,G,B);
  {$ENDIF Windows}
  {$IFDEF Darwin}
  Result := RGB(R,G,B); // https://www.delphipraxis.net/6963-getrvalue-getbvalue-und-getgvalue.html
  {$ENDIF Darwin}
  {$IFDEF LINUX}
  Result := RGB(R,G,B);
  {$ENDIF LINUX}
end;

function GetRVal(rgb: LongInt): BYTE;
begin
  {$IFDEF Windows}
  Result := GetRValue(rgb);
  {$ENDIF Windows}
  {$IFDEF Darwin}
  Result := Integer((rgb)); // https://www.delphipraxis.net/6963-getrvalue-getbvalue-und-getgvalue.html
  {$ENDIF Darwin}
  {$IFDEF LINUX}
  Result := Integer((rgb));
  {$ENDIF LINUX}

end;

function GetGVal(rgb: LongInt): BYTE;
begin
  {$IFDEF Windows}
  Result := GetGValue(rgb);
  {$ENDIF Windows}
  {$IFDEF Darwin}
  Result := Integer((rgb shr 8));
  {$ENDIF Darwin}
  {$IFDEF LINUX}
  Result := Integer((rgb shr 8));
  {$ENDIF LINUX}
end;

function GetBVal(rgb: LongInt): BYTE;
begin
  {$IFDEF Windows}
  Result := GetBValue(rgb);
  {$ENDIF Windows}
  {$IFDEF Darwin}
  Result := Integer((rgb shr 16));
  {$ENDIF Darwin}
  {$IFDEF LINUX}
  Result := Integer((rgb shr 16));
  {$ENDIF LINUX}
end;

{$IFDEF DELPHI10}
function GetTimeZone(DateTime: TDateTime): Integer;
var
  TimeZone: TTimeZone;
begin
  TimeZone := TTimeZone.Create;
  Result := TimeZone.Local.GetUtcOffset(DateTime).Hours;
  TimeZone.Free;
end;
{$ENDIF}

function SetDE_ENMonthNames(sText,sLANG_ID: string): string;
begin
  Result := sText;

  if(sLANG_ID = 'DE') then
  begin
    if(AnsiContainsStr(sText,'January')) then
      Result := AnsiReplaceStr(sText,'January','Januar');
    if(AnsiContainsStr(sText,'February')) then
      Result := AnsiReplaceStr(sText,'February','Februar');
    if(AnsiContainsStr(sText,'March')) then
      Result := AnsiReplaceStr(sText,'March','März');

    if(AnsiContainsStr(sText,'May')) then
      Result := AnsiReplaceStr(sText,'May','Mai');
    if(AnsiContainsStr(sText,'June')) then
      Result := AnsiReplaceStr(sText,'June','Juni');
    if(AnsiContainsStr(sText,'July')) then
      Result := AnsiReplaceStr(sText,'July','Juli');

    if(AnsiContainsStr(sText,'October')) then
      Result := AnsiReplaceStr(sText,'October','Oktober');

    if(AnsiContainsStr(sText,'December')) then
      Result := AnsiReplaceStr(sText,'December','Dezember');
  end
  else
  begin
    if(AnsiContainsStr(sText,'Januar')) then
      Result := AnsiReplaceStr(sText,'Januar','January');
    if(AnsiContainsStr(sText,'Februar')) then
      Result := AnsiReplaceStr(sText,'Februar','February');
    if(AnsiContainsStr(sText,'März')) then
      Result := AnsiReplaceStr(sText,'März','March');

    if(AnsiContainsStr(sText,'Mai')) then
      Result := AnsiReplaceStr(sText,'Mai','May');
    if(AnsiContainsStr(sText,'Juni')) then
      Result := AnsiReplaceStr(sText,'Juni','June');
    if(AnsiContainsStr(sText,'Juli')) then
      Result := AnsiReplaceStr(sText,'Juli','July');

    if(AnsiContainsStr(sText,'Oktober')) then
      Result := AnsiReplaceStr(sText,'Oktober','October');

    if(AnsiContainsStr(sText,'Dezember')) then
      Result := AnsiReplaceStr(sText,'Dezember','December');
  end;

end;

Procedure DrawPlanetEllipse(Image: TImage; SHP: TShape; rLAngle: Single; sLabel: string;
                      Dest: TCanvas;
                      xCenter, yCenter: Integer;         // mittelpunkt x,y
                      xRadius, yRadius: Integer; // breite, höhe
                      Inclination: Single; /// Orbit inclination
                      sType: string;
                      RotateAngle: Single = 0;  // winkel (0° bis 360°) drehung im uhrzeigersinn
                      sAngle: Single = 0;       // ARC start winkel
                      eAngle: Single = 360;     // ARC end winkel
                      Precision: Integer = 2500);    // genauigkeit
Var
  rAngle, step: Single;
  x1, y1, i: Integer;
  rSizeProz: Real;
  iSize, iSize0: Integer;

  Procedure GetPos(Angle: Single; Out x1, y1: Integer);
  Var
    x, y: Real;
    Theta: Single;
  Begin
    Theta := DegToRad(Angle); // -Angle: Laufrichtung der Planeten entgegen dem Uhrzeigersinn, daher umgekehrte Winkelzählung!
    x := xRadius*Sin(Theta);
    y := yRadius*Cos(Theta);
    x1 := Round(xCenter-x*Cos(rAngle)+y*Sin(rAngle));
    y1 := Round(yCenter-x*Sin(rAngle)-y*Cos(rAngle));
  End;

begin
  iSize := 10;

  rLAngle := rLAngle + RotateAngle; // Seems to be valid only for planets / non-inclinated orbits!
  // Modified by INCLINATION???
  //rLAngle := rLAngle + Inclination*0.9; // Linear approximation term
  //rLAngle := rLAngle + Inclination*5; // Linear approximation term

  if(sType = 'C') and (Inclination > 10) then
    rLAngle := rLAngle + 52; // inclination correction term

  if(rLAngle > 360) then rLAngle := rLAngle - 360;

  rAngle:=DegToRad(RotateAngle);

  step:=(eAngle-sAngle)/Precision;
  GetPos(sAngle,x1,y1);
  Dest.MoveTo(x1,y1);
  sAngle:=sAngle+step;

  // Plot inner images or shapes of planets and asteroids with smaller size
  if(sType <> 'C') then
  begin
    if(Uppercase(sLabel) = 'SATURN') then
      iSize0 := 40
    else
      iSize0 := 20;

    rSizeProz := 2*sqrt((x1-xCenter)*(x1-xCenter) + (y1-yCenter)*(y1-yCenter))/(1.0*Dest.Height);
    if(rSizeProz > 0.2) then
      iSize := iSize0
    else
      iSize := iSize0 - (iSize0 - Trunc(50*rSizeProz));

    //if(iSize < 16) then
    //  ShowMessage('Linesize: ' + IntToStr(iSize) + ', Planet: ' + sLabel);

    if(Image <> nil) then
    begin
      Image.Width := iSize;
      Image.Height:= iSize;
    end;
  end;

  if(iSize > 2) then
  begin
    While (sAngle<eAngle) Do
    Begin
      GetPos(sAngle,x1,y1);
      Dest.LineTo(x1,y1);

      // Draw solar object
      if(rLAngle >= sAngle-step) and (rLAngle <= sAngle) and (x1 > 0) and (x1 < Dest.Width) and (y1 > 0) and (y1 < Dest.Height) then
      begin
        // Plot picture of the solar object
        if(Image <> nil) then
        begin
          Image.Left := x1 - (Image.Width div 2);
          Image.Top := y1 - (Image.Height div 2);
          Image.Visible:=true;
        end
        else if(SHP <> nil) then
        begin
          SHP.Left := x1 - (SHP.Width div 2);
          SHP.Top := y1 - (SHP.Height div 2);
          SHP.Visible:=true;
        end
        else
        begin
          for i:=4 downto 0 do
            Dest.Ellipse(x1-i,y1-i,x1+i,y1+i);
        end;

        if(sLabel <> '') then
        begin
          Dest.TextOut(x1+10,y1,sLabel);
          Dest.MoveTo(x1,y1);
        end;
      end;

      sAngle:=sAngle+step;
    End;
    GetPos(eAngle,x1,y1);
    Dest.LineTo(x1,y1);
  end;

End;

Procedure DrawEllipse(Dest: TCanvas;
                      xCenter, yCenter,         // mittelpunkt x,y
                      xRadius, yRadius: Single; // breite, höhe
                      RotateAngle: Single = 0;  // winkel (0° bis 360°) drehung im uhrzeigersinn
                      sAngle: Single = 0;       // ARC start winkel
                      eAngle: Single = 360;     // ARC end winkel
                      Precision: Byte = 60);    // genauigkeit
Var
  rAngle, step: Single;
  x1, y1: Integer;

  Procedure GetPos(Angle: Single; Out x1, y1: Integer);
  Var x, y, Theta: Single;
  Begin
    Theta:=DegToRad(Angle);
    x:=xRadius*Sin(-Theta);
    y:=yRadius*Cos(-Theta);
    x1:=Round(xCenter-x*Cos(rAngle)+y*Sin(rAngle));
    y1:=Round(yCenter-x*Sin(rAngle)-y*Cos(rAngle));
  End;

begin
  rAngle:=DegToRad(RotateAngle);
  step:=(eAngle-sAngle)/Precision;
  GetPos(sAngle,x1,y1);
  Dest.MoveTo(x1,y1);
  sAngle:=sAngle+step;
  While sAngle<eAngle Do Begin
    GetPos(sAngle,x1,y1);
    Dest.LineTo(x1,y1);
    sAngle:=sAngle+step;
  End;
  GetPos(eAngle,x1,y1);
  Dest.LineTo(x1,y1);
End;

function HourToHHMMStr(rHour: Real): string;
begin
  Result := format('%.2d',[Trunc(rHour)]) + ':' + format('%.2d',[Round((rHour-Trunc(rHour))*60)])
end;

function DaysOfMonth(dtDate: TDateTime): Word;
var
  iYear,iMonth,iDay: Word;
  dtNextMonthDay: TDateTime;
begin
  DecodeDate(dtDate,iYear,iMonth,iDay);
  if(iMonth < 12) then
    dtNextMonthDay := EncodeDate(iYear,iMonth+1,1)
  else
    dtNextMonthDay := EncodeDate(iYear+1,1,1);

  Result := Trunc(dtNextMonthDay - EncodeDate(iYear,iMonth,1));

end;

function GetTimeZoneName(iLat_DEG, iLng_DEG: Integer): string;
begin
  Result := 'MESZ';
  if(iLng_DEG > -160) and (iLng_DEG < -60) then
  begin
    // USA
    if(iLat_DEG > 24) and (iLat_DEG < 50) then
      Result := 'USA';
  end;

end;

function CalcMoonRiseAndMoonSet(
  dtWT: TDateTime;
  iDST_HH,iUTC_HH,iGLng_DEG,iGLng_MIN: Integer;
  rSin_fGLat, rCos_fGLat: Real;
  var dtTimeMR: TDateTime;
  var dtTimeMS: TDateTime;
  var dtTimeCul: TDateTime;
  var rHgt_Max: Real): string;
{2013-03-17/fs
Calculates the time of moon rise, moon culmination and moon set of a given date
}
var
  iRA_HH, iRA_MM, iRA_SS: Word;
  iDEC_DEG: SmallInt;
  iDEC_MM, iDEC_SS: SmallInt;
  iHA_HH,iHA_MM,iHA_SS,iHA_MS: Word;
  rHgt_Prev, rAz, rHgt: Real;
  //rR, rDX, rDY: Real;
  dtST, dtRA, dtHA: TDateTime;
  dJulDat: Double;
  //iX0, iY0, iR0, iMag: Integer;
  rRA_SS: Real;
  rDEC_SS: Real;
  rLambdaSun, rMSun: Real;
  dtTime: TDateTime;
  //dtTimeStart, dtTimeEnd: TDateTime;
  iDay, iHH, iMM: Integer;
  bFound: Boolean;
  sErr: string;
begin
  rLambdaSun := 0;
  rMSun := 0;

  rAz:=0; rHgt:=0;

  iRA_HH:=0; iRA_MM:=0; iRA_SS:=0;
  iDEC_DEG:=0; iDEC_MM:=0; iDEC_SS:=0;
  iHA_HH:=0;iHA_MM:=0;iHA_SS:=0;iHA_MS:=0;

  dJulDat := 0;

  //dtWT := GetWTime();

  //DeCodeTime(dtWT,iWT_HH, iWT_MM, iWT_SS, iWT_MS);

  iDay := Trunc(dtWT);
  bFound := false;
  iHH := 0;
  dtTimeMR := 0; dtTimeMS := 0;
  rHgt_Max := -999;
  rHgt_Prev := -999;
  sErr := '';

  try
    while (not bFound) and (iHH < 24) do
    begin
      iMM := 0;
      while (not bFound) and (iMM < 60) do
      begin
        dtTime := iDay + EncodeTime(iHH, iMM, 0, 00);
        dtST := GetSIDTime(dtTime,iDST_HH,iUTC_HH,iGLng_DEG,iGLng_MIN,dJulDat);

        GetSunCoo(dtTime,
          iDST_HH,iUTC_HH, rLambdaSun, rMSun,
          iRA_HH, iRA_MM, rRA_SS,
          iDEC_DEG, iDEC_MM, rDEC_SS);

        GetMoonCoo(dtTime,
          iDST_HH,iUTC_HH,rLambdaSun,rMSun,
          iRA_HH, iRA_MM, rRA_SS,
          iDEC_DEG, iDEC_MM, rDEC_SS);

        iRA_SS := Trunc(rRA_SS);
        iDEC_SS := Trunc(rDEC_SS);

        dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS, Trunc(1000*(rRA_SS-iRA_SS)));
        dtHA := GetHA(dtST,dtRA);

        DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);
        CalcAZCoo(iDEC_DEG,iDEC_MM,iDEC_SS,iHA_HH,iHA_MM,iHA_SS,
          rSin_fGLat, rCos_fGLat,
          rAz,rHgt);

        if(rHgt_Prev <> -999) then
        begin
          if(rHgt >= 0) and (rHgt_Prev < 0) then // Moon Rise
            dtTimeMR := dtTime;// + 1/24; // Vollständig aufgegangen: + 1h HUMBUG! 03.07.2017/fs

          if(rHgt < 0) and (rHgt_Prev >= 0) then // Moon Set
            dtTimeMS := dtTime;

          if(rHgt > 0) and (rHgt > rHgt_Max) then // Moon Culmination
          begin
            rHgt_Max := rHgt;
            dtTimeCul := dtTime;
          end;
        end;

        //bFound := (dtTimeMR > 0) and (dtTimeMS > 0);

        rHgt_Prev := rHgt;
        iMM := iMM + 5;
      end; // while iMM..

      Inc(iHH);
    end; // while iHH..

    if(dtTimeMS < dtTimeMR) then dtTimeMS := dtTimeMS + 1;
  except
    on e: Exception do
    begin
      sErr := e.Message;
    end;
  end;
  //ShowMessage('Moon Rise: ' + DateTimeToStr(dtTimeMR) + ', Moon Set: ' + DateTimeToStr(dtTimeMS));
  //ShowMessage('Moon Culmination: ' + FloatToStr(rHgt_Max) + ' Degree, Time: ' + DateTimeToStr(dtTimeCul));

  Result := sErr;
end;

{function CalcMoonOfMonth
Returns the dates of full moon, new moon and blie bloon of a given month
}
procedure CalcMoonOfMonth(dtDate: TDateTime;
  var iFullMoon: Word;
  var iNewMoon: Word;
  var iBlueMoon: Word);
var
  iDays, i, iHH: Integer;
  iYear, iMonth, iD: Word;
  dtDateMonth: TDateTime;
  iMoonPhase, iMoonPhaseMax, iMoonPhaseMin: Integer;
begin
  iMoonPhaseMax := -1;
  iMoonPhaseMin := 999;

  iBlueMoon := 0;

  DecodeDate(dtDate,iYear,iMonth,iD);
  iDays := DaysInAMonth(iYear,iMonth);
  for i := 1 to iDays do
  begin
    if(i = 28) then
      iMoonPhaseMax := -1; // Reset preparation for bluemoon

    for iHH := 1 to 6 do
    begin
      dtDateMonth := EncodeDateTime(iYear,iMonth,i,(iHH-1)*(24 div 6),0,0,0);

      iMoonPhase := CalcMoonPhase(dtDateMonth);
      iMoonPhase := abs(iMoonPhase - 50);

      if(iMoonPhase > iMoonPhaseMax) and (i < 28) then
      begin
        iMoonPhaseMax := iMoonPhase;
        iFullMoon := DayOfTheMonth(dtDateMonth);
      end;

      if(iMoonPhase > iMoonPhaseMax) and (i >= 28) then
      begin
        iMoonPhaseMax := iMoonPhase;
        if(iMoonPhase > 48) then
          iBlueMoon := DayOfTheMonth(dtDateMonth);
      end;

      if(iMoonPhase < iMoonPhaseMin) then
      begin
        iMoonPhaseMin := iMoonPhase;
        iNewMoon := DayOfTheMonth(dtDateMonth);
      end;

    end;

  end;

  // Avoid BM directly after FM in some special cases (e.g. January 2021)
  if(iBlueMoon > 0) and (iBlueMoon - iFullMoon = 1) then
  begin
    iFullMoon := iBlueMoon;
    iBlueMoon := 0;
  end;

end;

procedure EquToAZCoo(dtDateTime: TDateTime;
  iDST_HH,iUTC_HH,
  iGLng_DEG,iGLng_MIN: SmallInt;
  rSin_fGLat, rCos_fGLat: Real;
  iDEC_DEG, iDEC_MM: SmallInt; rDEC_SS: Real;
  iRA_HH, iRA_MM: SmallInt; rRA_SS: Real;
  var rAz: Real; var rHgt: Real
  );
var
  iRA_SS: Integer;
  dtRA, dtHA, dtST, dJulDat: TDateTime;
  iHA_HH,iHA_MM,iHA_SS,iHA_MS: Word;
begin
  dJulDat := 0;
  dtST := GetSIDTime(dtDateTime,iDST_HH,iUTC_HH,iGLng_DEG,iGLng_MIN,dJulDat);

  iRA_SS := Trunc(rRA_SS);

  dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS, Trunc(1000*(rRA_SS-iRA_SS)));
  dtHA := GetHA(dtST,dtRA);

  DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);

  CalcAZCoo(iDEC_DEG,iDEC_MM,rDEC_SS,iHA_HH,iHA_MM,iHA_SS,
    rSin_fGLat, rCos_fGLat,
    rAz,rHgt);

end;

procedure CalcAZCoo(iDEC_DEG, iDEC_MIN: SmallInt; rDEC_SS: Real; iHA_HH, iHA_MIN, iHA_SS: SmallInt;
  rSin_fGLat, rCos_fGLat: Real;
  var fAz: Real; var fHgt: Real);
var
  fDEC, fHA: Real;
  fZ, fN: Real;
  iSign: SmallInt;
  rEpsilon: Real;
begin
  rEpsilon := 0.001;

  iSign := 1;

  if(iDEC_DEG < 0) then
    iSign := -1
  else if(iDEC_DEG = 0) and (iDEC_MIN < 0) then
    iSign := -1
  else if(iDEC_DEG = 0) and (iDEC_MIN = 0) and (rDEC_SS < 0) then
    iSign := -1;

  iDEC_DEG := abs(iDEC_DEG);
  iDEC_MIN := abs(iDEC_MIN);
  rDEC_SS := abs(rDEC_SS);

  fDEC := iSign*Pi*(iDEC_DEG + (iDEC_MIN + rDEC_SS/60.0)/60.0)/180.0;

  fHA := Pi*(iHA_HH + (iHA_MIN + iHA_SS/60.0)/60.0)/12.0;

  (* http://de.wikibooks.org/wiki/Astronomische_Berechnungen_f%C3%BCr_Amateure/_Positionsastronomie/_Koordinatentransformationen

      Z > 0, N > 0 : A liegt im ersten Quadranten, dh. 0° ≤ A ≤ 90°; A = arctan(Z/N)

      Z > 0, N < 0 : A liegt im zweiten Quadranten, dh. 90° < A ≤ 180°; A = 180° + arctan(Z/N)

      Z < 0, N < 0 : A liegt im dritten Quadranten, dh. 180° < A ≤ 270°; A = 180° + arctan(Z/N)

      Z < 0, N > 0 : A liegt im vierten Quadranten, dh. 270° < A < 360° ; A = 360° + arctan(Z/N)

  *)

  //fAz :=  arctan(sin(fHA)/(cos(fHA)*sin(fGLat)- tan(fDEC)*cos(fGLat) ));
  // arctan returns values between -pi/2 and +pi/2 ONLY -> use cases fZ, fN.

  fZ := sin(fHA);
  fN := cos(fHA)*rSin_fGLat - tan(fDEC)*rCos_fGLat;
  fAz := arctan360(fZ,fN);

  (*
  if(fHA = 0) then
    fAz := 180  // .. means 0° (North)
  else if(abs(fHA * 12.0/Pi - 12) < 0.00001) then
    fAz := 0    // means 180° (South)
  else
  *)

  fHgt := arcsin(cos(fDEC)*cos(fHA)*rCos_fGLat + sin(fDEC)*rSin_fGLat);
  fHgt := 180.0*fHgt/Pi;

  // Consider special calculation deviations with an epsilon band (e.g. vertical coordinate lines in W)
  if(abs(fHgt) < rEpsilon) then
    fHgt := 0
  else if(abs(fHgt - 180) < rEpsilon) then
    fHgt := 180;

end;

function GetHA(dtST,dtRA: TDateTime): TDateTime;
{2012-09-15/fs
Calculation Hour Angle
}
begin
  if(dtST < dtRA) then
    dtST := dtST + 1.0;

  Result := dtST - dtRA;
end;

function GetDecimalSeparator(): string;
{$IFDEF DELPHI10}
var FS: TFormatSettings;
begin
  FS := TFormatSettings.Create('de-DE');
  Result := FS.DecimalSeparator;
end;
{$ENDIF}
{$IFDEF LAZARUS}
begin
  Result := DefaultFormatSettings.DecimalSeparator; //DecimalSeparator;
end;
{$ENDIF}


procedure HMouseCooToHorizon(iWidth, iHeight: Integer; sHMode: string; rMouseX, rMouseY, rEyeFacH: Real; var rAz: Real; var rAlt: Real);
{
S: 0, W: 90, N: 180, O: 270
}
begin
  rAlt := rEyeFacH*Pi/2*(iHeight-rMouseY)/iHeight;
  //rAlt := arcsin((iHeight - rMouseY)/iHeight);

  if(sHMode = 'S') then
  begin
    if(rMouseX < iWidth / 2) then // O
      rAz := 3*Pi/2 + Pi/2*rMouseX/(iWidth div 2)
    else
      rAz := Pi/2*(rMouseX - (iWidth / 2))/(iWidth - (iWidth / 2));
  end
  else if (sHMode = 'N') then
  begin
    rAz := Pi/2 + Pi*(rMouseX)/(iWidth);
  end
  else if (sHMode = 'O') then
  begin
    rAz := Pi + Pi*(rMouseX)/(iWidth);
  end
  else if (sHMode = 'W') then
  begin
    rAz := Pi*(rMouseX)/(iWidth);
  end;

  rAlt := rAlt *180/Pi;
  rAz := rAz *180/Pi;

end;

procedure MouseCooToHorizon(iX0, iY0, iR0: Integer; rMouseX, rMouseY: Real; var rAz: Real; var rAlt: Real);
var
  rR: Real;
begin
  rR := sqrt((rMouseX-iX0)*(rMouseX-iX0) + (rMouseY-iY0)*(rMouseY-iY0));
  if(rR > iR0) then
  begin
    rAz := -1;
    rAlt := -1;
    exit;
  end;

  if(rMouseY <> iY0) then
  begin
    if(rMouseX <> iX0) then
      rAz := arctan360((rMouseX-iX0),(rMouseY-iY0))
    else
    begin
      if(rMouseY < iY0) then
        rAz := 0
      else
        rAz := 180;

    end;
  end
  else
  begin
    if(rMouseX > iX0) then
      rAz := 270
    else
      rAz := 90;
  end;

  rAlt := (Pi/2.0 * (1.0 - rR/iR0)) *180/Pi;
  //rAlt := arccos(rR/iR0) *180/Pi;
  //rAlt := (Pi/2 - arcsin(rR/iR0)) *180/Pi;
end;

function IsNumeric(sValue: string): Boolean;
var
  i: Integer;
  bIsAlphaNum: Boolean;
begin
  Result := false;
  bIsAlphaNum := false;

  i:=1;
  while (not  bIsAlphaNum) and (i <= Length(sValue)) do
  begin
    if(sValue[i] <> 'e') and (sValue[i] <> 'E') then
    begin
      bIsAlphaNum := (sValue[i] in ['a'..'z','A'..'Z',
        '*','#','~','?','=','!','"','%','&','/','(',')','{','}','[',']',
        '-','_',';',':']);
    end;
    Inc(i);
  end;

  Result := not bIsAlphaNum;

end;

function StrToFloatExt2(sValue: string): Real;
var
  i: Integer;
  bIsAlphaNum: Boolean;
begin
  Result := -999;
  bIsAlphaNum := false;

  if(Trim(sValue) = '') then exit;

  i:=1;
  while (not  bIsAlphaNum) and (i <= Length(sValue)) do
  begin
   bIsAlphaNum := (sValue[i] in ['a'..'z','A'..'Z']);
   Inc(i);
  end;

  if(bIsAlphaNum) then exit;

  sValue := AnsiReplaceStr(sValue,'.',GetDecimalSeparator());
  sValue := AnsiReplaceStr(sValue,',',GetDecimalSeparator());

  Result := StrToFloat(sValue);

end;

function StrToFloatExt3(sValue: string; var bIsAlphaNum: Boolean): Real;
var
  i, iLen: Integer;
begin
  Result := 0;
  bIsAlphaNum := false;
  iLen := length(sValue);

  if(Trim(sValue) = '') then
  begin
    bIsAlphaNum := true;
  end;

  if(iLen = 1) then
  begin
    //if(sValue[1] in ['a'..'z','A'..'Z']) or (sValue[1] = '+') or (sValue[1] = '-') or (sValue[1] = ',') or (sValue[1] = '.') then // This kills negative values!!! (E.g. 'Sirius A' magnitude)
    if(sValue[1] in ['a'..'z','A'..'Z']) or (sValue[1] = ',') or (sValue[1] = '.') then
      bIsAlphaNum := true;
  end;

  if(bIsAlphaNum) then
    exit;

  i:=1;
  while (not  bIsAlphaNum) and (i <= iLen) do
  begin
   bIsAlphaNum := (sValue[i] in ['a'..'z','A'..'Z']);
   if(not bIsAlphaNum) then
     bIsAlphaNum := (sValue[i] in ['!','"','$','%','&','/','(',')','=','?','*','+','~','''','#','_',':',';','>','<','|','^',' ','@','{','}','[',']','\']);

   // Check for E+-NUMBER
   if(i < Length(sValue)) then
   begin
     if(bIsAlphaNum) and ((sValue[i] = 'e') or (sValue[i] = 'E')) and ((sValue[i+1] = '-') or (sValue[i+1] = '+') or (sValue[i+1] in ['0'..'9'])) then
       bIsAlphaNum := false;
   end;

   Inc(i);
  end;

  //if(sValue[iLen] in ['a'..'z','A'..'Z']) or (sValue[1] = '+') or (sValue[1] = '-') or (sValue[1] = ',') or (sValue[1] = '.') then
  if(sValue[iLen] in ['a'..'z','A'..'Z']) or (sValue[1] = ',') or (sValue[1] = '.') then
    bIsAlphaNum := true;

  if(bIsAlphaNum) then
    exit;

  sValue := AnsiReplaceStr(sValue,'.',GetDecimalSeparator());
  sValue := AnsiReplaceStr(sValue,',',GetDecimalSeparator());

  Result := StrToFloat(sValue);

end;

function StrToFloatExt4(sValue: string): Real;
var
  i, iLen: Integer;
  bIsAlphaNum: Boolean;
begin
  Result := 0;
  bIsAlphaNum := false;
  iLen := length(sValue);

  if(Trim(sValue) = '') then
  begin
    bIsAlphaNum := true;
  end;

  if(iLen = 1) then
  begin
    //if(sValue[1] in ['a'..'z','A'..'Z']) or (sValue[1] = '+') or (sValue[1] = '-') or (sValue[1] = ',') or (sValue[1] = '.') then
    if(sValue[1] in ['a'..'z','A'..'Z']) or (sValue[1] = ',') or (sValue[1] = '.') then
      bIsAlphaNum := true;
  end;

  if(bIsAlphaNum) then
    exit;

  i:=1;
  while (not  bIsAlphaNum) and (i <= iLen) do
  begin
   bIsAlphaNum := (sValue[i] in ['a'..'z','A'..'Z']);
   if(not bIsAlphaNum) then
     bIsAlphaNum := (sValue[i] in ['!','"','$','%','&','/','(',')','=','?','*','~','''','#','_',':',';','>','<','|','^',' ','@','{','}','[',']','\']);

   // Check for E+-NUMBER
   if(i < Length(sValue)) then
   begin
     if(bIsAlphaNum) and ((sValue[i] = 'e') or (sValue[i] = 'E')) and ((sValue[i+1] = '-') or (sValue[i+1] = '+') or (sValue[i+1] in ['0'..'9'])) then
       bIsAlphaNum := false;
   end;

   Inc(i);
  end;

  //if(sValue[iLen] in ['a'..'z','A'..'Z']) or (sValue[1] = '+') or (sValue[1] = '-') or (sValue[1] = ',') or (sValue[1] = '.') then
  if(sValue[iLen] in ['a'..'z','A'..'Z']) or (sValue[1] = ',') or (sValue[1] = '.') then
    bIsAlphaNum := true;

  if(bIsAlphaNum) then
    exit;

  sValue := AnsiReplaceStr(sValue,'.',GetDecimalSeparator());
  sValue := AnsiReplaceStr(sValue,',',GetDecimalSeparator());

  Result := StrToFloat(sValue);

end;

function GetMonth(dtDateTime: TDateTime): Word;
var
iYear, iMonth, iDay: word;
begin
  DecodeDate(dtDateTime,iYear,iMonth,iDay);

  Result := iMonth;
end;

procedure CalcSunRiseAndSet(
  iGLng_DEG, iGLng_MIN,
  iGLat_DEG, iGLat_MIN,
  iUTC_HH,
  iDay, iDST_HH, iDawn_MIN: Integer; var rSunRise_HH: Real; var rSunSet_HH: Real);
{2012/fs
Calculation of Sun Rise and Sun Set
Ref.:
http://lexikon.astronomie.info/zeitgleichung/
rSunRise_HH: Sun Rise according to the angle iDawn_MIN (reaches from lower values)
rSunSet_HH: Sun Set according to the angle iDawn_MIN (reaches from higher values)
}
var
  rDec, rH, rB: Real;
  rArg: Real;
  rTimeEqu, rTimeDiff: Real;
  rRise, rSet: Real;
//const
  //ciDawn_MIN = -50;
begin
  rH := iDawn_MIN/60.0 * Pi/180;
  rB := (iGLat_DEG + iGLat_MIN/60.0)*Pi/180;

  // Declination of the Sun
  rDec := 0.4095*sin(0.016906*(iDay-80.086));

  // Zeitgleichung
  rTimeEqu := -0.171*sin(0.0337 * iDay + 0.465) - 0.1299*sin(0.01787 * iDay - 0.168);

  rArg := (sin(rH) - sin(rB)*sin(rDec)) / (cos(rB)*cos(rDec));

  if(abs(rArg) <= 1) then
  begin
    rTimeDiff := 12*arccos(rArg)/Pi;

    rRise := 12 - rTimeDiff - rTimeEqu;
    rSet := 12 + rTimeDiff - rTimeEqu;

    rSunRise_HH := rRise - (iGLng_DEG + iGLng_MIN/60.0)/15.0 + iUTC_HH + iDST_HH;
    rSunSet_HH := rSet - (iGLng_DEG + iGLng_MIN/60.0)/15.0 + iUTC_HH + iDST_HH;
  end
  else
  begin
    rSunRise_HH := -1;
    rSunSet_HH := -1;
    //rSunRise_HH := 0;
    //rSunSet_HH := 24;
  end;

end;

function GetDeltaKepler(rEx, rM: Real): Real;
{26.05.2013/fs
Returns correction value for larger rEx values
}
begin
  Result := 0;

  if(rEx <= 0.1) or (rM >= 6.25) or ((rM >= 3.0) and (rM <= 3.2)) then
    exit;

  if(rEx > 0.9) then
  begin
    if(rM > 6.125) then begin Result := -0.8; exit; end;
    if(rM > 6.00) then begin Result := -0.9; exit; end;
    if(rM > 5.75) then begin Result := -1.0; exit; end;
    if(rM > 5.5) then begin Result := -0.96; exit; end;
    if(rM > 5.25) then begin Result := -0.9; exit; end;
    if(rM > 5.0) then begin Result := -0.86; exit; end;
    if(rM > 4.75) then begin Result := -0.8; exit; end;
    if(rM > 4.5) then begin Result := -0.68; exit; end;
    if(rM > 4.25) then begin Result := -0.55; exit; end;
    if(rM > 4.0) then begin Result := -0.45; exit; end;
    if(rM > 3.75) then begin Result := -0.3; exit; end;
    if(rM > 3.5) then begin Result := -0.2; exit; end;
    if(rM > 3.25) then begin Result := -0.08; exit; end;
    if(rM > 3.0) then begin Result := 0.08; exit; end;
    if(rM > 2.75) then begin Result := 0.2; exit; end;
    if(rM > 2.5) then begin Result := 0.3; exit; end;
    if(rM > 2.25) then begin Result := 0.45; exit; end;
    if(rM > 2.0) then begin Result := 0.55; exit; end;
    if(rM > 1.75) then begin Result := 0.68; exit; end;
    if(rM > 1.5) then begin Result := 0.8; exit; end;
    if(rM > 1.25) then begin Result := 0.86; exit; end;
    if(rM > 1.0) then begin Result := 0.9; exit; end;
    if(rM > 0.75) then begin Result := 0.96; exit; end;
    if(rM > 0.5) then begin Result := 1; exit; end;
    if(rM > 0.25) then begin Result := 0.9; exit; end;
    if(rM > 0.125) then begin Result := 0.8; exit; end;
    if(rM > 0.0625) then begin Result := 0.7; exit; end;
    if(rM > 0.03125) then begin Result := 0.6; exit; end;
    if(rM < 0.01) then begin Result := 0.1; exit; end
    else begin Result := 0.5; exit; end;

  end;

  if(rEx > 0.7) then
  begin
    if(rM > 6.125) then begin Result := -0.5; exit; end;

    if(rM > 6.00) then begin Result := -0.6; exit; end;
    if(rM > 5.75) then begin Result := -0.75; exit; end;
    if(rM > 5.5) then begin Result := -0.8; exit; end;
    if(rM > 5.25) then begin Result := -0.78; exit; end;
    if(rM > 5.0) then begin Result := -0.75; exit; end;
    if(rM > 4.75) then begin Result := -0.66; exit; end;
    if(rM > 4.5) then begin Result := -0.58; exit; end;
    if(rM > 4.25) then begin Result := -0.49; exit; end;
    if(rM > 4.0) then begin Result := -0.38; exit; end;
    if(rM > 3.75) then begin Result := -0.28; exit; end;
    if(rM > 3.5) then begin Result := -0.18; exit; end;
    if(rM > 3.25) then begin Result := -0.06; exit; end;

    if(rM > 3.0) then begin Result := 0.06; exit; end;
    if(rM > 2.75) then begin Result := 0.18; exit; end;
    if(rM > 2.5) then begin Result := 0.28; exit; end;
    if(rM > 2.25) then begin Result := 0.38; exit; end;
    if(rM > 2.0) then begin Result := 0.49; exit; end;
    if(rM > 1.75) then begin Result := 0.58; exit; end;
    if(rM > 1.5) then begin Result := 0.66; exit; end;
    if(rM > 1.25) then begin Result := 0.75; exit; end;
    if(rM > 1.0) then begin Result := 0.78; exit; end;
    if(rM > 0.75) then begin Result := 0.8; exit; end;
    if(rM > 0.5) then begin Result := 0.75; exit; end;
    if(rM > 0.25) then begin Result := 0.6; exit; end;

    if(rM > 0.0) then begin Result := 0.5; exit; end;
  end;

  if(rEx > 0.6) then
  begin
    if(rM > 6.125) then begin Result := -0.25; exit; end;

    if(rM > 6.00) then begin Result := -0.35; exit; end;
    if(rM > 5.75) then begin Result := -0.55; exit; end;
    if(rM > 5.5) then begin Result := -0.6; exit; end;
    if(rM > 5.25) then begin Result := -0.6; exit; end;
    if(rM > 5.0) then begin Result := -0.58; exit; end;
    if(rM > 4.75) then begin Result := -0.55; exit; end;
    if(rM > 4.5) then begin Result := -0.45; exit; end;
    if(rM > 4.25) then begin Result := -0.4; exit; end;
    if(rM > 4.0) then begin Result := -0.3; exit; end;
    if(rM > 3.75) then begin Result := -0.24; exit; end;
    if(rM > 3.5) then begin Result := -0.18; exit; end;
    if(rM > 3.25) then begin Result := -0.05; exit; end;

    if(rM > 3.0) then begin Result := 0.08; exit; end;
    if(rM > 2.75) then begin Result := 0.18; exit; end;
    if(rM > 2.5) then begin Result := 0.25; exit; end;
    if(rM > 2.25) then begin Result := 0.32; exit; end;
    if(rM > 2.0) then begin Result := 0.4; exit; end;
    if(rM > 1.75) then begin Result := 0.48; exit; end;
    if(rM > 1.5) then begin Result := 0.55; exit; end;
    if(rM > 1.25) then begin Result := 0.58; exit; end;
    if(rM > 1.0) then begin Result := 0.6; exit; end;
    if(rM > 0.75) then begin Result := 0.58; exit; end;
    if(rM > 0.5) then begin Result := 0.5; exit; end;
    if(rM > 0.25) then begin Result := 0.35; exit; end;

    if(rM > 0.0) then begin Result := 0.3; exit; end;
  end;

  if(rEx > 0.5) then
  begin
    if(rM > 6.125) then begin Result := -0.2; exit; end;

    if(rM > 6.00) then begin Result := -0.32; exit; end;
    if(rM > 5.75) then begin Result := -0.5; exit; end;
    if(rM > 5.5) then begin Result := -0.58; exit; end;
    if(rM > 5.25) then begin Result := -0.6; exit; end;
    if(rM > 5.0) then begin Result := -0.58; exit; end;
    if(rM > 4.75) then begin Result := -0.54; exit; end;
    if(rM > 4.5) then begin Result := -0.48; exit; end;
    if(rM > 4.25) then begin Result := -0.4; exit; end;
    if(rM > 4.0) then begin Result := -0.32; exit; end;
    if(rM > 3.75) then begin Result := -0.24; exit; end;
    if(rM > 3.5) then begin Result := -0.14; exit; end;
    if(rM > 3.25) then begin Result := -0.05; exit; end;

    if(rM > 3.0) then begin Result := 0.05; exit; end;
    if(rM > 2.75) then begin Result := 0.14; exit; end;
    if(rM > 2.5) then begin Result := 0.24; exit; end;
    if(rM > 2.25) then begin Result := 0.32; exit; end;
    if(rM > 2.0) then begin Result := 0.4; exit; end;
    if(rM > 1.75) then begin Result := 0.48; exit; end;
    if(rM > 1.5) then begin Result := 0.54; exit; end;
    if(rM > 1.25) then begin Result := 0.58; exit; end;
    if(rM > 1.0) then begin Result := 0.6; exit; end;
    if(rM > 0.75) then begin Result := 0.58; exit; end;
    if(rM > 0.5) then begin Result := 0.5; exit; end;
    if(rM > 0.25) then begin Result := 0.32; exit; end;

    if(rM > 0.0) then begin Result := 0.2; exit; end;
  end;

  if(rEx > 0.3) then
  begin
    if(rM > 6.125) then begin Result := -0.08; exit; end;

    if(rM > 6.00) then begin Result := -0.16; exit; end;
    if(rM > 5.75) then begin Result := -0.28; exit; end;
    if(rM > 5.5) then begin Result := -0.35; exit; end;
    if(rM > 5.25) then begin Result := -0.39; exit; end;
    if(rM > 5.0) then begin Result := -0.4; exit; end;
    if(rM > 4.75) then begin Result := -0.38; exit; end;
    if(rM > 4.5) then begin Result := -0.35; exit; end;
    if(rM > 4.25) then begin Result := -0.3; exit; end;
    if(rM > 4.0) then begin Result := -0.24; exit; end;
    if(rM > 3.75) then begin Result := -0.18; exit; end;
    if(rM > 3.5) then begin Result := -0.1; exit; end;
    if(rM > 3.25) then begin Result := -0.04; exit; end;

    if(rM > 3.0) then begin Result := 0.04; exit; end;
    if(rM > 2.75) then begin Result := 0.1; exit; end;
    if(rM > 2.5) then begin Result := 0.18; exit; end;
    if(rM > 2.25) then begin Result := 0.4; exit; end;
    if(rM > 2.0) then begin Result := 0.3; exit; end;
    if(rM > 1.75) then begin Result := 0.35; exit; end;
    if(rM > 1.5) then begin Result := 0.38; exit; end;
    if(rM > 1.25) then begin Result := 0.4; exit; end;
    if(rM > 1.0) then begin Result := 0.39; exit; end;
    if(rM > 0.75) then begin Result := 0.35; exit; end;
    if(rM > 0.5) then begin Result := 0.28; exit; end;
    if(rM > 0.25) then begin Result := 0.16; exit; end;

    if(rM > 0.0) then begin Result := 0.08; exit; end;
  end;

  if(rEx > 0.1) then
  begin
    if(rM > 6.125) then begin Result := -0.05; exit; end;

    if(rM > 6.00) then begin Result := -0.08; exit; end;
    if(rM > 5.75) then begin Result := -0.12; exit; end;
    if(rM > 5.5) then begin Result := -0.15; exit; end;
    if(rM > 5.25) then begin Result := -0.18; exit; end;
    if(rM > 5.0) then begin Result := -0.2; exit; end;
    if(rM > 4.75) then begin Result := -0.2; exit; end;
    if(rM > 4.5) then begin Result := -0.18; exit; end;
    if(rM > 4.25) then begin Result := -0.16; exit; end;
    if(rM > 4.0) then begin Result := -0.14; exit; end;
    if(rM > 3.75) then begin Result := -0.1; exit; end;
    if(rM > 3.5) then begin Result := -0.06; exit; end;
    if(rM > 3.25) then begin Result := -0.02; exit; end;

    if(rM > 3.0) then begin Result := 0.02; exit; end;
    if(rM > 2.75) then begin Result := 0.6; exit; end;
    if(rM > 2.5) then begin Result := 0.1; exit; end;
    if(rM > 2.25) then begin Result := 0.14; exit; end;
    if(rM > 2.0) then begin Result := 0.16; exit; end;
    if(rM > 1.75) then begin Result := 0.18; exit; end;
    if(rM > 1.5) then begin Result := 0.2; exit; end;
    if(rM > 1.25) then begin Result := 0.2; exit; end;
    if(rM > 1.0) then begin Result := 0.18; exit; end;
    if(rM > 0.75) then begin Result := 0.15; exit; end;
    if(rM > 0.5) then begin Result := 0.12; exit; end;
    if(rM > 0.25) then begin Result := 0.08; exit; end;

    if(rM > 0.0) then begin Result := 0.05; exit; end;
  end;

end;

function KeplerSolve(rEx, rM, rEpsilon: Real): Real;
{26.05.2013/fs
Solves Kepler's equation E-esinE = M with accuracy rEpsilon
}
var
  rDelta, rDeltaE: Real;
begin
  rDelta := rEpsilon + 1;
  rDeltaE := 0;
  Result := rM + GetDeltaKepler(rEx,rM);

  while abs(rDelta) > rEpsilon do
  begin
    Result := Result - rDeltaE;
    rDelta := Result - rEx*sin(Result) - rM;
    rDeltaE := rDelta/(1-rEx*cos(Result));
  end;

end;

function GetEyePupil(iYearOfBirth: Integer): Real;
{18.05.2013/fs
Returns the eye pubil in dependence of the age
Source: http://www.der-kosmos.de/berechnungsgrundlagen.htm
}
var
  iYearNow, iAge: Integer;
begin
  Result := 7;

  iYearNow := YearOf(Now);
  iAge := iYearNow - iYearOfBirth;
  case iAge of
    6..25: Result := 8;
    26..35: Result := 7;
    36..45: Result := 6;
    46..55: Result := 5;
    56..65: Result := 4;
    66..75: Result := 3;
    76..120:  Result := 2.3;
  end;

end;

function StrToFloatExt(sVal: string): Real;
begin
  if(Trim(sVal) = '') then
    Result := 0
  else
  begin
    sVal := AnsiReplaceStr(sVal,'.',GetDecimalSeparator());
    sVal := AnsiReplaceStr(sVal,',',GetDecimalSeparator());
    Result := StrToFloat(sVal);
  end;

end;

function StrToTimeMS(sTime, sMSSep: string): TDateTime;
var
  iLen, iPos: Integer;
  sMS, sPre: string;
begin
  iPos := Pos(sMSSep,sTime);
  if(iPos > 0) then
  begin
    iLen := length(sTime);
    sPre := LeftStr(sTime,iPos-1);
    sMS := RightStr(sTime,iLen-iPos);
    if(sMS <> '') then
      Result := StrToTime(sPre) + StrToInt(sMS)/(24*3600*10)
    else
      Result := StrToTime(sPre);
  end
  else
    Result := StrToTime(sTime);

end;

procedure GetGalacticPlaneCoo(rLambda: Real; var rRA: Real; var rDEC: Real);
var
  rAngle: Real;
begin
  rAngle := (rLambda-33.0) *Pi/180;
  rRA := ArcTan360(cos(rAngle),-sin(27.4*Pi/180)*sin(rAngle)) + 192.25;
  rRA := GetNormVal(rRA,360);
  rDEC := arcsin(cos(27.4*Pi/180)*sin(rAngle)) *180/Pi;
end;

procedure GetGalacticPlaneCooExt(rLambda, rBeta: Real; var rRA: Real; var rDEC: Real);
var
  rAngle: Real;
begin
  rBeta := rBeta *Pi/180;
  rAngle := (rLambda-33.0) *Pi/180;
  rRA := ArcTan360(cos(rBeta)*cos(rAngle),sin(rBeta)*cos(27.4*Pi/180) - cos(rBeta)*sin(27.4*Pi/180)*sin(rAngle)) + 192.25;
  rRA := GetNormVal(rRA,360);
  rDEC := arcsin(cos(rBeta)*cos(27.4*Pi/180)*sin(rAngle) + sin(rBeta)*sin(27.4*Pi/180)) *180/Pi;
end;

procedure GetEcpliticCoo(rLambda: Real; dtTime: TDateTime; iDST_HH, iUTC_HH: SmallInt; var rRA: Real; var rDEC: Real);
var
  dtJulDat: TDateTime;
  rEpsilon: Real;
begin
  dtJulDat := GetJulTime(dtTime,iDST_HH,iUTC_HH);
  rEpsilon := EclipticObl(dtJulDat);

  rRA := ArcTan360(sin(rLambda *Pi/180.0)*cos(rEpsilon *Pi/180.0),cos(rLambda *Pi/180.0));
  rDEC := arcsin(sin(rEpsilon *Pi/180.0)*sin(rLambda *Pi/180.0)) *180.0/Pi;
  //if(rDEC < 0) then rDEC := 360.0 + rDEC;

end;

function GetJulTime(dtTimeIn: TDateTime; iDST_HH,iUTC_HH: SmallInt): Double;
begin
  Result := dtTimeIn - (iDST_HH+iUTC_HH)/24.0 - EncodeDateTime(2012,08,17,13,48,46,0) + 2456157.07553;
end;

function GetSIDTime(dtTimeIn: TDateTime;
  iDST_HH, iUTC_HH: SmallInt;
  iGLng_DEG: SmallInt; iGLng_MIN: Word;
  var dJulDat: Double): TDateTime;
var
  iYear, iMonth, iDay, iHour, iMin, iSec, iMSec : Word;
  fGL: Real;
  iHH, iMM, iSS: ShortInt;
  dT, dGMST_0_UT, dGMST, dGMST_L, dGMST_LH: Double;
  dGMST_L_HH, dGMST_L_MM, dGMST_L_SS: Double;
begin
  Result := 0;
  if(dtTimeIn = 0) then dtTimeIn := Now;

  dJulDat := GetJulTime(dtTimeIn,iDST_HH,iUTC_HH);

  dT := (dJulDat - 2451545.0)/36525.0;
  //dGMST_0_UT := 100.46061837 + 36000.770053608*dT + 0.000387933*dT*dT - dT*dT*dT/38710000;
  dGMST_0_UT := 280.46061837 + 360.98564736629*(dJulDat - 2451545.0) + 0.000387933*dT*dT - dT*dT*dT/38710000;

  DecodeDateTime(dtTimeIn,iYear, iMonth, iDay, iHour, iMin, iSec, iMSec);

  //dUT := (1.0*iHour - (iDST_HH+iUTC_HH) + 1.0*iMin/60.0 + 1.0*iSec/3600.0) * 360.0/24.0;

  dGMST := dGMST_0_UT;

  fGL := iGLng_DEG + iGLng_MIN/60.0;

  dGMST_L := dGMST + fGL;

  dGMST_LH := dGMST_L - 360.0*(Trunc(dGMST_L/360.0));

  dGMST_L_HH := dGMST_LH*24.0/360.0;
  dGMST_L_MM := 60.0*(dGMST_L_HH - Trunc(dGMST_L_HH));
  dGMST_L_SS := 60.0*(dGMST_L_MM - Trunc(dGMST_L_MM));

  iHH := Trunc(dGMST_L_HH);
  iMM := Trunc(dGMST_L_MM);
  iSS := Trunc(dGMST_L_SS);

  if(iHH > 23) then iHH := 23;
  if(iMM > 59) then iMM := 59;
  if(iSS > 59) then iSS := 59;

  if(iHH < 0) then iHH := 0;
  if(iMM < 0) then iMM := 0;
  if(iSS < 0) then iSS := 0;

  Result := EncodeTime(iHH,iMM,iSS,0);

end;

function HorizonToEquatorial(rAlt,rAz,rPhi: Real;
  var iHH_HH: Word; var iHH_MM: Word; var rHH_SS: Real;
  var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real): Boolean;
var
  rDEC, rH1, rH: Real;
  rArgDec, rArgH1: Real;
  rEps: Real;
begin
  // Test Practical Astronomy with your Calculator or Spreadsheet, p. 49
  //rAlt := (19 + 20/60.0 + 3.64/3600.0)*Pi/180;
  //rAz := (283 + 16/60.0 + 15.7/3600.0)*Pi/180;
  //rPhi := 52.0*Pi/180;

  Result := true;

  rEps := 0.0001;

  rArgDec := sin(rAlt)*sin(rPhi) + cos(rAlt)*cos(rPhi)*cos(rAz);

  if(rArgDec < -1) or (rArgDec > 1) then
  begin
    Result := false;
    rDEC := 0;
    rH := 0;
    exit;
  end;

  rDEC := arcsin(rArgDec);

  if(rAz = 0) then // Az = 0 points to north direction -> equals hour angle of 12:00:00
  begin
    //rH := 12;

    // Overwrite Declination value for rAz = 0:
    // rDEC must be always <= 90°!
    if(rPhi > 0) then
    begin
      if(rAlt <= rPhi) then
        rDEC := pi/2.0 + (rAlt - rPhi) // Northern hemisphere
      else
        rDEC := pi/2.0 + (rPhi - rAlt); // Northern hemisphere
    end
    else
    begin
      if((rAlt + rPhi) >= 0) then
        rDEC := -pi/2.0 + (rAlt + rPhi) // Southern hemisphere
      else
        rDEC := -pi/2.0 - (rAlt + rPhi); // Southern hemisphere

    end;

    if(rAlt <= abs(rPhi)) then
      rH := 12.0
    else
      rH := 0;

  end
  else if(abs(180.0 - rAz *180.0/Pi) < rEps) then
  begin
    rH := 0;
  end
  else if(abs(cos(rPhi)*cos(rDEC)) < rEps) then
  begin
    Result := false;
    rH := 12;
  end
  else
  begin
    rDEC := arcsin(rArgDec);

    rArgH1 := (sin(rAlt) - sin(rPhi)*sin(rDEC))/(cos(rPhi)*cos(rDEC));

    // Accept minor calculation deviations from formula above
    if(rArgH1 < -1) and ((-1 - rArgH1) < 0.0000001) then
      rArgH1 := -1;
    if(rArgH1 > 1) and ((rArgH1 - 1) < rEps) then
      rArgH1 := 1;

    if(rArgH1 >= -1) and (rArgH1 <= 1) then
      rH1 := arccos(rArgH1)
    else
    begin
      Result := false;
      exit;
    end;

    if(sin(rAz) < 0) then
      rH := rH1
    else
      rH := 2*Pi - rH1;

    rH := rH * 180.0/Pi;
    rH := rH * 24.0/360.0;
  end;

  rDEC := rDEC * 180.0/Pi;

  HoursToHH_MM_SS(rH,iHH_HH,iHH_MM,rHH_SS);
  DegToDEG_MM_SS2(rDEC,iDEC_DEG,iDEC_MM,rDEC_SS);

end;

procedure EclipticToEquatorial(dJD: Double; rLambda,rBeta: Real;
  var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
  var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real);
var
  rRA, rDEC, rEpsilon: Real;
begin
  rEpsilon := EclipticObl(dJD) *Pi/180;

  rRA := arctan360(sin(rLambda *Pi/180)*cos(rEpsilon)-tan(rBeta *Pi/180)*sin(rEpsilon),cos(rLambda *Pi/180));
  rRA := rRA/15;
  rDEC := arcsin(sin(rBeta *Pi/180)*cos(rEpsilon) + cos(rBeta *Pi/180)*sin(rEpsilon)*sin(rLambda *Pi/180));
  rDEC := rDEC *180/Pi;

  HoursToHH_MM_SS(rRA,iRA_HH,iRA_MM,rRA_SS);
  DegToDEG_MM_SS2(rDEC,iDEC_DEG,iDEC_MM,rDEC_SS); // Replaced from buggy DegToDEG_MM_SS!

end;

procedure EclipticToEquatorialV2(dJD: Double; rLambda,rBeta: Real;
  var rRA: Real; var rDEC: Real);
var
  rEpsilon: Real;
begin
  rEpsilon := EclipticObl(dJD) *Pi/180;

  rRA := arctan360(sin(rLambda *Pi/180)*cos(rEpsilon)-tan(rBeta *Pi/180)*sin(rEpsilon),cos(rLambda *Pi/180));
  rRA := rRA/15;
  rDEC := arcsin(sin(rBeta *Pi/180)*cos(rEpsilon) + cos(rBeta *Pi/180)*sin(rEpsilon)*sin(rLambda *Pi/180));
  rDEC := rDEC *180/Pi;

end;

function EclipticObl(dJD: Double): Real;
var
  rT: Real;
begin
  rT := (dJD - 2451545.0)/36525.0;

  Result := 23.0 + 26/60 + 21.45/3600 -
    (46.815*rT + 0.0006*rT*rT - 0.00181*rT*rT*rT)/3600.0;

end;

procedure EquatorialToEcliptic(dJD: Double; rRA, rDEC: Real; var rLambda: Real; var rBeta: Real);
var
  rEpsilon: Real;
begin
  rEpsilon := EclipticObl(dJD) *Pi/180;

  rLambda := arctan360(sin(rRA *Pi/12)*cos(rEpsilon)+tan(rDEC *Pi/180)*sin(rEpsilon),cos(rRA *Pi/12));
  rBeta := arcsin(sin(rDEC *Pi/180)*cos(rEpsilon)-cos(rDEC *Pi/180)*sin(rEpsilon)*sin(rRA *Pi/12));

  rBeta := rBeta *180/Pi;

end;

function ArcTan360(rZ,rN: Real): Real;
begin
  (* http://de.wikibooks.org/wiki/Astronomische_Berechnungen_f%C3%BCr_Amateure/_Positionsastronomie/_Koordinatentransformationen

      Z > 0, N > 0 : A liegt im ersten Quadranten, dh. 0° ≤ A ≤ 90°; A = arctan(Z/N)

      Z > 0, N < 0 : A liegt im zweiten Quadranten, dh. 90° < A ≤ 180°; A = 180° + arctan(Z/N)

      Z < 0, N < 0 : A liegt im dritten Quadranten, dh. 180° < A ≤ 270°; A = 180° + arctan(Z/N)

      Z < 0, N > 0 : A liegt im vierten Quadranten, dh. 270° < A < 360° ; A = 360° + arctan(Z/N)

  *)

  if(rN <> 0) then
  begin
    Result := arctan(rZ/rN);

    if(rZ > 0) and (rN < 0) then
      Result := Pi + Result;

    if(rZ < 0) and (rN < 0) then
      Result := Pi + Result;

    if(rZ < 0) and (rN > 0) then
      Result := crPI2 + Result; //6.2832 + Result;
  end
  else
  begin
    if(rZ >= 0) then
      Result := crPIHALF
    else
      Result := crPITHREEHALF; //2*Pi-Pi/2;
    //if(rZ < 0) then Result := crPITHREEHALF; //2*Pi-Pi/2;
  end;

  //Result := 180.0*Result/Pi;
  Result := Result/crPIDIV180;

end;

function GetNormVal(rVal, rNormVal: Real): Real;
begin
  Result := rVal - rNormVal*Trunc(rVal/rNormVal)

  (*
  if(rVal >= 0) then
    Result := rVal - rNormVal*Trunc(rVal/rNormVal)
  else
  begin
    Result := -rVal - rNormVal*Trunc(-rVal/rNormVal);
    Result := 360.0 + rVal;
  end;
  *)
end;


function DayDiffEpoch2010(iYear: Integer): Integer;
begin
  (*
  case iYear of
    1990: Result := -7305;
    1991: Result := -6940;
    1992: Result := -6575;
    1993: Result := -6209;
    1994: Result := -5844;
    1995: Result := -5479;
    1996: Result := -5114;
    1997: Result := -4748;
    1998: Result := -4383;
    1999: Result := -4018;
    2000: Result := -3653;
    2001: Result := -3287;
    2002: Result := -2922;
    2003: Result := -2557;
    2004: Result := -2192;
    2005: Result := -1826;
    2006: Result := -1461;
    2007: Result := -1096;
    2008: Result := -731;
    2009: Result := -365;
    2010: Result := 0;
    2011: Result := 365;
    2012: Result := 730;
    2013: Result := 1096;
    2014: Result := 1461;
    2015: Result := 1826;
    2016: Result := 2191;
    2017: Result := 2557;
    2018: Result := 2922;
    2019: Result := 3287;
    2020: Result := 3652;
    2021: Result := 4018;
    2022: Result := 4383;
    2023: Result := 4748;
    2024: Result := 5113;
    2025: Result := 5479;
    2026: Result := 5844;
    2027: Result := 6209;
    2028: Result := 6574;
    2029: Result := 6940;
    else
      begin
        Result := Trunc(EncodeDateTime(iYear,01,01,0,0,0,0) - EncodeDateTime(2010,01,01,0,0,0,0));
      end;
  end;
  *)
  Result := Trunc(EncodeDateTime(iYear,01,01,0,0,0,0) - EncodeDateTime(2010,01,01,0,0,0,0));

end;

procedure GetMoonCoo(dtDateTime: TDateTime;
  iDST_HH,iUTC_HH: SmallInt; rLambdaSun, rMSun: Real;
  var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
  var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real);
{11.03.2012/fs
Calculating the co-ordinates of the earth's moon
}
var
  dJD: Double;
  iD: Integer;
  rD: Real;
  //rLambdaSun, rMSun: Real;
  rL, rMm, rN, rEnu, rAe, rA3, rMms: Real;
  rEc, rA4, rLs, rV, rLss, rNs: Real;
  rX, rY: Real;
  rLambdaMoon, rBetaMoon, rI: Real;
const
  crL0 = 91.929336; // DEGREES Moon's mean longitude at the epoch
  crP0 = 130.143076; // DEGREES Mean longitude of the perigree at the epoch
  crN0 = 291.682547; // DEGREES Mean longitude of the node at the epoch
  crI = 5.145396; // DEGREES Inclination of Moon's orbit
  {
  crE = 0.0549; // Eccentricity of the Moon's orbit
  crA = 384401; // KM semi-major axis of Moon's orbit
  crTheta0 = 0.5181; // DEGREES Moon's angular diameter at distance a fromthe Earth
  crPi0 = 0.9507; // Moon's parallax at distance a from the Earth
  }
begin
  // Initialisation
  iRA_HH := 0; iRA_MM := 0; rRA_SS := 0;
  iDEC_DEG := 0; iDEC_MM := 0; rDEC_SS := 0;

  if(rLambdaSun < 0) then
    rLambdaSun := rLambdaSun + 360;

  if(rMSun < 0) then
    rMSun := rMSun + 360;

  //rLambdaSun := 158.171829;
  //rMSun := 238.533547;

  // Inclination variation:
  // https://eclipse.gsfc.nasa.gov/SEhelp/moonorbit.html
  //rI := crI;// - 0.15*cos(2*Pi*(dtDateTime - EncodeDate(2010,1,14))/(0.475 * 365)); // 0.475: Less than one half year
  rI := crI - 3.0*cos(2*Pi*(dtDateTime - EncodeDate(2010,1,14))/(0.475 * 365)); // 0.475: Less than one half year
  //ShowMessage(FloatToStr(3.0*cos(2*Pi*(dtDateTime - EncodeDate(2010,1,14))/(0.475 * 365))));

  iD := DayDiffEpoch2010(YearOf(dtDateTime)) + DayOfTheYear(dtDateTime);

  rD := iD + (dtDateTime - Trunc(dtDateTime));

  rL := 13.1763966*rD + crL0; // Corrected 08.12.18: rD instead if iD!
  rL := GetNormVal(rL,360);
  if(rL < 0) then rL := 360 + rL;

  rMm := rL - 0.1114041*rD - crP0;
  rMm := GetNormVal(rMm,360);
  if(rMm < 0) then rMm := 360 + rMm;

  rN := crN0 - 0.0529539*rD;
  rN := GetNormVal(rN,360);
  if(rN < 0) then rN := 360 + rN;

  rEnu := 1.2739*sin((2*(rL-rLambdaSun)-rMm)*Pi/180);

  rAe := 0.1858*sin(rMSun*Pi/180);
  rA3 := 0.37*sin(rMSun*Pi/180);

  rMms := rMm + rEnu - rAe - rA3;

  rEc := 6.2886*sin(rMms*Pi/180);
  rA4 := 0.214*sin(2*rMms*Pi/180);

  rLs := rL + rEnu + rEc - rAe + rA4;

  rV := 0.6583*sin(2.0*(rLs-rLambdaSun)*Pi/180);
  rLss := rLs + rV;

  rNs := rN - 0.16*sin(rMSun*Pi/180);

  rY := sin((rLss-rNs)*Pi/180)*cos(rI*Pi/180);
  rX := cos((rLss-rNs)*Pi/180);

  rLambdaMoon := arctan360(rY,rX) + rNs;
  rBetaMoon := 180.0/Pi*arcsin(
    sin((rLss-rNs)*Pi/180.0)*sin(rI*Pi/180.0)
    );

  dJD := GetJulTime(dtDateTime,iDST_HH,iUTC_HH);

  EclipticToEquatorial(dJD,rLambdaMoon,rBetaMoon,
    iRA_HH,iRA_MM,rRA_SS,
    iDEC_DEG,iDEC_MM,rDEC_SS);

end;

procedure GetSunCoo(dtDateTime: TDateTime;
  iDST_HH,iUTC_HH: SmallInt; var rLambda: Real; var rM: Real;
  var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
  var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real);
{17.02.2012/fs
Calculating the co-ordinates of the sun
Reference: Practical Astronomy with your spreadsheet P.103-104
}
var
  //dtTime0: TDateTime;
  iD: Integer;
  rD: Real;
  dJD: Double;
  rBeta: Real;
  rEpsilonG, rOmegaG, rE: Real;
  rN, rT: Real;
begin
  //dtTime0 := EncodeDateTime(2010,01,01,0,0,0,0);
  //iD := Trunc(Trunc(dtDateTime) - Trunc(dtTime0));
  iD := DayDiffEpoch2010(YearOf(dtDateTime)) + DayOfTheYear(dtDateTime);
  rD := iD + (dtDateTime - Trunc(dtDateTime));

  dJD := GetJulTime(dtDateTime,iDST_HH,iUTC_HH);

  rT := (dJD-2415020.0)/36525;

  rEpsilonG := GetNormVal(279.6966778 - 0.127250061*rT + 0.0003025*rT*rT,360);
  //rEpsilonG := GetNormVal(279.6966778 + 36000.76892*rT + 0.0003025*rT*rT,360);
  rOmegaG := 281.2208444 + 1.719175*rT + 0.000452778*rT*rT;
  rE := 0.01675104 - 0.0000418*rT - 0.000000126*rT*rT;

  (*
  rEpsilonG := 279.557208;
  rEpsilonG := GetNormVal(279.6966778 + 36000.76892*rT + 0.0003025*rT*rT,360);
  rOmegaG := 283.112438;
  rE := 0.016705;
  *)

  rN := 360.0/365.242191*rD;
  rN := GetNormVal(rN,360);
  if(rN < 0) then
    rN := rN + 360;

  rM := rN + rEpsilonG - rOmegaG;
  //rEc := 360/(Pi)*rE*sin(rM*Pi/180);

  rLambda := rN +
    360/(Pi)*rE*sin((rN + rEpsilonG - rOmegaG)*Pi/180) +
    rEpsilonG;

  rLambda := GetNormVal(rLambda,360);
  rBeta := 0;

  EclipticToEquatorial(dJD,rLambda,rBeta,
    iRA_HH,iRA_MM,rRA_SS,
    iDEC_DEG,iDEC_MM,rDEC_SS);
end;

(*
function GetYearVal(dtDate: TDateTime): Real;
// Returns a float number of the given date beginning with the year and with decimals calculated by month an day
var
 iYear, iMonth, iDay: Word;
begin
  DecodeDate(dtDate,iYear,iMonth,iDay);

  Result := iYear + (iMonth-1 + (iDay-1)/(1.0*DaysInAMonth(iYear,iMonth)))/12.0;

end;
*)

function CountChar(sInstr: string; cSym: Char): Integer;
{2012-10-23/fs
Returns the number of the specified symbol of an input string
}
var
  i, iLen: Integer;
begin
  Result := 0;
  iLen := length(sInstr);

  for i:=1 to iLen do
  begin
    if(sInstr[i] = cSym) then
      Inc(Result);
  end;

end;

{function CalcMoonPhase
Returns the moon phase percentage (0: new moon ... 100: full moon
}
function CalcMoonPhase(dtTime: TDateTime): Integer;
var
  dtTime0: TDateTime;
  rVal: Real;
begin
  //22.12.1999 18:31:18
  dtTime0 := EncodeDateTime(1999,12,22,18,31,18,0);
  rVal := abs((dtTime - dtTime0))/29.530588861;
  Result := Round(100*(rVal - Trunc(rVal)));
end;

{function FindLastDayOfWeek
Returns the DateTime of the last day of week occurence of the month
iSearchDay: 1: Sunday ... 7: Saturday
}
function FindLastDayOfWeek(iYYYY, iMonth, iSearchDay: Integer): TDateTime;
var
  iDay, iMaxDay: Integer;
begin
  iMaxDay := DaysInMonth(iMonth);
  Result := EncodeDate(iYYYY,iMonth,iMaxDay);

  iDay := DayOfWeek(Result);
  while (iDay <> iSearchDay) do
  begin
    Result := Result - 1.0;
    iDay := DayOfWeek(Result);
  end;

end;

{function GetDST
Returns Daylight saving delay hours
Die Sommerzeit beginnt am letzten Sonntag um 2.00Uhr im März und endet zum gleichen Zeitpunkt im Oktober.
DST begins at the last sunday of march 02:00 AM and ends at last sunday of october 02:00 AM
}
function GetDST(dtDate: TDateTime; sDST: string; iDST_DIFF: Integer): Integer;
var
  iYYYY, iMM, iDD: Word;
  dtDST_START, dtDST_END: TDateTime;
begin
  DecodeDate(dtDate,iYYYY,iMM,iDD);
  dtDST_START := 0;
  dtDST_END := 0;

  if(sDST = 'USA') then
  begin
    dtDST_START := FindLastDayOfWeek(iYYYY,3,1) - 14.0; // March, 2nd Sunday
    dtDST_END := FindLastDayOfWeek(iYYYY,10,1) + 7; // November, first Sunday
  end
  else if((sDST = 'MESZ') or (sDST = 'BST'))then
  begin
    dtDST_START := FindLastDayOfWeek(iYYYY,3,1); // March, last Sunday
    dtDST_END := FindLastDayOfWeek(iYYYY,10,1); // October, last Sunday
  end
  else if(sDST = 'SAM') then
  begin
    if(iMM >= 9) then
    begin
      dtDST_START := FindLastDayOfWeek(iYYYY,9,6) - 21.0; // 1st Saturday, September, this year
      dtDST_END := FindLastDayOfWeek(iYYYY+1,4,6) - 21.0; // 1st Saturday, April, next year
    end
    else
    begin
      dtDST_START := FindLastDayOfWeek(iYYYY-1,9,6) - 21.0; // 1st Saturday, September, last year
      dtDST_END := FindLastDayOfWeek(iYYYY,4,6) - 21.0; // 1st Saturday, April, this year
    end;
  end;

  if(dtDST_START > 0) and (dtDST_END > 0) and (dtDate >= dtDST_START) and (dtDate <= dtDST_END) then
    Result := iDST_DIFF
  else
    Result := 0;

end;

{function DayOfYear
Returns the day number of the year
}
function DayOfYear(dtDate: TDateTime): Integer;
var
  dtFirstDate: TDateTime;
  iYYYY, iMM, iDD: Word;
begin
  DecodeDate(dtDate, iYYYY, iMM, iDD);
  dtFirstDate := EncodeDate(iYYYY, 1, 1);

  Result := Trunc(dtDate - dtFirstDate) + 1;
end;

function HH_MM_SSToDeg(iHH, iMM: Word; rSS: Real): Real;
begin
  Result := iHH + iMM/60.0 + rSS/3600.0;
  Result := Result * 360.0/24.0;
end;

function DEG_MM_SSToDeg(iDEG, iMM: SmallInt; rSS: Real): Real;
begin
  Result := iDEG + iMM/60.0 + rSS/3600.0;
  Result := Result;
end;

procedure HoursToHH_MM_SS(fHours: Real; var iHH: Word; var iMM: Word; var rSS: Real);
var
  rMM: Real;
begin
  //fHours := abs(fHours)
  if(fHours < 0) then
    fHours := fHours + 24;

  fHours := GetNormVal(fHours,24);

  rMM := 60.0*(fHours - Trunc(fHours));
  rSS := 60.0*(rMM - Trunc(rMM));

  iHH := Trunc(fHours);
  iMM := Trunc(rMM);
end;

procedure DegToDEG_MM_SS(fAngle: Real; var iDEG: SmallInt; var iMM: SmallInt; var rSS: Real; IsCircleAngle: Boolean);
var
  rMM: Real;
begin
  if(IsCircleAngle) then
  begin
    if(abs(fAngle) > 360) then
    begin
      fAngle := fAngle - Trunc(fAngle/360.0)*360.0;
    end;
  end;

  if(fAngle < 0) then
    fAngle := 360.0 + fAngle; // is subtracted if negative.

  rMM := 60.0*(fAngle - Trunc(fAngle));
  rSS := 60.0*(rMM - Trunc(rMM));

  iDEG := Trunc(fAngle);
  iMM := Trunc(rMM);

end;

procedure DegToDEG_MM_SS2(fAngle: Real; var iDEG: SmallInt; var iMM: SmallInt; var rSS: Real);
// Returns negative angles
var
  rMM: Real;
  iSign: SmallInt;
begin
  if(fAngle < 0) then
    iSign := -1
  else
    iSign := 1;

  fAngle := abs(fAngle);

  if(fAngle >= 360) then
    fAngle := fAngle - 360;

  rMM := 60.0*(fAngle - Trunc(fAngle));

  iDEG := Trunc(fAngle);  // always >= 0 here
  iMM := Trunc(rMM);      // always >= 0 here
  rSS := 60.0*(rMM - iMM);// always >= 0 here

  // Set sign correctly
  if(iDEG > 0) then
    iDEG := iSign*iDEG
  else if(iMM > 0) then
    iMM := iSign*iMM
  else
    rSS := iSign*rSS;

end;

(*
{$IFDEF LAZARUS}
procedure ExecuteProcess();
  var
    AProcess: TProcess;
  // Hier beginnt der Befehlstext:
  begin
    // Nun erstellen wir das Objekt TProcess und
    // weisen es der Variable AProcess zu.
    AProcess := TProcess.Create(nil);

    // Lassen sie uns den FreePascal Compiler verwenden.
    // Dazu müssen wir den Kommandozeilenbefehl an AProcess
    // übergeben:
    AProcess.CommandLine := 'ppc386 -h';

    // Während das externe Programm läuft, soll unser
    // Programm natürlich nicht weiterlaufen.
    // Dies regeln wir mit folgender Bedingung:
    AProcess.Options := AProcess.Options + [poWaitOnExit];

    // Nun muss AProcess noch ausgeführt werden...
    AProcess.Execute;

    // Wegen der oben gesetzter Bedingung wird dieser
    // Codeabschnitt erst erreicht, wenn ppc386 beendet
    // ist:
    AProcess.Free;

end;
{$ENDIF}
*)

function StrIsNum(bFloatCheck: Boolean; var sVar: string): Boolean;
var
  i, iLen: Integer;
  sMsg: string;
begin
  //if(AnsiContainsStr(gsMsg,'Triangulum')) then
  //  ShowMessage('Stop...');

  sVar := Trim(sVar);
  Result := true;
  iLen := Length(sVar);
  if((iLen = 0) or (sVar = '-')) then
  begin
    Result := false;
    exit;
  end;

  i := 1;
  if(bFloatCheck) then
  begin
    sVar := AnsiReplaceStr(sVar,'.',GetDecimalSeparator());
    sVar := AnsiReplaceStr(sVar,',',GetDecimalSeparator());
  end;

  while (i <= iLen) and (Result) do
  begin
    if(bFloatCheck) then
      //Result := (sVar[i] in ['e','E',GetDecimalSeparator(),'+','-','0'..'9'])
      Result := (sVar[i] in ['e','E',',','.','+','-','0'..'9'])
    else
      Result := (sVar[i] in ['+','-','0'..'9']);

    Inc(i);
  end;

  if (not Result) and (sVar <> '?') then
  begin
    if(bFloatCheck) then
      sMsg := 'FloatCheck YES'
    else
      sMsg := 'FloatCheck NO';

{$IFDEF LAZARUS}
    MessageDlg('Method: ' + gsActMName + ', Msg: ' + gsMsg + #13 + #10 +
      'StrIsNum: ' + sVar + ' is not a number!' + #13 + #10 +
      'DecimalSeparator: ' + GetDecimalSeparator() + ' (' + sMsg + ')'
      ,mtConfirmation,[mbOk],0);
{$ENDIF}

  end;


end;

end.

