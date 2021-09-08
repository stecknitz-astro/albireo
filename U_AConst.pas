unit U_AConst;

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
  Classes, SysUtils, ExtCtrls, Graphics, StdCtrls, Controls, StrUtils,
  ContNrs,
  U_ABase;

const
  gcsVersion = '1';
  gcsSubVersion = '5'; // New Features
  gcsBuildVersion = '0'; // Bug Fixed
  gcsCommVersion = 'OSS';
  //gciCommLvl = 1; //: SmallInt; // = 1; // 0: Low computation resources (uses smaller astro database), 1: Normal computation resources (uses standard astro database)

  gcsAlbireoVersion = gcsCommVersion + ' - V.' + gcsVersion + '.' + gcsSubVersion + '.' + gcsBuildVersion;

  // Low-Resource-Mode: giCommLvl = 0:
  gciMaxLowResStars = 100000;
  gciMaxLowResGalaxies = 1000;

  gcsUpdateHost = 'www.stecknitz-astronomie.de/albireo';
  gcsUpdateURI = '/' + gcsVersion + '/' + gcsSubVersion + '/Albireo.exe';
  gcsUpdateVersionURI = '/' + gcsVersion + '/' + gcsSubVersion + '/index.html';

  gciBirthYear = 1970;
  ciWeeksMax = 2*52*7 +1;

  gcsWebInfo = 'https://www.stecknitz-astronomie.de';
  gcsWebAlbireoInfoDE = 'https://www.stecknitz-astronomie.de/albireo';
  gcsWebAlbireoInfoEN = 'https://www.stecknitz-astronomie.de/albireo';
  gcsWebAlbireoMail = 'albireo@stecknitz-astronomie.de';
  gcsWebAlbireoSpenden = 'https://www.stecknitz-astronomie.de/spende-fuer-albireo-astronomy-toolbox';
  gcsWebAlbireoDonate = 'https://www.stecknitz-astronomie.de/donate-for-albireo-astronomy-toolbox';

  gcsCAMLISTFILE = 'CamList.dat';

  // Colors
  clDaySky = TColor($FD5522);
  clDeepDarkSky = clBlack;
  clSummerDarkSky = TColor($110101);
  //clSunRise = TColor($0033FF);
  clSunRise = TColor($003355);
  clDkGray2  = TColor($404040); // clGray alias

  //crEyeFacH = 70.0/90.0; // < 1 // Eye cannot see the zenith for horizon view
  //crEyeFacH = 90.0/90.0; // < 1 // Eye can see the zenith for horizon view
  crEyeFacH_90 = 90.0/90.0; // < 1 // Eye can see the zenith for horizon view
  crEyeFacH_70 = 70.0/90.0; // < 1 // Eye cannot see the zenith for horizon view

  // Math. Constants
  crPIDIV180 = Pi/180; //0.0174533;
  crPIHALF = Pi/2;
  crPITHREEHALF = 3*Pi/2;
  crPI2 = 2*Pi;

  ciLandscapeNoDef = 0;
  ciLandscapeNoCust = 3;
  csLandscapeRelDir = 'img\Landscapes\';

type
  TEclipse = class(TObject)
  public
    dtDateTime: TDateTime;
    sEclType: string;
    sType: string;
    sLoc: string;
    dtDurationT: TDateTime;
  end;

type
  TSunEclipse = class(TEclipse)
  public
    rSize: Real;
  end;

type
  TMoonEclipse = class(TEclipse)
  public
    dtDurationPE, dtDurationPA: TDateTime;
  end;

type // Localize special datase ranges in AOList so spped up access control
  TAOIndexControl = record
    iMinMessier: Integer;
    iMaxMessier: Integer;
    iMin_S: Integer;
    iMax_S: Integer;
    iMin_Q: Integer;
    iMax_Q: Integer;
    iMin_G: Integer;
    iMax_G: Integer;
    iMin_GC: Integer;
    iMax_GC: Integer;
    iMin_OC: Integer;
    iMax_OC: Integer;
    iMin_PN: Integer;
    iMax_PN: Integer;
    iMin_N: Integer;
    iMax_N: Integer;

    iMin_P: Integer;
    iMax_P: Integer;
    iMin_PA: Integer;
    iMax_PA: Integer;
    iMin_C: Integer;
    iMax_C: Integer;
  end;

type
  TCamera = class(TObject)
  public
    sManufacturer: string;
    sModel: string;
    sSensorName: string;
    iSensorFormatIndex: Integer;
    sSensorType: string;

    fMegaPixel: Real;
    iQEff: Integer; // Quantum Efficiency
    iFWC: Integer;  // Fullwell-Capacity
    fDCRC: Real; // Dark Current Reference Current
    fDCRT: Real; // Dark Current Reference Temperature
    fDCHT: Real; // Dark Current Half-Step Temperature
    iRN: Integer; // Read-out-Noise (Ausleserasschen)
    iBits: Integer; // Bit depth per pixel
    iSensorYear: Integer; // Publication year of sensor
    iEstimated: SmallInt; // -1: Userdefined sensor values, 0: sensor values validated by specification, 1: Estimated sensor values

    constructor Create();
  end;

  TAOSearch = class(TObject)
    public
    sLabel: string;
    iAOIndex: Integer;
  end;

  TAObject = class(TObject)
  private

  public
    iAOIndex: Integer;
    sName_DE: string;
    sName_EN: string;
    rM: Real; // Scheinbare Helligkeit
    iRA_Hours, iRA_Min: ShortInt;
    rRA_Sec: Real; // Right ascensions hours, minutes, seconds
    iDec_Deg: SmallInt;
    iDec_Min: ShortInt;
    rDec_Sec: Real; // Declination Degrees (0-180), minutes, seconds
    sAOType: string[5]; // Type ID of the astronomical object [4]
    SHP: TShape; // Created on demand to save memory
    IMG: TImage;  // Created only for planets and stars
    L__AO: TLabel; // Created on demand to save memory
    sComment: string;

    constructor Create();
    destructor Destroy; override;
  end;

  TComet = class(TAObject)
  private

  public                            // SunDist  EarthDist
    rP, rOmegaQ, rOmega, rTp, rA, rE, rI, rRs, rRho: Real;
    //rTheta0, rV0, rDiameterRatio, rMassRatio: Real;
    Color: TColor;
    bShowPath: Boolean;
    iPathRA_Hours, iPathRA_Min: array[0..ciWeeksMax] of Integer;
    rPathRA_Sec: array[0..ciWeeksMax] of Real;
    iPathDec_Deg, iPathDec_Min: array[0..ciWeeksMax] of Integer;
    rPathDec_Sec: array[0..ciWeeksMax] of Real;

    constructor Create;
  end;

  TMoon = class(TAObject)
  private

  public
    iMoonIndex: Integer;
    iPlanetIndex: Integer;
    rMass_kg, rMass_exp, rRadius_km, rSMAxis_km, rRho_g_qcm, rOrbitalPeriod_d, rMag, rE: Real;
    Color: TColor;
    dtZeroPass: TDateTime;
  end;

  TPlanet = class(TAObject)
  private

  public
    iPlanetIndex: Integer;
    rTp, rEpsilon, rOmegaQ, rE, rA, rOmega, rI: Real;
    rTheta0, rV0, rDiameterRatio, rMassRatio: Real;
    bInnerPlanet: Boolean;
    Color: TColor;
    sPlanetType: string;
    iYearP, iYear, iYearN: Integer;
    dtOPDateP, dtConP: TDateTime;
    dtOPDate, dtCon: TDateTime;
    dtOPDateN, dtConN: TDateTime;
    bShowPath: Boolean;
    iPathRA_Hours, iPathRA_Min: array[0..ciWeeksMax] of Integer;
    rPathRA_Sec: array[0..ciWeeksMax] of Real;
    iPathDec_Deg, iPathDec_Min: array[0..ciWeeksMax] of Integer;
    rPathDec_Sec: array[0..ciWeeksMax] of Real;
    bHasImage: Boolean;
    rDistFromEarth_AU: Real;
    rDistFromSun_AU: Real;
    rVisuAngle_arcsec: Real;

    constructor Create();

    procedure SetImage();
  end;

  TMilkywayObject = class(TAObject)
  private

  public
    iMWItemNo: Integer;
    sMWType: string[5];
    iVisDim1: ShortInt;
    iVisDim2: ShortInt;

  end;

  TInterstellarObject = class(TAObject)
  private

  public
    sNGC: string; // NGC-ID or IC-ID
    sMessier: string[4]; // Messier Catalogue ID [12]
    sCatNo: string; // catalogue id (Flamsteed, HD, SAO, ...)
    sCon: string[3]; // Constellation-ID
    sCon2: string[3]; // Alternative Constellation-ID
    rDist_XLY: Real; // S: Distance from earth in light years, G: MLy
    iLYMult: ShortInt;

  procedure SetSHPSize();

  end;

type
  TStar = class(TInterstellarObject)
  private

  public
    sSym: string[10]; // Greek Star symbol [10]
    sSpType: string[20]; // Spectral Class [15]
    rSolFrac: Real; // S: Solar radius fraction
    sVarType: string; // Variable Type [10]
    rVarMMin: Real; // Variable Min
    rVarMMax: Real; // Variable Max
    rVarPer_D: Real; // Period in days
    rMAttend: Real; // Appearent Brighness of the attender
    iSepSec: WORD; // Separation in arc seconds
    iSepAngle: WORD; // Angle of attender
    //iSTemp: Integer; // Surface Temperature
    sConPartner: string; // Constellation partner [20]
    sMShowerName_DE: string; // Associated meteor shower [25]
    Color: TColor;
    //sMKKTypeDE,sMKKTypeEN,sSpecID: string; // MKK detail data [20]
    iaConIndex: array[0..5] of Integer; // Constellation indexes within the same constallation. Filled during runtime
    bSelSign: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure PrepareStar(bZoomed: Boolean);
  end;

type
  TGalaxy = class(TInterstellarObject)
  private

  public
    sGType: string[10]; // Galax type [10]
    rVisDim1, rVisDim2: Real;
    iRadSpeed: Integer;
    rRedshift: Real;
    rMagV, rMagU, rMagB, rMagR, rMagG, rMagI, rMagJ, rMagH, rMagK: Real;
    rMagsu, rMagsg, rMagsr, rMagsi, rMagsz: Real;

  end;

type
  TQuasar = class(TInterstellarObject)
  private

  public
    sQID: string[20];
    sQType: string[10];
    fRadSpeed: Real;
    fComDistMPc: Real;
    fRedshift: Real;
  end;

type
  TGlobularCluster = class(TInterstellarObject)
  private

  public
    iGCType: Integer;
    rVisDiam: Real;
    iRadSpeed: Integer;
  end;

type
  TNebula = class(TInterstellarObject)
  private

  public
    sNType: string[10];
    iVisDim1: Integer;
    iVisDim2: Integer;
    sStar: string[20];
    rStar_M: Real;
  end;

  type
  TPNebula = class(TInterstellarObject)
  private

  public
    sPNType: string[10];
    rVisDim1: Real;
    rVisDim2: Real;
    rStar_M: Real;
    iRadSpeed: Integer;
  end;

  type
  TOpenCluster = class(TInterstellarObject)
  private

  public
    sOCType: string[10];
    iDiam_M: Integer;
    iNum: Integer;
    rAge: Real;
    iRadSpeed: Integer;
  end;

type
  TMeteorShower = class(TObject)
    private

    public
      bActive: Boolean;
      sName_DE: string;
      sName_EN: string;
      sSign: string;
      sMaxDateMonth: string;
      iObjPerHour: Integer;
      iSpeed: Integer;
      sTime1: string;
      sTime2: string;
      sCometName: string;
      SHP: TShape;
      IMG: TImage;
      Lbl: TLabel;

    constructor Create;
    destructor Destroy; override;

end;

type
  TSign = class(TObject)
  private

  public
    sCon: string[10]; // Sign-ID
    sConDE: string[100]; // German Name
    sConEN: string[100]; // English Name
    bSelected: Boolean;

  end;

type
  TCBExt = class(TObject)
    private

    public
      sAOLabel: string[100]; // Label of astronomical object
      sAOType: string[10]; // Type of astronomical object
      sCon1: string[10]; // Constellation that contains the object os is nearby
      sCon2: string[10]; // Alternative constellation that contains the object os is nearby
  end;

type
  TADevice = class(TObject)
  private

  public
    sName: string;
    sDescription: string;
    sManufacturer: string;
    sArtNoManu: string;
    dtBDate: TDateTime;
    rPrice: Real;
    sCurrency: string;
    iMountType: SmallInt;
    sImagePath: string;
    sType: string;
    sCManu: string;
    sCModel: string;
    iFocalWidthDev_mm: Integer;
    iDiameter_mm: Integer;
    iDefault: Integer;

  end;

type
  TEarthLocation = class(TObject)

  public
    sCountry, sCity: string;
    rLatitude_DEG, rLatitude_MIN: Real;
    rLongitude_DEG, rLongitude_MIN: Real;
    iTimeZone: SmallInt;
    sLANG_ID: string[3];
    sDST: string;

    constructor Create(sSetCountry: string; sSetCity: string; rSetLatitude_DEG, rSetLatitude_MIN, rSetLongitude_DEG, rSetLongitude_MIN: Real; iSetTimeZone: Integer); overload;
end;

var
  gsMsg: string; // Global Message Text
  gsActMName: string; // Active Method Name
  gsAlbireoLocalDir: string; // Albireo's database & runtime resource directory
  grecAOIndexControl: TAOIndexControl;
  giRSCLvl: Integer; // Auto-Set by gciCommLvl when main form ist created

  procedure BeginMethod(sMName: string);
  procedure EndMethod(sMName: string);
  procedure SetMsg(sMsg: string);
  function GetSignObj(sCon: string; olSignList: TObjectList; var iSignIndex: Integer): TSign;
  function GetSignName(sCon: string; olSignList: TObjectList; sLANG_ID: string): string;
  procedure SetSHPSizeProc(sAOType: string; SHP: TShape; rMag: Real);
  //function CreateAOImage(): TImage;
  function GetIMGMag(Star: TStar; bZoomed: Boolean): Integer;

implementation

function GetIMGMag(Star: TStar; bZoomed: Boolean): Integer;
begin
  if(bZoomed) then
    Result := Round(((0.0-Star.rM) + 10)*1.585)
  else
    Result := Round(((0.0-Star.rM) + 7)*1.585);

  if(Result < 2) then Result := 3;
end;

(*
function CreateAOImage(): TImage;
begin
  //Result := TImage.Create(nil);

  Result.Proportional:=true;
  Result.Stretch:=false;
  Result.StretchInEnabled:=true;
  Result.StretchOutEnabled:=true;
  Result.Center:=true;
  Result.Cursor:=crCross;

  Result.Height:=0;
  Result.Width:=0;

end;
*)

function GetSignObj(sCon: string; olSignList: TObjectList; var iSignIndex: Integer): TSign;
var
  i: Integer;
  bFound: Boolean;
  Sign: TSign;
begin
  Result := nil;
  iSignIndex := -1;

  bFound := false;

  i:=0;
  while (not bFound) and (i < olSignList.Count) do
  begin
    //ShowMessage(IntToStr(i));
    Sign := (olSignList[i] as TSign);

    //bFound := ((CB__SIGNS.Items.Objects[i] as TSign).sCon = sCon);
    if(Sign <> nil) then
      bFound := (Sign.sCon = sCon);

    if(not bFound) then Inc(i);
  end;

  if(bFound) then
  begin
    iSignIndex := i;
    Result := Sign;
  end;

end;

function GetSignName(sCon: string; olSignList: TObjectList; sLANG_ID: string): string;
var
  i: Integer;
  bFound: Boolean;
  Sign: TSign;
begin
  Result := '';

  Sign := nil;
  bFound := false;
  i:=0;
  while (not bFound) and (i < olSignList.Count) do
  begin
    //ShowMessage(IntToStr(i));
    Sign := (olSignList[i] as TSign);

    if(Sign <> nil) then
      bFound := (Sign.sCon = sCon);

    if(not bFound) then Inc(i);
  end;

  if(bFound) and (Sign <> nil) then
  begin
    if(sLANG_ID = 'DE') then
      Result := Sign.sConDE
    else
      Result := Sign.sConEN;
  end;

end;

procedure SetMsg(sMsg: string);
begin
  gsMsg := sMsg;
end;

procedure BeginMethod(sMName: string);
begin
  gsActMName := sMName;
end;

procedure EndMethod(sMName: string);
begin
  gsActMName := '';
end;

procedure SetSHPSizeProc(sAOType: string; SHP: TShape; rMag: Real);
begin
  if(SHP = nil) then
    SHP := TShape.Create(nil);

  SHP.Visible:=false;
  SHP.Parent:=nil;
  SHP.Cursor := crCross;

  if(sAOType = 'G') then
  begin
    if(rMag <= 3) then // Unknown vmag
    begin
      SHP.Height:=3;
      SHP.Width:=6;
    end
    else if(rMag < 8) then
    begin
      SHP.Height:=12;
      SHP.Width:=24;
    end else if(rMag < 10) then
    begin
      SHP.Height:=8;
      SHP.Width:=16;
    end
    else
    begin
      SHP.Height:=4;
      SHP.Width:=8;
    end;
    exit;
  end;

  if(sAOType = 'GC') then
  begin
    if(rMag < 6) then
    begin
      SHP.Height:=12;
      SHP.Width:=12;
    end else if(rMag < 8) then
    begin
      SHP.Height:=9;
      SHP.Width:=9;
    end
    else
    begin
      SHP.Height:=6;
      SHP.Width:=6;
    end;
    exit;
  end;

  if(sAOType = 'OC') then
  begin
    if(rMag < 0.5) then
    begin
      // Not properly defined
      SHP.Height:=6;
      SHP.Width:=6;
    end else if (rMag < 2) then
    begin
      SHP.Height:=15;
      SHP.Width:=15;
    end else if(rMag < 5) then
    begin
      SHP.Height:=12;
      SHP.Width:=12;
    end else if(rMag < 8) then
    begin
      SHP.Height:=9;
      SHP.Width:=9;
    end
    else
    begin
      SHP.Height:=6;
      SHP.Width:=6;
    end;
    exit;
  end;

  if(sAOType = 'PN') then
  begin
    if(rMag < 1) then
    begin
      // Not properly defined
      SHP.Height:=6;
      SHP.Width:=6;
    end else if(rMag < 8) then
    begin
      SHP.Height:=12;
      SHP.Width:=12;
    end else if(rMag < 11) then
    begin
      SHP.Height:=9;
      SHP.Width:=9;
    end
    else
    begin
      SHP.Height:=6;
      SHP.Width:=6;
    end;
    exit;
  end;

  if(sAOType = 'N') then
  begin
    if(rMag < 1) then
    begin
      // Not properly defined
      SHP.Height:=6;
      SHP.Width:=6;
    end else if(rMag < 8) then
    begin
      SHP.Height:=12;
      SHP.Width:=12;
    end else if(rMag < 11) then
    begin
      SHP.Height:=9;
      SHP.Width:=9;
    end
    else
    begin
      SHP.Height:=6;
      SHP.Width:=6;
    end;
    exit;
  end;

end;

procedure TInterstellarObject.SetSHPSize();
begin
  if(sAOType = 'S') then
    exit;

  SetSHPSizeProc(sAOType,SHP,rM);
end;

constructor TEarthLocation.Create(sSetCountry: string; sSetCity: string; rSetLatitude_DEG, rSetLatitude_MIN, rSetLongitude_DEG, rSetLongitude_MIN: Real; iSetTimeZone: Integer);
begin
  Create();

  rLatitude_DEG := rSetLatitude_DEG;
  rLatitude_MIN := rSetLatitude_MIN;
  rLongitude_DEG := rSetLongitude_DEG;
  rLongitude_MIN := rSetLongitude_MIN;
  sCountry := sSetCountry;
  sCity := sSetCity;

  iTimeZone := iSetTimeZone;

  // Language
  if(
    (sCountry = 'Deutschland') or (sCountry = 'Germany') or
    (sCountry = 'Schweiz') or (sCountry = 'Switzerland') or
    (sCountry = 'Österreich') or (sCountry = 'Austria') or
    (sCountry = 'Liechtenstein')
    ) then
    sLANG_ID := 'DE'
  else
    sLANG_ID := 'EN';

  // DST-Model
  if(
    (sCountry = 'Deutschland') or (sCountry = 'Germany') or
    (sCountry = 'Schweiz') or (sCountry = 'Switzerland') or
    (sCountry = 'Österreich') or (sCountry = 'Austria') or
    (sCountry = 'Liechtenstein') or
    (sCountry = 'Schweden') or (sCountry = 'Sweden') or
    (sCountry = 'Norwegen') or (sCountry = 'Norway') or
    (sCountry = 'Finnland') or (sCountry = 'Finland') or
    (sCountry = 'Dänemark') or (sCountry = 'Danmark') or
    (sCountry = 'Niederlande') or (sCountry = 'Netherlands') or
    (sCountry = 'Belgien') or (sCountry = 'Belgium') or
    (sCountry = 'Luxemburg') or (sCountry = 'Luxembourg') or
    (sCountry = 'Frankreich') or (sCountry = 'France') or
    (sCountry = 'Spanien') or (sCountry = 'Spain') or
    (sCountry = 'Portugal') or
    (sCountry = 'Italien') or (sCountry = 'Italia') or
    (sCountry = 'Slovakei') or (sCountry = 'Slovakia') or
    (sCountry = 'Tschechien') or (sCountry = 'Czech Republic') or
    (sCountry = 'Kroatien') or (sCountry = 'Croatia') or
    (sCountry = 'Serbien') or (sCountry = 'Serbia') or
    (sCountry = 'Bosnien und Herzegowina') or (sCountry = 'Bosnia and Herzegowina') or
    (sCountry = 'Griechenland') or (sCountry = 'Greece') or
    (sCountry = 'Türkei') or (sCountry = 'Turkey') or
    (sCountry = 'Bulgarien') or (sCountry = 'Bulgaria') or
    (sCountry = 'Rumänien') or (sCountry = 'Romaniana') or
    (sCountry = 'Polen') or (sCountry = 'Poland') or
    (sCountry = 'Modavien') or (sCountry = 'Modavia') or
    (sCountry = 'Estland') or (sCountry = 'Estonia ') or
    (sCountry = 'Lettland') or (sCountry = 'Latvia') or
    (sCountry = 'Litauen') or (sCountry = 'Lithuania')
    ) then
   begin
     sDST := 'MESZ';
   end
   else if AnsiContainsStr(sCountry,'USA') then
   begin
     sDST := 'USA';
   end
   else if (
     (sCountry = 'Chile') or
     (sCountry = 'Uruguay') or
     (sCountry = 'Paraguay')
     ) then
   begin
     sDST := 'SAM';
   end
   else if (
     (sCountry = 'Großbritannien') or (sCountry = 'Great Britain') or
     (sCountry = 'Grossbritannien') or (sCountry = 'GreatBritain') or
     (sCountry = 'England') or
     (sCountry = 'Wales') or
     (sCountry = 'Schottland') or (sCountry = 'Scotland') or
     (sCountry = 'Nordirland') or (sCountry = 'Northern Ireland')
     ) then
   begin
     sDST := 'BST';
   end
   else
     sDST := '';
end;

constructor TAObject.Create();
begin
  inherited;

  IMG := nil;
  L__AO := nil;
  SHP := nil;

end;

destructor TAObject.Destroy;
begin
  SHP.Free;
  L__AO.Free;
  IMG.Free;

  inherited;
end;

constructor TStar.Create;
var
  i: Integer;
begin
  inherited;

  bSelSign := false;

  for i:=0 to 5 do
    iaConIndex[i] := -1;
end;

procedure TStar.PrepareStar(bZoomed: Boolean);
var
  iMag: Integer;
begin
  if(IMG <> nil) then
    IMG.Visible:=false;

  if(sSym = 'MARK') then
  begin
    // Handle Marks
    if(SHP = nil) then SHP := TShape.Create(nil);
    SHP.Visible:=false;

    if(AnsiContainsStr(Uppercase(sName_EN),'VOID')) then
    begin
      SHP.Height := 8;
      SHP.Width := 8;
      SHP.Shape := stRoundSquare;
      SHP.Pen.Color := clSilver; //Color;
      SHP.Brush.Color := clGray; //Color;
      SHP.Color:=Color;
      SHP.Cursor:=crCross;
    end
    else //if(sSpType = 'MARK-NCP') or (sSpType = 'MARK-SCP') then
    begin
      SHP.Height := 5;
      SHP.Width := 5;
      SHP.Shape := stSquare;
      SHP.Pen.Color := clLime; //Color;
      SHP.Brush.Color := clGreen; //Color;
      SHP.Color:=Color;
    end;
  end
  else if(sName_EN = 'Milkyway Center') then
  begin
    if(SHP = nil) then SHP := TShape.Create(nil);
    SHP.Height := 5;
    SHP.Width := 5;
    SHP.Shape := stTriangle;
    SHP.Pen.Color := clLime;
    SHP.Brush.Color := clGreen;
    SHP.Color:=Color;
  end
  else
  begin
    if(IMG <> nil) then
    begin
      iMag := GetIMGMag(self,bZoomed);
      IMG.Height:=iMag;
      IMG.Width:=IMG.Height;
    end;
  end;

end;

destructor TStar.Destroy;
begin
  inherited;
end;

constructor TMeteorShower.Create();
begin
  inherited;

  SHP := TShape.Create(nil);

  IMG := TImage.Create(nil);
  IMG.Height:=40;
  IMG.Width:=40;
  IMG.Cursor:=crCross;

  Lbl := TLabel.Create(nil);
  Lbl.Cursor:=crCross;

end;

destructor TMeteorShower.Destroy;
begin
  IMG.Destroy;
  SHP.Destroy;

  inherited;
end;

constructor TComet.Create();
begin
  inherited;

  bShowPath := false;
end;

constructor TPlanet.Create();
begin
  inherited;

  dtConP:=0;
  dtOPDateP:=0;
  dtCon:=0;
  dtOPDate:=0;
  dtConN:=0;
  dtOPDateN:=0;

  IMG := TImage.Create(nil);
  IMG.Proportional:=true;
  IMG.Stretch:=true;
  IMG.Cursor:=crCross;

  bShowPath := false;
end;

procedure TPlanet.SetImage();
begin
  if(FileExists(ConvertWinPath(gsAlbireoLocalDir + 'img\' + sName_EN + '.png'))) then
  begin
    IMG.Picture.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\' + sName_EN + '.png'));

    if(sName_EN = 'Saturn') then
    begin
      IMG.Width := 40;
      IMG.Height := 40;
    end
    else if (sName_EN = 'Pluto') then
    begin
      IMG.Width := 10;
      IMG.Height := 10;
    end
    else
    begin
      IMG.Width := 20;
      IMG.Height := 20;
    end;
    IMG.Transparent:=true;

    bHasImage := true;
  end
  else
    bHasImage := false;
end;

constructor TCamera.Create();
begin
  iSensorFormatIndex := -1;
  fMegaPixel := 0;
  sSensorType := 'CMOS';

  iQEff := 32;
  iFWC := 50000;
  fDCRC := 10;
  fDCRT := 25;
  fDCHT := 6.3;
  iRN := 5;
  iBits := 12;
end;

end.

