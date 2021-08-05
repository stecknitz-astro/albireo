unit U_AOVis;

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
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TATools,
  TATransformations, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Grids, ComCtrls, Buttons, ExtDlgs, ContNrs, StrUtils, types, Math, DateUtils,
  U_ABase, U_AConst, U_ALib, U_AlbireoLib, U_Translation, U_PictureViewer;

type

  { TF__AOVIS }

  TF__AOVIS = class(TForm)
    B__ADD_USR_PIC: TButton;
    B__PHOTO_ADD: TButton;
    B__PHOTO_DEL: TButton;
    CBX__REL: TCheckBox;
    CB__MOONS: TComboBox;
    CB__MSTAR_SEL: TComboBox;
    CHRT__MOONPOS: TChart;
    CHART__SPEC: TChart;
    CHRT__MOONPOSConstantLine1: TConstantLine;
    CHRT__MOONPOSConstantLine2: TConstantLine;
    CHRT__MOONPOSConstantLine3: TConstantLine;
    CHRT__MOONPOSLineSeries1: TLineSeries;
    GRD__HRD_MK: TStringGrid;
    IMG__FULLCONVECTION: TImage;
    L__REL: TLabel;
    L__PLANET: TLabel;
    L__MOON1: TLabel;
    L__MOON2: TLabel;
    L__MOON3: TLabel;
    L__MOON4: TLabel;
    L__MOON5: TLabel;
    L__MOONDATEPOS: TLabel;
    L__MOONPOS_MONTH: TLabel;
    L__MOONPOS_TITLE: TLabel;
    L__MOONPOS_EAST: TLabel;
    L__MOONPOS_WEST: TLabel;
    LINE_VIOLETT: TConstantLine;
    LINE__RED: TConstantLine;
    LINE__SUNCMP: TLineSeries;
    ODLG__JPG: TOpenPictureDialog;
    P__SCALE_OPTIONS: TPanel;
    P__SCALING: TPanel;
    P__PLANET_MOONS: TPanel;
    P__MOONPOS: TPanel;
    P__HRDATA: TPanel;
    ED__COMMENT: TEdit;
    GRD__AOV_PROP: TStringGrid;
    GRD__MOON: TStringGrid;
    GRD__STARSTRUCT: TStringGrid;
    IMG_HRD: TImage;
    IMG__AO: TImage;
    IMG__AOVIS: TImage;
    IMG__AO_USER_1: TImage;
    IMG__AO_USER_2: TImage;
    IMG__AO_USER_3: TImage;
    IMG__CONVECTION: TImage;
    IMG__CORE_FE: TImage;
    IMG__MOON: TImage;
    IMG__RADIATION: TImage;
    IMG__REF: TImage;
    L__MOONINFO: TLabel;
    L__OBJINFO: TLabel;
    L__1_TITLE1: TLabel;
    L__AO: TLabel;
    L__REF: TLabel;
    ODLG: TOpenPictureDialog;
    P__COMMENT_TITLE: TPanel;
    P__USERCOMMENT: TPanel;
    P__IMAGE3: TPanel;
    P__IMAGE2: TPanel;
    P__IMAGE1: TPanel;
    P__MOONS_INFO: TPanel;
    PC__MAIN_VISU: TPageControl;
    PC__OBSERVATION: TPageControl;
    P__AOVIS_CAPTION: TPanel;
    P__AOV_PHOTOBUTTONS: TPanel;
    P__AOV_VISU: TPanel;
    P__DATA: TPanel;
    P__HRD: TPanel;
    P__INFO_NO_PICTURE: TPanel;
    P__MAIN_PICTURE: TPanel;
    P__MAIN_SIZECOMP: TPanel;
    P__MOONS: TPanel;
    P__MYPHOTOS: TPanel;
    P__VISU: TPanel;
    SERIES__CSPEC: TLineSeries;
    SHP__MOON1: TShape;
    SHP__MOON2: TShape;
    SHP__MOON3: TShape;
    SHP__MOON4: TShape;
    SHP__MOON5: TShape;
    SHP__PLANET: TShape;
    SHP__ASTEROID: TShape;
    SHP__CORE: TShape;
    SHP__CORE2: TShape;
    SHP__CORE3: TShape;
    SHP__CORE_HE: TShape;
    SHP__HRD: TShape;
    SHP__REF: TShape;
    TB__AOV: TTrackBar;
    TGB__STARSTRUCT: TToggleBox;
    TIMER__COO: TTimer;
    TS__GRAPHICS: TTabSheet;
    TS__HRD: TTabSheet;
    TS__MOONS: TTabSheet;
    TS__OBSERVATION_PHOTO_1: TTabSheet;
    TS__OBSERVATION_PHOTO_2: TTabSheet;
    TS__OBSERVATION_PHOTO_3: TTabSheet;
    TS__PHOTOS: TTabSheet;
    TS__SIZECOMP: TTabSheet;
    procedure B__ADD_USR_PICClick(Sender: TObject);
    procedure B__MYPHOTOSClick(Sender: TObject);
    procedure B__PHOTO_ADDClick(Sender: TObject);
    procedure B__PHOTO_DELClick(Sender: TObject);
    procedure CBX__RELClick(Sender: TObject);
    procedure CB__MOONSChange(Sender: TObject);
    procedure CB__MSTAR_SELChange(Sender: TObject);
    procedure ED__COMMENTChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure GBX__AOMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GRD__STARSTRUCTDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure IMG__AOClick(Sender: TObject);
    procedure IMG__AOMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure IMG__AOVISClick(Sender: TObject);
    procedure IMG__AO_USER_1Click(Sender: TObject);
    procedure IMG__AO_USER_2Click(Sender: TObject);
    procedure IMG__AO_USER_3Click(Sender: TObject);
    procedure IMG__MOONClick(Sender: TObject);
    procedure IMG__REFMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure L__AOMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure L__REFMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PC__MAIN_VISUChange(Sender: TObject);
    procedure P__MAIN_SIZECOMPMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SHP__AOMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SHP__AOVISMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__REFMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure TB__AOVMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TGB__STARSTRUCTClick(Sender: TObject);
    procedure TIMER__COOTimer(Sender: TObject);

  private
    { private declarations }
    molMoonList: TObjectList;
    msMissingFile: string;

    procedure CalcAOWidth(sType: string; iRefWidth: Integer);
    function GetUserFileID(): string;
    function GetUserFileIndex(sID: string): Integer;
    procedure ShowAOData_S();
    procedure ShowAOData_P();
    procedure ShowAOData_C();
    procedure ShowAOData_G();
    procedure ShowAOData_Q();
    procedure ShowAOData_N();
    procedure ShowAOData_PN();
    procedure ShowAOData_OC();
    procedure ShowAOData_GC();
    procedure ShowAOPicture(bIni: Boolean; iIndex: Integer; sFileName: string);
    procedure ShowAOData_Sun();
    procedure ShowAOData_Moon();
    procedure ShowAOData_Earth();
    procedure ShowPicture();
    procedure ZoomObjects(iWheelDelta: Integer);
    procedure ShowMoon(iMoonIndex: Integer);
    procedure ShowStarStruc(iMStarIndex: Integer);
    procedure PutRA_DEC(iType: Word);
    procedure PutImageLicenseData(sLicenseName, sLicenseURL, sAuthor, sTitle, sSourceURL: string);
    procedure GenStarSpecChart(fTemp: Real; Clr: TColor; iMode: SmallInt);
    procedure ShowAstroPic(bPicExists: Boolean; sPath: string);
    procedure VisuAOData();
    procedure ShowStarImage(IMG: TImage; sSpecID: string);

  public
    { public declarations }
    msAlbireoLocalDir: string;

    mAObject: TAObject;
    msLANG_ID: string;
    mslUserFiles: TStringList;
    msUserPicFileName: string;
    molSignList: TObjectList;

    mrSin_fGLat, mrCos_fGLat: Real;
    miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN: SmallInt;
    mdtWT: TDateTime;
    mbTimePlay: Boolean;
    miAltRow, miLatRow: Integer;

    miMStarCnt: Integer; // 0: Single Star, 1: Double Star; 2: Triple Star, ...
    miMStarIndex: Integer; // Current Multi-Star Index (0,1,...)

  end; 

var
  F__AOVIS: TF__AOVIS;

const
  csPictureRes = 'AlbireoImageData.dat';

implementation

{$R *.lfm}

{ TF__AOVIS }

procedure TF__AOVIS.VisuAOData();
var
  iLKK: Integer;
  iSpecType: Integer;
  sSpec: string;
  iTMin,iTMax,iSTemp: Integer;
begin
  P__SCALE_OPTIONS.Visible:=false;
  CBX__REL.Checked:=false;
  iTMin:=0; iTMax:=0; iSTemp:=0;

  if((mAObject as TAObject).sAOType = 'S') then
    sSpec := GetSingleSpecType((mAObject as TStar).sSpType,miMStarIndex)
  else
    sSpec := '';

  case PC__MAIN_VISU.ActivePageIndex of
    0: ShowStarStruc(miMStarIndex);
    1:
    begin
      if((mAObject as TAobject).sAOType = 'MOON') then
      begin
        P__SCALE_OPTIONS.Visible:=true;
        CBX__REL.Checked:=true;
        TB__AOV.Position := 490;
      end;

      CalcAOWidth(mAObject.sAOType,TB__AOV.Max - TB__AOV.Position);
    end;
    2: // Reserved only for stars
    begin
      if((mAObject as TStar).sAOType = 'SUN') then
      begin
        SHP__HRD.Visible := false; // SUN is already shown in HRD as reference!
        exit;
      end;

      GetSpecIDData(sSpec,iTMin,iTMax,iSTemp);

      if (iSTemp > 0) then
        GenStarSpecChart(iSTemp,GetColorFromSpecType(sSpec),1);

      SHP__HRD.Brush.Color := GetColorFromSpecType(sSpec);

      iSpecType := GetHR_XProz_FromSpecType(sSpec);
      iLKK := GetHR_YProz_FromSpecType(sSpec);

      if((iSpecType = -1) or (iLKK = -1)) then
      begin
        if(msLANG_ID = 'DE') then
          MessageDlg('Keine Daten','HRD-Position kann nicht bestimmt werden',mtInformation,[mbOK],0)
        else
          MessageDlg('Missing Data','Unable to calcaulate HRD position',mtInformation,[mbOK],0);

        SHP__HRD.Visible:=false;

        exit;
      end;

      SHP__HRD.Left := 70 + Round(1050 * iSpecType/100);

      // 50%: Hauptreihe: Prozentwert justieren
      if(iLKK = 55) then
      begin
        iLKK :=  55 - Round(0.7*(55 - iSpecType));
      end
      else if(iLKK = 80) then
      begin
        iLKK := 80 - Round(0.3*(55 - iSpecType));
      end
      else if(iLKK = 35) then
      begin
        if(iSpecType < 50) then // Asymptotischer Ast der Unterriesen
        begin
          iLKK := iLKK - Round(0.3*(50 - iSpecType));
        end;
      end
      else if(iLKK = 30) then // Asymptotischer Ast der Riesen
      begin
        if(iSpecType < 50) then // Asymptotischer Ast der Unterriesen
        begin
          iLKK := iLKK - Round(0.4*(50 - iSpecType));
        end;
      end;

      SHP__HRD.Top := Round(703 * iLKK/100);

      SHP__HRD.Visible:=true;

    end;
  end; // case
end;

procedure TF__AOVIS.GenStarSpecChart(fTemp: Real; Clr: TColor; iMode: SmallInt);
var
  i: Integer;
  fNu,fExp,fFac,fX, fY: Real;
  fH, fK, fC: Real;
begin
  CHART__SPEC.Title.Text.Clear;
  if(msLANG_ID = 'DE') then
    CHART__SPEC.Title.Text.Add('Kont. Spektrum')
  else
    CHART__SPEC.Title.Text.Add('Cont Spectrum');

  fH := 6.626e-34;
  fK := 1.381e-23;
  fC := 299.792e6;

  (CHART__SPEC.Series[0] as TLineSeries).Clear;
  (CHART__SPEC.Series[0] as TLineSeries).SeriesColor := Clr;
  (CHART__SPEC.Series[3] as TLineSeries).Clear;

  //for i:= 1 to 5000 do
  i := 1;
  while (i < 10000) do
  begin
    fNu := fC/i * 1e9;
    fExp := fH*fNu/(fK*fTemp);
    fFac := 2.0*fH*fNu*fNu*fNu*fNu*fNu/(fC*fC);

    fX := i;
    fY := fFac/(exp(fExp) - 1);

    (CHART__SPEC.Series[0] as TLineSeries).AddXY(fX,fY);

    i := i + 10;
  end; // while

  i:=1;
  // Draw Sun compare line
  while (iMode = 1) and (i < 10000) do
  begin
    fNu := fC/i * 1e9;
    fExp := fH*fNu/(fK*5777);
    fFac := 2.0*fH*fNu*fNu*fNu*fNu*fNu/(fC*fC);

    fX := i;
    fY := fFac/(exp(fExp) - 1);

    (CHART__SPEC.Series[3] as TLineSeries).AddXY(fX,fY);

    i := i + 10;
  end; // while

  (CHART__SPEC.Series[1] as TConstantLine).Position:=400;
  (CHART__SPEC.Series[2] as TConstantLine).Position:=700;
  CHART__SPEC.BottomAxis.Range.Min:=1;
  CHART__SPEC.BottomAxis.Range.UseMin:=true;

end;

procedure TF__AOVIS.ShowMoon(iMoonIndex: Integer);
var
  Moon: TMoon;
  i, iCnt: Integer;
  iStartDay, iEndDay: Integer;
  iYear,iMonth,iDay: Word;
  rDayBuf, rDay0, rAmp, rAmpDate: Real;
  sMonthName, sDir: string;
begin
  if(iMoonIndex < 0) or (iMoonIndex > CB__MOONS.Items.Count) then exit;

  if(CB__MOONS.Items.Objects[iMoonIndex] = nil) then exit;

  iCnt := 200;

  // Show moon object details in data grid
  Moon := (CB__MOONS.Items.Objects[iMoonIndex] as TMoon);

  if(msLANG_ID = 'DE') then
  begin
    GRD__MOON.Cells[0,0] := 'Eigenschaft';
    GRD__MOON.Cells[1,0] := 'Wert';

    GRD__MOON.Cells[0,1] := 'Durchmesser [km]';
    GRD__MOON.Cells[1,1] := IntToStr(Round(2*Moon.rRadius_km));

    GRD__MOON.Cells[0,2] := 'Masse [kg]';
    GRD__MOON.Cells[1,2] := FloatToStr(Moon.rMass_kg) + ' E' + IntToStr(Round(Moon.rMass_exp));

    GRD__MOON.Cells[0,3] := 'Dichte [g/ccm]';
    GRD__MOON.Cells[1,3] := FloatToStr(Moon.rRho_g_qcm);

    GRD__MOON.Cells[0,4] := 'Umlaufzeit [Tagen]';
    GRD__MOON.Cells[1,4] := FloatToStr(Moon.rOrbitalPeriod_d);

    GRD__MOON.Cells[0,5] := 'Große Halbachse [km]';
    GRD__MOON.Cells[1,5] := IntToStr(Round(Moon.rSMAxis_km));

    GRD__MOON.Cells[0,6] := 'Bahnexzentrizität';
    GRD__MOON.Cells[1,6] := FloatToStr(Moon.rE);

    GRD__MOON.Cells[0,7] := 'Scheinbare Helligkeit';
    GRD__MOON.Cells[1,7] := FloatToStr(Moon.rMag) + ' mag';
  end
  else
  begin
    GRD__MOON.Cells[0,0] := 'Property';
    GRD__MOON.Cells[1,0] := 'Value';

    GRD__MOON.Cells[0,1] := 'Diameter [km]';
    GRD__MOON.Cells[1,1] := IntToStr(Round(2*Moon.rRadius_km));

    GRD__MOON.Cells[0,2] := 'Mass [kg]';
    GRD__MOON.Cells[1,2] := FloatToStr(Moon.rMass_kg) + ' E' + IntToStr(Round(Moon.rMass_exp));

    GRD__MOON.Cells[0,3] := 'Density [g/ccm]';
    GRD__MOON.Cells[1,3] := FloatToStr(Moon.rRho_g_qcm);

    GRD__MOON.Cells[0,4] := 'Orbital Period [d]';
    GRD__MOON.Cells[1,4] := FloatToStr(Moon.rOrbitalPeriod_d);

    GRD__MOON.Cells[0,5] := 'Semimajor Axis [km]';
    GRD__MOON.Cells[1,5] := IntToStr(Round(Moon.rSMAxis_km));

    GRD__MOON.Cells[0,6] := 'Eccentricity';
    GRD__MOON.Cells[1,6] := FloatToStr(Moon.rE);

    GRD__MOON.Cells[0,7] := 'Apparent Magnitude';
    GRD__MOON.Cells[1,7] := FloatToStr(Moon.rMag) +  ' mag';
  end;

  // Show Picture
  if((mAObject as TPlanet).sPlanetType = 'E') then
    IMG__MOON.Picture.LoadFromFile(msAlbireoLocalDir + 'img\Moon.jpeg')
  else
    LoadImgRes(IMG__MOON,Moon.sName_EN,'JPG');


  // Show Position Curve
  P__MOONPOS.Visible:=false;

  if (Moon.dtZeroPass > 0) and (Moon.rOrbitalPeriod_d > 0) then
  begin
    DecodeDate(mdtWT,iYear,iMonth,iDay);
    iEndDay := DaysOfMonth(mdtWT) - 1;
    iStartDay := Trunc(EncodeDate(iYear,iMonth,1));

    sMonthName := FormatDateTime('mmmm yyyy',mdtWT);
    sMonthName := SetDE_ENMonthNames(sMonthName,msLANG_ID);
    L__MOONPOS_MONTH.Caption:= sMonthName;

    rAmp := Moon.rSMAxis_km / ((mAObject as TPlanet).rDiameterRatio*12735/2.0);
    P__MOONPOS.Visible:=true;
    (CHRT__MOONPOS.Series[0] as TLineSeries).Clear;

    (CHRT__MOONPOS.Series[1] as TConstantLine).Position := mdtWT - iStartDay + 1;

    rDay0 := Moon.dtZeroPass; // EncodeDate(2020,12,23) + EncodeTime(0,0,0,0);

    rAmpDate := rAmp*sin(2*Pi/Moon.rOrbitalPeriod_d* (mdtWT - rDay0));

    if(msLANG_ID = 'DE') then
    begin
      if(rAmpDate > 0) then sDir := 'Ost'
      else sDir := 'West';

      L__MOONDATEPOS.Caption := 'Mondposition am ' + FormatDateTime('dd.mm.yy',mdtWT) + ' um ' + FormatDateTime('hh:mm',mdtWT) +
        ': ' + FloatToStrF(abs(rAmpDate),ffFixed,8,1) + ' Planetenradien ' + sDir
    end
    else
    begin
      if(rAmpDate > 0) then sDir := 'East'
      else sDir := 'West';

      L__MOONDATEPOS.Caption := 'Moon position ' + FormatDateTime('dd.mm.yy',mdtWT) + ' at ' + FormatDateTime('hh:mm',mdtWT) +
        ': ' + FloatToStrF(abs(rAmpDate),ffFixed,8,1) + ' planet radii ' + sDir;
    end;

    for i:=0 to iCnt do
    begin
      rDayBuf := iStartDay + i*iEndDay/iCnt;
      (CHRT__MOONPOS.Series[0] as TLineSeries).AddXY(rDayBuf - iStartDay + 1,rAmp*sin(2*Pi/Moon.rOrbitalPeriod_d* (rDayBuf - rDay0)));
    end;
  end;

end;

procedure TF__AOVIS.ZoomObjects(iWheelDelta: Integer);
var
  iPos: Integer;
begin
  if(iWheelDelta > 0) and (TB__AOV.Position < TB__AOV.Max) then
  begin
    iPos := TB__AOV.Position + 1;
    if(iPos > TB__AOV.Max) then TB__AOV.Position := TB__AOV.Max
    else TB__AOV.Position := iPos;
  end
  else
  begin
    iPos := TB__AOV.Position - 1;
    if(iPos < TB__AOV.Min) then TB__AOV.Position := TB__AOV.Min
    else TB__AOV.Position := iPos;
  end;

  CalcAOWidth(mAObject.sAOType,TB__AOV.Max - TB__AOV.Position);
end;


procedure TF__AOVIS.ShowPicture();
var
  iPNo: Integer;
begin
  iPNo := PC__OBSERVATION.ActivePageIndex;

  if((iPNo = 0) and (IMG__AO_USER_1.Picture.Height = 0)) then exit;
  if((iPNo = 1) and (IMG__AO_USER_2.Picture.Height = 0)) then exit;
  if((iPNo = 2) and (IMG__AO_USER_3.Picture.Height = 0)) then exit;

  F__PICTUREVIEWER := TF__PICTUREVIEWER.Create(nil);
  F__PICTUREVIEWER.msLANG_ID:=msLANG_ID;
  IniText(F__PICTUREVIEWER,msLANG_ID);

  case iPNo of
    0:
      F__PICTUREVIEWER.IMG__PIC.Picture := IMG__AO_USER_1.Picture;
    1:
      F__PICTUREVIEWER.IMG__PIC.Picture := IMG__AO_USER_2.Picture;
    2:
      F__PICTUREVIEWER.IMG__PIC.Picture := IMG__AO_USER_3.Picture;
  end; // case

  F__PICTUREVIEWER.ShowModal;
  F__PICTUREVIEWER.Destroy;
end;

procedure TF__AOVIS.ShowAOPicture(bIni: Boolean; iIndex: Integer; sFileName: string);
begin
  if(bIni) then
    case iIndex of
      0: IMG__AO_USER_1.Picture := nil;
      1: IMG__AO_USER_2.Picture := nil;
      2: IMG__AO_USER_3.Picture := nil;
    end
  else
    if(FileExists(sFileName)) then
    begin
      case iIndex of
        0: IMG__AO_USER_1.Picture.LoadFromFile(sFileName);
        1: IMG__AO_USER_2.Picture.LoadFromFile(sFileName);
        2: IMG__AO_USER_3.Picture.LoadFromFile(sFileName);
      end; // case..
    end; // if..

end;

function TF__AOVIS.GetUserFileID(): string;
var
  sID: string;
begin
  if(mAObject.sAOType = 'P') then
    sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|' + mAObject.sName_EN
  else if(mAObject.sAOType = 'C') then
    sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|' + mAObject.sName_EN
  else if(mAObject.sAOType = 'S') then
  begin
    if(((mAObject as TStar).sSym <> '') and ((mAObject as TStar).sCon <> '')) then
      sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|' + (mAObject as TStar).sSym + (mAObject as TStar).sCon
    else
      sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|' + (mAObject as TStar).sCatNo;

  end
  else if (mAObject.sAOType = 'SUN') then
    sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|SUN'
  else if (mAObject.sAOType = 'MOON') then
    sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|MOON'
  else if ((mAObject.sAOType = 'EARTH') or (mAObject.sAOType = 'E')) then
    sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|EARTH'
  else
    sID := mAObject.sAOType + '~' + IntToStr(PC__OBSERVATION.ActivePageIndex + 1) + '|' + (mAObject as TInterstellarObject).sNGC;

  Result := AnsiReplaceStr(sID,' ','#');
end;

function TF__AOVIS.GetUserFileIndex(sID: string): Integer;
var
  i, iIndex: Integer;
  bFound: Boolean;
begin
  bFound := false;

  iIndex := -1;
  i:=0;
  while ((not bFound) and (i < mslUserFiles.Count)) do
  begin
    if(LeftStr(mslUserFiles[i],length(sID)) = sID) then
    begin
      iIndex := i;
      bFound := true;
    end;
    Inc(i);
  end;

  Result := iIndex;
end;

procedure TF__AOVIS.ShowStarImage(IMG: TImage; sSpecID: string);
begin
  if(FileExists(ConvertWinPath(msAlbireoLocalDir + 'img\' + sSpecID + 'Star.jpg'))) then
    IMG.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\' + sSpecID + 'Star.jpg'))
  else if(sSpecID = 'R') or (sSpecID = 'N') or (sSpecID = 'S') or (sSpecID = 'C') then
      IMG.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\MStar.jpg'))
  else
    IMG.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\AStar.jpg'));
end;

procedure TF__AOVIS.CalcAOWidth(sType: string; iRefWidth: Integer);
var
  rAFac: Real;
  sCmpImg, sSpecType, sSpecID: string;
  iTMin,iTMax,iSTemp: Integer;
  rSolFrac: Real;
begin
  iTMin := 0; iTMax := 0; iSTemp := 0;
  sSpecTYpe := ''; sSpecID := '';

  if((Trim(sType) = '') or (iRefWidth <= 0)) then
    exit;

  if(sType = 'S') then // Star and Sun
  begin
    sSpecType := GetSingleSpecType((mAObject as TStar).sSpType,miMStarIndex);
    sSpecID := GetSpecIDData(sSpecType,iTMin,iTMax,iSTemp);

    SHP__REF.Height := iRefWidth;
    SHP__REF.Width := iRefWidth;

    IMG__AOVIS.Visible := true;
    IMG__REF.Visible := false;

    ShowStarImage(IMG__AO,sSpecID);

    rSolFrac := 0;

    if(miMStarIndex = 0) and ((mAObject as TStar).rSolFrac <= 0) then
    begin
      (mAObject as TStar).rSolFrac := GenSolFrac(sSpecType);  // Main star of a multiple star system
      rSolFrac := (mAObject as TStar).rSolFrac;
    end
    else if ((miMStarIndex = 0) and ((mAObject as TStar).rSolFrac > 0)) then
      rSolFrac := (mAObject as TStar).rSolFrac
    else if(miMStarIndex > 0) then
    begin
      rSolFrac := GenSolFrac(sSpecType); // Estimated Value for multiple star components
    end;

    if(rSolFrac > 0) then
      IMG__AO.Height := Round(rSolFrac*SHP__REF.Height);

    IMG__AO.Width := IMG__AO.Height;

    L__REF.Left := SHP__REF.Left + SHP__REF.Width + 2;
    L__REF.Top := SHP__REF.Top - 2;
    if(SHP__REF.Width < 10) then
      L__REF.Font.Size := 10
    else
      L__REF.Font.Size:= SHP__REF.Width div 2;

    if(msLANG_ID = 'DE') then
      L__REF.Caption:='SONNE'
    else
      L__REF.Caption:='SUN';

    L__REF.Visible:=true;


    if(IMG__AO.Width <= 500) then
    begin
      L__AO.Left := IMG__AO.Left + SHP__REF.Width + 2;
      L__AO.Top := IMG__AO.Top + IMG__AO.Height + 2;
    end
    else
    begin
      L__AO.Left := IMG__AO.Left;
      L__AO.Top := IMG__AO.Top;
    end;

    if(IMG__AO.Width <= 100) then
      L__AO.Font.Size:= IMG__AO.Width div 2
    else
      L__AO.Font.Size:= 50;

    if(msLANG_ID = 'DE') then
      L__AO.Caption:=(mAObject as TStar).sName_DE
    else
      L__AO.Caption:=(mAObject as TStar).sName_EN;

    L__AO.Visible:=true;
  end;

  if(sType = 'P') then // Planet and Earth
  begin
    // Compare asteroids with Moon instead of Earth because of the small sizes
    if((mAObject as TPlanet).sPlanetType <> 'P') then
    begin
      rAFac := 3.67;
      sCmpImg := 'MoonCmp.png'
    end
    else
    begin
      rAFac := 1.0;
      sCmpImg := 'Earth.png'
    end;

    if(not FileExists(ConvertWinPath(msAlbireoLocalDir + 'img\' + sCmpImg))) then
      MessageDlg('Err: ','Unable to open file ' + ConvertWinPath(msAlbireoLocalDir + 'img\' + sCmpImg),mtError,[mbOK],0);

    IMG__REF.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\' + sCmpImg));
    IMG__REF.Height := iRefWidth;
    IMG__REF.Width := iRefWidth;
    IMG__REF.Visible := true;
    SHP__REF.Visible := false;
    SHP__ASTEROID.Visible:=false;

    //ShowMessage('Search for file: ' + msAlbireoLocalDir + 'img\' + mAObject.sName_EN + '.png');

    if(FileExists(ConvertWinPath(msAlbireoLocalDir + 'img\' + mAObject.sName_EN + '.png'))) then
    begin
      IMG__AO.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\' + mAObject.sName_EN + '.png'));
      IMG__AOVIS.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\' + mAObject.sName_EN + '.png'));
      IMG__AOVIS.Visible := true;

      if((mAObject as TPlanet).rDiameterRatio > 0) then
      begin
        IMG__AO.Height := Round((mAObject as TPlanet).rDiameterRatio*IMG__REF.Height * rAFac);
      end;

      IMG__AO.Width := IMG__AO.Height;
    end
    else
    begin
      IMG__AOVIS.Visible := false;
      P__INFO_NO_PICTURE.Visible:=true;

      if((mAObject as TPlanet).rDiameterRatio > 0) then
      begin
        SHP__ASTEROID.Height := Round((mAObject as TPlanet).rDiameterRatio*IMG__REF.Height * rAFac);
        SHP__ASTEROID.Width := SHP__ASTEROID.Height;
        SHP__ASTEROID.Visible := true;
      end;
      (*
      if((mAObject as TPlanet).rDiameterRatio > 0) then
      begin
        IMG__AO.Height := Round((mAObject as TPlanet).rDiameterRatio*IMG__REF.Height * rAFac);
        IMG__AO.Width := IMG__AO.Height;

        IMG__AOVIS.Visible := true;
        IMG__AOVIS.Height := Round((mAObject as TPlanet).rDiameterRatio*IMG__REF.Height * rAFac);
        IMG__AOVIS.Width := IMG__AO.Height;
      end
      else
      begin
        IMG__AOVIS.Visible := false;
      end;
      *)
    end;
  end;

  if(sType = 'SUN') and (iRefWidth > 0) then // Sun and Earth
  begin
    //ShowMessage('iRefWidth: ' + IntToStr(iRefWidth));

    IMG__REF.Height := iRefWidth;
    IMG__REF.Width := iRefWidth;

    SHP__REF.Visible := false;
    IMG__REF.Visible := true;

    IMG__AOVIS.Visible := true;

    IMG__REF.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\Earth.png'));

    IMG__AO.Height := Round(695842.0/6371.0 *IMG__REF.Height);

    IMG__AO.Width := IMG__AO.Height;
  end;

  if(sType = 'MOON') then  // Moon and earth
  begin
    IMG__REF.Height := iRefWidth;
    IMG__REF.Width := iRefWidth;

    IMG__AOVIS.Visible := true;
    IMG__REF.Visible := true;
    SHP__REF.Visible := false;

    IMG__REF.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\Earth.png'));
    //IMG__AO.Picture.LoadFromFile(ConvertWinPath('img/MoonExp.png'));
    //IMG__AOVIS.Picture.LoadFromFile(ConvertWinPath('img/MoonExp.png'));
    IMG__AO.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\Moon.png'));
    IMG__AOVIS.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\Moon.png'));
    IMG__AOVIS.Tag := 0;

    IMG__AO.Height := Round( 1737.0/6371.0*IMG__REF.Height);
    IMG__AO.Width := IMG__AO.Height;

    if(CBX__REL.Checked) then
      IMG__REF.Left := (IMG__AO.Left + IMG__AO.Width div 2) + IMG__AO.Height* (384000 div (2*1737)) // True distance between earth and moon
    else
      IMG__REF.Left := 1025;

  end;

end;

procedure TF__AOVIS.ShowStarStruc(iMStarIndex: Integer);
var
  iXCenter, iYCenter: Integer;
  sName_Surface, sName_Surface_H_He_IA: string;
  sName_CZone, sName_FCZone, sName_CZone_HT: string;
  sName_RZone, sName_RZone_HT: string;
  sName_Core, sName_Core_H, sName_Core_deg: string;
  sName_He_IA, sName_C_O_IA, sName_He_50MK_IA, sName_F_HE_Fe, sName_Fe_IA: string;
  sSPType, sSpecID: string;
  iTMin,iTMax,iSTemp: Integer;
begin
  if((mAObject as TAObject).sAOType <> 'S') and ((mAObject as TAObject).sAOType <> 'SUN') then
    exit;

  iTMin := 0; iTMax := 0; iSTemp := 0;

  GRD__STARSTRUCT.Visible := false;
  SHP__CORE.Visible:=false;
  SHP__CORE2.Visible:=false;
  SHP__CORE_HE.Visible:=false;
  IMG__CORE_FE.Visible:=false;
  SHP__CORE3.Visible:=false;
  IMG__RADIATION.Visible:=false;
  IMG__CONVECTION.Visible:=false;
  IMG__FULLCONVECTION.Visible:=false;

  //ShowMessage(msAlbireoLocalDir + 'img\' + (mAObject as TStar).sSpecID + 'Star.jpg');
  sSPType := GetSingleSpecType((mAObject as TStar).sSpType,iMStarIndex);
  sSpecID := GetSpecIDData(sSPType,iTMin,iTMax,iSTemp);

  ShowStarImage(IMG__AOVIS,sSpecID);

  if(TGB__STARSTRUCT.State = cbUnchecked) then
    exit;

  if(msLANG_ID = 'DE') then
  begin
    sName_Surface := 'Oberfläche';
    sName_FCZone := 'Vollkonvektionszone (H)';
    sName_CZone := 'Konvektionszone (H)';
    sName_CZone_HT := 'WT durch Konvektion';
    sName_RZone := 'Strahlungszone (H)';
    sName_RZone_HT := 'WT durch Strahlung';
    sName_Core := 'Kern';
    sName_Core_H := 'H inaktiv (1 Mio. K)';
    sName_Surface_H_He_IA := 'Oberfläche H+He inaktiv';
    sName_Core_deg := 'Kern (entartet)';
    sName_He_IA := 'He inaktiv';
    sName_He_50MK_IA := 'He inaktiv (50 Mio. K)';
    sName_C_O_IA := 'C + O inaktiv';
    sName_F_HE_Fe  := 'Fusion schwerer Elemente bis Fe (500 Mio K.)';
    sName_Fe_IA := 'Eisenkern (inaktiv)';
  end
  else
  begin
    sName_Surface := 'Surface';
    sName_FCZone := 'Full Convection Zone (H)';
    sName_CZone := 'Convection Zone (H)';
    sName_CZone_HT := 'HT by convection';
    sName_RZone := 'Radiation Zone (H)';
    sName_RZone_HT := 'HT by radiation';
    sName_Core := 'Core';
    sName_Core_H := 'H inactive (1 Mio. K)';
    sName_Surface_H_He_IA := 'Surface H+He inactive';
    sName_Core_deg := 'Core (degenerated)';
    sName_He_IA := 'He inactive';
    sName_He_50MK_IA := 'He inactive (50 Mio. K)';
    sName_C_O_IA := 'C + O inactive';
    sName_F_HE_Fe  := 'Fusion heavier elements up to Fe (500 Mio K.)';
    sName_Fe_IA := 'Ferrum core (inactive)';

  end;

  GRD__STARSTRUCT.Visible := true;
  (*
  iXCenter := P__MAIN_PICTURE.Width div 2;
  iYCenter := P__MAIN_PICTURE.Height div 2;
  *)

  iXCenter := P__VISU.Width div 2;
  iYCenter := P__VISU.Height div 2;

  IMG__AOVIS.Left := iXCenter - (IMG__AOVIS.Width div 2) + 3;
  IMG__AOVIS.Top := iYCenter - (IMG__AOVIS.Height div 2) + 3;

  SHP__CORE.Left := iXCenter - (SHP__CORE.Width div 2);
  SHP__CORE.Top := iYCenter - (SHP__CORE.Height div 2);

  SHP__CORE2.Left := iXCenter - (SHP__CORE2.Width div 2);
  SHP__CORE2.Top := iYCenter - (SHP__CORE2.Height div 2);

  SHP__CORE3.Left := iXCenter - (SHP__CORE3.Width div 2);
  SHP__CORE3.Top := iYCenter - (SHP__CORE3.Height div 2);

  SHP__CORE_HE.Width := Round(SHP__CORE3.Width*0.8);
  SHP__CORE_HE.Height := Round(SHP__CORE3.Height*0.8);
  SHP__CORE_HE.Left := iXCenter - (SHP__CORE_HE.Width div 2);
  SHP__CORE_HE.Top := iYCenter - (SHP__CORE_HE.Height div 2);

  IMG__CONVECTION.Height := Round(IMG__AOVIS.Height*0.9);
  IMG__CONVECTION.Width := IMG__CONVECTION.Height;
  IMG__CONVECTION.Left := iXCenter - (IMG__CONVECTION.Width div 2);
  IMG__CONVECTION.Top := iYCenter - (IMG__CONVECTION.Height div 2);

  IMG__RADIATION.Height := Round(IMG__CONVECTION.Height*0.4);
  IMG__RADIATION.Width := IMG__RADIATION.Height;
  IMG__RADIATION.Left := iXCenter - (IMG__RADIATION.Width div 2);
  IMG__RADIATION.Top := iYCenter - (IMG__RADIATION.Height div 2);

  IMG__FULLCONVECTION.Height := Round(IMG__AOVIS.Height*0.9);
  IMG__FULLCONVECTION.Width := IMG__FULLCONVECTION.Height;
  IMG__FULLCONVECTION.Left := iXCenter - (IMG__FULLCONVECTION.Width div 2);
  IMG__FULLCONVECTION.Top := iYCenter - (IMG__FULLCONVECTION.Height div 2);

  SHP__CORE3.Visible:=true;
  IMG__RADIATION.Visible:=false;
  IMG__CONVECTION.Visible:=false;
  IMG__FULLCONVECTION.Visible:=false;

  // Set legend grid
  if(
    AnsiContainsStr(sSPType,'VI') or AnsiContainsStr(sSPType,'VII') or AnsiContainsStr(sSPType,'D')
    ) then
  begin
    if(AnsiContainsStr(sSPType,'VI') or AnsiContainsStr(sSPType,'SD')) then
    begin
      // Sub- und Weisse Zwerge
      if(AnsiContainsStr(sSPType,'O') or AnsiContainsStr(sSPType,'B') or AnsiContainsStr(sSPType,'A') or AnsiContainsStr(sSPType,'F')) then
      begin
        // Heiße Unterzwerge
        SHP__CORE3.Visible := true;
        SHP__CORE2.Visible := true;
        SHP__CORE.Visible := true;

        SHP__CORE3.Width := Round(IMG__AOVIS.Width*0.9);
        SHP__CORE3.Height := Round(IMG__AOVIS.Height*0.9);
        SHP__CORE3.Left := iXCenter - (SHP__CORE3.Width div 2);
        SHP__CORE3.Top := iYCenter - (SHP__CORE3.Height div 2);

        SHP__CORE2.Width := Round(SHP__CORE3.Width*0.9);
        SHP__CORE2.Height := Round(SHP__CORE3.Height*0.9);
        SHP__CORE2.Left := iXCenter - (SHP__CORE2.Width div 2);
        SHP__CORE2.Top := iYCenter - (SHP__CORE2.Height div 2);

        GRD__STARSTRUCT.RowCount:=4;
        GRD__STARSTRUCT.Cells[0,0] := sName_Surface;

        GRD__STARSTRUCT.Cells[1,0] := IntToStr(iSTemp) + 'K';

        GRD__STARSTRUCT.Cells[0,1] := sName_Core;
        GRD__STARSTRUCT.Cells[1,1] := sName_Core_H;

        GRD__STARSTRUCT.Cells[0,2] := sName_Core;
        GRD__STARSTRUCT.Cells[1,2] := 'Fusion He+He+He->C, He+C->O (100 Mio. K)';

        GRD__STARSTRUCT.Cells[0,3] := sName_Core;
        GRD__STARSTRUCT.Cells[1,3] := 'Fusion C+O (200 Mio. K)';
      end
      else
      begin
        // Kühle Unterzwerge: Wie normale Zwerge
        if(iSTemp > 4000) then
        begin
          IMG__RADIATION.Visible:=true;
          IMG__CONVECTION.Visible:=true;

          GRD__STARSTRUCT.RowCount:=4;
          GRD__STARSTRUCT.Cells[0,0] := sName_Surface;
          GRD__STARSTRUCT.Cells[1,0] := IntToStr(iSTemp) + 'K';

          GRD__STARSTRUCT.Cells[0,1] := sName_CZone;
          GRD__STARSTRUCT.Cells[1,1] := sName_CZone_HT;

          GRD__STARSTRUCT.Cells[0,2] := sName_RZone;
          GRD__STARSTRUCT.Cells[1,2] := sName_RZone_HT;

          GRD__STARSTRUCT.Cells[0,3] := sName_Core;
          GRD__STARSTRUCT.Cells[1,3] := 'Fusion H+H->He (10 Mio. K)';
        end
        else
        begin
          IMG__FULLCONVECTION.Visible:=true;

          GRD__STARSTRUCT.RowCount:=3;
          GRD__STARSTRUCT.Cells[0,0] := sName_Surface;
          GRD__STARSTRUCT.Cells[1,0] := IntToStr(iSTemp) + 'K';

          GRD__STARSTRUCT.Cells[0,1] := sName_FCZone;
          GRD__STARSTRUCT.Cells[1,1] := sName_CZone_HT;

          GRD__STARSTRUCT.Cells[0,2] := sName_Core;
          GRD__STARSTRUCT.Cells[1,2] := 'Fusion H+H->He (10 Mio. K)';
        end;
      end

    end; // VI - Subdwarfs

    if(AnsiContainsStr(sSPType,'VII') or AnsiContainsStr(sSPType,'D')) then
    begin
      // Weiße Zwerge
      SHP__CORE3.Visible := false;
      SHP__CORE2.Visible := true;
      SHP__CORE.Visible := true;

      SHP__CORE2.Brush.Style:=bsDiagCross;
      SHP__CORE.Brush.Style:=bsDiagCross;

      SHP__CORE2.Width := Round(IMG__AOVIS.Width*0.95);
      SHP__CORE2.Height := Round(IMG__AOVIS.Height*0.95);
      SHP__CORE2.Left := iXCenter - (SHP__CORE2.Width div 2);
      SHP__CORE2.Top := iYCenter - (SHP__CORE2.Height div 2);

      SHP__CORE.Width := Round(SHP__CORE2.Width*0.8);
      SHP__CORE.Height := Round(SHP__CORE2.Height*0.8);
      SHP__CORE.Left := iXCenter - (SHP__CORE.Width div 2);
      SHP__CORE.Top := iYCenter - (SHP__CORE.Height div 2);

      GRD__STARSTRUCT.RowCount:=3;
      GRD__STARSTRUCT.Cells[0,0] := sName_Surface_H_He_IA;
      GRD__STARSTRUCT.Cells[1,0] := IntToStr(iSTemp) + 'K';

      GRD__STARSTRUCT.Cells[0,1] := sName_Core_deg;
      GRD__STARSTRUCT.Cells[1,1] := sName_He_IA;

      GRD__STARSTRUCT.Cells[0,2] := sName_Core_deg;
      GRD__STARSTRUCT.Cells[1,2] := sName_C_O_IA;

    end;

  end
  else
  begin
    // Dwarfs, Giants and Hypergiants
    if((not AnsiContainsStr(sSPType,'I')) and (AnsiContainsStr(sSPType,'V')) and (iSTemp<=4000)) then
    begin
      IMG__FULLCONVECTION.Visible:=true;

      GRD__STARSTRUCT.RowCount:=3;
      GRD__STARSTRUCT.Cells[0,0] := sName_Surface;
      GRD__STARSTRUCT.Cells[1,0] := IntToStr(iSTemp) + 'K';

      GRD__STARSTRUCT.Cells[0,1] := sName_FCZone;
      GRD__STARSTRUCT.Cells[1,1] := sName_CZone_HT;

      GRD__STARSTRUCT.Cells[0,2] := sName_Core;
      GRD__STARSTRUCT.Cells[1,2] := 'Fusion H+H->He (10 Mio. K)';
    end
    else
    begin
      IMG__RADIATION.Visible:=true;
      IMG__CONVECTION.Visible:=true;

      GRD__STARSTRUCT.RowCount:=5;
      GRD__STARSTRUCT.Cells[0,0] := sName_Surface;
      GRD__STARSTRUCT.Cells[1,0] := IntToStr(iSTemp) + 'K';

      GRD__STARSTRUCT.Cells[0,1] := sName_CZone;
      GRD__STARSTRUCT.Cells[1,1] := sName_CZone_HT;

      GRD__STARSTRUCT.Cells[0,2] := sName_RZone;
      GRD__STARSTRUCT.Cells[1,2] := sName_RZone_HT;

      GRD__STARSTRUCT.Cells[0,3] := sName_Core;
      GRD__STARSTRUCT.Cells[1,3] := 'Fusion H+H->He (10 Mio. K)';

      GRD__STARSTRUCT.Cells[0,4] := sName_Core;
      GRD__STARSTRUCT.Cells[1,4] := sName_He_IA;
    end;

    if(  // Unterriesen und Riesen
        (AnsiContainsStr(sSPType,'II') or AnsiContainsStr(sSPType,'III'))
      ) then
    begin
      SHP__CORE2.Visible:=true;
      SHP__CORE.Visible:=true;

      GRD__STARSTRUCT.RowCount:=6;
      GRD__STARSTRUCT.Cells[0,4] := sName_Core;
      GRD__STARSTRUCT.Cells[1,4] := 'Fusion He+He+He->C (100 Mio. K)';

      SHP__CORE.Width := Round(SHP__CORE2.Width*0.3);
      SHP__CORE.Height := Round(SHP__CORE2.Height*0.3);
      SHP__CORE.Left := iXCenter - (SHP__CORE.Width div 2);
      SHP__CORE.Top := iYCenter - (SHP__CORE.Height div 2);
      SHP__CORE.Brush.Color:=clSilver;

      GRD__STARSTRUCT.Cells[0,5] := sName_Core;
      GRD__STARSTRUCT.Cells[1,5] := sName_C_O_IA;

    end
    else if (AnsiContainsStr(sSPType,'IV')) then // Unterriesen
    begin
      // Kleinere, dichtere, heißere Wasserstoffschale
      SHP__CORE3.Width := Round(IMG__AOVIS.Width*0.2);
      SHP__CORE3.Height := Round(IMG__AOVIS.Height*0.2);
      SHP__CORE3.Left := iXCenter - (SHP__CORE3.Width div 2);
      SHP__CORE3.Top := iYCenter - (SHP__CORE3.Height div 2);

      SHP__CORE_HE.Width := Round(SHP__CORE3.Width*0.8);
      SHP__CORE_HE.Height := Round(SHP__CORE3.Height*0.8);
      SHP__CORE_HE.Left := iXCenter - (SHP__CORE_HE.Width div 2);
      SHP__CORE_HE.Top := iYCenter - (SHP__CORE_HE.Height div 2);

      SHP__CORE_HE.Visible:=true;
      GRD__STARSTRUCT.RowCount:=5;
      GRD__STARSTRUCT.Cells[1,3] := 'Fusion H+H->He (30 Mio. K)';
      GRD__STARSTRUCT.Cells[0,4] := sName_Core;
      GRD__STARSTRUCT.Cells[1,4] := sName_He_50MK_IA;
    end
    else if(AnsiContainsStr(sSPType,'V')) then
    begin
      // Rest: Hauptreihensterne
      SHP__CORE.Visible:=true;
      SHP__CORE.Brush.Color:=clSilver; // $004860E6;
    end
    else if(AnsiContainsStr(sSPType,'I')) then  // Überriesen
    begin
      SHP__CORE2.Visible:=true;
      SHP__CORE.Visible:=true;

      IMG__CORE_FE.Left := iXCenter - (IMG__CORE_FE.Width div 2);
      IMG__CORE_FE.Top := iYCenter - (IMG__CORE_FE.Height div 2);
      IMG__CORE_FE.Visible:=true;

      GRD__STARSTRUCT.RowCount:=7;
      GRD__STARSTRUCT.Cells[0,4] := sName_Core;
      GRD__STARSTRUCT.Cells[1,4] := 'Fusion He+He+He->C (100 Mio. K)';

      GRD__STARSTRUCT.Cells[0,5] := sName_Core;
      GRD__STARSTRUCT.Cells[1,5] := sName_F_HE_Fe;

      GRD__STARSTRUCT.Cells[0,6] := sName_Core;
      GRD__STARSTRUCT.Cells[1,6] := sName_Fe_IA;

    end;

  end;

end;

procedure TF__AOVIS.PutRA_DEC(iType: Word);
var
  iSign: Integer;
  sSign: string;
  rAz, rHgt: Real;
  iDEC_DEG, iDEC_MM: SmallInt;
  iRA_HH, iRA_MM: Word;
  rLambdaSun, rMSun, rRA_SEC, rDEC_SEC: Real;
begin
  rAz := -1;
  rHgt := -1;

  iRA_HH:=0; iRA_MM:=0; rRA_SEC:=0; iDEC_DEG:=0; iDEC_MM:=0; rDEC_SEC:=0;
  rLambdaSun:=0; rMSun:=0;

  case iType of
    0: // Sun
    begin
      // Calc Plot RA and DEC of the Sun
      GetSunCoo(mdtWT,
        miDST_HH,miUTC_HH, rLambdaSun, rMSun,
        iRA_HH, iRA_MM, rRA_SEC,
        iDEC_DEG, iDEC_MM, rDEC_SEC);

    end;
    1: // Moon
    begin
      // Calc Plot RA and DEC of the Moon
      GetSunCoo(mdtWT,
        miDST_HH,miUTC_HH, rLambdaSun, rMSun,
        iRA_HH, iRA_MM, rRA_SEC,
        iDEC_DEG, iDEC_MM, rDEC_SEC);

      iRA_HH:=0; iRA_MM:=0; rRA_SEC:=0; iDEC_DEG:=0; iDEC_MM:=0; rDEC_SEC:=0;

      GetMoonCoo(mdtWT,
        miDST_HH,miUTC_HH,rLambdaSun,rMSun,
        iRA_HH, iRA_MM, rRA_SEC,
        iDEC_DEG, iDEC_MM, rDEC_SEC);

    end
    else
    begin
      iRA_HH := mAObject.iRA_Hours; iRA_MM := mAObject.iRA_Min; rRA_SEC := mAObject.rRA_Sec;
      iDEC_DEG := mAObject.iDec_Deg; iDEC_MM := mAObject.iDec_Min; rDEC_SEC := mAObject.rDec_Sec;
    end

  end;

  EquToAZCoo(mdtWT,
    miDST_HH,miUTC_HH,
    miGLng_DEG,miGLng_MIN,
    mrSin_fGLat, mrCos_fGLat,
    iDEC_DEG, iDEC_MM, rDEC_SEC,
    iRA_HH, iRA_MM, rRA_SEC,
    rAz, rHgt
    );

  rAz := rAz - 180;
  if(rAz < 0) then
    rAz := rAz + 360;

  iSign := 1;
  sSign := '';

  if(iDEC_DEG < 0) then
    iSign := -1
  else if ((iDEC_DEG = 0) and (iDEC_MM < 0)) then
    iSign := -1
  else if((mAObject.iDEC_DEG = 0) and (iDEC_MM = 0) and (rDEC_SEC < 0)) then
    iSign := -1;

  if(iSign = -1) then
    sSign := '-';

  if(msLANG_ID = 'DE') then
  begin
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Koordinaten';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Rektaszension';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := Format('%.2d',[iRA_HH]) + ':' + Format('%.2d',[iRA_MM]) + ':' + Format('%.2d',[Trunc(rRA_SEC)]); //FloatToStrF(mAObject.rRA_Sec,ffFixed,1,8);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Deklination';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sSign +
      Format('%.2d',[abs(iDEC_DEG)]) + ':' + Format('%.2d',[abs(iDEC_MM)]) + ':' + Format('%.2d',[Trunc(abs(rDEC_SEC))]);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    miAltRow := GRD__AOV_PROP.RowCount-1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Höhe';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF(rHgt,ffFixed,1,2) + '°';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    miLatRow := GRD__AOV_PROP.RowCount-1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Azimut';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF(rAz,ffFixed,1,2) + '°';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end
  else
  begin
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Coordinates';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'R.A.';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := Format('%.2d',[iRA_HH]) + ':' + Format('%.2d',[iRA_MM]) + ':' + Format('%.2d',[Trunc(rRA_SEC)]); //FloatToStrF(mAObject.rRA_Sec,ffFixed,1,8);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Declination';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sSign +
    Format('%.2d',[iDEC_DEG]) + ':' + Format('%.2d',[iDEC_MM]) + ':' + Format('%.2d',[Trunc(rDEC_SEC)]);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    miAltRow := GRD__AOV_PROP.RowCount-1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Altitude';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF(rHgt,ffFixed,1,2) + '°';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    miLatRow := GRD__AOV_PROP.RowCount-1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Latitude';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF(rAz,ffFixed,1,2) + '°';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if(not TIMER__COO.Enabled) and (mbTimePlay) then
    TIMER__COO.Enabled := true;

end;

procedure TF__AOVIS.ShowAOData_S();
var
  sMKKTypeDE,sMKKTypeEN: string;
  sSpecType: string;
  iTMin,iTMax,iSTemp: Integer;
begin
  TIMER__COO.Enabled:=false;

  sSpecType := '';
  iTMin := 0; iTMax := 0; iSTemp := 0;

  sMKKTypeDE:=''; sMKKTypeEN:='';
  GRD__AOV_PROP.RowCount := 2; // Because of Multistar usage the grid must be correctly initalized each call of this procedure

  GetMKKFromSpecType((mAObject as TStar).sSpType,miMStarIndex,sMKKTypeDE,sMKKTypeEN);

  if(((mAObject as TStar).sSym <> '') and ((mAObject as TStar).sCon <> '')) then
    P__AOVIS_CAPTION.Caption := (mAObject as TStar).sSym + ' ' + (mAObject as TInterStellarObject).sCon
  else
    P__AOVIS_CAPTION.Caption := (mAObject as TStar).sCatNo;

  PutRA_DEC(10);

  sSpecType := GetSingleSpecType((mAObject as TStar).sSpType,miMStarIndex);
  GetSpecIDData(sSpecType,iTMin,iTMax,iSTemp);

  if(msLANG_ID = 'DE') then
  begin
    if((mAObject as TStar).sName_DE <> '') and ((mAObject as TStar).sName_DE <> P__AOVIS_CAPTION.Caption) then
      P__AOVIS_CAPTION.Caption := (mAObject as TStar).sName_DE + ' (' + P__AOVIS_CAPTION.Caption + ')';

    if(sMKKTypeDE <> '') then
      P__AOVIS_CAPTION.Caption := P__AOVIS_CAPTION.Caption + ' - ' + sMKKTypeDE;

    if(mAObject.sName_DE <> '') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Bezeichnung';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := mAObject.sName_DE;
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

      IMG__AO.Hint := mAObject.sName_DE;
      SHP__REF.Hint:= 'Sonne';
      IMG__AOVIS.Hint := mAObject.sName_DE;
    end;

    if((mAObject as TStar).sCon<> '') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sternbild';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := GetSignName((mAObject as TStar).sCon,molSignList,msLANG_ID);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Scheinbare Helligkeit m';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr(mAObject.rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if((mAObject as TInterStellarObject).rDist_XLY > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung [LJ]';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TInterStellarObject).rDist_XLY);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

      if(mAObject.rM > -999) then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Absolute Helligkeit M';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF(
          mAObject.rM - 5*Log10((mAObject as TInterStellarObject).rDist_XLY / 3.26) + 5,ffFixed,8,1
          );
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;

    end;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Spektraltyp';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TStar).sSpType;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Oberflächentemperatur';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr(iSTemp) + 'K';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if (iSTemp > 0) then
      GenStarSpecChart(iSTemp,GetColorFromSpecType(sSpecType),1);

    if((mAObject as TStar).rSolFrac > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Verhältnis Sonnenradius';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rSolFrac);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    if((mAObject as TStar).rVarMMax > -999) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Veränderlich: M-Bereich';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rVarMMax) +
        ' - ' + FloatToStr((mAObject as TStar).rVarMMin);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

      if((mAObject as TStar).sVarType <> '') then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Typ';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TStar).sVarType;
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;

      if((mAObject as TStar).rVarPer_D > 0) then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Periode [Tagen]';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rVarPer_D);
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;
    end;
  end;

  if((mAObject as TStar).rMAttend > 0) then
  begin
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Doppel/Mehrfach-Stern: M-Begleiter';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rMAttend);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if((mAObject as TStar).iSepSec > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Trennungswinkel ['''']';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TStar).iSepSec);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    if((mAObject as TStar).rVarPer_D > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Pos.-Winkel [°]';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TStar).iSepAngle);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;
  end;

  if(msLANG_ID = 'EN') then
  begin
    if((mAObject as TStar).sName_EN <> '') and ((mAObject as TStar).sName_EN <> P__AOVIS_CAPTION.Caption) then
      P__AOVIS_CAPTION.Caption := (mAObject as TStar).sName_EN + ' (' + P__AOVIS_CAPTION.Caption + ')';

    if(sMKKTypeDE <> '') then
      P__AOVIS_CAPTION.Caption := P__AOVIS_CAPTION.Caption + ' - ' + sMKKTypeEN;

    if(mAObject.sName_EN <> '') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Name';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := mAObject.sName_EN;
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

      IMG__AO.Hint := mAObject.sName_EN;
      SHP__REF.Hint:= 'Sun';
      IMG__AOVIS.Hint := mAObject.sName_EN;
    end;

    if((mAObject as TStar).sCon<> '') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Constellation';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := GetSignName((mAObject as TStar).sCon,molSignList,msLANG_ID);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Appearent Magnitude m';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr(mAObject.rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if((mAObject as TInterStellarObject).rDist_XLY > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance [LY]';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TInterStellarObject).rDist_XLY);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

      if(mAObject.rM > -999) then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Absolute Magnitude M';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF(
          mAObject.rM - 5*Log10((mAObject as TInterStellarObject).rDist_XLY / 3.26) + 5,ffFixed,8,1
          );
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;
    end;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Spec.-Type';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TStar).sSpType;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Surface Temperatur';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr(iSTemp) + 'K';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if (iSTemp > 0) then
      GenStarSpecChart(iSTemp,GetColorFromSpecType(sSpecType),1);
      //GenStarSpecChart((mAObject as TStar).iSTemp,GetColorFromSpecType((mAObject as TStar).sSpType),1);

    if((mAObject as TStar).rSolFrac > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Ratio Sun Radius';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rSolFrac);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    if((mAObject as TStar).rVarMMax > -999) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Variable Star: M-Range:';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rVarMMax)
        + ' - ' + FloatToStr((mAObject as TStar).rVarMMin);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    if((mAObject as TStar).sVarType <> '') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Type';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TStar).sVarType;
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    if((mAObject as TStar).rVarPer_D > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Period [Days]';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rVarPer_D);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    if((mAObject as TStar).rMAttend > 0) then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Double/Multi-Star: M-Attendee';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TStar).rMAttend);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

      if((mAObject as TStar).iSepSec > 0) then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Sep.-Angle ['''']';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TStar).iSepSec);
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;

      if((mAObject as TStar).rVarPer_D > 0) then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '  Pos.-Angle [°]';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TStar).iSepAngle);
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;
    end;
  end;

  //CalcAOWidth(mAObject.sAOType,TB__AOV.Position);

  ShowStarStruc(miMStarIndex);

  TIMER__COO.Enabled:=true;

end;

procedure TF__AOVIS.ShowAOData_Sun();
begin

  PutRA_DEC(0);

  if(msLANG_ID = 'DE') then
  begin
    P__AOVIS_CAPTION.Caption := 'Sonne';

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung Perihel [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '147,099 Mio';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung Aphel [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '152,1 Mio';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Äquatordurchmesser [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '1.392.684';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Masse [kg]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '1,9884E30';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Alter [Jahren]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '4.57E9 ';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Scheinbare Helligkeit m';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr(mAObject.rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Absolute Helligkeit M';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '4.83';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Oberflächentemperatur';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '5777 K';

    GenStarSpecChart(5777.0,clYellow,0);

    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Spektraltyp';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := 'G2V';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  end
  else
  begin
    P__AOVIS_CAPTION.Caption := 'Sun';

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance Perihelion [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '147,099 Mio';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance Aphelion [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '152,1 Mio';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Equator Diameter [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '1.392.684';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Mass [kg]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '1,9884E30';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Age [Years]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '4.57E9 ';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Apparent Magnitude m';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr(mAObject.rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Absolute Magnitude M';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '4.83';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Surface Temperature';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '5777 K';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GenStarSpecChart(5777.0,clYellow,0);

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Spectral Type';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := 'G2V';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  end;

  //CalcAOWidth('SUN',TB__AOV.Position);

end;

procedure TF__AOVIS.ShowAOData_Earth();
begin
  if(msLANG_ID = 'DE') then
  begin
    P__AOVIS_CAPTION.Caption := 'Erde';
  end
  else
  begin
    P__AOVIS_CAPTION.Caption := 'Earth';
  end;
end;

procedure TF__AOVIS.ShowAOData_Moon();
begin
  PutRA_DEC(1);

  if(msLANG_ID = 'DE') then
  begin
    P__AOVIS_CAPTION.Caption := 'Mond';
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung Periapsis [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '363300';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung Apoapsis [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '405500';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Durchmesser [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '3476';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Masse [kg]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '7,349E22';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Umlaufzeit [Tagen]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '27,322';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Alter [Jahren]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '4.53E9';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Scheinbare Helligkeit';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr(mAObject.rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;


  end
  else
  begin
    P__AOVIS_CAPTION.Caption := 'Moon';
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance Periapsis [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '363300';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance Apoapsis [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '405500';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Diameter [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '3476';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Mass [kg]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '7,349E22';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sidereal Period  [Days]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '27,322';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Age [years]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := '4.53E9';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Apparent Magnitude';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr(mAObject.rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  end;
  CalcAOWidth('MOON',TB__AOV.Position);
end;

procedure TF__AOVIS.ShowAOData_P();
var
  sConDateP, sConDate, sConDateN: string;
  sOpDateP, sOpDate, sOpDateN: string;
begin
  PutRA_DEC(2);

  if(msLANG_ID = 'DE') then
  begin
    P__AOVIS_CAPTION.Caption := (mAObject as TPlanet).sName_DE;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Große Halbachse [AU]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rA,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Trop. Jahr';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rTp,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Elliptizität';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rE,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if((mAObject as TPlanet).sPlanetType <> 'E') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Scheinb. Helligkeit bei 1 AU';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rV0);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Durchmesserverhältnis';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rDiameterRatio);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Durchmesser [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr(Round((mAObject as TPlanet).rDiameterRatio * 12735));
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sonnendistanz [AU]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rDistFromSun_AU,ffFixed,8,3);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Erdentfernung [AU]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rDistFromEarth_AU,ffFixed,8,3);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Winkelgröße [asec]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rVisuAngle_arcsec,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if((mAObject as TPlanet).sPlanetType <> 'E') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Massenverhältnis';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rMassRatio);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Masse [kg]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rMassRatio * 5.974e24);

    // Print opposition and conjunction dates
    if(not (mAObject as TPlanet).bInnerPlanet) and ((mAObject as TPlanet).sPlanetType <> 'E') then
    begin
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '';

      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Datum (Schätzung)';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := 'Opposition | Konjunktion';
    end;

  end;

  if(msLANG_ID = 'EN') then
  begin
    P__AOVIS_CAPTION.Caption := (mAObject as TPlanet).sName_EN;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Semi-major axis [AU]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rA,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Trop. Year';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rTp,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Eccentricity';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rE,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if((mAObject as TPlanet).sPlanetType <> 'E') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Visual magnitude at 1 AU';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rV0);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Diameter Ratio';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rDiameterRatio);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Diameter [km]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr(Round((mAObject as TPlanet).rDiameterRatio * 12735));
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sun Distance [AU]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rDistFromSun_AU,ffFixed,8,3);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Earth Distance [AU]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rDistFromEarth_AU,ffFixed,8,3);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Angular size [asec]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStrF((mAObject as TPlanet).rVisuAngle_arcsec,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '';
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

    if((mAObject as TPlanet).sPlanetType <> 'E') then
    begin
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Mass Ratio';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rMassRatio);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Mass [kg]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPlanet).rMassRatio * 5.974e24);

    // Print opposition and conjunction dates
    if(not (mAObject as TPlanet).bInnerPlanet) and ((mAObject as TPlanet).sPlanetType <> 'E') then
    begin
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '';

      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Date (Estimated)';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := 'Opposition | Conjunction';
    end;
  end;

  if(not (mAObject as TPlanet).bInnerPlanet) and ((mAObject as TPlanet).sPlanetType <> 'E') then
  begin
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := IntToStr(YearOf(Now)-1);

    sConDateP := '-'; sConDate := '-'; sConDateN := '-';
    sOpDateP := '-'; sOpDate := '-'; sOpDateN := '-';

    if((mAObject as TPlanet).dtOPDateP > 0) then
      sOpDateP := DateToStr((mAObject as TPlanet).dtOPDateP);
    if((mAObject as TPlanet).dtConP > 0) then
      sConDateP := DateToStr((mAObject as TPlanet).dtConP);

    if((mAObject as TPlanet).dtOPDate > 0) then
      sOpDate := DateToStr((mAObject as TPlanet).dtOPDate);
    if((mAObject as TPlanet).dtCon > 0) then
      sConDate := DateToStr((mAObject as TPlanet).dtCon);

    if((mAObject as TPlanet).dtOPDateN > 0) then
      sOpDateN := DateToStr((mAObject as TPlanet).dtOPDateN);
    if((mAObject as TPlanet).dtConN > 0) then
      sConDateN := DateToStr((mAObject as TPlanet).dtConN);

    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sOPDateP + ' | ' + sConDateP;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := IntToStr(YearOf(Now));
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sOPDate + ' | ' + sConDate;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := IntToStr(YearOf(Now)+1);
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sOPDateN + ' | ' + sConDateN;
  end;

  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  // License annotations
  if(Uppercase((mAObject as TPlanet).sName_DE) = 'MERKUR') then
    PutImageLicenseData('Public Domain','','Messenger team, Jason Harwell','Mercury.tif','')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'VENUS') then
    PutImageLicenseData('Public Domain','','NASA or Ricardo Nunes - Image processing by R. Nunes','Venus-real color.jpg','http://www.astrosurf.com/nunes/explor/explor_m10.htm')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'ERDE') then
    PutImageLicenseData('Public Domain','','NASA/Apollo 17 crew; taken by either Harrison Schmitt or Ron Evans','The Earth seen from Apollo 17.jpg','https://www.nasa.gov/multimedia/imagegallery/image_feature_329.html')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'MARS') then
    PutImageLicenseData('Public Domain','','NASA/JPL/USGS','MARS-Viking.jpg','http://photojournal.jpl.nasa.gov/catalog/PIA00407')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(1) CERES') then
    PutImageLicenseData('Public Domain','','NASA/JPL-Caltech/UCLA/MPS/DLR/IDA','Ceres RC3 with bright spots.jpg','http://dawn.jpl.nasa.gov/multimedia/images/index.html?view=list_view&start=0')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(2) PALLAS') then
    PutImageLicenseData('Public Domain','','Hubble Space Telescope/STScI','PallasHST2007.jpg','http://www.lpi.usra.edu/meetings/lpsc2008/pdf/2502.pdf')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(4) VESTA') then
    PutImageLicenseData('Public Domain','','NASA/JPL-Caltech/UCLA/MPS/DLR/IDA','The Four Largest Asteroids.jpg','Full View of Vesta')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(5) ASTRAEA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','5Astraea (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=103')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(6) HEBE') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','ESO/M. Marsset','Not the mother of meteorites.jpg','https://www.eso.org/public/images/potw1725a/')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(7) IRIS') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','ESO/Vernazza et al.','Iris asteroid eso.jpg','https://www.skyandtelescope.com/observing/pop-in-on-pallas-and-iris-tonight')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(8) FLORA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','8Flora (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=106')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(9) METIS') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','9Metis (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=107')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(10) HYGIEA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','ESO/P. Vernazza et al./MISTRAL algorithm (ONERA/CNRS)','SPHERE image of Hygiea.jpg','https://www.eso.org/public/images/eso1918a/')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(12) VICTORIA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','12Victoria (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=517')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(13) EGERIA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','13Egeria (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=230')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(14) IRENE') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','14Irene (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=231')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(15) EUNOMIA') then
    PutImageLicenseData('Creative Common','','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','15Eunomia (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=108')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(16) PSYCHE') then
    PutImageLicenseData('Creative Common','','ESO/LAM','Psyche asteroid eso crop.jpg','https://www.eso.org/public/usa/blog/true-nature-of-asteroids/?lang')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(18) MELPOMENE') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','A601.M993.shape.png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=601')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(20) MASSALIA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','A112.M119.shape.png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=112')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(23) THALIA') then
    PutImageLicenseData('Creative Common','','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','23Thalia (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=115')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(24) THEMIS') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','A609.M1006.shape.png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=609')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(25) PHOCAEA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','25Phocaea (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=439')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(26) PROSEPINA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','A713.M1189.shape.png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=713')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(27) EUTERPE') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','27Euterpe (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=302')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(29) AMPHITRITE') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','ESO/Vernazza et al.','Potw1749a.tif','http://www.eso.org/public/images/potw1749a/')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(30) URANIA') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','30Urania (Lightcurve Inversion).png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=117')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = '(31) EUPHROSYNE') then
    PutImageLicenseData('Creative Common','https://creativecommons.org/licenses/by/4.0/deed.en','Astronomical Institute of the Charles University: Josef Ďurech, Vojtěch Sidorin','A643.M1067.shape.png','http://astro.troja.mff.cuni.cz/projects/asteroids3D/web.php?page=db_asteroid_detail&asteroid_id=643')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'JUPITER') then
    PutImageLicenseData('Public Domain','','NASA, ESA, and A. Simon (NASA Goddard), edited by PlanetUser','Jupiter, image taken by NASA''s Hubble Space Telescope, June 2019 - Edited.jpg','https://www.nasa.gov/feature/goddard/2019/hubble-new-portrait-of-jupiter')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'SATURN') then
    PutImageLicenseData('Public Domain','','NASA / JPL / Space Science Institute','Saturn during Equinox.jpg','http://www.nasa.gov/images/content/365640main_PIA11141_full.jpg')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'URANUS') then
    PutImageLicenseData('Public Domain','','NASA/JPL/Voyager mission (edited by Orange-kun)','Uranus (Edited).jpg','http://planetquest.jpl.nasa.gov/milestones_show/slide1.html')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'NEPTUN') then
    PutImageLicenseData('Public Domain','','NASA','Neptune Full.jpg','https://photojournal.jpl.nasa.gov/catalog/PIA01492')
  else if(Uppercase((mAObject as TPlanet).sName_DE) = 'PLUTO') then
    PutImageLicenseData('Public Domain','','NASA/JHUAPL/SwRI','Global LORRI mosaic of Pluto in true colour.jpg','http://www.nasa.gov/image-feature/global-mosaic-of-pluto-in-true-color');


  CalcAOWidth(mAObject.sAOType,TB__AOV.Position);

end;

procedure TF__AOVIS.PutImageLicenseData(sLicenseName, sLicenseURL, sAuthor, sTitle, sSourceURL: string);
begin
  if(sLicenseURL <> '') then
    sLicenseName := sLicenseName + ' (URL: ' + sLicenseURL + ')';

  if(msLANG_ID = 'DE') then
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Bildlizenz:'
  else
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Image License:';

  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sLicenseName;
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Autor:'
  else
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Author:';

  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sAuthor;
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  if(msLANG_ID = 'DE') then
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Titel:'
  else
    GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Title:';

  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sTitle;
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  if(sSourceURL <> '') then
  begin
    if(msLANG_ID = 'DE') then
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Bildquelle:'
    else
      GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Image Source:';

    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := sSourceURL;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;
end;

procedure TF__AOVIS.ShowAOData_C();
var
  rArgPer: Real;
  iYYYY, iMM, iMDD: Integer;
begin
  if(msLANG_ID = 'DE') then  P__AOVIS_CAPTION.Caption := (mAObject as TAObject).sName_DE;
  if(msLANG_ID = 'EN') then  P__AOVIS_CAPTION.Caption := (mAObject as TAObject).sName_EN;

  PutRA_DEC(3);

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung Erde (AE)';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance Earth (AU)';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TComet).rRho);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung Sonne (AE)';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance Sonne (AU)';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TComet).rRs);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Trop. Jahr';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Trop. Year';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TComet).rTp);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Gr. Halbachse (AE)';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Semimajor Axis (AU)';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TComet).rA);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Elliptizität';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Eccentricity';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TComet).rE);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Inklination';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Inclination';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TComet).rI);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  rArgPer := (mAObject as TComet).rOmegaQ - (mAObject as TComet).rOmega;
  if(rArgPer < 0) then rArgPer := rArgPer + 360;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Perihel-Argument';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Argument of the perihelion';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr(rArgPer);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Länge des aufsteigenden Knotens';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Longitude of the ascending node';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TComet).rOmega);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  iYYYY := Trunc((mAObject as TComet).rP);
  iMM := 1 + Trunc(((mAObject as TComet).rP - iYYYY)*12);
  iMDD := 1 + Round((((mAObject as TComet).rP - iYYYY)*12 - (iMM-1))*30);

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Perihel Transit';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Perihelion Transit';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr(iYYYY) + '/' + IntToStr(iMM) + '/' + IntToStr(iMDD);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

end;

procedure TF__AOVIS.ShowAOData_G();
begin
  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sternbild';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Constellation';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] :=
    GetSignName((mAObject as TInterstellarObject).sCon,molSignList,msLANG_ID);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if((mAObject as TGalaxy).sGType <> '') then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Typ';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Type';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TGalaxy).sGType;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TGalaxy).iRadSpeed <> -999) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Radialgeschw. [km/s]';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Radial Speed [km/s]';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TGalaxy).iRadSpeed);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

end;

procedure TF__AOVIS.ShowAOData_Q();
begin
  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'ID';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TInterstellarObject).sNGC;
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Laufzeitentfernung [MrdLj]';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Light-Travel Distance [MrdLy]';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TInterstellarObject).rDist_XLY);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Mitbewegte Entfernung [MPc]';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Comoving Distance [MPc]';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TQuasar).fComDistMPc);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if((mAObject as TQuasar).sQType <> '') then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Typ';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Type';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TQuasar).sQType;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TQuasar).fRadSpeed <> -999) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Rotverschiebung';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Redshift';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TQuasar).fRedshift);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

end;

procedure TF__AOVIS.ShowAOData_N();
begin
  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sternbild';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Constellation';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] :=
    GetSignName((mAObject as TInterstellarObject).sCon,molSignList,msLANG_ID);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if((mAObject as TNebula).sNType <> '') then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Typ';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Type';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TNebula).sNType;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TNebula).iVisDim1 > 0) and ((mAObject as TNebula).iVisDim2 > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Ausdehnung';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Extent';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TNebula).iVisDim1) + ' * ' + IntToStr((mAObject as TNebula).iVisDim2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TNebula).sStar <> '') then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Zentraler Stern';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Central Star';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TNebula).sStar;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TNebula).rStar_M > -999) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Stern-Magnitude';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Star Magnitude';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TNebula).rStar_M);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;
end;

procedure TF__AOVIS.ShowAOData_PN();
begin
  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sternbild';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Constellation';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] :=
    GetSignName((mAObject as TInterstellarObject).sCon,molSignList,msLANG_ID);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if((mAObject as TPNebula).sPNType <> '') then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Typ';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Type';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TPNebula).sPNType;
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TPNebula).rVisDim1 > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Ausdehnung (Bogensek.)';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Extent (arcsec)';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] :=
      FloatToStrF((mAObject as TPNebula).rVisDim1,ffFixed,8,2)
      + ' * ' + FloatToStrF((mAObject as TPNebula).rVisDim2,ffFixed,8,2);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TPNebula).rM > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Magnitude';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Magnitude';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloattoStr((mAObject as TPNebula).rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TPNebula).rStar_M > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Stern-Magnitude';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Star Magnitude';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TPNebula).rStar_M);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;
end;

procedure TF__AOVIS.ShowAOData_OC();
begin
  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sternbild';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Constellation';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] :=
    GetSignName((mAObject as TInterstellarObject).sCon,molSignList,msLANG_ID);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Typ';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Type';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TOpenCluster).sOCType;
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if((mAObject as TOpenCluster).iDiam_M > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Ausdehnung (Bogensek.)';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Extent (arcsec)';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TOpenCluster).iDiam_M);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TOpenCluster).rM > -999) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Magnitude';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Magnitude';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloattoStr((mAObject as TOpenCluster).rM);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TOpenCluster).iNum > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '#Sterne';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := '#Stars';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TOpenCluster).iNum);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TOpenCluster).rAge > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Alter (MJ)';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Age (MY)';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TOpenCluster).rAge);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;
end;

procedure TF__AOVIS.ShowAOData_GC();
begin
  if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Sternbild';
  if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Constellation';
  GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] :=
    GetSignName((mAObject as TInterstellarObject).sCon,molSignList,msLANG_ID);
  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;

  if((mAObject as TGlobularCluster).iGCType > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Typ';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Type';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := IntToStr((mAObject as TGlobularCluster).iGCType);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;

  if((mAObject as TGlobularCluster).rVisDiam > 0) then
  begin
    if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Ausdehnung (Bogensek.)';
    if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Extent (arcsec)';
    GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TGlobularCluster).rVisDiam);
    GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
  end;
end;

procedure TF__AOVIS.ShowAstroPic(bPicExists: Boolean; sPath: string);
begin
  if (bPicExists) and (sPath <> '') then
  begin
    IMG__REF.Visible:=false;
    SHP__REF.Visible:=false;
    P__SCALING.Visible:=false;
    IMG__AO.Picture.LoadFromFile(sPath);
    IMG__AOVIS.Picture.LoadFromFile(sPath);
    IMG__AO.Align:=alClient;
    msMissingFile := '';
    P__INFO_NO_PICTURE.Visible:=false;
  end
  else
  begin
    msMissingFile := sPath; // Remember for standard user file
    P__SCALING.Visible:=false;
    SHP__REF.Visible:=false;
    P__INFO_NO_PICTURE.Visible:=true;
  end;

end;

procedure TF__AOVIS.FormActivate(Sender: TObject);
var
  i,j, iXCenter, iYCenter: Integer;
  bPicExists: Boolean;
  sPath, sFileName: string;
  rAmp, rAMpDate: Real;
  Moon: TMoon;
  iVisMoonCnt, iVisMoonPos: Integer;
  sLabel, sSpecType: string;
begin
  bPicExists := false;

  IMG__CONVECTION.Visible:=false;
  IMG__RADIATION.Visible:=false;

  iXCenter := P__VISU.Width div 2;
  iYCenter := P__VISU.Height div 2;

  IMG__AOVIS.Left := iXCenter - (IMG__AOVIS.Width div 2);
  IMG__AOVIS.Top := iYCenter - (IMG__AOVIS.Height div 2);

  TB__AOV.Max := 500;
  TB__AOV.Position := 450;

  GRD__AOV_PROP.RowCount := 2;
  if(msLANG_ID = 'DE') then
  begin
    GRD__AOV_PROP.Cells[0,0] := 'Eigenschaft';
    GRD__AOV_PROP.Cells[1,0] := 'Wert';
  end
  else
  begin
    GRD__AOV_PROP.Cells[0,0] := 'Property';
    GRD__AOV_PROP.Cells[1,0] := 'Value';
  end;

  TS__HRD.TabVisible := false;

  miMStarCnt := 0;
  miMStarIndex := 0;

  if(LeftStr((mAObject as TAObject).sAOType,1) = 'S') then
  begin
    TGB__STARSTRUCT.Visible:= true;
    CHART__SPEC.Visible := true;
    TS__HRD.TabVisible := true;
    GRD__HRD_MK.Visible:=true;

    GRD__HRD_MK.ColWidths[0] := 30;
    GRD__HRD_MK.ColWidths[1] := 150;

    GRD__HRD_MK.Cells[0,0] := '0';
    GRD__HRD_MK.Cells[0,1] := 'Ia';
    GRD__HRD_MK.Cells[0,2] := 'Ib';
    GRD__HRD_MK.Cells[0,3] := 'II';
    GRD__HRD_MK.Cells[0,4] := 'III';
    GRD__HRD_MK.Cells[0,5] := 'IV';
    GRD__HRD_MK.Cells[0,6] := 'V';
    GRD__HRD_MK.Cells[0,7] := 'VI';
    GRD__HRD_MK.Cells[0,8] := 'VII';
    GRD__HRD_MK.Cells[0,9] := 'RD';
    GRD__HRD_MK.Cells[0,10] := 'BD';

    if(msLANG_ID = 'DE') then
    begin
      B__ADD_USR_PIC.Caption:='Eigenes JPG in Bibliothek kopieren';

      GRD__HRD_MK.Cells[1,0] := 'Hyperriesen';
      GRD__HRD_MK.Cells[1,1] := 'Überriesen Ia';
      GRD__HRD_MK.Cells[1,2] := 'Überriesen Ib';
      GRD__HRD_MK.Cells[1,3] := 'Helle Riesen';
      GRD__HRD_MK.Cells[1,4] := 'Riesen';
      GRD__HRD_MK.Cells[1,5] := 'Unterriesen';
      GRD__HRD_MK.Cells[1,6] := 'Zwerge';
      GRD__HRD_MK.Cells[1,7] := 'Unterzwerge';
      GRD__HRD_MK.Cells[1,8] := 'Weiße Zwerge';
      GRD__HRD_MK.Cells[1,9] := 'Rote Zwerge';
      GRD__HRD_MK.Cells[1,10] := 'Braune Zwerge';
    end
    else
    begin
      B__ADD_USR_PIC.Caption:='Copy your picture to the library';

      GRD__HRD_MK.Cells[1,0] := 'Hypergiants Ia';
      GRD__HRD_MK.Cells[1,1] := 'Supergiants Ia';
      GRD__HRD_MK.Cells[1,2] := 'Supergiants Ib';
      GRD__HRD_MK.Cells[1,3] := 'Bright Giants';
      GRD__HRD_MK.Cells[1,4] := 'Giants';
      GRD__HRD_MK.Cells[1,5] := 'Subgiants';
      GRD__HRD_MK.Cells[1,6] := 'Dwarfs';
      GRD__HRD_MK.Cells[1,7] := 'Subdwarfs';
      GRD__HRD_MK.Cells[1,8] := 'White Dwarfs';
      GRD__HRD_MK.Cells[1,9] := 'Red Dwarfs';
      GRD__HRD_MK.Cells[1,10] := 'Brown Dwarfs';
    end;
  end;

  if((mAObject as TAObject).sAOType = 'S') then
  begin
    sSpecType := (mAObject as TStar).sSpType;
    sSpecType := AnsiReplaceStr(sSpecType,'-','+');
    miMStarCnt := CountChar(sSpecType,'+');
  end;

  miMStarIndex := 0;

  // Show Multistar selection box
  if(miMStarCnt > 0) then
  begin
    CB__MSTAR_SEL.Items.Clear;

    for j:=0 to miMStarCnt do
    begin
      if(msLANG_ID = 'DE') then
        CB__MSTAR_SEL.Items.Add(IntToStr(j+1) + '. Komponente')
      else
        CB__MSTAR_SEL.Items.Add(IntToStr(j+1) + '. Component');

      CB__MSTAR_SEL.ItemIndex:=0;
    end;

    CB__MSTAR_SEL.Visible:=true;

  end
  else
    CB__MSTAR_SEL.Visible:=false;

  if((mAObject as TAObject).sAOType = 'S') or
    ((mAObject as TAObject).sAOType = 'P') or
    ((mAObject as TAObject).sAOType = 'SUN') or
    ((mAObject as TAObject).sAOType = 'MOON') then
  begin
    TS__SIZECOMP.TabVisible:=true;

    P__AOV_VISU.Align:=alClient;
    P__AOV_VISU.Visible := true;

    IMG__REF.Left := (P__MAIN_SIZECOMP.Width * 2) div 3;

    if((mAObject as TAObject).sAOType = 'SUN') then
    begin
      TGB__STARSTRUCT.Visible:=true;
      IMG__AO.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\GStar.jpg'));
      IMG__AOVIS.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\GStar.jpg'));
    end;

  end
  else
  begin
    TS__SIZECOMP.TabVisible:=false;

    sPath := '';

    if(
      (((mAObject as TAObject).sAOType = 'G') or
      ((mAObject as TAObject).sAOType = 'OC') or
      ((mAObject as TAObject).sAOType = 'GC') or
      ((mAObject as TAObject).sAOType = 'PN') or
      ((mAObject as TAObject).sAOType = 'N'))) then
    begin
      sPath := msAlbireoLocalDir + 'img\InterstellarLib\' + (mAObject as TInterstellarObject).sNGC + '.jpeg';
    end;

    if((mAObject as TAObject).sAOType = 'Q') then
    begin
      sFileName := (mAObject as TQuasar).sQID;
      sFileName := ReplaceStr(sFileName,'+','_');
      sFileName := ReplaceStr(sFileName,'-','_');
      sFileName := ReplaceStr(sFileName,' ','_');
      sFileName := ReplaceStr(sFileName,'/','_');
      sFileName := ReplaceStr(sFileName,'~','_');

      sPath := ConvertWinPath(msAlbireoLocalDir + 'img\InterstellarLib\' + sFileName + '.jpeg');
    end;

    if(sPath <> '') then
    begin
      if(FileExists(sPath)) then
        bPicExists := true;
    end;

    ShowAstroPic(bPicExists,sPath);

  end;

  // For DeepSky print Messier/NGC-No.
  if not (
    (mAObject.sAOType = 'P') or (mAObject.sAOType = 'C') or (mAObject.sAOType = 'S') or
    (mAObject.sAOType = 'SUN') or (mAObject.sAOType = 'MOON') or (mAObject.sAOType = 'EARTH')
    ) then
  begin
    P__AOVIS_CAPTION.Caption := '';

    if(Trim((mAObject as TInterstellarObject).sMessier) <> '') then
    begin
      P__AOVIS_CAPTION.Caption := AnsiReplaceStr((mAObject as TInterstellarObject).sMessier,'M','M ');

      if(msLANG_ID = 'DE') then
      begin
        if((mAObject as TInterstellarObject).sName_DE <> '') then
          P__AOVIS_CAPTION.Caption :=P__AOVIS_CAPTION.Caption + ' - ' + (mAObject as TInterstellarObject).sName_DE;
      end
      else
      begin
        if((mAObject as TInterstellarObject).sName_EN <> '') then
          P__AOVIS_CAPTION.Caption :=P__AOVIS_CAPTION.Caption + ' - ' + (mAObject as TInterstellarObject).sName_EN;
      end;

      if (P__AOVIS_CAPTION.Caption <> '') then
        P__AOVIS_CAPTION.Caption := Trim(P__AOVIS_CAPTION.Caption +  ' - ' +
          AnsiReplaceStr((mAObject as TInterstellarObject).sNGC,'NGC','NGC '))
      else
        P__AOVIS_CAPTION.Caption := AnsiReplaceStr((mAObject as TInterstellarObject).sNGC,'NGC','NGC ');

    end
    else
    begin
      if(msLANG_ID = 'DE') then
      begin
        if((mAObject as TInterstellarObject).sName_DE <> '') then
          P__AOVIS_CAPTION.Caption := (mAObject as TInterstellarObject).sName_DE;
      end
      else
      begin
        if((mAObject as TInterstellarObject).sName_EN <> '') then
          P__AOVIS_CAPTION.Caption := (mAObject as TInterstellarObject).sName_EN;
      end;

      if (P__AOVIS_CAPTION.Caption <> '') then
        P__AOVIS_CAPTION.Caption := Trim(P__AOVIS_CAPTION.Caption + ' - ' +
          AnsiReplaceStr((mAObject as TInterstellarObject).sNGC,'NGC','NGC '))
      else
        P__AOVIS_CAPTION.Caption := AnsiReplaceStr((mAObject as TInterstellarObject).sNGC,'NGC','NGC ');

    end;

    PutRA_DEC(10);

    if(mAObject.sAOType <> 'Q') then
    begin

      if((mAObject as TInterstellarObject).sMessier <> '') then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Messier';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := (mAObject as TInterstellarObject).sMessier;
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;

      if((mAObject as TInterstellarObject).sNGC <> '') then
      begin
        GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'NGC';
        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := AnsiReplaceStr((mAObject as TInterstellarObject).sNGC,'NGC','');
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;

      if((mAObject as TInterstellarObject).rDist_XLY > 0) then
      begin
        if(mAObject.sAOType = 'G') then
        begin
          if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung [MLj]';
          if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance [MLy]';
        end
        else
        begin
          if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Entfernung [Lj]';
          if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Distance [Ly]';
        end;

        GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TInterstellarObject).rDist_XLY);
        GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
      end;

    end;

    if((mAObject as TInterstellarObject).rM > -999) then
    begin
      if(msLANG_ID = 'DE') then  GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'Scheinb. Helligkeit';
      if(msLANG_ID = 'EN') then GRD__AOV_PROP.Cells[0,GRD__AOV_PROP.RowCount-1] := 'App. Magnitude';
      GRD__AOV_PROP.Cells[1,GRD__AOV_PROP.RowCount-1] := FloatToStr((mAObject as TInterstellarObject).rM);
      GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount+1;
    end;

  end;

  if(mAObject.sAOType = 'G') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Galaxie - Details'
    else
      Caption:='Galaxy - Details';

    ShowAOData_G();
  end;

  if(mAObject.sAOType = 'Q') then
  begin
    Caption:='Quasar - Details';

    ShowAOData_Q();
  end;

  if(mAObject.sAOType = 'GC') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Kugelsternhaufen - Details'
    else
      Caption:='Globular Cluster - Details';

    ShowAOData_GC();
  end;

  if(mAObject.sAOType = 'OC') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Offener Sternhaufen - Details'
    else
      Caption:='Open Cluster - Details';

    ShowAOData_OC();
  end;

  if(mAObject.sAOType = 'N') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Galaktischer Nebel - Details'
    else
      Caption:='Nebula - Details';

    ShowAOData_N();
  end;

  if(mAObject.sAOType = 'PN') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Planetarischer Nebel - Details'
    else
      Caption:='Planetary Nebula - Details';

    ShowAOData_PN();
  end;

  if(mAObject.sAOType = 'P') then
  begin
    if((mAObject as TPlanet).sPlanetType = 'P') then
      Caption:='Planet - Details';

    if((mAObject as TPlanet).sPlanetType = 'A') then
      Caption:='Asteroid - Details';

    if((mAObject as TPlanet).sPlanetType = 'p') then
    begin
      if(msLANG_ID = 'DE') then
        Caption:='Zwergplanet - Details'
      else
        Caption:='Dwarf Planet - Details';
    end;

    ShowAOData_P();

    // Show Moon data, if available
    molMoonList := TObjectList.Create;

    ImportCatalog_M(molMoonList,(mAObject as TPlanet).iPlanetIndex,msLANG_ID,msAlbireoLocalDir + 'AO-M.dat');

    iVisMoonCnt := 0;
    SHP__MOON1.Visible := false; SHP__MOON2.Visible := false; SHP__MOON3.Visible := false; SHP__MOON4.Visible := false; SHP__MOON5.Visible := false;
    L__MOON1.Visible := false; L__MOON2.Visible := false; L__MOON3.Visible := false; L__MOON4.Visible := false; L__MOON5.Visible := false;
    P__PLANET_MOONS.Visible:=false;

    if(molMoonList.Count > 0) then
    begin
      TS__MOONS.TabVisible := true;

      for i:=0 to molMoonList.Count-1 do
      begin
        Moon := (molMoonList[i] as TMoon);

        if(msLANG_ID = 'DE') then
          CB__MOONS.Items.AddObject(Moon.sName_DE,molMoonList[i])
        else
          CB__MOONS.Items.AddObject(Moon.sName_EN,molMoonList[i]);

        SHP__PLANET.Hint := P__AOVIS_CAPTION.Caption;
        L__PLANET.Caption:=P__AOVIS_CAPTION.Caption;

        // Display moon's east-west position constellation
        if(Moon.dtZeroPass > 0) and (Moon.rOrbitalPeriod_d > 0) and ((mAObject as TPlanet).rDiameterRatio > 0) then
        begin
          Inc(iVisMoonCnt);
          P__PLANET_MOONS.Visible:=true;

          rAmp := Moon.rSMAxis_km / ((mAObject as TPlanet).rDiameterRatio*12735/2.0);
          rAmp := SHP__PLANET.Width * rAmp / 2.0; // * (P__PLANET_MOONS.Width div 2);
          rAmpDate := -rAmp*sin(2*Pi/Moon.rOrbitalPeriod_d* (mdtWT - Moon.dtZeroPass));
          iVisMoonPos := SHP__PLANET.Left + (Int64(SHP__PLANET.Width) div 2) + Round(rAmpDate);

          if(msLANG_ID = 'DE') then
            sLabel := Moon.sName_DE
          else
            sLabel := Moon.sName_EN;

          case iVisMoonCnt of
            1:
            begin
              SHP__MOON1.Left:= iVisMoonPos;
              SHP__MOON1.Hint:=sLabel;
              SHP__MOON1.Visible:=true;
              L__MOON1.Left := SHP__MOON1.Left;
              L__MOON1.Caption:=sLabel;
              L__MOON1.Visible:=true;
            end;
            2:
            begin
              SHP__MOON2.Left:= iVisMoonPos;
              SHP__MOON2.Hint:=sLabel;
              SHP__MOON2.Visible:=true;
              L__MOON2.Left := SHP__MOON2.Left;
              L__MOON2.Caption:=sLabel;
              L__MOON2.Visible:=true;
            end;
            3:
            begin
              SHP__MOON3.Left:= iVisMoonPos;
              SHP__MOON3.Hint:=sLabel;
              SHP__MOON3.Visible:=true;
              L__MOON3.Left := SHP__MOON3.Left;
              L__MOON3.Caption:=sLabel;
              L__MOON3.Visible:=true;
            end;
            4:
            begin
              SHP__MOON4.Left:= iVisMoonPos;
              SHP__MOON4.Hint:=sLabel;
              SHP__MOON4.Visible:=true;
              L__MOON4.Left := SHP__MOON4.Left;
              L__MOON4.Caption:=sLabel;
              L__MOON4.Visible:=true;
            end;
            5:
            begin
              SHP__MOON5.Left:= iVisMoonPos;
              SHP__MOON5.Hint:=sLabel;
              SHP__MOON5.Visible:=true;
              L__MOON5.Left := SHP__MOON5.Left;
              L__MOON5.Caption:=sLabel;
              L__MOON5.Visible:=true;
            end;
          end; // case
        end;
      end;

      if(CB__MOONS.Items.Count > 0) then
      begin
        CB__MOONS.ItemIndex:=0;
        ShowMoon(0);
      end;
    end;

  end;

  if(mAObject.sAOType = 'C') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Komet - Details'
    else
      Caption:='Comet - Details';

    ShowAOData_C();
  end;

  if(mAObject.sAOType = 'S') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Stern - Details'
    else
      Caption:='Star - Details';

    ShowAOData_S();
  end;

  if(mAObject.sAOType = 'SUN') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Sonne - Details'
    else
      Caption:='Sun - Details';

    ShowAOData_Sun();
  end;

  if((mAObject as TAObject).sAOType = 'P') and ((mAObject as TPlanet).sPlanetType = 'E') then
  begin
    if(msLANG_ID = 'DE') then
    begin
      L__MOONINFO.Caption:='Klicken Sie auf das Bild um Bezeichnungen anzuzeigen >';
    end
    else
    begin
      L__MOONINFO.Caption:='Click on the image to get descriptions displayed >';
    end;

    L__MOONINFO.Visible := true;
  end;

  if(mAObject.sAOType = 'MOON') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Mond - Details'
    else
      Caption:='Moon - Details';

    if(msLANG_ID = 'DE') then
    begin
      L__OBJINFO.Caption:='< Klicken Sie auf das Bild um Bezeichnungen anzuzeigen';
    end
    else
    begin
      L__OBJINFO.Caption:='< Click on the image to get descriptions displayed';
    end;

    L__OBJINFO.Visible := true;

    ShowAOData_Moon();
  end;

  if(mAObject.sAOType = 'EARTH') then
  begin
    if(msLANG_ID = 'DE') then
      Caption:='Erde - Details'
    else
      Caption:='Earth - Details';

    ShowAOData_Earth();
  end;

  GRD__AOV_PROP.RowCount := GRD__AOV_PROP.RowCount-1;

  if(IMG__AO.Hint = '') then
    IMG__AO.Hint := P__AOVIS_CAPTION.Caption;

  CalcAOWidth(mAObject.sAOType,TB__AOV.Max - TB__AOV.Position);

end;

procedure TF__AOVIS.FormCreate(Sender: TObject);
begin
  mslUserFiles := TStringList.Create;
  if(FileExists(GetUserDir() + csPictureRes)) then
    mslUserFiles.LoadFromFile(GetUserDir() + csPictureRes);

  PC__MAIN_VISU.ActivePageIndex:=0;

  miAltRow := -1;
  miLatRow := -1;
  TIMER__COO.Enabled := false;

  msMissingFile := '';
end;

procedure TF__AOVIS.FormDestroy(Sender: TObject);
begin
  if(mslUserFiles.Count > 0) then
    mslUserFiles.SaveToFile(GetUserDir() + csPictureRes);

  mslUserFiles.Destroy;
end;

procedure TF__AOVIS.FormKeyPress(Sender: TObject; var Key: char);
begin
    if (Key = #27) then Close;
end;

procedure TF__AOVIS.FormResize(Sender: TObject);
begin
  if(LeftStr((mAObject as TAObject).sAOType,1) = 'S') and (PC__MAIN_VISU.ActivePageIndex = 0) then
    ShowStarStruc(miMStarIndex);

end;

procedure TF__AOVIS.CB__MOONSChange(Sender: TObject);
begin
  ShowMoon(CB__MOONS.ItemIndex);
end;

procedure TF__AOVIS.CB__MSTAR_SELChange(Sender: TObject);
begin
  miMStarIndex := CB__MSTAR_SEL.ItemIndex;
  VisuAOData();
end;

procedure TF__AOVIS.ED__COMMENTChange(Sender: TObject);
begin
  maObject.sComment:=ED__COMMENT.Text;
end;

procedure TF__AOVIS.B__PHOTO_ADDClick(Sender: TObject);
{2012/12/27 / fs
Modifying the userfiles resource
}
var
  iIndex: Integer;
  sID, sFileRes: string;
begin
  if(ODLG.Execute) then
  begin
    msUserPicFileName := ODLG.FileName;

    if(Trim(msUserPicFileName) = '') or (not FileExists(msUserPicFileName)) then
      exit;

    sID := GetUserFileID();

    sFileRes := sID + '~' + AnsiReplaceStr(msUserPicFileName,' ','#');

    iIndex := GetUserFileIndex(sID);

    if(iIndex >= 0) then
      mslUserFiles.Delete(iIndex);

    mslUserFiles.Add(sFileRes);

    ShowAOPicture(false,PC__OBSERVATION.ActivePageIndex,msUserPicFileName);
  end;
end;

procedure TF__AOVIS.B__MYPHOTOSClick(Sender: TObject);
begin
end;

procedure TF__AOVIS.B__ADD_USR_PICClick(Sender: TObject);
// Copy user's file to to interstellar lib
begin
  if(msMissingFile <> '') and (ODLG.Execute) and (ODLG.FileName <> '') and (FileExists(ODLG.FileName)) then
  begin
    if (not FileExists(msMissingFile)) then
    begin
      if(CopyFile(ODLG.FileName,msMissingFile)) then
        ShowAstroPic(true,msMissingFile);
    end;

  end;
end;

procedure TF__AOVIS.B__PHOTO_DELClick(Sender: TObject);
var
  iIndex: Integer;
  sID: string;
begin
  if(msLANG_ID = 'DE') then
    if(MessageDlg('Bildverweis wirklich entfernen?',mtConfirmation,[mbYes,mbNo],0) = mrNo) then
      exit;

  if(msLANG_ID = 'EN') then
    if(MessageDlg('Remove picture link?',mtConfirmation,[mbYes,mbNo],0) = mrNo) then
    exit;

  sID := GetUserFileID();

  iIndex := GetUserFileIndex(sID);

  if(iIndex >= 0) then
  begin
    mslUserFiles.Delete(iIndex);
    ShowAOPicture(true,PC__OBSERVATION.ActivePageIndex,'');
  end;

end;

procedure TF__AOVIS.CBX__RELClick(Sender: TObject);
begin
  if(CBX__REL.Checked) then
    TB__AOV.Position := 490;

  CalcAOWidth(mAObject.sAOType,TB__AOV.Max - TB__AOV.Position);
end;

procedure TF__AOVIS.FormShow(Sender: TObject);
var
  sID: string;
  iIndex: Integer;
  slDummyList: TStringList;
  sFileName: string;
  i,j: Integer;
begin
  IniText(F__AOVIS,msLANG_ID);

  if(msLANG_ID = 'DE') then
  begin
    Caption := 'Objekt Details';
  end;

  if(msLANG_ID = 'EN') then
  begin
    Caption := 'Object Details';
  end;

  for j:=0 to PC__OBSERVATION.PageCount-1 do
    ShowAOPicture(true,j,'');

  // Find user's picture
  sID := GetUserFileID();
  i:=0;
  while (i < PC__OBSERVATION.PageCount) do
  begin
    if(i > 0) then
      sID := AnsiReplaceStr(sID,'~' + IntToStr(i) + '|','~' + IntToStr(i+1) + '|');

    if(sID <> '') then
    begin
      iIndex := GetUserFileIndex(sID);
      if(iIndex >= 0) then
      begin
        slDummyList := TStringList.Create;
        slDummyList.Delimiter := '~';
        slDummyList.DelimitedText := mslUserFiles[iIndex];
        sFileName := AnsiReplaceStr(slDummyList[2],'#',' ');
        slDummyList.Destroy;
        if(sFileName <> '') and (FileExists(sFileName)) then
          ShowAOPicture(false,i,sFileName);

      end;
    end;
    Inc(i);
  end;

end;

procedure TF__AOVIS.FormWindowStateChange(Sender: TObject);
begin
  //ShowMessage('AOTpye: ' + (mAObject as TAObject).sAOType + ', ActivePageIndex: ' + IntToStr(PC__MAIN_VISU.ActivePageIndex) );
  if(LeftStr((mAObject as TAObject).sAOType,1) = 'S') and (PC__MAIN_VISU.ActivePageIndex = 0) then
    ShowStarStruc(miMStarIndex);

end;

procedure TF__AOVIS.GBX__AOMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.GRD__STARSTRUCTDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  sText, sSPType: string;
begin
  sSpType := Uppercase((mAObject as TStar).sSpType);

  with Sender as TStringGrid do
  begin
    sText := Cells[ACol, ARow];

    Canvas.Font.Color := clSilver;
    Canvas.Brush.Color := clBlack;

    if(aCol = 0) then
    begin
      Canvas.Font.Color := clBlack;

      if(
        AnsiContainsStr(sSPType,'VI') and (not AnsiContainsStr(sSPType,'VII')) or AnsiContainsStr(sSPType,'SD')
        ) then
      begin

        if(AnsiContainsStr(sSPType,'O') or AnsiContainsStr(sSPType,'B') or AnsiContainsStr(sSPType,'A') or AnsiContainsStr(sSPType,'F')) then
        begin
          // Heisse Subzwerge
          case aRow of
            0: begin Canvas.Brush.Color := GetColorFromSpecType(sSPType); end;
            1: begin Canvas.Brush.Color := SHP__CORE3.Brush.Color; end;
            2: begin Canvas.Brush.Color := SHP__CORE2.Brush.Color; end;
            3: begin Canvas.Brush.Color := SHP__CORE.Brush.Color; end;
          end;
        end
        else
        begin
          // Kühle Subzwerge
          case aRow of
            0: begin Canvas.Brush.Color := GetColorFromSpecType(sSPType); end;
            1: begin Canvas.Brush.Color := $0094E0EB; end;  // $0094E0EB  SHP__CONVECTION.Brush.Color
            2: begin Canvas.Brush.Color := $00C2F0F3; end; // SHP__RADIATION.Brush.Color; end;
            3: begin Canvas.Brush.Color := SHP__CORE3.Brush.Color; end;
          end;
        end;

      end // [1] if AnsiContainsStr(sSPType,'VI') and..
      else if(AnsiContainsStr(sSPType,'VII') or (sSPType[1] = 'D')) then
      begin

        // Weiße Zwerge
        case aRow of
          0: begin Canvas.Brush.Color := GetColorFromSpecType(sSPType); end;
          1: begin Canvas.Brush.Color := SHP__CORE2.Brush.Color; end; // He
          2: begin Canvas.Brush.Color := SHP__CORE.Brush.Color; end; // C+O
        end;

      end
      else // [2] if AnsiContainsStr(sSPType,'VI') and..
      begin

        // Zwerge, Riesen und Überriesen
        case aRow of
          0: begin Canvas.Brush.Color := GetColorFromSpecType(sSPType); end;
          1: begin Canvas.Brush.Color := $0094E0EB ; end;//SHP__CONVECTION.Brush.Color; end;   // $0094E0EB
          2: begin Canvas.Brush.Color := $00C2F0F3 end; //SHP__RADIATION.Brush.Color; end;
          3: begin Canvas.Brush.Color := SHP__CORE3.Brush.Color; end;
          4:
          begin
            if(AnsiContainsStr(sSPType,'IV')) then
              Canvas.Brush.Color := SHP__CORE_HE.Brush.Color
            else if(AnsiContainsStr(sSPType,'I')) then
              Canvas.Brush.Color := SHP__CORE2.Brush.Color
            else
              Canvas.Brush.Color := clSilver; // $004860E6;

          end;
          5:
          begin
            Canvas.Brush.Color := SHP__CORE.Brush.Color
          end; // 5
          6:
          begin
            if(AnsiContainsStr(sSPType,'I')) then
              Canvas.Brush.Color := clBlack; //SHP__CORE_FE.Brush.Color;
          end;
        end; // case..

      end; // [3] if AnsiContainsStr(sSPType,'VI') and..

    end; // if(aCol..

    Canvas.FillRect(aRect);
    Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, sText);

  end; // with...
end;

procedure TF__AOVIS.IMG__AOClick(Sender: TObject);
begin
  PC__MAIN_VISU.ActivePageIndex:=0;
end;

procedure TF__AOVIS.IMG__AOMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.IMG__AOVISClick(Sender: TObject);
begin
  if((mAObject as TAObject).sAOType = 'MOON') then
  begin
    if(IMG__AOVIS.Tag = 0)
    then
    begin
      IMG__AOVIS.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\MoonExp.png'));
      IMG__AOVIS.Tag := 1;
    end
    else
    begin
      IMG__AOVIS.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\Moon.png'));
      IMG__AOVIS.Tag := 0;
    end;
  end
  else
  begin
    TGB__STARSTRUCT.Checked:=true;
    ShowStarStruc(miMStarIndex);
  end;
end;

procedure TF__AOVIS.IMG__AO_USER_1Click(Sender: TObject);
begin
  ShowPicture();
end;

procedure TF__AOVIS.IMG__AO_USER_2Click(Sender: TObject);
begin
  ShowPicture();
end;

procedure TF__AOVIS.IMG__AO_USER_3Click(Sender: TObject);
begin
  ShowPicture();
end;

procedure TF__AOVIS.IMG__MOONClick(Sender: TObject);
begin
  if((mAObject as TAObject).sAOType = 'P') and ((mAObject as TPlanet).sPlanetType = 'E') then
  begin
    if(IMG__MOON.Tag = 0)
    then
    begin
      IMG__MOON.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\MoonExp.png'));
      IMG__MOON.Tag := 1;
    end
    else
    begin
      IMG__MOON.Picture.LoadFromFile(ConvertWinPath(msAlbireoLocalDir + 'img\Moon.png'));
      IMG__MOON.Tag := 0;
    end;
  end
end;

procedure TF__AOVIS.IMG__REFMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.L__AOMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.L__REFMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.PC__MAIN_VISUChange(Sender: TObject);
begin
  VisuAOData();
end;

procedure TF__AOVIS.P__MAIN_SIZECOMPMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.SHP__AOMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.SHP__AOVISMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TF__AOVIS.SHP__REFMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ZoomObjects(WheelDelta);
end;

procedure TF__AOVIS.TB__AOVMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CalcAOWidth(mAObject.sAOType,TB__AOV.Max - TB__AOV.Position);
end;

procedure TF__AOVIS.TGB__STARSTRUCTClick(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
  begin
    if(TGB__STARSTRUCT.State = cbUnchecked) then
      TGB__STARSTRUCT.Caption := 'Zeige Aufbau'
    else
      TGB__STARSTRUCT.Caption := 'Zeige Oberfläche';
  end
  else
  begin
    if(TGB__STARSTRUCT.State = cbUnchecked) then
      TGB__STARSTRUCT.Caption := 'Show interior'
    else
      TGB__STARSTRUCT.Caption := 'Show surface';
  end;

  ShowStarStruc(miMStarIndex);
end;

procedure TF__AOVIS.TIMER__COOTimer(Sender: TObject);
var
  rAz, rHgt: Real;
begin
  if(miAltRow < 0) or (miLatRow < 0) or (mdtWT <= 0) or (mAObject = nil) then
  begin
    TIMER__COO.Enabled := false;
    exit;
  end;

  mdtWT := mdtWT + 1/(24*3600);

  rAz:=0; rHgt:=0;

  EquToAZCoo(mdtWT,
    miDST_HH,miUTC_HH,
    miGLng_DEG,miGLng_MIN,
    mrSin_fGLat, mrCos_fGLat,
    mAObject.iDec_Deg, mAObject.iDec_Deg, Round(mAObject.rDec_Sec),
    mAObject.iRA_Hours, mAObject.iRA_Min,mAObject.rRA_Sec,
    rAz, rHgt
    );

  rAz := rAz - 180;
  if(rAz < 0) then
    rAz := rAz + 360;

  GRD__AOV_PROP.Cells[1,miAltRow] := FloatToStrF(rHgt,ffFixed,1,2) + '°';
  GRD__AOV_PROP.Cells[1,miLatRow] := FloatToStrF(rAz,ffFixed,1,2) + '°';

end;

end.

