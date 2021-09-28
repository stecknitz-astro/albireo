unit u_astrocalc;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, EditBtn, ExtCtrls,
  DateUtils, StrUtils, Math,
  U_AConst, U_Alib, U_Translation, U_NewCam, U_AlbireoLib;

type

  { TF__ASTROCALC }

  TF__ASTROCALC = class(TForm)
    BV_BG: TBevel;
    BV__ASTAR: TBevel;
    BV__DC: TBevel;
    B__ANGLE_CALC: TButton;
    B__CAMERAINFO: TButton;
    B__DT_TO_DEC: TButton;
    B__LAE: TButton;
    B__NEWCAM: TButton;
    B__PXC_CALC: TButton;
    CBX__CAMERA_STD: TCheckBox;
    CBX__DBEL: TCheckBox;
    CBX__MANUAL_SENSORSEL: TCheckBox;
    CB__CONV_DATE: TDateEdit;
    CB__CONV_TIME: TTimeEdit;
    CB__PXC_DIST_UNIT: TComboBox;
    CB__PXC_MANUF: TComboBox;
    CB__PXC_MODEL: TComboBox;
    CB__PXC_OBJ_DETAIL_UNIT: TComboBox;
    CB__PXC_SFORMAT: TComboBox;
    ED__AM_MPC: TEdit;
    ED__APERTURE: TSpinEdit;
    ED__DEG2: TEdit;
    ED__AM_AU: TEdit;
    ED__AM_KM: TEdit;
    ED__AM_LHH: TEdit;
    ED__AM_LMM: TEdit;
    ED__AM_LSS: TEdit;
    ED__AM_LY: TEdit;
    ED__AM_PC: TEdit;
    ED__ANGLE_DIST: TEdit;
    ED__ANGLE_SIZE: TEdit;
    ED__ANG_DEG: TSpinEdit;
    ED__ANG_HH: TSpinEdit;
    ED__ANG_MM: TSpinEdit;
    ED__ANG_MM1: TSpinEdit;
    ED__ANG_MS: TSpinEdit;
    ED__ANG_MS1: TSpinEdit;
    ED__ANG_SS: TSpinEdit;
    ED__ANG_SS1: TSpinEdit;
    ED__AOP: TEdit;
    ED__DEG: TEdit;
    ED__DEG1: TEdit;
    ED__DT_TO_DEC: TEdit;
    ED__EXP_CNT: TSpinEdit;
    ED__EXP_S: TSpinEdit;
    ED__FOCALLENGTH: TSpinEdit;
    ED__FRATIO: TFloatSpinEdit;
    ED__HH_DEC: TEdit;
    ED__LAE: TEdit;
    ED__LOAN: TEdit;
    ED__PXC_DIST: TFloatSpinEdit;
    ED__PXC_MP: TFloatSpinEdit;
    ED__PXC_OBJ_DETAIL: TFloatSpinEdit;
    ED__PXC_PX_SIZE: TFloatSpinEdit;
    GBX__EXP_SN_CALC: TGroupBox;
    GBX__PXC_SENSOR: TGroupBox;
    GBX__RESULT: TGroupBox;
    GBX__OPT_FL: TGroupBox;
    GBX__SIGNAL: TGroupBox;
    IMG__ANGLE_EN: TImage;
    IMG__ANGLE_DE: TImage;
    IMG__AM_DIST: TImage;
    IMG__AM_DIST1: TImage;
    L__APERTURE: TLabel;
    L__DSIMG: TLabel;
    L__DSIMG_TITLE: TLabel;
    L__FOCALLENGTH: TLabel;
    L__FRATIO: TLabel;
    L__FRATIO_TITLE: TLabel;
    L__ISCALE: TLabel;
    L__LIMG: TLabel;
    L__LIMG_TITLE: TLabel;
    L__AM_PC1: TLabel;
    L__ANG_DEG_APPSIZE: TLabel;
    L__50_EXPTIME: TLabel;
    L__50_EXPTIME_TITLE: TLabel;
    L__AE_EARTH: TLabel;
    L__AE_JUPITER: TLabel;
    L__AE_MARS: TLabel;
    L__AE_MERCURY: TLabel;
    L__AE_NEPTUN: TLabel;
    L__AE_PLUTO: TLabel;
    L__AE_SATURN: TLabel;
    L__AE_URANUS: TLabel;
    L__AE_VENUS: TLabel;
    L__AM_KM: TLabel;
    L__AM_LHH: TLabel;
    L__AM_LHH1: TLabel;
    L__AM_LSS: TLabel;
    L__AM_LSS1: TLabel;
    L__AM_LY: TLabel;
    L__AM_PC: TLabel;
    L__ANG_DEG: TLabel;
    L__ANG_DEG1: TLabel;
    L__ANG_DEG2: TLabel;
    L__ANG_DEG3: TLabel;
    L__ANG_HH_DEC: TLabel;
    L__ANG_MM: TLabel;
    L__ANG_MM1: TLabel;
    L__ANG_SS: TLabel;
    L__ANG_SS1: TLabel;
    L__ANG_SS2: TLabel;
    L__ANG_SS3: TLabel;
    L__AU_NEWHORIZONS_DIST: TLabel;
    L__AU_VOYAGER1: TLabel;
    L__AU_VOYAGER2: TLabel;
    L__DECIMAL: TLabel;
    L__DECIMAL1: TLabel;
    L__EARTH_DIAMETER: TLabel;
    L__EXP_MIN: TLabel;
    L__ANGLE: TLabel;
    L__ANGLE_DIST: TLabel;
    L__ANGLE_JUPITER: TLabel;
    L__ANGLE_SIZE: TLabel;
    L__BGL: TLabel;
    L__BGL_TITLE: TLabel;
    L__BG_MAG: TLabel;
    L__DC_SIGNAL_CELSIUS: TLabel;
    L__DC_SIGNAL_TITLE: TLabel;
    L__D_AIRY: TLabel;
    L__D_AIRY_TITLE: TLabel;
    L__EXP_CNT: TLabel;
    L__EXP_MIN1: TLabel;
    L__EXP_S: TLabel;
    L__INFO_UNIT: TLabel;
    L__LAE: TLabel;
    L__LOAN: TLabel;
    L__LOP: TLabel;
    L__LY_ALPACENTAURI: TLabel;
    L__LY_D_COMA: TLabel;
    L__LY_D_M31_DIST: TLabel;
    L__LY_D_MILKYWAY_DIAM: TLabel;
    L__LY_D_TWINQ: TLabel;
    L__LY_D_VIRGO_DIST: TLabel;
    L__LY_D_VUNIV: TLabel;
    L__LY_M42: TLabel;
    L__MAG_FP: TLabel;
    L__MAG_FP_TITLE: TLabel;
    L__MOON_ANGLE: TLabel;
    L__MOON_DIAMETER_DIAM: TLabel;
    L__NYQUIST: TLabel;
    L__NYQUIST2: TLabel;
    L__NYQUIST_TITLE: TLabel;
    L__OPTPX: TLabel;
    L__OPTPX_TITLE: TLabel;
    L__PIXPSTAR: TLabel;
    L__PXC_CNT: TLabel;
    L__PXC_DIST: TLabel;
    L__PXC_DIST_UNIT: TLabel;
    L__PXC_MANUF: TLabel;
    L__PXC_MODEL: TLabel;
    L__PXC_MP: TLabel;
    L__PXC_OBJ_DETAIL: TLabel;
    L__PXC_OBJ_DETAIL_UNIT: TLabel;
    L__PXC_RES: TLabel;
    L__PXC_SFORMAT: TLabel;
    L__PXC_SIZE: TLabel;
    L__PXPSTAR_TITLE: TLabel;
    L__RES_ATMOSPH: TLabel;
    L__RES_ATMOSPH_TITLE: TLabel;
    L__RES_CAMERA: TLabel;
    L__RES_CAMERA_TITLE: TLabel;
    L__RES_EYE_TITLE: TLabel;
    L__RES_HUBBLE_TITLE: TLabel;
    L__RES_LIMITED_BY: TLabel;
    L__RES_TELESCOPE: TLabel;
    L__RES_TELESCOPE_TITLE: TLabel;
    L__SENSDYN: TLabel;
    L__SENSDYN_TITLE: TLabel;
    L__SENSOR: TLabel;
    L__SIGNAL_BG: TLabel;
    L__SIGNAL_BG_TITLE: TLabel;
    L__SIGNAL_DC: TLabel;
    L__SIGNAL_STAR: TLabel;
    L__SIGNAL_STAR_TITLE: TLabel;
    L__SNR: TLabel;
    L__SNR_TXT: TLabel;
    L__STAR_MAG: TLabel;
    L__STAR_SATUR: TLabel;
    L__STAR_SATUR_TITLE: TLabel;
    L__SUN_ANGLE: TLabel;
    L__SUN_DIAMETER: TLabel;
    L__TELTYPE: TLabel;
    L__TELTYPE_TITLE: TLabel;
    P__CAMERA: TPanel;
    P__CAMERA_TITLE: TPanel;
    P__CAMSETTINGS: TPanel;
    P__AM_DIST_TITLE: TPanel;
    P__DEG_MM_SS_TITLE: TPanel;
    P__ANGLE_EXAMPLES_TITLE: TPanel;
    P__APP_SIZE_TITLE: TPanel;
    P__EXP_WARN: TPanel;
    P__HH_MM_SS_TITLE: TPanel;
    P__LAE_TITLE: TPanel;
    P__LAE: TPanel;
    P__DT_TO_DEC: TPanel;
    P__CONV: TPanel;
    P__DT_TO_DEC_TITLE: TPanel;
    P__PXC_INFO: TPanel;
    P__PXC_INFO_TITLE: TPanel;
    P__SETTINGS: TPanel;
    P__ORBCALC_TITLE: TPanel;
    P__SIGNALS_CAPTION: TPanel;
    P__SIGNALS: TPanel;
    P__NYQUIST: TPanel;
    P__OBJ__PX: TPanel;
    PC__APP: TPageControl;
    P__PIXCALC: TPanel;
    P__PIXCALC_TOP: TPanel;
    PC__ASTROCALC: TPageControl;
    P__OBJ_PX_TITLE: TPanel;
    P__NYQUIST_TITLE: TPanel;
    P__TELESCOPE: TPanel;
    P__TELESCOPE_TITLE: TPanel;
    RBG__SEEING: TRadioGroup;
    TS__SETTINGS: TTabSheet;
    TB__BG_MAG: TTrackBar;
    TB__DC_SIGNAL: TTrackBar;
    TB__STAR_MAG: TTrackBar;
    TS__NYQUIST: TTabSheet;
    TS__OBJ_PX: TTabSheet;
    TS__PIXCALC: TTabSheet;
    TS__ANGLES: TTabSheet;
    TS__ASTROMETRY: TTabSheet;
    TS__CONV: TTabSheet;
    TS__SIGNALS: TTabSheet;
    procedure B__ANGLE_CALCClick(Sender: TObject);
    procedure B__CAMERAINFOClick(Sender: TObject);
    procedure B__DT_TO_DECClick(Sender: TObject);
    procedure B__LAEClick(Sender: TObject);
    procedure B__NEWCAMClick(Sender: TObject);
    procedure B__PXC_CALCClick(Sender: TObject);
    procedure CBX__DBELChange(Sender: TObject);
    procedure CBX__MANUAL_SENSORSELChange(Sender: TObject);
    procedure CB__PXC_MANUFChange(Sender: TObject);
    procedure CB__PXC_MODELChange(Sender: TObject);
    procedure CB__PXC_SFORMATChange(Sender: TObject);
    procedure ED__AM_AUChange(Sender: TObject);
    procedure ED__AM_KMChange(Sender: TObject);
    procedure ED__AM_KMMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ED__AM_LHHChange(Sender: TObject);
    procedure ED__AM_LMMChange(Sender: TObject);
    procedure ED__AM_LSSChange(Sender: TObject);
    procedure ED__AM_LYChange(Sender: TObject);
    procedure ED__AM_MPCChange(Sender: TObject);
    procedure ED__AM_PCChange(Sender: TObject);
    procedure ED__ANG_DEGChange(Sender: TObject);
    procedure ED__ANG_HHChange(Sender: TObject);
    procedure ED__ANG_MM1Change(Sender: TObject);
    procedure ED__ANG_MMChange(Sender: TObject);
    procedure ED__ANG_MS1Change(Sender: TObject);
    procedure ED__ANG_MSChange(Sender: TObject);
    procedure ED__ANG_SS1Change(Sender: TObject);
    procedure ED__ANG_SSChange(Sender: TObject);
    procedure ED__APERTUREChange(Sender: TObject);
    procedure ED__DEG1Change(Sender: TObject);
    procedure ED__DEGChange(Sender: TObject);
    procedure ED__EXP_CNTChange(Sender: TObject);
    procedure ED__EXP_SChange(Sender: TObject);
    procedure ED__FOCALLENGTHChange(Sender: TObject);
    procedure ED__FRATIOChange(Sender: TObject);
    procedure ED__HH_DECChange(Sender: TObject);
    procedure ED__PXC_MPChange(Sender: TObject);
    procedure ED__PXC_PX_SIZEChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure L__AE_EARTHClick(Sender: TObject);
    procedure L__AE_JUPITERClick(Sender: TObject);
    procedure L__AE_MARSClick(Sender: TObject);
    procedure L__AE_MERCURYClick(Sender: TObject);
    procedure L__AE_NEPTUNClick(Sender: TObject);
    procedure L__AE_PLUTOClick(Sender: TObject);
    procedure L__AE_SATURNClick(Sender: TObject);
    procedure L__AE_URANUSClick(Sender: TObject);
    procedure L__AE_VENUSClick(Sender: TObject);
    procedure L__ANGLE_JUPITERClick(Sender: TObject);
    procedure L__AU_NEWHORIZONSClick(Sender: TObject);
    procedure L__AU_VOYAGER1Click(Sender: TObject);
    procedure L__AU_VOYAGER2Click(Sender: TObject);
    procedure L__LY_ALPACENTAURIClick(Sender: TObject);
    procedure L__LY_D_M31Click(Sender: TObject);
    procedure L__LY_D_TWINQClick(Sender: TObject);
    procedure L__LY_D_VIRGOClick(Sender: TObject);
    procedure L__LY_D_COMAClick(Sender: TObject);
    procedure L__LY_D_MILKYWAYClick(Sender: TObject);
    procedure L__LY_D_VUNIVClick(Sender: TObject);
    procedure L__LY_M42Click(Sender: TObject);
    procedure L__MOON_ANGLEClick(Sender: TObject);
    procedure L__MOON_DIAMETER_DIAMMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure L__MOON_DIAMETER_DIAMMouseEnter(Sender: TObject);
    procedure L__MOON_DIAMETER_DIAMMouseLeave(Sender: TObject);
    procedure L__SUN_ANGLEClick(Sender: TObject);
    procedure L__SUN_DIAMETERClick(Sender: TObject);
    procedure L__EARTH_DIAMETERClick(Sender: TObject);
    procedure L__MOON_DIAMETERClick(Sender: TObject);
    procedure RBG__SEEINGClick(Sender: TObject);
    procedure TB__BG_MAGChange(Sender: TObject);
    procedure TB__DC_SIGNALChange(Sender: TObject);
    procedure TB__STAR_MAGChange(Sender: TObject);
  private
    mbCalcDistInOperation: Boolean;
    mCamera: TCamera;

    function GetCnt(iMax,iMin,iVal: Integer; bUp: Boolean): Integer;
    procedure CalcDist(iMode: Integer; sValEdit: string);
    procedure CalcDEGMMSSToDEC();
    procedure CalcHHMMSSToDEC();
    procedure CalcToHHMMSS(sValue: string; rFac: Real);
    procedure GetSensorDims(iIndex: Integer; var fX: Real; var fY: Real);
    function GetLScale(sUnit: string): Real;
    procedure CalcObjPX();
    procedure CalcObjAngle();
    procedure GetCanonModels();
    procedure GetNikonModels();
    procedure GetOlympusModels();
    procedure GetSonyModels();
    procedure GetCanonModelData(sModel: string; Camera: TCamera);//var iFIndex: Integer; var fMPValue: Real);
    procedure GetNikonModelData(sModel: string; Camera: TCamera);
    procedure GetOlympusModelData(sModel: string; Camera: TCamera);
    procedure GetSonyModelData(sModel: string; Camera: TCamera);
    function GetStarDiscDiam(): Integer;
    procedure ShowCameraProps();
    procedure SaveCamera(Camera: TCamera);
    procedure GetUserDefinedModels();
    procedure GetCustomModelData(sModel, sManufacturer: string; Camera: TCamera);
    function GetCamRes(): Real;
    function GetDCSignal(): Real; procedure Out_DCSignal();
    function GetBGSignal(): Real; procedure Out_BGSignal();
    function GetExpTime50FWSec(): Real; procedure Out_ExpTime50FWSec();
    function GetStarSignal(): Real; procedure Out_StarSignal();
    function GetStarSaturSec(): Real; procedure Out_StarSaturSec();
    function GetBGL(): Real; procedure Out_BGL();
    procedure ReadCamera(var Camera: TCamera);
    procedure SetCamera(var Camera: TCamera; F__NEWCAM: TF__NEWCAM);
    procedure GetSBIGModels();
    procedure GetSBIGModelData(sModel: string; Camera: TCamera);
    procedure CalcA0_SNR();
    procedure Out_SensorDyn();
    procedure CalcAllSignalValues();
    procedure SetFContColor_EXP_S(iValue: Integer);
    function CalcPixSize(sSenForm: string; fMPixel: Real): Real;
    procedure CameraDetails();
    procedure AddCamera();
    procedure PrepareDistEdits();
    procedure EnterDistLabel(DistLabel: TLabel);
    procedure LeaveDistLabel(DistLabel: TLabel);
    procedure MDownDistLabel(DistLabel: TLabel);
    procedure CalcOptFL_LIMG();
    procedure CalcOptFL_DSIMG();
    procedure CalcMag_FP();

  public
    msLANG_ID: string;
    mADevice: TADevice;
    msAlbireoLocalDir: string;
    mslCustCameraList: TStringList;
    mbCamHasAdded: Boolean;
    mrS0, mrBGL, mfExpTimeSec, mfStarSaturSec: Real;
    //mbIsRegistered: Boolean;
    mbSetFormDim: Boolean;
    miFormLeft, miFormTop, miFormHeight, miFormWidth: Integer;

    procedure IniAstroCalc();
    procedure ActivateCamera();
    procedure GetCameraModels();

  end;

var
  F__ASTROCALC: TF__ASTROCALC;

const
  ciMIndex_1_32 = 0;
  ciMIndex_1_27 = 1;
  ciMIndex_1_25 = 2;
  ciMIndex_1_18 = 3;
  ciMIndex_2_3 = 4;
  ciMIndex_1 = 5;
  ciMIndex_FOURTHIRDS = 6;
  ciMIndex_FOVEON = 7;
  ciMIndex_APSC = 8;
  ciMIndex_APSH = 9;
  ciMIndex_DX = 10;
  ciMIndex_KLEINBILD = 11;
  ciMIndex_MITTELFORMAT = 12;
  ciMIndex_CANON_DCS3 = 13;
  ciMIndex_CANON_DCS1 = 14;
  ciMIndex_NIKON_FX = 15;
  ciMIndex_NIKON_DX = 16;
  ciMIndex_1_63 = 17;
  ciMIndex_1_1 = 18;
  ciMIndex_1_2 = 19;
  ciMIndex_SBIG1 = 20;

  clSlCol = clBlue;
  clLvCol = clAqua;

implementation

{$R *.lfm}

{ TF__ASTROCALC }

procedure TF__ASTROCALC.CalcMag_FP();
begin
  // Source: https://astrofotografie.hohmann-edv.de/aufnahmetechniken/grundlagen.fokale.projektion.php

  if(ED__FOCALLENGTH.Value > 0) and (ED__PXC_PX_SIZE.Value > 0) then
    L__MAG_FP.Caption :=
      IntToStr(Round(2.0 * 0.29 * ED__FOCALLENGTH.Value / ED__PXC_PX_SIZE.Value)) + 'x';
end;

procedure TF__ASTROCALC.CalcOptFL_LIMG();
  // Lucky Imaging: Nyquist criteria applied on telescope resolution! Gets to large focal lengths used for planet photography
begin
  if(msLANG_ID = 'DE') then
    L__LIMG_TITLE.Caption:= 'Lucky Imaging (kleine Bel.-Zeiten):'
  else
    L__LIMG_TITLE.Caption:= 'Lucky Imaging (low exposure):';

  L__LIMG.Caption := IntToStr(Round(206.0 * ED__PXC_PX_SIZE.Value / (138.0/ED__APERTURE.Value))) + ' mm';
end;

procedure TF__ASTROCALC.CalcOptFL_DSIMG();
  // Nyquist criteria appliead on seeing resolution. Used for DeepSky photography
begin
  if(msLANG_ID = 'DE') then
    L__DSIMG_TITLE.Caption:= 'Für DeepSky bei ausgew. Seeing (große Bel.-Zeiten):'
  else
    L__DSIMG_TITLE.Caption:= 'DeepSky related to selected seeing (high exposure):';

  L__DSIMG.Caption:= IntToStr(Round(206.0 * ED__PXC_PX_SIZE.Value / GetStarDiscDiam() )) + ' mm';
end;

procedure TF__ASTROCALC.MDownDistLabel(DistLabel: TLabel);
begin
  if (not Assigned(DistLabel)) then
    exit;

  if(msLANG_ID = 'DE') then
    P__AM_DIST_TITLE.Caption:='Entfernungsumrechnung: ' + DistLabel.Caption
  else
    P__AM_DIST_TITLE.Caption:='Distance Calculation: ' + DistLabel.Caption;

end;

procedure TF__ASTROCALC.LeaveDistLabel(DistLabel: TLabel);
begin
  if (not Assigned(DistLabel)) then
    exit;

  DistLabel.Font.Color:=clLvCol;
end;


procedure TF__ASTROCALC.EnterDistLabel(DistLabel: TLabel);
begin
  if (not Assigned(DistLabel)) then
    exit;

  DistLabel.Font.Color:=clSlCol;

end;

procedure TF__ASTROCALC.PrepareDistEdits();
begin

  if(msLANG_ID = 'DE') then
    P__AM_DIST_TITLE.Caption:='Entfernungsumrechnung'
  else
    P__AM_DIST_TITLE.Caption:='Distance Calculation';

end;

procedure TF__ASTROCALC.AddCamera();
begin
  //if ( not IsAdvancedFeature(msLANG_ID,mbIsRegistered) ) then
  //  exit;

  F__NEWCAM := TF__NEWCAM.Create(nil);
  F__NEWCAM.msLANG_ID := msLANG_ID;
  F__NEWCAM.ED__SENSORYEAR.Caption := IntToStr(YearOf(Now));

  IniText(F__NEWCAM,msLANG_ID);

  if(msLANG_ID = 'DE') then
    F__NEWCAM.Caption:='Kamera & Sensor hinzufügen'
  else
    F__NEWCAM.Caption:='Add Camera & Sensor';

  if(CB__PXC_MANUF.Text <> '') then
    F__NEWCAM.ED__CAM_MANU.Text := CB__PXC_MANUF.Text;

  if(F__NEWCAM.ShowModal = mrOK) then
  begin
    if(CB__PXC_MANUF.Items.IndexOf(F__NEWCAM.ED__CAM_MANU.Text) = -1) then
    begin
      CB__PXC_MANUF.Items.Add(F__NEWCAM.ED__CAM_MANU.Text);

      CB__PXC_MODEL.Items.Clear;
      //CB__PXC_MODEL.Items.Add(F__NEWCAM.ED__CAM_MODEL.Text);
    end;

    if (CB__PXC_MODEL.Items.IndexOf(F__NEWCAM.ED__CAM_MODEL.Text) = -1) then
    begin
      CB__PXC_MODEL.Items.Add(F__NEWCAM.ED__CAM_MODEL.Text);

      SetCamera(mCamera,F__NEWCAM);

      mCamera.iEstimated:=-1; // User defined sensor data

      SaveCamera(mCamera);

      mslCustCameraList.AddObject(mCamera.sManufacturer + '/' + mCamera.sModel,mCamera);

      if(CB__PXC_MANUF.Items.IndexOf(mCamera.sManufacturer) = -1) then
        CB__PXC_MANUF.Items.Add(mCamera.sManufacturer);

      mbCamHasAdded := true;

      CB__PXC_MANUF.Text:=mCamera.sManufacturer;
      CB__PXC_MODEL.Text:=mCamera.sModel;
      ED__PXC_MP.Text:=FloatToStr(mCamera.fMegaPixel);
      CB__PXC_SFORMAT.ItemIndex:=mCamera.iSensorFormatIndex;
      ED__PXC_PX_SIZE.Value := CalcPixSize(CB__PXC_SFORMAT.Text,ED__PXC_MP.Value);

    end
    else
    begin

      if(msLANG_ID = 'DE') then
        MessageDlg('Aktion abgebrochen','Ein Kameramodell ''' + F__NEWCAM.ED__CAM_MODEL.Text +
          ''' von Hersteller ''' + F__NEWCAM.ED__CAM_MANU.Text +
          ''' ist bereits vorhanden',mtWarning,[mbOK],0)
      else
        MessageDlg('Action aborted','A camera model ''' + F__NEWCAM.ED__CAM_MODEL.Text +
          ''' by mynufacturer ''' + F__NEWCAM.ED__CAM_MANU.Text +
          ''' is already defined',mtWarning,[mbOK],0);

    end;
  end;

  F__NEWCAM.Free;

end;

procedure TF__ASTROCALC.CameraDetails();
begin
  //if ( not IsAdvancedFeature(msLANG_ID,mbIsRegistered) ) then
  //  exit;

  F__NEWCAM := TF__NEWCAM.Create(nil);
  F__NEWCAM.msLANG_ID := msLANG_ID;

  IniText(F__NEWCAM,msLANG_ID);

  if(msLANG_ID = 'DE') then
  begin
    F__NEWCAM.Caption:='Kamera & Sensor';
    F__NEWCAM.BT__OK.Caption:='Übernehmen';
  end
  else
  begin
    F__NEWCAM.Caption:='Camera & Sensor';
    F__NEWCAM.BT__OK.Caption:='Save';
  end;

  ReadCamera(mCamera);

  F__NEWCAM.BT__OK.Visible := false;
  F__NEWCAM.BT__CANCEL.Visible := false;
  F__NEWCAM.mbModifiedCamera:=true;

  if(F__NEWCAM.ShowModal = mrOK) and (F__NEWCAM.BT__OK.Visible) then
    SetCamera(mCamera,F__NEWCAM);

  F__NEWCAM.Destroy;

end;

procedure TF__ASTROCALC.CalcAllSignalValues();
begin
  mrS0 := GetStarBaseSignalCompact(ED__APERTURE.Value,L__TELTYPE.Caption);

  Out_StarSignal();
  Out_BGSignal();
  Out_DCSignal();
  Out_ExpTime50FWSec();
  Out_StarSaturSec();
  Out_BGL();
  CalcA0_SNR();
  Out_SensorDyn();

end;

function TF__ASTROCALC.GetDCSignal(): Real;
begin
  Result := mCamera.fDCRC/(power(2,(mCamera.fDCRT - TB__DC_SIGNAL.Position)/mCamera.fDCHT));
end;

procedure TF__ASTROCALC.Out_DCSignal();
var
  fDCSignal: Real;
begin
  //if( not IsAdvancedFeature('',mbIsRegistered) ) then
  //  exit;

  L__DC_SIGNAL_CELSIUS.Caption := IntToStr(TB__DC_SIGNAL.Position) + ' °C';
  fDCSignal := GetDCSignal();
  L__SIGNAL_DC.Caption := FloatToStrF(fDCSignal,ffFixed,8,1) + ' e-/s';
end;

procedure TF__ASTROCALC.Out_SensorDyn();
var
  fDCSignal, fSensorDyn: Real;
begin
  //if( not IsAdvancedFeature('',mbIsRegistered) ) then
  //  exit;

  fDCSignal := GetDCSignal();
  fSensorDyn := mCamera.iFWC/sqrt(fDCSignal * ED__EXP_S.Value);

  if(fSensorDyn > 0) then
  begin
    if(CBX__DBEL.Checked) then
      L__SENSDYN.Caption := FloatToStrF(10*log10(fSensorDyn),ffFixed,8,1) + ' dB'
    else
      L__SENSDYN.Caption := FloatToStrF(fSensorDyn,ffFixed,8,1);
  end
  else
    L__SENSDYN.Caption := '---';

end;

function TF__ASTROCALC.GetBGSignal(): Real;
var
  rCamRes: Real;
  //iStarDiam: Integer;
begin
  rCamRes := GetCamRes();
  //iStarDiam := GetStarDiscDiam();

  // *iStarDiam^2: Star diameter > 1'' because auf seeing
  //Result := rCamRes*rCamRes* mrS0*power(10.0,-0.4*TB__BG_MAG.Position) *iStarDiam;//*iStarDiam;
  Result := rCamRes*rCamRes* mrS0*power(10.0,-0.4*TB__BG_MAG.Position);

end;

procedure TF__ASTROCALC.Out_BGSignal();
var
  rBGS: Real;
begin
  //if( not IsAdvancedFeature('',mbIsRegistered) ) then
  //  exit;

  L__BG_MAG.Caption := 'Mag ' + IntToStr(TB__BG_MAG.Position) + '/[]''''';

  rBGS := GetBGSignal();

  L__SIGNAL_BG.Caption := FloatToStrF(rBGS,ffFixed,8,2) + ' e-/s';
end;

function TF__ASTROCALC.GetStarSaturSec(): Real;
var
  fSignal: Real;
begin
  fSignal := GetBGSignal() + GetDCSignal() + GetStarSignal();

  if(fSignal > 0) then
    Result := mCamera.iFWC / fSignal
  else
    Result := 0;

end;

procedure TF__ASTROCALC.Out_StarSaturSec();
begin
  //if( not IsAdvancedFeature('',mbIsRegistered) ) then
  //  exit;

  mfStarSaturSec := GetStarSaturSec();

  if(mfStarSaturSec < 0.1) then
    L__STAR_SATUR.Caption := FloatToStrF(1000* mfStarSaturSec,ffFixed,8,1) + ' ms'
  else if(mfStarSaturSec < 60) then
    L__STAR_SATUR.Caption := FloatToStrF(mfStarSaturSec,ffFixed,8,1) + ' s'
  else
    L__STAR_SATUR.Caption := FloatToStrF(Trunc(mfStarSaturSec/60.0),ffFixed,8,1) + ' Min ' +
      FloatToStrF(Trunc(mfStarSaturSec - 60*Trunc(mfStarSaturSec/60.0)),ffFixed,8,1) + ' s';
end;

function TF__ASTROCALC.GetExpTime50FWSec(): Real;
var
  fSignal: Real;
begin
  fSignal := GetBGSignal() + GetDCSignal();

  if(fSignal > 0) then
    Result := mCamera.iFWC / 2.0 / fSignal
  else
    Result := 0;

end;

procedure TF__ASTROCALC.Out_ExpTime50FWSec();
begin
  //if( not IsAdvancedFeature('',mbIsRegistered) ) then
  //  exit;

  mfExpTimeSec := GetExpTime50FWSec();

  L__50_EXPTIME.Caption:= IntToStr(Trunc(mfExpTimeSec/60)) + ' Min ' + IntToStr(Trunc(mfExpTimeSec - 60*Trunc(mfExpTimeSec/60))) + ' s';
end;

function TF__ASTROCALC.GetStarSignal(): Real;
begin
  Result := mrS0*power(10.0,-0.4*TB__STAR_MAG.Position)
end;

procedure TF__ASTROCALC.Out_StarSignal();
var
  fStarSignal: Real;
begin
  //if( not IsAdvancedFeature('',mbIsRegistered) ) then
  //  exit;

  L__STAR_MAG.Caption := 'Mag ' + IntToStr(TB__STAR_MAG.Position);

  fStarSignal := GetStarSignal();

  if(fStarSignal > 100000) then
    L__SIGNAL_STAR.Caption :=
      FloatToStrF(fStarSignal/1e6,ffFixed,8,2) + ' Me-/s'
  else
    L__SIGNAL_STAR.Caption :=
      FloatToStrF(fStarSignal,ffFixed,8,2) + ' e-/s';
end;

function TF__ASTROCALC.GetBGL(): Real;
var
  fStarSignal, fBGSignal, fDCSignal: Real;
begin
  //Result := -1;
  (*

  fSignal := GetBGSignal() - GetDCSignal();

  if(fSignal > 0) then
    Result := 10* (mCamera.iRN * mCamera.iRN)/fSignal;
  *)
  fStarSignal := GetStarSignal();
  fBGSignal := GetBGSignal();
  fDCSignal := GetDCSignal();

  Result := 10*(mCamera.iRN*mCamera.iRN)/(fStarSignal+fBGSignal+fDCSignal);

end;

procedure TF__ASTROCALC.Out_BGL();
begin
  //if( not IsAdvancedFeature('',mbIsRegistered) ) then
  //  exit;

  mrBGL := GetBGL();

  if(mrBGL > 0) then
  begin
    if(mrBGL < 60) then
      L__BGL.Caption := IntToStr(Round(mrBGL)) + ' s'
    else
      L__BGL.Caption := IntToStr(Trunc(mrBGL/60.0)) + ' Min ' +
        IntToStr(Trunc(mrBGL - 60.0*Trunc(mrBGL/60.0))) + ' s';

  end
  else
  begin
    if(msLANG_ID = 'DE') then
      L__BGL.Caption := 'Hintergrundlimitierung aufgrund hohem Dunkelstromrauschen nicht erreichbar'
    else
      L__BGL.Caption := 'Background limitation not accessible because of high dark current noise';

  end;
end;

procedure TF__ASTROCALC.CalcA0_SNR();
var
  fStarSignal, fBGSignal, fDCSignal: Real;
  fExpNoise, fSNR: Real;
begin
  fStarSignal := GetStarSignal() * ED__EXP_S.Value;
  fBGSignal := GetBGSignal() * ED__EXP_S.Value;
  fDCSignal := GetDCSignal() * ED__EXP_S.Value;

  fExpNoise := sqrt((mCamera.iRN * mCamera.iRN + fDCSignal + fBGSignal + fStarSignal)/ED__EXP_CNT.Value);

  fSNR := fStarSignal/fExpNoise;

  if(msLANG_ID = 'DE') then
  begin
    if(fSNR < 3) then
      L__SNR_TXT.Caption:='Signal nicht erkennbar'
    else if(fSNR < 10) then
      L__SNR_TXT.Caption:='Signal ist schwach'
    else if(fSNR < 100) then
      L__SNR_TXT.Caption:='Signal sicher erkennbar'
    else
      L__SNR_TXT.Caption:='Signal sehr deutlich';

  end
  else
  begin
    if(fSNR < 3) then
      L__SNR_TXT.Caption:='Signal not cognizable'
    else if(fSNR < 10) then
      L__SNR_TXT.Caption:='Signal is weak'
    else if(fSNR < 100) then
      L__SNR_TXT.Caption:='Signal easily cognizable'
    else
      L__SNR_TXT.Caption:='Signal very clearly';
  end;

  if(CBX__DBEL.Checked) then
  begin
    fSNR := 10*log10(fSNR);
    L__SNR.Caption:='SNR: ' + FloatToStrF(fSNR,ffFixed,8,1) + ' dB';
  end
  else
    L__SNR.Caption:='SNR: ' + FloatToStrF(fSNR,ffFixed,8,1);

  SetFContColor_EXP_S(ED__EXP_S.Value);

end;

procedure TF__ASTROCALC.ReadCamera(var Camera:TCamera);
begin
  if(Camera = nil) then
    exit;

  F__NEWCAM.ED__CAM_MANU.Text := Camera.sManufacturer;
  F__NEWCAM.ED__CAM_MODEL.Text := Camera.sModel;
  F__NEWCAM.CB__CAM_SENSOR.ItemIndex := Camera.iSensorFormatIndex;
  F__NEWCAM.ED__SENSORNAME.Text := Camera.sSensorName;

  if(Camera.sSensorType = 'CMOS') then
  begin
    F__NEWCAM.RB__CMOS.Checked := true;
    F__NEWCAM.RB__CCD.Checked := false;
  end
  else if(Camera.sSensorType = 'CCD') then
  begin
    F__NEWCAM.RB__CMOS.Checked := false;
    F__NEWCAM.RB__CCD.Checked := true;
  end;

  F__NEWCAM.ED__MP_INT.Text := IntToStr(Trunc(Camera.fMegaPixel));
  F__NEWCAM.ED__CAM_MP_DEC.Text := IntToStr(Trunc( 10*(Camera.fMegaPixel - Trunc(Camera.fMegaPixel)) ) );

  F__NEWCAM.ED__QEFF.Text := IntToStr(Camera.iQEff);
  F__NEWCAM.ED__DCRC.Text := FloatToStr(Camera.fDCRC);
  F__NEWCAM.ED__DCRT.Text := FloatToStr(Camera.fDCRT);
  F__NEWCAM.ED__DCHT.Text := FloatToStr(Camera.fDCHT);
  F__NEWCAM.ED__FWC.Text  := FloatToStr(Camera.iFWC);
  F__NEWCAM.ED__RN.Text   := IntToStr(Camera.iRN);
  F__NEWCAM.ED__BITS.Text := IntToStr(Camera.iBits);
  if(Camera.iSensorYear > 0) then
    F__NEWCAM.ED__SENSORYEAR.Text := IntToStr(Camera.iSensorYear)
  else
    F__NEWCAM.ED__SENSORYEAR.Text := '';

  F__NEWCAM.RB__SV_U.Checked := false;
  F__NEWCAM.RB__SV_E.Checked := false;
  F__NEWCAM.RB__SV_S.Checked := false;

  if(Camera.iEstimated = -1) then
    F__NEWCAM.RB__SV_U.Checked := true
  else if(Camera.iEstimated = 1) then
    F__NEWCAM.RB__SV_E.Checked := true
  else
    F__NEWCAM.RB__SV_S.Checked := true;

end;

procedure TF__ASTROCALC.SetCamera(var Camera:TCamera; F__NEWCAM: TF__NEWCAM);
begin
  if(Camera = nil) then
    exit;

  Camera.sManufacturer:=F__NEWCAM.ED__CAM_MANU.Text;
  Camera.sModel:=F__NEWCAM.ED__CAM_MODEL.Text;
  Camera.iSensorFormatIndex:=F__NEWCAM.CB__CAM_SENSOR.ItemIndex;
  Camera.sSensorName:=F__NEWCAM.ED__SENSORNAME.Text;

  if(F__NEWCAM.RB__CMOS.Checked) then
    Camera.sSensorType:='CMOS';

  if(F__NEWCAM.RB__CCD.Checked) then
    Camera.sSensorType:='CCD';

  Camera.fMegaPixel:=1.0*StrToInt(F__NEWCAM.ED__MP_INT.Text) + StrToInt(F__NEWCAM.ED__CAM_MP_DEC.Text)/10.0;
  Camera.iQEff := StrToInt(F__NEWCAM.ED__QEFF.Text);
  Camera.fDCRC := StrToFloatExt(F__NEWCAM.ED__DCRC.Text);
  Camera.fDCRT := StrToFloatExt(F__NEWCAM.ED__DCRT.Text);
  Camera.fDCHT := StrToFloatExt(F__NEWCAM.ED__DCHT.Text);
  Camera.iFWC := StrToInt(F__NEWCAM.ED__FWC.Text);
  Camera.iRN := StrToInt(F__NEWCAM.ED__RN.Text);
  Camera.iBits := StrToInt(F__NEWCAM.ED__BITS.Text);
  if(Trim(F__NEWCAM.ED__SENSORYEAR.Text) <> '') then
    Camera.iSensorYear:=StrToInt(F__NEWCAM.ED__SENSORYEAR.Text)
  else
    Camera.iSensorYear:=0;

  // Save / re-generate file
  SaveCamera(Camera);

end;

function TF__ASTROCALC.GetCamRes(): Real;
begin
  Result := arctan(ED__PXC_PX_SIZE.Value / ED__FOCALLENGTH.Value / 1000)*180/Pi * 3600;
end;

procedure TF__ASTROCALC.GetCustomModelData(sModel, sManufacturer: string; Camera: TCamera);
var
  i: Integer;
begin
  if((mslCustCameraList = nil) or (Camera = nil)) then
    exit;

  for i:=0 to mslCustCameraList.Count-1 do
  begin
    //if(CB__PXC_MANUF.Text = (mslCustCameraList.Objects[i] as TCamera).sManufacturer) then
    if(sManufacturer = (mslCustCameraList.Objects[i] as TCamera).sManufacturer) then
    begin
      //if(CB__PXC_MODEL.Text = (mslCustCameraList.Objects[i] as TCamera).sModel) then
      if(sModel = (mslCustCameraList.Objects[i] as TCamera).sModel) then
      begin
        Camera.sManufacturer:=(mslCustCameraList.Objects[i] as TCamera).sManufacturer;
        Camera.sModel:=(mslCustCameraList.Objects[i] as TCamera).sModel;
        Camera.sSensorName:=(mslCustCameraList.Objects[i] as TCamera).sSensorName;
        Camera.iSensorFormatIndex := (mslCustCameraList.Objects[i] as TCamera).iSensorFormatIndex;
        Camera.fMegaPixel := (mslCustCameraList.Objects[i] as TCamera).fMegaPixel;
        Camera.iQEff := (mslCustCameraList.Objects[i] as TCamera).iQEff;
        Camera.fDCRT := (mslCustCameraList.Objects[i] as TCamera).fDCRT;
        Camera.fDCHT := (mslCustCameraList.Objects[i] as TCamera).fDCHT;
        Camera.fDCRC := (mslCustCameraList.Objects[i] as TCamera).fDCRC;
        Camera.iFWC := (mslCustCameraList.Objects[i] as TCamera).iFWC;
        Camera.iRN:=(mslCustCameraList.Objects[i] as TCamera).iRN;
        Camera.iBits:=(mslCustCameraList.Objects[i] as TCamera).iBits;
        Camera.iSensorYear:=(mslCustCameraList.Objects[i] as TCamera).iSensorYear;
        Camera.sSensorType:=(mslCustCameraList.Objects[i] as TCamera).sSensorType;
      end;
    end;
  end;
end;

procedure TF__ASTROCALC.GetUserDefinedModels();
var
  i: Integer;
begin
  if(mslCustCameraList = nil) then
    exit;

  for i:=0 to mslCustCameraList.Count-1 do
  begin
    if(CB__PXC_MANUF.Text = (mslCustCameraList.Objects[i] as TCamera).sManufacturer) then
    begin
      CB__PXC_MODEL.Items.AddObject((mslCustCameraList.Objects[i] as TCamera).sModel,(mslCustCameraList.Objects[i] as TCamera));
    end;
  end;

end;

procedure TF__ASTROCALC.SaveCamera(Camera: TCamera);
var
  tfCamListFile: TextFile;
  sLine: string;
  i, iFound: Integer;
  bFound: Boolean;
begin
  if (Camera = nil) or (mslCustCameraList = nil) then
    exit;

  // Save only if camera key manufacturer+model is UNIQUE!
  // Check for camera key...
  bFound := false; i:=0; iFound := -1;
  while ((not bFound) and (i < mslCustCameraList.Count)) do
  begin
    if(
      ((mslCustCameraList.Objects[i] as TCamera).sManufacturer = Camera.sManufacturer)
      and ((mslCustCameraList.Objects[i] as TCamera).sModel = Camera.sModel)
      ) then
    begin
      iFound := i;
      bFound := true;
    end;

    Inc(i);
  end;

  if(not bFound) then
  begin
    AssignFile(tfCamListFile,msAlbireoLocalDir + gcsCAMLISTFILE);
    if(FileExists(msAlbireoLocalDir + gcsCAMLISTFILE)) then
    begin
      Append(tfCamListFile);
    end
    else
    begin
      ReWrite(tfCamListFile);
      sLine :='DATE;MANUFACTURER;MODEL;SENSORNAME;SENSOR_FORMAT_INDEX;MEGAPIXEL;QEFF;DCRC;DCRT;DCHT;FULLWELLCAP;READNOISE;BITS;SENSORYEAR;SENSORTYPE';
      WriteLn(tfCamListFile,sLine);
    end;

    sLine := DateTimeToStr(Now) + ';' + Camera.sManufacturer + ';' + Camera.sModel + ';' + Camera.sSensorName;
    sLine := sLine + ';' + IntToStr(Camera.iSensorFormatIndex) + ';' + FloatToStr(Camera.fMegaPixel);
    sLine := sLine + ';' + IntToStr(Camera.iQEff) + ';' + FloatToStr(Camera.fDCRC) + ';' + FloatToStr(Camera.fDCRT) + ';' + FloatToStr(Camera.fDCHT);
    sLine := sLine + ';' + IntToStr(Camera.iFWC) + ';' + IntToStr(Camera.iRN) + ';' + IntToStr(Camera.iBits);
    sLine := sLine + ';' + IntToStr(Camera.iSensorYear) + ';' + Camera.sSensorType;

    WriteLn(tfCamListFile,sLine);

    CloseFile(tfCamListFile);
  end
  else if(iFound >= 0) then // Save changed camera data into file by overwriting camera file
  begin
    AssignFile(tfCamListFile,msAlbireoLocalDir + gcsCAMLISTFILE);
    ReWrite(tfCamListFile);

    sLine :='DATE;MANUFACTURER;MODEL;SENSORNAME;SENSOR_FORMAT_INDEX;MEGAPIXEL;QEFF;DCRC;DCRT;DCHT;FULLWELLCAP;READNOISE;BITS;SENSORYEAR;SENSORTYPE';
    WriteLn(tfCamListFile,sLine);

    // Kann auch generell so verwendet werden, wenn die Kamera in die Liste integriert ist.

    for i:=0 to mslCustCameraList.Count-1 do
    begin
      if(i = iFound) then
      begin
        sLine := DateTimeToStr(Now) + ';' + Camera.sManufacturer + ';' + Camera.sModel + ';' + Camera.sSensorName;
        sLine := sLine + ';' + IntToStr(Camera.iSensorFormatIndex) + ';' + FloatToStr(Camera.fMegaPixel);
        sLine := sLine + ';' + IntToStr(Camera.iQEff) + ';' + FloatToStr(Camera.fDCRC) + ';' + FloatToStr(Camera.fDCRT) + ';' + FloatToStr(Camera.fDCHT);
        sLine := sLine + ';' + IntToStr(Camera.iFWC) + ';' + IntToStr(Camera.iRN) + ';' + IntToStr(Camera.iBits);
        sLine := sLine + ';' + IntToStr(Camera.iSensorYear) + ';' + Camera.sSensorType;
      end
      else
      begin
        sLine := DateTimeToStr(Now) + ';' + (mslCustCameraList.Objects[i] as TCamera).sManufacturer + ';' +
          (mslCustCameraList.Objects[i] as TCamera).sModel + ';' +
          (mslCustCameraList.Objects[i] as TCamera).sSensorName;
        sLine := sLine + ';' + IntToStr((mslCustCameraList.Objects[i] as TCamera).iSensorFormatIndex) + ';' +
          FloatToStr((mslCustCameraList.Objects[i] as TCamera).fMegaPixel);
        sLine := sLine + ';' + IntToStr((mslCustCameraList.Objects[i] as TCamera).iQEff) + ';' +
          FloatToStr((mslCustCameraList.Objects[i] as TCamera).fDCRC) + ';' +
          FloatToStr((mslCustCameraList.Objects[i] as TCamera).fDCRT) + ';' +
          FloatToStr((mslCustCameraList.Objects[i] as TCamera).fDCHT);
        sLine := sLine + ';' + IntToStr((mslCustCameraList.Objects[i] as TCamera).iFWC) + ';' +
          IntToStr((mslCustCameraList.Objects[i] as TCamera).iRN) + ';' +
          IntToStr((mslCustCameraList.Objects[i] as TCamera).iBits);
        sLine := sLine + ';' + IntToStr((mslCustCameraList.Objects[i] as TCamera).iSensorYear) + ';' +
          (mslCustCameraList.Objects[i] as TCamera).sSensorType;
      end;

      WriteLn(tfCamListFile,sLine);
    end;

    CloseFile(tfCamListFile);
  end;

end;

procedure TF__ASTROCALC.GetCameraModels();
begin
  CB__PXC_MODEL.Items.Clear;
  CB__PXC_MODEL.Text:='';

  if(CB__PXC_MANUF.Text = 'Canon') then
    GetCanonModels()
  else if(CB__PXC_MANUF.Text = 'Nikon') then
    GetNikonModels()
  else if(CB__PXC_MANUF.Text = 'Olympus') then
    GetOlympusModels()
  else if(CB__PXC_MANUF.Text = 'Sony Alpha') then
    GetSonyModels()
  else if(CB__PXC_MANUF.Text = 'SBIG') then
    GetSBIGModels()
  else GetUserDefinedModels();

end;

procedure TF__ASTROCALC.ActivateCamera();
var
  sManufacturer, sModel: string;
  fX, fY, fPixSize: Real;
begin
  if(mCamera = nil) then
    exit;

  sManufacturer := CB__PXC_MANUF.Text;
  sModel := CB__PXC_MODEL.Text;
  fX := 0; fY := 0;

  mCamera.sManufacturer:=sManufacturer;
  mCamera.sModel:=sModel;

  mCamera.iEstimated:=1; // Estimated sensor data

  if(sManuFacturer = 'Canon') then GetCanonModelData(sModel,mCamera)// iFIndex,fMPValue)
  else if(sManuFacturer = 'Nikon') then GetNikonModelData(sModel,mCamera)// iFIndex,fMPValue)
  else if(sManuFacturer = 'Olympus') then GetOlympusModelData(sModel,mCamera)// iFIndex,fMPValue)
  else if(sManuFacturer = 'Sony Alpha') then GetSonyModelData(sModel,mCamera)// iFIndex,fMPValue)
  else if(sManuFacturer = 'SBIG') then GetSBIGModelData(sModel,mCamera)// iFIndex,fMPValue)
  else
  begin
    mCamera.iEstimated:=-1; // Userdefinded sensor data
    GetCustomModelData(sModel,sManufacturer,mCamera);
  end;

  // Modify S0 by Camera Value, if given
  if(mCamera.iQEff > 0) and (mCamera.iQEff <= 100) then
    mrS0 := mrS0/0.6 * mCamera.iQEff/100;

  Out_StarSignal();

  Out_StarSaturSec();

  if(mCamera.iSensorFormatIndex > -1) and (mCamera.fMegaPixel > 0) then
  begin
    CB__PXC_SFORMAT.ItemIndex := mCamera.iSensorFormatIndex;
    ED__PXC_MP.Value := mCamera.fMegaPixel;
    GetSensorDims(mCamera.iSensorFormatIndex,fX,fY);

    fPixSize := sqrt(fX*fY / (ED__PXC_MP.Value * 1000000))*1000;
    ED__PXC_PX_SIZE.Value:=fPixSize;

    B__CAMERAINFO.Enabled := true;
  end;

  ShowCameraProps();

end;

procedure TF__ASTROCALC.ShowCameraProps();
var
  iStarDiam: Integer;
  fCamRes: Real;
  fNyquist: Real;
  iNyquist: Integer;
  fRes: Real;
begin
  fCamRes := GetCamRes();
  iStarDiam := GetStarDiscDiam();
  L__RES_ATMOSPH.Caption:= FloatToStrF(iStarDiam/2.0,ffFixed,8,1) + '''''';
  L__OPTPX.Caption:= FloatToStrF(iStarDiam/2.0 * ED__FOCALLENGTH.Value/206,ffFixed,8,1) + ' mym';

  if(fCamRes > 0) then
    L__RES_CAMERA.Caption := FloatToStrF(fCamRes,ffFixed,8,1) + ' ''''';

  if(fCamRes <=0) or (iStarDiam <= 0) then
    exit;

  Out_DCSignal();
  Out_BGSignal();
  Out_ExpTime50FWSec();
  Out_BGL();
  CalcA0_SNR();
  Out_SensorDyn();

  SetFContColor_EXP_S(ED__EXP_S.Value);

  if(138.0/ED__APERTURE.Value <=  iStarDiam/2.0) then
  begin
    fRes := iStarDiam;

    if(msLANG_ID = 'DE') then
      L__RES_LIMITED_BY.Caption:='Auflösung begrenzt durch Atmosphäre (Seeing)'
    else
      L__RES_LIMITED_BY.Caption:='Resolution limited by atmosphere (Seeing)'

  end
  else
  begin
    fRes := 138.0/ED__APERTURE.Value * 2;

    if(msLANG_ID = 'DE') then
      L__RES_LIMITED_BY.Caption:='Auflösung begrenzt durch Teleskop (Öffnung)'
    else
      L__RES_LIMITED_BY.Caption:='Resolution limited by telescope (aperture)'
  end;

  //fNyquist := iStarDiam / fCamRes;
  fNyquist := fRes / fCamRes;
  iNyquist := Round(fNyquist);

  if(fRes < fCamRes) then
    L__PIXPSTAR.Caption := '1'
  else
    L__PIXPSTAR.Caption := IntToStr(iNyquist);

  if(msLANG_ID = 'DE') then
  begin
    if(iNyquist < 2) then
    begin
      L__NYQUIST.Caption := 'UNDERSAMPED. STERNE SIND PIXEL. Wenig Auflösung.';
      L__NYQUIST2.Caption := 'Kleinere Pixelgröße oder längere Brennweist empfohlen.';
    end
    else if(iNyquist >=2) and (iNyquist <= 3) then
    begin
      L__NYQUIST.Caption := 'OPTIMALES SAMPLING. NYQUIST CRITERIA ERFÜLLT!';
      L__NYQUIST2.Caption := 'SN-Verh. und Auflösung o.k.';
    end
    else
    begin
      if(iNyquist = 4) then
        L__NYQUIST.Caption := 'GERING OVERSAMPED. STERNE SIND AUFGEBLÄHT. Reduz. SN-Verh.'
      else
        L__NYQUIST.Caption := 'OVERSAMPED. STERNE SIND AUFGEBLÄHT. Geringes SN-Verh.';

      L__NYQUIST2.Caption := 'Größere Pixel oder kleinere Brennweite empfohlen.';
    end;
  end
  else
  begin
    if(iNyquist < 2) then
    begin
      L__NYQUIST.Caption := 'UNDERSAMPED. STARS ARE ANGLED. Low Resolution';
      L__NYQUIST2.Caption := 'Smaller pixels or larger focal width recommended.';
    end
    else if(iNyquist >=2) and (iNyquist <= 3) then
    begin
      L__NYQUIST.Caption := 'WELL SAMPLED. NYQUIST CRITERIA MATCHED!';
      L__NYQUIST2.Caption := 'SN-ratio and resolution o.k.';
    end
    else
    begin
      if(iNyquist = 4) then
        L__NYQUIST.Caption := 'SLIGHTLY OVERSAMPED. STARS ARE BOBBLES. Reduced SN-Ratio'
      else
        L__NYQUIST.Caption := 'OVERSAMPED. STARS ARE BOBBLES. Low SN-Ratio';

      L__NYQUIST2.Caption := 'Larger pixels or smaller focal width recommended.';
    end;

  end;

  CalcOptFL_LIMG();
  CalcOptFL_DSIMG();
  CalcMag_FP();

end;

function TF__ASTROCALC.GetStarDiscDiam(): Integer;
var
  iIndex: Integer;
begin
  Result := 0;
  iIndex := RBG__SEEING.ItemIndex;

  if(iIndex > 7) then
    Result := 2
  else if(iIndex = 0) then
    Result := 14
  else if(iIndex = 1) then
    Result := 13
  else
    Result := 3 + (6 - iIndex);

end;

procedure TF__ASTROCALC.IniAstroCalc();
var
  iStarDiam: Integer;
begin
  IniText(F__ASTROCALC,msLANG_ID);

  if(msLANG_ID = 'DE') then
  begin
    L__ISCALE.Caption := '(Abbildungsmaßstab)';
    L__PXPSTAR_TITLE.Caption:='Pixel pro Stern:';
    L__OPTPX_TITLE.Caption:='Optimale Pixelgröße';
    L__RES_LIMITED_BY.Caption:='Auflösung begrenzt durch ';
    L__RES_CAMERA_TITLE.Caption:='Auflösung Kamera:';
    L__RES_ATMOSPH_TITLE.Caption:='Auflösung Atmosph.:';
    L__PXC_OBJ_DETAIL.Caption:='Reale Größe';

    CB__PXC_DIST_UNIT.Items.Clear;
    CB__PXC_DIST_UNIT.Items.Add('km');
    CB__PXC_DIST_UNIT.Items.Add('AE');
    CB__PXC_DIST_UNIT.Items.Add('Lj');
    CB__PXC_DIST_UNIT.Items.Add('MLj');
    CB__PXC_DIST_UNIT.Items.Add('Pc');
    CB__PXC_DIST_UNIT.Items.Add('MPc');

    CB__PXC_OBJ_DETAIL_UNIT.Items.Clear;
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('km');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('AE');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('Lj');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('MLj');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('Pc');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('MPc');
  end
  else
  begin
    L__ISCALE.Caption := '(Image Scale)';
    L__PXPSTAR_TITLE.Caption:='Pixel per star:';
    L__OPTPX_TITLE.Caption:='Optimum pixel size';
    L__RES_LIMITED_BY.Caption:='Resolution limited by ';
    L__RES_CAMERA_TITLE.Caption:='Resolution Camera:';
    L__RES_ATMOSPH_TITLE.Caption:='Resolution Atmosph.:';
    L__PXC_OBJ_DETAIL.Caption:='Real Size';

    CB__PXC_DIST_UNIT.Items.Clear;
    CB__PXC_DIST_UNIT.Items.Add('km');
    CB__PXC_DIST_UNIT.Items.Add('AU');
    CB__PXC_DIST_UNIT.Items.Add('Ly');
    CB__PXC_DIST_UNIT.Items.Add('MLy');
    CB__PXC_DIST_UNIT.Items.Add('Pc');
    CB__PXC_DIST_UNIT.Items.Add('MPc');

    CB__PXC_OBJ_DETAIL_UNIT.Items.Clear;
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('km');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('AU');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('Ly');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('MLy');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('Pc');
    CB__PXC_OBJ_DETAIL_UNIT.Items.Add('MPc');
  end;

  CB__PXC_DIST_UNIT.ItemIndex:=2;
  CB__PXC_OBJ_DETAIL_UNIT.ItemIndex:=2;

  L__RES_TELESCOPE.Caption := FloatToStrF(138.0/ED__APERTURE.Value,ffFixed,8,1) + ' ''''';
  ED__FRATIO.Value := ED__FOCALLENGTH.Value/ED__APERTURE.Value;

  RBG__SEEING.ItemIndex := 7;

  iStarDiam := GetStarDiscDiam();
  L__RES_ATMOSPH.Caption:= FloatToStrF(iStarDiam/2.0,ffFixed,8,1) + '''''';
  L__OPTPX.Caption:= FloatToStrF(iStarDiam/2.0 * ED__FOCALLENGTH.Value/206,ffFixed,8,1) + ' mym';

  if(mslCustCameraList = nil) then
  begin
    // Adding user-defined manufacturers
    mslCustCameraList := TStringList.Create;
    ImportCameraList(mslCustCameraList,msAlbireoLocalDir + gcsCAMLISTFILE);
  end;

end;

function TF__ASTROCALC.GetLScale(sUnit: string): Real;
begin
  Result := 1; // km

  if(sUnit = 'AE') or (sUnit = 'AU') then
    Result := 149.598E6;

  if(sUnit = 'Lj') or (sUnit = 'Ly') then
    Result := 9.460E12;

  if(sUnit = 'MLj') or (sUnit = 'MLy') then
    Result := 9.460E18;

  if(sUnit = 'Pc') then
    Result := 3.26 * 9.460E12;

  if(sUnit = 'MPc') then
    Result := 3.26 * 9.460E18;

end;

procedure TF__ASTROCALC.CalcObjAngle();
var
  fSCALE_KM_DIST, fSCALE_KM_OBJ: Real;
  fAngle, fAngleMM, fAngleSS: Real;
  iAngleDEG, iAngleMM: Integer;
  sAngleDEG, sAngleMM, sAngleSS: string;
begin
  if(Trim(CB__PXC_DIST_UNIT.Text) = '') then
    exit;

  if(Trim(CB__PXC_OBJ_DETAIL_UNIT.Text) = '') then
    exit;

  fSCALE_KM_DIST := GetLScale(CB__PXC_DIST_UNIT.Text);

  if(ED__PXC_DIST.Value = 0) or (fSCALE_KM_DIST = 0) then
    exit;

  fSCALE_KM_OBJ := GetLScale(CB__PXC_OBJ_DETAIL_UNIT.Text);

  fAngle := arctan((ED__PXC_OBJ_DETAIL.Value * fSCALE_KM_OBJ) / (ED__PXC_DIST.Value * fSCALE_KM_DIST))*180/Pi;

  iAngleDEG := Trunc(fAngle);
  fAngleMM := (fAngle - iAngleDEG)*60.0;
  iAngleMM := Trunc(fAngleMM);
  fAngleSS := (fAngleMM - iAngleMM)*60.0;

  sAngleDEG := IntToStr(iAngleDEG);
  sAngleMM := IntToStr(iAngleMM);
  sAngleSS := FloatToStrF(fAngleSS,ffFixed,8,1);

  L__PXC_RES.Caption := sAngleDEG + '° ' + sAngleMM + ''' ' + sAngleSS + ''''' ';

end;

procedure TF__ASTROCALC.CalcObjPX();
var
  fObjPX: Real;
  fSCALE_KM_DIST, fSCALE_KM_OBJ: Real;
begin
  if(Trim(CB__PXC_DIST_UNIT.Text) = '') then
    exit;

  if(Trim(CB__PXC_OBJ_DETAIL_UNIT.Text) = '') then
    exit;

  if(ED__PXC_PX_SIZE.Value = 0) then
  begin
    if(msLANG_ID = 'DE') then
      L__PXC_CNT.Caption:='Kein Sensor'
    else
      L__PXC_CNT.Caption:='No sensor';

    exit;
  end;

  fSCALE_KM_DIST := GetLScale(CB__PXC_DIST_UNIT.Text);

  if(ED__PXC_DIST.Value = 0) or (fSCALE_KM_DIST = 0) then
    exit;

  fSCALE_KM_OBJ := GetLScale(CB__PXC_OBJ_DETAIL_UNIT.Text);
  fObjPX := ED__FOCALLENGTH.Value * (ED__PXC_OBJ_DETAIL.Value * fSCALE_KM_OBJ) / (ED__PXC_DIST.Value * fSCALE_KM_DIST);
  fObjPX := fObjPX / ED__PXC_PX_SIZE.Value * 1000;

  L__PXC_CNT.Caption := FloatToStrF(fObjPX,ffFixed,8,1) + ' Px';

end;

procedure TF__ASTROCALC.GetSensorDims(iIndex: Integer; var fX: Real; var fY: Real);
begin
  fX := 0; fY := 0;

  case iIndex of
  ciMIndex_1_32: begin fX := 4.5; fY := 3.4; end;
  ciMIndex_1_27: begin fX := 5.4; fY := 4.0; end;
  ciMIndex_1_25: begin fX := 5.8; fY := 4.3; end;
  ciMIndex_1_18: begin fX := 7.2; fY := 5.4; end;
  ciMIndex_2_3: begin fX := 8.8; fY := 6.6; end;
  ciMIndex_1: begin fX := 13.2; fY := 8.8; end;
  ciMIndex_FOURTHIRDS: begin fX := 17.3; fY := 13.0; end;
  ciMIndex_FOVEON: begin fX := 20.7; fY := 13.8; end;
  ciMIndex_APSC: begin fX := 22.2; fY := 14.8; end;
  ciMIndex_APSH: begin fX := 27.9; fY := 18.6; end;
  ciMIndex_DX: begin fX := 23.7; fY := 15.6; end;
  ciMIndex_KLEINBILD: begin fX := 36; fY := 24; end;
  ciMIndex_MITTELFORMAT: begin fX := 48; fY := 36; end;
  ciMIndex_CANON_DCS3: begin fX := 20.5; fY := 16.4; end;
  ciMIndex_CANON_DCS1: begin fX := 28.7; fY := 19.1; end;
  ciMIndex_NIKON_FX: begin fX := 24; fY := 36; end;
  ciMIndex_NIKON_DX: begin fX := 16; fY := 24; end;
  ciMIndex_1_63: begin fX := 8; fY := 6; end;
  ciMIndex_1_1: begin fX := 14.4; fY := 9.9; end;
  ciMIndex_1_2: begin fX := 7.4; fY := 5.95; end;
  ciMIndex_SBIG1: begin fX := 10.24; fY := 10.24; end;

  end; // case
end;

procedure TF__ASTROCALC.CalcToHHMMSS(sValue: string; rFac: Real);
var
  rValEdit: Real;
  bIsAlphaNum: Boolean;
  iMS: Integer;
begin
  bIsAlphaNum := true;

  rValEdit := StrToFloatExt3(sValue,bIsAlphaNum);

  if(bIsAlphaNum) then
    exit;

  rValEdit := rValEdit / rFac;

  ED__ANG_HH.Value := Trunc(rValEdit);
  ED__ANG_MM1.Value  := Trunc((rValEdit - ED__ANG_HH.Value)*60);
  ED__ANG_SS1.Value  := Trunc(((rValEdit - ED__ANG_HH.Value)*60 - ED__ANG_MM1.Value)*60);
  iMS := Round((((rValEdit - ED__ANG_HH.Value)*60 - ED__ANG_MM1.Value)*60 - ED__ANG_SS1.Value)*1000);
  if(iMS = 1000) then
  begin
    iMS := 0;
    ED__ANG_SS1.Value := ED__ANG_SS1.Value + 1;
  end;
  ED__ANG_MS1.Value  := iMS;

end;

procedure TF__ASTROCALC.CalcHHMMSSToDEC();
var
  rVal: Real;
begin
  rVal :=
    (ED__ANG_HH.Value +
    ED__ANG_MM1.Value/60.0 +
    (ED__ANG_SS1.Value + ED__ANG_MS1.Value/1000.0)/3600.0);

  ED__HH_DEC.Text := FloatToStr(rVal);
  ED__DEG1.Text := FloatToStr(rVal*15);

end;

procedure TF__ASTROCALC.CalcDEGMMSSToDEC();
begin
  ED__DEG.Text := FloatToStr(
    ED__ANG_DEG.Value +
    ED__ANG_MM.Value/60.0 +
    (ED__ANG_SS.Value + ED__ANG_MS.Value/1000.0)/3600.0
    );
end;

procedure TF__ASTROCALC.CalcDist(iMode: Integer; sValEdit: string);
var
  iLen: Integer;
  dValEdit: Double;
  dVal: Double;
  bIsAlphaNum: Boolean;
begin
  if(mbCalcDistInOperation) then
    exit;

  mbCalcDistInOperation := true;

  iLen := Length(sValEdit);
  bIsAlphaNum := true;

  // Initialisation of all input fields
  if(Trim(sValEdit) = '') then
  begin
    ED__AM_MPC.Text := '';
    ED__AM_PC.Text := '';
    ED__AM_LY.Text := '';
    ED__AM_LHH.Text := '';
    ED__AM_LMM.Text := '';
    ED__AM_LSS.Text := '';
    ED__AM_AU.Text := '';
    ED__AM_KM.Text := '';
  end;

  if(Pos(',',sValEdit) = iLen) then
  begin
    mbCalcDistInOperation := false;
    exit;
  end;

  if(Pos('.',sValEdit) = iLen) then
  begin
    mbCalcDistInOperation := false;
    exit;
  end;

  dValEdit := StrToFloatExt3(sValEdit,bIsAlphaNum);
  if(bIsAlphaNum) then
  begin
    mbCalcDistInOperation := false;
    exit;
  end;

  case iMode of
    0: // Mega Parsec edited
    begin
      dVal := dValEdit * 1000000;
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal:= dValEdit * 3.26 * 1000000;
      ED__AM_LY.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24) * 1000000;
      ED__AM_LHH.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24*60) * 1000000;
      ED__AM_LMM.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24*3600) * 1000000;
      ED__AM_LSS.Text:=FloatToStr(dVal);

      dVal := 360*60*60*dValEdit/(2*Pi) * 1000000;
      ED__AM_AU.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24*3600) * 300000 * 1000000;
      ED__AM_KM.Text:=FloatToStr(dVal);

    end;
    1: // Parsec edited
    begin
      dVal := dValEdit / 1000000;
      ED__AM_MPC.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26;
      ED__AM_LY.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24);
      ED__AM_LHH.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24*60);
      ED__AM_LMM.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24*3600);
      ED__AM_LSS.Text:=FloatToStr(dVal);

      dVal := 360*60*60*dValEdit/(2*Pi);
      ED__AM_AU.Text:=FloatToStr(dVal);

      dVal := dValEdit * 3.26 * (365*24*3600) * 300000;
      ED__AM_KM.Text:=FloatToStr(dVal);

    end;
    2: // Lightyear edited
    begin
      dVal := dValEdit / 3.26 / 1000000;
      ED__AM_MPC.Text:=FloatToStr(dVal);

      dVal := dValEdit / 3.26;
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal := dValEdit * (365*24);
      ED__AM_LHH.Text:=FloatToStr(dVal);

      dVal := dValEdit * (365*24*60);
      ED__AM_LMM.Text:=FloatToStr(dVal);

      dVal := dValEdit * (365*24*3600);
      ED__AM_LSS.Text:=FloatToStr(dVal);

      dVal := 360*60*60/3.26*dValEdit/(2*Pi);
      ED__AM_AU.Text:=FloatToStr(dVal);

      dVal := dValEdit * (365*24*3600) * 300000;
      ED__AM_KM.Text:=FloatToStr(dVal);

    end;

    3: // Lighthours edited
    begin
      dVal := dValEdit / 3.26 / (365*24) / 1000000;
      ED__AM_MPC.Text:=FloatToStr(dVal);

      dVal := dValEdit / 3.26 / (365*24);
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal := dValEdit / (365*24);
      ED__AM_LY.Text:=FloatToStr(dVal);

      dVal := dValEdit * (60);
      ED__AM_LMM.Text:=FloatToStr(dVal);

      dVal := dValEdit * (3600);
      ED__AM_LSS.Text:=FloatToStr(dVal);

      dVal := 360*60*60/(3.26*365*24)*dValEdit/(2*Pi);
      ED__AM_AU.Text:=FloatToStr(dVal);

      dVal:= dValEdit * (3600) * 300000;
      ED__AM_KM.Text:=FloatToStr(dVal);

    end;
    4: // Lightminutes edited
    begin
      dVal := dValEdit / 3.26 / (365*24*60) / 1000000;
      ED__AM_MPC.Text:=FloatToStr(dVal);

      dVal := dValEdit / 3.26 / (365*24*60);
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal := dValEdit / (365*24*60);
      ED__AM_LY.Text:=FloatToStr(dVal);

      dVal := dValEdit / 60;
      ED__AM_LHH.Text:=FloatToStr(dVal);

      dVal := dValEdit * (60);
      ED__AM_LSS.Text:=FloatToStr(dVal);

      dVal := 360*60*60/(3.26*365*24*60)*dValEdit/(2*Pi);
      ED__AM_AU.Text:=FloatToStr(dVal);

      dVal := dValEdit * (60) * 300000;
      ED__AM_KM.Text:=FloatToStr(dVal);

    end;
    5: // Lightseconds edited
    begin
      dVal := dValEdit / 3.26 / (365*24*60*60) / 1000000;
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal := dValEdit / 3.26 / (365*24*60*60);
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal := dValEdit / (365*24*60*60);
      ED__AM_LY.Text:=FloatToStr(dVal);

      dVal := dValEdit / (60 * 60);
      ED__AM_LHH.Text:=FloatToStr(dVal);

      dVal := dValEdit / 60;
      ED__AM_LMM.Text:=FloatToStr(dVal);

      dVal := 360*60*60/(3.26*365*24*60*60)*dValEdit/(2*Pi);
      ED__AM_AU.Text:=FloatToStr(dVal);

      dVal := dValEdit * 300000;
      ED__AM_KM.Text:=FloatToStr(dVal);

    end;
    6: // AU edited (calulatd in Ls)
    begin
      dValEdit := dValEdit * 499.004784;

      dVal := dValEdit / 3.26 / (365*24*60*60) / 1000000;
      ED__AM_MPC.Text:=FloatToStr(dVal);

      dVal := dValEdit / 3.26 / (365*24*60*60);
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal := dValEdit / (365*24*60*60);
      ED__AM_LY.Text:=FloatToStr(dVal);

      dVal := dValEdit / (60*60);
      ED__AM_LHH.Text:=FloatToStr(dVal);

      dVal := dValEdit / 60;
      ED__AM_LMM.Text:=FloatToStr(dVal);

      dVal := dValEdit;
      ED__AM_LSS.Text:=FloatToStr(dVal);

      dVal := dValEdit * 300000;
      ED__AM_KM.Text:=FloatToStr(dVal);

    end;
    7: // km edited (calulatd in Ls, except AU)
    begin
      dValEdit := dValEdit * 0.00000333564;

      dVal := dValEdit / 3.26 / (365*24*60*60) / 1000000;
      ED__AM_MPC.Text:=FloatToStr(dVal);

      dVal := dValEdit / 3.26 / (365*24*60*60);
      ED__AM_PC.Text:=FloatToStr(dVal);

      dVal := dValEdit / (365*24*60*60);
      ED__AM_LY.Text:=FloatToStr(dVal);

      dVal := dValEdit / (60*60);
      ED__AM_LHH.Text:=FloatToStr(dVal);

      dVal := dValEdit / 60;
      ED__AM_LMM.Text:=FloatToStr(dVal);

      dVal := dValEdit;
      ED__AM_LSS.Text:=FloatToStr(dVal);

      dVal := StrToFloat(sValEdit) / 149597870.7;
      ED__AM_AU.Text:=FloatToStr(dVal);

    end;
  end;

  mbCalcDistInOperation := false;

end;

function  TF__ASTROCALC.GetCnt(iMax,iMin,iVal: Integer; bUp: Boolean): Integer;
begin
  Result := iVal;
  if(iVal <= iMin) and (not bUp) then exit;
  if(iVal >= iMax) and (bUp) then exit;

  if(bUp) then
    Result := iVal + 1
  else
    Result := iVal - 1;

end;

(*
0;C/2018 Y1 (Iwamoto);C/2018 Y1 (Iwamoto);
*)

procedure TF__ASTROCALC.B__DT_TO_DECClick(Sender: TObject);
var
  iYear: Integer;
  fYearDays, fDays: Real;
begin
  iYear := YearOf(CB__CONV_DATE.Date);
  fYearDays := EncodeDate(iYear+1, 1, 1) - EncodeDate(iYear, 1, 1);
  //ShowMessage(FloatToStr(CB__CONV_DATE.Date));
  fDays := CB__CONV_DATE.Date + CB__CONV_TIME.Time - EncodeDate(iYear, 1, 1);

  ED__DT_TO_DEC.Text := FloatToStr(
    iYear + 1.0*fDays/fYearDays
    );
end;

procedure TF__ASTROCALC.B__ANGLE_CALCClick(Sender: TObject);
var
  fDist, fSize, rAngle: Real;
  bIsAlphaNum: Boolean;
begin
  bIsAlphaNum := true;

  fDist := StrToFloatExt3(ED__ANGLE_DIST.Text,bIsAlphaNum);
  if(bIsAlphaNum) or (fDist = 0) then
    exit;

  fSize := StrToFloatExt3(ED__ANGLE_SIZE.Text,bIsAlphaNum);

  if(bIsAlphaNum) then
    exit;

  rAngle := arctan(fSize/fDist);
  //rAngle := 2*rAngle;
  rAngle := rAngle * 180.0/Pi;

  //ED__DEG.SetFocus;
  ED__DEG2.Text := FloatToStrF(rAngle,ffFixed,9,6);

end;

procedure TF__ASTROCALC.B__CAMERAINFOClick(Sender: TObject);
begin
  CameraDetails();
end;

procedure TF__ASTROCALC.B__LAEClick(Sender: TObject);
var
  fAOP, fLOAN, fLAE: Real;
  sAOP, sLOAN: string;
begin
  sAOP := AnsiReplaceStr(ED__AOP.Text,',',DefaultFormatSettings.DecimalSeparator);
  sAOP := AnsiReplaceStr(sAOP,'.',DefaultFormatSettings.DecimalSeparator);

  sLOAN := AnsiReplaceStr(ED__LOAN.Text,',',DefaultFormatSettings.DecimalSeparator);
  sLOAN := AnsiReplaceStr(sLOAN,'.',DefaultFormatSettings.DecimalSeparator);

  fAOP := StrToFloat(sAOP);
  fLOAN := StrToFloat(sLOAN);

  fLAE := fAOP + fLOAN;
  if(fLAE >= 360) then
    fLAE := fLAE - 360;

  ED__LAE.Text := FloatToStrF(fLAE,ffFixed,9,6);

end;

procedure TF__ASTROCALC.B__NEWCAMClick(Sender: TObject);
begin
  AddCamera();
end;

procedure TF__ASTROCALC.B__PXC_CALCClick(Sender: TObject);
begin
  CalcObjAngle();
  CalcObjPX();
end;

procedure TF__ASTROCALC.CBX__DBELChange(Sender: TObject);
begin
  CalcA0_SNR();
  Out_SensorDyn();
end;

procedure TF__ASTROCALC.CBX__MANUAL_SENSORSELChange(Sender: TObject);
begin
  if (CBX__MANUAL_SENSORSEL.Checked) then
  begin
    CB__PXC_MANUF.Text:='';
    CB__PXC_MODEL.Text:='';
    CB__PXC_SFORMAT.DroppedDown:=true;
    B__CAMERAINFO.Enabled := false;
  end
  else
  begin
    CB__PXC_SFORMAT.DroppedDown:=false;
  end;

end;

procedure TF__ASTROCALC.GetNikonModels();
begin
  CB__PXC_MODEL.Items.Add('D5');
  CB__PXC_MODEL.Items.Add('D4');
  CB__PXC_MODEL.Items.Add('D4s');
  CB__PXC_MODEL.Items.Add('D3s');
  CB__PXC_MODEL.Items.Add('D3');
  CB__PXC_MODEL.Items.Add('D3X');
  CB__PXC_MODEL.Items.Add('D2Xs');
  CB__PXC_MODEL.Items.Add('D2X');
  CB__PXC_MODEL.Items.Add('D1X');
  CB__PXC_MODEL.Items.Add('D1');
  CB__PXC_MODEL.Items.Add('D2HS');
  CB__PXC_MODEL.Items.Add('D2H');
  CB__PXC_MODEL.Items.Add('D1H');
  CB__PXC_MODEL.Items.Add('D850');
  CB__PXC_MODEL.Items.Add('D810');
  CB__PXC_MODEL.Items.Add('D800');
  CB__PXC_MODEL.Items.Add('D700');
  CB__PXC_MODEL.Items.Add('D750');
  CB__PXC_MODEL.Items.Add('Df');
  CB__PXC_MODEL.Items.Add('D610');
  CB__PXC_MODEL.Items.Add('D600');
  CB__PXC_MODEL.Items.Add('D500');
  CB__PXC_MODEL.Items.Add('D300S');
  CB__PXC_MODEL.Items.Add('D300');
  CB__PXC_MODEL.Items.Add('D200');
  CB__PXC_MODEL.Items.Add('D100');
  CB__PXC_MODEL.Items.Add('D7500');
  CB__PXC_MODEL.Items.Add('D7200');
  CB__PXC_MODEL.Items.Add('D7100');
  CB__PXC_MODEL.Items.Add('D7000');
  CB__PXC_MODEL.Items.Add('D90');
  CB__PXC_MODEL.Items.Add('D80');
  CB__PXC_MODEL.Items.Add('D70s');
  CB__PXC_MODEL.Items.Add('D70');
  CB__PXC_MODEL.Items.Add('D5600');
  CB__PXC_MODEL.Items.Add('D5500');
  CB__PXC_MODEL.Items.Add('D5300');
  CB__PXC_MODEL.Items.Add('D5200');
  CB__PXC_MODEL.Items.Add('D5100');
  CB__PXC_MODEL.Items.Add('D5000');
  CB__PXC_MODEL.Items.Add('D60');
  CB__PXC_MODEL.Items.Add('D40X');
  CB__PXC_MODEL.Items.Add('D50');
  CB__PXC_MODEL.Items.Add('D3400');
  CB__PXC_MODEL.Items.Add('D3300');
  CB__PXC_MODEL.Items.Add('D3200');
  CB__PXC_MODEL.Items.Add('D3100');
  CB__PXC_MODEL.Items.Add('D3000');
  CB__PXC_MODEL.Items.Add('D40');
  CB__PXC_MODEL.Items.Add('D3500');


end;

procedure TF__ASTROCALC.GetOlympusModels();
begin
  CB__PXC_MODEL.Items.Add('E-1');
  CB__PXC_MODEL.Items.Add('E-3');
  CB__PXC_MODEL.Items.Add('E-5');
  CB__PXC_MODEL.Items.Add('E-30');
  CB__PXC_MODEL.Items.Add('E-300');
  CB__PXC_MODEL.Items.Add('E-330');
  CB__PXC_MODEL.Items.Add('E-400');
  CB__PXC_MODEL.Items.Add('E-410');
  CB__PXC_MODEL.Items.Add('E-420');
  CB__PXC_MODEL.Items.Add('E-450');
  CB__PXC_MODEL.Items.Add('E-500');
  CB__PXC_MODEL.Items.Add('E-510');
  CB__PXC_MODEL.Items.Add('E-520');
  CB__PXC_MODEL.Items.Add('E-620');

  (*
  CB__PXC_MODEL.Items.Add('MU-II');

  CB__PXC_MODEL.Items.Add('OM-1');
  CB__PXC_MODEL.Items.Add('OM-2');
  CB__PXC_MODEL.Items.Add('OM-2SP');
  CB__PXC_MODEL.Items.Add('OM-3');
  CB__PXC_MODEL.Items.Add('OM-3 Ti');
  CB__PXC_MODEL.Items.Add('OM-4');
  CB__PXC_MODEL.Items.Add('OM-4 Ti');
  CB__PXC_MODEL.Items.Add('OM-10');
  CB__PXC_MODEL.Items.Add('OM-20');
  CB__PXC_MODEL.Items.Add('OM-30');
  *)
  CB__PXC_MODEL.Items.Add('OM-D E-M1');
  CB__PXC_MODEL.Items.Add('OM-D E-M1 Mark II');
  CB__PXC_MODEL.Items.Add('OM-D E-M5');
  CB__PXC_MODEL.Items.Add('OM-D E-M5 Mark II');

  CB__PXC_MODEL.Items.Add('PEN E-P5');
  CB__PXC_MODEL.Items.Add('XZ-1');

end;

procedure TF__ASTROCALC.GetSonyModels();
begin
  CB__PXC_MODEL.Items.Add('ALPHA 900');
  CB__PXC_MODEL.Items.Add('ALPHA 850');
  CB__PXC_MODEL.Items.Add('ALPHA 99');
  CB__PXC_MODEL.Items.Add('ALPHA 99 II');
  CB__PXC_MODEL.Items.Add('ALPHA 100');
  CB__PXC_MODEL.Items.Add('ALPHA 700');
  CB__PXC_MODEL.Items.Add('ALPHA 200');
  CB__PXC_MODEL.Items.Add('ALPHA 300');
  CB__PXC_MODEL.Items.Add('ALPHA 350');
  CB__PXC_MODEL.Items.Add('ALPHA 230');
  CB__PXC_MODEL.Items.Add('ALPHA 330');
  CB__PXC_MODEL.Items.Add('ALPHA 380');
  CB__PXC_MODEL.Items.Add('ALPHA 500');
  CB__PXC_MODEL.Items.Add('ALPHA 550');
  CB__PXC_MODEL.Items.Add('ALPHA 450');
  CB__PXC_MODEL.Items.Add('ALPHA 290');
  CB__PXC_MODEL.Items.Add('ALPHA 390');
  CB__PXC_MODEL.Items.Add('ALPHA 560');
  CB__PXC_MODEL.Items.Add('ALPHA 580');
  CB__PXC_MODEL.Items.Add('ALPHA 33');
  CB__PXC_MODEL.Items.Add('ALPHA 55');
  CB__PXC_MODEL.Items.Add('ALPHA 35');
  CB__PXC_MODEL.Items.Add('ALPHA 65');
  CB__PXC_MODEL.Items.Add('ALPHA 77');
  CB__PXC_MODEL.Items.Add('ALPHA 57');
  CB__PXC_MODEL.Items.Add('ALPHA 37');
  CB__PXC_MODEL.Items.Add('ALPHA 58');
  CB__PXC_MODEL.Items.Add('ALPHA 77 II');
  CB__PXC_MODEL.Items.Add('ALPHA 68');

end;

procedure TF__ASTROCALC.GetSBIGModels();
begin
  CB__PXC_MODEL.Items.Add('ST-9E');
end;

procedure TF__ASTROCALC.GetSBIGModelData(sModel: string; Camera: TCamera);
begin
  if(sModel = 'ST-9E') then
  begin
    Camera.sSensorType := 'CCD';
    Camera.iSensorFormatIndex := ciMIndex_SBIG1;
    Camera.fMegaPixel := 0.262144;
    Camera.sSensorName:='KAF-0261e';
    Camera.iSensorYear:=2002;
    Camera.iFWC:=200000;
    Camera.fDCRC:=550;
    Camera.fDCRT:=25;
    Camera.fDCHT:=6.3;
    Camera.iQEff:=56;
  end;
end;

procedure TF__ASTROCALC.GetCanonModels();
begin
  // Vollformatsensor
  CB__PXC_MODEL.Items.Add('1Ds');
  CB__PXC_MODEL.Items.Add('1Ds Mark II');
  CB__PXC_MODEL.Items.Add('1Ds Mark III');
  CB__PXC_MODEL.Items.Add('1D X');
  CB__PXC_MODEL.Items.Add('1D C');
  CB__PXC_MODEL.Items.Add('1D XII');
  CB__PXC_MODEL.Items.Add('5D');
  CB__PXC_MODEL.Items.Add('5D Mark II');
  CB__PXC_MODEL.Items.Add('5D Mark III');
  CB__PXC_MODEL.Items.Add('5Ds');
  CB__PXC_MODEL.Items.Add('5Ds R');
  CB__PXC_MODEL.Items.Add('5D Mark IV');
  CB__PXC_MODEL.Items.Add('6D');
  CB__PXC_MODEL.Items.Add('6D Mark II');

  // APS-H Sensor
  CB__PXC_MODEL.Items.Add('1D');
  CB__PXC_MODEL.Items.Add('1D Mark II');
  CB__PXC_MODEL.Items.Add('1D Mark II N');
  CB__PXC_MODEL.Items.Add('1D Mark III');
  CB__PXC_MODEL.Items.Add('1D Mark IV');


  // APS-C Sensor
  CB__PXC_MODEL.Items.Add('7D');
  CB__PXC_MODEL.Items.Add('7D Mark II');
  CB__PXC_MODEL.Items.Add('D30');
  CB__PXC_MODEL.Items.Add('D60' );
  CB__PXC_MODEL.Items.Add('10D');
  CB__PXC_MODEL.Items.Add('20D/20Da');
  CB__PXC_MODEL.Items.Add('30D');
  CB__PXC_MODEL.Items.Add('40D');
  CB__PXC_MODEL.Items.Add('50D');
  CB__PXC_MODEL.Items.Add('60D/60Da');
  CB__PXC_MODEL.Items.Add('70D');
  CB__PXC_MODEL.Items.Add('80D');
  CB__PXC_MODEL.Items.Add('77D');
  CB__PXC_MODEL.Items.Add('100D');
  CB__PXC_MODEL.Items.Add('200D');
  CB__PXC_MODEL.Items.Add('300D');
  CB__PXC_MODEL.Items.Add('300D');
  CB__PXC_MODEL.Items.Add('350D');
  CB__PXC_MODEL.Items.Add('400D');
  CB__PXC_MODEL.Items.Add('450D');
  CB__PXC_MODEL.Items.Add('500D');
  CB__PXC_MODEL.Items.Add('550D');
  CB__PXC_MODEL.Items.Add('600D');
  CB__PXC_MODEL.Items.Add('650D');
  CB__PXC_MODEL.Items.Add('700D');
  CB__PXC_MODEL.Items.Add('750D/760D');
  CB__PXC_MODEL.Items.Add('800D');
  CB__PXC_MODEL.Items.Add('1000D');
  CB__PXC_MODEL.Items.Add('1100D');
  CB__PXC_MODEL.Items.Add('1200D');
  CB__PXC_MODEL.Items.Add('1300D');
  CB__PXC_MODEL.Items.Add('2000D');
  CB__PXC_MODEL.Items.Add('4000D');

  // Canon-Vertrieb
  CB__PXC_MODEL.Items.Add('DCS 1');
  CB__PXC_MODEL.Items.Add('DCS 3');
  CB__PXC_MODEL.Items.Add('D2000');
  CB__PXC_MODEL.Items.Add('D6000');

end;

procedure TF__ASTROCALC.CB__PXC_MANUFChange(Sender: TObject);
begin
  GetCameraModels();
end;

procedure TF__ASTROCALC.GetSonyModelData(sModel: string; Camera: TCamera);
begin
  if(Trim(sModel) = '') then
    exit;

  if(sModel = 'ALPHA 900') then begin Camera.iSensorFormatIndex := ciMIndex_KLEINBILD; Camera.fMegaPixel := 24.6; end;
  if(sModel = 'ALPHA 850') then begin Camera.iSensorFormatIndex := ciMIndex_KLEINBILD; Camera.fMegaPixel := 24.6; end;
  if(sModel = 'ALPHA 99') then begin Camera.iSensorFormatIndex := ciMIndex_KLEINBILD; Camera.fMegaPixel := 24.3; end;
  if(sModel = 'ALPHA 99 II') then begin Camera.iSensorFormatIndex := ciMIndex_KLEINBILD; Camera.fMegaPixel := 42.4; end;
  if(sModel = 'ALPHA 100') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 10.2; end;
  if(sModel = 'ALPHA 700') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 12.4; end;
  if(sModel = 'ALPHA 200') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 10.2; end;
  if(sModel = 'ALPHA 300') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 10.2; end;
  if(sModel = 'ALPHA 350') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14.2; end;
  if(sModel = 'ALPHA 230') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 10.2; end;
  if(sModel = 'ALPHA 330') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 10.2; end;
  if(sModel = 'ALPHA 380') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14.2; end;
  if(sModel = 'ALPHA 500') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 12.3; end;
  if(sModel = 'ALPHA 550') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14.2; end;
  if(sModel = 'ALPHA 450') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14.2; end;
  if(sModel = 'ALPHA 290') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14.0; end;
  if(sModel = 'ALPHA 390') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14.2; end;
  if(sModel = 'ALPHA 560') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14.2; end;
  if(sModel = 'ALPHA 580') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 16.2; end;
  if(sModel = 'ALPHA 33') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 14; end;
  if(sModel = 'ALPHA 55') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 16.2; end;
  if(sModel = 'ALPHA 35') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 16.2; end;
  if(sModel = 'ALPHA 65') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 24.3; end;
  if(sModel = 'ALPHA 77') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 24.3; end;
  if(sModel = 'ALPHA 57') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 16.1; end;
  if(sModel = 'ALPHA 37') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 16.1; end;
  if(sModel = 'ALPHA 58') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 20; end;
  if(sModel = 'ALPHA 77 II') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 24; end;
  if(sModel = 'ALPHA 68') then begin Camera.iSensorFormatIndex := ciMIndex_APSC; Camera.fMegaPixel := 24; end;

end;

procedure TF__ASTROCALC.GetNikonModelData(sModel: string; Camera: TCamera);
begin
  if(Trim(sModel) = '') then
    exit;

  if(sModel ='D5') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 20.8; end;
  if(sModel ='D4') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 16.2; end;
  if(sModel ='D4s') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 16.2; end;
  if(sModel ='D3s') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 12.1; end;
  if(sModel ='D3') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 12.1; Camera.iQEff:=40; Camera.iFWC:=66000; Camera.iRN:=5; end;
  if(sModel ='D3X') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 24.5; end;
  if(sModel ='D2Xs') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 12.4; end;
  if(sModel ='D2X') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 12.4; end;
  if(sModel ='D1X') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 5.3; end;
  if(sModel ='D1') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 2.66; end;
  if(sModel ='D2HS') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 4.1; end;
  if(sModel ='D2H') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 4.1; end;
  if(sModel ='D1H') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 2.7; end;
  if(sModel ='D850') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 45.7; end;
  if(sModel ='D810') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 36.3; end;
  if(sModel ='D800') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 36.3; end;
  if(sModel ='D700') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 12.1; end;
  if(sModel ='D750') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 24.3; end;
  if(sModel ='Df') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 16.2; end;
  if(sModel ='D610') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 24.3; end;
  if(sModel ='D600') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_FX; Camera.fMegaPixel := 24.3; end;
  if(sModel ='D500') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 20.9; end;
  if(sModel ='D300S') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 12.3; end;
  if(sModel ='D300') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 12.3; Camera.iFWC:=42000; Camera.iRN:=4; end;
  if(sModel ='D200') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 10.2; Camera.iFWC:=32000; Camera.iRN:=7;end;
  if(sModel ='D100') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 6.1; end;
  if(sModel ='D7500') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 20.6; end;
  if(sModel ='D7200') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.2; end;
  if(sModel ='D7100') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.1; end;
  if(sModel ='D7000') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 16.2; end;
  if(sModel ='D90') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 12.3; Camera.iQEff:=38; end; // CCD!
  if(sModel ='D80') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 10.2; Camera.iQEff:=38; end; // CCD!
  if(sModel ='D70s') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 6.1; Camera.iQEff:=38; end; // CCD!
  if(sModel ='D70') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 6.1; Camera.iQEff:=38; Camera.iFWC:=25000; Camera.iRN:=7; end; // CCD!
  if(sModel ='D5600') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.2; end;
  if(sModel ='D5500') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.2; end;
  if(sModel ='D5300') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.1; end;
  if(sModel ='D5200') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.1; end;
  if(sModel ='D5100') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 16.2; end;
  if(sModel ='D5000') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 12.3; end;
  if(sModel ='D60') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 10.2; Camera.iQEff:=38; end; // CCD!
  if(sModel ='D40X') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 10.2; Camera.iQEff:=38; end; // CCD!
  if(sModel ='D50') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 6.1; Camera.iQEff:=38; Camera.iFWC:=30000; Camera.iRN:=6; end; // CCD!
  if(sModel ='D3400') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.2; end;
  if(sModel ='D3300') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.2; end;
  if(sModel ='D3200') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.2; end;
  if(sModel ='D3100') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 14.2; end;
  if(sModel ='D3000') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 10.2; end;
  if(sModel ='D40') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 6.1; Camera.iQEff:=38; end; // CCD!
  if(sModel ='D3500') then begin Camera.iSensorFormatIndex := ciMIndex_NIKON_DX; Camera.fMegaPixel := 24.2; end;

end;

procedure TF__ASTROCALC.GetOlympusModelData(sModel: string; Camera: TCamera);
begin
  if(Trim(sModel) = '') then
    exit;

  if(sModel ='E-1') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 5; end;
  if(sModel ='E-3') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 10.1; end;
  if(sModel ='E-5') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 12.3; end;
  if(sModel ='E-30') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 12.3; end;
  if(sModel ='E-300') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 8; end;
  if(sModel ='E-330') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 7.5; end;
  if(sModel ='E-400') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 10; end;
  if(sModel ='E-410') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 10; end;
  if(sModel ='E-420') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 10; end;
  if(sModel ='E-450') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 10; end;
  if(sModel ='E-500') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 8; end;
  if(sModel ='E-510') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 10; end;
  if(sModel ='E-520') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 10; end;
  if(sModel ='E-620') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 12.3; end;
  if(sModel ='OM-D E-M1') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 16.3; end;
  if(sModel ='OM-D E-M1 Mark II') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 20; end;
  if(sModel ='OM-D E-M5') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 16; end;
  if(sModel ='OM-D E-M1 Mark II') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 16; end;
  if(sModel ='PEN E-P5') then begin Camera.iSensorFormatIndex := ciMIndex_FOURTHIRDS; Camera.fMegaPixel := 16.3; end;
  if(sModel ='XZ-1') then begin Camera.iSensorFormatIndex := ciMIndex_1_63; Camera.fMegaPixel := 10; end;

end;

procedure TF__ASTROCALC.GetCanonModelData(sModel: string; Camera: TCamera);//var iFIndex: Integer; var fMPValue: Real);
begin
  if(Trim(sModel) = '') then
    exit;

  // Canon defaults
  Camera.sSensorName:='DIGIC';

  if(sModel = '1Ds') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=11.1;
  end;
  if(sModel = '1Ds Mark II') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=16.7;
  end;
  if(sModel = '1Ds Mark III') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=21.1;
    Camera.iQEff:=30;
  end;
  if(sModel = '1D X') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=18.1;
    Camera.iFWC:=88000;
    Camera.sSensorName:='DIGIC 5+';
    Camera.iSensorYear:=2011;
    Camera.fDCRC:=10;
    Camera.iEstimated:=0;
  end;
  if(sModel = '1D C') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=18.1;
  end;
  if(sModel = '1D X Mark II') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=20.2;
  end;
  if(sModel = '5D Mark II') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=21.1;
    Camera.iQEff:=32;
    Camera.iFWC:=66000;
    Camera.iRN:=3;
    Camera.iEstimated:=0;
  end;
  if(sModel = '5D Mark III') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=22.3;
    Camera.iQEff:=32;
    Camera.iFWC:=69000;
    Camera.sSensorName:='DIGIC 5+';
    Camera.iSensorYear:=2011;
    Camera.iEstimated:=0;
  end;
  if(sModel = '5DS') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=50.6;
    Camera.iFWC:=80000;
    Camera.iRN:=4;
    Camera.iEstimated:=0;
  end;
  if(sModel = '5DS R') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=50.6;
    Camera.iFWC:=80000;
    Camera.iRN:=4;
    Camera.iEstimated:=0;
  end;
  if(sModel = '5D Mark IV') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=30.4;
    Camera.sSensorName:='DIGIC 6+';
    Camera.iSensorYear:=2016;
  end;
  if(sModel = '6D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=20;
    Camera.iFWC:=80000;
    Camera.sSensorName:='DIGIC 5+';
    Camera.iSensorYear:=2011;
    Camera.fDCRC:=2;
    Camera.iEstimated:=0;
  end;
  if(sModel = '6D Mark II') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=26.2;
  end;

  if(sModel = '1D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSH;
    Camera.fMegaPixel :=4.15;
  end;
  if(sModel = '1D Mark II') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSH;
    Camera.fMegaPixel :=8.2;
    Camera.iFWC:=80000;
    Camera.iRN:=4;
    Camera.fDCRC:=7;
    Camera.iEstimated:=0;
  end;
  if(sModel = '1DX Mark II') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_KLEINBILD;
    Camera.fMegaPixel :=20.1;
    Camera.iFWC:=80000;
    Camera.iRN:=4;
    Camera.sSensorName:='DIGIC X';
    Camera.iSensorYear:=2020;
    Camera.iEstimated:=0;
  end;
  if(sModel = '1D Mark II N') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSH;
    Camera.fMegaPixel :=8.2;
  end;
  if(sModel = '1D Mark III') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSH;
    Camera.fMegaPixel :=10.1;
    Camera.iFWC:=70000;
    Camera.iRN:=4;
    Camera.iEstimated:=0;
  end;
  if(sModel = '1D Mark IV') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSH;
    Camera.fMegaPixel :=16.1;
    Camera.iFWC:=56000;
    Camera.iRN:=2;
    Camera.fDCRC:=2;
    Camera.iEstimated:=0;
  end;

  if(sModel = '7D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
    Camera.iQEff:=36;
    Camera.iFWC:=25000;
    Camera.iRN:=3;
    Camera.fDCRC:=2;
    Camera.iEstimated:=0;
  end;
  if(sModel = '7D Mark II') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=20.2;
    Camera.sSensorName:='DIGIC 6';
    Camera.iSensorYear:=2013;
    Camera.fDCRC:=0.2;
    Camera.iEstimated:=0;
  end;
  if(sModel = 'D30') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=3.1;
  end;
  if(sModel = 'D60') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=6.3;
  end;
  if(sModel = '10D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=6.3;
    Camera.iQEff:=24;
    Camera.iFWC:=44000;
    Camera.iRN:=10;
    Camera.iEstimated:=0;
  end;
  if(sModel = '20D/20Da') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=8.2;
    Camera.iQEff:=30;
    Camera.iFWC:=52000;
    Camera.iRN:=4;
    Camera.iEstimated:=0;
  end;
  if(sModel = '30D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=8.2;
  end;
  if(sModel = '40D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=10.1;
    Camera.iQEff:=30;
    Camera.iFWC:=44000;
    Camera.iRN:=4;
    Camera.iEstimated:=0;
  end;
  if(sModel = '50D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=15.1;
    Camera.iQEff:=36;
    Camera.iFWC:=26000;
    Camera.iRN:=3;
    Camera.sSensorName:='DIGIC 4';
    Camera.iSensorYear:=2008;
    Camera.iEstimated:=0;
  end;
  if(sModel = '60D/60Da') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=17.9;
  end;
  if(sModel = '70D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=20.2;
    Camera.sSensorName:='DIGIC 5+';
    Camera.iSensorYear:=2011;
    Camera.iEstimated:=0;
  end;
  if(sModel = '80D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.2;
  end;
  if(sModel = '77D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.2;
  end;
  if(sModel = '100D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;
  if(sModel = '200D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24;
  end;
  if(sModel = '250D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.1;
    Camera.sSensorName:='DIGIC 8';
  end;
  if(sModel = '300D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=6.3;
  end;
  if(sModel = '350D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=8;
    Camera.iQEff := 27;
    Camera.iFWC:=43000;
    Camera.iRN:=4;
    Camera.iEstimated:=0;
  end;
  if(sModel = '400D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=10.1;
  end;
  if(sModel = '450D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=12.2;
    Camera.iQEff:=30;
  end;
  if(sModel = '500D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=15.1;
    Camera.iQEff:=36;
  end;
  if(sModel = '550D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;
  if(sModel = '600D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;
  if(sModel = '650D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;
  if(sModel = '700D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;
  if(sModel = '750D/760D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.2;
  end;
  if(sModel = '800D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.2;
  end;
  if(sModel = '1000D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=10.1;
    Camera.iQEff:=32;
    Camera.iFWC:=50000;
    Camera.sSensorName:='DIGIC III';
    Camera.iSensorYear := 2006;
    Camera.iEstimated:=0;
  end;
  if(sModel = '1100D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=12.2;
  end;
  if(sModel = '1200D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;
  if(sModel = '1300D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;
  if(sModel = '2000D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.1;
  end;
  if(sModel = '4000D') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=18;
  end;

  if(sModel = 'DCS 3') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_CANON_DCS3;
    Camera.fMegaPixel :=1.3;
  end;
  if(sModel = 'DCS 1') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_CANON_DCS1;
    Camera.fMegaPixel :=6;
  end;
  if(sModel = 'D2000') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=2;
  end;
  if(sModel = 'D6000') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSH;
    Camera.fMegaPixel :=6;
  end;
  if(sModel = 'M5') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.2;
    Camera.sSensorName:='DIGIC 7';
    Camera.iSensorYear:=2017;
  end;
  if(sModel = 'M8') then
  begin
    Camera.iSensorFormatIndex := ciMIndex_APSC;
    Camera.fMegaPixel :=24.2;
    Camera.sSensorName:='DIGIC 8';
    Camera.iSensorYear:=2018;
  end;

end;

procedure TF__ASTROCALC.CB__PXC_MODELChange(Sender: TObject);
begin
  ActivateCamera();
end;

function TF__ASTROCALC.CalcPixSize(sSenForm: string; fMPixel: Real): Real;
var
   fX, fY, fPixSize: Real;
begin
  Result := 0.0;

  fX := 0; fY := 0;

  (*
  1/3.2'' (4.5 x 3.4)
  1/2.7'' (5.4 x 4.0)
  1/2.5'' (5.8 x 4.3)
  1/1.8'' (7.2 x 5.4)
  2/3'' (8.8 x 6.6)
  1'' (13.2 x 8.8)
  Four-Thirds (17.3 x 13.0)
  Foveon (20.7 x 13.8)
  APS-C (22.2 x 14.8)
  APS-H (27.9 x 18.6)
  DX (23.7 x 15.6)
  Kleinbild (36 x 24)
  Mittelformat (48 x 36)
  Canon DCS 3 (20.5 x 16.4)
  Canon DCS 1 (28.7 x 19.1)
  Nikon FX (24 x 36)
  Nikon DX (16 x 24)
  1/1.63''
  1.1 (14.4 x 9.9)
  1/2 (7.4 x 5.95)
*)
  if(Trim(sSenForm) = '') then
    exit;

  if(AnsiContainsStr(sSenForm,'1/3.2')) then begin fX := 4.5; fY := 3.4; end;
  if(AnsiContainsStr(sSenForm,'1/2.7')) then begin fX := 5.4; fY := 4.0; end;
  if(AnsiContainsStr(sSenForm,'1/2.5')) then begin fX := 5.8; fY := 4.3; end;
  if(AnsiContainsStr(sSenForm,'1/1.8')) then begin fX := 7.2; fY := 5.4; end;
  if(AnsiContainsStr(sSenForm,'2/3')) then begin fX := 8.8; fY := 6.6; end;
  if(AnsiContainsStr(sSenForm,'1''')) then begin fX := 13.2; fY := 8.8; end;
  if(AnsiContainsStr(sSenForm,'Four-Th')) then begin fX := 17.3; fY := 13.0; end;
  if(AnsiContainsStr(sSenForm, 'Fove')) then begin fX := 20.7; fY := 13.8; end;
  if(AnsiContainsStr(sSenForm,'APS-C')) then begin fX := 22.2; fY := 14.8; end;
  if(AnsiContainsStr(sSenForm,'APS-H')) then begin fX := 27.9; fY := 18.6; end;
  if(AnsiContainsStr(sSenForm,'DX')) then begin fX := 23.7; fY := 15.6; end;
  if(AnsiContainsStr(sSenForm,'Kleinbild, 35mm format')) then begin fX := 36; fY := 24; end;
  if(AnsiContainsStr(sSenForm,'Mittelformat')) then begin fX := 48; fY := 36; end;
  if(AnsiContainsStr(sSenForm,'Canon DCS 3')) then begin fX := 20.5; fY := 16.4; end;
  if(AnsiContainsStr(sSenForm,'Canon DCS 1')) then begin fX := 28.7; fY := 19.1; end;
  if(AnsiContainsStr(sSenForm,'Nikon FX')) then begin fX := 36; fY := 24; end;
  if(AnsiContainsStr(sSenForm,'Nikon DX')) then begin fX := 16; fY := 24; end;
  if(AnsiContainsStr(sSenForm,'1/1.63')) then begin fX := 8; fY := 6; end;
  if(AnsiContainsStr(sSenForm,'1.1')) then begin fX := 14.4; fY := 9.9; end;
  if(AnsiContainsStr(sSenForm,'1/2')) then begin fX := 7.4; fY := 5.95; end;

  if((fX = 0) or (fY = 0)) then
    exit;

  fPixSize := sqrt(fX*fY / (fMPixel * 1000000))*1000;
  Result:=fPixSize;

  //ShowCameraProps();

end;

procedure TF__ASTROCALC.CB__PXC_SFORMATChange(Sender: TObject);
var
  fPixSize: Real;
begin
  fPixSize := CalcPixSize(CB__PXC_SFORMAT.Text,ED__PXC_MP.Value);
  ED__PXC_PX_SIZE.Value:=fPixSize;

  ShowCameraProps();
end;

procedure TF__ASTROCALC.ED__AM_AUChange(Sender: TObject);
begin
  if(ED__AM_AU.Focused) then
    CalcDist(6,ED__AM_AU.Text);
end;

procedure TF__ASTROCALC.ED__AM_KMChange(Sender: TObject);
begin
  if(ED__AM_KM.Focused) then
    CalcDist(7,ED__AM_KM.Text);
end;

procedure TF__ASTROCALC.ED__AM_KMMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //_CatchWarn_ButtonShiftXY(Button, Shift, X, Y);

  PrepareDistEdits();
end;

procedure TF__ASTROCALC.ED__AM_LHHChange(Sender: TObject);
begin
  if(ED__AM_LHH.Focused) then
    CalcDist(3,ED__AM_LHH.Text);
end;

procedure TF__ASTROCALC.ED__AM_LMMChange(Sender: TObject);
begin
  if(ED__AM_LMM.Focused) then
    CalcDist(4,ED__AM_LMM.Text);
end;

procedure TF__ASTROCALC.ED__AM_LSSChange(Sender: TObject);
begin
  if(ED__AM_LSS.Focused) then
    CalcDist(5,ED__AM_LSS.Text);
end;

procedure TF__ASTROCALC.ED__AM_LYChange(Sender: TObject);
begin
  if(ED__AM_LY.Focused) then
    CalcDist(2,ED__AM_LY.Text);
end;

procedure TF__ASTROCALC.ED__AM_MPCChange(Sender: TObject);
begin
  if(ED__AM_PC.Focused) then
    CalcDist(0,ED__AM_PC.Text);
end;

procedure TF__ASTROCALC.ED__AM_PCChange(Sender: TObject);
begin
  if(ED__AM_PC.Focused) then
    CalcDist(1,ED__AM_PC.Text);
end;

procedure TF__ASTROCALC.ED__ANG_DEGChange(Sender: TObject);
begin
  if(ED__ANG_DEG.Focused) then
    CalcDEGMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__ANG_HHChange(Sender: TObject);
begin
  if(ED__ANG_HH.Focused) then
    CalcHHMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__ANG_MM1Change(Sender: TObject);
begin
  if(ED__ANG_MM1.Focused) then
    CalcHHMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__ANG_MMChange(Sender: TObject);
begin
  if(ED__ANG_MM.Focused) then
    CalcDEGMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__ANG_MS1Change(Sender: TObject);
begin
  if(ED__ANG_MS1.Focused) then
    CalcHHMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__ANG_MSChange(Sender: TObject);
begin
  if(ED__ANG_MS.Focused) then
    CalcDEGMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__ANG_SS1Change(Sender: TObject);
begin
  if(ED__ANG_SS1.Focused) then
    CalcHHMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__ANG_SSChange(Sender: TObject);
begin
  if(ED__ANG_SS.Focused) then
    CalcDEGMMSSToDEC();
end;

procedure TF__ASTROCALC.ED__APERTUREChange(Sender: TObject);
var
  iStarDiam: Integer;
begin
  if(ED__APERTURE.Focused) then
  begin
    L__RES_TELESCOPE.Caption := FloatToStrF(138.0/ED__APERTURE.Value,ffFixed,8,1) + ' ''''';
    ED__FRATIO.Value := ED__FOCALLENGTH.Value/ED__APERTURE.Value;
    L__D_AIRY.Caption:= FloatToStrF(1.22*550/1000000000 *1000/ED__APERTURE.Value *180/Pi *3600,ffFixed,8,1) + ' ''''';

    iStarDiam := GetStarDiscDiam();
    L__OPTPX.Caption:= FloatToStrF(iStarDiam/2.0 * ED__FOCALLENGTH.Value/206,ffFixed,8,1) + ' mym';

    ShowCameraProps();

    CalcAllSignalValues();

  end;
end;

procedure TF__ASTROCALC.ED__DEG1Change(Sender: TObject);
var
  rValEdit: Real;
  bIsAlphaNum: Boolean;
begin
  bIsAlphaNum := true;

  if not ED__DEG1.Focused then
    exit;

  CalcToHHMMSS(ED__DEG1.Text,15.0);

  rValEdit := StrToFloatExt3(ED__DEG1.Text,bIsAlphaNum);

  if(bIsAlphaNum) then
    exit;

  ED__HH_DEC.Text := FloatToStr(rValEdit/15.0);
end;

procedure TF__ASTROCALC.ED__DEGChange(Sender: TObject);
var
  rValEdit: Real;
  bIsAlphaNum: Boolean;
  iMS: Integer;
begin
  if not ED__DEG.Focused then
    exit;

  bIsAlphaNum := true;

  rValEdit := StrToFloatExt3(ED__DEG.Text,bIsAlphaNum);

  if(bIsAlphaNum) then
    exit;

  ED__ANG_DEG.Value := Trunc(rValEdit);
  ED__ANG_MM.Value  := Trunc((rValEdit - ED__ANG_DEG.Value)*60);
  ED__ANG_SS.Value  := Trunc(((rValEdit - ED__ANG_DEG.Value)*60 - ED__ANG_MM.Value)*60);
  iMS := Round((((rValEdit - ED__ANG_DEG.Value)*60 - ED__ANG_MM.Value)*60 - ED__ANG_SS.Value)*1000);
  if(iMS = 1000) then
  begin
    iMS := 0;
    ED__ANG_SS.Value := ED__ANG_SS.Value + 1;
  end;
  ED__ANG_MS.Value  := iMS;

end;

procedure TF__ASTROCALC.ED__EXP_CNTChange(Sender: TObject);
begin
  CalcA0_SNR();
end;

procedure TF__ASTROCALC.SetFContColor_EXP_S(iValue: Integer);
begin
  if(iValue < mrBGL) then
  begin
    ED__EXP_S.Font.Color:=clRed;
    L__BGL.Font.Color:=clRed;
    L__BGL_TITLE.Font.Color:=clRed;
    L__50_EXPTIME.Font.Color:=clBlack;
    L__50_EXPTIME_TITLE.Font.Color:=clBlack;
    L__STAR_SATUR.Font.Color:=clBlack;
    L__STAR_SATUR_TITLE.Font.Color:=clBlack;
    if(msLANG_ID = 'DE') then
      P__EXP_WARN.Caption:='Rauschen zu stark'
    else
      P__EXP_WARN.Caption:='Noise too much';

    P__EXP_WARN.Visible:=true;

  end
  else if (iValue > mfExpTimeSec) or (iValue > mfStarSaturSec) then
  begin
    ED__EXP_S.Font.Color:=clRed;
    L__BGL.Font.Color:=clBlack;
    L__BGL_TITLE.Font.Color:=clBlack;

    if((iValue > mfExpTimeSec) and (iValue <= mfStarSaturSec)) then
    begin
      L__50_EXPTIME.Font.Color:=clRed;
      L__50_EXPTIME_TITLE.Font.Color:=clRed;
      L__STAR_SATUR.Font.Color:=clBlack;
      L__STAR_SATUR_TITLE.Font.Color:=clBlack;
      if(msLANG_ID = 'DE') then
        P__EXP_WARN.Caption:='Objekt überbelichtet'
      else
        P__EXP_WARN.Caption:='Object overexposed';

      P__EXP_WARN.Visible:=true;
    end;
    if((iValue <= mfExpTimeSec) and (iValue > mfStarSaturSec)) then
    begin
      L__50_EXPTIME.Font.Color:=clBlack;
      L__50_EXPTIME_TITLE.Font.Color:=clBlack;
      L__STAR_SATUR.Font.Color:=clRed;
      L__STAR_SATUR_TITLE.Font.Color:=clRed;

      if(msLANG_ID = 'DE') then
        P__EXP_WARN.Caption:='Stern überbelichtet'
      else
        P__EXP_WARN.Caption:='Star overexposed';

      P__EXP_WARN.Visible:=true;
    end;
    if((iValue > mfExpTimeSec) and (iValue > mfStarSaturSec)) then
    begin
      L__50_EXPTIME.Font.Color:=clRed;
      L__50_EXPTIME_TITLE.Font.Color:=clRed;
      L__STAR_SATUR.Font.Color:=clRed;
      L__STAR_SATUR_TITLE.Font.Color:=clRed;
      if(msLANG_ID = 'DE') then
        P__EXP_WARN.Caption:='Alles überbelichtet'
      else
        P__EXP_WARN.Caption:='Anything overexposed';

      P__EXP_WARN.Visible:=true;
    end;

  end
  else
  begin
    ED__EXP_S.Font.Color:=clBlack;
    L__BGL.Font.Color:=clBlack;
    L__BGL_TITLE.Font.Color:=clBlack;
    L__50_EXPTIME.Font.Color:=clBlack;
    L__50_EXPTIME_TITLE.Font.Color:=clBlack;
    L__STAR_SATUR.Font.Color:=clBlack;
    L__STAR_SATUR_TITLE.Font.Color:=clBlack;
    P__EXP_WARN.Caption:='';
    P__EXP_WARN.Visible:=false;
  end;
end;

procedure TF__ASTROCALC.ED__EXP_SChange(Sender: TObject);
begin
  CalcA0_SNR();
  Out_SensorDyn();

  //mBGL, mfExpTimeSec, mfStarSaturSec
  SetFContColor_EXP_S(ED__EXP_S.Value);
end;

procedure TF__ASTROCALC.ED__FOCALLENGTHChange(Sender: TObject);
var
  iStarDiam: Integer;
begin
  ED__FRATIO.Value := ED__FOCALLENGTH.Value/ED__APERTURE.Value;

  iStarDiam := GetStarDiscDiam();
  L__OPTPX.Caption:= FloatToStrF(iStarDiam/2.0 * ED__FOCALLENGTH.Value/206,ffFixed,8,1) + ' mym';

  ShowCameraProps();

  CalcObjAngle();
  CalcObjPX();

  CalcAllSignalValues();

end;

procedure TF__ASTROCALC.ED__FRATIOChange(Sender: TObject);
begin
  if(ED__FRATIO.Focused) then
  begin
    ED__APERTURE.Value := ED__FOCALLENGTH.Value/ED__FRATIO.Value;
    L__RES_TELESCOPE.Caption := FloatToStrF(138.0/ED__APERTURE.Value,ffFixed,8,1) + ' ''''';
    L__D_AIRY.Caption:= FloatToStrF(1.22*550/1000000000 *1000/ED__APERTURE.Value *180/Pi *3600,ffFixed,8,1) + ' ''''';

    ShowCameraProps();

    CalcAllSignalValues();

  end;
end;

procedure TF__ASTROCALC.ED__HH_DECChange(Sender: TObject);
var
  rValEdit: Real;
  bIsAlphaNum: Boolean;
begin
  bIsAlphaNum := true;

  if not ED__HH_DEC.Focused then
    exit;

  CalcToHHMMSS(ED__HH_DEC.Text,1.0);

  rValEdit := StrToFloatExt3(ED__HH_DEC.Text,bIsAlphaNum);

  if(bIsAlphaNum) then
    exit;

  ED__DEG1.Text := FloatToStr(rValEdit*15.0);
end;

procedure TF__ASTROCALC.ED__PXC_MPChange(Sender: TObject);
begin
  ShowCameraProps();
end;

procedure TF__ASTROCALC.ED__PXC_PX_SIZEChange(Sender: TObject);
begin
  ShowCameraProps();
end;

procedure TF__ASTROCALC.FormCreate(Sender: TObject);
begin
  mbSetFormDim := false;
  mbCalcDistInOperation := false;

  mCamera := TCamera.Create();

  PC__ASTROCALC.ActivePageIndex:=0;
  PC__APP.ActivePageIndex:=0;

  mrS0 := GetStarBaseSignalCompact(ED__APERTURE.Value,L__TELTYPE.Caption);

  TS__NYQUIST.TabVisible:=true;
  TS__SIGNALS.TabVisible:=true;
  TS__CONV.TabVisible:=true;
  B__NEWCAM.Visible:=true;

  mslCustCameraList := nil;

end;

procedure TF__ASTROCALC.FormDestroy(Sender: TObject);
begin
  mCamera.Destroy;
  mslCustCameraList.Free
end;

procedure TF__ASTROCALC.FormShow(Sender: TObject);
var
  i: Integer;
begin
  mbCamHasAdded := false;

  if(mADevice = nil) then
    IniAstroCalc();

  // Adding user-defined manufacturers
  //mslCustCameraList := TStringList.Create;
  //ImportCameraList(mslCustCameraList,msAlbireoLocalDir + gcsCAMLISTFILE);

  for i:=0 to mslCustCameraList.Count-1 do
  begin
    if(CB__PXC_MANUF.Items.IndexOf((mslCustCameraList.Objects[i] as TCamera).sManufacturer) = -1 ) then
    begin
      CB__PXC_MANUF.Items.AddObject((mslCustCameraList.Objects[i] as TCamera).sManufacturer,mslCustCameraList.Objects[i] as TCamera);
    end;
  end;

  if(msLANG_ID = 'EN') then
  begin
    GBX__EXP_SN_CALC.Caption := 'Exposure && SNR value';
    L__ANGLE_DIST.Caption:='Distance D';
    L__ANGLE_SIZE.Caption:='Size G';

    IMG__ANGLE_DE.Visible:=false;
    IMG__ANGLE_EN.Visible:=true;

    GBX__OPT_FL.Caption := 'Optimal focal lengths';
    L__LIMG_TITLE.Caption := 'Lucky Imaging (short exposures):';
    L__DSIMG_TITLE.Caption := 'For DeepSky with selected seeing (long exposures):';
    P__SETTINGS.Caption:='Telescope & Camera Sensor';
    L__MAG_FP_TITLE.Caption:='Focal Projection Magnification';
  end
  else
  begin
    GBX__OPT_FL.Caption := 'Optimale Brennweiten';
    L__LIMG_TITLE.Caption := 'Lucky Imaging (kleine Bel.-Zeiten):';
    L__DSIMG_TITLE.Caption := 'Für DeepSky bei ausgew. Seeing (große Bel.-Zeiten):';
    P__SETTINGS.Caption:='Teleskop & Kamerasensor';
    L__MAG_FP_TITLE.Caption:='Vergrößerung bei fokaler Projektion';
  end;

  if(mbSetFormDim) then
  begin
    Top := miFormTop + 51;
    Left := miFormLeft + 9;
    Width := miFormWidth - 23;
    Height := miFormHeight - 42;
  end;

end;

procedure TF__ASTROCALC.L__AE_EARTHClick(Sender: TObject);
begin
  ED__AM_AU.SetFocus;
  ED__AM_AU.Text := '1';
  CalcDist(6,'1');
end;

procedure TF__ASTROCALC.L__AE_JUPITERClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '779000000';
  CalcDist(7,'779000000');
end;

procedure TF__ASTROCALC.L__AE_MARSClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '227900000';
  CalcDist(7,'227900000');
end;

procedure TF__ASTROCALC.L__AE_MERCURYClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '57900000';
  CalcDist(7,'57900000');
end;

procedure TF__ASTROCALC.L__AE_NEPTUNClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '4495000000';
  CalcDist(7,'4495000000');
end;

procedure TF__ASTROCALC.L__AE_PLUTOClick(Sender: TObject);
begin
  ED__AM_AU.SetFocus;
  ED__AM_AU.Text := '39,482';
  CalcDist(6,'39,482');
end;

procedure TF__ASTROCALC.L__AE_SATURNClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '1433000000';
  CalcDist(7,'1433000000');
end;

procedure TF__ASTROCALC.L__AE_URANUSClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '2871000000';
  CalcDist(7,'2871000000');
end;

procedure TF__ASTROCALC.L__AE_VENUSClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '108200000';
  CalcDist(7,'108200000');
end;

procedure TF__ASTROCALC.L__ANGLE_JUPITERClick(Sender: TObject);
begin
  ED__ANG_SS.SetFocus;
  ED__ANG_DEG.Value := 0;
  ED__ANG_MS.Value := 0;
  ED__ANG_MM.Value := 0;

  ED__ANG_SS.Value := 47;
end;

procedure TF__ASTROCALC.L__AU_NEWHORIZONSClick(Sender: TObject);
begin
  ED__AM_AU.SetFocus;
  ED__AM_AU.Text := '43.65';
  CalcDist(6,'43.65');
end;

procedure TF__ASTROCALC.L__AU_VOYAGER1Click(Sender: TObject);
begin
  ED__AM_AU.SetFocus;
  ED__AM_AU.Text := '144.61';
  CalcDist(6,'144.61');
end;

procedure TF__ASTROCALC.L__AU_VOYAGER2Click(Sender: TObject);
begin
  ED__AM_AU.SetFocus;
  ED__AM_AU.Text := '119.75';
  CalcDist(6,'119.75');
end;



procedure TF__ASTROCALC.L__LY_ALPACENTAURIClick(Sender: TObject);
begin
  ED__AM_LY.SetFocus;
  ED__AM_LY.Text := '4.34';
  CalcDist(2,'4.34');
end;

procedure TF__ASTROCALC.L__LY_D_M31Click(Sender: TObject);
begin
  ED__AM_LY.SetFocus;
  ED__AM_LY.Text := '2500000';
  CalcDist(2,'2500000');
end;

procedure TF__ASTROCALC.L__LY_D_TWINQClick(Sender: TObject);
begin
  ED__AM_PC.SetFocus;
  ED__AM_PC.Text := '2400000000';
  CalcDist(1,'2400000000');
end;

procedure TF__ASTROCALC.L__LY_D_VIRGOClick(Sender: TObject);
begin
  ED__AM_PC.SetFocus;
  ED__AM_PC.Text := '19938650';
  CalcDist(1,'19938650');
end;

procedure TF__ASTROCALC.L__LY_D_COMAClick(Sender: TObject);
begin
  ED__AM_PC.SetFocus;
  ED__AM_PC.Text := '100000000';
  CalcDist(1,'100000000');
end;

procedure TF__ASTROCALC.L__LY_D_MILKYWAYClick(Sender: TObject);
begin
  ED__AM_LY.SetFocus;
  ED__AM_LY.Text := '180000';
  CalcDist(2,'180000');
end;

procedure TF__ASTROCALC.L__LY_D_VUNIVClick(Sender: TObject);
begin
  ED__AM_PC.SetFocus;
  ED__AM_PC.Text := '3200000000';
  CalcDist(1,'3200000000');
end;

procedure TF__ASTROCALC.L__LY_M42Click(Sender: TObject);
begin
  ED__AM_LY.SetFocus;
  ED__AM_LY.Text := '1350';
  CalcDist(2,'1350');
end;

procedure TF__ASTROCALC.L__MOON_ANGLEClick(Sender: TObject);
begin
  ED__ANG_MM.SetFocus;
  ED__ANG_DEG.Value := 0;
  ED__ANG_SS.Value := 0;
  ED__ANG_MS.Value := 0;
  ED__ANG_MM.Value := 31;
end;

procedure TF__ASTROCALC.L__MOON_DIAMETER_DIAMMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //_CatchWarn_ButtonShiftXY(Button, Shift, X, Y);

  MDownDistLabel(Sender as TLabel);
end;

procedure TF__ASTROCALC.L__MOON_DIAMETER_DIAMMouseEnter(Sender: TObject);
begin
  EnterDistLabel(Sender as TLabel);
end;

procedure TF__ASTROCALC.L__MOON_DIAMETER_DIAMMouseLeave(Sender: TObject);
begin
  LeaveDistLabel(Sender as TLabel);
end;


procedure TF__ASTROCALC.L__SUN_ANGLEClick(Sender: TObject);
begin
  ED__ANG_MM.SetFocus;
  ED__ANG_DEG.Value := 0;
  ED__ANG_SS.Value := 0;
  ED__ANG_MS.Value := 0;
  ED__ANG_MM.Value := 32;
end;

procedure TF__ASTROCALC.L__SUN_DIAMETERClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '1392684';
  CalcDist(7,'1392684');
end;

procedure TF__ASTROCALC.L__EARTH_DIAMETERClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '12735';
  CalcDist(7,'12735');
end;

procedure TF__ASTROCALC.L__MOON_DIAMETERClick(Sender: TObject);
begin
  ED__AM_KM.SetFocus;
  ED__AM_KM.Text := '3476';
  CalcDist(7,'3476');
end;

procedure TF__ASTROCALC.RBG__SEEINGClick(Sender: TObject);
begin
  ShowCameraProps();
end;

procedure TF__ASTROCALC.TB__BG_MAGChange(Sender: TObject);
begin
  Out_BGSignal();
  Out_ExpTime50FWSec();
  Out_StarSaturSec();
  Out_BGL();
  CalcA0_SNR();
end;

procedure TF__ASTROCALC.TB__DC_SIGNALChange(Sender: TObject);
begin
  Out_DCSignal();
  Out_ExpTime50FWSec();
  Out_StarSaturSec();
  Out_BGL();
  CalcA0_SNR();
  Out_SensorDyn();
end;

procedure TF__ASTROCALC.TB__STAR_MAGChange(Sender: TObject);
begin
  Out_StarSignal();
  Out_StarSaturSec();
  Out_BGL();
  CalcA0_SNR();
end;

end.

