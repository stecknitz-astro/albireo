unit U_Prefs;

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, Spin, ExtCtrls, IniFiles, U_Translation, U_ABase, U_ALib, U_AConst,
  StrUtils;

type

  { TF__PREFS }

  TF__PREFS = class(TForm)
    B__GOTO_OUT: TButton;
    B__CANCEL: TButton;
    B__RESET: TButton;
    B__OK: TButton;
    CBX__DST: TCheckBox;
    CBX__HDUST: TCheckBox;
    CBX__LOWHIGHRES: TCheckBox;
    ED__GOTO_OUT: TEdit;
    ED__BIRTH_YEAR: TEdit;
    ED__COUNTRY: TEdit;
    ED__DST_HH: TEdit;
    ED__CITY: TEdit;
    ED__DST: TEdit;
    ED__LTHICKNESS: TSpinEdit;
    ED__UTC_HH: TEdit;
    ED__GLNG_DEG: TEdit;
    ED__GLAT_MIN: TEdit;
    ED__GLAT_DEG: TEdit;
    ED__GLNG_MIN: TEdit;
    GBX__GEO_COORD: TGroupBox;
    GBX__TIMESETTINGS: TGroupBox;
    GBX__LANG: TGroupBox;
    GBX__BIRTH_YEAR: TGroupBox;
    GBX__ANIMATION: TGroupBox;
    GBX__COUNTRIES: TGroupBox;
    GBX__CITY: TGroupBox;
    GBX__GOTO_OUT: TGroupBox;
    GBX__CNST: TGroupBox;
    Image1: TImage;
    IMG__HOR_1: TImage;
    IMG__HOR_0: TImage;
    IMG__DE: TImage;
    IMG__BUTTONS: TImage;
    IMG__EN: TImage;
    L__LTHICKNESS: TLabel;
    L__CNST_COL_RED: TLabel;
    L__CNST_COL_LIME: TLabel;
    L__CNST_COL_BLUE: TLabel;
    L__GOTO_OUT: TLabel;
    L__HORPIC: TLabel;
    LB__CITY: TListBox;
    LB__COUNTRY: TListBox;
    L__CITY: TLabel;
    L__COUNTRY: TLabel;
    L__REFRESHRATE: TLabel;
    L__BIRTH_YEAR: TLabel;
    L__LNG: TLabel;
    L__LINETHICKNESS: TLabel;
    L__UTC_HH: TLabel;
    L__DST_HH: TLabel;
    L__GLAT_MIN: TLabel;
    L__GDEG: TLabel;
    L__GLAT: TLabel;
    RB__CNST_COL_RED: TRadioButton;
    RB__CNST_COL_LIME: TRadioButton;
    RB__CNST_COL_BLUE: TRadioButton;
    RB__HOR_2: TRadioButton;
    RB__HOR_0: TRadioButton;
    RB__HOR_1: TRadioButton;
    RB__LANG_EN: TRadioButton;
    RB__LANG_DE: TRadioButton;
    ED__REFRESHRATE: TSpinEdit;
    SDIRDLG: TSelectDirectoryDialog;
    UD__BIRTH_YEAR: TUpDown;
    procedure B__GOTO_OUTClick(Sender: TObject);
    procedure B__OKClick(Sender: TObject);
    procedure B__RESETClick(Sender: TObject);
    procedure ED__REFRESHRATEChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure LB__CITYDblClick(Sender: TObject);
    procedure LB__COUNTRYDblClick(Sender: TObject);
    procedure UD__BIRTH_YEARChanging(Sender: TObject; var AllowChange: Boolean);
  private
    { private declarations }
    FiGLat_DEG: SmallInt;
    FiGLat_MIN: SmallInt;
    FiGLng_DEG: SmallInt;
    FiGLng_MIN: SmallInt;
    FiDST_HH: SmallInt;
    FiUTC_HH: SmallInt;
    FsLANG_ID: string;
    FbSwitchLANG: Boolean; // Remember, if language switch has occurred
    FiBirthYear: Integer;
    FiRefreshRateMinutes: SmallInt;
    FiFirstStart: SmallInt;
    FsCountry, FsCity, FsDST: string;
    FbHDust: Boolean; // Horizon Dust Effect
    FiLThickness: ShortInt; // Visualisation Line Thickness
    FiLandscapeNo: Integer; // Horizon View
    FsGotoOutputDir: string; // Directory for GoTo control files
    FCColor: TColor;
    FsAlbireoLocalDir: string;

    procedure UseData();
    procedure ShowData();
    procedure SavePrefs();

  public
    { public declarations }
    property msAlbireoLocalDir: string read FsAlbireoLocalDir write FsAlbireoLocalDir;
    property miGLat_DEG: SmallInt read FiGLat_DEG write FiGLat_DEG;
    property miGLat_MIN: SmallInt read FiGLat_MIN write FiGLat_MIN;
    property miGLng_DEG: SmallInt read FiGLng_DEG write FiGLng_DEG;
    property miGLng_MIN: SmallInt read FiGLng_MIN write FiGLng_MIN;
    property miDST_HH: SmallInt read FiDST_HH write FiDST_HH;
    property miUTC_HH: SmallInt read FiUTC_HH write FiUTC_HH;
    property msLANG_ID: string read FsLANG_ID write FsLANG_ID;
    property mbSwitchLANG: Boolean read FbSwitchLANG write FbSwitchLANG; // Remember, if language switch has occurred
    property miBirthYear: Integer read FiBirthYear write FiBirthYear;
    property miRefreshRateMinutes: SmallInt read FiRefreshRateMinutes write FiRefreshRateMinutes;
    property miFirstStart: SmallInt read FiFirstStart write FiFirstStart;
    property msCountry: string read FsCountry write FsCountry;
    property msCity: string read FsCity write FsCity;
    property msDST: string read FsDST write FsDST;
    property mbHDust: Boolean read FbHDust write FbHDust; // Horizon Dust Effect
    property miLThickness: ShortInt read FiLThickness write FiLThickness; // Visualisation Line Thickness
    property miLandscapeNo: Integer read FiLandscapeNo write FiLandscapeNo; // Horizon View
    property msGotoOutputDir: string read FsGotoOutputDir write FsGotoOutputDir; // Directory for GoTo control files
    property mCColor: TColor read FCColor write FCColor;

  end;

var
  F__PREFS: TF__PREFS;

implementation

{$R *.lfm}

procedure TF__PREFS.FormShow(Sender: TObject);
var
  i: Integer;
  slBuffer: TStringList;
  sCityFileName: string;
begin
  IniText(F__PREFS,msLANG_ID);

  if(msLANG_ID = 'DE') then
  begin
    Caption := 'Voreinstellungen';
    sCityFileName := ConvertWinPath(msAlbireoLocalDir + 'CityCoo.txt');
  end
  else
  begin
    Caption := 'Preferences';
    sCityFileName := ConvertWinPath(msAlbireoLocalDir + 'CityCoo_EN.txt');
  end;

  ED__BIRTH_YEAR.Text := IntToStr(gciBirthYear);

  ShowData();

  LB__COUNTRY.Clear;
  LB__CITY.Clear;

  slBuffer := TStringList.Create;
  slBuffer.Sorted := true;

  GetCountryStringList(msLANG_ID,slBuffer,sCityFileName);

  for i:=0 to slBuffer.Count-1 do
    LB__COUNTRY.Items.Add(slBuffer[i]);


  slBuffer.Free;

end;

procedure TF__PREFS.LB__CITYDblClick(Sender: TObject);
var
  iIndex: Integer;
  EarthLocation: TEarthLocation;
begin
  iIndex := LB__CITY.ItemIndex;

  if(iIndex > -1) then
  begin
    EarthLocation := (LB__CITY.Items.Objects[iIndex] as TEarthLocation);
    if(EarthLocation <> nil) then
    begin
      ED__GLAT_DEG.Text := FloatToStr(EarthLocation.rLatitude_DEG);
      ED__GLAT_MIN.Text := FloatToStr(EarthLocation.rLatitude_MIN);
      ED__GLNG_DEG.Text := FloatToStr(EarthLocation.rLongitude_DEG);
      ED__GLNG_MIN.Text := FloatToStr(EarthLocation.rLongitude_MIN);

      ED__UTC_HH.Text := IntToStr(EarthLocation.iTimeZone);

      if(EarthLocation.sLANG_ID = 'DE') then
      begin
        RB__LANG_DE.Checked:=true;
        RB__LANG_EN.Checked:=false;
      end
      else
      begin
        RB__LANG_DE.Checked:=false;
        RB__LANG_EN.Checked:=true;
      end;

      ED__CITY.Text := EarthLocation.sCity;
      ED__COUNTRY.Text := EarthLocation.sCountry;

      CBX__DST.Checked := (Trim(EarthLocation.sDST) <> '');
      ED__DST.Text := EarthLocation.sDST;
    end;
  end;
end;

procedure TF__PREFS.LB__COUNTRYDblClick(Sender: TObject);
var
  i: Integer;
  slCities: TStringList;
  sCountrySel: string;
  sCityCooFile: string;
begin
  slCities := TStringList.Create;

  sCountrySel := LB__COUNTRY.Items[LB__COUNTRY.ItemIndex];

  LB__CITY.Clear;

  if(msLANG_ID = 'DE') then
    sCityCooFile := msAlbireoLocalDir + 'CityCoo.txt'
  else
    sCityCooFile := msAlbireoLocalDir + 'CityCoo_EN.txt';

  GetCitiesList(sCountrySel,msLANG_ID,slCities,sCityCooFile);

  for i:=0 to slCities.Count-1 do
  begin
    LB__CITY.AddItem(slCities[i],slCities.Objects[i]);
  end;

  slCities.Destroy;
end;

procedure TF__PREFS.B__OKClick(Sender: TObject);
begin
  UseData();
  SavePrefs();
end;

procedure TF__PREFS.B__GOTO_OUTClick(Sender: TObject);
var
  sDir: string;
begin
  if(SDIRDLG.Execute and (DirectoryExists(SDIRDLG.FileName))) then
  begin
    sDir := SDIRDLG.FileName;

    {$IFDEF LINUX}
    if (RightStr(sDir,1) <> '/') then
      sDir := sDir + '/';
    {$ENDIF LINUX}
    {$IFDEF Windows}
    if (RightStr(sDir,1) <> '\') then
      sDir := sDir + '\';
    {$ENDIF Windows}

    ED__GOTO_OUT.Text := sDir;
  end;
end;

procedure TF__PREFS.B__RESETClick(Sender: TObject);
begin
  msDST := 'MESZ';
  msCountry := 'Deutschland';
  msCity := 'Frankfurt a. M.';

  miGLat_DEG := 50; // 53
  miGLat_MIN := 7;  // 40
  miGLng_DEG := 8;  // 10
  miGLng_MIN := 41; // 47

  miDST_HH := 1;
  miUTC_HH := 1;

  msLANG_ID := 'DE';

  miBirthYear := 1964;

  miRefreshRateMinutes := 5;

  RB__HOR_0.Checked:=true;
  RB__HOR_1.Checked:=false;
  RB__HOR_2.Checked:=false;

  miLThickness := 2;
  mCColor := clRed;

  {$IFDEF LINUX}
  ED__GOTO_OUT.Text := '~/';
  {$ENDIF LINUX}
  {$IFDEF Windows}
  ED__GOTO_OUT.Text := 'C:\';
  {$ENDIF Windows}

  ShowData();
end;

procedure TF__PREFS.ED__REFRESHRATEChange(Sender: TObject);
begin

end;

procedure TF__PREFS.FormActivate(Sender: TObject);
begin
  {$IFDEF LINUX}
  ED__GOTO_OUT.Text := '~/';
  {$ENDIF LINUX}
  {$IFDEF Windows}
  ED__GOTO_OUT.Text := 'C:\';
  {$ENDIF Windows}
end;

procedure TF__PREFS.FormCreate(Sender: TObject);
begin
  mbSwitchLANG := false;
end;

procedure TF__PREFS.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then Close;
end;

procedure TF__PREFS.UD__BIRTH_YEARChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  ED__BIRTH_YEAR.Text := IntToStr(UD__BIRTH_YEAR.Position);
end;

procedure TF__PREFS.UseData();
begin
  miGLat_DEG := StrToInt(ED__GLAT_DEG.Text);
  miGLat_MIN := StrToInt(ED__GLAT_MIN.Text);
  miGLng_DEG := StrToInt(ED__GLNG_DEG.Text);
  miGLng_MIN := StrToInt(ED__GLNG_MIN.Text);

  if(not CBX__DST.Checked) then
    miDST_HH := 0
  else
    miDST_HH := StrToInt(ED__DST_HH.Text);

  miUTC_HH := StrToInt(ED__UTC_HH.Text);

  if(RB__LANG_DE.Checked) then
  begin
    mbSwitchLANG := (msLANG_ID = 'EN');
    msLANG_ID := 'DE';
  end;
  if(RB__LANG_EN.Checked) then
  begin
    mbSwitchLANG := (msLANG_ID = 'DE');
    msLANG_ID := 'EN';
  end;

  miBirthYear := StrToInt(ED__BIRTH_YEAR.Text);
  miFirstStart := 0;
  msCountry := ED__COUNTRY.Text;
  msCity := ED__CITY.Text;
  msDST := ED__DST.Text;

  mbHDust := CBX__HDUST.Checked;
  miLThickness := ED__LTHICKNESS.Value;

  if(RB__CNST_COL_RED.Checked) then
    mCColor := clRed
  else if (RB__CNST_COL_LIME.Checked) then
    mCColor := clLime
  else if(RB__CNST_COL_BLUE.checked) then
    mCColor := clAqua
  else
    mCColor := clRed;

  if(RB__HOR_0.Checked) then
    miLandscapeNo := 0
  else if(RB__HOR_1.Checked) then
    miLandscapeNo := 1
  else if(RB__HOR_2.Checked) then
    miLandscapeNo := 2;

  if(CBX__LOWHIGHRES.Checked) then
    giRSCLvl := 0
  else
    giRSCLvl := 1;

  msGotoOutputDir := ED__GOTO_OUT.Text;

end;

procedure TF__PREFS.ShowData();
begin
  ED__DST.Text := msDST;
  ED__COUNTRY.Text := msCountry;
  ED__CITY.Text:=msCity;

  ED__GLAT_DEG.Text := IntToStr(miGLat_DEG);
  ED__GLAT_MIN.Text := IntToStr(miGLat_MIN);
  ED__GLNG_DEG.Text := IntToStr(miGLng_DEG);
  ED__GLNG_MIN.Text := IntToStr(miGLng_MIN);

  CBX__DST.Checked := (miDST_HH <> 0);
  ED__DST_HH.Text := IntToStr(miDST_HH);
  ED__UTC_HH.Text := IntToStr(miUTC_HH);

  if(msLANG_ID = 'DE') then
  begin
    RB__LANG_DE.Checked:=true;
    RB__LANG_EN.Checked:=false;
  end;
  if(msLANG_ID = 'EN') then
  begin
    RB__LANG_DE.Checked:=false;
    RB__LANG_EN.Checked:=true;
  end;

  ED__BIRTH_YEAR.Text := IntToStr(miBirthYear);

  if(miRefreshRateMinutes > 0) and (miRefreshRateMinutes < 61) then
    ED__REFRESHRATE.Value := miRefreshRateMinutes
  else
    ED__REFRESHRATE.Value := 5;

  CBX__HDUST.Checked := mbHDust;

  ED__LTHICKNESS.Value := miLThickness;

  if(mCColor = clRed) then
    begin RB__CNST_COL_RED.Checked := true; RB__CNST_COL_LIME.Checked := false; RB__CNST_COL_BLUE.Checked := false; end
  else if (mCColor = clLime) then
    begin RB__CNST_COL_RED.Checked := false; RB__CNST_COL_LIME.Checked := true; RB__CNST_COL_BLUE.Checked := false; end
  else if(mCColor = clAqua) then
    begin RB__CNST_COL_RED.Checked := false; RB__CNST_COL_LIME.Checked := false; RB__CNST_COL_BLUE.Checked := true; end
  else
    begin RB__CNST_COL_RED.Checked := true; RB__CNST_COL_LIME.Checked := false; RB__CNST_COL_BLUE.Checked := false; end;

  case miLandscapeNo of
    0:
    begin
      RB__HOR_0.Checked:=true;
      RB__HOR_1.Checked:=false;
      RB__HOR_2.Checked:=false;
    end;
    1:
    begin
      RB__HOR_0.Checked:=false;
      RB__HOR_1.Checked:=true;
      RB__HOR_2.Checked:=false;
    end;
    2:
    begin
      RB__HOR_0.Checked:=false;
      RB__HOR_1.Checked:=false;
      RB__HOR_2.Checked:=true;
    end;
  end; // case

  ED__GOTO_OUT.Text := msGotoOutputDir;
end;

procedure TF__PREFS.SavePrefs();
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(msAlbireoLocalDir + 'ATB.ini');

  IniFile.WriteInteger('CONF','GLAT_DEG',miGLat_DEG);
  IniFile.WriteInteger('CONF','GLAT_MIN',miGLat_MIN);
  IniFile.WriteInteger('CONF','GLNG_DEG',miGLng_DEG);
  IniFile.WriteInteger('CONF','GLNG_MIN',miGLng_MIN);
  IniFile.WriteInteger('CONF','DST_HH',miDST_HH);
  IniFile.WriteInteger('CONF','UTC_HH',miUTC_HH);
  IniFile.WriteInteger('CONF','REFRESHRATE_MINUTES',miRefreshrateMinutes);
  IniFile.WriteInteger('CONF','LTHICKNESS',miLThickness);
  IniFile.WriteInteger('CONF','FIRSTSTART',miFirstStart);
  IniFile.WriteInteger('CONF','LANDSCAPE',miLandscapeNo);
  IniFile.WriteString('CONF','GOTOOUTDIR',msGotoOutputDir);
  IniFile.WriteInteger('CONF','RSCLVL',giRSCLvl);
  if(mCColor = clRed) then
    begin IniFile.WriteInteger('CONF','CCOLOR',0); end
  else if (mCColor = clLime) then
    begin IniFile.WriteInteger('CONF','CCOLOR',1); end
  else if(mCColor = clAqua) then
    begin IniFile.WriteInteger('CONF','CCOLOR',2); end
  else
    begin IniFile.WriteInteger('CONF','CCOLOR',0); end;

  if(mbHDust) then
    IniFile.WriteInteger('CONF','HDUST',1)
  else
    IniFile.WriteInteger('CONF','HDUST',0);

  if(RB__LANG_DE.Checked) then
    IniFile.WriteString('USER','LANGUAGE','DE')
  else
    IniFile.WriteString('USER','LANGUAGE','EN');

  IniFile.WriteInteger('USER','YEAROFBIRTH',miBirthYear);
  IniFile.WriteString('USER','COUNTRY',msCountry);
  IniFile.WriteString('USER','CITY',msCity);
  IniFile.WriteString('USER','DST',msDST);

  IniFile.Destroy;

end;


end.

