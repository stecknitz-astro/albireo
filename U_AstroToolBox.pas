unit U_AstroToolBox;

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

interface

uses
  Classes, SysUtils, FileUtil, SpinEx, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ActnList, Menus, Buttons, EditBtn, Spin, Grids,
  ExtDlgs, DateUtils, IniFiles, Windows, StrUtils,
  LCLType, // HBitmap type
  IntfGraphics, // TLazIntfImage type
  fpImage, // TFPColor type
  LCLIntf,
  ContNrs, Math,
  Types, LResources, MaskEdit,
  U_Prefs, U_ALib, U_AlbireoLib,
  U_ABase, U_AConst,
  U_Translation, U_TimeStat,
  U_AOVis,
  U_About, U_SetPrefsInfo, U_DSGVO, U_AstroCalc, U_StartUp,
  U_MShower, U_ADM, U_PictureViewer,
  U_AstroVoids,
  U_StrmDlg, U_HorCust, U_ADBInfo;

const
 // Astro-Object type index
 ciAOF_CB = 0;
 ciAOF_EComets = 1;
 ciAOF_SM_CON = 2;
 ciAOF_SM_ECLIPTIC = 3;
 ciAOF_SM_EQUATOR = 4;
 ciAOF_SM_GALACTIC = 5;
 ciAOF_Galaxies = 6;
 ciAOF_GlobularClusters = 7;
 ciAOF_SM_HS = 8;
 ciAOF_Moon = 9;
 ciAOF_GalacticNebula = 10;
 ciAOF_OpenClusters = 11;
 ciAOF_PlanetaryNebula = 12;
 ciAOF_Sun = 13;
 ciAOF_MShower = 14;
 ciAOF_Messier = 15;
 ciAOF_Quasars = 16;
 ciAOF_SM_MERI = 17;
 ciAOF_ASTEROIDS = 18;
 ciAOF_STAR_DESCR_LOW = 19;
 ciAOF_STAR_DESCR_HIGH = 20;
 ciAOF_CONNAMES = 21;
 ciAOF_SM_AS = 22;
 ciAOF_COO_EQU_RA = 23;
 ciAOF_COO_EQU_DEC = 24;

 // Page index
 ciPAGE_ALBIREO = 0;
 ciPAGE_HA = 1;
 ciPAGE_DB = 2;
 ciPAGE_STARMAP = 3;
 ciPAGE_DEVICES = 4;
 ciPAGE_SOLSYS = 5;

 // Animation Refresh Time
 ciGraphFastRefreshTime = 1000;
 ciGraphNormalRefreshTime = 60000;

 csLICENSETXT = 'Control.atb';
 csREGISTRATION = 'CHANNEL';

 crMagPosStd = 5.0;
 crMagPosStdZoom = 7.5;
 crMagPosStd_G = 10.0;
 crMagPosStdZoom_G = 11.0;

 csREALTIMESTOPPED_DE = 'Echtzeitdarstellung angehalten';
 csREALTIMESTOPPED_EN = 'Real-Time mode is stopped';

 // Streaming Support
 csCurTime = 'CurrentTime.txt';
 csSidTime = 'SiderialTime.txt';
 csJulTime = 'JulianTime.txt';
 csSunRiseToday = 'SunRiseToday.txt';
 csSunSetToday = 'SunSetToday.txt';
 csMoonRiseToday = 'MoonRiseToday.txt';
 csMoonSetToday = 'MoonSetToday.txt';
 csText1 = 'Text1.txt';
 csText2 = 'Text2.txt';
 csText3 = 'Text3.txt';
 csRandomRecomNGC = 'RandomRecomNGC.txt';
 csAObjectDist = 'AObjectDist.txt';

type
  { TF__ASTROTOOLBOX }

  TF__ASTROTOOLBOX = class(TForm)
    BB__SWITCH_LANG: TBitBtn;
    B__AZIMUTH_INFO: TButton;
    BV__RIGHT: TBevel;
    BV__LEFT: TBevel;
    BV__BOTTOM: TBevel;
    Button1: TButton;
    Button2: TButton;
    B__LOC_RESET: TButton;
    B__MAGSIZE_G_RESET: TButton;
    B__MAGSIZE_RESET: TButton;
    B__PARALLACTIC_INFO: TButton;
    B__RESET_TAB: TButton;
    B__ADEV_PHOTO_ADD: TButton;
    B__ADEV_PHOTO_DEL: TButton;
    B__ASTROCALC: TButton;
    B__SEARCH_CLEAR: TButton;
    B__ADEV_DEL: TButton;
    B__ADEV_MASK_NEW: TButton;
    B__ADEV_MOD: TButton;
    CB__ADEV_NAME: TComboBox;
    CB__ADEV_OK_TYPE: TComboBox;
    CB__ADEV_TYPE: TComboBox;
    CB__AO: TComboBox;
    CB__ASTEROIDS: TComboBox;
    CB__CITIES: TComboBox;
    CB__COMETS: TComboBox;
    CB__COUNTRIES: TComboBox;
    CB__SIGNS: TComboBox;
    CB__WT: TDateEdit;
    CBX__TELESCOPE_DEF: TCheckBox;
    CBX__CERES: TCheckBox;
    CBX__PLUTO: TCheckBox;
    CBX__EXACTMATCH: TCheckBox;
    CBX__GOTOOUT: TCheckBox;
    CBX__LT: TCheckBox;
    ED__AZ_DEG: TEdit;
    ED__AZ_MM: TEdit;
    ED__AZ_SS: TEdit;
    ED__WT_HH: TEdit;
    ED__ADEV_APT: TEdit;
    ED__ADEV_ARTNO: TEdit;
    ED__ADEV_DESCR: TEdit;
    ED__ADEV_FW: TEdit;
    ED__ADEV_MANU: TEdit;
    ED__DEC_DEG: TEdit;
    ED__DEC_MM: TEdit;
    ED__DEC_SS: TEdit;
    ED__HA_HH: TEdit;
    ED__HA_MM: TEdit;
    ED__HA_SS: TEdit;
    ED__HGT_HH: TEdit;
    ED__HGT_MM: TEdit;
    ED__HGT_SS: TEdit;
    ED__MAG: TEdit;
    ED__OCULAR_FW: TSpinEdit;
    ED__OK_VIEW: TEdit;
    ED__RZ_HH: TEdit;
    ED__RZ_MM: TEdit;
    ED__RZ_SS: TEdit;
    ED__SEARCH: TEdit;
    ED__WT_MM: TEdit;
    ED__WT_SS: TEdit;
    GBX__ADEV: TGroupBox;
    GRD__RECOM_PROP: TStringGrid;
    IMG__MOON_CORNER: TImage;
    IMG__RECOMMEND: TImage;
    IMG__MW: TImage;
    IMG__SOLSYS: TImage;
    L__LIVETABLES: TLabel;
    L__ANGLE_PA: TLabel;
    L__LT_INTERVAL: TLabel;
    L__SOLSYS_PLUTO: TLabel;
    L__DELTA_AZ_ZOOM: TLabel;
    L__DELTA_HGT_ZOOM: TLabel;
    L__GOTOOUT: TLabel;
    L__ASTROCALC: TLabel;
    L__SOLSYS_CERES: TLabel;
    L__ZERO_S: TLabel;
    L__ATYPE: TLabel;
    L__AZ: TLabel;
    L__CONSTELL_TITLE: TLabel;
    L__CONSTELL: TLabel;
    L__ATYPE_TITLE: TLabel;
    L__EAST: TLabel;
    L__NORTH: TLabel;
    L__SOUTH: TLabel;
    L__WEST: TLabel;
    L__ZC_L: TLabel;
    L__2MASX: TLabel;
    L__BV_RIGHT: TLabel;
    L__BV_BOTTOM: TLabel;
    L__BV_LEFT: TLabel;
    L__BOTTOM_INFO: TLabel;
    L__ECL: TLabel;
    L__IC: TLabel;
    L__MCG: TLabel;
    L__MESSIER: TLabel;
    L__MESSIER5: TLabel;
    L__NGC: TLabel;
    L__OTHER: TLabel;
    L__STARTSTOP: TLabel;
    L__DST: TLabel;
    L__LOCATION: TLabel;
    L__MAGSIZE: TLabel;
    L__MAGSIZE_G: TLabel;
    L__MOON_RISE: TLabel;
    L__MOON_RISE_TITLE: TLabel;
    L__MOON_SET: TLabel;
    L__MOON_SET_TITLE: TLabel;
    L__OC_CITY: TLabel;
    L__OC_COUNTRY: TLabel;
    L__OC_LAT: TLabel;
    L__OC_LONG: TLabel;
    L__SELMAG_TITLE: TLabel;
    L__DBFILTER_TITLE: TLabel;
    L__MAG_HIGH: TLabel;
    L__MAG_LOW: TLabel;
    L__SUNRISE_TITLE: TLabel;
    L__SUNSET_TITLE: TLabel;
    L__SUN_RISE: TLabel;
    L__SUN_SET: TLabel;
    L__TELESCOPE_DEF: TLabel;
    L__SOLSYS_DIST: TLabel;
    L__SOLSYS_INCL_TITLE: TLabel;
    L__SOLSYS_PLANE_TITLE: TLabel;
    L__UGC: TLabel;
    L__ZC_D: TLabel;
    L__ZC_IN: TLabel;
    L__ZC_L1: TLabel;
    L__ZC_U: TLabel;
    L__ZC_R: TLabel;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MENU__ASTROPHOTO_SIGNAL: TMenuItem;
    MENU__ASTROPHOTO_NYS: TMenuItem;
    MENU__ASTRPHOTO_SIMAGE: TMenuItem;
    MENU__ASTROPHOTO_SETTINGS: TMenuItem;
    MENU__ASTROPHOTO: TMenuItem;
    MENU__DONATION: TMenuItem;
    MENU__KOCHAB: TMenuItem;
    MENU__REFRESH_APP: TMenuItem;
    MENU__T_STARTSTOP: TMenuItem;
    MENU__APPCONTROL: TMenuItem;
    MENU__GOTO: TMenuItem;
    MENU__MY_TEL: TMenuItem;
    MENU__TABLES: TMenuItem;
    MENU__SPACELAB: TMenuItem;
    MENU__SOLSYS: TMenuItem;
    MENU__MAP: TMenuItem;
    MENU__TODAY: TMenuItem;
    MENU__MODULES: TMenuItem;
    MENU__EYEFAC: TMenuItem;
    MENU__ADB_ADMIN: TMenuItem;
    MENU__ADB_SHOW: TMenuItem;
    MENU__SCLASS_CARBON: TMenuItem;
    MENU__SCLASS_Y: TMenuItem;
    MENU__SCLASS_T: TMenuItem;
    MENU__SCLASS_L: TMenuItem;
    MENU__SCLASS_BD: TMenuItem;
    MenuItem17: TMenuItem;
    MENU__SCLASS_2: TMenuItem;
    MENU__SCLASS_C: TMenuItem;
    MENU__SCLASS_S: TMenuItem;
    MENU__SCLASS_N: TMenuItem;
    MENU__SCLASS_SPECIFIC: TMenuItem;
    MENU__LCLASS_SPECIFIC: TMenuItem;
    MENU__LCLASS_S1: TMenuItem;
    MENU__SCLASS_1: TMenuItem;
    MENU__SCLASS_ALL: TMenuItem;
    MENU__LCLASS_ALL: TMenuItem;
    MENU__SCLASS_R: TMenuItem;
    MENU__SCLASS_M: TMenuItem;
    MENU__SCLASS_K: TMenuItem;
    MENU__SCLASS_G: TMenuItem;
    MENU__SCLASS_F: TMenuItem;
    MENU__SCLASS_A: TMenuItem;
    MENU__SCLASS_B: TMenuItem;
    MENU__SCLASS_O: TMenuItem;
    MENU__LCLASS_7: TMenuItem;
    MENU__LCLASS_6: TMenuItem;
    MENU__LCLASS_5: TMenuItem;
    MENU__LCLASS_4: TMenuItem;
    MENU__LCLASS_3: TMenuItem;
    MENU__LCLASS_2: TMenuItem;
    MENU__LCLASS_1: TMenuItem;
    MENU__LCLASS_0: TMenuItem;
    MENU__SPCLASS: TMenuItem;
    MENU__LCLASS: TMenuItem;
    MENU__SHOW_STARS: TMenuItem;
    P__ASTROCALC: TPanel;
    P__AZIMUTH_DIR: TPanel;
    P__HA_TITLE: TPanel;
    PMENU__POLEP: TMenuItem;
    PMENU__RA_SCALA: TMenuItem;
    PMENU__DEC_SCALA: TMenuItem;
    PMENU__COO_EQU: TMenuItem;
    PMENU__HS: TMenuItem;
    PMENU__AS: TMenuItem;
    MENU__DOWNLOAD: TMenuItem;
    MENU__TWITCH: TMenuItem;
    MENU__WEBSITE: TMenuItem;
    MENU__WEB: TMenuItem;
    MENU__HOR: TMenuItem;
    MENU__HOR_3: TMenuItem;
    MENU__STRM_LANGFLIP: TMenuItem;
    MENU__STRM_VIEWFLIP: TMenuItem;
    MENU__STRM_NOW: TMenuItem;
    MENU__STRM_PREFS: TMenuItem;
    MENU__STRM: TMenuItem;
    MENU__HOR_0: TMenuItem;
    MENU__HOR_1: TMenuItem;
    MENU__HOR_2: TMenuItem;
    MENU__SHOW_MESSIERONLY: TMenuItem;
    MENU__VISIBLE_ONLY: TMenuItem;
    ODLG__FILE: TOpenDialog;
    P__FASTZOOM_ITEMS: TPanel;
    P__ZC_LABEL: TPanel;
    P__ZC_CONTROL: TPanel;
    P__ZC_TITLE: TPanel;
    P__FASTZOOM: TPanel;
    P__MAGSIZE_LABEL: TPanel;
    P__MAGSIZE_CONTROL: TPanel;
    P__MAGSIZE_TITLE: TPanel;
    P__CATINFO_LABEL: TPanel;
    P__CATINFO_CONTROL: TPanel;
    P__CATINFO_ITEMS: TPanel;
    P__CATINFO_TITLE: TPanel;
    P__KOCHAB_TITLE: TPanel;
    P__KOCHAB_CONTROL: TPanel;
    P__CATINFO: TPanel;
    P__RECOM_LEFT: TPanel;
    P__RECOM_NAVPIC: TPanel;
    PC__RECOM: TPageControl;
    P__RECOMMEND_SHOW: TPanel;
    P__RECOM_P0: TPanel;
    P__RECOM_RIGHT: TPanel;
    P__REWIND_MINUS: TPanel;
    P__FORWARD_PLUS: TPanel;
    P__RECOM_RANGE: TPanel;
    P__RECOM_INFO1: TPanel;
    P__RECOM_TITLE: TPanel;
    P__NOW: TPanel;
    P__STARTSTOP: TPanel;
    P__COUNTRIES: TPanel;
    P__MOON: TPanel;
    PMENU__ADM: TMenuItem;
    PMENU__AO: TPopupMenu;
    PMENU__HOR: TPopupMenu;
    PMENU__SOLARSYSTEM: TPopupMenu;
    P__BASEDATA: TPanel;
    P__RECOMMED: TPanel;
    PC__TABLES_SELMAG: TPageControl;
    PC__MAG: TPageControl;
    P__MAGSIZE: TPanel;
    PMENU__MMAG_9: TMenuItem;
    MENU__COMMENT: TMenuItem;
    P__SOLSYS_TITLE_INT: TPanel;
    P__ADEV_PHOTO_CONTROL: TPanel;
    P__ADEV_PHOTO_CAPTION: TPanel;
    PMENU__MONTH_PREV: TMenuItem;
    PMENU__MONTH_NEXT: TMenuItem;
    PMENU__MONTH: TMenuItem;
    PMENU__WEEK_NEXT: TMenuItem;
    PMENU__WEEK_PREV: TMenuItem;
    PMENU__WEEK: TMenuItem;
    PMENU__DAY_PREV: TMenuItem;
    PMENU__NEXT_DAY: TMenuItem;
    PMENU__DAY: TMenuItem;
    MENU__PDF: TMenuItem;
    PMENU__DATENAVIG: TPopupMenu;
    P__ALBIREO_INFO: TPanel;
    P__SELMAG: TPanel;
    P__SETTIME: TPanel;
    MENU__ADB: TMenuItem;
    PMENU__CONSTELLATIONS: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    PMENU__VIEW_W: TMenuItem;
    PMENU__VIEW_E: TMenuItem;
    PMENU__VIEW_S: TMenuItem;
    PMENU__VIEW_N: TMenuItem;
    PMENU__VIEW_HORIZON: TMenuItem;
    PMENU__VIEW_STARMAP: TMenuItem;
    PMENU__VIEW: TMenuItem;
    PMENU__MMAG_8: TMenuItem;
    PMENU__MMAG_7: TMenuItem;
    PMENU__MMAG_6: TMenuItem;
    PMENU__MMAG_5: TMenuItem;
    PMENU__MMAG_4: TMenuItem;
    PMENU__MMAG_3: TMenuItem;
    PMENU__MMAG_2: TMenuItem;
    PMENU__MMAG_1: TMenuItem;
    PMENU__MMAG_0: TMenuItem;
    PMENU__LT_3: TMenuItem;
    PMENU__LT_2: TMenuItem;
    PMENU__LT_1: TMenuItem;
    PMENU__LINETHICKNESS: TMenuItem;
    PMENU__CB: TMenuItem;
    PMENU__CONNAMES: TMenuItem;
    PMENU__CON: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    P__NAVIG_TITLE: TPanel;
    P__DAWN_AM_A: TPanel;
    P__DAWN_AM_C: TPanel;
    P__DAWN_AM_N: TPanel;
    P__DAWN_AM_PREV_PM: TPanel;
    P__DAWN_PM_A: TPanel;
    P__DAWN_PM_C: TPanel;
    P__DAWN_PM_N: TPanel;
    P__DAYLIGHT: TPanel;
    P__NIGHT_AM: TPanel;
    P__NIGHT_PM: TPanel;
    P__TIME_SEL: TPanel;
    P__DATETIME_VISU: TPanel;
    P__DATETIME_SET: TPanel;
    P__TIMESETTINGS: TPanel;
    P__HOME_IMG: TPanel;
    P__KOCHAB: TPanel;
    P__STAR_LOC_MAIN: TPanel;
    IMG__AZIMUTH: TImage;
    IMG__PARALLACTIC: TImage;
    L__DECLINATION: TLabel;
    L__DEC_SIGN: TLabel;
    L__HGT: TLabel;
    L__HGT_HH_MINUS: TLabel;
    L__HOURANGLE: TLabel;
    L__RZ: TLabel;
    PC__MONT: TPageControl;
    P__AONAME: TPanel;
    P__AOTYPE: TPanel;
    GRD__AO: TStringGrid;
    GRD__TELPROP: TStringGrid;
    IMG__HOR: TImage;
    IMG__ADEV_PHOTO: TImage;
    IMG__POS_MOON: TImage;
    Label1: TLabel;
    L__DEEPSKY: TLabel;
    L__DS_CONSTELLATION: TLabel;
    L__HD_R: TLabel;
    L__HD_L: TLabel;
    L__SOLARSYSTEM: TLabel;
    L__SOLSYS_ASTEROIDS: TLabel;
    L__SOLSYS_COMETS: TLabel;
    L__NAVSOLSYS: TLabel;
    L__ADEV_ARTNO: TLabel;
    L__ADEV_DESCR: TLabel;
    L__ADEV_FW: TLabel;
    L__ADEV_FW1: TLabel;
    L__ADEV_FW2: TLabel;
    L__ADEV_FW_MM: TLabel;
    L__ADEV_FW_MM1: TLabel;
    L__ADEV_FW_MM2: TLabel;
    L__ADEV_ID: TLabel;
    L__ADEV_MANU: TLabel;
    L__ADEV_OK_TYPE: TLabel;
    L__ADEV_TYPE: TLabel;
    L__MAG_X_MAX1: TLabel;
    L__OK_VIEW: TLabel;
    L__FIRSTRUN_TEXT0_DE: TLabel;
    L__FIRSTRUN_INFO2_DE: TLabel;
    L__FIRSTRUN_INFO2_EN: TLabel;
    L__ADEV_NAME: TLabel;
    L__FIRSTRUN_TEXT0_DE1: TLabel;
    LB__SEARCHRES: TListBox;
    L__DEVICES: TLabel;
    L__ALBIREO: TLabel;
    L__HA: TLabel;
    L__MAP: TLabel;
    L__TABLE: TLabel;
    L__MS: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    PMENU__STAR_DESCR_HIGH: TMenuItem;
    PMENU__STAR_DESCR_LOW: TMenuItem;
    PMENU__MW_SUPPRESSED: TMenuItem;
    PMENU__STAR_DESCR: TMenuItem;
    PMENU__ASTEROIDS: TMenuItem;
    PMENU__PVIS: TMenuItem;
    PMENU__PSHOW: TMenuItem;
    PMENU__MW_ALWAYS_VISIBLE: TMenuItem;
    PMENU__MW_REALISTIC: TMenuItem;
    PMENU__MILKYWAY: TMenuItem;
    P__AZIMUTH: TPanel;
    P__HORDUST: TPanel;
    PMENU__MERI: TMenuItem;
    MENU__OPENOBJ: TMenuItem;
    MENU__PLCOM_PATH: TMenuItem;
    MENU_VIEW_NAVIGATION: TMenuItem;
    MENU__VIEW_TIMECONTROL: TMenuItem;
    MENU__VIEW: TMenuItem;
    MENU__DSGVO: TMenuItem;
    P__LANDSCAPE: TPanel;
    P__PARALLACTIC: TPanel;
    P__PK: TPanel;
    P__KOCHABMETHOD: TPanel;
    P__SOLSYS: TPanel;
    P__SOLSYS_BASE: TPanel;
    P__SOLSYS_CB_SEL: TPanel;
    P__NAV_SOLSYS: TPanel;
    P__INCL: TPanel;
    P__SOLSYS_TITLE: TPanel;
    P__STAR_LOC: TPanel;
    P__TELPROP: TPanel;
    P__ADEV_PHOTO: TPanel;
    P__PHOTO: TPanel;
    PMENU__Q: TMenuItem;
    PMENU__MESSIER: TMenuItem;
    PMENU__PLANETS: TMenuItem;
    MENU__SELSIGN: TMenuItem;
    PMENU__MSHOWER: TMenuItem;
    PMENU__AUXLINE_RESET: TMenuItem;
    MENU__ABOUT: TMenuItem;
    PMENU__STARS_AM: TMenuItem;
    PMENU__STARS_SPEC: TMenuItem;
    PMENU__STARS: TMenuItem;
    PMENU__ECLIPTIC: TMenuItem;
    PMENU__EQUATOR: TMenuItem;
    PMENU__GALACTIC: TMenuItem;
    PMENU__COO_HOR: TMenuItem;
    PMENU__AUXLINE: TMenuItem;
    MENU__EXPORT_SVG: TMenuItem;
    PMENU__AOBJECT: TPopupMenu;
    P__FIRSTRUN: TPanel;
    P__DEV_BUTTONS: TPanel;
    P__DEV_FIELDS: TPanel;
    P__SEARCHALL: TPanel;
    P__SEARCH: TPanel;
    PMENU__N: TMenuItem;
    PC__WORKBENCH: TPageControl;
    P__CONTENT: TPanel;
    PMENU__COMETS: TMenuItem;
    MENU__ADEV_NAME: TMenuItem;
    ODLG__PIC: TOpenPictureDialog;
    PMENU__ADEV_NAME: TPopupMenu;
    P__DEVICES: TPanel;
    PMENU__MOON: TMenuItem;
    PMENU__SUN: TMenuItem;
    PMENU__OC: TMenuItem;
    PMENU__PN: TMenuItem;
    PMENU__GC: TMenuItem;
    PMENU__GALAXIES: TMenuItem;
    PMENU__SM_REFRESH: TMenuItem;
    MENU__TIMESTAT: TMenuItem;
    MENU__UPDATE: TMenuItem;
    MENU__HELP: TMenuItem;
    MENU__PREFS: TMenuItem;
    MENU__EXTRA: TMenuItem;
    MENU__QUIT: TMenuItem;
    MENU__FILE: TMenuItem;
    MMENU__MAIN: TMainMenu;
    PMENU__STARMAP: TPopupMenu;
    P__DEVICES_VISU: TPanel;
    P__MAP: TPanel;
    P__STARMAP: TPanel;
    P__TABLES: TPanel;
    P__SELECT: TPanel;
    P__GO_HOURANGLE: TPanel;
    P__GO_HOME: TPanel;
    P__NAVIG: TPanel;
    P__TABLE_TITLE: TPanel;
    P__VISIBLE: TPanel;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB3: TRadioButton;
    RB4: TRadioButton;
    RB5: TRadioButton;
    RB6: TRadioButton;
    RB7: TRadioButton;
    RB8: TRadioButton;
    RB__MESSIER: TRadioButton;
    RB__COMETS: TRadioButton;
    RB__G: TRadioButton;
    RB__GC: TRadioButton;
    RB__N: TRadioButton;
    RB__OC: TRadioButton;
    RB__P: TRadioButton;
    RB__PN: TRadioButton;
    RB__Q: TRadioButton;
    RB__S: TRadioButton;
    SDLG: TSaveDialog;
    SB__MAIN: TStatusBar;
    SHP__ZOMM_MOVE: TShape;
    SHP_COMP1: TShape;
    SHP__NORTH: TShape;
    SHP__NORTH1: TShape;
    SHP__ZIN: TShape;
    SHP__ZOMM_MOVE1: TShape;
    SHP__ZOUT: TShape;
    SHP__MC: TShape;
    SHP__MESSIER: TShape;
    SHP__MESSIER3: TShape;
    SHP__MESSIER4: TShape;
    SHP__MESSIER5: TShape;
    SHP__MESSIER6: TShape;
    SHP__MESSIER7: TShape;
    SHP__MESSIER8: TShape;
    SHP__NGC: TShape;
    SHP__SU1: TShape;
    SHP__TVIEW: TShape;
    SHP__KOCHAB: TShape;
    SHP__KOCHAB1: TShape;
    SHP__POLARIS: TShape;
    SHP__NCP: TShape;
    SHP__POLARIS1: TShape;
    SHP__POS_SUN: TShape;
    SHP__SUN: TShape;
    SHP__SUN_INCL: TShape;
    SHP__ZD: TShape;
    SHP__ZL: TShape;
    SHP__ZR: TShape;
    SHP__ZU: TShape;
    SPL__TELPROP: TSplitter;
    SPL__DEV: TSplitter;
    GRD__RECOM_P: TStringGrid;
    GRD__RECOM_MS: TStringGrid;
    GRD__RECOM_ECL: TStringGrid;
    TIMER__STRM_VIEWFLIP: TTimer;
    TIMER__STRM_DATA: TTimer;
    TB__LT: TTrackBar;
    TS__RECOM_ECL: TTabSheet;
    TS__RECOM_MS: TTabSheet;
    TS__RECOM_PLANETS: TTabSheet;
    TS__RECOM_DS: TTabSheet;
    TS__RECOM_P0: TTabSheet;
    TB__SELMAG_G: TTrackBar;
    TIMER: TTimer;
    TIMER__GENMAP: TTimer;
    TIMER__WHEELZOOM: TTimer;
    TS__SELMAG_G: TTabSheet;
    TB__SELMAG: TTrackBar;
    TS__SELMAG_S: TTabSheet;
    TB__MAG: TTrackBar;
    TB__MAG_G: TTrackBar;
    TS__MAG_GAL: TTabSheet;
    TS__MAG_STARS: TTabSheet;
    TB__SOLSYS: TTrackBar;
    TB__TIME_24H: TTrackBar;
    TS__AZIMUTH: TTabSheet;
    TS__PARALLACTIC: TTabSheet;
    TS__SOLSYS: TTabSheet;
    TIMER__AOTABLE: TTimer;
    TS__ALBIREO: TTabSheet;
    TS__DEVICES: TTabSheet;
    TS__HOURANGLE: TTabSheet;
    TS__MAP: TTabSheet;
    TS__TABLES: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure BV__BOTTOMMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BV__LEFTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BV__RIGHTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure B__ASTROCALCClick(Sender: TObject);
    procedure B__ADEV_MASK_NEWClick(Sender: TObject);
    procedure B__ADEV_PHOTO_ADDClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure B__ADEV_DELClick(Sender: TObject);
    procedure B__ADEV_MODClick(Sender: TObject);
    procedure B__ADEV_PHOTO_DELClick(Sender: TObject);
    procedure B__AZIMUTH_INFOClick(Sender: TObject);
    procedure B__LOC_RESETClick(Sender: TObject);
    procedure B__MAGSIZE_G_RESETClick(Sender: TObject);
    procedure B__MAGSIZE_RESETClick(Sender: TObject);
    procedure B__PARALLACTIC_INFOClick(Sender: TObject);
    procedure B__RESET_TABClick(Sender: TObject);
    procedure B__SEARCH_CLEARClick(Sender: TObject);
    procedure B__TIMEPLAYClick(Sender: TObject);
    procedure BB__SWITCH_LANGClick(Sender: TObject);
    procedure CBX__AZ_SCALEClick(Sender: TObject);
    procedure CBX__CERESChange(Sender: TObject);
    procedure CBX__LTClick(Sender: TObject);
    procedure CBX__EXACTMATCHClick(Sender: TObject);
    procedure CBX__PLUTOChange(Sender: TObject);
    procedure CBX__TELESCOPE_DEFChange(Sender: TObject);
    procedure CB__ADEV_NAMEChange(Sender: TObject);
    procedure CB__ADEV_OK_TYPEChange(Sender: TObject);
    procedure CB__ADEV_TYPEChange(Sender: TObject);
    procedure CB__AOChange(Sender: TObject);
    procedure CB__ASTEROIDSChange(Sender: TObject);
    procedure CB__CITIESCloseUp(Sender: TObject);
    procedure CB__COMETSChange(Sender: TObject);
    procedure CB__COUNTRIESCloseUp(Sender: TObject);
    procedure CB__SIGNSChange(Sender: TObject);
    procedure CB__WTChange(Sender: TObject);
    procedure ED__ADEV_APTChange(Sender: TObject);
    procedure ED__ADEV_ARTNOChange(Sender: TObject);
    procedure ED__ADEV_DESCRChange(Sender: TObject);
    procedure ED__ADEV_FWChange(Sender: TObject);
    procedure ED__ADEV_MANUChange(Sender: TObject);
    procedure ED__OCULAR_FWChange(Sender: TObject);
    procedure ED__SEARCHKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ED__WT_HHClick(Sender: TObject);
    procedure ED__WT_HHKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ED__WT_HHMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ED__WT_MMClick(Sender: TObject);
    procedure ED__WT_MMKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ED__WT_MMMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ED__WT_SSClick(Sender: TObject);
    procedure ED__WT_SSKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ED__WT_SSMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure GRD__AODblClick(Sender: TObject);
    procedure GRD__AODrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure GRD__AOHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure GRD__AOKeyPress(Sender: TObject; var Key: char);
    procedure GRD__RECOM_PDblClick(Sender: TObject);
    procedure IMG__ADEV_PHOTOClick(Sender: TObject);
    (*
    procedure HTTPCLIENTError(const msg: string; aSocket: TLSocket);
    function HTTPCLIENTInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: integer): integer;
    *)
    procedure IMG__FLAGClick(Sender: TObject);
    procedure IMG__HOMEClick(Sender: TObject);
    procedure IMG__HORMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure IMG__MOON_CORNERClick(Sender: TObject);
    procedure IMG__POS_MOONMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure IMG__SOLSYSMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LB__SEARCHRESClick(Sender: TObject);
    procedure LB__SEARCHRESMouseEnter(Sender: TObject);
    procedure LB__SEARCHRESMouseLeave(Sender: TObject);
    procedure L__ALBIREOClick(Sender: TObject);
    procedure L__ALBIREOMouseEnter(Sender: TObject);
    procedure L__ALBIREOMouseLeave(Sender: TObject);
    procedure L__ASTROCALCClick(Sender: TObject);
    procedure L__ASTROCALCMouseEnter(Sender: TObject);
    procedure L__ASTROCALCMouseLeave(Sender: TObject);
    procedure L__BV_BOTTOMClick(Sender: TObject);
    procedure L__BV_LEFTClick(Sender: TObject);
    procedure L__BV_RIGHTClick(Sender: TObject);
    procedure L__DEVICESClick(Sender: TObject);
    procedure L__DEVICESMouseEnter(Sender: TObject);
    procedure L__DEVICESMouseLeave(Sender: TObject);
    procedure L__HAClick(Sender: TObject);
    procedure L__HAMouseEnter(Sender: TObject);
    procedure L__HAMouseLeave(Sender: TObject);
    procedure L__MAPClick(Sender: TObject);
    procedure L__MAPMouseEnter(Sender: TObject);
    procedure L__MAPMouseLeave(Sender: TObject);
    procedure L__NAVSOLSYSClick(Sender: TObject);
    procedure L__NAVSOLSYSMouseEnter(Sender: TObject);
    procedure L__NAVSOLSYSMouseLeave(Sender: TObject);
    procedure L__OC_LATClick(Sender: TObject);
    procedure L__OC_LONGClick(Sender: TObject);
    procedure L__STARTSTOPClick(Sender: TObject);
    procedure L__TABLEClick(Sender: TObject);
    procedure L__TABLEMouseEnter(Sender: TObject);
    procedure L__TABLEMouseLeave(Sender: TObject);
    procedure L__ZC_DClick(Sender: TObject);
    procedure L__ZC_INClick(Sender: TObject);
    procedure L__ZC_L1Click(Sender: TObject);
    procedure L__ZC_LClick(Sender: TObject);
    procedure L__ZC_RClick(Sender: TObject);
    procedure L__ZC_UClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MENU__ADB_ADMINClick(Sender: TObject);
    procedure MENU__ADB_SHOWClick(Sender: TObject);
    procedure MENU__ASTROPHOTO_NYSClick(Sender: TObject);
    procedure MENU__ASTROPHOTO_SETTINGSClick(Sender: TObject);
    procedure MENU__ASTROPHOTO_SIGNALClick(Sender: TObject);
    procedure MENU__ASTRPHOTO_SIMAGEClick(Sender: TObject);
    procedure MENU__DONATIONClick(Sender: TObject);
    procedure MENU__EYEFACClick(Sender: TObject);
    procedure MENU__GOTOClick(Sender: TObject);
    procedure MENU__KOCHABClick(Sender: TObject);
    procedure MENU__LCLASS_0Click(Sender: TObject);
    procedure MENU__LCLASS_1Click(Sender: TObject);
    procedure MENU__LCLASS_2Click(Sender: TObject);
    procedure MENU__LCLASS_3Click(Sender: TObject);
    procedure MENU__LCLASS_4Click(Sender: TObject);
    procedure MENU__LCLASS_5Click(Sender: TObject);
    procedure MENU__LCLASS_6Click(Sender: TObject);
    procedure MENU__LCLASS_7Click(Sender: TObject);
    procedure MENU__LCLASS_ALLClick(Sender: TObject);
    procedure MENU__MAPClick(Sender: TObject);
    procedure MENU__MY_TELClick(Sender: TObject);
    procedure MENU__REFRESH_APPClick(Sender: TObject);
    procedure MENU__SCLASS_AClick(Sender: TObject);
    procedure MENU__SCLASS_ALLClick(Sender: TObject);
    procedure MENU__SCLASS_BClick(Sender: TObject);
    procedure MENU__SCLASS_BDClick(Sender: TObject);
    procedure MENU__SCLASS_CARBONClick(Sender: TObject);
    procedure MENU__SCLASS_CClick(Sender: TObject);
    procedure MENU__SCLASS_FClick(Sender: TObject);
    procedure MENU__SCLASS_GClick(Sender: TObject);
    procedure MENU__SCLASS_KClick(Sender: TObject);
    procedure MENU__SCLASS_LClick(Sender: TObject);
    procedure MENU__SCLASS_MClick(Sender: TObject);
    procedure MENU__SCLASS_NClick(Sender: TObject);
    procedure MENU__SCLASS_OClick(Sender: TObject);
    procedure MENU__SCLASS_RClick(Sender: TObject);
    procedure MENU__SCLASS_SClick(Sender: TObject);
    procedure MENU__SCLASS_TClick(Sender: TObject);
    procedure MENU__SCLASS_YClick(Sender: TObject);
    procedure MENU__SPACELABClick(Sender: TObject);
    procedure MENU__TABLESClick(Sender: TObject);
    procedure MENU__TODAYClick(Sender: TObject);
    procedure MENU__T_STARTSTOPClick(Sender: TObject);
    procedure PMENU__ASClick(Sender: TObject);
    procedure PMENU__DEC_SCALAClick(Sender: TObject);
    procedure PMENU__HSClick(Sender: TObject);
    procedure MENU__COMMENTClick(Sender: TObject);
    procedure MENU__DOWNLOADClick(Sender: TObject);
    procedure MENU__HORClick(Sender: TObject);
    procedure MENU__HOR_3Click(Sender: TObject);
    procedure MENU__PDFClick(Sender: TObject);
    procedure MENU__SHOW_MESSIERONLYClick(Sender: TObject);
    procedure MENU__STRM_LANGFLIPClick(Sender: TObject);
    procedure MENU__STRM_NOWClick(Sender: TObject);
    procedure MENU__STRM_PREFSClick(Sender: TObject);
    procedure MENU__STRM_VIEWFLIPClick(Sender: TObject);
    procedure MENU__TWITCHClick(Sender: TObject);
    procedure MENU__VISIBLE_ONLYClick(Sender: TObject);
    procedure MENU__GRD_MAG_6Click(Sender: TObject);
    procedure MENU__WEBSITEClick(Sender: TObject);
    procedure PMENU__RA_SCALAClick(Sender: TObject);
    procedure P__ASTROCALCClick(Sender: TObject);
    procedure P__ASTROCALCMouseEnter(Sender: TObject);
    procedure P__ASTROCALCMouseLeave(Sender: TObject);
    procedure P__CATINFO_CONTROLClick(Sender: TObject);
    procedure P__KOCHAB_CONTROLClick(Sender: TObject);
    procedure P__MAGSIZE_CONTROLClick(Sender: TObject);
    procedure P__REWIND_MINUSClick(Sender: TObject);
    procedure PC__TABLES_SELMAGChange(Sender: TObject);
    procedure PMENU__MMAG_9Click(Sender: TObject);
    procedure P__DEVICESClick(Sender: TObject);
    procedure P__FORWARD_PLUSClick(Sender: TObject);
    procedure P__GO_HOMEClick(Sender: TObject);
    procedure P__GO_HOURANGLEClick(Sender: TObject);
    procedure P__LICENSEDClick(Sender: TObject);
    procedure PMENU__ADMClick(Sender: TObject);
    procedure PMENU__CBClick(Sender: TObject);
    procedure PMENU__CONNAMESClick(Sender: TObject);
    procedure PMENU__CONClick(Sender: TObject);
    procedure PMENU__ASTEROIDSClick(Sender: TObject);
    procedure PMENU__DAY_PREVClick(Sender: TObject);
    procedure PMENU__LT_1Click(Sender: TObject);
    procedure PMENU__LT_2Click(Sender: TObject);
    procedure PMENU__LT_3Click(Sender: TObject);
    procedure PMENU__MMAG_0Click(Sender: TObject);
    procedure PMENU__MMAG_1Click(Sender: TObject);
    procedure PMENU__MMAG_2Click(Sender: TObject);
    procedure PMENU__MMAG_3Click(Sender: TObject);
    procedure PMENU__MMAG_4Click(Sender: TObject);
    procedure PMENU__MMAG_5Click(Sender: TObject);
    procedure PMENU__MMAG_6Click(Sender: TObject);
    procedure PMENU__MMAG_7Click(Sender: TObject);
    procedure PMENU__MMAG_8Click(Sender: TObject);
    procedure PMENU__MONTH_NEXTClick(Sender: TObject);
    procedure PMENU__MONTH_PREVClick(Sender: TObject);
    procedure PMENU__MW_ALWAYS_VISIBLEClick(Sender: TObject);
    procedure MENU__HOR_0Click(Sender: TObject);
    procedure MENU__HOR_1Click(Sender: TObject);
    procedure MENU_VIEW_NAVIGATIONClick(Sender: TObject);
    procedure MENU__DSGVOClick(Sender: TObject);
    procedure MENU__HOR_2Click(Sender: TObject);
    procedure MENU__OPENOBJClick(Sender: TObject);
    procedure MENU__PLCOM_PATHClick(Sender: TObject);
    procedure MENU__ABOUTClick(Sender: TObject);
    procedure MENU__ADEV_NAMEClick(Sender: TObject);
    procedure MENU__PREFSClick(Sender: TObject);
    procedure MENU__QUITClick(Sender: TObject);
    procedure MENU__SELSIGNClick(Sender: TObject);
    procedure MENU__SOLSYSClick(Sender: TObject);
    procedure MENU__TIMESTATClick(Sender: TObject);
    procedure MENU__UPDATEClick(Sender: TObject);
    procedure MENU__VIEW_TIMECONTROLClick(Sender: TObject);
    procedure PMENU__AOBJECTPopup(Sender: TObject);
    procedure PMENU__MERIClick(Sender: TObject);
    procedure PMENU__MW_REALISTICClick(Sender: TObject);
    procedure PMENU__MW_SUPPRESSEDClick(Sender: TObject);
    procedure PMENU__NEXT_DAYClick(Sender: TObject);
    procedure PMENU__POLEPClick(Sender: TObject);
    procedure PMENU__PVISClick(Sender: TObject);
    procedure PMENU__PSHOWClick(Sender: TObject);
    procedure PMENU__STAR_DESCR_HIGHClick(Sender: TObject);
    procedure PMENU__STAR_DESCR_LOWClick(Sender: TObject);
    procedure PMENU__VIEW_EClick(Sender: TObject);
    procedure PMENU__VIEW_NClick(Sender: TObject);
    procedure PMENU__VIEW_SClick(Sender: TObject);
    procedure PMENU__VIEW_STARMAPClick(Sender: TObject);
    procedure PMENU__VIEW_WClick(Sender: TObject);
    procedure PMENU__WEEK_NEXTClick(Sender: TObject);
    procedure PMENU__WEEK_PREVClick(Sender: TObject);
    procedure P__MAPClick(Sender: TObject);
    procedure P__MAPMouseEnter(Sender: TObject);
    procedure P__MAPMouseLeave(Sender: TObject);
    procedure P__NAVIG_TITLEClick(Sender: TObject);
    procedure P__NAV_SOLSYSClick(Sender: TObject);
    procedure P__NAV_SOLSYSMouseEnter(Sender: TObject);
    procedure P__NAV_SOLSYSMouseLeave(Sender: TObject);
    procedure P__NOWClick(Sender: TObject);
    procedure P__PKClick(Sender: TObject);
    procedure PC__WORKBENCHChange(Sender: TObject);
    procedure PMENU__AUXLINE_RESETClick(Sender: TObject);
    procedure PMENU__ECLIPTICClick(Sender: TObject);
    procedure PMENU__EQUATORClick(Sender: TObject);
    procedure PMENU__COMETSClick(Sender: TObject);
    procedure PMENU__GALACTICClick(Sender: TObject);
    procedure PMENU__GALAXIESClick(Sender: TObject);
    procedure PMENU__GCClick(Sender: TObject);
    procedure PMENU__MESSIERClick(Sender: TObject);
    procedure PMENU__MOONClick(Sender: TObject);
    procedure PMENU__MSHOWERClick(Sender: TObject);
    procedure PMENU__NClick(Sender: TObject);
    procedure PMENU__OCClick(Sender: TObject);
    procedure PMENU__PLANETSClick(Sender: TObject);
    procedure PMENU__PNClick(Sender: TObject);
    procedure PMENU__QClick(Sender: TObject);
    procedure PMENU__SM_REFRESHClick(Sender: TObject);
    procedure PMENU__STARS_AMClick(Sender: TObject);
    procedure PMENU__STARS_SPECClick(Sender: TObject);
    procedure PMENU__SUNClick(Sender: TObject);
    procedure P__DAWN_AM_PREV_PMDblClick(Sender: TObject);
    procedure P__DAWN_AM_ADblClick(Sender: TObject);
    procedure P__DAWN_AM_CDblClick(Sender: TObject);
    procedure P__DAWN_AM_NDblClick(Sender: TObject);
    procedure P__DAWN_PM_ADblClick(Sender: TObject);
    procedure P__DAWN_PM_CDblClick(Sender: TObject);
    procedure P__DAWN_PM_NDblClick(Sender: TObject);
    procedure P__DAYLIGHTDblClick(Sender: TObject);
    procedure P__DEVICESMouseEnter(Sender: TObject);
    procedure P__DEVICESMouseLeave(Sender: TObject);
    procedure P__GO_HOMEMouseEnter(Sender: TObject);
    procedure P__GO_HOMEMouseLeave(Sender: TObject);
    procedure P__GO_HOURANGLEMouseEnter(Sender: TObject);
    procedure P__GO_HOURANGLEMouseLeave(Sender: TObject);
    procedure P__MOONClick(Sender: TObject);
    procedure P__NIGHT_AMDblClick(Sender: TObject);
    procedure P__NIGHT_PMDblClick(Sender: TObject);
    procedure P__OBSCOOClick(Sender: TObject);
    procedure P__RECOM_LEFTClick(Sender: TObject);
    procedure P__RECOM_RIGHTClick(Sender: TObject);
    procedure P__SOLSYSClick(Sender: TObject);
    procedure P__SOLSYSPaint(Sender: TObject);
    procedure P__INCLPaint(Sender: TObject);
    procedure P__STARMAPMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure P__STARMAPMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure P__STARMAPMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure P__STARMAPPaint(Sender: TObject);
    procedure P__TABLESClick(Sender: TObject);
    procedure P__TABLESMouseEnter(Sender: TObject);
    procedure P__TABLESMouseLeave(Sender: TObject);
    procedure P__ZC_CONTROLClick(Sender: TObject);
    procedure RB__MESSIERChange(Sender: TObject);
    procedure RB__OCChange(Sender: TObject);
    procedure RB__COMETSChange(Sender: TObject);
    procedure RB__GCChange(Sender: TObject);
    procedure RB__GChange(Sender: TObject);
    procedure RB__NChange(Sender: TObject);
    procedure RB__PChange(Sender: TObject);
    procedure RB__PNChange(Sender: TObject);
    procedure RB__QChange(Sender: TObject);
    procedure RB__SChange(Sender: TObject);
    procedure RB__SClick(Sender: TObject);
    procedure SHP__ZINMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZOUTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__POS_SUNMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GRD__RECOM_MSDblClick(Sender: TObject);
    procedure SHP__ZDMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZDMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZRMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZRMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZUMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SHP__ZUMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__LTMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure TB__LTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__MAGKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TB__MAGMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TB__MAGMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__MAG_GKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure TB__MAG_GMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TB__MAG_GMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__SELMAGMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TB__SELMAG_GMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TB__SELMAG_GMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__SOLSYSMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__TIME_24HChange(Sender: TObject);
    procedure TB__TIME_24HMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TB__TIME_24HMouseEnter(Sender: TObject);
    procedure TB__TIME_24HMouseLeave(Sender: TObject);
    procedure TB__TIME_24HMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TIMER__GENMAPTimer(Sender: TObject);
    procedure TIMERTimer(Sender: TObject);
    procedure TIMER__AOTABLETimer(Sender: TObject);
    procedure TB__SELMAGMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TIMER__STRM_DATATimer(Sender: TObject);
    procedure TIMER__STRM_VIEWFLIPTimer(Sender: TObject);
    procedure TIMER__WHEELZOOMTimer(Sender: TObject);
  private
    { private declarations }
    miGLat_DEG: SmallInt;
    miGLat_MIN: SmallInt;
    mrSin_fGLat, mrCos_fGLat: Real;

    miGLng_DEG: SmallInt;
    miGLng_MIN: SmallInt;
    miDST_HH_DEF: SmallInt;
    miDST_HH: SmallInt;
    miUTC_HH: SmallInt;
    miBirthYear: Integer;
    miRefreshRateMinutes: SmallInt;

    miGLat_DEG_DEF: SmallInt;
    miGLat_MIN_DEF: SmallInt;
    mrSin_fGLat_DEF, mrCos_fGLat_DEF: Real;
    miGLng_DEG_DEF: SmallInt;
    miGLng_MIN_DEF: SmallInt;
    miUTC_HH_DEF: SmallInt;

    mdtStart, mdtStart0: TDateTime;

    msLANG_ID: string;
    msCountry, msCity, msDST: string;

    molMWData, molAOList: TObjectList; // List of all astronomical objects
    mslPlanetPlot: TStringList; // Store index of plotted planets in solar system view to delete the images fastly
    mslCometPlot: TStringList; // Store index of plotted comets in solar system view to delete the images fastly

    mslVisibleAOList: TStringList; // List of all visible astronomical objects
    molSignList: TObjectList; // List of astronomical constellations
    molADevList: TObjectList; // List of all astronomical devices
    //molObservationList: TObjectList; // List of Observations
    molMeteorShowers: TObjectList;
    miMSDoneMonth: Integer;
    mbHDust: Boolean; // Show Horizon Dust Effect
    miLThickness: ShortInt; // Visualisation line thickness
    msTinyStarFirstCatNo: string; // First *imported* tiny star id
    miTinyStarBlockWidth: Integer; // Width of tiny star block to jump over in GenStarMap();

    msUserPath: string;

    //SkyBitmap: Graphics.TBitmap;

    mbTimePlay: Boolean;
    miTimePlayMode: ShortInt;

    mrRise_HH, mrSet_HH: Real;
    mrDwn_AM_HH_A, mrDwn_PM_HH_A, mrDwn_AM_HH_C, mrDwn_PM_HH_C, mrDwn_AM_HH_N, mrDwn_PM_HH_N: Real;

    mbSM_HS: Boolean; // Switch height scale lines for azimutal reference system
    mbSM_AS: Boolean; // Switch azimut scale linies for azimutal reference system
    mbSM_MERI: Boolean; // Switch Meridian lines
    mbSM_GALACTIC: Boolean; // Switch galactic plane
    mbSM_EQUATOR: Boolean; // Switch equatorial plane
    mbSM_RA: Boolean; // RA-Scaling
    mbSM_DEC: Boolean; // RA-Scaling
    mbSM_ECLIPTIC: Boolean; // Switch ecliptic plane
    mbSM_CON: Boolean; // Switch constellations
    mbShowGalaxies: Boolean; // Show Galaxies
    mbShowQuasars: Boolean; // Show Quasars
    mbShowGlobularClusters: Boolean; // Show Globular Clusters
    mbShowGalacticNebula: Boolean; // Show Galactic Nebula
    mbShowPlanetaryNebula: Boolean; // Show Planetary Nebula
    mbShowOpenClusters: Boolean; // Show Open Clusters
    mbShowSun: Boolean; // Show Sun Position
    mbShowMoon: Boolean; // Show Moon Position
    mbShowCB: Boolean; // Show Constellation Boundaries
    mbShowEComets: Boolean; // Show elliptical comets
    mbShowMShower: Boolean; // Show meteor showers
    mbShowMessier: Boolean; // Show Messier objects
    miShowMilkyway: Smallint; // -1: Always visible, 0: Suppressed; 1: Realistic
    mbShowAsteroids: Boolean; // Show asteroid bodies
    //mbShowStarDescr: Boolean; // Show brightest star descriptions in starmap
    miShowStarDescr: SmallInt; // Show brightest star descriptions in starmap: 0: None, 1: Low, 2: High descriptive
    mbShowConstellations: Boolean;
    mbShowPoleP: Boolean; // Show pole precession path

    mbSearch: Boolean; // Search Mode
    mbSearchMsg: Boolean; // Display waring messages only 1 time per serach
    mbSearchFound: Boolean; // Search successful
    msSearch: string; // Search string

    miSMDisplayMode: SmallInt;

    mslCBList: TStringList; // Constellation Boundaries

    mfMagMin, mfMagMax: Real;
    mbADEVChanged: Boolean; // Astronomical device data changed
    molCBExt: TObjectList; // Contains additionally links between astronomical objects and constellations
    mrS0: Real; // Signal value of A0V-star, apparent magnitude 0

    miFirstStart: SmallInt;

    // Starmap Zoom Variables
    //miZoomLevel: Integer;
    mbZoomMode: Boolean;
    mrZoomHgtMax: Real;
    mrZoomHgtMin: Real;
    mrZoomAzMax: Real;
    mrZoomAzMin: Real;
    mrZoomX1, mrZoomY1, mrZoomX2, mrZoomY2: Real;
    miZoomLvl: Integer;
    mrMagPos: Real;
    mrMagPos_G: Real;
    mbWheeled: Boolean;

    mslAsteroidsCometsDisplayed: TStringList;

    miX_P, miY_P: Integer;
    mbPlanetShow: Boolean;
    mbPlanetImage: Boolean;

    msHorDir: string;
    miLandscapeNo: Integer; // Number of landscape
    msGotoOutputDir: string; // Goto output directory for control files

    miPlComPathWeeks: Integer; // -miPlComPathWeeks .. 0 .. +miPlComPathWeeks Weeks for Planet and Comet Paths
    mbChangedLoc: Boolean; // TRUE, if watching position is temporarily changed, else FALSE

    msRAVal, msDECVal: string;

    miStarmapView: Integer;
    miSkyType: Integer; // 2: Daysky, 1: Twilight, 0: Night Sky

    mslColType: TStringList; // 0: string column, 1: Numeric column
    miSCI_GRD_AO: Integer; // Sorted Column Index of grid GRD__AO
    miSCO_GRD_AO: Integer; // Sorted Column Order of grid GRD__AO

    //msAlbireoLocalDir: string; // Local directory to store processing data which can be modified by the user
    mslAOUserFields: TStringList;

    mslRecommendedPics: TStringList;
    miRecomIndex: Integer; // Recommended picture index
    miRecomRAUp, miRecomRADwn, miRecomDECDiff: Integer;
    mslDSImgList: TStringList; // NGC/IC..- numers of objects which have an image in InterstellarLib library

    mPng_O, mPng_B, mPng_A, mPng_F, mPng_G, mPng_K, mPng_M: TPortableNetworkGraphic;

    mslEclipses: TStringList;

    // Streaming support
    msStrmDir: string;
    miStrmDataPeriod: Integer;
    miStrmViewFlipPeriod: Integer;
    msStrmText1_DE, msStrmText1_EN, msStrmText2_DE, msStrmText2_EN, msStrmText3_DE, msStrmText3_EN: string;

    mbFastZoom: Boolean;
    mbExecZoom: Boolean; // Suppresses new left zoom co.ordinates as long ZoomExec is active!

    miAOTableSec: Integer;

    mrEyeFacH: Real; // Visual horizon view height

    {$IFDEF Darwin}
    mbOnStarmapPaintBusy: Boolean;
    mbOnSolSysPaintBusy: Boolean;
    {$ENDIF Darwin}

    function ShowPrefs(): Boolean;

    procedure CalcST(var iST_HH: Word;var iST_MM: Word; var iST_SS: Word; var iST_MS: Word);
    procedure TrigST();
    procedure SetTime(dtDateTime: TDateTime);
    function GetWTime(): TDateTime;
    procedure CalcHA(iST_HH, iST_MM, iST_SS, iRZ_HH, iRZ_MM, iRZ_SS: Word);
    procedure CalcRZ(iST_HH, iST_MM, iST_SS, iHA_HH, iHA_MM, iHA_SS: Word);
    procedure CalcAZ(iDEC_DEG, iDEC_MIN, iDEC_SS, iHA_HH, iHA_MIN, iHA_SS: SmallInt;
      var fAz: Real; var fHgt: Real);
    procedure SlideTime;
    procedure SetSlideTime();
    procedure CheckForUpdate(sHTML: string);
    function GetHTTPMetaValue(sHTML, sName: string): string;
    procedure SwitchLang();
    procedure SendUpdateReq;
    procedure SetupRemainingCB();
    procedure ImportCatalogs();
    procedure ImportCatalog_MS(sAOFileName: string);
    procedure ImportSigns(olSignList: TObjectList; sAOFileName: string);
    procedure SelSign();
    procedure ShowTable_S();
    procedure ShowTable_G();
    procedure ShowTable_Q;
    procedure ShowTable_GC;
    procedure ShowTable_N;
    procedure ShowTable_PN;
    procedure ShowTable_OC;
    procedure ShowTable_P;
    procedure ShowTable_C;
    procedure ShowTable_Messier;
    procedure ShowAOTable();
    function GetVisualHeight(dtTime, dtRA: TDateTime; rDEC: Real): Real;
    function RegisterAObject(iIndex: Integer): Boolean;
    procedure SetTimePanels();
    procedure SwitchTimePLAY();
    procedure SetMoonPanel;
    procedure ShowTimeStat();
    procedure SetNow();
    procedure GenMap(CustomControl: TCustomControl);
    procedure GenMapExec(CustomControl: TCustomControl);
    function GetStarIndex(sSym,sSign: string): Integer;
    procedure GenStarMap(WinControl: TWinControl); //; SkyBitmap: Graphics.TBitmap);
    procedure GenSigns(Panel: TPanel; iR0, iX0, iY0, iSkyType: Integer);
    function GetAObjectIndexByName(sName,sAOType: string): Integer;
    procedure ReCalcPlanetPos(dtDateTime: TDateTime);
    procedure ReCalcPlanetPath(dtDateTime: TDateTime; iWeeks: Integer);
    //procedure SelAObjects();
    procedure GenEquPath(Panel: TPanel; iR0, iX0, iY0: Integer; sType: string);
    procedure EnableMenu(sMenuName: string);
    procedure AOVisOnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure AOVisOnMouseEnter(Sender: TObject);
    //procedure AOVisOnMouseLeave(Sender: TObject);
    function NormalizeSearchArg(sText: string): string;
    function HighLightSearch(AObject: TAObject): Boolean;
    procedure GenCB(Panel: TPanel; iR0, iX0, iY0: Integer);
    procedure SetDefaultADev(ADevice: TADevice);
    procedure AddADev();
    procedure ModADev();
    procedure DelADev();
    function GetADev(sName: string; var iIndex: Integer): TADevice;
    procedure ShowADev(ADevice: TADevice);
    procedure IniCB__ADEV();
    procedure CalcADEVProp;
    procedure CalcMagProp;
    procedure EnableDevButtons();
    procedure ChangeADEVName(sNewName: string);
    procedure ShowLatLong();
    procedure ShowMeteoriteShowerGrid(dtDateTime: TDateTime);
    procedure ShowAO();
    procedure ValuateVisibility(AObject: TAObject; rHgt: Real; iRow: Integer; dtTime: TDateTime; iCol: Integer);
    procedure FindConRABoundary(rRA,rDEC: Real; iDirRA, iDirDEC: Integer; var slMatchedCon: TStringList);
    function CheckOnConRA(rRA, rDEC: Real; var slMatchedCon: TStringList): Boolean;
    function FindCon(rRA,rDEC: Real): TStringList;
    procedure RegListItems(slIn: TStringList;  bAnalyse: Boolean; var slRegID: TStringList; var slRegCnt: TStringList; var slRes: TStringList);
    procedure ExecDevices();
    procedure ExecAlbireo();
    procedure ExecMAP();
    procedure ExecTable();
    procedure ExecHA();
    procedure ExecSolSys();
    procedure AnimateButton(iMode: Integer; xLabel: TLabel);
    procedure IniADev();
    function GenAOSearch(i: Integer): TAOSearch;
    procedure StarMapDims(Control: TControl; var iR0: Integer; var iX0: Integer; var iY0: Integer);
    procedure ZoomModeTransformation(iR0: Integer; var rDX: Real; var rDY: Real);
    procedure ZoomModeTransformation_INV(iR0: Integer; var rDX: Real; var rDY: Real);
    function CanPlotAO(rAz, rHgt: Real): Boolean;
    procedure ActivateAOFeature(iAOFeature: Integer; bActive: Boolean);
    procedure ColorizeSky(Panel: TPanel; iSkyType,iR0,iX0,iY0: Integer);
    function ColorizeSkyBG(Control: TControl): Integer; //;var Bitmap: Graphics.TBitmap): Integer;
    function GetMeteorShower(sASName_DE: string): TMeteorShower;
    procedure SignActive(iSignIndex: Integer; bActive: Boolean);
    function GenMapPos(iR0, iX0, iY0: Integer; rAz, rHgt: Real; var iLeft: Integer; var iTop: Integer): Boolean;
    procedure GenAOLabel(AObject: TAObject; WinControl: TWinControl);
    procedure IniCB_ADEV_TYPE();
    procedure IniTelProp();
    procedure CleanStartOfStarmap();
    procedure IniAObjects();
    procedure IniAObjectsAll();
    procedure CalcPlanetOpCon(iYear: Integer; Planet: TPlanet;
        var dtOpdate: TDateTime; var dtConDate: TDateTime);
//        var slConList1: TStringList; var slConList2: TStringList);
    procedure IniCalcPlanetOpCon();
    procedure ShowSolSys();
    procedure SolSys();
    procedure CalcKochabMethod();
    procedure ShowAstroCalc(ADevice: TADevice);
    procedure PlotSolSysBodyPath(iR0, iX0, iY0: Integer; dtST: TDateTime;
      iIndex: Integer); //SkyBitMap: Graphics.TBitmap);
    procedure ShowAOVis(iIndex: Integer);
    procedure RegisterAsVisibleAO(i: Integer);
    procedure IniCountries();
    procedure GetHorPic(iSkyType: Integer);
    function GetSiderialTime(): TDateTime;
    function GetSiderialTimeExt(dtDateTime: TDateTime): TDateTime;
    function GetMapCooFromAO(olDataList: TObjectList; iIndex: Integer; dtST: TDateTime; var iLeft: Integer; var iTop: Integer): Boolean;
    function GetMapCooFromAOExt(olDataList: TObjectList; iIndex: Integer; dtST: TDateTime;
      var rAz: Real; var rHgt: Real;
      var iLeft: Integer; var iTop: Integer
      ): Boolean;
    procedure PrepMilkyway();
    procedure GenMilkyway(iSkyType: Integer);
    procedure PlotRing(Panel: TPanel; cColor: TColor; iR0,iX0,iY0, iThickness: Integer);
    procedure SetAOTableTitle(sAOType: string);
    procedure ShowMeridian(iX0, iY0: Integer);
    //procedure UpdateSolarSystem();
    procedure RecalcStarMap();
    procedure SetStarmapView(iViewMode: Integer);
    procedure SetDBSheetMaxMag(rMag1, rMag2: Real);
    procedure ShowDBAObject();
    procedure OpenADM();
    procedure PutDECVal(IAObject: TInterstellarObject; GRD: TStringGrid; iCol, iRow: Integer);
    procedure MeteorShowerMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AccTime(bForward: Boolean);
    procedure DateRefresh();
    procedure ShowPictureViewer(IMG: TImage);
    procedure ShowAstroVoidForm(sVoidName, sConst: string; iCenterDist, iSize: Integer);
    function GetAOIndexFromStarMap(): Integer;
    procedure SaveAOUserFields(sType: string; iAOIndex: Integer; sValue: string);
    procedure AssignAOUserFields();
    procedure SetStarImage(Star: TStar; iTop,iLeft: Integer);
    procedure TrackMagnitudes();
    procedure TrackMagnitudes_G();
    procedure SetMagInterval(var rLow: Real; var rHigh: Real);
    procedure SetP__MAGSIZE(bGalaxyTabMode: Boolean);
    procedure SetShapeImage(sAOType: string; Galaxy: TGalaxy; iTop,iLeft: Integer);
    procedure SetP__SELMAG(bGalaxyTabMode: Boolean);
    procedure ExecZoom(rX2, rY2: Real; iWheelDelta: Integer; bDoCenter: Boolean);
    procedure GenTimeScale(bCreateLabel: Boolean);
    procedure GetRecommendations(Date: TDateTime);
    procedure ShowRecomImg();
    procedure ReadDSImgList();
    function GetRandomPictureFile(): string;
    function GetRandomRecomPictureFile(var AObject: TAObject): string;
    function GetRandomRecomAObject(): TAObject;
    procedure SetTimeEdits(bPlus: Boolean; iStep: Integer);
    procedure SetTimeEditValues(bPlus: Boolean; iStep: Integer);
    procedure SetMainInfoData(Date: TDateTime);
    procedure ShowEclipsesForMonth(Date: TDateTime);
    procedure SetGenMapInterval(bSetFast: Boolean);
    procedure SetInterstellarObjectShapeColor(InterstellarObject: TInterstellarObject);
    procedure ShowStrmDlg();
    procedure TimerStrmData();
    procedure ToggleStrmDataNow();
    procedure ToggleStrmViewFlipNow();
    procedure TimerStrmViewFlip();
    procedure ToggleStrmLangFlipNow();
    procedure ToggleStrmFlipAll();
    function ShowHorCustDlg(): Integer;
    procedure ExecCustHorizons();
    procedure WheelTime(iWheelDelta: Integer);
    procedure WheelZoom(iX, iY, iWheelDelta: Integer; bDoCenter: Boolean);
    procedure ClearSearch();
    procedure SetTimePlayOFF();
    procedure ExecAOSearch();
    procedure ControlKochabPanel();
    procedure ControlCatinfoPanel();
    procedure ControlMagsizePanel();
    procedure Do_MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; bDoCenter: Boolean;
      var Handled: Boolean);
    procedure MoveZoom(iXProz,iYProz: Integer);
    procedure SwitchNavigation();
    procedure SwitchTimeControl();
    procedure InvalidateZoom();
    procedure ControlZoomPanel();
    procedure ShowHorizonMovePanels(X, Y: Integer);
    function GetSelAOTypeName(): string;
    function GetVisualObjectHeight(rAngle_Minutes: Real; iVisuFac: Integer): Integer;
    procedure IniSelTableView();
    procedure OnSelSpecClass(iClass,iType: SmallInt);
    function SLFilter(sSpecType: string): Boolean;
    procedure IniLiveTableMode();
    procedure OpenADBInfo();
    procedure RefreshApp();

  public

    { public declarations }

  end;

const
  cfRA_POLARIS = 2.530278/24;
  cfDEC_POLARIS = 89.26417;
  cfRA_KOCHAB = 14.845092/24;
  cfDEC_KOCHAB = 74.1555;

var
  F__ASTROTOOLBOX: TF__ASTROTOOLBOX;

implementation

{$R *.lfm}

{ TF__ASTROTOOLBOX }

procedure TF__ASTROTOOLBOX.RefreshApp();
begin
  case PC__WORKBENCH.ActivePageIndex of
  ciPAGE_STARMAP:
    CleanStartOfStarMap();
  ciPAGE_SOLSYS:
    ShowSolSys();
  ciPAGE_DB:
    ExecTable();
  end; // case

end;

procedure TF__ASTROTOOLBOX.OpenADBInfo();
begin
  F__ADBINFO := TF__ADBINFO.Create(nil);
  F__ADBINFO.msLANG_ID:=msLANG_ID;

  IniText(F__ADBINFO,msLANG_ID);

  F__ADBINFO.ShowModal;
  F__ADBINFO.Destroy;
end;

procedure TF__ASTROTOOLBOX.IniLiveTableMode();
begin
  CBX__LT.Checked:=false;
  TIMER__AOTABLE.Enabled:=false;
  L__LT_INTERVAL.Font.Color:=clGray;
  L__LT_INTERVAL.Caption:=IntToStr(TB__LT.Position) + ' Min.';
end;

procedure TF__ASTROTOOLBOX.OnSelSpecClass(iClass,iType: SmallInt);
begin
  case iClass of
    0: // Spectral Class
    begin
      if(MENU__SCLASS_O.Checked or
        MENU__SCLASS_B.Checked or
        MENU__SCLASS_A.Checked or
        MENU__SCLASS_F.Checked or
        MENU__SCLASS_G.Checked or
        MENU__SCLASS_K.Checked or
        MENU__SCLASS_M.Checked or
        MENU__SCLASS_CARBON.Checked or
        MENU__SCLASS_R.Checked or
        MENU__SCLASS_N.Checked or
        MENU__SCLASS_S.Checked or
        MENU__SCLASS_C.Checked or
        MENU__SCLASS_BD.Checked or
        MENU__SCLASS_L.Checked or
        MENU__SCLASS_T.Checked or
        MENU__SCLASS_Y.Checked
        )
      then
        MENU__SCLASS_ALL.Checked:=false
      else
        MENU__SCLASS_ALL.Checked:=true;

      // Carbon class selection
      if(
        MENU__SCLASS_R.Checked or
        MENU__SCLASS_N.Checked or
        MENU__SCLASS_S.Checked or
        MENU__SCLASS_C.Checked
      ) then
        MENU__SCLASS_CARBON.Checked := false;

      if(MENU__SCLASS_CARBON.Checked) then
      begin
        MENU__SCLASS_R.Checked := false;
        MENU__SCLASS_N.Checked := false;
        MENU__SCLASS_S.Checked := false;
        MENU__SCLASS_C.Checked := false;
      end;

      // Brown Dwarf class selection
      if(
        MENU__SCLASS_L.Checked or
        MENU__SCLASS_T.Checked or
        MENU__SCLASS_Y.Checked
      ) then
        MENU__SCLASS_BD.Checked := false;

      if(MENU__SCLASS_BD.Checked) then
      begin
        MENU__SCLASS_L.Checked := false;
        MENU__SCLASS_T.Checked := false;
        MENU__SCLASS_Y.Checked := false;
      end;

    end;
    1: // Luminance Class
    begin
      if(MENU__LCLASS_0.Checked or
        MENU__LCLASS_1.Checked or
        MENU__LCLASS_2.Checked or
        MENU__LCLASS_3.Checked or
        MENU__LCLASS_4.Checked or
        MENU__LCLASS_5.Checked or
        MENU__LCLASS_6.Checked or
        MENU__LCLASS_7.Checked)
      then
        MENU__LCLASS_ALL.Checked:=false
      else
        MENU__LCLASS_ALL.Checked:=true;
    end;
  end;

  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.IniSelTableView();
begin
  MENU__SHOW_MESSIERONLY.Checked := false;
  MENU__SHOW_MESSIERONLY.Enabled := false;

  MENU__SHOW_STARS.Checked := false;
  MENU__SHOW_STARS.Enabled := false;

  miSCI_GRD_AO := 0;
  miSCO_GRD_AO := -1;
end;

function TF__ASTROTOOLBOX.GetSelAOTypeName(): string;
// Returns name of selected astronomical type in radiobutton caption of the navigation area
begin
  if(RB__S.Checked) then
    Result := RB__S.Caption
  else if(RB__OC.Checked) then
    Result := RB__OC.Caption
  else if(RB__N.Checked) then
    Result := RB__N.Caption
  else if(RB__PN.Checked) then
    Result := RB__PN.Caption
  else if(RB__GC.Checked) then
    Result := RB__GC.Caption
  else if(RB__G.Checked) then
    Result := RB__G.Caption
  else if(RB__Q.Checked) then
    Result := RB__Q.Caption
  else if(RB__P.Checked) then
    Result := RB__P.Caption
  else if(RB__COMETS.Checked) then
    Result := RB__COMETS.Caption
  else
    Result := '';

end;

procedure TF__ASTROTOOLBOX.ShowHorizonMovePanels(X, Y: Integer);
begin
  L__BV_RIGHT.Visible := (X > P__LANDSCAPE.Width - 20);
  BV__RIGHT.Visible := (X > P__LANDSCAPE.Width - 20);

  L__BV_LEFT.Visible := (X < 20);
  BV__LEFT.Visible := (X < 20);

  L__BV_BOTTOM.Visible := (Y > P__LANDSCAPE.Height - 20);
  BV__BOTTOM.Visible := (Y > P__LANDSCAPE.Height - 20);

end;

procedure TF__ASTROTOOLBOX.ControlZoomPanel();
begin
  if(P__FASTZOOM_ITEMS.Visible) then
  begin
    P__FASTZOOM.Height := 25;
    P__FASTZOOM_ITEMS.Visible:=false;
    P__ZC_CONTROL.Caption:='[]';
  end
  else
  begin
    P__FASTZOOM.Height := 132;
    P__FASTZOOM_ITEMS.Visible:=true;
    P__ZC_CONTROL.Caption:='x';
  end;
end;

procedure TF__ASTROTOOLBOX.InvalidateZoom();
begin
  mbZoomMode := false;

  P__FASTZOOM.Visible := mbZoomMode;

  mrMagPos := crMagPosStd;
  mrMagPos_G := crMagPosStd_G;

  TB__MAG_G.Position:=Round(10*mrMagPos_G);
  L__MAGSIZE_G.Caption:=AnsiReplaceStr(FloatToStrF(mrMagPos_G,ffFixed,8,1),',','.');

  TB__MAG.Position:=Round(10*mrMagPos);
  L__MAGSIZE.Caption:=AnsiReplaceStr(FloatToStrF(mrMagPos,ffFixed,8,1),',','.');

  miZoomLvl := 0;
end;

procedure TF__ASTROTOOLBOX.SwitchTimeControl();
begin
  MENU__VIEW_TIMECONTROL.Checked := not MENU__VIEW_TIMECONTROL.Checked;

  P__SELECT.Visible:= MENU__VIEW_TIMECONTROL.Checked;

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
  begin
    InvalidateZoom();
    CleanStartOfStarmap();
  end;
end;


procedure TF__ASTROTOOLBOX.SwitchNavigation();
begin
  MENU_VIEW_NAVIGATION.Checked := not MENU_VIEW_NAVIGATION.Checked;
  P__NAVIG.Visible := MENU_VIEW_NAVIGATION.Checked;
  SB__MAIN.Visible := not SB__MAIN.Visible;

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
  begin
    InvalidateZoom();

    P__MAGSIZE.Visible := not P__MAGSIZE.Visible;
    P__SETTIME.Visible := not P__SETTIME.Visible;

    CleanStartOfStarmap();
  end;
end;

procedure TF__ASTROTOOLBOX.MoveZoom(iXProz,iYProz: Integer);
var
  rDX, rDY: Real;
begin
  rDX := (mrZoomX2 - mrZoomX1)*iXProz/100.0;
  rDY := (mrZoomY2 - mrZoomY1)*iYProz/100.0;

  if ((mrZoomX1 + rDX) >= 0) and ((mrZoomX2 + rDX) <= P__STARMAP.Width) and
     //((mrZoomY1 + rDY) >= 0) and ((mrZoomY2 + rDY) <= (P__STARMAP.Height - P__LANDSCAPE.Height)) and
     ((mrZoomY1 + rDY) >= 0) and ((mrZoomY2 + rDY) <= P__STARMAP.Height) and
    (mbZoomMode) then
  begin
    mrZoomX1 := mrZoomX1 + rDX;
    mrZoomX2 := mrZoomX2 + rDX;
    mrZoomY1 := mrZoomY1 + rDY;
    mrZoomY2 := mrZoomY2 + rDY;

    mbZoomMode := false;
    ExecZoom(mrZoomX2,mrZoomY2,-998,false);
  end;
end;

procedure TF__ASTROTOOLBOX.Do_MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; bDoCenter: Boolean;
  var Handled: Boolean);
begin
  //Handled := false;
  try
    if(not Handled) and (not mbWheeled) then
    begin
      if(ssShift in Shift) then
        WheelTime(WheelDelta)
      else
        WheelZoom(MousePos.X ,MousePos.Y, WheelDelta,bDoCenter);

    end;
  finally
    Handled := true;
    mbWheeled := true;
  end;
end;

procedure TF__ASTROTOOLBOX.ControlMagsizePanel();
begin
  if(PC__MAG.Visible) then
  begin
    P__MAGSIZE.Align:=alNone;
    P__MAGSIZE.Left:=P__STARMAP.Width - P__MAGSIZE_TITLE.Width - 2;
    P__MAGSIZE.Height := 25;
    PC__MAG.Visible:=false;
    P__MAGSIZE_CONTROL.Caption:='[]';
  end
  else
  begin
    P__MAGSIZE.Align:=alRight;
    PC__MAG.Visible:=true;
    P__MAGSIZE_CONTROL.Caption:='x';
  end;

  CleanStartOfStarMap();
  //GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.ControlCatinfoPanel();
begin
  if(P__CATINFO_ITEMS.Visible) then
  begin
    P__CATINFO.Height := 25;
    P__CATINFO_ITEMS.Visible:=false;
    P__CATINFO_CONTROL.Caption:='[]';
  end
  else
  begin
    P__CATINFO.Height := 208;
    P__CATINFO_ITEMS.Visible:=true;
    P__CATINFO_CONTROL.Caption:='x';
  end;
end;

procedure TF__ASTROTOOLBOX.ControlKochabPanel();
begin
  if(P__KOCHABMETHOD.Visible) then
  begin
    P__KOCHAB.Height := 32;
    P__KOCHABMETHOD.Visible:=false;
    P__KOCHAB_CONTROL.Caption:='[]';
  end
  else
  begin
    P__KOCHAB.Height := 300;
    P__KOCHABMETHOD.Visible:=true;
    P__KOCHAB_CONTROL.Caption:='x';
  end;
end;

procedure TF__ASTROTOOLBOX.ExecAOSearch();
var
  bCheckSearchText: Boolean;
  iLen: Integer;
  sQStr,sNormStr: string;
begin

  sNormStr := Trim(Uppercase(ED__SEARCH.Text));

  iLen := Length(sNormStr);

  if(iLen = 0) then
    exit;

  SetTimePlayOFF(); // Avoid starmap autorecalc during (extended) search!

  if(iLen = 2) and (LeftStr(sNormStr,1) = 'M') then
  begin
    sQStr := 'M -> Messier M00..?';

    if(MessageDlg('Requester',sQStr,mtConfirmation,[mbYes,mbNo],0) = mrYes) then
      sNormStr := AnsiReplaceStr(sNormStr,'M','M00');

    iLen := Length(sNormStr);
  end;

  if(iLen = 3) and (LeftStr(sNormStr,1) = 'M') then
  begin
    sQStr := 'M -> Messier M0..?';

    if(MessageDlg('Requester',sQStr,mtConfirmation,[mbYes,mbNo],0) = mrYes) then
      sNormStr := AnsiReplaceStr(sNormStr,'M','M0');

    iLen := Length(sNormStr);
  end;

  bCheckSearchText :=  (iLen > 3);

  if(bCheckSearchText) then
  begin
    if(iLen < 6) and (LeftStr(sNormStr,2) = 'HD') then
      bCheckSearchText := false;

    if(iLen < 6) and (LeftStr(sNormStr,2) = 'HR') then
      bCheckSearchText := false;

    if(iLen < 7) and (LeftStr(sNormStr,3) = 'TYC') then
      bCheckSearchText := false;

    if(iLen < 6) and (LeftStr(sNormStr,3) = 'HIP') then
      bCheckSearchText := false;

    if(iLen < 6) and (LeftStr(sNormStr,2) = 'BD') then
      bCheckSearchText := false;

    if(iLen < 6) and (LeftStr(sNormStr,2) = 'CD') then
      bCheckSearchText := false;


    if(iLen < 8) and (AnsiContainsStr(sNormStr,'MAS')) then
      bCheckSearchText := false;

    if(iLen < 7) and (AnsiContainsStr(sNormStr,'ESO')) then
      bCheckSearchText := false;

    if(iLen < 8) and (AnsiContainsStr(sNormStr,'MCG')) then
      bCheckSearchText := false;

    if(sNormStr = 'NGC') then
      bCheckSearchText := false;

    if(sNormStr = 'IC') then
      bCheckSearchText := false;

  end;

  if(not bCheckSearchText) then
  begin
    if(msLANG_ID = 'DE') then
      MessageDlg('Suchanfrage fehlerhaft','Suchanfrage zu unspezifisch bzw. ungültig',mtError,[mbOK],0)
    else
      MessageDlg('Search request fault','Non-specific query / not valid query',mtError,[mbOK],0);

  end;

  if  (bCheckSearchText) then
  begin
    P__SEARCHALL.Height := 150;
    LB__SEARCHRES.Items.Clear;
    LB__SEARCHRES.Visible:=true;

    //Normalise Search string
    msSearch := Trim(sNormStr);

    msSearch := msSearch + ' ';

    if(LeftStr(msSearch,4) = 'ALF ') then
      msSearch := AnsiReplaceStr(msSearch,'ALF ','ALPHA');

    if(LeftStr(msSearch,4) = 'BET ') then
      msSearch := AnsiReplaceStr(msSearch,'BET ','BETA');

    if(LeftStr(msSearch,4) = 'GAM ') then
      msSearch := AnsiReplaceStr(msSearch,'GAM ','GAMMA');

    if(LeftStr(msSearch,4) = 'DEL ') then
      msSearch := AnsiReplaceStr(msSearch,'DEL ','DELTA');

    if(LeftStr(msSearch,4) = 'EPS ') then
      msSearch := AnsiReplaceStr(msSearch,'EPS ','EPSILON');

    if(LeftStr(msSearch,4) = 'ZET ') then
      msSearch := AnsiReplaceStr(msSearch,'ZET ','ZETA');

    if(LeftStr(msSearch,4) = 'KAP ') then
      msSearch := AnsiReplaceStr(msSearch,'KAP ','KAPPA');

    if(LeftStr(msSearch,4) = 'LAM ') then
      msSearch := AnsiReplaceStr(msSearch,'LAM ','LAMBDA');

    if(LeftStr(msSearch,4) = 'SIG ') then
      msSearch := AnsiReplaceStr(msSearch,'SIG ','SIGMA');

    if(LeftStr(msSearch,4) = 'JOT ') then
      msSearch := AnsiReplaceStr(msSearch,'JOT ','IOTA');

    if(LeftStr(msSearch,4) = 'IOT ') then
      msSearch := AnsiReplaceStr(msSearch,'IOT ','IOTA');

    if(LeftStr(msSearch,4) = 'OMI ') then
      msSearch := AnsiReplaceStr(msSearch,'OMI ','OMIKRON');

    if(LeftStr(msSearch,4) = 'YPS ') then
      msSearch := AnsiReplaceStr(msSearch,'YPS ','YPSILON');

    if(LeftStr(msSearch,4) = 'OME ') then
      msSearch := AnsiReplaceStr(msSearch,'OME ','OMEGA');

    (*
    msSearch := AnsiReplaceStr(msSearch,' ','');
    msSearch := AnsiReplaceStr(msSearch,'-','');
    msSearch := AnsiReplaceStr(msSearch,'/','');
    *)
    msSearch := AnsiReplaceStr(msSearch,'MESSIER','M');
    msSearch := NormalizeSearchArg(msSearch);

    mbSearch := true;
    mbSearchMsg := true;

    GenMap(P__STARMAP);
  end;
end;

procedure TF__ASTROTOOLBOX.SetTimePlayOFF();
begin
  if(mbTimePlay) then
  begin
    SwitchTimePlay();

    if(msLANG_ID = 'DE') then
      L__BOTTOM_INFO.Caption:= csREALTIMESTOPPED_DE
    else
      L__BOTTOM_INFO.Caption:= csREALTIMESTOPPED_EN;

    Application.ProcessMessages;
  end;
end;

procedure TF__ASTROTOOLBOX.ClearSearch();
begin
  ED__SEARCH.Text:='';
  P__SEARCHALL.Height := 35;
  LB__SEARCHRES.Clear;
  LB__SEARCHRES.Visible:=false;

  Application.ProcessMessages;

  if(not mbSearch) then exit;

  mbSearch := false;
  msSearch := '';
end;

procedure TF__ASTROTOOLBOX.WheelZoom(iX, iY, iWheelDelta: Integer; bDoCenter: Boolean);
begin
  ExecZoom(iX ,iY, iWheelDelta,bDoCenter);
end;

procedure TF__ASTROTOOLBOX.WheelTime(iWheelDelta: Integer);
begin
  SetTimePlayOFF();

  if not ((ED__WT_HH.Focused) or (ED__WT_MM.Focused) or (ED__WT_SS.Focused)) then
    ED__WT_HH.SetFocus;

  if(iWheelDelta > 0) then
  begin
    P__FORWARD_PLUS.Tag := 1;
    P__REWIND_MINUS.Tag := 0;
  end
  else
  begin
    P__FORWARD_PLUS.Tag := 0;
    P__REWIND_MINUS.Tag := 1;
  end;

  SetTimeEdits(iWheelDelta > 0,1);

end;

procedure TF__ASTROTOOLBOX.ExecCustHorizons();
var
  iCustLandscape: Integer;
begin
  iCustLandscape := ShowHorCustDlg();

  if(iCustLandscape > -1) then
  begin
    miLandscapeNo := iCustLandscape;
    MENU__HOR_0.Checked:=false;
    MENU__HOR_1.Checked:=false;
    MENU__HOR_2.Checked:=false;
    MENU__HOR_3.Checked:=true;
    GenMap(P__STARMAP);
  end;
end;

function TF__ASTROTOOLBOX.ShowHorCustDlg(): Integer;
begin
  Result := -1;

  F__HOR_CUST := TF__HOR_CUST.Create(nil);
  F__HOR_CUST.msLANG_ID := msLANG_ID;

  IniText(F__HOR_CUST,msLANG_ID);

  if(F__HOR_CUST.ShowModal = mrOK) then
  begin
    Result := ciLandscapeNoCust + F__HOR_CUST.mslHCImageFiles.Count -1;
  end;

  F__HOR_CUST.Destroy;
end;

procedure TF__ASTROTOOLBOX.ToggleStrmFlipAll();
begin
  ToggleStrmDataNow();
  ToggleStrmViewFlipNow();
  ToggleStrmLangFlipNow();
end;

procedure TF__ASTROTOOLBOX.ToggleStrmLangFlipNow();
begin
  MENU__STRM_LANGFLIP.Checked := not MENU__STRM_LANGFLIP.Checked;
end;

procedure TF__ASTROTOOLBOX.TimerStrmViewFlip();
begin
  case TIMER__STRM_VIEWFLIP.Tag of
    0: // Total -> S (2)
    begin
      SetStarmapView(2);
      TIMER__STRM_VIEWFLIP.Tag := 2;
    end; // case
    1: // N (1) -> W (4)
    begin
      SetStarmapView(4);
      TIMER__STRM_VIEWFLIP.Tag := 4;
    end; // case
    2: // S (2) -> O (3)
    begin
      SetStarmapView(3);
      TIMER__STRM_VIEWFLIP.Tag := 3;
    end; // case
    3: // O (3) -> N (1)
    begin
      SetStarmapView(1);
      TIMER__STRM_VIEWFLIP.Tag := 1;
    end; // case
    4: // W (4) -> T (0)
    begin
      // Switch Language flip
      if(MENU__STRM_LANGFLIP.Checked) then
        SwitchLang();

      SetStarmapView(0);
      TIMER__STRM_VIEWFLIP.Tag := 0;
    end;
  end; // case
end;

procedure TF__ASTROTOOLBOX.ToggleStrmViewFlipNow();
begin
  TIMER__STRM_VIEWFLIP.Interval := 1000 * miStrmViewFlipPeriod;

  TIMER__STRM_VIEWFLIP.Enabled := not TIMER__STRM_VIEWFLIP.Enabled;
  MENU__STRM_VIEWFLIP.Checked:=TIMER__STRM_VIEWFLIP.Enabled;

end;

procedure TF__ASTROTOOLBOX.ToggleStrmDataNow();
begin
  TIMER__STRM_DATA.Interval := 1000 * miStrmDataPeriod;

  TIMER__STRM_DATA.Enabled := not TIMER__STRM_DATA.Enabled;
  MENU__STRM_NOW.Checked:=TIMER__STRM_DATA.Enabled;

end;

procedure TF__ASTROTOOLBOX.TimerStrmData();
var
  dJulDat: Double;
  sRandomRecomFile, sNGC: string;
  AObject: TAObject;
  sLabel, sText: string;
  bHasName, bHasMessier: Boolean;
begin
  dJulDat := 0;

  WriteTextFile(DateTimeToStr(Now),msStrmDir + '\' + csCurTime);

  WriteTextFile(TimeToStr(GetSIDTime(Now,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat)),msStrmDir + '\' + csSidTime);
  WriteTextFile(FloatToStr(dJulDat),msStrmDir + '\' + csJulTime);

  WriteTextFile(L__SUN_RISE.Caption,msStrmDir + '\' + csSunRiseToday);
  WriteTextFile(L__SUN_SET.Caption,msStrmDir + '\' + csSunSetToday);

  WriteTextFile(L__MOON_RISE.Caption,msStrmDir + '\' + csMoonRiseToday);
  WriteTextFile(L__MOON_SET.Caption,msStrmDir + '\' + csMoonSetToday);

  if(msLANG_ID = 'DE') then
  begin
    WriteTextFile(msStrmText1_DE,msStrmDir + '\' + csText1);
    WriteTextFile(msStrmText2_DE,msStrmDir + '\' + csText2);
    WriteTextFile(msStrmText3_DE,msStrmDir + '\' + csText3);
  end
  else
  begin
    WriteTextFile(msStrmText1_EN,msStrmDir + '\' + csText1);
    WriteTextFile(msStrmText2_EN,msStrmDir + '\' + csText2);
    WriteTextFile(msStrmText3_EN,msStrmDir + '\' + csText3);
  end;

  AObject := nil;

  // Copy random recommended picture file into streaming dir
  sRandomRecomFile := GetRandomRecomPictureFile(AObject);
  FileUtil.CopyFile(sRandomRecomFile,msStrmDir + '\RandRecomFile.jpg');

  // Big in OBS reading longer text content - empty file at first!
  sLabel := '';
  WriteTextFile(sLabel,msStrmDir + '\' + csRandomRecomNGC);
  Sleep(2000);

  if(AObject <> nil) then
  begin
    bHasName := false; bHasMessier := false;

    if(msLANG_ID = 'DE') then
    begin
      if((AObject as TInterstellarObject).sName_DE <> '') then
        sLabel := ' ' + (AObject as TInterstellarObject).sName_DE;
    end
    else
    begin
      if((AObject as TInterstellarObject).sName_EN <> '') then
        sLabel := ' ' + (AObject as TInterstellarObject).sName_EN;
    end;

    bHasName := (sLabel <> '');
    bHasMessier := false;

    if ((AObject as TInterstellarObject).sMessier <> '') then
    begin
      if (sLabel <> '') then sLabel := sLabel + ' ';
      sText := (AObject as TInterstellarObject).sMessier;
      sText := AnsiReplaceStr(sText,'M00','M');
      sText := AnsiReplaceStr(sText,'M0','M');
      sLabel := sLabel + sText;
      bHasMessier := true;
    end;

    // Shorten Output (OBS fault by reading text): Do not NGC, if object has no special name or object has a special name and no messier no.
    if ((not bHasName) or (bHasName and (not bHasMessier))) and ((AObject as TInterstellarObject).sNGC <> '') then
    begin
      if (sLabel <> '') then
        sLabel := sLabel + ' (' + AnsiReplaceStr((AObject as TInterstellarObject).sNGC,'NGC','NGC ') + ')'
      else
        sLabel := sLabel + AnsiReplaceStr((AObject as TInterstellarObject).sNGC,'NGC','NGC ');

    end;

    if((AObject as TInterstellarObject).sCon <> '') then
    begin
      if (sLabel <> '') then
      begin
        // Write Sign only for Messier- or other identified object
        sLabel := sLabel + ' , ' + GetSignName((AObject as TInterstellarObject).sCon,molSignList,msLANG_ID);
      end;
    end;

    if(sLabel <> '') then
      WriteTextFile(sLabel,msStrmDir + '\' + csRandomRecomNGC);

    // Prepare more object detail data...
    if((AObject as TInterstellarObject).rDist_XLY > 0) then
    begin
      Sleep(1000); // Wait after showing more general description data

      if(msLANG_ID = 'DE') then
      begin
        if((AObject.sAOType = 'G') or (AObject.sAOType = 'Q')) then
          WriteTextFile('Entfernung: ' + FloatToStrF((AObject as TInterstellarObject).rDist_XLY,ffFixed,8,1) + ' MLJ',msStrmDir + '\' + csAObjectDist)
        else
          WriteTextFile('Entfernung: ' + FloatToStrF((AObject as TInterstellarObject).rDist_XLY,ffFixed,8,1) + ' LJ',msStrmDir + '\' + csAObjectDist);
      end
      else
      begin
        if((AObject.sAOType = 'G') or (AObject.sAOType = 'Q')) then
          WriteTextFile('Distance: ' + FloatToStrF((AObject as TInterstellarObject).rDist_XLY,ffFixed,8,1) + ' MLY',msStrmDir + '\' + csAObjectDist)
        else
          WriteTextFile('Distance: ' + FloatToStrF((AObject as TInterstellarObject).rDist_XLY,ffFixed,8,1) + ' LY',msStrmDir + '\' + csAObjectDist);
      end
    end
    else WriteTextFile('',msStrmDir + '\' + csAObjectDist);

  end
  else
  begin
    sNGC := AnsiReplaceStr(ExtractFileName(sRandomRecomFile),'.jpeg','');
    sNGC := Trim(AnsiReplaceStr(sNGC,'NGC','NGC '));

    if(sNGC <> '') then
      WriteTextFile(sNGC,msStrmDir + '\' + csRandomRecomNGC);
  end;

end;

procedure TF__ASTROTOOLBOX.ShowStrmDlg();
var
  IniFile: TIniFile;
begin
  F__STREAMING := TF__STREAMING.Create(nil);
  F__STREAMING.msLANG_ID := msLANG_ID;

  F__STREAMING.ED__STRM_DATADIR.Text := msStrmDir;
  F__STREAMING.TB__STRM_PERIOD.Position:=miStrmDataPeriod;
  F__STREAMING.TB__STRM_VIEWFLIP.Position:=miStrmViewFlipPeriod;

  F__STREAMING.ED__TEXT_1_DE.Text := msStrmText1_DE;
  F__STREAMING.ED__TEXT_1_EN.Text := msStrmText1_EN;
  F__STREAMING.ED__TEXT_2_DE.Text := msStrmText2_DE;
  F__STREAMING.ED__TEXT_2_EN.Text := msStrmText2_EN;
  F__STREAMING.ED__TEXT_3_DE.Text := msStrmText3_DE;
  F__STREAMING.ED__TEXT_3_EN.Text := msStrmText3_EN;

  IniText(F__STREAMING,msLANG_ID);

  if(F__STREAMING.ShowModal = mrOK) then
  begin
    msStrmDir := F__STREAMING.ED__STRM_DATADIR.Text;
    miStrmDataPeriod := F__STREAMING.TB__STRM_PERIOD.Position;
    miStrmViewFlipPeriod := F__STREAMING.TB__STRM_VIEWFLIP.Position;

    msStrmText1_DE := F__STREAMING.ED__TEXT_1_DE.Text;
    msStrmText1_EN := F__STREAMING.ED__TEXT_1_EN.Text;
    msStrmText2_DE := F__STREAMING.ED__TEXT_2_DE.Text;
    msStrmText2_EN := F__STREAMING.ED__TEXT_2_EN.Text;
    msStrmText3_DE := F__STREAMING.ED__TEXT_3_DE.Text;
    msStrmText3_EN := F__STREAMING.ED__TEXT_3_EN.Text;

    // Save to inifile
    IniFile := TIniFile.Create(gsAlbireoLocalDir + 'ATB.ini');

    IniFile.WriteString('STREAM','STRMDIR',msStrmDir);
    IniFile.WriteInteger('STREAM','STRMPERIOD',miStrmDataPeriod);
    IniFile.WriteInteger('STREAM','STRMVIEWFLIPPERIOD',miStrmViewFlipPeriod);
    IniFile.WriteString('STREAM','STRMTEXT1_DE',msStrmText1_DE);
    IniFile.WriteString('STREAM','STRMTEXT1_EN',msStrmText1_EN);
    IniFile.WriteString('STREAM','STRMTEXT2_DE',msStrmText2_DE);
    IniFile.WriteString('STREAM','STRMTEXT2_EN',msStrmText2_EN);
    IniFile.WriteString('STREAM','STRMTEXT3_DE',msStrmText3_DE);
    IniFile.WriteString('STREAM','STRMTEXT3_EN',msStrmText3_EN);

    IniFile.Destroy;
  end;

  F__STREAMING.Free;
end;

procedure TF__ASTROTOOLBOX.SetInterstellarObjectShapeColor(InterstellarObject: TInterstellarObject);
{ Shape Colors:
Messier: Yellow
NGC: Fuchsia
IC: Blue
UGC: Green
ESO: Skyblue
2MASX: White
L__MCG: Black
Other: Gray
}
begin
  if(InterstellarObject.sMessier <> '') then
    InterstellarObject.SHP.Pen.Color := clYellow
  else if (LeftStr(InterstellarObject.sNGC,3) = 'NGC') then
    InterstellarObject.SHP.Pen.Color := clFuchsia
  else if (LeftStr(InterstellarObject.sNGC,2) = 'IC') then
    InterstellarObject.SHP.Pen.Color := clBlue
  else if (LeftStr(InterstellarObject.sNGC,3) = 'UGC') then
    InterstellarObject.SHP.Pen.Color := clGreen
  else if (LeftStr(InterstellarObject.sNGC,3) = 'ESO') then
    InterstellarObject.SHP.Pen.Color := clSkyBlue
  else if (LeftStr(InterstellarObject.sNGC,4) = '2MAS') then
    InterstellarObject.SHP.Pen.Color := clWhite
  else if (LeftStr(InterstellarObject.sNGC,3) = 'MCG') then
    InterstellarObject.SHP.Pen.Color := clSilver
  else
    InterstellarObject.SHP.Pen.Color := clGray;

end;

procedure TF__ASTROTOOLBOX.SetGenMapInterval(bSetFast: Boolean);
begin
  if(bSetFast) then
  begin
    TIMER__GENMAP.Interval := ciGraphFastRefreshTime
  end
  else
  begin
    TIMER__GENMAP.Interval := ciGraphNormalRefreshTime;
  end;

end;

procedure TF__ASTROTOOLBOX.ShowEclipsesForMonth(Date: TDateTime);
var
  i: Integer;
  iYear, iMonth, iYearSel, iMonthSel: Word;
  SE: TSunEclipse;
  ME: TMoonEclipse;
begin
  if(msLANG_ID = 'DE') then
  begin
    GRD__RECOM_ECL.RowCount := 1;
    GRD__RECOM_ECL.Cells[0,GRD__RECOM_ECL.RowCount-1] := 'Zeit (UTC)';
    GRD__RECOM_ECL.Cells[1,GRD__RECOM_ECL.RowCount-1] := 'Finsternis';
    GRD__RECOM_ECL.Cells[2,GRD__RECOM_ECL.RowCount-1] := 'Typ';
    GRD__RECOM_ECL.Cells[3,GRD__RECOM_ECL.RowCount-1] := 'Dauer';
    GRD__RECOM_ECL.Cells[4,GRD__RECOM_ECL.RowCount-1] := 'Ort';
    L__ECL.Caption := 'Sonnen- und Mondfinsternisse im ' + FormatDateTime('MMMM YYYY',Date)
  end
  else
  begin
    GRD__RECOM_ECL.RowCount := 1;
    GRD__RECOM_ECL.Cells[0,GRD__RECOM_ECL.RowCount-1] := 'Time (UTC)';
    GRD__RECOM_ECL.Cells[1,GRD__RECOM_ECL.RowCount-1] := 'Eclipse';
    GRD__RECOM_ECL.Cells[2,GRD__RECOM_ECL.RowCount-1] := 'Type';
    GRD__RECOM_ECL.Cells[3,GRD__RECOM_ECL.RowCount-1] := 'Duration';
    GRD__RECOM_ECL.Cells[4,GRD__RECOM_ECL.RowCount-1] := 'Location';
    L__ECL.Caption := 'Sun and Moon Eclipses in ' + FormatDateTime('MMMM YYYY',Date);
  end;

  GRD__RECOM_ECL.ColWidths[4] := 400;

  if(mslEclipses = nil) then
    exit;

  GRD__RECOM_ECL.RowCount := GRD__RECOM_ECL.RowCount + 1;

  iYearSel := YearOf(Date);
  iMonthSel := MonthOf(Date);

  for i:=0 to mslEclipses.Count-1 do
  begin
    iYear := YearOf((mslEclipses.Objects[i] as TEclipse).dtDateTime);
    iMonth := MonthOf((mslEclipses.Objects[i] as TEclipse).dtDateTime);

    if(iYear = iYearSel) and (iMonth = iMonthSel) then
    begin
      GRD__RECOM_ECL.Cells[0,GRD__RECOM_ECL.RowCount-1] := DateTimeToStr((mslEclipses.Objects[i] as TEclipse).dtDateTime);

      if((mslEclipses.Objects[i] as TEclipse).sEclType = 'S') then
      begin
        if(msLANG_ID = 'DE') then
          GRD__RECOM_ECL.Cells[1,GRD__RECOM_ECL.RowCount-1] := 'Sonnenfinsternis'
        else
          GRD__RECOM_ECL.Cells[1,GRD__RECOM_ECL.RowCount-1] := 'Sun Eclipse';

        SE := (mslEclipses.Objects[i] as TSunEclipse);

        GRD__RECOM_ECL.Cells[2,GRD__RECOM_ECL.RowCount-1] := TranslateEclStr(msLANG_ID,SE.sType,'S','TYPE');

        if(SE.dtDurationT > 0) then
          GRD__RECOM_ECL.Cells[3,GRD__RECOM_ECL.RowCount-1] := FormatDateTime('hh:mm:ss',SE.dtDurationT)
        else
          GRD__RECOM_ECL.Cells[3,GRD__RECOM_ECL.RowCount-1] := '-';

        GRD__RECOM_ECL.Cells[4,GRD__RECOM_ECL.RowCount-1] := TranslateEclStr(msLANG_ID,SE.sLoc,'S','LOC');
      end;

      if((mslEclipses.Objects[i] as TEclipse).sEclType = 'M') then
      begin
        if(msLANG_ID = 'DE') then
          GRD__RECOM_ECL.Cells[1,GRD__RECOM_ECL.RowCount-1] := 'Mondfinsternis'
        else
          GRD__RECOM_ECL.Cells[1,GRD__RECOM_ECL.RowCount-1] := 'Moon Eclipse';

        ME := (mslEclipses.Objects[i] as TMoonEclipse);
        GRD__RECOM_ECL.Cells[2,GRD__RECOM_ECL.RowCount-1] := TranslateEclStr(msLANG_ID,ME.sType,'M','TYPE');

        if(ME.dtDurationPE > 0) then
          GRD__RECOM_ECL.Cells[3,GRD__RECOM_ECL.RowCount-1] := FormatDateTime('hh:mm:ss',ME.dtDurationPE)
        else
          GRD__RECOM_ECL.Cells[3,GRD__RECOM_ECL.RowCount-1] := '-';

        GRD__RECOM_ECL.Cells[4,GRD__RECOM_ECL.RowCount-1] := TranslateEclStr(msLANG_ID,ME.sLoc,'M','LOC');
      end;

      GRD__RECOM_ECL.RowCount := GRD__RECOM_ECL.RowCount + 1;
    end;

  end;
  GRD__RECOM_ECL.RowCount := GRD__RECOM_ECL.RowCount - 1;
end;

procedure TF__ASTROTOOLBOX.SetMainInfoData(Date: TDateTime);
var
  rMoonR_HH, rMoonS_HH, rHgtMax: Real;
  dtTimeMR,dtTimeMS,dtTimeCul: TDateTime;
begin
  dtTimeMR:=0;dtTimeMS:=0;dtTimeCul:=0;rHgtMax:=0;

  CalcMoonRiseAndMoonSet(Date,
    miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,
    mrSin_fGLat, mrCos_fGLat,
    dtTimeMR,dtTimeMS,dtTimeCul,rHgtMax);

  rMoonR_HH := (dtTimeMR - Trunc(dtTimeMR))*24;
  rMoonS_HH := (dtTimeMS - Trunc(dtTimeMS))*24;

  if(Trunc(Date) = Trunc(Now)) then
  begin
    if(msLANG_ID = 'DE') then
    begin
      L__ALBIREO.Caption:='Heute';
      TS__RECOM_P0.Caption:='Heute';
      P__ALBIREO_INFO.Caption:= FormatDateTime('d. mmmm yyyy',Date);
    end
    else
    begin
      L__ALBIREO.Caption:='Today';
      TS__RECOM_P0.Caption:='Today';
      P__ALBIREO_INFO.Caption:= FormatDateTime('MMMM DD, YYYY',Date);
    end;
  end
  else
  begin
    if(msLANG_ID = 'DE') then
    begin
      L__ALBIREO.Caption := FormatDateTime('d. mmmm yyyy',Date);
      TS__RECOM_P0.Caption := FormatDateTime('d. mmmm yyyy',Date);
      P__ALBIREO_INFO.Caption:= FormatDateTime('d. mmmm yyyy',Date);
    end
    else
    begin
      L__ALBIREO.Caption := FormatDateTime('MMMM DD, YYYY',Date);
      TS__RECOM_P0.Caption := FormatDateTime('MMMM DD, YYYY',Date);
      P__ALBIREO_INFO.Caption:= FormatDateTime('MMMM DD, YYYY',Date);
    end;
  end;

  L__SUN_RISE.Caption := IntToStr(Trunc(mrRise_HH)) + ':' + Format('%.2d',[Trunc((mrRise_HH - Trunc(mrRise_HH))*60)]);
  L__SUN_SET.Caption := IntToStr(Trunc(mrSet_HH)) + ':' + Format('%.2d',[Trunc((mrSet_HH - Trunc(mrSet_HH))*60)]);

  L__MOON_RISE.Caption := IntToStr(Trunc(rMoonR_HH)) + ':' + Format('%.2d',[Trunc((rMoonR_HH - Trunc(rMoonR_HH))*60)]);
  L__MOON_SET.Caption := IntToStr(Trunc(rMoonS_HH)) + ':' + Format('%.2d',[Trunc((rMoonS_HH - Trunc(rMoonS_HH))*60)]);

end;

procedure TF__ASTROTOOLBOX.SetTimeEditValues(bPlus: Boolean; iStep: Integer);
begin
  if bPlus then
  begin
    if(ED__WT_HH.Focused) then
    begin
      if (StrToInt(ED__WT_HH.Text) < 24 - iStep) then
       begin
         ED__WT_HH.Text := format('%.2d',[StrToInt(ED__WT_HH.Text) + iStep]);
       end
       else
       begin
         CB__WT.Date := CB__WT.Date + 1;
         ED__WT_HH.Text := '00';
       end;
       ED__WT_MM.Text := '00';
       ED__WT_SS.Text := '00';
    end
    else if(ED__WT_MM.Focused) then
    begin
      if (StrToInt(ED__WT_MM.Text) < 60 - iStep) then
       begin
         ED__WT_MM.Text := format('%.2d',[StrToInt(ED__WT_MM.Text) + iStep]);
       end
       else
       begin
         if(StrToInt(ED__WT_HH.Text) < 23) then
           ED__WT_HH.Text := format('%.2d',[StrToInt(ED__WT_HH.Text) + 1])
         else
         begin
           ED__WT_HH.Text := '00';
           CB__WT.Date := CB__WT.Date + 1;
         end;
         ED__WT_MM.Text := '00';
       end;
       ED__WT_SS.Text := '00';
    end
    else if(ED__WT_SS.Focused) then
    begin
      if (StrToInt(ED__WT_SS.Text) < 60 - iStep) then
         ED__WT_SS.Text := format('%.2d',[StrToInt(ED__WT_SS.Text) + iStep]);

    end;
    (*
    case P__FORWARD_PLUS.Tag of
    1:
    begin
      if (StrToInt(ED__WT_HH.Text) < 24 - iStep) then
       begin
         ED__WT_HH.Text := format('%.2d',[StrToInt(ED__WT_HH.Text) + iStep]);
       end
       else
       begin
         CB__WT.Date := CB__WT.Date + 1;
         ED__WT_HH.Text := '00';
       end;
       ED__WT_MM.Text := '00';
       ED__WT_SS.Text := '00';
    end;
    2:
    begin
      if (StrToInt(ED__WT_MM.Text) < 60 - iStep) then
       begin
         ED__WT_MM.Text := format('%.2d',[StrToInt(ED__WT_MM.Text) + iStep]);
       end
       else
       begin
         if(StrToInt(ED__WT_HH.Text) < 23) then
           ED__WT_HH.Text := format('%.2d',[StrToInt(ED__WT_HH.Text) + 1])
         else
         begin
           ED__WT_HH.Text := '00';
           CB__WT.Date := CB__WT.Date + 1;
         end;
         ED__WT_MM.Text := '00';
       end;
       ED__WT_SS.Text := '00';
    end;
    3:
      if (StrToInt(ED__WT_SS.Text) < 60 - iStep) then
         ED__WT_SS.Text := format('%.2d',[StrToInt(ED__WT_SS.Text) + iStep]);

    end; // case..
    *)
  end
  else
  begin
    if(ED__WT_HH.Focused) then
    begin
      if (StrToInt(ED__WT_HH.Text) >= 0 + iStep) then
       begin
         ED__WT_HH.Text := format('%.2d',[StrToInt(ED__WT_HH.Text) - iStep]);
       end
       else
       begin
         CB__WT.Date := CB__WT.Date - 1;
         ED__WT_HH.Text := '23';
       end;
       ED__WT_MM.Text := '00';
       ED__WT_SS.Text := '00';
    end
    else if(ED__WT_MM.Focused) then
    begin
      if (StrToInt(ED__WT_MM.Text) >= 0 + iStep) then
       begin
         ED__WT_MM.Text := format('%.2d',[StrToInt(ED__WT_MM.Text) - iStep]);
       end
       else
       begin
         if(StrToInt(ED__WT_HH.Text) > 0) then
           ED__WT_HH.Text := format('%.2d',[StrToInt(ED__WT_HH.Text) - 1])
         else
         begin
           ED__WT_HH.Text := '59';
           CB__WT.Date := CB__WT.Date - 1;
         end;

         ED__WT_MM.Text := '59';
       end;

       ED__WT_SS.Text := '00';
    end
    else if(ED__WT_SS.Focused) then
    begin
      if (StrToInt(ED__WT_SS.Text) >= 0 + iStep) then
         ED__WT_SS.Text := format('%.2d',[StrToInt(ED__WT_SS.Text) - iStep]);

    end;
  end; // if..

end;

procedure TF__ASTROTOOLBOX.SetTimeEdits(bPlus: Boolean; iStep: Integer);
begin
  ClearSearch();

  if(not mbTimePlay) then
  begin
    if(mbZoomMode) then // If zoomed move with smaller 10 minutes steps
    begin
     P__FORWARD_PLUS.Tag := 2;
     P__REWIND_MINUS.Tag := 2;
     iStep := 10;
    end
    else
    begin
      P__FORWARD_PLUS.Tag := 1;
      P__REWIND_MINUS.Tag := 1;
    end;

    SetTimeEditValues(bPlus,iStep);
    SetSlideTime();
    TrigST();

    case PC__WORKBENCH.ActivePageIndex of
      ciPAGE_DB: ShowAOTable();
      ciPAGE_STARMAP: RecalcStarMap();
    end;

  end
  else
    AccTime(bPlus);

end;

function TF__ASTROTOOLBOX.GetRandomPictureFile(): string;
var
  iRnd: Integer;
begin
  Result := '';
  if(mslDSImgList = nil) then
    exit;

  Randomize();

  iRnd := Random(mslDSImgList.Count);

  if(iRnd >= 0) and (iRnd < mslDSImgList.Count) then
    Result := gsAlbireoLocalDir + 'img\InterstellarLib\' + mslDSImgList[iRnd] + '.jpeg';

end;


function TF__ASTROTOOLBOX.GetRandomRecomAObject(): TAObject;
var
  iRnd: Integer;
begin
  Result := nil;

  if(mslRecommendedPics = nil) or (mslRecommendedPics.Count = 0) then
    exit;

  Randomize();

  iRnd := Random(mslRecommendedPics.Count);

  if(iRnd >= 0) and (iRnd < mslRecommendedPics.Count) then
    Result := mslRecommendedPics.Objects[iRnd] as TInterstellarObject;

end;

function TF__ASTROTOOLBOX.GetRandomRecomPictureFile(var AObject: TAObject): string;
var
  iRnd: Integer;
begin
  Result := '';
  AObject := nil;

  if(mslRecommendedPics = nil) then
    exit;

  Randomize();

  iRnd := Random(mslRecommendedPics.Count);

  if(iRnd >= 0) and (iRnd < mslRecommendedPics.Count) then
  begin
    Result := mslRecommendedPics[iRnd];
    AObject := (mslRecommendedPics.Objects[iRnd] as TAObject);
  end;

end;

procedure TF__ASTROTOOLBOX.ReadDSImgList();
var
  Info : TSearchRec;
  sFileName, sFileExt, sNGC: string;
begin
  mslDSImgList.Clear;

  If (SysUtils.FindFirst (gsAlbireoLocalDir + 'img\InterstellarLib\' + '*.jpeg',faAnyFile,Info) = 0) then
  begin
    repeat
      sFileName := ExtractFileName(Info.Name);
      sFileExt := ExtractFileExt(sFileName);
      sNGC := AnsiReplaceStr(sFileName,sFileExt,'');
      mslDSImgList.Add(sNGC);
    until (SysUtils.FindNext(info)<>0);
    SysUtils.FindClose(Info);
  end;

end;

procedure TF__ASTROTOOLBOX.ShowRecomImg();
var
  sLabel, sNGC, sMessier: string;
  iRAUp, iRADwn: Integer;
begin
  try
    Screen.Cursor := crHourGlass;

    if(miRecomIndex < mslRecommendedPics.Count) then
    begin
      if(msLANG_ID = 'DE') then
        sLabel:= (mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sName_DE
      else
        sLabel:= (mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sName_EN;

      sLabel := Trim(sLabel);
      sNGC := (mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sNGC;

      sNGC := AnsiReplaceStr(sNGC,'NGC','NGC ');
      sNGC := AnsiReplaceStr(sNGC,'IC','IC ');
      sNGC := AnsiReplaceStr(sNGC,'Cr','Cr ');
      sNGC := AnsiReplaceStr(sNGC,'Abell','Abell ');
      sNGC := AnsiReplaceStr(sNGC,'Barnard','Barnard ');

      sMessier := (mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sMessier;

      if(sMessier <> '') then
      begin
        sNGC := AnsiReplaceStr(sMessier,'M','Messier ') + ' - ' + sNGC;
      end;

      if(sLabel <> '') then
        P__RECOM_TITLE.Caption := sLabel + ' (' + sNGC + ')'
      else
        P__RECOM_TITLE.Caption := sNGC;

      IMG__RECOMMEND.Picture.LoadFromFile(mslRecommendedPics[miRecomIndex]);

      GRD__RECOM_PROP.RowCount:=1;

      if(msLANG_ID = 'DE') then
        GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'Daten'
      else
        GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'Data';

      GRD__RECOM_PROP.RowCount := GRD__RECOM_PROP.RowCount + 1;

      GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'RA';
      GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
        Format('%.2d',[(mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).iRA_Hours]) + ':' +
        Format('%.2d',[abs((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).iRA_Min)]) + ':' +
        Format('%.2d',[Trunc(abs((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).rRA_Sec))]);
      GRD__RECOM_PROP.RowCount := GRD__RECOM_PROP.RowCount + 1;

      GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'DEC';
      GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
        Format('%.2d',[(mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).iDec_Deg]) + ':' +
        Format('%.2d',[abs((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).iDec_Min)]) + ':' +
        Format('%.2d',[Trunc(abs((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).rDec_Sec))]);
      GRD__RECOM_PROP.RowCount := GRD__RECOM_PROP.RowCount + 1;

      if(msLANG_ID = 'DE') then
      begin
        GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'Sternbild:';
      end
      else
      begin
        GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'Constellation:';
      end;

      GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
        (mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sCon;
      GRD__RECOM_PROP.RowCount := GRD__RECOM_PROP.RowCount + 1;


      if(msLANG_ID = 'DE') then
      begin
        GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'Winkelausdehnung:';
      end
      else
      begin
        GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'Angular size:';
      end;

      if((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sAOType = 'G') then
      GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
        FloatToStrF((mslRecommendedPics.Objects[miRecomIndex] as TGalaxy).rVisDim1,ffFixed,8,1) + 'x' +
        FloatToStrF((mslRecommendedPics.Objects[miRecomIndex] as TGalaxy).rVisDim2,ffFixed,8,1) + ' qarcs'
      else if((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sAOType = 'GC') then
        GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
          FloatToStrF((mslRecommendedPics.Objects[miRecomIndex] as TGlobularCluster).rVisDiam,ffFixed,8,1) + ''''''
      else if((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sAOType = 'N') then
        GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
        IntToStr((mslRecommendedPics.Objects[miRecomIndex] as TNebula).iVisDim1) + 'x' +
        IntToStr((mslRecommendedPics.Objects[miRecomIndex] as TNebula).iVisDim2) + ' qarcs'
      else if((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sAOType = 'PN') then
        GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
        FloatToStrF((mslRecommendedPics.Objects[miRecomIndex] as TPNebula).rVisDim1,ffFixed,8,1) + 'x' +
        FloatToStrF((mslRecommendedPics.Objects[miRecomIndex] as TPNebula).rVisDim2,ffFixed,8,1) + ' qarcs'
      else if((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sAOType = 'OC') then
        GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
        IntToStr((mslRecommendedPics.Objects[miRecomIndex] as TOpenCluster).iDiam_M) + '''''';

      GRD__RECOM_PROP.RowCount := GRD__RECOM_PROP.RowCount + 1;


      if((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).rDist_XLY > 0) then
      begin
        GRD__RECOM_PROP.Cells[0,GRD__RECOM_PROP.RowCount-1] := 'Dist:';
        GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
          FloatToStrF((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).rDist_XLY,ffFixed,8,1);

        if((mslRecommendedPics.Objects[miRecomIndex] as TInterstellarObject).sAOType = 'G') then
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] + ' M'
        else
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] + ' ';


        if(msLANG_ID = 'DE') then
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] + 'Lj'
        else
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] :=
          GRD__RECOM_PROP.Cells[1,GRD__RECOM_PROP.RowCount-1] + 'Ly';


        GRD__RECOM_PROP.RowCount := GRD__RECOM_PROP.RowCount + 1;
      end;

      GRD__RECOM_PROP.RowCount := GRD__RECOM_PROP.RowCount - 1;

      if(miRecomRAUp >= 24.0) then
        iRAUp := miRecomRAUp - 24
      else
        iRAUp := miRecomRAUp;

      if(miRecomRADwn < 0) then
        iRADwn := miRecomRADwn + 24
      else
        iRADwn := miRecomRADwn;

      P__RECOM_RANGE.Caption := 'RA: ' + Format('%.2d',[iRAUp]) + ':00  -  ' + Format('%.2d',[iRADwn]) + ':00';
      if(msLANG_ID = 'DE') then
      begin
        if(miGLat_DEG > 0) then
          P__RECOM_RANGE.Caption := P__RECOM_RANGE.Caption + ' , DEC: Nördl. Pol  -  -' + IntToStr(miRecomDECDiff) + '°'
        else
          P__RECOM_RANGE.Caption := P__RECOM_RANGE.Caption + ' , DEC: Südl. Pol  -  ' + IntToStr(miRecomDECDiff) + '°'

      end
      else
      begin
        if(miGLat_DEG > 0) then
          P__RECOM_RANGE.Caption := P__RECOM_RANGE.Caption + ' , DEC: Northern Pole  -  -' + IntToStr(miRecomDECDiff) + '°'
        else
          P__RECOM_RANGE.Caption := P__RECOM_RANGE.Caption + ' , DEC: Southern Pole  -  ' + IntToStr(miRecomDECDiff) + '°'
      end;

    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TF__ASTROTOOLBOX.GetRecommendations(Date: TDateTime);
{22.09.2020/fs
Show DeepSky recommendations

// sPath := msAlbireoLocalDir + 'img\InterstellarLib\' + (mAObject as TInterstellarObject).sNGC + '.jpeg';
}
var
  i, iGCnt, iFound, iRA_Comp, iRA_Object: Integer;
  sAOType: string;
  //slDSImgList: TStringList;
  dtWatchTime,dtSTime: TDateTime;
  iYear,iMonth,iDay: Word;
  bInRange: Boolean;
  iDEC_HH_Object: SmallInt;
  bEastPos, bInner: Boolean;
begin
  mslRecommendedPics.Clear;
  miRecomIndex := -1;

  GRD__RECOM_P.RowCount := 2;

  GRD__RECOM_P.Cells[0,0] := 'Name';
  if(msLANG_ID = 'DE') then
  begin
    GRD__RECOM_P.Cells[1,0] := 'Typ';
    GRD__RECOM_P.Cells[3,0] := 'Sichtbarkeit';
  end
  else
  begin
    GRD__RECOM_P.Cells[1,0] := 'Type';
    GRD__RECOM_P.Cells[3,0] := 'Visibility';
  end;

  if(miGLat_DEG > 0) then
    miRecomDECDiff := 90 - miGLat_DEG
  else
    miRecomDECDiff := 90 + miGLat_DEG;

  GRD__RECOM_P.Cells[0,1] := '';
  GRD__RECOM_P.Cells[1,1] := '';

  iGCnt := 0;
  // Calc RA South at 00:00 current day
  DecodeDate(Trunc(Date+1),iYear,iMonth,iDay);
  dtWatchtime := EncodeDate(iYear,iMonth,iDay);
  dtSTime := GetSiderialTimeExt(dtWatchtime);

  iRA_Comp := Round(dtSTime*24.0);

  for i:=0 to molAOList.Count-1 do
  begin
    sAOType := (molAOList[i] as TAObject).sAOType;
    if(sAOType = 'G') or
      (sAOType = 'N') or
      (sAOType = 'PN') or
      (sAOType = 'GC') or
      (sAOType = 'OC') or
      (sAOType = 'P') then
    begin
      if(sAOType = 'G') then
      begin
        if(iGCnt > 800) then
          continue;

        Inc(iGCnt);
      end;

      iRA_Object := (molAOList[i] as TAObject).iRA_Hours;

      if(
        ((molAOList[i] as TAObject).sAOType = 'P') and
        ((molAOList[i] as TPlanet).bInnerPlanet)
        ) then
      begin
        // Venus & Mercury as morning/evening stars
        miRecomRAUp := iRA_Comp + 10;
        miRecomRADwn := iRA_Comp - 10;
        miRecomDECDiff := Round(miRecomDECDiff * 0.8); // Exclude deep horizon positions for inner planets
        bInner := true;
      end
      else // all others around midnight
      begin
        miRecomRAUp := iRA_Comp + 4;
        miRecomRADwn := iRA_Comp - 4;
        bInner := false;
      end;

      if(miRecomRAUp > 24) then
      begin
        bInRange := ((iRA_Object <= (miRecomRAUp - 24)) or (iRA_Object >= miRecomRADwn));
        bEastPos := (iRA_Object <= (miRecomRAUp - 24));
      end
      else if(miRecomRADwn < 0) then
      begin
        bInRange := ((iRA_Object <= miRecomRAUp) or (iRA_Object >= 24.0 + miRecomRADwn));
        bEastPos := (iRA_Object <= miRecomRAUp);
      end
      else
      begin
        bInRange := (iRA_Object <= miRecomRAUp) and (iRA_Object >= miRecomRADwn);
        bEastPos := (iRA_Object > iRA_Comp);
      end;

      // Check DEC visibility (N/S Sky)
      if(bInRange) then
      begin
        iDEC_HH_Object := (molAOList[i] as TAObject).iDec_Deg;
        if(miGLat_DEG > 0) then
          bInRange := (iDEC_HH_Object >= -miRecomDECDiff)
        else
          bInRange :=  (iDEC_HH_Object <= miRecomDECDiff);

      end;

      if(bInRange) then
      begin
        if(sAOType = 'P') then
        begin
          if(msLANG_ID = 'DE') then
          begin
            GRD__RECOM_P.Cells[0,GRD__RECOM_P.RowCount-1] := (molAOList[i] as TAObject).sName_DE;

            if((molAOList[i] as TPlanet).sPlanetType = 'P') then
              GRD__RECOM_P.Cells[1,GRD__RECOM_P.RowCount-1] := 'Planet'
            else if ((molAOList[i] as TPlanet).sPlanetType = 'p') then
              GRD__RECOM_P.Cells[1,GRD__RECOM_P.RowCount-1] := 'Zwergplanet'
            else
              GRD__RECOM_P.Cells[1,GRD__RECOM_P.RowCount-1] := 'Asteroid';

            if(bInner) then
            begin
              if(bEastPos) then
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := 'Morgenstern'
              else
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := 'Abendstern';
            end
            else
            begin
              if(bEastPos) then
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := '2. Nachthälfte'
              else
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := '1. Nachthälfte';
            end;

          end
          else
          begin
            GRD__RECOM_P.Cells[0,GRD__RECOM_P.RowCount-1] := (molAOList[i] as TAObject).sName_EN;

            if((molAOList[i] as TPlanet).sPlanetType = 'P') then
              GRD__RECOM_P.Cells[1,GRD__RECOM_P.RowCount-1] := 'Planet'
            else if ((molAOList[i] as TPlanet).sPlanetType = 'p') then
              GRD__RECOM_P.Cells[1,GRD__RECOM_P.RowCount-1] := 'Dwarf planet'
            else
              GRD__RECOM_P.Cells[1,GRD__RECOM_P.RowCount-1] := 'Asteroid';

            if(bInner) then
            begin
              if(bEastPos) then
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := 'Morning Star'
              else
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := 'Evening Star';
            end
            else
            begin
              if(bEastPos) then
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := '2nd half of night'
              else
                GRD__RECOM_P.Cells[3,GRD__RECOM_P.RowCount-1] := '1st half of night';
            end;
          end;

          GRD__RECOM_P.Cells[2,GRD__RECOM_P.RowCount-1] := IntToStr(i);
          GRD__RECOM_P.RowCount := GRD__RECOM_P.RowCount + 1;

        end
        else
        begin
          iFound := mslDSImgList.IndexOf((molAOList[i] as TInterstellarObject).sNGC);
          if(iFound > -1) then
            mslRecommendedPics.AddObject(gsAlbireoLocalDir + 'img\InterstellarLib\' + mslDSImgList[iFound] + '.jpeg',
              (molAOList[i] as TInterstellarObject));
        end;
      end;
    end;
  end;

  GRD__RECOM_P.RowCount := GRD__RECOM_P.RowCount - 1;

  // Show first recommended object
  if(mslRecommendedPics.Count > 0) then
  begin
    miRecomIndex := 0;
    ShowRecomImg();
  end;

end;

procedure TF__ASTROTOOLBOX.GenTimeScale(bCreateLabel: Boolean);
var
  i: Integer;
  L__TIME: TLabel;
begin
  for i:=0 to 24 do
  begin
    // Recall GenTimeScale: Find label component named 'L__TIME_..'. Only if not found then create.
    if(bCreateLabel) then
    begin
    L__TIME := TLabel.Create(F__ASTROTOOLBOX);
    L__TIME.Parent := P__DATETIME_VISU;
    L__TIME.Font.Color:=clSilver;
    L__TIME.Font.Style:=[];
    L__TIME.Font.Size:=10;
    L__TIME.Font.Color:=clRed;
    L__TIME.Top := 32;
    L__TIME.Left := i*((P__DATETIME_VISU.Width div 24)-1);
    L__TIME.Name:='L__TIME_' + IntToStr(i);

    if(i < 10) then
      L__TIME.Caption:='0' + IntToStr(i) + ':00'
    else
      L__TIME.Caption:=IntToStr(i) + ':00';

    end
    else
    begin
      L__TIME := (FindComponentExt(F__ASTROTOOLBOX,'L__TIME_' + IntToStr(i)) as TLabel);
      if(L__TIME <> nil) then
      begin
        L__TIME.Left := i*((P__DATETIME_VISU.Width div 24)-1);
      end;
    end;
  end;
end;

procedure TF__ASTROTOOLBOX.ExecZoom(rX2, rY2: Real; iWheelDelta: Integer; bDoCenter: Boolean);
// iWheelDelat < 0: To me - Zoom In
// iWheelDeta > 0: From me - Zoom Out
var
  i, iR0, iX0, iY0: Integer;
  raAz, raAlt: array[1..4] of Real;
  rZoomX1, rZoomY1, rZoomX2, rZoomY2: Real;
  rFac, rMagDiff: Real;
  rMoveToCenterX, rMoveToCenterY: Real;
  rDHGT, rDAZ, rSS: Real;
  iDEG,iMM: SmallInt;
begin

  mbExecZoom := true;

  SetTimePlayOFF();

  mbFastZoom := false;

  iR0:=0;iX0:=0;iY0:=0;

  if(miStarmapView = 0) then
    rMagDiff := 0.2
  else
    rMagDiff := 0.4;

  rFac := 0.9;

  if(iWheelDelta < 0) and (iWheelDelta > -998) and (mbZoomMode) then
  begin
    // Zoom In
    Inc(miZoomLvl);

    // Adjust Y due to hidden horizon (enlarged width of P__STARMAP!) - only for the first ADDITIONAL zoom (miZoomInLv = 2)
    if(miStarmapView > 0) and (miZoomLvl = 2) then
    begin
      mrZoomY1 := Round(mrZoomY1 / ((P__STARMAP.Height - P__LANDSCAPE.Height - P__HORDUST.Height)/P__STARMAP.Height) );
      mrZoomY2 := Round(mrZoomY2 / ((P__STARMAP.Height - P__LANDSCAPE.Height - P__HORDUST.Height)/P__STARMAP.Height) );
    end;

    rZoomX1 := (mrZoomX1 + mrZoomX2)/2.0 - rFac*(mrZoomX2 - mrZoomX1)/2.0;
    rZoomY1 := (mrZoomY1 + mrZoomY2)/2.0 - rFac*(mrZoomY2 - mrZoomY1)/2.0;
    rZoomX2 := (mrZoomX1 + mrZoomX2)/2.0 + rFac*(mrZoomX2 - mrZoomX1)/2.0;
    rZoomY2 := (mrZoomY1 + mrZoomY2)/2.0 + rFac*(mrZoomY2 - mrZoomY1)/2.0;

    mrZoomX1 := rZoomX1; mrZoomY1 := rZoomY1;
    mrZoomX2 := rZoomX2; mrZoomY2 := rZoomY2;

    if(bDoCenter) then
    begin
      // Center current mouse position
      rMoveToCenterX := (rX2 - (P__STARMAP.Width div 2))/(P__STARMAP.Width div 2) * (mrZoomX2 - mrZoomX1)/2.0;
      rMoveToCenterY := (rY2 - (P__STARMAP.Height div 2))/(P__STARMAP.Height div 2) * (mrZoomY2 - mrZoomY1)/2.0;

      mrZoomX1 := mrZoomX1 + rMoveToCenterX;
      mrZoomX2 := mrZoomX2 + rMoveToCenterX;

      mrZoomY1 := mrZoomY1 + rMoveToCenterY;
      mrZoomY2 := rZoomY2 + rMoveToCenterY;
    end;

    mrZoomY2 := mrZoomY1 + P__STARMAP.Height*(mrZoomX2 - mrZoomX1)/P__STARMAP.Width;

    if(mrMagPos < crMagPosStdZoom + 2) then
      mrMagPos := mrMagPos + rMagDiff;

    if(mrMagPos_G < crMagPosStdZoom_G + 2) then
      mrMagPos_G := mrMagPos_G + rMagDiff;

    mbFastZoom := true;
  end
  else if(iWheelDelta <= -998) or (not mbZoomMode) then
  begin
    if(not mbZoomMode) then
    begin
      mrZoomX2 := rX2;
      mrZoomY2 := rY2;

      //Memo1.Lines.Add('miZoomLvl: ' + IntToStr(miZoomLvl));

      if(iWheelDelta = -998) and (miStarmapView > 0) and (miZoomLvl = 1) then
      begin
        // Recalc Y-Interval for interval movement after first zoom
        mrZoomY1 := Round(mrZoomY1 / ((P__STARMAP.Height - P__LANDSCAPE.Height)/P__STARMAP.Height) );
        mrZoomY2 := Round(mrZoomY2 / ((P__STARMAP.Height - P__LANDSCAPE.Height)/P__STARMAP.Height) );
        Inc(miZoomLvl);
      end;

      if(iWheelDelta = -999) then miZoomLvl := 1;

      if(abs(iWheelDelta) < 998) and (iWheelDelta < 0) then // < 0 (e.g. -120) wheeled IN (wheeled to me)
      begin
        // Started whith wheeled zoome:
        mrZoomX1 := mrZoomX1 - 100;
        mrZoomX2 := mrZoomX1 + 200;

        mrZoomY1 := mrZoomY1 - 100;
        mrZoomY2 := mrZoomY1 + 200;

        miZoomLvl := 1;
      end;

      mrMagPos := crMagPosStdZoom;
      mrMagPos_G := crMagPosStdZoom_G;

    end
    else
    begin
      InvalidateZoom();
      CleanStartOfStarmap();
      mbExecZoom := false;
      exit;
    end;
  end
  else if(iWheelDelta > 0) and (mbZoomMode) then
  begin
    // Zoom out
    //Dec(miZoomLvl);
    if(miZoomLvl > 1) then
    begin
      rZoomX1 := (mrZoomX1 + mrZoomX2)/2.0 - 1.0/rFac*(mrZoomX2 - mrZoomX1)/2.0;
      rZoomY1 := (mrZoomY1 + mrZoomY2)/2.0 - 1.0/rFac*(mrZoomY2 - mrZoomY1)/2.0;
      rZoomX2 := (mrZoomX1 + mrZoomX2)/2.0 + 1.0/rFac*(mrZoomX2 - mrZoomX1)/2.0;
      //rZoomY2 := (mrZoomY1 + mrZoomY2)/2.0 + 1.0/rFac*(mrZoomY2 - mrZoomY1)/2.0;

      mrZoomX1 := rZoomX1; mrZoomY1 := rZoomY1;
      mrZoomX2 := rZoomX2; // mrZoomY2 := rZoomY2;

      mrZoomY2 := mrZoomY1 + P__STARMAP.Height*(mrZoomX2 - mrZoomX1)/P__STARMAP.Width;

      if(mrMagPos > crMagPosStdZoom) then
        mrMagPos := mrMagPos - rMagDiff;

      if(mrMagPos_G > crMagPosStdZoom_G) then
        mrMagPos_G := mrMagPos_G - rMagDiff;

      mbFastZoom := true;
    end
    else
    begin
      // Back to un-zoomed starmap, if minus-zoom is pressed directly after the first zoom
      InvalidateZoom();
      CleanStartOfStarmap();
      mbExecZoom := false;
      exit;
    end;
  end;

  for i:=Low(raAz) to High(raAz) do // raAz dims equals raAlt dims!
  begin
    raAz[i] := 0; raAlt[i] := 0;
  end;

  if(miStarmapView = 0) then
  begin
    StarMapDims(P__STARMAP,iR0,iX0,iY0);
    MouseCooToHorizon(iX0,iY0,iR0,mrZoomX1,mrZoomY1,raAz[1],raAlt[1]);
    MouseCooToHorizon(iX0,iY0,iR0,mrZoomX1,mrZoomY2,raAz[2],raAlt[2]);
    MouseCooToHorizon(iX0,iY0,iR0,mrZoomX2,mrZoomY1,raAz[3],raAlt[3]);
    MouseCooToHorizon(iX0,iY0,iR0,mrZoomX2,mrZoomY2,raAz[4],raAlt[4]);
  end
  else
  begin
    (*
    Memo1.Lines.Add('');
    Memo1.Lines.Add('---');
    Memo1.Lines.Add('mrZoomX1: ' + FloatToStr(mrZoomX1) + ', mrZoomX2: ' + FloatToStr(mrZoomX2) + ' - mrZoomY1: ' + FloatToStr(mrZoomY1) + ', mrZoomY2: ' + FloatToStr(mrZoomY2));
    Memo1.Lines.Add('P__STARMAP.Height: ' + IntToStr(P__STARMAP.Height) + ' - mrZoomY1: ' + FloatToStr(mrZoomY1) + ', mrZoomY2: ' + FloatToStr(mrZoomY2));
    *)

    HMouseCooToHorizon(P__STARMAP.Width,P__STARMAP.Height,msHorDir,mrZoomX1,mrZoomY1,mrEyeFacH,raAz[1],raAlt[1]);
    HMouseCooToHorizon(P__STARMAP.Width,P__STARMAP.Height,msHorDir,mrZoomX1,mrZoomY2,mrEyeFacH,raAz[2],raAlt[2]);
    HMouseCooToHorizon(P__STARMAP.Width,P__STARMAP.Height,msHorDir,mrZoomX2,mrZoomY1,mrEyeFacH,raAz[3],raAlt[3]);
    HMouseCooToHorizon(P__STARMAP.Width,P__STARMAP.Height,msHorDir,mrZoomX2,mrZoomY2,mrEyeFacH,raAz[4],raAlt[4]);

    //Memo1.Lines.Add('raAlt1: ' + FloatToStr(raAlt[1]) + ', raAlt2: ' + FloatToStr(raAlt[2]) + ', raAlt3: ' + FloatToStr(raAlt[3]) + ', raAlt4: ' + FloatToStr(raAlt[4]));

  end;

  mrZoomAzMax := MaxFloatValue(raAz);
  mrZoomAzMin := MinFloatValue(raAz);
  mrZoomHgtMax := MaxFloatValue(raAlt);
  mrZoomHgtMin := MinFloatValue(raAlt);

  //Memo1.Lines.Add('mrZoomHgtMin: ' + FloatToStr(mrZoomHgtMin) + ', mrZoomHgtMax: ' + FloatToStr(mrZoomHgtMax) + ' - mrZoomAzMin: ' + FloatToStr(mrZoomAzMin) + ', mrZoomAzMax: ' + FloatToStr(mrZoomAzMax));

  mbZoomMode := (mrZoomHgtMax > mrZoomHgtMin) and (mrZoomAzMax > mrZoomAzMin) and (miZoomLvl > 0);
  //  and ((mrZoomHgtMax-mrZoomHgtMin) > 0) and (miZoomLvl > 0) ;
  //mbZoomMode := (mrZoomHgtMax > mrZoomHgtMin) and (mrZoomAzMax > mrZoomAzMin)
  //  and ((mrZoomHgtMax-mrZoomHgtMin) > 0) and (miZoomLvl > 0) ;


  P__FASTZOOM.Visible := mbZoomMode;

  if(not mbZoomMode) then
  begin
    InvalidateZoom();
    (*
    mbFastZoom := false;
    TB__MAG_G.Position:=Trunc(10*crMagPosStd_G);
    mrMagPos := crMagPosStd;
    mrMagPos_G := crMagPosStd_G;
    *)
  end
  else
  begin
    // Show Hgt/Az zoom window range
    rDHGT := mrZoomHgtMax - mrZoomHgtMin;

    iDEG:=0; iMM:=0; rSS:=0;
    DegToDEG_MM_SS2(rDHGT,iDEG,iMM,rSS);
    if(rSS < 10) then
      L__DELTA_HGT_ZOOM.Caption := 'H.: ' + Format('%.2d',[iDEG]) + ':' +
        Format('%.2d',[iMM]) +
        ':0' + FloatToStrF(rSS,ffFixed,8,3)
    else
      L__DELTA_HGT_ZOOM.Caption := 'H.: ' + Format('%.2d',[iDEG]) + ':' +
        Format('%.2d',[iMM]) +
        ':' + FloatToStrF(rSS,ffFixed,8,3);

    rDAZ := mrZoomAzMax-mrZoomAzMin;
    if(rDAZ > 180) then
      rDAZ := 360 - rDAZ;

    iDEG:=0; iMM:=0; rSS:=0;
    DegToDEG_MM_SS2(rDAz,iDEG,iMM,rSS);
    if(rSS < 10) then
      L__DELTA_AZ_ZOOM.Caption := 'A.: ' + Format('%.2d',[iDEG]) + ':' +
        Format('%.2d',[iMM]) +
        ':0' + FloatToStrF(rSS,ffFixed,8,3)
    else
      L__DELTA_AZ_ZOOM.Caption := 'A.: ' + Format('%.2d',[iDEG]) + ':' +
        Format('%.2d',[iMM]) +
        ':' + FloatToStrF(rSS,ffFixed,8,3);

  end;

  CleanStartOfStarmap();

  //ShowMessage(FloatToStr(mrMagPos));
  TB__MAG.Position:=Round(10*mrMagPos);
  L__MAGSIZE.Caption:=AnsiReplaceStr(FloatToStrF(mrMagPos,ffFixed,8,1),',','.');

  //if(mbShowGalaxies) then
  //begin
  TB__MAG_G.Position:=Round(10*mrMagPos_G);
  L__MAGSIZE_G.Caption:=AnsiReplaceStr(FloatToStrF(mrMagPos_G,ffFixed,8,1),',','.');
  //end;

  mbExecZoom := false;
end;

procedure TF__ASTROTOOLBOX.SetP__SELMAG(bGalaxyTabMode: Boolean);
var
  fLow, fHigh: Real;
begin
  fLow := 0; fHigh := 0;

  SetMagInterval(fLow,fHigh);

  TS__SELMAG_G.TabVisible:=bGalaxyTabMode;
  TS__SELMAG_S.TabVisible:=not bGalaxyTabMode;

  L__MAG_LOW.Caption:=FloatToStrF(fLow,ffFixed,8,1);
  L__MAG_HIGH.Caption:=FloatToStrF(fHigh,ffFixed,8,1);

end;

procedure TF__ASTROTOOLBOX.SetMagInterval(var rLow: Real; var rHigh: Real);
begin
  if(RB__G.Checked) then
  begin
    rHigh := 1.0*TB__SELMAG_G.Position/10.0;

    if(TB__SELMAG_G.Position <= 100) then
      rLow := 0.0
    else if(TB__SELMAG_G.Position <= 120) then
      rLow := 6.0
    else if(TB__SELMAG_G.Position <= 130) then
      rLow := 7.0
    else if(TB__SELMAG_G.Position <= 140) then
      rLow := 8.0
    else if(TB__SELMAG_G.Position <= 150) then
      rLow := 9.0
    else
      rLow := 10.0;

  end
  else if(RB__S.Checked) then
  begin
    rHigh := 1.0*TB__SELMAG.Position/10.0;

    if(TB__SELMAG.Position <= 70) then
      rLow := -2.0
    else if(TB__SELMAG.Position <= 80) then
      rLow := 7.0
    else if(TB__SELMAG.Position <= 85) then
      rLow := 8.0
    else if(TB__SELMAG.Position <= 90) then
      rLow := 8.5
    else if(TB__SELMAG.Position <= 93) then
      rLow := 9.0
    else if(TB__SELMAG.Position <= 95) then
      rLow := 9.3
    else if(TB__SELMAG.Position <= 97) then
      rLow := 9.5
    else if(TB__SELMAG.Position <= 98) then
      rLow := 9.6
    else if(TB__SELMAG.Position = 100) then // ALL other faint extra-stars
    begin
      rLow := 11.04; // below faintest regular stars
      rHigh := 23.0;
    end
    else
      rLow := rHigh - 0.1;

  end;

  L__MAG_LOW.Caption := FloatToStrF(rLow,ffFixed,8,2);
  L__MAG_HIGH.Caption := FloatToStrF(rHigh,ffFixed,8,2);

end;

procedure TF__ASTROTOOLBOX.SetP__MAGSIZE(bGalaxyTabMode: Boolean);
begin
  if(bGalaxyTabMode) then
  begin
    P__MAGSIZE.Parent := TS__TABLES; TS__MAG_GAL.TabVisible := true;
    TS__MAG_STARS.TabVisible := false;
  end
  else
  begin
    P__MAGSIZE.Parent := TS__MAP; TS__MAG_GAL.TabVisible := mbShowGalaxies;
    TS__MAG_STARS.TabVisible := true;
  end;
end;

procedure TF__ASTROTOOLBOX.TrackMagnitudes_G();
begin
  if((not mbZoomMode) and (TB__MAG_G.Position > 120)) then
  begin
    if(msLANG_ID = 'DE') then
    begin
      if(MessageDlg('Anwendungshinweis','Die Berechnung der Sternenkarte benötigt bei dieser Einstellung u.U. zuviel CPU, Arbeitsspeicher und Berechnungszeit. ' +
        #13 + #10 +
        'Möchten Sie wirklich fortfahren?',mtWarning,[mbYes,mbNo],0) = mrNo) then
        TB__MAG_G.Position := Round(crMagPosStd_G*10);
    end
    else
    begin
      if(MessageDlg('Application Note','To continue with the starmap representation, the computer could exhaust in CPU, memory and calcualtion time.' +
        #13 + #10 +
        'Do you really want to continue?',mtWarning,[mbYes,mbNo],0) = mrNo) then
        TB__MAG_G.Position := Round(crMagPosStd_G*10);
    end;

  end;

  mrMagPos_G := (TB__MAG_G.Position / 10.0);
  L__MAGSIZE_G.Caption:= AnsiReplaceStr(FloatToStrF(mrMagPos_G,ffFixed,8,1),',','.');

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
  begin
    CleanStartOfStarmap();
  end;
  //else if(PC__WORKBENCH.ActivePageIndex = ciPAGE_DB) and (RB__G.Checked) then
  //  ShowTable_G(0,10);

end;

procedure TF__ASTROTOOLBOX.TrackMagnitudes();
begin
  if((not mbZoomMode) and (TB__MAG.Position > 70)) then
  begin
    if(msLANG_ID = 'DE') then
    begin
      if(MessageDlg('Anwendungshinweis','Die Berechnung der Sternenkarte benötigt bei dieser Einstellung u.U. zuviel CPU, Arbeitsspeicher und Berechnungszeit. ' +
        #13 + #10 +
        'Möchten Sie wirklich fortfahren?',mtWarning,[mbYes,mbNo],0) = mrNo) then
        TB__MAG.Position := Round(crMagPosStd*10);
    end
    else
    begin
      if(MessageDlg('Application Note','To continue with the starmap representation, the computer could exhaust in CPU, memory and calcualtion time.' +
        #13 + #10 +
        'Do you really want to continue?',mtWarning,[mbYes,mbNo],0) = mrNo) then
        TB__MAG.Position := Round(crMagPosStd*10);
    end;

  end;

  mrMagPos := (TB__MAG.Position / 10.0) ;
  L__MAGSIZE.Caption:= AnsiReplaceStr(FloatToStrF(mrMagPos,ffFixed,8,1),',','.');

  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  PMENU__MMAG_9.Checked := false;
  PMENU__MMAG_9.Checked := false;
  PMENU__MMAG_9.Checked := false;

  case Trunc(mrMagPos) of
    0: PMENU__MMAG_0.Checked := true;
    1: PMENU__MMAG_1.Checked := true;
    2: PMENU__MMAG_2.Checked := true;
    3: PMENU__MMAG_3.Checked := true;
    4: PMENU__MMAG_4.Checked := true;
    5: PMENU__MMAG_5.Checked := true;
    6: PMENU__MMAG_6.Checked := true;
    7: PMENU__MMAG_7.Checked := true;
    8: PMENU__MMAG_8.Checked := true;
    9: PMENU__MMAG_9.Checked := true;
    (*
    10: PMENU__MMAG_9.Checked := true;
    11: PMENU__MMAG_9.Checked := true;
    12: PMENU__MMAG_9.Checked := true;
    *)
  end; // case

  CleanStartOfStarmap();

end;

procedure TF__ASTROTOOLBOX.SetShapeImage(sAOType: string; Galaxy: TGalaxy; iTop,iLeft: Integer);
begin
  if(sAOType <> 'G') then
    exit;

  Galaxy.SHP.ShowHint:=true;
  SetAOLabel(Galaxy,msLANG_ID);
  //Galaxy.SHP.Hint:=Galaxy.sLabel;

  Galaxy.SetSHPSize();

  Galaxy.SHP.Top := iTop;
  Galaxy.SHP.Left := iLeft;
  Galaxy.SHP.Visible:=true;
end;

procedure TF__ASTROTOOLBOX.SetStarImage(Star: TStar; iTop,iLeft: Integer);
begin
  PrepStarImage(Star.IMG);

  if(LeftStr(Star.sSpType,1) = 'O') then
    Star.IMG.Picture.Graphic := mPng_O
  else if(LeftStr(Star.sSpType,1) = 'B') then
    Star.IMG.Picture.Graphic := mPng_B
  else if(LeftStr(Star.sSpType,1) = 'A') then
    Star.IMG.Picture.Graphic := mPng_A
  else if(LeftStr(Star.sSpType,1) = 'F') then
    Star.IMG.Picture.Graphic := mPng_F
  else if(LeftStr(Star.sSpType,1) = 'G') then
    Star.IMG.Picture.Graphic := mPng_G
  else if(LeftStr(Star.sSpType,1) = 'K') then
    Star.IMG.Picture.Graphic := mPng_K
  else if(LeftStr(Star.sSpType,1) = 'M') then
    Star.IMG.Picture.Graphic := mPng_M
  else
    Star.IMG.Picture.Graphic := mPng_A;

  Star.IMG.ShowHint:=true;
  SetAOLabel(Star,msLANG_ID);

  Star.PrepareStar(mbZoomMode);

  Star.IMG.Top := iTop;
  Star.IMG.Left := iLeft;
end;

procedure TF__ASTROTOOLBOX.AssignAOUserFields();
var
  tfFile: TextFile;
  iAOIndex: Integer;
  slBuffer: TStringList;
  sFileName, sLine: string;
begin
  sFileName := ConvertWinPath(gsAlbireoLocalDir + 'AOUserFields.dat');

  if (not (FileExists(sFileName))) then
    exit;

  slBuffer := TStringList.Create;
  slBuffer.Delimiter:='~';
  slBuffer.StrictDelimiter:=true;
  slBuffer.QuoteChar := '"';

  AssignFile(tfFile,sFileName);
  Reset(tfFile);

  while not eof(tfFile) do
  begin
    slBuffer.Clear;

    ReadLn(tfFile,sLine);
    sLine := Trim(sLine);
    if(sLine <> '') then
    begin
      slBuffer.DelimitedText:=sLine;
      if(slBuffer.Count > 2) then
      begin
        iAOIndex := StrToInt(slBuffer[1]);
        if(Uppercase(slBuffer[0]) = 'COMMENT') then
        begin
          if(iAOIndex < molAOList.Count) then
          begin
            (molAOList[iAOIndex] as TAObject).sComment := slBuffer[2];
            mslAOUserFields.Add(slBuffer.QuoteChar + 'COMMENT' + slBuffer.QuoteChar + '~' +
            IntToStr(iAOIndex) + '~' +
            slBuffer.QuoteChar + slBuffer[2] + slBuffer.QuoteChar);
          end;
        end;
      end; // if slBuffer.Count..
    end; // if sLine <> ..
  end; // while...
  CloseFile(tfFile);

  //mslAOUserFields.Add(sType + '~' + IntToStr(iAOIndex) + '~' + sValue);
  slBuffer.Free;

end;

procedure TF__ASTROTOOLBOX.SaveAOUserFields(sType: string; iAOIndex: Integer; sValue: string);
var
  i, iFound: Integer;
  slBuffer: TStringList;
  sEntry: string;
begin
  iFound:=-1; i:=0;
  slBuffer := TStringList.Create;
  slBuffer.Delimiter:='~';
  slBuffer.StrictDelimiter:=true;
  slBuffer.QuoteChar:='"';

  sEntry := slBuffer.QuoteChar + sType + slBuffer.QuoteChar +
  '~' + IntToStr(iAOIndex) +
  '~' + slBuffer.QuoteChar + sValue + slBuffer.QuoteChar;

  // Find iAOIndex entry
  while (i < mslAOUserFields.Count) and (iFound = -1) do
  begin
    slBuffer.Clear;
    slBuffer.DelimitedText := mslAOUserFields[i];
    if(slBuffer.Count > 2) then
    begin
      if(iAOIndex = StrToInt(slBuffer[1])) and (slBuffer[0] = sType) then
      begin
        iFound := i;
      end;
    end;

    if(iFound = -1) then
      Inc(i);
  end;

  if(iFound = -1) then
    mslAOUserFields.Add(sEntry)
  else
    mslAOUserFields[iFound] := sEntry;

  slBuffer.Free;
end;

procedure TF__ASTROTOOLBOX.ShowAstroVoidForm(sVoidName, sConst: string; iCenterDist, iSize: Integer);
begin
  F__ASTROVOIDS := TF__ASTROVOIDS.Create(nil);

  F__ASTROVOIDS.msLANG_ID := msLANG_ID;

  F__ASTROVOIDS.P__VOIDNAME.Caption := sVoidName;
  F__ASTROVOIDS.L__CONST.Caption := sConst;
  F__ASTROVOIDS.L__DIST.Caption := IntToStr(iCenterDist div 1000000);
  F__ASTROVOIDS.L__SIZE.Caption := IntToStr(iSize div 1000000);

  //IniText(F__ASTROVOIDS,msLANG_ID);

  F__ASTROVOIDS.ShowModal;

  F__ASTROVOIDS.Destroy;
end;

procedure TF__ASTROTOOLBOX.ShowPictureViewer(IMG: TImage);
begin
  if(IMG = nil) or (IMG.Picture = nil) or (IMG.Picture.Bitmap = nil) then
  begin
    if(msLANG_ID = 'DE') then
      MessageDlg('Info','Kein Bild anzuzeigen',mtInformation,[mbOK],0)
    else
      MessageDlg('Info','No picture to display',mtInformation,[mbOK],0);

    exit;
  end;

  F__PICTUREVIEWER := TF__PICTUREVIEWER.Create(nil);
  F__PICTUREVIEWER.msLANG_ID := msLANG_ID;
  F__PICTUREVIEWER.IMG__PIC.Picture := IMG.Picture;

  IniText(F__PICTUREVIEWER,msLANG_ID);

  F__PICTUREVIEWER.ShowModal;

  F__PICTUREVIEWER.Destroy;
end;

procedure TF__ASTROTOOLBOX.DateRefresh();
begin
  mdtStart := 0;
  mdtStart0 := 0;
  //mbTimePlay := false;

  //P__NOW.Visible:=true;

  P__COUNTRIES.Visible := true;

  ReCalcPlanetPos(CB__WT.Date);
  TrigST();
  SetTimePanels();
  SetMoonPanel();

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
    CleanStartOfStarmap(); //GenMap(P__STARMAP);

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_SOLSYS) then
  begin
    ShowSolSys();
  end;

  SetMainInfoData(CB__WT.Date);
  GetRecommendations(CB__WT.Date);
  ShowMeteoriteShowerGrid(CB__WT.Date);
//  ShowMeteoriteShowerGrid(GetWTime());
  ShowEclipsesForMonth(CB__WT.Date);

end;

procedure TF__ASTROTOOLBOX.AccTime(bForward: Boolean);
begin
  mdtStart := Now;
  mdtStart0 := GetWTime();
  mbTimePlay := true;
  if(bForward) then
  begin
    if(miTimePlayMode <= 0) then
      miTimePlayMode := 1
    else if (miTimePlayMode > 5) then
      miTimePlayMode := 5
    else
      miTimePlayMode := miTimePlayMode + 1; // accelerate mode
  end
  else
  begin
    if(miTimePlayMode >= 0) then
      miTimePlayMode := -1
    else if (miTimePlayMode < -5) then
      miTimePlayMode := -5
    else
      miTimePlayMode := miTimePlayMode - 1; // accelerate mode
  end;

  //P__NOW.Visible:=false;

  P__COUNTRIES.Visible := false;

  CB__WT.Enabled := false;
  ED__WT_HH.Enabled := false;
  ED__WT_MM.Enabled := false;
  ED__WT_SS.Enabled := false;

  TIMER__GENMAP.Enabled := false;
  SetGenMapInterval(true);
  TIMER__GENMAP.Enabled := true;

end;

procedure TF__ASTROTOOLBOX.MeteorShowerMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MeteorShower: TMeteorShower;
begin
  if (Button <> mbLeft) then
    exit;

  MeteorShower := nil;

  if((Sender as TComponent).Tag >= 0) and ((Sender as TComponent).Tag < molAOList.Count) then
    MeteorShower := GetMeteorShower((molAOList[(Sender as TComponent).Tag] as TStar).sMShowerName_DE);

  if(MeteorShower = nil) then
    exit;

  F__MSHOWER := TF__MSHOWER.Create(nil);

  F__MSHOWER.msLANG_ID := msLANG_ID;

  IniText(F__MSHOWER,msLANG_ID);

  if(msLANG_ID = 'DE') then
  begin
    F__MSHOWER.Caption := 'Meteorschauer';
    F__MSHOWER.P__MS_TITLE.Caption := MeteorShower.sName_DE;
  end
  else
  begin
    F__MSHOWER.Caption := 'Meteor Shower';
    F__MSHOWER.P__MS_TITLE.Caption := MeteorShower.sName_EN;
  end;

  F__MSHOWER.GRD__MSHOWER.Cells[1,0] := MeteorShower.sSign;
  F__MSHOWER.GRD__MSHOWER.Cells[1,1] := MeteorShower.sMaxDateMonth;
  F__MSHOWER.GRD__MSHOWER.Cells[1,2] := IntToStr(MeteorShower.iObjPerHour);
  F__MSHOWER.GRD__MSHOWER.Cells[1,3] := IntToStr(MeteorShower.iSpeed);
  F__MSHOWER.GRD__MSHOWER.Cells[1,4] := MeteorShower.sTime1;
  F__MSHOWER.GRD__MSHOWER.Cells[1,5] := MeteorShower.sTime2;
  F__MSHOWER.GRD__MSHOWER.Cells[1,6] := MeteorShower.sCometName;

  F__MSHOWER.ShowModal;

  F__MSHOWER.Destroy;

end;

procedure TF__ASTROTOOLBOX.OpenADM();
var
  i: Integer;
  AObject: TAObject;
begin
  F__ADM := TF__ADM.Create(nil);
  F__ADM.miDST_HH:=miDST_HH;
  F__ADM.miUTC_HH:=miUTC_HH;
  F__ADM.molAOList:=molAOList;

  for i:=0 to molAOList.Count-1 do
  begin
    AObject := (molAOList[i] as TAObject);

    if(AObject.sAOType = 'P') and ((AObject as TPlanet).sPlanetType = 'A') then // Put Asteroids into combobox CB__ASTEROIDS
    begin
      F__ADM.CB__ASTEROID.Items.AddObject((AObject as TPlanet).SHP.Hint,(molAOList[i] as TPlanet));
    end;
  end;

  F__ADM.ShowModal;
  F__ADM.Destroy;
end;

procedure TF__ASTROTOOLBOX.SetDBSheetMaxMag(rMag1, rMag2: Real);
begin
  (*
  else if(RB__G.Checked) then
  begin
    if(msLANG_ID = 'DE') then
    begin
      if(rMag1 <= 0) then
       P__TABLE_TITLE.Caption:='Galaxien bis ' +  FloatToStrF(rMag2,ffFixed,8,2) + 'mag'
      else
        P__TABLE_TITLE.Caption:='Galaxien zw. ' +  FloatToStrF(rMag1,ffFixed,8,2) + '...' + FloatToStrF(rMag2,ffFixed,8,2) + 'mag';
    end
    else
    begin
      if(rMag1 <= 0) then
       P__TABLE_TITLE.Caption:='Galaxies up to ' +  FloatToStrF(rMag2,ffFixed,8,2) + 'mag'
      else
        P__TABLE_TITLE.Caption:='Galaxies between ' +  FloatToStrF(rMag1,ffFixed,8,2) + '...' + FloatToStrF(rMag2,ffFixed,8,2) + 'mag';
    end;
  end;
  *)

  ShowAOTable();

end;

(*
procedure TF__ASTROTOOLBOX.UpdateSolarSystem();
begin
  if(msLANG_ID = 'DE') then
    ODLG__FILE.Title := 'Planetendaten-Datei öffnen'
  else
    ODLG__FILE.Title := 'Open planet data file';

  if(ODLG__FILE.Execute) and (FileExists(ODLG__FILE.FileName)) then
  begin
    // Backup file
    FileUtil.CopyFile(ConvertWinPath('AO-P.dat'),ConvertWinPath('AO-P.dat.bak'));

    // Overwrite with new file
    if(FileUtil.CopyFile(ODLG__FILE.FileName,ConvertWinPath('AO-P.dat'))) then
    begin
      if(msLANG_ID = 'DE') then
        MessageDlg('Udpate Planetendaten','Update-Datei ist vorbereitet. Bitte starte Albireo neu.',mtInformation,[mbOK],0)
      else
        MessageDlg('Updating Planet data','Update file prepared. Please restart Albireo.',mtInformation,[mbOK],0);
    end;
  end;
end;
*)

procedure TF__ASTROTOOLBOX.GetHorPic(iSkyType: Integer);
var
  Picture: TPicture;
  dtDT: TDateTime;
  sFileName: string;
  iVisMonth: Integer; // Normal count norther hemisphere, inverse count southern hemisphere
  iHHCorr, iFade: Integer;
  rAmp, rOffset: Real;
  iTryCnt: ShortInt;
begin
  dtDT := GetWTime();

  //sAlbireoVersion := gcsAlbireoVersion;

  //bIsLandscapeCust := (miLandscapeNo >= ciLandscapeNoCust);

  if((miLandscapeNo >= ciLandscapeNoCust)) then
    iTryCnt := 2
  else
    iTryCnt := 1;

  repeat
    if((miLandscapeNo >= ciLandscapeNoCust) and (iTryCnt = 1) ) then // Switch for 2nd run if customer landscape is not found. Use insteadly landscap ciLandscapeNoDef.
      miLandScapeNo := ciLandscapeNoDef;

    // Monthly scenery:
    if(miGLat_DEG < -10) then // Southern hemisphere
    begin
      iVisMonth := 13 - GetMonth(dtDT);
      sFileName := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(miLandscapeNo) + '_' + msHorDir + '_' + IntToStr(iVisMonth) + '_DAY.png';
      if(not FileExists(sFileName)) then
        sFileName := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(miLandscapeNo) + '_' + msHorDir + '_DAY.png'
    end
    else if(abs(miGLat_DEG) <= 10) then
    begin
      iVisMonth := 0; // Near equator - no season dependence of horizon image
      sFileName := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(miLandscapeNo) + '_' + msHorDir + '_DAY.png';
    end
    else
    begin
      iVisMonth := GetMonth(dtDT); // Northern hemisphere
      sFileName := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(miLandscapeNo) + '_' + msHorDir + '_' + IntToStr(iVisMonth) + '_DAY.png';
      if(not FileExists(sFileName)) then
        sFileName := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(miLandscapeNo) + '_' + msHorDir + '_DAY.png'
    end;

    Dec(iTryCnt);

    until (
      (iTryCnt  < 1) or // Leave loop for predefinded Landscapes
      ( (iTryCnt = 1) and (FileExists(sFileName)) ) // Leave loop only if customer landscape was found
      );

  if(not FileExists(sFileName)) then
    exit;

  Picture := TPicture.Create;
  Picture.LoadFromFile(ConvertWinPath(sFileName));

  // Minimum Fade Value: Real Midnight. 0:00 respectively 01:00 DST
  // Maximum Fade Value: 12hs later! (Daytime) ALWAYS 32

  iHHCorr := (StrToInt(ED__WT_HH.Text)) - miDST_HH;

  // Amplitude: proportional rNightRange [2..32]
  //ShowMessage(FloatToStr(mrSet_HH) + ', ' + FloatToStr(mrRise_HH));
  rAmp := (24 - mrSet_HH) + mrRise_HH; // Summer: 5 - Winter: 16

  rOffset := 32 - rAmp;
  //Frequency: 24hs (rHH/24* 2*pi - pi/2)
  iFade := Round(rOffset + rAmp*sin(iHHCorr/24.0 *2*Pi -Pi/2)) ;    // Max always: 32.

  // Daylight scenery:
  case iSkyType of
    1: // Twilight
    begin
      FadeIn(Picture.Bitmap,IMG__HOR,iFade div 2); //16);
      //MM__MW.Lines.Add(IntToStr(iFade div 2));
    end;
    2: // DaySky
    begin
      FadeIn(Picture.Bitmap,IMG__HOR,32);
    end;
    else
    begin // Deep Night
      FadeIn(Picture.Bitmap,IMG__HOR,iFade div 4); //2);
      //MM__MW.Lines.Add(IntToStr(iFade div 4));
      //FadeIn(Picture.Bitmap,16);
    end;
  end; // case

  Picture.Free;
end;

procedure TF__ASTROTOOLBOX.ShowAstroCalc(ADevice: TADevice);
var
  bCameraCheckedIni: Boolean;
begin
  F__ASTROCALC := TF__ASTROCALC.Create(nil);

  F__ASTROCALC.msLANG_ID := msLANG_ID;
  F__ASTROCALC.msAlbireoLocalDir:=gsAlbireoLocalDir;

  F__ASTROCALC.mbSetFormDim := true;
  F__ASTROCALC.miFormTop := P__SELECT.Height;
  F__ASTROCALC.miFormLeft := P__NAVIG.Width;
  F__ASTROCALC.miFormHeight := PC__WORKBENCH.Height + P__SETTIME.Height;
  F__ASTROCALC.miFormWidth:= PC__WORKBENCH.Width;

  if(ADevice <> nil) then
  begin
    F__ASTROCALC.mrS0 := mrS0;
    F__ASTROCALC.L__TELTYPE.Caption:=CB__ADEV_TYPE.Text;
    F__ASTROCALC.ED__FOCALLENGTH.Value := ADevice.iFocalWidthDev_mm;
    F__ASTROCALC.ED__APERTURE.Value := ADevice.iDiameter_mm;
    F__ASTROCALC.L__D_AIRY.Caption:= FloatToStrF(1.22*550/1000000000 *1000/F__ASTROCALC.ED__APERTURE.Value *180/Pi *3600,ffFixed,8,1) + ' ''''';
    F__ASTROCALC.CB__PXC_MANUF.Text:=ADevice.sCManu;

    F__ASTROCALC.IniAstroCalc();

    if(Trim(ADevice.sCManu) <> '') then
      F__ASTROCALC.GetCameraModels();

    F__ASTROCALC.CB__PXC_MODEL.Text:=ADevice.sCModel;

    F__ASTROCALC.CBX__CAMERA_STD.Enabled:=true; // Standard camera assignment only enabled for a given telescope.

    if(Trim(ADevice.sCManu) <> '') then
      F__ASTROCALC.CBX__CAMERA_STD.Checked := true;

    //F__ASTROCALC.IniAstroCalc();

    if(ADevice.sCManu <> '') and (ADevice.sCModel <> '') then
      F__ASTROCALC.ActivateCamera();

    F__ASTROCALC.PC__ASTROCALC.ActivePageIndex:=2;
  end;

  bCameraCheckedIni := F__ASTROCALC.CBX__CAMERA_STD.Checked;
  (*
  F__ASTROCALC.Left := P__NAVIG.Width + 2;  // 352
  F__ASTROCALC.Top := P__SELECT.Height + 2;  // 92
  F__ASTROCALC.Height := PC__WORKBENCH.Height;  // 804
  F__ASTROCALC.Width := PC__WORKBENCH.Width; // 906
  *)

  F__ASTROCALC.Show; //ShowModal;

  if(F__ASTROCALC.CBX__CAMERA_STD.Checked) and (ADevice <> nil) then
  begin
    if((not bCameraCheckedIni)
      or (Trim(ADevice.sCManu) <> F__ASTROCALC.CB__PXC_MANUF.Text)
      or (Trim(ADevice.sCModel) <> F__ASTROCALC.CB__PXC_MODEL.Text)
      ) then
    begin
      ADevice.sCManu := F__ASTROCALC.CB__PXC_MANUF.Text;
      ADevice.sCModel := F__ASTROCALC.CB__PXC_MODEL.Text;
      mbADEVChanged:=true;
    end;
  end;

  // Standard camera removed?
  if(bCameraCheckedIni and (not F__ASTROCALC.CBX__CAMERA_STD.Checked)) then
  begin
    ADevice.sCManu := '';
    ADevice.sCModel := '';
    mbADEVChanged:=true;
  end;

  //F__ASTROCALC.Destroy;
end;

procedure TF__ASTROTOOLBOX.CalcKochabMethod();
var
  dtST, dtHA_POLARIS, dtHA_KOCHAB, dtJulDat, dtWT: TDateTime;
  iX0, iY0: Integer;
  rBaseK, rBaseP, rRad: Real;
begin
  dtJulDat := 0;
  dtWT := GetWTime();
  dtST := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dtJulDat);

  dtHA_POLARIS := GetHA(dtST,cfRA_POLARIS);
  dtHA_KOCHAB := GetHA(dtST,cfRA_KOCHAB);

  SHP__NCP.Left := P__KOCHABMETHOD.Width div 2 - SHP__NCP.Width div 2;
  SHP__NCP.Top := P__KOCHABMETHOD.Height div 2 - SHP__NCP.Height div 2;

  iX0 := P__KOCHABMETHOD.Width div 2;
  iY0 := P__KOCHABMETHOD.Height div 2;
  rRad := 1.8*sqrt(iX0*iX0 + iY0*iY0);

  rBaseK := rRad*cos(cfDEC_KOCHAB*Pi/180);
  rBaseP := rRad*cos(cfDEC_POLARIS*Pi/180);

  L__ANGLE_PA.Caption := HourToHHMMStr((1.0-dtHA_KOCHAB)*12.0);

  SHP__POLARIS.Left := Round(iX0 - rBaseP*sin(dtHA_POLARIS*2*Pi)) - SHP__POLARIS.Width div 2;
  SHP__POLARIS.Top := Round(iY0 - rBaseP*cos(dtHA_POLARIS*2*Pi)) - SHP__POLARIS.Height div 2;

  SHP__KOCHAB.Left := Round(iX0 - rBaseK*sin(dtHA_KOCHAB*2*Pi)) - SHP__KOCHAB.Width div 2;
  SHP__KOCHAB.Top := Round(iY0 - rBaseK*cos(dtHA_KOCHAB*2*Pi)) - SHP__KOCHAB.Height div 2;

end;

procedure TF__ASTROTOOLBOX.ShowSolSys();
begin
  {$IFDEF Darwin}
  P__SOLSYS.Repaint;
  P__INCL.Repaint;
  {$ENDIF Darwin}
  {$IFDEF Windows}
  SolSys();
  {$ENDIF Windows}
end;

procedure TF__ASTROTOOLBOX.SolSys();
var
  iXCenter, iYCenter, iZCenter: Integer;
  i,j: Integer;
  bEnd: Boolean;
  Planet: TPlanet;
  Comet: TComet;
  rB, rE: Real;
  iSCALE: Integer;
  fSCALE: Real;
  rLE,rL1,rL: Real;
  sLabel: string;
  PImage: TImage;
  //Bitmap: Graphics.TBitmap;
  rAVal, rEVal: Real;
  rArgP: Real;
  rMinAU, rMaxAU: Real;
  bDoPlot: Boolean;
  //rRA_DEG: Real;
begin
  P__SOLSYS.Refresh;

  rLE := 0;
  rL1 := 0;

  rMinAU := 0;
  rMaxAU := TB__SOLSYS.Position;

  if(msLANG_ID = 'DE') then
    L__SOLSYS_DIST.Caption := IntToStr(TB__SOLSYS.Position) +
      ' AE: ' + FloatToStrF(TB__SOLSYS.Position*149.6,ffFixed,8,1) + ' Mio. km'
  else
    L__SOLSYS_DIST.Caption := IntToStr(TB__SOLSYS.Position) +
      ' AU: ' + FloatToStrF(TB__SOLSYS.Position*149.6,ffFixed,8,1) + ' Mio. km';

  // Apparent size of sun
  if(rMaxAU >= 5) then
    SHP__SUN.Width := 24 - 22*(Trunc(rMaxAU) - 5) div (Int64(TB__SOLSYS.Max) - 5)
  else
    SHP__SUN.Width := 24;

  SHP__SUN.Height := SHP__SUN.Width;

  // Apparent size of sun
  if(rMaxAU >= 5) then
    SHP__SUN_INCL.Width := 16 - 14*(Trunc(rMaxAU) - 5) div (Int64(TB__SOLSYS.Max) - 5)
  else
    SHP__SUN_INCL.Width := 16;

  SHP__SUN_INCL.Height := SHP__SUN_INCL.Width;


  SHP__SUN.Visible:=false;
  SHP__SUN.OnMouseUp := @AOVisOnMouseUp;

  IMG__SOLSYS.Canvas.Brush.Color:=clBlack;

  //IMG__SOLSYS.Canvas.Rectangle(0,0,IMG__SOLSYS.Width,IMG__SOLSYS.Height);
  P__INCL.Canvas.Rectangle(0,0,P__INCL.Width,P__INCL.Height);
  IMG__SOLSYS.Canvas.TextOut(IMG__SOLSYS.Width div 2 - 30,18,'< 00:00:00 RA');
  IMG__SOLSYS.Canvas.TextOut(10,IMG__SOLSYS.Height div 2 - 10,'06:00:00 RA');
  IMG__SOLSYS.Canvas.TextOut(IMG__SOLSYS.Width div 2 - 30,IMG__SOLSYS.Height - 20,'12:00:00 RA');
  IMG__SOLSYS.Canvas.TextOut(IMG__SOLSYS.Width - 80,IMG__SOLSYS.Height div 2 - 20,'18:00:00 RA');

  SHP__SUN.Repaint;
  SHP__SUN_INCL.Repaint;

  // Set Sun inlcination panel
  SHP__SUN_INCL.Left := P__INCL.Width div 2 - (SHP__SUN_INCL.Width div 2);
  SHP__SUN_INCL.Top := P__INCL.Height div 2 - (SHP__SUN_INCL.Height div 2);

  // Set SUN
  iXCenter := (IMG__SOLSYS.Width div 2);
  iYCenter := (IMG__SOLSYS.Height div 2);// - SHP__SUN_INCL.Height;
  iZCenter := (P__INCL.Height div 2);

  SHP__SUN.Left := iXCenter - (SHP__SUN.Width div 2);
  SHP__SUN.Top := iYCenter - (SHP__SUN.Height div 2);

  if(iXCenter < iYCenter) then
    iSCALE := iXCenter
  else
    iSCALE := iYCenter;

  bEnd := false;
  i:=0;

  IMG__SOLSYS.Canvas.Pen.Color:=clLime;
  IMG__SOLSYS.Canvas.Pen.Width:=miLThickness;
  P__INCL.Canvas.Pen.Width:=miLThickness;

  // Delete Planet, Asteroid and Comet images and shapes
  for i:=0 to mslPlanetPlot.Count-1 do
  begin
    j := StrToInt(mslPlanetPlot[i]);

    if((molAOList[j] as TPlanet).IMG <> nil) then
    begin
      (molAOList[j] as TPlanet).IMG.Visible:=false;
      (molAOList[j] as TPlanet).SHP.Visible:=false;
    end;

  end;

  mslPlanetPlot.Clear;

  for i:=0 to mslCometPlot.Count-1 do
  begin
    j := StrToInt(mslCometPlot[i]);

    //(molAOList[j] as TComet).IMG.Visible:=false;
    (molAOList[j] as TComet).SHP.Visible:=false;
  end;
  mslCometPlot.Clear;
  Application.ProcessMessages;

  // Plot solar system...
  i:=0;
  while ((i < molAOList.Count) and (not bEnd)) do
  begin
    if((molAOList[i] as TAObject).sAOType = 'P') or ((molAOList[i] as TAObject).sAOType = 'E') or
      ((molAOList[i] as TAObject).sAOType = 'C') then
    begin
      // Draw earth and planets
      if((molAOList[i] as TAObject).sAOType = 'P') or ((molAOList[i] as TAObject).sAOType = 'E') then
      begin
        Planet := (molAOList[i] as TPlanet);

        Planet.SHP.Visible:=false;
        Planet.IMG.Visible:=false;

        IMG__SOLSYS.Canvas.Pen.Color := Planet.Color;
        P__INCL.Canvas.Pen.Color := Planet.Color;

        //ShowMessage(Planet.sName_DE);

        GetPlanetAngles(CB__WT.Date,Planet,rLE,rL1);
        if(Uppercase((molAOList[i] as TAObject).sName_EN) = 'EARTH') then // .sAOType = 'E') then
        begin
          rL := rLE;
          Planet.rI := 0;
        end
        else
          rL := rL1;

        rAVal := Planet.rA; rEVal := Planet.rE;

        bDoPlot := true;

        if(msLANG_ID = 'DE') then
          sLabel := Planet.sName_DE
        else
          sLabel := Planet.sName_EN;

        if(Planet.sPlanetType = 'A') then
        begin
          if ((not ((mslAsteroidsCometsDisplayed.IndexOf(sLabel) > -1) or (CB__ASTEROIDS.ItemIndex = 0))) or (CB__ASTEROIDS.ItemIndex = 1)) then
            bDoPlot := false;
        end;

        if(Uppercase(Planet.sName_DE) = '(1) CERES') and (not CBX__CERES.Checked) then
          bDoPlot := false;

        if(Uppercase(Planet.sName_DE) = 'PLUTO') and (not CBX__PLUTO.Checked) then
          bDoPlot := false;

        if(bDoPlot) then
        begin
          if(FileExists(gsAlbireoLocalDir + 'img\' + Planet.sName_EN + '.png')) then
          begin
            PImage := Planet.IMG;
            PImage.Width:=20;
            PImage.Height:=20;
            PImage.Visible:=false;
            PImage.Parent := P__SOLSYS;
            PImage.Picture.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\' + Planet.sName_EN + '.png'));
            PImage.OnMouseUp := @AOVisOnMouseUp;
            PImage.OnMouseEnter := @AOVisOnMouseEnter;
            //PImage.OnMouseLeave := @AOVisOnMouseLeave;
            PImage.Tag:=i;
          end
          else
            PImage := nil;

          Planet.SHP.OnMouseUp := @AOVisOnMouseUp;
          Planet.SHP.Parent := P__SOLSYS;
          Planet.SHP.Height:=6;
          Planet.SHP.Width:=6;
          Planet.SHP.Shape:=stCircle;
          Planet.SHP.Brush.Color:=clMaroon;
          Planet.SHP.Pen.Color:=clMaroon;
          Planet.SHP.Tag:=i;
          Planet.SHP.Visible := false;

          if(rL > 360) then rL := rL - 360;
          if(rL < 0) then rL := rL + 360;

          rE := rAVal * rEVal;
          rB := rAVal*sqrt(1-rEVal*rEVal);

          rArgP := Planet.rOmegaQ;

          rArgP := -rArgP + 90;
          if(rArgP < 0) then rArgP := rArgP + 360;

          if(rAVal > rMinAU) and (rAVal <= rMaxAU) then
          begin
            mslPlanetPlot.Add(IntToStr(i));

            //rRA_DEG := (Planet.iRA_Hours + Planet.iRA_Min/60.0 + Planet.rRA_Sec/3600.0)*360.0/24;
            //rRA_DEG := -999;
            fSCALE := iSCALE* cos(Planet.rI * Pi/180.0);
            // Inner solar system representation !
            DrawPlanetEllipse(PImage,Planet.SHP,rL,sLabel,IMG__SOLSYS.Canvas,
              iXCenter + Round(fSCALE*rE/rMaxAU *cos(rArgP*Pi/180)),
              iYCenter + Round(fSCALE*rE/rMaxAU *sin(rArgP*Pi/180)),
              Round(fSCALE*(rAVal/rMaxAU)),Round(fSCALE*(rB/rMaxAU)),
              Planet.rI,
              'P',
              rArgP);

            // Draw inclination view
            P__INCL.Canvas.MoveTo(
              iXCenter - Round(iSCALE*(rAVal-rE)/rMaxAU * cos(Planet.rI * Pi/180.0)),
              iZCenter + Round(iSCALE*(rAVal-rE)/rMaxAU * sin(Planet.rI * Pi/180.0))
              );
            P__INCL.Canvas.LineTo(
              iXCenter + Round(iSCALE*(rAVal + rE)/rMaxAU*cos(Planet.rI * Pi/180.0)),
              iZCenter - Round(iSCALE*(rAVal + rE)/rMaxAU*sin(Planet.rI * Pi/180.0))
              );

          end;
        end;  // if bDoPlot..
      end; // if((molAOList[i] as TAObject).sAOType = 'P')..

      // Draw comets
      if((molAOList[i] as TAObject).sAOType = 'C') then
      begin
        Comet := (molAOList[i] as TComet);

        if(msLANG_ID = 'DE') then
          sLabel := Comet.sName_DE
        else
          sLabel := Comet.sName_EN;

        if((CB__COMETS.ItemIndex <> 1) and ((mslAsteroidsCometsDisplayed.IndexOf(sLabel) > -1) or (CB__COMETS.ItemIndex = 0))) then
        begin
          mslCometPlot.Add(IntToStr(i));

          IMG__SOLSYS.Canvas.Pen.Color := clYellow;
          P__INCL.Canvas.Pen.Color := clYellow; P__INCL.Canvas.Pen.Width:=miLThickness;

          GetCometAngles(CB__WT.Date,Comet,rLE,rL1);
          rL := rL1;

          //rRA_DEG := (Comet.iRA_Hours + Comet.iRA_Min/60.0 + Comet.rRA_Sec/3600.0)*360.0/24;

          rAVal := Comet.rA; rEVal := Comet.rE;

          if(rL > 360) then rL := rL - 360;
          if(rL < 0) then rL := rL + 360;

          rE := rAVal * rEVal;
          rB := rAVal*sqrt(1-rEVal*rEVal);

          rArgP := Comet.rOmegaQ;// - Comet.rOmega;
          //rArgP := - Comet.rOmega;
          //rArgP := 360 + Comet.rOmegaQ - Comet.rOmega;
          if(rArgP >= 360) then rArgP := rArgP - 360;
          if(rArgP < 0) then rArgP := rArgP + 360;

          rArgP := -rArgP + 90;
          if(rArgP < 0) then rArgP := rArgP + 360;

          Comet.SHP.OnMouseUp := @AOVisOnMouseUp;
          Comet.SHP.Parent := P__SOLSYS;
          Comet.SHP.Height:=6;
          Comet.SHP.Width:=6;
          Comet.SHP.Shape:=stCircle;
          Comet.SHP.Tag:=i;
          Comet.SHP.Visible := false;

          //fSCALE := iSCALE* cos(Comet.rI * Pi/180.0); // Projection of the inclination of the xy-plane!
          fSCALE := iSCALE; // Projection of the inclination of the xy-plane!
          //DrawPlanetEllipse(nil,rL,sLabel,IMG__SOLSYS.Canvas,iXCenter - Round(iSCALE*rE/rMaxAU),iYCenter,Round(iSCALE*(rAVal/rMaxAU)),Round(iSCALE*(rB/rMaxAU)),Comet.rOmegaQ);
          DrawPlanetEllipse(nil,Comet.SHP,rL,sLabel,IMG__SOLSYS.Canvas,
            iXCenter + Round(fSCALE*rE/rMaxAU *cos(rArgP*Pi/180)),
            iYCenter + Round(fSCALE*rE/rMaxAU *sin(rArgP*Pi/180)),
            Round(fSCALE*(rAVal/rMaxAU)),Round(fSCALE*(rB/rMaxAU)),
            Comet.rI,
            'C',
            rArgP);

          // Draw inclination view
          P__INCL.Canvas.MoveTo(
            iXCenter - Round(iSCALE*(rAVal-rE)/rMaxAU * cos(Comet.rI * Pi/180.0)),
            iZCenter + Round(iSCALE*(rAVal-rE)/rMaxAU * sin(Comet.rI * Pi/180.0))
            );
          P__INCL.Canvas.LineTo(
            iXCenter + Round(iSCALE*(rAVal + rE)/rMaxAU*cos(Comet.rI * Pi/180.0)),
            iZCenter - Round(iSCALE*(rAVal + rE)/rMaxAU*sin(Comet.rI * Pi/180.0))
            );

        end;
      end;

    end;

    Inc(i);
  end; // while
  SHP__SUN.Visible:=true;

end;

procedure TF__ASTROTOOLBOX.IniCalcPlanetOpCon();
var
  i: Integer;
  iPSearch: Integer;
  sLastType: string;
  dtOpDate, dtConDate: TDateTime;
  //slConListU, slConListL: TStringList;
  iYear: Integer;
  iY: Integer;
begin
  //slConListU := TStringList.Create;
  //slConListL := TStringList.Create;
  sLastType := '';

  iYear := YearOf(Now);

  for iY := iYear-1 to iYear+1 do
  begin
    iPSearch := -1;
    i := 0;
    while (i < molAOList.Count) and (iPSearch < 1) do
    begin
      if(sLastType = 'P') and ((molAOList[i] as TAObject).sAOType <> 'P') and ((molAOList[i] as TAObject).sAOType <> 'E') then
        iPSearch := 1; // break;

      if((molAOList[i] as TAObject).sAOType = 'P') then
      begin
        if( not (molAOList[i] as TPlanet).bInnerPlanet) then
        begin
          iPSearch := 0;

          dtOpDate := 0; // Opposition date of the year
          dtConDate := 0; // (Upper) conjunction date of the year

          CalcPlanetOpCon(iY,(molAOList[i] as TPlanet),
            dtOpdate,dtConDate);
            //slConListU, slConListL);

          if(iY = iYear-1) then
          begin
            (molAOList[i] as TPlanet).iYearP := iY;
            (molAOList[i] as TPlanet).dtOPDateP:= dtOpDate;
            (molAOList[i] as TPlanet).dtConP:= dtConDate;
          end;

          if(iY = iYear) then
          begin
            (molAOList[i] as TPlanet).iYear := iY;
            (molAOList[i] as TPlanet).dtOPDate:= dtOpDate;
            (molAOList[i] as TPlanet).dtCon:= dtConDate;
          end;

          if(iY = iYear+1) then
          begin
            (molAOList[i] as TPlanet).iYearN := iY;
            (molAOList[i] as TPlanet).dtOPDateN:= dtOpDate;
            (molAOList[i] as TPlanet).dtConN:= dtConDate;
          end;

        end; // if molAOList..InnerPlanet
      end; // if molAOList..P

      if((molAOList[i] as TAObject).sAOType <> 'E') then
        sLastType := (molAOList[i] as TAObject).sAOType;

      Inc(i);
    end; // while..
  end; // for iY..

  (*
  slConListU.Free;
  slConListL.Free;
  *)
end;

procedure TF__ASTROTOOLBOX.CalcPlanetOpCon(iYear: Integer; Planet: TPlanet;
    var dtOpdate: TDateTime; var dtConDate: TDateTime);
    //var slConList1: TStringList; var slConList2: TStringList);
{27.11.2017/fs
Evaluates the day of opposition and conjunction / outer and inner conjunction of a given planet
}
var
  iDay, iDayStart, iDayEnd: Integer;
  dtFDOY, dtLDOY, dtTime: TDateTime;
  dtRA: TDateTime;
  iRA_HH,iRA_MM, iRA_SS: Word;  // Position in space! useful to visualize planet's orbit!!!
  rRA_SS: Real;
  iDEC_DEG,iDEC_MM: SmallInt;
  rDEC_SS: Real;
  iDST_HH: Integer;
  iRA_HH_SUN, iRA_MM_SUN: Word;
  rRA_SS_SUN: Real;
  iDEC_DEG_SUN, iDEC_MM_SUN: SmallInt;
  rDEC_SS_SUN: Real;
  rLambdaSun, RMSun: Real;
  dtRA_SUN: TDateTime;
  rMinOpp, rMinCon: Real;
  rR: Real;
begin
  rMinOpp := 999;
  rMinCon := 999;
  //dtHA:=0; iDEC_SS:=0;
  //bConSwitch := true;

  dtFDOY := EncodeDate(iYear, 1, 1);
  iDayStart := Trunc(dtFDOY);
  dtLDOY := EncodeDate(iYear, 12, 31);
  iDayEnd := Trunc(dtLDOY);

  for iDay := iDayStart to iDayEnd do
  begin
    iDST_HH := GetDST(iDay,msDST,miDST_HH_DEF);

    if(Planet.bInnerPlanet) then
    begin
      // Fill conjunction stringlists (higher speed) leads to multiple incidents!
     dtTime := 1.0*iDay + 0.5; // 12:00 lunchtime
     //dtST := GetSIDTime(dtTime,iDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dtJulDat);

     iRA_HH:=0;iRA_MM:=0;rRA_SS:=0; iDEC_DEG:=0;iDEC_MM:=0;rDEC_SS:=0;rR:=0;

     GetPlanetCoo(dtTime,Planet,
       miDST_HH,miUTC_HH,
       iRA_HH,iRA_MM,rRA_SS,
       iDEC_DEG,iDEC_MM,rDEC_SS,rR);

     if(iDEC_DEG >= 270) then iDEC_DEG := iDEC_DEG - 360;

     iRA_SS := Trunc(rRA_SS);
     //iDEC_SS := Trunc(rDEC_SS);

     dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS, Trunc(1000*(rRA_SS - iRA_SS)));

     rLambdaSun:=0; rMSun:=0;
     iRA_HH_SUN:=0;iRA_MM_SUN:=0;rRA_SS_SUN:=0;
     iDEC_DEG_SUN:=0;iDEC_MM_SUN:=0;rDEC_SS_SUN:=0;

     GetSunCoo(dtTime,
       iDST_HH,miUTC_HH, rLambdaSun, rMSun,
       iRA_HH_SUN, iRA_MM_SUN, rRA_SS_SUN,
       iDEC_DEG_SUN, iDEC_MM_SUN, rDEC_SS_SUN);

     dtRA_SUN := EncodeTime(iRA_HH_SUN, iRA_MM_SUN, Trunc(rRA_SS_SUN),
       Trunc(1000*(rRA_SS_SUN-Trunc(rRA_SS_SUN))));
    end
    else
    begin
      // Opposition / Outer planets: Find pos as midnight
      dtTime := 1.0*iDay + iDST_HH/24.0; // Midnight, 01:00 DST
      //dtST := GetSIDTime(dtTime,iDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dtJulDat);

      iRA_HH:=0;iRA_MM:=0;rRA_SS:=0; iDEC_DEG:=0;iDEC_MM:=0;rDEC_SS:=0;rR:=0;

      GetPlanetCoo(dtTime,Planet,
        iDST_HH,miUTC_HH,
        iRA_HH,iRA_MM,rRA_SS,
        iDEC_DEG,iDEC_MM,rDEC_SS,rR);

      //ShowMessage('miDST_HH: ' + IntToStr(miDST_HH));
      if(iDEC_DEG >= 270) then iDEC_DEG := iDEC_DEG - 360;

      iRA_SS := Trunc(rRA_SS);
      //iDEC_SS := Trunc(rDEC_SS);

      dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS, Trunc(1000*(rRA_SS-iRA_SS)));
      //dtHA := GetHA(dtST,dtRA);

      rLambdaSun := 0;
      rMSun := 0;

      rLambdaSun:=0; rMSun:=0;
      iRA_HH_SUN:=0;iRA_MM_SUN:=0;rRA_SS_SUN:=0;
      iDEC_DEG_SUN:=0;iDEC_MM_SUN:=0;rDEC_SS_SUN:=0;

      GetSunCoo(dtTime,
        iDST_HH,miUTC_HH, rLambdaSun, rMSun,
        iRA_HH_SUN, iRA_MM_SUN, rRA_SS_SUN,
        iDEC_DEG_SUN, iDEC_MM_SUN, rDEC_SS_SUN);

      dtRA_SUN := EncodeTime(iRA_HH_SUN, iRA_MM_SUN, Trunc(rRA_SS_SUN),
        Trunc(1000*(rRA_SS_SUN-Trunc(rRA_SS_SUN))));

      if(abs(0.5 - abs(dtRA_SUN-dtRA)) < rMinOpp) then
      begin
        rMinOpp := abs(0.5 - abs(dtRA_SUN-dtRA));
        if(rMinOpp < 0.01) then
          dtOpdate := iDay;
      end;

      if(abs(abs(dtRA_SUN-dtRA)) < rMinCon) then
      begin
        rMinCon := abs(abs(dtRA_SUN-dtRA));
        if(rMinCon < 0.01) then
          dtConDate := iDay;
      end;
    end;

  end;
end;

procedure TF__ASTROTOOLBOX.IniAObjects();
var
  k,i: Integer;
  //VisibleAO: TAObject;
begin
  if(molAOList.Count = 0) or (mslVisibleAOList.Count = 0) then
    exit;

  P__STARMAP.DisableAlign;

  SHP__POS_SUN.Visible:=false;
  IMG__POS_MOON.Visible:=false;

  k:=-1;
  repeat
    Inc(k);
    i := StrToInt(mslVisibleAOList[k]);

    // Hide all previous objects
    if((molAOList[i] as TAObject).sAOType = 'P') and ((molAOList[i] as TPlanet).IMG.Visible) then
    begin
      (molAOList[i] as TPlanet).IMG.Visible:=false;

      if((molAOList[i] as TAObject).L__AO <> nil) then
        (molAOList[i] as TPlanet).L__AO.Visible:=false;

    end;

    // Clean all objects - if location change has been made.
    if((molAOList[i] as TAObject).L__AO <> nil) then
    begin
      (molAOList[i] as TAObject).L__AO.Width := 0;
      (molAOList[i] as TAObject).L__AO.Visible:=false;
    end;

    if((molAOList[i] as TAObject).SHP <> nil) then
      (molAOList[i] as TAObject).SHP.Visible:=false;

    if((molAOList[i] as TAObject).sAOType = 'S') then
    begin
      (molAOList[i] as TStar).PrepareStar(false);
    end;

  until (k >= mslVisibleAOList.Count-1);

  P__STARMAP.EnableAlign;
end;

procedure TF__ASTROTOOLBOX.IniAObjectsAll();
var
  i: Integer;
begin
  if(molAOList.Count = 0) or (mslVisibleAOList.Count = 0) then
    exit;

  mslVisibleAOList.Clear;

  P__STARMAP.DisableAlign;

  SHP__POS_SUN.Visible:=false;
  IMG__POS_MOON.Visible:=false;

  i:=-1;
  repeat
    Inc(i);

    // Hide all previous objects
    if((molAOList[i] as TAObject).sAOType = 'P') then
    begin
      (molAOList[i] as TPlanet).IMG.Visible:=false;

      if((molAOList[i] as TAObject).L__AO <> nil) then
        (molAOList[i] as TPlanet).L__AO.Visible:=false;

    end;

    if((molAOList[i] as TAObject).L__AO <> nil) then
    begin
      (molAOList[i] as TAObject).L__AO.Width := 0;
      (molAOList[i] as TAObject).L__AO.Visible:=false;
    end;

    if((molAOList[i] as TAObject).SHP <> nil) then
      (molAOList[i] as TAObject).SHP.Visible:=false;

    (*
    if((molAOList[i] as TAObject).sAOType = 'S') and ((molAOList[i] as TStar).L__CON <> nil) then
    begin
      (molAOList[i] as TStar).L__CON.Width := 0;
      (molAOList[i] as TStar).L__CON.Visible:=false;
    end;
    *)

  until (i >= molAOList.Count-1);

  P__STARMAP.EnableAlign;
end;

procedure TF__ASTROTOOLBOX.IniTelProp();
begin
  if(msLANG_ID = 'DE') or (msLANG_ID = 'D') then
  begin
    GRD__TELPROP.Cells[0,0] := 'Eigenschaft';
    GRD__TELPROP.Cells[1,0] := 'Wert';
    GRD__TELPROP.Cells[0,1] := 'Öffnungsverhältnis';
    GRD__TELPROP.Cells[0,2] := 'Vergrößerungen';
    GRD__TELPROP.Cells[0,3] := 'Okularbrennweiten';
    GRD__TELPROP.Cells[0,4] := 'Austrittspupille';
    GRD__TELPROP.Cells[0,5] := 'Auflösungsvermögen';
    GRD__TELPROP.Cells[0,6] := 'Grenzgröße';
    GRD__TELPROP.Cells[0,7] := 'Anzahl Sterne';
    GRD__TELPROP.Cells[0,8] := 'Opt. wirks. Fläche qcm';
    GRD__TELPROP.Cells[0,9] := 'Transmission-%';
  end
  else
  begin
    GRD__TELPROP.Cells[0,0] := 'Property';
    GRD__TELPROP.Cells[1,0] := 'Value';
    GRD__TELPROP.Cells[0,1] := 'Aperture Ratio';
    GRD__TELPROP.Cells[0,2] := 'Magnifications';
    GRD__TELPROP.Cells[0,3] := 'Eyepiece focal lengths';
    GRD__TELPROP.Cells[0,4] := 'Exit Pupil';
    GRD__TELPROP.Cells[0,5] := 'Resolution Capacity';
    GRD__TELPROP.Cells[0,6] := 'Limiting Magnitude';
    GRD__TELPROP.Cells[0,7] := 'No of Stars';
    GRD__TELPROP.Cells[0,8] := 'Opt. valuable area qcm';
    GRD__TELPROP.Cells[0,9] := 'Transmission-%';
  end;
end;

function TF__ASTROTOOLBOX.CanPlotAO(rAz, rHgt: Real): Boolean;
{
Decide to plot an astronomical object in normal mode or in zoom mode
}
var
  bAzCond: Boolean;
begin
  if(mbZoomMode) then
  begin
    if(mrZoomAzMax > 270) and (mrZoomAzMin < 90) then
      bAzCond := (rAz >= mrZoomAzMax) or (rAz <= mrZoomAzMin) // 360 -> 90!
    else
      bAzCond := (rAz <= mrZoomAzMax) and (rAz >= mrZoomAzMin); // 90 -> 270

    Result := (
      ((rHgt <= mrZoomHgtMax) and (rHgt >= mrZoomHgtMin) and bAzCond) or // not centered rect
      //((rHgt >= 0) and (mrZoomHgtMax-mrZoomHgtMin > 0))
      ((rHgt >= 0) and (mrZoomHgtMax-mrZoomHgtMin < 3))
      ); // centered rect!
  end
  else
  begin
    Result := (rHgt >= 0);
  end;
end;

procedure TF__ASTROTOOLBOX.ZoomModeTransformation_INV(iR0: Integer; var rDX: Real; var rDY: Real);
{Move and rescale Az/Hgt-Area into the center and extend to the boundaries
}
var
 rR_Z0, rDX_Z0, rDY_Z0: Real;
 rR_ZHMax, rR_ZHMin: Real;
 rMeanAz: Real;
 rDX_ZAMax,rDX_ZAMin,rDY_ZAMax,rDY_ZAMin,rR_ZA: Real;
begin
  if(mrZoomAzMax > 270) and (mrZoomAzMin < 90) then
  begin
     rMeanAz := (mrZoomAzMax+mrZoomAzMin+360)/2.0;
     if(rMeanAz >= 360) then
       rMeanAz := rMeanAz - 360;

  end
  else
    rMeanAz := (mrZoomAzMax+mrZoomAzMin)/2;

  rR_Z0 := iR0*(pi/2 - (mrZoomHgtMax+mrZoomHgtMin)/2 *pi/180)*2/pi;
  rDX_Z0 := rR_Z0*sin(rMeanAz *pi/180); // DX-Displacement
  rDY_Z0 := rR_Z0*cos(rMeanAz *pi/180); // DY-Displacement

  rR_ZHMax := iR0*(pi/2 - mrZoomHgtMax *pi/180)*2/pi;
  rR_ZHMin := iR0*(pi/2 - mrZoomHgtMin *pi/180)*2/pi;

  rDX_ZAMax := rR_Z0*sin(mrZoomAzMax *pi/180);
  rDY_ZAMax := rR_Z0*cos(mrZoomAzMax *pi/180);

  rDX_ZAMin := rR_Z0*sin(mrZoomAzMin *pi/180);
  rDY_ZAMin := rR_Z0*cos(mrZoomAzMin *pi/180);

  rR_ZA := sqrt((rDX_ZAMax-rDX_ZAMin)*(rDX_ZAMax-rDX_ZAMin) + (rDY_ZAMax-rDY_ZAMin)*(rDY_ZAMax-rDY_ZAMin));

  if(mrZoomHgtMax-mrZoomHgtMin > 5) then
  //if(mrZoomHgtMax-mrZoomHgtMin > 0) then
  begin
    rDX := rDX *abs(rR_ZHMin-rR_ZHMax)/(2*iR0) + rDX_Z0;
    rDY := rDY *abs(rR_ZHMin-rR_ZHMax)/(2*iR0) + rDY_Z0;
  end
  else
  begin
    rDX := rDX *rR_ZA/iR0;
    rDY := rDY *rR_ZA/iR0;
  end;
end;

procedure TF__ASTROTOOLBOX.ZoomModeTransformation(iR0: Integer; var rDX: Real; var rDY: Real);
{Move and rescale Az/Hgt-Area into the center and extend to the boundaries
}
var
 rR_Z0, rDX_Z0, rDY_Z0: Real;
 rR_ZHMax, rR_ZHMin: Real;
 rMeanAz: Real;
 rDX_ZAMax,rDX_ZAMin,rDY_ZAMax,rDY_ZAMin,rR_ZA: Real;
begin
  if(mrZoomAzMax > 270) and (mrZoomAzMin < 90) then  // S
  begin
     rMeanAz := (mrZoomAzMax+mrZoomAzMin+360)/2.0;
     if(rMeanAz >= 360) then
       rMeanAz := rMeanAz - 360;

  end
  else
    rMeanAz := (mrZoomAzMax+mrZoomAzMin)/2;

  rR_Z0 := iR0*(1 - (mrZoomHgtMax+mrZoomHgtMin)/180);
  rDX_Z0 := rR_Z0*sin(rMeanAz *pi/180); // DX-Displacement
  rDY_Z0 := rR_Z0*cos(rMeanAz *pi/180); // DY-Displacement

  rR_ZHMax := iR0*(1 - mrZoomHgtMax /90);
  rR_ZHMin := iR0*(1 - mrZoomHgtMin /90);

  rDX_ZAMax := rR_Z0*sin(mrZoomAzMax *pi/180);
  rDY_ZAMax := rR_Z0*cos(mrZoomAzMax *pi/180);

  rDX_ZAMin := rR_Z0*sin(mrZoomAzMin *pi/180);
  rDY_ZAMin := rR_Z0*cos(mrZoomAzMin *pi/180);

  rR_ZA := sqrt((rDX_ZAMax-rDX_ZAMin)*(rDX_ZAMax-rDX_ZAMin) + (rDY_ZAMax-rDY_ZAMin)*(rDY_ZAMax-rDY_ZAMin));

  if(mrZoomHgtMax-mrZoomHgtMin > 5) then
  //if(mrZoomHgtMax-mrZoomHgtMin > 0) then
  begin
    rDX := (rDX - rDX_Z0)*(2*iR0)/abs(rR_ZHMin-rR_ZHMax);
    rDY := (rDY - rDY_Z0)*(2*iR0)/abs(rR_ZHMin-rR_ZHMax);
  end
  else
  begin
    rDX := (rDX*iR0/rR_ZA);
    rDY := (rDY*iR0/rR_ZA);
  end;
end;

procedure TF__ASTROTOOLBOX.StarMapDims(Control: TControl; var iR0: Integer; var iX0: Integer; var iY0: Integer);
begin
  iR0 := Control.Height div 2; // Min(SHP__MAP.Height, SHP__MAP.Width);
  iX0 := (Control.Width - 2) div 2;
  iY0 := (Control.Height - 2) div 2;
end;

procedure TF__ASTROTOOLBOX.ExecDevices();
begin
  PC__WORKBENCH.ActivePageIndex := ciPAGE_DEVICES;
  PC__WORKBENCHChange(nil);

  EnableMenu('MENU__DEVICES');
end;

procedure TF__ASTROTOOLBOX.ExecAlbireo();
begin
  PC__WORKBENCH.ActivePageIndex := ciPAGE_ALBIREO;
  PC__WORKBENCHChange(nil);
  EnableMenu('MENU__ALBIREO');
end;

procedure TF__ASTROTOOLBOX.ExecHA();
begin
  PC__WORKBENCH.ActivePageIndex := ciPAGE_HA;
  PC__WORKBENCHChange(nil);
  EnableMenu('MENU__HA');

  if(CB__AO.Items.Count > 0) then
  begin
    CB__AO.Text := CB__AO.Items[0];
    ShowAO();
  end;
end;

procedure TF__ASTROTOOLBOX.ExecTable();
begin
  PC__WORKBENCH.ActivePageIndex := ciPAGE_DB;
  PC__WORKBENCHChange(nil);
  EnableMenu('MENU__TABLE');

  Application.ProcessMessages;

  //ShowAOTable(-2,crMagPosStd);
  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.ExecMAP();
begin
  PC__WORKBENCH.ActivePageIndex := ciPAGE_STARMAP;
  PC__WORKBENCHChange(nil);
  EnableMenu('MENU__STARMAP');
  P__SEARCHALL.Visible:=true;

  miTimePlayMode := 0;

  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.ExecSolSys();
begin
  PC__WORKBENCH.ActivePageIndex := ciPAGE_SOLSYS;
  PC__WORKBENCHChange(nil);
  EnableMenu('MENU__SOLSYS');

  Application.ProcessMessages;

  ShowSolSys();

end;

procedure TF__ASTROTOOLBOX.AnimateButton(iMode: Integer; xLabel: TLabel);
// iMode: -1: OnMouseEnter; 0:OnMouseMove; 1: OnMouseLeave
var
  FColor: TColor;
begin
  case iMode of
    -1: FColor := clWhite;
    0: FColor := clWhite;
    1: FColor := clSilver;
    else FColor := clGray;
  end;

  if(xLabel.Name = 'L__ALBIREO') then
  begin
    if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_ALBIREO) then
      xLabel.Font.Color := FColor;
  end
  else if(xLabel.Name = 'L__HA') then
  begin
    if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_HA) then
      xLabel.Font.Color := FColor;
  end
  else if(xLabel.Name = 'L__TABLE') then
  begin
    if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_DB) then
      xLabel.Font.Color := FColor;
  end
  else if(xLabel.Name = 'L__MAP') then
  begin
    if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_STARMAP) then
      xLabel.Font.Color := FColor;
  end
  else if(xLabel.Name = 'L__DEVICES') then
  begin
    if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_DEVICES) then
      xLabel.Font.Color := FColor;
  end
  else if(xLabel.Name = 'L__NAVSOLSYS') then
  begin
    if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_SOLSYS) then
      xLabel.Font.Color := FColor;
  end
  else if(xLabel.Name = 'L__ASTROCALC') then
  begin
    if(not Assigned(F__ASTROCALC)) then
      xLabel.Font.Color := FColor;
  end
end;

function TF__ASTROTOOLBOX.CheckOnConRA(rRA, rDEC: Real; var slMatchedCon: TStringList): Boolean;
{
Checks, if co-ordinates rRA and rDEC are nearly by a RA/DEC boundary (polygon point) of a constellation. If yes,
the constellation ID is returned.
}
var
  i: Integer;
  slBuf, slRA: TStringList;
  rRA_Con, rDEC_Con: Real;
  iHH, iMM: Word;
  //rSS: Real;
  //iMatchCnt: Integer;
  //sTest: string;
  bFound: Boolean;
  sCon: string;
begin
  sCon := '';
  Result := false;
  bFound := false;

  //iMatchCnt := 0;

  slBuf := TStringList.Create;
  slBuf.Delimiter:=';';

  slRA := TStringList.Create;
  slRA.Delimiter:=':';

  //Memo2.Clear;

  i:=0;
  while (i < mslCBList.Count) do
  begin
    slBuf.DelimitedText := mslCBList[i];
    //if(slBuf.Count < 3) then continue;
    //sTest := slBuf.Text;
    if(slBuf.Count = 3) then
    begin
      //ShowMessage(IntToStr(slBuf.Count));
      slRA.DelimitedText := slBuf[1];

      if(slRA.Count = 3) then
      begin
        iHH := StrToInt(slRA[0]);
        iMM := StrToInt(slRA[1]);
        //rSS := StrToFloat(slRA[2]);

        rDEC_Con := StrToFloat(AnsiReplaceStr(slBuf[2],'.',DefaultFormatSettings.DecimalSeparator));
        //rRA_Con := HH_MM_SSToDeg(iHH,iMM,0);
        rRA_Con := iHH + iMM/60.0;

        bFound := (abs(rRA_Con - rRA) < 0.1) and (abs(rDEC_Con - rDEC) < 10);

        if(bFound) then
        begin
          sCon := slBuf[0];
          slMatchedCon.Add(sCon);
        end;
      end;
    end;

    Inc(i);
  end;

  // Find the best matched constellation (left/right Con from the RA-boundary)
  (*
  for j:=0 to slMatchedCon.Count-1 do
  begin
    ShowMessage('CheckOnConRA: ' + slMatchedCon[j]);
  end;
  *)
  Result := (slMatchedCon.Count > 0);

  slBuf.Destroy;
  slRA.Destroy;
end;

procedure TF__ASTROTOOLBOX.FindConRABoundary(rRA,rDEC: Real; iDirRA, iDirDEC: Integer; var slMatchedCon: TStringList);
{Returns the RA-Boundary of a constellation
Approach: Increase RA by a give object until a RA of a constellation is matched.
  Return this matching constalletion RA
Further Arg: Also Moving +- on DEC
}
var
  i: Integer;
  rD, rStep, rRACurr, rDECCurr: Real;
  bFound: Boolean;
  //sCon: string;
  bQuit: Boolean;
begin
  i:= 0;

  rRACurr := rRA;
  rDECCurr := rDEC;

  bQuit := false;

  rStep := 0.1;
  rD := 0;
  bFound := false;
  while (not bQuit) and (not bFound) do
  begin
    rD := rStep*i;

    // RA-Move
    if(iDirRA <> 0) then
    begin
      if(iDirRA = -1) then
      begin
        rRACurr := rRA - rD;
        if (rRACurr < 0) then
          rRACurr := rRACurr + 24;
      end
      else
      begin
        rRACurr := rRA + rD;
        if (rRACurr >= 24) then
          rRACurr := rRACurr - 24;
      end;

      bQuit := (rD > 24.0);
    end;

    // DEC-Move
    if(iDirDEC <> 0) then
    begin
      if(iDirDEC = 1) then // North
      begin
        rDECCurr := rDEC + rD;
      end
      else // South
      begin
        rDECCurr := rDEC - rD;
      end;

      bQuit := (rD > 90);
    end;

    bFound := CheckOnConRA(rRACurr, rDECCurr, slMatchedCon);

    Inc(i);
  end;

end;

function TF__ASTROTOOLBOX.FindCon(rRA,rDEC: Real): TStringList;
var
  //i,iRes: Integer;
  slMatchedConW, slMatchedConE: TStringList;
  slMatchedConN, slMatchedConS: TStringList;
  slRegCon, slRegCnt: TStringList;
begin
  Result := TStringList.Create;

  slRegCon := TStringList.Create;
  slRegCnt := TStringList.Create;

  slMatchedConW := TStringList.Create;
  slMatchedConE := TStringList.Create;
  slMatchedConN := TStringList.Create;
  slMatchedConS := TStringList.Create;

  FindConRABoundary(rRA,rDEC, -1, 0, slMatchedConW);
  FindConRABoundary(rRA,rDEC, 1, 0, slMatchedConE);
  FindConRABoundary(rRA,rDEC, 0, 1, slMatchedConN);
  FindConRABoundary(rRA,rDEC, 0, -1, slMatchedConS);

  // Register & count constellations
  RegListItems(slMatchedConW,false,slRegCon,slRegCnt,Result);
  RegListItems(slMatchedConE,false,slRegCon,slRegCnt,Result);
  RegListItems(slMatchedConN,false,slRegCon,slRegCnt,Result);
  RegListItems(slMatchedConS,true,slRegCon,slRegCnt,Result); // Calculate Result-List
  (*
  for i:=0 to 2 do
    if(i < Result.Count) then
      ShowMessage('Winner' + IntToStr(i+1) + ': ' + Result[i]);
  *)
  slMatchedConW.Destroy;
  slMatchedConE.Destroy;
  slMatchedConN.Destroy;
  slMatchedConS.Destroy;

  slRegCnt.Destroy;
  slRegCon.Destroy;
end;

procedure TF__ASTROTOOLBOX.RegListItems(slIn: TStringList; bAnalyse: Boolean; var slRegID: TStringList; var slRegCnt: TStringList; var slRes: TStringList);
var
  i, j, iRes, iVal: Integer;
  iMax, iMaxIndex: Integer;
  slCntDummy: TStringList;
begin
  iMaxIndex := -1;

  for i:=0 to slIn.Count-1 do
  begin
    //ShowMessage('Register Con ' + slIn[i]);

    iRes := slRegID.IndexOf(slIn[i]);
    if(iRes = -1) then
    begin
      slRegID.Add(slIn[i]);
      slRegCnt.Add('1');
    end
    else
    begin
      slRegCnt[iRes] := IntToStr(StrToInt(slRegCnt[iRes]) + 1);
      //format('%.3d',[i]);
    end;
  end;

  if(bAnalyse) then
  begin
    slCntDummy := TStringList.Create;

    // Copy Cnt-List
    for i:=0 to slRegCnt.Count-1 do
      slCntDummy.Add(slRegCnt[i]);

    for j:=0 to slRegCnt.Count-1 do
    begin
      // Calculate Result list
      iMax := 0;
      for i:= 0 to slRegCnt.Count-1 do
      begin
        iVal := StrToInt(slCntDummy[i]); // Important: Use Dummy-stringlist which content changes
        if(iMax < iVal) then
        begin
          iMax := iVal;
          iMaxIndex := i;
        end;
      end;

      if(iMaxIndex < slRegID.Count) then
      begin
        slRes.Add(slRegID[iMaxIndex]);
        slCntDummy[iMaxIndex] := IntToStr(StrToInt(slCntDummy[iMaxIndex])*(-1));
      end;
    end;

    slCntDummy.Destroy;
  end;

end;

procedure TF__ASTROTOOLBOX.ShowMeteoriteShowerGrid(dtDateTime: TDateTime);
{2013/12/14 / fs
Lists all meteorite showers of a selected month in a TStringGrid element
Used TObjectList: molAsteroidShowers
}
var
  i, iPos, iLen, iRow, iVal: Integer;
  sVDate: string;
  iMonth, iMonthSel: Integer;
begin
  iMonth := GetMonth(dtDateTime);

  if(miMSDoneMonth = iMonth) then
    exit;

  if(msLANG_ID = 'DE') then
  begin
    L__MS.Caption := 'Meteorschauer im ' + SetDE_ENMonthNames(FormatDateTime('mmmm',dtDateTime),msLANG_ID);
    GRD__RECOM_MS.Cells[0,0] := 'Strom';
    GRD__RECOM_MS.Cells[1,0] := 'Sternb.';
  end
  else
  begin
    L__MS.Caption := 'Meteor shower in ' + SetDE_ENMonthNames(FormatDateTime('mmmm',dtDateTime),msLANG_ID);
    GRD__RECOM_MS.Cells[0,0] := 'Shower';
    GRD__RECOM_MS.Cells[1,0] := 'Con.';
  end;

  miMSDoneMonth := iMonth;

  iRow := 0;
  GRD__RECOM_MS.RowCount := 2;
  GRD__RECOM_MS.Cells[0,1] := '';
  GRD__RECOM_MS.Cells[1,1] := '';
  GRD__RECOM_MS.Cells[2,1] := '';
  GRD__RECOM_MS.Cells[3,1] := '';

  for i:=0 to molMeteorShowers.Count-1 do
  begin
    sVDate := (molMeteorShowers[i] as TMeteorShower).sMaxDateMonth;
    iLen := length(sVDate);
    iPos := Pos('-',sVDate);
    iMonthSel := StrToInt(Copy(sVDate,iPos+1,iLen-iPos));
    if(iMonthSel = iMonth) then
    begin
      Inc(iRow);

      (molMeteorShowers[i] as TMeteorShower).bActive:=true;

      // Save meteor shower object in first cell for detail dialogue access
      GRD__RECOM_MS.Objects[0,iRow] := (molMeteorShowers[i] as TMeteorShower);

      if(msLANG_ID = 'DE') then
        GRD__RECOM_MS.Cells[0,iRow] := (molMeteorShowers[i] as TMeteorShower).sName_DE;
      if(msLANG_ID = 'EN') then
        GRD__RECOM_MS.Cells[0,iRow] := (molMeteorShowers[i] as TMeteorShower).sName_EN;

      GRD__RECOM_MS.Cells[1,iRow] := (molMeteorShowers[i] as TMeteorShower).sSign;
      iVal := (molMeteorShowers[i] as TMeteorShower).iObjPerHour;
      if(iVal > 0) then
        GRD__RECOM_MS.Cells[2,iRow] := IntToStr(iVal);

      iVal := (molMeteorShowers[i] as TMeteorShower).iSpeed;
      if(iVal > 0) then
      GRD__RECOM_MS.Cells[3,iRow] := IntToStr(iVal);

      GRD__RECOM_MS.RowCount:=GRD__RECOM_MS.RowCount+1;
    end;
  end;
  // Delete last empty line
  if(GRD__RECOM_MS.RowCount > 2) then
    GRD__RECOM_MS.RowCount := GRD__RECOM_MS.RowCount-1;


end;

procedure TF__ASTROTOOLBOX.ChangeADEVName(sNewName: string);
var
  iIndex: Integer;
  sName: string;
  ADevice: TADevice;
begin
  sName := Trim(CB__ADEV_NAME.Text);
  iIndex := -1;
  if(sName = '') then exit;

  ADevice := GetADev(sName, iIndex);

  if(ADevice <> nil) and (iIndex >= 0) then
  begin
    (molADevList[iIndex] as TADevice).sName:=sNewName;
    CB__ADEV_NAME.Items[CB__ADEV_NAME.ItemIndex]:=sNewName;

    mbADEVChanged := true; // Save after finishing program
  end;

end;

procedure TF__ASTROTOOLBOX.EnableDevButtons();
begin
  if(CB__ADEV_NAME.Text <> '') then
    B__ADEV_DEL.Enabled := true
  else
    B__ADEV_DEL.Enabled := false;

  if(ED__ADEV_DESCR.Text <> '') or
    (ED__ADEV_MANU.Text <> '') or
    (ED__ADEV_ARTNO.Text <> '') or
    (ED__ADEV_FW.Text <> '') or
    (ED__ADEV_APT.Text <> '') then
  begin
    B__ADEV_MOD.Enabled := true;
  end
  else
  begin
    B__ADEV_MOD.Enabled := false;
  end;

end;

(*
procedure TF__ASTROTOOLBOX.SaveObservations;
{2013/05/11 / fs
Store Observation data into file
}
var
  i: Integer;
  slBuffer: TStringList;
  Observation: TObservation;
  tfObs: TextFile;
  sLine,sVar: string;
begin
  BeginMethod('SaveObservations');

  // Import astronomical observation list
  AssignFile(tfObs,'Observations.dat');
  ReWrite(tfObs);

  for i:=0 to molObservationList.Count-1 do
  begin
    sLine := (molObservationList[i] as TObservation).sName;
    sLine := (molObservationList[i] as TObservation).sADevName;
    sLine := sLine + ';' + FloatToStr((molObservationList[i] as TObservation).dtTimestamp);
    sLine := sLine + ';' + (molObservationList[i] as TObservation).sImagePath;
    sLine := sLine + ';' + (molObservationList[i] as TObservation).sNotes;
    sLine := sLine + ';' + (molObservationList[i] as TObservation).sCamera;
    sLine := sLine + ';' + IntToStr((molObservationList[i] as TObservation).iExposureTime1_sec);
    sLine := sLine + ';' + IntToStr((molObservationList[i] as TObservation).iNoOfImages1);
    sLine := sLine + ';' + IntToStr((molObservationList[i] as TObservation).iExposureTime2_sec);
    sLine := sLine + ';' + IntToStr((molObservationList[i] as TObservation).iNoOfImages2);
    sLine := sLine + ';' + IntToStr((molObservationList[i] as TObservation).iExposureTime3_sec);
    sLine := sLine + ';' + IntToStr((molObservationList[i] as TObservation).iNoOfImages3);

    WriteLn(tfObs,sLine);
  end;

  CloseFile(tfObs);

  SetMsg('');
  EndMethod('SaveObservations');
end;

procedure TF__ASTROTOOLBOX.LoadObservations;
{2013/05/11 / fs
Load Observation data from file
}
var
  i: Integer;
  slBuffer: TStringList;
  Observation: TObservation;
  tfObs: TextFile;
  sLine,sVar: string;
begin
  BeginMethod('LoadObservations');

  slBuffer := TStringList.Create;
  slBuffer.Delimiter:=';';

  // Import astronomical observation list
  AssignFile(tfObs,'Observations.dat');
  Reset(tfObs);

  while not eof(tfObs) do
  begin
    Observation := nil;
    slBuffer.Clear;
    ReadLn(tfObs,sLine);
    sLine := AnsiReplaceStr(sLine,' ','~');
    SetMsg(sLine);

    slBuffer.DelimitedText:=sLine;

    try
    for i:=0 to slBuffer.Count-1 do
    begin
      if(i = 0) then
      begin
        // Generation and Initialisation
        Observation := TObservation.Create;

        Observation.dtTimestamp:=0;
        Observation.iExposureTime1_sec:=-999;
        Observation.iNoOfImages1:=-999;
        Observation.iExposureTime2_sec:=-999;
        Observation.iNoOfImages2:=-999;
        Observation.iExposureTime3_sec:=-999;
        Observation.iNoOfImages3:=-999;
      end;

      sVar := Trim(slBuffer[i]);
      case i of
        0: Observation.sName := slBuffer[i];
        1: Observation.sADevName := slBuffer[i];
        2: if(StrIsNum(true,sVar)) then Observation.dtTimestamp := StrToFloat(sVar);
        3: Observation.sImagePath := slBuffer[i];
        4: Observation.sNotes := AnsiReplaceStr(slBuffer[i],'~',' ');
        5: Observation.sCamera := AnsiReplaceStr(slBuffer[i],'~',' ');
        6: if(StrIsNum(false,sVar)) then Observation.iExposureTime1_sec := StrToInt(sVar);
        7: if(StrIsNum(false,sVar)) then Observation.iNoOfImages1 := StrToInt(sVar);
        8: if(StrIsNum(false,sVar)) then Observation.iExposureTime2_sec := StrToInt(sVar);
        9: if(StrIsNum(false,sVar)) then Observation.iNoOfImages2 := StrToInt(sVar);
       10: if(StrIsNum(false,sVar)) then Observation.iExposureTime3_sec := StrToInt(sVar);
       11: if(StrIsNum(false,sVar)) then Observation.iNoOfImages3 := StrToInt(sVar);
      end; // case
    end;
    if(Observation <> nil) then
    begin
      molObservationList.Add(Observation);
    end;
    except
      on e: Exception do
      begin
        MessageDlg('Error in Line: ' + sLine,mtError,[mbOK],0);
      end;
    end;
  end;

  CloseFile(tfObs);

  SetMsg('');
  EndMethod('LoadObservations');
end;
*)
procedure TF__ASTROTOOLBOX.CalcMagProp;
var
  iADEV_FW, iOCULAR_FW, iMag, iAperture: Integer;
  fMaxMag: Real;
  iNum: Integer;
begin
  iOCULAR_FW := -1;
  iADEV_FW := -1;

  if(Trim(ED__OCULAR_FW.Text) = '') or (StrToInt(ED__OCULAR_FW.Text) = 0) then exit;

  if(Trim(ED__ADEV_FW.Text) <>'') and (Trim(ED__OCULAR_FW.Text) <> '') then
  begin
    iADEV_FW := StrToInt(ED__ADEV_FW.Text);
    iOCULAR_FW := StrToInt(ED__OCULAR_FW.Text);
    //if(iOCULAR_FW > 0) then
    iMag := Round(iADEV_FW/(1.0*iOCULAR_FW));

    // Gesichtfeld (Wahres Feld)
    ED__MAG.Text := IntToStr(iMag);

    if(CB__ADEV_OK_TYPE.Text = 'Kellner') then
      ED__OK_VIEW.Text := FloatToStrF(40.0 / iMag,ffFixed,8,2)
    else if(CB__ADEV_OK_TYPE.Text = 'Plössl') then
      ED__OK_VIEW.Text := FloatToStrF(50.0 / iMag,ffFixed,8,2)
    else if(CB__ADEV_OK_TYPE.Text = 'Super-Plössl') then
      ED__OK_VIEW.Text := FloatToStrF(52.0 / iMag,ffFixed,8,2)
    else if(CB__ADEV_OK_TYPE.Text = 'Ultra-Wide-Angle') then
      ED__OK_VIEW.Text := FloatToStrF(66.0 / iMag,ffFixed,8,2)
    else if(CB__ADEV_OK_TYPE.Text = 'Panoptik') then
      ED__OK_VIEW.Text := FloatToStrF(68.0 / iMag,ffFixed,8,2)
    else if(CB__ADEV_OK_TYPE.Text = 'Nagler') then
      ED__OK_VIEW.Text := FloatToStrF(82.0 / iMag,ffFixed,8,2)
    else
      ED__OK_VIEW.Text := FloatToStrF(40.0 / iMag,ffFixed,8,2);


    if(Trim(ED__ADEV_APT.Text) <> '') then
    begin
      iAperture := StrToInt(ED__ADEV_APT.Text);
      if(iAperture > 0) and (iADEV_FW > 0) then
        GRD__TELPROP.Cells[1,4] := FloatToStrF((1.0*iOCULAR_FW) /((1.0*iADEV_FW)/iAperture),ffFixed,8,1) + ' mm';

      if(iAperture > 0) then
      begin
        iNum := 4850;

        fMaxMag := 7.5 + 5*log10(iAperture/10);
        GRD__TELPROP.Cells[1,6] := FloatToStrF(fMaxMag,ffFixed,8,1);

        if(fMaxMag >= 6) and (fMaxMag < 7) then
          iNum := Round((14300-4850)*(fMaxMag - 6)/(7-6) + 4850)
        else if(fMaxMag >= 7) and (fMaxMag < 8) then
          iNum := Round((41000-14300)*(fMaxMag - 7)/(8-7) + 14300)
        else if(fMaxMag >= 8) and (fMaxMag < 9) then
          iNum := Round((117000-41000)*(fMaxMag - 8)/(9-8) + 41000)
        else if(fMaxMag >= 9) and (fMaxMag < 10) then
          iNum := Round((324000-117000)*(fMaxMag - 9)/(10-9) + 117000)
        else if(fMaxMag >= 10) and (fMaxMag < 11) then
          iNum := Round((870000-324000)*(fMaxMag - 10)/(11-10) + 324000)
        else if(fMaxMag >= 11) and (fMaxMag < 12) then
          iNum := Round((2270000-870000)*(fMaxMag - 11)/(12-11) + 870000)
        else if(fMaxMag >= 12) and (fMaxMag < 13) then
          iNum := Round((5700000-2270000)*(fMaxMag - 12)/(13-12) + 2270000)
        else if(fMaxMag >= 13) and (fMaxMag < 14) then
          iNum := Round((13800000-5700000)*(fMaxMag - 13)/(14-13) + 5700000)
        else if(fMaxMag >= 14) and (fMaxMag < 15) then
          iNum := Round((32000000-13800000)*(fMaxMag - 14)/(15-14) + 13800000)
        else if(fMaxMag >= 15) and (fMaxMag < 16) then
          iNum := Round((71000000-32000000)*(fMaxMag - 15)/(16-15) + 32000000)
        else if(fMaxMag >= 16) and (fMaxMag < 17) then
          iNum := Round((150000000-71000000)*(fMaxMag - 16)/(17-16) + 71000000)
        else if(fMaxMag >= 17) and (fMaxMag < 18) then
          iNum := Round((296000000-150000000)*(fMaxMag - 17)/(18-17) + 150000000)
        else if(fMaxMag >= 18) and (fMaxMag < 19) then
          iNum := Round((560000000-296000000)*(fMaxMag - 18)/(19-18) + 296000000)
        else if(fMaxMag >= 19) and (fMaxMag <= 20) then
          iNum := Round((1000000000-560000000)*(fMaxMag - 19)/(20-19) + 560000000);

        if(iNum > 1000000) then
        begin
          iNum := iNum div 1000000;
          iNum := iNum * 1000000;
        end
        else if(iNum > 100000) then
        begin
          iNum := iNum div 10000;
          iNum := iNum * 10000;
        end
        else if(iNum > 10000) then
        begin
          iNum := iNum div 1000;
          iNum := iNum * 1000;
        end
        else if(iNum > 1000) then
        begin
          iNum := iNum div 100;
          iNum := iNum * 100;
        end;

        if(fMaxMag <= 20) then
          GRD__TELPROP.Cells[1,7] := Format('%.0n', [iNum / 1])//IntToStr(iNum)
        else if(fMaxMag > 20) then
          GRD__TELPROP.Cells[1,7] := '> ' + Format('%.0n', [iNum / 1]);

      end;

    end;
  end;

  ED__OCULAR_FW.Color := $00004080;

  if (iOCULAR_FW > 0) and (mfMagMin > 0) and (mfMagMax > 0) and (iADEV_FW > 0) then
  begin
    if (iOCULAR_FW > 1.0*iADEV_FW/mfMagMin) or (iOCULAR_FW < 1.0*iADEV_FW/mfMagMax) then
      ED__OCULAR_FW.Color := clRed;
  end


end;

procedure TF__ASTROTOOLBOX.CalcADEVProp;
var
  iADEV_FW, iAperture : Integer;
  iOptArea_qcm: Integer;
  fTransmission: Real;
begin
  GRD__TELPROP.Cells[1,1] := '';
  GRD__TELPROP.Cells[1,2] := '';
  GRD__TELPROP.Cells[1,3] := '';
  GRD__TELPROP.Cells[1,4] := '';
  GRD__TELPROP.Cells[1,5] := '';

  if(Trim(ED__ADEV_FW.Text) <>'') and (Trim(ED__ADEV_APT.Text) <> '') then
  begin
    iADEV_FW :=StrToInt(ED__ADEV_FW.Text);
    iAperture := StrToInt(ED__ADEV_APT.Text);
    mfMagMax := 2*iAperture; //iAperture;

    if(iAperture > 0) then
    begin
      GRD__TELPROP.Cells[1,1] := FloatToStrF((1.0*iADEV_FW)/iAperture,ffFixed,8,1);
      GRD__TELPROP.Cells[1,5] := FloatToStrF(115.0/iAperture,ffFixed,8,1) + ' arcsec';
      mfMagMin := 1.0*iAperture/GetEyePupil(miBirthYear);
      GRD__TELPROP.Cells[1,2] := FloatToStrF(mfMagMin,ffFixed,8,1) + ' ... ' + FloatToStrF(mfMagMax,ffFixed,8,1);

      if(mfMagMin > 0) then
        GRD__TELPROP.Cells[1,3] := FloatToStrF(iADEV_FW / (1.0*mfMagMax),ffFixed,8,1) + 'x ... ' + FloatToStrF(1.0*iADEV_FW/mfMagMin,ffFixed,8,1) + 'x';

      iOptArea_qcm := 0; fTransmission:=0;
      mrS0 := GetStarBaseSignal(iAperture, CB__ADEV_TYPE.Text, iOptArea_qcm, fTransmission);

      GRD__TELPROP.Cells[1,8] :=IntToStr(iOptArea_qcm);
      GRD__TELPROP.Cells[1,9] := IntToStr(Round(fTransmission*100));

    end;
  end;
end;

procedure TF__ASTROTOOLBOX.IniCB__ADEV();
{2013/04/25 / fs
Initializing combobox with astronomical device names
}
var
  i: Integer;
begin
  CB__ADEV_NAME.Text := '';
  CB__ADEV_NAME.Items.Clear;
  for i:=0 to molADevList.Count-1 do
  begin
    CB__ADEV_NAME.Items.Add((molADevList[i] as TADevice).sName);
  end;
end;

procedure TF__ASTROTOOLBOX.SetDefaultADev(ADevice: TADevice);
var
  i: Integer;
begin
  if(ADevice = nil) then
    exit;

  if(CBX__TELESCOPE_DEF.Checked) then
  begin
    for i:=0 to molADevList.Count-1 do
      (molADevList[i] as TADevice).iDefault:=0;

    ADevice.iDefault:=1;
  end
  else
    ADevice.iDefault:=0;

end;

procedure TF__ASTROTOOLBOX.AddADev();
{April 2013/fs
Add Astronomical Device
}
var
  ADevice: TADevice;
  sName, sMsg, sMsg1: string;
begin
  sName := '';

  if(msLANG_ID = 'DE') then
  begin
    sMsg1 := 'Teleskop erfassen';
    sMsg := 'Mein neues Kürzel:';
  end
  else
  begin
    sMsg1 := 'Telescope registration';
    sMsg := 'My new telesope ID:';
  end;

  if((not InputQuery(sMsg1,sMsg,sName)) or (Trim(sName) = '')) then
    exit;

  ADevice := TADevice.Create;

  ADevice.sName := sName;

  ADevice.sDescription := ED__ADEV_DESCR.Text;
  ADevice.sManufacturer := ED__ADEV_MANU.Text;
  ADevice.sArtNoManu := ED__ADEV_ARTNO.Text;
  ADevice.sType := CB__ADEV_TYPE.Text;
  SetDefaultADev(ADevice);

  if(ED__ADEV_FW.Text <> '') then
    ADevice.iFocalWidthDev_mm := StrToInt(ED__ADEV_FW.Text);
  if(ED__ADEV_APT.Text <> '') then
    ADevice.iDiameter_mm := StrToInt(ED__ADEV_APT.Text);

  molADevList.Add(ADevice);

  CB__ADEV_NAME.Items.Add(sName);
  CB__ADEV_NAME.Text:=sName;

  if(msLANG_ID = 'DE') then
    sMsg := 'Geräteinformationen angelegt.';

  if(msLANG_ID = 'EN') then
    sMsg := 'Device information added.';

  IniADev();

  MessageDlg(sMsg,mtInformation,[mbOK],0);

end;

procedure TF__ASTROTOOLBOX.DelADev();
var
  sMsg: string;
  iIndex, iSelIndex: Integer;
begin
  if(msLANG_ID = 'DE') then
    sMsg := 'Möchten Sie diese Teleskopdaten wirklich löschen?'
  else
    sMsg := 'Do you really want to delete the selected telescope data?';

  if(MessageDlg(sMsg,mtConfirmation,[mbYes,mbNo],0) = mrNo) then exit;

  iIndex := -1;
  GetADev(CB__ADEV_NAME.Text,iIndex);

  if(iIndex > -1) then
  begin
    molADevList.Delete(iIndex);

    ED__ADEV_DESCR.Text := '';
    ED__ADEV_MANU.Text := '';
    ED__ADEV_ARTNO.Text := '';
    CB__ADEV_TYPE.Text := '';

    IMG__ADEV_PHOTO.Picture := nil;

    iSelIndex := CB__ADEV_NAME.ItemIndex;
    if(iSelIndex > -1) then
      CB__ADEV_NAME.Items.Delete(iSelIndex);

    mbADEVChanged := true;
  end;

  if(msLANG_ID = 'DE') then
    sMsg := 'Ausgewählte Teleskopdaten gelöscht.';

  if(msLANG_ID = 'EN') then
    sMsg := 'Selected telescope data deleted.';

  IniADev();

  MessageDlg(sMsg,mtInformation,[mbOK],0);
end;

procedure TF__ASTROTOOLBOX.ShowADev(ADevice: TADevice);
{2013/04/16 / fs
Show Astronomical Device
}
//var
//  iSelIndex, iCBIndex: Integer;
begin
  if(ADevice = nil) then exit;

  try
    Screen.Cursor:=crHourGlass;

    IMG__ADEV_PHOTO.Picture := nil;

    ED__ADEV_DESCR.Text := ADevice.sDescription;
    ED__ADEV_MANU.Text := ADevice.sManufacturer;
    ED__ADEV_ARTNO.Text := ADevice.sArtNoManu;
    CB__ADEV_TYPE.Text:= ADevice.sType;

    if(ADevice.iFocalWidthDev_mm > 0) then
      ED__ADEV_FW.Text := IntToStr(ADevice.iFocalWidthDev_mm);
    if(ADevice.iDiameter_mm > 0) then
      ED__ADEV_APT.Text := IntToStr(ADevice.iDiameter_mm);

    CBX__TELESCOPE_DEF.Checked := (ADevice.iDefault = 1);

    if(ADevice.sImagePath <> '') then
      IMG__ADEV_PHOTO.Picture.LoadFromFile(ConvertWinPath(ADevice.sImagePath));

  finally
    Screen.Cursor:=crDefault;
  end;

end;

function TF__ASTROTOOLBOX.GetADev(sName: string; var iIndex: Integer): TADevice;
{April 2013/fs
Identify and return Astronomical Device by name
}
var
  i: Integer;
  bFound: Boolean;
begin
  Result := nil;
  iIndex := -1;

  i:=0;
  bFound := false;
  while (not bFound) and (i < molADevList.Count)  do
  begin
    bFound := ((molADevList[i] as TADevice).sName = sName);
    if not bFound then
      Inc(i);
  end;

  if(bFound) then
  begin
    iIndex := i;
    Result := (molADevList[i] as TADevice);
  end;

end;

procedure TF__ASTROTOOLBOX.ModADev();
{2013/04/16 / fs
Modify Astronomical Device
}
var
  iIndex: Integer;
begin
  iIndex := -1;
  GetADev(CB__ADEV_NAME.Text,iIndex);

  if(iIndex < 0) then exit;

  (molADevList[iIndex] as TADevice).sDescription := ED__ADEV_DESCR.Text;
  (molADevList[iIndex] as TADevice).sManufacturer := ED__ADEV_MANU.Text;
  (molADevList[iIndex] as TADevice).sArtNoManu := ED__ADEV_ARTNO.Text;
  (molADevList[iIndex] as TADevice).sType := CB__ADEV_TYPE.Text;
  SetDefaultADev((molADevList[iIndex] as TADevice));

  if(ED__ADEV_FW.Text <> '') then
    (molADevList[iIndex] as TADevice).iFocalWidthDev_mm := StrToInt(ED__ADEV_FW.Text);
  if(ED__ADEV_APT.Text <> '') then
    (molADevList[iIndex] as TADevice).iDiameter_mm := StrToInt(ED__ADEV_APT.Text);


end;

procedure TF__ASTROTOOLBOX.GenCB(Panel: TPanel; iR0, iX0, iY0: Integer);
{2013/03/28 / fs
Generation of constellation boundaries
}
var
  iDEC_MM: SmallInt;
  iDEC_DEG, iDEC_SS: SmallInt;
  i: Integer; //, iR0, iX0, iY0: Integer;
  rDEC_SS: Real;
  sCon, sConS: string;
  iHA_HH,iHA_MM,iHA_SS,iHA_MS: Word;
  rDec, rAz, rHgt: Real;
  //rDecS, rAzS, rHgtS, rRS, rDXS, rDYS: Real;
  dJulDat, dtWT, dtST, dtRA, dtHA: TDateTime;
  slBuf: TStringList;
  iLeft, iTop: Integer;
  //rDX, rDY: Real;
  bBroken: Boolean;
begin
  if not mbShowCB then exit;
  bBroken := false;

  rAz:=0; rHgt:=0;
  //rDXS := 0; rDYS := 0; rHgtS := 0; rAzS := 0;

  dJulDat := 0;

  dtWT := GetWTime();
  dtST := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat);

  sConS := '';
  i:=0;

  slBuf := TStringList.Create();

  Panel.Canvas.Pen.Color:= TColor($004080);//clBlue;

  while i < mslCBList.Count do
  begin
    slBuf.Delimiter:=';';
    slBuf.DelimitedText:=mslCBList[i];

    if(slBuf.Count = 3) then
    begin
      iDEC_DEG:=0; iDEC_MM:=0; iDEC_SS:=0;rDEC_SS:=0;
      iHA_HH:=0;iHA_MM:=0;iHA_SS:=0;iHA_MS:=0;

      sCon := Trim(slBuf[0]);
      dtRA := StrToTimeMS(Trim(slBuf[1]),'.');
      rDec := StrToFloatExt(Trim(slBuf[2]));

      //DegToDEG_MM_SS(rDEC,iDEC_DEG,iDEC_MM,rDEC_SS,false);
      DegToDEG_MM_SS2(rDEC,iDEC_DEG,iDEC_MM,rDEC_SS);

      dtHA := GetHA(dtST,dtRA);

      DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);
      CalcAZ(iDEC_DEG,iDEC_MM,iDEC_SS,iHA_HH,iHA_MM,iHA_SS,rAz,rHgt);

      rHgt := Pi*rHgt/180;
      rAz := Pi*rAz/180;

      iLeft := 0; iTop := 0;
      if GenMapPos(iR0, iX0, iY0, rAz*180/Pi,rHgt*180/Pi,iLeft,iTop) then
      begin
        if((i = 0) or (sConS <> sCon) or (not CanPlotAO(rAz*180/Pi,rHgt*180/Pi)) or (bBroken)) then
          // Plot broken line by Hgt=0
          Panel.Canvas.MoveTo(iLeft,iTop)
        else
          Panel.Canvas.LineTo(iLeft,iTop);

        bBroken := false;
        sConS := sCon;
      end
      else
        bBroken := true;

    end;
    Inc(i);
  end;

  slBuf.Destroy;

end;

{
procedure TF__ASTROTOOLBOX.AOVisOnMouseLeave(Sender: TObject);
var
  iIndex: Integer;
begin
  iIndex := -1;

  if((Sender as TComponent).Name = 'IMG__POS_MOON') then
    iIndex := molAOList.Count-1
  else if((Sender as TComponent).Name = 'SHP__POS_SUN') then
    iIndex := molAOList.Count-2
  else
    iIndex := (Sender as TComponent).Tag;// (Sender as TShape).Tag;
  (*
  if((iIndex >= 0) and (iIndex < molAOList.Count)) then
  begin
    (molAOList[iIndex] as TAObject).SHP.Height := (molAOList[iIndex] as TAObject).SHP.Height - 5;
    (molAOList[iIndex] as TAObject).SHP.Width := (molAOList[iIndex] as TAObject).SHP.Width - 5;
  end;
  *)
end;
}

procedure TF__ASTROTOOLBOX.AOVisOnMouseEnter(Sender: TObject);
var
  iIndex: Integer;
begin
  iIndex := -1;

  if((Sender as TComponent).Name = 'IMG__POS_MOON') then
    iIndex := molAOList.Count-1
  else if((Sender as TComponent).Name = 'SHP__POS_SUN') then
    iIndex := molAOList.Count-2
  else
    iIndex := (Sender as TComponent).Tag;// (Sender as TShape).Tag;

  if((iIndex >= 0) and (iIndex < molAOList.Count)) then
  begin
    //(molAOList[iIndex] as TAObject).SHP.Height := (molAOList[iIndex] as TAObject).SHP.Height + 5;
    //(molAOList[iIndex] as TAObject).SHP.Width := (molAOList[iIndex] as TAObject).SHP.Width + 5;

    MENU__PLCOM_PATH.Checked := false;

    if((molAOList[iIndex] as TAObject).sAOType = 'P') then
      MENU__PLCOM_PATH.Checked := (molAOList[iIndex] as TPlanet).bShowPath;

    if((molAOList[iIndex] as TAObject).sAOType = 'C') then
      MENU__PLCOM_PATH.Checked := (molAOList[iIndex] as TComet).bShowPath;

  end;

end;

procedure TF__ASTROTOOLBOX.ShowAOVis(iIndex: Integer);
var
  sCommentPrev: string;
begin
  if(iIndex >= 0) and (iIndex < molAOList.Count) then // and ((molAOList[iIndex] as TAObject).sAOType <> 'E') then
  begin
    sCommentPrev:= '';

    if(LeftStr((molAOList[iIndex] as TAObject).sAOType,1) = 'S')
      and ((molAOList[iIndex] as TStar).sSpType = 'MARK-VOID') then
    begin
      ShowAstroVoidForm(
      GetAOLabel((molAOList[iIndex] as TStar),msLANG_ID),
      (molAOList[iIndex] as TStar).sCon,
      Round((molAOList[iIndex] as TStar).rDist_XLY),
      Round((molAOList[iIndex] as TStar).rSolFrac)
      )
    end
    else
    begin
      F__AOVIS := TF__AOVIS.Create(nil);
      F__AOVIS.msAlbireoLocalDir:=gsAlbireoLocalDir;

      F__AOVIS.msLANG_ID := msLANG_ID;
      F__AOVIS.mAObject := (molAOList[iIndex] as TAObject);
      F__AOVIS.molSignList := molSignList;
      F__AOVIS.mrSin_fGLat := mrSin_fGLat;
      F__AOVIS.mrCos_fGLat := mrCos_fGLat;
      F__AOVIS.miDST_HH := miDST_HH;
      F__AOVIS.miUTC_HH := miUTC_HH;
      F__AOVIS.miGLng_DEG := miGLng_DEG;
      F__AOVIS.miGLng_MIN := miGLng_MIN;
      F__AOVIS.mdtWT := GetWTime();
      F__AOVIS.mbTimePlay := mbTimePlay;

      //if(LeftStr(F__AOVIS.mAObject.sAOType,1) = 'S') then
      //  F__AOVIS.WindowState:=wsMaximized;

      F__AOVIS.P__USERCOMMENT.Visible := true;
      F__AOVIS.ED__COMMENT.Text:=(molAOList[iIndex] as TAObject).sComment;
      sCommentPrev := F__AOVIS.ED__COMMENT.Text;
      F__AOVIS.P__MOONPOS.Visible := true;

      F__AOVIS.ShowModal;

      // Check for updating comment...
      if (sCommentPrev <> F__AOVIS.mAObject.sComment) then
      begin
        (molAOList[iIndex] as TAObject).sComment := F__AOVIS.mAObject.sComment;
        SaveAOUserFields('COMMENT',iIndex,F__AOVIS.mAObject.sComment);
      end;

      F__AOVIS.Destroy;
    end;

  end;
end;

procedure TF__ASTROTOOLBOX.AOVisOnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  iIndex: Integer;
begin
  iIndex := -1;

  if((Sender as TComponent).Name = 'IMG__POS_MOON') then
    iIndex := molAOList.Count-1
  else if(((Sender as TComponent).Name = 'SHP__POS_SUN') or ((Sender as TComponent).Name = 'SHP__SUN') or ((Sender as TComponent).Name = 'SHP__SUN_INCL')) then
    iIndex := molAOList.Count-2
  else
    iIndex := (Sender as TComponent).Tag;// (Sender as TShape).Tag;

  if(Button = mbLeft) then
    ShowAOVis(iIndex);

end;

procedure TF__ASTROTOOLBOX.ShowLatLong();
begin
  L__OC_CITY.Caption := msCity;
  L__OC_COUNTRY.Caption:='(' + msCountry + ')';

  L__OC_LAT.Caption := FloatToStrF(miGLat_DEG + miGLat_MIN/60.0,ffFixed,8,2) + '°';
  if(miGLng_DEG >= 0) then
  begin
    if(msLANG_ID = 'DE') then
      L__OC_LONG.Caption := FloatToStrF(miGLng_DEG + miGLng_MIN/60.0,ffFixed,8,2) + '° ö. L.';
    if(msLANG_ID = 'EN') then
      L__OC_LONG.Caption := FloatToStrF(miGLng_DEG + miGLng_MIN/60.0,ffFixed,8,2) + '° e. L.';
  end
  else
  begin
    if(msLANG_ID = 'DE') then
      L__OC_LONG.Caption := FloatToStrF(-(miGLng_DEG + miGLng_MIN/60.0),ffFixed,8,2) + '° w. L.';
    if(msLANG_ID = 'EN') then
      L__OC_LONG.Caption := FloatToStrF(-(miGLng_DEG + miGLng_MIN/60.0),ffFixed,8,2) + '° w. L.';
  end;

  P__FIRSTRUN.Visible := (miFirstStart = 1);
end;

procedure TF__ASTROTOOLBOX.EnableMenu(sMenuName: string);
begin
  Cursor := crDefault;
  //SPL__SEARCHRES.Visible := false;

  P__SEARCHALL.Visible:=false;

  L__ALBIREO.Font.Color := clSilver;
  L__HA.Font.Color := clSilver;
  L__TABLE.Font.Color := clSilver;
  L__MAP.Font.Color := clSilver;
  L__DEVICES.Font.Color := clSilver;
  L__NAVSOLSYS.Font.Color := clSilver;
  L__ASTROCALC.Font.Color := clSilver;

  if(sMenuName = 'MENU__ALBIREO') then
  begin
    L__ALBIREO.Font.Color := clRed;
  end
  else if(sMenuName = 'MENU__HA') then
  begin
    L__HA.Font.Color := clRed;
  end
  else if(sMenuName = 'MENU__TABLE') then
  begin
    L__TABLE.Font.Color := clRed;
  end
  else if(sMenuName = 'MENU__STARMAP') then
  begin
    L__MAP.Font.Color := clRed;
  end
  else if(sMenuName = 'MENU__DEVICES') then
  begin
    L__DEVICES.Font.Color := clRed;
  end
  else if(sMenuName = 'MENU__SOLSYS') then
  begin
    L__NAVSOLSYS.Font.Color := clRed;
  end
  else if(sMenuName = 'MENU__ASTROCALC') then
  begin
    L__ASTROCALC.Font.Color := clRed;
  end;

end;

(*
procedure TF__ASTROTOOLBOX.SelAObjects();
var
  i: Integer;
begin
  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_HA) then
  begin
    CB__AO.Clear;

    if(LeftStr(CB__SIGNS.Items[CB__SIGNS.ItemIndex],1) <> '_') then
      for i:=0 to molAOList.Count-1 do
        RegisterAObject(i);

  end;

end;
*)

procedure TF__ASTROTOOLBOX.ReCalcPlanetPos(dtDateTime: TDateTime);
{2012-11-1/fs
Calculates all planet and comot co-ordinates for a given date
sType: 'P': Planet, 'C': Comet
}
var
  i: Integer;
  iRA_HH: Word; iRA_MM: Word; rRA_SS: Real;
  iDEC_DEG: SmallInt; iDEC_MM: SmallInt; rDEC_SS: Real;
  Planet: TPlanet;
  Comet: TComet;
  rRs, rRho, rR: Real;
  bFound: Boolean;
begin
  bFound := false;
  if (molAOList = nil) then exit;

  iRA_HH:=0;iRA_MM:=0;rRA_SS:=0;
  iDEC_DEG:=0;iDEC_MM:=0;rDEC_SS:=0;
  rRs:=0; rR := 0;

  for i:=0 to molAOList.Count-1 do
  begin
    if((molAOList[i] as TAObject).sAOType = 'P') then
    begin
      bFound := true;
      Planet := (molAOList[i] as TPlanet);

      Planet.rDistFromEarth_AU:= GetPlanetCoo(dtDateTime,Planet,
        miDST_HH,miUTC_HH,
        iRA_HH,iRA_MM,rRA_SS,
        iDEC_DEG,iDEC_MM,rDEC_SS,rR);

      Planet.rVisuAngle_arcsec:=Get_arcsec(Planet.rDiameterRatio,Planet.rDistFromEarth_AU);
      Planet.rDistFromSun_AU:=rR;
    end;

    if((molAOList[i] as TAObject).sAOType = 'C') then
    begin
      bFound := true;
      Comet := (molAOList[i] as TComet);

      rRho := 0;

      GetCometCoo(dtDateTime,Comet,
        miDST_HH,miUTC_HH,
        iRA_HH,iRA_MM,rRA_SS,
        iDEC_DEG,iDEC_MM,rDEC_SS,rRs,rRho);

      (molAOList[i] as TComet).rRs := rRs;
      (molAOList[i] as TComet).rRho := rRho;
    end;

    if(bFound) then
    begin
      (molAOList[i] as TAObject).iRA_Hours:=iRA_HH;
      (molAOList[i] as TAObject).iRA_Min:=iRA_MM;
      (molAOList[i] as TAObject).rRA_Sec:=rRA_SS;
      (molAOList[i] as TAObject).iDec_Deg:=iDEC_DEG;
      (molAOList[i] as TAObject).iDec_Min:=iDEC_MM;
      (molAOList[i] as TAObject).rDec_Sec:=rDEC_SS;
      bFound := false;
    end;
  end;

  ReCalcPlanetPath(dtDateTime,miPlComPathWeeks);
end;

procedure TF__ASTROTOOLBOX.ReCalcPlanetPath(dtDateTime: TDateTime; iWeeks: Integer);
{2019-02-11/fs
Calculates all planet and comot co-ordinate paths for a given date
sType: 'P': Planet, 'C': Comet
}
var
  i,j: Integer;
  iRA_HH: Word; iRA_MM: Word; rRA_SS: Real;
  iDEC_DEG: SmallInt; iDEC_MM: SmallInt; rDEC_SS: Real;
  Planet: TPlanet;
  Comet: TComet;
  rRs, rRho, rR: Real;
  bFound: Boolean;
begin
  bFound := false;
  if (molAOList = nil) then exit;

  for i:=0 to molAOList.Count-1 do
  begin
    if((molAOList[i] as TAObject).sAOType = 'P') then
    begin
      bFound := true;
    end;

    if((molAOList[i] as TAObject).sAOType = 'C') then
    begin
      bFound := true;
    end;

    if(bFound) then
    begin

      for j:=0 to 2*iWeeks*7 - 1 do
      begin
        iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
        iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0;
        rRs:=0; rR:=0;

        if((molAOList[i] as TAObject).sAOType = 'P') then
        begin
          Planet := (molAOList[i] as TPlanet);

          GetPlanetCoo(dtDateTime + 1.0*(j-iWeeks*7),Planet,
            miDST_HH,miUTC_HH,
            iRA_HH,iRA_MM,rRA_SS,
            iDEC_DEG,iDEC_MM,rDEC_SS,rR);
        end
        else if((molAOList[i] as TAObject).sAOType = 'C') then
        begin
          Comet := (molAOList[i] as TComet);

          //if(AnsiContainsStr((molAOList[i] as TAObject).sLabel,'Wirtanen') and (j = 40)) then
          //  ShowMessage('Stop!');
          rRho := 0;

          GetCometCoo(dtDateTime + 1.0*(j-iWeeks*7),Comet,
            miDST_HH,miUTC_HH,
            iRA_HH,iRA_MM,rRA_SS,
            iDEC_DEG,iDEC_MM,rDEC_SS,rRs,rRho);

          (molAOList[i] as TComet).rRs := rRs;
          (molAOList[i] as TComet).rRho := rRho;
        end
        else
        begin
          //ShowMessage('Storage Inconsistency 1 for object ' + (molAOList[i] as TAObject).sLabel);
          continue;
        end;

        if((molAOList[i] as TAObject).sAOType = 'P') then
        begin
          (molAOList[i] as TPlanet).iPathRA_Hours[j]:=iRA_HH;
          (molAOList[i] as TPlanet).iPathRA_Min[j]:=iRA_MM;
          (molAOList[i] as TPlanet).rPathRA_Sec[j]:=rRA_SS;

          (molAOList[i] as TPlanet).iPathDec_Deg[j]:=iDEC_DEG;
          (molAOList[i] as TPlanet).iPathDec_Min[j]:=iDEC_MM;
          (molAOList[i] as TPlanet).rPathDec_Sec[j]:=rDEC_SS;
        end
        else if((molAOList[i] as TAObject).sAOType = 'C') then
        begin
          (molAOList[i] as TComet).iPathRA_Hours[j]:=iRA_HH;
          (molAOList[i] as TComet).iPathRA_Min[j]:=iRA_MM;
          (molAOList[i] as TComet).rPathRA_Sec[j]:=rRA_SS;

          (molAOList[i] as TComet).iPathDec_Deg[j]:=iDEC_DEG;
          (molAOList[i] as TComet).iPathDec_Min[j]:=iDEC_MM;
          (molAOList[i] as TComet).rPathDec_Sec[j]:=rDEC_SS;
          (*
          if(AnsiContainsStr((molAOList[i] as TAObject).sLabel,'C/2017 T2')) then
            MM__MW.Lines.Add('j: ' + IntToStr(j) +
              ', RA: ' + IntToStr(iRA_HH) + ':' + IntToStr(iRA_MM) + ':' + FloatToStrF(rRA_SS,ffFixed,8,1) +
              ', DC: ' + IntToStr(iDEC_DEG) + ':' + IntToStr(iDEC_MM) + ':' + FloatToStrF(rDEC_SS,ffFixed,8,1)
            );
          *)
        end
        else
        begin
          //ShowMessage('Storage Inconsistency 2 for object ' + (molAOList[i] as TAObject).sLabel);
          continue;
        end;

      end; // for j:=0 to 2*iWeeks

      bFound := false;
    end; // if bFound..
  end; // for i:=0 to molAOList.Count-1..
end;

function TF__ASTROTOOLBOX.GetAObjectIndexByName(sName,sAOType: string): Integer;
{2012-10-31/fs
Returns the object list index of the named and typed astronomical object; -1 if not found
}
var
  i: Integer;
  bFound: Boolean;
begin
  bFound := false;
  Result := -1;
  i:=0;

  while (i < molAOList.Count) and (not bFound) do
  begin
    if((molAOList[i] as TAObject).sAOType = sAOType) and (
      (((molAOList[i] as TAObject).sName_DE = sName) or (((molAOList[i] as TAObject).sName_EN = sName)))
    ) then
    begin
      bFound := true;
    end;

    Inc(i);
  end;

  if(bFound) then
    Result := i-1;

end;

function TF__ASTROTOOLBOX.GetStarIndex(sSym,sSign: string): Integer;
{2012-10-20/fs
Returns the object list index of the selected star; -1 if not found
}
var
  bFound: Boolean;
  i: Integer;
  bStarSequence: Boolean;
begin
  Result := -1;

  bFound := false;
  bStarSequence := false;

  i := 0;
  while (i < molAOList.Count) and (not bFound) do
  begin
    if(not bStarSequence) and ((molAOList[i] as TAObject).sAOType <> 'S') then
    begin
      Inc(i);
      continue; // Find begin of star sequece
    end;

    if((molAOList[i] as TAObject).sAOType = 'S') then
    begin
      bStarSequence := true;
      if(((molAOList[i] as TStar).sCon = sSign) and ((molAOList[i] as TStar).sSym = sSym))then
      begin
        bFound := true;
        Result := i;
      end;
    end
    else
      i := molAOList.Count; // break after last star!

    Inc(i);
  end;

end;

function TF__ASTROTOOLBOX.ColorizeSkyBG(Control: TControl): Integer;
var
  rHH: Real;
  //iR0, iX0, iY0: Integer;
begin
  Result := -999;

  rHH := (StrToInt(ED__WT_HH.Text));

  if((rHH > (mrRise_HH + 1)) and (rHH < (mrSet_HH - 1))) then
  begin
    Result := 2;
    Control.Color:=clDaySky;  // Daysky
  end
  else if (
    ((rHH > (mrRise_HH - 1)) and (rHH <= (mrRise_HH + 1))) or
    ((rHH >= (mrSet_HH - 1)) and (rHH < (mrSet_HH + 1)))
    ) then
  begin
    Result := 1;
    Control.Color:=clNavy;  // Twilight sky
  end
  else
  begin
    Result := 0;
    //ShowMessage('Sunrise: ' + FloatToStr(mrRise_HH));

    // Deepest night? Illuminate milkyway!
    // 1. Before Midnight (pm)
    if(mrSet_HH-miDST_HH < 21) and (rHH >= 12) and (rHH > (mrSet_HH + 2)) then
        Result := -1;

    // 2. After Midnight (am)
    if(mrRise_HH-miDST_HH > 4) and (rHH < 12) and (rHH < (mrRise_HH - 2)) then
    begin
        Result := -1;
    end;

    if(Result = -1) then
      Control.Color:=clDeepDarkSky  // Night sky
    else
      Control.Color:=clSummerDarkSky

  end;

  P__STARMAP.Canvas.Brush.Color:=Control.Color;
  P__STARMAP.Canvas.Pen.Color:=Control.Color;
  P__STARMAP.Canvas.FillRect(0,0,P__STARMAP.Width,P__STARMAP.Height);

  P__STARMAP.Refresh;
  P__HORDUST.Color := Control.Color;

end;

procedure TF__ASTROTOOLBOX.PrepMilkyway();
{22.05.2020/fs
Render the milkyway
BAsed on an image with DEC/RA-Co-ordinates
}
{

 begin
   pngbmp := TPortableNetworkGraphic.Create;
   MemoryStream := TMemoryStream.Create;

   SrcIntfImg:=TLazIntfImage.Create(0,0);
   SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
   TempIntfImg:=TLazIntfImage.Create(0,0);
   TempIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);

   for py:=0 to SrcIntfImg.Height-1 do begin
     for px:=0 to SrcIntfImg.Width-1 do begin
       CurColor:=SrcIntfImg.Colors[px,py];
       //CurColor.Red:=(CurColor.Red*iFadeLevel) shr 5;
       CurColor.Green:=(CurColor.Green*iFadeLevel) shr 5;
       CurColor.Blue:=(CurColor.Blue*iFadeLevel) shr 5;
       TempIntfImg.Colors[px,py]:=CurColor;
     end;
   end;

   TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
   pngbmp.BitmapHandle:=ImgHandle;
   pngbmp.MaskHandle:=ImgMaskHandle;
   pngbmp.SaveToStream(MemoryStream);
   MemoryStream.Position:=0;
   IMG.Picture.LoadFromStream(MemoryStream);

 SrcIntfImg.Free;
 TempIntfImg.Free;
 pngbmp.Free;
 MemoryStream.Free;
}
(*
var
  iX, iY: Integer;
  PixelColor: TColor;
  fRA, fDEC: Real;
  MilkyWayObject: TMilkyWayObject;
  bMWRecognized: Boolean;
  iBlue, iGreen, iRed, iLowCol: Word;
*)
var
   SrcIntfImg: TLazIntfImage;
   px, py: Integer;
   CurColor: TFPColor;
   fRA, fDEC: Real;
   MilkyWayObject: TMilkyWayObject;
   iCnt: Integer;
   iColor: Word;
begin
  iCnt := 0;
  molMWData.Clear;

  //IMG__MW.Picture.LoadFromFile('img/MW.png');

  SrcIntfImg:=nil;
  SrcIntfImg:=TLazIntfImage.Create(0,0);

  SrcIntfImg.LoadFromBitmap(IMG__MW.Picture.Bitmap.Handle,IMG__MW.Picture.Bitmap.MaskHandle);
  py:=0;
  while py < SrcIntfImg.Height-1 do begin
    px:=0;
    while px < SrcIntfImg.Width-1 do begin

      CurColor:=SrcIntfImg.Colors[px,py];
      iColor := (CurColor.Blue shr 8) div 3 + (CurColor.Green shr 8) div 3 + (CurColor.Red shr 8) div 3;

      //if(iColor < 200) // Elminiation background
      if(iColor < 210) // Elminiation background
        and (iColor > 10)  // Elmination stars
        then
      begin
        Inc(iCnt);
        fRA := 23.99 - (23.99*px)/(1.0*SrcIntfImg.Width);
        fDEC := 90.0 - (180.0 * py)/(1.0*SrcIntfImg.Height);

        MilkyWayObject := TMilkyWayObject.Create();
        MilkyWayObject.iRA_Hours:=Trunc(fRA);
        MilkyWayObject.iRA_Min:=Trunc((fRA - Trunc(fRA))*60);
        if(fDEC < 0) then
          MilkyWayObject.iDec_Deg:=-Trunc(abs(fDEC))
        else
          MilkyWayObject.iDec_Deg:=Trunc(fDEC);

        MilkyWayObject.iDec_Min:=Trunc((abs(fDEC) - Trunc(abs(fDEC)))*60);

        if(iColor < 130) then
          MilkyWayObject.rM := 5
        else if (iColor > 180) then
          MilkyWayObject.rM:=7
        else
          MilkyWayObject.rM:=6; // ... by PixelColor

        MilkyWayObject.iVisDim1:=7;
        MilkyWayObject.iVisDim2:=7;
        MilkyWayObject.sAOType:='MW';

        molMWData.Add(MilkyWayObject);

      end;

      px := px + 5;
    end;
    py := py + 5;
  end;

  SrcIntfImg.Free;

  (*
  for iY:=0 to IMG__MW.Picture.Height-1 do
  begin
    for iX:=0 to IMG__MW.Picture.Width-1 do
    begin
      PixelColor := IMG__MW.Picture.Bitmap.;

      iBlue := Blue(PixelColor);
      iGreen := Green(PixelColor);
      iRed := Red(PixelColor);

      if(iBlue > iLowCol) and (iGreen > iLowCol) and (iRed > iLowCol) then
      begin
        fRA := 23.99*iX/IMG__MW.Picture.Width;
        fDEC := 90.0 - 180 * iY/IMG__MW.Picture.Height;

        MM.Lines.Add(
          'iX: ' + IntToStr(iX) + ', iY: ' + IntToStr(iY) +
          '| iBlue: ' + IntToStr(iBlue) + ', iGreen: ' + IntToStr(iGreen) + ', iRed: ' + IntToStr(iRed)
          );
        MilkyWayObject := TMilkyWayObject.Create();
        MilkyWayObject.iRA_Hours:=Trunc(fRA);
        MilkyWayObject.iRA_Min:=Trunc((fRA - Trunc(fRA))*60);
        MilkyWayObject.iDec_Deg:=Trunc(fDEC);
        MilkyWayObject.iDec_Min:=Trunc((abs(fDEC) - Trunc(abs(fDEC)))*60);

        MilkyWayObject.rM:=6; // ... by PixelColor
        MilkyWayObject.Destroy;

        //molMWData.Add(MilkyWayObject);
      end;

    end;
  end;
  *)
end;

procedure TF__ASTROTOOLBOX.GenMilkyway(iSkyType: Integer);
var
  i, iLeft, iTop: Integer;
  iRad: Integer;
  dtST: TDateTime;
begin
  if(((iSkyType > 0) and (miShowMilkyway = 1)) or (miShowMilkyway = 0)) then // Do not show milkyway on day/twilight sky.
    exit;

  // Draw Milkyway
 dtST := GetSiderialTime();
 //Panel.Canvas.Ellipse(700,500,800,700);

 for i:=0 to molMWData.Count-1 do
 begin
   if((molMWData[i] as TAObject).sAOType = 'MW') then
   begin
     iRad := (molMWData[i] as TMilkywayObject).iVisDim1;
     //iRad := 2;

     iLeft:=0; iTop:=0;

     if(GetMapCooFromAO(molMWData, i,dtST,iLeft,iTop)) then
     begin
       if((molMWData[i] as TMilkywayObject).rM = 5) then
       begin
         if(iSkyType < 0) then
         begin
           P__STARMAP.Canvas.Pen.Color := TColor($00333333);
           P__STARMAP.Canvas.Brush.Color := TColor($00333333);
         end
         else if(miShowMilkyway = -1) then
         begin
           case iSkyType of
             2: // Day
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($CC5555);
               P__STARMAP.Canvas.Brush.Color := TColor($CC5555);
             end;
             1: // Twilight
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($BB4444);
               P__STARMAP.Canvas.Brush.Color := TColor($BB4444);
             end;
             0: // Night brighter
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($BB4444);
               P__STARMAP.Canvas.Brush.Color := TColor($BB4444);
             end;
           end; // end case..
         end;
       end
       else if((molMWData[i] as TMilkywayObject).rM = 6) then
       begin
         if(miShowMilkyWay = -1) then
         begin
           case iSkyType of
             2: // Day
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($BB4444);
               P__STARMAP.Canvas.Brush.Color := TColor($BB4444);
             end;
             1: // Twilight
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($AA3333);
               P__STARMAP.Canvas.Brush.Color := TColor($AA3333);
             end
             else
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($9922222);
               P__STARMAP.Canvas.Brush.Color := TColor($992222);
             end;
           end; // end case..
         end
         else
         begin
           P__STARMAP.Canvas.Pen.Color := TColor($00222222);
           P__STARMAP.Canvas.Brush.Color := TColor($00222222);
         end;
       end
       else if((molMWData[i] as TMilkywayObject).rM = 7) then
       begin
         if(iSkyType < 0) then
         begin
           P__STARMAP.Canvas.Pen.Color := TColor($00111111);
           P__STARMAP.Canvas.Brush.Color := TColor($00111111);
         end
         else if(miShowMilkyWay = -1) then
         begin
           case iSkyType of
             2: // Day
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($992222);
               P__STARMAP.Canvas.Brush.Color := TColor($992222);
             end;
             1: // Twilight
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($881111);
               P__STARMAP.Canvas.Brush.Color := TColor($881111);
             end
             else
             begin
               P__STARMAP.Canvas.Pen.Color := TColor($770000);
               P__STARMAP.Canvas.Brush.Color := TColor($770000);
             end;
           end; // end case..
         end;
       end;
       P__STARMAP.Canvas.Rectangle(iLeft-iRad,iTop-iRad,iLeft+iRad,iTop+iRad);
       //Ellipse(iLeft-iRad,iTop-iRad,iLeft+iRad,iTop+iRad);
     end;
   end;
 end;

end;

procedure TF__ASTROTOOLBOX.GenMap(CustomControl: TCustomControl);
begin
  {$IFDEF Darwin}
  CustomControl.Repaint;
  {$ENDIF Darwin}
  {$IFDEF Windows}
  GenMapExec(CustomControl);
  {$ENDIF Windows}
end;

procedure TF__ASTROTOOLBOX.ShowMeridian(iX0, iY0: Integer);
begin
  if (not mbZoomMode) then
  begin
    P__STARMAP.Canvas.Font.Color := clRed;
    P__STARMAP.Canvas.Font.Size := 14;

    if (miStarmapView = 0) then
    begin
      if(mbSM_MERI) then
      begin
        P__STARMAP.Canvas.MoveTo(iX0,0);
        P__STARMAP.Canvas.Pen.Color:=clGreen;
        P__STARMAP.Canvas.LineTo(iX0,iY0);
        P__STARMAP.Canvas.Pen.Color:=clLime;
        P__STARMAP.Canvas.LineTo(iX0,2*iY0);

        P__STARMAP.Canvas.Font.Color := clGreen;
        P__STARMAP.Canvas.Font.Size := 10;
        P__STARMAP.Canvas.TextOut(iX0+5,iY0 div 2,'Meridian-N');
        P__STARMAP.Canvas.Font.Color := clLime;
        P__STARMAP.Canvas.TextOut(iX0+5,iY0 + iY0 div 2,'Meridian-S');

        P__STARMAP.Canvas.Font.Color := clRed;
        P__STARMAP.Canvas.Font.Size := 14;
      end;

      P__STARMAP.Canvas.Pen.Color := clRed;
      P__STARMAP.Canvas.TextOut(iX0,5,'N');
      P__STARMAP.Canvas.TextOut(iX0,2*iY0-35,'S');
    end;

    if (miStarmapView > 0) then
    begin
      if(miTimePlayMode = 0) then
      begin
        P__STARMAP.Canvas.Pen.Color := clRed;
        P__STARMAP.Canvas.Font.Color := clRed;
        P__STARMAP.Canvas.TextOut(iX0,2*iY0-25,msHorDir);
      end;
      if(msHorDir = 'S') then
      begin
        if(mbSM_MERI) then
        begin
          // South-Meridian...
          P__STARMAP.Canvas.Pen.Color:=clLime;
          P__STARMAP.Canvas.MoveTo(iX0,0);
          P__STARMAP.Canvas.LineTo(iX0,2*iY0);
          P__STARMAP.Canvas.Font.Color := clLime;
          P__STARMAP.Canvas.Font.Size := 10;
          P__STARMAP.Canvas.TextOut(iX0+5,iY0,'Meridian-S');

          P__STARMAP.Canvas.Font.Color := clRed;
          P__STARMAP.Canvas.Font.Size := 14;
        end;
        if(miTimePlayMode = 0) then
        begin
          P__STARMAP.Canvas.Pen.Color:=clRed;
          P__STARMAP.Canvas.Font.Color := clRed;
          P__STARMAP.Canvas.TextOut(25,2*iY0-25,'O');
          P__STARMAP.Canvas.TextOut(2*iX0 - 25,2*iY0-35,'W');
        end;
      end;
      if(msHorDir = 'N') then
      begin
        if(mbSM_MERI) then
        begin
          // North-Meridian...
          P__STARMAP.Canvas.Pen.Color:=clGreen;
          P__STARMAP.Canvas.MoveTo(iX0,0);
          P__STARMAP.Canvas.LineTo(iX0,2*iY0);
          P__STARMAP.Canvas.Pen.Color:=clRed;
          P__STARMAP.Canvas.Font.Color := clGreen;
          P__STARMAP.Canvas.Font.Size := 10;
          P__STARMAP.Canvas.TextOut(iX0+5,iY0,'Meridian-N');

          P__STARMAP.Canvas.Font.Color := clRed;
          P__STARMAP.Canvas.Font.Size := 14;
        end;

        if(miTimePlayMode = 0) then
        begin
          P__STARMAP.Canvas.Pen.Color:=clRed;
          P__STARMAP.Canvas.Font.Color := clRed;
          P__STARMAP.Canvas.TextOut(25,2*iY0-25,'W');
          P__STARMAP.Canvas.TextOut(2*iX0 - 25,2*iY0-35,'O');
        end;
      end;
      if(msHorDir = 'O') then
      begin
        if(miTimePlayMode = 0) then
        begin
          P__STARMAP.Canvas.Pen.Color:=clRed;
          P__STARMAP.Canvas.Font.Color := clRed;
          P__STARMAP.Canvas.TextOut(25,2*iY0-25,'N');
          P__STARMAP.Canvas.TextOut(2*iX0 - 25,2*iY0-35,'S');
        end;
      end;


      if(msHorDir = 'W') then
      begin
        if(miTimePlayMode = 0) then
        begin
          P__STARMAP.Canvas.Pen.Color:=clRed;
          P__STARMAP.Canvas.Font.Color := clRed;
          P__STARMAP.Canvas.TextOut(25,2*iY0-25,'S');
          P__STARMAP.Canvas.TextOut(2*iX0 - 25,2*iY0-35,'N');
        end;
      end;
    end;
  end;
end;

procedure TF__ASTROTOOLBOX.GenMapExec(CustomControl: TCustomControl);
var
  iR0, iX0, iY0: Integer;
begin
  P__LANDSCAPE.Visible := (miStarmapView > 0) and (not mbZoomMode);
  P__LANDSCAPE.Refresh;

  if(miStarmapView > 0) and (not mbZoomMode) then
    CustomControl.Height := TS__MAP.Height - P__LANDSCAPE.Height + 3 // 3 Px overlap! Horizontal panel break line hidden...
  else
    CustomControl.Height := TS__MAP.Height;

  StarMapDims(CustomControl,iR0,iX0,iY0);

  try
    IMG__POS_MOON.Visible:=false;
    DisplayMoonPhase(GetWTime(),IMG__POS_MOON);

    Screen.Cursor:=crHourGlass;

    miSkyType := ColorizeSkyBG(CustomControl);

    GenSigns(P__STARMAP,iR0,iX0,iY0,miSkyType);

    GenCB(P__STARMAP,iR0,iX0,iY0);

    if (not mbZoomMode) then
    begin
      if(miTimePlayMode <> 0) and (miStarmapView > 0) then
      begin
        P__HORDUST.Visible := true;
        ColorizeSky(P__HORDUST,miSkyType,iR0,iX0,iY0);
      end
      else if(miTimePlayMode = 0) then
      begin
        P__HORDUST.Visible := false;
        ColorizeSky(P__STARMAP,miSkyType,iR0,iX0,iY0);
      end;
    end;

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'POLP_N');
    GenEquPath(P__STARMAP,iR0,iX0,iY0,'POLP_S');

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'ECLIPTIC');

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'EQUATOR');

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'GALACTIC');

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'RA');

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'DEC');

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'HOR');

    GenEquPath(P__STARMAP,iR0,iX0,iY0,'HGT');

    ShowMeridian(iX0, iY0);

    GenStarMap(CustomControl);

  finally
    Screen.Cursor:=crDefault;

  end;

end;

function TF__ASTROTOOLBOX.NormalizeSearchArg(sText: string): string;
begin
  Result := Uppercase(sText);
  Result := AnsiReplaceStr(Result,'/','');
  Result := AnsiReplaceStr(Result,' ','');
  Result := AnsiReplaceStr(Result,'-','');
end;

function TF__ASTROTOOLBOX.HighLightSearch(AObject: TAObject): Boolean;
var
  sLabel: string;
begin
  Result := false;

  if not ((mbSearch) and (msSearch <> '')) then
    exit;

  sLabel := NormalizeSearchArg(GetAOLabel(AObject,msLANG_ID));

  if(CBX__EXACTMATCH.Checked) then
  begin

    //if(AnsicontainsStr(NormalizeSearchArg(AObject.sName_DE),'CAPH')) then
    //  ShowMessage('Stop!');

    Result :=
      (sLabel = msSearch) or
      (NormalizeSearchArg(AObject.sName_DE) = msSearch) or
      (NormalizeSearchArg(AObject.sName_EN) = msSearch);

    if(not Result) then
    begin
      if(AObject.sAOType = 'S') then
      begin
        Result :=
          (NormalizeSearchArg((AObject as TStar).sSym) = msSearch) or
          (NormalizeSearchArg((AObject as TStar).sCatNo) = msSearch) or
          (NormalizeSearchArg((AObject as TStar).sCon) = msSearch);
      end;

      if(AObject.sAOType = 'G') or
        (AObject.sAOType = 'GC') or
        (AObject.sAOType = 'OC') or
        (AObject.sAOType = 'N') or
        (AObject.sAOType = 'PN') then
      begin
        Result :=
          (NormalizeSearchArg((AObject as TInterstellarObject).sMessier) = msSearch) or
          (NormalizeSearchArg((AObject as TInterstellarObject).sNGC) = msSearch) or
          (NormalizeSearchArg((AObject as TInterstellarObject).sCatNo) = msSearch) or
          (NormalizeSearchArg((AObject as TInterstellarObject).sCon) = msSearch);
      end;
    end;
  end
  else
  begin
    Result :=
      (AnsiContainsStr(sLabel,msSearch)) or
      (AnsiContainsStr(NormalizeSearchArg(AObject.sName_DE),msSearch)) or
      (AnsiContainsStr(NormalizeSearchArg(AObject.sName_EN),msSearch));

    if(not Result) then
    begin
      if(AObject.sAOType = 'S') then
      begin
        Result :=
          (AnsiContainsStr(NormalizeSearchArg((AObject as TStar).sSym),msSearch)) or
          (AnsiContainsStr(NormalizeSearchArg((AObject as TStar).sCatNo),msSearch)) or
          (AnsiContainsStr(NormalizeSearchArg((AObject as TStar).sCon),msSearch));
      end;

      if(AObject.sAOType = 'G') or
        (AObject.sAOType = 'GC') or
        (AObject.sAOType = 'OC') or
        (AObject.sAOType = 'N') or
        (AObject.sAOType = 'PN') then
      begin
        Result :=
          (AnsiContainsStr(NormalizeSearchArg((AObject as TInterstellarObject).sMessier),msSearch)) or
          (AnsiContainsStr(NormalizeSearchArg((AObject as TInterstellarObject).sNGC),msSearch)) or
          (AnsiContainsStr(NormalizeSearchArg((AObject as TInterstellarObject).sCatNo),msSearch)) or
          (AnsiContainsStr(NormalizeSearchArg((AObject as TInterstellarObject).sCon),msSearch));
      end;
    end;
  end;

  if(Result) then
  begin
    mbSearchFound := true;
    //if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!
  end;

end;

function TF__ASTROTOOLBOX.GenAOSearch(i: Integer): TAOSearch;
begin
  Result := TAOSearch.Create;
  Result.sLabel:=GetAOLabel((molAOList[i] as TAObject),msLANG_ID);
  Result.iAOIndex:=i;
end;

function TF__ASTROTOOLBOX.GetMeteorShower(sASName_DE: string): TMeteorShower;
var
  i: Integer;
  bFound: Boolean;
begin
  Result := nil;

  bFound := false;
  i:=0;
  while ((not bFound) and (i < molMeteorShowers.Count)) do
  begin
    bFound := ((molMeteorShowers[i] as TMeteorShower).sName_DE = sASName_DE) and ((molMeteorShowers[i] as TMeteorShower).bActive);
    Inc(i);
  end;

  if(bFound) then
    Result := (molMeteorShowers[i-1] as TMeteorShower);

end;

function TF__ASTROTOOLBOX.GenMapPos(iR0, iX0, iY0: Integer; rAz, rHgt: Real; var iLeft: Integer; var iTop: Integer): Boolean;
var
  rR, rDX, rDY: Real;
  rZoomAzMin, rZoomAzMax: Real;
begin
  Result := false;
  //rEyeDiffA := 214.0/180.0 // < 1

  // Internal Yaw values. Different from displayed (+ Pi)
  // N: Az = Pi (180)
  // O: Az = 3Pi/2  (270)
  // S: Az = 2pi bzw. 0  (0 bzw. 360)
  // W: Az = P/2 (90)

  if(miStarmapView > 0) and (not mbZoomMode) then
    rHgt := rHgt / mrEyeFacH;

  if(miStarmapView > 0) then
  begin
    // 1. Filter
    if (msHorDir = 'S') and (rAz < 270) and (rAz > 90) then
      exit;
    if (msHorDir = 'N') and ((rAz > 270) or (rAz < 90)) then
      exit;
    if (msHorDir = 'O') and (rAz > 0) and (rAz < 180) then
      exit;
    if (msHorDir = 'W') and (rAz > 180) and (rAz < 360) then
      exit;
  end;

  Result := true;

  rHgt := crPIDIV180*rHgt; // Pi/180...
  rAz := crPIDIV180*rAz;

  if(miStarmapView > 0) then
  begin
    if(mbZoomMode) then
    begin
      // Transform arguments
      rHgt := crPIHALF*(rHgt - mrZoomHgtMin *crPIDIV180)/(mrZoomHgtMax *crPIDIV180 - mrZoomHgtMin *crPIDIV180);

      if(msHorDir = 'S') then
      begin
        if(mrZoomAzMax >= 270) and (mrZoomAzMin <= 90) then
        begin
          rZoomAzMin := mrZoomAzMax - 360.0; // negative
          rZoomAzMax := mrZoomAzMin;
          if(rAz > crPITHREEHALF) then
            rAz := rAz - 2*Pi; // negative.
        end
        else
        begin
          rZoomAzMin := mrZoomAzMin;
          rZoomAzMax := mrZoomAzMax;
        end;

        rAz := Pi*(rAz - rZoomAzMin *crPIDIV180)/(rZoomAzMax *crPIDIV180 - rZoomAzMin *crPIDIV180) - crPIHALF;

        if(rAz < 0) then
          rAz := rAz + 2*Pi;

        (*  BUGGY!
        if(mrZoomAzMax >= 270) and (mrZoomAzMin <= 90) then
        begin
        if(rAz >= crPITHREEHALF) then
          rAz := crPITHREEHALF + crPIHALF*(rAz - mrZoomAzMax *crPIDIV180)/(360 *crPIDIV180 - mrZoomAzMax *crPIDIV180)
        else
          rAz := crPIHALF*(rAz)/(mrZoomAzMin *crPIDIV180);

        end
        else
        begin
          rAz := crPITHREEHALF + Pi*(rAz - mrZoomAzMin *crPIDIV180)/(mrZoomAzMax *crPIDIV180 - mrZoomAzMin *crPIDIV180);
        end;
        *)

      end
      else if(msHorDir = 'N') then
        rAz := crPIHALF + Pi*(rAz - mrZoomAzMin *crPIDIV180)/(mrZoomAzMax *crPIDIV180 - mrZoomAzMin *crPIDIV180)
      else if(msHorDir = 'O') then
        rAz := Pi + Pi*(rAz - mrZoomAzMin *crPIDIV180)/(mrZoomAzMax *crPIDIV180 - mrZoomAzMin *crPIDIV180)
      else if(msHorDir = 'W') then
        rAz := Pi*(rAz - mrZoomAzMin *crPIDIV180)/(mrZoomAzMax *crPIDIV180 - mrZoomAzMin *crPIDIV180);

      //rDY := P__STARMAP.Height * (1-(rHgt)/(crPIHALF));
    end;
    //else
    //  rDY := P__STARMAP.Height * (1-sin(rHgt));

    rDY := P__STARMAP.Height * (1-(rHgt)/(crPIHALF));
    //rDY := P__STARMAP.Height * (1-sin(rHgt));
    rDX := 0;

    if(msHorDir = 'S') then
    begin
      if(rAz >= crPITHREEHALF) then
        //rDX := (P__STARMAP.Width div 2)*(rAz-crPITHREEHALF)/(2*Pi-crPITHREEHALF)
        rDX := (P__STARMAP.Width shr 1)*(rAz-crPITHREEHALF)/(2*Pi-crPITHREEHALF)
      else
        //rDX := (P__STARMAP.Width div 2) + (P__STARMAP.Width div 2)*(rAz)/(crPIHALF);
        rDX := (P__STARMAP.Width shr 1) + (P__STARMAP.Width shr 1)*(rAz)/(crPIHALF);
    end;

    if(msHorDir = 'N') then
      rDX := (P__STARMAP.Width)*(rAz-crPIHALF)/Pi;

    if(msHorDir = 'O') then
      rDX := (P__STARMAP.Width)*(rAz-Pi)/Pi;

    if(msHorDir = 'W') then
      rDX := (P__STARMAP.Width)*rAz/Pi;

    iTop := Round(rDY);
    iLeft := Round(rDX);

  end
  else
  begin
    if(rHgt < 0) then
      rR := iR0
    else
      rR := iR0*(1 - 0.63662*rHgt);

    rDX := rR*sin(rAz);
    rDY := rR*cos(rAz);
    if(mbZoomMode) then
      ZoomModeTransformation(iR0,rDX,rDY);

    iTop := Round(iY0 + rDY);
    iLeft := Round(iX0 + rDX);
  end;

end;


procedure TF__ASTROTOOLBOX.GenAOLabel(AObject: TAObject; WinControl: TWinControl);
begin
  if(AObject.sAOType = 'S') then
    exit; // No default label for each star to save memory!

  if(AObject.sAOType = 'E') or
    (AObject.sAOType = 'SUN') or
    (AObject.sAOType = 'P') or
    (AObject.sAOType = 'C') then
    exit;

  if(
    ((AObject as TInterstellarObject).sMessier <> '') or
      (
        mbZoomMode and
       (
        AnsiContainsStr( (AObject as TInterstellarObject).sNGC,'NGC') or
        AnsiContainsStr( (AObject as TInterstellarObject).sNGC,'IC') or
        AnsiContainsStr( (AObject as TInterstellarObject).sNGC,'Cr')
        )
        and (AObject.rM < 7)
      )
      )
  then
  begin
    if(AObject.L__AO = nil) then
      AObject.L__AO := TLabel.Create(nil);

    AObject.L__AO.Left:= AObject.SHP.Left+2;
    AObject.L__AO.Top:= AObject.SHP.Top+2;

    if((AObject as TInterstellarObject).sMessier <> '') then
    begin
      AObject.L__AO.Font.Color := clYellow;
      AObject.L__AO.Caption := (AObject as TInterstellarObject).sMessier
    end
    else
    begin
     AObject.L__AO.Font.Color := clWhite;
     AObject.L__AO.Caption := GetAOLabel(AObject,msLANG_ID);
    end;

    AObject.L__AO.Visible := true;
    AObject.L__AO.Parent := WinControl;
  end;

end;

procedure TF__ASTROTOOLBOX.PlotSolSysBodyPath(iR0, iX0, iY0: Integer; dtST: TDateTime;
  iIndex: Integer); //SkyBitMap: Graphics.TBitmap);
var
  //iPathRA_HH, iPathRA_MM, iPathRA_SS: Word;
  iPathDEC_DEG: SmallInt;
  //iPathDEC_MM, iPathDEC_SS: SmallInt;
  iPathHA_HH,iPathHA_MM,iPathHA_SS,iPathHA_MS: Word;
  dtPathRA, dtPathHA: TDateTime;
  rPathAz,rPathHgt: Real;
  iPathLeft, iPathTop: Integer;
  iPathRA_Hours, iPathRA_Min: Integer;
  rPathRA_Sec: Real;
  iPathDec_Min: Integer;
  rPathDec_Sec: Real;
  j: Integer;
  bShowPath: Boolean;
begin
  bShowPath := false;

  if not (
    ((molAOList[iIndex] as TAObject).sAOType = 'C') or
    ((molAOList[iIndex] as TAObject).sAOType = 'P')
    ) then
  exit;

  if((molAOList[iIndex] as TAObject).sAOType = 'C') then
  begin
    bShowPath := (molAOList[iIndex] as TComet).bShowPath;
  end;

  if((molAOList[iIndex] as TAObject).sAOType = 'P') then
  begin
    bShowPath := (molAOList[iIndex] as TPlanet).bShowPath;
  end;

  if(bShowPath) then
  begin
    for j:= 0 to 2*miPlComPathWeeks*7 - 1 do
    begin
      if((molAOList[iIndex] as TAObject).sAOType = 'C') then
      begin
        iPathRA_Hours := (molAOList[iIndex] as TComet).iPathRA_Hours[j];
        iPathRA_Min := (molAOList[iIndex] as TComet).iPathRA_Min[j];
        rPathRA_Sec := (molAOList[iIndex] as TComet).rPathRA_Sec[j];

        iPathDec_Deg := (molAOList[iIndex] as TComet).iPathDec_Deg[j];
        iPathDec_Min := (molAOList[iIndex] as TComet).iPathDec_Min[j];
        rPathDec_Sec := (molAOList[iIndex] as TComet).rPathDec_Sec[j];

      end
      else if((molAOList[iIndex] as TAObject).sAOType = 'P') then
      begin
        iPathRA_Hours := (molAOList[iIndex] as TPlanet).iPathRA_Hours[j];
        iPathRA_Min := (molAOList[iIndex] as TPlanet).iPathRA_Min[j];
        rPathRA_Sec := (molAOList[iIndex] as TPlanet).rPathRA_Sec[j];

        iPathDec_Deg := (molAOList[iIndex] as TPlanet).iPathDec_Deg[j];
        iPathDec_Min := (molAOList[iIndex] as TPlanet).iPathDec_Min[j];
        rPathDec_Sec := (molAOList[iIndex] as TPlanet).rPathDec_Sec[j];
      end
      else
        continue;

      if(j <= miPlComPathWeeks*7) then
        P__STARMAP.Canvas.Pen.Color := clGreen
      else
        P__STARMAP.Canvas.Pen.Color := clLime;

      dtPathRA := EncodeTime(
        iPathRA_Hours,
        iPathRA_Min,
        Trunc(rPathRA_Sec), Trunc(1000*(rPathRA_Sec - Trunc(rPathRA_Sec))));

      dtPathHA := GetHA(dtST,dtPathRA);

      DeCodeTime(dtPAthHA,iPathHA_HH, iPathHA_MM, iPathHA_SS, iPathHA_MS);

      rPathAz:=0;rPathHgt:=0;

      CalcAZ(
        iPathDec_Deg,
        iPathDec_Min,
        Trunc(rPathDec_Sec),
        iPathHA_HH,iPathHA_MM,iPathHA_SS,
        rPathAz,rPathHgt);

      iPathLeft:=0;iPathTop:=0;

      GenMapPos(iR0, iX0, iY0, rPathAz,rPathHgt,
      iPathLeft,iPathTop);

      if(j=0) then
        P__STARMAP.Canvas.MoveTo(iPathLeft,iPathTop)
      else
        P__STARMAP.Canvas.LineTo(iPathLeft,iPathTop);

      if((j mod 7) = 0) then
        P__STARMAP.Canvas.EllipseC(iPathLeft,iPathTop,3,3)
      else
        P__STARMAP.Canvas.Rectangle(iPathLeft,iPathTop,iPathLeft+3,iPathTop+3);

    end;
  end;

end;

procedure TF__ASTROTOOLBOX.RegisterAsVisibleAO(i: Integer);
begin
  mslVisibleAOList.Add(IntToStr(i));
end;

function TF__ASTROTOOLBOX.GetVisualObjectHeight(rAngle_Minutes: Real; iVisuFac: Integer): Integer;
begin
  Result := iVisuFac; //iVisuFac: Illustration factor, non-physical

  if(mbZoomMode) and ((mrZoomHgtMax - mrZoomHgtMin) > 0) then
  begin
    if(miStarMapView = 0) then
      Result := Round(rAngle_Minutes/60.0 * Pi/180.0 * P__STARMAP.Height / ((mrZoomHgtMax - mrZoomHgtMin)*Pi/180.0 ))
    else
      Result := Round(rAngle_Minutes/60.0 * Pi/180.0 * (P__STARMAP.Height - P__LANDSCAPE.Height)/ ((mrZoomHgtMax - mrZoomHgtMin)*Pi/180.0));

    //ShowMessage('Internal: Angle: '+ FloatToStr(rAngle_Minutes*60.0) + ', ' + FloatToStr(rAngle_Minutes/60.0 * Pi/180.0 * P__STARMAP.Height / ((mrZoomHgtMax - mrZoomHgtMin)*Pi/180.0 )));

  end
  else
  begin
    if(miStarMapView = 0) then
      Result := Round(rAngle_Minutes/60.0 * Pi/180.0 * P__STARMAP.Height / Pi)// / 180.0)
    else
    begin
      if(mrEyeFacH = crEyeFacH_70) then
        Result := Round(rAngle_Minutes/60.0 * Pi/180.0 * (P__STARMAP.Height - P__LANDSCAPE.Height)/ (70.0*Pi/180.0))
      else
        Result := Round(rAngle_Minutes/60.0 * Pi/180.0 * (P__STARMAP.Height - P__LANDSCAPE.Height)/ (Pi/2.0));

    end;

  end;

  if(Result < iVisuFac) then
    Result := iVisuFac; // Return always a value > 0 - but plat smaller icons!

end;

procedure TF__ASTROTOOLBOX.GenStarMap(WinControl: TWinControl); //; SkyBitmap: Graphics.TBitmap);
{2012-10/fs
Generates the star map incl. planets
}
var
  i: Integer;
  iRA_HH, iRA_MM, iRA_SS: Word;
  iDEC_DEG: SmallInt;
  iDEC_MM, iDEC_SS: SmallInt;
  iHA_HH,iHA_MM,iHA_SS,iHA_MS: Word;
  rAz,rHgt: Real;
  dJulDat, dtWT, dtST, dtRA, dtHA: TDateTime;
  iX0, iY0, iR0: Integer;
  rRA_SS: Real;
  rDEC_SS: Real;
  rLambdaSun, rMSun: Real;
  AOSearch: TAOSearch;
  iBrightStarLabel_M: Integer;
  MeteorShower: TMeteorShower;
  bCanVisu: Boolean;
  bObjectFound, bObjectFoundAndDisplayed: Boolean;
  iLeft, iTop: Integer;
  sSignName: string;
  sLabel: string;
begin
  bObjectFound := false;
  bObjectFoundAndDisplayed := false;
  mslVisibleAOList.Clear;

  WinControl.DisableAlign;

  SHP__POS_SUN.Visible:=false;
  IMG__POS_MOON.Visible:=false;

  rLambdaSun := 0;
  rMSun := 0;

  iR0:=0;iX0:=0;iY0:=0;
  StarMapDims(WinControl,iR0,iX0,iY0);

  rAz:=0; rHgt:=0;

  iRA_HH:=0; iRA_MM:=0; iRA_SS:=0; rRA_SS:=0;
  iDEC_DEG:=0; iDEC_MM:=0; iDEC_SS:=0; rDEC_SS:=0;
  iHA_HH:=0;iHA_MM:=0;iHA_SS:=0;iHA_MS:=0;

  dJulDat := 0;

  dtWT := GetWTime();
  dtST := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat);

  if(mbShowSun) or (mbShowMoon) then
  begin
    GetSunCoo(dtWT,
      miDST_HH,miUTC_HH, rLambdaSun, rMSun,
      iRA_HH, iRA_MM, rRA_SS,
      iDEC_DEG, iDEC_MM, rDEC_SS);
  end;

  if(mbShowSun) then
  begin
    iRA_SS := Trunc(rRA_SS);
    iDEC_SS := Trunc(rDEC_SS);

    //ShowMessage((molAOList[molAOList.Count-2] as TAObject).sAOType);

    (molAOList[molAOList.Count-2] as TAObject).iDec_Deg  := iDEC_DEG;
    (molAOList[molAOList.Count-2] as TAObject).iDec_Min  := iDEC_MM;
    (molAOList[molAOList.Count-2] as TAObject).rDec_Sec  := rDEC_SS;
    (molAOList[molAOList.Count-2] as TAObject).iRA_Hours := iRA_HH;
    (molAOList[molAOList.Count-2] as TAObject).iRA_Min   := iRA_MM;
    (molAOList[molAOList.Count-2] as TAObject).rRA_Sec   := rRA_SS;

    dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS, Trunc(1000*(rRA_SS-iRA_SS)));
    dtHA := GetHA(dtST,dtRA);

    DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);
    CalcAZ(iDEC_DEG,iDEC_MM,iDEC_SS,iHA_HH,iHA_MM,iHA_SS,rAz,rHgt);

    rHgt := Pi*rHgt/180;
    iLeft := 0; iTop := 0;
    if(CanPlotAO(rAz,rHgt *180/Pi)) then
    begin
      GenMapPos(iR0, iX0, iY0, rAz,rHgt*180/Pi,iLeft,iTop);
      if(iLeft > 0) then
      begin
        SHP__POS_SUN.Height := GetVisualObjectHeight(32,15);
        SHP__POS_SUN.Width:=SHP__POS_SUN.Height;

        SHP__POS_SUN.Top := Round(iTop) - SHP__POS_SUN.Height div 2;
        SHP__POS_SUN.Left := Round(iLeft) - SHP__POS_SUN.Width div 2;

        // Colorization effect
        if(rHgt*180/Pi < 5) then
        begin
          SHP__POS_SUN.Brush.Color:=clRed;
          SHP__POS_SUN.Pen.Color:=clRed;
        end
        else if(rHgt*180/Pi < 10) then
        begin
          SHP__POS_SUN.Brush.Color:=$000080FF;
          SHP__POS_SUN.Pen.Color:=$000080FF;
        end
        else if(rHgt*180/Pi < 15) then
        begin
          SHP__POS_SUN.Brush.Color:=$0049D6FE;
          SHP__POS_SUN.Pen.Color:=$0049D6FE;
        end
        else
        begin
          SHP__POS_SUN.Brush.Color:=clYellow;
          SHP__POS_SUN.Pen.Color:=clYellow;
        end;

        if(msLANG_ID = 'DE') then
          SHP__POS_SUN.Hint := 'Sonne';

        if(msLANG_ID = 'EN') then
          SHP__POS_SUN.Hint := 'Sun';

        SHP__POS_SUN.ShowHint := true;
        SHP__POS_SUN.Parent := WinControl;
        SHP__POS_SUN.Visible:=true;
      end;
    end;
  end
  else
    SHP__POS_SUN.Visible:=false;

  if(mbShowMoon) then
  begin
    GetMoonCoo(dtWT,
      miDST_HH,miUTC_HH,rLambdaSun,rMSun,
      iRA_HH, iRA_MM, rRA_SS,
      iDEC_DEG, iDEC_MM, rDEC_SS);

    iRA_SS := Trunc(rRA_SS); //Round(rRA_SS);
    iDEC_SS := Trunc(rDEC_SS); //Round(rDEC_SS);

    (molAOList[molAOList.Count-1] as TAObject).iDec_Deg  := iDEC_DEG;
    (molAOList[molAOList.Count-1] as TAObject).iDec_Min  := iDEC_MM;
    (molAOList[molAOList.Count-1] as TAObject).rDec_Sec  := Round(rDEC_SS);
    (molAOList[molAOList.Count-1] as TAObject).iRA_Hours := iRA_HH;
    (molAOList[molAOList.Count-1] as TAObject).iRA_Min   := iRA_MM;
    (molAOList[molAOList.Count-1] as TAObject).rRA_Sec   := Round(rRA_SS);

    dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS, Trunc(1000*(rRA_SS-iRA_SS)));
    dtHA := GetHA(dtST,dtRA);

    DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);
    CalcAZ(iDEC_DEG,iDEC_MM,iDEC_SS,iHA_HH,iHA_MM,iHA_SS,rAz,rHgt);

    iLeft := 0; iTop := 0;
    if(CanPlotAO(rAz,rHgt)) then
    begin
      GenMapPos(iR0, iX0, iY0, rAz,rHgt,iLeft,iTop);
      if(iLeft > 0) then
      begin
        IMG__POS_MOON.Height := GetVisualObjectHeight(32,15);
        IMG__POS_MOON.Width := IMG__POS_MOON.Height;

        IMG__POS_MOON.Top := Round(iTop) - IMG__POS_MOON.Height div 2;
        IMG__POS_MOON.Left := Round(iLeft) - IMG__POS_MOON.Width div 2;

        if(rHgt < 5) then
          FadeInRed(IMG__POS_MOON,IMG__POS_MOON.Picture.Bitmap,4)
        else if(rHgt < 10) then
          FadeInRed(IMG__POS_MOON,IMG__POS_MOON.Picture.Bitmap,16);

        if(msLANG_ID = 'DE') then
          IMG__POS_MOON.Hint := 'Mond';

        if(msLANG_ID = 'EN') then
          IMG__POS_MOON.Hint := 'Moon';

        IMG__POS_MOON.ShowHint := true;
        IMG__POS_MOON.Parent := WinControl;
        IMG__POS_MOON.Visible:=true;
      end;
    end;
  end
  else
    IMG__POS_MOON.Visible:=false;

  i:=-1;
  repeat
    Inc(i);

    if((molAOList[i] as TAObject).SHP <> nil) then
      (molAOList[i] as TAObject).SHP.Visible:=false;

    if((molAOList[i] as TAObject).IMG <> nil) then
      (molAOList[i] as TAObject).IMG.Visible:=false;

    if((molAOList[i] as TAObject).L__AO <> nil) then
      (molAOList[i] as TAObject).L__AO.Visible:=false;

    bCanVisu := false;

    if HighLightSearch(molAOList[i] as TAObject) then
    begin
      bObjectFound := true;
      AOSearch := GenAOSearch(i);
      if(LB__SEARCHRES.Items.IndexOf(AOSearch.sLabel) = -1) then
        LB__SEARCHRES.Items.AddObject(AOSearch.sLabel,AOSearch);

    end;

    if(not mbSearch) then
    begin
      if((molAOList[i] as TAObject).sAOType = 'S') and ((molAOList[i] as TStar).rM > mrMagPos) then
        continue;

      if(((molAOList[i] as TAObject).sAOType = 'G') and ((molAOList[i] as TGalaxy).rM > mrMagPos_G)) then
        continue;
    end
    else
    begin
      if((i mod 500000) = 0) then
      begin
        //Application.ProcessMessages;

        if(msLANG_ID = 'DE') and (MessageDlg('Rückfrage','Suche fortsetzen?',mtConfirmation,[mbYes,mbNo],0) = mrNo) then
          mbSearch := false;

        if(msLANG_ID = 'EN') and (MessageDlg('Request','Continue Search?',mtConfirmation,[mbYes,mbNo],0) = mrNo) then
          mbSearch := false;

      end;
    end;

    if(
      ((molAOList[i] as TAObject).sAOType <> 'S') and
      ((molAOList[i] as TAObject).sAOType <> 'P')
      ) then // Plot ALWAYS stars and planets!
    begin
      if( // Do not plot moons
        ((molAOList[i] as TAObject).sAOType = 'M')
        ) then continue;

      // Filter objects before performing time consuming operations take place
      if(not mbShowMessier) then
      begin
        bCanVisu := (
          (((molAOList[i] as TAObject).sAOType = 'G') and (mbShowGalaxies) ) or
          (((molAOList[i] as TAObject).sAOType = 'GC') and (mbShowGlobularClusters)) or
          (((molAOList[i] as TAObject).sAOType = 'N') and (mbShowGalacticNebula)) or
          (((molAOList[i] as TAObject).sAOType = 'PN') and (mbShowPlanetaryNebula)) or
          (((molAOList[i] as TAObject).sAOType = 'OC') and (mbShowOpenClusters)) or
          (((molAOList[i] as TAObject).sAOType = 'C') and mbShowEComets) or
          (((molAOList[i] as TAObject).sAOType = 'Q') and (mbShowQuasars)) or
          (((molAOList[i] as TAObject).sAOType = 'MW'))
        );

        if (not bCanVisu) then
          bCanVisu := HighLightSearch((molAOList[i] as TAObject));

        if(not bCanVisu) then
          Continue;
      end
      else
        if (not bCanVisu) then
          bCanVisu := HighLightSearch((molAOList[i] as TAObject));
    end;

    iRA_HH := (molAOList[i] as TAObject).iRA_Hours;
    iRA_MM := (molAOList[i] as TAObject).iRA_Min;
    iRA_SS := Trunc((molAOList[i] as TAObject).rRA_Sec);

    iDEC_DEG := (molAOList[i] as TAObject).iDec_Deg;
    iDEC_MM := (molAOList[i] as TAObject).iDec_Min;
    iDEC_SS := Round((molAOList[i] as TAObject).rDec_Sec);

    if(iRA_HH >= 24) then
      iRA_HH := Trunc(iRA_HH - 24.0);

    if(iRA_MM > 59) then
    begin
      iRA_MM := 0;

      if(iRA_HH < 23) then
        iRA_HH := iRA_HH + 1
      else
        iRA_HH := 0;

    end;

    if(iRA_SS > 59) then
    begin
      iRA_SS := 59;
      dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS,0);
    end
    else
      dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS,
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - iRA_SS)));

    dtHA := GetHA(dtST,dtRA);

    DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);
    CalcAZ(iDEC_DEG,iDEC_MM,iDEC_SS,iHA_HH,iHA_MM,iHA_SS,rAz,rHgt);

    if ((molAOList[i] as TAObject).sAOType = 'S') and ((molAOList[i] as TStar).sMShowerName_DE <> '') then
    begin
      MeteorShower := GetMeteorShower((molAOList[i] as TStar).sMShowerName_DE);
      if(MeteorShower <> nil) and (MeteorShower.bActive) then
      begin
        MeteorShower.IMG.Visible:=false;
        MeteorShower.Lbl.Visible:=false;
      end;
    end;

    if (
      not CanPlotAO(rAz,rHgt)
       ) then
      continue;

    iTop := 0;
    iLeft := 0;

    if not GenMapPos(iR0, iX0, iY0, rAz,rHgt,iLeft,iTop) then
      continue;

    if(iLeft < 0) or (iTop < 0) or (iLeft > WinControl.Width) or (iTop > WinControl.Height) then
      continue;

    sLabel := GetAOLabel((molAOList[i] as TAObject),msLANG_ID);

    if((molAOList[i] as TAObject).SHP <> nil) then
    begin
      (molAOList[i] as TAObject).SHP.Top := iTop;
      (molAOList[i] as TAObject).SHP.Left := iLeft;
    end;

    if((molAOList[i] as TAObject).IMG <> nil) then
    begin
      (molAOList[i] as TAObject).IMG.Top := iTop;
      (molAOList[i] as TAObject).IMG.Left := iLeft;
    end;

    // Plot star
    if((molAOList[i] as TAObject).sAOType = 'S') then
    begin
      if((molAOList[i] as TStar).rM > mrMagPos) and (not bObjectFound) then
        continue;

      if(miSMDisplayMode = 1) and ((molAOList[i] as TAObject).SHP <> nil) then
      begin
        (molAOList[i] as TAObject).SHP.Pen.Color := clSilver;
        (molAOList[i] as TAObject).SHP.Brush.Color := clWhite;
      end;

      if((molAOList[i] as TAObject).IMG <> nil) then
      begin
        (molAOList[i] as TAObject).IMG.Width := GetIMGMag((molAOList[i] as TStar),mbZoomMode);
        (molAOList[i] as TAObject).IMG.Height := (molAOList[i] as TAObject).IMG.Width;
      end;

      //if ((molAOList[i] as TStar).sSym = 'MARK') and ((molAOList[i] as TAObject).SHP = nil) then
      //  (molAOList[i] as TAObject).SHP := TShape.Create(nil);

      // Modification for found search item
      if HighlightSearch(molAOList[i] as TAObject) then
      begin
        if((miSMDisplayMode = 1) or ((molAOList[i] as TStar).sSym = 'MARK'))
          and ((molAOList[i] as TAObject).SHP <> nil) then
        begin
          (molAOList[i] as TAObject).SHP.Height := 20;
          (molAOList[i] as TAObject).SHP.Width := 20;
          (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
        end
        else
        begin
          if((molAOList[i] as TAObject).IMG = nil) then
          begin
            (molAOList[i] as TStar).IMG := TImage.Create(nil);
            SetStarImage((molAOList[i] as TStar),iTop,iLeft);
          end;
          (molAOList[i] as TAObject).IMG.Height := 20;
          (molAOList[i] as TAObject).IMG.Width := 20;
          (molAOList[i] as TAObject).IMG.Visible:=true; // Display found object in any case.
        end;

        bObjectFoundAndDisplayed := true;
        if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!

      end
      else
      begin
        if((miSMDisplayMode = 1) or ((molAOList[i] as TStar).sSym = 'MARK'))
          and ((molAOList[i] as TAObject).SHP <> nil) then
          (molAOList[i] as TAObject).SHP.Visible:=true //((molAOList[i] as TStar).rM <= mrMagPos)
        else
        begin
          if((molAOList[i] as TAObject).IMG = nil) then
          begin
            (molAOList[i] as TStar).IMG := TImage.Create(nil);
            SetStarImage((molAOList[i] as TStar),iTop,iLeft);
          end;
          (molAOList[i] as TAObject).IMG.Visible:=((molAOList[i] as TStar).rM <= mrMagPos);
        end;
      end;

      if((miSMDisplayMode = 1) or ((molAOList[i] as TStar).sSym = 'MARK'))
        and ((molAOList[i] as TAObject).SHP <> nil) then
      begin

        (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
        (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;
      end
      else
      begin
        if((molAOList[i] as TAObject).IMG = nil) then
        begin
          (molAOList[i] as TStar).IMG := TImage.Create(nil);
          SetStarImage((molAOList[i] as TStar),iTop,iLeft);
        end;

        (molAOList[i] as TAObject).IMG.Top := (molAOList[i] as TAObject).IMG.Top - (molAOList[i] as TAObject).IMG.Height div 2 ; //4;
        (molAOList[i] as TAObject).IMG.Left := (molAOList[i] as TAObject).IMG.Left - (molAOList[i] as TAObject).IMG.Width div 2 ; //4;
      end;

      // Print Labels of brightest stars or alpha stars
      if(mbZoomMode) then iBrightStarLabel_M := 4
      else iBrightStarLabel_M := 2;

      if(
       (
       (((molAOList[i] as TStar).rM < iBrightStarLabel_M) and (miShowStarDescr = 1)) or
       ((((molAOList[i] as TStar).sSym = 'alpha') or (LeftStr((molAOList[i] as TStar).sSym,3) = 'alf')) and (miShowStarDescr = 2))
       )
       ) then
      begin
        if((molAOList[i] as TAObject).L__AO = nil) then
          (molAOList[i] as TAObject).L__AO := TLabel.Create(nil);

        (molAOList[i] as TAObject).L__AO.Left:=(molAOList[i] as TAObject).IMG.Left+2;
        (molAOList[i] as TAObject).L__AO.Top:=(molAOList[i] as TAObject).IMG.Top+2;

        (molAOList[i] as TAObject).L__AO.Font.Color:= (molAOList[i] as TStar).Color; //clRed;
        (molAOList[i] as TAObject).L__AO.Caption := sLabel;
        (molAOList[i] as TAObject).L__AO.Parent := WinControl;
        (molAOList[i] as TAObject).L__AO.Visible := true;
      end;

      // Show constellation names near beta-stars by default. There are some exceptions...
      if(
        (mbShowConstellations) and
        (
          (((molAOList[i] as TStar).sSym = 'beta') and
            ((molAOList[i] as TStar).sCon <> 'Hya') and
            ((molAOList[i] as TStar).sCon <> 'Cyg') and
            ((molAOList[i] as TStar).sCon <> 'Psc')) or
          (((molAOList[i] as TStar).sCon = 'Hya') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Mic') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Tuc') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Lyn') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Sex') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Vul') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Tel') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Ant') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Phe') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Cae') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Nor') and ((molAOList[i] as TStar).sSym = 'delta')) or
          (((molAOList[i] as TStar).sCon = 'Psc') and ((molAOList[i] as TStar).sSym = 'delta')) or
          (((molAOList[i] as TStar).sCon = 'Equ') and ((molAOList[i] as TStar).sSym = 'gamma')) or
          (((molAOList[i] as TStar).sCon = 'Cyg') and ((molAOList[i] as TStar).sSym = 'alpha')) or
          (((molAOList[i] as TStar).sCon = 'Eri') and ((molAOList[i] as TStar).sSym = 'gamma'))
        )
        ) or ((molAOList[i] as TStar).bSelSign) then
      begin
        sSignName := GetSignName((molAOList[i] as TStar).sCon,molSignList,msLANG_ID);
        if(sSignName <> '') then
        begin
          if((molAOList[i] as TAObject).L__AO = nil) then
            (molAOList[i] as TAObject).L__AO := TLabel.Create(nil);

          (molAOList[i] as TAObject).L__AO.Left:=(molAOList[i] as TAObject).IMG.Left+2;
          (molAOList[i] as TAObject).L__AO.Top:=(molAOList[i] as TAObject).IMG.Top+2;

          (molAOList[i] as TAObject).L__AO.Font.Color:= clLime; //(molAOList[i] as TStar).Color; //clRed;

          if(miStarmapView > 0) then
            (molAOList[i] as TAObject).L__AO.Font.Size := 14
          else
            (molAOList[i] as TAObject).L__AO.Font.Size := 10;

          (molAOList[i] as TAObject).L__AO.Caption := sSignName;

          (molAOList[i] as TAObject).L__AO.Parent := WinControl;
          (molAOList[i] as TAObject).L__AO.Visible := true;
         end;
      end;

      if( (molAOList[i] as TStar).sSym <> 'MARK') then
      begin
        if(miSMDisplayMode = 1) and ((molAOList[i] as TAObject).SHP <> nil) then
        begin
          (molAOList[i] as TAObject).SHP.PopupMenu := PMENU__AOBJECT;
          (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
          (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
          //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
        end
        else
        begin
          if((molAOList[i] as TAObject).IMG = nil) then
          begin
            (molAOList[i] as TStar).IMG := TImage.Create(nil);
            SetStarImage((molAOList[i] as TStar),iTop,iLeft);
          end;

          (molAOList[i] as TAObject).IMG.PopupMenu := PMENU__AOBJECT;
          (molAOList[i] as TAObject).IMG.OnMouseUp := @AOVisOnMouseUp;
          (molAOList[i] as TAObject).IMG.OnMouseEnter := @AOVisOnMouseEnter;
          //(molAOList[i] as TAObject).IMG.OnMouseLeave := @AOVisOnMouseLeave;
        end;

        if((molAOList[i] as TAObject).IMG = nil) then
        begin
          (molAOList[i] as TStar).IMG := TImage.Create(nil);
          SetStarImage((molAOList[i] as TStar),iTop,iLeft);
        end;

        (molAOList[i] as TAObject).IMG.ShowHint := true;
        (molAOList[i] as TAObject).IMG.Tag:=i;
        (molAOList[i] as TAObject).IMG.Parent := WinControl;
      end
      else
      begin
        // Marks
        if((molAOList[i] as TAObject).SHP <> nil) then
        begin
          if(msLANG_ID = 'DE') then
            (molAOList[i] as TAObject).SHP.Hint:=(molAOList[i] as TAObject).sName_DE
          else
            (molAOList[i] as TAObject).SHP.Hint:=(molAOList[i] as TAObject).sName_EN;

          (molAOList[i] as TAObject).SHP.ShowHint:=true;
        end;

        if( (molAOList[i] as TStar).sSpType = 'MARK-VOID')
          and ((molAOList[i] as TAObject).SHP <> nil) then
        begin
          (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
        end;

        (molAOList[i] as TAObject).SHP.ShowHint := true;
        (molAOList[i] as TAObject).SHP.Tag:=i;
        (molAOList[i] as TAObject).SHP.Parent := WinControl;
      end;

      // Plot meteor shower image near main star
      if (mbShowMShower) and ((molAOList[i] as TStar).sMShowerName_DE <> '') and ((molAOList[i] as TStar).IMG <> nil) then
      begin
        MeteorShower := GetMeteorShower((molAOList[i] as TStar).sMShowerName_DE);
        if(MeteorShower <> nil) and (MeteorShower.bActive) then
        begin
          if(msLANG_ID = 'DE') then
            MeteorShower.IMG.Hint := MeteorShower.sName_DE;

          if(msLANG_ID = 'EN') then
            MeteorShower.IMG.Hint := MeteorShower.sName_EN;

          MeteorShower.IMG.OnMouseUp := @MeteorShowerMouseUp;
          MeteorShower.IMG.Left:=(molAOList[i] as TAObject).IMG.Left-5;
          MeteorShower.IMG.Top:=(molAOList[i] as TAObject).IMG.Top;
          MeteorShower.IMG.Tag:=i;
          MeteorShower.IMG.Visible:=true;
          MeteorShower.IMG.Parent := WinControl;

          MeteorShower.Lbl.OnMouseUp := @MeteorShowerMouseUp;
          MeteorShower.Lbl.Font.Color:=clBlue;
          MeteorShower.Lbl.Caption := MeteorShower.sName_DE;
          MeteorShower.Lbl.Left:=(molAOList[i] as TAObject).IMG.Left-5;
          MeteorShower.Lbl.Top:=(molAOList[i] as TAObject).IMG.Top+40;
          MeteorShower.Lbl.Visible:=true;
          MeteorShower.Lbl.Tag:=i;
          MeteorShower.Lbl.Parent := WinControl;

        end;
      end;

      RegisterAsVisibleAO(i);
      continue;
     end;

    // Plot comets
    if((mbZoomMode or mbShowEComets or (HighlightSearch(molAOList[i] as TAObject))) and ((molAOList[i] as TAObject).sAOType = 'C')) then
    begin
      (molAOList[i] as TAObject).SHP.PopupMenu := PMENU__AOBJECT;

      // Plot Comet path
      PlotSolSysBodyPath(iR0, iX0, iY0, dtST, i); //, SkyBitmap);

      (molAOList[i] as TAObject).SHP.Pen.Color := clLime;
      (molAOList[i] as TAObject).SHP.Brush.Color := (molAOList[i] as TComet).Color; //clWhite;
      (molAOList[i] as TAObject).SHP.ShowHint := true;
      (molAOList[i] as TAObject).SHP.Hint := sLabel;
      (molAOList[i] as TAObject).SHP.Parent := WinControl;

      if HighlightSearch(molAOList[i] as TAObject) then
      begin
        (molAOList[i] as TAObject).SHP.Height := 20;
        (molAOList[i] as TAObject).SHP.Width := 20;
        (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
        bObjectFoundAndDisplayed := true;
        if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!
      end
      else
      begin
        (molAOList[i] as TAObject).SHP.Height:=8;
        (molAOList[i] as TAObject).SHP.Width:=8;
      end;

      // Adjust Shape Position
      (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
      (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

      (molAOList[i] as TAObject).SHP.Visible:=true;
      (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
      (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
      //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
      (molAOList[i] as TAObject).SHP.Tag:=i;

      (molAOList[i] as TAObject).L__AO.Left := (molAOList[i] as TAObject).SHP.Left+2;
      (molAOList[i] as TAObject).L__AO.Top := (molAOList[i] as TAObject).SHP.Top+2;

      if((molAOList[i] as TComet).rRs < 3) then
        (molAOList[i] as TAObject).L__AO.Font.Color := clLime
      else
        (molAOList[i] as TAObject).L__AO.Font.Color := clGreen;

      (molAOList[i] as TAObject).L__AO.Caption := (molAOList[i] as TAObject).SHP.Hint  +
        ' ' + FloatToStrF((molAOList[i] as TComet).rRs,ffFixed,8,1) + ' AU';
      (molAOList[i] as TAObject).L__AO.Visible := true;
      (molAOList[i] as TAObject).L__AO.Parent := WinControl;

      (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
      (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
      //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
      (molAOList[i] as TAObject).SHP.Tag:=i;

      RegisterAsVisibleAO(i);
      continue;
    end;

    // Plot planets
    if((molAOList[i] as TAObject).sAOType = 'P') and (mbPlanetShow) then
    begin
      if((molAOList[i] as TPlanet).sPlanetType = 'E') then // Do not visualize earth
        continue;

      if ((not mbShowAsteroids) and ((molAOList[i] as TPlanet).sPlanetType = 'A')) then
        continue;

      (molAOList[i] as TAObject).SHP.PopupMenu := PMENU__AOBJECT;

      PlotSolSysBodyPath(iR0, iX0, iY0, dtST, i); //, SkyBitmap);

      (molAOList[i] as TAObject).SHP.Pen.Color := clYellow;
      (molAOList[i] as TAObject).SHP.Brush.Color := (molAOList[i] as TPlanet).Color; //clWhite;
      (molAOList[i] as TAObject).SHP.ShowHint := true;
      (molAOList[i] as TAObject).SHP.Parent := WinControl;

      if HighlightSearch(molAOList[i] as TAObject) then
      begin
       (molAOList[i] as TAObject).SHP.Height := 20;
       (molAOList[i] as TAObject).SHP.Width := 20;
       (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
       bObjectFoundAndDisplayed := true;
       if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!
      end
      else
      begin
        (molAOList[i] as TAObject).SHP.Height:=12;
        (molAOList[i] as TAObject).SHP.Width:=12;
      end;

      //if(mbPlanetImage) and (Uppercase((molAOList[i] as TPlanet).sPlanetType) = 'P') and FileExists(ConvertWinPath('img\' + (molAOList[i] as TPlanet).sName_EN + '.png')) then
      if(mbPlanetImage) and (Uppercase((molAOList[i] as TPlanet).sPlanetType) = 'P') and ((molAOList[i] as TPlanet).bHasImage) then
      begin
         (molAOList[i] as TPlanet).IMG.PopupMenu := PMENU__AOBJECT;
         (molAOList[i] as TPlanet).IMG.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TPlanet).IMG.Height div 2 ;
         (molAOList[i] as TPlanet).IMG.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TPlanet).IMG.Width div 2 ;
         (molAOList[i] as TPlanet).IMG.Visible:=true;

        //GetVisualObjectHeight(32,7);
        if( (molAOList[i] as TPlanet).sName_DE = 'Saturn' ) then
          (molAOList[i] as TPlanet).IMG.Height := GetVisualObjectHeight((molAOList[i] as TPlanet).rVisuAngle_arcsec/60.0,15)
        else
          (molAOList[i] as TPlanet).IMG.Height := GetVisualObjectHeight((molAOList[i] as TPlanet).rVisuAngle_arcsec/60.0,10);

        (molAOList[i] as TPlanet).IMG.Width := (molAOList[i] as TPlanet).IMG.Height;
        (*

        if((molAOList[i] as TPlanet).sName_DE = 'Saturn') then
         begin
           // Larger because of Saturn's ring system
           (molAOList[i] as TPlanet).IMG.Height := 20;
           (molAOList[i] as TPlanet).IMG.Width := 20;
         end
         else
         begin
           (molAOList[i] as TPlanet).IMG.Width := 12;
           (molAOList[i] as TPlanet).IMG.Height := 12;
         end;
         *)

         (molAOList[i] as TPlanet).IMG.OnMouseUp := @AOVisOnMouseUp;
         (molAOList[i] as TPlanet).IMG.OnMouseEnter := @AOVisOnMouseEnter;
         //(molAOList[i] as TPlanet).IMG.OnMouseLeave := @AOVisOnMouseLeave;
         (molAOList[i] as TPlanet).IMG.Parent:=WinControl;
         (molAOList[i] as TPlanet).IMG.Tag:=i;
      end
      else
      begin
        // Adjust Shape Position
        (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
        (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

        (molAOList[i] as TAObject).SHP.Visible:=true;
        (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
        (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
        //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
        (molAOList[i] as TAObject).SHP.Tag:=i;
      end;

      (molAOList[i] as TAObject).L__AO.Left := (molAOList[i] as TAObject).SHP.Left+2;
      (molAOList[i] as TAObject).L__AO.Top := (molAOList[i] as TAObject).SHP.Top+2;
      (molAOList[i] as TAObject).L__AO.Font.Color := clAqua;
      (molAOList[i] as TAObject).L__AO.Caption := Trim((molAOList[i] as TAObject).SHP.Hint);
      (molAOList[i] as TAObject).L__AO.Visible := true;
      (molAOList[i] as TAObject).L__AO.Transparent := true;
      (molAOList[i] as TAObject).L__AO.Parent := WinControl;

      RegisterAsVisibleAO(i);
      continue;
    end;

    // Plot Galaxies
    if(
      ((molAOList[i] as TAObject).sAOType = 'G')
      and (
        mbShowGalaxies or mbSearch
        or (mbShowMessier and ((molAOList[i] as TInterstellarObject).sMessier <> ''))
      )
      ) then
    begin
      if HighlightSearch(molAOList[i] as TAObject) then
      begin
        if((molAOList[i] as TGalaxy).SHP = nil) then
          (molAOList[i] as TAObject).SHP := TShape.Create(nil);

        SetShapeImage((molAOList[i] as TAObject).sAOType,(molAOList[i] as TGalaxy),iTop,iLeft);

        (molAOList[i] as TAObject).SHP.Height := 20;
        (molAOList[i] as TAObject).SHP.Width := 20;
        (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.

        bObjectFoundAndDisplayed := true;
        if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!

      end
      else if((molAOList[i] as TGalaxy).rM > mrMagPos_G) or (mbSearch and (not mbShowGalaxies)) then  // contiune for too faint galaxies or searched object not found
        continue
      else if((molAOList[i] as TGalaxy).SHP = nil) then
      begin
        (molAOList[i] as TGalaxy).SHP := TShape.Create(nil);
        SetShapeImage((molAOList[i] as TAObject).sAOType,(molAOList[i] as TGalaxy),iTop,iLeft);
      end;

      (molAOList[i] as TAObject).SHP.Shape := stEllipse;

      SetInterstellarObjectShapeColor(molAOList[i] as TInterstellarObject);

      (molAOList[i] as TAObject).SHP.Brush.Color:=clGray;
      (molAOList[i] as TAObject).SHP.ShowHint := true;

      // Adjust Shape Position
      (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
      (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

      (molAOList[i] as TAObject).SHP.Parent := WinControl;
      (molAOList[i] as TAObject).SHP.Visible:= true; //((molAOList[i] as TGalaxy).rM <= mrMagPos_G);

      (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
      (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
      //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
      (molAOList[i] as TAObject).SHP.Tag:=i;

      GenAOLabel((molAOList[i] as TAObject),WinControl);

      RegisterAsVisibleAO(i);
      continue;
    end;

    // Plot Quasars
    if(
      ((molAOList[i] as TAObject).sAOType = 'Q')
      and (
        mbShowQuasars or (HighlightSearch(molAOList[i] as TAObject))
        or (mbShowMessier and ((molAOList[i] as TInterstellarObject).sMessier <> ''))
      )
      ) then
    begin
      (molAOList[i] as TAObject).SHP.Shape := stEllipse;

      SetInterstellarObjectShapeColor(molAOList[i] as TInterstellarObject);

      //(molAOList[i] as TAObject).SHP.Pen.Color := clMaroon;
      (molAOList[i] as TAObject).SHP.Brush.Color:=clMaroon;
      (molAOList[i] as TAObject).SHP.ShowHint := true;

      if HighlightSearch(molAOList[i] as TAObject) then
      begin
       (molAOList[i] as TAObject).SHP.Height := 20;
       (molAOList[i] as TAObject).SHP.Width := 20;
       (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
       bObjectFoundAndDisplayed := true;
       if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!

      end
      else
      begin
        (molAOList[i] as TAObject).SHP.Height:=4;
        (molAOList[i] as TAObject).SHP.Width:=8;
      end;

      // Adjust Shape Position
      (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
      (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

      (molAOList[i] as TAObject).SHP.Parent := WinControl;
      (molAOList[i] as TAObject).SHP.Visible:=true;

      (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
      (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
      //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
      (molAOList[i] as TAObject).SHP.Tag:=i;

      GenAOLabel((molAOList[i] as TAObject),WinControl);

      RegisterAsVisibleAO(i);
      continue;
    end;

    // Plot Globular Clusters
    if(
      ((molAOList[i] as TAObject).sAOType = 'GC')
      and (
        mbShowGlobularClusters or (HighlightSearch(molAOList[i] as TAObject))
        or (mbShowMessier and ((molAOList[i] as TInterstellarObject).sMessier <> ''))
      )
      ) then
    begin
      (molAOList[i] as TAObject).SHP.Shape := stCircle;

      SetInterstellarObjectShapeColor(molAOList[i] as TInterstellarObject);

      (molAOList[i] as TAObject).SHP.ShowHint := true;

      if HighlightSearch(molAOList[i] as TAObject) then
      begin
       (molAOList[i] as TAObject).SHP.Height := 20;
       (molAOList[i] as TAObject).SHP.Width := 20;
       (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
       bObjectFoundAndDisplayed := true;
       if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!

      end;

      // Adjust Shape Position
      (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
      (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

      (molAOList[i] as TAObject).SHP.Parent := WinControl;
      (molAOList[i] as TAObject).SHP.Visible:=true;

      (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
      (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
      //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
      (molAOList[i] as TAObject).SHP.Tag:=i;

      GenAOLabel((molAOList[i] as TAObject),WinControl);

      RegisterAsVisibleAO(i);
      continue;
    end;


    // Plot Galactic Nebula
    if(
      ((molAOList[i] as TAObject).sAOType = 'N')
      and (
        mbShowGalacticNebula or (HighlightSearch(molAOList[i] as TAObject))
        or (mbShowMessier and ((molAOList[i] as TInterstellarObject).sMessier <> ''))
      )
      ) then
    begin
      (molAOList[i] as TAObject).SHP.Shape := stCircle;

      SetInterstellarObjectShapeColor(molAOList[i] as TInterstellarObject);

      //(molAOList[i] as TAObject).SHP.Pen.Color := clRed;
      (molAOList[i] as TAObject).SHP.Brush.Color:=clRed;
      (molAOList[i] as TAObject).SHP.ShowHint := true;

      if HighlightSearch(molAOList[i] as TAObject) then
      begin
       (molAOList[i] as TAObject).SHP.Height := 20;
       (molAOList[i] as TAObject).SHP.Width := 20;
       (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
       bObjectFoundAndDisplayed := true;
       if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!

      end;

      // Adjust Shape Position
      (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
      (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

      (molAOList[i] as TAObject).SHP.Parent := WinControl;
      (molAOList[i] as TAObject).SHP.Visible:=true;

      (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
      (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
      //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
      (molAOList[i] as TAObject).SHP.Tag:=i;

      GenAOLabel((molAOList[i] as TAObject),WinControl);

      RegisterAsVisibleAO(i);
      continue;
    end;

    // Plot Planetary Nebula
    if(
      ((molAOList[i] as TAObject).sAOType = 'PN')
      and (
        mbShowPlanetaryNebula or (HighlightSearch(molAOList[i] as TAObject))
        or (mbShowMessier and ((molAOList[i] as TInterstellarObject).sMessier <> ''))
      )
      ) then
    begin
      (molAOList[i] as TAObject).SHP.Shape := stCircle;

      SetInterstellarObjectShapeColor(molAOList[i] as TInterstellarObject);

      //(molAOList[i] as TAObject).SHP.Pen.Color := clPurple;
      (molAOList[i] as TAObject).SHP.Brush.Color:=clPurple;
      (molAOList[i] as TAObject).SHP.ShowHint := true;

      if HighlightSearch(molAOList[i] as TAObject) then
      begin
       (molAOList[i] as TAObject).SHP.Height := 20;
       (molAOList[i] as TAObject).SHP.Width := 20;
       (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
       bObjectFoundAndDisplayed := true;
       if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!

      end;

      // Adjust Shape Position
      (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
      (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

      (molAOList[i] as TAObject).SHP.Parent := WinControl;
      (molAOList[i] as TAObject).SHP.Visible:=true;

      (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
      (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
      //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
      (molAOList[i] as TAObject).SHP.Tag:=i;

      GenAOLabel((molAOList[i] as TAObject),WinControl);

      RegisterAsVisibleAO(i);
      continue;
    end;

    // Plot Open Clusters
    if(
      ((molAOList[i] as TAObject).sAOType = 'OC')
      and (
        mbShowOpenClusters or (HighlightSearch(molAOList[i] as TAObject))
        or (mbShowMessier and ((molAOList[i] as TInterstellarObject).sMessier <> ''))
      )
      ) then
    begin
      (molAOList[i] as TAObject).SHP.Shape := stDiamond;

      SetInterstellarObjectShapeColor(molAOList[i] as TInterstellarObject);

      //(molAOList[i] as TAObject).SHP.Pen.Color := clPurple;
      (molAOList[i] as TAObject).SHP.Brush.Color:=clWhite;
      (molAOList[i] as TAObject).SHP.ShowHint := true;

      if HighlightSearch(molAOList[i] as TAObject) then
      begin
       (molAOList[i] as TAObject).SHP.Height := 20;
       (molAOList[i] as TAObject).SHP.Width := 20;
       (molAOList[i] as TAObject).SHP.Visible:=true; // Display found object in any case.
       bObjectFoundAndDisplayed := true;
       if(CBX__EXACTMATCH.Checked) then mbSearch := false; // Stop search for any higher magnitudes in GenStarMap()!
      end;

      // Adjust Shape Position
      if((molAOList[i] as TAObject).SHP <> nil) then
      begin
        (molAOList[i] as TAObject).SHP.Top := (molAOList[i] as TAObject).SHP.Top - (molAOList[i] as TAObject).SHP.Height div 2 ; //4;
        (molAOList[i] as TAObject).SHP.Left := (molAOList[i] as TAObject).SHP.Left - (molAOList[i] as TAObject).SHP.Width div 2 ; //4;

        (molAOList[i] as TAObject).SHP.Parent := WinControl;
        (molAOList[i] as TAObject).SHP.Visible:=true;

        (molAOList[i] as TAObject).SHP.OnMouseUp := @AOVisOnMouseUp;
        (molAOList[i] as TAObject).SHP.OnMouseEnter := @AOVisOnMouseEnter;
        //(molAOList[i] as TAObject).SHP.OnMouseLeave := @AOVisOnMouseLeave;
        (molAOList[i] as TAObject).SHP.Tag:=i;
      end;

      GenAOLabel((molAOList[i] as TAObject),WinControl);

      RegisterAsVisibleAO(i);
      continue;
    end;

  until (i >= molAOList.Count-1);

  WinControl.EnableAlign;

  if(mbSearch) and (mbSearchMsg) then
  begin
    mbSearchMsg := false;
    if(not bObjectFound) then
    begin
      if(msLANG_ID = 'DE') then MessageDlg('Suche fehlgeschlagen','''' + msSearch + ''' nicht in Datenbank gefunden',mtInformation,[mbOK],0)
      else MessageDlg('Search failed','''' + msSearch + ''' not found in database',mtInformation,[mbOK],0)
    end
    else
    begin
      if(not bObjectFoundAndDisplayed) then
      begin
        if(msLANG_ID = 'DE') then MessageDlg('Suche erfolgreich','''' + msSearch + ''' in aktueller Himmelsansicht unter Horizont',mtInformation,[mbOK],0)
        else MessageDlg('Search successful','''' + msSearch + ''' in current sky view below horizon',mtInformation,[mbOK],0)
      end;
    end;
  end;

end;

(*
procedure GradHorizontal(Canvas:TCanvas; Rect:TRect; FromColor, ToColor:TColor) ;
 var
   X:integer;
   dr,dg,db:Extended;
   C1,C2:TColor;
   r1,r2,g1,g2,b1,b2:Byte;
   R,G,B:Byte;
   cnt:integer;
 begin
   C1 := FromColor;
   R1 := GetRValue(C1) ;
   G1 := GetGValue(C1) ;
   B1 := GetBValue(C1) ;

   C2 := ToColor;
   R2 := GetRValue(C2) ;
   G2 := GetGValue(C2) ;
   B2 := GetBValue(C2) ;

   dr := (R2-R1) / Rect.Right-Rect.Left;
   dg := (G2-G1) / Rect.Right-Rect.Left;
   db := (B2-B1) / Rect.Right-Rect.Left;

   cnt := 0;
   for X := Rect.Left to Rect.Right-1 do
   begin
     R := R1+Ceil(dr*cnt) ;
     G := G1+Ceil(dg*cnt) ;
     B := B1+Ceil(db*cnt) ;

     !!!P__STARMAP.Canvas.EllipseC();

     Canvas.Pen.Color := RGB(R,G,B) ;
     Canvas.MoveTo(X,Rect.Top) ;
     Canvas.LineTo(X,Rect.Bottom) ;
     inc(cnt) ;
   end;
 end;
*)

procedure TF__ASTROTOOLBOX.PlotRing(Panel: TPanel; cColor: TColor; iR0,iX0,iY0, iThickness: Integer);
var
  iR: Integer;
begin
  for iR := iR0 to iR0+iThickness do // Only horizon line!
  begin
    Panel.Canvas.Brush.Style:=bsClear;
    Panel.Canvas.Pen.Color := cColor;

    if(miStarmapView = 0) then
      Panel.Canvas.EllipseC(iX0,iY0,iR,iR);

    if(miStarmapView > 0) then
      Panel.Canvas.Line(0,Panel.Height - (iR0-iR),Panel.Width,Panel.Height - (iR0-iR));

  end;
end;

procedure TF__ASTROTOOLBOX.ColorizeSky(Panel: TPanel; iSkyType,iR0,iX0,iY0: Integer);
var
  iR:integer;
  dr,dg,db:Extended;
  C1,C2:TColor;
  r1,r2,g1,g2,b1,b2:Byte;
  R,G,B:Byte;
  cnt:integer;
  FromColor, ToColor, HorColor:TColor;
  iBorder: Integer;
begin

   case iSkyType of
     1: // Twilight
     begin
       FromColor := clNavy;
       ToColor := clSunRise;
     end;
     2: // DaySky
     begin
       FromColor := clDaySky;
       ToColor := clSkyBlue;
     end;
     0: // Summer's dark
     begin
       FromColor := clSummerDarkSky;
       ToColor := clNavy;
     end;
     else
     begin
       FromColor := clDeepDarkSky;
       ToColor := clDkGray2;
     end;
   end; // case

   if(miStarmapView > 0) then
     GetHorPic(iSkyType);

   //if (not mbHDust) or (miTimePlayMode <> 0) then exit;
   if (not mbHDust) then exit;

   C1 := FromColor;
   HorColor := ToColor;

   R1 := GetRVal(C1) ;
   G1 := GetGVal(C1) ;
   B1 := GetBVal(C1) ;

   C2 := ToColor;
   R2 := GetRVal(C2) ;
   G2 := GetGVal(C2) ;
   B2 := GetBVal(C2) ;

   iBorder := iR0 div 10;
   dr := (R2-R1) div iBorder;
   dg := (G2-G1) div iBorder;
   db := (B2-B1) div iBorder;

   cnt := 0;
   for iR := iR0-iBorder to iR0 do // Only horizon line!
   begin
     R := R1+Ceil(dr*cnt) ;
     G := G1+Ceil(dg*cnt) ;
     B := B1+Ceil(db*cnt) ;

     Panel.Canvas.Brush.Style:=bsClear;
     Panel.Canvas.Pen.Color := GetRGB(R,G,B) ;
     if(miStarmapView = 0) then
       Panel.Canvas.EllipseC(iX0,iY0,iR,iR)
     else if(miStarmapView > 0) then
       Panel.Canvas.Line(0,Panel.Height - (iR0-iR),Panel.Width,Panel.Height - (iR0-iR));

     Inc(cnt) ;

     if(miStarmapView > 0) and (iR = iR0 - 1) then
       HorColor := Panel.Canvas.Pen.Color;

   end;

   if(miStarmapView > 0) then
   begin
     P__LANDSCAPE.Color:=HorColor;
     P__LANDSCAPE.BevelColor:=HorColor;
   end;

end;
{
procedure TF__ASTROTOOLBOX.GetPoleTraceCoo(bNorth: Boolean; iIndex: Integer; var rRA: Real; var rDEC: Real);
// Attention: rRA is between 0..360 Degree here
(*
var
  raRACoo: array[0..12] of Real;
  raDECCoo: array[0..12] of Real;
*)
begin
  if(bNorth) then
  begin

    //EclipticToEquatorial(mdtWF,

    (*
    raRACoo[0] := 18 *360/24; // Current north pole RA
    raDECCoo[0] := 66.57; // Current north pole DEC

    raRACoo[1] := (22+55/60) *360/24  // +4000
    raDECCoo[1] := 78 + 46/60;

    raRACoo[2] := (22+09/60) *360/24  // +6000
    raDECCoo[2] := 68 + 45/60;

    raRACoo[3] := (21+19/60) *360/24  // +8000
    raDECCoo[3] := 58 + 49/60;

    raRACoo[4] := (20+37/60) *360/24  // +10000
    raDECCoo[4] := 50 + 40/60;

    raRACoo[5] := (19+26/60) *360/24  // +12000
    raDECCoo[5] := 45 + 32/60;

    raRACoo[6] := (19+26/60) *360/24  // +14000
    raDECCoo[6] := 45 + 32/60;
    *)
  end;

end;
}
procedure TF__ASTROTOOLBOX.GenEquPath(Panel: TPanel; iR0, iX0, iY0: Integer; sType: string);
var
  iRA_HH, iRA_MM: Word;
  iDEC_DEG, iDEC_MM: SmallInt;
  ii, i, i1, i2: Integer;
  rRA_SS, rDEC_SS: Real;
  rRA, rDEC: Real;
  iHA_HH,iHA_MM,iHA_SS,iHA_MS: Word;
  rAz,rHgt: Real;
  //rR, rDX, rDY, rDX_Partner, rDY_Partner: Real;
  dtJulDat, dtWT, dtST, dtRA, dtHA: TDateTime;
  iXPrev: Integer;
  iCnt: Integer;
  j, j1, j2: Integer;
  bPlotLabel: Boolean;
  sLabel: string;
  iLeft, iTop: Integer;
  //rHgtOffset: Real;
  (*
  rMWBaseFactor: Real;
  bPlotLine: Boolean;
  ColorStd: TColor;
  *)
  rMWDisVal: Real;
  rLambda, rBeta0: Real;
  rPhi, rHA_SS: Real;
begin
  if(not mbShowPoleP) and ((sType = 'POLP_N') or (sType = 'POLP_S')) then
    exit;

  if(not mbSM_ECLIPTIC) and (sType = 'ECLIPTIC') then
    exit;

  if(not mbSM_EQUATOR) and (sType = 'EQUATOR') then
    exit;

  if(not mbSM_GALACTIC) and (sType = 'GALACTIC') then
    exit;

  if(not mbSM_RA) and (sType = 'RA') then
    exit;

  if(not mbSM_DEC) and (sType = 'DEC') then
    exit;

  if(not mbSM_HS) and (sType = 'HOR') then
    exit;

  if(not mbSM_AS) and (sType = 'HGT') then
    exit;

  if(sType <> 'ECLIPTIC') and
    (sType <> 'EQUATOR') and
    (sType <> 'GALACTIC') and
    (sType <> 'RA') and
    (sType <> 'DEC') and
    (sType <> 'POLP_N') and
    (sType <> 'POLP_S') and
    (sType <> 'HOR') and
    (sType <> 'HGT')
    then
    exit;

  iCnt := 0;
  i1 :=0; i2 := 0;
  j1 := 1; j2 := 1;
  iXPrev := 0;
  iHA_HH:=0; iHA_MM:=0; iHA_SS:=0;
  iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0;
  rRA:=0; rDEC:=0;
  rLambda:=0; rBeta0:=0;

  rPhi := (miGLat_DEG + miGLat_MIN/60.0) * Pi/180.0;

  Panel.Canvas.Pen.Width:=3;

  if(LeftStr(sType,4) = 'POLP') then
  begin
    Panel.Canvas.Pen.Color:=clBlue;
    //Bitmap.Canvas.Brush.Color:=clYellow;
    i1 := -180; i2 := 180;
    j1 := 1; j2 := 1;
    if(sType = 'POLP_N') then
      EquatorialToEcliptic(GetWTime(),13.9,66.57,rLambda,rBeta0) // 18 !!!???
    else
      EquatorialToEcliptic(GetWTime(),10.1,-66.57,rLambda,rBeta0); // 6 !!!???

    // Precession: Beta0 is constant!!!
  end;

  if(sType = 'ECLIPTIC') then
  begin
    Panel.Canvas.Pen.Color:=clYellow;
    i1 := 0; i2 := 360;
    j1 := 1; j2 := 1;
  end;

  if(sType = 'RA') then
  begin
    Panel.Canvas.Pen.Color:=clPurple;
    Panel.Canvas.Font.Color:=clFuchsia;
    //Panel.Canvas.Brush.Color:=clTeal;

    i1 := -90; i2 := 90;
    j1 := 0; j2 := 23;

    Panel.Canvas.Pen.Width:=1;
  end;

  if(sType = 'EQUATOR') then
  begin
    Panel.Canvas.Pen.Color:=clFuchsia;
    i1 := 0; i2 := 360;
    j1 := 0; j2 := 0;
    Panel.Canvas.Pen.Width:=2;
  end;

  if(sType = 'DEC') then
  begin
    Panel.Canvas.Pen.Color:=clPurple;
    Panel.Canvas.Font.Color:=clFuchsia;
    i1 := 0; i2 := 360;
    j1 := -9; j2 := 9;
    Panel.Canvas.Pen.Width:=1;
  end;

  if(sType = 'HOR') then
  begin
    Panel.Canvas.Pen.Color:=clGray;
    Panel.Canvas.Font.Color:=clSilver;

    rPhi := (miGLat_DEG + miGLat_MIN/60.0) * Pi/180.0;
    i1 := 1; // Do not start with 0 to avoid unwanted lines e.g. in skymap mode
    i2 := 359;
    j1 := 0; j2 := 8;
    (* USE:
    HorizonToEquatorial(rAlt,rAz,rPhi, iHH_HH, iHH_MM, rHH_SS,
      iDEC_DEG, iDEC_MM, rDEC_SS);
    *)

    Panel.Canvas.Pen.Width:=1;
  end;

  if(sType = 'HGT') then
  begin
    Panel.Canvas.Pen.Color:=clGray;
    Panel.Canvas.Font.Color:=clSilver;

    rPhi := (miGLat_DEG + miGLat_MIN/60.0) * Pi/180.0;
    i1 := 0; i2 := 89;
    case miStarMapView of
      0: begin j1 := 0; j2 := 35; end; // All
      1: begin j1 := 0; j2 := 35; end; //North
      2: begin j1 := 9; j2 := 27; end; //South
      3: begin j1 := 0; j2 := 18; end; //East
      4: begin j1 := 18; j2 := 35; end; //West
    end; // case

     // rAz
    (* USE:
    HorizonToEquatorial(rAlt,rAz,rPhi, iHH_HH, iHH_MM, rHH_SS,
      iDEC_DEG, iDEC_MM, rDEC_SS);
    *)

    Panel.Canvas.Pen.Width:=1;
  end;

  if(sType = 'GALACTIC') then
  begin
    Panel.Canvas.Pen.Color:=clBlue;
    i1 := 0; i2 := 360;
    j1 := 0; j2 := 0;
  end;

  dtJulDat := 0;
  dtWT := GetWTime();
  dtST := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dtJulDat);

  for j:=j1 to j2 do
  begin
    // Plot Missing line to top on 0 degree north
    if(sType = 'HGT') and (miStarMapView = 1) and (j = j1+1) then
    begin
      Panel.Canvas.LineTo(iXPrev,3);
    end;

    iXPrev := 0;
    //iYPrev := 0;
    iCnt := 0;

    bPlotLabel := (sType = 'DEC') or (sType = 'RA') or (sType = 'HOR') or (sType = 'HGT');

    if(sType = 'DEC') then
      sLabel := IntToStr(j*10)
    else if(sType = 'RA') then
      sLabel := IntToStr(j)
    else if(sType = 'HOR') then
      sLabel := IntToStr(j*10)
    else if(sType = 'HGT') then
      sLabel := IntToStr(j*10)
    else
      sLabel := '';

    //ShowMessage('STOP! j: ' + IntToStr(j));

    for ii:=i1 to i2 do
    begin
      i := ii;

      if(sType = 'RA') then
      begin
        if(ii = i2) then iCnt := 0;
        // ii -> i: Count from south(-90) DEC to north(+90) for norther skies, from north DEC (+90) to south(-90) for southern skies!!!
        // to get similar RA scaling lines
        if(miGLat_DEG > 0) then
          i := ii
        else
          i := -ii;

        rRA := j*15;
        rDEC := i*1;

        if(j=0) then
        begin
          Panel.Canvas.Pen.Color:=clFuchsia;
          Panel.Canvas.Pen.Width:=2;
        end
        else
        begin
          Panel.Canvas.Pen.Color:=clPurple;
          Panel.Canvas.Pen.Width:=1;
        end;
      end;

      if(sType = 'ECLIPTIC') then
        GetEcpliticCoo(i*1.0,dtWT,miDST_HH,miUTC_HH,rRA,rDEC);

      if(sType = 'POLP_N') then
      begin
        rLambda := i*1.0;
        EclipticToEquatorialV2(GetWTime(),rLambda,rBeta0,rRA,rDEC);
        rRA := rRA * 360/24;
      end;

      if(sType = 'POLP_S') then
      begin
        rLambda := i*1.0;
        EclipticToEquatorialV2(GetWTime(),rLambda,rBeta0,rRA,rDEC);
        rRA := rRA * 360/24;
      end;

      if(sType = 'EQUATOR') then
      begin
        rRA := i*1.0;
        rDEC := 0;
      end;

      if(sType = 'DEC') then
      begin
        rRA := i*1.0;
        rDEC := j*10;

        if ((not bPlotLabel) and (i = 180)) then
          bPlotLabel := true;

        if(j=0) then
        begin
          Panel.Canvas.Pen.Color:=clFuchsia;
          Panel.Canvas.Pen.Width:=2;
        end
        else
        begin
          Panel.Canvas.Pen.Color:=clPurple;
          Panel.Canvas.Pen.Width:=1;
        end;
      end;

      if(sType = 'HOR') then
      begin
        iHA_HH:=0; iHA_MM:=0; rHA_SS:=0;
        iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0;

        if not HorizonToEquatorial(j*10.0* Pi/180,i*1.0* Pi/180,rPhi,
          iHA_HH, iHA_MM, rHA_SS,
          iDEC_DEG, iDEC_MM, rDEC_SS) then
          continue;

        iHA_SS := Trunc(rHA_SS);

        rDEC := DEG_MM_SSToDeg(iDEC_DEG,iDEC_MM,rDEC_SS);

      end;

      if(sType = 'HGT') then
      begin
        iHA_HH:=0; iHA_MM:=0; rHA_SS:=0;
        iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0;

        if( not HorizonToEquatorial(i* Pi/180,j*10.0* Pi/180,rPhi,
          iHA_HH, iHA_MM, rHA_SS,
          iDEC_DEG, iDEC_MM, rDEC_SS) ) then
          continue; // Error in Coo-Transformation above. Try next chance.

        iHA_SS := Trunc(rHA_SS);

        rDEC := DEG_MM_SSToDeg(iDEC_DEG,iDEC_MM,rDEC_SS);

      end;

      if(sType = 'GALACTIC') then
      begin
        GetGalacticPlaneCoo(i*1.0,rRA,rDEC);
        rMWDisVal := 1.0 + cos(i*Pi/180);

        if(rMWDisVal > 1.8) then
          Panel.Canvas.Pen.Color:= TColor($00AABBFF)
        else if(rMWDisVal > 1.5) then
          Panel.Canvas.Pen.Color:= TColor($0099AAEE)
        else if(rMWDisVal > 1.3) then
          Panel.Canvas.Pen.Color:= TColor($008899DD)
        else if(rMWDisVal > 1.0) then
          Panel.Canvas.Pen.Color:= TColor($007788CC)
        else if(rMWDisVal > 1.0) then
          Panel.Canvas.Pen.Color:= TColor($006677BB)
        else if(rMWDisVal > 0.7) then
          Panel.Canvas.Pen.Color:= TColor($005566AA)
        else if(rMWDisVal > 0.4) then
          Panel.Canvas.Pen.Color:= TColor($00445599)
        else if(rMWDisVal > 0.1) then
          Panel.Canvas.Pen.Color:= TColor($00334488)
        else
          Panel.Canvas.Pen.Color:= TColor($00223377);

      end;

      if(sType <> 'HOR') and (sType <> 'HGT') then
      begin
        iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
        HoursToHH_MM_SS(rRA *24.0/360.0,iRA_HH,iRA_MM,rRA_SS);

        //DegToDEG_MM_SS(rDEC,iDEC_DEG,iDEC_MM,rDEC_SS,false);
        iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0;
        DegToDEG_MM_SS2(rDEC,iDEC_DEG,iDEC_MM,rDEC_SS);

        dtRA := EncodeTime(iRA_HH, iRA_MM, Trunc(rRA_SS), Trunc(1000*(rRA_SS - Trunc(rRA_SS))));
        dtHA := GetHA(dtST,dtRA);

        DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);
      end;

      rAz:=0; rHgt:=0;
      CalcAZ(iDEC_DEG,iDEC_MM,Trunc(rDEC_SS),iHA_HH,iHA_MM,iHA_SS,rAz,rHgt);

      //if(CanPlotAO(rAz,rHgt-rHgtOffset)) then
      if(CanPlotAO(rAz,rHgt)) then
      begin
        iLeft := 0; iTop := 0;

        if GenMapPos(iR0, iX0, iY0, rAz,rHgt,iLeft,iTop) then
        begin
          if(iCnt = 0) then
          begin
            // Move to the first point
            Panel.Canvas.MoveTo(
              iLeft,iTop
              );
          end
          else
          begin
            Panel.Canvas.LineTo(iLeft,iTop);

            if(bPlotLabel) then
            begin
              //if(sLabel = '0') then
              //   ShowMessage('j: ' + IntToStr(j) + ', i: ' + InttoStr(i));

              if(miStarmapView > 0) then
                Panel.Canvas.TextOut(iLeft,iTop-10,sLabel)
              else
                Panel.Canvas.TextOut(iLeft,iTop,sLabel);

              bPlotLabel := false;
            end;

            (*
            bPlotLine := true;

            if(bPlotLine) then
              Panel.Canvas.LineTo(iLeft,iTop)
            else
              Panel.Canvas.MoveTo(iLeft,iTop);
            *)

            // Show precssion tick marks
            if(sType = 'POLP_N') then
            begin
              if((i mod 30) = 0) then
              begin
                Panel.Canvas.Ellipse(iLeft-2,iTop-2,iLeft+2,iTop+2);
                Panel.Canvas.TextOut(iLeft,iTop,IntToStr(Round(25800.0*(-i + 90.0)/360.0)));
                Panel.Canvas.MoveTo(iLeft,iTop);
              end;
            end;
            if(sType = 'POLP_S') then
            begin
              if((i mod 30) = 0) then
              begin
                Panel.Canvas.Ellipse(iLeft-2,iTop-2,iLeft+2,iTop+2);
                Panel.Canvas.TextOut(iLeft,iTop,IntToStr(Round((-i-90)*25800.0/360.0)));
                Panel.Canvas.MoveTo(iLeft,iTop);
              end;
            end;

          end;

          iXPrev := iLeft;
          //iYPrev := iTop;

          Inc(iCnt);
        end
        else
          iCnt := 0;

      end // CanPlot(..)
      else
        iCnt := 0;

    end; // for i
  end; // for j

  Panel.Canvas.Font.Color:=clRed;

end;

function TF__ASTROTOOLBOX.GetSiderialTimeExt(dtDateTime: TDateTime): TDateTime;
var
  dtJulDat: TDateTime;
begin
  dtJulDat:=0;
  Result := GetSIDTime(dtDateTime,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dtJulDat);
end;

function TF__ASTROTOOLBOX.GetSiderialTime(): TDateTime;
var
  dtWT, dtJulDat: TDateTime;
begin
  dtJulDat:=0;
  dtWT := GetWTime();
  Result := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dtJulDat);
end;

function TF__ASTROTOOLBOX.GetMapCooFromAOExt(olDataList: TObjectList; iIndex: Integer; dtST: TDateTime;
  var rAz: Real; var rHgt: Real;
  var iLeft: Integer; var iTop: Integer
  ): Boolean;
{26.05.2019/fs
Returns Map co-ordinates (Azimuth, Height) and(Left, Top) from an astronomical object.
TRUE, if above horizon, else FALSE.
dtST must be requested once by GetSiderialTime().
}
var
  iRA_HH, iRA_MM, iRA_SS: Integer;
  iDEC_DEG, iDEC_MM, iDEC_SS: Integer;
  iHA_HH,iHA_MM,iHA_SS,iHA_MS: Word;
  dtRA, dtHA: TDateTime;
  iR0,iX0,iY0: Integer;
begin
  iLeft := -1; iTop := -1;
  rAZ := -1; rHgt := -1;

  Result := false;

  if(iIndex < 0) or (iIndex >= olDataList.Count) then
    exit;

  iRA_HH := (olDataList[iIndex] as TAObject).iRA_Hours;
  iRA_MM := (olDataList[iIndex] as TAObject).iRA_Min;
  iRA_SS := Trunc((olDataList[iIndex] as TAObject).rRA_Sec);

  iDEC_DEG := (olDataList[iIndex] as TAObject).iDec_Deg;
  iDEC_MM := (olDataList[iIndex] as TAObject).iDec_Min;
  iDEC_SS := Round((olDataList[iIndex] as TAObject).rDec_Sec);

  if(iRA_SS > 59) then
    iRA_SS := 59;

  dtRA := EncodeTime(iRA_HH, iRA_MM, iRA_SS, Trunc(1000*((olDataList[iIndex] as TAObject).rRA_Sec - iRA_SS)));
  dtHA := GetHA(dtST,dtRA);

  DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);
  CalcAZ(iDEC_DEG,iDEC_MM,iDEC_SS,iHA_HH,iHA_MM,iHA_SS,rAz,rHgt);

  if(CanPlotAO(rAz,rHgt)) then
  begin
    iR0:=0; iX0:=0; iY0:=0;

    StarMapDims(P__STARMAP,iR0,iX0,iY0);
    Result := GenMapPos(iR0, iX0, iY0, rAz,rHgt,iLeft,iTop);
  end;
end;

function TF__ASTROTOOLBOX.GetMapCooFromAO(olDataList: TObjectList; iIndex: Integer; dtST: TDateTime; var iLeft: Integer; var iTop: Integer): Boolean;
{26.05.2019/fs
Returns Map co-ordinates (Left, Top) from an astronomical object.
TRUE, if above horizon, else FALSE.
dtST must be requested by GetSiderialTime().
}
var
  rAz, rHgt: Real;
begin
  rAz:=0; rHgt:=0; iLeft:=0; iTop:=0;
  Result := GetMapCooFromAOExt(olDataList, iIndex,dtST,rAz,rHgt,iLeft,iTop);
end;

procedure TF__ASTROTOOLBOX.GenSigns(Panel: TPanel; iR0, iX0, iY0, iSkyType: Integer);
{12.2013/fs
Generate Scale lines and Constallation Figures
}
var
  i: Integer;
  (*
  iRA_HH, iRA_MM, iRA_SS: Integer;
  iDEC_DEG, iDEC_MM, iDEC_SS: Integer;
  iHA_HH,iHA_MM,iHA_SS,iHA_MS: Word;
  rAz,rHgt,rR: Real;
  rDX, rDY, rDX_Partner, rDY_Partner: Real;
  dJulDat, dtST, dtRA, dtHA: TDateTime;
  *)
  rAz,rHgt: Real;
  dtST: TDateTime;
  iIndex, iPos, iLen: Integer;
  sConPartnerList, sConPartner: string;
  j: Integer;
  slConPartnerList: TStringList;
  //iR: Integer;
  rAngle: Real;
  Sign: TSign;
  iSignIndex: Integer;
  iXOffset: Integer;
  iLeft, iTop, iLeft_Partner, iTop_Partner: Integer;
  bgColor: TColor;
  bFound: Boolean;
begin
  slConPartnerList := TStringList.Create;

  case iSkyType of
    0: bgColor := clSummerDarkSky;
    1: bgColor := clNavy;
    2: bgColor := clDaySky;
    else bgColor := clBlack;
  end; // case

  if(
    (not mbZoomMode) and (miShowMilkyway <> 0) and
    ((iSkyType < 1) or (miShowMilkyWay = -1))
    ) then
  begin
    GenMilkyway(iSkyType);
    PlotRing(Panel,bgColor,iR0,iX0,iY0,100);
  end;

  Panel.Canvas.Pen.Color := clRed;
  Panel.Canvas.Pen.Width:=miLThickness;
  Panel.Canvas.Brush.Color:=clBlue;
  Panel.Canvas.Brush.Style:=bsClear;
  Panel.Canvas.Font.Color := clRed;

  if(miStarmapView = 0) and (not mbZoomMode) then
  begin
    Panel.Canvas.Pen.Width:=3;
    Panel.Canvas.EllipseC(iX0,iY0,iY0+2,iY0+2);
    Panel.Canvas.Font.Size := 10;

    // Marks around the map
    for i:=0 to 359 do
    begin
      rAngle := i*1 *Pi/180;

      if((i mod 10) = 0) then
      begin
        Panel.Canvas.Pen.Color:=clRed;
        Panel.Canvas.Pen.Width:=3;
        Panel.Canvas.Line(Round(iX0+iY0*sin(rAngle)),Round(iY0+iY0*cos(rAngle)),
          Round(iX0+(iY0+9)*sin(rAngle)),Round(iY0+(iY0+9)*cos(rAngle)));

        if(rAngle < Pi) then
          iXOffset := 8
        else
          iXOffset := -10;

        Panel.Canvas.TextOut(Round(iX0-10+(iY0+15)*sin(rAngle)) + iXOffset,Round(iY0-10+(iY0+15)*cos(rAngle)),IntToStr(i));
      end
      else if((i mod 5) = 0) then
      begin
        Panel.Canvas.Pen.Color:=clRed;
        Panel.Canvas.Pen.Width:=2;

        Panel.Canvas.Line(Round(iX0+iY0*sin(rAngle)),Round(iY0+iY0*cos(rAngle)),
          Round(iX0+(iY0+7)*sin(rAngle)),Round(iY0+(iY0+7)*cos(rAngle)));
      end
      else if((i mod 2) = 0) then
      begin
        Panel.Canvas.Pen.Color:=clBlue;
        Panel.Canvas.Pen.Width:=2;

        Panel.Canvas.Line(Round(iX0+iY0*sin(rAngle)),Round(iY0+iY0*cos(rAngle)),
          Round(iX0+(iY0+6)*sin(rAngle)),Round(iY0+(iY0+6)*cos(rAngle)));
      end;

    end;

    Panel.Canvas.Font.Size := 14;

    Panel.Canvas.TextOut(iX0+iY0+35,iY0,'W');

    if(msLANG_ID = 'DE') then
      Panel.Canvas.TextOut(iX0-iY0-50,iY0,'O');
    if(msLANG_ID = 'EN') then
      Panel.Canvas.TextOut(iX0-iY0-50,iY0,'E');

    Panel.Canvas.Font.Color := clRed;
    Panel.Canvas.Font.Size := 12;

    // Print Zenith Cross
    Panel.Canvas.Line(iX0-5,iY0,iX0+5,iY0);
    Panel.Canvas.Line(iX0,iY0-5,iX0,iY0+5);
    Panel.Canvas.TextOut(iX0,iY0,'Z');

  end;

  if(not mbSM_CON) or (mbZoomMode and ( (mrZoomHgtMax - mrZoomHgtMin < 5) or (mrZoomAzMax - mrZoomAzMin < 5) ) ) then
    exit;

  rAz:=0; rHgt:=0;
  (*
  iRA_HH:=0; iRA_MM:=0; iRA_SS:=0;
  iDEC_DEG:=0; iDEC_MM:=0; iDEC_SS:=0;
  iHA_HH:=0;iHA_MM:=0;iHA_SS:=0;iHA_MS:=0;

  dJulDat := 0;
  *)

  dtST := GetSiderialTime();

  bFound := false;

  for i:=0 to molAOList.Count-1 do
  begin
    if((molAOList[i] as TAObject).sAOType <> 'S') then
    begin
      if(bFound) then
      begin
        break;
        ShowMessage('BREAK: Index: ' + IntToStr(i) + ', AOType: ' + (molAOList[i] as TAObject).sAOType);
      end
      else continue;
    end
    else
      bFound := true;
    //      break; // Leave for-loop!

    // Continue with stars
    if((molAOList[i] as TAObject).rM >= 5.5) then
    begin
      //ShowMessage('Index: ' + IntToStr(i) + ', AOType: ' + (molAOList[i] as TAObject).sAOType);
      continue; // Next for item...
    end;

    rAz:=0; rHgt:=0; iLeft:=0; iTop:=0;
    if(GetMapCooFromAOExt(molAOList, i,dtST,rAz,rHgt,iLeft,iTop)) then
    begin
      // Find the sign partner and the co-ordinates
      sConPartnerList := Trim((molAOList[i] as TStar).sConPartner);

      if(length(sConPartnerList) > 0) then
      begin
        // Constellation nodes
        slConPartnerList.Delimiter := '|';
        slConPartnerList.DelimitedText:=sConPartnerList;

        for j:=0 to slConPartnerList.Count-1 do
        begin
          sConPartner := slConPartnerList[j];

          if(Trim(sConPartner) = '-') then
            continue;

          iPos := Pos('/',sConPartner);
          // Check, if a FOREIGN sign is addressed
          // Example: alpha/And (Sirrah) in Pegasus / Andromeda intersection
          if(iPos > 0) then
          begin
            iLen := length(sConPartner);
            iIndex := GetStarIndex(
              LeftStr(sConPartner,iPos-1), // Star Symbol in foreign sign
              RightStr(sConPartner,iLen-iPos) // Foreign sign id
              );

          end
          else
          begin
            // Star symbol is in the same sign
            // Accelerate for next index searches!
            if(j <= 5) then
            begin
              iIndex := (molAOList[i] as TStar).iaConIndex[j];
              if(iIndex = -1) then
              begin
                iIndex := GetStarIndex(sConPartner,(molAOList[i] as TStar).sCon);
                if(iIndex > -1) then
                  (molAOList[i] as TStar).iaConIndex[j] := iIndex;
              end;
            end
            else
              iIndex := GetStarIndex(sConPartner,(molAOList[i] as TStar).sCon);

          end;

          if(iIndex > -1) then
          begin
            // Partner found. Draw sign shape!
            iLeft_Partner:=0; iTop_Partner:=0;
            if( not GetMapCooFromAOExt(molAOList, iIndex,dtST,rAz,rHgt,iLeft_Partner,iTop_Partner)) then
              continue;

            iSignIndex:=0;
            Sign := GetSignObj(Trim((molAOList[i] as TStar).sCon),molSignList,iSignIndex);

            if(Sign <> nil) and (Sign.bSelected) then
              Panel.Canvas.Pen.Color := clYellow
            else
              Panel.Canvas.Pen.Color := clRed;

            Panel.Canvas.Brush.Color:=clSkyBlue;

            if(not mbZoomMode) then
            begin
              Panel.Canvas.Line(
                iLeft,iTop,
                iLeft_Partner,iTop_Partner
               )
            end
            else
            begin
              if(CanPlotAO(rAz,rHgt)) then
              begin
                Panel.Canvas.Line(
                  iLeft,iTop,
                  iLeft_Partner,iTop_Partner
                  );
              end;
            end;
          end;
        end;

      end;
    end
    else
    begin
      if((molAOList[i] as TAObject).SHP <> nil) then
        (molAOList[i] as TAObject).SHP.Brush.Color:=clBlack;
    end;

  end; // for

  slConPartnerList.Destroy;
end;

procedure TF__ASTROTOOLBOX.SetNow();
begin
  SetTime(Now);
  mdtStart0 := Now;//GetWTime();
  mdtStart := Now;
end;

procedure TF__ASTROTOOLBOX.ShowTimeStat();
var
  dtTimeMR: TDateTime;
  dtTimeMS: TDateTime;
  dtTimeCul: TDateTime;
  rHgt_Max: Real;
  dtWT, dtWTNew: TDateTime;
begin
  //CalcMoonRiseAndMoonSet(dtTimeMR,dtTimeMS,dtTimeCul,rHgt_Max);
  dtWT := GetWTime();
  dtTimeMR:=0; dtTimeMS:=0; dtTimeCul:=0; rHgt_Max:=0;

  CalcMoonRiseAndMoonSet(dtWT,
    miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,
    mrSin_fGLat, mrCos_fGLat,
    dtTimeMR,dtTimeMS,dtTimeCul,rHgt_Max);

  F__TIMESTAT := TF__TIMESTAT.Create(nil);

  F__TIMESTAT.msLANG_ID:=msLANG_ID;

  F__TIMESTAT.mdtWTime:=GetWTime();
  F__TIMESTAT.mrRise_HH   := mrRise_HH;
  F__TIMESTAT.mrDwn1_HH   := mrDwn_AM_HH_A;
  F__TIMESTAT.mrDwn1_HH_N := mrDwn_AM_HH_N;
  F__TIMESTAT.mrDwn1_HH_C := mrDwn_AM_HH_C;

  F__TIMESTAT.mrSet_HH    := mrSet_HH;
  F__TIMESTAT.mrDwn2_HH   := mrDwn_PM_HH_A;
  F__TIMESTAT.mrDwn2_HH_N := mrDwn_PM_HH_N;
  F__TIMESTAT.mrDwn2_HH_C := mrDwn_PM_HH_C;

  F__TIMESTAT.mdtTimeMR := dtTimeMR;
  F__TIMESTAT.mdtTimeMS := dtTimeMS;
  F__TIMESTAT.mdtTimeCul := dtTimeCul;
  F__TIMESTAT.mrHgt_Max := rHgt_Max;

  F__TIMESTAT.miDST_HH := miDST_HH;
  F__TIMESTAT.miDST_HH_DEF := miDST_HH_DEF;
  F__TIMESTAT.msDST := msDST;
  F__TIMESTAT.miUTC_HH := miUTC_HH;
  F__TIMESTAT.miGLng_DEG := miGLng_DEG;
  F__TIMESTAT.miGLng_MIN := miGLng_MIN;
  F__TIMESTAT.mrSin_fGLat := mrSin_fGLat;
  F__TIMESTAT.mrCos_fGLat := mrCos_fGLat;
  F__TIMESTAT.miGLat_DEG := miGLat_DEG;
  F__TIMESTAT.miGLat_MIN := miGLat_MIN;

  F__TIMESTAT.msAlbireoLocalDir:=gsAlbireoLocalDir;

  IniText(F__TIMESTAT,msLANG_ID);

  if(msLANG_ID = 'DE') then
    F__TIMESTAT.Caption := 'Sonne und Mond'
  else
    F__TIMESTAT.Caption := 'Sun and Moon';

  F__TIMESTAT.msLANG_ID := msLANG_ID;

  F__TIMESTAT.PrepareMoonEclipses(mslEclipses);
  F__TIMESTAT.PrepareSunEclipses(mslEclipses);

  F__TIMESTAT.ShowModal;

  dtWTNew := F__TIMESTAT.mdtWTimeNew;

  F__TIMESTAT.Destroy;

  if(dtWTNew > 0) then
  begin
    if(mbTimePlay) then
      SwitchTimePLAY();

    SetTime(dtWTNew);
    PC__WORKBENCH.ActivePageIndex := ciPAGE_STARMAP;
  end;

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
    CleanStartOfStarMap();

end;

procedure TF__ASTROTOOLBOX.SetMoonPanel;
var
  iMP: Integer;
begin
  iMP := CalcMoonPhase(CB__WT.Date);
  //ShowMessage(IntToStr(iMP));

  //if((iMP > 70) and (iMP < 80)) or ((iMP > 20) and (iMP < 30)) then
  if((iMP >= 70) and (iMP <= 100)) or ((iMP >= 0) and (iMP <= 30)) then
    SHP__MC.Shape := stRectangle // Half Moon
  else
    SHP__MC.Shape := stCircle;

  SHP__MC.Left := 42 - Round(2*iMP*21.0/100.0);
end;

procedure TF__ASTROTOOLBOX.SetTimePanels();
var
  iDay: Integer;
  iXOffset, iXLen: Integer;
  //iXPosL, iXPosR: Integer;
  bAMBreak, bPMBreak: Boolean;
  rDiffDST: Real;
begin
  if(miGLng_DEG = 999) and (miGLat_DEG = 999) then
    exit;

  iXOffset := 0;

  bAMBreak := false;
  bPMBreak := false;

  iXLen := P__DAYLIGHT.Width - iXOffset;

  iDay := DayOfYear(CB__WT.Date);
  miDST_HH := GetDST(CB__WT.Date,msDST,miDST_HH_DEF); // Adjust Daylight saving time

  // Visualize DST active
  if(miDST_HH > 0) then
  begin
    L__DST.Visible := true;
  end
  else
  begin
    L__DST.Visible := false;
  end;

  // Sunrise and Sunset: -50 Minutes
  CalcSunRiseAndSet(
    miGLng_DEG, miGLng_MIN,
    miGLat_DEG, miGLat_MIN,
    miUTC_HH,
    iDay,miDST_HH,-50,mrRise_HH, mrSet_HH);

  // Dawn: Time between Start of Dawn: 0 Degree and Rise/Set at 18 Degree

  // Civil: -6 Degree
  CalcSunRiseAndSet(
    miGLng_DEG, miGLng_MIN,
    miGLat_DEG, miGLat_MIN,
    miUTC_HH,
    iDay,miDST_HH,-6*60,mrDwn_AM_HH_C, mrDwn_PM_HH_C);

  // Nautical: -12 Degree
  CalcSunRiseAndSet(
    miGLng_DEG, miGLng_MIN,
    miGLat_DEG, miGLat_MIN,
    miUTC_HH,
    iDay,miDST_HH,-12*60,mrDwn_AM_HH_N, mrDwn_PM_HH_N);

  // Astronomical: -18 Degree
  CalcSunRiseAndSet(
    miGLng_DEG, miGLng_MIN,
    miGLat_DEG, miGLat_MIN,
    miUTC_HH,
    iDay,miDST_HH,-18*60,mrDwn_AM_HH_A, mrDwn_PM_HH_A);
  (*
  MM__MW.Clear;
  MM__MW.Lines.Add('mrRise_HH: ' + FloatToStr(mrRise_HH));
  MM__MW.Lines.Add('mrSet_HH: ' + FloatToStr(mrSet_HH));
  MM__MW.Lines.Add('');
  MM__MW.Lines.Add('mrDwn_AM_HH_C: ' + FloatToStr(mrDwn_AM_HH_C));
  MM__MW.Lines.Add('mrDwn_PM_HH_C: ' + FloatToStr(mrDwn_PM_HH_C));
  MM__MW.Lines.Add('');
  MM__MW.Lines.Add('mrDwn_AM_HH_N: ' + FloatToStr(mrDwn_AM_HH_N));
  MM__MW.Lines.Add('mrDwn_PM_HH_N: ' + FloatToStr(mrDwn_PM_HH_N));
  MM__MW.Lines.Add('');
  MM__MW.Lines.Add('mrDwn_AM_HH_A: ' + FloatToStr(mrDwn_AM_HH_A));
  MM__MW.Lines.Add('mrDwn_PM_HH_A: ' + FloatToStr(mrDwn_PM_HH_A));
  *)
  P__DAWN_AM_C.Visible := false;
  P__DAWN_AM_N.Visible:=false;
  P__DAWN_AM_A.Visible:=false;
  P__NIGHT_AM.Visible:=false;

  P__DAWN_PM_C.Visible := false;
  P__DAWN_PM_N.Visible:=false;
  P__DAWN_PM_A.Visible:=false;
  P__NIGHT_PM.Visible:=false;

  P__DAWN_AM_PREV_PM.Visible:= false;

  // I) BEFORE SUNRISE              <--|SUNRISE
  // Find TW status from the day before
  if(mrRise_HH > 0) then
  begin
    if(mrDwn_AM_HH_C > 0) then
    begin
      P__DAWN_AM_C.Width := Round(iXLen*mrRise_HH/24.0 - iXLen*mrDwn_AM_HH_C/24.0);
      P__DAWN_AM_C.Left := Round(iXLen*mrDwn_AM_HH_C/24.0);
    end
    else
    begin
      P__DAWN_AM_C.Width := Round(iXLen*mrRise_HH/24.0);
      P__DAWN_AM_C.Left := 0;
      bAMBreak := true;
    end;
    P__DAWN_AM_C.Visible:=true;

    if(not bAMBreak)then
    begin
      if(mrDwn_AM_HH_N > 0) then
      begin
        P__DAWN_AM_N.Width := Round(iXLen*mrDwn_AM_HH_C/24.0 - iXLen*mrDwn_AM_HH_N/24.0);
        P__DAWN_AM_N.Left := Round(iXLen*mrDwn_AM_HH_N/24.0);
      end
      else
      begin
        P__DAWN_AM_N.Width := Round(iXLen*mrDwn_AM_HH_C/24.0);
        P__DAWN_AM_N.Left := 0;
        bAMBreak := true;
      end;
      P__DAWN_AM_N.Visible:=true;
    end;

    if(not bAMBreak)then
    begin
      P__DAWN_AM_A.Visible:=false;
      if(mrDwn_AM_HH_A > 0) then
      begin
        P__DAWN_AM_A.Width := Round(iXLen*mrDwn_AM_HH_N/24.0 - iXLen*mrDwn_AM_HH_A/24.0);
        P__DAWN_AM_A.Left := Round(iXLen*mrDwn_AM_HH_A/24.0);
      end
      else
      begin
        P__DAWN_AM_A.Width := Round(iXLen*mrDwn_AM_HH_N/24.0);
        P__DAWN_AM_A.Left := 0;
        bAMBreak := true;
      end;
      P__DAWN_AM_A.Visible:=true;
    end;

    if(not bAMBreak)then
    begin
      P__NIGHT_AM.Visible:=false;
      P__NIGHT_AM.Width := Round(iXLen*mrDwn_AM_HH_A/24.0);
      P__NIGHT_AM.Left := 0;
      P__NIGHT_AM.Visible:=true;
    end;

  end; // Do nothing for AM panels (all invisible) if no sunrise is defined: Sun stays / under horizon (??) whole day

  // II) AFTER SUNSET                SUNSET|-->
  // Civil Panel settings
  if(mrSet_HH > 0) then
  begin
    // OK. Sunset occurs:
    P__DAWN_PM_C.Left := Round(iXLen*mrSet_HH/24.0);
    if(mrDwn_PM_HH_C > 0) then
      P__DAWN_PM_C.Width := Round(iXLen*mrDwn_PM_HH_C/24.0 - iXLen*mrSet_HH/24.0)
    else
      P__DAWN_PM_C.Width := Round(iXLen - iXLen*mrSet_HH/24.0); // No end of civil tw: Until Midnight!

    P__DAWN_PM_C.Visible := true;
    //P__DAWN_PM_C.Hint := 'Bürgerliche Dämmerung ' + ;
  end
  else
    bPMBreak := true; // No sunset at all this day!!!

  if(not bPMBreak) then
  begin
    if(mrDwn_PM_HH_C > -1) then
    begin
      // OK. Civil TW ends before MN!
      P__DAWN_PM_N.Left := Round(iXLen*mrDwn_PM_HH_C/24.0);
      if(mrDwn_PM_HH_N > 0) then
        P__DAWN_PM_N.Width := Round(iXLen*mrDwn_PM_HH_N/24.0 - iXLen*mrDwn_PM_HH_C/24.0)
      else
        P__DAWN_PM_N.Width := Round(iXLen - iXLen*mrDwn_PM_HH_C/24.0); // No end of civil tw: Until Midnight!

      P__DAWN_PM_N.Visible := true;
    end
    else
      bPMBreak := true;

  end;

  if(not bPMBreak) then
  begin
    if(mrDwn_PM_HH_N > -1) then
    begin
      // OK. Nautical TW ends before MN!
      P__DAWN_PM_A.Left := Round(iXLen*mrDwn_PM_HH_N/24.0);
      if(mrDwn_PM_HH_A > 0) then
        P__DAWN_PM_A.Width := Round(iXLen*mrDwn_PM_HH_A/24.0 - iXLen*mrDwn_PM_HH_N/24.0)
      else
        P__DAWN_PM_A.Width := Round(iXLen - iXLen*mrDwn_PM_HH_N/24.0); // No end of nautical tw: Until Midnight!

      P__DAWN_PM_A.Visible := true;
    end
    else
      bPMBreak := true;

  end;

  if(not bPMBreak) then
  begin
    if(mrDwn_PM_HH_A > -1) then
    begin
      // OK. Astronomical TW ends before MN!
      P__NIGHT_PM.Left := Round(iXLen*mrDwn_PM_HH_A/24.0);
      P__NIGHT_PM.Width := Round(iXLen - iXLen*mrDwn_PM_HH_A/24.0);

      P__NIGHT_PM.Visible := true;
    end
    else
      bPMBreak := true;

  end;

  if(miDST_HH > 0) then
  begin
    rDiffDST := 0;
    if(mrDwn_PM_HH_C > 24) then
    begin
      rDiffDST :=  mrDwn_PM_HH_C - 24;
      P__DAWN_AM_PREV_PM.Color := P__DAWN_AM_C.Color;
      P__DAWN_AM_PREV_PM.Caption := P__DAWN_AM_C.Caption;
    end
    else if(mrDwn_PM_HH_N > 24) then
    begin
      rDiffDST :=  mrDwn_PM_HH_N - 24;
      P__DAWN_AM_PREV_PM.Color := P__DAWN_AM_N.Color;
      P__DAWN_AM_PREV_PM.Caption := P__DAWN_AM_N.Caption;
    end
    else if(mrDwn_PM_HH_A > 24) then
    begin
      rDiffDST :=  mrDwn_PM_HH_A - 24;
      P__DAWN_AM_PREV_PM.Color := P__DAWN_AM_A.Color;
      P__DAWN_AM_PREV_PM.Caption := P__DAWN_AM_A.Caption;
    end;

    if(rDiffDST > 0) then
    begin
      P__DAWN_AM_PREV_PM.Width := Round(rDiffDST*iXLen/24.0);
      //MM__MW.Lines.Add('P__DAWN_AM_PREV_PM.Width: ' + IntToStr(P__DAWN_AM_PREV_PM.Width));
      P__DAWN_AM_PREV_PM.left := 0;
      P__DAWN_AM_PREV_PM.visible := true;
    end;

   (*  ODER
    if(mrDwn_AM_HH_C > 1) and (mrDwn_AM_HH_C < 2) then
    begin
      rDiffDST :=  mrDwn_AM_HH_C - 1;
      P__DAWN_AM_PREV_PM.Color := P__DAWN_AM_C.Color;
      P__DAWN_AM_PREV_PM.Width := Round((1.0-rDiffDST)*iXLen/24.0);
      P__DAWN_AM_PREV_PM.left := 0;
      P__DAWN_AM_PREV_PM.visible := true;
    end;

    if(mrDwn_AM_HH_N > 1) and (mrDwn_AM_HH_N < 2) then
    begin
      rDiffDST :=  mrDwn_AM_HH_N - 1;
      P__DAWN_AM_PREV_PM.Color := P__DAWN_AM_N.Color;
      P__DAWN_AM_PREV_PM.Width := Round((1.0-rDiffDST)*iXLen/24.0);
      P__DAWN_AM_PREV_PM.left := 0;
      P__DAWN_AM_PREV_PM.visible := true;
    end;

    if(mrDwn_AM_HH_A > 1) and (mrDwn_AM_HH_A < 2) then
    begin
      rDiffDST :=  mrDwn_AM_HH_A - 1;
      P__DAWN_AM_PREV_PM.Color := P__DAWN_AM_A.Color;
      P__DAWN_AM_PREV_PM.Width := Round((1.0-rDiffDST)*iXLen/24.0);
      P__DAWN_AM_PREV_PM.left := 0;
      P__DAWN_AM_PREV_PM.visible := true;
    end;
   *)

  end;

end;

procedure TF__ASTROTOOLBOX.ShowAOTable();
var
  iColType: Integer;
begin
   try
    Screen.Cursor:=crHourGlass;

    mslColType.Clear;

    P__SELMAG.Visible := false;

    if(RB__S.Checked) then
      ShowTable_S()
    else if(RB__G.Checked) then
      ShowTable_G()
    else if(RB__GC.Checked) then
      ShowTable_GC()
    else if(RB__N.Checked) then
      ShowTable_N()
    else if(RB__PN.Checked) then
      ShowTable_PN()
    else if(RB__P.Checked) then
      ShowTable_P()
    else if(RB__OC.Checked) then
      ShowTable_OC()
    else if(RB__Q.Checked) then
      ShowTable_Q()
    else if(RB__COMETS.Checked) then
      ShowTable_C()
    else if(RB__MESSIER.Checked) then
      ShowTable_Messier();

    if(miSCI_GRD_AO > -1) and (miSCI_GRD_AO < GRD__AO.ColCount) and (miSCO_GRD_AO > -1) then
    begin
      iColType := StrToInt(mslColType[miSCI_GRD_AO]);
      SortStringGrid(GRD__AO,miSCI_GRD_AO,iColType,true,(miSCO_GRD_AO = 1));
    end;

  finally
    miSCI_GRD_AO := 0;
    miSCO_GRD_AO := -1;

    Screen.Cursor:=crDefault;
  end;
end;

function TF__ASTROTOOLBOX.GetVisualHeight(dtTime, dtRA: TDateTime; rDEC: Real): Real;
var
  dtST, dtHA: TDateTime;
  fGLat, fHA: Real;
  dJulDat: Double;
begin
  fGLat := miGLat_DEG + miGLat_MIN/60.0;
  fGLat := Pi*fGLat/180.0;

  rDec := rDec*Pi/180.0;

  dJulDat := 0;

  dtST := GetSIDTime(dtTime,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat);

  dtHA := GetHA(dtST,dtRA);

  fHA := dtHA*2*PI;

  Result := 180*arcsin(cos(rDEC)*cos(fHA)*cos(fGLat) + sin(rDEC)* sin(fGLat))/Pi;

end;

procedure TF__ASTROTOOLBOX.CalcAZ(iDEC_DEG, iDEC_MIN, iDEC_SS, iHA_HH, iHA_MIN, iHA_SS: SmallInt;
  var fAz: Real; var fHgt: Real);
var
  rSin_fGLat: Real;
  rCos_fGLat: Real;
begin

  rSin_fGLat := mrSin_fGLat; rCos_fGLat := mrCos_fGLat;

  CalcAZCoo(iDEC_DEG, iDEC_MIN, iDEC_SS, iHA_HH, iHA_MIN, iHA_SS,
    rSin_fGLat, rCos_fGLat,
    fAz, fHgt);

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_HA) then
  begin
    RB1.Checked := false;
    RB2.Checked := false;
    RB3.Checked := false;
    RB4.Checked := false;
    RB5.Checked := false;
    RB6.Checked := false;
    RB7.Checked := false;
    RB8.Checked := false;

    RB5.Checked := (fAz >= 270+22.5) or (fAz <= 45-22.5);
    RB6.Checked := (fAz > 45-22.5) and (fAz <= 90-22.5);
    RB7.Checked := (fAz > 90-22.5) and (fAz <= 90+22.5);
    RB8.Checked := (fAz > 90+22.5) and (fAz <= 180-22.5);
    RB1.Checked := (fAz > 180-22.5) and (fAz <= 180+22.5);
    RB2.Checked := (fAz > 180+22.5) and (fAz <= 270-22.5);
    RB3.Checked := (fAz > 270-22.5) and (fAz <= 270+22.5);
    RB4.Checked := (fAz > 270+22.5) and (fAz <= 360-22.5);

    if(fHgt > 0) then
    begin
      P__VISIBLE.Font.Color := clWhite;

      if(msLANG_ID = 'DE') then
        P__VISIBLE.Caption := 'Über Horizont'
      else
        P__VISIBLE.Caption := 'Above Horizon';

      L__HGT_HH_MINUS.Visible := false;
    end
    else
    begin
      P__VISIBLE.Font.Color := clGray;

      if(msLANG_ID = 'DE') then
        P__VISIBLE.Caption := 'Unter Horizont'
      else
        P__VISIBLE.Caption := 'Below Horizon';

      L__HGT_HH_MINUS.Visible := true;
    end;
  end;

end;

procedure TF__ASTROTOOLBOX.ValuateVisibility(AObject: TAObject; rHgt: Real; iRow: Integer; dtTime: TDateTime; iCol: Integer);
var
  rHour, rDECVal: Real;
  bIsMatchingObject: Boolean;
begin
  rHour := (dtTime - Trunc(dtTime))*24;

  //if(AObject.sName_DE = 'SCP') then
  //  ShowMessage('STOP: ' + FloatToStr((miGLat_DEG + miGLat_MIN/60.0) - 90.0));

  if(rHgt > 0) then
  begin
    if(rHour >= mrRise_HH) and (rHour <= mrSet_HH) then
    begin
      if(msLANG_ID = 'DE') then
        GRD__AO.Cells[iCol,iRow] := 'Tageshimmel'
      else
        GRD__AO.Cells[iCol,iRow] := 'Day Sky';
    end
    else
    begin
      if(rHgt > 30) then
      begin
        if(msLANG_ID = 'DE') then
          GRD__AO.Cells[iCol,iRow] := 'Gut sichtbar'
        else
          GRD__AO.Cells[iCol,iRow] := 'Conspicuous';
      end
      else
      begin
        if(rHgt > 10) then
        begin
          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[iCol,iRow] := 'Sichtbar'
          else
            GRD__AO.Cells[iCol,iRow] := 'Visible';

        end
        else
        begin
          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[iCol,iRow] := 'Horizontnähe'
          else
            GRD__AO.Cells[iCol,iRow] := 'Near Horizon';
        end;
      end;
    end;
  end
  else
  begin
    rDECVal := AObject.iDec_Deg + AObject.iDec_Min/60.0;

    bIsMatchingObject := (AObject.sAOType = 'S') or (AObject.sAOType = 'G') or (AObject.sAOType = 'Q') or
      (AObject.sAOType = 'GC') or (AObject.sAOType = 'OC') or (AObject.sAOType = 'N') or (AObject.sAOType = 'PN');

    if((bIsMatchingObject) and (rDECVal < (miGLat_DEG + miGLat_MIN/60.0) - 90.0)) then
    begin
      if(msLANG_ID = 'DE') then
        GRD__AO.Cells[iCol,iRow] := 'Niemals sichtbar'
      else
        GRD__AO.Cells[iCol,iRow] := 'Never visible';

    end
    else
    begin
      if(msLANG_ID = 'DE') then
        GRD__AO.Cells[iCol,iRow] := 'Unter Horizont'
      else
        GRD__AO.Cells[iCol,iRow] := 'Under Horizon';
    end;

  end;
end;

procedure TF__ASTROTOOLBOX.ShowTable_C();
{2012-11-11/fs
Show dynamic Comet Table
}
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  iRA_HH,iRA_MM: Word;
  rRA_SS: Real;
  iDEC_DEG,iDEC_MM: SmallInt;
  rDEC_SS,rRs,rRho: Real;
begin
  dtTime := GetWTime(); // Displayed time

  SetAOTableTitle(RB__COMETS.Caption);

  iRA_HH := 0; iRA_MM := 0; rRA_SS := 0;
  iDEC_DEG := 0; iDEC_MM := 0; rDEC_SS := 0;

  mslColType.Clear;

  // Grid initializing
  GRD__AO.ColCount:=13;  GRD__AO.ColWidths[0]:=0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := '';
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[10,1] := '';
  GRD__AO.Cells[11,0] := 'RA';
  GRD__AO.Cells[11,1] := '';
  GRD__AO.Cells[12,0] := 'DEC';
  GRD__AO.Cells[12,1] := '';

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');

      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Komet'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Entfernung Erde [AE]'; mslColType.Add('1');
        GRD__AO.Cells[4,0] := 'Entfernung Sonne [AE]'; mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'Perihel-Epoche'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Trop. Jahr [d]'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Exentrizität'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Inklination'; mslColType.Add('1');
        GRD__AO.Cells[9,0] := 'Halbachse des Orbits [AU]'; mslColType.Add('1');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Comet'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Distance Earth [AU]'; mslColType.Add('1');
        GRD__AO.Cells[4,0] := 'Distance Sun [AU]'; mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'Epoch of Perihelion'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Trop. Year [d]'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Eccentricity'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Inclination'; mslColType.Add('1');
        GRD__AO.Cells[9,0] := 'Semi-major axis of the orbit [AU]'; mslColType.Add('1');
      end;

       mslColType.Add('0'); // 10
       mslColType.Add('0'); // 11
       mslColType.Add('2'); // 12
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'C') then
    begin
     rRs:=0; rRho:=0;

     GetCometCoo(dtTime,(molAOList[i] as TComet),
       miDST_HH,miUTC_HH,
       iRA_HH,iRA_MM,rRA_SS,
       iDEC_DEG,iDEC_MM,rDEC_SS,rRs,rRho);

     if(iDEC_DEG >= 270) then iDEC_DEG := iDEC_DEG - 360;

     dtRA := EncodeTime(
       iRA_HH,
       iRA_MM,
       Trunc(rRA_SS),
       Round(1000*(rRA_SS - Trunc(rRA_SS)))
       );

     rDEC := iDec_DEG +
       iDec_MM/60.0 +
       rDec_SS/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      if((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked))) then
      begin
        iRow := GRD__AO.RowCount-1;

        GRD__AO.Cells[0,iRow] := IntToStr(i);
        GRD__AO.Cells[1,iRow] := IntToStr(iRow);

        GRD__AO.Cells[11,iRow] := format('%.2d',[Trunc(iRA_HH)]) + ':' + format('%.2d',[Trunc(iRA_MM)]) + ':' + FloatToStrF(rRA_SS,ffFixed,8,2);
        GRD__AO.Cells[12,iRow] := format('%.2d',[Trunc(iDEC_DEG)]) + ':' + format('%.2d',[Trunc(iDEC_MM)]) + ':' + FloatToStrF(rDEC_SS,ffFixed,8,2);

        if(msLANG_ID = 'DE') then
        begin
          GRD__AO.Cells[2,iRow] := (molAOList[i] as TAObject).sName_DE;
        end;

        if(msLANG_ID = 'EN') then
        begin
          GRD__AO.Cells[2,iRow] := (molAOList[i] as TAObject).sName_EN;
        end;

        GRD__AO.Cells[3,iRow] := FloatToStrF((molAOList[i] as TComet).rRho,ffFixed,8,3);
        GRD__AO.Cells[4,iRow] := FloatToStrF((molAOList[i] as TComet).rRs,ffFixed,8,3);
        GRD__AO.Cells[5,iRow] := FloatToStrF((molAOList[i] as TComet).rP,ffFixed,8,3);
        GRD__AO.Cells[6,iRow] := FloatToStrF((molAOList[i] as TComet).rTp,ffFixed,8,3);
        GRD__AO.Cells[7,iRow] := FloatToStrF((molAOList[i] as TComet).rE,ffFixed,9,5);
        GRD__AO.Cells[8,iRow] := FloatToStrF((molAOList[i] as TComet).rI,ffFixed,8,3);
        GRD__AO.Cells[9,iRow] := FloatToStrF((molAOList[i] as TComet).rA,ffFixed,8,3);

        ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,10);

        GRD__AO.RowCount:=GRD__AO.RowCount+1;
      end;
    end;
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.ShowTable_P;
{2012-11-11/fs
Show dynamic Planet Table
}
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  bOverHorizon: Boolean;
  rHgt: Real;
  dtTime: TDateTime;
  iRA_HH,iRA_MM: Word;
  rRA_SS: Real;
  iDEC_DEG,iDEC_MM: SmallInt;
  rDEC_SS: Real;
  rR: Real;
begin
  dtTime := GetWTime(); // Displayed time

  iRA_HH := 0; iRA_MM := 0; rRA_SS := 0;
  iDEC_DEG := 0; iDEC_MM := 0; rDEC_SS := 0; rR:=0;

  SetAOTableTitle(RB__P.Caption);

  mslColType.Clear;

  // Grid initializing
  GRD__AO.ColCount:=16; GRD__AO.ColWidths[0]:=0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,0] := '';
  GRD__AO.Cells[7,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,0] := 'RA';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := 'DEC';
  GRD__AO.Cells[9,1] := '';

  // Oppositions and Conjunctions of outer planets
  if(msLANG_ID = 'DE') then
  begin
   GRD__AO.Cells[10,0] := 'Opp-' + IntToStr(YearOf(Now)-1);
   GRD__AO.Cells[11,0] := 'Konj-' + IntToStr(YearOf(Now)-1);
   GRD__AO.Cells[12,0] := 'Opp-' + IntToStr(YearOf(Now));
   GRD__AO.Cells[13,0] := 'Konj-' + IntToStr(YearOf(Now));
   GRD__AO.Cells[14,0] := 'Opp-' + IntToStr(YearOf(Now)+1);
   GRD__AO.Cells[15,0] := 'Konj-' + IntToStr(YearOf(Now)+1);
  end
  else
  begin
    GRD__AO.Cells[10,0] := 'Opp-' + IntToStr(YearOf(Now)-1);
    GRD__AO.Cells[11,0] := 'Conj-' + IntToStr(YearOf(Now)-1);
    GRD__AO.Cells[12,0] := 'Opp-' + IntToStr(YearOf(Now));
    GRD__AO.Cells[13,0] := 'Conj-' + IntToStr(YearOf(Now));
    GRD__AO.Cells[14,0] := 'Opp-' + IntToStr(YearOf(Now)+1);
    GRD__AO.Cells[15,0] := 'Conj-' + IntToStr(YearOf(Now)+1);
  end;

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');

      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Planet'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Typ'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Trop. Jahr [d]'; mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'Exentrizität'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Halbachse des Orbits'; mslColType.Add('1');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Planet'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Type'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Trop. Year [d]';mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'Eccentricity'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Semi-major axis of the orbit';mslColType.Add('1');
      end;
      mslColType.Add('0'); // 7
      mslColType.Add('0'); // 8
      mslColType.Add('2'); // 9
      mslColType.Add('0'); // 10
      mslColType.Add('0'); // 11
      mslColType.Add('0'); // 12
      mslColType.Add('0'); // 13
      mslColType.Add('0'); // 14
      mslColType.Add('0'); // 15
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'P') then
    begin
     (molAOList[i] as TPlanet).rDistFromEarth_AU := GetPlanetCoo(dtTime,(molAOList[i] as TPlanet),
       miDST_HH,miUTC_HH,
       iRA_HH,iRA_MM,rRA_SS,
       iDEC_DEG,iDEC_MM,rDEC_SS,rR);

     (molAOList[i] as TPlanet).rDistFromSun_AU:=rR;

     if(iDEC_DEG >= 270) then iDEC_DEG := iDEC_DEG - 360;

     dtRA := EncodeTime(
       iRA_HH,
       iRA_MM,
       Trunc(rRA_SS),
       Round(1000*(rRA_SS - Trunc(rRA_SS)))
       );
     rDEC := iDec_DEG +
       iDec_MM/60.0 +
       rDec_SS/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      if((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked))) then
      begin
        iRow := GRD__AO.RowCount-1;

        GRD__AO.Cells[0,iRow] := IntToStr(i);
        GRD__AO.Cells[1,iRow] := IntToStr(iRow);

        GRD__AO.Cells[8,iRow] := format('%.2d',[Trunc(iRA_HH)]) + ':' + format('%.2d',[Trunc(iRA_MM)]) + ':' + FloatToStrF(rRA_SS,ffFixed,8,2);
        GRD__AO.Cells[9,iRow] := format('%.2d',[Trunc(iDEC_DEG)]) + ':' + format('%.2d',[Trunc(iDEC_MM)]) + ':' + FloatToStrF(rDEC_SS,ffFixed,8,2);

        if(msLANG_ID = 'DE') then
        begin
          GRD__AO.Cells[2,iRow] := (molAOList[i] as TAObject).sName_DE;

          if((molAOList[i] as TPlanet).bInnerPlanet) then
            GRD__AO.Cells[3,iRow] := 'Innerer Pl.'
          else
          begin
            if((molAOList[i] as TPlanet).sPlanetType = 'P') then
             GRD__AO.Cells[3,iRow] := 'Äußerer Pl.'
            else if ((molAOList[i] as TPlanet).sPlanetType = 'p') then
              GRD__AO.Cells[3,iRow] := 'Zwergplanet'
            else if ((molAOList[i] as TPlanet).sPlanetType = 'E') then
              GRD__AO.Cells[3,iRow] := 'Terra'
            else
              GRD__AO.Cells[3,iRow] := 'Asteroid';
          end;
        end;

        if(msLANG_ID = 'EN') then
        begin
          GRD__AO.Cells[2,iRow] := (molAOList[i] as TAObject).sName_EN;

          if((molAOList[i] as TPlanet).bInnerPlanet) then
            GRD__AO.Cells[3,iRow] := 'Inner Pl.'
          else
          begin
            if((molAOList[i] as TPlanet).sPlanetType = 'P') then
             GRD__AO.Cells[3,iRow] := 'Outer Pl.'
            else if ((molAOList[i] as TPlanet).sPlanetType = 'p') then
              GRD__AO.Cells[3,iRow] := 'Dwarf Planet'
            else
              GRD__AO.Cells[3,iRow] := 'Asteroid';
          end;
        end;

        GRD__AO.Cells[4,iRow] := FloatToStr((molAOList[i] as TPlanet).rTp);
        GRD__AO.Cells[5,iRow] := FloatToStr((molAOList[i] as TPlanet).rE);
        GRD__AO.Cells[6,iRow] := FloatToStr((molAOList[i] as TPlanet).rA);

        ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,7);

        if(not (molAOList[i] as TPlanet).bInnerPlanet) then
        begin
          if((molAOList[i] as TPlanet).dtOPDateP > 0) then
            GRD__AO.Cells[10,iRow] := DateToStr((molAOList[i] as TPlanet).dtOPDateP);

          if((molAOList[i] as TPlanet).dtConP > 0) then
            GRD__AO.Cells[11,iRow] := DateToStr((molAOList[i] as TPlanet).dtConP);

          if((molAOList[i] as TPlanet).dtOPDate > 0) then
            GRD__AO.Cells[12,iRow] := DateToStr((molAOList[i] as TPlanet).dtOPDate);

          if((molAOList[i] as TPlanet).dtCon > 0) then
            GRD__AO.Cells[13,iRow] := DateToStr((molAOList[i] as TPlanet).dtCon);

          if((molAOList[i] as TPlanet).dtOPDateN > 0) then
            GRD__AO.Cells[14,iRow] := DateToStr((molAOList[i] as TPlanet).dtOPDateN);

          if((molAOList[i] as TPlanet).dtConN > 0) then
            GRD__AO.Cells[15,iRow] := DateToStr((molAOList[i] as TPlanet).dtConN);
        end;

        GRD__AO.RowCount:=GRD__AO.RowCount+1;
      end;

    end; // if..
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.ShowTable_PN;
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  sMessier: string;
  sCon: string;
begin
  dtTime := GetWTime(); // Displayed time

  SetAOTableTitle(RB__PN.Caption);

  mslColType.Clear;

  // Grid initializing
  GRD__AO.ColCount:=12; GRD__AO.ColWidths[0]:=0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,0] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,0] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := 'RA';
  GRD__AO.Cells[10,1] := '';
  GRD__AO.Cells[11,0] := 'DEC';
  GRD__AO.Cells[11,1] := '';

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');

      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Planetarischer Nebel'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Ausdehnung qasc'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Scheibare Helligkeit'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Entfernung [LJ]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Typ'; mslColType.Add('0');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Planetary Nebula'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name';mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Extension qasc'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Magnitude'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Distance [LY]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Type'; mslColType.Add('0');
      end;
      mslColType.Add('0'); // 9
      mslColType.Add('0'); // 10
      mslColType.Add('2'); // 11
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'PN') then
    begin
      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        (molAOList[i] as TAObject).iRA_Min,
        Trunc((molAOList[i] as TAObject).rRA_Sec),
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      if(
        ((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked)))
        and
        ((not MENU__SHOW_MESSIERONLY.Checked) or (MENU__SHOW_MESSIERONLY.Checked and ( (molAOList[i] as TInterstellarObject).sMessier <> '')))
        )then
      begin
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));

        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;

          GRD__AO.Cells[0,iRow] := IntToStr(i);
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          GRD__AO.Cells[10,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,11,iRow);

          sMessier := (molAOList[i] as TInterstellarObject).sMessier;
          GRD__AO.Cells[2,iRow] := GetMessierNum(sMessier);

          if((molAOList[i] as TInterstellarObject).sNGC <> '') then
            GRD__AO.Cells[3,iRow] := (molAOList[i] as TInterstellarObject).sNGC
          else
            GRD__AO.Cells[3,iRow] := sMessier;

          if(Trim((molAOList[i] as TInterstellarObject).sCon) <> '') then
            GRD__AO.Cells[3,iRow] := GRD__AO.Cells[3,iRow] + '/' + (molAOList[i] as TInterstellarObject).sCon;

          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_DE
          else
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_EN;

          if(((molAOList[i] as TPNebula).rVisDim1 > 0) and ((molAOList[i] as TPNebula).rVisDim2 > 0)) then
            GRD__AO.Cells[5,iRow] := FloatToStrF((molAOList[i] as TPNebula).rVisDim1 * (molAOList[i] as TPNebula).rVisDim2,ffFixed,8,2)
          else
            GRD__AO.Cells[5,iRow] := '';

          if((molAOList[i] as TPNebula).rM > -999) then
            GRD__AO.Cells[6,iRow] := FloatToStrF((molAOList[i] as TPNebula).rM,ffFixed,8,1)
          else
            GRD__AO.Cells[6,iRow] := '-';

          if((molAOList[i] as TInterstellarObject).rDist_XLY > 0) then
            GRD__AO.Cells[7,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rDist_XLY)
          else
            GRD__AO.Cells[7,iRow] := '-';

          GRD__AO.Cells[8,iRow] := (molAOList[i] as TPNebula).sPNType;

          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,9);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end;
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;

end;

procedure TF__ASTROTOOLBOX.ShowTable_OC;
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  sMessier: string;
  sCon: string;
begin
  dtTime := GetWTime(); // Displayed time

  SetAOTableTitle(RB__OC.Caption);

  mslColType.Clear;

  // Grid initializing
  GRD__AO.ColCount:=12; GRD__AO.ColWidths[0]:=0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,0] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := 'RA';
  GRD__AO.Cells[10,1] := '';
  GRD__AO.Cells[11,0] := 'DEC';
  GRD__AO.Cells[11,1] := '';

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');
      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Offener Sternhaufen'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Scheinbarer Durchm. in Bogenminuten'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Scheibare Helligkeit'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Entfernung [LJ]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Typ'; mslColType.Add('0');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Open Cluster'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Diameter in Arc Min'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Magnitude'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Distance [LY]';mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Type'; mslColType.Add('0');
      end;
      mslColType.Add('0'); // 9
      mslColType.Add('0'); // 10
      mslColType.Add('2'); // 11
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'OC') then
    begin
      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        (molAOList[i] as TAObject).iRA_Min,
        Trunc((molAOList[i] as TAObject).rRA_Sec),
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      if(
        ((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked)))
        and
        ((not MENU__SHOW_MESSIERONLY.Checked) or (MENU__SHOW_MESSIERONLY.Checked and ( (molAOList[i] as TInterstellarObject).sMessier <> '')))
        )then
      begin
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));
        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;

          GRD__AO.Cells[0,iRow] := IntToStr(i);
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          GRD__AO.Cells[10,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,11,iRow);

          sMessier := (molAOList[i] as TInterstellarObject).sMessier;
          GRD__AO.Cells[2,iRow] := GetMessierNum(sMessier);

          if((molAOList[i] as TInterstellarObject).sNGC <> '') then
            GRD__AO.Cells[3,iRow] := (molAOList[i] as TInterstellarObject).sNGC
          else
            GRD__AO.Cells[3,iRow] := sMessier;

          if(Trim((molAOList[i] as TInterstellarObject).sCon) <> '') then
            GRD__AO.Cells[3,iRow] := GRD__AO.Cells[3,iRow] + '/' + (molAOList[i] as TInterstellarObject).sCon;

          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_DE
          else
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_EN;

          GRD__AO.Cells[5,iRow] := IntToStr((molAOList[i] as TOpenCluster).iDiam_M);

          if((molAOList[i] as TOpenCluster).rM > -999) then
            GRD__AO.Cells[6,iRow] := FloatToStr((molAOList[i] as TOpenCluster).rM)
          else
            GRD__AO.Cells[6,iRow] := '-';

          if((molAOList[i] as TInterstellarObject).rDist_XLY > 0) then
            GRD__AO.Cells[7,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rDist_XLY)
          else
            GRD__AO.Cells[7,iRow] := '-';

          GRD__AO.Cells[8,iRow] := (molAOList[i] as TOpenCluster).sOCType;

          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,9);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end;
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.ShowTable_N;
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  sMessier: string;
  sCon: string;
begin
  dtTime := GetWTime(); // Displayed time

  SetAOTableTitle(RB__N.Caption);

  mslColType.Clear;

  // Grid initializing
  GRD__AO.ColCount:=12; GRD__AO.ColWidths[0]:=0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,0] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := 'RA';
  GRD__AO.Cells[10,1] := '';
  GRD__AO.Cells[11,0] := 'DEC';
  GRD__AO.Cells[11,1] := '';

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');
      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Galaktischer Nebel'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Ausdehnung qamin'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'Zugehöriger Stern'; mslColType.Add('0');
        GRD__AO.Cells[7,0] := 'Entfernung [LJ]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Typ'; mslColType.Add('0');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Nebula'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Extension qamin'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'Assigned Star'; mslColType.Add('0');
        GRD__AO.Cells[7,0] := 'Distance [LY]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Type'; mslColType.Add('0');
      end;
      mslColType.Add('0'); // 9
      mslColType.Add('0'); // 10
      mslColType.Add('2'); // 11
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'N') then
    begin
      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        (molAOList[i] as TAObject).iRA_Min,
        Trunc((molAOList[i] as TAObject).rRA_Sec),
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      if(
        ((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked)))
        and
        ((not MENU__SHOW_MESSIERONLY.Checked) or (MENU__SHOW_MESSIERONLY.Checked and ( (molAOList[i] as TInterstellarObject).sMessier <> '')))
        )then
      begin
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));
        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;

          GRD__AO.Cells[0,iRow] := IntToStr(i);
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          GRD__AO.Cells[10,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,11,iRow);

          sMessier := (molAOList[i] as TInterstellarObject).sMessier;
          GRD__AO.Cells[2,iRow] := GetMessierNum(sMessier);

          if((molAOList[i] as TInterstellarObject).sNGC <> '') then
            GRD__AO.Cells[3,iRow] := (molAOList[i] as TInterstellarObject).sNGC
          else
            GRD__AO.Cells[3,iRow] := sMessier;

          if(Trim((molAOList[i] as TInterstellarObject).sCon) <> '') then
            GRD__AO.Cells[3,iRow] := GRD__AO.Cells[3,iRow] + '/' + (molAOList[i] as TInterstellarObject).sCon;

          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_DE
          else
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_EN;

          if(((molAOList[i] as TNebula).iVisDim1 > 0) and ((molAOList[i] as TNebula).iVisDim2 > 0)) then
            GRD__AO.Cells[5,iRow] := IntToStr((molAOList[i] as TNebula).iVisDim1 * (molAOList[i] as TNebula).iVisDim2)
          else
            GRD__AO.Cells[5,iRow] := '';

          GRD__AO.Cells[6,iRow] := (molAOList[i] as TNebula).sStar;

          if((molAOList[i] as TInterstellarObject).rDist_XLY > 0) then
            GRD__AO.Cells[7,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rDist_XLY)
          else
            GRD__AO.Cells[7,iRow] := '-';

          GRD__AO.Cells[8,iRow] := (molAOList[i] as TNebula).sNType;

          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,9);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end;
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.SetAOTableTitle(sAOType: string);
var
  sMagText: string;
begin
  sMagText := '';

  if(RB__P.Checked) then
  begin
    if(msLANG_ID = 'DE') then
      P__TABLE_TITLE.Caption := 'Planeten'
    else
      P__TABLE_TITLE.Caption := 'Planets';

  end
  else if(RB__COMETS.Checked) then
  begin
    if(msLANG_ID = 'DE') then
      P__TABLE_TITLE.Caption := 'Kometen'
    else
      P__TABLE_TITLE.Caption := 'Comets';

  end
  else if(RB__G.Checked) then
  begin
    if(msLANG_ID = 'DE') then
      P__TABLE_TITLE.Caption := 'Galaxien bis Mag ' + FloatToStrF(mrMagPos_G,ffFixed,8,1)
    else
      P__TABLE_TITLE.Caption := 'Galaxies up to Mag ' + FloatToStrF(mrMagPos_G,ffFixed,8,1);

  end
  else if(CB__SIGNS.ItemIndex >= 1) then
  begin
    if(msLANG_ID = 'DE') then
      P__TABLE_TITLE.Caption := sAOType + ' in ' + CB__SIGNS.Text + sMagText
    else
      P__TABLE_TITLE.Caption := sAOType + ' in ' + CB__SIGNS.Text + sMagText;

  end
  else if(CB__SIGNS.ItemIndex = 0) then
  begin
    if(msLANG_ID = 'DE') then
      P__TABLE_TITLE.Caption := 'Alle ' + sAOType + sMagText
    else
      P__TABLE_TITLE.Caption := 'All ' + sAOType + sMagText;

  end;

end;

procedure TF__ASTROTOOLBOX.ShowTable_GC;
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  sMessier: string;
  sCon: string;
begin
  dtTime := GetWTime(); // Displayed time

  SetAOTableTitle(RB__GC.Caption);

  mslColType.Clear;

  // Grid initializing
  GRD__AO.ColCount:=12;  GRD__AO.ColWidths[0] := 0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,0] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := 'RA';
  GRD__AO.Cells[10,1] := '';
  GRD__AO.Cells[11,0] := 'DEC';
  GRD__AO.Cells[11,1] := '';

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');
      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Kugelsternhaufen'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Ausdehnung asc'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'M'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Entfernung [LJ]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Typ'; mslColType.Add('0');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Globular Cluster'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Extension asc'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'Mag'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Distance [LY]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Type'; mslColType.Add('0');
      end;
      mslColType.Add('0'); // 9
      mslColType.Add('0'); // 10
      mslColType.Add('2'); // 11
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'GC') then
    begin
      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        (molAOList[i] as TAObject).iRA_Min,
        Trunc((molAOList[i] as TAObject).rRA_Sec),
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      if(
        ((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked)))
        and
        ((not MENU__SHOW_MESSIERONLY.Checked) or (MENU__SHOW_MESSIERONLY.Checked and ( (molAOList[i] as TInterstellarObject).sMessier <> '')))
        )then
      begin
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));
        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;

          GRD__AO.Cells[0,iRow] := IntToStr(i);
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          GRD__AO.Cells[10,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,11,iRow);

          sMessier := (molAOList[i] as TInterstellarObject).sMessier;
          GRD__AO.Cells[2,iRow] := GetMessierNum(sMessier);

          if((molAOList[i] as TInterstellarObject).sNGC <> '') then
            GRD__AO.Cells[3,iRow] := (molAOList[i] as TInterstellarObject).sNGC
          else
            GRD__AO.Cells[3,iRow] := sMessier;

          if(Trim((molAOList[i] as TInterstellarObject).sCon) <> '') then
            GRD__AO.Cells[3,iRow] := GRD__AO.Cells[3,iRow] + '/' + (molAOList[i] as TInterstellarObject).sCon;

          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_DE
          else
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_EN;

          GRD__AO.Cells[5,iRow] := FloatToStr((molAOList[i] as TGlobularCluster).rVisDiam);

          if((molAOList[i] as TAObject).rM > 0) then
            GRD__AO.Cells[6,iRow] := FloatToStr((molAOList[i] as TAObject).rM)
          else
            GRD__AO.Cells[6,iRow] := '-';


          if((molAOList[i] as TInterstellarObject).rDist_XLY > 0) then
            GRD__AO.Cells[7,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rDist_XLY)
          else
            GRD__AO.Cells[7,iRow] := '-';

          GRD__AO.Cells[8,iRow] := IntToStr((molAOList[i] as TGlobularCluster).iGCType);

          //if(not CBX__VISIBLE_ONLY.Checked) then
          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,9);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end;
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.ShowTable_G();
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  sMessier: string;
  sCon: string;
  iModDiv: Integer;
  bGChunk: Boolean;
  iStartIndex,iEndIndex: Integer;
  rMag1, rMag2: Real;
begin
  dtTime := GetWTime(); // Displayed time
  iModDiv := 1;
  bGChunk := false;

  P__SELMAG.Visible := true;

  mslColType.Clear;

  rMag1 := 0.0; rMag2 := crMagPosStd_G;
  SetMagInterval(rMag1,rMag2);

  // Grid initializing
  GRD__AO.ColCount:=12;  GRD__AO.ColWidths[0] := 0;
  GRD__AO.RowCount:=2;
  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,0] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := 'RA';
  GRD__AO.Cells[10,1] := '';
  GRD__AO.Cells[11,0] := 'DEC';
  GRD__AO.Cells[11,1] := '';

  iStartIndex := GetAOIC_Min_G();
  iEndIndex := GetAOIC_Max_G();
  // Fill Content
  //for i:=0 to molAOList.Count-1 do
  for i:=iStartIndex to iEndIndex do
  begin
    // Set Captions
    if(i = iStartIndex) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1'); // DB Index!
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');
      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Galaxie'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Ausdehnung qasc'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'M'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Entfernung [MLJ]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Typ'; mslColType.Add('0');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Galaxy'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Vis. Extension qasc'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'M'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Distance [MLY]'; mslColType.Add('1');
        GRD__AO.Cells[8,0] := 'Type'; mslColType.Add('0');
      end;
      mslColType.Add('0'); // 9
      mslColType.Add('0'); // 10
      mslColType.Add('2'); // 11
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'G') then
    begin
      if(not bGChunk) then
        bGChunk := true;

      if((molAOList[i] as TAObject).rM < rMag1) or ((molAOList[i] as TAObject).rM > rMag2) then
        continue;

      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        (molAOList[i] as TAObject).iRA_Min,
        Trunc((molAOList[i] as TAObject).rRA_Sec),
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      if(
        ((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked)))
        and
        ((not MENU__SHOW_MESSIERONLY.Checked) or (MENU__SHOW_MESSIERONLY.Checked and ( (molAOList[i] as TInterstellarObject).sMessier <> '')))
        )then
      begin
        // star constellation activated?
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));
        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;
          GRD__AO.Cells[0,iRow] := IntToStr(i); // DB Index
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          if(iRow < 10) then
            iModDiv := 1
          else if(iRow < 100) then
            iModDiv := 10
          else if(iRow < 1000) then
            iModDiv := 100
          else
            iModDiv := 1000;

          if((iRow mod iModDiv) = 0) then begin Refresh(); Application.ProcessMessages; Refresh(); end;

          if(msLANG_ID = 'DE') then
          begin
            if(rMag1 <= 0) then
              P__TABLE_TITLE.Caption := IntToStr(iRow) + ' Galaxien bis Mag ' + FloatToStrF(rMag2,ffFixed,8,1)
            else
              P__TABLE_TITLE.Caption := IntToStr(iRow) + ' Galaxien von Mag ' + FloatToStrF(rMag1,ffFixed,8,1)
                + ' bis Mag ' + FloatToStrF(rMag2,ffFixed,8,1);
          end
          else
          begin
            if(rMag1 <= 0) then
              P__TABLE_TITLE.Caption := IntToStr(iRow) + ' Galaxies up to Mag ' + FloatToStrF(rMag2,ffFixed,8,1)
            else
            P__TABLE_TITLE.Caption := IntToStr(iRow) + ' Galaxies from Mag ' + FloatToStrF(rMag1,ffFixed,8,1)
              + ' to Mag ' + FloatToStrF(rMag2,ffFixed,8,1);

          end;

          GRD__AO.Cells[10,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,11,iRow);

          sMessier := (molAOList[i] as TInterstellarObject).sMessier;
          GRD__AO.Cells[2,iRow] := GetMessierNum(sMessier);

          if((molAOList[i] as TInterstellarObject).sNGC <> '') then
            GRD__AO.Cells[3,iRow] := (molAOList[i] as TInterstellarObject).sNGC
          else
            GRD__AO.Cells[3,iRow] := sMessier;

          if(Trim((molAOList[i] as TInterstellarObject).sCon) <> '') then
            GRD__AO.Cells[3,iRow] := GRD__AO.Cells[3,iRow] + '/' +
                (molAOList[i] as TInterstellarObject).sCon;

          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_DE
          else
            GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sName_EN;

          if(((molAOList[i] as TGalaxy).rVisDim1 > 0) and ((molAOList[i] as TGalaxy).rVisDim2 > 0)) then
            GRD__AO.Cells[5,iRow] := IntToStr(Round((molAOList[i] as TGalaxy).rVisDim1 * (molAOList[i] as TGalaxy).rVisDim2))
          else
            GRD__AO.Cells[5,iRow] := '';

          GRD__AO.Cells[6,iRow] := FloatToStr((molAOList[i] as TAObject).rM);

          if((molAOList[i] as TInterstellarObject).rDist_XLY > 0) then
            GRD__AO.Cells[7,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rDist_XLY)
          else
            GRD__AO.Cells[7,iRow] := '-';

          GRD__AO.Cells[8,iRow] := (molAOList[i] as TGalaxy).sGType;

          //if(not CBX__VISIBLE_ONLY.Checked) then
          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,9);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end
    else
      if(bGChunk) then // Quit after galaxy data section (chunk)
        break;

  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.ShowTable_Q;
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  sCon: string;
begin
  dtTime := GetWTime(); // Displayed time

  SetAOTableTitle(RB__Q.Caption);

  mslColType.Clear;

  // Grid initializing
  GRD__AO.ColCount:=11; GRD__AO.ColWidths[0] := 0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,0] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := 'RA';
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := 'DEC';
  GRD__AO.Cells[10,1] := '';

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');
      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'QID'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Typ'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Fluchtgeschwindigkeit'; mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'M'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Mitbewegte Entfernung [MPc]'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Typ'; mslColType.Add('0');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'QID'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Type'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Radial Speed'; mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'M'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Moved Distance [MPc]'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'Type'; mslColType.Add('0');
      end;
      mslColType.Add('0'); // 8
      mslColType.Add('0'); // 9
      mslColType.Add('2'); // 10
    end;

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'Q') then
    begin
      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        (molAOList[i] as TAObject).iRA_Min,
        Trunc((molAOList[i] as TAObject).rRA_Sec),
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      if((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked))) then
      begin
        // star constellation activated?
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));
        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;
          GRD__AO.Cells[0,iRow] := IntToStr(i);
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          GRD__AO.Cells[9,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,10,iRow);

          GRD__AO.Cells[2,iRow] := (molAOList[i] as TQuasar).sQID;
          GRD__AO.Cells[3,iRow] := FloatToStr((molAOList[i] as TQuasar).fRedshift);
          GRD__AO.Cells[4,iRow] := FloatToStr((molAOList[i] as TQuasar).fRadSpeed);

          if((molAOList[i] as TAObject).rM > -999) then
            GRD__AO.Cells[5,iRow] := FloatToStr((molAOList[i] as TAObject).rM)
          else
            GRD__AO.Cells[5,iRow] := '-';

          GRD__AO.Cells[6,iRow] := FloatToStr((molAOList[i] as TQuasar).fComDistMPc);
          GRD__AO.Cells[7,iRow] := (molAOList[i] as TQuasar).sQType;

          //if(not CBX__VISIBLE_ONLY.Checked) then
          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,8);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end;
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.ShowTable_Messier();
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  rHgt: Real;
  bOverHorizon: Boolean;
  dtTime: TDateTime;
  sCon: string;
  iStartIndex,iEndIndex: Integer;
begin
  dtTime := GetWTime(); // Displayed time

  SetAOTableTitle(RB__MESSIER.Caption);

  // Grid initializing
  GRD__AO.ColCount:=11;  GRD__AO.ColWidths[0] := 0;
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := 'RA';
  GRD__AO.Cells[9,1] := '';
  GRD__AO.Cells[10,0] := 'DEC';
  GRD__AO.Cells[10,1] := '';

  mslColType.Clear;

  //MM.Lines.Clear;

  iStartIndex := GetAOIC_MinMessier();
  iEndIndex := GetAOIC_MaxMessier();
  // Fill Content
  for i:=iStartIndex to iEndIndex do//molAOList.Count-1 do
  begin
    // Set Captions
    if(i = iStartIndex) then
    begin
      GRD__AO.Cells[0,0] := ''; mslColType.Add('1');
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');
      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'NGC'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Sternbild'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Typ'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'Entfernung [LJ]'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'M'; mslColType.Add('1');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Messier'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'NGC'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'Constellation'; mslColType.Add('0');
        GRD__AO.Cells[5,0] := 'Type'; mslColType.Add('0');
        GRD__AO.Cells[6,0] := 'Distance [LY]'; mslColType.Add('1');
        GRD__AO.Cells[7,0] := 'M'; mslColType.Add('1');
      end;
      mslColType.Add('0'); // 8
      mslColType.Add('0'); // 8
      mslColType.Add('2'); // 10
    end;

    // Check & Set Content
    if(
      ((molAOList[i] as TAObject).sAOType = 'G') or
      ((molAOList[i] as TAObject).sAOType = 'OC') or
      ((molAOList[i] as TAObject).sAOType = 'GC') or
      ((molAOList[i] as TAObject).sAOType = 'PN') or
      ((molAOList[i] as TAObject).sAOType = 'N')
      ) then
    begin
      //MM.Lines.Add((molAOList[i] as TInterstellarObject).sNGC);
      //if() then
      //  continue;

      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        (molAOList[i] as TAObject).iRA_Min,
        Trunc((molAOList[i] as TAObject).rRA_Sec),
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0;

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      if(
        ((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked)))
        and ((molAOList[i] as TInterstellarObject).sMessier <> '')
        ) then
      begin
        // star constellation activated?
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));
        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;

          GRD__AO.Cells[0,iRow] := IntToStr(i);
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          GRD__AO.Cells[9,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,10,iRow);

          GRD__AO.Cells[2,iRow] := GetMessierNum((molAOList[i] as TInterstellarObject).sMessier);

          GRD__AO.Cells[3,iRow] := (molAOList[i] as TInterstellarObject).sNGC;
          GRD__AO.Cells[4,iRow] := (molAOList[i] as TInterstellarObject).sCon;
          GRD__AO.Cells[5,iRow] := (molAOList[i] as TInterstellarObject).sAOType;

          GRD__AO.Cells[6,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rDist_XLY);
          if((molAOList[i] as TAObject).sAOType = 'G') then
            GRD__AO.Cells[6,iRow] := GRD__AO.Cells[6,iRow] + 'e6';

          if((molAOList[i] as TAObject).rM > -999) then
            GRD__AO.Cells[7,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rM)
          else
            GRD__AO.Cells[7,iRow] := '-';

          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,8);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end;
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;
end;

procedure TF__ASTROTOOLBOX.PutDECVal(IAObject: TInterstellarObject; GRD: TStringGrid; iCol, iRow: Integer);
{27.10.19/fs
Sign declination degree values correctly if negative.
}
begin
  GRD.Cells[iCol,iRow] := format('%.2d',[Abs(Trunc(IAObject.iDec_Deg))]) + ':' +
    format('%.2d',[Abs(IAObject.iDec_Min)]) + ':' +
    FloatToStrF(Abs(IAObject.rDec_Sec),ffFixed,8,2);

  if(IAObject.iDec_Deg < 0) or
    (IAObject.iDec_Min < 0) or
    (IAObject.rDec_Sec < 0) then
    GRD.Cells[iCol,iRow] := '-' + GRD.Cells[iCol,iRow];

end;


function TF__ASTROTOOLBOX.SLFilter(sSpecType: string): Boolean;
// Filtering for Spectral Class & Luminance Class
var
  sClass, sClass2: string;
begin
  Result := false;

  // Luminance class
  if (not MENU__LCLASS_ALL.Checked) then
  begin
    sClass := GetLClass(sSpecType);
    sClass2 := GetLClass_MS(sSpecType);

    if ((sClass = '0') or (sClass2 = '0')) and (MENU__LCLASS_0.Checked) then
      Result := true;
    if (
        (sClass = 'I') or (sClass = 'Ia') or (sClass = 'Ib') or
        (sClass2 = 'I') or (sClass2 = 'Ia') or (sClass2 = 'Ib')
      )
      and (MENU__LCLASS_1.Checked) then
      Result := true;

    if ((sClass = 'II') or (sClass2 = 'II')) and (MENU__LCLASS_2.Checked) then
      Result := true;
    if ((sClass = 'III') or (sClass2 = 'III')) and (MENU__LCLASS_3.Checked) then
      Result := true;
    if ((sClass = 'IV') or (sClass2 = 'IV')) and (MENU__LCLASS_4.Checked) then
      Result := true;
    if ((sClass = 'V') or (sClass2 = 'V')) and (MENU__LCLASS_5.Checked) then
      Result := true;
    if ((sClass = 'VI') or (sClass2 = 'VI')) and (MENU__LCLASS_6.Checked) then
      Result := true;
    if ((sClass = 'VII') or (sClass2 = 'VII')) and (MENU__LCLASS_7.Checked) then
      Result := true;
  end
  else
    Result := true;

  if(not Result) then
    exit
  else
    Result := false;

  // Spectral class (star color)
  if (not MENU__SCLASS_ALL.Checked) then
  begin
    sClass := GetSClass(sSpecType);
    sClass2 := GetSClass_MS(sSpecType); // Matching spectral class of compagnion star

    if ((sClass = 'O') or (sClass2 = 'O')) and (MENU__SCLASS_O.Checked) then
      Result := true;
    if ((sClass = 'B') or (sClass2 = 'B'))and (MENU__SCLASS_B.Checked) then
      Result := true;
    if ((sClass = 'A') or (sClass2 = 'A')) and (MENU__SCLASS_A.Checked) then
      Result := true;
    if ((sClass = 'F') or (sClass2 = 'F')) and (MENU__SCLASS_F.Checked) then
      Result := true;
    if ((sClass = 'G') or (sClass2 = 'G')) and (MENU__SCLASS_G.Checked) then
      Result := true;
    if ((sClass = 'K') or (sClass2 = 'K')) and (MENU__SCLASS_K.Checked) then
      Result := true;
    if ((sClass = 'M') or (sClass2 = 'M')) and (MENU__SCLASS_M.Checked) then
      Result := true;

    // Carbon Class Filter
    if ((sClass = 'R') or (sClass2 = 'R')) and (MENU__SCLASS_CARBON.Checked or MENU__SCLASS_R.Checked) then
      Result := true;
    if ((sClass = 'N') or (sClass2 = 'N')) and (MENU__SCLASS_CARBON.Checked or MENU__SCLASS_N.Checked) then
      Result := true;
    if ((sClass = 'S') or (sClass2 = 'S')) and (MENU__SCLASS_CARBON.Checked or MENU__SCLASS_S.Checked) then
      Result := true;
    if ((sClass = 'C') or (sClass2 = 'C')) and (MENU__SCLASS_CARBON.Checked or MENU__SCLASS_C.Checked) then
      Result := true;

    // Brown Dwarfs Filter
    if ((sClass = 'L') or (sClass2 = 'L')) and (MENU__SCLASS_BD.Checked or MENU__SCLASS_L.Checked) then
      Result := true;
    if ((sClass = 'T') or (sClass2 = 'T')) and (MENU__SCLASS_BD.Checked or MENU__SCLASS_T.Checked) then
      Result := true;
    if ((sClass = 'Y') or (sClass2 = 'Y')) and (MENU__SCLASS_BD.Checked or MENU__SCLASS_Y.Checked) then
      Result := true;
  end
  else
    Result := true;

end;

procedure TF__ASTROTOOLBOX.ShowTable_S();
var
  i, iRow: Integer;
  dtRA: TDateTime;
  rDEC: Real;
  dtTime: TDateTime;
  rHgt: Real;
  bOverHorizon: Boolean;
  sCon: string;
  iMinutes, iSeconds: Integer;
  iModDiv: Integer;
  bFilter: Boolean;
  rMax1, rMax2: Real;
begin
  dtTime := GetWTime(); // Displayed time

  P__SELMAG.Visible := true;
  //SetAOTableTitle(RB__S.Caption);
  MENU__SHOW_STARS.Enabled:=true;

  mslColType.Clear;

  rMax1 := -2.0; rMax2 := crMagPosStd;
  SetMagInterval(rMax1,rMax2);

  // Grid initializing
  GRD__AO.ColCount:=10;  GRD__AO.ColWidths[0] := 0; // Database index field invisible
  GRD__AO.RowCount:=2;

  GRD__AO.Cells[0,1] := '';
  GRD__AO.Cells[1,1] := '';
  GRD__AO.Cells[2,1] := '';
  GRD__AO.Cells[3,1] := '';
  GRD__AO.Cells[4,1] := '';
  GRD__AO.Cells[5,1] := '';
  GRD__AO.Cells[6,1] := '';
  GRD__AO.Cells[7,0] := DateTimeToStr(dtTime);
  GRD__AO.Cells[7,1] := '';
  GRD__AO.Cells[8,0] := 'RA';
  GRD__AO.Cells[8,1] := '';
  GRD__AO.Cells[9,0] := 'DEC';
  GRD__AO.Cells[9,1] := '';

  iRow := 0;
  iModDiv := 1;

  // Fill Content
  for i:=0 to molAOList.Count-1 do
  begin
    // Set Captions
    if(i = 0) then
    begin
      GRD__AO.Cells[1,0] := ''; mslColType.Add('1'); // Field 0: Always Database index
      GRD__AO.Cells[1,0] := 'Index |A'; mslColType.Add('1');
      if(msLANG_ID = 'DE') then
      begin
        GRD__AO.Cells[2,0] := 'Object'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'M'; mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'Entfernung [LJ]/[MLJ]'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Spektraltyp'; mslColType.Add('0');
      end;
      if(msLANG_ID = 'EN') then
      begin
        GRD__AO.Cells[2,0] := 'Object'; mslColType.Add('0');
        GRD__AO.Cells[3,0] := 'Name'; mslColType.Add('0');
        GRD__AO.Cells[4,0] := 'M'; mslColType.Add('1');
        GRD__AO.Cells[5,0] := 'Distance [LY]/[MLY]'; mslColType.Add('1');
        GRD__AO.Cells[6,0] := 'Spectral type'; mslColType.Add('0');
      end;
      mslColType.Add('0'); // 7
      mslColType.Add('0'); // 8
      mslColType.Add('2'); // 9
    end; // i=0

    // Check & Set Content
    if((molAOList[i] as TAObject).sAOType = 'S') then
    begin

      if(
        ((molAOList[i] as TAObject).rM < rMax1)
        ) then
      begin
        continue;
      end;

      if( // Accelerate low amgnitude search values < 6mag based on most basic dataset
        ((molAOList[i] as TAObject).rM > 6) and ((molAOList[i] as TAObject).rM > rMax2)  // 7 and higher are sorted (increasing Mag-value)
        ) then
      begin
        continue;
      end;

      iMinutes := (molAOList[i] as TAObject).iRA_Min;
      iSeconds := Trunc((molAOList[i] as TAObject).rRA_Sec);

      if(iSeconds >= 60) then
      begin
        if(iMinutes < 59) then
        begin
          iMinutes := iMinutes + 1;
          iSeconds := 0;
        end
        else
        begin
          if((molAOList[i] as TAObject).iRA_Hours < 24) then
          begin
            (molAOList[i] as TAObject).iRA_Hours := (molAOList[i] as TAObject).iRA_Hours + 1;
            iMinutes := 0;
            iSeconds := 0;
          end
          else
          begin
            (molAOList[i] as TAObject).iRA_Hours := 1;
            iMinutes := 0;
            iSeconds := 0;
          end;
        end;
      end;

      dtRA := EncodeTime(
        (molAOList[i] as TAObject).iRA_Hours,
        iMinutes,
        iSeconds,
        Trunc(1000*((molAOList[i] as TAObject).rRA_Sec - Trunc((molAOList[i] as TAObject).rRA_Sec)))
        );
      rDEC := (molAOList[i] as TAObject).iDec_Deg +
        (molAOList[i] as TAObject).iDec_Min/60.0 +
        (molAOList[i] as TAObject).rDec_Sec/3600.0; // Changed.

      rHgt := GetVisualHeight(dtTime,dtRA,rDEC);
      bOverHorizon := (rHgt > 0);

      sCon := '';

      bFilter := SLFilter((molAOList[i] as TStar).sSpType);

      if((bOverHorizon) or ((not bOverHorizon) and (not MENU__VISIBLE_ONLY.Checked)))
        and (bFilter) and (
         ((molAOList[i] as TAObject).rM >= rMax1) and ((molAOList[i] as TAObject).rM <= rMax2)
          ) then
      begin
        // star constellation activated?
        if(CB__SIGNS.ItemIndex > 0) then sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));
        if(CB__SIGNS.ItemIndex = 0) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon) = sCon) or
          (Uppercase((molAOList[i] as TInterstellarObject).sCon2) = sCon) then
        begin
          iRow := GRD__AO.RowCount-1;

          if(iRow < 10) then
            iModDiv := 1
          else if(iRow < 100) then
            iModDiv := 10
          else if(iRow < 1000) then
            iModDiv := 100
          else
            iModDiv := 1000;

          if((iRow mod iModDiv) = 0) then begin Refresh(); Application.ProcessMessages; Refresh(); end;

          if(msLANG_ID = 'DE') then
          begin
            if(rMax1 <= -2) then
             P__TABLE_TITLE.Caption:= IntToStr(iRow) + ' Sterne bis ' +  FloatToStrF(rMax2,ffFixed,8,2) + 'mag'
            else
              P__TABLE_TITLE.Caption:=IntToStr(iRow) + ' Sterne zw. ' +  FloatToStrF(rMax1,ffFixed,8,2) + '...' + FloatToStrF(rMax2,ffFixed,8,2) + 'mag';
          end
          else
          begin
            if(rMax1 <= -2) then
             P__TABLE_TITLE.Caption := IntToStr(iRow) + ' stars up to ' +  FloatToStrF(rMax2,ffFixed,8,2) + 'mag'
            else
              P__TABLE_TITLE.Caption := IntToStr(iRow) + ' stars between ' +  FloatToStrF(rMax1,ffFixed,8,2) + '...' + FloatToStrF(rMax2,ffFixed,8,2) + 'mag';
          end;

          GRD__AO.Cells[0,iRow] := IntToStr(i);
          GRD__AO.Cells[1,iRow] := IntToStr(iRow);

          GRD__AO.Cells[8,iRow] := format('%.2d',[Trunc((molAOList[i] as TInterstellarObject).iRA_Hours)]) +
            ':' + format('%.2d',[(molAOList[i] as TInterstellarObject).iRA_Min]) +
            ':' + FloatToStrF((molAOList[i] as TInterstellarObject).rRA_Sec,ffFixed,8,2);

          PutDECVal((molAOList[i] as TInterstellarObject),GRD__AO,9,iRow);

          if((molAOList[i] as TStar).sSym <> '') then
          begin
            if((molAOList[i] as TStar).sCon <> '') then
              GRD__AO.Cells[2,iRow] := (molAOList[i] as TStar).sSym + ' ' + (molAOList[i] as TStar).sCon
            else
              GRD__AO.Cells[2,iRow] := (molAOList[i] as TStar).sSym;

          end
          else
            GRD__AO.Cells[2,iRow] := (molAOList[i] as TStar).sCatNo;

          if(msLANG_ID = 'DE') then
            GRD__AO.Cells[3,iRow] := (molAOList[i] as TAObject).sName_DE;

          if(msLANG_ID = 'EN') then
            GRD__AO.Cells[3,iRow] := (molAOList[i] as TAObject).sName_EN;

          if((molAOList[i] as TStar).sSym <> 'MARK') then
            GRD__AO.Cells[4,iRow] := FloatToStr((molAOList[i] as TAObject).rM)
          else
            GRD__AO.Cells[4,iRow] := '';

          if((molAOList[i] as TStar).sSym <> 'MARK') then
          begin
          if((molAOList[i] as TInterstellarObject).rDist_XLY > 0) then
            GRD__AO.Cells[5,iRow] := FloatToStr((molAOList[i] as TInterstellarObject).rDist_XLY)
          else
            GRD__AO.Cells[5,iRow] := '-';
          end
          else
            GRD__AO.Cells[5,iRow] := '';

          GRD__AO.Cells[6,iRow] := (molAOList[i] as TStar).sSpType;

          // Draw 5th column only if checkbox CBX__VISIBLE_ONLY is not checked
          //if(not CBX__VISIBLE_ONLY.Checked) then
          ValuateVisibility((molAOList[i] as TAObject),rHgt,iRow,dtTime,7);

          GRD__AO.RowCount:=GRD__AO.RowCount+1;
        end;
      end;
    end; // if..
  end; //for..

  if(msLANG_ID = 'DE') then
  begin
    if(rMax1 <= -2) then
     P__TABLE_TITLE.Caption:= IntToStr(iRow) + ' Sterne bis ' +  FloatToStrF(rMax2,ffFixed,8,2) + 'mag'
    else
      P__TABLE_TITLE.Caption := IntToStr(iRow) + ' Sterne zw. ' +  FloatToStrF(rMax1,ffFixed,8,2) + '...' + FloatToStrF(rMax2,ffFixed,8,2) + 'mag';
  end
  else
  begin
    if(rMax1 <= -2) then
     P__TABLE_TITLE.Caption := IntToStr(iRow) + ' stars up to ' +  FloatToStrF(rMax2,ffFixed,8,2) + 'mag'
    else
      P__TABLE_TITLE.Caption := IntToStr(iRow) + ' stars between ' +  FloatToStrF(rMax1,ffFixed,8,2) + '...' + FloatToStrF(rMax2,ffFixed,8,2) + 'mag';
  end;

  // Remove last empty line
  if(GRD__AO.RowCount > 2) then
    GRD__AO.RowCount:=GRD__AO.RowCount-1;

end;

function TF__ASTROTOOLBOX.RegisterAObject(iIndex: Integer): Boolean;
// Set the selection list of CB_AO.
// Objects should be limited to max mag 7.0
var
  AObject: TAObject;
  sText: string;
  sMessier: string;
begin
  Result := false;
  sText := 'UNKOWN';

  AObject := (molAOList[iIndex] as TAObject);

  if(AObject.sAOType = '') then exit;

  if(RB__S.Checked) and (AObject.sAOType = 'S') and (AObject.rM > 6.0) then
    exit; // Omit faint stars for the list of CB_AO. Otherwise a lot of resources and time is wasted.

  if(RB__S.Checked) and (AObject.sAOType <> 'S') then exit;
  if(RB__G.Checked) and (AObject.sAOType <> 'G') then exit;
  if(RB__Q.Checked) and (AObject.sAOType <> 'Q') then exit;
  if(RB__GC.Checked) and (AObject.sAOType <> 'GC') then exit;
  if(RB__N.Checked) and (AObject.sAOType <> 'N') then exit;
  if(RB__PN.Checked) and (AObject.sAOType <> 'PN') then exit;
  if(RB__OC.Checked) and (AObject.sAOType <> 'OC') then exit;
  if(RB__P.Checked) and (not ((AObject.sAOType = 'P') or (AObject.sAOType = 'E'))) then exit;
  //if(RB__P.Checked) and ((AObject as TPlanet).sPlanetType = 'E') then exit; // Exclude Earth dataset
  if(RB__COMETS.Checked) and (AObject.sAOType <> 'C') then exit;
  //...

  Result := true;

  // Registering...
  if(AObject.sAOType = 'P') or (AObject.sAOType = 'C') then
  begin
    if(msLANG_ID = 'DE') then
      if(Trim(AObject.sName_DE) <> '') then
        sText := AObject.sName_DE;

    if(msLANG_ID = 'EN') then
      if(Trim(AObject.sName_EN) <> '') then
        sText := AObject.sName_EN;
  end
  else
  begin
    //sType := AObject.sAOType;

    if(AObject.sAOType = 'S') then
    begin
      if((molAOList[iIndex] as TStar).sSym <> '') then
        sText := (molAOList[iIndex] as TStar).sSym // Greek symbol
      else
        sText := (molAOList[iIndex] as TStar).sCatNo;  // Catalogue number

      if((molAOList[iIndex] as TStar).sCon <> '') then
        sText := sText + ' ' + (molAOList[iIndex] as TStar).sCon; // Add constellation

    end
    else
    begin
      sText := (molAOList[iIndex] as TInterstellarObject).sNGC; // NGC-ID
      sMessier := (molAOList[iIndex] as TInterstellarObject).sMessier;
      sText := Trim(sMessier + ' ' + sText);
    end;

    if(msLANG_ID = 'DE') then
      if(Trim(AObject.sName_DE) <> '') then
        sText := sText + ' (' + AObject.sName_DE + ')';

    if(msLANG_ID = 'EN') then
      if(Trim(AObject.sName_EN) <> '') then
        sText := sText + ' (' + AObject.sName_EN + ')';
  end;

  if(AObject.sAOType = 'S') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TStar))
  else if(AObject.sAOType = 'G') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TGalaxy))
  else if(AObject.sAOType = 'Q') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TQuasar))
  else if(AObject.sAOType = 'GC') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TGlobularCluster))
  else if(AObject.sAOType = 'N') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TNebula))
  else if(AObject.sAOType = 'PN') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TPNebula))
  else if(AObject.sAOType = 'OC') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TOpenCluster))
  else if(AObject.sAOType = 'P') and (Uppercase(sText) <> 'ERDE') and (Uppercase(sText) <> 'EARTH') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TPlanet))
  else if(AObject.sAOType = 'C') then
    CB__AO.Items.AddObject(sText,(molAOList[iIndex] as TComet));

  // Other astronomical objects
  // ...

end;

procedure TF__ASTROTOOLBOX.SelSign();
var
  i: Integer;
  sCon: string;
  bFound, bHasFound: Boolean;
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_HA) then
    exit;

  if(CB__SIGNS.ItemIndex < 0) then
    exit;

  L__ATYPE.Caption := GetSelAOTypeName();

  try
    Screen.Cursor:=crHourGlass;

    CB__AO.Clear;

    if(RB__P.Checked) or (RB__COMETS.Checked) then
    begin
      L__CONSTELL_TITLE.Visible:=false;
      L__CONSTELL.Visible:=false;

      bFound := false; bHasFound := false;

      for i:=0 to molAOList.Count-1 do
      begin
        bFound := RegisterAObject(i);
        if(bFound) then bHasFound := true;

        // After end of type sequence stop the search.
        if(bHasFound) and (not bFound) then
          break;

      end;
    end
    else if(CB__SIGNS.ItemIndex > 0) then  // NOT _All
    begin
      L__CONSTELL_TITLE.Visible:=true;
      L__CONSTELL.Visible:=true;

      sCon := Trim(Uppercase((CB__SIGNS.Items.Objects[CB__SIGNS.ItemIndex] as TSign).sCon));

      i:=0;
      bFound := false; bHasFound := false;
      while i < molAOList.Count do
      begin
        if
          ((molAOList[i] as TAObject).sAOType <> 'P') and
          ((molAOList[i] as TAObject).sAOType <> 'E') and
          ((molAOList[i] as TAObject).sAOType <> 'C') and
          ((molAOList[i] as TAObject).sAOType <> 'SUN') and
          ((molAOList[i] as TAObject).sAOType <> 'MOON') and
          ((molAOList[i] as TAObject).sAOType <> 'EARTH') then
          if (Trim((Uppercase((molAOList[i] as TInterstellarObject).sCon))) = sCon) then
            bFound := RegisterAObject(i);

        Inc(i);

        if(bFound) then bHasFound := true;

        // After end of type sequence stop the search.
        if(bHasFound) and (not bFound) then
          break;

      end;// while
    end;

  finally
    Screen.Cursor:=crDefault;
  end;

end;

procedure TF__ASTROTOOLBOX.SetupRemainingCB();
var
  i: Integer;
  AObject: TAObject;
  bIniComets: Boolean;
  bIniAsteroids: Boolean;
begin
  bIniComets := (CB__COMETS.Items.Count = 0);
  bIniAsteroids := (CB__ASTEROIDS.Items.Count = 0);

  if(bIniComets) then
  begin
    if(msLANG_ID = 'EN') then
    begin
     CB__COMETS.Items.AddObject('- Show all -',nil);
     CB__COMETS.Items.AddObject('- Suppress -',nil);
    end
    else
    begin
      CB__COMETS.Items.AddObject('- Zeige alle -',nil);
      CB__COMETS.Items.AddObject('- Zeige keine -',nil);
    end;
  end;

  if(bIniAsteroids) then
  begin
    if(msLANG_ID = 'EN') then
    begin
     CB__ASTEROIDS.Items.AddObject('Show all',nil);
     CB__ASTEROIDS.Items.AddObject('Suppress',nil);
    end
    else
    begin
      CB__ASTEROIDS.Items.AddObject('Zeige alle',nil);
      CB__ASTEROIDS.Items.AddObject('Zeige keine',nil);
    end;
  end;

  for i:=0 to molAOList.Count-1 do
  begin
    AObject := (molAOList[i] as TAObject);

    if(AObject.sAOType = 'S') and (AObject.rM > 6.5) then
      continue;

    if(AObject.sAOType = 'S') or
      (AObject.sAOType = 'G') or
      (AObject.sAOType = 'GC') or
      (AObject.sAOType = 'OC') or
      (AObject.sAOType = 'PN') or
      (AObject.sAOType = 'N') then
    begin
      continue; //!
      (*
      if((AObject as TInterstellarObject).sCon = '') then // Interstellar object with unassigned star constellation
      begin
        j:=0; bFound:=false;
        while (not bFound) and (j < molCBExt.Count) do
        begin
          CBExt := (molCBExt[j] as TCBExt);

          // Identify object and assign constellation IDs
          if(CBExt.sAOLabel = AObject.sLabel) then
          begin
            (molAOList[i] as TInterstellarObject).sCon := CBExt.sCon1;
            (molAOList[i] as TInterstellarObject).sCon2 := CBExt.sCon2;
            bFound := true;
          end;

          Inc(j);
        end;
      end;
      *)
    end // if(AOBject[i]..
    else if(AObject.sAOType = 'C') then // Put Comets into combobox CB__COMETS
    begin
      if(bIniComets) then
      begin
        if(msLANG_ID = 'DE') then
          CB__COMETS.Items.AddObject((molAOList[i] as TComet).sName_DE,(molAOList[i] as TComet))
        else
          CB__COMETS.Items.AddObject((molAOList[i] as TComet).sName_EN,(molAOList[i] as TComet));

      end;
    end
    else if(AObject.sAOType = 'P') and ((AObject as TPlanet).sPlanetType = 'A') then // Put Asteroids into combobox CB__ASTEROIDS
    begin
      if(bIniAsteroids) then
      begin
        if(msLANG_ID = 'DE') then
          CB__ASTEROIDS.Items.AddObject((AObject as TPlanet).sName_DE,(molAOList[i] as TPlanet))
        else
          CB__ASTEROIDS.Items.AddObject((AObject as TPlanet).sNAME_EN,(molAOList[i] as TPlanet));

      end;
    end;
  end; // for i..

end;

procedure TF__ASTROTOOLBOX.ImportCatalogs();
var
  i: Integer;
  sProgress, sPicFile: string;
begin
  CB__AO.Items.Clear;
  sProgress := 'l';

  molAOList.Clear;
  try
    Screen.Cursor:=crHourGlass;

    ImportCBExt(molCBExt,gsAlbireoLocalDir + 'CBExt.dat');

    sPicFile := GetRandomPictureFile();
    if(sPicFile <> '') then
      F__STARTUP.IMG__BACKGROUND.Picture.LoadFromFile(sPicFile);

    ImportCatalog_P(molAOList,miDST_HH,miUTC_HH,msLANG_ID,gsAlbireoLocalDir + 'AO-P.dat');
    //exit;
    ImportCatalog_C(molAOList,miDST_HH,miUTC_HH,msLANG_ID,gsAlbireoLocalDir + 'AO-C.dat');
    ImportCatalog_S(molAOList,gsAlbireoLocalDir + 'AO-S.dat',
      msTinyStarFirstCatNo,miTinyStarBlockWidth);

    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '30%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    ImportCatalog_GC(molAOList,msLANG_ID,gsAlbireoLocalDir + 'AO-GC.dat');
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '35%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    ImportCatalog_N(molAOList,msLANG_ID,gsAlbireoLocalDir + 'AO-N.dat');
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '40%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    ImportCatalog_PN(molAOList,msLANG_ID,miGLAT_DEG + miGLAT_MIN/60,gsAlbireoLocalDir + 'AO-PN.dat');
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '45%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    ImportCatalog_OC(molAOList,msLANG_ID,miGLAT_DEG + miGLAT_MIN/60,gsAlbireoLocalDir + 'AO-OC.dat');
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '50%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    sPicFile := GetRandomPictureFile();
    if(sPicFile <> '') then
      F__STARTUP.IMG__BACKGROUND.Picture.LoadFromFile(sPicFile);

    ImportCatalog_G(molAOList,gsAlbireoLocalDir + 'AO-G.dat');
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '60%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    ImportCatalog_Q(molAOList,msLANG_ID,gsAlbireoLocalDir + 'AO-Q.dat');
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '62%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    // ImportCatalog_P(molAOList,miDST_HH,miUTC_HH,msLANG_ID,gsAlbireoLocalDir + 'AO-P.dat');
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '65%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    //ImportCatalog_C(molAOList,miDST_HH,miUTC_HH,msLANG_ID,gsAlbireoLocalDir + 'AO-C.dat');
    (*
    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '70%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;
    *)
    PrepMilkyWay();

    ImportCatalog_MS(gsAlbireoLocalDir + 'AO-MS.dat');
    ImportSigns(molSignList,gsAlbireoLocalDir + 'Signs.dat');
    ImportEclipses(mslEclipses,gsAlbireoLocalDir);

    if(F__STARTUP <> nil) then
    begin
      F__STARTUP.L__PROGRESS.Caption := '75%';
      for i:=0 to 5 do
        F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
      F__STARTUP.Repaint;
    end;

    AddStaticAO(molAOList,msLANG_ID);

    F__STARTUP.L__PROGRESS.Caption := '76%';
    F__STARTUP.Repaint;

    SetupRemainingCB();

    F__STARTUP.L__PROGRESS.Caption := '77%';
    F__STARTUP.Repaint;

    F__STARTUP.L__PROGRESS.Caption := '78%';
    F__STARTUP.Repaint;

    F__STARTUP.L__PROGRESS.Caption := '79%';
    F__STARTUP.Repaint;

    SetupRemainingCB();

    ReCalcPlanetPos(Now);

  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TF__ASTROTOOLBOX.ImportCatalog_MS(sAOFileName: string);
{2013-12-14/fs
Import Asteroid Shower Catalogue
}
var
  i: Integer;
  slBuffer: TStringList;
  MeteorShower: TMeteorShower;
  tfAO: TextFile;
  sLine, sVar: string;
  bIsHeader: Boolean;
begin
  bIsHeader := true;

  BeginMethod('ImportCatalog_MS');

  slBuffer := TStringList.Create;
  slBuffer.Delimiter:=';';
  // Import astronomical objects catalogue
  //AssignFile(tfAO,ConvertWinPath('AO-MS.dat'));
  AssignFile(tfAO,ConvertWinPath(sAOFileName));
  Reset(tfAO);

  while not eof(tfAO) do
  begin
    MeteorShower := nil;
    slBuffer.Clear;

    ReadLn(tfAO,sLine);
    if(bIsHeader) then
    begin
      // Forget header line!
      ReadLn(tfAO,sLine);
      bIsHeader := false;
    end;

    sLine := AnsiReplaceStr(sLine,' ','~');
    SetMsg(sLine);

    slBuffer.DelimitedText:=sLine;

    for i:=0 to slBuffer.Count-1 do
    begin
      if(i = 0) then
      begin
        MeteorShower := TMeteorShower.Create;
        MeteorShower.bActive:=false;

        MeteorShower.SHP.Shape:=stCircle;
        MeteorShower.SHP.Height:=20 ;
        MeteorShower.SHP.ShowHint := true;
        //MeteorShower.SHP.Cursor := crCross;
        MeteorShower.SHP.Pen.Color := clSilver;
        MeteorShower.SHP.Brush.Color := clSilver;
        MeteorShower.SHP.Visible:=false;

        MeteorShower.IMG.Visible:=false;
        MeteorShower.IMG.ShowHint:=true;
        MeteorShower.IMG.Proportional:=true;
        MeteorShower.IMG.Stretch:=true;
        MeteorShower.IMG.Picture.LoadFromFile(ConvertWinPath(ExtractFilePath(sAOFileName) + '\img\Meteorshower.png'));
        //LoadImgRes(MeteorShower.IMG,'Meteorshower','PNG');
        MeteorShower.IMG.Transparent:=true;
        //MeteorShower.IMG.Picture.LoadFromLazarusResource('img/Meteorshower.png'); //.LoadFromFile('img\Meteorshower.png');
      end;

      sVar := slBuffer[i];

      case i of
       0: MeteorShower.sName_DE := slBuffer[i];
       1: MeteorShower.sName_EN := slBuffer[i];
       2: MeteorShower.sSign := slBuffer[i];
       3: MeteorShower.sMaxDateMonth := slBuffer[i];
       4: MeteorShower.iObjPerHour := StrToInt(sVar);
       5: if(sVar <> '-') then MeteorShower.iSpeed := StrToInt(sVar)
          else ShowMessage('Err: ' + MeteorShower.sName_DE);
       6: MeteorShower.sTime1 := slBuffer[i];
       7: MeteorShower.sTime2 := slBuffer[i];
       8: MeteorShower.sCometName := slBuffer[i];
      end; // case
    end;

    if(MeteorShower <> nil) then
    begin
      //SetAOLabel(Nebula);
      molMeteorShowers.Add(MeteorShower);
    end;

  end; // while not eof...


  CloseFile(tfAO);

  SetMsg('');
  EndMethod('ImportCatalog_MS');
end;

procedure TF__ASTROTOOLBOX.ImportSigns(olSignList: TObjectList; sAOFileName: string);
{2012-09-12/fs
// Import signs catalogue
}
var
  i: Integer;
  slBuffer: TStringList;
  //AObject: TAObject;
  Sign: TSign;
  tfAO: TextFile;
  sLine, sVar: string;
begin
  BeginMethod('ImportSigns');

  slBuffer := TStringList.Create;
  slBuffer.Delimiter:=';';

  CB__SIGNS.Items.Clear;

  if(msLANG_ID = 'DE') then
    CB__SIGNS.Items.Add('_Alle Sternbilder');
  if(msLANG_ID = 'EN') then
    CB__SIGNS.Items.Add('_All Constellations');

  //AssignFile(tfAO,ConvertWinPath(msAlbireoLocalDir + 'Signs.dat'));
  AssignFile(tfAO,ConvertWinPath(sAOFileName));
  Reset(tfAO);

  while not eof(tfAO) do
  begin
    Sign := nil;
    slBuffer.Clear;
    ReadLn(tfAO,sLine);
    sLine := AnsiReplaceStr(sLine,' ','~');

    slBuffer.DelimitedText:=sLine;
    for i:=0 to slBuffer.Count-1 do
    begin
      if(i = 0) then Sign := TSign.Create;
      Sign.bSelected:=false;

      sVar := AnsiReplaceStr(slBuffer[i],'~',' ');

      case i of
        0: Sign.sCon := sVar;
        1:
        begin

          sVar := AnsiReplaceStr(sVar,'ae','ä');
          sVar := AnsiReplaceStr(sVar,'oe','ö');

          if(not AnsiContainsStr(sVar,'aue')) then // protect 'Bildhauer'
            sVar := AnsiReplaceStr(sVar,'ue','ü');

          sVar := AnsiReplaceStr(sVar,'AE','Ä');
          sVar := AnsiReplaceStr(sVar,'OE','Ö');
          sVar := AnsiReplaceStr(sVar,'UE','Ü');

          Sign.sConDE := sVar;
        end;
        3: Sign.sConEN := sVar;
      end; // case
    end; // for
    if(Sign <> nil) then
    begin
      if(msLANG_ID = 'DE') then
      begin
        CB__SIGNS.Items.AddObject(Sign.sCon + ' (' + Sign.sConDE + ')',Sign);
      end;

      if(msLANG_ID = 'EN') then
      begin
        CB__SIGNS.Items.AddObject(Sign.sCon + ' (' + Sign.sConEN + ')',Sign);
      end;

      olSignList.Add(Sign);
    end;
  end; // while

  CloseFile(tfAO);
  CB__SIGNS.ItemIndex:=0;

  slBuffer.Destroy;

  EndMethod('ImportSigns');
end;

procedure TF__ASTROTOOLBOX.SignActive(iSignIndex: Integer; bActive: Boolean);
// Set bActive PMENU__DAY of a Sign and re-generate the starmap
begin
  if(iSignIndex > -1) and (iSignIndex < molSignList.Count) then
    (molSignList[iSignIndex] as TSign).bSelected := bActive;
end;

procedure TF__ASTROTOOLBOX.SendUpdateReq;
begin
  (*
  HTTPClient.Host := gcsUpdateHost;
  HTTPClient.URI := gcsUpdateVersionURI;
  HTTPClient.Port := 80;
  ShowMessage('Host: ' + gcsUpdateHost + ', URI: ' + HTTPClient.URI);
  HTTPClient.SendRequest;
  *)
end;

procedure TF__ASTROTOOLBOX.SwitchLang();
begin
  try
    Screen.Cursor := crHourGlass;

    if(msLANG_ID = 'DE') then
    begin
      msLANG_ID := 'EN';
      BB__SWITCH_LANG.Caption := 'DE < Sprache wechseln > EN';
      if(CB__SIGNS.Text = '_Alle Sternbilder') then
        CB__SIGNS.Text := '_All Constellations';

      CB__SIGNS.Items[0] := '_All Constellations';

      MENU__LCLASS_0.Caption := '0 - Hypergiants';
      MENU__LCLASS_1.Caption := 'I - Supergiants';
      MENU__LCLASS_2.Caption := 'II - Bright Giants';
      MENU__LCLASS_3.Caption := 'III - Giants';
      MENU__LCLASS_4.Caption := 'IV - Subgiants';
      MENU__LCLASS_5.Caption := 'V - Dwarfs';
      MENU__LCLASS_6.Caption := 'VI/sd - Subdwarfs';
      MENU__LCLASS_7.Caption := 'VII/D - White Dwarfs';

      P__ALBIREO_INFO.Caption:= FormatDateTime('MMMM DD, YYYY',Now);
    end
    else
    begin
      msLANG_ID := 'DE';
      BB__SWITCH_LANG.Caption := 'DE < Switch Language > EN';
      if(CB__SIGNS.Text = '_All Constellations') then
        CB__SIGNS.Text := '_Alle Sternbilder';

      CB__SIGNS.Items[0] := '_Alle Sternbilder';

      MENU__LCLASS_0.Caption := '0 - Hyperriesen';
      MENU__LCLASS_1.Caption := '1 - Überriesen';
      MENU__LCLASS_2.Caption := 'II - Helle Riesen';
      MENU__LCLASS_3.Caption := 'III - Riesen';
      MENU__LCLASS_4.Caption := 'IV - Unterriesen';
      MENU__LCLASS_5.Caption := 'V - Zwerge';
      MENU__LCLASS_6.Caption := 'VI/sd - Unterzwerge';
      MENU__LCLASS_7.Caption := 'VII/D - Weiße Zwerge';

      P__ALBIREO_INFO.Caption:= FormatDateTime('d. mmmm yyyy',Now);
    end;

    IniText(F__ASTROTOOLBOX,msLANG_ID);
    //ImportCatalogs(); // Memory Leak
    ShowLatLong();

    miMSDoneMonth := 0; ShowMeteoriteShowerGrid(GetWTime());

    ShowAOTable();
    IniCB_ADEV_TYPE();

    IniTelProp();

    if(CB__SIGNS.Items.Count > 0) then
      CB__SIGNS.ItemIndex:=0;

    case PC__WORKBENCH.ActivePageIndex of
      ciPAGE_STARMAP: CleanStartOfStarmap();
    end;

  finally
    Screen.Cursor := crDefault;
  end;
end;

function TF__ASTROTOOLBOX.GetHTTPMetaValue(sHTML, sName: string): string;
{
<html>
<head>
<meta name="subversion" value="1">
</head>
<body>
<H1>HTTP Error 403 - Forbidden</H1>
</body>
</html>
}
var
  sNum: string;
  i,iPos: Integer;
  sMETA: string;
begin
  (*
  sHTML :=
    '<html>' +
    '<head>' +
    '<meta name="subversion" value="1">' +
    '</head>' +
    '<body>' +
    '<H1>HTTP Error 403 - Forbidden</H1>' +
    '</body>' +
    '</html>';
  *)
  sHTML := AnsiReplaceStr(sHTML,'''','"');

  sMETA := '<meta name="' + sName + '" value="';
  iPos := Pos(sMETA,sHTML);

  Result := '';
  if(iPos > 0) then
  begin
    i:=0;
    while (sHTML[iPos+length(sMETA)+i] <> '"') and (iPos+length(sMETA)+i < length(sHTML)) do
    begin
      sNum := sHTML[iPos+length(sMETA)+i];
      Result := Result + sNum;
      Inc(i);
    end;
  end;

end;

procedure TF__ASTROTOOLBOX.CheckForUpdate(sHTML: string);
var
  sSubversion: string;
  iSubVersion: Integer;
  sUpdateURL: string;
begin
  sSubversion := GetHTTPMetaValue(sHTML,'subversion');
  if(Trim(sSubVersion) = '') then exit;

  iSubversion := StrToInt(sSubVersion);
  sUpdateURL := 'http://' + gcsUpdateHost + gcsUpdateURI;
  if(StrToInt(gcsSubVersion) < iSubVersion) then
  begin
    ExecOpen(sUpdateURL);
  end;

end;

procedure TF__ASTROTOOLBOX.SetSlideTime();
begin
  TB__TIME_24H.Position := (StrToInt(ED__WT_HH.Text))*60 + (StrToInt(ED__WT_MM.Text));
end;

procedure TF__ASTROTOOLBOX.SlideTime;
var
  dtTime: TDateTime;
  iWT_HH, iWT_MM, iWT_SS, iWT_MS: Word;
begin
  if(TB__TIME_24H.Position >= 1440) then exit;

  dtTime := TB__TIME_24H.Position/1440.0;

  DecodeTime(dtTime,iWT_HH, iWT_MM, iWT_SS, iWT_MS);

  ED__WT_HH.Text := format('%.2d',[iWT_HH]);
  ED__WT_MM.Text := format('%.2d',[iWT_MM]);
  ED__WT_SS.Text := format('%.2d',[iWT_SS]);

  TrigST();
end;

procedure TF__ASTROTOOLBOX.CalcRZ(iST_HH, iST_MM, iST_SS, iHA_HH, iHA_MM, iHA_SS: Word);
var
  dtHA: TDateTime;
  dtRZ: TDateTime;
  dtST: TDateTime;
  iRZ_HH, iRZ_MM, iRZ_SS, iRZ_MS: Word;
begin
  dtST := EncodeTime(iST_HH, iST_MM, iST_SS,00);
  dtHA := EncodeTime(iHA_HH, iHA_MM, iHA_SS,00);
  dtRZ := dtST - dtHA;

  DeCodeTime(dtRZ,iRZ_HH, iRZ_MM, iRZ_SS, iRZ_MS);

  if(iRZ_SS >= 60) then
  begin
    iRZ_MM := iRZ_MM + iRZ_SS div 60;
    iRZ_SS := iRZ_SS - 60*(iRZ_SS div 60);
  end;

  ED__RZ_HH.Text := format('%.2d',[iRZ_HH]);//IntToStr(iRZ_HH);
  ED__RZ_MM.Text := format('%.2d',[iRZ_MM]);//IntToStr(iRZ_MM);
  ED__RZ_SS.Text := format('%.2d',[iRZ_SS]);//IntToStr(iRZ_SS);
end;

procedure TF__ASTROTOOLBOX.CalcHA(iST_HH, iST_MM, iST_SS, iRZ_HH, iRZ_MM, iRZ_SS: Word);
var
  dtHA: TDateTime;
  dtRZ: TDateTime;
  dtST: TDateTime;
  iHA_HH, iHA_MM, iHA_SS, iHA_MS: Word;
  rAz,rHgt: Real;
  iAZ_DEG, iAZ_MM: SmallInt;
  rAZ_SS: Real;
  iHGT_DEG: SmallInt;
  iHGT_MM: SmallInt;
  rHGT_SS: Real;
  iDEC_DEG, iDEC_MM, iDEC_SS: SmallInt;
begin
  if (PC__WORKBENCH.ActivePageIndex = ciPAGE_HA) then
  begin
    if(iST_SS >= 60) then
    begin
      iST_MM := iST_MM + iST_SS div 60;
      iST_SS := iST_SS - 60*(iST_SS div 60);
    end;

    if(iRZ_SS >= 60) then
    begin
      iRZ_MM := iRZ_MM + iRZ_SS div 60;
      iRZ_SS := iRZ_SS - 60*(iRZ_SS div 60);
    end;

    dtST := EncodeTime(iST_HH, iST_MM, iST_SS, 00);
    dtRZ := EncodeTime(iRZ_HH, iRZ_MM, iRZ_SS, 00);
    dtHA := GetHA(dtST,dtRZ);

    DeCodeTime(dtHA,iHA_HH, iHA_MM, iHA_SS, iHA_MS);

    iDEC_DEG := StrToInt(ED__DEC_DEG.Text);
    iDEC_MM := StrToInt(ED__DEC_MM.Text);
    iDEC_SS := StrToInt(ED__DEC_SS.Text);

    // Height / Azimuth Calculation
    rAz:=0; rHgt:=0;
    CalcAZ(iDEC_DEG,iDEC_MM,iDEC_SS,
      iHA_HH,iHA_MM,iHA_SS,
      rAz,rHgt);

    iAZ_DEG:=0; iAZ_MM:=0; rAZ_SS:=0;
    iHGT_DEG:=0; iHGT_MM:=0; rHGT_SS:=0;

    DegToDEG_MM_SS2(rAz,iAZ_DEG, iAZ_MM, rAZ_SS);
    DegToDEG_MM_SS2(rHgt,iHGT_DEG, iHGT_MM, rHGT_SS);

    ED__HA_HH.Text := format('%.2d',[iHA_HH]);//IntToStr(iHA_HH);
    ED__HA_MM.Text := format('%.2d',[iHA_MM]);//IntToStr(iHA_MM);
    ED__HA_SS.Text := format('%.2d',[iHA_SS]);//IntToStr(iHA_SS);

    ED__AZ_DEG.Text := format('%.3d',[iAZ_DEG]);//IntToStr(iAZ_DEG);
    ED__AZ_MM.Text := format('%.2d',[iAZ_MM]);//IntToStr(iAZ_MM);
    ED__AZ_SS.Text := format('%.2d',[Round(rAZ_SS)]);//IntToStr(Round(rAZ_SS));

    ED__HGT_HH.Text := format('%.2d',[abs(iHGT_DEG)]);//IntToStr(abs(iHGT_DEG)); // Minus is displayed seperately under fct. CalcAZ(..)
    ED__HGT_MM.Text := format('%.2d',[iHGT_MM]);//IntToStr(iHGT_MM);
    ED__HGT_SS.Text := format('%.2d',[Round(rHGT_SS)]);//IntToStr(Round(rHGT_SS));

    // Generate Goto Output data
    if(CBX__GOTOOUT.Checked) then
    begin
      if(iDEC_DEG >= 0) then
      begin
        WriteTextFile2(FloatToStrF(dtRZ*24.0,ffFixed,8,4),FloatToStrF(iDEC_DEG + iDEC_MM/60.0 + iDEC_SS/3600.0,ffFixed,8,4),msGotoOutputDir + 'GotoRA_DEC.txt');
        WriteTextFile2(FloatToStrF(dtHA*24.0,ffFixed,8,4),FloatToStrF(iDEC_DEG + iDEC_MM/60.0 + iDEC_SS/3600.0,ffFixed,8,4),msGotoOutputDir + 'GotoHA_DEC.txt');
      end
      else
      begin
        WriteTextFile2(FloatToStrF(dtRZ*24.0,ffFixed,8,4),FloatToStrF(iDEC_DEG - iDEC_MM/60.0 - iDEC_SS/3600.0,ffFixed,8,4),msGotoOutputDir + 'GotoRA_DEC.txt');
        WriteTextFile2(FloatToStrF(dtHA*24.0,ffFixed,8,4),FloatToStrF(iDEC_DEG - iDEC_MM/60.0 - iDEC_SS/3600.0,ffFixed,8,4),msGotoOutputDir + 'GotoHA_DEC.txt');
      end;

      WriteTextFile2(FloatToStrF(rAz,ffFixed,8,4),FloatToStrF(rHgt,ffFixed,8,4),msGotoOutputDir + 'GotoAZ_HGT.txt');

    end;

  end;

  // Polaris-Kochab representation
  CalcKochabMethod();

end;

function TF__ASTROTOOLBOX.GetWTime(): TDateTime;
var
  iYY, iMTH, iDD, iHH, iMM, iSS: Word;
begin
  iHH := (StrToInt(ED__WT_HH.Text));
  iMM := (StrToInt(ED__WT_MM.Text));
  iSS := (StrToInt(ED__WT_MM.Text));

  DecodeDate(CB__WT.Date,iYY,iMTH,iDD);

  Result := EncodeDateTime(iYY,iMTH,iDD,iHH,iMM,iSS,0);
end;

procedure TF__ASTROTOOLBOX.SetTime(dtDateTime: TDateTime);
var
  iHH, iMM, iSS, iMS: Word;
begin
  CB__WT.Date := dtDateTime;

  DecodeTime(dtDateTime,iHH, iMM, iSS, iMS);

  ED__WT_HH.Text := format('%.2d',[iHH]);
  ED__WT_MM.Text := format('%.2d',[iMM]);
  ED__WT_SS.Text := format('%.2d',[iSS]);

  SetSlideTime();

  TrigST();
end;

procedure TF__ASTROTOOLBOX.TrigST();
var
  dJulDat: Double;
  dtWT, dtST: TDateTime;
  iHH, iMM, iSS, iMS: Word;
begin
  dJulDat := 0;

  dtWT := GetWTime();
  dtST := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat);

  DecodeTime(dtST,iHH, iMM, iSS, iMS);

  CalcHA(
    iHH,
    iMM,
    iSS,
    StrToInt(ED__RZ_HH.Text),
    StrToInt(ED__RZ_MM.Text),
    StrToInt(ED__RZ_SS.Text)
    );

end;

procedure TF__ASTROTOOLBOX.CalcST(var iST_HH: Word;var iST_MM: Word; var iST_SS: Word; var iST_MS: Word);
var
  dJulDat: Double;
  dtWT, dtST: TDateTime;
begin
  dJulDat := 0;

  dtWT := GetWTime();
  dtST := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat);

  DecodeTime(dtST,iST_HH, iST_MM, iST_SS, iST_MS);

  CalcHA(
    iST_HH,
    iST_MM,
    iST_SS,
    StrToInt(ED__RZ_HH.Text),
    StrToInt(ED__RZ_MM.Text),
    StrToInt(ED__RZ_SS.Text)
    );

  (*
  ED__ST_HH.Text := format('%.2d',[iHH]);//IntToStr(iHH);
  ED__ST_MM.Text := format('%.2d',[iMM]);//IntToStr(iMM);
  ED__ST_SS.Text := format('%.2d',[iSS]);//IntToStr(iSS);

  ED__JT.Text := FloatToStr(dJulDat);

  CalcHA(
    StrToInt(ED__ST_HH.Text),
    StrToInt(ED__ST_MM.Text),
    StrToInt(ED__ST_SS.Text),
    StrToInt(ED__RZ_HH.Text),
    StrToInt(ED__RZ_MM.Text),
    StrToInt(ED__RZ_SS.Text)
    );
  *)
end;

function TF__ASTROTOOLBOX.ShowPrefs(): Boolean;
begin
  Result := false;

  F__PREFS := TF__PREFS.Create(nil);
  F__PREFS.msAlbireoLocalDir:=gsAlbireoLocalDir;

  F__PREFS.miGLat_DEG := miGLat_DEG;
  F__PREFS.miGLat_MIN := miGLat_MIN;
  F__PREFS.miGLng_DEG := miGLng_DEG;
  F__PREFS.miGLng_MIN := miGLng_MIN;
  F__PREFS.miDST_HH := miDST_HH_DEF;
  F__PREFS.miUTC_HH := miUTC_HH;
  F__PREFS.msLANG_ID := msLANG_ID;
  F__PREFS.miBirthYear := miBirthYear;
  F__PREFS.miRefreshRateMinutes := miRefreshRateMinutes;
  F__PREFS.miFirstStart := miFirstStart;
  F__PREFS.msCity:=msCity;
  F__PREFS.msCountry:=msCountry;
  F__PREFS.msDST:=msDST;
  F__PREFS.mbHDust:=mbHDust;
  F__PREFS.miLThickness := miLThickness;
  F__PREFS.miLandscapeNo := miLandscapeNo;
  F__PREFS.msGotoOutputDir:=msGotoOutputDir;
  F__PREFS.CBX__LOWHIGHRES.Checked:=(giRSCLvl = 0);

  if(F__PREFS.ShowModal = mrOK) then
  begin
    miGLat_DEG := F__PREFS.miGLat_DEG;
    miGLat_MIN := F__PREFS.miGLat_MIN;
    miGLng_DEG := F__PREFS.miGLng_DEG;
    miGLng_MIN := F__PREFS.miGLng_MIN;
    miDST_HH_DEF := F__PREFS.miDST_HH;
    miUTC_HH := F__PREFS.miUTC_HH;
    msLANG_ID := F__PREFS.msLANG_ID;
    miBirthYear := F__PREFS.miBirthYear;
    miRefreshRateMinutes := F__PREFS.miRefreshRateMinutes;
    miFirstStart := F__PREFS.miFirstStart;
    msCity := F__PREFS.msCity;
    msCountry := F__PREFS.msCountry;
    msDST := F__PREFS.msDST;
    mbHDust := F__PREFS.mbHDust;
    miLThickness := F__PREFS.miLThickness;
    miLandscapeNo := F__PREFS.miLandscapeNo;
    msGotoOutputDir := F__PREFS.msGotoOutputDir;

    if(F__PREFS.CBX__LOWHIGHRES.Checked) then
      giRSCLvl := 0
    else
      giRSCLvl := 1;

    case miLandscapeNo of
      0:
      begin
        MENU__HOR_0.Checked:=true;
        MENU__HOR_1.Checked:=false;
        MENU__HOR_2.Checked:=false;
      end;
      1:
      begin
        MENU__HOR_0.Checked:=false;
        MENU__HOR_1.Checked:=true;
        MENU__HOR_2.Checked:=false;
      end;
      2:
      begin
        MENU__HOR_0.Checked:=false;
        MENU__HOR_1.Checked:=false;
        MENU__HOR_2.Checked:=true;
      end;
    end; // case

    // Language switching?
    if(F__PREFS.mbSwitchLANG) then
    begin
      // Adapt all visible Texts to German / English
      IniText(F__ASTROTOOLBOX,msLANG_ID);
    end;

    Result := true;
  end;

  F__PREFS.Destroy;

end;

procedure TF__ASTROTOOLBOX.TIMERTimer(Sender: TObject);
var
  dJulDat: Double;
  dtTime: TDateTime;
begin
  dJulDat := 0;
  dtTime := Now;

  //mdtStart0 := mdtStart0 + miTimePlayMode/96;  // 1/4 h

  mdtStart0 := mdtStart0 + miTimePlayMode/(24*60)*1;  // 1 Min
  (*
  if(miTimePlayMode > 0) then
    mdtStart0 := mdtStart0 + miTimePlayMode/48;

  if(miTimePlayMode < 0) then
    mdtStart0 := mdtStart0 + miTimePlayMode/48;
  *)

  if((Trunc(dtTime*24*3600) mod 2) = 0) then
    mbWheeled := false;

  if(msLANG_ID = 'EN') then
  begin
    SB__MAIN.Panels[0].Text:='Time: ' + TimeToStr(dtTime);
    SB__MAIN.Panels[1].Text:='Siderial Time: ' + TimeToStr(GetSIDTime(dtTime,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat));
    SB__MAIN.Panels[2].Text:='Julian Time: ' + FloatToStr(dJulDat);
  end
  else
  begin
    SB__MAIN.Panels[0].Text:='Zeit: ' + TimeToStr(dtTime);
    SB__MAIN.Panels[1].Text:='Sternzeit: ' + TimeToStr(GetSIDTime(dtTime,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat));
    SB__MAIN.Panels[2].Text:='Julianische Zeit: ' + FloatToStr(dJulDat);
  end;

  if(mbTimePlay) then
    SetTime(mdtStart0 + dtTime - mdtStart);

  if(CBX__LT.Checked) and (miAOTableSec >= 0)  then
  begin
    L__LT_INTERVAL.Caption := Format('%.2d',[miAOTableSec div 60]) + ':' + Format('%.2d',[miAOTableSec - 60*(miAOTableSec div 60)]);
    Dec(miAOTableSec);
  end;
end;

procedure TF__ASTROTOOLBOX.TIMER__AOTABLETimer(Sender: TObject);
begin
  case PC__WORKBENCH.ActivePageIndex of
    ciPAGE_DB: begin ShowAOTable(); miAOTableSec := TB__LT.Position*60; end;
  end;
end;

procedure TF__ASTROTOOLBOX.TB__SELMAGMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.TIMER__STRM_DATATimer(Sender: TObject);
begin
  TimerStrmData();
end;

procedure TF__ASTROTOOLBOX.TIMER__STRM_VIEWFLIPTimer(Sender: TObject);
begin
  TimerStrmViewFlip();
end;

procedure TF__ASTROTOOLBOX.TIMER__WHEELZOOMTimer(Sender: TObject);
begin
  //mbExecWheelZoom := false;
  TIMER__WHEELZOOM.Enabled:=false;
end;

procedure TF__ASTROTOOLBOX.IniCB_ADEV_TYPE();
begin
  CB__ADEV_TYPE.Items.Clear;
  if(msLANG_ID = 'DE') then
  begin
    CB__ADEV_TYPE.Items.Add('Maksutov');
    CB__ADEV_TYPE.Items.Add('Maksutov-Newton');
    CB__ADEV_TYPE.Items.Add('Ritchey-Chretien');
    CB__ADEV_TYPE.Items.Add('Reflektor-Newton');
    CB__ADEV_TYPE.Items.Add('Refraktor-Achromat');
    CB__ADEV_TYPE.Items.Add('Refraktor-Apochromat');
    CB__ADEV_TYPE.Items.Add('Refraktor-Chromat');
    CB__ADEV_TYPE.Items.Add('Refraktor-ED');
    CB__ADEV_TYPE.Items.Add('Schmidt-Cassegrain');
  end
  else
  begin
    CB__ADEV_TYPE.Items.Add('Macsutov');
    CB__ADEV_TYPE.Items.Add('Macsutov-Newton');
    CB__ADEV_TYPE.Items.Add('Ritchey-Chretien');
    CB__ADEV_TYPE.Items.Add('Reflector-Newton');
    CB__ADEV_TYPE.Items.Add('Refractor-Achromat');
    CB__ADEV_TYPE.Items.Add('Refractor-Apochromat');
    CB__ADEV_TYPE.Items.Add('Refractor-Chromat');
    CB__ADEV_TYPE.Items.Add('Refractor-ED');
    CB__ADEV_TYPE.Items.Add('Schmidt-Cassegrain');
  end;
end;

procedure TF__ASTROTOOLBOX.FormCreate(Sender: TObject);
begin
  {$IFDEF Darwin}
  mbOnStarmapPaintBusy := false;
  mbOnSolSysPaintBusy := false;
  {$ENDIF Darwin}

  giRSCLvl := 1; // Begin with high resource mode by default.

  mslEclipses := TStringList.Create;

  gsAlbireoLocalDir := GetLocalUserAppDataPath() + '\Albireo\';

  grecAOIndexControl.iMaxMessier:=0;
  grecAOIndexControl.iMax_Q:=0;
  grecAOIndexControl.iMin_Q:=0;
  grecAOIndexControl.iMax_G:=0;
  grecAOIndexControl.iMin_G:=0;
  grecAOIndexControl.iMax_GC:=0;
  grecAOIndexControl.iMin_GC:=0;
  grecAOIndexControl.iMax_OC:=0;
  grecAOIndexControl.iMin_OC:=0;
  grecAOIndexControl.iMax_N:=0;
  grecAOIndexControl.iMin_N:=0;
  grecAOIndexControl.iMax_PN:=0;
  grecAOIndexControl.iMin_PN:=0;

  mPng_O := TPortableNetworkGraphic.Create;
  mPng_O.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\StandardStar_O.png'));
  mPng_O.Transparent:=true; mPng_O.TransparentMode:=tmFixed;
  mPng_B := TPortableNetworkGraphic.Create;
  mPng_B.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\StandardStar_B.png'));
  mPng_B.Transparent:=true; mPng_B.TransparentMode:=tmFixed;
  mPng_A := TPortableNetworkGraphic.Create;
  mPng_A.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\StandardStar_A.png'));
  mPng_A.Transparent:=true; mPng_A.TransparentMode:=tmFixed;
  mPng_F := TPortableNetworkGraphic.Create;
  mPng_F.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\StandardStar_F.png'));
  mPng_F.Transparent:=true; mPng_F.TransparentMode:=tmFixed;
  mPng_G := TPortableNetworkGraphic.Create;
  mPng_G.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\StandardStar_G.png'));
  mPng_G.Transparent:=true; mPng_G.TransparentMode:=tmFixed;
  mPng_K := TPortableNetworkGraphic.Create;
  mPng_K.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\StandardStar_K.png'));
  mPng_K.Transparent:=true; mPng_K.TransparentMode:=tmFixed;
  mPng_M := TPortableNetworkGraphic.Create;
  mPng_M.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'img\StandardStar_M.png'));
  mPng_M.Transparent:=true; mPng_M.TransparentMode:=tmFixed;

  msRAVal := ''; msDECVal := '';
  miStarmapView := 0;

  miPlComPathWeeks := 4;
  miLandscapeNo := 0;
  mrS0 := 0;
  msGotoOutputDir := 'C:\';

  MENU__HOR_0.Checked:=true;

  mslAsteroidsCometsDisplayed := TStringList.Create;
  mslPlanetPlot := TStringList.Create;
  mslCometPlot := TStringList.Create;

  molMWData := TObjectList.Create();
  molMWData.OwnsObjects:=true;

  molAOList := TObjectList.Create;
  molAOList.OwnsObjects:=true;

  mslVisibleAOList := TStringList.Create;
  molSignList := TObjectList.Create;
  molADevList := TObjectList.Create;
  molMeteorShowers := TObjectList.Create;
  molCBExt := TObjectList.Create;

  miGLng_DEG := 999;
  miGLat_DEG := 999;

  msHorDir := 'S';

  msTinyStarFirstCatNo := '';
  miTinyStarBlockWidth := 0;

  miTimePlayMode := 0;
  mbSearch := false;

  RB__S.Font.Color:=clGray;
  miFirstStart := 1;
  miBirthYear := 1970;
  miDST_HH_DEF := 1;
  mbHDust := true;
  miLThickness := 1;

  mbPlanetShow := true;
  mbPlanetImage := true;
  mbSM_MERI := false;
  mbSM_RA := false;
  mbSM_DEC := false;
  mbShowPoleP := false;

  mbZoomMode := false;
  mbFastZoom := false;
  mbWheeled := false;
  miZoomLvl := 0;
  mbExecZoom := false;

  mbADEVChanged := false;

  Caption := 'Albireo Astronomy ToolBox ' + gcsAlbireoVersion;

  mbShowGalaxies := false;
  mbShowCB := false;
  mbShowEComets := false;
  miShowMilkyway := 1;
  mbShowAsteroids := false;
  miShowStarDescr := 1; // Start with low star description level
  PMENU__STAR_DESCR_LOW.Checked:=true;
  PMENU__STAR_DESCR_HIGH.Checked:=false;
  mbShowConstellations := false;

  mslColType := TStringList.Create;
  miSCI_GRD_AO := 0;//-1;
  miSCO_GRD_AO := -1;

  mslAOUserFields := TStringList.Create;
  mslRecommendedPics := TStringList.Create;
  mslDSImgList := TStringList.Create;

  ReadDSImgList();

  // Free version restrictions
  B__ASTROCALC.Visible:=true;
  MENU__ADB.Visible:=true; // Available for developers only to adjust asteroid ephemerides
  MENU__COMMENT.Visible:=true;
  RB__MESSIER.Visible:=true;
  MENU__STRM.Visible:=true;

  TS__MAG_GAL.TabVisible := false;

  msStrmDir := 'C:';
  miStrmDataPeriod := 60;
  miStrmViewFlipPeriod := 0; // Viewflip stopped

  mrEyeFacH := crEyeFacH_70;
end;

procedure TF__ASTROTOOLBOX.FormDestroy(Sender: TObject);
begin
  if(miFirstStart = 1) then exit;

  mslEclipses.Free;
  molMWData.Free;
  molMeteorShowers.Free;
  molSignList.Free;
  mslVisibleAOList.Free;
  //molAOList.Free;
  molADevList.Free;
  molCBExt.Free;
  mslAsteroidsCometsDisplayed.Free;
  mslColType.Free;
  mslPlanetPlot.Free;
  mslCometPlot.Free;

  mslDSImgList.free;
  mslRecommendedPics.Free;
  mslAOUserFields.Free;

  mPng_O.Free;
  mPng_B.Free;
  mPng_A.Free;
  mPng_F.Free;
  mPng_G.Free;
  mPng_K.Free;
  mPng_M.Free;

  molAOList.Free;
end;

procedure TF__ASTROTOOLBOX.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case PC__WORKBENCH.ActivePageIndex of
    ciPAGE_STARMAP:
    begin
      if(Key = VK_ESCAPE) then
      begin
        if(mbZoomMode) then
        begin
          mrZoomHgtMax := -999;
          mrZoomHgtMin := -999;
          mrZoomAzMax := -999;
          mrZoomAzMin := -999;
          mbZoomMode := false;
          mbFastZoom := false;

          P__FASTZOOM.Visible := mbZoomMode;

          mrZoomX1 := 0;
          mrZoomY1 := 0;
          miX_P := 0;
          miY_P := 0;

          mrMagPos := crMagPosStd;
          mrMagPos_G := crMagPosStd_G;
          L__MAGSIZE.Caption:=AnsiReplaceStr(FloatToStrF(mrMagPos,ffFixed,8,1),',','.');
          TB__MAG.Position:=Round(10*mrMagPos);

          TIMER__GENMAP.Enabled:=false;
          SetGenMapInterval(false);

          CleanStartOfStarMap();
          //GenMap(P__STARMAP);
        end
        else if(mbTimePlay) then
          SwitchTimePlay();

      end; // Key = VK_ESCAPE..
    end;
  end; // case
  (*
  case PC__WORKBENCH.ActivePageIndex of
    ciPAGE_STARMAP:
    begin
      if (Key = VK_F5) then
       CleanStartOfStarMap();

      if(Key = VK_ESCAPE) then
      begin
        if(mbZoomMode) then
        begin
          mrZoomHgtMax := -999;
          mrZoomHgtMin := -999;
          mrZoomAzMax := -999;
          mrZoomAzMin := -999;
          mbZoomMode := false;
          mbFastZoom := false;

          P__FASTZOOM.Visible := mbZoomMode;

          mrZoomX1 := 0;
          mrZoomY1 := 0;
          miX_P := 0;
          miY_P := 0;

          mrMagPos := crMagPosStd;
          mrMagPos_G := crMagPosStd_G;
          L__MAGSIZE.Caption:=AnsiReplaceStr(FloatToStrF(mrMagPos,ffFixed,8,1),',','.');
          TB__MAG.Position:=Round(10*mrMagPos);

          TIMER__GENMAP.Enabled:=false;
          SetGenMapInterval(false);

          CleanStartOfStarMap();
          //GenMap(P__STARMAP);
        end
        else if(mbTimePlay) then
          SwitchTimePlay();

      end; // Key = VK_ESCAPE..
    end;
    ciPAGE_SOLSYS:
      if (Key = VK_F5) then
        ShowSolSys();
  end; // case
  *)
end;

procedure TF__ASTROTOOLBOX.FormResize(Sender: TObject);
begin
  case PC__WORKBENCH.ActivePageIndex of
    ciPAGE_STARMAP:
    begin
      if(not mbTimePlay) then
        SwitchTimePlay();

      SetGenMapInterval(true); //ciGraphFastRefreshTime; // Strange. MUST be set to repaint the frame.
    end;
    ciPAGE_SOLSYS:
    begin
      SetGenMapInterval(true); //ciGraphFastRefreshTime;
      TIMER__GENMAP.Enabled:=true;
      TIMER__GENMAP.Tag:=1; // Stop after 1x execution
    end;
  end;

  SetTimePanels();
  GenTimeScale(false);
end;

procedure TF__ASTROTOOLBOX.IniCountries();
var
  i: Integer;
  slBuffer: TStringList;
  sCityFileName: string;
begin
  CB__COUNTRIES.Clear;
  CB__CITIES.Clear;

  if(msLANG_ID = 'DE') then
  begin
    CB__COUNTRIES.Items.Add('-Länder-');
    sCityFileName := gsAlbireoLocalDir + 'CityCoo.txt';
  end
  else
  begin
    CB__COUNTRIES.Items.Add('-Countries-');
    sCityFileName := gsAlbireoLocalDir + 'CityCoo_EN.txt';
  end;

  slBuffer := TStringList.Create;
  slBuffer.Sorted := true;

  GetCountryStringList(msLANG_ID,slBuffer,sCityFileName);

  for i:=0 to slBuffer.Count-1 do
    CB__COUNTRIES.Items.Add(slBuffer[i]);

  slBuffer.Free;

  CB__COUNTRIES.ItemIndex:=0;
end;

procedure TF__ASTROTOOLBOX.FormShow(Sender: TObject);
var
  dtWT: TDateTime;
  IniFile: TIniFile;
  sProgress: string;
  i: Integer;
  sPicFile: string;
begin
  sProgress := '';

  if(not DirectoryExists(gsAlbireoLocalDir)) then
    CreateDir(gsAlbireoLocalDir);

  if(FileExists(gsAlbireoLocalDir + 'ATB.ini')) then
  begin
    F__STARTUP := TF__STARTUP.Create(nil);
    F__STARTUP.Show;
    F__STARTUP.L__INFO.Caption := 'Loading prefernces...';

    sProgress := 'l';

    F__STARTUP.L__PROGRESSBAR.Caption := sProgress;

    F__STARTUP.L__PROGRESS.Caption := '5%';
    for i:=0 to 5-1 do
      F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;

    F__STARTUP.Repaint;

    IniFile := TIniFile.Create(gsAlbireoLocalDir + 'ATB.ini');

    miGLat_DEG := IniFile.ReadInteger('CONF','GLAT_DEG',53);
    miGLat_MIN := IniFile.ReadInteger('CONF','GLAT_MIN',40);

    mrSin_fGLat := sin(Pi*(miGLat_DEG + miGLat_MIN/60.0)/180.0);
    mrCos_fGLat := cos(Pi*(miGLat_DEG + miGLat_MIN/60.0)/180.0);

    miGLng_DEG := IniFile.ReadInteger('CONF','GLNG_DEG',10);
    miGLng_MIN := IniFile.ReadInteger('CONF','GLNG_MIN',47);
    miDST_HH_DEF := IniFile.ReadInteger('CONF','DST_HH',1);
    miUTC_HH := IniFile.ReadInteger('CONF','UTC_HH',1);
    msGotoOutputDir := IniFile.ReadString('CONF','GOTOOUTDIR','C:\');

    msLANG_ID := IniFile.ReadString('USER','LANGUAGE','DE');
    msCountry := IniFile.ReadString('USER','COUNTRY','');
    msCity := IniFile.ReadString('USER','CITY','');
    msDST := IniFile.ReadString('USER','DST','MESZ');

    miBirthYear := IniFile.ReadInteger('USER','YEAROFBIRTH',gciBirthYear);
    miRefreshRateMinutes := IniFile.ReadInteger('CONF','REFRESHRATE_MINUTES',5);

    msUserPath := IniFile.ReadString('USER','PATH','');

    mbHDust := (IniFile.ReadInteger('CONF','HDUST',1) = 1);
    miLThickness := IniFile.ReadInteger('CONF','LTHICKNESS',1);
    PMENU__LT_1.Checked:=(miLThickness = 1);
    PMENU__LT_2.Checked:=(miLThickness = 2);
    PMENU__LT_3.Checked:=(miLThickness = 3);

    giRSCLvl := IniFile.ReadInteger('CONF','RSCLVL',1);

    miLandscapeNo := IniFile.ReadInteger('CONF','LANDSCAPE',0);

    msStrmDir := IniFile.ReadString('STREAM','STRMDIR','C:');
    miStrmDataPeriod := IniFile.ReadInteger('STREAM','STRMPERIOD',60);
    miStrmViewFlipPeriod := IniFile.ReadInteger('STREAM','STRMVIEWFLIPPERIOD',60);
    msStrmText1_DE := IniFile.ReadString('STREAM','STRMTEXT1_DE','Sternenhimmel JETZT');
    msStrmText1_EN := IniFile.ReadString('STREAM','STRMTEXT1_EN','Starry Sky NOW');
    msStrmText2_DE := IniFile.ReadString('STREAM','STRMTEXT2_DE','DeepSky Empfehlung');
    msStrmText2_EN := IniFile.ReadString('STREAM','STRMTEXT2_EN','DeepSky Recommendation');
    msStrmText3_DE := IniFile.ReadString('STREAM','STRMTEXT3_DE','');
    msStrmText3_EN := IniFile.ReadString('STREAM','STRMTEXT3_EN','');

    IniFile.Destroy;

    miFirstStart := 0; //IniFile.ReadInteger('CONF','FIRSTSTART',1);

    if(msLANG_ID = 'DE') then
      F__STARTUP.L__INFO.Caption := 'Lade Voreinstellungen...OK'
    else
      F__STARTUP.L__INFO.Caption := 'Loading prefernces...OK';

    F__STARTUP.L__INFO.Refresh;
    F__STARTUP.Repaint;
  end;

  if(miFirstStart = 1) then
  begin
    F__SETPREFSINFO := TF__SETPREFSINFO.Create(nil);
    F__SETPREFSINFO.ShowModal;
    msLANG_ID := F__SETPREFSINFO.msLANG_ID;
    F__SETPREFSINFO.Destroy;

    ShowPrefs();

    if(msLANG_ID = 'DE') then
      MessageDlg('Albireo','Konfiguration abgeschlossen. Bitte starten Sie Albireo neu.',mtInformation,[mbOK],0)
    else
      MessageDlg('Albireo','Configuration completed. Please restart Albireo.',mtInformation,[mbOK],0);

    miFirstStart := 1;

  end
  else
  begin
    sPicFile := GetRandomPictureFile();
    if(sPicFile <> '') then
      F__STARTUP.IMG__BACKGROUND.Picture.LoadFromFile(sPicFile);

    if(msLANG_ID = 'DE') then
      F__STARTUP.L__INFO.Caption := 'Initialisiere Anwendung...'
    else
      F__STARTUP.L__INFO.Caption := 'Initialize Application...';

    F__STARTUP.L__PROGRESS.Caption := '10%';
    for i:=0 to 5 do
      F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;

    F__STARTUP.Repaint;

    P__FIRSTRUN.Visible := (miFirstStart = 1);

    TIMER.Enabled := true;

    PC__WORKBENCH.ActivePageIndex := ciPAGE_ALBIREO;
    CB__WT.Date := Now;
    mdtStart := 0;
    mdtStart0 := 0;
    miSMDisplayMode := 0;

    if(msLANG_ID = 'DE') then
    begin
      P__DAWN_AM_C.Caption:='DB';
      P__DAWN_PM_C.Caption:='DB';
    end;
    if(msLANG_ID = 'EN') then
    begin
      // Adapt all visible Texts to German / English
      IniText(F__ASTROTOOLBOX,msLANG_ID);
      P__DAWN_AM_C.Caption:='DC';
      P__DAWN_PM_C.Caption:='DC';
    end;

    F__STARTUP.L__PROGRESS.Caption:='20%';
    for i:=0 to 10 do
      F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;

    F__STARTUP.Repaint;

    if(msLANG_ID = 'DE') then
      F__STARTUP.L__INFO.Caption := 'Lade Teleskopeinstellungen...'
    else
      F__STARTUP.L__INFO.Caption := 'Loading telescope settings...';

    F__STARTUP.L__INFO.Refresh;
    F__STARTUP.Repaint;

    IniTelProp();

    F__STARTUP.L__PROGRESS.Caption:='25%';
    for i:=0 to 5 do
      F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;

    sPicFile := GetRandomPictureFile();
    if(sPicFile <> '') then
      F__STARTUP.IMG__BACKGROUND.Picture.LoadFromFile(sPicFile);

    F__STARTUP.Repaint;

    PMENU__MMAG_0.Checked := (mrMagPos = 0);
    PMENU__MMAG_1.Checked := (mrMagPos = 1);
    PMENU__MMAG_2.Checked := (mrMagPos = 2);
    PMENU__MMAG_3.Checked := (mrMagPos = 3);
    PMENU__MMAG_4.Checked := (mrMagPos = 4);
    PMENU__MMAG_5.Checked := (mrMagPos = 5);
    PMENU__MMAG_6.Checked := (mrMagPos = 6);
    PMENU__MMAG_7.Checked := (mrMagPos = 7);
    PMENU__MMAG_8.Checked := (mrMagPos = 8);

    if(msLANG_ID = 'DE') then
      F__STARTUP.L__INFO.Caption := 'Lade Astrobibliotheken...'
    else
      F__STARTUP.L__INFO.Caption := 'Loading astronomical libraries...';

    F__STARTUP.Repaint;

    // Import Astronomical Objects
    ImportCatalogs();

    F__STARTUP.L__PROGRESS.Caption:='80%';
    for i:=0 to 5 do
      F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
    F__STARTUP.Repaint;

    mbTimePlay := false;

    SwitchTimePLAY();
    SetNow();

    mslCBList := TStringList.Create;
    mslCBList.LoadFromFile(ConvertWinPath(gsAlbireoLocalDir + 'CB.dat'));

    if(msLANG_ID = 'DE') then
      F__STARTUP.L__INFO.Caption := 'Lade Gerätedaten...'
    else
      F__STARTUP.L__INFO.Caption := 'Loading device data...';

    F__STARTUP.L__INFO.Refresh;
    F__STARTUP.L__PROGRESS.Caption:='90%';
    for i:=0 to 10 do
      F__STARTUP.L__PROGRESSBAR.Caption := F__STARTUP.L__PROGRESSBAR.Caption + sProgress;
    F__STARTUP.Repaint;

    LoadDevices(molADevList);

    IniCB__ADEV();

    if(msLANG_ID = 'DE') then
      F__STARTUP.L__INFO.Caption := 'Abschließen der Initialisierung...'
    else
      F__STARTUP.L__INFO.Caption := 'Terminating the initialization...';

    F__STARTUP.Repaint;

    mfMagMin := -1; mfMagMax:= -1;

    miMSDoneMonth := 0;

    GRD__RECOM_P.ColWidths[2] := 0;

    if(msLANG_ID = 'DE') then
    begin
      GRD__RECOM_MS.Cells[0,0] := 'Strom';
      GRD__RECOM_MS.Cells[1,0] := 'Sternb.';
      GRD__RECOM_MS.Cells[2,0] := '#Met/h';
      GRD__RECOM_MS.Cells[3,0] := 'km/s';
    end;

    if(msLANG_ID = 'EN') then
    begin
      GRD__RECOM_MS.Cells[0,0] := 'Stream';
      GRD__RECOM_MS.Cells[1,0] := 'Con.';
      GRD__RECOM_MS.Cells[2,0] := '#Met/h';
      GRD__RECOM_MS.Cells[3,0] := 'km/s';
    end;
    GRD__RECOM_MS.ColWidths[0] := 120;
    GRD__RECOM_MS.ColWidths[1] := 50;
    GRD__RECOM_MS.ColWidths[2] := 50;

    // Show Constellations by default
    PMENU__CON.Checked := true;
    mbSM_CON := PMENU__CON.Checked;

    PMENU__MOON.Checked := true;
    mbShowMoon := PMENU__MOON.Checked;

    PMENU__SUN.Checked := true;
    mbShowSun := PMENU__SUN.Checked;

    GRD__AO.ColWidths[0] := 60;

    ShowLatLong();

    // Device section
    IniCB_ADEV_TYPE();

    F__STARTUP.Hide;
    F__STARTUP.Free;
    F__STARTUP := nil;
  end;

  // Set Colors
  if(miFirstStart = 1) then begin Close; exit; end;

  P__HORDUST.Visible:=false;

  P__STAR_LOC_MAIN.Color := clBlack;
  TB__TIME_24H.Hint := FormatDateTime('hh:nn:ss',1.0*TB__TIME_24H.Position / 1440);

  dtWT := GetWTime();
  SetMainInfoData(dtWT);

  WindowState := wsMaximized;

  IniCalcPlanetOpCon();

  mbChangedLoc := false;
  B__LOC_RESET.Visible:=false;
  IniCountries();

  AssignAOUserFields();

  if(RB__S.Checked) then
  begin

    if(msLANG_ID = 'DE') then
      P__TABLE_TITLE.Caption:='Alle Sterne bis ' +  FloatToStrF(crMagPosStd,ffFixed,8,2) + 'mag'
    else
      P__TABLE_TITLE.Caption:='All stars up to ' +  FloatToStrF(crMagPosStd,ffFixed,8,2) + 'mag';

  end;


  SetTimePanels();
  GenTimeScale(true);
  GetRecommendations(Now);
  ShowMeteoriteShowerGrid(Now);
  ShowEclipsesForMonth(Now);

  mrMagPos := crMagPosStd;
  mrMagPos_G := crMagPosStd_G;
  L__MAGSIZE.Caption:= AnsiReplaceStr(FloatToStrF(mrMagPos,ffFixed,8,1),',','.');
  L__MAGSIZE_G.Caption:= AnsiReplaceStr(FloatToStrF(mrMagPos_G,ffFixed,8,1),',','.');
  PC__MAG.ActivePageIndex:=0;

end;

procedure TF__ASTROTOOLBOX.FormWindowStateChange(Sender: TObject);
begin
  if(miFirstStart = 1) then exit;

  if(F__ASTROTOOLBOX.WindowState <> wsMinimized) then
  begin
    case PC__WORKBENCH.ActivePageIndex of
      ciPAGE_STARMAP:
      begin
        if(not mbTimePlay) then
          SwitchTimePlay();

        SetGenMapInterval(true); //ciGraphFastRefreshTime; // Strange. MUST be set to repaint the frame.
      end;
      ciPAGE_SOLSYS:
      begin
        //ShowSolSys();
        SetGenMapInterval(true); //ciGraphFastRefreshTime;
        TIMER__GENMAP.Enabled:=true;
        TIMER__GENMAP.Tag:=1; // Stop after 1x execution
      end;
    end;
  end;

  SetTimePanels();
  GenTimeScale(false);
end;

procedure TF__ASTROTOOLBOX.GRD__AODblClick(Sender: TObject);
begin
  ShowDBAObject();
end;

procedure TF__ASTROTOOLBOX.ShowDBAObject();
var
  iRow, iAOIndex: Integer;
  sMsgTitle, sMsg: string;
begin

  iRow := GRD__AO.Row;

  if(iRow <= 0) then exit;

  if(GRD__AO.Rows[iRow].Count < 1) then
    exit;

  if(Trim(GRD__AO.Cells[0,iRow]) = '') then
    exit;

  iAOIndex := StrToInt(GRD__AO.Cells[0,iRow]);

  if(iAOIndex < 0) then exit;

  if(RB__S.Checked and ((molAOList[iAOIndex] as TStar).sSym = 'MARK')) then
  begin
    if(msLANG_ID = 'DE') then
    begin
      if((molAOList[iAOIndex] as TStar).sSpType = 'MARK-VOID') then
      begin
        sMsgTitle := 'Markierung';
        sMsg := (molAOList[iAOIndex] as TStar).sName_DE;
      end
      else
      begin
        sMsgTitle := 'Markierung';
        sMsg := 'Richtung auf: ' + (molAOList[iAOIndex] as TStar).sName_DE;
      end;
    end
    else
    begin
      if((molAOList[iAOIndex] as TStar).sSpType = 'MARK-VOID') then
      begin
        sMsgTitle := 'Mark';
        sMsg := (molAOList[iAOIndex] as TStar).sName_DE;
      end
      else
      begin
        sMsgTitle := 'Mark';
        sMsg := 'Directed to: ' + (molAOList[iAOIndex] as TStar).sName_EN;
      end;

    end;

    if((molAOList[iAOIndex] as TStar).sSpType = 'MARK-VOID') then
      ShowAstroVoidForm(
      GetAOLabel((molAOList[iAOIndex] as TStar),msLANG_ID),
      (molAOList[iAOIndex] as TStar).sCon,
      Round((molAOList[iAOIndex] as TStar).rDist_XLY),
      Round((molAOList[iAOIndex] as TStar).rSolFrac)
      )
    else
      MessageDlg(sMsgTitle,sMsg,mtInformation,[mbOK],0);

    exit;
  end;

  ShowAOVis(iAOIndex);
end;

procedure TF__ASTROTOOLBOX.GRD__AODrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  s: string;
begin
  with Sender as TStringGrid do
  begin
    s := Cells[ACol, ARow];

    if(ARow = 0) then
    begin
      Canvas.Brush.Color := clRed;
      Canvas.Font.Color:=$00101010;
    end
    else
    begin
      if(gdSelected in aState ) then
        Canvas.Brush.Color := clMaroon
      else
        Canvas.Brush.Color := $00101010;

      if(ACol = 1) and (AnsiContainsStr(s,'MARK')) then
      begin
        if(s = 'MARK-VOID') then
          Canvas.Font.Color := clNavy
        else
          Canvas.Font.Color := clLime;
      end
      else
      begin
        if(Cells[aCol,ARow] = 'Unter Horizont') or (Cells[aCol,ARow] = 'Under Horizon') then
          Canvas.Font.Color:=clYellow
        else if(Cells[aCol,ARow] = 'Tageshimmel') or (Cells[aCol,ARow] = 'Day Sky') then
          Canvas.Font.Color:=clSkyBlue
        else if(Cells[aCol,ARow] = 'Niemals sichtbar') or (Cells[aCol,ARow] = 'Never visible') then
          Canvas.Font.Color:=clSilver
        else
          Canvas.Font.Color:=clRed;

      end;
    end;

    // Set spectral type color font to spectral colour
    //if((GRD__AO.Cells[5,0] = 'Spektraltyp') or (GRD__AO.Cells[5,0] = 'Spectral Type')) and (aRow > 0) and (aCol = 5) then
    if(RB__S.Checked and (aRow > 0) and (aCol = 5)) then
    begin
      if(AnsiContainsStr(GRD__AO.Cells[1,aRow],'MARK')) then
      begin
        if(GRD__AO.Cells[1,aRow] = 'MARK-VOID') then
          Canvas.Font.Color := clNavy
        else
          Canvas.Font.Color := clLime;

      end
      else
        Canvas.Font.Color := GetColorFromSpecType(GRD__AO.Cells[5,aRow]);

    end;

    Canvas.FillRect(aRect);
    Canvas.TextRect(aRect, aRect.Left + 2, aRect.Top + 2, s);

  end;
end;

procedure TF__ASTROTOOLBOX.GRD__AOHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var
  iSortColOrder, iColType: Integer;
begin
  if(Index < 0) then
    exit;

  try
    Screen.Cursor:=crHourGlass;

    if(Index <> miSCI_GRD_AO) then
    begin
      if(miSCI_GRD_AO >= 0) then
      begin
        GRD__AO.Cells[miSCI_GRD_AO,0] := AnsiReplaceStr(GRD__AO.Cells[miSCI_GRD_AO,0],' |A','');
        GRD__AO.Cells[miSCI_GRD_AO,0] := AnsiReplaceStr(GRD__AO.Cells[miSCI_GRD_AO,0],' |D','');
      end;

      miSCO_GRD_AO := -1; // Reset column order if other column is selected
    end;

    miSCI_GRD_AO := Index;

    case miSCO_GRD_AO of
      0:
      begin
        iSortColOrder := 1;
        if(Pos(' |',GRD__AO.Cells[Index,0]) > 0) then
          GRD__AO.Cells[Index,0] := AnsiReplaceStr(GRD__AO.Cells[Index,0],' |D',' |A')
        else
          GRD__AO.Cells[Index,0] := GRD__AO.Cells[Index,0] + ' |A';
      end;
      1:
      begin
        iSortColOrder := 0;
        if(Pos(' |',GRD__AO.Cells[Index,0]) > 0) then
          GRD__AO.Cells[Index,0] := AnsiReplaceStr(GRD__AO.Cells[Index,0],' |A',' |D')
        else
          GRD__AO.Cells[Index,0] := GRD__AO.Cells[Index,0] + ' |D';
      end;
      else
      begin
        iSortColOrder := 1;
        if(Pos(' |',GRD__AO.Cells[Index,0]) > 0) then
          GRD__AO.Cells[Index,0] := AnsiReplaceStr(GRD__AO.Cells[Index,0],' |D',' |A')
        else
          GRD__AO.Cells[Index,0] := GRD__AO.Cells[Index,0] + ' |A';

      end;
    end;
    miSCO_GRD_AO := iSortColOrder;

    iColType := StrToInt(mslColType[miSCI_GRD_AO]);

    SortStringGrid(GRD__AO,miSCI_GRD_AO,iColTYpe,true,(miSCO_GRD_AO = 1));

  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TF__ASTROTOOLBOX.GRD__AOKeyPress(Sender: TObject; var Key: char);
begin
  ShowDBAObject();
end;

procedure TF__ASTROTOOLBOX.GRD__RECOM_PDblClick(Sender: TObject);
var
  iIndex: Integer;
begin
  iIndex := StrToInt(GRD__RECOM_P.Cells[2,GRD__RECOM_P.Row]);
  ShowAOVis(iIndex);
end;

procedure TF__ASTROTOOLBOX.IMG__ADEV_PHOTOClick(Sender: TObject);
begin
  ShowPictureViewer(IMG__ADEV_PHOTO);
end;

(*
procedure TF__ASTROTOOLBOX.HTTPCLIENTError(const msg: string; aSocket: TLSocket
  );
begin
  ShowMessage(Msg);
end;
function TF__ASTROTOOLBOX.HTTPCLIENTInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: integer): integer;
var
  sHTML: string;
begin
  HTTPClient.GetMessage(sHTML,ASocket);
  //ShowMessage(sHTML);
  CheckForUpdate(sHTML);
end;
*)
procedure TF__ASTROTOOLBOX.IMG__FLAGClick(Sender: TObject);
begin
  SwitchLang();
end;

procedure TF__ASTROTOOLBOX.IMG__HOMEClick(Sender: TObject);
begin

end;

procedure TF__ASTROTOOLBOX.IMG__HORMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ShowHorizonMovePanels(X,Y);
end;

procedure TF__ASTROTOOLBOX.IMG__MOON_CORNERClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.IMG__POS_MOONMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AOVisOnMouseUp(Sender,Button,Shift,X,Y);
end;

procedure TF__ASTROTOOLBOX.IMG__SOLSYSMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  iDiff: Integer;
begin
  iDiff := 1;

  if(WheelDelta > 0) then
  begin
    if(TB__SOLSYS.Position >= TB__SOLSYS.Min+iDiff) then
      TB__SOLSYS.Position := TB__SOLSYS.Position - iDiff;
  end
  else
    if(TB__SOLSYS.Position+iDiff <= TB__SOLSYS.Max) then
      TB__SOLSYS.Position := TB__SOLSYS.Position + iDiff;

   ShowSolSys();
end;

procedure TF__ASTROTOOLBOX.LB__SEARCHRESClick(Sender: TObject);
var
  AOSearch: TAOSearch;
  iIndex: Integer;
begin
  iIndex := LB__SEARCHRES.ItemIndex;
  AOSearch := niL;
  if(iIndex > -1) then
  begin
    AOSearch := (LB__SEARCHRES.Items.Objects[iIndex] as TAOSearch);
    if(AOSearch <> nil) then
    begin
      ShowAOVis(AOSearch.iAOIndex);
      (*
      //ShowMessage(IntToStr(AOSearch.iAOIndex))
     F__AOVIS := TF__AOVIS.Create(nil);
     F__AOVIS.msLANG_ID := msLANG_ID;
     F__AOVIS.mAObject := (molAOList[AOSearch.iAOIndex] as TAObject);
     F__AOVIS.molSignList := molSignList;

     F__AOVIS.ShowModal;
     F__AOVIS.Destroy;
     *)
    end
    else
    begin
      if(msLANG_ID = 'DE') then
        ShowMessage('Gesuchtes Objekt nicht gefunden!')
      else
        ShowMessage('Search object not found!');
    end;
  end;
end;

procedure TF__ASTROTOOLBOX.LB__SEARCHRESMouseEnter(Sender: TObject);
begin
  if(LB__SEARCHRES.Count > 0) then
    Screen.Cursor := crHandPoint;
end;

procedure TF__ASTROTOOLBOX.LB__SEARCHRESMouseLeave(Sender: TObject);
begin
  Screen.Cursor := crDefault;
end;

procedure TF__ASTROTOOLBOX.L__ALBIREOClick(Sender: TObject);
begin
  ExecAlbireo();
end;

procedure TF__ASTROTOOLBOX.L__ALBIREOMouseEnter(Sender: TObject);
begin
  AnimateButton(-1,L__ALBIREO);
  P__GO_HOME.Color := clMaroon;
end;

procedure TF__ASTROTOOLBOX.L__ALBIREOMouseLeave(Sender: TObject);
begin
  AnimateButton(1,L__ALBIREO);
  P__GO_HOME.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__ASTROCALCClick(Sender: TObject);
begin
  EnableMenu('MENU__ASTROCALC');
  ShowAstroCalc(nil);
end;

procedure TF__ASTROTOOLBOX.L__ASTROCALCMouseEnter(Sender: TObject);
begin
  AnimateButton(-1,L__ASTROCALC);
  P__ASTROCALC.Color := clMaroon;
end;

procedure TF__ASTROTOOLBOX.L__ASTROCALCMouseLeave(Sender: TObject);
begin
  AnimateButton(1,L__ASTROCALC);

  P__ASTROCALC.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__BV_BOTTOMClick(Sender: TObject);
begin
  SetStarmapView(L__BV_BOTTOM.Tag);
end;

procedure TF__ASTROTOOLBOX.L__BV_LEFTClick(Sender: TObject);
begin
  SetStarmapView(L__BV_LEFT.Tag);
end;

procedure TF__ASTROTOOLBOX.L__BV_RIGHTClick(Sender: TObject);
begin
  SetStarmapView(L__BV_RIGHT.Tag);
end;

procedure TF__ASTROTOOLBOX.L__DEVICESClick(Sender: TObject);
begin
  ExecDevices();
end;

procedure TF__ASTROTOOLBOX.L__DEVICESMouseEnter(Sender: TObject);
begin
  AnimateButton(-1,L__DEVICES);
  P__DEVICES.Color := clMaroon;
end;

procedure TF__ASTROTOOLBOX.L__DEVICESMouseLeave(Sender: TObject);
begin
  AnimateButton(1,L__DEVICES);
  P__DEVICES.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__HAClick(Sender: TObject);
begin
  ExecHA();
  P__GO_HOURANGLE.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__HAMouseEnter(Sender: TObject);
begin
  AnimateButton(-1,L__HA);
  P__GO_HOURANGLE.Color := clMaroon;
end;

procedure TF__ASTROTOOLBOX.L__HAMouseLeave(Sender: TObject);
begin
  AnimateButton(1,L__HA);
  P__GO_HOURANGLE.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__MAPClick(Sender: TObject);
begin
  ExecMAP();
end;

procedure TF__ASTROTOOLBOX.L__MAPMouseEnter(Sender: TObject);
begin
  AnimateButton(-1,L__MAP);
  P__MAP.Color := clMaroon; // $00000022;
end;

procedure TF__ASTROTOOLBOX.L__MAPMouseLeave(Sender: TObject);
begin
  AnimateButton(1,L__MAP);
  P__MAP.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__NAVSOLSYSClick(Sender: TObject);
begin
  ExecSolSys();
end;

procedure TF__ASTROTOOLBOX.L__NAVSOLSYSMouseEnter(Sender: TObject);
begin
  AnimateButton(-1,L__NAVSOLSYS);
  P__NAV_SOLSYS.Color := clMaroon; // $00000022;
end;

procedure TF__ASTROTOOLBOX.L__NAVSOLSYSMouseLeave(Sender: TObject);
begin
  AnimateButton(1,L__NAVSOLSYS);
  P__NAV_SOLSYS.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__OC_LATClick(Sender: TObject);
begin
  ShowPrefs();
  ShowLatLong();
end;

procedure TF__ASTROTOOLBOX.L__OC_LONGClick(Sender: TObject);
begin
  ShowPrefs();
  ShowLatLong();
end;

procedure TF__ASTROTOOLBOX.L__STARTSTOPClick(Sender: TObject);
begin
  SwitchTimePLAY();
end;

procedure TF__ASTROTOOLBOX.L__TABLEClick(Sender: TObject);
begin
  ExecTable();
end;

procedure TF__ASTROTOOLBOX.L__TABLEMouseEnter(Sender: TObject);
begin
  AnimateButton(-1,L__TABLE);
  P__TABLES.Color := clMaroon;
end;

procedure TF__ASTROTOOLBOX.L__TABLEMouseLeave(Sender: TObject);
begin
  AnimateButton(1,L__TABLE);
  P__TABLES.Color := $00101010;
end;

procedure TF__ASTROTOOLBOX.L__ZC_DClick(Sender: TObject);
begin
  MoveZoom(0,10);
end;

procedure TF__ASTROTOOLBOX.L__ZC_INClick(Sender: TObject);
begin
  if(mbZoomMode) then
    ExecZoom(0,0,-1,false);
end;

procedure TF__ASTROTOOLBOX.L__ZC_L1Click(Sender: TObject);
begin
  if(mbZoomMode) then
    ExecZoom(0,0,1,false);
end;

procedure TF__ASTROTOOLBOX.L__ZC_LClick(Sender: TObject);
begin
  MoveZoom(-10,0);
end;

procedure TF__ASTROTOOLBOX.L__ZC_RClick(Sender: TObject);
begin
  MoveZoom(10,0);
end;

procedure TF__ASTROTOOLBOX.L__ZC_UClick(Sender: TObject);
begin
  MoveZoom(0,-10);
end;

procedure TF__ASTROTOOLBOX.MenuItem11Click(Sender: TObject);
begin
  ToggleStrmFlipAll();
end;

procedure TF__ASTROTOOLBOX.MENU__ADB_ADMINClick(Sender: TObject);
begin
  OpenADM();
end;

procedure TF__ASTROTOOLBOX.MENU__ADB_SHOWClick(Sender: TObject);
begin
  OpenADBInfo();
end;

procedure TF__ASTROTOOLBOX.MENU__ASTROPHOTO_NYSClick(Sender: TObject);
begin
  EnableMenu('MENU__ASTROCALC');
  ShowAstroCalc(nil);
  F__ASTROCALC.PC__ASTROCALC.ActivePageIndex:=2;
  F__ASTROCALC.PC__APP.ActivePageIndex:=2;
end;

procedure TF__ASTROTOOLBOX.MENU__ASTROPHOTO_SETTINGSClick(Sender: TObject);
begin
  EnableMenu('MENU__ASTROCALC');
  ShowAstroCalc(nil);
  F__ASTROCALC.PC__ASTROCALC.ActivePageIndex:=2;
  F__ASTROCALC.PC__APP.ActivePageIndex:=0;
end;

procedure TF__ASTROTOOLBOX.MENU__ASTROPHOTO_SIGNALClick(Sender: TObject);
begin
  EnableMenu('MENU__ASTROCALC');
  ShowAstroCalc(nil);
  F__ASTROCALC.PC__ASTROCALC.ActivePageIndex:=2;
  F__ASTROCALC.PC__APP.ActivePageIndex:=3;
end;

procedure TF__ASTROTOOLBOX.MENU__ASTRPHOTO_SIMAGEClick(Sender: TObject);
begin
  EnableMenu('MENU__ASTROCALC');
  ShowAstroCalc(nil);
  F__ASTROCALC.PC__ASTROCALC.ActivePageIndex:=2;
  F__ASTROCALC.PC__APP.ActivePageIndex:=1;
end;

procedure TF__ASTROTOOLBOX.MENU__DONATIONClick(Sender: TObject);
begin
  ExecOpen('https://www.tipeeestream.com/stecknitzastro/donation');
end;

procedure TF__ASTROTOOLBOX.MENU__EYEFACClick(Sender: TObject);
begin
  if(not MENU__EYEFAC.Checked) then
  begin
    MENU__EYEFAC.Checked := true;
    mrEyeFacH := crEyeFacH_70;
  end
  else
  begin
    MENU__EYEFAC.Checked := false;
    mrEyeFacH := crEyeFacH_90;
  end;

  // Refresh view if any horizon view is active
  if(miStarMapView > 0) then
    CleanStartOfStarmap();

end;

procedure TF__ASTROTOOLBOX.MENU__GOTOClick(Sender: TObject);
begin
  P__GO_HOURANGLE.Color:=$00101010;
  ExecHA();
end;

procedure TF__ASTROTOOLBOX.MENU__KOCHABClick(Sender: TObject);
begin
  ControlKochabPanel();
  MENU__KOCHAB.Checked := not MENU__KOCHAB.Checked;
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_0Click(Sender: TObject);
begin
  OnSelSpecClass(1,1);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_1Click(Sender: TObject);
begin
  OnSelSpecClass(1,2);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_2Click(Sender: TObject);
begin
  OnSelSpecClass(1,3);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_3Click(Sender: TObject);
begin
  OnSelSpecClass(1,4);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_4Click(Sender: TObject);
begin
  OnSelSpecClass(1,5);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_5Click(Sender: TObject);
begin
  OnSelSpecClass(1,6);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_6Click(Sender: TObject);
begin
  OnSelSpecClass(1,7);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_7Click(Sender: TObject);
begin
  OnSelSpecClass(1,8);
end;

procedure TF__ASTROTOOLBOX.MENU__LCLASS_ALLClick(Sender: TObject);
begin
  if(MENU__LCLASS_ALL.Checked) then
  begin
    MENU__LCLASS_0.Checked := false;
    MENU__LCLASS_1.Checked := false;
    MENU__LCLASS_2.Checked := false;
    MENU__LCLASS_3.Checked := false;
    MENU__LCLASS_4.Checked := false;
    MENU__LCLASS_5.Checked := false;
    MENU__LCLASS_6.Checked := false;
    MENU__LCLASS_7.Checked := false;
  end;

  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.MENU__MAPClick(Sender: TObject);
begin
  ExecMAP();
end;

procedure TF__ASTROTOOLBOX.MENU__MY_TELClick(Sender: TObject);
begin
  ExecDevices();
end;

procedure TF__ASTROTOOLBOX.MENU__REFRESH_APPClick(Sender: TObject);
begin
  RefreshApp();
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_AClick(Sender: TObject);
begin
  OnSelSpecClass(0,3);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_ALLClick(Sender: TObject);
begin
  if(MENU__SCLASS_ALL.Checked) then
  begin
    MENU__SCLASS_O.Checked := false;
    MENU__SCLASS_B.Checked := false;
    MENU__SCLASS_A.Checked := false;
    MENU__SCLASS_F.Checked := false;
    MENU__SCLASS_G.Checked := false;
    MENU__SCLASS_K.Checked := false;
    MENU__SCLASS_M.Checked := false;

    MENU__SCLASS_CARBON.Checked := false;
    MENU__SCLASS_R.Checked := false;
    MENU__SCLASS_N.Checked := false;
    MENU__SCLASS_S.Checked := false;
    MENU__SCLASS_C.Checked := false;

    MENU__SCLASS_BD.Checked := false;
    MENU__SCLASS_L.Checked := false;
    MENU__SCLASS_T.Checked := false;
    MENU__SCLASS_Y.Checked := false;
  end;

  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_BClick(Sender: TObject);
begin
  OnSelSpecClass(0,2);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_BDClick(Sender: TObject);
begin
  if(MENU__SCLASS_BD.Checked) then
  begin
    MENU__SCLASS_L.Checked := false;
    MENU__SCLASS_T.Checked := false;
    MENU__SCLASS_Y.Checked := false;
  end;

  OnSelSpecClass(0,13);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_CARBONClick(Sender: TObject);
begin
  if(MENU__SCLASS_CARBON.Checked) then
  begin
    MENU__SCLASS_R.Checked := false;
    MENU__SCLASS_N.Checked := false;
    MENU__SCLASS_S.Checked := false;
    MENU__SCLASS_C.Checked := false;
  end;

  OnSelSpecClass(0,8);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_CClick(Sender: TObject);
begin
  OnSelSpecClass(0,12);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_FClick(Sender: TObject);
begin
  OnSelSpecClass(0,4);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_GClick(Sender: TObject);
begin
  OnSelSpecClass(0,5);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_KClick(Sender: TObject);
begin
  OnSelSpecClass(0,6);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_LClick(Sender: TObject);
begin
  OnSelSpecClass(0,14);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_MClick(Sender: TObject);
begin
  OnSelSpecClass(0,7);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_NClick(Sender: TObject);
begin
  OnSelSpecClass(0,10);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_OClick(Sender: TObject);
begin
  OnSelSpecClass(0,1);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_RClick(Sender: TObject);
begin
  OnSelSpecClass(0,9);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_SClick(Sender: TObject);
begin
  OnSelSpecClass(0,11);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_TClick(Sender: TObject);
begin
  OnSelSpecClass(0,15);
end;

procedure TF__ASTROTOOLBOX.MENU__SCLASS_YClick(Sender: TObject);
begin
  OnSelSpecClass(0,16);
end;

procedure TF__ASTROTOOLBOX.MENU__SPACELABClick(Sender: TObject);
begin
  EnableMenu('MENU__ASTROCALC');
  ShowAstroCalc(nil);
end;

procedure TF__ASTROTOOLBOX.MENU__TABLESClick(Sender: TObject);
begin
  P__TABLES.Color:=$00101010;
  ExecTable();
end;

procedure TF__ASTROTOOLBOX.MENU__TODAYClick(Sender: TObject);
begin
  ExecAlbireo();
end;

procedure TF__ASTROTOOLBOX.MENU__T_STARTSTOPClick(Sender: TObject);
begin
  SwitchTimePLAY();
end;

procedure TF__ASTROTOOLBOX.PMENU__ASClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_SM_AS, not PMENU__AS.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__DEC_SCALAClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_COO_EQU_DEC, not PMENU__DEC_SCALA.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__HSClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_SM_HS, not PMENU__HS.Checked);
end;

procedure TF__ASTROTOOLBOX.MENU__COMMENTClick(Sender: TObject);
var
  iAOIndex: Integer;
  sCaption, sQuest, sComment: string;
begin
  iAOIndex := GetAOIndexFromStarMap();
  if(iAOIndex < 0) then exit;

  sComment := (molAOList[iAOIndex] as TAObject).sComment;

  if(msLANG_ID = 'DE') then
  begin
    sCaption := 'Ihr Kommentar';
    sQuest := 'Zu Objekt ' + GetAOLabel((molAOList[iAOIndex] as TAObject),msLANG_ID) + ': ';
  end
  else
  begin
    sCaption := 'Your Comment';
    sQuest := 'Regarding Object ' + GetAOLabel((molAOList[iAOIndex] as TAObject),msLANG_ID) + ': ';
  end;

  if(InputQuery(sCaption,sQuest,sComment) ) then
  begin
    if(Trim(sComment) <> '') then
    begin
      (molAOList[iAOIndex] as TAObject).sComment:=AnsiReplaceStr(sComment,'~','-'); // Reserved ~ Separator Mask
      SaveAOUserFields('COMMENT',iAOIndex,sComment);
    end;
  end;

end;

procedure TF__ASTROTOOLBOX.MENU__DOWNLOADClick(Sender: TObject);
begin
  ExecOpen('https://www.stecknitz-astronomie.de/albireo');
end;

procedure TF__ASTROTOOLBOX.MENU__HORClick(Sender: TObject);
begin
  ExecCustHorizons();
end;

procedure TF__ASTROTOOLBOX.MENU__HOR_3Click(Sender: TObject);
begin
  ExecCustHorizons();
end;

procedure TF__ASTROTOOLBOX.MENU__PDFClick(Sender: TObject);
var
  sDocFileName: string;
begin
  if(msLANG_ID = 'DE') then
    sDocFileName := 'Albireo.pdf'
  else
    sDocFileName := 'Albireo_EN.pdf';

  sDocFileName := ExtractFilePath(Application.ExeName) + sDocFileName;

  if(FileExists(sDocFileName)) then
    OpenDocument(sDocFileName)
  else
    MessageDlg('Error',sDocFileName + ': Document not found here.',mtError,[mbOK],0);

end;

procedure TF__ASTROTOOLBOX.MENU__SHOW_MESSIERONLYClick(Sender: TObject);
begin
  MENU__SHOW_MESSIERONLY.Checked:=not MENU__SHOW_MESSIERONLY.Checked;

  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.MENU__STRM_LANGFLIPClick(Sender: TObject);
begin
  ToggleStrmLangFlipNow();
end;

procedure TF__ASTROTOOLBOX.MENU__STRM_NOWClick(Sender: TObject);
begin
  ToggleStrmDataNow();
end;

procedure TF__ASTROTOOLBOX.MENU__STRM_PREFSClick(Sender: TObject);
begin
   ShowStrmDlg();
end;

procedure TF__ASTROTOOLBOX.MENU__STRM_VIEWFLIPClick(Sender: TObject);
begin
  ToggleStrmViewFlipNow();
end;

procedure TF__ASTROTOOLBOX.MENU__TWITCHClick(Sender: TObject);
begin
  ExecOpen('https://www.twitch.tv/stecknitzastro');
end;

procedure TF__ASTROTOOLBOX.MENU__VISIBLE_ONLYClick(Sender: TObject);
begin
  MENU__VISIBLE_ONLY.Checked:=not MENU__VISIBLE_ONLY.Checked;

  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.MENU__GRD_MAG_6Click(Sender: TObject);
begin
end;

procedure TF__ASTROTOOLBOX.MENU__WEBSITEClick(Sender: TObject);
begin
  ExecOpen(gcsWebInfo);
end;

procedure TF__ASTROTOOLBOX.PMENU__RA_SCALAClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_COO_EQU_RA, not PMENU__RA_SCALA.Checked);
end;

procedure TF__ASTROTOOLBOX.P__ASTROCALCClick(Sender: TObject);
begin
  P__ASTROCALC.Color:=$00101010;
  ShowAstroCalc(nil);
end;

procedure TF__ASTROTOOLBOX.P__ASTROCALCMouseEnter(Sender: TObject);
begin
  P__ASTROCALC.Color := clMaroon; // $00000022;
  if(not Assigned(F__ASTROCALC)) then
    L__ASTROCALC.Font.Color:=clWhite;
end;

procedure TF__ASTROTOOLBOX.P__ASTROCALCMouseLeave(Sender: TObject);
begin
  P__ASTROCALC.Color := $00101010;
  if(not Assigned(F__ASTROCALC)) then
    L__ASTROCALC.Font.Color:=clSilver;
end;

procedure TF__ASTROTOOLBOX.P__CATINFO_CONTROLClick(Sender: TObject);
begin
  ControlCatinfoPanel();
end;

procedure TF__ASTROTOOLBOX.P__KOCHAB_CONTROLClick(Sender: TObject);
begin
  ControlKochabPanel();
end;

procedure TF__ASTROTOOLBOX.P__MAGSIZE_CONTROLClick(Sender: TObject);
begin
  ControlMagsizePanel();
end;

procedure TF__ASTROTOOLBOX.P__REWIND_MINUSClick(Sender: TObject);
begin
  SetTimeEdits(false,1);
end;

procedure TF__ASTROTOOLBOX.PC__TABLES_SELMAGChange(Sender: TObject);
var
  fLow, fHigh: Real;
begin
  fLow:=0; fHigh:=0;
  SetMagInterval(fLow,fHigh);
  L__MAG_LOW.Caption:=FloatToStrF(fLow,ffFixed,8,1);
  L__MAG_HIGH.Caption:=FloatToStrF(fHigh,ffFixed,8,1);
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_9Click(Sender: TObject);
begin
  mrMagPos := 9;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := true;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.P__DEVICESClick(Sender: TObject);
begin
  P__DEVICES.Color:=$00101010;
  ExecDevices();
end;

procedure TF__ASTROTOOLBOX.P__FORWARD_PLUSClick(Sender: TObject);
begin
  SetTimeEdits(true,1);
end;

procedure TF__ASTROTOOLBOX.P__GO_HOMEClick(Sender: TObject);
begin
  P__GO_HOME.Color:=$00101010;
  ExecAlbireo();
end;

procedure TF__ASTROTOOLBOX.P__GO_HOURANGLEClick(Sender: TObject);
begin
  P__GO_HOURANGLE.Color:=$00101010;
  ExecHA();
end;

procedure TF__ASTROTOOLBOX.P__LICENSEDClick(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
    ExecOpen(gcsWebAlbireoSpenden)
  else
    ExecOpen(gcsWebAlbireoDonate);
end;

procedure TF__ASTROTOOLBOX.PMENU__ADMClick(Sender: TObject);
begin
  OpenADM();
end;

procedure TF__ASTROTOOLBOX.PMENU__CBClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_CB,not PMENU__CB.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__CONNAMESClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_CONNAMES,not PMENU__CONNAMES.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__CONClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_SM_CON, not PMENU__CON.Checked);
end;
(*
procedure TF__ASTROTOOLBOX.MENU__DONATEClick(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
    ExecOpen(gcsWebAlbireoSpenden)
  else
    ExecOpen(gcsWebAlbireoDonate);
end;
*)

procedure TF__ASTROTOOLBOX.PMENU__ASTEROIDSClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_ASTEROIDS, not PMENU__ASTEROIDS.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__DAY_PREVClick(Sender: TObject);
begin
  CB__WT.Date := CB__WT.Date - 1;
end;

procedure TF__ASTROTOOLBOX.PMENU__LT_1Click(Sender: TObject);
begin
  miLThickness := 1;
  CleanStartOfStarmap();
  PMENU__LT_1.Checked:=true;
  PMENU__LT_2.Checked:=false;
  PMENU__LT_3.Checked:=false;
end;

procedure TF__ASTROTOOLBOX.PMENU__LT_2Click(Sender: TObject);
begin
  miLThickness := 2;
  CleanStartOfStarmap();
  PMENU__LT_1.Checked:=false;
  PMENU__LT_2.Checked:=true;
  PMENU__LT_3.Checked:=false;
end;

procedure TF__ASTROTOOLBOX.PMENU__LT_3Click(Sender: TObject);
begin
  miLThickness := 3;
  CleanStartOfStarmap();
  PMENU__LT_1.Checked:=false;
  PMENU__LT_2.Checked:=false;
  PMENU__LT_3.Checked:=true;
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_0Click(Sender: TObject);
begin
  mrMagPos := 0;
  PMENU__MMAG_0.Checked := true;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_1Click(Sender: TObject);
begin
  mrMagPos := 1;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := true;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_2Click(Sender: TObject);
begin
  mrMagPos := 2;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := true;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_3Click(Sender: TObject);
begin
  mrMagPos := 3;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := true;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_4Click(Sender: TObject);
begin
  mrMagPos := 4;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := true;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_5Click(Sender: TObject);
begin
  mrMagPos := 5;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := true;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_6Click(Sender: TObject);
begin
  mrMagPos := 6;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := true;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_7Click(Sender: TObject);
begin
  mrMagPos := 7;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := true;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MMAG_8Click(Sender: TObject);
begin
  mrMagPos := 8;
  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := true;
  PMENU__MMAG_9.Checked := false;
  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.PMENU__MONTH_NEXTClick(Sender: TObject);
begin
  CB__WT.Date := CB__WT.Date + 30;
end;

procedure TF__ASTROTOOLBOX.PMENU__MONTH_PREVClick(Sender: TObject);
begin
  CB__WT.Date := CB__WT.Date - 30;
end;

procedure TF__ASTROTOOLBOX.PMENU__MW_ALWAYS_VISIBLEClick(Sender: TObject);
begin
  if(not PMENU__MW_ALWAYS_VISIBLE.Checked) then
  begin
    PMENU__MW_ALWAYS_VISIBLE.Checked := true;
    PMENU__MW_REALISTIC.Checked := false;
    PMENU__MW_REALISTIC.Enabled := true;
    PMENU__MW_SUPPRESSED.Checked := false;
    PMENU__MW_SUPPRESSED.Enabled := true;
    miShowMilkyway := -1;
    CleanStartOfStarmap();
    PMENU__MW_ALWAYS_VISIBLE.Enabled := false;
  end;
end;

procedure TF__ASTROTOOLBOX.MENU__HOR_0Click(Sender: TObject);
begin
  miLandscapeNo := 0;
  MENU__HOR_0.Checked:=true;
  MENU__HOR_1.Checked:=false;
  MENU__HOR_2.Checked:=false;
  MENU__HOR_3.Checked:=false;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.MENU__HOR_1Click(Sender: TObject);
begin
  miLandscapeNo := 1;
  MENU__HOR_0.Checked:=false;
  MENU__HOR_1.Checked:=true;
  MENU__HOR_2.Checked:=false;
  MENU__HOR_3.Checked:=false;
  MENU__HOR_3.Checked:=false;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.MENU_VIEW_NAVIGATIONClick(Sender: TObject);
begin
  SwitchNavigation();
end;

procedure TF__ASTROTOOLBOX.MENU__DSGVOClick(Sender: TObject);
begin
  F__DSGVO := TF__DSGVO.Create(nil);
  F__DSGVO.msLANG_ID:=msLANG_ID;
  F__DSGVO.ShowModal;
  F__DSGVO.Destroy;
end;

procedure TF__ASTROTOOLBOX.MENU__HOR_2Click(Sender: TObject);
begin
  miLandscapeNo := 2;
  MENU__HOR_0.Checked:=false;
  MENU__HOR_1.Checked:=false;
  MENU__HOR_2.Checked:=true;
  MENU__HOR_3.Checked:=false;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.MENU__OPENOBJClick(Sender: TObject);
var
  iIndex: Integer;
begin
  iIndex := -1;

  if((PMENU__AOBJECT.PopupComponent).Name = 'IMG__POS_MOON') then
    iIndex := molAOList.Count-1
  else if((PMENU__AOBJECT.PopupComponent).Name = 'SHP__POS_SUN') then
    iIndex := molAOList.Count-2
  else
    iIndex := (PMENU__AOBJECT.PopupComponent).Tag;// (PMENU__AOBJECT.PopupComponent).Tag;

  if(iIndex > -1) then
  begin
    ShowAOVis(iIndex);
    GenMap(P__STARMAP);
  end;

end;

function TF__ASTROTOOLBOX.GetAOIndexFromStarMap(): Integer;
begin
  Result := -1;

  if(PMENU__AOBJECT.PopupComponent is TShape) then
    Result := (PMENU__AOBJECT.PopupComponent as TShape).Tag
  else if (PMENU__AOBJECT.PopupComponent is TImage) then
    Result := (PMENU__AOBJECT.PopupComponent as TImage).Tag
  else
    exit;

end;

procedure TF__ASTROTOOLBOX.MENU__PLCOM_PATHClick(Sender: TObject);
var
  iAOIndex: Integer;
begin
  iAOIndex := GetAOIndexFromStarMap();
  if(iAOIndex < 0) then exit;

  if((molAOList[iAOIndex] as TAObject).sAOType = 'P') then
  begin
    (molAOList[iAOIndex] as TPlanet).bShowPath := not (molAOList[iAOIndex] as TPlanet).bShowPath;
    MENU__PLCOM_PATH.Checked := (molAOList[iAOIndex] as TPlanet).bShowPath;
  end;

  if((molAOList[iAOIndex] as TAObject).sAOType = 'C') then
  begin
    (molAOList[iAOIndex] as TComet).bShowPath := not (molAOList[iAOIndex] as TComet).bShowPath;
    MENU__PLCOM_PATH.Checked := (molAOList[iAOIndex] as TComet).bShowPath;
  end;

  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.MENU__ABOUTClick(Sender: TObject);
begin
  F__ABOUT := TF__ABOUT.Create(nil);
  F__ABOUT.msLANG_ID:=msLANG_ID;
  F__ABOUT.ShowModal;
  F__ABOUT.Destroy;
end;

procedure TF__ASTROTOOLBOX.MENU__ADEV_NAMEClick(Sender: TObject);
var
  sMsg, sNewName: string;
begin
  if(msLANG_ID = 'DE') then
    sMsg := 'Gerätename ändern'
  else
    sMsg := 'Change device name';

  sNewName:='';

  if(InputQuery(sMsg,'Name:',sNewName) and (Trim(sNewName) <> '')) then
  begin
    ChangeADEVName(sNewName);
  end;
end;

procedure TF__ASTROTOOLBOX.ED__ADEV_APTChange(Sender: TObject);
begin
  CalcADEVProp();
  CalcMagProp();
  EnableDevButtons();
end;

procedure TF__ASTROTOOLBOX.ED__ADEV_ARTNOChange(Sender: TObject);
begin
  EnableDevButtons();
end;

procedure TF__ASTROTOOLBOX.ED__ADEV_DESCRChange(Sender: TObject);
begin
  EnableDevButtons();
end;

procedure TF__ASTROTOOLBOX.ED__ADEV_FWChange(Sender: TObject);
begin
  CalcADEVProp();
  EnableDevButtons();
end;

procedure TF__ASTROTOOLBOX.ED__ADEV_MANUChange(Sender: TObject);
begin
  EnableDevButtons();
end;

procedure TF__ASTROTOOLBOX.ED__OCULAR_FWChange(Sender: TObject);
begin
  CalcMagProp();
end;


procedure TF__ASTROTOOLBOX.ED__SEARCHKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key = 13) then
    ExecAOSearch();
end;

procedure TF__ASTROTOOLBOX.ED__WT_HHClick(Sender: TObject);
begin
  if (not mbTimePlay) then
  begin
    P__REWIND_MINUS.Font.Color := clRed;
    P__FORWARD_PLUS.Font.Color := clRed;

    ED__WT_HH.Color:=$00464646;
    ED__WT_MM.Color:=$00101010;
    ED__WT_SS.Color:=$00101010;
  end;
end;

procedure TF__ASTROTOOLBOX.ED__WT_HHKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key = VK_UP) then
    SetTimeEdits(true,1)
  else if(Key = VK_DOWN) then
    SetTimeEdits(false,1)
  else if(Key = VK_RETURN) then
  begin
    if not (IsInteger(ED__WT_HH.Text)) then
      ED__WT_HH.Text := '00';

    if(StrToInt(ED__WT_HH.Text) < 0) or (StrToInt(ED__WT_HH.Text) > 23) then
      ED__WT_HH.Text := '00';

    ED__WT_MM.Text := '00';
    ED__WT_SS.Text := '00';
    SetTimeEdits(true,0);
  end;

end;

procedure TF__ASTROTOOLBOX.ED__WT_HHMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  SetTimeEdits(WheelDelta > 0,1);
end;

procedure TF__ASTROTOOLBOX.ED__WT_MMClick(Sender: TObject);
begin
  if (not mbTimePlay) then
  begin
    P__REWIND_MINUS.Font.Color := clRed;
    P__FORWARD_PLUS.Font.Color := clRed;

    ED__WT_HH.Color:=$00101010;
    ED__WT_MM.Color:=$00464646;
    ED__WT_SS.Color:=$00101010;
  end;

end;

procedure TF__ASTROTOOLBOX.ED__WT_MMKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key = VK_UP) then
    SetTimeEdits(true,1)
  else if(Key = VK_DOWN) then
    SetTimeEdits(false,1)
  else if(Key = VK_RETURN) then
  begin
    if not (IsInteger(ED__WT_MM.Text)) then
      ED__WT_MM.Text := '00';

    if(StrToInt(ED__WT_MM.Text) < 0) or (StrToInt(ED__WT_MM.Text) > 59) then
      ED__WT_MM.Text := '00';

    ED__WT_SS.Text := '00';
    SetTimeEdits(true,0);
  end;
end;

procedure TF__ASTROTOOLBOX.ED__WT_MMMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  SetTimeEdits(WheelDelta > 0,1);
end;

procedure TF__ASTROTOOLBOX.ED__WT_SSClick(Sender: TObject);
begin
  if (not mbTimePlay) then
  begin
    P__REWIND_MINUS.Font.Color := clRed;
    P__FORWARD_PLUS.Font.Color := clRed;

    ED__WT_HH.Color:=$00101010;
    ED__WT_MM.Color:=$00101010;
    ED__WT_SS.Color:=$00464646;
  end;
end;

procedure TF__ASTROTOOLBOX.ED__WT_SSKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(Key = VK_UP) then
    SetTimeEdits(true,1)
  else if(Key = VK_DOWN) then
    SetTimeEdits(false,1)
  else if(Key = VK_RETURN) then
  begin
    if not (IsInteger(ED__WT_SS.Text)) then
      ED__WT_SS.Text := '00';

    if(StrToInt(ED__WT_SS.Text) < 0) or (StrToInt(ED__WT_SS.Text) > 59) then
      ED__WT_SS.Text := '00';

    SetTimeEdits(true,0);
  end;
end;

procedure TF__ASTROTOOLBOX.ED__WT_SSMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  SetTimeEdits(WheelDelta > 0,1);
end;

procedure TF__ASTROTOOLBOX.FormActivate(Sender: TObject);
begin
  if(Assigned(F__ASTROCALC)) then
    F__ASTROCALC.Free;
end;

procedure TF__ASTROTOOLBOX.SwitchTimePLAY();
begin
  if(molAOList.Count = 0) then exit;

  L__BOTTOM_INFO.Caption := '';

  miTimePlayMode := 0;

  ED__WT_HH.Color:=$00101010;
  ED__WT_MM.Color:=$00101010;
  ED__WT_SS.Color:=$00101010;

  if(not mbTimePlay) then
  begin
    mdtStart := Now;
    mdtStart0 := GetWTime();
    mbTimePlay := true;

    //P__NOW.Visible := false;

    P__COUNTRIES.Visible := false;

    CB__WT.Enabled := false;
    ED__WT_HH.Enabled := false;
    ED__WT_MM.Enabled := false;
    ED__WT_SS.Enabled := false;

    SetGenMapInterval(false); //ciGraphNormalRefreshTime;
    TIMER__GENMAP.Enabled:=true;

    L__STARTSTOP.Caption:=';'; // Webdings font ||

    P__REWIND_MINUS.Tag := 0;
    P__FORWARD_PLUS.Tag := 0;
    P__REWIND_MINUS.Font.Color:=clRed;
    P__FORWARD_PLUS.Font.Color:=clRed;
    (*
    P__REWIND_MINUS.Font.Name:='Webdings';
    P__REWIND_MINUS.Caption:='7'; // Webdings font <<
    P__FORWARD_PLUS.Font.Name:='Webdings';
    P__FORWARD_PLUS.Caption:='8'; // Webdings font >>
    *)
  end
  else
  begin
    mdtStart := 0;
    mdtStart0 := 0;
    mbTimePlay := false;
    TIMER__GENMAP.Enabled:=false;

    //P__NOW.Visible:=true;

    P__COUNTRIES.Visible := true;

    CB__WT.Enabled := true;
    ED__WT_HH.Enabled := true;
    ED__WT_MM.Enabled := true;
    ED__WT_SS.Enabled := true;

    L__STARTSTOP.Caption:='4'; // Webdings font >

    P__REWIND_MINUS.Tag := 0;
    P__FORWARD_PLUS.Tag := 0;
    P__REWIND_MINUS.Font.Color:=clGray;
    P__FORWARD_PLUS.Font.Color:=clGray;

    (*
    P__REWIND_MINUS.Font.Name:='Arial'; <- Geht nicht zur Laufzeit!
    P__REWIND_MINUS.Caption:='-';

    P__FORWARD_PLUS.Font.Name:='Arial';
    P__FORWARD_PLUS.Caption:='+';
    *)
  end;

  CleanStartOfStarmap();
end;

procedure TF__ASTROTOOLBOX.B__TIMEPLAYClick(Sender: TObject);
begin
  SwitchTimePlay();
end;

procedure TF__ASTROTOOLBOX.Button2Click(Sender: TObject);
(*
var
  iRA_HH: Word; iRA_MM: Word; rRA_SS: Real;
  iDEC_DEG: SmallInt; iDEC_MM: SmallInt; rDEC_SS: Real;
  dtDateTime: TDateTime;
  Planet: TPlanet;
  Comet: TComet;
  i: Integer;
  sDummy: string;
  rLambdaSun,rMSun: Real;
  dtTimeMR: TDateTime;
  dtTimeMS: TDateTime;
  dtTimeCul: TDateTime;
  rHgt_Max: Real;
  rRs: Real;
  rRA, rDEC: Real;
  sType, sLine: string;
  slConList: TStringList;
  tfOut: TextFile;
  j: Integer;
*)
begin
  //P__STARMAP.Canvas.TextOut(100,100,'TEST');
  (*
  mbZoomMode := not (mbZoomMode);
  if(mbZoomMode) then
  begin
    mrZoomHgtMax := 70;
    mrZoomHgtMin := 30;
    mrZoomAzMax := 90;
    mrZoomAzMin := 40;
  end
  else
  begin
    mrZoomHgtMax := -999;
    mrZoomHgtMin := -999;
    mrZoomAzMax := -999;
    mrZoomAzMin := -999;
  end;

  GenMap();
  *)

  (*
  AssignFile(tfOut,'CBExt.dat');
  ReWrite(tfOut);

  for i:=0 to molAOList.Count-1 do
  begin
    sType := (molAOList[i] as TAObject).sAOType;
    rRA := (molAOList[i] as TAObject).iRA_Hours + (molAOList[i] as TAObject).iRA_Min/60.0;
    rDEC := (molAOList[i] as TAObject).iDec_Deg + (molAOList[i] as TAObject).iDec_Min/60.0;
    slConList := FindCon(rRA,rDEC);
    sLine := (molAOList[i] as TAObject).sLabel + ';' + sType;
    for j:=0 to 1 do
    begin
      if(j < slConList.Count) then
      begin
        sLine := sLine + ';' + slConList[j];
      end;
    end;
    WriteLn(tfOut,sLine);
    //ED__LICENSECODE.Text := IntToStr(i) + ' (' + sType + ')';
    //ED__LICENSECODE.Refresh;
    Application.ProcessMessages;
  end;

  CloseFile(tfOut);
  *)
  (*
  dtDateTime := EncodeDateTime(1984,1,1,0,0,0,0);
  Comet := nil;
  i := GetAObjectIndexByName('Halley','C');
  if(i>-1) then
  begin
    Comet := (molAOList[i] as TComet);

    GetCometCoo(dtDateTime,Comet,
        miDST_HH,miUTC_HH,
        iRA_HH,iRA_MM,rRA_SS,
        iDEC_DEG,iDEC_MM,rDEC_SS,rRs);
  end;
  *)

  //CalcMoonRiseAndMoonSet(dtTimeMR,dtTimeMS,dtTimeCul,rHgt_Max);

  (*
  rLambdaSun := 0; rMSun := 0;

  //dtDateTime := EncodeDateTime(2003,7,27,0,0,0,0);
  dtDateTime := EncodeDateTime(2003,9,1,0,0,0,0);
  GetSunCoo(dtDateTime,
    miDST_HH,miUTC_HH,rLambdaSun,rMSun,
    iRA_HH, iRA_MM, rRA_SS,
    iDEC_DEG, iDEC_MM, rDEC_SS);

  GetMoonCoo(dtDateTime,
    miDST_HH,miUTC_HH,rLambdaSun,rMSun,
    iRA_HH, iRA_MM, rRA_SS,
    iDEC_DEG, iDEC_MM, rDEC_SS);
  *)
  (*
  sDummy := '-3';
  ShowMessage(LeftStr(sDummy,1));

  dtDateTime := Now; //EncodeDateTime(2003,11,22,0,0,0,0);
  Planet := nil;
  i := GetAObjectIndexByName('Uranus','P');
  if(i>-1) then
    Planet := (molAOList[i] as TPlanet);

  if(Planet <> nil) then
    GetPlanetCoo(dtDateTime,Planet,
      miDST_HH,miUTC_HH,
      iRA_HH,iRA_MM,rRA_SS,
      iDEC_DEG,iDEC_MM,rDEC_SS);
  *)
  //ED__SEARCH.Color:=StringToColor('$00FFAAAA');
end;

procedure TF__ASTROTOOLBOX.B__ADEV_PHOTO_ADDClick(Sender: TObject);
var
  iIndex: Integer;
begin
  GetADev(CB__ADEV_NAME.Text,iIndex);

  if(iIndex > -1) and (ODLG__PIC.Execute) and (ODLG__PIC.FileName <> '') then
  begin
    try
      Screen.Cursor:=crHourGlass;

      (molADevList[iIndex] as TADevice).sImagePath := ODLG__PIC.FileName;
      IMG__ADEV_PHOTO.Picture.LoadFromFile(ConvertWinPath(ODLG__PIC.FileName));
    finally
      Screen.Cursor:=crDefault;
    end;

    mbADEVChanged := true;
  end;
end;

procedure TF__ASTROTOOLBOX.B__ADEV_MASK_NEWClick(Sender: TObject);
begin
  //ShowMessage(IntToStr(B__ADEV_MASK_NEW.Tag));

  if(B__ADEV_MASK_NEW.Tag = 0) then
  begin
    IniADev();
    if(msLANG_ID = 'DE') then
    begin
      B__ADEV_MASK_NEW.Caption:= 'Erfassung abbrechen';
      B__ADEV_DEL.Caption := 'Löschen';
    end
    else
    begin
      B__ADEV_MASK_NEW.Caption:= 'Cancel';
      B__ADEV_DEL.Caption := 'Delete';
    end;

    B__ADEV_MASK_NEW.Tag := 1;

  end
  else
  begin
    CB__ADEV_NAME.Enabled:=true;

    if(msLANG_ID = 'DE') then
      B__ADEV_MASK_NEW.Caption:= 'Neues Teleskop erfassen'
    else
      B__ADEV_MASK_NEW.Caption:= 'New Telescope';

    B__ADEV_MASK_NEW.Tag := 0;
  end;
end;

procedure TF__ASTROTOOLBOX.Button1Click(Sender: TObject);
begin
  ShowPrefs();
end;

procedure TF__ASTROTOOLBOX.BV__BOTTOMMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetStarmapView(BV__BOTTOM.Tag);
end;

procedure TF__ASTROTOOLBOX.BV__LEFTMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetStarmapView(BV__LEFT.Tag);
end;

procedure TF__ASTROTOOLBOX.BV__RIGHTMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetStarmapView(BV__RIGHT.Tag);
end;

procedure TF__ASTROTOOLBOX.B__ASTROCALCClick(Sender: TObject);
var
  sName: string;
  ADevice: TADevice;
  iIndex: Integer;
begin
  iIndex := -1;
  sName := Trim(CB__ADEV_NAME.Text);

  ADevice := GetADev(sName, iIndex);

  if(ADevice <> nil) then
  begin
    ShowAstroCalc(ADevice);
  end;

end;

procedure TF__ASTROTOOLBOX.IniADev();
begin
CB__ADEV_NAME.Text  := '';
ED__ADEV_DESCR.Text := '';
ED__ADEV_MANU.Text  := '';
ED__ADEV_ARTNO.Text := '';
ED__ADEV_FW.Text    := '';
ED__ADEV_APT.Text   := '';
ED__MAG.Text := '';
CB__ADEV_TYPE.Text := '';

IMG__ADEV_PHOTO.Picture := nil;

ED__ADEV_MANU.SetFocus;
CB__ADEV_NAME.Enabled:=false;

CBX__TELESCOPE_DEF.Checked:=false;

end;

procedure TF__ASTROTOOLBOX.B__ADEV_DELClick(Sender: TObject);
begin
  DelADev();
end;

procedure TF__ASTROTOOLBOX.B__ADEV_MODClick(Sender: TObject);
begin
  mbADEVChanged := true;

  if(CB__ADEV_NAME.Text <> '') then
    ModADev()
  else
    AddADev();

  B__ADEV_MOD.Enabled := false;
  CB__ADEV_NAME.Enabled := true;

  if(msLANG_ID = 'DE') then
    B__ADEV_MASK_NEW.Caption := 'Neues Teleskop erfassen'
  else
    B__ADEV_MASK_NEW.Caption := 'New Telescope';

  B__ADEV_MASK_NEW.Tag := 0;

end;

procedure TF__ASTROTOOLBOX.B__ADEV_PHOTO_DELClick(Sender: TObject);
var
  sMsg: string;
  iIndex: Integer;
begin
  if(msLANG_ID = 'DE') then
    sMsg := 'Möchten Sie dieses Gerätefoto nicht mehr verwenden?'
  else
    sMsg := 'Do you really remove the photo link?';

  if(MessageDlg(sMsg,mtConfirmation,[mbYes,mbNo],0) = mrNo) then exit;

  iIndex:=-1;
  GetADev(CB__ADEV_NAME.Text,iIndex);

  if(iIndex > -1) then
  begin
    (molADevList[iIndex] as TADevice).sImagePath := '';
    IMG__ADEV_PHOTO.Picture := nil;
    mbADEVChanged := true;
  end;

end;

procedure TF__ASTROTOOLBOX.B__AZIMUTH_INFOClick(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
    MessageDlg('Einstellung Azimutale Montierung',
      '- Azimutskala so einstellen, dass' +  #13#10 +
      '  Teleskop bei Markierung 0°/360 Grad' + #13#10 +
      '  nach Süden zeigt.' + #13#10 +
      '  90°: West, 180°: Nord, 270°: Ost',
      mtInformation,[mbOK],0)
  else
    MessageDlg('Setup azmiuth mount',
      '- For 0°/360° azimuth the telescope' + #13#10 +
      '  must be  directed to SOUTH.' + #13#10 +
      '  WEST: 90°' + #13#10 +
      '  NORTH: 180°' + #13#10 +
      '  EAST: 270°',
      mtInformation,[mbOK],0);

end;

procedure TF__ASTROTOOLBOX.RecalcStarMap();
var
  iIndex: Integer;
  EarthLocation: TEarthLocation;
begin
  iIndex := CB__CITIES.ItemIndex;
  if(iIndex > 0) and (Trim(CB__CITIES.Text) <> '') and (Trim(CB__COUNTRIES.Text) <> '') and (not mbChangedLoc) then
  begin
    if(not mbChangedLoc) then
    begin
      miGLat_DEG_DEF := miGLat_DEG;
      miGLat_MIN_DEF := miGLat_MIN;
      miGLng_DEG_DEF := miGLng_DEG;
      miGLng_MIN_DEF := miGLng_MIN;
      mrSin_fGLat_DEF := mrSin_fGLat;
      mrCos_fGLat_DEF := mrCos_fGLat;
      miUTC_HH_DEF := miUTC_HH;
    end;

    mbChangedLoc := true;

    mrSin_fGLat := sin(Pi*(miGLat_DEG + miGLat_MIN/60.0)/180.0);
    mrCos_fGLat := cos(Pi*(miGLat_DEG + miGLat_MIN/60.0)/180.0);

    EarthLocation := (CB__CITIES.Items.Objects[iIndex] as TEarthLocation);
    miGLat_DEG := Trunc(EarthLocation.rLatitude_DEG);
    miGLat_MIN := Trunc(EarthLocation.rLatitude_MIN);
    miGLng_DEG := Trunc(EarthLocation.rLongitude_DEG);
    miGLng_MIN := Trunc(EarthLocation.rLongitude_MIN);

    mrSin_fGLat := sin(Pi*(miGLat_DEG + miGLat_MIN/60.0)/180.0);
    mrCos_fGLat := cos(Pi*(miGLat_DEG + miGLat_MIN/60.0)/180.0);

    miUTC_HH := EarthLocation.iTimeZone;
    SetTime(GetWTime() - ((miUTC_HH_DEF - miUTC_HH)/24.0));

    //P__NOW.Visible := false;

    B__LOC_RESET.Visible:=true;
  end;

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
    CleanStartOfStarmap();

end;

procedure TF__ASTROTOOLBOX.B__LOC_RESETClick(Sender: TObject);
begin
  if(mbChangedLoc) then
  begin
    SetTime(GetWTime() + ((miUTC_HH_DEF - miUTC_HH)/24.0));

    miGLat_DEG := miGLat_DEG_DEF;
    miGLat_MIN := miGLat_MIN_DEF;
    miGLng_DEG := miGLng_DEG_DEF;
    miGLng_MIN := miGLng_MIN_DEF;
    mrSin_fGLat := mrSin_fGLat_DEF;
    mrCos_fGLat := mrCos_fGLat_DEF;
    miUTC_HH := miUTC_HH_DEF;

    mbChangedLoc := false;

    if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
      CleanStartOfStarmap();

    CB__COUNTRIES.ItemIndex:=0;
    CB__CITIES.ItemIndex:=0;

    //P__NOW.Visible := true;
    B__LOC_RESET.Visible:=false;
  end;
end;

procedure TF__ASTROTOOLBOX.B__MAGSIZE_G_RESETClick(Sender: TObject);
begin
  if(mbZoomMode) then
    mrMagPos_G := crMagPosStdZoom_G
  else
    mrMagPos_G := crMagPosStd_G;

  TB__MAG_G.Position:=Trunc(mrMagPos_G*10.0);

  L__MAGSIZE_G.Caption:= AnsiReplaceStr(FloatToStrF(mrMagPos_G,ffFixed,8,1),',','.');

  GenMap(P__STARMAP);
  //CleanStartOfStarmap();

end;

procedure TF__ASTROTOOLBOX.B__MAGSIZE_RESETClick(Sender: TObject);
begin
  if(mbZoomMode) then
    mrMagPos := crMagPosStdZoom
  else
    mrMagPos := crMagPosStd;

  TB__MAG.Position:=Trunc(mrMagPos*10.0);

  L__MAGSIZE.Caption:= AnsiReplaceStr(FloatToStrF(mrMagPos,ffFixed,8,1),',','.');

  PMENU__MMAG_0.Checked := false;
  PMENU__MMAG_1.Checked := false;
  PMENU__MMAG_2.Checked := false;
  PMENU__MMAG_3.Checked := false;
  PMENU__MMAG_4.Checked := false;
  PMENU__MMAG_5.Checked := false;
  PMENU__MMAG_6.Checked := false;
  PMENU__MMAG_7.Checked := false;
  PMENU__MMAG_8.Checked := false;
  PMENU__MMAG_9.Checked := false;
  PMENU__MMAG_9.Checked := false;
  PMENU__MMAG_9.Checked := false;
  PMENU__MMAG_9.Checked := false;

  if(mbZoomMode) then
    PMENU__MMAG_7.Checked := true
  else
    PMENU__MMAG_5.Checked := true;

  //GenMap(P__STARMAP);
  CleanStartOfStarmap();

end;

procedure TF__ASTROTOOLBOX.B__PARALLACTIC_INFOClick(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
    MessageDlg('Einstellung der Stundenwinkel-Skala',
      '- Die Stundenachst muss auf den Himmelspol' +  #13#10 +
      '  nahe Polaris zeigen' + #13#10 +
      '- Bei waagerechter Deklinationsachse' + #13#10 +
      '   muss der Zeiger der Stundenachse' + #13#10 +
      '   auf 0h stehen',
      mtInformation,[mbOK],0)
  else
    MessageDlg('Setup hour angle scale',
      '- Hour axis MUST be directed to' + #13#10 +
      '  north pole (near polaris)' + #13#10 +
      '- When the declination axis is moved horizontal' + #13#10 +
      '  the pointer of the hour axis scale' + #13#10 +
      '  has to point to 0h',
      mtInformation,[mbOK],0);
end;

procedure TF__ASTROTOOLBOX.B__RESET_TABClick(Sender: TObject);
var
  rLow, rHigh: Real;
begin
  if(RB__S.Checked) then
  begin
    TB__SELMAG.Position := Round(crMagPosStd)*10;

    rLow := -2.0; rHigh := crMagPosStd;

    SetMagInterval(rLow,rHigh);
    SetDBSheetMaxMag(rLow,rHigh);
  end
  else if(RB__G.Checked) then
  begin
    TB__SELMAG_G.Position := Round(crMagPosStd_G)*10;

    rLow := 0; rHigh := crMagPosStd_G;

    SetMagInterval(rLow,rHigh);
    SetDBSheetMaxMag(rLow,rHigh);
  end;

end;

procedure TF__ASTROTOOLBOX.B__SEARCH_CLEARClick(Sender: TObject);
begin
  ClearSearch();
  CleanStartOfStarMap();
end;

procedure TF__ASTROTOOLBOX.BB__SWITCH_LANGClick(Sender: TObject);
begin
  SwitchLang();
end;

procedure TF__ASTROTOOLBOX.CBX__AZ_SCALEClick(Sender: TObject);
begin
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.CBX__CERESChange(Sender: TObject);
begin
  ShowSolSys();
end;

procedure TF__ASTROTOOLBOX.CBX__LTClick(Sender: TObject);
begin
  TIMER__AOTABLE.Enabled := CBX__LT.Checked;
  TB__LT.Enabled := CBX__LT.Checked;
  if(CBX__LT.Checked) then
  begin
    L__LT_INTERVAL.Font.Color:=clRed;
    miAOTableSec := TB__LT.Position*60;
  end
  else
    IniLiveTableMode();

end;

procedure TF__ASTROTOOLBOX.CBX__EXACTMATCHClick(Sender: TObject);
begin
  if(Trim(ED__SEARCH.Text) <> '') then
  begin
    if(CBX__EXACTMATCH.Checked) then
      ExecAOSearch()
    else
    begin
      if(msLANG_ID = 'DE') and (MessageDlg('Rückfrage','Die Suche ohne extakte Übereinstimmung kann u.U. mehrere Minuten in Anspruch nehmen. Suche wirklich fortsetzten?',
        mtConfirmation,[mbYes,mbNo],0) = mrYes) then
        ExecAOSearch();

      if(msLANG_ID = 'EN') and (MessageDlg('Request','Search without exact match could take serveral minutes. Continue?',
        mtConfirmation,[mbYes,mbNo],0) = mrYes) then
        ExecAOSearch();
    end;
  end;
end;

procedure TF__ASTROTOOLBOX.CBX__PLUTOChange(Sender: TObject);
begin
  ShowSolSys();
end;

procedure TF__ASTROTOOLBOX.CBX__TELESCOPE_DEFChange(Sender: TObject);
begin
  EnableDevButtons();
end;

procedure TF__ASTROTOOLBOX.CB__ADEV_NAMEChange(Sender: TObject);
var
  sName: string;
  iIndex: Integer;
  ADevice: TADevice;
begin
  sName := Trim(CB__ADEV_NAME.Text);

  if(msLANG_ID = 'DE') then
    B__ADEV_DEL.Caption:=  sName + ' löschen'
  else
    B__ADEV_DEL.Caption:=  'Delete ' + sName;

  iIndex:=-1;
  ADevice := GetADev(sName, iIndex);

  if(ADevice <> nil) then
  begin
    ShowADev(ADevice);
    CalcMagProp();
  end;

  EnableDevButtons();

  B__ADEV_MOD.Enabled := false;
end;

procedure TF__ASTROTOOLBOX.CB__ADEV_OK_TYPEChange(Sender: TObject);
begin
  {
  Kellner: 40°
  Plössl: 50°
  Super-Plössl: 52°
  Ultra-Wide-Angle: 66°
  Panoptik: 68°
  Nagler: 82°
  }
  CalcMagProp();
end;

procedure TF__ASTROTOOLBOX.CB__ADEV_TYPEChange(Sender: TObject);
begin
  EnableDevButtons();
end;

procedure TF__ASTROTOOLBOX.CB__AOChange(Sender: TObject);
begin
  ShowAO();
end;

procedure TF__ASTROTOOLBOX.CB__ASTEROIDSChange(Sender: TObject);
var
  iIndex: Integer;
  iIndexACD: Integer;
begin
  iIndexACD := -1;
  iIndex := CB__ASTEROIDS.ItemIndex;

  if(iIndex > 1) then // Skip first 2 entries
  begin
    iIndexACD := mslAsteroidsCometsDisplayed.IndexOf(CB__ASTEROIDS.Items[iIndex]);

    if(iIndexACD > -1) then
      mslAsteroidsCometsDisplayed.Delete(iIndexACD)
    else
      mslAsteroidsCometsDisplayed.Add(CB__ASTEROIDS.Items[iIndex]);
  end;

  ShowSolSys();

  if(TB__SOLSYS.Position < 3) and (iIndexACD = -1) then
  begin
    if(msLANG_ID = 'DE') then
      MessageDlg('Info','Astroidenbahnen außerhalb des eingestellten Rahmens',mtInformation,[mbOK],0)
    else
      MessageDlg('Info','Astroid belt outside of selected range',mtInformation,[mbOK],0);

  end;
end;

procedure TF__ASTROTOOLBOX.CB__CITIESCloseUp(Sender: TObject);
begin
  if((CB__CITIES.ItemIndex > 0) and (Trim(CB__CITIES.Text) <> '')) then
    RecalcStarMap();
end;

procedure TF__ASTROTOOLBOX.CB__COMETSChange(Sender: TObject);
var
  iIndex: Integer;
  iIndexACD: Integer;
begin
  iIndex := CB__COMETS.ItemIndex;
  if(iIndex > 1) then // Skip first 2 entries
  begin
    iIndexACD := mslAsteroidsCometsDisplayed.IndexOf(CB__COMETS.Items[iIndex]);

    if(iIndexACD > -1) then
      mslAsteroidsCometsDisplayed.Delete(iIndexACD)
    else
      mslAsteroidsCometsDisplayed.Add(CB__COMETS.Items[iIndex]);
  end;

  ShowSolSys();

end;

procedure TF__ASTROTOOLBOX.CB__COUNTRIESCloseUp(Sender: TObject);
var
  i, iIndex: Integer;
  slCities: TStringList;
  sCountrySel, sCityCooFile: string;
begin
  iIndex := CB__COUNTRIES.ItemIndex;

  if(iIndex < 0) then
    exit;

  sCountrySel := CB__COUNTRIES.Items[iIndex];

  CB__CITIES.Clear;

  slCities := TStringList.Create;

  if(msLANG_ID = 'DE') then
    sCityCooFile := gsAlbireoLocalDir + 'CityCoo.txt'
  else
    sCityCooFile := gsAlbireoLocalDir + 'CityCoo_EN.txt';

  GetCitiesList(sCountrySel,msLANG_ID,slCities,sCityCooFile);

  if(msLANG_ID = 'DE') then
    CB__CITIES.Items.Add('-Städte-')
  else
    CB__CITIES.Items.Add('-Cities-');

  for i:=0 to slCities.Count-1 do
  begin
    CB__CITIES.AddItem(slCities[i],slCities.Objects[i]);
  end;

  slCities.Destroy;

  CB__CITIES.ItemIndex:=0;

end;
procedure TF__ASTROTOOLBOX.ShowAO();
var
  sText: string;
  iSel: Integer;
  iRA_HH, iRA_MIN, iRA_SEC: SmallInt;
  iDEC_DEG, iDEC_MIN, iDEC_SEC: SmallInt;
  iST_HH, iST_MIN, iST_SEC, iST_MS: Word;
  sMessier: string;
begin
  iRA_HH:=0; iRA_MIN:=0; iRA_SEC:=0;
  iDEC_DEG:=0; iDEC_MIN:=0; iDEC_SEC:=0;

  iSel := CB__AO.ItemIndex;

  if(iSel > -1) then
  begin
    //P__STAR_LOC_MAIN.Visible := (CB__AO.Text <> '');

    // RA
    iRA_HH := (CB__AO.Items.Objects[iSel] as TAObject).iRA_Hours;
    iRA_MIN := (CB__AO.Items.Objects[iSel] as TAObject).iRA_Min;
    iRA_SEC := Round((CB__AO.Items.Objects[iSel] as TAObject).rRA_Sec);

    // DECLINATION
    iDEC_DEG := (CB__AO.Items.Objects[iSel] as TAObject).iDec_Deg;
    iDEC_MIN := (CB__AO.Items.Objects[iSel] as TAObject).iDec_Min;
    iDEC_SEC := Round((CB__AO.Items.Objects[iSel] as TAObject).rDec_Sec);

    iST_HH:=0; iST_MIN:=0; iST_SEC:=0; iST_MS:=0;
    CalcST(iST_HH,iST_MIN,iST_SEC,iST_MS);

    CalcHA(
      iST_HH, iST_MIN, iST_SEC,
      iRA_HH, iRA_MIN, iRA_SEC);

    if((CB__AO.Items.Objects[iSel] as TAObject).sAOType = 'P') or
      ((CB__AO.Items.Objects[iSel] as TAObject).sAOType = 'C') then
    begin
      if(msLANG_ID = 'DE') then
        if((CB__AO.Items.Objects[iSel] as TAObject).sName_DE <> '') then
          sText := (CB__AO.Items.Objects[iSel] as TAObject).sName_DE;

      if(msLANG_ID = 'EN') then
        if((CB__AO.Items.Objects[iSel] as TAObject).sName_EN <> '') then
          sText := (CB__AO.Items.Objects[iSel] as TAObject).sName_EN;
    end
    else
    begin
      if((CB__AO.Items.Objects[iSel] as TAObject).sAOType = 'S') then
        sText := (CB__AO.Items.Objects[iSel] as TStar).sSym + '-' +
          (CB__AO.Items.Objects[iSel] as TInterstellarObject).sCon
      else
      begin
        sText := (CB__AO.Items.Objects[iSel] as TInterstellarObject).sNGC + '-' +
          (CB__AO.Items.Objects[iSel] as TInterstellarObject).sCon;

        sMessier := (CB__AO.Items.Objects[iSel] as TInterstellarObject).sMessier;
        sText := Trim(sMessier + ' ' + sText);
      end;

      if(msLANG_ID = 'DE') then
        if((CB__AO.Items.Objects[iSel] as TAObject).sName_DE <> '') then
          sText := sText + ' (' + (CB__AO.Items.Objects[iSel] as TAObject).sName_DE + ')';

      if(msLANG_ID = 'EN') then
        if((CB__AO.Items.Objects[iSel] as TAObject).sName_EN <> '') then
          sText := sText + ' (' + (CB__AO.Items.Objects[iSel] as TAObject).sName_EN + ')';
    end;

  end;

  if(iDEC_SEC >= 60) then
  begin
    iDEC_MIN := iDEC_MIN + iDEC_SEC div 60;
    iDEC_SEC := iDEC_SEC - 60*(iDEC_SEC div 60);
  end;

  L__DEC_SIGN.Visible := (iDEC_DEG < 0);
  ED__DEC_DEG.Text := format('%.2d',[abs(iDEC_DEG)]);//IntToStr(iDEC_DEG);
  ED__DEC_MM.Text := format('%.2d',[abs(iDEC_MIN)]);//IntToStr(iDEC_MIN);
  ED__DEC_SS.Text := format('%.2d',[abs(iDEC_SEC)]);//IntToStr(iDEC_SEC);

  if(iRA_SEC >= 60) then
  begin
    iRA_MIN := iRA_MIN + iRA_SEC div 60;
    iRA_SEC := iRA_SEC - 60*(iRA_SEC div 60);
  end;

  ED__RZ_HH.Text := format('%.2d',[iRA_HH]);//IntToStr(iRA_HH);
  ED__RZ_MM.Text := format('%.2d',[iRA_MIN]);//IntToStr(iRA_MIN);
  ED__RZ_SS.Text := format('%.2d',[iRA_SEC]);//IntToStr(iRA_SEC);

end;

procedure TF__ASTROTOOLBOX.CB__SIGNSChange(Sender: TObject);
begin
  miSCI_GRD_AO := 0;
  miSCO_GRD_AO := -1;

  SelSign();

  ShowAOTable();

  if(CB__SIGNS.ItemIndex > -1) and (LeftStr(CB__SIGNS.Items[CB__SIGNS.ItemIndex],1) <> '_') then
    L__CONSTELL.Caption := CB__SIGNS.Items[CB__SIGNS.ItemIndex]
  else
  begin
    if(msLANG_ID = 'DE') then
      L__CONSTELL.Caption := 'Bitte Sternbild auswählen'
    else
      L__CONSTELL.Caption := 'Please select constellation';

  end;

end;

procedure TF__ASTROTOOLBOX.CB__WTChange(Sender: TObject);
begin
  DateRefresh();
end;

procedure TF__ASTROTOOLBOX.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  sCommentFile: string;
begin
  if(mbADEVChanged) then
    SaveDevices(molADevList);

  sCommentFile := ConvertWinPath(gsAlbireoLocalDir + 'AOUserFields.dat');

  if(mslAOUserFields.Count > 0) then
    mslAOUserFields.SaveToFile(sCommentFile);

end;

procedure TF__ASTROTOOLBOX.MENU__PREFSClick(Sender: TObject);
begin
  ShowPrefs();

  ShowLatLong();

  PMENU__LT_1.Checked:=(miLThickness = 1);
  PMENU__LT_2.Checked:=(miLThickness = 2);
  PMENU__LT_3.Checked:=(miLThickness = 3);

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
    GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.MENU__QUITClick(Sender: TObject);
begin
  Close;
end;

procedure TF__ASTROTOOLBOX.MENU__SELSIGNClick(Sender: TObject);
var
  iAOIndex: Integer;
  iSignIndex: Integer;
  Sign: TSign;
  sCon: string;
begin
  // Find astronomicl object
  iAOIndex := GetAOIndexFromStarMap();
  if(iAOIndex < 0) then exit;

  if((molAOList[iAOIndex] as TAObject).sAOType <> 'S') then
    exit;

  //iAOIndex := (PMENU__AOBJECT.PopupComponent as TShape).Tag;
  sCon := (molAOList[iAOIndex] as TStar).sCon;

  // Find Sign
  iSignIndex:=-1;
  Sign := GetSignObj(sCon,molSignList,iSignIndex);

  // Switch selection status!
  if(Sign <> nil) then
  begin
    (*
    if((molAOList[iAOIndex] as TStar).bSelSign) then
      ShowMessage('Index: ' + IntToStr(iAOIndex) + ', bSelSign is TRUE')
    else
      ShowMessage('Index: ' + IntToStr(iAOIndex) + ', bSelSign is FALSE');
    *)
    (molAOList[iAOIndex] as TStar).bSelSign := not Sign.bSelected; // Save selection status in star object

    SignActive(iSignIndex,not Sign.bSelected);
    CleanStartOfStarMap();
    //GenMap(P__STARMAP);
  end;

end;

procedure TF__ASTROTOOLBOX.MENU__SOLSYSClick(Sender: TObject);
begin
  ExecSolSys();
end;

procedure TF__ASTROTOOLBOX.MENU__TIMESTATClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.MENU__UPDATEClick(Sender: TObject);
begin
  SendUpdateReq();
end;

procedure TF__ASTROTOOLBOX.MENU__VIEW_TIMECONTROLClick(Sender: TObject);
begin
  SwitchTimeControl();
end;

procedure TF__ASTROTOOLBOX.PMENU__AOBJECTPopup(Sender: TObject);
var
  iIndex: Integer;
begin
  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
  begin
    MENU__SELSIGN.Visible:=true;
    MENU__PLCOM_PATH.Visible:=true;

    iIndex := (PMENU__AOBJECT.PopupComponent).Tag;

    if(iIndex > -1) then
    begin
      MENU__PLCOM_PATH.Enabled := (
        ((molAOList[iIndex] as TAObject).sAOType = 'P') or
        ((molAOList[iIndex] as TAObject).sAOType = 'C')
        );
    end;

  end
  else
  begin
    MENU__SELSIGN.Visible:=false;
    MENU__PLCOM_PATH.Visible:=false;
  end;

end;

procedure TF__ASTROTOOLBOX.PMENU__MERIClick(Sender: TObject);
begin
  mbSM_MERI := not mbSM_MERI;
  PMENU__MERI.Checked:=mbSM_MERI;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__MW_REALISTICClick(Sender: TObject);
begin
  if(not PMENU__MW_REALISTIC.Checked) then
  begin
    PMENU__MW_REALISTIC.Checked:=true;
    PMENU__MW_REALISTIC.Checked := true;
    PMENU__MW_ALWAYS_VISIBLE.Checked := false;
    PMENU__MW_ALWAYS_VISIBLE.Enabled := true;
    PMENU__MW_SUPPRESSED.Checked :=  false;
    PMENU__MW_SUPPRESSED.Enabled := true;
    miShowMilkyway := 1;
    CleanStartOfStarmap();
    PMENU__MW_REALISTIC.Enabled := false;
  end;
end;

procedure TF__ASTROTOOLBOX.PMENU__MW_SUPPRESSEDClick(Sender: TObject);
begin
  if(not PMENU__MW_SUPPRESSED.Checked) then
  begin
    PMENU__MW_SUPPRESSED.Checked:=true;
    PMENU__MW_ALWAYS_VISIBLE.Checked := false;
    PMENU__MW_ALWAYS_VISIBLE.Enabled := true;
    PMENU__MW_REALISTIC.Checked :=  false;
    PMENU__MW_REALISTIC.Enabled := true;
    miShowMilkyway := 0;
    CleanStartOfStarmap();
    PMENU__MW_SUPPRESSED.Enabled := false;
  end;

end;

procedure TF__ASTROTOOLBOX.PMENU__NEXT_DAYClick(Sender: TObject);
begin
  CB__WT.Date := CB__WT.Date + 1;
end;

procedure TF__ASTROTOOLBOX.PMENU__POLEPClick(Sender: TObject);
begin
  mbShowPoleP := not mbShowPoleP;
  PMENU__POLEP.Checked:=mbShowPoleP;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__PVISClick(Sender: TObject);
begin
  mbPlanetImage := not mbPlanetImage;
  PMENU__PVIS.Checked:=mbPlanetImage;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__PSHOWClick(Sender: TObject);
begin
  mbPlanetShow := not mbPlanetShow;
  PMENU__PSHOW.Checked:=mbPlanetShow;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__STAR_DESCR_HIGHClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_STAR_DESCR_HIGH, not PMENU__STAR_DESCR_HIGH.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__STAR_DESCR_LOWClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_STAR_DESCR_LOW, not PMENU__STAR_DESCR_LOW.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__VIEW_EClick(Sender: TObject);
begin
  SetStarmapView(3);
end;

procedure TF__ASTROTOOLBOX.PMENU__VIEW_NClick(Sender: TObject);
begin
  SetStarmapView(1);
end;

procedure TF__ASTROTOOLBOX.PMENU__VIEW_SClick(Sender: TObject);
begin
  SetStarmapView(2);
end;

procedure TF__ASTROTOOLBOX.PMENU__VIEW_STARMAPClick(Sender: TObject);
begin
  SetStarmapView(0);
end;

procedure TF__ASTROTOOLBOX.PMENU__VIEW_WClick(Sender: TObject);
begin
  SetStarmapView(4);
end;

procedure TF__ASTROTOOLBOX.PMENU__WEEK_NEXTClick(Sender: TObject);
begin
  CB__WT.Date := CB__WT.Date + 7;
end;

procedure TF__ASTROTOOLBOX.PMENU__WEEK_PREVClick(Sender: TObject);
begin
  CB__WT.Date := CB__WT.Date - 7;
end;

procedure TF__ASTROTOOLBOX.P__MAPClick(Sender: TObject);
begin
  P__MAP.Color:=$00101010;
  ExecMAP();
end;

procedure TF__ASTROTOOLBOX.P__MAPMouseEnter(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_STARMAP) then
  begin
    P__MAP.Color := clMaroon; // $00000022;
    L__MAP.Font.Color:=clWhite;
  end;

end;

procedure TF__ASTROTOOLBOX.P__MAPMouseLeave(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_STARMAP) then
  begin
    P__MAP.Color := $00101010;
    L__MAP.Font.Color:=clSilver;
  end;
end;

procedure TF__ASTROTOOLBOX.P__NAVIG_TITLEClick(Sender: TObject);
begin
  ExecAlbireo();
end;

procedure TF__ASTROTOOLBOX.P__NAV_SOLSYSClick(Sender: TObject);
begin
  P__NAV_SOLSYS.Color:=$00101010;
  ExecSolSys();
end;

procedure TF__ASTROTOOLBOX.P__NAV_SOLSYSMouseEnter(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_SOLSYS) then
  begin
    P__NAV_SOLSYS.Color := clMaroon; // $00000022;
    L__NAVSOLSYS.Font.Color:=clWhite;
  end;
end;

procedure TF__ASTROTOOLBOX.P__NAV_SOLSYSMouseLeave(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_SOLSYS) then
  begin
    P__NAV_SOLSYS.Color := $00101010;
    L__NAVSOLSYS.Font.Color:=clSilver;
  end;

end;

procedure TF__ASTROTOOLBOX.P__NOWClick(Sender: TObject);
begin
  ClearSearch;

  SetNow();
  ReCalcPlanetPos(Now);

  case PC__WORKBENCH.ActivePageIndex of
    ciPAGE_DB: ShowAOTable();
    ciPAGE_STARMAP: CleanStartOfStarmap();
    ciPAGE_SOLSYS: ShowSolSys();
  end;

end;

procedure TF__ASTROTOOLBOX.P__PKClick(Sender: TObject);
begin
  CalcKochabMethod();
end;

procedure TF__ASTROTOOLBOX.PC__WORKBENCHChange(Sender: TObject);
begin
  MENU__VIEW_TIMECONTROL.Checked:=P__SELECT.Visible;

  LB__SEARCHRES.Visible:=false;
  P__SEARCHALL.Height := 35;
  SetP__MAGSIZE(false);

  TIMER__GENMAP.Enabled:=false;

  case PC__WORKBENCH.ActivePageIndex of
    ciPAGE_HA:
    begin
      if(RB__MESSIER.Checked) then
      begin
        RB__MESSIER.Checked:=false;
        RB__S.Checked:=true;
      end;
      Application.ProcessMessages;
      Refresh();
      SelSign();

      P__KOCHAB.Visible:=false;
      P__AOTYPE.Visible:= true;
    end;
    ciPAGE_DB:
    begin
      if(RB__MESSIER.Checked) then
      begin
        RB__MESSIER.Checked:=false;
        RB__S.Checked:=true;
      end;

      P__KOCHAB.Visible:=false;
      P__AOTYPE.Visible:= true;

      // Stop livetable-mode
      IniLiveTableMode();

    end;
    ciPAGE_STARMAP:
    begin
      P__KOCHAB.Visible:=true;
      P__AOTYPE.Visible := false;
      TIMER__GENMAP.Enabled:=mbTimePlay;
    end;
    else
    begin
      P__KOCHAB.Visible:=true;
      P__AOTYPE.Visible := false;
    end;
  end; // case

  MENU__EXPORT_SVG.Enabled := (PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__AUXLINE_RESETClick(Sender: TObject);
begin
  PMENU__ECLIPTIC.Checked := false;
  mbSM_ECLIPTIC := PMENU__ECLIPTIC.Checked;

  PMENU__EQUATOR.Checked := false;
  mbSM_EQUATOR := PMENU__EQUATOR.Checked;

  PMENU__GALACTIC.Checked := false;
  mbSM_GALACTIC := PMENU__GALACTIC.Checked;

  PMENU__AS.Checked := false;
  mbSM_AS := PMENU__AS.Checked;

  PMENU__HS.Checked := false;
  mbSM_HS := PMENU__HS.Checked;

  mbSM_RA := false;
  PMENU__RA_SCALA.Checked:=mbSM_RA;

  mbSM_DEC := false;
  PMENU__DEC_SCALA.Checked:=mbSM_DEC;

  mbSM_MERI := false;
  PMENU__MERI.Checked:=mbSM_MERI;

  GenMap(P__STARMAP);

end;

procedure TF__ASTROTOOLBOX.PMENU__ECLIPTICClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_SM_ECLIPTIC, not PMENU__ECLIPTIC.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__EQUATORClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_SM_EQUATOR, not PMENU__EQUATOR.Checked);
end;

procedure TF__ASTROTOOLBOX.ActivateAOFeature(iAOFeature: Integer; bActive: Boolean);
begin
  case iAOFeature of
    ciAOF_COO_EQU_RA:
    begin
      PMENU__RA_SCALA.Checked:=bActive;
      mbSM_RA := PMENU__RA_SCALA.Checked;
    end;
    ciAOF_COO_EQU_DEC:
    begin
      PMENU__DEC_SCALA.Checked:=bActive;
      mbSM_DEC := PMENU__DEC_SCALA.Checked;
    end;
    ciAOF_CB:
    begin
      PMENU__CB.Checked := bActive;
      mbShowCB := PMENU__CB.Checked;
    end;
    ciAOF_CONNAMES:
    begin
      PMENU__CONNAMES.Checked := bActive;
      mbShowConstellations := PMENU__CONNAMES.Checked;
    end;
    ciAOF_EComets:
    begin
      PMENU__COMETS.Checked := bActive;
      mbShowEComets := PMENU__COMETS.Checked;
    end;
    ciAOF_SM_CON:
    begin
      PMENU__CON.Checked := bActive;
      mbSM_CON := PMENU__CON.Checked;
    end;
    ciAOF_SM_ECLIPTIC:
    begin
      PMENU__ECLIPTIC.Checked := bActive;
      mbSM_ECLIPTIC := PMENU__ECLIPTIC.Checked;
    end;
    ciAOF_SM_EQUATOR:
    begin
      PMENU__EQUATOR.Checked := bActive;
      mbSM_EQUATOR := PMENU__EQUATOR.Checked;
    end;
    ciAOF_SM_GALACTIC:
    begin
      PMENU__GALACTIC.Checked := bActive;
      mbSM_GALACTIC := PMENU__GALACTIC.Checked;
    end;
    ciAOF_Galaxies:
    begin
      PMENU__GALAXIES.Checked := bActive;
      mbShowGalaxies := PMENU__GALAXIES.Checked;
      TS__MAG_GAL.TabVisible := mbShowGalaxies;
      if(mbShowGalaxies) then
      begin
        PC__MAG.ActivePageIndex:=1;
        TB__MAG_G.Position := Trunc(crMagPosStd_G*10);
        mrMagPos_G := crMagPosStd_G;
      end
      else
        PC__MAG.ActivePageIndex:=0;

    end;
    ciAOF_Quasars:
    begin
      PMENU__Q.Checked := bActive;
      mbShowQuasars := PMENU__Q.Checked;
    end;
    ciAOF_GlobularClusters:
    begin
      PMENU__GC.Checked := bActive;
      mbShowGlobularClusters := PMENU__GC.Checked;
    end;
    ciAOF_SM_HS:
    begin
      PMENU__HS.Checked := bActive;
      mbSM_HS := PMENU__HS.Checked;
    end;
    ciAOF_SM_AS:
    begin
      PMENU__AS.Checked := bActive;
      mbSM_AS := PMENU__AS.Checked;
    end;
    ciAOF_SM_MERI:
    begin
      PMENU__MERI.Checked := bActive;
      mbSM_MERI := PMENU__MERI.Checked;
    end;
    ciAOF_Moon:
    begin
      PMENU__MOON.Checked := bActive;
      mbShowMoon := PMENU__MOON.Checked;
    end;
    ciAOF_GalacticNebula:
    begin
      PMENU__N.Checked := bActive;
      mbShowGalacticNebula := PMENU__N.Checked;
    end;
    ciAOF_OpenClusters:
    begin
      PMENU__OC.Checked := bActive;
      mbShowOpenClusters := PMENU__OC.Checked;
    end;
    ciAOF_PlanetaryNebula:
    begin
      PMENU__PN.Checked := bActive;
      mbShowPlanetaryNebula := PMENU__PN.Checked;
    end;
    ciAOF_Sun:
    begin
      PMENU__SUN.Checked := bActive;
      mbShowSun := PMENU__SUN.Checked;
    end;
    ciAOF_MShower:
    begin
      PMENU__MSHOWER.Checked := bActive;
      mbShowMShower := PMENU__MSHOWER.Checked;
    end;
    ciAOF_Messier:
    begin
      PMENU__MESSIER.Checked := bActive;
      mbShowMessier := PMENU__MESSIER.Checked;
    end;
    ciAOF_ASTEROIDS:
    begin
      PMENU__ASTEROIDS.Checked := bActive;
      mbShowAsteroids := PMENU__ASTEROIDS.Checked;
    end;
    ciAOF_STAR_DESCR_LOW:
    begin
      PMENU__STAR_DESCR_LOW.Checked := bActive;
      if(bActive) then
        miShowStarDescr := 1
      else
        miShowStarDescr := 0;

      PMENU__STAR_DESCR_HIGH.Checked := false;
    end;
    ciAOF_STAR_DESCR_HIGH:
    begin
      PMENU__STAR_DESCR_HIGH.Checked := bActive;
      if(bActive) then
        miShowStarDescr := 2
      else
        miShowStarDescr := 0;

      PMENU__STAR_DESCR_LOW.Checked := false;
    end;
  end; // case

  P__CATINFO.Visible:= mbShowPlanetaryNebula or  mbShowOpenClusters or mbShowGalacticNebula or mbShowGlobularClusters or mbShowQuasars or mbShowGalaxies;

  CleanStartOfStarmap();
  //GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__COMETSClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_EComets, not PMENU__COMETS.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__GALACTICClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_SM_GALACTIC, not PMENU__GALACTIC.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__GALAXIESClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_Galaxies, not PMENU__GALAXIES.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__GCClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_GlobularClusters, not PMENU__GC.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__MESSIERClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_Messier, not PMENU__MESSIER.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__MOONClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_Moon, not PMENU__MOON.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__MSHOWERClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_MShower, not PMENU__MSHOWER.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__NClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_GalacticNebula, not PMENU__N.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__OCClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_OpenClusters, not PMENU__OC.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__PLANETSClick(Sender: TObject);
begin
end;

procedure TF__ASTROTOOLBOX.PMENU__PNClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_PlanetaryNebula, not PMENU__PN.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__QClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_QUASARS, not PMENU__Q.Checked);
end;

procedure TF__ASTROTOOLBOX.PMENU__SM_REFRESHClick(Sender: TObject);
begin
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__STARS_AMClick(Sender: TObject);
begin
  if(miSMDisplayMode = 0) then
  begin
    miSMDisplayMode := 1;
    mbSM_CON := true;
    PMENU__CON.Checked:=true;
    PMENU__STARS_SPEC.Checked:=false;
    PMENU__STARS_AM.Checked:=true;
  end;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__STARS_SPECClick(Sender: TObject);
begin
  if(miSMDisplayMode = 1) then miSMDisplayMode := 0;
  PMENU__STARS_SPEC.Checked:=true;
  PMENU__STARS_AM.Checked:=false;
  GenMap(P__STARMAP);
end;

procedure TF__ASTROTOOLBOX.PMENU__SUNClick(Sender: TObject);
begin
  ActivateAOFeature(ciAOF_Sun, not PMENU__SUN.Checked);
end;

procedure TF__ASTROTOOLBOX.P__DAWN_AM_PREV_PMDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DAWN_AM_ADblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DAWN_AM_CDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DAWN_AM_NDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DAWN_PM_ADblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DAWN_PM_CDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DAWN_PM_NDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DAYLIGHTDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__DEVICESMouseEnter(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_DEVICES) then
  begin
    P__DEVICES.Color := clMaroon; // $00000022;
    L__DEVICES.Font.Color:=clWhite;
  end;
end;

procedure TF__ASTROTOOLBOX.P__DEVICESMouseLeave(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_DEVICES) then
  begin
    P__DEVICES.Color := $00101010;
    L__DEVICES.Font.Color:=clSilver;
  end;
end;

procedure TF__ASTROTOOLBOX.P__GO_HOMEMouseEnter(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_ALBIREO) then
  begin
    P__GO_HOME.Color := clMaroon; // $00000022;
    L__ALBIREO.Font.Color:=clWhite;
  end;
end;

procedure TF__ASTROTOOLBOX.P__GO_HOMEMouseLeave(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_ALBIREO) then
  begin
    P__GO_HOME.Color := $00101010;
    L__ALBIREO.Font.Color:=clSilver;
  end;
end;

procedure TF__ASTROTOOLBOX.P__GO_HOURANGLEMouseEnter(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_HA) then
  begin
    P__GO_HOURANGLE.Color := clMaroon; // $00000022;
    L__HA.Font.Color:=clWhite;
  end;
end;

procedure TF__ASTROTOOLBOX.P__GO_HOURANGLEMouseLeave(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_HA) then
  begin
    P__GO_HOURANGLE.Color := $00101010;
    L__HA.Font.Color:=clSilver;
  end;
end;

procedure TF__ASTROTOOLBOX.P__MOONClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__NIGHT_AMDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__NIGHT_PMDblClick(Sender: TObject);
begin
  ShowTimeStat();
end;

procedure TF__ASTROTOOLBOX.P__OBSCOOClick(Sender: TObject);
begin
  ShowPrefs();
end;

procedure TF__ASTROTOOLBOX.P__RECOM_LEFTClick(Sender: TObject);
begin
  if(miRecomIndex > 0) then
  begin
    Dec(miRecomIndex);
    if(miRecomIndex > -1) and (mslRecommendedPics.Count > miRecomIndex) then
      ShowRecomImg();
  end;

  if(miRecomIndex <= 0) then
    P__RECOM_LEFT.Font.Color := clGray
  else
    P__RECOM_LEFT.Font.Color := clWhite;

  P__RECOM_RIGHT.Font.Color := clWhite;

end;

procedure TF__ASTROTOOLBOX.P__RECOM_RIGHTClick(Sender: TObject);
begin
  if(miRecomIndex < mslRecommendedPics.Count - 2) then
  begin
    Inc(miRecomIndex);
    if(miRecomIndex > -1) and (mslRecommendedPics.Count > miRecomIndex) then
      ShowRecomImg();
  end;

  if(miRecomIndex >= mslRecommendedPics.Count - 1) then
    P__RECOM_RIGHT.Font.Color := clGray
  else
    P__RECOM_RIGHT.Font.Color := clWhite;

  P__RECOM_LEFT.Font.Color := clWhite;
end;

procedure TF__ASTROTOOLBOX.P__SOLSYSClick(Sender: TObject);
begin
  ShowSolSys();
end;

procedure TF__ASTROTOOLBOX.P__SOLSYSPaint(Sender: TObject);
begin
  {$IFDEF Darwin}
  if(not mbOnSolSysPaintBusy) then
  begin
    mbOnSolSysPaintBusy := true;
    SolSys();
    mbOnSolSysPaintBusy := false;
  end;
  {$ENDIF Darwin}

end;

procedure TF__ASTROTOOLBOX.P__INCLPaint(Sender: TObject);
begin
  {$IFDEF Darwin}
  if(not mbOnSolSysPaintBusy) then
  begin
    mbOnSolSysPaintBusy := true;
    SolSys();
    mbOnSolSysPaintBusy := false;
  end;
  {$ENDIF Darwin}
end;

procedure TF__ASTROTOOLBOX.P__STARMAPMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  iR0, iX0, iY0: Integer;
  dJulDat, dtWT, dtST: TDateTime;
  rRA: Real;
  rAz, rAlt: Real;
  iHH_HH, iHH_MM: Word;
  iRA_HH, iRA_MM: Word;
  rHH_SS: Real;
  iDEC_DEG, iDEC_MM: SmallInt;
  rDEC_SS: Real;
  rPhi: Real;
  rRA_SS: Real;
  rDX, rDY, rR: Real;
  sDECSign: string;
  Rect: TRect;
const
  ciIdontknowwhyvalue = 20;
begin
  //if(ssLeft in Shift) then
  mbWheeled := false;

  dJulDat := 0;

  dtWT := GetWTime();
  dtST := GetSIDTime(dtWT,miDST_HH,miUTC_HH,miGLng_DEG,miGLng_MIN,dJulDat);

  sDECSign := '';
  iR0:=0; iX0:=0; iY0:=0;

  StarMapDims(P__STARMAP,iR0,iX0,iY0);

  rDX := X-iX0;
  rDY := Y-iY0;

  rR := sqrt(rDX*rDX + rDY*rDY);

  if(ssLeft in Shift) and ((rR <= iR0) or (miStarmapView > 0)) then
  begin
    if(((miX_P = 0) and (miY_P = 0)) or ((abs(miX_P - X) > 4) and (abs(miY_P - Y) > 4))) and
     ((not ((mrZoomX1 < iX0) and (miX_P > iX0) and (mrZoomY1 < iY0) and (miY_P > iY0))) or (miStarmapView > 0)) then // Not over zenith!
    begin
      if(not mbZoomMode) then
      begin
        Rect := TRect.Create(Round(mrZoomX1),Round(mrZoomY1),X,Y);
        P__STARMAP.Canvas.DrawFocusRect(Rect);
        miX_P := X; miY_P := Y;
      end;
    end;
  end;

  if(not (ssLeft in Shift)) and (not mbZoomMode) and (not mbExecZoom) then
  begin
    mrZoomX1 := X;
    mrZoomY1 := Y;
  end;

  if(mbZoomMode) then
  begin
    if(miStarmapView = 0) then
    begin
      ZoomModeTransformation_INV(iR0,rDX,rDY);
      X := Round(iX0+rDX);
      Y := Round(iY0+rDY);
    end
    else
    begin
      // Y-Problem:
      // First zoom mode miZoomY1/2 refers to small p__STARMAP-Height because P__LANDSCAME and P__HODRDUST are shown
      // Zoom afterwards miZoomY1/2 refers to P__STARMAP-Height without P__LANDSCAPE and P__HORDUST!
      X := Round(mrZoomX1 + X*(mrZoomX2 - mrZoomX1)/P__STARMAP.Width);
      (*
      Memo1.Lines.Add('---');
      Memo1.Lines.Add('Y: ' + FloatToStr(Y));
      Memo1.Lines.Add('mrZoomY1: ' + FloatToStr(mrZoomY1) + ', ' + 'mrZoomY2: ' + FloatToStr(mrZoomY2));
      Memo1.Lines.Add('ZoomLvl: ' + IntToStr(miZoomLvl));
      *)
      if(miZoomLvl = 1) then // First zoom
        Y := Round((mrZoomY1 + Y*(mrZoomY2 - mrZoomY1)/P__STARMAP.Height)*((P__STARMAP.Height + P__LANDSCAPE.Height + P__HORDUST.Height + ciIdontknowwhyvalue)/P__STARMAP.Height))
      else // successive zooms
        Y := Round((mrZoomY1 + Y*(mrZoomY2 - mrZoomY1)/P__STARMAP.Height));
      (*
      Memo1.Lines.Add('Y: ' + FloatToStr(Y));

      Memo1.Lines.Add('Height: ' + FloatToStr(crEyeFacH*Pi/2*(P__STARMAP.Height - Y)/(P__STARMAP.Height *180./Pi)));
      *)
    end;
  end;

  rAz:=0; rAlt:=0;
  if(miStarmapView = 0) then
    MouseCooToHorizon(iX0,iY0,iR0,X,Y,rAz,rAlt)
  else
    HMouseCooToHorizon(P__STARMAP.Width,P__STARMAP.Height,msHorDir,X,Y,mrEyeFacH,rAz,rAlt);

  if(rAlt >=0) and (rAz >= 0) then
  begin
   rAlt := rAlt *Pi/180.0;
   rAz := rAz *Pi/180.0;

   rPhi := (miGLat_DEG + miGLat_MIN/60.0) * Pi/180.0;

   rAz := rAz + pi; // Convert Az to SUD: 0 -> 180
   if (rAz > 2*Pi) then rAz := rAz - 2*Pi;

   iHH_HH:=0; iHH_MM:=0; rHH_SS:=0;
   iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0;

   HorizonToEquatorial(rAlt,rAz, rPhi,
      iHH_HH, iHH_MM, rHH_SS,
      iDEC_DEG, iDEC_MM, rDEC_SS);

    if(iDEC_DEG + iDEC_MM/60 + rDEC_SS/3600 < 0) then
      sDECSign := '-';

    rRA := dtST*24 - (iHH_HH + iHH_MM/60.0 + rHH_SS/3600);
    if(rRA < 0) then rRA := 24.0 + rRA;

    iRA_HH:=0; iRA_MM:=0; rRA_SS := 0;
    HoursToHH_MM_SS(rRA,iRA_HH,iRA_MM,rRA_SS);

    msRAVal := IntToStr(iRA_HH) + ';' + IntToStr(iRA_MM);
    msDECVal := IntToStr(iDEC_DEG) + ';' + IntToStr(iDEC_MM);

    SB__MAIN.Panels[4].Text := 'H: ' + Format('%.*d',[2, iHH_HH]) + ':' + Format('%.*d',[2, iHH_MM]) + ':' + Format('%.*d',[2, Round(rHH_SS)]) +
      ' / RA: ' + Format('%.*d',[2, iRA_HH]) + ':' + Format('%.*d',[2, iRA_MM]) + ':' + Format('%.*d',[2, Round(rRA_SS)]);
    SB__MAIN.Panels[5].Text := 'Alt: ' + Format('%.*d',[2, Round(rAlt*180/Pi)]) +
      ' / Az: ' + Format('%.*d',[2, Round(rAz *180/Pi)]) +
      ' / DEC: ' + sDECSign + Format('%.*d',[2, abs(iDEC_DEG)]) + ':' + Format('%.*d',[2, abs(iDEC_MM)]) + ':' + Format('%.*d',[2, Round(abs(rDEC_SS))]);

    if(ssCtrl in Shift) then
    begin
      FindCon(rRA,iDEC_DEG + iDEC_MM/60.0 + rDEC_SS/3600);
    end;
  end
  else
  begin
    Cursor := crDefault;
    SB__MAIN.Panels[4].Text := 'H: ';
    SB__MAIN.Panels[5].Text := 'DEC: ';
  end;

end;

procedure TF__ASTROTOOLBOX.P__STARMAPMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(Button = mbLeft) then
    ExecZoom(X,Y,-999,false);
end;

procedure TF__ASTROTOOLBOX.P__STARMAPMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  Do_MouseWheel(Shift,WheelDelta,MousePos,true,Handled);
end;

procedure TF__ASTROTOOLBOX.P__STARMAPPaint(Sender: TObject);
begin
  {$IFDEF Darwin}
  if(not mbOnStarmapPaintBusy) then
  begin
    mbOnStarmapPaintBusy := true;
    GenMapExec(Sender as TCustomControl);
    mbOnStarMapPaintBusy := false;
  end;
  {$ENDIF Darwin}
end;

procedure TF__ASTROTOOLBOX.P__TABLESClick(Sender: TObject);
begin
  P__TABLES.Color:=$00101010;
  ExecTable();
end;

procedure TF__ASTROTOOLBOX.CleanStartOfStarmap();
{14.11.2017/fs
Generates Starmap 2 times to prevent artefacts
}
var
  bTimerEnabled: Boolean;
begin
  bTimerEnabled := TIMER__GENMAP.Enabled;

  if(bTimerEnabled) then
    TIMER__GENMAP.Enabled:=false;

  if (miTimePlayMode = 0) then
    IniAObjects();

  GenMap(P__STARMAP);

  if(bTimerEnabled) then
    TIMER__GENMAP.Enabled:=true;

end;

procedure TF__ASTROTOOLBOX.P__TABLESMouseEnter(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_DB) then
  begin
    P__TABLES.Color := clMaroon; // $00000022;
    L__TABLE.Font.Color:=clWhite;
  end;
end;

procedure TF__ASTROTOOLBOX.P__TABLESMouseLeave(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex <> ciPAGE_DB) then
  begin
    P__TABLES.Color := $00101010;
    L__TABLE.Font.Color:=clSilver;
  end;
end;

procedure TF__ASTROTOOLBOX.P__ZC_CONTROLClick(Sender: TObject);
begin
  ControlZoomPanel();
end;

procedure TF__ASTROTOOLBOX.RB__MESSIERChange(Sender: TObject);
begin
  SetP__MAGSIZE(false);
  P__SELMAG.Visible:=false;

  IniSelTableView();

  if(RB__MESSIER.Checked) then
  begin
    SelSign();
    ShowTable_Messier();
  end;

end;

procedure TF__ASTROTOOLBOX.RB__OCChange(Sender: TObject);
begin
  SetP__MAGSIZE(false);
  P__SELMAG.Visible:=false;

  IniSelTableView();

  if(RB__OC.Checked) then
  begin
    MENU__SHOW_MESSIERONLY.Checked := false;
    MENU__SHOW_MESSIERONLY.Enabled := true;

    SelSign();
    ShowTable_OC();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__COMETSChange(Sender: TObject);
begin
  IniSelTableView();

  if(RB__COMETS.Checked) then
  begin
    SelSign();
    ShowTable_C();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__GCChange(Sender: TObject);
begin
  SetP__MAGSIZE(false);
  P__SELMAG.Visible:=false;

  IniSelTableView();

  if(RB__GC.Checked) then
  begin
    MENU__SHOW_MESSIERONLY.Checked := false;
    MENU__SHOW_MESSIERONLY.Enabled := true;
    SelSign();
    ShowTable_GC();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__GChange(Sender: TObject);
begin
  SetP__SELMAG(true);

  IniSelTableView();

  if(RB__G.Checked) then
  begin
    MENU__SHOW_MESSIERONLY.Checked := false;
    MENU__SHOW_MESSIERONLY.Enabled := true;
    SelSign();
    ShowAOTable();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__NChange(Sender: TObject);
begin
  SetP__MAGSIZE(false);
  P__SELMAG.Visible:=false;

  IniSelTableView();

  if(RB__N.Checked) then
  begin
    MENU__SHOW_MESSIERONLY.Checked := false;
    MENU__SHOW_MESSIERONLY.Enabled := true;
    SelSign();
    ShowTable_N();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__PChange(Sender: TObject);
begin
  IniSelTableView();

  if(RB__P.Checked) then
  begin
    SelSign();
    ShowTable_P();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__PNChange(Sender: TObject);
begin
  SetP__MAGSIZE(false);
  P__SELMAG.Visible:=false;

  IniSelTableView();

  if(RB__PN.Checked) then
  begin
    MENU__SHOW_MESSIERONLY.Checked := false;
    MENU__SHOW_MESSIERONLY.Enabled := true;
    SelSign();
    ShowTable_PN();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__QChange(Sender: TObject);
begin
  SetP__MAGSIZE(false);
  P__SELMAG.Visible:=false;

  IniSelTableView();

  if(RB__Q.Checked) then
  begin
    SelSign();
    ShowTable_Q();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__SChange(Sender: TObject);
begin
  SetP__MAGSIZE(false);
  P__SELMAG.Visible:=true;

  IniSelTableView();

  if(RB__S.Checked) then
  begin
    SelSign();
    ShowAOTable();
  end;
end;

procedure TF__ASTROTOOLBOX.RB__SClick(Sender: TObject);
begin
  RB__S.Font.Color:=clGreen; RB__S.Repaint;
end;

procedure TF__ASTROTOOLBOX.SHP__ZINMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(mbZoomMode) then
    ExecZoom(0,0,-1,false);
end;

procedure TF__ASTROTOOLBOX.SHP__ZOUTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(mbZoomMode) then
    ExecZoom(0,0,1,false);
end;

procedure TF__ASTROTOOLBOX.SetStarmapView(iViewMode: Integer);
begin
  miStarmapView := iViewMode;
  mrMagPos := crMagPosStd ;

  mbZoomMode := false;
  P__FASTZOOM.Visible := mbZoomMode;

  P__LANDSCAPE.Visible := (miStarmapView > 0);

  PMENU__VIEW_STARMAP.Checked:=false;
  PMENU__VIEW_N.Checked:=false;
  PMENU__VIEW_S.Checked:=false;
  PMENU__VIEW_E.Checked:=false;
  PMENU__VIEW_W.Checked:=false;

  L__BV_LEFT.Visible := false;
  BV__LEFT.Visible := false;
  L__BV_RIGHT.Visible := false;
  BV__RIGHT.Visible := false;
  L__BV_BOTTOM.Visible := false;
  BV__BOTTOM.Visible := false;

  // SetStellarObjectShapeColor...
  (*
  if(InterstellarObject.sMessier <> '') then
    InterstellarObject.SHP.Pen.Color := clYellow
  else if (LeftStr(InterstellarObject.sNGC,3) = 'NGC') then
    InterstellarObject.SHP.Pen.Color := clFuchsia
  else if (LeftStr(InterstellarObject.sNGC,2) = 'IC') then
    InterstellarObject.SHP.Pen.Color := clBlue
  else if (LeftStr(InterstellarObject.sNGC,2) = 'UGC') then
    InterstellarObject.SHP.Pen.Color := clGreen
  else if (LeftStr(InterstellarObject.sNGC,2) = 'ESO') then
    InterstellarObject.SHP.Pen.Color := clSkyBlue
  else if (LeftStr(InterstellarObject.sNGC,2) = '2MASX') then
    InterstellarObject.SHP.Pen.Color := clWhite
  else if (LeftStr(InterstellarObject.sNGC,2) = 'MCG') then
    InterstellarObject.SHP.Pen.Color := clSilver
  else
    InterstellarObject.SHP.Pen.Color := clGray;
  *)

  P__CATINFO.Visible:=false;
  case miStarmapView of
    0:
    begin
      P__HORDUST.Visible:=(miStarmapView > 0);
      PMENU__VIEW_STARMAP.Checked:=true;
      P__CATINFO.Visible:= mbShowPlanetaryNebula or  mbShowOpenClusters or mbShowGalacticNebula or mbShowGlobularClusters or mbShowQuasars or mbShowGalaxies;
    end;
    1: // North
    begin
      PMENU__VIEW_N.Checked:=true;
      msHorDir := 'N';

      L__BV_LEFT.Caption := 'West';
      L__BV_LEFT.Tag := 4;
      BV__LEFT.Tag := 4;

      if(msLANG_ID = 'DE') then
        L__BV_RIGHT.Caption := 'Ost'
      else
        L__BV_RIGHT.Caption := 'East';

      L__BV_RIGHT.Tag := 3;
      BV__RIGHT.Tag := 3;

      if(msLANG_ID = 'DE') then
        L__BV_BOTTOM.Caption := 'Süd'
      else
        L__BV_BOTTOM.Caption := 'South';

      L__BV_BOTTOM.Tag := 2;
      BV__BOTTOM.Tag := 2;
    end;
    2: // South
    begin
      PMENU__VIEW_S.Checked:=true;
      msHorDir := 'S';

      if(msLANG_ID = 'DE') then
        L__BV_LEFT.Caption := 'Ost'
      else
        L__BV_LEFT.Caption := 'East';

      L__BV_LEFT.Tag := 3;
      BV__LEFT.Tag := 3;

      L__BV_RIGHT.Caption := 'West';

      L__BV_RIGHT.Tag := 4;
      BV__RIGHT.Tag := 4;

      if(msLANG_ID = 'DE') then
        L__BV_BOTTOM.Caption := 'Nord'
      else
        L__BV_BOTTOM.Caption := 'North';

      L__BV_BOTTOM.Tag := 1;
      BV__BOTTOM.Tag := 1;
    end;
    3: // East
    begin
      PMENU__VIEW_E.Checked:=true;
      msHorDir := 'O';

      if(msLANG_ID = 'DE') then
        L__BV_LEFT.Caption := 'Nord'
      else
        L__BV_LEFT.Caption := 'North';

      L__BV_LEFT.Tag := 1;
      BV__LEFT.Tag := 1;

      if(msLANG_ID = 'DE') then
        L__BV_RIGHT.Caption := 'Süd'
      else
        L__BV_RIGHT.Caption := 'South';

      L__BV_RIGHT.Tag := 2;
      BV__RIGHT.Tag := 2;

      L__BV_BOTTOM.Caption := 'West';

      L__BV_BOTTOM.Tag := 4;
      BV__BOTTOM.Tag := 4;
    end;
    4: // West
    begin
      PMENU__VIEW_W.Checked:=true;
      msHorDir := 'W';

      if(msLANG_ID = 'DE') then
        L__BV_LEFT.Caption := 'Süd'
      else
        L__BV_LEFT.Caption := 'South';

      L__BV_LEFT.Tag := 2;
      BV__LEFT.Tag := 2;

      if(msLANG_ID = 'DE') then
        L__BV_RIGHT.Caption := 'Nord'
      else
        L__BV_RIGHT.Caption := 'North';

      L__BV_RIGHT.Tag := 1;
      BV__RIGHT.Tag := 1;

      if(msLANG_ID = 'DE') then
        L__BV_BOTTOM.Caption := 'Ost'
      else
        L__BV_BOTTOM.Caption := 'East';

      L__BV_BOTTOM.Tag := 3;
      BV__BOTTOM.Tag := 3;
    end;
  end;

  if (miTimePlayMode <> 0) then
    IniAObjects();

  CleanStartOfStarmap();

end;

procedure TF__ASTROTOOLBOX.SHP__POS_SUNMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  AOVisOnMouseUp(Sender,Button,Shift,X,Y);
end;

procedure TF__ASTROTOOLBOX.GRD__RECOM_MSDblClick(Sender: TObject);
var
  MeteorShower: TMeteorShower;
  iRow: Integer;
begin
  F__MSHOWER := TF__MSHOWER.Create(nil);

  F__MSHOWER.msLANG_ID := msLANG_ID;

  IniText(F__MSHOWER,msLANG_ID);

  iRow := GRD__RECOM_MS.Row;
  if(iRow < 0) or (iRow > GRD__RECOM_MS.RowCount-1) or (Trim(GRD__RECOM_MS.Cells[0,iRow]) = '') then
    exit;

  MeteorShower := (GRD__RECOM_MS.Objects[0,iRow] as TMeteorShower);

  if(MeteorShower = nil) then
    exit;

  if(msLANG_ID = 'DE') then
  begin
    F__MSHOWER.Caption := 'Meteorschauer';
    F__MSHOWER.P__MS_TITLE.Caption := MeteorShower.sName_DE;
  end
  else
  begin
    F__MSHOWER.Caption := 'Meteor Shower';
    F__MSHOWER.P__MS_TITLE.Caption := MeteorShower.sName_EN;
  end;

  F__MSHOWER.GRD__MSHOWER.Cells[1,0] := MeteorShower.sSign;
  F__MSHOWER.GRD__MSHOWER.Cells[1,1] := MeteorShower.sMaxDateMonth;
  F__MSHOWER.GRD__MSHOWER.Cells[1,2] := IntToStr(MeteorShower.iObjPerHour);
  F__MSHOWER.GRD__MSHOWER.Cells[1,3] := IntToStr(MeteorShower.iSpeed);
  F__MSHOWER.GRD__MSHOWER.Cells[1,4] := MeteorShower.sTime1;
  F__MSHOWER.GRD__MSHOWER.Cells[1,5] := MeteorShower.sTime2;
  F__MSHOWER.GRD__MSHOWER.Cells[1,6] := MeteorShower.sCometName;

  F__MSHOWER.ShowModal;

  F__MSHOWER.Destroy;

end;

procedure TF__ASTROTOOLBOX.SHP__ZDMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZD.Color:=clFuchsia;
end;

procedure TF__ASTROTOOLBOX.SHP__ZDMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZD.Color:=clAqua;
  MoveZoom(0,10);
end;

procedure TF__ASTROTOOLBOX.SHP__ZLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZL.Color:=clFuchsia;
end;

procedure TF__ASTROTOOLBOX.SHP__ZLMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZL.Color:=clAqua;
  MoveZoom(-10,0);
end;

procedure TF__ASTROTOOLBOX.SHP__ZRMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZR.Color:=clFuchsia;
end;

procedure TF__ASTROTOOLBOX.SHP__ZRMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZR.Color:=clAqua;
  MoveZoom(10,0);
end;

procedure TF__ASTROTOOLBOX.SHP__ZUMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZU.Color:=clFuchsia;
end;

procedure TF__ASTROTOOLBOX.SHP__ZUMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SHP__ZU.Color:=clAqua;
  MoveZoom(0,-10);
end;

procedure TF__ASTROTOOLBOX.TB__LTMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  L__LT_INTERVAL.Caption := IntToStr(TB__LT.Position) + ' Min.';
end;

procedure TF__ASTROTOOLBOX.TB__LTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if(CBX__LT.Checked) then
  begin
    TIMER__AOTABLE.Enabled:=false;
    TIMER__AOTABLE.Interval := TB__LT.Position * 60 * 1000;
    miAOTableSec := TB__LT.Position*60;
    TIMER__AOTABLE.Enabled:=true;
  end;
end;

procedure TF__ASTROTOOLBOX.TB__MAGKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TrackMagnitudes();
end;

procedure TF__ASTROTOOLBOX.TB__MAGMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  L__MAGSIZE.Caption:= AnsiReplaceStr(FloatToStrF(1.0*TB__MAG.Position/10.0,ffFixed,8,1),',','.');
  TB__MAG.Hint := L__MAGSIZE.Caption;

end;

procedure TF__ASTROTOOLBOX.TB__MAGMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TrackMagnitudes();
end;

procedure TF__ASTROTOOLBOX.TB__MAG_GKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TrackMagnitudes_G();
end;

procedure TF__ASTROTOOLBOX.TB__MAG_GMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  L__MAGSIZE_G.Caption:= AnsiReplaceStr(FloatToStrF(1.0*TB__MAG_G.Position/10.0,ffFixed,8,1),',','.');
  TB__MAG_G.Hint := L__MAGSIZE_G.Caption;
end;

procedure TF__ASTROTOOLBOX.TB__MAG_GMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  TrackMagnitudes_G();
end;

procedure TF__ASTROTOOLBOX.TB__SELMAGMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  fLow, fHigh: Real;
begin
  fLow := -2.0; fHigh := crMagPosStd;

  SetMagInterval(fLow,fHigh);

end;

procedure TF__ASTROTOOLBOX.TB__SELMAG_GMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  rLow, rHigh: Real;
begin
  rLow := -2.0; rHigh := crMagPosStd_G;

  SetMagInterval(rLow,rHigh);
end;

procedure TF__ASTROTOOLBOX.TB__SELMAG_GMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowAOTable();
end;

procedure TF__ASTROTOOLBOX.TB__SOLSYSMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowSolSys();
end;

procedure TF__ASTROTOOLBOX.TB__TIME_24HChange(Sender: TObject);
begin
  TB__TIME_24H.Hint := FormatDateTime('hh:nn:ss',1.0*TB__TIME_24H.Position / 1440);
end;

procedure TF__ASTROTOOLBOX.TB__TIME_24HMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetTimePlayOFF();
end;

procedure TF__ASTROTOOLBOX.TB__TIME_24HMouseEnter(Sender: TObject);
begin
  P__DAYLIGHT.Visible:=true;
end;

procedure TF__ASTROTOOLBOX.TB__TIME_24HMouseLeave(Sender: TObject);
begin
  P__DAYLIGHT.Visible:=false;
end;

procedure TF__ASTROTOOLBOX.TB__TIME_24HMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if(not mbTimePlay) then
  begin
    ClearSearch();
    SlideTime();
    mdtStart0 := GetWTime();
    if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
      CleanStartOfStarmap; //GenMap(P__STARMAP);

    if(PC__WORKBENCH.ActivePageIndex = ciPAGE_DB) then
    begin
      IniLiveTableMode();
      ShowAOTable();
    end;

  end;
end;

procedure TF__ASTROTOOLBOX.TIMER__GENMAPTimer(Sender: TObject);
begin
  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_STARMAP) then
  begin
    CleanStartOfStarMap();

    // Comes from fast fresh?
    if(TIMER__GENMAP.Interval = ciGraphFastRefreshTime) and (miTimePlayMode = 0) then
    begin
      SetGenMapInterval(false);
      if(mbTimePlay) then
      begin
        SwitchTimePlay(); // Stop always real-time after fast Refresh inluding previous real-time mode.

        if(msLANG_ID = 'DE') then
          L__BOTTOM_INFO.Caption:= csREALTIMESTOPPED_DE
        else
          L__BOTTOM_INFO.Caption:= csREALTIMESTOPPED_EN;

      end;
    end;
  end;

  if(PC__WORKBENCH.ActivePageIndex = ciPAGE_SOLSYS) then
  begin
    if(TIMER__GENMAP.Interval = ciGraphFastRefreshTime) and (miTimePlayMode = 0) then
      SetGenMapInterval(false); //ciGraphNormalRefreshTime;

    ShowSolSys();

    if (TIMER__GENMAP.Tag = 1) then
    begin
      TIMER__GENMAP.Tag := 0;
      TIMER__GENMAP.Enabled:=false;
    end;

  end;


end;

initialization
  {$I Albireo.lrs}

end.

