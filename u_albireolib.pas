unit U_AlbireoLib;

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
  FileUtil, SpinEx,
  ActnList, Buttons, EditBtn,
  //Spin, ExtDlgs, IniFiles, Windows,
  MaskEdit,
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, StrUtils,
  Menus,
  DateUtils, Math, ContNrs, Grids,
  Zipper,
  LCLType, // HBitmap type
  IntfGraphics, // TLazIntfImage type
  fpImage, // TFPColor type
  LCLIntf,
  LResources,
  U_AConst, U_ABase, U_ALib;

const
  scFirstTinyStarCtNo = 'HD 109857A';
  csDevFile = 'Devices.dat';

procedure GetCometAngles(dtDateTime: TDateTime; Comet: TComet;
  var rLE: Real; var rL: Real);

procedure GetPlanetAngles(dtDateTime: TDateTime; Planet: TPlanet;
  var rLE: Real; var rL1: Real);

function GetPlanetCoo(dtDateTime: TDateTime; Planet: TPlanet;
  iDST_HH,iUTC_HH: SmallInt;
  var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
  var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real; var rR: Real): Real; // Returns distance from earth

//procedure GetAOObjectPathVal(AObject: TAobject; iGLat_HH, GLat_MM, iDST_HH, iUTC_HH: SmallInt; var rRA: Real; var rDEC: Real);
procedure GetCometCoo(dtDateTime: TDateTime; Comet: TComet;
  iDST_HH,iUTC_HH: SmallInt;
  var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
  var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real;
  var rRs: Real;
  var rRho: Real);
procedure ImportCatalog_S(var olAOList: TObjectList; sAOFileName: string; var sTinyStarFirstCatNo: string; var iTinyStarBlockWidth: Integer);
procedure ImportCatalog_G(var olAOList: TObjectList; sAOFileName: string);
procedure ImportCatalog_Q(var olAOList: TObjectList; sLANG_ID: string; sAOFileName: string);
procedure ImportCatalog_GC(var olAOList: TObjectList; sLANG_ID: string; sAOFileName: string);
procedure ImportCatalog_N(var olAOList: TObjectList; sLANG_ID: string; sAOFileName: string);
procedure ImportCatalog_P(var olAOList: TObjectList; iDST_HH, iUTC_HH: SmallInt; sLANG_ID: string; sAOFileName: string);
procedure ImportCatalog_C(var olAOList: TObjectList; iDST_HH,iUTC_HH: SmallInt; sLANG_ID: string; sAOFileName: string);
procedure ImportCatalog_PN(var olAOList: TObjectList; sLANG_ID: string; rGLat: Real; sAOFileName: string);
procedure ImportCatalog_OC(var olAOList: TObjectList; sLANG_ID: string; rGLat: Real; sAOFileName: string);
procedure ImportCatalog_M(var olMoonList: TObjectList; iPlanetIndex: Integer; sLANG_ID: string; sAOFileName: string);
procedure ImportCBExt(var olCBExt: TObjectList; sAOFileName: string);
procedure ImportCatalog_MW(var olAOList: TObjectList; sLANG_ID: string; rGLat: Real; sAOFileName: string);
procedure ImportCameraList(var slCameraList: TStringList; sCamFileName: string);
procedure ImportEclipses(var slEclipses: TStringList; sEclipseDataDir: string);

function GetAOLabel(AObject: TAObject; sLANG_ID: string): string;
procedure SetAOLabel(AObject: TAObject; sLANG_ID: string);
procedure AddStaticAO(var olAOList: TObjectList; sLANG_ID: string);

function LoadImgRes(IMG: TImage; sImage: string; sType: string): Boolean;
function DisplayMoonPhase(dtWTime: TDatetime; var IMG: TImage): Real;

function GetSingleSpecType(sSpecType: string; iMStarIndex: Integer): string;
function GetSpecIDData(sSingleSpecType: string; var iTMin: Integer; var iTMax: Integer; var iSTemp: Integer): string;

function GetColorFromSpecType(sSpecType: string): TColor;
function GetColorFromSpecTypeExt(sSpecType: string; iMStarIndex: Integer;
  var iSTemp: Integer; var sMKKTypeDE: string; var sMKKTypeEN: string; var sSpecID: string): TColor;

function GetSpecDX(sSpecType: string; iIndex: Integer; iXMin, iXMax: Integer): Integer;
function GetHR_XProz_FromSpecType(sSpecType: string): Integer;
function GetHR_YProz_FromSpecType(sSpecType: string): Integer;
function GetMKKFromSpecType(sSpecType: string; iMStarIndex: Integer; var sMKKTypeDE: string; var sMKKTypeEN: string): Boolean;
function GetLClassCore(sSpecType, sSym: string): string;
function GetLClass(sSpecType: string): string;
function GetLClass_MS(sSpecType: string): string;
function GetSClass(sSpecType: string): string;
function GetSClass_MS(sSpecType: string): string;

procedure SortStringGrid(GRD: TStringGrid; iColIndex, iColType: Integer; bHasTitleRow, bAsc: Boolean);
//function IsAdvancedFeature(sLANG_ID: string; bIsRegistered: Boolean): Boolean;
procedure PrepStarImage(var IMG: TImage);
procedure UnZipFile(sZipFileName, sUnzippedFolderName: string);
function PrepareImportFile(sFullAOFileName: string): Boolean;
procedure AOIndexControl(AObject: TAObject; iLocalIndex, iAOListIndex: Integer);
function GetAOIC_MaxMessier(): Integer;
function GetAOIC_MinMessier(): Integer;
function GetAOIC_Max_G(): Integer;
function GetAOIC_Min_G(): Integer;
function FindComponentExt(Form: TForm; sSearchName: string): TComponent;
procedure FadeInRed(IMG: TImage; ABitMap: Graphics.TBitmap; iFadeLevel: Integer);
procedure FadeIn(ABitMap: Graphics.TBitmap; IMG__HOR: TImage; iDarknessLevel: Integer);
procedure FadeIn2(aBitMap: Graphics.TBitmap; IMG__HOR: TImage);
procedure LoadDevices(olADevList: TObjectList);
procedure SaveDevices(olADevList: TObjectList);
function GenSolFrac(sSpType: string): Real;

type
  TRGBTripleArray = array[0..32767] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

implementation

procedure LoadDevices(olADevList: TObjectList);
{2013/04/25 / fs
Load Device data from file
}
var
  i: Integer;
  slBuffer: TStringList;
  ADevice: TADevice;
  tfDevices: TextFile;
  sLine,sVar: string;
  sFile: string;
begin
  BeginMethod('LoadDevices');

  //sFile := ConvertWinPath(GetUserDir() + csDevFile);
  sFile := ConvertWinPath(gsAlbireoLocalDir + csDevFile);

  if(not FileExists(sFile)) then
    exit;

  slBuffer := TStringList.Create;
  slBuffer.Delimiter:=';';

  // Import astronomical device list
  AssignFile(tfDevices,sFile);
  Reset(tfDevices);

  while not eof(tfDevices) do
  begin
    ADevice := nil;
    slBuffer.Clear;
    ReadLn(tfDevices,sLine);
    sLine := AnsiReplaceStr(sLine,' ','~');
    SetMsg(sLine);
    (*
    if(AnsiContainsStr(sLine,'TAObject')) then
      slBuffer.DelimitedText:=''
    else
      slBuffer.DelimitedText:=sLine;
    *)

    slBuffer.DelimitedText:=sLine;

    try
    for i:=0 to slBuffer.Count-1 do
    begin
      if(i = 0) then
      begin
        // Generation and Initialisation
        ADevice := TADevice.Create;

        ADevice.iFocalWidthDev_mm:=-999;
        ADevice.iDiameter_mm:=-999;
        //ADevice.iFocalWidthOcu_mm:=-999;
      end;

      sVar := Trim(slBuffer[i]);
      case i of
       0: ADevice.sName := AnsiReplaceStr(slBuffer[i],'~',' ');
       1: ADevice.sDescription := AnsiReplaceStr(slBuffer[i],'~',' ');
       2: ADevice.sManufacturer := AnsiReplaceStr(slBuffer[i],'~',' ');
       3: ADevice.sArtNoManu := AnsiReplaceStr(slBuffer[i],'~',' ');
       4: if(StrIsNum(true,sVar)) then ADevice.dtBDate := StrToFloat(sVar);
       5: if(StrIsNum(true,sVar)) then ADevice.rPrice := StrToFloat(sVar);
       6: ADevice.sCurrency := AnsiReplaceStr(slBuffer[i],'~',' ');
       7: if(StrIsNum(false,sVar)) then ADevice.iMountType := StrToInt(sVar);
       8: ADevice.sImagePath := AnsiReplaceStr(slBuffer[i],'~',' ');
       9: if(StrIsNum(false,sVar)) then ADevice.iFocalWidthDev_mm := StrToInt(sVar);
      10: if(StrIsNum(false,sVar)) then ADevice.iDiameter_mm := StrToInt(sVar);
      11: ADevice.sType := AnsiReplaceStr(slBuffer[i],'~',' ');
      12: ADevice.sCManu := AnsiReplaceStr(slBuffer[i],'~',' ');
      13: ADevice.sCModel := AnsiReplaceStr(slBuffer[i],'~',' ');
      14: if(sVar <> '') then
            ADevice.iDefault:=StrToInt(sVar)
          else
            ADevice.iDefault := 0;

      end; // case
    end;
    if(ADevice <> nil) then
    begin
      olADevList.Add(ADevice);
    end;
    except
      on e: Exception do
      begin
        MessageDlg('Error in Line: ' + sLine,mtError,[mbOK],0);
      end;
    end;
  end;

  CloseFile(tfDevices);

  SetMsg('');
  EndMethod('LoadDevices');
end;

procedure SaveDevices(olADevList: TObjectList);
{2013/04/25 / fs
Store Device data into file
}
var
  i: Integer;
  //slBuffer: TStringList;
  //ADevice: TADevice;
  tfDevices: TextFile;
  sLine: string;
begin
  BeginMethod('SaveDevices');

  if(olADevList.Count = 0) then
    exit;

  // Import astronomical device list
  //AssignFile(tfDevices,ConvertWinPath(GetUserDir + csDevFile));
  AssignFile(tfDevices,ConvertWinPath(gsAlbireoLocalDir + csDevFile));

  ReWrite(tfDevices);

  for i:=0 to olADevList.Count-1 do
  begin
    sLine := (olADevList[i] as TADevice).sName;
    sLine := sLine + ';' + AnsiReplaceStr((olADevList[i] as TADevice).sDescription,' ','~');
    sLine := sLine + ';' + AnsiReplaceStr((olADevList[i] as TADevice).sManufacturer,' ','~');
    sLine := sLine + ';' + AnsiReplaceStr((olADevList[i] as TADevice).sArtNoManu,' ','~');
    sLine := sLine + ';' + FloatToStr((olADevList[i] as TADevice).dtBDate);
    sLine := sLine + ';' + FloatToStr((olADevList[i] as TADevice).rPrice);
    sLine := sLine + ';' + (olADevList[i] as TADevice).sCurrency;
    sLine := sLine + ';' + IntToStr((olADevList[i] as TADevice).iMountType);
    sLine := sLine + ';' + AnsiReplaceStr((olADevList[i] as TADevice).sImagePath,' ','~');

    sLine := sLine + ';' + IntToStr((olADevList[i] as TADevice).iFocalWidthDev_mm);
    sLine := sLine + ';' + IntToStr((olADevList[i] as TADevice).iDiameter_mm);
    sLine := sLine + ';' + AnsiReplaceStr((olADevList[i] as TADevice).sType,' ','~');
    sLine := sLine + ';' + AnsiReplaceStr((olADevList[i] as TADevice).sCManu,' ','~');
    sLine := sLine + ';' + AnsiReplaceStr((olADevList[i] as TADevice).sCModel,' ','~');
    sLine := sLine + ';' + IntToStr((olADevList[i] as TADevice).iDefault);

    WriteLn(tfDevices,sLine);
  end;

  CloseFile(tfDevices);

  SetMsg('');
  EndMethod('SaveDevices');
end;

procedure FadeInRed(IMG: TImage; ABitMap: Graphics.TBitmap; iFadeLevel: Integer);
 var
   SrcIntfImg, TempIntfImg: TLazIntfImage;
   ImgHandle,ImgMaskHandle: HBitmap;
   px, py: Integer;
   CurColor: TFPColor;
   pngbmp: TPortableNetworkGraphic;
   MemoryStream: TMemoryStream;
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
 end;

procedure FadeIn(ABitMap: Graphics.TBitmap; IMG__HOR: TImage; iDarknessLevel: Integer);
 var
   SrcIntfImg, TempIntfImg: TLazIntfImage;
   ImgHandle,ImgMaskHandle: HBitmap;
   //FadeStep: Integer;
   px, py: Integer;
   CurColor: TFPColor;
   //TempBitmap: TBitmap;
   pngbmp: TPortableNetworkGraphic;
   MemoryStream: TMemoryStream;
 begin
   pngbmp := TPortableNetworkGraphic.Create;
   MemoryStream := TMemoryStream.Create;

   SrcIntfImg:=TLazIntfImage.Create(0,0);
   SrcIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
   TempIntfImg:=TLazIntfImage.Create(0,0);
   TempIntfImg.LoadFromBitmap(ABitmap.Handle,ABitmap.MaskHandle);
   //TempBitmap:=TBitmap.Create;
   //for FadeStep:=16 to 16 do begin
     for py:=0 to SrcIntfImg.Height-1 do begin
       for px:=0 to SrcIntfImg.Width-1 do begin
         CurColor:=SrcIntfImg.Colors[px,py];
         CurColor.Red:=(CurColor.Red*iDarknessLevel) shr 5;
         CurColor.Green:=(CurColor.Green*iDarknessLevel) shr 5;
         CurColor.Blue:=(CurColor.Blue*iDarknessLevel) shr 5;
         TempIntfImg.Colors[px,py]:=CurColor;
       end;
     end;
     TempIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,false);
     pngbmp.BitmapHandle:=ImgHandle;
     pngbmp.MaskHandle:=ImgMaskHandle;
     pngbmp.SaveToStream(MemoryStream);
     MemoryStream.Position:=0;
     //pngbmp.SaveToFile('Temp.png');
     //IMG__HOR.Picture.LoadFromFile('Temp.png');
     IMG__HOR.Picture.LoadFromStream(MemoryStream);

     //IMG__HOR.Canvas.Draw(0,0,TempBitmap);
   //end;
   SrcIntfImg.Free;
   TempIntfImg.Free;
   pngbmp.Free;
   MemoryStream.Free;
 end;

procedure FadeIn2(aBitMap: Graphics.TBitmap; IMG__HOR: TImage);
 var
   IntfImg1, IntfImg2: TLazIntfImage;
   ImgHandle,ImgMaskHandle: HBitmap;
   FadeStep: Integer;
   px, py: Integer;
   TempBitmap: Graphics.TBitmap;
   Row1, Row2: PRGBTripleArray;
 begin

   IntfImg1:=TLazIntfImage.Create(0,0);
   IntfImg1.LoadFromBitmap(aBitmap.Handle,aBitmap.MaskHandle);

   IntfImg2:=TLazIntfImage.Create(0,0);
   IntfImg2.LoadFromBitmap(aBitmap.Handle,aBitmap.MaskHandle);

   TempBitmap:=Graphics.TBitmap.Create;

   //with Scanline-like
   ShowMessage(IntToStr(IntfImg1.Width));
   for FadeStep:=16 downto 16 do begin
   //for FadeStep:=32 downto 1 do begin
     for py:=0 to IntfImg1.Height-1 do begin
       Row1 := IntfImg1.GetDataLineStart(py); //like Delphi TBitMap.ScanLine
       Row2 := IntfImg2.GetDataLineStart(py); //like Delphi TBitMap.ScanLine
       for px:=0 to IntfImg1.Width-1 do begin
         Row2^[px].rgbtRed:= (FadeStep * Row1^[px].rgbtRed) shr 5;
         Row2^[px].rgbtGreen := (FadeStep * Row1^[px].rgbtGreen) shr 5; // Fading
         Row2^[px].rgbtBlue := (FadeStep * Row1^[px].rgbtBlue) shr 5;
       end;
     end;
     IntfImg2.CreateBitmaps(ImgHandle,ImgMaskHandle,false);

     TempBitmap.Handle:=ImgHandle;
     TempBitmap.MaskHandle:=ImgMaskHandle;

     IMG__HOR.Canvas.Draw(0,0,TempBitmap);
   end;

   IntfImg1.Free;
   IntfImg2.Free;
   TempBitmap.Free;
 end;

procedure ImportEclipses(var slEclipses: TStringList; sEclipseDataDir: string);
var
  sSunFile, sMoonFile: string;
  i: Integer;
  slBuffer: TStringList;
  SunEclipse: TSunEclipse;
  MoonEclipse: TMoonEclipse;
  bIsHeader: Boolean;
  iYear, iMonth, iDay, iHH, iMM: Word;
  iHH1, iMM1, iSS1, iHH2, iMM2, iHH3, iMM3: Word;
  tfAO: TextFile;
  sLine, sVar: string;
begin
  bIsHeader := true;
  iDay := 0; iMonth:=0; iYear:=0;
  iHH:=0; iMM:=0;

  slBuffer := TStringList.Create;

  // 1. Import sun eclipses
  slBuffer.Delimiter:=';';
  sSunFile := sEclipseDataDir + 'AO-SEcl.dat';
  // Import astronomical objects catalogue
  //AssignFile(tfAO,ConvertWinPath('AO-C.dat'));
  AssignFile(tfAO,ConvertWinPath(sSunFile));
  Reset(tfAO);

  while not eof(tfAO) do
  begin
    SunEclipse := nil;

    slBuffer.Clear;
    ReadLn(tfAO,sLine);
    if(bIsHeader) then
    begin
      ReadLn(tfAO,sLine);
      bIsHeader := false;
    end;

    sLine := AnsiReplaceStr(sLine,' ','~');

    iHH1 := 999;
    iMM1 := 999;
    iSS1 := 999;

    slBuffer.DelimitedText:=sLine;
    for i:=0 to slBuffer.Count-1 do
    begin
      if(i = 0) then
      begin
        SunEclipse := TSunEclipse.Create;

        SunEclipse.dtDateTime:=0;
        SunEclipse.dtDurationT:=0;
        SunEclipse.rSize:=0;
        SunEclipse.sType:='';
        SunEclipse.sLoc:='';
        SunEclipse.sEclType:='S';
      end;

      sVar := slBuffer[i];
      case i of
       0: if(StrIsNum(false,sVar)) then iYear:= StrToInt(sVar);
       1: if(StrIsNum(false,sVar)) then iMonth:= StrToInt(sVar);
       2: if(StrIsNum(false,sVar)) then iDay:= StrToInt(sVar);
       3: if(StrIsNum(false,sVar)) then iHH:= StrToInt(sVar);
       4: if(StrIsNum(false,sVar)) then iMM:= StrToInt(sVar);
       5: if(StrIsNum(false,sVar)) then iMM1 := StrToInt(sVar);
       6: if(StrIsNum(false,sVar)) then iSS1 := StrToInt(sVar);
       7: SunEclipse.sType := slBuffer[i];
       8: if(StrIsNum(true,sVar)) then SunEclipse.rSize := StrToFloatExt4(slBuffer[i]);
       9: SunEclipse.sLoc := slBuffer[i];
      end; // case
    end;

    if(SunEclipse <> nil) then
    begin
      SunEclipse.dtDateTime := EncodeDate(iYear,iMonth,iDay) + EncodeTime(iHH,iMM,0,0);
      if((iMM1 < 999) and (iSS1 < 999)) then
        SunEclipse.dtDurationT:=EncodeTime((iMM1 div 60),iMM1 - iMM1*(iMM1 div 60),iSS1,0);

      slEclipses.AddObject(DateToStr(SunEclipse.dtDateTime),SunEclipse);
    end;

  end;

  CloseFile(tfAO);

  // 2. Import moon eclipses
  bIsHeader := true;
  sMoonFile := sEclipseDataDir + 'AO-MEcl.dat';
  AssignFile(tfAO,ConvertWinPath(sMoonFile));
  Reset(tfAO);

  while not eof(tfAO) do
  begin
    MoonEclipse := nil;

    slBuffer.Clear;
    ReadLn(tfAO,sLine);
    if(bIsHeader) then
    begin
      ReadLn(tfAO,sLine);
      bIsHeader := false;
    end;

    sLine := AnsiReplaceStr(sLine,' ','~');

    iHH1 := 999;
    iMM1 := 999;
    iHH2 := 999;
    iMM2 := 999;
    iHH3 := 999;
    iMM3 := 999;

    slBuffer.DelimitedText:=sLine;
    for i:=0 to slBuffer.Count-1 do
    begin
      if(i = 0) then
      begin
        MoonEclipse := TMoonEclipse.Create;

        MoonEclipse.dtDateTime:=0;
        MoonEclipse.sType:='';
        MoonEclipse.dtDurationT:=0;
        MoonEclipse.dtDurationPE:=0;
        MoonEclipse.dtDurationPA:=0;
        MoonEclipse.sLoc:='';
        MoonEclipse.sEclType:='M';
      end;

      sVar := slBuffer[i];
      case i of
       0: if(StrIsNum(false,sVar)) then iYear:= StrToInt(sVar);
       1: if(StrIsNum(false,sVar)) then iMonth:= StrToInt(sVar);
       2: if(StrIsNum(false,sVar)) then iDay:= StrToInt(sVar);
       3: if(StrIsNum(false,sVar)) then iHH:= StrToInt(sVar);
       4: if(StrIsNum(false,sVar)) then iMM:= StrToInt(sVar);
       5: MoonEclipse.sType := slBuffer[i];
       6: if(StrIsNum(false,sVar)) then
         iHH1 := StrToInt(sVar);
       7: if(StrIsNum(false,sVar)) then iMM1 := StrToInt(sVar);
       8: if(StrIsNum(false,sVar)) then iHH2 := StrToInt(sVar);
       9: if(StrIsNum(false,sVar)) then iMM2 := StrToInt(sVar);
       10: if(StrIsNum(false,sVar)) then iHH3 := StrToInt(sVar);
       11: if(StrIsNum(false,sVar)) then iMM3 := StrToInt(sVar);
       12: MoonEclipse.sLoc := slBuffer[i];
      end; // case
    end;

    if(MoonEclipse <> nil) then
    begin
      MoonEclipse.dtDateTime := EncodeDate(iYear,iMonth,iDay) + EncodeTime(iHH,iMM,0,0);
      if((iHH1 < 999) and (iMM1 < 999)) then
        MoonEclipse.dtDurationPE:=EncodeTime(iHH1,iMM1,0,0);
      if((iHH2 < 999) and (iMM2 < 999)) then
        MoonEclipse.dtDurationPA:=EncodeTime(iHH2,iMM2,0,0);
      if((iHH3 < 999) and (iMM3 < 999)) then
        MoonEclipse.dtDurationT:=EncodeTime(iHH3,iMM3,0,0);

      slEclipses.AddObject(DateToStr(MoonEclipse.dtDateTime),MoonEclipse);
    end;

  end;

  CloseFile(tfAO);

end;

function FindComponentExt(Form: TForm; sSearchName: string): TComponent;
var
  i: Integer;
  sCompName: string;
begin
  Result := nil;

  sSearchName := Trim(Uppercase(sSearchName));

  for i:=0 to Form.ComponentCount-1 do
  begin
    sCompName := Trim(Uppercase(Trim(Form.Components[i].Name)));
    if(sCompName = sSearchName) then
    begin
      Result := Form.Components[i];
      break;
    end;
  end;
end;

function GetAOIC_Min_G(): Integer;
begin
  Result := grecAOIndexControl.iMin_G;
end;

function GetAOIC_Max_G(): Integer;
begin
  Result := grecAOIndexControl.iMax_G;
end;

function GetAOIC_MinMessier(): Integer;
begin
  Result := grecAOIndexControl.iMinMessier;
end;

function GetAOIC_MaxMessier(): Integer;
begin
  Result := grecAOIndexControl.iMaxMessier;
end;

procedure AOIndexControl(AObject: TAObject; iLocalIndex, iAOListIndex: Integer);
begin
  if(AObject.sAOType = 'S') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_S:=iAOListIndex;

    grecAOIndexControl.iMax_S:=iAOListIndex;
  end
  else if(AObject.sAOType = 'Q') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_Q:=iAOListIndex;

    grecAOIndexControl.iMax_Q:=iAOListIndex;
  end
  else if(AObject.sAOType = 'G') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_G:=iAOListIndex;

    grecAOIndexControl.iMax_G:=iAOListIndex;
    if((AObject as TInterstellarObject).sMessier <> '') then
    begin
      if(grecAOIndexControl.iMaxMessier = 0) then
        grecAOIndexControl.iMinMessier:=iAOListIndex;

      if(grecAOIndexControl.iMaxMessier < iAOListIndex) then
        grecAOIndexControl.iMaxMessier:=iAOListIndex;
    end;
  end
  else if(AObject.sAOType = 'GC') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_GC:=iAOListIndex;

    grecAOIndexControl.iMax_GC:=iAOListIndex;
    if((AObject as TInterstellarObject).sMessier <> '') then
    begin
      if(grecAOIndexControl.iMaxMessier = 0) then
        grecAOIndexControl.iMinMessier:=iAOListIndex;

      if(grecAOIndexControl.iMaxMessier < iAOListIndex) then
        grecAOIndexControl.iMaxMessier:=iAOListIndex;
    end;
  end
  else if(AObject.sAOType = 'OC') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_OC:=iAOListIndex;

    grecAOIndexControl.iMax_OC:=iAOListIndex;
    if((AObject as TInterstellarObject).sMessier <> '') then
    begin
      if(grecAOIndexControl.iMaxMessier = 0) then
        grecAOIndexControl.iMinMessier:=iAOListIndex;

      if(grecAOIndexControl.iMaxMessier < iAOListIndex) then
        grecAOIndexControl.iMaxMessier:=iAOListIndex;
    end;
  end
  else if(AObject.sAOType = 'N') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_N:=iAOListIndex;

    grecAOIndexControl.iMax_N:=iAOListIndex;
    if((AObject as TInterstellarObject).sMessier <> '') then
    begin
      if(grecAOIndexControl.iMaxMessier = 0) then
        grecAOIndexControl.iMinMessier:=iAOListIndex;

      if(grecAOIndexControl.iMaxMessier < iAOListIndex) then
        grecAOIndexControl.iMaxMessier:=iAOListIndex;
    end;
  end
  else if(AObject.sAOType = 'PN') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_PN:=iAOListIndex;

    grecAOIndexControl.iMax_PN:=iAOListIndex;
    if((AObject as TInterstellarObject).sMessier <> '') then
    begin
      if(grecAOIndexControl.iMaxMessier = 0) then
        grecAOIndexControl.iMinMessier:=iAOListIndex;

      if(grecAOIndexControl.iMaxMessier < iAOListIndex) then
        grecAOIndexControl.iMaxMessier:=iAOListIndex;
    end;
  end
  else if(AObject.sAOType = 'P') then
  begin

    if((AObject as TPlanet).sPlanetType <> 'A') then
    begin
      if(iLocalIndex = 1) then
        grecAOIndexControl.iMin_P:=iAOListIndex;

      grecAOIndexControl.iMax_P:=iAOListIndex;
    end
    else
    begin
      if(iLocalIndex = 1) then
        grecAOIndexControl.iMin_PA:=iAOListIndex;

      grecAOIndexControl.iMax_PA:=iAOListIndex;
    end;
  end
  else if(AObject.sAOType = 'C') then
  begin
    if(iLocalIndex = 1) then
      grecAOIndexControl.iMin_C:=iAOListIndex;

    grecAOIndexControl.iMax_C:=iAOListIndex;
  end;
end;

function PrepareImportFile(sFullAOFileName: string): Boolean;
var
  sDirWithSlash, sFileName, sZipFileName: string;
begin
  Result := false;

  if(not FileExists(sFullAOFileName)) then
  begin
    sFileName := ExtractFileName(sFullAOFileName);
    sDirWithSlash := ExtractFilePath(sFullAOFileName);
    sZipFileName := AnsiReplaceStr(sFileName,'.dat','.zip');

    if(FileExists(sDirWithSlash + sZipFileName)) then
    begin
      UnZipFile(sDirWithSlash + sZipFileName,sDirWithSlash);
      Application.ProcessMessages;
      if(not FileExists(sFullAOFileName)) then
      begin
        MessageDlg('File error','Unable to extract data from file ' + sFileName,mtError,[mbOK],0);
        exit;
      end
      else
        Result := true;

    end
    else
    begin
      MessageDlg('File error','Unable to find compressed data file ' + sZipFileName,mtError,[mbOK],0);
      exit;
    end;
  end
  else
    Result := true;

end;

procedure UnZipFile(sZipFileName, sUnzippedFolderName: string);
var
  UnZipper: TUnZipper;
begin
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := sZipFileName;
    UnZipper.OutputPath := sUnzippedFolderName;
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;
  finally
    UnZipper.Free;
  end;
end;

procedure PrepStarImage(var IMG: TImage);
begin
  IMG.Transparent:=true;
  IMG.Proportional:=true;
  IMG.Stretch:=false;
  IMG.StretchInEnabled:=true;
  IMG.StretchOutEnabled:=true;
  IMG.Center:=true;
  IMG.Cursor:=crCross;
  IMG.Height:=0;
  IMG.Width:=0;
end;

(*
function IsAdvancedFeature(sLANG_ID: string; bIsRegistered: Boolean): Boolean;
begin
  Result := false;

  if((gcsCommVersion = '') or (not bIsRegistered)) then
  begin
    if(sLANG_ID <> '') then
    begin
      if(sLANG_ID = 'DE') then
        MessageDlg('Lizenzinformation','Dieses Feature ist für die FREE-Version oder nicht lizensierte Versionen nicht verfügbar.',mtInformation,[mbOK],0)
      else
        MessageDlg('License Information','This feature is not available for FREE version and non-licensed versions.',mtInformation,[mbOK],0);

    end;

    exit;
  end
  else
    Result := (Trunc(Pi) = Round(sqrt(12-3)));

end;
*)

procedure SortStringGrid(GRD: TStringGrid; iColIndex, iColType: Integer; bHasTitleRow, bAsc: Boolean);
var
  slBuffer, slLineBuffer: TStringList;
  sLine, sItem, sColContent: string;
  i,j: Integer;
  iStartRow: Integer;
  //bIsNumeric: Boolean;
  iDSPos,k, iZPos: Integer;
  //sMinus: string;
  sSubText: string;
begin
  if(GRD = nil) then
    exit;

  if(iColIndex < 0) or (iColIndex > GRD.ColCount-1) then
    exit;

  //bIsNumeric := (iColType = 1);

  slBuffer := TStringList.Create;
  slBuffer.Delimiter:=';';
  slBuffer.Sorted := true;

  if(bHasTitleRow) then
    iStartRow := 1
  else
    iStartRow := 0;

  for j:=iStartRow to GRD.RowCount-1 do
  begin
    sLine := '';
    sColContent := '';
    //sMinus := '';
    for i:=0 to GRD.ColCount-1 do
    begin
      sItem := AnsiReplaceStr(GRD.Cells[i,j],'|','##');
      sItem := AnsiReplaceStr(sItem,';','~~');
      //sItem := AnsiReplaceStr(GRD.Cells[i,j],' ','%%');
      sItem := AnsiReplaceStr(sItem,' ','%%');

      if(i = iColIndex) then
      begin
        sColContent := Trim(sItem);

        //if(bIsNumeric) then
        case iColType of
          1: // Numeric
          begin
            // Mark negative sign by adding a positiv offset so that the result remains always positive
            // 100 should be enough because strongest negatives should be only magnitudes....
            // this will order in a correct way
            if(sColContent <> '') and (sColContent <> '-') then
              //sColContent := FloatToStr(StrToFloat(sColContent) + 100.0); // cases ',0' sort error!
              sColContent := FloatToStrF(StrToFloat(sColContent) + 100.0,ffFixed,8,2);

            // Fill control columns with leading zeros for numeric sort
            iDSPos := Pos(',',sColContent);
            if(iDSPos <= 0) then
              iDSPos := Pos('.',sColContent);

            if(iDSPos > 0) then
              iZPos := 6-iDSPos
            else
              iZPos := 5-Length(sColContent);

            for k:=0 to iZPos do
            begin
              sColContent := '0' + sColContent;
            end;

          end;
          2: // Degree DEG:MIN:SEC
          begin
            iDSPos := Pos(':',sColContent);
            if(iDSPos > 1) then
            begin
              sSubText := LeftStr(sColContent,iDSPos-1);
              if(sSubText <> '') and (sSubText <> '-') then
              begin
                sColContent := RightStr(sColContent,length(sColContent)-iDSPos);
                sSubText := FloatToStr(StrToFloat(sSubText) + 100.0);
                sColContent := sSubText + '::' + sColContent;
              end;

              iZPos := 5-Length(sSubText);
              for k:=0 to iZPos do
              begin
                sColContent := '0' + sColContent;
              end;

            end;
          end;
        end; //case

      end;

      sLine := sLine + sItem + '|';
    end;

    // Start with control column to be sorted
    //sColContent := AnsiReplaceStr(sColContent,',0',',NULL#');
    slBuffer.Add(sColContent + '|' + sLine);
  end;

  slLineBuffer := TStringList.Create;
  slLineBuffer.Delimiter:='|';

  // Refill Stringgrid
  GRD.RowCount:=iStartRow;

  if(bAsc) then
  begin
    // Ascending TStringGrid refill
    for j:=0 to slBuffer.Count-1 do
    begin
      slLineBuffer.DelimitedText:=slBuffer[j];
      GRD.RowCount := GRD.RowCount + 1;
      for i:=1 to slLineBuffer.Count-1 do // Omit 1st control column
      begin
        sItem := AnsiReplaceStr(slLineBuffer[i],'%%',' ');
        sItem := AnsiReplaceStr(sItem,'~~',';');
        sItem := AnsiReplaceStr(sItem,'##','|');
        if(i-1 < GRD.ColCount) and (j+iStartRow < GRD.RowCount) then
          GRD.Cells[i-1,j+iStartRow] := sItem;
      end;
    end;
  end
  else
  begin
    // Descending TStringGrid refill
    for j:=slBuffer.Count-1 downto 0 do
    begin
      slLineBuffer.DelimitedText:=slBuffer[j];
      GRD.RowCount := GRD.RowCount + 1;
      for i:=1 to slLineBuffer.Count-1 do // Omit 1st control column
      begin
        sItem := AnsiReplaceStr(slLineBuffer[i],'%%',' ');
        sItem := AnsiReplaceStr(sItem,'~~',';');
        sItem := AnsiReplaceStr(sItem,'##','|');
        if(i-1 < GRD.ColCount) and (((slBuffer.Count-1)-j)+iStartRow < GRD.RowCount) then
          GRD.Cells[i-1,((slBuffer.Count-1)-j)+iStartRow] := sItem;
      end;
    end;
  end;

  slLineBuffer.Destroy;
  slBuffer.Destroy;
end;

function GetLClassCore(sSpecType, sSym: string): string;
var
  bType0Test: Boolean;
begin
  Result := '';

  //sSpecType := Uppercase(Trim(sSpecType));
  sSpecType := Trim(sSpecType);

  if(Length(sSpecType) = 0) then
    exit;

  if (AnsiContainsStr(sSpecType,'MARK-')) then
    exit;

  if(sSym = '') then
    bType0Test := (length(sSpecType) > 5) and (AnsiContainsStr(RightStr(sSpecType,3),'0'))
      and (not AnsiContainsStr(sSpecType,'V'))
      and (not AnsiContainsStr(sSpecType,'I'))
  else
    bType0Test := (AnsiContainsStr(sSpecType,sSym + '0')); // Search fpr +0 in spectral types indicating multiple stars

  if(bType0Test) then
  begin
    Result := '0'; // Hyperriese 0
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'III')) then
  begin
    Result := 'III'; // Riesen III
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'VII') or (LeftStr(sSpecType,1) = sSym + 'D')) then
  begin
    Result := 'VII'; // Weiße Zwerge
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'II')) then
  begin
    Result := 'II'; // Helle Riesen II
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'Ia')) then
  begin
    Result := 'Ia'; // Überriesen 1a
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'Ib')) then
  begin
    Result := 'Ib'; // Überriesen 1b
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'IV')) then
  begin
    Result := 'IV'; // Unterriesen
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'VI')  or (LeftStr(sSpecType,2) = sSym + 'SD')) then
  begin
    Result := 'VI'; // Sub-Zwerge
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'V')) then  // Falls, wie bei Mizar ein 'i' am Ende und vorher ein 'V'
  begin
    Result := 'V'; // Hauptreihe
  end
  else if (AnsiContainsStr(sSpecType,sSym + 'I')) then
  begin
    Result := 'I'; // Überriesen
  end;


end;

function GetLClass(sSpecType: string): string;
begin
  Result := GetLClassCore(sSpecType,'');
end;

function GetLClass_MS(sSpecType: string): string;
begin
  Result := GetLClassCore(sSpecType,'+');
end;

function GetSClass(sSpecType: string): string;
begin
  Result := '';

  if(AnsiContainsStr(sSpecType,'MARK')) then
    exit;

  sSpecType := Trim(sSpecType);

  if(LeftStr(sSpecType,1) = 'O') then
    Result := 'O';
  if(LeftStr(sSpecType,1) = 'B') then
    Result := 'B';
  if(LeftStr(sSpecType,1) = 'A') then
    Result := 'A';
  if(LeftStr(sSpecType,1) = 'F') then
    Result := 'F';
  if(LeftStr(sSpecType,1) = 'G') then
    Result := 'G';
  if(LeftStr(sSpecType,1) = 'K') then
    Result := 'K';
  if(LeftStr(sSpecType,1) = 'M') then
    Result := 'M';

  // Carbon Classes of Red Giants
  if(LeftStr(sSpecType,1) = 'R') then
    Result := 'R';
  if(LeftStr(sSpecType,1) = 'N') then
    Result := 'N';
  if(LeftStr(sSpecType,1) = 'S') then
    Result := 'S';
  if(LeftStr(sSpecType,1) = 'C') then
    Result := 'C';

  // Brown Dwarfs
  if(LeftStr(sSpecType,1) = 'L') then
    Result := 'L';
  if(LeftStr(sSpecType,1) = 'T') then
    Result := 'T';
  if(LeftStr(sSpecType,1) = 'Y') then
    Result := 'Y';

end;

function GetSClass_MS(sSpecType: string): string;
// Find spectral types in multiple stars
begin
  Result := '';

  if(AnsiContainsStr(sSpecType,'MARK')) then
    exit;

  sSpecType := Trim(sSpecType);

  sSpecType := AnsiReplaceStr(sSpecType,'-','+');

  if(AnsiContainsStr(sSpecType,'+O')) then
    Result := 'O';
  if(AnsiContainsStr(sSpecType,'+B')) then
    Result := 'B';
  if(AnsiContainsStr(sSpecType,'+A')) then
    Result := 'A';
  if(AnsiContainsStr(sSpecType,'+F')) then
    Result := 'F';
  if(AnsiContainsStr(sSpecType,'+G')) then
    Result := 'G';
  if(AnsiContainsStr(sSpecType,'+K')) then
    Result := 'K';
  if(AnsiContainsStr(sSpecType,'+M')) then
    Result := 'M';

  // Carbon Classes of Red Giants
  if(AnsiContainsStr(sSpecType,'+R')) then
    Result := 'R';
  if(AnsiContainsStr(sSpecType,'+N')) then
    Result := 'N';
  if(AnsiContainsStr(sSpecType,'+S')) then
    Result := 'S';
  if(AnsiContainsStr(sSpecType,'+C')) then
    Result := 'C';

  // Brown Dwarfs
  if(AnsiContainsStr(sSpecType,'+L')) then
    Result := 'L';
  if(AnsiContainsStr(sSpecType,'+T')) then
    Result := 'T';
  if(AnsiContainsStr(sSpecType,'+Y')) then
    Result := 'Y';

end;

function GetHR_YProz_FromSpecType(sSpecType: string): Integer;
begin
  Result := -1;

  sSpecType := Uppercase(Trim(sSpecType));

  if(Length(sSpecType) = 0) then
    exit;

  if (AnsiContainsStr(sSpecType,'MARK-')) then
  begin
    Result := -1; // Special Mark symbol. No Sprctral type descriptor!
    exit;
  end;

  Result := 55;

  if(
  (length(sSpecType) > 5)
  and (AnsiContainsStr(RightStr(sSpecType,3),'0'))
  and (not AnsiContainsStr(sSpecType,'V'))
  and (not AnsiContainsStr(sSpecType,'I'))
  ) then
  begin
    Result := 5; // Hyperriese 0
    exit;
  end;


  if (AnsiContainsStr(sSpecType,'III')) then
  begin
    Result := 35; // Riesen III
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'VII') or (LeftStr(sSpecType,1) = 'D')) then
  begin
    Result := 85; // Weiße Zwerge
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'II')) then
  begin
    Result := 30; // Helle Riesen II
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'IA')) then
  begin
    Result := 10; // Überriesen 1a
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'IB')) then
  begin
    Result := 20; // Überriesen 1b
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'IV')) then
  begin
    Result := 40; // Unterriesen
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'VI')  or (LeftStr(sSpecType,2) = 'SD')) then
  begin
    Result := 75; // Sub-Zwerge
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'V')) then  // Falls, wie bei Mizar ein 'i' am Ende und vorher ein 'V'
  begin
    Result := 55; // Hauptreihe
    exit;
  end;

  if (AnsiContainsStr(sSpecType,'I')) then
  begin
    Result := 20; // Überriesen
    exit;
  end;

end;

function GetSpecDX(sSpecType: string; iIndex: Integer; iXMin, iXMax: Integer): Integer;
// Sub-Shifting Spectral type O0..9, B0..9, A0..9, F0..9, ...
begin
  Result := iXMin;

  if(Length(sSpecType) < 2) then exit;

  if(sSpecType[iIndex] = '1') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*1.0/9.0)
  else if (sSpecType[iIndex] = '2') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*2.0/9.0)
  else if (sSpecType[iIndex] = '3') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*3.0/9.0)
  else if (sSpecType[iIndex] = '4') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*4.0/9.0)
  else if (sSpecType[iIndex] = '5') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*5.0/9.0)
  else if (sSpecType[iIndex] = '6') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*6.0/9.0)
  else if (sSpecType[iIndex] = '7') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*7.0/9.0)
  else if (sSpecType[iIndex] = '8') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*8.0/9.0)
  else if (sSpecType[iIndex] = '9') then
    Result := iXMin + Trunc((iXMax - iXMin - 1)*9.0/9.0);

end;

function GetMKKFromSpecType(sSpecType: string; iMStarIndex: Integer; var sMKKTypeDE: string; var sMKKTypeEN: string): Boolean;
var
  sMultiStarDE, sMultiStarEN: string;
  iCountMStar: Integer;
begin
  Result := false;

  sMKKTypeDE := '';
  sMKKTypeEN := '';
  sMultiStarDE := 'Einzelstern';
  sMultiStarEN := 'Single Star';

  if(Length(sSpecType) = 0) then
    exit;

  // Mask Size class intermediaries
  sSpecType := AnsiReplaceStr(sSpecType,'I-I','I/I');
  sSpecType := AnsiReplaceStr(sSpecType,'V-I','V/I');
  sSpecType := AnsiReplaceStr(sSpecType,'I-V','I/V');

  sSpecType := AnsiReplaceStr(sSpecType,'-','+');

  iCountMStar := CountChar(sSpecType,'+');
  case iCountMStar of
    0: begin sMultiStarEN := 'Single Star'; sMultiStarDE := 'Einzelstern'; end;
    1: begin sMultiStarEN := 'Double Star'; sMultiStarDE := 'Doppelstern'; end;
    else
    begin
      sMultiStarEN := IntToStr(iCountMStar + 1) + 'x Multi Star System';
      sMultiStarDE := IntToStr(iCountMStar + 1) + '-fach Sternsystem';
    end;
  end;

  // if last symbol is '+':
  case iCountMStar of
    0:
    begin
      sMultiStarEN := 'Single Star'; sMultiStarDE := 'Einzelstern';
    end;
    1:
    begin
      sMultiStarEN := 'Double Star'; sMultiStarDE := 'Doppelstern';
    end
    else
    begin
      sMultiStarEN := 'Multi Star'; sMultiStarDE := 'Mehrfachstern';
    end;
  end; // case

  sSpecType := GetSingleSpecType(sSpecType,iMStarIndex);

  if(Length(sSpecType) = 0) then
    exit;

  if(AnsiContainsStr(sSpecType,'VII') or (LeftStr(sSpecType,2) = 'DQ') or (LeftStr(sSpecType,2) = 'DA') or (LeftStr(sSpecType,2) = 'DZ')) then
  begin
    sMKKTypeDE := 'Weißer Zwerg' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'White Dwarf' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'III')) then
  begin
    sMKKTypeDE := 'Riese' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Giant' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'II')) then
  begin
    sMKKTypeDE := 'Heller Riese' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Bright Giant' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'IA')) then
  begin
    sMKKTypeDE := 'Überriese Ia' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Supergiant Ia' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'IB')) then
  begin
    sMKKTypeDE := 'Überriese Ib' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Supergiant Ib' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'IV')) then
  begin
    sMKKTypeDE := 'Unterriese' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Subgiant' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'VI')  or (LeftStr(sSpecType,2) = 'SD')) then
  begin
    sMKKTypeDE := 'Unterzwerg' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Subdwarf' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'V')) then
  begin
    sMKKTypeDE := 'Zwerg' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Dwarf' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (AnsiContainsStr(sSpecType,'I')) then  // falls vorher kleines i am Ende ond vorher in 'V' wie bei Mizar
  begin
    sMKKTypeDE := 'Überriese' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Supergiant' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if (LeftStr(sSpecType,1) = 'L') or (LeftStr(sSpecType,1) = 'T') or (LeftStr(sSpecType,1) = 'Y') then
  begin
    sMKKTypeDE := 'Brauner Zwerg' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Brown Dwarf' + ' - ' +  sMultiStarEN;
    Result := true;
  end
  else if(AnsiContainsStr(RightStr(sSpecType,3),'0')) then
  begin
    sMKKTypeDE := 'Hyperriese' + ' - ' +  sMultiStarDE;
    sMKKTypeEN := 'Hypergiant' + ' - ' +  sMultiStarEN;
    Result := true;
  end;

end;

function GetHR_XProz_FromSpecType(sSpecType: string): Integer;
begin
  Result := -1;
  //iIndex := 2;

  sSpecType := Uppercase(Trim(sSpecType));

  if(Length(sSpecType) = 0) then
    exit;

  if (AnsiContainsStr(sSpecType,'MARK-')) then
  begin
    Result := -1; // Special Mark!
  end;

  if(sSpecType[1] = 'D') and (length(sSpecType) >= 3) then
  begin
    Result := 40; // Default.

    // Sonderfallbehandlung: Weiße Zwerge
    if (sSpecType[3] = '0') then
      Result := 15; // Blau

    if (sSpecType[3] = '1') then
      Result := 25; // Weißblau
    if (sSpecType[3] = '2') then
      Result := 25; // Weißblau

    if (sSpecType[3] = '3') then
      Result := 40; // Weiß
    if (sSpecType[3] = '4') then
      Result := 40; // Weiß
    if (sSpecType[3] = '4') then
      Result := 40; // Weiß
    if (sSpecType[3] = '5') then
      Result := 40; // Weiß

    if (sSpecType[3] = '6') then
      Result := 50; // Weiß-Gelb
    if (sSpecType[3] = '7') then
      Result := 50; // Weiß-Gelb
    if (sSpecType[3] = '8') then
      Result := 50; // Weiß-Gelb

    if (sSpecType[3] = '7') then
      Result := 58; // Gelb

    exit;
  end;

  if (sSpecType[1] = 'O') then
    Result := GetSpecDX(sSpecType,2,10,20); // Blau

  if (sSpecType[1] = 'B') then
    Result := GetSpecDX(sSpecType,2,20,35); // Weißblau

  if (sSpecType[1] = 'A') then
    Result := GetSpecDX(sSpecType,2,35,45); // Weiß

  if (sSpecType[1] = 'F') then
    Result := GetSpecDX(sSpecType,2,45,55); // Weiß-Gelb

  if (sSpecType[1] = 'G') then
    Result := GetSpecDX(sSpecType,2,55,60); // Gelb $00A60000

  if (sSpecType[1] = 'K') then
    Result := GetSpecDX(sSpecType,2,60,65); // Hellrot

  if (sSpecType[1] = 'R') then
    Result := GetSpecDX(sSpecType,2,65,70); // Rot-Orange

  if (sSpecType[1] = 'M') then
    Result := GetSpecDX(sSpecType,2,70,80); // Rot

  if (sSpecType[1] = 'N') or (sSpecType[1] = 'S') then
    Result := GetSpecDX(sSpecType,2,80,90); // Schwarzrot

  if (sSpecType[1] = 'L') then
    Result := GetSpecDX(sSpecType,2,90,100); // Dunkelrot

  if (sSpecType[1] = 'T') then
    Result := GetSpecDX(sSpecType,2,100,110); // Dunkelrot 2

  if (sSpecType[1] = 'Y') then
    Result := 110; // Schwarzrot

end;

function GetSingleSpecType(sSpecType: string; iMStarIndex: Integer): string;
// Returns a part of a string list that corresponds to the indexed multiple star, if exists
var
  slSpecList: TStringList;
begin
  sSpecType := AnsiReplaceStr(sSpecType,'-','+');

  slSpecList := TStringList.Create;
  slSpecList.Delimiter:='+';
  slSpecList.StrictDelimiter:=true;
  slSpecList.DelimitedText:=sSpecType;

  if(iMStarIndex < slSpecList.Count) then
    Result := slSpecList[iMStarIndex]
  else
    Result := '';

  slSpecList.Free;

end;

function GetSpecIDData(sSingleSpecType: string; var iTMin: Integer; var iTMax: Integer; var iSTemp: Integer): string;
var
  iPart: Integer;
  iSTemp64: Int64;
  sSpecID: string;
begin
  sSpecID := '';
  Result := '';

  iTMin := 4900; iTMax := 6000;
  iSTemp := -1;
  iSTemp64 := -1;
  iPart := 0;

  if(length(sSingleSpecType) >= 2) then
    begin
      if(sSingleSpecType[2] = '0') then iPart := 0;
      if(sSingleSpecType[2] = '1') then iPart := 1;
      if(sSingleSpecType[2] = '2') then iPart := 2;
      if(sSingleSpecType[2] = '3') then iPart := 3;
      if(sSingleSpecType[2] = '4') then iPart := 4;
      if(sSingleSpecType[2] = '5') then iPart := 5;
      if(sSingleSpecType[2] = '6') then iPart := 6;
      if(sSingleSpecType[2] = '7') then iPart := 7;
      if(sSingleSpecType[2] = '8') then iPart := 8;
      if(sSingleSpecType[2] = '9') then iPart := 9;
    end;

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'O')) then
      begin iTMin := 30000; iTMax := 40000; sSpecID := 'O'; end; // Blau

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'B')) then
      begin iTMin := 10000; iTMax := 30000; sSpecID := 'B'; end; // Weißblau

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'A')) then
      begin iTMin := 7500; iTMax := 10000; sSpecID := 'A'; end; // Weiß

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'F')) then
      begin iTMin := 6000; iTMax := 7500; sSpecID := 'F'; end; // Weiß-Gelb

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'G')) then
      begin iTMin := 4900; iTMax := 6000; sSpecID := 'G'; end; // Gelb

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'K')) then
      begin iTMin := 3500; iTMax := 4900; sSpecID := 'K'; end; // Hellrot

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'M')) then
      begin iTMin := 2400; iTMax := 3500; sSpecID := 'M'; end; // Rot

    // Carbon stars
    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'R')) then
      begin iTMin := 3500; iTMax := 5400; sSpecID := 'R'; end; // Schwarzrot

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'N')) then
      begin iTMin := 2000; iTMax := 3500; sSpecID := 'N'; end; // Schwarzrot

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'S')) then
      begin iTMin := 1900; iTMax := 3500; sSpecID := 'S'; end; // Schwarzrot

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'C')) then
      begin iTMin := 2000; iTMax := 5400; sSpecID := 'C'; end; // Schwarzrot

    // Brown Dwarfs
    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'L')) then
      begin iTMin := 2000; iTMax := 2500; sSpecID := 'L'; end; // Dunkelrot

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'T')) then
      begin iTMin := 1000; iTMax := 2000; sSpecID := 'T'; end; // Dunkelrot 2

    if (AnsiContainsStr(LeftStr(sSingleSpecType,4),'Y')) then
      begin iTMin := 500; iTMax := 1000; sSpecID := 'Y'; end; // Schwarzrot

    if (
      ((LeftStr(sSingleSpecType,2) = 'DQ') or (LeftStr(sSingleSpecType,2) = 'DA') or (LeftStr(sSingleSpecType,2) = 'DZ') )
      and (length(sSingleSpecType) >= 3)
      ) then
    begin
      // white dwarf classification
      sSpecID := 'K';

      if(LeftStr(sSingleSpecType,3) = '1') then
        begin iSTemp := 35000; sSpecID := 'O'; end;// Blau

      if(LeftStr(sSingleSpecType,3) = '2') then
        begin iSTemp := 20000; sSpecID := 'B'; end; // Weißblau
      if(LeftStr(sSingleSpecType,3) = '3') then
        begin iSTemp := 20000; sSpecID := 'B'; end; // Weißblau

      if(LeftStr(sSingleSpecType,3) = '4') then
        begin iSTemp := 8750; sSpecID := 'A'; end;  // Weiß
      if(LeftStr(sSingleSpecType,3) = '5') then
        begin iSTemp := 8750; sSpecID := 'A'; end;  // Weiß
      if(LeftStr(sSingleSpecType,3) = '6') then
        begin iSTemp := 8750; sSpecID := 'A'; end;  // Weiß

      if(LeftStr(sSingleSpecType,3) = '7') then
        begin iSTemp := 6750; sSpecID := 'F'; end;  // Weiß-Gelb
      if(LeftStr(sSingleSpecType,3) = '8') then
        begin iSTemp := 6750; sSpecID := 'F'; end;  // Weiß-Gelb

      if(LeftStr(sSingleSpecType,3) = '9') then
        begin iSTemp := 5450; sSpecID := 'G'; end;  // Gelb

    end;

    if(iSTemp = -1) then
    begin
      iSTemp64 := Round(iTMax - ((1.0*iTMax - 1.0*iTMin)/9.0 *iPart));
      iSTemp := iSTemp64;
    end;

    Result := sSpecID;

end;

function GetColorFromSpecTypeExt(sSpecType: string; iMStarIndex: Integer; var iSTemp: Integer;
  var sMKKTypeDE: string; var sMKKTypeEN: string; var sSpecID: string): TColor;
var
  iTMin, iTMax: Integer;
begin
  sMKKTypeDE := '';
  sMKKTypeEN := '';
  sSpecID := '';
  iTMin:=0; iTMax:=0; iSTemp:=0;

  // Extract the correct spectral part if a multistar notation is used
  sSpecType := GetSingleSpecType(sSpecType,iMStarIndex);

  Result := GetColorFromSpecType(sSpecType);

  if(Length(sSpecType) = 0) then
    exit;

  GetMKKFromSpecType(sSpecType,0,sMKKTypeDE,sMKKTypeEN);
  sSpecID := GetSpecIDData(sSpecType,iTMin,iTMax,iSTemp);

end;

function GetColorFromSpecType(sSpecType: string): TColor;
var
  sSubSpec: string;
begin
  Result := $00FFFFFF;
  //sSpecType := Uppercase(Trim(sSpecType)); // Kein uppercase, b <> B, z.b. in K2Ib

  if(Length(sSpecType) = 0) then
    exit;

  if (AnsiContainsStr(sSpecType,'MARK-')) then
  begin
    Result := clLime;
    exit;
  end; // Special Mark!

  sSubSpec := LeftStr(sSpecType,3); // not 4 because auf Polaris spectral type (double star)

  if (AnsiContainsStr(sSubSpec,'O')) then
    begin Result := $00FFFF00; exit; end; // Blau

  if (AnsiContainsStr(sSubSpec,'B')) then
    begin Result := $00FFFFAA; exit; end; // Weißblau

  if (AnsiContainsStr(sSubSpec,'A')) then
    begin Result := $00FFFFFF; exit; end; // Weiß

  if (AnsiContainsStr(sSubSpec,'F')) then
    begin Result := $00AAFFFF; exit; end; // Weiß-Gelb

  if (AnsiContainsStr(sSubSpec,'G')) then
    begin Result := clYellow; exit; end;// Gelb $00A60000
    //Result := $0000FFFF; // Gelb $00A60000

  if (AnsiContainsStr(sSubSpec,'K')) then
    begin Result := $0000AAFF; exit; end; // Hellrot

  if (AnsiContainsStr(sSubSpec,'M')) then
    begin Result := $000000FF; exit; end; // Rot

  // Carbon classes
  if (AnsiContainsStr(sSubSpec,'R')) then
    begin Result := $000022FF; exit; end; // Rot-Orange

  if (AnsiContainsStr(sSubSpec,'N')) then
    begin Result := $000000DD; exit; end; // Schwarzrot

  if (AnsiContainsStr(sSubSpec,'S')) then
    begin Result := $000000DD; exit; end; // Schwarzrot

  if (AnsiContainsStr(sSubSpec,'C')) then
    begin Result := $000000EE; exit; end; // Rot

  // Browsn Dwarfs
  if (AnsiContainsStr(sSubSpec,'L')) then
    begin Result := $000000CC; exit; end; // Dunkelrot

  if (AnsiContainsStr(sSubSpec,'T')) then
    begin Result := $000000AA; exit; end; // Dunkelrot 2

  if (AnsiContainsStr(sSubSpec,'Y')) then
    begin Result := $00000055; exit; end; // Schwarzrot


  if ((LeftStr(sSpecType,1) = 'D') and (length(sSpectype) >= 3)) then
  begin
    // white dwarf classification
    if(sSpecType[3] = '1') then
      Result := $00FFFF00; // Blau

    if(sSpecType[3] = '2') then
      Result := $00FFFFAA; // Weißblau

    if(sSpecType[3] = '3') then
      Result := $00FFFFAA; // Weißblau

    if(sSpecType[3] = '4') then
      Result := $00FFFFFF; // Weiß
    if(sSpecType[3] = '5') then
      Result := $00FFFFFF; // Weiß
    if(sSpecType[3] = '6') then
      Result := $00FFFFFF; // Weiß

    if(sSpecType[3] = '7') then
      Result := $00AAFFFF; // Weiß-Gelb
    if(LeftStr(sSpecType,3) = '8') then
      Result := $00AAFFFF; // Weiß-Gelb

    if(sSpecType[3] = '9') then
      Result := clYellow; // Gelb

    exit;
  end;

end;

function DisplayMoonPhase(dtWTime: TDatetime; var IMG: TImage): Real;
var
  fMoonPhaseProz: Real;
  iMoonPicNo: Integer;
begin
  // iMoonPhaseProz = 0: Vollmond
  // iMoonPhaseProz = 50: Neumond
  // iMoonPhaseProz = 100: Vollmnond

  fMoonPhaseProz :=CalcMoonPhase(dtWTime);
  // Use Standard Picture for Golden Handle
  if not ((fMoonPhaseProz >= 85) and (fMoonPhaseProz <= 86)) then
  begin
    if(fMoonPhaseProz <= 50) then
      iMoonPicNo := 15 + Round(15*(fMoonPhaseProz/50))
    else
      iMoonPicNo := Round((fMoonPhaseProz-50.0)/50 * 15);

    if(iMoonPicNo = 0) then iMoonPicNo := 30;
    LoadImgRes(IMG,'Moon-Mini-' + format('%.2d',[iMoonPicNo]),'PNG');

  end;


  Result := fMoonPhaseProz;
end;

function LoadImgRes(IMG: TImage; sImage: string; sType: string): Boolean;
var
  JPGImage: TJPEGImage;
  PortableNetworkGraphic: TPortableNetworkGraphic; // Because of transparency!
begin
  Result := false;
  sImage := Trim(sImage);
  if(sImage = '') then exit;

  JPGImage := nil;
  PortableNetworkGraphic := nil;

  sType := Uppercase(sType);
  try
    try
    if((sType = 'JPG') or (sType = 'JPEG')) then
    begin
      JPGImage := TJPEGImage.Create;
      JPGImage.LoadFromLazarusResource(sImage);
      IMG.Picture.Graphic := JPGImage;
      Result := true;
    end
    else if(sType = 'PNG') then
    begin
      PortableNetworkGraphic := TPortableNetworkGraphic.Create;
      PortableNetworkGraphic.LoadFromLazarusResource(sImage);
      IMG.Picture.Graphic := PortableNetworkGraphic;
      Result := true;
    end
    else
      Result := false;

    except
      Result := false;
    end;

  finally
    if(JPGImage <> nil) then
      JPGImage.Free;

    if(PortableNetworkGraphic <> nil) then
      PortableNetworkGraphic.Free;
  end;
end;

procedure GetCometAngles(dtDateTime: TDateTime; Comet: TComet;
  var rLE: Real; var rL: Real);
{2012/05/26/fs
Calculating the co-ordinates of a given planet
}
var
rP, rTp,  rE, rOmegaQ, rOmega, rI: Real;
//rA: Real;
rYear, rMc, rECorr: Real;
//rE0: Real;
rTanNuHalf, rNu, rPsi: Real;
//rX, rY: Real;
//rR, rLs, rRE,
//rVal: Real;
rTE, rEpsilonE, rOmegaQE, rEE: Real;
//rAE: Real;
iD: Integer;
dtTime0: TDateTime;
//rVE,
rNE, rME: Real;
//rLambda, rBeta, rAlpha, rDelta, rRho: Real;
//dJD: Double;
begin
rTE := 0.999996;
rEpsilonE := 99.556772;
rOmegaQE := 103.2055;
rEE := 0.016671;
//rAE :=0.999985;

rP := Comet.rP;
rTp := Comet.rTp;
rOmegaQ := Comet.rOmegaQ;
rE := Comet.rE;
//rA :=Comet.rA;
rOmega :=Comet.rOmega;
rI := Comet.rI;

// Comet calculations
// -1-
//rYear := YearOf(dtDateTime) - rP;
//rYear := GetYearVal(dtDateTime) - rP;
rYear := GetDECYear(dtDateTime) - rP;

// -2-
rMc := 360*rYear/rTp;
rMc := GetNormVal(rMc,360);

// -3-
rMc := Pi*rMc/180;

// -4- / -5-
rECorr := KeplerSolve(rE,rMc,1e-7);

// -6-
rTanNuHalf := sqrt((1+rE)/(1-rE))*tan(rECorr/2);

// -7-
rNu := 2*arctan(rTanNuHalf);
//rNu := 2*arctan(rTanNuHalf);
rNu := 180.0*rNu/Pi;

// -8-
rL := rNu + rOmegaQ;

// -9-
//rR := (rA*(1-rE*rE)) / (1+rE*cos(rNu*Pi/180));

// -10-
rPsi := arcsin(sin((rL-rOmega)*Pi/180)*sin(rI*Pi/180));
rPsi := 180.0*rPsi/Pi;

// -11-
//rY := sin((rL-rOmega)*Pi/180)*cos(rI*Pi/180);

// -12-
//rX := cos((rL-rOmega)*Pi/180);

// -13-
//rVal := arctan360(rY,rX);

// -14-
//rLs := rOmega + rVal;

// -15-
//rRs := rR*cos(rPsi*Pi/180);

// -16-
// Earth calculations
//iD := DayDiffEpoch2010(YearOf(dtDateTime)) + DayOfTheYear(dtDateTime);
dtTime0 := EncodeDateTime(1990,01,01,0,0,0,0);
iD := Trunc(dtDateTime - dtTime0);
rNE := 360*iD/(365.242191*rTE);
rNE := GetNormVal(rNE,360);

// -17-
rME := rNE + rEpsilonE - rOmegaQE;

// -18-
rLE := rNE + 360/Pi*rEE*sin(rME*Pi/180) + rEpsilonE;
rLE := GetNormVal(rLE,360);

// -19-
//rVE := rLE - rOmegaQE;

// -20-
//rRE := (rAE*(1-rEE*rEE)) / (1+rEE*cos(rVE*Pi/180));

end;

procedure GetPlanetAngles(dtDateTime: TDateTime; Planet: TPlanet;
  var rLE: Real; var rL1: Real);
{December 2018/fs
Calculating the angles of a given planet
}
var
  dtTime0: TDateTime;
  iD: Integer;
  rTp, rEpsilon, rOmegaQ, rE, rOmega, rI: Real;
  rTE, rEpsilonE, rOmegaQE, rEE: Real;
  rNp, rMp, rVp, rLp: Real;
  rNE, rME, rVE: Real;
  //rA, rAE, rPsi, rRE, rR, : Real;
  rX, rY, rArcTan: Real;
  //rR1: Real;
  (*
  bInnerPlanet: Boolean;
  rLambda, rBeta: Real;
  rAlpha, rDelta: Real;
  dJD: Double;
  *)
begin
  rTE := 0.999996;
  rEpsilonE := 99.556772;
  rOmegaQE := 103.2055;
  rEE := 0.016671;
  //rAE :=0.999985;

  rTp := Planet.rTp;
  rEpsilon := Planet.rEpsilon;
  rOmegaQ := Planet.rOmegaQ;
  rE := Planet.rE;
  //rA :=Planet.rA;
  rOmega :=Planet.rOmega;
  rI := Planet.rI;
  //bInnerPlanet := Planet.bInnerPlanet;

  dtTime0 := EncodeDateTime(2010,01,01,0,0,0,0);
  iD := Trunc(dtDateTime - dtTime0);

  // Planet orbit calculations
  rNp := 360/365.242191 * iD/rTp;
  rNp := GetNormVal(rNp,360);
  rMp := rNp + rEpsilon - rOmegaQ;
  rVp := rMp + 360/Pi*rE*sin(rMp *Pi/180);
  rVp := GetNormVal(rVp,360);
  rLp := rVp + rOmegaQ;
  rLp := GetNormVal(rLp,360);
  //rR := rA*(1-rE*rE)/(1+rE*cos(rVp *Pi/180));

  // Earth orbit calculations
  rNE := 360/365.242191 * iD/rTE;
  rNE := GetNormVal(rNE,360);
  rME := rNE + rEpsilonE - rOmegaQE;
  rVE := rME + 360/Pi*rEE*sin(rME *Pi/180);
  rVE := GetNormVal(rVE,360);
  rLE := rVE + rOmegaQE;
  rLE := GetNormVal(rLE,360);
  //rRE := rAE*(1-rEE*rEE)/(1+rEE*cos(rVE *Pi/180));

  // More Calculus
  //rPsi := arcsin(sin((rLp-rOmega)*Pi/180)*sin(rI*Pi/180)) *180/Pi;
  rY := sin((rLp-rOmega)*Pi/180)*cos(rI*Pi/180);
  rX := cos((rLp-rOmega)*Pi/180);
  rArcTan := arctan360(rY,rX);
  rL1 := rArcTan + rOmega;
  //rR1 := rR*cos(rPsi *Pi/180);

end;

function GetPlanetCoo(dtDateTime: TDateTime; Planet: TPlanet;
    iDST_HH,iUTC_HH: SmallInt;
    var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
    var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real; var rR: Real): Real;
  {November 2012/fs
  Calculating the co-ordinates of a given planet
  }
  var
    dtTime0: TDateTime;
    iD: Real; //Integer;
    rTp, rEpsilon, rOmegaQ, rE, rA, rOmega, rI: Real;
    rTE, rEpsilonE, rOmegaQE, rEE, rAE: Real;
    rNp, rMp, rVp, rLp: Real;
    rNE, rME, rVE, rLE, rRE: Real;
    rPsi, rX, rY, rArcTan, rL1, rR1: Real;
    bInnerPlanet: Boolean;
    rLambda, rBeta: Real;
    //rAlpha, rDelta: Real;
    dJD: Double;
  begin
    rTE := 0.999996;
    rEpsilonE := 99.556772;
    rOmegaQE := 103.2055;
    rEE := 0.016671;
    rAE :=0.999985;

    rTp := Planet.rTp;
    rEpsilon := Planet.rEpsilon;
    rOmegaQ := Planet.rOmegaQ;
    rE := Planet.rE;
    rA :=Planet.rA;
    rOmega :=Planet.rOmega;
    rI := Planet.rI;
    bInnerPlanet := Planet.bInnerPlanet;
    (*
    if(Uppercase(Trim(sPlanet)) = 'JUPITER') then
    begin
      rTp := 11.857911;
      rEpsilon := 337.917132;
      rOmegaQ := 14.6633;
      rE := 0.048907;
      rA :=5.20278;
      rOmega :=100.595;
      rI := 1.3035;
      bInnerPlanet := false;
    end;

    if(Uppercase(Trim(sPlanet)) = 'MERKUR') then
    begin
      rTp := 0.24085;
      rEpsilon := 75.5671;
      rOmegaQ := 77.612;
      rE := 0.205627;
      rA :=0.387098;
      rOmega := 48.449;
      rI := 7.0051;
      bInnerPlanet := true;
    end;
    *)

    dtTime0 := EncodeDateTime(2010,01,01,0,0,0,0);
    //iD := Trunc(dtDateTime - dtTime0);
    iD := dtDateTime - dtTime0;

    // Planet orbit calculations
    rNp := 360/365.242191 * iD/rTp;
    rNp := GetNormVal(rNp,360);
    rMp := rNp + rEpsilon - rOmegaQ;
    rVp := rMp + 360/Pi*rE*sin(rMp *Pi/180);
    rVp := GetNormVal(rVp,360);
    rLp := rVp + rOmegaQ;
    rLp := GetNormVal(rLp,360);
    rR := rA*(1-rE*rE)/(1+rE*cos(rVp *Pi/180));

    // Earth orbit calculations
    rNE := 360/365.242191 * iD/rTE;
    rNE := GetNormVal(rNE,360);
    rME := rNE + rEpsilonE - rOmegaQE;
    rVE := rME + 360/Pi*rEE*sin(rME *Pi/180);
    rVE := GetNormVal(rVE,360);
    rLE := rVE + rOmegaQE;
    rLE := GetNormVal(rLE,360);
    rRE := rAE*(1-rEE*rEE)/(1+rEE*cos(rVE *Pi/180));

    // More Calculus
    rPsi := arcsin(sin((rLp-rOmega)*Pi/180)*sin(rI*Pi/180)) *180/Pi;
    rY := sin((rLp-rOmega)*Pi/180)*cos(rI*Pi/180);
    rX := cos((rLp-rOmega)*Pi/180);
    rArcTan := arctan360(rY,rX);
    rL1 := rArcTan + rOmega;
    rR1 := rR*cos(rPsi *Pi/180);

    if(bInnerPlanet) then
    begin
      rLambda := 180 + rLE + arctan360(rR1*sin((rLE-rL1)* Pi/180),rRE-rR1*cos((rLE-rL1)* Pi/180));

      (*
      rBeta := arctan(
        rR1*tan(rPsi* Pi/180)*sin((rLambda - rL1) *Pi/180) / (rRE*sin((rL1-rLE) *Pi/180))
      );
      *)
    end
    else
    begin
      rLambda := arctan360(rRE*sin((rL1-rLE) *Pi/180),(rR1-rRE*cos((rL1-rLE) *Pi/180))) + rL1;
    end;
    rLambda := GetNormVal(rLambda,360);

    rBeta := arctan(
      rR1*tan(rPsi *Pi/180)*sin((rLambda - rL1) *Pi/180) / (rRE*sin((rL1-rLE) *Pi/180))
      );
    rBeta := rBeta *180/Pi;

    dJD := GetJulTime(dtTime0,iDST_HH,iUTC_HH);

    EclipticToEquatorial(dJD,rLambda,rBeta,
      iRA_HH,iRA_MM,rRA_SS,
      iDEC_DEG,iDEC_MM,rDEC_SS);

    // Distance from earth: squared sum of the tangens argument fraction! (Calculating the coordinates of a planet p. 125-127), Practical Astronomy...
    Result := sqrt(
      rR1*tan(rPsi *Pi/180) * rR1*tan(rPsi *Pi/180) +
      (rRE*sin((rL1-rLE) *Pi/180)/(sin((rLambda - rL1) *Pi/180))) * (rRE*sin((rL1-rLE) *Pi/180)/(sin((rLambda - rL1) *Pi/180)))
      );

    //Result := sqrt( (rR1*tan(rPsi *Pi/180)*sin((rLambda - rL1) *Pi/180))*(rR1*tan(rPsi *Pi/180)*sin((rLambda - rL1) *Pi/180)) +
    //  (rRE*sin((rL1-rLE) *Pi/180))*(rRE*sin((rL1-rLE) *Pi/180))
    //  );


  end;

(*
  procedure GetAOObjectPathVal(AObject: TAObject; iGLat_HH, GLat_MM, iDST_HH, iUTC_HH: SmallInt; var rRA: Real; var rDEC: Real);
  begin
    rRA := 0;
    rDEC := 0;
  end;
*)
  procedure GetCometCoo(dtDateTime: TDateTime; Comet: TComet;
  iDST_HH,iUTC_HH: SmallInt;
  var iRA_HH: Word; var iRA_MM: Word; var rRA_SS: Real;
  var iDEC_DEG: SmallInt; var iDEC_MM: SmallInt; var rDEC_SS: Real;
  var rRs: Real; // Sun distance
  var rRho: Real // Earth distance
  );
{2012/05/26/fs
Calculating the co-ordinates of a given planet
}
var
  rP, rTp, rA, rE, rOmegaQ, rOmega, rI: Real;
  rYear, rMc, rECorr: Real;
  // rE0: Real;
  rTanNuHalf, rNu, rL, rR, rPsi, rX, rY: Real;
  rVal, rLs: Real;
  rTE, rEpsilonE, rOmegaQE, rEE, rAE: Real;
  iD: Double; //Integer;
  dtTime0: TDateTime;
  rVE, rNE, rME, rRE, rLE: Real;
  rLambda, rBeta: Real;
  //rAlpha, rDelta: Real;
  dJD: Double;
begin
  rTE := 0.999996;
  rEpsilonE := 99.556772;
  rOmegaQE := 103.2055;
  rEE := 0.016671;
  rAE :=0.999985;

  rP := Comet.rP;
  rTp := Comet.rTp;
  rOmegaQ := Comet.rOmegaQ;
  rE := Comet.rE;
  rA :=Comet.rA;
  rOmega :=Comet.rOmega;
  rI := Comet.rI;

  //if(AnsiContainsStr(Comet.sName_DE,'Neowise')) then
  //  ShowMessage('Neowise!');

  //if(rI > 90) then rI := -180 + rI;

  // Comet calculations
  // -1-
  //rYear := YearOf(dtDateTime) - rP;
  //rYear := GetYearVal(dtDateTime) - rP;  98.286392;61.009686845407;
  rYear := GetDECYear(dtDateTime) - rP;

  // -2-
  rMc := 360*rYear/rTp;
  rMc := GetNormVal(rMc,360);

  // -3-
  rMc := Pi*rMc/180;

  // -4- / -5-
  rECorr := KeplerSolve(rE,rMc,1e-7);

  // -6-
  rTanNuHalf := sqrt((1+rE)/(1-rE))*tan(rECorr/2);

  // -7-
  rNu := 2*arctan(rTanNuHalf);
  rNu := 180.0*rNu/Pi;

  // -8-
  rL := rNu + rOmegaQ;

  // -9-
  rR := (rA*(1-rE*rE)) / (1+rE*cos(rNu*Pi/180));

  // -10-
  rPsi := arcsin(sin((rL-rOmega)*Pi/180)*sin(rI*Pi/180));
  rPsi := 180.0*rPsi/Pi;

  // -11-
  rY := sin((rL-rOmega)*Pi/180)*cos(rI*Pi/180);

  // -12-
  rX := cos((rL-rOmega)*Pi/180);

  // -13-
  rVal := arctan360(rY,rX);

  // -14-
  rLs := rOmega + rVal;

  // -15-
  rRs := rR*cos(rPsi*Pi/180);

  // -16-
  // Earth calculations
  //iD := DayDiffEpoch2010(YearOf(dtDateTime)) + DayOfTheYear(dtDateTime);
  dtTime0 := EncodeDateTime(1990,01,01,0,0,0,0);
  //iD := Trunc(dtDateTime - dtTime0);
  iD := dtDateTime - dtTime0;
  rNE := 360*iD/(365.242191*rTE);
  rNE := GetNormVal(rNE,360);

  // -17-
  rME := rNE + rEpsilonE - rOmegaQE;

  // -18-
  rLE := rNE + 360/Pi*rEE*sin(rME*Pi/180) + rEpsilonE;
  rLE := GetNormVal(rLE,360);

  // -19-
  rVE := rLE - rOmegaQE;

  // -20-
  rRE := (rAE*(1-rEE*rEE)) / (1+rEE*cos(rVE*Pi/180));

  // -21-
  if(rRs < rRE) then
    rLambda := 180 + rLE + 180.0/Pi*arctan((rRs*sin((rLE-rLs)*Pi/180)) / (rRE-rRs*cos((rLE-rLs)*Pi/180)))
  else
    rLambda := 180.0/Pi*arctan((rRE*sin((rLs-rLE)*Pi/180)) / (rRs - rRE*cos((rLs-rLE)*Pi/180))) + rLs;

  rLambda := GetNormVal(rLambda,360);

  // -22-
  rBeta := arctan((rRs*tan(rPsi*Pi/180)*sin((rLambda-rLs)*Pi/180)) / (rRE*sin((rLs-rLE)*Pi/180)));
  rBeta := rBeta*180.0/Pi;

  // -23-  -24-
  dJD := GetJulTime(dtDateTime,iDST_HH,iUTC_HH);

  EclipticToEquatorial(dJD,rLambda,rBeta,
    iRA_HH,iRA_MM,rRA_SS,
    iDEC_DEG,iDEC_MM,rDEC_SS);

  if(rRE*rRE + rR*rR - 2*rRE*rR*cos((rL-rLE)*Pi/180)*cos(rPsi*Pi/180) >=0) then
    rRho := sqrt(rRE*rRE + rR*rR - 2*rRE*rR*cos((rL-rLE)*Pi/180)*cos(rPsi*Pi/180))
  else
    rRho := 0;

end;

  procedure ImportCatalog_C(var olAOList: TObjectList; iDST_HH,iUTC_HH: SmallInt; sLANG_ID: string; sAOFileName: string);
  {2012-09-12/fs
  Import Catalogue of solar comets (elliptical path)
  }
  var
    i: Integer;
    slBuffer: TStringList;
    Comet: TComet;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iRA_HH,iRA_MM: Word;
    rRA_SS: Real;
    iDEC_DEG, iDEC_MM: SmallInt;
    rDEC_SS: Real;
    rRs, rRho: Real;
    bIsHeader: Boolean;
    iLineCnt: Integer;
  begin
    bIsHeader := true;

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';

    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-C.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCnt := 1;

    while not eof(tfAO) do
    begin
      Comet := nil;

      slBuffer.Clear;
      ReadLn(tfAO,sLine);
      if(bIsHeader) then
      begin
        ReadLn(tfAO,sLine);
        bIsHeader := false;
      end;

      sLine := AnsiReplaceStr(sLine,' ','~');
      //Memo1.Lines.Add(sLine);

      slBuffer.DelimitedText:=sLine;
      //ShowMessage(sLine);
      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          Comet := TComet.Create;//(P__STARMAP);

          if(Comet.SHP = nil) then
            Comet.SHP := TShape.Create(nil);

          Comet.rOmegaQ := -999;
          Comet.rOmega := -999;
          Comet.rTp := -999;
          Comet.rA := -999;
          Comet.rE := -999;
          Comet.rI := -999;

          Comet.SHP.Shape:=stCircle;
          Comet.SHP.Height:=0 ;
          Comet.SHP.ShowHint := false;

          if(Comet.L__AO = nil) then
            Comet.L__AO := TLabel.Create(nil);

          Comet.L__AO.Visible:=true;
          Comet.SHP.Pen.Color := clAqua;
          Comet.SHP.Brush.Color := clAqua;
          Comet.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];
        case i of
         //0: // Do nothing.
         1: Comet.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         2: Comet.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         3: if(StrIsNum(true,sVar)) then Comet.rP := StrToFloatExt4(sVar);
         4: if(StrIsNum(true,sVar)) then Comet.rOmegaQ := StrToFloatExt4(sVar);
         5: if(StrIsNum(true,sVar)) then Comet.rOmega := StrToFloatExt4(sVar);
         6: if(StrIsNum(true,sVar)) then Comet.rTp := StrToFloatExt4(sVar);
         7: if(StrIsNum(true,sVar)) then Comet.rA := StrToFloatExt4(sVar);
         8: if(StrIsNum(true,sVar)) then Comet.rE := StrToFloatExt4(sVar);
         9: if(StrIsNum(true,sVar)) then Comet.rI := StrToFloatExt4(sVar);
         10: Comet.sAOType := slBuffer[i];
        end; // case
      end;
      if(Comet <> nil) then
      begin
        iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
        iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0; rRs:=0; rRho:=0;

        GetCometCoo(Now,Comet,
          iDST_HH,iUTC_HH,
          iRA_HH,iRA_MM,rRA_SS,
          iDEC_DEG,iDEC_MM,rDEC_SS,rRs,rRho);

        // Adjust Right ascension and declination for the current day
        Comet.iRA_Hours:=iRA_HH;
        Comet.iRA_Min:=iRA_MM;
        Comet.rRA_Sec:=rRA_SS;
        Comet.iDec_Deg:=iDEC_DEG;
        Comet.iDec_Min:=iDEC_MM;
        Comet.rDec_Sec:=rDEC_SS;
        Comet.rRs := rRs;
        Comet.rRho := rRho;

        SetAOLabel(Comet,sLANG_ID);

        AOIndexControl(Comet,iLineCnt,olAOList.Count);

        olAOList.Add(Comet);
      end;

      Inc(iLineCnt);
    end;

    CloseFile(tfAO);

  end;

  procedure ImportCatalog_M(var olMoonList: TObjectList; iPlanetIndex: Integer; sLANG_ID: string; sAOFileName: string);
  {2016-01-31/fs
  Import Catalogue of Moons
  }
  var
    i: Integer;
    slBuffer,slBufZeroPass: TStringList;
    Moon: TMoon;
    tfAO: TextFile;
    sLine, sVar: string;
  begin
    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';

    slBufZeroPass := TStringList.Create;
    slBufZeroPass.Delimiter:='/';
    slBufZeroPass.StrictDelimiter:=true;

    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-M.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    while not eof(tfAO) do
    begin
      Moon := nil;
      slBuffer.Clear;
      ReadLn(tfAO,sLine);
      // Suppress comments
      while (LeftStr(Trim(sLine),2) = '//') do
      begin
        ReadLn(tfAO,sLine);
      end;

      sLine := AnsiReplaceStr(sLine,' ','~');
      //Memo1.Lines.Add(sLine);

      slBuffer.DelimitedText:=sLine;
      //ShowMessage(sLine);
      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          Moon := TMoon.Create;
          if(Moon.SHP = nil) then
            Moon.SHP := TShape.Create(nil);

          Moon.rRadius_km := -999;
          Moon.rMass_kg := -999;
          Moon.rSMAxis_km := -999;
          Moon.rOrbitalPeriod_d := -999;
          Moon.rRho_g_qcm := -999;
          Moon.rMag := -999;
          Moon.rE := -999;

          Moon.SHP.Shape:=stCircle;
          Moon.SHP.Height:=0 ;
          Moon.SHP.ShowHint := false;

          if(Moon.L__AO = nil) then
            Moon.L__AO := TLabel.Create(nil);

          Moon.L__AO.Visible:=true;
          Moon.SHP.Pen.Color := clAqua;
          Moon.SHP.Brush.Color := clAqua;
          Moon.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];
        case i of
         0: if(StrIsNum(false,sVar)) then Moon.iMoonIndex := StrToInt(slBuffer[i]);
         1: if(StrIsNum(false,sVar)) then Moon.iPlanetIndex := StrToInt(slBuffer[i]);
         2: Moon.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         3: Moon.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         4: if(StrIsNum(true,sVar)) then Moon.rRadius_km := StrToFloatExt4(sVar);
         5: if(StrIsNum(true,sVar)) then
           Moon.rSMAxis_km := StrToFloatExt4(sVar);
         6: if(StrIsNum(true,sVar)) then
           Moon.rMass_kg := StrToFloatExt4(sVar);
         7: if(StrIsNum(false,sVar)) then Moon.rMass_Exp := StrToInt(sVar);
         8: if(StrIsNum(true,sVar)) then Moon.rOrbitalPeriod_d := StrToFloatExt4(sVar);
         9: if(StrIsNum(true,sVar)) then Moon.rRho_g_qcm := StrToFloatExt4(sVar);
         10: if(StrIsNum(true,sVar)) then Moon.rMag := StrToFloatExt4(sVar);
         11: if(StrIsNum(true,sVar)) then
         begin
           Moon.rE := StrToFloatExt4(sVar);
           //ShowMessage(FloatToStr(Moon.rE));
         end;
         12: Moon.Color := StringToColor(slBuffer[i]);
         13: Moon.sAOType := slBuffer[i];
         14:
         begin
           slBufZeroPass.DelimitedText := slBuffer[i];

           if(slBufZeroPass.Count = 6) then
           begin
             Moon.dtZeroPass := EncodeDate(StrToInt(slBufZeroPass[0]),StrToInt(slBufZeroPass[1]),StrToInt(slBufZeroPass[2])) +
               EncodeTime(StrToInt(slBufZeroPass[3]),StrToInt(slBufZeroPass[4]),StrToInt(slBufZeroPass[5]),0);
           end
           else
             Moon.dtZeroPass := 0;

         end;
        end; // case
      end;

      if(Moon <> nil) and (Moon.iPlanetIndex = iPlanetIndex) then
      begin
        SetAOLabel(Moon,sLANG_ID);
        olMoonList.Add(Moon);
      end;

    end;

    slBufZeroPass.Destroy;
    CloseFile(tfAO);

  end;

  procedure ImportCatalog_P(var olAOList: TObjectList; iDST_HH, iUTC_HH: SmallInt; sLANG_ID: string; sAOFileName: string);
  {2012-09-12/fs
  Import Catalogue of solar planets
  }
  var
    i: Integer;
    slBuffer: TStringList;
    Planet: TPlanet;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iRA_HH,iRA_MM: Word;
    rRA_SS: Real;
    iDEC_DEG, iDEC_MM: SmallInt;
    rDEC_SS: Real;
    bIsHeader: Boolean;
    rR: Real;
    iLineCntP, iLineCntPA: Integer;
  begin
    bIsHeader := true;

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';

    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-P.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCntP := 1;
    iLineCntPA := 1;

    while not eof(tfAO) do
    begin
      Planet := nil;
      slBuffer.Clear;
      ReadLn(tfAO,sLine);
      if(bIsHeader) then
      begin
        // Forget header line!
        ReadLn(tfAO,sLine);
        bIsHeader := false;
      end;

      sLine := AnsiReplaceStr(sLine,' ','~');
      //Memo1.Lines.Add(sLine);

      slBuffer.DelimitedText:=sLine;
      //ShowMessage(sLine);
      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          Planet := TPlanet.Create;//(P__STARMAP);

          if(Planet.SHP = nil) then
            Planet.SHP := TShape.Create(nil);

          Planet.rTp := -999;
          Planet.rEpsilon := -999;
          Planet.rOmegaQ := -999;
          Planet.rE := -999;
          Planet.rA := -999;
          Planet.rI := -999;
          Planet.rOmega := -999;
          Planet.rTheta0 := -999;
          Planet.rV0 := -999;
          Planet.rDiameterRatio := -999;
          Planet.rMassRatio := -999;
          Planet.sPlanetType := 'P';

          Planet.SHP.Shape:=stCircle;
          Planet.SHP.Height:=0 ;
          Planet.SHP.ShowHint := false;

          if(Planet.L__AO = nil) then
            Planet.L__AO := TLabel.Create(nil);

          Planet.L__AO.Visible:=true;
          Planet.SHP.Pen.Color := clAqua;
          Planet.SHP.Brush.Color := clAqua;
          Planet.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];
        case i of
         0: Planet.iPlanetIndex := StrToInt(slBuffer[i]);
         1: Planet.sName_DE := Trim(AnsiReplaceStr(slBuffer[i],'~',' '));
         2: Planet.sName_EN := Trim(AnsiReplaceStr(slBuffer[i],'~',' '));
         3: if(StrIsNum(true,sVar)) then Planet.rTp := StrToFloatExt4(sVar); // Orbital Period

         // Epsilon =  Longitude of ascendending node + Argument of perihelion - nT n=2 Pi / T_Orbit * T_time_of_pericenter_passage
         // http://scienceworld.wolfram.com/physics/LongitudeatEpoch.html
         4: if(StrIsNum(true,sVar)) then Planet.rEpsilon := StrToFloatExt4(sVar); // Longitude at the epoch (S: 123 Practical Astronomy with your Calculator or Spreadsheet

         5: if(StrIsNum(true,sVar)) then Planet.rOmegaQ := StrToFloatExt4(sVar); // Longitude of ascending node + Argument of perihelion ( - 360)
         6: if(StrIsNum(true,sVar)) then Planet.rE := StrToFloatExt4(sVar); // Eccentricity
         7: if(StrIsNum(true,sVar)) then Planet.rA := StrToFloatExt4(sVar); // Semimajor Axis
         8: if(StrIsNum(true,sVar)) then Planet.rI := StrToFloatExt4(sVar); // Inclination
         9: if(StrIsNum(true,sVar)) then Planet.rOmega := StrToFloatExt4(sVar); // Longitude of ascending node
         10: if(StrIsNum(true,sVar)) then Planet.rTheta0 := StrToFloatExt4(sVar); // visual diameter arcsec
         11: if(StrIsNum(true,sVar)) then Planet.rV0 := StrToFloatExt4(sVar); // Visual magnitude
         12: if(StrIsNum(false,sVar)) then
           Planet.bInnerPlanet:= (slBuffer[i] = '1');
         13: if(StrIsNum(true,sVar)) then Planet.rDiameterRatio := StrToFloatExt4(sVar);
         14: if(StrIsNum(true,sVar)) then Planet.rMassRatio := StrToFloatExt4(sVar);
         15: Planet.Color := StringToColor(slBuffer[i]);
         16:
         begin
           Planet.sPlanetType := slBuffer[i];

           if(Planet.sPlanetType <> 'A') then
             iLineCntPA := 1;

         end;
         17: Planet.sAOType := slBuffer[i];
        end; // case
      end;
      if(Planet <> nil) then
      begin
        iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
        iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0; rR :=0;

        Planet.rDistFromEarth_AU := GetPlanetCoo(Now,Planet,
          iDST_HH,iUTC_HH,
          iRA_HH,iRA_MM,rRA_SS,
          iDEC_DEG,iDEC_MM,rDEC_SS,rR);

        Planet.rVisuAngle_arcsec := Get_arcsec(Planet.rDiameterRatio,Planet.rDistFromEarth_AU);
        Planet.rDistFromSun_AU := rR;

        // Adjust Right ascension and declination for the current day
        Planet.iRA_Hours:=iRA_HH;
        Planet.iRA_Min:=iRA_MM;
        Planet.rRA_Sec:=rRA_SS;
        Planet.iDec_Deg:=iDEC_DEG;
        Planet.iDec_Min:=iDEC_MM;
        Planet.rDec_Sec:=rDEC_SS;

        Planet.SetImage();

        SetAOLabel(Planet,sLANG_ID);

        if(Planet.sPlanetType <> 'A') then
          AOIndexControl(Planet,iLineCntP,olAOList.Count)
        else
          AOIndexControl(Planet,iLineCntPA,olAOList.Count);

        olAOList.Add(Planet);
      end;

      if(Planet.sPlanetType <> 'A') then
        Inc(iLineCntP)
      else
        Inc(iLineCntPA);

    end;

    CloseFile(tfAO);

  end;

  function GenSolFrac(sSpType: string): Real;
  begin
    Result := 0;

    if(AnsiContainsStr(sSpType,'K1III') or AnsiContainsStr(sSpType,'K2III')) then
      Result := 30
    else if(AnsiContainsStr(sSpType,'III') and AnsiContainsStr(sSpType,'K')) then
        Result := 40
    else if(AnsiContainsStr(sSpType,'M1III') or AnsiContainsStr(sSpType,'M2III')) then
      Result := 150
    else if(AnsiContainsStr(sSpType,'III') and AnsiContainsStr(sSpType,'M')) then
      Result := 300
    else if(AnsiContainsStr(sSpType,'III') and AnsiContainsStr(sSpType,'G')) then
      Result := 10
    else if(AnsiContainsStr(sSpType,'III')) then
      Result := 30
    else if(AnsiContainsStr(sSpType,'II')) then
      Result := 90
    else if(AnsiContainsStr(sSpType,'Ib')) then
    begin
      if(LeftStr(sSpType,1) = 'O') then
        Result := 10
      else if(LeftStr(sSpType,1) = 'B') then
        Result := 20
      else if(LeftStr(sSpType,1) = 'A') then
        Result := 30
      else if(LeftStr(sSpType,1) = 'F') then
        Result := 40
      else if(LeftStr(sSpType,1) = 'G') then
        Result := 70
      else if(LeftStr(sSpType,1) = 'K') then
        Result := 80
      else if(LeftStr(sSpType,1) = 'M') then
        Result := 100
      else
        Result := 120
    end
    else if(AnsiContainsStr(sSpType,'Iab')) then
    begin
      if(LeftStr(sSpType,1) = 'O') then
        Result := 30
      else if(LeftStr(sSpType,1) = 'B') then
        Result := 50
      else if(LeftStr(sSpType,1) = 'A') then
        Result := 70
      else if(LeftStr(sSpType,1) = 'F') then
        Result := 100
      else if(LeftStr(sSpType,1) = 'G') then
        Result := 200
      else if(LeftStr(sSpType,1) = 'K') then
        Result := 600
      else if(LeftStr(sSpType,1) = 'M') then
        Result := 800
      else
        Result := 900;
    end
    else if(AnsiContainsStr(sSpType,'Ia')) then
    begin
      if(LeftStr(sSpType,1) = 'O') then
        Result := 60
      else if(LeftStr(sSpType,1) = 'B') then
        Result := 80
      else if(LeftStr(sSpType,1) = 'A') then
        Result := 150
      else if(LeftStr(sSpType,1) = 'F') then
        Result := 200
      else if(LeftStr(sSpType,1) = 'G') then
        Result := 500
      else if(LeftStr(sSpType,1) = 'K') then
        Result := 800
      else if(AnsiContainsStr(sSpType,'M1')) then
        Result := 1500
      else if(AnsiContainsStr(sSpType,'M2')) then
        Result := 1400
      else if(LeftStr(sSpType,1) = 'M') then
        Result := 1000
      else
        Result := 1600;
    end
    else if(AnsiContainsStr(sSpType,'VII')) then
      Result := 0.01
    else if(AnsiContainsStr(sSpType,'VI')) then
      Result := 0.1
    else if(AnsiContainsStr(sSpType,'V')) then
    begin
      if(LeftStr(sSpType,1) = 'O') then
        Result := 6
      else if(LeftStr(sSpType,1) = 'B') then
          Result := 5
      else if(LeftStr(sSpType,1) = 'A') then
          Result := 3
      else if(LeftStr(sSpType,1) = 'F') then
          Result := 2
      else if(LeftStr(sSpType,1) = 'G') then
          Result := 1
      else if(LeftStr(sSpType,1) = 'K') then
          Result := 0.5
      else if(LeftStr(sSpType,1) = 'M') then
          Result := 0.1
      else
        Result := 0.01;
    end;
  end;

  procedure ImportCatalog_S(var olAOList: TObjectList; sAOFileName: string;
    var sTinyStarFirstCatNo: string; var iTinyStarBlockWidth: Integer);
  {2012-09-12/fs
  Import Star Catalogue
  }
  var
    iLineCnt, i: Integer;
    slBuffer: TStringList;
    Star: TStar;
    tfAO: TextFile;
    sLine,sVar: string;
    iDecSign: Integer;
    sSpType: string;
    iSTemp: Integer;
    sMKKTypeDE,sMKKTypeEN,sSpecID: string;
    bIsHeader: Boolean;
    //png_O, png_B, png_A, png_F, png_G, png_K, png_M: TPortableNetworkGraphic;
    //sAlbireoLocalDir: string;
    //png: TPortableNetworkGraphic; // <- This variant does not work (reason unknown) (memory exhaustion)
  begin
    BeginMethod('ImportCatalog_S');
    bIsHeader := true;
    // Check for .dat-file. If not found, extract from .zip-file.
    if(not PrepareImportFile(sAOFileName)) then
      exit;
    (*
    sAlbireoLocalDir := ExtractFilePath(sAOFileName);
    ShowMessage('STOP');

    //png := TPortableNetworkGraphic.Create; // <- This variant does not work (reason unknown) (memory exhaustion)
    // This code is also unable to move into a IMG preparation procedure! (memory exhaustion!)
    png_O := TPortableNetworkGraphic.Create;
    png_O.LoadFromFile(ConvertWinPath(sAlbireoLocalDir + 'img\StandardStar_O.png'));
    png_O.Transparent:=true; png_O.TransparentMode:=tmFixed;
    png_B := TPortableNetworkGraphic.Create;
    png_B.LoadFromFile(ConvertWinPath(sAlbireoLocalDir + 'img\StandardStar_B.png'));
    png_B.Transparent:=true; png_B.TransparentMode:=tmFixed;
    png_A := TPortableNetworkGraphic.Create;
    png_A.LoadFromFile(ConvertWinPath(sAlbireoLocalDir + 'img\StandardStar_A.png'));
    png_A.Transparent:=true; png_A.TransparentMode:=tmFixed;
    png_F := TPortableNetworkGraphic.Create;
    png_F.LoadFromFile(ConvertWinPath(sAlbireoLocalDir + 'img\StandardStar_F.png'));
    png_F.Transparent:=true; png_F.TransparentMode:=tmFixed;
    png_G := TPortableNetworkGraphic.Create;
    png_G.LoadFromFile(ConvertWinPath(sAlbireoLocalDir + 'img\StandardStar_G.png'));
    png_G.Transparent:=true; png_G.TransparentMode:=tmFixed;
    png_K := TPortableNetworkGraphic.Create;
    png_K.LoadFromFile(ConvertWinPath(sAlbireoLocalDir + '\img\StandardStar_K.png'));
    png_K.Transparent:=true; png_K.TransparentMode:=tmFixed;
    png_M := TPortableNetworkGraphic.Create;
    png_M.LoadFromFile(ConvertWinPath(sAlbireoLocalDir + 'img\StandardStar_M.png'));
    png_M.Transparent:=true; png_M.TransparentMode:=tmFixed;
    *)

    // Tiny stars block initialisation (jump over in GenStarMap!)
    //bTinyStarBlockIsActive := false;
    iTinyStarBlockWidth := 0;
    sTinyStarFirstCatNo := '';

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';

    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-S.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));

    Reset(tfAO);

    iLineCnt := 1;
    while (
      (not eof(tfAO)) and
      (not ((giRSCLvl = 0) and (iLineCnt > (gciMaxLowResStars + 1))))
      ) do
    begin
      Star := nil;
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

      iDecSign := 1;

      try
      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          // Generation and Initialisation
          Star := TStar.Create;

          Star.rMAttend:=-999;
          Star.iSepAngle:=0;
          Star.rSolFrac:=-999;
          Star.iSepSec:=0;
          Star.rVarMMax:=-999;
          Star.rVarMMin:=-999;
          Star.rVarPer_D:=-999;
          Star.sMShowerName_DE:='';
          //Star.L__AO.Visible:=true;
          //Star.IMG.ShowHint := false;

          if(Star.SHP <> nil) then
          begin
            Star.SHP.Shape:=stCircle;
            Star.SHP.Height:=0 ;
            Star.SHP.ShowHint := false;
            Star.SHP.Cursor := crCross;
            Star.SHP.Pen.Color := clAqua;
            Star.SHP.Brush.Color := clAqua;
          end;
        end;

        sVar := Trim(slBuffer[i]);

        //if(sVar = '61~B') then
        //  ShowMessage(sVar);

        case i of
         0: Star.sCatNo := AnsiReplaceStr(sVar,'~',' ');
         1: Star.sSym := slBuffer[i];
         2: Star.sCon := slBuffer[i];
         3: Star.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         4: Star.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         5: if(StrIsNum(true,sVar)) then Star.rM := StrToFloatExt4(sVar);
         6: if(StrIsNum(false,sVar)) then Star.iRA_Hours := StrToInt(sVar);
         7: if(StrIsNum(false,sVar)) then Star.iRA_Min := StrToInt(sVar);
         8: if(StrIsNum(true,sVar)) then
           Star.rRA_Sec := StrToFloatExt4(sVar);
         9:
           begin
             if((length(sVar) > 0) and (LeftStr(sVar,1) = '-')) then iDecSign := -1;

             if(StrIsNum(false,sVar)) then
               Star.iDec_Deg := StrToInt(sVar);

           end;
         10: if(StrIsNum(false,sVar)) then Star.iDec_Min := iDecSign*StrToInt(sVar);
         11: if(StrIsNum(true,sVar)) then Star.rDec_Sec := iDecSign*StrToFloatExt4(sVar);
         12:
         begin
           sSPType := AnsiReplaceStr(Trim(slBuffer[i]),'~','');
           sSPType := Trim(sSPType);

           // Add MKK-Type if not defined
           if(
           (Length(sSpType) > 1) and
           (not AnsiContainsStr(Uppercase(sSpType),'V')) and
           (not AnsiContainsStr(Star.sSym,'MARK')) and
           (not AnsiContainsStr(Uppercase(sSpType),'I'))
           ) then
           begin
             if(AnsiContainsStr(sSPType,'S')) then
               sSPTYpe := AnsiReplaceStr(sSPType,'S','III(S)')
             else if(AnsiContainsStr(sSPType,'R')) then
               sSPTYpe := AnsiReplaceStr(sSPType,'R','III(R)')
             else if(AnsiContainsStr(sSPType,'N')) then
               sSPTYpe := AnsiReplaceStr(sSPType,'N','III(N)')
             else if(AnsiContainsStr(sSPType,'e')) then
               sSPTYpe := AnsiReplaceStr(sSPType,'e','III(e)')
             else sSpType := sSpType + '(V)';

           end;
           Star.sSpType := sSpType;
         end;
         13: if(StrIsNum(true,sVar)) then Star.rDist_XLY := StrToFloatExt4(sVar);
         14: if(StrIsNum(true,sVar)) then Star.rSolFrac := StrToFloatExt4(sVar) else Star.rSolFrac := -1;
         15: Star.sAOType := 'S'; //slBuffer[i]; Do not use slBuffer because it may contain an other value if fields a corrupted
         // Location near assigned meteor shower?
         16: Star.sMShowerName_DE := sVar;
         17: Star.sConPartner:= slBuffer[i];
         // 4 (Field 17-20): Variable Star Attributes
         18: Star.sVarType := slBuffer[i];
         19: if(StrIsNum(true,sVar)) then Star.rVarMMax := StrToFloatExt4(sVar);
         20: if(StrIsNum(true,sVar)) then Star.rVarMMin := StrToFloatExt4(sVar);
         21: if(StrIsNum(true,sVar)) then Star.rVarPer_D := StrToFloatExt4(sVar);
         // 3 Binary Star Attribues
         22: if(StrIsNum(true,sVar)) then Star.rMAttend := StrToFloatExt4(sVar);
         23: if(StrIsNum(true,sVar)) then
         begin
           if(sVar = '-1') then sVar := '0'; // Avoid negatives for WORD datatype!
           Star.iSepSec := Round(StrToFloatExt4(sVar)); // StrToInt(sVar); //StrToFloatExt4(sVar);
         end;
         24: if(StrIsNum(true,sVar)) then
         begin
           if(sVar = '-1') then sVar := '0'; // Avoid negatives for WORD datatype!
           Star.iSepAngle := Round(StrToFloatExt4(sVar));
         end;
        end; // case
      end;
      if(Star <> nil) and
        (Star.iRA_Hours + Star.iRA_Min/60.0 + Star.rRA_Sec/3600 <> 0) and // Avoid stars with undefined RA/DEC co-ordinates!
        (Star.iDec_Deg + Star.iDec_Min/60.0 + Star.rDec_Sec/3600 <> 0)
        then
      begin
        Star.iAOIndex := olAOList.Count; // Next Index

        iSTemp := -1;
        Star.Color := GetColorFromSpecTypeExt(sSPType,0,iSTemp,sMKKTypeDE,sMKKTypeEN,sSpecID);
        ///Star.iSTemp := iSTemp;
        //Star.sMKKTypeDE := sMKKTypeDE;
        //Star.sMKKTypeEN := sMKKTypeEN;
        //Star.sSpecID := sSpecID;

        // Unkown SolFrac estimated by luminosity class!
        if(Star.rSolFrac <= 0) and (Star.sSpType <> '') then
        begin
          Star.rSolFrac:= GenSolFrac(Star.sSpType);
        end;

        Star.PrepareStar(false);

        // Register only visible stars which can achieve position above the horizon
        (*
        if((Star.iDec_Deg + Star.iDec_Min/60 + Star.rDec_Sec/3600) >= -(90 - rGLat)) then
        begin
          if(bTinyStarBlockisActive) then
          begin
            if(sTinyStarFirstCatNo = '') then
              sTinyStarFirstCatNo := Star.sCatNo;

            Inc(iTinyStarBlockWidth);
          end;

          olAOList.Add(Star);
        end;
        *)
        // Problem: Re-Importing Catalogs leads to unresolvable screen buffering problem under lazarus,
        // when the location has been tamporariliy changed!
        // Consequence: Import ALL stars!
        AOIndexControl(Star,iLineCnt,olAOList.Count);

        olAOList.Add(Star);

        Inc(iLineCnt);
      end;
      except
        on e: Exception do
        begin
          MessageDlg('Error in Line: ' + sLine + ' Line # ' + IntToStr(iLineCnt),mtError,[mbOK],0);
        end;
      end;
    end;

    //ShowMessage('Imported Stars: ' + IntToStr(iLineCnt));

    //png.Free;
    (*
    png_O.Free;
    png_B.Free;
    png_A.Free;
    png_F.Free;
    png_G.Free;
    png_K.Free;
    png_M.Free;
    *)
    slBuffer.Free;;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCatalog_S');
  end;

procedure ImportCatalog_MW(var olAOList: TObjectList; sLANG_ID: string; rGLat: Real; sAOFileName: string);
  {2019-05-26/fs
  Import Milkyway items
  }
  var
    i: Integer;
    slBuffer: TStringList;
    MO: TMilkywayObject;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iDecSign: Integer;
  begin
    BeginMethod('ImportCatalog_MW');

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';

    // Import astronomical objects catalogue
    //if(not FileExists(ConvertWinPath('AO-MW.dat'))) then
    if(not FileExists(ConvertWinPath(sAOFileName))) then
      exit;

    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    while not eof(tfAO) do
    begin
      MO := nil;
      slBuffer.Clear;
      ReadLn(tfAO,sLine);
      sLine := AnsiReplaceStr(sLine,' ','~');
      SetMsg(sLine);

      if(AnsiContainsStr(sLine,'TAObject')) then
        slBuffer.DelimitedText:=''
      else
        slBuffer.DelimitedText:=sLine;

      iDecSign := 1;

      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          MO := TMilkywayObject.Create;//(P__STARMAP);

          MO.SHP.Shape:=stCircle;
          MO.SHP.Height:=0 ;
          MO.SHP.ShowHint := false;
          MO.L__AO.Visible:=true;
          MO.SHP.Pen.Color := clAqua;
          MO.SHP.Brush.Color := clAqua;
          MO.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];

        case i of
         0: if(StrIsNum(false,sVar)) then MO.iMWItemNo := StrToInt(sVar);
         1: MO.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         2: MO.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         3: if(StrIsNum(true,sVar)) then MO.rM := StrToFloatExt4(sVar);
         4: if(StrIsNum(false,sVar)) then MO.iRA_Hours := StrToInt(sVar);
         5:
           if(StrIsNum(true,sVar)) then
           begin
             MO.iRA_Min := Trunc(StrToFloatExt4(sVar));
             MO.rRA_Sec:= (StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60;
           end;
         6:
           begin
             if((length(sVar) > 0) and (sVar[1] = '-')) then iDecSign := -1;

             if(StrIsNum(false,sVar)) then MO.iDec_Deg := StrToInt(sVar);
           end;
         7: if(StrIsNum(false,sVar)) then MO.iDec_Min := iDecSign*StrToInt(sVar);
         8: MO.sMWType := slBuffer[i];
         9: if(StrIsNum(false,sVar)) then MO.iVisDim1 := StrToInt(sVar) else MO.iVisDim1 := -1;
         10: if(StrIsNum(false,sVar)) then MO.iVisDim2 := StrToInt(sVar) else MO.iVisDim2 := -1;
         11: MO.sAOType := slBuffer[i];
        end; // case
      end;

      if(MO <> nil) then
      begin
        SetAOLabel(MO,sLANG_ID);
        //if((MO.iDec_Deg + MO.iDec_Min/60 + MO.rDec_Sec/3600) >= -(90 - rGLat)) then
        olAOList.Add(MO);
      end;

    end;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCatalog_MW');
  end;

  procedure ImportCatalog_G(var olAOList: TObjectList; sAOFileName: string);
  {2012-09-12/fs
  Import Galaxy Catalogue
  }
  var
    iLineCnt,i,j: Integer;
    slBuffer, slMagnitudes: TStringList;
    Galaxy: TGalaxy;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iDecSign: Integer;
    bIsHeader: Boolean;
    sMag: string;
    rMagStat: Real;
    iMagStatCnt: Integer;
  begin
    bIsHeader := true;

    BeginMethod('ImportCatalog_G');

    if(not PrepareImportFile(sAOFileName)) then
      exit;

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';

    slMagnitudes := TStringList.Create;
    slMagnitudes.Delimiter:='|';
    slMagnitudes.StrictDelimiter:=true;

    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-G.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCnt := 1;
    //while not eof(tfAO) do
    while (
      (not eof(tfAO)) and
      (not ((giRSCLvl = 0) and (iLineCnt > (gciMaxLowResGalaxies + 1) )))
      ) do
    begin
      Galaxy := nil;
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

      iDecSign := 1;

      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          Galaxy := TGalaxy.Create;//(P__STARMAP);

          //if(Galaxy.SHP = nil) then
          //  Galaxy.SHP := TShape.Create(nil);
          (*
          Galaxy.SHP.Shape:=stCircle;
          Galaxy.SHP.Height:=0 ;
          Galaxy.SHP.ShowHint := false;
          *)

          (*
          if(Galaxy.L__AO = nil) then
            Galaxy.L__AO := TLabel.Create(nil);

          Galaxy.L__AO.Visible:=true;
          *)

          //  sMagU, sMagB, sMagV,sMagR, sMagG, sMagI, sMagJ, sMagH, sMagK: string;
          //  sMagsu, sMagsg, sMagsr, sMagsi, sMagsz: string;
          (*
          Galaxy.SHP.Pen.Color := clAqua;
          Galaxy.SHP.Brush.Color := clAqua;
          Galaxy.SHP.Cursor := crCross;
          *)
          Galaxy.rM:=999;
          Galaxy.rMagU:=999;
          Galaxy.rMagB:=999;
          Galaxy.rMagV:=999;
          Galaxy.rMagR:=999;
          Galaxy.rMagG:=999;
          Galaxy.rMagI:=999;
          Galaxy.rMagJ:=999;
          Galaxy.rMagH:=999;
          Galaxy.rMagK:=999;
          Galaxy.rMagsu:=999;
          Galaxy.rMagsg:=999;
          Galaxy.rMagsr:=999;
          Galaxy.rMagsi:=999;
          Galaxy.rMagsz:=999;
        end;

        sVar := AnsiReplaceStr(slBuffer[i],'~',' '); //slBuffer[i];

        case i of
         //0: if(StrIsNum(false,sVar)) then Galaxy.sCatNo := sVar;    // CatNo
         0: Galaxy.sCatNo := sVar;
         1: Galaxy.sNGC := AnsiReplaceStr(
           AnsiReplaceStr(slBuffer[i],'~',' '),'NAME',''
           ); // NGC
         2: Galaxy.sCon := slBuffer[i];                             // Constellation
         3: Galaxy.sMessier := AnsiReplaceStr(slBuffer[i],'~',' '); // Messier
         4: Galaxy.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');           // Name DE
         5: Galaxy.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');           // Name EN
         6:
         begin
           slMagnitudes.DelimitedText:=sVar;
           j:=0;
           rMagStat:=0;iMagStatCnt:=0;

           while j < slMagnitudes.Count do
           begin
             sMag := slMagnitudes[j];

             case j of
              // 0: VMag
              0: begin if(StrIsNum(true,sMag)) then Galaxy.rM := StrToFloatExt4(sMag); if(Galaxy.rM > 0) and (Galaxy.rM < 999) then begin rMagStat := Galaxy.rM; Inc(iMagStatCnt); end; end;
              1: begin if(StrIsNum(true,sMag)) then Galaxy.rMagU := StrToFloatExt4(sMag); if(Galaxy.rMagU > 0) and (Galaxy.rMagU < 999) then begin rMagStat := rMagStat + Galaxy.rMagU; Inc(iMagStatCnt); end; end;
              2: begin if(StrIsNum(true,sMag)) then Galaxy.rMagB := StrToFloatExt4(sMag); if(Galaxy.rMagB > 0) and (Galaxy.rMagB < 999) then begin rMagStat := rMagStat + Galaxy.rMagB; Inc(iMagStatCnt); end; end;
              3: begin if(StrIsNum(true,sMag)) then Galaxy.rMagR := StrToFloatExt4(sMag); if(Galaxy.rMagR > 0) and (Galaxy.rMagR < 999) then begin rMagStat := rMagStat + Galaxy.rMagR; Inc(iMagStatCnt); end; end;
              4: begin if(StrIsNum(true,sMag)) then Galaxy.rMagG := StrToFloatExt4(sMag); if(Galaxy.rMagG > 0) and (Galaxy.rMagG < 999) then begin rMagStat := rMagStat + Galaxy.rMagG; Inc(iMagStatCnt); end; end;
              5: begin if(StrIsNum(true,sMag)) then Galaxy.rMagI := StrToFloatExt4(sMag); if(Galaxy.rMagI > 0) and (Galaxy.rMagI < 999) then begin rMagStat := rMagStat + Galaxy.rMagI; Inc(iMagStatCnt); end; end;
              6: begin if(StrIsNum(true,sMag)) then Galaxy.rMagJ := StrToFloatExt4(sMag); if(Galaxy.rMagJ > 0) and (Galaxy.rMagJ < 999) then begin rMagStat := rMagStat + Galaxy.rMagJ; Inc(iMagStatCnt); end; end;
              7: begin if(StrIsNum(true,sMag)) then Galaxy.rMagH := StrToFloatExt4(sMag); if(Galaxy.rMagH > 0) and (Galaxy.rMagH < 999) then begin rMagStat := rMagStat + Galaxy.rMagH; Inc(iMagStatCnt); end; end;
              8: begin if(StrIsNum(true,sMag)) then Galaxy.rMagK := StrToFloatExt4(sMag); if(Galaxy.rMagK > 0) and (Galaxy.rMagK < 999) then begin rMagStat := rMagStat + Galaxy.rMagK; Inc(iMagStatCnt); end; end;
              9: begin if(StrIsNum(true,sMag)) then Galaxy.rMagsu := StrToFloatExt4(sMag); if(Galaxy.rMagsu > 0) and (Galaxy.rMagsu < 999) then begin rMagStat := rMagStat + Galaxy.rMagsu; Inc(iMagStatCnt); end; end;
              10: begin if(StrIsNum(true,sMag)) then Galaxy.rMagsg := StrToFloatExt4(sMag); if(Galaxy.rMagsg > 0) and (Galaxy.rMagsg < 999) then begin rMagStat := rMagStat + Galaxy.rMagsg; Inc(iMagStatCnt); end; end;
              11: begin if(StrIsNum(true,sMag)) then Galaxy.rMagsr := StrToFloatExt4(sMag); if(Galaxy.rMagsr > 0) and (Galaxy.rMagsr < 999) then begin rMagStat := rMagStat + Galaxy.rMagsr; Inc(iMagStatCnt); end; end;
              12: begin if(StrIsNum(true,sMag)) then Galaxy.rMagsi := StrToFloatExt4(sMag); if(Galaxy.rMagsi > 0) and (Galaxy.rMagsi < 999) then begin rMagStat := rMagStat + Galaxy.rMagsi; Inc(iMagStatCnt); end; end;
              13: begin if(StrIsNum(true,sMag)) then Galaxy.rMagsz := StrToFloatExt4(sMag); if(Galaxy.rMagsz > 0) and (Galaxy.rMagsz < 999) then begin rMagStat := rMagStat + Galaxy.rMagsz; Inc(iMagStatCnt); end; end;
             end; // case
             Inc(j);
           end; // while

           if(Galaxy.rM <= 0) or (Galaxy.rM >= 999) then
           begin
             if(rMagStat > 0) and (iMagStatCnt > 0) then
             begin
               rMagStat := rMagStat/iMagStatCnt;
               Galaxy.rM := rMagStat;
             end;
           end;

           Galaxy.rMagV := Galaxy.rM;

           //if(StrIsNum(true,sVar)) then Galaxy.rM := StrToFloatExt4(sVar);   // Magnitude
         end;
         7: if(StrIsNum(false,sVar)) then Galaxy.iRA_Hours := StrToInt(sVar); // RA HH
         8:
           if(StrIsNum(true,sVar)) then                                       // RA_MM,SSS
           begin
             Galaxy.iRA_Min := Trunc(StrToFloatExt4(sVar));
             Galaxy.rRA_Sec:= (StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60;
           end;
         9:                                                                   // DEC_DEG
           begin
             if((length(sVar) > 0) and (sVar[1] = '-')) then iDecSign := -1;

             if(StrIsNum(false,sVar)) then Galaxy.iDec_Deg := StrToInt(sVar);
           end;
         10: if(StrIsNum(true,sVar)) then Galaxy.iDec_Min := iDecSign*Trunc(StrToFloatExt4(sVar)); // DEC_MIN
         11: Galaxy.sGType := AnsiReplaceStr(slBuffer[i],'~',' ');                                             // G-Type
         12:
         begin
           if(Pos('#',sVar) > 0) then
           begin
             sVar := AnsiReplaceStr(sVar,'#','');
             if(StrIsNum(true,sVar)) then
               Galaxy.rRedshift := StrToFloatExt4(sVar);
           end
           else if(StrIsNum(true,sVar)) then Galaxy.rDist_XLY := StrToFloatExt4(sVar);    // DistMLY (positive) or redshift (negative)
         end;
         //13: if(StrIsNum(false,sVar)) then Galaxy.iRadSpeed := StrToInt(sVar) else Galaxy.iRadSpeed := -1;  // RadSpeed
         13: if(StrIsNum(true,sVar)) then Galaxy.iRadSpeed := Round(StrToFloatExt4(sVar)) else Galaxy.iRadSpeed := -1;  // RadSpeed
         14: if(StrIsNum(true,sVar)) then Galaxy.rVisDim1 := StrToFloatExt4(sVar) else Galaxy.rVisDim1 := -1;  // Dim 1
         15: if(StrIsNum(true,sVar)) then Galaxy.rVisDim2 := StrToFloatExt4(sVar) else Galaxy.rVisDim2 := -1;  // Dim 2
         16: Galaxy.sAOType := 'G'; //slBuffer[i]; Do not use slBuffer because it may contain an other value if fields a corrupted                                           // 'G'
        end; // case
      end;

      if(Galaxy <> nil) and
      (Galaxy.iRA_Hours + Galaxy.iRA_Min/60.0 + Galaxy.rRA_Sec/3600 <> 0) and // Avoid galaxies with undefined RA/DEC co-ordinates!
      (Galaxy.iDec_Deg + Galaxy.iDec_Min/60.0 + Galaxy.rDec_Sec/3600 <> 0) and
      ((Galaxy.sNGC <> '')) // an ID MUST be given.
      then
      begin
        AOIndexControl(Galaxy,iLineCnt,olAOList.Count);
        olAOList.Add(Galaxy);
      end;

      Inc(iLineCnt);
    end;

    CloseFile(tfAO);

    slMagnitudes.Free;
    slBuffer.Free;

    SetMsg('');
    EndMethod('ImportCatalog_G');
  end;

  procedure ImportCatalog_Q(var olAOList: TObjectList; sLANG_ID: string; sAOFileName: string);
  {2017-02-16/fs
  Import Quasar Catalogue
  }
  var
    iLineCnt,i: Integer;
    slBuffer: TStringList;
    Quasar: TQuasar;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iDecSign: Integer;
    bIsHeader: Boolean;
  begin
    bIsHeader := true;

    BeginMethod('ImportCatalog_Q');

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';
    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-Q.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCnt := 1;
    while not eof(tfAO) do
    begin
      Quasar := nil;
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

      iDecSign := 1;

      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          Quasar := TQuasar.Create;//(P__STARMAP);

          if(Quasar.SHP = nil) then
            Quasar.SHP := TShape.Create(nil);

          Quasar.SHP.Shape:=stCircle;
          Quasar.SHP.Height:=0 ;
          Quasar.SHP.ShowHint := false;

          if(Quasar.L__AO = nil) then
            Quasar.L__AO := TLabel.Create(nil);

          Quasar.L__AO.Visible:=true;

          Quasar.SHP.Pen.Color := clGray;
          Quasar.SHP.Brush.Color := clGray;
          Quasar.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];

        case i of
         0: if(StrIsNum(false,sVar)) then Quasar.sCatNo := sVar;
         1: begin Quasar.sQID := AnsiReplaceStr(slBuffer[i],'~',' '); Quasar.sNGC:= Quasar.sQID; end;
         2: Quasar.sCon := slBuffer[i];
         3: Quasar.sMessier := AnsiReplaceStr(slBuffer[i],'~',' ');
         4: Quasar.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         5: Quasar.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         6: if(StrIsNum(true,sVar)) then Quasar.rM := StrToFloatExt4(sVar);
         7: if(StrIsNum(false,sVar)) then Quasar.iRA_Hours := StrToInt(sVar);
         8:
           if(StrIsNum(true,sVar)) then
           begin
             Quasar.iRA_Min := Trunc(StrToFloatExt4(sVar));
             Quasar.rRA_Sec:= (StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60;
           end;
         9:
           begin
             if((length(sVar) > 0) and (sVar[1] = '-')) then iDecSign := -1;

             if(StrIsNum(false,sVar)) then Quasar.iDec_Deg := StrToInt(sVar);
           end;
         10: if(StrIsNum(true,sVar)) then Quasar.iDec_Min := iDecSign*Trunc(StrToFloatExt4(sVar));
         11: Quasar.sQType := slBuffer[i];
         12: if(StrIsNum(true,sVar)) then Quasar.rDist_XLY := StrToFloatExt4(sVar); // Laufzeitentfernung / Time Travel Distance
         13: if(StrIsNum(true,sVar)) then Quasar.fComDistMPc := StrToFloatExt4(sVar) else Quasar.fComDistMPc := -1; // Mitbewegte Entfernung / Comoving Distance
         14: if(StrIsNum(true,sVar)) then Quasar.fRedshift := StrToFloatExt4(sVar) else Quasar.fRedshift := -1;
         15: Quasar.sAOType := 'Q'; //slBuffer[i]; Do not use slBuffer because it may contain an other value if fields a corrupted slBuffer[i]; // There are other fields added!
        end; // case
      end;

      if(Quasar <> nil) then
      begin
        SetAOLabel(Quasar,sLANG_ID);
        AOIndexControl(Quasar,iLineCnt,olAOList.Count);
        olAOList.Add(Quasar);
      end;

      Inc(iLineCnt);
    end;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCatalog_Q');
  end;

  procedure ImportCatalog_GC(var olAOList: TObjectList; sLANG_ID: string; sAOFileName: string);
  {2012-09-12/fs
  Import Globular Cluster Catalogue
  }
  var
    iLineCnt, i: Integer;
    slBuffer: TStringList;
    GlobularCluster: TGlobularCluster;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iDecSign: Integer;
    bIsHeader: Boolean;
  begin
    bIsHeader := true;
    BeginMethod('ImportCatalog_GC');

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';
    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-GC.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCnt := 1;
    while not eof(tfAO) do
    begin
      GlobularCluster := nil;
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

      iDecSign := 1;
      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          GlobularCluster := TGlobularCluster.Create;//(P__STARMAP);

          if(GlobularCluster.SHP = nil) then
            GlobularCluster.SHP := TShape.Create(nil);

          GlobularCluster.SHP.Shape:=stCircle;
          GlobularCluster.SHP.Height:=0 ;
          GlobularCluster.SHP.ShowHint := false;

          if(GlobularCluster.L__AO = nil) then
            GlobularCluster.L__AO := TLabel.Create(nil);

          GlobularCluster.L__AO.Visible:=true;

          GlobularCluster.SHP.Pen.Color := clAqua;
          GlobularCluster.SHP.Brush.Color := clAqua;
          GlobularCluster.SHP.Cursor := crCross;
        end;
        sVar := slBuffer[i];

        case i of
         0: if(StrIsNum(false,sVar)) then GlobularCluster.sCatNo := sVar;
         1: GlobularCluster.sNGC := slBuffer[i];
         2: GlobularCluster.sCon := slBuffer[i];
         3: GlobularCluster.sMessier := AnsiReplaceStr(slBuffer[i],'~',' ');
         4: GlobularCluster.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         5: GlobularCluster.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         6: if(StrIsNum(true,sVar)) then GlobularCluster.rM := StrToFloatExt4(sVar);
         7: if(StrIsNum(false,sVar)) then GlobularCluster.iRA_Hours := StrToInt(sVar);
         8: //if(StrIsNum(false,sVar)) then GlobularCluster.iRA_Min := StrToInt(sVar);
           if(StrIsNum(true,sVar)) then
           begin
             GlobularCluster.iRA_Min := Trunc(StrToFloatExt4(sVar));
             GlobularCluster.rRA_Sec:= (StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60;
           end;
         9:
           begin
             if((length(sVar) > 0) and (sVar[1] = '-')) then iDecSign := -1;
             if(StrIsNum(false,sVar)) then GlobularCluster.iDec_Deg := StrToInt(sVar);
           end;
         10: if(StrIsNum(true,sVar)) then
           begin
             GlobularCluster.iDec_Min := iDecSign*Trunc(StrToFloatExt4(sVar));
             GlobularCluster.rDec_Sec := iDecSign*(StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60.0;
           end;
         11: if(StrIsNum(false,sVar)) then GlobularCluster.iGCType := StrToInt(slBuffer[i]);
         12: if(StrIsNum(true,sVar)) then GlobularCluster.rDist_XLY := StrToFloatExt4(sVar);
         13: if(StrIsNum(false,sVar)) then GlobularCluster.iRadSpeed := StrToInt(sVar) else GlobularCluster.iRadSpeed := -1;
         14: if(StrIsNum(true,sVar)) then GlobularCluster.rVisDiam := StrToFloatExt4(sVar) else GlobularCluster.rVisDiam := -1;
         15: GlobularCluster.sAOType := 'GC'; //slBuffer[i]; Do not use slBuffer because it may contain an other value if fields a corruptedslBuffer[i];
        end; // case
      end;

      if(GlobularCluster <> nil) then
      begin
        SetAOLabel(GlobularCluster, sLANG_ID);
        GlobularCluster.SetSHPSize();

        AOIndexControl(GlobularCluster,iLineCnt,olAOList.Count);
        olAOList.Add(GlobularCluster);
      end;

      Inc(iLineCnt);
    end;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCatalog_GC');
  end;

  procedure ImportCatalog_PN(var olAOList: TObjectList; sLANG_ID: string; rGLat: Real; sAOFileName: string);
  {2012-10-11/fs
  Import Planetary Nebular Catalogue
  }
  var
    iLineCnt,i: Integer;
    slBuffer: TStringList;
    PNebula: TPNebula;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iDecSign: Integer;
    bIsHeader: Boolean;
  begin
    bIsHeader := true;

    BeginMethod('ImportCatalog_PN');

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';
    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-PN.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCnt := 1;
    while not eof(tfAO) do
    begin
      PNebula := nil;
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

      iDecSign := 1;

      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          PNebula := TPNebula.Create;//(P__STARMAP);

          if(PNebula.SHP = nil) then
            PNebula.SHP := TShape.Create(nil);

          PNebula.SHP.Shape:=stCircle;
          PNebula.SHP.Height:=0 ;
          PNebula.SHP.ShowHint := false;

          if(PNebula.L__AO = nil) then
            PNebula.L__AO := TLabel.Create(nil);

          PNebula.L__AO.Visible:=true;

          PNebula.SHP.Pen.Color := clAqua;
          PNebula.SHP.Brush.Color := clAqua;
          PNebula.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];

        case i of
         0: if(StrIsNum(false,sVar)) then PNebula.sCatNo := sVar;
         1: PNebula.sNGC := slBuffer[i];
         2: PNebula.sCon := slBuffer[i];
         3: PNebula.sMessier := AnsiReplaceStr(slBuffer[i],'~',' ');
         4: PNebula.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         5: PNebula.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         6: if(StrIsNum(false,sVar)) then PNebula.iRA_Hours := StrToInt(sVar);
         7: //if(StrIsNum(false,sVar)) then Nebula.iRA_Min := StrToInt(sVar);
           if(StrIsNum(true,sVar)) then
           begin
             PNebula.iRA_Min := Trunc(StrToFloatExt4(sVar));
             PNebula.rRA_Sec:= (StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60;
           end;
         8:
           begin
             if((length(sVar) > 0) and (sVar[1] = '-')) then iDecSign := -1;
             if(StrIsNum(false,sVar)) then PNebula.iDec_Deg := StrToInt(sVar);
           end;
         9: if(StrIsNum(true,sVar)) then
         begin
           PNebula.iDec_Min := iDecSign*Trunc(StrToFloatExt4(sVar));  //iDecSign*StrToInt(sVar);
           PNebula.rDec_Sec := iDecSign*(StrToFloatExt4(sVar) - abs(PNebula.iDec_Min))*60;
         end;
         10: PNebula.sPNType := slBuffer[i];
         11: if(StrIsNum(true,sVar)) then PNebula.rVisDim1 := StrToFloatExt4(sVar) else PNebula.rVisDim1 := -1;
         12: if(StrIsNum(true,sVar)) then PNebula.rVisDim2 := StrToFloatExt4(sVar) else PNebula.rVisDim1 := -1;
         13: if(StrIsNum(true,sVar)) then PNebula.rM := StrToFloatExt4(sVar);
         14: if(StrIsNum(true,sVar)) then PNebula.rStar_M := StrToFloatExt4(sVar);
         15: if(StrIsNum(true,sVar)) then PNebula.rDist_XLY := StrToFloatExt4(sVar);
         16: if(StrIsNum(false,sVar)) then PNebula.iRadSpeed := StrToInt(sVar) else PNebula.iRadSpeed := -1;
         17: PNebula.sAOType := 'PN'; //slBuffer[i]; Do not use slBuffer because it may contain an other value if fields a corrupted slBuffer[i];
        end; // case
      end;

      if(PNebula <> nil) then
      begin
        // Recalc VisDims - because of mixed arcmins and arcsecs: Anything < 10 is interpreted as arcmins and are multiblied by 60!
        if(PNebula.rVisDim1 > 0) and (PNebula.rVisDim1 < 10) then
          PNebula.rVisDim1 := PNebula.rVisDim1*60;

        if(PNebula.rVisDim2 > 0) and (PNebula.rVisDim2 < 10) then
          PNebula.rVisDim2 := PNebula.rVisDim2*60;

        SetAOLabel(PNebula,sLANG_ID);
        PNebula.SetSHPSize();
        AOIndexControl(PNebula,iLineCnt,olAOList.Count);
        olAOList.Add(PNebula);
      end;

      Inc(iLineCnt);
    end;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCatalog_PN');
  end;

  procedure ImportCatalog_OC(var olAOList: TObjectList; sLANG_ID: string; rGLat: Real; sAOFileName: string);
  {2013-02-16/fs
  Import Open Cluster Catalogue
  }
  var
    iLineCnt,i: Integer;
    slBuffer: TStringList;
    OpenCluster: TOpenCluster;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iDecSign: Integer;
    bIsHeader: Boolean;
  begin
    bIsHeader := true;

    BeginMethod('ImportCatalog_OC');

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';
    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-OC.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCnt := 1;
    while not eof(tfAO) do
    begin
      OpenCluster := nil;
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

      iDecSign := 1;

      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          OpenCluster := TOpenCluster.Create;//(P__STARMAP);

          if(OpenCluster.SHP = nil) then
            OpenCluster.SHP := TShape.Create(nil);

          OpenCluster.SHP.Shape:=stCircle;
          OpenCluster.SHP.Height:=0 ;
          OpenCluster.SHP.ShowHint := false;

          if(OpenCluster.L__AO = nil) then
            OpenCluster.L__AO := TLabel.Create(nil);

          OpenCluster.L__AO.Visible:=true;

          OpenCluster.SHP.Pen.Color := clAqua;
          OpenCluster.SHP.Brush.Color := clAqua;
          OpenCluster.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];

        case i of
         0: if(StrIsNum(false,sVar)) then OpenCluster.sCatNo := sVar;
         1: begin OpenCluster.sNGC := slBuffer[i]; end; //if(slBuffer[i] = 'NGC2358') then ShowMessage('Stop!'); end;
         2: OpenCluster.sCon := slBuffer[i];
         3: OpenCluster.sMessier := AnsiReplaceStr(slBuffer[i],'~',' ');
         4: OpenCluster.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         5: OpenCluster.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         6: if(StrIsNum(false,sVar)) then OpenCluster.iRA_Hours := StrToInt(sVar);
         7: //if(StrIsNum(false,sVar)) then Nebula.iRA_Min := StrToInt(sVar);
           if(StrIsNum(true,sVar)) then
           begin
             OpenCluster.iRA_Min := Trunc(StrToFloatExt4(sVar));
             OpenCluster.rRA_Sec:= (StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60;
           end;
         8:
           begin
             if((length(sVar) > 0) and (sVar[1] = '-')) then iDecSign := -1;
             if(StrIsNum(false,sVar)) then OpenCluster.iDec_Deg := StrToInt(sVar);
           end;
         9: if(StrIsNum(true,sVar)) then
           begin
             OpenCluster.iDec_Min := iDecSign*Trunc(StrToFloatExt4(sVar));
             OpenCluster.rDec_Sec := iDecSign*(StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60.0;
           end;
         10: OpenCluster.sOCType := slBuffer[i];
         11: if(StrIsNum(false,sVar)) then OpenCluster.iDiam_M := StrToInt(sVar) else OpenCluster.iDiam_M := -1;
         12: if(StrIsNum(true,sVar) and (sVar <> '-999')) then OpenCluster.rM := StrToFloatExt4(sVar)
             else
               OpenCluster.rM := -999;
         13: if(StrIsNum(false,sVar)) then OpenCluster.iNum := StrToInt(sVar) else OpenCluster.iNum := -1;
         14: if(StrIsNum(true,sVar)) then OpenCluster.rDist_XLY := StrToFloatExt4(sVar);
         15: if(StrIsNum(false,sVar)) then OpenCluster.rAge := StrToInt(sVar) else OpenCluster.rAge := -1;
         16: OpenCluster.sAOType := 'OC' //slBuffer[i]; Do not use slBuffer because it may contain an other value if fields a corrupted slBuffer[i];
        end; // case
      end;

      if(OpenCluster <> nil) then
      begin
        SetAOLabel(OpenCluster, sLANG_ID);
        OpenCluster.SetSHPSize();

        AOIndexControl(OpenCluster,iLineCnt,olAOList.Count);
        olAOList.Add(OpenCluster);
      end;

      Inc(iLineCnt);
    end;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCatalog_OC');
  end;

  procedure ImportCatalog_N(var olAOList: TObjectList; sLANG_ID: string; sAOFileName: string);
  {2012-10-11/fs
  Import Nebular Catalogue
  }
  var
    iLineCnt,i: Integer;
    slBuffer: TStringList;
    Nebula: TNebula;
    //Sign: TSign;
    tfAO: TextFile;
    sLine, sVar: string;
    iDecSign: Integer;
    bIsHeader: Boolean;
  begin
    bIsHeader := true;

    BeginMethod('ImportCatalog_N');

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';
    // Import astronomical objects catalogue
    //AssignFile(tfAO,ConvertWinPath('AO-N.dat'));
    AssignFile(tfAO,ConvertWinPath(sAOFileName));
    Reset(tfAO);

    iLineCnt := 1;
    while not eof(tfAO) do
    begin
      Nebula := nil;
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

      iDecSign := 1;

      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          Nebula := TNebula.Create;//(P__STARMAP);

          if(Nebula.SHP = nil) then
            Nebula.SHP := TShape.Create(nil);

          Nebula.SHP.Shape:=stCircle;
          Nebula.SHP.Height:=0 ;
          Nebula.SHP.ShowHint := false;

          if(Nebula.L__AO = nil) then
            Nebula.L__AO := TLabel.Create(nil);

          Nebula.L__AO.Visible:=true;

          Nebula.SHP.Pen.Color := clAqua;
          Nebula.SHP.Brush.Color := clAqua;
          Nebula.SHP.Cursor := crCross;
        end;

        sVar := slBuffer[i];

        case i of
         0: if(StrIsNum(false,sVar)) then Nebula.sCatNo := sVar;
         1: Nebula.sNGC := slBuffer[i];
         2: Nebula.sCon := slBuffer[i];
         3: Nebula.sMessier := AnsiReplaceStr(slBuffer[i],'~',' ');
         4: Nebula.sName_DE := AnsiReplaceStr(slBuffer[i],'~',' ');
         5: Nebula.sName_EN := AnsiReplaceStr(slBuffer[i],'~',' ');
         6: if(StrIsNum(false,sVar)) then Nebula.iRA_Hours := StrToInt(sVar);
         7: //if(StrIsNum(false,sVar)) then Nebula.iRA_Min := StrToInt(sVar);
           if(StrIsNum(true,sVar)) then
           begin
             Nebula.iRA_Min := Trunc(StrToFloatExt4(sVar));
             Nebula.rRA_Sec:= (StrToFloatExt4(sVar) - Trunc(StrToFloatExt4(sVar)))*60;
           end;
         8:
           begin
             if((length(sVar) > 0) and (sVar[1] = '-')) then iDecSign := -1;
             if(StrIsNum(false,sVar)) then Nebula.iDec_Deg := StrToInt(sVar);
           end;
         9: if(StrIsNum(true,sVar)) then
           begin
             Nebula.iDec_Min := iDecSign*Trunc(StrToFloatExt4(sVar));
             Nebula.rDec_Sec := (StrToFloatExt4(sVar) - abs(Nebula.iDec_Min))*60;
           end;
         10: Nebula.sNType := slBuffer[i];
         11: if(StrIsNum(false,sVar)) then Nebula.iVisDim1 := StrToInt(sVar) else Nebula.iVisDim1 := -1;
         12: if(StrIsNum(false,sVar)) then Nebula.iVisDim2 := StrToInt(sVar) else Nebula.iVisDim1 := -1;
         13: Nebula.sStar := slBuffer[i];
         14: if(StrIsNum(true,sVar)) then Nebula.rStar_M := StrToFloatExt4(sVar);
         15: if(StrIsNum(true,sVar)) then Nebula.rDist_XLY := StrToFloatExt4(sVar);
         16: if(StrIsNum(true,sVar)) then Nebula.rM := StrToFloatExt4(sVar) else Nebula.rM := -1;
         17: Nebula.sAOType := 'N'; //slBuffer[i]; Do not use slBuffer because it may contain an other value if fields a corrupted slBuffer[i];
        end; // case
      end;

      if(Nebula <> nil) then
      begin
        SetAOLabel(Nebula, sLANG_ID);
        Nebula.SetSHPSize();
        AOIndexControl(Nebula,iLineCnt,olAOList.Count);
        olAOList.Add(Nebula);
      end;

      Inc(iLineCnt);
    end;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCatalog_N');
  end;

  procedure ImportCameraList(var slCameraList: TStringList; sCamFileName: string);
  var
    i: Integer;
    slBuffer: TStringList;
    Camera: TCamera;
    tfAO: TextFile;
    sLine, sVar: string;
    bIsHeader: Boolean;
  begin
    bIsHeader := true;

    if(not FileExists(ConvertWinPath(sCamFileName))) then
      exit;

    BeginMethod('ImportCameraList');

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';
    AssignFile(tfAO,ConvertWinPath(sCamFileName));
    Reset(tfAO);

    while not eof(tfAO) do
    begin
      Camera := nil;
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
          Camera := TCamera.Create;
          Camera.sManufacturer:='';
          Camera.sModel:='';
          Camera.sSensorName:='';
          Camera.iSensorFormatIndex:=-1;
          Camera.sSensorType:='CMOS';
          Camera.fMegaPixel:=0;
          Camera.iFWC:=50000;
          Camera.fDCRC:=10;
          Camera.iQEff:=32;
          Camera.fDCRT:=25;
          Camera.fDCHT:=6.3;
          Camera.iRN:=5;
          Camera.iBits:=12;
          Camera.iSensorYear:=0;
          Camera.iEstimated:=-1; // Userdefined sensor data
        end;

        sVar := Trim(slBuffer[i]);

        case i of
         //0: // Index. Do Nothing
         1: Camera.sManufacturer:= AnsiReplaceStr(slBuffer[i],'~',' ');
         2: Camera.sModel:= AnsiReplaceStr(slBuffer[i],'~',' ');
         3: Camera.sSensorName:= AnsiReplaceStr(slBuffer[i],'~',' ');
         4: if(StrIsNum(false,sVar)) then Camera.iSensorFormatIndex := StrToInt(sVar);
         5:
           if(StrIsNum(true,sVar)) then
           begin
             Camera.fMegaPixel := StrToFloatExt4(sVar);
           end;
         6: if(sVar <> '') then Camera.iQEff := StrToInt(sVar);
         7: if(sVar <> '') then Camera.fDCRC := StrToFloatExt4(sVar);
         8: if(sVar <> '') then Camera.fDCRT := StrToFloatExt4(sVar);
         9: if(sVar <> '') then Camera.fDCHT := StrToFloatExt4(sVar);
         10: if(sVar <> '') then Camera.iFWC := StrToInt(sVar);
         11: if(sVar <> '') then Camera.iRN := StrToInt(sVar);
         12: if(sVar <> '') then Camera.iBits := StrToInt(sVar);
         13: if(sVar <> '') then Camera.iSensorYear := StrToInt(sVar);
         14: if(sVar <> '') then Camera.sSensorType := sVar;
        end; // case
      end;

      if(Camera <> nil) then
      begin
        slCameraList.AddObject(Camera.sManufacturer + '/' + Camera.sModel,Camera);
      end;

    end;

    CloseFile(tfAO);

    SetMsg('');
    EndMethod('ImportCameraList');
  end;

  procedure ImportCBExt(var olCBExt: TObjectList; sAOFileName: string);
  // Import complete links of astronomical objects & constellations
  var
    i: Integer;
    slBuffer: TStringList;
    tfCBExt: TextFile;
    sLine, sVar: string;
    CBExt: TCBExt;
  begin
    BeginMethod('ImportCBExt');

    olCBExt.Clear;

    slBuffer := TStringList.Create;
    slBuffer.Delimiter:=';';
    //AssignFile(tfCBExt,ConvertWinPath('CBExt.dat'));
    AssignFile(tfCBExt,ConvertWinPath(sAOFileName));
    Reset(tfCBExt);

    while not eof(tfCBExt) do
    begin
      CBExt := nil;
      slBuffer.Clear;
      ReadLn(tfCBExt,sLine);
      sLine := AnsiReplaceStr(sLine,' ','~');
      SetMsg(sLine);
      slBuffer.DelimitedText:=sLine;

      for i:=0 to slBuffer.Count-1 do
      begin
        if(i = 0) then
        begin
          CBExt := TCBExt.Create;
        end;

        sVar := slBuffer[i];

        case i of
         0: CBExt.sAOLabel := sVar;
         1: CBExt.sAOType := sVar;
         2: CBExt.sCon1 := sVar;
         3: CBExt.sCon2 := sVar;
        end; // case
      end; // for

      if(CBExt <> nil) then
      begin
        olCBExt.Add(CBExt);
      end;

    end; // while

    CloseFile(tfCBExt);

    SetMsg('');
    EndMethod('ImportCBExt');
  end;

function GetAOLabel(AObject: TAObject; sLANG_ID: string): string;
var
  sLabel: string;
begin
  Result := '';
  sLabel := '';

  if(AObject.sAOType = 'S') and ((AObject as TStar).sSym = 'MARK') then
  begin
    if(sLANG_ID = 'DE') then
      sLabel := AObject.sName_DE
    else
      sLabel := AObject.sName_EN;

  end else if(AObject.sAOType = 'S') then
  begin
    if(((AObject as TStar).sSym <> '') and ((AObject as TInterstellarObject).sCon <> '')) then
      sLabel :=
        (AObject as TStar).sSym + '/' + (AObject as TInterstellarObject).sCon
    else
    begin
      sLabel :=
        (AObject as TStar).sCatNo;

      if((AObject as TInterstellarObject).sCon <> '') then
        sLabel := sLabel + '/' + (AObject as TInterstellarObject).sCon;

    end;
  end else if(AObject.sAOType = 'G') or
    (AObject.sAOType = 'GC') or
    (AObject.sAOType = 'N') or
    (AObject.sAOType = 'PN') or
    (AObject.sAOType = 'OC') then
    sLabel :=
      (AObject as TInterstellarObject).sNGC;

  if(Trim(sLabel) <> '') then
  begin
    if(AObject.sAOType = 'G') and ((AObject as TGalaxy).sMessier <> '') then
      sLabel := sLabel + ' (' + (AObject as TGalaxy).sMessier + ')';

    if(AObject.sAOType = 'GC') and ((AObject as TGlobularCluster).sMessier <> '') then
      sLabel := sLabel + ' (' + (AObject as TGlobularCluster).sMessier + ')';

    if(AObject.sAOType = 'N') and ((AObject as TNebula).sMessier <> '') then
      sLabel := sLabel + ' (' + (AObject as TNebula).sMessier + ')';

    if(AObject.sAOType = 'PN') and ((AObject as TPNebula).sMessier <> '') then
      sLabel := sLabel + ' (' + (AObject as TPNebula).sMessier + ')';

    if(AObject.sAOType = 'OC') and ((AObject as TOpenCluster).sMessier <> '') then
      sLabel := sLabel + ' (' + (AObject as TOpenCluster).sMessier + ')';

    if(sLANG_ID = 'DE') then
    begin
      if(AObject.sName_DE <> '') then
        sLabel := sLabel + ' ' + AObject.sName_DE;
    end;

    if(sLANG_ID = 'EN') then
    begin
      if(AObject.sName_EN <> '') then
        sLabel := sLabel + ' ' + AObject.sName_EN;
    end;
  end
  else
  begin
    if(AObject.sAOType = 'G') and ((AObject as TGalaxy).sMessier <> '') then
      sLabel := 'Messier ' + AnsiReplaceStr((AObject as TGalaxy).sMessier,'M','');

    if(AObject.sAOType = 'GC') and ((AObject as TGlobularCluster).sMessier <> '') then
      sLabel := 'Messier ' + AnsiReplaceStr((AObject as TGlobularCluster).sMessier,'M','');

    if(AObject.sAOType = 'N') and ((AObject as TNebula).sMessier <> '') then
      sLabel := 'Messier ' + AnsiReplaceStr((AObject as TNebula).sMessier,'M','');

    if(AObject.sAOType = 'PN') and ((AObject as TPNebula).sMessier <> '') then
      sLabel := 'Messier ' + AnsiReplaceStr((AObject as TPNebula).sMessier,'M','');

    if(AObject.sAOType = 'OC') and ((AObject as TOpenCluster).sMessier <> '') then
      sLabel := 'Messier ' + AnsiReplaceStr((AObject as TOpenCluster).sMessier,'M','');

    if(AObject.sAOType = 'Q') then
    begin
      if ((AObject as TQuasar).sQID <> '') then
        sLabel := (AObject as TQuasar).sQID
      else
        sLabel := (AObject as TQuasar).sCatNo;
    end;

    if(sLANG_ID = 'DE') then
    begin
      if(AObject.sName_DE <> '') then
        sLabel := AObject.sName_DE;
    end;

    if(sLANG_ID = 'EN') then
    begin
      if(AObject.sName_EN <> '') then
        sLabel := AObject.sName_EN;
    end;
  end;

  Result := sLabel;

end;

procedure SetAOLabel(AObject: TAObject; sLANG_ID: string);
begin
  if(AObject.SHP <> nil) then
    AObject.SHP.Hint:=GetAOLabel(AObject,sLANG_ID);

  if(AObject.IMG <> nil) then
    AObject.IMG.Hint:=GetAOLabel(AObject,sLANG_ID);

end;

procedure AddStaticAO(var olAOList: TObjectList; sLANG_ID: string);
{2015-06-22/fs
Adds static astronomical objects to the astronomicl object list
}
var
  MoonObject: TAObject;
  SunObject: TStar;
begin
  //AObject := TAObject.Create();
  SunObject := TStar.Create();
  SunObject.sName_DE := 'Sonne';
  SunObject.sName_EN := 'Sun';
  SunObject.sAOType := 'SUN';
  SunObject.rM := -26.7;
  //SunObject.sSpecID:='G';
  SunObject.sSpType:='G2V';
  ///SunObject.iSTemp:=5778;

  olAOList.Add(SunObject);

  MoonObject := TAObject.Create();
  MoonObject.sName_DE := 'Mond';
  MoonObject.sName_EN := 'Moon';
  MoonObject.sAOType := 'MOON';
  MoonObject.rM := -12.74;

  olAOList.Add(MoonObject);

end;


end.

