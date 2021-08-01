unit U_HorCust;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  Buttons, StdCtrls, ExtDlgs,
  StrUtils,FileUtil,
  U_AConst, U_ALib;

type

  { TF__HOR_CUST }

  TF__HOR_CUST = class(TForm)
    BB__OK: TBitBtn;
    B__ADD: TButton;
    B__FORWARD: TButton;
    B__REWIND: TButton;
    CBX__HOR_CURR: TCheckBox;
    CB__MONTH: TComboBox;
    CB__MONTH_SEL: TComboBox;
    IMG__HC_EAST: TImage;
    IMG__HC_NORTH: TImage;
    IMG__HC_SOUTH: TImage;
    IMG__HC_WEST: TImage;
    L__CUSTOM_SERIESNO: TLabel;
    L__IMG_EAST: TLabel;
    L__IMG_NORTH: TLabel;
    L__IMG_WEST: TLabel;
    L__MONTH: TLabel;
    MM__INFO: TMemo;
    P__ADD_HOR: TPanel;
    P__BROWSE: TPanel;
    P__HOR_CURR: TPanel;
    P__HOR_SETUP: TPanel;
    P__IMG_WEST: TPanel;
    P__IMG_EAST: TPanel;
    P__IMG_SOUTH: TPanel;
    P__IMG_NORTH: TPanel;
    L__IMG_SOUTH: TLabel;
    OPICDLG: TOpenPictureDialog;
    P__INFO: TPanel;
    P__INFO1: TPanel;
    P__WORK: TPanel;
    RB__EAST: TRadioButton;
    RB__NORTH: TRadioButton;
    RB__SOUTH: TRadioButton;
    RB__WEST: TRadioButton;
    SHP_EAST: TShape;
    SHP_SOUTH: TShape;
    SHP_WEST: TShape;
    SHP_NORTH: TShape;
    SHP__DIR1: TShape;
    procedure B__ADDClick(Sender: TObject);
    procedure B__FORWARDClick(Sender: TObject);
    procedure B__REWINDClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    miHCImageFileSel: Integer;

    procedure AddCustHorPic();
    procedure ReadHCImgFilesFromDir();
    procedure ShowHCImg(iHCINo, iMonthNo: Integer);
    procedure ShowCustomSeriesNo();

  public
    msLANG_ID: string;
    mslHCImageFiles: TStringList;

  end;

var
  F__HOR_CUST: TF__HOR_CUST;

implementation

{$R *.lfm}

procedure TF__HOR_CUST.ShowCustomSeriesNo();
begin
  if (msLANG_ID = 'DE') then
    L__CUSTOM_SERIESNO.Caption := 'Serie Nr.: ' + IntToStr(miHCImageFileSel) + '/' + IntToStr(mslHCImageFiles.Count)
  else
    L__CUSTOM_SERIESNO.Caption := 'Series No.: ' + IntToStr(miHCImageFileSel) + '/' + IntToStr(mslHCImageFiles.Count);

  B__REWIND.Enabled := (miHCImageFileSel > 1);
  B__REWIND.Enabled := (miHCImageFileSel < mslHCImageFiles.Count);
  BB__OK.Enabled := (miHCImageFileSel > 0);
end;

procedure TF__HOR_CUST.ShowHCImg(iHCINo, iMonthNo: Integer);
var
  sImgFile, sDirect: string;
  i,iNo: Integer;
begin
  if(iHCINo < 1) or (mslHCImageFiles.Count < iHCINo) then
    exit;

  miHCImageFileSel := iHCINo;

  iNo := ciLandscapeNoCust -1 + iHCINo;

  ShowCustomSeriesNo();

  for i:=0 to 3 do
  begin
    sImgFile := '';

    case i of
      0: sDirect := 'S';
      1: sDirect := 'N';
      2: sDirect := 'O';
      3: sDirect := 'W'
      else
        sDirect := 'S';
    end; // case

    // Find month file
    if(iMonthNo > 0) and (iMonthNo < 13) then
      sImgFile := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(iNo) + '_' + sDirect + '_' + IntToStr(iMonthNo) + '_DAY.png';

    if(sImgFile = '') or (not FileExists(sImgFile)) then
      iMonthNo := 0;

    // Find default file
    if (iMonthNo < 1) or (iMonthNo > 12) then
      sImgFile := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(iNo) + '_' + sDirect + '_DAY.png';

    if FileExists(sImgFile) then
    case i of
      0: IMG__HC_SOUTH.Picture.LoadFromFile(sImgFile);
      1: IMG__HC_NORTH.Picture.LoadFromFile(sImgFile);
      2: IMG__HC_EAST.Picture.LoadFromFile(sImgFile);
      3: IMG__HC_WEST.Picture.LoadFromFile(sImgFile);
    end; // case

  end;

  if(iMonthNo > -1) and (iMonthNo < CB__MONTH_SEL.Items.Count) then
    CB__MONTH_SEL.ItemIndex := iMonthNo;

end;

procedure TF__HOR_CUST.ReadHCImgFilesFromDir();
var
  slBuffer, slFileStruct: TStringList;
  i, iIndex, iIndexPrev: Integer;
  sFileName: string;
begin
  slBuffer := TStringList.Create;
  slBuffer.Clear;

  slFileStruct := TStringList.Create;
  slFileStruct.Delimiter:='_';
  slFileStruct.StrictDelimiter:=true;

  mslHCImageFiles.Clear;

  ReadFilesFromDir(slBuffer,gsAlbireoLocalDir + csLandscapeRelDir,'.png');
  iIndex := 0; iIndexPrev := 0;
  // Remove 0,1,2-Landscapes from the list (are predefinded)
  for i:=0 to slBuffer.Count-1 do
  begin
    sFileName := slBuffer[i];
    if(
      (not AnsiContainsStr(sFileName,'Landscape_0')) and
      (not AnsiContainsStr(sFileName,'Landscape_1')) and
      (not AnsiContainsStr(sFileName,'Landscape_2'))
      ) then
    begin
      slFileStruct.Clear;
      slFileStruct.DelimitedText:=sFileName;

      iIndex := StrToInt(slFileStruct[1]);
      if(iIndexPrev < iIndex) then
        mslHCImageFiles.Add(sFileName);

      iIndexPrev := iIndex;

    end;

  end;

  if(iIndex > 0) then
  begin
    miHCImageFileSel := 1;
    CBX__HOR_CURR.Visible := true;
    CBX__HOR_CURR.Checked := false;
    BB__OK.Enabled := true;
  end;

  slFileStruct.Destroy;
  slBuffer.Destroy;

  ShowHCImg(1,GetMonth(Now));

  ShowCustomSeriesNo();
end;

procedure TF__HOR_CUST.B__ADDClick(Sender: TObject);
begin
  AddCustHorPic();
end;

procedure TF__HOR_CUST.B__FORWARDClick(Sender: TObject);
begin
    ShowHCImg(miHCImageFileSel+1,CB__MONTH_SEL.ItemIndex);
end;

procedure TF__HOR_CUST.B__REWINDClick(Sender: TObject);
begin
  ShowHCImg(miHCImageFileSel-1,CB__MONTH_SEL.ItemIndex);
end;

procedure TF__HOR_CUST.FormCreate(Sender: TObject);
begin
  mslHCImageFiles := TStringList.Create;
  miHCImageFileSel := 0;

  ReadHCImgFilesFromDir();
end;

procedure TF__HOR_CUST.FormDestroy(Sender: TObject);
begin
  mslHCImageFiles.Free;
end;

procedure TF__HOR_CUST.FormShow(Sender: TObject);
var
  i: Integer;
begin
  CB__MONTH.Clear;
  MM__INFO.Clear;

  if(msLANG_ID = 'DE') then
  begin
    Caption := 'Benutzerdefinierte Horizonte';

    CB__MONTH.Items.Add('Alle Monate');
    CB__MONTH.Items.Add('1 - Januar');
    CB__MONTH.Items.Add('2 - Februar');
    CB__MONTH.Items.Add('3 - März');
    CB__MONTH.Items.Add('4 - April');
    CB__MONTH.Items.Add('5 - Mai');
    CB__MONTH.Items.Add('6 - Juni');
    CB__MONTH.Items.Add('7 - Juli');
    CB__MONTH.Items.Add('8 - August');
    CB__MONTH.Items.Add('9 - September');
    CB__MONTH.Items.Add('10 - Oktober');
    CB__MONTH.Items.Add('11 - November');
    CB__MONTH.Items.Add('12 - Dezember');

    // Set info box
    MM__INFO.Lines.Add('- PNG-Bildformat erforderlich');
    MM__INFO.Lines.Add('- Seitenverhältnis 4:1 - 6:1');
    MM__INFO.Lines.Add('- Horizont sollte transparent sein');
    MM__INFO.Lines.Add('- PNG-Datei kleiner als 1.5 MB');

    CBX__HOR_CURR.Caption:= 'Ausgewählten Horizont ersetzen';
  end
  else
  begin
    Caption := 'User Defined Horizon Views';

    CB__MONTH.Items.Add('Any Months');
    CB__MONTH.Items.Add('1 - January');
    CB__MONTH.Items.Add('2 - February');
    CB__MONTH.Items.Add('3 - March');
    CB__MONTH.Items.Add('4 - April');
    CB__MONTH.Items.Add('5 - May');
    CB__MONTH.Items.Add('6 - June');
    CB__MONTH.Items.Add('7 - July');
    CB__MONTH.Items.Add('8 - August');
    CB__MONTH.Items.Add('9 - September');
    CB__MONTH.Items.Add('10 - October');
    CB__MONTH.Items.Add('11 - November');
    CB__MONTH.Items.Add('12 - December');

    // Set info box
    MM__INFO.Lines.Add('- PNG image format required');
    MM__INFO.Lines.Add('- Aspect ratio 4:1 - 6:1');
    MM__INFO.Lines.Add('- Horizon should be transparent');
    MM__INFO.Lines.Add('- PNG file smaller than 1.5 MB');

    CBX__HOR_CURR.Caption:= 'Replace selected horizon image';
  end;
  CB__MONTH.Text:= CB__MONTH.Items[0];

  // Copy to month selection box
  CB__MONTH_SEL.Clear;
  for i:=0 to CB__MONTH.Items.Count-1 do
  begin
    CB__MONTH_SEL.Items.Add(CB__MONTH.Items[i]);
  end;
  CB__MONTH_SEL.Text:= CB__MONTH_SEL.Items[0];

end;

procedure TF__HOR_CUST.AddCustHorPic();
var
  sDestFile, sHorDir: string;
begin
  if((OPICDLG.Execute) and (OPICDLG.FileName <> '') and (FileExists(OPICDLG.FileName)) ) then
  begin
    if(RB__NORTH.Checked) then
      sHorDir := 'N'
    else if(RB__EAST.Checked) then
      sHorDir := 'O'
    else if(RB__WEST.Checked) then
      sHorDir := 'W'
    else
      sHorDir := 'S';

    if(CBX__HOR_CURR.Visible) and (CBX__HOR_CURR.Checked) then
    begin
      if(CB__MONTH.ItemIndex > 0) then
        sDestFile := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(ciLandscapeNoCust + mslHCImageFiles.Count-1) + '_' + sHorDir + '_' + IntToStr(CB__MONTH.ItemIndex) + '_DAY.png'
      else
        sDestFile := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(ciLandscapeNoCust + mslHCImageFiles.Count-1) + '_' + sHorDir + '_DAY.png';
    end
    else
    begin
      if(CB__MONTH.ItemIndex > 0) then
        sDestFile := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(ciLandscapeNoCust + mslHCImageFiles.Count) + '_' + sHorDir + '_' + IntToStr(CB__MONTH.ItemIndex) + '_DAY.png'
      else
        sDestFile := gsAlbireoLocalDir + csLandscapeRelDir + 'Landscape_' + IntToStr(ciLandscapeNoCust + mslHCImageFiles.Count) + '_' + sHorDir + '_DAY.png';

      mslHCImageFiles.Add(sDestFile);

      CBX__HOR_CURR.Visible := true;
      CBX__HOR_CURR.Checked := false;

    end;

    FileUtil.CopyFile(OPICDLG.FileName,sDestFile);

    if(RB__NORTH.Checked) then
      IMG__HC_NORTH.Picture.LoadFromFile(sDestFile)
    else if(RB__EAST.Checked) then
      IMG__HC_EAST.Picture.LoadFromFile(sDestFile)
    else if(RB__WEST.Checked) then
      IMG__HC_WEST.Picture.LoadFromFile(sDestFile)
    else
      IMG__HC_SOUTH.Picture.LoadFromFile(sDestFile);

  end;

end;

end.

