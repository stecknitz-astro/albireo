unit U_Devices; 
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
  ExtCtrls, ComCtrls, ExtDlgs, ContNrs, U_AConst;

type

  { TF__DEVICES }

  TF__DEVICES = class(TForm)
    B__ADEV_PHOTO_DEL: TButton;
    B__ADEV_PHOTO_ADD: TButton;
    B__ADEV_DEL: TButton;
    B__ADEV_MOD: TButton;
    B__ADEV_ADD: TButton;
    CB__ADEV_NAME: TComboBox;
    ED__ADEV_ARTNO: TEdit;
    ED__ADEV_DESCR: TEdit;
    ED__ADEV_MANU: TEdit;
    GBX__ADEV_PROP: TGroupBox;
    GBX__ADEV_PHOTO: TGroupBox;
    IMG__ADEV_PHOTO: TImage;
    L__ADEV_ARTNO: TLabel;
    L__ADEV_DESCR: TLabel;
    L__ADEV_MANU: TLabel;
    L__ADEV_NAME: TLabel;
    ODLG__IMAGE: TOpenPictureDialog;
    P__ADEV_PHOTO_BUTTONS: TPanel;
    P__ADEV_GENERAL: TPanel;
    PC__ADEV: TPageControl;
    P__TOP: TPanel;
    P__BUTTON: TPanel;
    TS__DETAIL: TTabSheet;
    TA__ADEV_GENERAL: TTabSheet;
    procedure B__ADEV_ADDClick(Sender: TObject);
    procedure B__ADEV_DELClick(Sender: TObject);
    procedure B__ADEV_MODClick(Sender: TObject);
    procedure CB__ADEV_NAMEChange(Sender: TObject);
  private
    { private declarations }
    procedure AddADev();
    procedure ModADev();
    function GetADev(sName: string; var iIndex: Integer): TADevice;
    procedure ShowADev(ADevice: TADevice);
    procedure DelADev();

  public
    { public declarations }
    molADevList: TObjectList;
    msLANG_ID: string;
  end; 

var
  F__DEVICES: TF__DEVICES;

implementation

{$R *.lfm}

{ TF__DEVICES }

procedure TF__DEVICES.DelADev();
var
  sMsg: string;
  iIndex, iSelIndex: Integer;
begin
  if(msLANG_ID = 'DE') then
    sMsg := 'Möchten Sie diese Geräteinformation löschen?'
  else
    sMsg := 'Do you really want to delete this device information?';

  iIndex := -1;

  if(MessageDlg(sMsg,mtConfirmation,[mbYes,mbNo],0) = mrNo) then exit;

  GetADev(CB__ADEV_NAME.Text,iIndex);

  if(iIndex > -1) then
  begin
    molADevList.Delete(iIndex);
    ED__ADEV_DESCR.Text := '';
    ED__ADEV_MANU.Text := '';
    ED__ADEV_ARTNO.Text := '';

    iSelIndex := CB__ADEV_NAME.ItemIndex;
    if(iSelIndex > -1) then
      CB__ADEV_NAME.Items.Delete(iSelIndex);
  end;

end;

procedure TF__DEVICES.ShowADev(ADevice: TADevice);
{2013/04/16 / fs
Show Astronomical Device
}
begin
  if(ADevice = nil) then exit;

  ED__ADEV_DESCR.Text := ADevice.sDescription;
  ED__ADEV_MANU.Text := ADevice.sManufacturer;
  ED__ADEV_ARTNO.Text := ADevice.sArtNoManu;

end;

function TF__DEVICES.GetADev(sName: string; var iIndex: Integer): TADevice;
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

procedure TF__DEVICES.ModADev();
{2013/04/16 / fs
Modify Astronomical Device
}
var
  iIndex: Integer;
begin
  iIndex := -1;

  //ADevice := GetADev(CB__ADEV_NAME.Text,iIndex);
  GetADev(CB__ADEV_NAME.Text,iIndex);

  if(iIndex < 0) then exit;

  (molADevList[iIndex] as TADevice).sDescription := ED__ADEV_DESCR.Text;
  (molADevList[iIndex] as TADevice).sManufacturer := ED__ADEV_MANU.Text;
  (molADevList[iIndex] as TADevice).sArtNoManu := ED__ADEV_ARTNO.Text;

end;

procedure TF__DEVICES.B__ADEV_DELClick(Sender: TObject);
begin
  DelADev();
end;

procedure TF__DEVICES.B__ADEV_MODClick(Sender: TObject);
begin
  ModADev();
end;

procedure TF__DEVICES.B__ADEV_ADDClick(Sender: TObject);
begin
  AddADev();
end;

procedure TF__DEVICES.CB__ADEV_NAMEChange(Sender: TObject);
var
  sName: string;
  iIndex: Integer;
  ADevice: TADevice;
begin
  sName := Trim(CB__ADEV_NAME.Text);
  iIndex := -1;

  ADevice := GetADev(sName, iIndex);

  if(ADevice <> nil) then
    ShowADev(ADevice);

end;

procedure TF__DEVICES.AddADev();
{April 2013/fs
Add Astronomical Device
}
var
  ADevice: TADevice;
  sName, sMsg: string;
begin
  if(msLANG_ID = 'DE') then
    sMsg := 'Neues Gerät erfassen'
  else
    sMsg := 'Assign new device';

  sName := '';

  if((not InputQuery(sMsg,'Name:',sName)) or (Trim(sName) = '')) then
    exit;

  ADevice := TADevice.Create;

  ADevice.sName := sName;

  ADevice.sDescription := ED__ADEV_DESCR.Text;
  ADevice.sManufacturer := ED__ADEV_MANU.Text;
  ADevice.sArtNoManu := ED__ADEV_ARTNO.Text;

  molADevList.Add(ADevice);

  CB__ADEV_NAME.Items.Add(sName);

  if(msLANG_ID = 'DE') then
    sMsg := 'Geräteinformationen angelegt.';

  if(msLANG_ID = 'EN') then
    sMsg := 'Device information added.';

  MessageDlg(sMsg,mtInformation,[mbOK],0);

end;


end.

