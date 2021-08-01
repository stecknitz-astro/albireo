unit U_NewCam;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

type

  { TF__NEWCAM }

  TF__NEWCAM = class(TForm)
    BT__CANCEL: TBitBtn;
    BT__OK: TBitBtn;
    CB__CAM_SENSOR: TComboBox;
    ED__SENSORYEAR: TEdit;
    ED__SENSORNAME: TEdit;
    ED__BITS: TEdit;
    ED__RN: TEdit;
    ED__DCRC: TEdit;
    ED__FWC: TEdit;
    ED__DCRT: TEdit;
    ED__DCHT: TEdit;
    ED__QEFF: TEdit;
    ED__CAM_MP_DEC: TEdit;
    ED__MP_INT: TEdit;
    ED__CAM_MODEL: TEdit;
    ED__CAM_MANU: TEdit;
    GBX__SENSOR: TGroupBox;
    GBX__SENSOR_VALID: TGroupBox;
    L__PUBYEAR_SENSOR: TLabel;
    L__SENSORNAME: TLabel;
    L__BITS: TLabel;
    L__RN: TLabel;
    L__DCRC: TLabel;
    L__FWC: TLabel;
    L__DCRT: TLabel;
    L__DCHT: TLabel;
    L__QEFF: TLabel;
    L__CAM_MP_DOT: TLabel;
    L__CAM_MP: TLabel;
    L__CAM_MODEL: TLabel;
    L__CAM_MANU: TLabel;
    L__SENSOR: TLabel;
    L__SENSOR1: TLabel;
    RB__SV_S: TRadioButton;
    RB__SV_E: TRadioButton;
    RB__SV_U: TRadioButton;
    RB__CCD: TRadioButton;
    RB__CMOS: TRadioButton;
    procedure CB__CAM_SENSORChange(Sender: TObject);
    procedure ED__BITSChange(Sender: TObject);
    procedure ED__CAM_MANUChange(Sender: TObject);
    procedure ED__CAM_MODELChange(Sender: TObject);
    procedure ED__CAM_MP_DECChange(Sender: TObject);
    procedure ED__DCHTChange(Sender: TObject);
    procedure ED__DCRCChange(Sender: TObject);
    procedure ED__DCRTChange(Sender: TObject);
    procedure ED__FWCChange(Sender: TObject);
    procedure ED__MP_INTChange(Sender: TObject);
    procedure ED__QEFFChange(Sender: TObject);
    procedure ED__RNChange(Sender: TObject);
    procedure ED__SENSORNAMEChange(Sender: TObject);
    procedure ED__SENSORYEARChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RB__CCDChange(Sender: TObject);
    procedure RB__CMOSChange(Sender: TObject);
  private
    procedure HasChanged();

  public
    msLANG_ID: string;
    mbModifiedCamera: Boolean;
  end;

var
  F__NEWCAM: TF__NEWCAM;

implementation

{$R *.lfm}

{ TF__NEWCAM }

procedure TF__NEWCAM.HasChanged();
begin
  if not mbModifiedCamera then
    exit;

  BT__OK.Visible:=true;
  BT__CANCEL.Visible:=true;
end;

procedure TF__NEWCAM.RB__CMOSChange(Sender: TObject);
begin
  if(RB__CMOS.Checked) then
  begin
    ED__QEFF.Text := '32';
    ED__RN.Text := '5';
    HasChanged();
  end;
end;

procedure TF__NEWCAM.RB__CCDChange(Sender: TObject);
begin
  if(RB__CCD.Checked) then
  begin
    ED__QEFF.Text := '56';
    ED__RN.Text := '15';
    HasChanged();
  end;
end;

procedure TF__NEWCAM.ED__CAM_MANUChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.CB__CAM_SENSORChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__BITSChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__CAM_MODELChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__CAM_MP_DECChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__DCHTChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__DCRCChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__DCRTChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__FWCChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__MP_INTChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__QEFFChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__RNChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__SENSORNAMEChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.ED__SENSORYEARChange(Sender: TObject);
begin
  HasChanged();
end;

procedure TF__NEWCAM.FormCreate(Sender: TObject);
begin
  mbModifiedCamera := false;
end;

end.

