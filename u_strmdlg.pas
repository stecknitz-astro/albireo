unit U_StrmDlg;

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Buttons, ExtCtrls;

type

  { TF__STREAMING }

  TF__STREAMING = class(TForm)
    BB__CANCEL: TBitBtn;
    BB__OK: TBitBtn;
    B__STRM_DATADIR: TButton;
    ED__STRM_DATADIR: TEdit;
    ED__TEXT_1_DE: TEdit;
    ED__TEXT_1_EN: TEdit;
    ED__TEXT_2_DE: TEdit;
    ED__TEXT_2_EN: TEdit;
    ED__TEXT_3_DE: TEdit;
    ED__TEXT_3_EN: TEdit;
    L__STRM_DATATDIR: TLabel;
    L__STRM_PERIOD: TLabel;
    L__STRM_PERIOD_MS_TITLE: TLabel;
    L__STRM_VIEWFLIP: TLabel;
    L__STRM_VIEWFLIP_TITLE: TLabel;
    L__TEXT_1_DE: TLabel;
    L__TEXT_1_EN: TLabel;
    L__TEXT_2_DE: TLabel;
    L__TEXT_2_EN: TLabel;
    L__TEXT_3_DE: TLabel;
    L__TEXT_3_EN: TLabel;
    P__VIEWFLIP_TITLE: TPanel;
    P__VIEWFLIP: TPanel;
    P__STRM_DATAFILES: TPanel;
    P__STRM_DATAFILES_TITLE: TPanel;
    SDDLG: TSelectDirectoryDialog;
    TB__STRM_PERIOD: TTrackBar;
    TB__STRM_VIEWFLIP: TTrackBar;
    procedure B__STRM_DATADIRClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TB__STRM_PERIODChange(Sender: TObject);
    procedure TB__STRM_VIEWFLIPChange(Sender: TObject);
  private
    procedure StreamingDataDirSelected();
    procedure TrackBarDataChanged();
    procedure TrackBarViewFlipChanged();

  public
    msLANG_ID: string;

  end;

var
  F__STREAMING: TF__STREAMING;

implementation

{$R *.lfm}

{ TF__STREAMING }

procedure TF__STREAMING.TrackBarDataChanged();
begin
  L__STRM_PERIOD.Caption := IntToStr(Trunc(TB__STRM_PERIOD.Position / 60)) + ' Min : ' +
    IntToStr(Trunc(TB__STRM_PERIOD.Position - 60*Trunc(TB__STRM_PERIOD.Position / 60))) + ' s';
end;

procedure TF__STREAMING.TrackBarViewFlipChanged();
begin
  L__STRM_VIEWFLIP.Caption := IntToStr(Trunc(TB__STRM_VIEWFLIP.Position / 60)) + ' Min : ' +
    IntToStr(Trunc(TB__STRM_VIEWFLIP.Position - 60*Trunc(TB__STRM_VIEWFLIP.Position / 60))) + ' s';
end;

procedure TF__STREAMING.StreamingDataDirSelected();
begin
  if(SDDLG.Execute and (SDDLG.FileName <> '') and (DirectoryExists(SDDLG.FileName))) then
  begin
    ED__STRM_DATADIR.Text := SDDLG.FileName;
  end;
end;

procedure TF__STREAMING.B__STRM_DATADIRClick(Sender: TObject);
begin
  StreamingDataDirSelected();
end;

procedure TF__STREAMING.FormShow(Sender: TObject);
begin
  TrackBarDataChanged();
  TrackBarViewFlipChanged();
end;

procedure TF__STREAMING.TB__STRM_PERIODChange(Sender: TObject);
begin
    TrackBarDataChanged();
end;

procedure TF__STREAMING.TB__STRM_VIEWFLIPChange(Sender: TObject);
begin
  TrackBarViewFlipChanged();
end;

end.

