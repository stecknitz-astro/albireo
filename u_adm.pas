unit U_ADM;

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
  StdCtrls, StrUtils, EditBtn, Spin, ContNrs,
  U_AConst, U_ALib, U_AlbireoLib;

type

  { TF__ADM }

  TF__ADM = class(TForm)
    B__LINEDELETE: TButton;
    B__PROCESS: TButton;
    B__CALC_ARCSEC: TButton;
    B__ADDTEMP: TButton;
    B__ADDTOLIB: TButton;
    B__CALC_OMEGA: TButton;
    B__CALC_LEPOCH: TButton;
    CB__ASTEROID: TComboBox;
    ED__DEL_STRING: TEdit;
    ED__CALC_LE_DATE: TDateEdit;
    ED__CALC_LE_DEC_MM: TLabeledEdit;
    ED__CALC_LE_DEC_SS: TLabeledEdit;
    ED__CALC_LE_RA_SS: TLabeledEdit;
    ED__CALC_LE_RA_MM: TLabeledEdit;
    ED__ECCENTRICITY: TLabeledEdit;
    ED__INCLINATION: TLabeledEdit;
    ED__NAME_DE: TLabeledEdit;
    ED__NAME_EN: TLabeledEdit;
    ED__SMAXIS: TLabeledEdit;
    ED__OPERIOD: TLabeledEdit;
    ED__APERI: TLabeledEdit;
    ED__LAN: TLabeledEdit;
    ED__OMEGA_Q: TLabeledEdit;
    ED__LEPOCH: TLabeledEdit;
    GBX__CALC_LEPOCH: TGroupBox;
    ED__CALC_LE_RA_HH: TLabeledEdit;
    ED__CALC_LE_DEC_DEG: TLabeledEdit;
    ED__MASS: TLabeledEdit;
    ED__DIAMETER: TLabeledEdit;
    ED__MAG: TLabeledEdit;
    ED__MAXITER: TLabeledEdit;
    ED__EPSILON: TLabeledEdit;
    ED__VDIAM_AS: TLabeledEdit;
    GBX__LINEDELETE: TGroupBox;
    L__IGNOREPOS: TLabel;
    L__LD_TOTAL: TLabel;
    L__LD_CNT: TLabel;
    L__LD_TOTAL_TITLE: TLabel;
    L__LD_CNT_TITLE: TLabel;
    L__TOTAL_CNT: TLabel;
    L__TOTAL_CNT_TITLE: TLabel;
    L__ADDED: TLabel;
    L__SKIPPED: TLabel;
    L__EXCLUDED_CNT_TITLE: TLabel;
    L__PROCESSED_CNT: TLabel;
    L__EXCLUDED_CNT: TLabel;
    L__PROCESSED_CNT_TITLE: TLabel;
    L__ASTEROID: TLabel;
    L__CALC_LE_DATE: TLabel;
    L__APERI: TBoundLabel;
    L__CALC_LE_DEC: TBoundLabel;
    L__CALC_LE_RA: TBoundLabel;
    L__CALC_LE_TIME: TLabel;
    L__DIAMETER: TBoundLabel;
    L__ECCENTRICITY: TBoundLabel;
    L__EPSILON: TBoundLabel;
    L__INCLINATION: TBoundLabel;
    L__LAN: TBoundLabel;
    L__LEPOCH: TBoundLabel;
    L__MAG: TBoundLabel;
    L__MASS: TBoundLabel;
    L__MAXITER: TBoundLabel;
    L__NAME_DE: TBoundLabel;
    L__NAME_EN: TBoundLabel;
    L__OMEGA: TBoundLabel;
    L__OPERIOD: TBoundLabel;
    L__SMAXIS: TBoundLabel;
    L__VDIAM_AS: TBoundLabel;
    MM__SKIPPED: TMemo;
    MM__SIMBARDCONF_S: TMemo;
    MM__INFO: TMemo;
    MM__SIMBARDCONF_G: TMemo;
    MM__ADDED: TMemo;
    ODLG: TOpenDialog;
    P__BUTTONS: TPanel;
    PC__ADM: TPageControl;
    ED__CALC_LE_TIME: TTimeEdit;
    RB__GEN_DS_STARS: TRadioButton;
    RB__GEN_DS_GALAXIES: TRadioButton;
    SDLG: TSaveDialog;
    ED__IGNOREPOS: TSpinEdit;
    TS__LIBMOD: TTabSheet;
    TS__SIMBAD_IMP: TTabSheet;
    TS__PLANETS: TTabSheet;
    procedure B__ADDTEMPClick(Sender: TObject);
    procedure B__ADDTOLIBClick(Sender: TObject);
    procedure B__CALC_ARCSECClick(Sender: TObject);
    procedure B__CALC_LEPOCHClick(Sender: TObject);
    procedure B__CALC_OMEGAClick(Sender: TObject);
    procedure B__LINEDELETEClick(Sender: TObject);
    procedure B__PROCESSClick(Sender: TObject);
    procedure CB__ASTEROIDChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function ProcessSimbadStarFile(sInFilename, sOutFilename: string): Boolean;
    function ProcessSimbadGalaxyFile(sInFilename, sOutFilename: string): Boolean;
    function CheckOnUniqueAO(sAOType,sID1,sID2: string): Boolean;
    procedure Filemod_DelLine(sExpression: string; iPos: Integer);

  public
    miDST_HH,miUTC_HH: Integer;
    molAOList: TObjectList;
  end;

var
  F__ADM: TF__ADM;

implementation

{$R *.lfm}

{ TF__ADM }

procedure TF__ADM.Filemod_DelLine(sExpression: string; iPos: Integer);
var
  tfInFile, tfOutFile: TextFile;
  sInFile, sOutFile: string;
  iInCnt, iDelCnt: Integer;
  sInLine: string;
begin
  if(ODLG.Execute and FileExists(ODLG.FileName)) then
  begin
    sInFile := ODLG.FileName;
    if(SDLG.Execute and (SDLG.FileName <> '')) then
    begin
      sOutFile := SDLG.FileName;

      AssignFile(tfInFile,sInFile);
      AssignFile(tfOutFile,sOutFile);

      // Generate Albireo compatible star dataset from Simbad data file
      Reset(tfInFile);
      ReWrite(tfOutFile);

      iInCnt:=0; iDelCnt:=0;
      while (not eof(tfInFile)) do
      begin
        Inc(iInCnt);
        L__LD_TOTAL.Caption := IntToStr(iInCnt); L__LD_TOTAL.Refresh;

        ReadLn(tfInFile,sInLine);

        if(not AnsiContainsStr(sInLine,sExpression)) or (Pos(sExpression,sInLine) >= iPos) then
          WriteLn(tfOutFile,sInLine)
        else
        begin
          Inc(iDelCnt);
          L__LD_CNT.Caption := IntToStr(iDelCnt); L__LD_CNT.Refresh;
        end;

      end;

      CloseFile(tfOutFile);
      CloseFile(tfInFile);

    end;
  end;
end;

function TF__ADM.CheckOnUniqueAO(sAOType,sID1,sID2: string): Boolean;
// 13.08.2020/fs
// Check, if astronomical object is already in database.
// Restricted to interstellar Objects
var
  i, iStarCnt: Integer;
  sID_CMP, sID2_CMP: string;
  sCAT_NO_DB, sNGC_DB, sMessier_DB: string;
  bDataChunk, bFound: Boolean;
begin
  sID_CMP := Uppercase(AnsiReplaceStr(Trim(sID1),' ',''));
  sID2_CMP := Uppercase(AnsiReplaceStr(Trim(sID2),' ',''));

  bFound := (sID_CMP = '');

  bDataChunk := false;
  i:=0; iStarCnt := 0;

  while (i < molAOList.Count) and (not bFound)  do
  begin
    if((molAOList[i] as TAObject).sAOType = sAOType) then
    begin
      bDataChunk := true;

      if(sAOType = 'G') then
      begin
        sCAT_NO_DB := Uppercase(AnsiReplaceStr(Trim((molAOList[i] as TInterstellarObject).sCatNo),' ',''));
        sNGC_DB := Uppercase(AnsiReplaceStr(Trim((molAOList[i] as TInterstellarObject).sNGC),' ',''));
        sMessier_DB := Uppercase(AnsiReplaceStr(Trim((molAOList[i] as TInterstellarObject).sMessier),' ',''));

        bFound :=
          ((sNGC_DB <> '') and (sID_CMP = sNGC_DB)) or
          ((sMessier_DB <> '') and ((sID_CMP = sMessier_DB) or (AnsiReplaceStr(sID_CMP,'M','M0') = sMessier_DB) or (AnsiReplaceStr(sID_CMP,'M','M00') = sMessier_DB)));

        if(not bFound) then
          bFound :=
          ((sCAT_NO_DB <> '') and (sID_CMP = sCAT_NO_DB));

        if(not bFound) and (sID2_CMP <> '') then
          bFound :=
          ((sNGC_DB <> '') and (sID2_CMP = sNGC_DB)) or
          ((sMessier_DB <> '') and ((sID2_CMP = sMessier_DB) or (AnsiReplaceStr(sID2_CMP,'M','M0') = sMessier_DB) or (AnsiReplaceStr(sID2_CMP,'M','M00') = sMessier_DB)));

      end
      else if(sAOType = 'S') then
      begin
        Inc(iStarCnt);

        sID1 := Uppercase(AnsiReplaceStr(Trim(sID1),' ',''));
        sCAT_NO_DB := Uppercase(AnsiReplaceStr(Trim((molAOList[i] as TStar).sCatNo),' ',''));

        bFound := (sCAT_NO_DB = sID1);
      end;

      if(bFound) then
      begin
        //ShowMessage('Found: ' + sID1 + ' (' + sCAT_NO_DB + ')');
        break;
      end;

    end
    else if(bDataChunk) then
    begin
      //ShowMessage('LAST: NGC: ' + sNGC_DB + ', Messier: ' + sMessier_DB);
      //ShowMessage((molAOList[i] as TAObject).sAOType);

      //ShowMessage('DataChunk ' + (molAOList[i] as TAObject).sAOType + ' at: ' + IntToStr(iStarCnt) + ' (' + (molAOList[i] as TStar).sCatNo + ')');
      break; // No more galaxies following in database
    end;

    Inc(i);
  end; // while..

  //ShowMessage('Stars counted: ' + IntToStr(iStarCnt) + ' last: ' + (molAOList[i-1] as TStar).sCatNo + ' - Searched for: ' + sID1);

  Result := bFound;
end;

function TF__ADM.ProcessSimbadGalaxyFile(sInFilename, sOutFilename: string): Boolean;
var
  tfInFile, tfOutFile: TextFile;
  sInLine, sOutLine: string;
  iInCnt, iCnt: Integer;
  slLineBuffer, slRADECBuffer, slIDBuffer, slAngSizeBuffer, slMTypeSizeBuffer: TStringList;
  sRA, sDEC, sRedSh: string;
  iRA_HH, iRA_MM: Word;
  iDEC_DEG, iDEC_MM: SmallInt;
  rRA_SS, rDEC_SS: Real;
  //rDistMLy: Real;
  sID,sID2,sConstID: string;
  sOType, sDim1, sDim2, sMag, sMType: string;
  bIsGalaxy: Boolean;
  //sRA_HH, sRA_MM, sRA_SS, sDEC_DEG, sDEC_MM, sDEC_SS: string;
  bFound: Boolean;
  iExCnt: Integer;
  bGo: Boolean;
  sMagU, sMagB, sMagV,sMagR, sMagG, sMagI, sMagJ, sMagH, sMagK: string;
  sMagsu, sMagsg, sMagsr, sMagsi, sMagsz: string;
begin
  Result := false;

  MM__SKIPPED.Clear;

  slLineBuffer := TStringList.Create;
  slLineBuffer.Delimiter:='|';
  slLineBuffer.StrictDelimiter:=true;

  slRADECBuffer := TStringList.Create;
  slRADECBuffer.Delimiter:=' ';
  slRADECBuffer.StrictDelimiter:=true;

  slIDBuffer := TStringList.Create;
  slIDBuffer.Delimiter:=' ';
  slIDBuffer.StrictDelimiter:=true;

  slAngSizeBuffer := TStringList.Create;
  slAngSizeBuffer.Delimiter:=' ';
  slAngSizeBuffer.StrictDelimiter:=true;

  slMTypeSizeBuffer := TStringList.Create;
  slMTypeSizeBuffer.Delimiter:=' ';
  slMTypeSizeBuffer.StrictDelimiter:=true;

  AssignFile(tfInFile,sInFilename);
  AssignFile(tfOutFile,sOutFilename);

  // Generate Albireo compatible star dataset from Simbad data file
  Reset(tfInFile);
  ReWrite(tfOutFile);

  iInCnt:=0; iCnt:=0; iExCnt:=0; bGo:=false;
  while (not eof(tfInFile)) do
  begin
    Inc(iInCnt);

    //rDistMLy:=-1;
    sID:=''; sID2:=''; sConstID:='';
    sDim1:=''; sDim2:='';
    sMType:='';

    ReadLn(tfInFile,sInLine);
    if(iInCnt > 9) then
    begin
      //ShowMessage(sInLine);

      slLineBuffer.DelimitedText:=sInLine;
      if(slLineBuffer.Count > 21) and
        ((bGo) or (MessageDlg('Run from here?',sInLine,mtConfirmation,[mbYes,mbNo],0) = mrYes)) then
      begin
        bGo := true;

        if((iInCnt mod 100) = 0) then
          Application.ProcessMessages;

        slIDBuffer.DelimitedText:=Trim(slLineBuffer[1]);

        if(slIDBuffer.Count = 0) then
          continue;

        if(slIDBuffer.Count > 1) then
        begin
          sID := Trim(slIDBuffer[0])  + ' ' + Trim(slIDBuffer[1]);
          sID2 := Trim(slIDBuffer[0]) + Trim(slIDBuffer[1]);
        end
        else
        begin
          //sID := Trim(slLineBuffer[0]);
          sID := Trim(slIDBuffer[0]);
          sID2 := '';
        end;

        if(Trim(sID) = '') then
          continue;

        //ShowMessage('slLineBuffer[1]: ' + slLineBuffer[1] + ', sID: ' + sID + ', sID2: ' + sID2);

        sOType := Trim(slLineBuffer[2]);
        bIsGalaxy := (
          (sOType = 'G') or
          (sOType = 'GiC') or
          (sOType = 'BiC') or
          (sOType = 'GiG') or
          (sOType = 'GiP') or
          (sOType = 'HzG') or
          (sOType = 'rG') or
          (sOType = 'H2G') or
          (sOType = 'LSB') or
          (sOType = 'AG?') or
          (sOType = 'EmG') or
          (sOType = 'SBG') or
          (sOType = 'bCG') or
          (sOType = 'LeG') or
          (sOType = 'AGN') or
          (sOType = 'LIN') or
          (sOType = 'SyG') or
          (sOType = 'Sy1') or
          (sOType = 'Sy2')
          );

        if(not bIsGalaxy) then
          continue;

        slRADECBuffer.DelimitedText:=slLineBuffer[3];

        if(slRADECBuffer.Count < 2) then
          continue;

        sRA := slRADECBuffer[0];
        sDEC := slRADECBuffer[1];

        iRA_HH := 0; iRA_MM := 0; rRA_SS := -999;
        iDEC_DEG := 0; iDEC_MM := 0; rDEC_SS := -999;

        HoursToHH_MM_SS(StrToFloatExt4(sRA)*24.0/360.0,iRA_HH,iRA_MM,rRA_SS);
        DegToDEG_MM_SS2(StrToFloatExt4(sDEC),iDEC_DEG,iDEC_MM,rDEC_SS);

        if(rRA_SS = -999) or (rDEC_SS = -999) then
          bIsGalaxy := false;

        if(iRA_HH > 24) or (iRA_MM > 59) or (rRA_SS >=60) then
          bIsGalaxy := false;

        if(iDEC_DEG >= 360) or (iDEC_MM > 59) or (rDEC_SS >= 60) then
          bIsGalaxy := false;
        (*
        if not (
          (AnsiContainsStr(Uppercase(sID),'NGC')) or
          (AnsiContainsStr(Uppercase(sID),'IC')) or
          (AnsiContainsStr(Uppercase(sID),'UGC')) or
          (AnsiContainsStr(Uppercase(sID),'ARP'))
          ) then
          bIsGalaxy := false;
        *)

        if(bIsGalaxy) then
        begin
          //if(sID = 'MCG+06-07-047') then
          //  ShowMessage('STOP!');

          bFound := CheckOnUniqueAO('G',sID,sID2);

          // If the simbad galaxy is not identified by any ID, add this to the import list.
          if(not bFound) then
          begin
            slAngSizeBuffer.DelimitedText:=slLineBuffer[22];
            if(slAngSizeBuffer.Count > 0) then
              sDim1 := slAngSizeBuffer[0];
            if(slAngSizeBuffer.Count > 1) then
              sDim2 := slAngSizeBuffer[1];

            slMTypeSizeBuffer.DelimitedText:=slLineBuffer[21];
            if(slMTypeSizeBuffer.Count > 0) then
              sMType := slMTypeSizeBuffer[0];

            sMType := sMType + '|' + sOType;

            sRedSh := Trim(slLineBuffer[5]);

            sMagU := Trim(slLineBuffer[7]);
            sMagB := Trim(slLineBuffer[8]);
            sMagV := Trim(slLineBuffer[9]);
            sMagR := Trim(slLineBuffer[10]);
            sMagG := Trim(slLineBuffer[11]);
            sMagI := Trim(slLineBuffer[12]);
            sMagJ := Trim(slLineBuffer[13]);
            sMagH := Trim(slLineBuffer[14]);
            sMagK := Trim(slLineBuffer[15]);
            sMagsu := Trim(slLineBuffer[16]);
            sMagsg := Trim(slLineBuffer[17]);
            sMagsr := Trim(slLineBuffer[18]);
            sMagsi := Trim(slLineBuffer[19]);
            sMagsz := Trim(slLineBuffer[20]);

            sMag := sMagV + '|' +
              sMagU + '|' +
              sMagB + '|' +
              sMagR + '|' +
              sMagG + '|' +
              sMagI + '|' +
              sMagJ + '|' +
              sMagH + '|' +
              sMagK + '|' +
              sMagsu + '|' +
              sMagsg + '|' +
              sMagsr + '|' +
              sMagsi + '|' +
              sMagsz;

            (*
            sMag := Trim(slLineBuffer[7]); //vmag
            if((sMag = '') or (sMag = '~')) then
              sMag := Trim(slLineBuffer[8]); // gmag
            *)
            sOutLine := sID + ';' + sID2 + ';' + sConstID + ';;;;' +
              sMag + ';' +
              IntToStr(iRA_HH) + ';' + AnsiReplaceStr(FloatToStrF(iRA_MM + rRA_SS/60.0,ffFixed,8,2),',','.') + ';' +
              IntToStr(iDEC_DEG) + ';' + AnsiReplaceStr(FloatToStrF(iDEC_MM + rDEC_SS/60.0,ffFixed,8,2),',','.') + ';' +
              sMType + ';' + // Type
              '#' + sRedSh + ';' +  // Distance/Redshift: Leading '#' - Else: MLy
              Trim(slLineBuffer[4]) + ';' +  // Radspeed
              sDim1 + ';' +
              sDim2 + ';' +
              'G';

            WriteLn(tfOutFile,sOutLine);
            Flush(tfOutFile);

            Inc(iCnt);
            L__PROCESSED_CNT.Caption:=IntToStr(iCnt); if(((iCnt+iExCnt) mod 10) = 0) then L__PROCESSED_CNT.Refresh;
            L__TOTAL_CNT.Caption:=IntToStr(iCnt+iExCnt); if(((iCnt+iExCnt) mod 10) = 0) then L__TOTAL_CNT.Refresh;
            MM__ADDED.Lines.Add(sID + ' (' + sID2 + ')');

            if(not Result) then
              Result := true;
          end
          else
          begin
            Inc(iExCnt);
            L__EXCLUDED_CNT.Caption:=IntToStr(iExCnt); if(((iCnt+iExCnt) mod 10) = 0) then L__EXCLUDED_CNT.Refresh;
            L__TOTAL_CNT.Caption:=IntToStr(iCnt+iExCnt); if(((iCnt+iExCnt) mod 10) = 0) then L__TOTAL_CNT.Refresh;
            MM__SKIPPED.Lines.Add(sID + ' (' + sID2 + ')');
          end;

        end; // bIsGalaxy..

      end;
    end;
  end;

  CloseFile(tfInFile);
  CloseFile(tfOutFile);

  slIDBuffer.Free;
  slRADECBuffer.Free;
  slLineBuffer.Free;
  slAngSizeBuffer.Free;
  slMTypeSizeBuffer.Free;

  if(Result) then
    MessageDlg('Albireo Database',IntToStr(iCnt) + ' Albireo galaxy datasets sucessfully generated.',mtInformation,[mbOK],0)
  else
    MessageDlg('Albireo Database','Unable to generate Albireo galaxy datasets',mtWarning,[mbOK],0);


end;

function TF__ADM.ProcessSimbadStarFile(sInFilename, sOutFilename: string): Boolean;
var
  tfInFile, tfOutFile: TextFile;
  sInLine, sOutLine: string;
  iInCnt, iCnt: Integer;
  slLineBuffer, slRADECBuffer, slIDBuffer: TStringList;
  sRA, sDEC, sPlx: string;
  iRA_HH, iRA_MM: Word;
  iDEC_DEG, iDEC_MM: SmallInt;
  rRA_SS, rDEC_SS: Real;
  rPlx_mas, rDistLy: Real;
  sID,sID2,sConstID: string;
  bFound: Boolean;
  //sRA_HH, sRA_MM, sRA_SS, sDEC_DEG, sDEC_MM, sDEC_SS: string;
begin
  Result := false;

  slLineBuffer := TStringList.Create;
  slLineBuffer.Delimiter:='|';
  slLineBuffer.StrictDelimiter:=true;

  slRADECBuffer := TStringList.Create;
  slRADECBuffer.Delimiter:=' ';
  slRADECBuffer.StrictDelimiter:=true;

  slIDBuffer := TStringList.Create;
  slIDBuffer.Delimiter:=' ';
  slIDBuffer.StrictDelimiter:=true;

  AssignFile(tfInFile,sInFilename);
  AssignFile(tfOutFile,sOutFilename);

  // Generate Albireo compatible star dataset from Simbad data file
  Reset(tfInFile);
  ReWrite(tfOutFile);

  iInCnt:=0; iCnt:=0;
  while (not eof(tfInFile)) do
  begin
    Inc(iInCnt);

    rPlx_mas:=-1; rDistLy:=-1;
    sID:=''; sID2:=''; sConstID:='';

    ReadLn(tfInFile,sInLine);
    if(iInCnt > 9) then
    begin
      //ShowMessage(sInLine);

      slLineBuffer.DelimitedText:=sInLine;
      if(slLineBuffer.Count > 6) then
      begin
        slIDBuffer.DelimitedText:=Trim(slLineBuffer[1]);
        if(slIDBuffer.Count > 2) then
        begin
          sID := Trim(slIDBuffer[0]) + ' ' + Trim(slIDBuffer[1]);
          sID2 := Trim(slIDBuffer[1]);
          sConstID := Trim(slIDBuffer[2]);
        end
        else
          sID := Trim(slLineBuffer[1]);

        bFound := CheckOnUniqueAO('S',sID,sID2);

        if(not bFound) then
        begin
          slRADECBuffer.DelimitedText:=slLineBuffer[3];

          sRA := slRADECBuffer[0];
          sDEC := slRADECBuffer[1];

          iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
          iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0;

          HoursToHH_MM_SS(StrToFloatExt4(sRA)*24.0/360.0,iRA_HH,iRA_MM,rRA_SS);
          DegToDEG_MM_SS2(StrToFloatExt4(sDEC),iDEC_DEG,iDEC_MM,rDEC_SS);

          sPlx := Trim(slLineBuffer[4]);
          if(sPlx <> '') then
          begin
            rPlx_mas := StrToFloatExt4(sPlx);
            if(rPlx_mas > 0) then
              rDistLy := 3261.47086 / rPlx_mas;
          end;

          sOutLine := sID + ';' + sID2 + ';' + sConstID + ';;;' +
            Trim(slLineBuffer[5]) + ';' +
            IntToStr(iRA_HH) + ';' + IntToStr(iRA_MM) + ';' + AnsiReplaceStr(FloatToStrF(rRA_SS,ffFixed,8,1),',','.') + ';' +
            IntToStr(iDEC_DEG) + ';' + IntToStr(iDEC_MM) + ';' + AnsiReplaceStr(FloatToStrF(rDEC_SS,ffFixed,8,1),',','.') + ';' +
            Trim(slLineBuffer[6]) + ';' +
            AnsiReplaceStr(FloatToStrF(rDistLy,ffFixed,8,1),',','.') + ';-1;S;0;-';

          WriteLn(tfOutFile,sOutLine);
          Flush(tfOutFile);

          Inc(iCnt);
          L__PROCESSED_CNT.Caption:=IntToStr(iCnt); if((iInCnt mod 10) = 0) then L__PROCESSED_CNT.Refresh;
        end
        else
        begin
          L__EXCLUDED_CNT.Caption:=IntToStr(iInCnt-iCnt); if((iInCnt mod 10) = 0) then L__EXCLUDED_CNT.Refresh;
        end;

        L__TOTAL_CNT.Caption:=IntToStr(iInCnt); if((iInCnt mod 10) = 0) then L__TOTAL_CNT.Refresh;

        if(not Result) then
          Result := true;

      end;
    end;
  end;

  CloseFile(tfInFile);
  CloseFile(tfOutFile);

  slIDBuffer.Free;
  slRADECBuffer.Free;
  slLineBuffer.Free;

  if(Result) then
    MessageDlg('Albireo Database',IntToStr(iCnt) + ' Albireo star datasets sucessfully generated.',mtInformation,[mbOK],0)
  else
    MessageDlg('Albireo Database','Unable to generate Albireo star datasets',mtWarning,[mbOK],0);


end;

procedure TF__ADM.B__CALC_OMEGAClick(Sender: TObject);
var
  fVal1, fVal2, fVal3: Real;
begin
  if(Trim(ED__APERI.Text) = '') or (Trim(ED__LAN.Text) = '') then
    exit;

  fVal1 := StrToFloatExt(ED__APERI.Text);
  fVal2 := StrToFloatExt(ED__LAN.Text);

  fVal3 := fVal1 + fVal2;
  if(fVal3 > 360) then
    fVal3 := fVal3 - 360;

  ED__OMEGA_Q.Text:=FloatToStrF(fVal3,ffFixed,6,3);

end;

procedure TF__ADM.B__LINEDELETEClick(Sender: TObject);
begin
  Filemod_DelLine(ED__DEL_STRING.Text,ED__IGNOREPOS.Value);
end;

procedure TF__ADM.B__PROCESSClick(Sender: TObject);
begin
  if(ODLG.Execute and (ODLG.FileName <> '')) then
  begin
    if(SDLG.Execute and (SDLG.FileName <> '')) then
    if(RB__GEN_DS_STARS.Checked) then
      ProcessSimbadStarFile(ODLG.FileName,SDLG.FileName)
    else if(RB__GEN_DS_GALAXIES.Checked) then
      ProcessSimbadGalaxyFile(ODLG.FileName,SDLG.FileName);

  end;
end;

procedure TF__ADM.CB__ASTEROIDChange(Sender: TObject);
var
  Planet: TPlanet;
  iIndex: Integer;
begin
  iIndex := CB__ASTEROID.ItemIndex;

  if(iIndex < 0) then
    exit;

  if(CB__ASTEROID.Items.Objects[iIndex] <> nil) then
  begin
    Planet := (CB__ASTEROID.Items.Objects[iIndex] as TPlanet);

    ED__NAME_DE.Text := Planet.sName_DE;
    ED__NAME_EN.Text := Planet.sName_EN;
    ED__OPERIOD.Text := FloatToStrF(Planet.rTp,ffFixed,8,3);
    ED__SMAXIS.Text := FloatToStrF(Planet.rA,ffFixed,8,3);
    ED__INCLINATION.Text := FloatToStrF(Planet.rI,ffFixed,8,3);
    ED__ECCENTRICITY.Text := FloatToStrF(Planet.rE,ffFixed,8,3);
    ED__LAN.Text := FloatToStrF(Planet.rOmega,ffFixed,8,3);
    ED__OMEGA_Q.Text := FloatToStrF(Planet.rOmegaQ,ffFixed,8,3);
    ED__DIAMETER.Text := FloatToStrF(Planet.rDiameterRatio*12735.0,ffFixed,8,3);
    ED__VDIAM_AS.Text := FloatToStrF(Planet.rTheta0,ffFixed,8,3);
    ED__MASS.Text := FloatToStrF(Planet.rMassRatio*5.9723*1000000,ffFixed,8,3);
    ED__MAG.Text := FloatToStrF(Planet.rM,ffFixed,8,3);
    ED__LEPOCH.Text := FloatToStrF(Planet.rEpsilon,ffFixed,8,3);
  end;

end;

procedure TF__ADM.FormCreate(Sender: TObject);
begin
  ED__CALC_LE_DATE.Date:=Date;
  ED__CALC_LE_TIME.Time:=Time;
end;

procedure TF__ADM.B__CALC_LEPOCHClick(Sender: TObject);
var
  //rRA_HH, rDEC_DEG: Real;
  rRA_HH_IT, rDEC_DEG_IT: Real;
  rRA_HH1, rDEC_DEG1: Real;
  rRA_HH2, rDEC_DEG2: Real;
  //rRA_HH3, rDEC_DEG3: Real;
  iRA_HH, iRA_MM: Word;
  iDEC_DEG, iDEC_MM: SmallInt;
  rRA_SS, rDEC_SS: Real;
  rRA_HH_START, rDEC_DEG_START: Real;
  dtTime: TDateTime;
  Planet: TPlanet;
  rRADiff1, rRADiff2, rDECDiff1, rDECDiff2, rRADiff, rDECDiff: Real;
  rDiff1, rDiff2, rDiff, rDiffPrev: Real;
  //iMaxIter: Integer;
  rDiffEpsilon: Real;
  i, iMax: Integer;
  iSign: SmallInt;
  rStep: Real;
  bMatched: Boolean;
  rR: Real;
begin
  if(Trim(ED__OMEGA_Q.Text) = '') or
    (Trim(ED__LAN.Text) = '') or
    (Trim(ED__NAME_DE.Text) = '') or
    (Trim(ED__NAME_EN.Text) = '') or
    (Trim(ED__ECCENTRICITY.Text) = '') or
    (Trim(ED__INCLINATION.Text) = '') or
    (Trim(ED__SMAXIS.Text) = '') or
    (Trim(ED__OPERIOD.Text) = '') or
    (Trim(ED__MASS.Text) = '') or
    (Trim(ED__DIAMETER.Text) = '') then
    exit;

  MM__INFO.Clear;

  //iMaxIter := StrToInt(ED__MAXITER.Text);
  rDiffEpsilon := StrToFloatExt(ED__EPSILON.Text);

  Planet := TPlanet.Create();
  Planet.sName_DE:=ED__NAME_DE.Text;
  Planet.sName_EN:=ED__NAME_EN.Text;
  Planet.bHasImage:=false;
  Planet.bInnerPlanet:=false;
  Planet.rTp:=StrToFloatExt(ED__OPERIOD.Text);
  Planet.rA:=StrToFloatExt(ED__SMAXIS.Text);
  Planet.rI:=StrToFloatExt(ED__INCLINATION.Text);
  Planet.rE:=StrToFloatExt(ED__ECCENTRICITY.Text);
  Planet.rOmega:=StrToFloatExt(ED__LAN.Text);
  Planet.rOmegaQ:=StrToFloatExt(ED__OMEGA_Q.Text);
  Planet.rDiameterRatio:=StrToFloatExt(ED__DIAMETER.Text)/12735.0;
  Planet.rTheta0:=StrToFloatExt(ED__DIAMETER.Text)/149597870.7 * 180/Pi * 3600;
  Planet.rMassRatio:=StrToFloatExt(ED__MASS.Text)/5.9723/1000000;
  Planet.rM:=StrToFloatExt(ED__MAG.Text);
  Planet.sPlanetType:='A';
  Planet.sAOType:='P';

  Planet.rEpsilon:=180 + 30;

  rRA_HH_START := StrToFloatExt(ED__CALC_LE_RA_HH.Text) + StrToFloatExt(ED__CALC_LE_RA_MM.Text)/60.0 + StrToFloatExt(ED__CALC_LE_RA_SS.Text)/3600.0;

  if(StrToFloatExt(ED__CALC_LE_DEC_DEG.Text) > 0) then
    rDEC_DEG_START := StrToFloatExt(ED__CALC_LE_DEC_DEG.Text) + StrToFloatExt(ED__CALC_LE_DEC_MM.Text)/60.0 + StrToFloatExt(ED__CALC_LE_DEC_SS.Text)/3600.0
  else
  begin
    rDEC_DEG_START := Abs(StrToFloatExt(ED__CALC_LE_DEC_DEG.Text)) + StrToFloatExt(ED__CALC_LE_DEC_MM.Text)/60.0 + StrToFloatExt(ED__CALC_LE_DEC_SS.Text)/3600.0;
    rDEC_DEG_START := -rDEC_DEG_START;
  end;

  dtTime := ED__CALC_LE_DATE.Date + ED__CALC_LE_TIME.Time;

  iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
  iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0; rR:=0;

  // 1st preparational run
  GetPlanetCoo(dtTime,Planet,
    miDST_HH,miUTC_HH,
    iRA_HH,iRA_MM,rRA_SS,
    iDEC_DEG,iDEC_MM,rDEC_SS,rR);

  if(iDEC_DEG >= 270) then iDEC_DEG := iDEC_DEG - 360;

  rRA_HH1 := iRA_HH + iRA_MM/60.0 + rRA_SS/3600.0;
  rDEC_DEG1 := abs(iDEC_DEG) + abs(iDEC_MM/60.0) + abs(rDEC_SS/3600.0);

  if(iDEC_DEG < 0) or (iDEC_MM < 0) or (rDEC_SS < 0) then
    rDEC_DEG1 := -rDEC_DEG1;

  MM__INFO.Lines.Add('RA1: ' + FloatToStrF(rRA_HH1,ffFixed,6,2) + ', DEC1: ' + FloatToStrF(rDEC_DEG1,ffFixed,6,2));


  Planet.rEpsilon:=180 - 30;

  iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
  iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0; rR:=0;

  // 2nd preparational run
  GetPlanetCoo(dtTime,Planet,
    miDST_HH,miUTC_HH,
    iRA_HH,iRA_MM,rRA_SS,
    iDEC_DEG,iDEC_MM,rDEC_SS,rR);

  Planet.rDistFromSun_AU := rR;

  if(iDEC_DEG >= 270) then iDEC_DEG := iDEC_DEG - 360;

  rRA_HH2 := iRA_HH + iRA_MM/60.0 + rRA_SS/3600.0;
  rDEC_DEG2 := abs(iDEC_DEG) + abs(iDEC_MM/60.0) + abs(rDEC_SS/3600.0);

  if(iDEC_DEG < 0) or (iDEC_MM < 0) or (rDEC_SS < 0) then
    rDEC_DEG2 := -rDEC_DEG2;

  MM__INFO.Lines.Add('RA2: ' + FloatToStrF(rRA_HH2,ffFixed,6,2) + ', DEC2: ' + FloatToStrF(rDEC_DEG2,ffFixed,6,2));

  // Consider Difference Start-value < 24 estimation value > 0: 23h - 1h = 2h!!!, not 22h!
  rRADiff1 := CycleDiff(rRA_HH_START,rRA_HH1,24) *180/12; // *180/12: Harmonize as degrees together wirh DEC-values to get better differences below.
  rRADiff2 := CycleDiff(rRA_HH_START,rRA_HH2,24) *180/12;

  rDECDiff1 := CycleDiff(rDEC_DEG_START+90,rDEC_DEG1+90,180); // -90..90 -> 0..180
  rDECDiff2 := CycleDiff(rDEC_DEG_START+90,rDEC_DEG2+90,180);

  //rRADiff1 := abs(rRA_HH_START - rRA_HH1)*180/12; rRADiff2:= abs(rRA_HH_START - rRA_HH2)*180/12; // Convert to Degrees becaus of Diff.
  //rDECDiff1 := abs(rDEC_DEG_START - rDEC_DEG1); rDECDiff2:= abs(rDEC_DEG_START - rDEC_DEG2);

  // Decide, what direction 1 (180°+30°) or 2 (180°-30°) has smaller deviations
  rDiff1 := sqrt(rRADiff1*rRADiff1 + rDECDiff1*rDECDiff1);
  rDiff2 := sqrt(rRADiff2*rRADiff2 + rDECDiff2*rDECDiff2);

  if(rDiff1 <= rDiff2) then
  begin
    iSign := 1;
    rDiff := rDiff1;
    rRA_HH_IT := rRA_HH1;
    rDEC_DEG_IT := rDEC_DEG1;
  end
  else
  begin
    iSign := -1;
    rDiff := rDiff2;
    rRA_HH_IT := rRA_HH2;
    rDEC_DEG_IT := rDEC_DEG2;
  end;

  i := 0;
  iMax := 100;
  rStep:=5;
  rDiffPrev := 999;
  bMatched:=false;

  while (i < iMax) do
  begin
    Planet.rEpsilon:=Planet.rEpsilon + iSign*rStep;

    iRA_HH:=0; iRA_MM:=0; rRA_SS:=0;
    iDEC_DEG:=0; iDEC_MM:=0; rDEC_SS:=0; rR:=0;

    GetPlanetCoo(dtTime,Planet,
      miDST_HH,miUTC_HH,
      iRA_HH,iRA_MM,rRA_SS,
      iDEC_DEG,iDEC_MM,rDEC_SS,rR);

    Planet.rDistFromSun_AU:=rR;

    if(iDEC_DEG >= 270) then iDEC_DEG := iDEC_DEG - 360;

    rRA_HH_IT := iRA_HH + iRA_MM/60.0 + rRA_SS/3600.0;
    rDEC_DEG_IT := abs(iDEC_DEG) + abs(iDEC_MM/60.0) + abs(rDEC_SS/3600.0);

    if(iDEC_DEG < 0) or (iDEC_MM < 0) or (rDEC_SS < 0) then
      rDEC_DEG_IT := -rDEC_DEG_IT;

    rRADiff := CycleDiff(rRA_HH_START,rRA_HH_IT,24) *180/12; // *180/12: Harmonize as degrees together wirh DEC-values to get better differences below.
    rDECDiff := CycleDiff(rDEC_DEG_START+90,rDEC_DEG_IT+90,180); // -90..90 -> 0..180

    rDiff := sqrt(rRADiff*rRADiff + rDECDiff*rDECDiff);
    MM__INFO.Lines.Add('Epsilon: ' + FloatToStr(Planet.rEpsilon) + ', ' +
    'RA_IT: ' + FloatToStrF(rRA_HH_IT,ffFixed,6,2) + ', DEC_IT: ' + FloatToStrF(rDEC_DEG_IT,ffFixed,6,2) +
      ', Diff: ' + FloatToStr(rDiff));

    if(rDiff < rDiffEpsilon) then
    begin
      i:=iMax;
      bMatched := true;
    end
    else
      Inc(i);

    if(rDiff > rDiffPrev) then
    begin
      iSign := -1*iSign;
      rStep := rStep / 10.0;
    end;

    rDiffPrev := rDiff;
  end; // while i...

  if(bMatched) then
  begin
    if (MessageDlg('Success','Planet.rEpsilon coverged to ' + FloatToStr(Planet.rEpsilon) + '. Take over?',mtConfirmation,[mbYes,mbNo],0) = mrYes) then
      ED__LEPOCH.Text:=FloatToStr(Planet.rEpsilon);

    MM__INFO.Lines.Add('');
    MM__INFO.Lines.Add('New dataset');
    MM__INFO.Lines.Add('XX;' +
      Planet.sName_DE + ';' +
      Planet.sName_EN + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rTp,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rEpsilon,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rOmegaQ,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rE,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rA,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rI,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rOmega,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rTheta0,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rV0,ffFixed,9,4),',','.') + ';0;' +
      AnsiReplaceStr(FloatToStrF(Planet.rDiameterRatio,ffFixed,9,4),',','.') + ';' +
      AnsiReplaceStr(FloatToStrF(Planet.rMassRatio,ffFixed,9,4),',','.') + ';$00555555;A;P;-;'
    );
  end
  else
    MessageDlg('Failure. No convergence.','Please check the target RA/DEC and oribital parameters',mtWarning,[mbOK],0);

end;

procedure TF__ADM.B__ADDTEMPClick(Sender: TObject);
begin
  if(Trim(ED__OMEGA_Q.Text) = '') or
    (Trim(ED__LAN.Text) = '') or
    (Trim(ED__NAME_DE.Text) = '') or
    (Trim(ED__NAME_EN.Text) = '') or
    (Trim(ED__ECCENTRICITY.Text) = '') or
    (Trim(ED__INCLINATION.Text) = '') or
    (Trim(ED__SMAXIS.Text) = '') or
    (Trim(ED__OPERIOD.Text) = '') or
    (Trim(ED__MASS.Text) = '') or
    (Trim(ED__DIAMETER.Text) = '') or
    (Trim(ED__LEPOCH.Text) = '') then
    exit;

  if(Trim(ED__CALC_LE_DATE.Text) = '') or
    (Trim(ED__CALC_LE_TIME.Text) = '') or
    (Trim(ED__CALC_LE_RA_HH.Text) = '') or
    (Trim(ED__CALC_LE_DEC_DEG.Text) = '') then
    exit;

end;

procedure TF__ADM.B__ADDTOLIBClick(Sender: TObject);
begin
    if(Trim(ED__OMEGA_Q.Text) = '') or
    (Trim(ED__LAN.Text) = '') or
    (Trim(ED__NAME_DE.Text) = '') or
    (Trim(ED__NAME_EN.Text) = '') or
    (Trim(ED__ECCENTRICITY.Text) = '') or
    (Trim(ED__INCLINATION.Text) = '') or
    (Trim(ED__SMAXIS.Text) = '') or
    (Trim(ED__OPERIOD.Text) = '') or
    (Trim(ED__MASS.Text) = '') or
    (Trim(ED__DIAMETER.Text) = '') or
    (Trim(ED__LEPOCH.Text) = '') then
    exit;

end;

procedure TF__ADM.B__CALC_ARCSECClick(Sender: TObject);
begin
  ED__VDIAM_AS.Text := FloatToStrF(
    StrToFloatExt4(ED__DIAMETER.Text)/(StrToFloatExt4(ED__SMAXIS.Text)*149597870.700)*180.0/Pi*3600.0
  ,ffFixed,8,2);
end;

end.

