unit u_adbinfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  U_AConst;

type

  { TF__ADBINFO }

  TF__ADBINFO = class(TForm)
    L__INFO_DEEPSKY: TLabel;
    L__CNT_G: TLabel;
    L__CNT_Q: TLabel;
    L__CNT_Q_TITLE: TLabel;
    L__CNT_PA: TLabel;
    L__CNT_C: TLabel;
    L__CNT_C_TITLE: TLabel;
    L__CNT_P_TITLE: TLabel;
    L__CNT_OC: TLabel;
    L__CNT_OC_TITLE: TLabel;
    L__CNT_P: TLabel;
    L__CNT_PN: TLabel;
    L__CNT_PN_TITLE: TLabel;
    L__CNT_N: TLabel;
    L__CNT_GC: TLabel;
    L__CNT_G_TITLE: TLabel;
    L__CNT_GC_TITLE: TLabel;
    L__CNT_N_TITLE: TLabel;
    L__CNT_PA_TITLE: TLabel;
    L__CNT_S: TLabel;
    L__CNT_S_TITLE: TLabel;
    L__INFO_SOLARSYSTEM: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public
    msLANG_ID: string;

    procedure DisplayCnt();
  end;

var
  F__ADBINFO: TF__ADBINFO;

implementation

{$R *.lfm}

{ TF__ADBINFO }

procedure TF__ADBINFO.DisplayCnt();
begin
  // DSOs
  if(grecAOIndexControl.iMax_S > 0) then
    L__CNT_S.Caption := IntToStr(grecAOIndexControl.iMax_S - grecAOIndexControl.iMin_S);  // +1 -1 = 0 because of first line

  if(grecAOIndexControl.iMax_G > 0) then
    L__CNT_G.Caption := IntToStr(grecAOIndexControl.iMax_G - grecAOIndexControl.iMin_G);

  if(grecAOIndexControl.iMax_Q > 0) then
    L__CNT_Q.Caption := IntToStr(grecAOIndexControl.iMax_Q - grecAOIndexControl.iMin_Q);

  if(grecAOIndexControl.iMax_GC > 0) then
    L__CNT_GC.Caption := IntToStr(grecAOIndexControl.iMax_GC - grecAOIndexControl.iMin_GC);

  if(grecAOIndexControl.iMax_N > 0) then
    L__CNT_N.Caption := IntToStr(grecAOIndexControl.iMax_N - grecAOIndexControl.iMin_N);

  if(grecAOIndexControl.iMax_PN > 0) then
    L__CNT_PN.Caption := IntToStr(grecAOIndexControl.iMax_PN - grecAOIndexControl.iMin_PN);

  if(grecAOIndexControl.iMax_OC > 0) then
    L__CNT_OC.Caption := IntToStr(grecAOIndexControl.iMax_OC - grecAOIndexControl.iMin_OC);

  // Solar System

  if(grecAOIndexControl.iMax_P > 0) then
    L__CNT_P.Caption := IntToStr(grecAOIndexControl.iMax_P - grecAOIndexControl.iMin_P);

  if(grecAOIndexControl.iMax_PA > 0) then
    L__CNT_PA.Caption := IntToStr(grecAOIndexControl.iMax_PA - grecAOIndexControl.iMin_PA + 1);  // +1 because of first line is counted only for planets

  if(grecAOIndexControl.iMax_C > 0) then
    L__CNT_C.Caption := IntToStr(grecAOIndexControl.iMax_C - grecAOIndexControl.iMin_C);

end;

procedure TF__ADBINFO.FormCreate(Sender: TObject);
begin
  if(msLANG_ID = 'DE') then
  begin
    L__CNT_S_TITLE.Caption:='Anzahl Sterne';
    L__CNT_G_TITLE.Caption:='Anzahl Galaxien';
    L__CNT_Q_TITLE.Caption:='Anzahl Quasare';
    L__CNT_GC_TITLE.Caption:='Anzahl Kugelsternhaufen';
    L__CNT_N_TITLE.Caption:='Anzahl galaktische Nebel';
    L__CNT_PN_TITLE.Caption:='Anzahl planetarische Nebel';
    L__CNT_OC_TITLE.Caption:='Anzahl offene Sternhaufen';

    L__CNT_P_TITLE.Caption:='Anzahl Planeten';
    L__CNT_PA_TITLE.Caption:='Anzahl Asteroiden';
    L__CNT_C_TITLE.Caption:='Anzahl Kometen';
  end
  else
  begin
    L__CNT_S_TITLE.Caption:='Count Stars';
    L__CNT_G_TITLE.Caption:='Count Galaxies';
    L__CNT_Q_TITLE.Caption:='Count Quasars';
    L__CNT_GC_TITLE.Caption:='Count Globular Cluster';
    L__CNT_N_TITLE.Caption:='Count Nebula';
    L__CNT_PN_TITLE.Caption:='Count Planetary Nebula';
    L__CNT_OC_TITLE.Caption:='Count Open Cluster';

    L__CNT_P_TITLE.Caption:='Count Planets';
    L__CNT_PA_TITLE.Caption:='Count Asteroids';
    L__CNT_C_TITLE.Caption:='Count Comets';
  end;

  DisplayCnt();

end;

end.

