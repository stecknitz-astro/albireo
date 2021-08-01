unit U_ABase;

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

//{$DEFINE DELPHI10} // Define for Development under Delphi 10+
{$DEFINE LAZARUS} // Define for Development under Lazarus
//{$DEFINE WINDOWS} // Define for Development under Lazarus
//{$DEFINE MACOS} // Define for Development under Lazarus
//{$DEFINE LINUX} // Define for Development under Lazarus


{$IFDEF LAZARUS}
{$mode objfpc}{$H+}
{$ENDIF}

interface
{$IFDEF LAZARUS}
uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, StrUtils, // ContNrs,
  Menus,  DateUtils,  LCLIntf,
  // Process, Math,
  {$IFDEF Windows}
  //Windows, ShellAPI,
  {$ENDIF WIndows}
  {$IFDEF Darwin}
  MacOSAll, CocoaAll,
  {$ENDIF Darwin}
  {$IFDEF LINUX}
  Unix,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}
  LResources;

{$ENDIF}

{$IFDEF DELPHI10}
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,StrUtils,
  Math, System.DateUtils;
{$ENDIF}

function ConvertWinPath(sFilePath: string): string;

implementation

function ConvertWinPath(sFilePath: string): string;
begin
  {$IFDEF Windows}
  //if(LeftStr(sFilePath,1) = '\') or (LeftStr(sFilePath,2) = 'C:') or (LeftStr(sFilePath,2) = 'D:') then
  if(LeftStr(sFilePath,1) = '\') or (AnsiContainsStr(LeftStr(sFilePath,2),':')) then
    Result := sFilePath
  else
    Result := Application.Location + sFilePath;
  {$ENDIF Windows}

  {$IFDEF Darwin}
  sFilePath := AnsiReplaceStr(sFilePath,'\','/');
  //if(LeftStr(sFilePath,1) = '/') or (LeftStr(sFilePath,2) = 'C:') or (LeftStr(sFilePath,2) = 'D:') then
  if(LeftStr(sFilePath,1) = '/') or (AnsiContainsStr(LeftStr(sFilePath,2),':')) then
    Result := sFilePath
  else
    Result := Application.Location + sFilePath;
  {$ENDIF Darwin}

  {$IFDEF LINUX}
  sFilePath := AnsiReplaceStr(sFilePath,'\','/');
  //if(LeftStr(sFilePath,1) = '/') or (LeftStr(sFilePath,2) = 'C:') or (LeftStr(sFilePath,2) = 'D:') then
  if(LeftStr(sFilePath,1) = '/') or (AnsiContainsStr(LeftStr(sFilePath,2),':')) then
    Result := sFilePath
  else
    Result := Application.Location + sFilePath;
  {$ENDIF LINUX}

end;

end.

