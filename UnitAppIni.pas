unit UnitAppIni;

interface

uses inifiles;

var AppIni :TIniFile;

implementation

uses System.sysutils;

initialization

AppIni := TIniFile.Create( ExtractFileDir(ParamStr(0))  + '\oxygen73gui.ini');

finalization

Appini.Free;

end.
