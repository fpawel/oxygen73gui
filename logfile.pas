unit logfile;

interface

uses sysutils;

procedure LogfileWriteLn(level, str: string);
procedure LogfileWriteException(e: Exception);

implementation

uses JclDebug, classes, vcl.forms, winapi.windows;

var
    _logfile: TextFile;
    ProcessID: Cardinal;

//procedure EventLogWriteLn(level, str: string; EventId: Word = 0);
//var
//    h: THandle;
//begin
//    str := Format('[%d] %s %s', [ProcessID, level, str]);
//
//    h := RegisterEventSource(nil, PChar(ExtractFileName(Application.ExeName)));
//    if h > 0 then
//        try
//            ReportEvent(h, 0, 0, EventId, nil, 1, 0, @str, nil);
//        finally
//            DeregisterEventSource(h);
//        end;
//end;

procedure LogfileOpen;
var
    ADir, ALogFileName: string;
begin
    GetWindowThreadProcessId(Application.Handle, ProcessID);

    ADir := ExtractFileDir(Application.ExeName) + '\Logs';

    if not DirectoryExists(ADir) then
        CreateDir(ADir);

    ALogFileName := ADir + '\' + FormatDateTime('YYYY-MM-dd', now) + '.gui.log';

    AssignFile(_logfile, ALogFileName, CP_UTF8);
    if FileExists(ALogFileName) then
        Append(_logfile)
    else
        Rewrite(_logfile);
end;

procedure LogfileWriteLn(level, str: string);
begin
    Writeln(_logfile, FormatDateTime('hh:nn:ss', now), ' ', '[', ProcessID, ']',
      ' ', level, ' ', str);
    //EventLogWriteLn(level, str);

end;

function strReplaceLn(s: string): string;
begin
    result := stringreplace(Trim(s), #13, ':', [rfReplaceAll, rfIgnoreCase])
end;

procedure LogfileWriteException(e: Exception);
var
    stackList: TJclStackInfoList; // JclDebug.pas
    sl: TStringList;
    I: Integer;

begin


    stackList := JclCreateStackList(false, 1, Caller(1, false));
    sl := TStringList.Create;
    stackList.AddToStrings(sl, false, false, false, false);
    LogfileWriteLn('EXN', e.ClassName + ' ' + strReplaceLn(e.Message) + #10 +sl.Text);
    sl.Free;
    stackList.Free;
end;

initialization

LogfileOpen;

finalization

CloseFile(_logfile);

end.
