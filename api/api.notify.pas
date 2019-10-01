
unit api.notify;

interface

uses  Winapi.Windows, Winapi.Messages ;

type
    
    TStatusMessage = record
    public
        Ok : Boolean;
        Text : string;
        Detail : string;
        
    end;
    
    TMeasurement = record
    public
        Temperature : Double;
        Pressure : Double;
        Humidity : Double;
        Places : TArray<Double>;
        StoredAt : TDateTime;
        
    end;
    
    TStringHandler = reference to procedure (x:string);
    TStatusMessageHandler = reference to procedure (x:TStatusMessage);
    TMeasurementHandler = reference to procedure (x:TMeasurement);
    

procedure HandleCopydata(var Message: TMessage);
procedure Finalize;
procedure Initialize;
var
   HandleWriteConsole : TStringHandler;
   HandleStatus : TStatusMessageHandler;
   HandleMeasurement : TMeasurementHandler;

implementation 

uses Grijjy.Bson, Grijjy.Bson.Serialization, stringutils, sysutils;

type
    TServerAppCmd = (CmdWriteConsole, CmdStatus,  CmdMeasurement);

    type _deserializer = class
        class function deserialize<T>(str:string):T;static;
    end;

var _enabled:boolean;

class function _deserializer.deserialize<T>(str:string):T;
begin
    TgoBsonSerializer.Deserialize(str, Result);
end;

procedure Initialize;
begin
    
    if not Assigned(HandleWriteConsole) then
        raise Exception.Create('HandleWriteConsole must be set');
    
    if not Assigned(HandleStatus) then
        raise Exception.Create('HandleStatus must be set');
    
    if not Assigned(HandleMeasurement) then
        raise Exception.Create('HandleMeasurement must be set');
    
   _enabled := true;
end;

procedure Finalize;
begin
   _enabled := false;
end;

procedure HandleCopydata(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    cmd: TServerAppCmd;
    str:string;
begin
    if _enabled = false then
        exit;
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := TServerAppCmd(Message.WParam);
    Message.result := 1;
    SetString(str, PWideChar(cd.lpData), cd.cbData div 2);
    case cmd of
        CmdWriteConsole:
            HandleWriteConsole(str);
        CmdStatus:
            HandleStatus(_deserializer.deserialize<TStatusMessage>(str));
        CmdMeasurement:
            HandleMeasurement(_deserializer.deserialize<TMeasurement>(str));
        
    else
        raise Exception.Create('wrong message: ' + IntToStr(Message.WParam));
    end;
end;

initialization
    _enabled := false;

end.