unit MainSvcClient;

interface

uses MainSvc;

var
    MainSvcApi: TMainSvc.Iface;

procedure Connect;

implementation

uses System.SysUtils, registry, winapi.windows, Thrift.Protocol,
    Thrift.Transport,
    logfile;

procedure Connect;
var
    Protocol: IProtocol;
    Transport: ITransport;
    key: TRegistry;
begin
    Transport := TSocketImpl.Create('127.0.0.1',
      StrToInt(GetEnvironmentVariable('OXYGEN73_API_PORT')), 50000);
    Protocol := TBinaryProtocolImpl.Create(Transport);
    MainSvcApi := TMainSvc.TClient.Create(Protocol);
    Transport.Open;
end;

end.
