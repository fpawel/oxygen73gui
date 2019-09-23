unit MainSvcClient;

interface

uses MainSvc;

var
    MainSvcApi: TMainSvc.Iface;

procedure Connect;

implementation

uses System.SysUtils, registry, winapi.windows, Thrift.Protocol, Thrift.Transport;

procedure Connect;
var
    Protocol: IProtocol;
    Transport: ITransport;
    key: TRegistry;
begin
    key := TRegistry.Create(KEY_READ);
    try
        if not key.OpenKey( 'oxygen73\tcp', False) then
            raise Exception.Create('cant open oxygen73\tcp');
        Transport := TSocketImpl.Create(key.ReadString('main_ip'),
            key.ReadInteger('main_port'), 1000);
    finally
        key.CloseKey;
        key.Free;
    end;
    Protocol := TBinaryProtocolImpl.Create(Transport);
    MainSvcApi := TMainSvc.TClient.Create(Protocol);
    Transport.Open;
end;

end.
