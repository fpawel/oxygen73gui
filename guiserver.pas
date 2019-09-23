unit guiserver;

interface

uses apitypes, guisvc;

procedure RunGuiServer;
procedure StopGuiServer;

implementation

uses registry, winapi.windows, sysutils, classes, Thrift.Protocol,
    Thrift.Transport, Thrift.Server, Thrift.Socket, Unit1, Vcl.graphics;

var
    _server: IServer;
    _thr: TThread;

    // --- begin TGuiServerHandler ---------------------------------------------------

type
    TGuiServerHandler = class(TInterfacedObject, TGuiSvc.Iface)
    protected
        procedure notifyWriteConsole(const str: System.string);
        procedure notifyStatus(ok: System.Boolean; const str: System.string);
        procedure notifyMeasurement(const measurement: IMeasurement);

    public
        constructor Create;
        destructor Destroy; override;

    end;

constructor TGuiServerHandler.Create;
begin
    inherited Create;
end;

destructor TGuiServerHandler.Destroy;
begin
    inherited Destroy;
end;

procedure TGuiServerHandler.notifyWriteConsole(const str: System.string);
begin
    // OutputDebugStringW(PWideChar('notifyWriteConsole: ' + str));

end;

procedure TGuiServerHandler.notifyStatus(ok: System.Boolean;
const str: System.string);
begin
    TThread.Synchronize(_thr,
        procedure
        begin
            Form1.PanelTop.Font.Color := clNavy;
            if not ok then
                Form1.PanelTop.Font.Color := clRed;
            Form1.PanelTop.Caption := '     ' + TimeToStr(now) + ': ' + str;
        end);

end;

procedure TGuiServerHandler.notifyMeasurement(const measurement: IMeasurement);
begin
    // OutputDebugStringW(PWideChar('notifyMeasurement: '));

end;

// --- end TGuiServerHandler ---------------------------------------------------

procedure StopGuiServer;
begin
    _thr.Suspended := true;
    _server.Stop;
end;

procedure RunGuiServer;
var
    handler: TGuiSvc.Iface;
    processor: IProcessor;
    Transport: IServerTransport;
    key: TRegistry;
begin

    key := TRegistry.Create(KEY_READ);
    try
        if not key.OpenKey('oxygen73\tcp', False) then
            raise Exception.Create('cant open oxygen73\tcp');
        Transport := TServerSocketImpl.Create
          (TServerSocket.Create(key.ReadString('gui_ip'),
          key.ReadInteger('gui_port')));

        // TServerSocketImpl.Create
        // (TServerSocket.Create(key.ReadString('gui_ip'),
        // key.ReadInteger('gui_port'), 1000));
    finally
        key.CloseKey;
        key.Free;
    end;

    handler := TGuiServerHandler.Create;
    processor := TGuiSvc.TProcessorImpl.Create(handler);
    _server := TSimpleServer.Create(processor, Transport);

    _thr := TThread.CreateAnonymousThread(
        procedure
        begin
            try
                _server.Serve();
            except
                on e: Exception do
                    TThread.Synchronize(_thr,
                        procedure
                        begin
                            raise e;
                        end);
            end;
        end);
    _thr.Suspended := False;

end;

end.
