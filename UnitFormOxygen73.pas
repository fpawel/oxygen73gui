unit UnitFormOxygen73;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MainSvcClient,
    Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.ExtCtrls, System.ImageList,
    Vcl.ImgList, VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.TeeProcs,
    VclTee.Chart;

type
    TFormOxygen73 = class(TForm)
        ImageList4: TImageList;
        PanelMain: TPanel;
        Panel2: TPanel;
        Splitter2: TSplitter;
        Panel3: TPanel;
        Panel4: TPanel;
        PanelTop: TPanel;
        ToolBar3: TToolBar;
        ToolButton1: TToolButton;
        ToolButton4: TToolButton;
        Panel5: TPanel;
        Panel6: TPanel;
        Splitter1: TSplitter;
    ToolButton2: TToolButton;
        procedure FormShow(Sender: TObject);
        procedure Splitter1Moved(Sender: TObject);
        procedure Splitter2Moved(Sender: TObject);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    private
        { Private declarations }
        procedure HandleStatus(ok: Boolean; s, det: string);
        procedure HandleCopydata(var Message: TMessage); message WM_COPYDATA;
        function ExceptionDialog(e: Exception): Boolean;
    public
        { Public declarations }
        procedure AppException(Sender: TObject; e: Exception);
    end;

var
    FormOxygen73: TFormOxygen73;

implementation

uses apitypes, mainsvc, JclDebug, vclutils, UnitFormCatalogue, UnitFormProducts,
    UnitFormChart,
    logfile, api.notify, Thrift.Transport, UnitFormJournal;

{$R *.dfm}

procedure TFormOxygen73.FormCreate(Sender: TObject);
begin
    Application.OnException := AppException;
end;

procedure TFormOxygen73.FormShow(Sender: TObject);
begin
    Self.OnShow := nil;

    try
        MainSvcClient.Connect;
    except
        on e: Exception do
        begin
            if ExceptionDialog(e) then
                Close
            else
                FormShow(Sender);
            exit;
        end;

    end;

    with FormCatalogue do
    begin
        Parent := Panel4;
        Font.Assign(Self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        FetchYearsMonths;
        Show;
    end;

    with FormProducts do
    begin
        Parent := Panel3;
        Font.Assign(Self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        // SetPartyID(-1);
        Show;
    end;

    with FormChart do
    begin
        Parent := Panel2;
        Font.Assign(Self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;

    api.notify.HandleWriteConsole := FormJournal.NewEntry;
    api.notify.HandleMeasurement := procedure(m: TMeasurement)
        begin
        end;
    api.notify.HandleStatus := procedure(x: TStatusMessage)
        begin
            HandleStatus(x.ok, x.Text, x.Detail);
        end;
    api.notify.Initialize;

end;

procedure TFormOxygen73.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    api.notify.Finalize;
    //SendMessage(FindWindow('Oxygen73WindowClass', ''), WM_CLOSE, 0,0);
end;

procedure TFormOxygen73.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    FormChart.ChangeAxisOrder(GetVCLControlAtPos(Self, MousePos), WheelDelta);
end;

procedure TFormOxygen73.Splitter1Moved(Sender: TObject);
begin
    Repaint;
end;

procedure TFormOxygen73.Splitter2Moved(Sender: TObject);
begin
    OutputDebugStringW(PWideChar(Inttostr(FormProducts.StringGrid1.Height)));
end;

procedure TFormOxygen73.ToolButton1Click(Sender: TObject);
begin
    Formjournal.Show;
end;

function TFormOxygen73.ExceptionDialog(e: Exception): Boolean;
begin
    result := MessageBox(Handle, PChar(e.ClassName + #10#10 + e.Message +
      #10#10), PChar(ExtractFileName(Application.ExeName)),
      MB_ABORTRETRYIGNORE or MB_ICONERROR) = IDABORT;

end;

procedure TFormOxygen73.AppException(Sender: TObject; e: Exception);
begin
    LogfileWriteException(e);
    if ExceptionDialog(e) then
        Close;
end;

procedure TFormOxygen73.HandleStatus(ok: Boolean; s, det: string);
begin
    Panel5.Font.Color := clRed;
    if ok then
        Panel5.Font.Color := clNavy;
    Panel5.Caption := TimeToStr(now);
    Panel6.Caption := s;
    Panel6.Hint := det;
    Panel6.ShowHint := True;
end;

procedure TFormOxygen73.HandleCopydata(var Message: TMessage);
begin
    api.notify.HandleCopydata(Message);
end;

end.
