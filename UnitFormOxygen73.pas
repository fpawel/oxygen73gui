unit UnitFormOxygen73;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MainSvcClient,
    Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.ExtCtrls, System.ImageList,
    Vcl.ImgList, VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.TeeProcs,
    VclTee.Chart, Vcl.Menus, Vcl.Imaging.pngimage;

type
    TCopyDataCmd = (cdcWriteConsole, cdcStatus, cdcNewMeasurements,
      cdcMeasurements);

    TStatusMessage = record
    public
        Ok: Boolean;
        Text: string;
        Detail: string;
    end;

    TFormOxygen73 = class(TForm)
        ImageList4: TImageList;
        PanelMain: TPanel;
        Panel2: TPanel;
        Splitter2: TSplitter;
        Panel3: TPanel;
        Panel4: TPanel;
        PanelTop: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Splitter1: TSplitter;
        MainMenu1: TMainMenu;
        N1: TMenuItem;
        N2: TMenuItem;
        N3: TMenuItem;
        N4: TMenuItem;
        N5: TMenuItem;
        N6: TMenuItem;
        N7: TMenuItem;
        N8: TMenuItem;
        procedure FormShow(Sender: TObject);
        procedure Splitter1Moved(Sender: TObject);
        procedure Splitter2Moved(Sender: TObject);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure ToolButton1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure N6Click(Sender: TObject);
        procedure N8Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
    procedure N5Click(Sender: TObject);
    private
        { Private declarations }
        FEnableCopyData: Boolean;
        procedure HandleStatus(m: TStatusMessage);

        procedure HandleCopydata(var Message: TMessage); message WM_COPYDATA;
        function ExceptionDialog(e: Exception): Boolean;
    public
        { Public declarations }
        procedure AppException(Sender: TObject; e: Exception);
    end;

var
    FormOxygen73: TFormOxygen73;

implementation

uses Grijjy.Bson, Grijjy.Bson.Serialization, dateutils, JclDebug, vclutils,
    UnitFormCatalogue, UnitFormProducts,
    UnitFormChart, unitmeasurement,
    math, logfile, Thrift.Transport, UnitFormJournal, stringgridutils, myutils,
    apitypes, UnitFormEditSerialsDialog, UnitFormAppConfig,
  UnitFormEditAppConfigToml;

{$R *.dfm}

type
    TJsonCD = class
        class function unmarshal<T>(Message: TMessage): T; static;
    end;

function getCopyDataStr(Message: TMessage): string;
var
    cd: PCOPYDATASTRUCT;
    cmd: TCopyDataCmd;
begin
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := TCopyDataCmd(Message.WParam);
    SetString(Result, PWideChar(cd.lpData), cd.cbData div 2);
end;

class function TJsonCD.unmarshal<T>(Message: TMessage): T;
begin
    TgoBsonSerializer.deserialize(getCopyDataStr(Message), Result);
end;

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

    FEnableCopyData := true;

end;

procedure TFormOxygen73.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FEnableCopyData := false;
    // SendMessage(FindWindow('Oxygen73WindowClass', ''), WM_CLOSE, 0,0);
end;

procedure TFormOxygen73.HandleCopydata(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
begin
    if FEnableCopyData = false then
        exit;
    cd := PCOPYDATASTRUCT(Message.LParam);
    Message.Result := 1;
    case TCopyDataCmd(Message.WParam) of
        cdcWriteConsole:
            FormJournal.NewEntry(getCopyDataStr(Message));
        cdcStatus:
            HandleStatus(TJsonCD.unmarshal<TStatusMessage>(Message));
        cdcNewMeasurements:
            FormCatalogue.HandleNewMeasurements
              (TMeasurement.DeserializeMeasurements(cd.lpData));
        cdcMeasurements:
            FormCatalogue.HandleMeasurements
              (TMeasurement.DeserializeMeasurements(cd.lpData));
    else
        raise Exception.Create('wrong message: ' + IntToStr(Message.WParam));
    end;
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
    OutputDebugStringW(PWideChar(IntToStr(FormProducts.StringGrid1.Height)));
end;

procedure TFormOxygen73.ToolButton1Click(Sender: TObject);
begin
    FormJournal.Show;
end;

function TFormOxygen73.ExceptionDialog(e: Exception): Boolean;
begin
    Result := MessageBox(Handle, PChar(e.ClassName + #10#10 + e.Message +
      #10#10), PChar(ExtractFileName(Application.ExeName)),
      MB_ABORTRETRYIGNORE or MB_ICONERROR) = IDABORT;
end;

procedure TFormOxygen73.AppException(Sender: TObject; e: Exception);
begin
    LogfileWriteException(e);
    if ExceptionDialog(e) then
        Close;
end;

procedure TFormOxygen73.HandleStatus(m: TStatusMessage);
begin

    if m.Ok then
        Panel5.Font.Color := clNavy
    else
        Panel5.Font.Color := clRed;
    Panel6.Font.Color := Panel5.Font.Color;
    Panel5.Caption := TimeToStr(now);
    Panel6.Caption := m.Text;
    Panel6.Hint := m.Detail;
    Panel6.ShowHint := true;
end;

procedure TFormOxygen73.N4Click(Sender: TObject);
begin
    FormAppConfig.Position := poScreenCenter;
    FormAppConfig.ShowModal;
end;

procedure TFormOxygen73.N5Click(Sender: TObject);
begin
    FormEditAppConfigToml.Position := poScreenCenter;
    FormEditAppConfigToml.ShowModal;
end;

procedure TFormOxygen73.N6Click(Sender: TObject);
begin
    FormEditSerialsDialog.Position := poScreenCenter;
    FormEditSerialsDialog.ShowModal;
    FormCatalogue.ComboBox1Change(FormCatalogue.ComboBox1);
end;

procedure TFormOxygen73.N8Click(Sender: TObject);
begin
    if MessageBox(Handle, 'Подтвердите необходимость создания новой партии.',
      'Запрос подтверждения', mb_IconQuestion or mb_YesNo) <> mrYes then
        exit;
    MainSvcApi.createNewParty;
    FormCatalogue.FetchYearsMonths;
end;

end.
