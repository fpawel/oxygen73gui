unit UnitFormOxygen73;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MainSvcClient,
    Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.ExtCtrls, System.ImageList,
    Vcl.ImgList, VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.TeeProcs,
    VclTee.Chart, Vcl.Menus, Vcl.Imaging.pngimage, UnitFormConsole;

type

    TStatusMessage = record
    public
        Ok: Boolean;
        Text: string;
    end;

    TFormOxygen73 = class(TForm)
        ImageList4: TImageList;
        PanelMain: TPanel;
        Panel2: TPanel;
        Splitter2: TSplitter;
        Panel3: TPanel;
        Panel4: TPanel;
        PanelBottom: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Splitter1: TSplitter;
        MainMenu1: TMainMenu;
        N1: TMenuItem;
        N3: TMenuItem;
        N6: TMenuItem;
        N7: TMenuItem;
        N8: TMenuItem;
        Panel7: TPanel;
        Panel8: TPanel;
        N9: TMenuItem;
        GroupBox2: TGroupBox;
        PanelMessageBox: TPanel;
        ImageInfo: TImage;
        N2: TMenuItem;
        procedure FormShow(Sender: TObject);
        procedure Splitter1Moved(Sender: TObject);
        procedure Splitter2Moved(Sender: TObject);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure N6Click(Sender: TObject);
        procedure N8Click(Sender: TObject);
        procedure N9Click(Sender: TObject);
        procedure N3Click(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure N2Click(Sender: TObject);
    private
        { Private declarations }
        FEnableCopyData: Boolean;
        procedure HandleStatusComport(m: TStatusMessage);
        procedure HandleStatusComportHum(m: TStatusMessage);

        procedure HandleCopydata(var Message: TMessage); message WM_COPYDATA;
        function ExceptionDialog(e: Exception): Boolean;
    public
        { Public declarations }
        procedure AppException(Sender: TObject; e: Exception);
        procedure ShowModalPanel(AText: string);
        procedure HideModalPanel;

    end;

var
    FormOxygen73: TFormOxygen73;

implementation

uses Grijjy.Bson, Grijjy.Bson.Serialization, dateutils, vclutils, JclDebug,
    UnitFormCatalogue, UnitFormProducts,
    UnitFormChart, unitmeasurement, math, logfile, Thrift.Transport,
    stringgridutils, myutils,
    apitypes, UnitFormEditSerialsDialog, UnitFormAppConfig,
    UnitFormEditAppConfig, UnitFormFoundProducts, UnitAppIni;

{$R *.dfm}

type
    TJsonCD = class
        class function unmarshal<T>(Message: TMessage): T; static;
    end;

    TCopyDataCmd = (cdcWriteConsole, cdcStatusComport, cdcStatusComportHum,
      cdcNewMeasurements, cdcMeasurements, cdcProductMeasurements,
      cdcErrorOccurred, cdcVacuumBegin, cdcVacuumEnd);

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
        Parent := GroupBox2;
        Font.Assign(Self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        // ReloadData;
        Show;
    end;

    with FormAppConfig do
    begin
        Parent := Panel4;
        Font.Assign(Self.Font);
        BorderStyle := bsNone;
        Align := alTop;
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
    FormChart.Save;
    // if Length(GetEnvironmentVariable('OXYGEN73_DEV_MODE')) > 0 then
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
            FormConsole.NewLine(getCopyDataStr(Message));
        cdcStatusComport:
            HandleStatusComport(TJsonCD.unmarshal<TStatusMessage>(Message));
        cdcStatusComportHum:
            HandleStatusComportHum(TJsonCD.unmarshal<TStatusMessage>(Message));
        cdcNewMeasurements:
            FormCatalogue.HandleNewMeasurements
              (TMeasurements.deserialize(cd.lpData));

        cdcMeasurements:
            FormCatalogue.HandleMeasurements
              (TMeasurements.deserialize(cd.lpData));

        cdcProductMeasurements:
            FormFoundProducts.HandleMeasurements
              (TProductMeasurements.deserialize(cd.lpData));

        cdcErrorOccurred:
            AppException(Self, Exception.Create(getCopyDataStr(Message)));

        cdcVacuumBegin:
            ShowModalPanel('Выполняется дефрагментация');

        cdcVacuumEnd:
            HideModalPanel;
    else
        raise Exception.Create('wrong message: ' + IntToStr(Message.WParam));
    end;
end;

procedure TFormOxygen73.ShowModalPanel(AText: string);
begin
    PanelMessageBox.Caption := AText;
    PanelMain.Enabled := false;
    PanelMessageBox.Show;
    PanelMessageBox.BringToFront;
    OnResize(Self);
end;

procedure TFormOxygen73.HideModalPanel;
begin
    PanelMessageBox.Hide;
    PanelMain.Enabled := true;
end;

procedure TFormOxygen73.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    FormChart.ChangeAxisOrder(GetVCLControlAtPos(Self, MousePos), WheelDelta);

end;

procedure TFormOxygen73.FormResize(Sender: TObject);
begin
    if PanelMessageBox.Visible then
    begin
        PanelMessageBox.Left := ClientWidth div 2 - PanelMessageBox.Width div 2;
        PanelMessageBox.Top := ClientHeight div 2 -
          PanelMessageBox.Height div 2;
    end;
end;

procedure TFormOxygen73.Splitter1Moved(Sender: TObject);
begin
    Repaint;
end;

procedure TFormOxygen73.Splitter2Moved(Sender: TObject);
begin
    OutputDebugStringW(PWideChar(IntToStr(FormProducts.StringGrid1.Height)));
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

procedure TFormOxygen73.HandleStatusComport(m: TStatusMessage);
begin

    if m.Ok then
        Panel6.Font.Color := clNavy
    else
        Panel6.Font.Color := clRed;
    Panel5.Caption := TimeToStr(now);
    Panel6.Caption := m.Text;
    Panel6.Hint := m.Text;
    Panel6.ShowHint := true;
end;

procedure TFormOxygen73.HandleStatusComportHum(m: TStatusMessage);
begin

    if m.Ok then
        Panel8.Font.Color := clNavy
    else
        Panel8.Font.Color := clRed;
    Panel7.Caption := TimeToStr(now);
    Panel8.Caption := m.Text;
    Panel8.Hint := m.Text;
    Panel8.ShowHint := true;
end;

procedure TFormOxygen73.N1Click(Sender: TObject);
begin
    FormEditAppConfig.Position := poScreenCenter;
    FormEditAppConfig.ShowModal;
end;

procedure TFormOxygen73.N2Click(Sender: TObject);
begin
    MainSvcApi.vacuum;
end;

procedure TFormOxygen73.N3Click(Sender: TObject);
begin
    FormConsole.Position := poScreenCenter;
    FormConsole.Show;

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
    FormEditSerialsDialog.Position := poScreenCenter;
    FormEditSerialsDialog.ShowModal;
end;

procedure TFormOxygen73.N9Click(Sender: TObject);
var
    value: string;
    serial: Integer;
begin
    value := AppIni.ReadString('input', 'serial', '');
    if not InputQuery('Поиск ЭХЯ по номеру', 'Введите серийный номер ЭХЯ', value)
    then
        exit;
    AppIni.WriteString('input', 'serial', value);
    serial := StrToInt(value);
    FormFoundProducts.Upload(serial);
    FormFoundProducts.ShowModal;

end;

end.
