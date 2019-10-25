unit UnitFormAppConfig;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, System.Generics.collections,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
    EWrongInputExcpetion = class(Exception);

    TFormAppConfig = class(TForm)
        GroupBox2: TGroupBox;
        Panel1: TPanel;
        Shape1: TShape;
        Panel2: TPanel;
        ComboBoxComport: TComboBox;
        Panel17: TPanel;
        Panel18: TPanel;
        ComboBoxComportHumidity: TComboBox;
        procedure FormCreate(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ComboBoxComportChange(Sender: TObject);
    private
        { Private declarations }
        FEnableOnEdit: boolean;

    public
        { Public declarations }
    end;

var
    FormAppConfig: TFormAppConfig;

implementation

{$R *.dfm}

uses stringutils, apitypes, registry, MainSvcClient;

procedure EnumComports(const Ports: TStrings);
var
    nInd: integer;
begin
    with TRegistry.Create(KEY_READ) do
        try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKey('hardware\devicemap\serialcomm', false) then
            begin
                Ports.BeginUpdate();
                GetValueNames(Ports);
                for nInd := Ports.count - 1 downto 0 do
                    Ports.Strings[nInd] := ReadString(Ports.Strings[nInd]);
            end;

        finally
            Ports.EndUpdate();
            CloseKey();
            Free();
        end
end;

procedure setupCB(cb: TComboBox; s: string);
begin
    cb.ItemIndex := cb.Items.IndexOf(s);
end;

procedure TFormAppConfig.FormCreate(Sender: TObject);
begin
    FEnableOnEdit := false;
end;

procedure TFormAppConfig.FormShow(Sender: TObject);
var
    s: string;
    v: IAppConfig;
begin
    FEnableOnEdit := false;
    EnumComports(ComboBoxComport.Items);
    EnumComports(ComboBoxComportHumidity.Items);
    v := MainSvcApi.getAppConfig();

    setupCB(ComboBoxComport, v.Comport);
    setupCB(ComboBoxComportHumidity, v.ComportHumidity);

    FEnableOnEdit := true;
end;

procedure TFormAppConfig.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormAppConfig.ComboBoxComportChange(Sender: TObject);
var
    v: IAppConfig;
begin
    if not FEnableOnEdit then
        exit;
    v := MainSvcApi.getAppConfig();
    v.Comport := ComboBoxComport.Text;
    v.ComportHumidity := ComboBoxComportHumidity.Text;
    MainSvcApi.setAppConfig(v);
end;

end.
