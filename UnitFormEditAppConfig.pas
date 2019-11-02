unit UnitFormEditAppConfig;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
    System.ImageList, Vcl.ImgList, Vcl.StdCtrls;

type
    TFormEditAppConfig = class(TForm)
        ImageList4: TImageList;
        ToolBarStop: TToolBar;
        ToolButton2: TToolButton;
        RichEdit1: TRichEdit;
    ToolButton3: TToolButton;
        procedure ToolButton2Click(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure RichEdit1Change(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    FormEditAppConfig: TFormEditAppConfig;

implementation

{$R *.dfm}

uses Winapi.RichEdit, System.Character, MainSvcClient;

procedure TFormEditAppConfig.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormEditAppConfig.FormShow(Sender: TObject);
begin
    RichEdit1.Text :=  MainSvcApi.getAppConfigYaml;
end;

procedure TFormEditAppConfig.RichEdit1Change(Sender: TObject);
begin
    ToolButton2.Enabled := true;
end;

procedure TFormEditAppConfig.ToolButton2Click(Sender: TObject);
var
    ASelStart : integer;
begin
    ASelStart := RichEdit1.SelStart;
    MainSvcApi.setAppConfigYaml(RichEdit1.Text);
    RichEdit1.Text := MainSvcApi.getAppConfigYaml;
    RichEdit1.SelStart := ASelStart;
     RichEdit1.SelLength := 0 ;
     ToolButton2.Enabled := false;
end;

procedure TFormEditAppConfig.ToolButton3Click(Sender: TObject);
var
    ASelStart : integer;
begin
    ASelStart := RichEdit1.SelStart;
    RichEdit1.Text :=  MainSvcApi.getAppConfigYaml;
    ToolButton2.Enabled := false;
    RichEdit1.SelStart := ASelStart;
     RichEdit1.SelLength := 0 ;
end;

end.
