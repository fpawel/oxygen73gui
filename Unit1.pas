unit Unit1;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, MainSvcClient,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.ExtCtrls, System.ImageList,
  Vcl.ImgList, VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart;

type
    TForm1 = class(TForm)
    ImageList4: TImageList;
    PanelTop: TPanel;
    ToolBar3: TToolBar;
    ToolButton1: TToolButton;
    ToolButton4: TToolButton;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Panel3: TPanel;
    Panel4: TPanel;
        procedure FormShow(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    Form1: TForm1;

implementation

uses apitypes,mainsvc, vclutils, UnitFormCatalogue,UnitFormProducts, UnitFormChart,
   guiserver;

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    MainSvcApi.closeClient;
    StopGuiServer;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    FormChart.ChangeAxisOrder(GetVCLControlAtPos(self, MousePos),
      WheelDelta);
end;

procedure TForm1.FormShow(Sender: TObject);

begin
    Self.OnShow := nil;
    guiserver.RunGuiServer;
    MainSvcClient.Connect;
    MainSvcApi.openClient;



    with FormCatalogue do
    begin
        Parent := Panel4;
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        FetchYearsMonths;
        Show;
    end;

    with FormProducts do
    begin
        Parent := Panel3;
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        //SetPartyID(-1);
        Show;
    end;

    with FormChart do
    begin
        Parent := Panel2;
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;
end;

procedure TForm1.Splitter1Moved(Sender: TObject);
begin
    Repaint;
end;

procedure TForm1.Splitter2Moved(Sender: TObject);
begin
    OutputDebugStringW(PWideChar(Inttostr(FormProducts.StringGrid1.Height)));
end;

end.
