unit UnitFormPopup;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.grids, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
    TFormPopup = class(TForm)
    ImageError: TImage;
    ImageInfo: TImage;
    Memo1: TMemo;
        procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure SetText(s:string);
        procedure ShowStringGridCellText(AStringGrid: TStringGrid);
        procedure ShowAtStringGridCell(AStringGrid: TStringGrid);
    end;

var
    FormPopup: TFormPopup;

implementation

{$R *.dfm}

procedure TFormPopup.FormActivate(Sender: TObject);
begin
    HideCaret(Memo1.Handle);
end;

procedure TFormPopup.FormCreate(Sender: TObject);
begin
    //
end;

procedure TFormPopup.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormPopup.FormPaint(Sender: TObject);
begin
    HideCaret(meMo1.Handle)
end;

procedure TFormPopup.SetText(s:string);
begin
    meMo1.Text := stringreplace(s, ': ', #13#10#9' - ', [rfReplaceAll, rfIgnoreCase]);
end;


procedure TFormPopup.ShowAtStringGridCell(AStringGrid: TStringGrid);
var
    r: TRect;
    pt: TPoint;
begin
    r := AStringGrid.CellRect(AStringGrid.Col, AStringGrid.Row);
    pt := AStringGrid.ClientToScreen(r.TopLeft);
    Left := pt.x + 3;
    Top := pt.y + AStringGrid.RowHeights[AStringGrid.Row] + 3;
    Show;
end;

procedure TFormPopup.ShowStringGridCellText(AStringGrid: TStringGrid);
begin
    SetText(AStringGrid.Cells[AStringGrid.Col,
      AStringGrid.Row]);
    ImageError.Hide;
    ImageInfo.Show;
    ShowAtStringGridCell(AStringGrid);
end;

end.
