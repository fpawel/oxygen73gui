unit UnitFormEditSerialsDialog;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    apitypes, mainsvc, Thrift.Collections, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.Grids;

type
    TFormEditSerialsDialog = class(TForm)
        StringGrid1: TStringGrid;
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure FormCreate(Sender: TObject);
        procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
          const Value: string);
    procedure FormShow(Sender: TObject);
    private
        { Private declarations }
        FProducts: IThriftList<IProduct>;
        Last_Edited_Col, Last_Edited_Row: Integer;
        procedure SetupStringGrid;
    public
        { Public declarations }

    end;

var
    FormEditSerialsDialog: TFormEditSerialsDialog;

implementation

uses mainsvcclient, stringgridutils, UnitFormCatalogue;

{$R *.dfm}

procedure TFormEditSerialsDialog.FormCreate(Sender: TObject);
var
    c, r: Integer;
begin
    with StringGrid1 do
    begin
        ColCount := 5;
        RowCount := 10;
        FixedCols := 0;
        FixedRows := 0;
    end;
end;

procedure TFormEditSerialsDialog.FormShow(Sender: TObject);
begin
    FProducts := MainSvcApi.listLastPartyProducts;
    SetupStringGrid;
end;

procedure TFormEditSerialsDialog.StringGrid1DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    place: Integer;
    r: TRect;
begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);

    cnv.Font.Color := clBlack;
    cnv.Pen.Color := $00BCBCBC;
    cnv.Pen.Width := 1;
    cnv.Brush.Color := cl3DLight;
    cnv.FillRect(Rect);

    place := ACol * 10 + ARow;

    r := Rect;
    StringGrid_DrawCellText(grd, ACol, ARow, r, taLeftJustify,
      inttostr(place + 1));
    r.Left := r.Left + 30;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else
        cnv.Brush.Color := clWhite;
    cnv.FillRect(r);

    cnv.MoveTo(r.Left - 1, r.Top);
    cnv.LineTo(r.Left - 1, r.Bottom);

    StringGrid_DrawCellText(grd, ACol, ARow, r, taLeftJustify,
      grd.Cells[ACol, ARow]);
    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);

end;

procedure TFormEditSerialsDialog.StringGrid1SetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
    v: Integer;
    r: TRect;
begin
    
    With StringGrid1 do
    begin
        if EditorMode then
        begin
            Last_Edited_Col := ACol; // Remember column of cell being edited
            Last_Edited_Row := ARow; // Remember row of cell being edited
        end;
        Last_Edited_Col := -1; // Indicate no cell is edited
        Last_Edited_Row := -1; // Indicate no cell is edited
        // Do whatever wanted after user has finish editing a cell
        OnSetEditText := nil;
        try
            if TryStrToInt(Value, v) = true then
                MainSvcApi.setLastPartyProductSerialAtPlace(ACol * 10 + ARow, v)
            else
                Cells[ACol,ARow] := '';
        finally
            OnSetEditText := StringGrid1SetEditText;
        end;
    end;

end;



procedure TFormEditSerialsDialog.SetupStringGrid;
Var
    c, r: Integer;
    p: IProduct;
begin
    with StringGrid1 do
    begin
        for c := 0 to 4 do
        begin
            for r := 0 to 9 do
            begin
                p := FProducts[c * 5 + r];
                if p.Serial <> 0 then
                    Cells[c, r] := inttostr(p.Serial)
                else
                    Cells[c, r] := '';
            end;
        end;
    end;
end;

end.
