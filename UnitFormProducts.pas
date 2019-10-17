unit UnitFormProducts;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, Thrift.Collections, VclTee.Series,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, apitypes, mainsvc,
    Vcl.ExtCtrls, Vcl.Menus;

type
    TFormProducts = class(TForm)
        StringGrid1: TStringGrid;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        N2: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
        procedure N2Click(Sender: TObject);
    private
        { Private declarations }
        FProducts: IThriftList<IProduct>;
        FPartyID: int64;
        procedure SetupStringGrid;

        procedure DrawCellProduct(ACol, ARow: Integer; Rect: TRect);
        procedure DrawCellAmbientParam(ACol, ARow: Integer; Rect: TRect);

        function CellSeries(ACol, ARow: Integer): TFastLineSeries;

    public
        { Public declarations }
        procedure SetPartyID(APartyID: int64);
        procedure RedrawPlace(APlace: Integer);
        procedure RedrawAmbient;

    end;

var
    FormProducts: TFormProducts;

implementation

{$R *.dfm}

uses mainsvcclient, stringgridutils, stringutils, UnitFormChart;

function newRect(l, t, r, b: Integer): TRect;
begin
    result := Rect(l, t, r, b);
end;

procedure TFormProducts.FormCreate(Sender: TObject);
begin
    //
end;

procedure TFormProducts.N2Click(Sender: TObject);
var
    c, r, place: Integer;
    b: Boolean;
begin
    b := (Sender as TComponent).Tag = 1;
    with StringGrid1, StringGrid1.Selection do
    begin
        for c := Left to Right do
            for r := Top to Bottom do
            begin
                if c < 5 then
                begin
                    place := c * 10 + r;
                    if (place > -1) and (place < 50) then
                        FormChart.FSeriesPlace[place].Active := b;
                end
                else
                begin
                    case r of
                        0:
                            FormChart.FSeriesTemp.Active := b;
                        1:
                            FormChart.FSeriesPress.Active := b;
                        2:
                            FormChart.FSeriesHum.Active := b;
                    end;
                end;
                Cells[c,r] := Cells[c,r];
            end;

    end;
end;

procedure TFormProducts.SetPartyID(APartyID: int64);
begin
    FPartyID := APartyID;
    FProducts := MainSvcApi.listProducts(APartyID);
    SetupStringGrid;
end;

procedure TFormProducts.RedrawAmbient;
begin
    with StringGrid1 do
    begin
        Cells[5, 0] := Cells[5, 0];
        Cells[5, 1] := Cells[5, 1];
        Cells[5, 2] := Cells[5, 2];
    end;

end;

procedure TFormProducts.RedrawPlace(APlace: Integer);
var
    c, r: Integer;
begin
    with StringGrid1 do
        for c := 0 to 4 do
            for r := 0 to 9 do
                if c * 5 + r = APlace then
                    Cells[c, r] := Cells[c, r];

end;

function TFormProducts.CellSeries(ACol, ARow: Integer): TFastLineSeries;
begin
    if ACol < 5 then
        result := FormChart.FSeriesPlace[ACol * 10 + ARow]
    else
        case ARow of
            0:
                result := FormChart.FSeriesTemp;
            1:
                result := FormChart.FSeriesPress;
            2:
                result := FormChart.FSeriesHum;
        else
            result := nil;
        end;

end;

procedure TFormProducts.SetupStringGrid;
Var
    c, r: Integer;
    p: IProduct;
begin
    StringGrid_Clear(StringGrid1, 0, 0);
    with StringGrid1 do
    begin
        ColCount := 6;
        RowCount := 10;
        ColWidths[5] := 180;
        for c := 0 to 4 do
        begin
            ColWidths[c + 0] := 180;
            for r := 0 to 9 do
            begin
                p := FProducts[c * 5 + r];
                if p.Serial <> 0 then
                begin
                    Cells[c, r] := inttostr(p.Serial);
                    if p.place <> c * 5 + r then
                        raise Exception.Create('unexpected');

                end;
            end;
        end;
    end;
end;

procedure TFormProducts.StringGrid1DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;
    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption;
    grd.Canvas.FillRect(Rect);
    if ACol < 5 then
        DrawCellProduct(ACol, ARow, Rect)
    else
        DrawCellAmbientParam(ACol, ARow, Rect);

end;

procedure TFormProducts.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    ACol, ARow: Integer;
    ser: TFastLineSeries;
    count_right_axis:integer;
begin
    if (GetAsyncKeyState(VK_LBUTTON) >= 0) then
        exit;
    StringGrid1.MouseToCell(X, Y, ACol, ARow);
    if X > StringGrid1.CellRect(ACol, ARow).Left + 20 then
        exit;
    ser := CellSeries(ACol, ARow);
    ser.Active := not ser.Active;
    StringGrid1.Cells[ACol, ARow] := StringGrid1.Cells[ACol, ARow];

    FormChart.UpdateRightAxis;


end;

procedure TFormProducts.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
    FormChart.ActiveSeries := CellSeries(ACol, ARow);

end;

procedure TFormProducts.DrawCellAmbientParam(ACol, ARow: Integer; Rect: TRect);
var
    grd: TStringGrid;
    cnv: TCanvas;
    r: TRect;
    product: IProduct;
    brushColor: TColor;
    ser: TFastLineSeries;
    d: Integer;
begin
    case ARow of
        0:
            ser := FormChart.FSeriesTemp;
        1:
            ser := FormChart.FSeriesPress;
        2:
            ser := FormChart.FSeriesHum;
    else
        exit;
    end;
    grd := StringGrid1;
    cnv := grd.Canvas;

    StringGrid_DrawCheckbox2(grd, cnv, Rect, ser.Active);

    r := Rect;

    r.Left := r.Left + 20;
    StringGrid_DrawCellText(StringGrid1, ACol, ARow, r, taLeftJustify,
      ser.Title);

    r.Left := r.Left + 40;
    d := round(r.Top + r.Height / 2);
    brushColor := cnv.Brush.Color;
    cnv.Brush.Color := ser.SeriesColor;
    cnv.FillRect(newRect(r.Left, d - 2, r.Left + 20, d + 2));
    cnv.Brush.Color := brushColor;

    if ser.YValues.Count > 0 then
    begin
        r.Left := r.Left + 60;
        StringGrid_DrawCellText(StringGrid1, ACol, ARow, r, taLeftJustify,
          FloatToStr(ser.YValues[ser.YValues.Count - 1]));
    end;

end;

procedure TFormProducts.DrawCellProduct(ACol, ARow: Integer; Rect: TRect);
var
    grd: TStringGrid;
    cnv: TCanvas;
    r: TRect;
    place, d: Integer;
    product: IProduct;
    brushColor: TColor;
    ser: TFastLineSeries;
begin
    place := ACol * 10 + ARow;
    if not Assigned(FProducts) or (place >= FProducts.Count) then
    begin
        exit;
    end;

    product := FProducts[place];
    grd := StringGrid1;
    cnv := grd.Canvas;
    ser := FormChart.FSeriesPlace[place];

    StringGrid_DrawCheckbox2(grd, cnv, Rect, ser.Active);

    r := Rect;
    r.Left := r.Left + 20;

    cnv.Font.Style := [fsBold];
    StringGrid_DrawCellText(StringGrid1, ACol, ARow, r, taLeftJustify,
      inttostr(ACol * 10 + ARow + 1));
    cnv.Font.Style := [];

    r.Left := r.Left + 25;

    d := round(r.Top + r.Height / 2);
    brushColor := cnv.Brush.Color;
    cnv.Brush.Color := ser.SeriesColor;
    cnv.FillRect(newRect(r.Left, d - 2, r.Left + 20, d + 2));
    cnv.Brush.Color := brushColor;

    r.Left := r.Left + 20;

    if product.Serial <> 0 then
    begin
        cnv.Font.Style := [fsItalic];
        StringGrid_DrawCellText(StringGrid1, ACol, ARow, r, taLeftJustify,
          inttostr(product.Serial));
        cnv.Font.Style := [];
    end;

    if ser.YValues.Count > 0 then
    begin
        r.Left := r.Left + 60;
        StringGrid_DrawCellText(StringGrid1, ACol, ARow, r, taLeftJustify,
          FloatToStr(ser.YValues[ser.YValues.Count - 1]));
    end;
    // StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);
end;

end.
