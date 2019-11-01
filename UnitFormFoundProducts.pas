unit UnitFormFoundProducts;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Grids, apitypes,
    Thrift.Collections, Vcl.ExtCtrls, UnitFormChart, UnitMeasurement;

type
    TFormFoundProducts = class(TForm)
        StringGrid1: TStringGrid;
        Splitter1: TSplitter;
    Panel2: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
    private
        { Private declarations }
        FProductBuckets: IThriftList<IProductBucket>;
        FFormChart: TFormChart;
        FSelectedIProductBucket:IProductBucket;
    public
        { Public declarations }
        procedure Upload(serial: Integer);
        procedure HandleMeasurements(ms: TProductMeasurements);
    end;

var
    FormFoundProducts: TFormFoundProducts;

implementation

{$R *.dfm}

uses MainSvcClient, myutils, dateutils, stringgridutils, UnitFormProducts;

procedure TFormFoundProducts.FormCreate(Sender: TObject);
var
    I: TFormChart;
begin
    with StringGrid1 do
    begin
        ColCount := 6;
        Cells[0, 0] := 'ЭХЯ';
        Cells[1, 0] := 'Место';
        Cells[2, 0] := 'Партия';
        Cells[3, 0] := 'Загрузка';
        Cells[4, 0] := 'Начало';
        Cells[5, 0] := 'Конец';
        ColWidths[0] := 30;
        ColWidths[1] := 50;
        ColWidths[2] := 70;
        ColWidths[3] := 120;
        ColWidths[4] := 120;
        ColWidths[5] := 120;
    end;
    FFormChart := nil;
    FSelectedIProductBucket := nil;

end;

procedure TFormFoundProducts.StringGrid1DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;

begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if gdFixed in State then
        cnv.Brush.Color := cl3DLight;

    ta := taLeftJustify;

    StringGrid_DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
    // StringGrid_DrawCellBounds(cnv, acol, arow,  Rect);
end;

procedure TFormFoundProducts.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    p: IProductBucket;
    I: Integer;
begin

    if ARow <= 0 then
    begin
        FSelectedIProductBucket := nil;
        exit;
    end;

    p := FProductBuckets[ARow - 1];

    if FSelectedIProductBucket = p then
        exit;

    FSelectedIProductBucket := p;

    if Assigned(FFormChart) then
        FFormChart.Free;

    FFormChart := TFormChart.Create(self);
    with FFormChart do
    begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsNone;
        Font.Assign(self.Font);

        for I := 0 to 49 do
            Chart1.RemoveSeries(FSeriesPlace[I]);
        Chart1.AddSeries(FSeriesPlace[p.Place]);

        FSeriesTemp.Active := true;
        FSeriesPress.Active := true;
        FSeriesHum.Active := true;
        FSeriesPlace[p.Place].Active := true;

        Chart1.Legend.Font.Assign(self.Font);
        Chart1.Legend.Show;
        UpdateRightAxis;
    end;
    Panel2.Show;
    MainSvcApi.requestProductMeasurements(p.BucketID, p.Place);
    StringGrid1.Enabled := false;
end;

procedure TFormFoundProducts.Upload(serial: Integer);
var
    I: Integer;
    p: IProductBucket;
    canselect:BOOLean;

    function fmtdt(t: TTimeUnixMillis; inch: int64): string;
    begin
        result := FormatDateTime('dd/mm/yy hh:nn',
          IncHour(unixMillisToDateTime(p.PartyCreatedAt), inch));
    end;

begin
    FProductBuckets := MainSvcApi.findProductsBySerial(serial);
    with StringGrid1 do
    begin
        RowCount := FProductBuckets.Count + 1;
        if RowCount = 1 then
            exit;
        FixedRows := 1;

        for I := 0 to FProductBuckets.Count - 1 do
        begin
            p := FProductBuckets[I];
            Cells[0, I + 1] := IntToStr(p.ProductID);
            Cells[1, I + 1] := IntToStr(p.Place);
            Cells[2, I + 1] := IntToStr(p.PartyID);
            Cells[3, I + 1] := fmtdt(p.PartyCreatedAt, 0);

            Cells[4, I + 1] := fmtdt(p.BucketCreatedAt, -3);
            Cells[5, I + 1] := fmtdt(p.BucketUpdatedAt, -3);
        end;
        StringGrid_SetupColumnsWidth(StringGrid1);
        Width := ColWidths[0] + ColWidths[1] + ColWidths[2] + ColWidths[3] +
          ColWidths[4] + ColWidths[5] + 10;
    end;
    Caption := 'Поиск ЭХЯ ' + IntToStr(serial);

    StringGrid1SelectCell(StringGrid1, 0, StringGrid1.Row, CanSelect);

end;

procedure TFormFoundProducts.HandleMeasurements(ms: TProductMeasurements);
var
    I: Integer;
    m : TProductMeasurement;
begin
    StringGrid1.Enabled := true;

    if not Assigned(FFormChart) or not Assigned(FSelectedIProductBucket) or
    (FSelectedIProductBucket.BucketID <> ms.BucketID) then
        exit;

    FFormChart.FSeriesTemp.Clear;
    FFormChart.FSeriesPress.Clear;
    FFormChart.FSeriesHum.Clear;
    FFormChart.FSeriesPlace[FSelectedIProductBucket.Place].Clear;
    for m in ms.Measurements do
        FFormChart.AddProductMeasurement(m, FSelectedIProductBucket.Place);
    FFormChart.Show;
end;

end.
