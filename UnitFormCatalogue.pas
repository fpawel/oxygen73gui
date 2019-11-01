unit UnitFormCatalogue;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
    mainsvc, Thrift.Collections, apitypes, MainSvcClient, stringgridutils,
    stringutils, UnitMeasurement;

type
    TFormCatalogue = class(TForm)
        StringGrid1: TStringGrid;
        Panel1: TPanel;
        ComboBox1: TComboBox;
        procedure ComboBox1Change(Sender: TObject);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
        FYearMonth: IThriftList<IYearMonth>;
        FSelectedBucket: IBucket;
        FBuckets: IThriftList<IBucket>;
        function getLastBucket: IBucket;
    public
        { Public declarations }
        property SelectedBucket: IBucket read FSelectedBucket;
        // property LastBucket: IBucket read getLastBucket;
        procedure ReloadData;
        procedure HandleNewMeasurements(ms: TMeasurements);
        procedure HandleMeasurements(ms: TMeasurements);
    end;

var
    FormCatalogue: TFormCatalogue;

implementation

{$R *.dfm}

uses myutils, dateutils, UnitFormOxygen73, UnitFormProducts, math,
    UnitFormChart, UnitFormEditSerialsDialog;

function formatPartyTime(t: int64): string;
begin
    result := FormatDateTime('dd/mm hh:nn', unixMillisToDateTime(t))
end;

procedure TFormCatalogue.FormCreate(Sender: TObject);
begin
    FSelectedBucket := nil;

    with StringGrid1 do
    begin
        ColCount := 5;
        Cells[0, 0] := 'День';
        Cells[1, 0] := 'Начало';
        Cells[2, 0] := 'Конец';
        Cells[3, 0] := 'Партия';
        Cells[4, 0] := 'Загрузка';
        ColWidths[0] := 40;
        ColWidths[1] := 70;
        ColWidths[2] := 70;
        ColWidths[3] := 70;
        ColWidths[4] := 120;
    end;
end;

procedure TFormCatalogue.HandleNewMeasurements(ms: TMeasurements);
var
    I: Integer;
    value: double;
    m: TMeasurement;
begin
    ReloadData;
    if not Assigned(FSelectedBucket) or (FSelectedBucket.BucketID <> ms.BucketID)
    then
        exit;

    FBuckets[FBuckets.Count - 1].UpdatedAt :=
      DateTimeToUnixMillis(IncHour(now, 3));
    with FormCatalogue.StringGrid1 do
        Cells[2, RowCount - 1] := TimeToStr(now);
    for m in ms.Measurements do
        FormChart.AddMeasurement(m);
    for I := 0 to 49 do
        FormProducts.RedrawPlace(I);
    FormProducts.RedrawAmbient;
end;

procedure TFormCatalogue.HandleMeasurements(ms: TMeasurements);
var
    I: Integer;
    value: double;
    m: TMeasurement;
    buk: IBucket;
    CreatedAt, UpdatedAt: TDateTime;
begin
    buk := self.SelectedBucket;
    if not Assigned(buk) or (buk.BucketID <> ms.BucketID) then
        exit;
    FormChart.FSeriesTemp.Clear;
    FormChart.FSeriesPress.Clear;
    FormChart.FSeriesHum.Clear;
    for I := 0 to 49 do
        FormChart.FSeriesPlace[I].Clear;
    CreatedAt := IncHour(unixMillisToDateTime(buk.CreatedAt), -3);
    UpdatedAt := IncHour(unixMillisToDateTime(buk.UpdatedAt), -3);
    for m in ms.Measurements do
        if (m.StoredAt >= CreatedAt) and (m.StoredAt <= UpdatedAt) then
            FormChart.AddMeasurement(m);
    for I := 0 to 49 do
        FormProducts.RedrawPlace(I);
    FormProducts.RedrawAmbient;
end;

function TFormCatalogue.getLastBucket: IBucket;
begin
    if not Assigned(FormCatalogue.FBuckets) or (FormCatalogue.FBuckets.Count = 0)
    then
        exit(nil);
    result := FormCatalogue.FBuckets[FormCatalogue.FBuckets.Count - 1];
end;

procedure TFormCatalogue.ComboBox1Change(Sender: TObject);
var
    I: Integer;
    prevRowIsLast, CanSelect: Boolean;
begin
    with StringGrid1 do
    begin
        OnSelectCell := nil;
        with FYearMonth[ComboBox1.ItemIndex] do
            FBuckets := MainSvcApi.listBucketsOfYearMonth(year, month);
        prevRowIsLast := Row = RowCount - 1;
        RowCount := FBuckets.Count + 1;
        if RowCount = 1 then
            exit;
        FixedRows := 1;
        for I := 0 to FBuckets.Count - 1 do
            with FBuckets[I] do
            begin
                Cells[0, I + 1] :=
                  Inttostr2(DayOf(unixMillisToDateTime(CreatedAt)));
                Cells[1, I + 1] :=
                  TimeToStr(IncHour(unixMillisToDateTime(CreatedAt), -3));
                Cells[2, I + 1] :=
                  TimeToStr(IncHour(unixMillisToDateTime(UpdatedAt), -3));
                Cells[3, I + 1] := IntToStr(PartyID);
                Cells[4, I + 1] := formatPartyTime(PartyCreatedAt);
            end;
        if prevRowIsLast then
            Row := RowCount - 1;
        OnSelectCell := StringGrid1SelectCell;
        StringGrid1SelectCell(StringGrid1, 0, Row, CanSelect);
    end;

end;

procedure TFormCatalogue.ReloadData;
var
    ComboBox1ItemIndex, I: Integer;

begin
    ComboBox1ItemIndex := ComboBox1.ItemIndex;
    FYearMonth := MainSvcApi.listYearMonths;
    ComboBox1.Clear;
    if FYearMonth.Count = 0 then
    begin
        FYearMonth.Add(TYearMonthImpl.Create);
        FYearMonth[0].year := YearOf(now);
        FYearMonth[0].month := MonthOf(now);
    end;

    for I := 0 to FYearMonth.Count - 1 do
        with FYearMonth[I] do
            ComboBox1.Items.Add(Format('%d %s',
              [year, FormatDateTime('MMMM', IncMonth(0, month))]));

    ComboBox1.ItemIndex := ComboBox1ItemIndex;
    ComboBox1Change(nil);
end;

procedure TFormCatalogue.StringGrid1DrawCell(Sender: TObject;
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
    case ACol of
        0:
            begin
                ta := taCenter;
                cnv.Font.Color := clGreen;
            end;
        1:
            begin
                ta := taLeftJustify;
                cnv.Font.Color := clBlack;
            end;
    end;

    if (ARow - 1 > -1) AND (ARow - 1 < FBuckets.Count) AND
      (FBuckets[ARow - 1].IsLast = true) then
    begin
        cnv.Font.Color := clBlue;
        // cnv.Font.Style := [fsBold];
    end
    else
    begin
        cnv.Font.Style := [];
    end;

    StringGrid_DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
    // StringGrid_DrawCellBounds(cnv, acol, arow,  Rect);
end;

procedure TFormCatalogue.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    party: IParty;
    t: TDateTime;
    I: Integer;
    products: IThriftList<IProduct>;
begin
    if ARow - 1 >= FBuckets.Count then
    begin
        FSelectedBucket := nil;
        exit;
    end;
    FSelectedBucket := FBuckets[ARow - 1];
    party := MainSvcApi.getParty(FSelectedBucket.PartyID);
    with StringGrid1 do
        FormOxygen73.Caption := Format('Загрузка №%d от %s: %s - %s...%s',
          [party.PartyID, formatPartyTime(party.CreatedAt), Cells[0, ARow],
          Cells[1, ARow], Cells[2, ARow]]);
    products := MainSvcApi.listProducts(party.PartyID);
    FormProducts.SetParty(party.PartyID, products);
    MainSvcApi.requestMeasurements(FSelectedBucket.CreatedAt,
      FSelectedBucket.UpdatedAt);

end;

end.
