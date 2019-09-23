unit UnitFormCatalogue;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
    mainsvc, Thrift.Collections, apitypes, MainSvcClient, stringgridutils, stringutils;

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
    private
        { Private declarations }
        FYearMonth: IThriftList<IYearMonth>;
        FBuckets: IThriftList<IBucket>;

    public
        { Public declarations }
        procedure FetchYearsMonths;
    end;

var
    FormCatalogue: TFormCatalogue;

implementation

{$R *.dfm}

uses dateutils, Unit1, UnitFormProducts, math, UnitFormChart;

function unixMillisToDateTime(t: int64): TDateTime;
begin
    result := IncMilliSecond(EncodeDateTime(1970, 1, 1, 0, 0, 0, 0), t);
end;

function formatPartyTime(t: int64): string;
begin
    result := FormatDateTime('dd.mm.yy hh:nn', unixMillisToDateTime(t))
end;

procedure TFormCatalogue.ComboBox1Change(Sender: TObject);
var
    I: Integer;
    CanSelect: Boolean;
begin
    with StringGrid1 do
    begin
        OnSelectCell := nil;
        with FYearMonth[ComboBox1.ItemIndex] do
            Fbuckets := MainSvcApi.listBucketsOfYearMonth(year, month);
        RowCount := Fbuckets.Count + 1;
        if RowCount = 1 then
            exit;

        FixedRows := 1;
        Cells[0, 0] := 'День';
        Cells[1, 0] := 'Начало';
        Cells[2, 0] := 'Конец';
        Cells[3, 0] := 'Загрузка';
        Cells[4, 0] := 'Создана';
        ColWidths[0] := 40;
        ColWidths[1] := 70;
        ColWidths[2] := 70;
        ColWidths[3] := 70;
        ColWidths[4] := 120;

        for I := 0 to Fbuckets.Count - 1 do
            with Fbuckets[I] do
            begin
                Cells[0, I + 1] :=
                  Inttostr2(DayOf(unixMillisToDateTime(CreatedAt)));
                Cells[1, I + 1] := TimeToStr(unixMillisToDateTime(CreatedAt));
                Cells[2, I + 1] := TimeToStr(unixMillisToDateTime(UpdatedAt));

                Cells[3, I + 1] := Inttostr(PartyID);

                Cells[4, I + 1] := formatPartyTime(PartyCreatedAt);
            end;
        Row := RowCount - 1;
        OnSelectCell := StringGrid1SelectCell;
        StringGrid1SelectCell(StringGrid1, 0, Row, CanSelect);

    end;

end;

procedure TFormCatalogue.FetchYearsMonths;
var
    I: Integer;

begin
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

    ComboBox1.ItemIndex := 0;
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

    DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
    // StringGrid_DrawCellBounds(cnv, acol, arow,  Rect);
end;

procedure TFormCatalogue.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    party: IParty;
    buk: IBucket;
    m: IMeasurement;
    t: TDateTime;
    I: Integer;
begin
    if ARow - 1 >= FBuckets.Count then
        exit;
    buk := FBuckets[ARow - 1];
    party := MainSvcApi.getParty(buk.PartyID);
    with StringGrid1 do
        Form1.Caption := Format('Загрузка №%d от %s: %s - %s...%s',
          [party.PartyID, formatPartyTime(party.CreatedAt), Cells[0, ARow],
          Cells[1, ARow], Cells[2, ARow]]);
    FormProducts.SetPartyID(party.PartyID);

    FormChart.FSeriesTemp.Clear;
    FormChart.FSeriesPress.Clear;
    FormChart.FSeriesHum.Clear;
    for I := 0 to 49 do
        FormChart.FSeriesPlace[I].Clear;
    for m in MainSvcApi.listMeasurements(buk.CreatedAt, buk.UpdatedAt) do
    begin
        t := unixMillisToDateTime(m.StoredAt);
        if not IsNaN(m.Temperature) then
            FormChart.FSeriesTemp.AddXY(t, m.Temperature);
        if not IsNaN(m.Pressure) then
            FormChart.FSeriesPress.AddXY(t, m.Pressure);
        if not IsNaN(m.Humidity) then
            FormChart.FSeriesHum.AddXY(t, m.Humidity);
        for I := 0 to 49 do
            if not IsNaN(m.Places[I]) then
                FormChart.FSeriesPlace[I].AddXY(t, m.Places[I]);

    end;

end;

end.
