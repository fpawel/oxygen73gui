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
    procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
		FYearMonth: IThriftList<IYearMonth>;
		FSelectedBucket: IBucket;
		FBuckets: IThriftList<IBucket>;
		function getLastBucket: IBucket;
        procedure ReloadData;
		procedure ReloadSelectedBucket;
	public
		{ Public declarations }
		property SelectedBucket: IBucket read FSelectedBucket;
		// property LastBucket: IBucket read getLastBucket;
        procedure ReloadYearsMonths;
		procedure ReloadSelectedMonth;
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
		ColCount := 6;
		Cells[0, 0] := 'День';
		Cells[1, 0] := 'Начало';
		Cells[2, 0] := 'Конец';
		Cells[3, 0] := 'Партия';
		Cells[4, 0] := 'Загрузка';
        Cells[5, 0] := 'График';
		ColWidths[0] := 40;
		ColWidths[1] := 70;
		ColWidths[2] := 70;
		ColWidths[3] := 70;
		ColWidths[4] := 120;
        ColWidths[5] := 80;
	end;
end;

procedure TFormCatalogue.FormShow(Sender: TObject);
begin
	ReloadData
end;

procedure TFormCatalogue.HandleNewMeasurements(ms: TMeasurements);
var
	I: Integer;
	value: double;
	m: TMeasurement;
begin
	ReloadYearsMonths;
    ReloadSelectedMonth;
	if not Assigned(FSelectedBucket) or (FSelectedBucket.BucketID <> ms.BucketID)
	then
		exit;

	FSelectedBucket.UpdatedAt :=
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

	buk: IBucket;
	CreatedAt, UpdatedAt: TDateTime;
	m: TMeasurement;
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

	FormChart.hide;
	for m in ms.Measurements do
	begin
		if (m.StoredAt >= CreatedAt) and (m.StoredAt <= UpdatedAt) then
			FormChart.AddMeasurement(m);
	end;
	FormProducts.RedrawAmbient;
	for I := 0 to 49 do
		FormProducts.RedrawPlace(I);
	FormChart.show;

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
	ReloadSelectedMonth;
	CanSelect := true;
	with StringGrid1 do
		if (RowCount > 1) AND (Row > 0) AND (Row < RowCount) then
			StringGrid1SelectCell(StringGrid1, 0, Row, CanSelect);

end;

procedure TFormCatalogue.ReloadSelectedMonth;
var
	I: Integer;
	prevRowIsLast: Boolean;
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
                Cells[5, I + 1] := IntToStr(BucketID);
			end;
		if prevRowIsLast then
			Row := RowCount - 1;
		OnSelectCell := StringGrid1SelectCell;
	end;

end;

procedure TFormCatalogue.ReloadSelectedBucket;
var
	party: IParty;
	t: TDateTime;
	I: Integer;
	products: IThriftList<IProduct>;
begin
	party := MainSvcApi.getParty(FSelectedBucket.PartyID);
	with StringGrid1 do
		FormOxygen73.Caption := Format('Загрузка №%d от %s',
		  [party.PartyID, formatPartyTime(party.CreatedAt)]);
	products := MainSvcApi.listProducts(party.PartyID);
	FormProducts.SetParty(party.PartyID, products);
	MainSvcApi.requestMeasurements(FSelectedBucket.BucketID);

	FormOxygen73.PanelMessageBox.Caption := Format('Открывается график %d',
	  [FSelectedBucket.BucketID]);
	FormOxygen73.PanelMain.Enabled := false;
	FormOxygen73.PanelMessageBox.show;
	FormOxygen73.PanelMessageBox.BringToFront;
	FormOxygen73.OnResize(FormOxygen73);

end;

procedure TFormCatalogue.ReloadYearsMonths;
var
	ComboBox1ItemIndex, I: Integer;
	ComboBox1OnChangeOrig: procedure(x: TObject) of object;

begin
	ComboBox1OnChangeOrig := ComboBox1.OnChange;
	ComboBox1.OnChange := nil;

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
	ComboBox1.OnChange := ComboBox1OnChangeOrig;

end;

procedure TFormCatalogue.ReloadData;
begin
	ReloadYearsMonths;
	ReloadSelectedMonth;
    if FBuckets.Count > 0 then
    begin
    	FSelectedBucket := FBuckets.Last;
        ReloadSelectedBucket;
    end;
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

begin
	if ARow - 1 >= FBuckets.Count then
	begin
		FSelectedBucket := nil;
		exit;
	end;
	FSelectedBucket := FBuckets[ARow - 1];
	ReloadSelectedBucket;

end;

end.
