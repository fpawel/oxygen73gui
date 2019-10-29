unit UnitFormChart;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
    Vcl.ExtCtrls, VclTee.TeeProcs, VclTee.Chart, VclTee.Series, Vcl.StdCtrls,
    Vcl.ComCtrls, Vcl.ToolWin, System.ImageList, Vcl.ImgList, UnitMeasurement;

type
    TFormChart = class(TForm)
        ImageList1: TImageList;
        Panel5: TPanel;
        ToolBar1: TToolBar;
        ToolButton1: TToolButton;
        ToolButton3: TToolButton;
        Panel11: TPanel;
        GridPanel1: TGridPanel;
        Chart1: TChart;
        PanelX: TPanel;
        PanelY: TPanel;
        PanelT: TPanel;
        PanelP: TPanel;
        PanelH: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure Chart1UndoZoom(Sender: TObject);
        procedure Chart1AfterDraw(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
        procedure ToolButton3Click(Sender: TObject);
    private
        { Private declarations }
        FAxisTemp, FAxisPress, FAxisHum: TChartAxis;
        procedure SetActiveSeries(ser: TFastLineSeries);
        function GetActiveSeries: TFastLineSeries;
        procedure ShowCurrentScaleValues;

    public
        { Public declarations }
        FSeriesTemp, FSeriesPress, FSeriesHum: TFastLineSeries;
        FSeriesPlace: array [0 .. 49] of TFastLineSeries;
        procedure ChangeAxisOrder(c: TWinControl; WheelDelta: Integer);

        procedure AddMeasurement(m: TMeasurement);

        procedure UpdateRightAxis;

        property ActiveSeries: TFastLineSeries read GetActiveSeries
          write SetActiveSeries;

    end;

var
    FormChart: TFormChart;

implementation

{$R *.dfm}

uses dateutils, math, System.Types;

procedure TFormChart.FormCreate(Sender: TObject);
var
    ser: TFastLineSeries;
    i: Integer;

begin

    ser := TFastLineSeries.Create(nil);
    ser.XValues.DateTime := true;
    ser.Title := 'T,"C';
    ser.VertAxis := aRightAxis;
    Chart1.AddSeries(ser);
    FSeriesTemp := ser;

    FAxisTemp := Chart1.RightAxis;
    FAxisTemp.PositionUnits := muPixels;
    FAxisTemp.Grid.Hide;
    FAxisTemp.Title.Text := 'T\"C';
    FAxisTemp.Title.Show;
    FAxisTemp.Title.Position := TAxisTitlePosition.tpEnd;
    FAxisTemp.Title.Font.Size := 11;
    FAxisTemp.Title.Font.Color := clRed;
    FAxisTemp.LabelsFont.Color := clRed;

    FAxisPress := TChartAxis.Create(Chart1);
    FAxisPress.OtherSide := true;
    FAxisPress.PositionUnits := muPixels;
    FAxisPress.PositionPercent := -80;
    FAxisPress.Grid.Hide;
    FAxisPress.Title.Text := 'P,мм.рт.ст.';
    FAxisPress.Title.Show;
    FAxisPress.Title.Position := TAxisTitlePosition.tpEnd;
    FAxisPress.Title.Font.Size := 11;
    FAxisPress.Title.Font.Color := clBlue;
    FAxisPress.LabelsFont.Color := clBlue;

    FAxisHum := TChartAxis.Create(Chart1);
    FAxisHum.OtherSide := true;
    FAxisHum.PositionUnits := muPixels;
    FAxisHum.PositionPercent := -160;
    FAxisHum.Grid.Hide;
    FAxisHum.Title.Text := 'H,%';
    FAxisHum.Title.Show;
    FAxisHum.Title.Position := TAxisTitlePosition.tpEnd;
    FAxisHum.Title.Font.Size := 11;
    FAxisHum.Title.Font.Color := $00A8974E;
    FAxisHum.LabelsFont.Color := $00A8974E;

    ser := TFastLineSeries.Create(nil);
    ser.XValues.DateTime := true;
    ser.Title := 'P,мм.рт.ст.';
    ser.VertAxis := aRightAxis;
    Chart1.AddSeries(ser);
    ser.CustomVertAxis := FAxisPress;
    FSeriesPress := ser;

    ser := TFastLineSeries.Create(nil);
    ser.XValues.DateTime := true;
    ser.Title := 'H,%';
    ser.VertAxis := aRightAxis;
    Chart1.AddSeries(ser);
    ser.CustomVertAxis := FAxisHum;
    FSeriesHum := ser;

    for i := 1 to 50 do
    begin
        ser := TFastLineSeries.Create(nil);
        ser.XValues.DateTime := true;
        ser.Title := Format('%02d', [i]);
        Chart1.AddSeries(ser);
        FSeriesPlace[i - 1] := ser;
    end;

    for i := 0 to Chart1.SeriesCount - 1 do
        Chart1.Series[i].Active := false;
end;

procedure TFormChart.AddMeasurement(m: TMeasurement);
var
    i: Integer;
begin
    if not IsNaN(m.Temperature) then
        FSeriesTemp.AddXY(m.StoredAt, m.Temperature);
    if not IsNaN(m.Pressure) then
        FSeriesPress.AddXY(m.StoredAt, m.Pressure);
    if not IsNaN(m.Humidity) then
        FSeriesHum.AddXY(m.StoredAt, m.Humidity);
    for i := 0 to 49 do
        if (m.Places[i] <> 112) and (not IsNaN(m.Places[i])) then
            FSeriesPlace[i].AddNullXY(m.StoredAt, m.Places[i]);
end;

procedure TFormChart.Chart1AfterDraw(Sender: TObject);
var
    i, xPos, yPos, a, b: Integer;
    ser: TChartSeries;

    marker_place: boolean;
    marker_rects: array of TRect;
    marker_rect, r2: TRect;
    marker_text: string;
    function pow2(X: Extended): Extended;
    begin
        exit(IntPower(X, 2));
    end;

begin

    ShowCurrentScaleValues;

    if (not ToolButton1.Down) and (not ToolButton3.Down) then
        exit;

    Chart1.Canvas.Pen.Style := psSolid;
    Chart1.Canvas.Pen.Width := 1;
    Chart1.Canvas.Pen.Mode := pmCopy;
    Chart1.Canvas.Font.Size := 10;

    for ser in Chart1.SeriesList do
    begin
        if not ser.Active then
            Continue;
        Chart1.Canvas.Pen.Color := ser.Color;
        if ser.Tag > 0 then
            Chart1.Canvas.Brush.Color := ser.Color;

        for i := ser.FirstValueIndex to ser.LastValueIndex do
        begin
            xPos := ser.CalcXPos(i);
            yPos := ser.CalcYPos(i);

            if not PtInRect(Chart1.ChartRect, Point(xPos, yPos)) then
                Continue;

            if (i > ser.FirstValueIndex) AND (i < ser.LastValueIndex) AND
              (pow2(xPos - a) + pow2(yPos - b) < pow2(7)) then
                Continue;

            if ser.Tag > 0 then
            begin
                Chart1.Canvas.Ellipse(xPos - 5, yPos - 5, xPos + 5, yPos + 5);
            end
            else
            begin
                // Parameters are
                // X-Coord, Y-Coord, X-Radius, Y-Radius, Start Angle, End Angle, Hole%
                Chart1.Canvas.Donut(xPos, yPos, 3, 3, -1, 361, 100);
            end;
            a := xPos;
            b := yPos;

            marker_text := Format('%s Х %g',
              [formatDatetime('h:n:s.zzz', ser.XValues[i]), ser.YValues[i]]);
            with marker_rect do
            begin
                Left := xPos;
                Top := yPos - Canvas.TextHeight(marker_text);
                Right := xPos + Canvas.TextWidth(marker_text);
                Bottom := yPos;
            end;

            marker_place := ToolButton3.Down;
            for r2 in marker_rects do
            begin
                if System.Types.IntersectRect(marker_rect, r2) then
                begin
                    marker_place := false;
                    break;
                end;
            end;
            if marker_place then
            begin
                Chart1.Canvas.Font.Color := ser.Color;
                Chart1.Canvas.TextOut(marker_rect.Left, marker_rect.Top,
                  marker_text);
                SetLength(marker_rects, length(marker_rects) + 1);
                marker_rects[length(marker_rects) - 1] := marker_rect;
            end;
        end;
    end;

    ser := ActiveSeries;
    if not Assigned(ser) then
        exit;

end;

procedure TFormChart.Chart1UndoZoom(Sender: TObject);
begin
    Chart1.BottomAxis.Automatic := true;
    Chart1.LeftAxis.Automatic := true;
    FAxisTemp.Automatic := true;
    FAxisTemp.Automatic := true;
    FAxisHum.Automatic := true;
end;

procedure TFormChart.SetActiveSeries(ser: TFastLineSeries);
var
    s: TChartSeries;
begin
    for s in Chart1.SeriesList do
    begin
        if s <> ser then
        begin
            s.Tag := 0;
            (s as TFastLineSeries).LinePen.Width := 1;
        end
        else
        begin
            s.Tag := 1;
            (s as TFastLineSeries).LinePen.Width := 4;
        end;
    end;
end;

function TFormChart.GetActiveSeries: TFastLineSeries;
var
    s: TChartSeries;
begin
    for s in Chart1.SeriesList do

        if s.Tag > 0 then
            exit(s as TFastLineSeries);
    exit(nil);
end;

procedure TFormChart.ShowCurrentScaleValues;
var
    s, s1, s2, s3: string;
    v: double;
    procedure ShowAxisOrders(ax: TChartAxis; pn: TPanel; prefix:string);
    begin
        with ax do
        begin
            if Maximum = Minimum then
                pn.Caption := prefix+': нет значений'
            else
                pn.Caption := prefix+': ' + FormatFloat('#0.0##', Maximum - Minimum);
        end;

    end;

begin

    with Chart1.Axes.Bottom do
    begin
        v := Maximum - Minimum;

        s1 := TimetoStr(Minimum);
        s2 := TimetoStr(Maximum);
        s3 := TimetoStr(v);

        if v = 0 then
            s := 'нет значений'
        else if v < IncSecond(0, 1) then
            s := inttostr(MilliSecondsBetween(Maximum, Minimum)) + 'мс'
        else if v < IncMinute(0, 1) then
            s := inttostr(SecondsBetween(Maximum, Minimum)) + ' c'
        else if v < Inchour(0, 1) then
            s := inttostr(minutesBetween(Maximum, Minimum)) + ' минут'
        else if v < Incday(0, 1) then
            s := inttostr(hoursBetween(Maximum, Minimum)) + ' часов'
        else
            s := inttostr(daysBetween(Maximum, Minimum)) + ' дней';

    end;
    PanelX.Caption := Format('X: %s', [s]);
    ShowAxisOrders(Chart1.Axes.Left, PanelY, 'Y');
    ShowAxisOrders(Chart1.Axes.Right, PanelT, 'T');
    ShowAxisOrders(FAxisPress, PanelP, 'P');
    ShowAxisOrders(FAxisHum, PanelH, 'H');
end;

procedure TFormChart.ToolButton1Click(Sender: TObject);
begin
    if ToolButton1.Down then
        ToolButton3.Down := false;
    Chart1.Repaint;
end;

procedure TFormChart.ToolButton3Click(Sender: TObject);
begin
    if ToolButton3.Down then
        ToolButton1.Down := false;
    Chart1.Repaint;
end;

procedure TFormChart.ChangeAxisOrder(c: TWinControl; WheelDelta: Integer);
var
    a: TChartAxis;

    step: double;
begin
    if c = PanelY then
        a := Chart1.LeftAxis
    else if c = PanelX then
        a := Chart1.BottomAxis
    else if c = PanelT then
        a := Chart1.RightAxis

    else if c = PanelP then
        a := FAxisPress

    else if c = PanelH then
        a := FAxisHum

    else
        exit;
    if a.Minimum = a.Maximum then
        exit;

    step := (a.Maximum - a.Minimum) * 0.03;
    if WheelDelta < 0 then
        step := step * -1;

    if a.Minimum - step >= a.Maximum + step then
        exit;

    a.SetMinMax(a.Minimum - step, a.Maximum + step);

end;

procedure TFormChart.UpdateRightAxis;
begin
    FAxisTemp.Visible := FSeriesTemp.Visible;
    FAxisPress.Visible := FSeriesPress.Visible;
    FAxisHum.Visible := FSeriesHum.Visible;

    if not FSeriesPress.Visible and not FSeriesHum.Visible then
    begin
        Chart1.MarginRight := 30;
        exit;
    end;

    if not FSeriesTemp.Visible and FSeriesPress.Visible and not FSeriesHum.Visible
    then
    begin
        Chart1.MarginRight := 50;
        FAxisPress.PositionPercent := 0;
        exit;
    end;

    if not FSeriesTemp.Visible and not FSeriesPress.Visible and FSeriesHum.Visible
    then
    begin
        Chart1.MarginRight := 50;
        FAxisHum.PositionPercent := 0;
        exit;
    end;

    if FSeriesTemp.Visible and FSeriesPress.Visible and not FSeriesHum.Visible
    then
    begin
        Chart1.MarginRight := 100;
        FAxisPress.PositionPercent := -80;
        exit;
    end;

    if FSeriesTemp.Visible and not FSeriesPress.Visible and FSeriesHum.Visible
    then
    begin
        Chart1.MarginRight := 100;
        FAxisHum.PositionPercent := -80;
        exit;
    end;

    if not FSeriesTemp.Visible and FSeriesPress.Visible and FSeriesHum.Visible
    then
    begin
        Chart1.MarginRight := 100;
        FAxisPress.PositionPercent := 0;
        FAxisHum.PositionPercent := -80;
        exit;
    end;

    if FSeriesTemp.Visible and FSeriesPress.Visible and FSeriesHum.Visible then
    begin
        Chart1.MarginRight := 170;
        FAxisPress.PositionPercent := -80;
        FAxisHum.PositionPercent := -160;
        exit;
    end;
end;

end.
