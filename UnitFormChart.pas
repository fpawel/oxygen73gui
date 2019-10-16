unit UnitFormChart;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
    Vcl.ExtCtrls, VclTee.TeeProcs, VclTee.Chart, VclTee.Series, Vcl.StdCtrls,
    Vcl.ComCtrls, Vcl.ToolWin, System.ImageList, Vcl.ImgList;

type
    TFormChart = class(TForm)
        ImageList1: TImageList;
        Panel5: TPanel;
        ToolBar1: TToolBar;
        ToolButton1: TToolButton;
        ToolButton3: TToolButton;
        Panel11: TPanel;
        GridPanel1: TGridPanel;
    MemoX: TMemo;
    MemoY1: TMemo;
    MemoY2: TMemo;
    Chart1: TChart;
        procedure FormCreate(Sender: TObject);
        procedure Chart1UndoZoom(Sender: TObject);
        procedure Chart1AfterDraw(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure MemoXMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    private
        { Private declarations }
        procedure SetActiveSeries(ser: TFastLineSeries);
        function GetActiveSeries: TFastLineSeries;
        procedure ShowCurrentScaleValues;

    public
        { Public declarations }
        FSeriesTemp, FSeriesPress, FSeriesHum: TFastLineSeries;
        FSeriesPlace: array [0 .. 49] of TFastLineSeries;
        procedure ChangeAxisOrder(c: TWinControl; WheelDelta: integer);
        property ActiveSeries: TFastLineSeries read GetActiveSeries
          write SetActiveSeries;
    end;

var
    FormChart: TFormChart;

implementation

{$R *.dfm}

uses dateutils, math, System.Types ;

procedure TFormChart.FormCreate(Sender: TObject);
var
    ser: TFastLineSeries;
    i: integer;
    axisPress, axisHum:TChartAxis;

begin

    ser := TFastLineSeries.Create(nil);
    ser.XValues.DateTime := true;
    ser.Title := 'T,"C';
    ser.VertAxis := aRightAxis;
    Chart1.AddSeries(ser);
    FSeriesTemp := ser;

    axisPress := TChartAxis.Create(Chart1);
    axisPress.OtherSide := true;
    axisPress.PositionUnits := muPixels;
    axisPress.PositionPercent := 60;
    axisPress.Grid.Hide;

    ser := TFastLineSeries.Create(nil);
    ser.XValues.DateTime := true;
    ser.Title := 'P,мм';
    ser.VertAxis := aRightAxis;
    Chart1.AddSeries(ser);
    ser.CustomVertAxis := axisPress;
    FSeriesPress := ser;


    axisHum := TChartAxis.Create(Chart1);
    axisHum.OtherSide := true;
    axisHum.PositionUnits := muPixels;
    axisHum.PositionPercent := 130;
    axisHum.Grid.Hide;


    ser := TFastLineSeries.Create(nil);
    ser.XValues.DateTime := true;
    ser.Title := 'H,%';
    ser.VertAxis := aRightAxis;
    Chart1.AddSeries(ser);
    ser.CustomVertAxis := axisHum;
    FSeriesHum := ser;


    for i := 1 to 50 do
    begin
        ser := TFastLineSeries.Create(nil);
        ser.XValues.DateTime := true;
        ser.Title := Format('%02d', [i]);
        Chart1.AddSeries(ser);
        FSeriesPlace[i - 1] := ser;
    end;

    for i := 0 to Chart1.SeriesCount-1 do
        Chart1.Series[i].Active := false;


end;

procedure TFormChart.Chart1AfterDraw(Sender: TObject);
var
    i, xPos, yPos, a, b: integer;
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

procedure TFormChart.MemoXMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
    TMemo(Sender).SetFocus;
end;

procedure TFormChart.ShowCurrentScaleValues;
var
    s, s1, s2, s3: string;
    v: double;
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
    MemoX.Text := Format('X: %s', [s]);
    with Chart1.Axes.Left do
    begin
        if Maximum = Minimum then
            MemoY1.Text := 'Y1: нет значений'
        else
            MemoY1.Text := Format('Y1: %g', [Maximum - Minimum]);
    end;
    with Chart1.Axes.Right do
    begin
        if Maximum = Minimum then
            MemoY2.Text := 'Y2: нет значений'
        else
            MemoY2.Text := Format('Y2: %g', [Maximum - Minimum]);
    end;

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

procedure TFormChart.ChangeAxisOrder(c: TWinControl; WheelDelta: integer);
var
    a: TChartAxis;
    step: double;
begin
    if c = MemoY1 then
        a := Chart1.LeftAxis
    else if c = MemoX then
        a := Chart1.BottomAxis
    else if c = MemoY2 then
        a := Chart1.RightAxis
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

end.
