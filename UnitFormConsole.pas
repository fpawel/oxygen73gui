unit UnitFormConsole;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
    Vcl.Samples.Spin, Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
    Vcl.Grids, Vcl.Menus;

type

    TLogLevel = (loglevDebug, loglevInfo, loglevWarn, loglevError);

    TEntry = record
        FLevel: TLogLevel;
        FTime1, FTime2: TDateTime;
        FText: string;
        FCount: integer;

    end;

    TFormConsole = class(TForm)
        ImageList4: TImageList;
        StringGrid1: TStringGrid;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1DblClick(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);

    private
        { Private declarations }
        FEntries: TArray<TEntry>;

    public
        FFileName: string;
        { Public declarations }
        procedure NewLine(AText: string);
        procedure NewLineLevel(ALevel: TLogLevel; AText: string);
        procedure Clear;
    end;

var
    FormConsole: TFormConsole;

implementation

uses dateutils, stringutils,
    stringgridutils, strutils, types,
    UnitFormPopup;

{$R *.dfm}


function loglevelColor(lev: TLogLevel): Tcolor;
begin
    case lev of
        loglevDebug:
            result := clGray;
        loglevInfo:
            result := clBlack;
        loglevWarn:
            result := clMaroon;
        loglevError:
            result := clRed;
    else
        result := clBlack;
    end;
end;

function formatEntryText(r: TEntry): string;
begin
    result := r.FText;
    if r.FCount > 1 then
        result := Format('%s [%d] %s', [formatDatetime('hh:mm:ss', r.FTime2),
          r.FCount, r.FText]);
end;

procedure TFormConsole.FormCreate(Sender: TObject);
begin
    SetLength(FEntries, 0);
end;

procedure TFormConsole.FormResize(Sender: TObject);
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 70;
        ColWidths[1] := self.Width - ColWidths[0] - 30;
    end;
end;

procedure TFormConsole.StringGrid1DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
begin
    if StringGrid1.Row >= length(FEntries) then
            exit;
    FormPopup.ShowStringGridCellText(StringGrid1);
end;

procedure TFormConsole.StringGrid1DrawCell(Sender: TObject; ACol, ARow: integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;
    AText: string;

begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    AText := grd.Cells[ACol, ARow];

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption;

    ta := taLeftJustify;

    if ARow < length(FEntries) then
        with FEntries[ARow] do
            case ACol of
                0:
                    cnv.Font.Color := loglevelColor(FLevel);
                1:
                    cnv.Font.Color := loglevelColor(FLevel);
            end;

    StringGrid_DrawCellText(StringGrid1, ACol, ARow, Rect, ta, StringGrid1.Cells[ACol,ARow]);
    // StringGrid_DrawCellBounds(StringGrid1.Canvas, ACol, ARow, Rect);
end;

procedure TFormConsole.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
    GridRect: TGridRect;
begin

    if (ssCtrl in Shift) and (Key = 65) then
        with StringGrid1 do
        begin
            GridRect.Top := 0;
            GridRect.Left := 0;
            GridRect.Right := 1;
            GridRect.Bottom := RowCount - 1;
            Selection := GridRect;
        end;

end;

procedure TFormConsole.Clear;
begin
    with StringGrid1 do
    begin
        RowCount := 1;
        Cells[0, 0] := '';
        Cells[1, 0] := '';
    end;
end;

procedure TFormConsole.N1Click(Sender: TObject);
begin
    StringGrid_CopytoClipboard(StringGrid1);
end;

function parseLogLevel(s: String): TLogLevel;
var
    xs: TStringDynArray;
begin
    xs := SplitString(s, ' ');
    result := loglevDebug;
    if (length(xs) > 1) then
    begin
        s := LowerCase(xs[1]);
        if s = 'inf' then
            result := loglevInfo
        else if s = 'wrn' then
            result := loglevWarn
        else if s = 'err' then
            result := loglevError
    end;
end;

procedure TFormConsole.NewLineLevel(ALevel: TLogLevel; AText: string);
begin

    with StringGrid1 do
    begin
        if (length(FEntries) > 0) ANd
          (FEntries[length(FEntries) - 1].FText = AText) then
            with FEntries[length(FEntries) - 1] do
            begin
                FCount := FCount + 1;
                FTime2 := now;
                StringGrid_RedrawRow(StringGrid1, RowCount - 1);
                exit;
            end;

        SetLength(FEntries, length(FEntries) + 1);
        with FEntries[length(FEntries) - 1] do
        begin
            FLevel := ALevel;
            FText := AText;
            FTime1 := now;
            FTime2 := now;
            FCount := 1;
        end;
        if length(FEntries) = 1 then
            StringGrid_RedrawRow(StringGrid1, RowCount - 1)
        else
            RowCount := RowCount + 1;
        Row := RowCount - 1;
        with FEntries[length(FEntries) - 1] do
        begin
            Cells[0, Row] := formatDatetime('hh:mm:ss', FTime1);
            Cells[1, Row] := formatEntryText(FEntries[Row]);
        end;




    end;
end;

procedure TFormConsole.NewLine(AText: string);
begin
    NewLineLevel(parseLogLevel(AText), AText);
end;

end.
