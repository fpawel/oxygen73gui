unit UnitFormJournal;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    UnitFormConsole, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls;

type

    TFormJournal = class(TForm)
        Splitter1: TSplitter;
        ListBox1: TListBox;
        Panel1: TPanel;
        Panel2: TPanel;
        Edit1: TEdit;
        Label1: TLabel;
        MemoError: TMemo;
        Timer1: TTimer;
        procedure FormShow(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure FormPaint(Sender: TObject);
        procedure Edit1Change(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
    private
        { Private declarations }

    public
        { Public declarations }
        procedure FetchDays;
        procedure FetchEntries;
        procedure NewEntry(line: string);
    end;

var
    FormJournal: TFormJournal;

implementation

uses FireDAC.Comp.Client, FireDAC.stan.param, dateutils,
    RegularExpressions, apitypes, Thrift.Collections, stringgridutils,
    MainSvcClient, myutils;

{$R *.dfm}

procedure TFormJournal.FormCreate(Sender: TObject);
begin
    MemoError.Hide;

end;

procedure TFormJournal.FormPaint(Sender: TObject);
begin
    HideCaret(MemoError.Handle)
end;

procedure TFormJournal.FormShow(Sender: TObject);
begin
    FetchDays;
    with FormConsole do
    begin
        Parent := Panel1;
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;
end;

procedure TFormJournal.ListBox1Click(Sender: TObject);
begin
    FetchEntries;
end;

procedure TFormJournal.NewEntry(line: string);
var
    re: TRegEx;
begin
    with ListBox1 do
        if (Count = 0) OR (ItemIndex <> Count - 1) OR
          (Length(Trim(Edit1.Text)) > 0) then
            exit;
    FormConsole.NewLine(now, line);

end;

procedure TFormJournal.Timer1Timer(Sender: TObject);
begin
    FetchEntries;
    Timer1.Enabled := false;
end;

procedure TFormJournal.FetchEntries;
var
    xs: IThriftList<ILogEntry>;
    ent: ILogEntry;
begin
    FormConsole.Hide;
    FormConsole.Clear;
    MemoError.Hide;
    if ListBox1.ItemIndex = -1 then
        exit;

    try
        xs := MainSvcApi.LogEntriesOfDay
          (DateTimeToUnixMillis(StrToDate(ListBox1.Items[ListBox1.ItemIndex])),
          Trim(Edit1.Text));
    except
        on e: Exception do
        begin
            MemoError.Text := e.Message;
            MemoError.Show;
            exit;
        end;
    end;

    for ent in xs do
        FormConsole.NewLine(IncHour(UnixMillisToDateTime(ent.Time), -3),
          ent.line);
    FormConsole.Show;

end;

procedure TFormJournal.Edit1Change(Sender: TObject);
begin
    Timer1.Enabled := true;
end;

procedure TFormJournal.FetchDays;
var
    dt: TDateTime;
    tm: TTimeUnixMillis;
    days: IThriftList<TTimeUnixMillis>;
begin
    days := MainSvcApi.ListLogEntriesDays;
    with ListBox1 do
    begin
        Clear;
        for tm in MainSvcApi.ListLogEntriesDays do
            Items.Add(DateToStr(UnixMillisToDateTime(tm)));
        ItemIndex := Items.Count - 1
    end;
    ListBox1Click(ListBox1);

end;

end.
