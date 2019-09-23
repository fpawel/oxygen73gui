unit UnitFormParty;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, mainsvc;

type
    TFormParty = class(TForm)
        StringGrid1: TStringGrid;
    private
        { Private declarations }
        FProducts: TArray<IProduct>;
    public
        { Public declarations }
        procedure reload_data;
    end;

var
    FormParty: TFormParty;

implementation

{$R *.dfm}

procedure TFormParty.reload_data;
begin

end;

end.
