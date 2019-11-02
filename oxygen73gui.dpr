program oxygen73gui;

uses
  Vcl.Forms,
  mainsvc in 'gen-delphi\mainsvc.pas',
  Thrift.Collections in 'thrift\Thrift.Collections.pas',
  Thrift.Exception in 'thrift\Thrift.Exception.pas',
  Thrift in 'thrift\Thrift.pas',
  Thrift.Processor.Multiplex in 'thrift\Thrift.Processor.Multiplex.pas',
  Thrift.Protocol.Compact in 'thrift\Thrift.Protocol.Compact.pas',
  Thrift.Protocol.JSON in 'thrift\Thrift.Protocol.JSON.pas',
  Thrift.Protocol.Multiplex in 'thrift\Thrift.Protocol.Multiplex.pas',
  Thrift.Protocol in 'thrift\Thrift.Protocol.pas',
  Thrift.Serializer in 'thrift\Thrift.Serializer.pas',
  Thrift.Server in 'thrift\Thrift.Server.pas',
  Thrift.Socket in 'thrift\Thrift.Socket.pas',
  Thrift.Stream in 'thrift\Thrift.Stream.pas',
  Thrift.Transport.MsxmlHTTP in 'thrift\Thrift.Transport.MsxmlHTTP.pas',
  Thrift.Transport in 'thrift\Thrift.Transport.pas',
  Thrift.Transport.Pipes in 'thrift\Thrift.Transport.Pipes.pas',
  Thrift.Transport.WinHTTP in 'thrift\Thrift.Transport.WinHTTP.pas',
  Thrift.TypeRegistry in 'thrift\Thrift.TypeRegistry.pas',
  Thrift.Utils in 'thrift\Thrift.Utils.pas',
  Thrift.WinHTTP in 'thrift\Thrift.WinHTTP.pas',
  MainSvcClient in 'MainSvcClient.pas',
  UnitFormProducts in 'UnitFormProducts.pas' {FormProducts},
  UnitFormCatalogue in 'UnitFormCatalogue.pas' {FormCatalogue},
  stringgridutils in 'utils\stringgridutils.pas',
  stringutils in 'utils\stringutils.pas',
  vclutils in 'utils\vclutils.pas',
  UnitFormChart in 'UnitFormChart.pas' {FormChart},
  apitypes in 'gen-delphi\apitypes.pas',
  logfile in 'logfile.pas',
  Grijjy.Bson in 'grijjy\Grijjy.Bson.pas',
  Grijjy.Bson.Serialization in 'grijjy\Grijjy.Bson.Serialization.pas',
  Grijjy.SysUtils in 'grijjy\Grijjy.SysUtils.pas',
  Grijjy.DateUtils in 'grijjy\Grijjy.DateUtils.pas',
  Grijjy.Bson.IO in 'grijjy\Grijjy.Bson.IO.pas',
  Grijjy.BinaryCoding in 'grijjy\Grijjy.BinaryCoding.pas',
  Grijjy.Collections in 'grijjy\Grijjy.Collections.pas',
  UnitFormPopup in 'UnitFormPopup.pas' {FormPopup},
  myutils in 'utils\myutils.pas',
  UnitFormOxygen73 in 'UnitFormOxygen73.pas' {FormOxygen73},
  UnitMeasurement in 'UnitMeasurement.pas',
  UnitFormEditSerialsDialog in 'UnitFormEditSerialsDialog.pas' {FormEditSerialsDialog},
  UnitFormAppConfig in 'UnitFormAppConfig.pas' {FormAppConfig},
  UnitFormEditAppConfig in 'UnitFormEditAppConfig.pas' {FormEditAppConfig},
  UnitAppIni in 'UnitAppIni.pas',
  UnitFormFoundProducts in 'UnitFormFoundProducts.pas' {FormFoundProducts},
  UnitFormConsole in 'UnitFormConsole.pas' {FormConsole};

{$R *.res}

begin

    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TFormOxygen73, FormOxygen73);
  Application.CreateForm(TFormChart, FormChart);
  Application.CreateForm(TFormProducts, FormProducts);
  Application.CreateForm(TFormCatalogue, FormCatalogue);
  Application.CreateForm(TFormConsole, FormConsole);
  Application.CreateForm(TFormPopup, FormPopup);
  Application.CreateForm(TFormEditSerialsDialog, FormEditSerialsDialog);
  Application.CreateForm(TFormAppConfig, FormAppConfig);
  Application.CreateForm(TFormEditAppConfig, FormEditAppConfig);
  Application.CreateForm(TFormFoundProducts, FormFoundProducts);
  Application.CreateForm(TFormConsole, FormConsole);
  Application.Run;

end.
