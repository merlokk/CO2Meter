program OnlineLogger;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  GoogleAPI in 'GoogleAPI.pas',
  def in 'def.pas',
  GoogleSender in 'GoogleSender.pas',
  CO2Meter in '..\CO2Meter.pas',
  CO2MeterConnector in 'CO2MeterConnector.pas',
  MainExecutor in 'MainExecutor.pas',
  LocalStorage in 'LocalStorage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
