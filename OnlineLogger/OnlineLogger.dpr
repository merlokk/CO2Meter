program OnlineLogger;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {MainFrm},
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
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
