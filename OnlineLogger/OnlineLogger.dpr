program OnlineLogger;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  CO2Meter in '..\CO2Meter.pas',
  GoogleAPI in 'GoogleAPI.pas',
  def in 'def.pas',
  GoogleSender in 'GoogleSender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
