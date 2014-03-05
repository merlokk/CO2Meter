program getdata7798;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {fMain},
  CO2Meter in '..\CO2Meter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
