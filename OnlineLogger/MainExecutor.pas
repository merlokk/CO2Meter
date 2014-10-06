unit MainExecutor;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, System.AnsiStrings,
  LocalStorage, CO2MeterConnector, GoogleSender, def;
type

  TMainExecutor = class
  private
    metr: TCO2MeterConnector;
    storage: TLocalStorage;
    sender: TGoogleSender;
  public
    constructor Create;
    destructor Destroy; override;

    procedure WorkCycle;
  end;

implementation

{ TMainExecutor }

constructor TMainExecutor.Create;
begin

end;

destructor TMainExecutor.Destroy;
begin

  inherited;
end;

procedure TMainExecutor.WorkCycle;
var
  mes: TMeasurements;
begin
  storage.Load;
  metr.GetData;

  storage.Save;

  sender.SendData(mes);

  storage.Clear;
end;

end.
