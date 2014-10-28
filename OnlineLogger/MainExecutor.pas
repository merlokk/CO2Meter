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

    FSender: TComponent;
  public
    constructor Create(ASender: TComponent);
    destructor Destroy; override;

    procedure Init(AComPort: integer; AClientID, AClientSecret, AStoreFileName: string);
    procedure WorkCycle;

    property CO2Meter: TCO2MeterConnector read metr;
    property GoogleSender: TGoogleSender read sender;
  end;

implementation

{ TMainExecutor }

constructor TMainExecutor.Create(ASender: TComponent);
begin
  FSender := ASender;
  metr := nil;
  storage := nil;
  sender := nil;
end;

destructor TMainExecutor.Destroy;
begin
  metr.Terminate;
  sender.Free;
  metr.Free;
  storage.Free;

  inherited;
end;

procedure TMainExecutor.Init(AComPort: integer; AClientID, AClientSecret, AStoreFileName: string);
begin
  sender.Free;
  if metr <> nil then metr.Terminate;
  metr.Free;
  storage.Free;

  metr := TCO2MeterConnector.Create;
  metr.ComPort := AComPort;
  sender := TGoogleSender.Create(FSender, AClientID, AClientSecret);
  storage := TLocalStorage.Create(AStoreFileName);
end;

procedure TMainExecutor.WorkCycle;
var
  mes: TMeasurements;
begin
  storage.Load;

  mes := metr.GetData;

  storage.Add(mes);

  storage.Save;

  mes := storage.Get;
  try
    if length(mes) > 0 then
      if sender.SendData(mes) then
      begin
        storage.Clear;
        storage.Save;
      end;
  except
  end;
end;

end.
