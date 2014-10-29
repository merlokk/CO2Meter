unit CO2MeterConnector;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, System.AnsiStrings, DateUtils, System.SyncObjs,
  CO2Meter, def;

type
  TCO2MeterConnector = class (TThread)
  private
    FComPort: integer;
    FInterval: integer; //samples rate
    FMeasurements: TMeasurements;

    LastMesMade: integer;
    metr: TCO2Meter;
    cs: TCriticalSection;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;

    function GetData: TMeasurements;
    function GetDataCount: integer;
    function GetDataStartDate: TDateTime;

    property ComPort: integer read FComPort write FComPort;
  end;

implementation

{ TCO2MeterConnector }

constructor TCO2MeterConnector.Create;
begin
  inherited;

  LastMesMade := 0;
  FInterval := 60; //300;
  SetLength(FMeasurements, 0);
  cs := TCriticalSection.Create;
  metr := TCO2Meter.Create;
end;

destructor TCO2MeterConnector.Destroy;
begin
  metr.Destroy;
  cs.Destroy;

  inherited;
end;

procedure TCO2MeterConnector.Execute;
var
  i: integer;
  mes: TMeasurement;
  secs: integer;
  setDT: TDateTime;
  samplesCount,
  samplesRate: Cardinal;
  samplesStartDate: TDateTime;
begin
  setDT := 0;

  LastMesMade := (SecondsBetween(now, EncodeDate(2000, 1, 1)) div FInterval) * FInterval;
  sleep(100);

  while not Terminated do
  begin
    if not metr.Connected then metr.OpenPort(FComPort);

    if metr.Connected then
    begin
      secs := SecondsBetween(now, EncodeDate(2000, 1, 1));

      if LastMesMade + FInterval <= (secs div FInterval) * FInterval then
      try
        // add online data to list
        mes.Clear;
        metr.GetCurrentMeasurement(mes.Temperature, mes.CO2Level, mes.Humidity);
        mes.Date := now;
        mes.InternalDate := (SecondsBetween(mes.Date, EncodeDate(2000, 1, 1)) div FInterval) * FInterval;

        LastMesMade := mes.InternalDate;

        cs.Enter;
        try
          SetLength(FMeasurements, length(FMeasurements) + 1);
          FMeasurements[length(FMeasurements) - 1] := mes;
        finally
          cs.Leave;
        end;

        // sync clock and get offline data {TODO}
        if setDT + 60 / MinsPerDay < Now then
        try
          metr.GetMemoryStat(samplesCount, samplesRate, samplesStartDate);

          // if there is no filling memory. With guard interval
          if (samplesStartDate + (samplesRate * (samplesCount + 1) + 60) / SecsPerDay < Now) then
          begin
            // get data


            // set datetime and sampling
            setDT := Now;
            metr.SetDateTime(setDT);
            metr.SetSamplingRate(FInterval);
          end;
        except
          Sleep(100);
        end;



      except
        Sleep(100);
      end;
    end;

    for i := 1 to 10 do
      if not Terminated then sleep(50);
  end;
end;

function TCO2MeterConnector.GetData: TMeasurements;
begin
  cs.Enter;
  try
    Result := Copy(FMeasurements, 0, length(FMeasurements));
    SetLength(FMeasurements, 0);
  finally
    cs.Leave;
  end;
end;

function TCO2MeterConnector.GetDataCount: integer;
begin
  cs.Enter;
  try
    Result := length(FMeasurements)
  finally
    cs.Leave;
  end;
end;

function TCO2MeterConnector.GetDataStartDate: TDateTime;
var
  i: Integer;
begin
  Result := 0;

  cs.Enter;
  try
    for i := 0 to length(FMeasurements) - 1 do
      if (Result < FMeasurements[i].Date) or (Result = 0) then
        Result := FMeasurements[i].Date;
  finally
    cs.Leave;
  end;
end;

end.
