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

    FAZLogStartDate,
    FAZLogEndDate,
    FLastAZDateGot: TDateTime;

    LastMesMade: integer;
    metr: TCO2Meter;
    cs: TCriticalSection;
    function GetConnected: boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;

    function GetData: TMeasurements;
    function GetDataCount: integer;
    function GetDataStartDate: TDateTime;

    property ComPort: integer read FComPort write FComPort;

    property AZLogStartDate: TDateTime read FAZLogStartDate;
    property AZLogEndDate: TDateTime read FAZLogEndDate;
    property DataStartDate: TDateTime read GetDataStartDate;
    property DataCount: integer read GetDataCount;

    property Connected: boolean read GetConnected;
  end;

implementation

{ TCO2MeterConnector }

constructor TCO2MeterConnector.Create;
begin
  inherited;

  FAZLogStartDate := 0;
  FAZLogEndDate := 0;
  FLastAZDateGot := 0;

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
  mesl: TMeasurements;
  secs: integer;
  setDT: TDateTime;
  samplesCount,
  samplesRate: Cardinal;
  samplesStartDate,
  samplesEndDate: TDateTime;
  samples: TStringList;
begin
  setDT := 0;

  LastMesMade := (SecondsBetween(now, EncodeDate(2000, 1, 1)) div FInterval) * FInterval;
  sleep(100);

  while not Terminated do
  begin
    if not metr.Connected then
      try
        metr.OpenPort(FComPort);
      except
      end;

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
          samplesEndDate := SamplesStartDate + SamplesRate * (SamplesCount / 3 - 1) / SecsPerDay;

          FAZLogStartDate := samplesStartDate;
          FAZLogEndDate := samplesEndDate;

          // if there is no filling memory. (With guard interval)
          if (samplesEndDate + 60 / SecsPerDay < Now) or
             (samplesCount >= CO2METER_MAX_MEM_SAMPLES * 3) then
          begin
            //if we have got data - do not get it twice!
            if FLastAZDateGot < FAZLogEndDate then
            begin
              // get data
              samples := TStringList.Create;
              try
                metr.GetSamples(samplesCount, samplesRate, samplesStartDate, samples);

                // get samples from string list
                SetLength(mesl, 0);
                for i := 0 to samples.Count - 1 do
                begin
                  if (length(samples[i]) < 7) or (samples[i][1] <> 'd') then Continue;

                  mes.FromAZLog(samples[i]);
                  mes.Date := SamplesStartDate + i * integer(SamplesRate) / SecsPerDay;
                  mes.SetInternalDateFromDate;

                  SetLength(mesl, length(mesl) + 1);
                  mesl[length(mesl) - 1] := mes;
                end;

                // sort samples (it is allready sorted, but....)
                mesl.Sort;

                // normalize samples. each sample must have "date" field according to FInterval
                mesl.Normalize(FInterval);

                // remove duplicates
                mesl.MarkDuplicates;
                mesl.RemoveNullData;

                // add samples
                for i := 0 to length(mesl) - 1 do
                begin
                  cs.Enter;
                  try
                    SetLength(FMeasurements, length(FMeasurements) + 1);
                    FMeasurements[length(FMeasurements) - 1] := mesl[i];
                  finally
                    cs.Leave;
                  end;
                end;

                FLastAZDateGot := FAZLogEndDate;

                // start another sampling cycle if I can  {TODO}

              finally
                FreeAndNil(samples)
              end;
            end;

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

function TCO2MeterConnector.GetConnected: boolean;
begin
  Result := metr.Connected;
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
