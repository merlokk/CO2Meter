unit CO2Meter;

//
// COM port component from http://sourceforge.net/projects/comport/files/comport/
//

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, DateUtils,
  cport;

type

  TCO2Meter = class
  private
    FPort: TComPort;
    FComPortActive: boolean;

    procedure PortSend(AData: string);
    function PortReceive(AOneLine: boolean; ATimeout: cardinal; ATimeoutAfterLastSymb: cardinal): string;
    function SendCommand(ACommand: string; ATimeout: cardinal; ATimeoutAfterLastSymb: cardinal): string;

    function StrNextVal(s: string): string;
    function StrGetVal(s: string): string;
    function StrGetLastVal(s: string): string;

    procedure CheckResponse(r: string; CmdClass: Char);
    procedure ParseGetMemoryStat(res: string; var SamplesCount: Cardinal; var SamplesRate: Cardinal; var SamplesStartDate: TDateTime);
    function GetConnected: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenPort(AComPort: integer);
    procedure ClosePort;

    procedure GetInfo(var Id: string; var Version: string);
    procedure GetMemoryStat(var SamplesCount: cardinal; var SamplesRate: cardinal; var SamplesStartDate: TDateTime);
    function RawGetSamples: string;
    procedure GetSamples(var SamplesCount: cardinal; var SamplesRate: cardinal; var SamplesStartDate: TDateTime; Samples: TStringList);
    procedure GetCurrentMeasurement(var Temperature: real; var CO2Level: integer; var Humidity: real);

    procedure SetDateTime(DT: TDateTime);
    procedure SetId(Id: string);
    procedure SetSamplingRate(SamplesRate: cardinal);

    property Connected: boolean read GetConnected;
  end;

const
  CO2METER_MAX_MEM_SAMPLES = 5333;

implementation

{ TCO2Meter }

procedure TCO2Meter.CheckResponse(r: string; CmdClass: Char);
begin
  if length(r) < 4 then raise Exception.Create('No response from device.');
  if r[length(r)] <> #$0D then raise Exception.Create('Invalid response from device.');
  if r[1] <> CmdClass then raise Exception.Create('Invalid class response from device. class=' + r[1]);
end;

procedure TCO2Meter.ClosePort;
begin
  FPort.Connected := false;
end;

constructor TCO2Meter.Create;
begin
  inherited Create;

  FComPortActive := false;
  FPort := TComPort.Create(Nil);
end;

destructor TCO2Meter.Destroy;
begin
  FPort.Free;

  inherited
end;

function TCO2Meter.GetConnected: boolean;
begin
  Result := FPort.Connected;
end;

procedure TCO2Meter.GetCurrentMeasurement(var Temperature: real;
  var CO2Level: integer; var Humidity: real);
var
  res,
  temp,
  co2,
  hum: string;
  sl: TStringList;
begin
  Temperature := 0;
  CO2Level := 0;
  Humidity := 0;

  res := SendCommand(':', 500, 0);
  CheckResponse(res, ':');

  sl := TStringList.Create;
  try
    sl.Delimiter := ':';
    sl.DelimitedText := res;
    if sl.Count < 4 then Exception.Create('Invalid response from device. Wrong response length');
    temp := sl[1];
    co2 := sl[2];
    hum := sl[3];
  finally
    sl.Free;
  end;

  if temp[1] <> 'T' then Exception.Create('Invalid response from device. Can''t get temperature');
  temp := Copy(temp, 2, length(temp) - 2);
  temp := StringReplace(temp, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  Temperature := StrToFloatDef(temp, 0);

  if co2[1] <> 'C' then Exception.Create('Invalid response from device. Can''t get CO2 level');
  co2 := Copy(co2, 2, length(co2) - 4);
  CO2Level := StrToIntDef(co2, 0);

  if hum[1] <> 'H' then Exception.Create('Invalid response from device. Can''t get humidity');
  hum := Copy(hum, 2, length(hum) - 2);
  hum := StringReplace(hum, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  Humidity := StrToFloatDef(hum, 0);
end;

procedure TCO2Meter.GetInfo(var Id, Version: string);
var
  res: string;
  i: integer;
begin
  Id := '';
  Version := '';

  res := SendCommand('I', 500, 0);
  CheckResponse(res, 'i');

  // fixes bug
  for i := 1 to length(res) do if res[i] = #0 then res[i] := ' ';

  res := StrNextVal(res);
  Id := StrGetVal(res);
  res := StrNextVal(res);
  Version := Trim(StrGetLastVal(res));
end;

procedure TCO2Meter.GetMemoryStat(var SamplesCount, SamplesRate: cardinal;
  var SamplesStartDate: TDateTime);
var
  res: string;
begin
  SamplesCount := 0;
  SamplesRate := 0;
  SamplesStartDate := EncodeDate(2000, 1, 1);

  res := SendCommand('M', 500, 0);
  CheckResponse(res, 'm');
  ParseGetMemoryStat(res, SamplesCount, SamplesRate, SamplesStartDate);
end;

procedure TCO2Meter.GetSamples(var SamplesCount: cardinal; var SamplesRate: cardinal; var SamplesStartDate: TDateTime; Samples: TStringList);
var
  res: string;
  sl: TStringList;
begin
  Samples.Clear;

  res := RawGetSamples;
  sl := TStringList.Create;
  try
   sl.Text := res;

   if sl.Count < 2 then raise Exception.Create('Invalid received data.');

   ParseGetMemoryStat(sl[0] + #$0D, SamplesCount, SamplesRate, SamplesStartDate);
   sl.Delete(0);

   if sl.Count <> integer(SamplesCount div 3) then raise Exception.Create('Length of received data less then declared.');

   Samples.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TCO2Meter.OpenPort(AComPort: integer);
begin
  FPort.Port := 'COM' + IntToStr(AComPort);
  FPort.BaudRate := br9600;

  FPort.Open;
end;

function TCO2Meter.PortReceive(AOneLine: boolean; ATimeout: cardinal; ATimeoutAfterLastSymb: cardinal): string;
var
  dataout,
  s: string;
  cnt: integer;
  tc,
  tcb: cardinal;
begin
  dataout := '';

  tc := GetTickCount;
  tcb := 0;

  while true do
  begin
    cnt := FPort.InputCount;
    if cnt <> 0 then
    begin
      FPort.ReadStr(s, cnt);
      dataout := dataout + s;
      tcb := GetTickCount;
    end;

    sleep(10);
    if (ATimeout <> 0) and (tc + ATimeout < GetTickCount) then break;
    if (ATimeoutAfterLastSymb <> 0) and (tcb <> 0) and (tcb + ATimeoutAfterLastSymb < GetTickCount) then break;
    if AOneLine and (length(dataout) > 0)  and (dataout[length(dataout)] = #$0D) then break;
  end;

  Result := dataout;
end;

procedure TCO2Meter.PortSend(AData: string);
var
  dataout: string;
begin
  if not FPort.Connected then raise Exception.Create('COM port not opened');

  //flush
  FPort.ReadStr(dataout, FPort.InputCount);
  //write
  FPort.WriteStr(AData);
end;

function TCO2Meter.RawGetSamples: string;
begin
  PortSend('D' + #$0D);

  Result := PortReceive(false, 0, 500);
end;

function TCO2Meter.SendCommand(ACommand: string; ATimeout,
  ATimeoutAfterLastSymb: cardinal): string;
begin
  PortSend(ACommand + #$0D);

  Result := PortReceive(true, ATimeout, ATimeoutAfterLastSymb);
end;

procedure TCO2Meter.SetDateTime(DT: TDateTime);
var
  res: string;
  d: integer;
begin
  d := SecondsBetween(EncodeDate(2000, 1, 1), DT);

  res := SendCommand('C ' + IntToStr(d), 500, 0);
  if res <> '>'#$0D then raise Exception.Create('Invalid response from device.');
end;

procedure TCO2Meter.SetId(Id: string);
var
  res: string;
begin
  if length(Id) <> 8 then raise Exception.Create('Invalid ID length.');

  res := SendCommand('J ' + Id + ' 1', 500, 0);
  CheckResponse(res, 'i');
end;

procedure TCO2Meter.SetSamplingRate(SamplesRate: cardinal);
var
  res: string;
begin
  if SamplesRate > SecsPerDay then raise Exception.Create('Invalid sampling rate.');

  res := SendCommand('S ' + IntToStr(SamplesRate), 500, 0);
  CheckResponse(res, 'm');
end;

procedure TCO2Meter.ParseGetMemoryStat(res: string; var SamplesCount: Cardinal; var SamplesRate: Cardinal; var SamplesStartDate: TDateTime);
begin
  SamplesCount := 0;
  SamplesRate := 0;
  SamplesStartDate := EncodeDate(2000, 1, 1);

  res := StrNextVal(res);
  SamplesCount := StrToIntDef(StrGetVal(res), 0);

  res := StrNextVal(res);
  SamplesRate := StrToIntDef(StrGetVal(res), 0);

  res := StrNextVal(res);
  //  here constant "C"

  res := StrNextVal(res);
  res := '$' + StringReplace(StrGetLastVal(res), ' ', '', [rfReplaceAll]);
  SamplesStartDate := SamplesStartDate + StrToIntDef(res, 0) / SecsPerDay;
end;

function TCO2Meter.StrGetLastVal(s: string): string;
begin
  Result := Copy(s, 1, length(s) - 1);
end;

function TCO2Meter.StrGetVal(s: string): string;
begin
  Result := Copy(s, 1, Pos(' ', s) - 1);
end;

function TCO2Meter.StrNextVal(s: string): string;
begin
  Result := Copy(s, Pos(' ', s) + 1, length(s));
end;

end.
