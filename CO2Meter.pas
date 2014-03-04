unit CO2Meter;

//
// COM port component http://sourceforge.net/projects/comport/files/comport/
//

interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  cport;

type

  TCO2Meter = class
  private
    FPort: TComPort;
    FComPortActive: boolean;

    procedure PortSend(AData: string);
    function PortReceive(AOneLine: boolean; ATimeout: cardinal; ATimeoutAfterLastSymb: cardinal): string;
    function SendCommand(ACommand: string; ATimeout: cardinal; ATimeoutAfterLastSymb: cardinal): string;
  public
    constructor Create;
    destructor Free;

    procedure OpenPort(AComPort: integer);
    procedure ClosePort;

    procedure GetInfo(var Id: string; var Version: string);
  end;

implementation

{ TCO2Meter }

procedure TCO2Meter.ClosePort;
begin
  FPort.Close;
end;

constructor TCO2Meter.Create;
begin
  inherited Create;

  FComPortActive := false;
end;

destructor TCO2Meter.Free;
begin
  inherited Free;

end;

procedure TCO2Meter.GetInfo(var Id, Version: string);
var
  res: string;
  i: integer;
begin
  Id := '';
  Version := '';

  res := SendCommand('I', 0, 0);
  if length(res) < 4 then Exception.Create('No response from device.');
  if res[length(res)] <> #$0D then Exception.Create('Invalid response from device.');
  if res[1] <> 'i' then Exception.Create('Invalid class response from device. class=' + res[1]);
  for i := 1 to length(res) do if res[i] = #0 then res[i] := ' ';
  res := Copy(res, Pos(' ', res) + 1, length(res));
  Id := Copy(res, 1, Pos(' ', res) - 1);
  res := Copy(res, Pos(' ', res) + 1, length(res));
  Version := Trim(Copy(res, 1, length(res) - 1));
end;

procedure TCO2Meter.OpenPort(AComPort: integer);
begin
  FPort := TComPort.Create(Nil);
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
  if not FPort.Connected then Exception.Create('COM port not opened');

  //flush
  FPort.ReadStr(dataout, FPort.InputCount);
  //write
  FPort.WriteStr(AData);
end;

function TCO2Meter.SendCommand(ACommand: string; ATimeout,
  ATimeoutAfterLastSymb: cardinal): string;
begin
  PortSend(ACommand + #$0D);

  Result := PortReceive(true, ATimeout, ATimeoutAfterLastSymb);
end;

end.
