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
    function PortReceive(ATimeout: integer; ATimeoutAfterLastSymb: integer): string;
    function SendCommand(ACommand: string; ATimeout: integer; ATimeoutAfterLastSymb: integer): string;
  public
    constructor Create;
    destructor Free;

    procedure OpenPort(AComPort: integer);
    procedure ClosePort;

    function GetInfo(var Id: string; var Version: string): string;
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

function TCO2Meter.GetInfo(var Id, Version: string): string;
var
  res: string;
begin
  Result := '';
  Id := '';
  Version := '';

  res := SendCommand('I', 0, 0);
end;

procedure TCO2Meter.OpenPort(AComPort: integer);
begin
  FPort := TComPort.Create(Nil);
  FPort.Port := 'COM' + IntToStr(AComPort);
  FPort.BaudRate := br9600;

  FPort.Open;
end;

function TCO2Meter.PortReceive(ATimeout: integer; ATimeoutAfterLastSymb: integer): string;
var
  dataout,
  s: string;
  cnt: integer;
  tc: cardinal;
begin
  dataout := '';

  tc := GetTickCount;

  while true do
  begin
    cnt := FPort.InputCount;
    if cnt <> 0 then
    begin
      FPort.ReadStr(s, cnt);
      dataout := dataout + s;
    end;

    sleep(10);
    if tc + ATimeout < GetTickCount then break;
  end;

  // похоже выгребли не все
  if (dataout = '') or (crc16AnsiString(dataout) <> #0#0) then
   begin
    sleep(100);

    FPort.ReadStr(s, FPort.InputCount);
    dataout := dataout + s;
    if s <> '' then Result := merNone;
   end;
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
  ATimeoutAfterLastSymb: integer): string;
begin
  PortSend(ACommand);

  Result := PortReceive;
end;

end.
