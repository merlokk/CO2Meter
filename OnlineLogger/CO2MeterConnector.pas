unit CO2MeterConnector;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, System.AnsiStrings,
  CO2Meter, def;

type
  TCO2MeterConnector = class (TThread)
  private
    metr: TCO2Meter;

    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetData;
  end;

implementation

{ TCO2MeterConnector }

constructor TCO2MeterConnector.Create;
begin
  inherited;

  metr := TCO2Meter.Create;
end;

destructor TCO2MeterConnector.Destroy;
begin
  metr.Destroy;

  inherited;
end;

procedure TCO2MeterConnector.Execute;
var
  i: integer;
begin
  while not Terminated do
  begin
    if not metr.Connected then metr.OpenPort(10);



    for i := 1 to 10 do
      if not Terminated then sleep(50);
  end;
end;

procedure TCO2MeterConnector.GetData;
begin

end;

end.
