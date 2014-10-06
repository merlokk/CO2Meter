unit def;

interface
uses
  SysUtils, Variants, System.AnsiStrings;

type
  TMeasurementRec = packed record
    Id,
    EditTag: string;

    Date: TDateTime;
    InternalDate: int64;
    Temperature: real;
    CO2Level: integer;
    Humidity: real;

    procedure Clear;
  end;

  TMeasurements = array of TMeasurementRec;

implementation

{ TMeasurementRec }

procedure TMeasurementRec.Clear;
begin
  Id := '';
  EditTag := '';

  Date := 0;
  InternalDate := 0;
  Temperature := 0;
  CO2Level := 0;;
  Humidity := 0;
end;

end.
