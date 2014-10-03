unit def;

interface
uses
  SysUtils, Variants, System.AnsiStrings;

type
  TMeasurementRec = packed record
    Data: TDateTime;
    Temperature: real;
    CO2Level: integer;
    Humidity: real;
  end;

  TMeasurements = array of TMeasurementRec;

implementation

end.
