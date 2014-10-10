unit def;

interface
uses
  SysUtils, Variants, System.AnsiStrings, Generics.Collections, Generics.Defaults;

type
  TMeasurement = packed record
    Id,
    EditTag: string;

    Date: TDateTime;
    InternalDate: int64;
    Temperature: real;
    CO2Level: integer;
    Humidity: real;

    procedure Clear;
    procedure Serialize;
    procedure Deserialize;
  end;

  TMeasurements = array of TMeasurement;

  TMeasurementsHelper = record helper for TMeasurements
    procedure Sort;
  end;

implementation

{ TMeasurementRec }

procedure TMeasurement.Clear;
begin
  Id := '';
  EditTag := '';

  Date := 0;
  InternalDate := 0;
  Temperature := 0;
  CO2Level := 0;
  Humidity := 0;
end;

{ TMeasurementsHelper }

procedure TMeasurementsHelper.Sort;
begin
  TArray.Sort<TMeasurement>(Self, TDelegatedComparer<TMeasurement>.Construct(
    function(const Left, Right: TMeasurement): Integer
    begin
      Result := TComparer<Integer>.Default.Compare(Left.InternalDate, Right.InternalDate);
    end)
  );
end;

procedure TMeasurement.Deserialize;
begin

end;

procedure TMeasurement.Serialize;
begin

end;

end.
