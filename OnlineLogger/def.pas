unit def;

interface
uses
  SysUtils, Variants, System.AnsiStrings, Generics.Collections, Generics.Defaults,
  System.JSON;

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
    procedure Serialize(AItem: TJSONObject);
    procedure Deserialize(AItem: TJSONObject);
  end;

  TMeasurements = array of TMeasurement;

  TMeasurementsHelper = record helper for TMeasurements
    procedure Sort;
  end;

function ExtractFromQuotes(s: string): string;

implementation

{ functions }

function ExtractFromQuotes(s: string): string;
begin
  if (length(s) > 1) and (s[1] = '"') then s := Copy(s, 2, length(s));
  if (length(s) > 1) and (s[length(s)] = '"') then s := Copy(s, 1, length(s) - 1);
  Result := s;
end;

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

procedure TMeasurement.Deserialize(AItem: TJSONObject);
begin
  Clear;

  Date := StrToDateTimeDef(ExtractFromQuotes(AItem.GetValue('date').ToString), 0);
  InternalDate := StrToIntDef(ExtractFromQuotes(AItem.GetValue('idate').ToString), 0);
  Temperature := JsonToFloat(ExtractFromQuotes(AItem.GetValue('temperature').ToString));
  CO2Level := StrToIntDef(ExtractFromQuotes(AItem.GetValue('co2level').ToString), 0);
  Humidity := JsonToFloat(ExtractFromQuotes(AItem.GetValue('humidity').ToString));

  if InternalDate = 0 then Clear;
end;

procedure TMeasurement.Serialize(AItem: TJSONObject);
begin
  AItem.addpair('date', DateTimeToStr(Date));
  AItem.addpair('idate', IntToStr(InternalDate));
  AItem.addpair('temperature', FloatToJson(Temperature));
  AItem.addpair('co2level', IntToStr(CO2Level));
  AItem.addpair('humidity', FloatToJson(Humidity));
end;

end.
