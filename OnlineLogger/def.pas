unit def;

interface
uses
  SysUtils, Variants, System.AnsiStrings, Generics.Collections, Generics.Defaults,
  System.JSON, Classes;

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

    procedure FromAZLog(ASample: string);

    procedure SetDateFromInternalDate;
    procedure SetInternalDateFromDate;
  end;

  TMeasurements = array of TMeasurement;

  TMeasurementsHelper = record helper for TMeasurements
    procedure Sort;
    procedure Normalize(AInterval: integer);
    function GetLinearIntData(ADate: int64): TMeasurement;  // linear aproximation of data

    procedure MarkDuplicates;  // mark duplicates with InternalDate = 0
    procedure RemoveNullData;  // clear entries with InternalDate = 0
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

function TMeasurementsHelper.GetLinearIntData(ADate: int64): TMeasurement;
var
  i: integer;
  shift,
  delta: int64;
begin
  Result.InternalDate := 0;

  for i := 0 to length(Self) - 2 do
    if (Self[i].InternalDate <= ADate) and (Self[i + 1].InternalDate >= ADate) then
    begin
      Result.InternalDate := ADate;
      Result.SetDateFromInternalDate;

      shift := ADate - Self[i].InternalDate;
      delta := Self[i + 1].InternalDate - Self[i].InternalDate;

      if delta <> 0 then
      begin
        Result.CO2Level := Trunc(
          Self[i].CO2Level + (Self[i + 1].CO2Level - Self[i].CO2Level) * (shift / delta) );
        Result.Temperature :=
          Self[i].Temperature + (Self[i + 1].Temperature - Self[i].Temperature) * (shift / delta);
        Result.Humidity :=
          Self[i].Humidity + (Self[i + 1].Humidity - Self[i].Humidity) * (shift / delta);
      end
      else
        Result := Self[i];

      break;
    end;
end;

procedure TMeasurementsHelper.MarkDuplicates;
var
  i: integer;
begin
  for i := length(Self) - 1 downto 1 do
    if Self[i].InternalDate = Self[i - 1].InternalDate then
      Self[i].InternalDate := 0;
end;

procedure TMeasurementsHelper.Normalize(AInterval: integer);
var
  i: integer;
begin
  if length(Self) < 1 then exit;

  if length(Self) < 2 then
  begin
    Self[1].InternalDate := (Self[1].InternalDate div AInterval) * AInterval;
    Self[1].SetDateFromInternalDate;
    exit;
  end;

  for i := 0 to length(Self) - 1 do
    Self[i] := Self.GetLinearIntData( (Self[i].InternalDate div AInterval + 1) * AInterval );

  SetLength(Self, length(Self) - 1);
end;

procedure TMeasurementsHelper.RemoveNullData;
var
  i: Integer;
  j: Integer;
begin
  for i := length(Self) - 1 downto 0 do
    if Self[i].InternalDate = 0 then
    begin
      for j := i to length(Self) - 2 do
        Self[j] := self[j + 1];
      SetLength(Self, length(Self) - 1);
    end;
end;

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

procedure TMeasurement.FromAZLog(ASample: string);
begin
  Clear;

  // format: "d 261 697 701" --> "Temperature, CO2level, Humidity"
  if (length(ASample) < 7) or (ASample[1] <> 'd') then exit;
  with TStringList.Create do
  try
    Delimiter := ' ';
    DelimitedText := ASample;
    if Count < 4 then exit;

    Temperature := StrToIntDef(Strings[1], 0) / 10;
    CO2Level := StrToIntDef(Strings[2], 0);
    Humidity := StrToIntDef(Strings[3], 0) / 10;
  finally
    Free;
  end;

end;

procedure TMeasurement.Serialize(AItem: TJSONObject);
begin
  AItem.addpair('date', DateTimeToStr(Date));
  AItem.addpair('idate', IntToStr(InternalDate));
  AItem.addpair('temperature', FloatToJson(Temperature));
  AItem.addpair('co2level', IntToStr(CO2Level));
  AItem.addpair('humidity', FloatToJson(Humidity));
end;

procedure TMeasurement.SetDateFromInternalDate;
begin
  Date :=  EncodeDate(2000, 1, 1) + InternalDate / SecsPerDay;
end;

procedure TMeasurement.SetInternalDateFromDate;
begin
  InternalDate := Round((Date - EncodeDate(2000, 1, 1)) * SecsPerDay);
end;

end.
