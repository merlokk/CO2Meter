unit LocalStorage;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, System.AnsiStrings, System.JSON, System.IOUtils,
  def;

type
  TLocalStorage = class
  private
    FStoreFileName: string;

    FMesArray: TMeasurements;
    procedure Sort;
  public
    constructor Create(AStoreFileName: string);
    destructor Destroy; override;

    procedure Save;
    procedure Load;
    procedure Add(AMArray: TMeasurements);
    function Get: TMeasurements;
    procedure Clear;
  end;


implementation

{ TLocalStorage }

procedure TLocalStorage.Add(AMArray: TMeasurements);
var
  start,
  i: Integer;
begin
  if length(AMArray) = 0 then exit;

  start := length(FMesArray);
  SetLength(FMesArray, length(FMesArray) + length(AMArray));

  for i := start to Length(FMesArray) - 1 do
    FMesArray[i] := AMArray[i - start];

  Sort;
end;

procedure TLocalStorage.Clear;
begin
  SetLength(FMesArray, 0);
end;

constructor TLocalStorage.Create(AStoreFileName: string);
begin
  FStoreFileName := AStoreFileName;

  Clear;
end;

destructor TLocalStorage.Destroy;
begin
  Clear;

  inherited;
end;

function TLocalStorage.Get: TMeasurements;
begin
  Result := FMesArray;
end;

procedure TLocalStorage.Load;
var
  js,
  item: TJSONObject;
  arr: TJSONArray;
  i: integer;
  mes: TMeasurement;
begin
  js := nil;
  try
    js := TJSONObject.ParseJSONValue(TFile.ReadAllText(FStoreFileName)) as TJSONObject;
  except
  end;

  Clear;
  if Assigned(js) then
  begin
    arr := js.GetValue('list') as TJSONArray;
    if not Assigned(arr) then exit;

    for i := 0 to arr.Count - 1 do
    begin
      item := arr.Items[i] as TJSONObject;
      if not Assigned(item) then continue;

      mes.Deserialize(item);
      if mes.InternalDate = 0 then continue;

      SetLength(FMesArray, length(FMesArray) + 1);
      FMesArray[length(FMesArray) - 1] := mes;
    end;

    js.Free;
  end;

  Sort;
end;

procedure TLocalStorage.Save;
var
  i: Integer;
  js,
  item: TJSONObject;
  arr: TJSONArray;
begin
  Sort;

  js := TJSONObject.Create;
  arr := TJSONArray.Create;

  for i := 0 to length(FMesArray) - 1 do
  begin
   item := TJSONObject.Create;
   FMesArray[i].Serialize(item);
   arr.Add(item);
  end;

  js.AddPair(TJSONPair.Create('list', arr));

   TFile.WriteAllText(FStoreFileName, js.ToString);

   js.Free;
end;

procedure TLocalStorage.Sort;
begin
  FMesArray.Sort;
end;

end.
