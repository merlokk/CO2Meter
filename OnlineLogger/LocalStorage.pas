unit LocalStorage;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, System.AnsiStrings,
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
begin


  Sort;
end;

procedure TLocalStorage.Save;
begin
  Sort;

end;

procedure TLocalStorage.Sort;
begin
  FMesArray.Sort;
end;

end.
