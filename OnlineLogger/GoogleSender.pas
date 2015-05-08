unit GoogleSender;

interface
uses
  System.Classes, SysUtils, Variants, System.AnsiStrings, GoogleAPI, DateUtils,
  IniFiles,
  def;

type
  TGoogleSender = class
  private
    FAPI: TGoogleAPI;

    FFileName,
    FFileID: string;
    FMyWorksheet: TWorksheet;

    FAddedRec: TIntDatesQueue;

    function GetFileName(AFileDate: TDateTime): string;

    procedure ChangeWorkFile(FileDate: TDateTime);
    function SendData1Month(AMeasurements: TMeasurements): boolean;
  public
    constructor Create(Sender: TComponent; AClientID, AClientSecret: string; AIniFile: TIniFile = nil);
    destructor Destroy; override;

    function isValidWorkFile: boolean;
    function SendData(AMeasurements: TMeasurements): boolean;

    property AddedRecords: TIntDatesQueue read FAddedRec;
  end;

implementation

{ TGoogleSender }

constructor TGoogleSender.Create(Sender: TComponent; AClientID, AClientSecret: string; AIniFile: TIniFile = nil);
begin
  FFileName := '';
  FFileID := '';
  FMyWorksheet.Clear;

  FAddedRec := TIntDatesQueue.Create;

  FAPI := TGoogleAPI.Create(Sender, AClientID, AClientSecret, AIniFile);
end;

destructor TGoogleSender.Destroy;
begin
  FAddedRec.Free;
  FAPI.Free;

  inherited;
end;

function TGoogleSender.isValidWorkFile: boolean;
begin
  Result := (FFileName <> '') and
    (FFileID <> '') and
    (FMyWorksheet.Id <> '');
end;

function TGoogleSender.SendData1Month(AMeasurements: TMeasurements): boolean;
var
  i: integer;
  me: TMeasurement;
  mes: TMeasurements;
begin
  Result := true;
  if length(AMeasurements) = 0 then exit;

  Result := false;

  // if we have a valid workfile links then don't refresh workfile
  if (not isValidWorkFile) or (FFileName <> GetFileName(AMeasurements[0].Date)) then
    ChangeWorkFile(AMeasurements[0].Date);

  if not isValidWorkFile then exit;

  // works with data
  for i := 0 to length(AMeasurements) - 1 do
  try
    mes := FAPI.GetListRow(FFileID, FMyWorksheet.Id, 'internaldate=' + IntToStr(AMeasurements[i].InternalDate));
    if length(mes) = 0 then
    begin
      me := FAPI.AddListRow(FFileID, FMyWorksheet.Id, AMeasurements[i]);
      if me.InternalDate = 0 then
      begin
        FFileID := '';
        break;
      end;
    end;

    FAddedRec.Enqueue(AMeasurements[i].InternalDate);
  except
    FFileID := '';
    break;
  end;

  Result := isValidWorkFile;
end;

function TGoogleSender.SendData(AMeasurements: TMeasurements): boolean;
var
  i: Integer;
  bgmonth: integer;
  bgid: integer;
  tmes: TMeasurements;
begin
  Result := true;
  FAddedRec.Clear;
  if length(AMeasurements) = 0 then exit;

  // data may be from different months. we separate it.
  bgid := 0;
  bgmonth := MonthOfTheYear(AMeasurements[bgid].Date);
  for i := 0 to length(AMeasurements) - 1 do
    if bgmonth <> MonthOfTheYear(AMeasurements[i].Date) then
    begin
      tmes := Copy(AMeasurements, bgid, i - bgid);

      bgid := i;
      bgmonth := MonthOfTheYear(AMeasurements[bgid].Date);

      Result := Result and SendData1Month(tmes);
    end;

  // the last portion of data
  tmes := Copy(AMeasurements, bgid, length(AMeasurements));
  Result := Result and SendData1Month(tmes);
end;

function TGoogleSender.GetFileName(AFileDate: TDateTime): string;
begin
  Result := FormatDateTime('yyyy_mm', AFileDate);
end;

procedure TGoogleSender.ChangeWorkFile(FileDate: TDateTime);
var
  dirID,
  wsID: string;
  ws: TWorksheets;
  cls: TGCells;
  i: Integer;
begin
  // update current file name
  FFileName := GetFileName(FileDate);

  // get file directory
  dirID := FAPI.GetDirectoryID('root', 'CO2Meter');
  if dirID = '' then
    dirID := FAPI.CreateDirectory('root', 'CO2Meter');

  // get current file
  FFileID := FAPI.GetFileID(dirID, FFileName);
  if FFileID = '' then
  begin
    FFileID := FAPI.CreateFile(dirID, FFileName);
    Sleep(300);
  end;

  if FFileID = '' then exit;

  // get worksheet from file
  wsID := '';
  FMyWorksheet.Title := '';
  ws := FAPI.GetWorksheetList(FFileID);
  for i := 0 to length(ws) - 1 do
    if ws[i].Title = 'CO2Data' then
    begin
      FMyWorksheet := ws[i];
      break;
    end;

  // if we dont have program's worksheet - try to rename Sheet1 (default sheet)
  if FMyWorksheet.Id = '' then
  begin
    for i := 0 to length(ws) - 1 do
      if ws[i].Title = 'Sheet1' then
      begin
        FMyWorksheet := ws[i];
        break;
      end;

    if FMyWorksheet.Id <> '' then
    begin
      cls := FAPI.GetCells(FFileID, FMyWorksheet.Id, 1, 1, 1, 5);
      if FAPI.GetCellValue(cls, 1, 1) = '' then
        FMyWorksheet := FAPI.EditWorksheetParams(FFileID, FMyWorksheet.Id, FMyWorksheet.EditTag, 'CO2Data', 1000, 10);
    end;
  end;

  // if we cant do anything with default sheet - create our worksheet into file
  if (FMyWorksheet.Id = '') or (FMyWorksheet.Title <> 'CO2Data') then
    FMyWorksheet := FAPI.CreateWorksheet(FFileID, 'CO2Data', 1000, 10);

  // get worksheet header
  if FMyWorksheet.Id = '' then exit;
  cls := FAPI.GetCells(FFileID, FMyWorksheet.Id, 1, 1, 1, 5);

  // if header is empty - save the new one
  if FAPI.GetCellValue(cls, 1, 1) = '' then
  begin
    FAPI.SetCellValue(cls, 1, 1, 'Date');
    FAPI.SetCellValue(cls, 1, 2, 'InternalDate');
    FAPI.SetCellValue(cls, 1, 3, 'Temperature');
    FAPI.SetCellValue(cls, 1, 4, 'Humidity');
    FAPI.SetCellValue(cls, 1, 5, 'CO2Level');
    FAPI.SetCells(FFileID, FMyWorksheet.Id, cls);
  end;
end;

end.
