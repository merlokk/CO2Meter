unit GoogleSender;

interface
uses
  System.Classes, SysUtils, Variants, System.AnsiStrings, GoogleAPI, def;

type
  TGoogleSender = class
  private
    FAPI: TGoogleAPI;
  public
    constructor Create(Sender: TComponent; AClientID, AClientSecret: string);

    procedure SendData(AMeasurements: TMeasurements);
  end;

implementation

{ TGoogleSender }

constructor TGoogleSender.Create(Sender: TComponent; AClientID, AClientSecret: string);
begin
  FAPI := TGoogleAPI.Create(Sender, AClientID, AClientSecret);
end;

procedure TGoogleSender.SendData(AMeasurements: TMeasurements);
var
  dirID,
  fileName,
  fileID: string;
  ws: TWorksheets;
  wst: TWorksheet;
  wsID: string;
  i: integer;
  s: string;
begin
  // get file directory
  dirID := FAPI.GetDirectoryID('root', 'CO2Meter');
  if dirID = '' then dirID := FAPI.CreateDirectory('root', 'CO2Meter');

  // get current file
  fileName := FormatDateTime('yyyy_mm', Now);
  fileID := FAPI.GetFileID(dirID, fileName);
  if fileID = '' then fileID := FAPI.CreateFile(dirID, fileName);

  if fileID = '' then exit;

  // get worksheet from file
  wsID := '';
  wst.Title := '';
  ws := FAPI.GetWorksheetList(fileID);
  for i := 0 to length(ws) - 1 do
  if ws[i].Title = 'CO2Data' then
  begin
    wst := ws[i];
    break;
  end;

  // if we dont have program's worksheet - try to rename Sheet1 (default sheet)
  if wst.Id = '' then
  begin
    for i := 0 to length(ws) - 1 do
    if ws[i].Title = 'Sheet1' then
    begin
      wst := ws[i];
      break;
    end;

    if wst.Id <> '' then
    begin
      s := FAPI.GetCells(wst.Id, 1, 1, 1, 4);
      if s = '' then
        wst := FAPI.EditWorksheetParams(fileID, wst.Id, wst.EditTag, 'CO2Data', 1000, 10);
    end;
  end;

  // if we cant do anything with default sheet - create our worksheet into file
  if (wst.Id = '') or (wst.Title <> 'CO2Data') then
    wst := FAPI.CreateWorksheet(fileID, 'CO2Data', 1000, 10);

  // get worksheet header
  if wst.Id <> '' then exit;
    s := FAPI.GetCells(wst.Id, 1, 1, 1, 4);

  // if header is empty - save the new one
  if s = '' then
//    FAPI.SetCells(wst.Id, 1, 1, 1, 4, 'Date, DateInt, Temperature, Humidity, CO2Level');

end;

end.
