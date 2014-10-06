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
  cls: TGCells;
  wsID: string;
  i: integer;
  me: TMeasurement;
  mes: TMeasurements;
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
      cls := FAPI.GetCells(fileID, wst.Id, 1, 1, 1, 5);
      if FAPI.GetCellValue(cls, 1, 1) = '' then
        wst := FAPI.EditWorksheetParams(fileID, wst.Id, wst.EditTag, 'CO2Data', 1000, 10);
    end;
  end;

  // if we cant do anything with default sheet - create our worksheet into file
  if (wst.Id = '') or (wst.Title <> 'CO2Data') then
    wst := FAPI.CreateWorksheet(fileID, 'CO2Data', 1000, 10);

  // get worksheet header
  if wst.Id = '' then exit;
  cls := FAPI.GetCells(fileID, wst.Id, 1, 1, 1, 5);

  // if header is empty - save the new one
  if FAPI.GetCellValue(cls, 1, 1) = '' then
  begin
    FAPI.SetCellValue(cls, 1, 1, 'Date');
    FAPI.SetCellValue(cls, 1, 2, 'InternalDate');
    FAPI.SetCellValue(cls, 1, 3, 'Temperature');
    FAPI.SetCellValue(cls, 1, 4, 'Humidity');
    FAPI.SetCellValue(cls, 1, 5, 'CO2Level');

    FAPI.SetCells(fileID, wst.Id, cls);
  end;

  // works with data
  for i := 0 to length(AMeasurements) - 1 do
  begin
    mes := FAPI.GetListRow(fileID, wst.Id, 'internaldate=' + IntToStr(AMeasurements[i].InternalDate));
    if length(mes) = 0 then
      me := FAPI.AddListRow(fileID, wst.Id, AMeasurements[i]);
  end;
end;

end.
