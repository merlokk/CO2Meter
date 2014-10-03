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
begin
  dirID := FAPI.GetDirectoryID('root', 'CO2Meter');
  if dirID = '' then dirID := FAPI.CreateDirectory('root', 'CO2Meter');

  fileName := FormatDateTime('yyyy_mm', Now);
  fileID := FAPI.GetFileID(dirID, fileName);
  if fileID = '' then fileID := FAPI.CreateFile(dirID, fileName);

  if fileID = '' then exit;

  ws := FAPI.GetWorksheetList(fileID);
end;

end.
