unit GoogleAPI;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XMLIntf, XMLDoc, StrUtils, System.AnsiStrings,
  REST.Types, System.JSON, IPPeerClient, DateUtils,
  REST.Authenticator.OAuth, REST.Authenticator.OAuth.WebForm.Win,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, def;

type
  TWorksheet = packed record
    Title,
    Id,
    EditTag: string;
    ColCount,
    RowCount: integer;

    procedure Clear;
  end;
  TWorksheets = array of TWorksheet;

  TGCell = packed record
    Id,
    Title,
    InputValue,
    Value,
    EditTag: string;
    Col,
    Row: integer;

    procedure Clear;
    function GetUrlEditTag: string;
  end;
  TGCells = array of TGCell;

  TGoogleAPI = class
  private
    FOwner: TComponent;

    OAuth2Authenticator: TOAuth2Authenticator;
    // google drive
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    // spreadsheet
    SRESTClient: TRESTClient;
    SRESTRequest: TRESTRequest;
    SRESTResponse: TRESTResponse;

    //  authenticator
    procedure TitleChanged(const ATitle: string; var DoCloseWebView: boolean);

    function GetAuthenticated: boolean;
    procedure ClearRESTConnector;

    function GetSpValue(val: TJSONValue): string;
    function ExtractFromQuotes(s: string): string;
    function ExtractWorksheetMetadata(spreadsheet: TJSONObject): TWorksheet;
    function ExtractCellMetadata(cell: TJSONObject): TGCell;
    function ExtractMeasurement(cell: TJSONObject): TMeasurement;
  public
    constructor Create(AOwner: TComponent; AClientID, AClientSecret: string);

    procedure Authenticate;
    function TryAuthenticate: boolean;
    property Authenticated: boolean read GetAuthenticated;

    function isDirectoryExist(AParent, ADirName: string): boolean;
    function CreateDirectory(AParent, ADirName: string): string;
    function GetDirectoryID(AParent, ADirName: string): string;

    function CreateFile(ADir, AFileName: string): string;
    function GetFileID(ADir, AFileName: string): string;

    function GetWorksheetList(AFileID: string): TWorksheets;
    function CreateWorksheet(AFileID, AWorksheetName: string; ARowCount, AColCount: integer): TWorksheet;
    function EditWorksheetParams(AFileID, AWorksheetID, AWorksheetVersion, AWorksheetName: string; ARowCount, AColCount: integer): TWorksheet;
    function GetCells(AFileID, AWorksheetID: string; AMinRow, AMaxRow, AMinCol, AMaxCol: integer): TGCells;
    function SetCell(AFileID, AWorksheetID: string; ACell: TGCell): TGCell;
    function SetCells(AFileID, AWorksheetID: string; ACells: TGCells): TGCells;

    function GetListRow(AFileID, AWorksheetID, AQuery: string): TMeasurements;
    function AddListRow(AFileID, AWorksheetID: string; AMes: TMeasurement): TMeasurement;

    function GetCellValue(ACells: TGCells; ARow, ACol: integer): string;
    procedure SetCellValue(var ACells: TGCells; ARow, ACol: integer; AInputValue: string);
  end;

  TOAuth2AuthenticatorHelper = class helper for TOAuth2Authenticator
    procedure GetNewToken;
  end;

  TJSONObjectHelper = class helper for TJSONObject
    function TryGetValue(const APath: string): string;
  end;

implementation

function TJSONObjectHelper.TryGetValue(const APath: string): string;
var
  LJSONValue: TJSONValue;
begin
  Result := '';
  LJSONValue := FindValue(APath);
  if LJSONValue <> nil then
  begin
    try
      Result := LJSONValue.ToString;
    except
    end;
  end;
end;

{ TOAuth2AuthenticatorHelper }

procedure TOAuth2AuthenticatorHelper.GetNewToken;
var
  LClient: TRestClient;
  LRequest: TRESTRequest;
  LToken: string;
  LIntValue: int64;
begin
  try
    LClient := TRestClient.Create(Self.AccessTokenEndpoint);

    LRequest := TRESTRequest.Create(nil);
    LRequest.Method := TRESTRequestMethod.rmPOST;
    LRequest.Client := LClient;

    LRequest.AddAuthParameter('refresh_token', Self.RefreshToken, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_id', Self.ClientID, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('client_secret', Self.ClientSecret, TRESTRequestParameterKind.pkGETorPOST);
    LRequest.AddAuthParameter('grant_type', 'refresh_token', TRESTRequestParameterKind.pkGETorPOST);

    LRequest.Execute;

    if LRequest.Response.GetSimpleValue('access_token', LToken) then
      Self.AccessToken := LToken;
    if LRequest.Response.GetSimpleValue('refresh_token', LToken) then
      Self.RefreshToken := LToken;

    /// detect token-type. this is important for how using it later
    if LRequest.Response.GetSimpleValue('token_type', LToken) then
      Self.TokenType := OAuth2TokenTypeFromString(LToken);

    /// if provided by the service, the field "expires_in" contains
    /// the number of seconds an access-token will be valid
    if LRequest.Response.GetSimpleValue('expires_in', LToken) then
    begin
      LIntValue := StrToIntdef(LToken, -1);
      if (LIntValue > -1) then
        Self.AccessTokenExpiry := IncSecond(Now, LIntValue)
      else
        Self.AccessTokenExpiry := 0.0;
    end;

  finally
    FreeAndNil(LClient);
    FreeAndNil(LRequest);
  end;

end;


{ TGoogleAPI }


procedure TGoogleAPI.TitleChanged(const ATitle: string;
  var DoCloseWebView: boolean);
begin
  if (StartsText('Success code', ATitle)) then
  begin
    OAuth2Authenticator.AuthCode:= Copy(ATitle, 14, Length(ATitle));
    if (OAuth2Authenticator.AuthCode <> '') then
      DoCloseWebView := TRUE;
  end;
end;

function TGoogleAPI.TryAuthenticate: boolean;
begin
  Result := true;
  if not Authenticated then
  try
    if OAuth2Authenticator.RefreshToken <> '' then
      OAuth2Authenticator.GetNewToken;

    if not Authenticated then
      Authenticate();
  except
    Result := false;
  end;
end;

constructor TGoogleAPI.Create(AOwner: TComponent; AClientID, AClientSecret: string);
begin
  FOwner := AOwner;

  OAuth2Authenticator := TOAuth2Authenticator.Create(AOwner);
  OAuth2Authenticator.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth';
  OAuth2Authenticator.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
  OAuth2Authenticator.RedirectionEndpoint := 'urn:ietf:wg:oauth:2.0:oob';
  OAuth2Authenticator.Scope := 'https://www.googleapis.com/auth/drive https://spreadsheets.google.com/feeds/';
  OAuth2Authenticator.ClientID := AClientID;
  OAuth2Authenticator.ClientSecret := AClientSecret;

  RESTClient := TRESTClient.Create(AOwner);
  RESTClient.Authenticator := OAuth2Authenticator;
  RESTClient.BaseURL := 'https://www.googleapis.com/drive/v2';

  RESTResponse := TRESTResponse.Create(AOwner);

  RESTRequest := TRESTRequest.Create(AOwner);
  RESTRequest.Client := RESTClient;
  RESTRequest.Response := RESTResponse;

  SRESTClient := TRESTClient.Create(AOwner);
  SRESTClient.Authenticator := OAuth2Authenticator;
  SRESTClient.BaseURL := 'https://spreadsheets.google.com/feeds';

  SRESTResponse := TRESTResponse.Create(AOwner);

  SRESTRequest := TRESTRequest.Create(AOwner);
  SRESTRequest.Client := SRESTClient;
  SRESTRequest.Response := SRESTResponse;

  Authenticate;
end;

function TGoogleAPI.CreateDirectory(AParent, ADirName: string): string;
var
  JSONObject: TJSONObject;
begin
  Result := '';
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  RESTRequest.Method:=rmPOST;
  RESTRequest.Resource:='/files';
  JSONObject := TJSONObject.Create;
  JSONObject.AddPair('title', ADirName);
  JSONObject.AddPair('parents', TJSONArray.Create(TJSONObject.Create(TJSONPair.Create('id', AParent))));
  JSONObject.AddPair('mimeType', 'application/vnd.google-apps.folder');
  RESTRequest.AddBody(JSONObject);

  RESTRequest.Execute;

  if Assigned(RESTRequest.Response.JSONValue) then
    begin
      JSONObject := RESTRequest.Response.JSONValue as TJSONObject;
      Result := JSONObject.TryGetValue('id');
    end;
end;

function TGoogleAPI.CreateFile(ADir, AFileName: string): string;
var
  JSONObject: TJSONObject;
begin
  Result := '';
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  RESTRequest.Method:=rmPOST;
  RESTRequest.Resource:='/files';
  JSONObject := TJSONObject.Create;
  JSONObject.AddPair('title', AFileName);
  JSONObject.AddPair('parents', TJSONArray.Create(TJSONObject.Create(TJSONPair.Create('id', ADir))));
  JSONObject.AddPair('mimeType', 'application/vnd.google-apps.spreadsheet');
  RESTRequest.AddBody(JSONObject);

  RESTRequest.Execute;

  if Assigned(RESTRequest.Response.JSONValue) then
    begin
      JSONObject := RESTRequest.Response.JSONValue as TJSONObject;
      Result := JSONObject.TryGetValue('id');
    end;
end;

function TGoogleAPI.CreateWorksheet(AFileID, AWorksheetName: string; ARowCount,
  AColCount: integer): TWorksheet;
var
  JSONObject: TJSONObject;
  entry: TJSONObject;
begin
  Result.Clear;
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  SRESTRequest.Method:=rmPOST;
  SRESTRequest.Resource:='/worksheets/' + AFileID + '/private/full?alt=json';
//  SRESTRequest.Params.AddItem('alt', 'json', pkGETorPOST); -- it cant do that in POST!

// have tried to send JSON, but it cant receive it(
//  SRESTRequest.AddBody('{"entry": {"title": {	"type": "text",	"$t": "Expenses"}, "gs$colCount": {"$t": "10"},"gs$rowCount": {	"$t": "50"} }}',
// TRESTContentType.ctAPPLICATION_JSON);

  SRESTRequest.AddBody('<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gs="http://schemas.google.com/spreadsheets/2006">' +
    ' <title>' + AWorksheetName + '</title>' +
    ' <gs:rowCount>' + IntToStr(ARowCount) + '</gs:rowCount>' +
    ' <gs:colCount>' + IntToStr(AColCount) + '</gs:colCount>' +
    '</entry>',
    TRESTContentType.ctAPPLICATION_ATOM_XML);
  SRESTRequest.Execute;
  if Assigned(SRESTRequest.Response.JSONValue) then
  begin
    JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

    entry := JSONObject.GetValue('entry') as TJSONObject;
    if not Assigned(entry) then exit;
    Result := ExtractWorksheetMetadata(entry);
  end;
end;

function TGoogleAPI.EditWorksheetParams(AFileID, AWorksheetID, AWorksheetVersion,
  AWorksheetName: string; ARowCount, AColCount: integer): TWorksheet;
var
  JSONObject: TJSONObject;
  entry: TJSONObject;
begin
  Result.Clear;
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  SRESTRequest.Method:=rmPUT;
  SRESTRequest.Resource:='/worksheets/' + AFileID + '/private/full/' + AWorksheetID + '/' + AWorksheetVersion + '?alt=json';

  SRESTRequest.AddBody('<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gs="http://schemas.google.com/spreadsheets/2006"> '+
    ' <title>' + AWorksheetName + '</title>  '+
    ' <gs:rowCount>' + IntToStr(ARowCount) + '</gs:rowCount> '+
    ' <gs:colCount>' + IntToStr(AColCount) + '</gs:colCount> '+
    '</entry>', TRESTContentType.ctAPPLICATION_ATOM_XML);
  SRESTRequest.Execute;
  if Assigned(SRESTRequest.Response.JSONValue) then
  begin
    JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

    entry := JSONObject.GetValue('entry') as TJSONObject;
    if not Assigned(entry) then exit;
    Result := ExtractWorksheetMetadata(entry);
  end;
end;

function TGoogleAPI.GetAuthenticated: boolean;
begin
  Result := (OAuth2Authenticator.AccessToken <> '') and (OAuth2Authenticator.AccessTokenExpiry > (now + 10 / SecsPerDay));
end;

function TGoogleAPI.GetCells(AFileID, AWorksheetID: string; AMinRow, AMaxRow, AMinCol,
  AMaxCol: integer): TGCells;
var
  JSONObject,
  feed,
  item: TJSONObject;
  entry: TJSONArray;
  i: integer;
begin
  SetLength(Result, 0);
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  SRESTRequest.Method:=rmGET;
  SRESTRequest.Resource:='/cells/' + AFileID + '/' + AWorksheetID + '/private/full';
  SRESTRequest.Params.AddItem('min-row', IntToStr(AMinRow), pkGETorPOST);
  SRESTRequest.Params.AddItem('max-row', IntToStr(AMaxRow), pkGETorPOST);
  SRESTRequest.Params.AddItem('min-col', IntToStr(AMinCol), pkGETorPOST);
  SRESTRequest.Params.AddItem('max-col', IntToStr(AMaxCol), pkGETorPOST);
  SRESTRequest.Params.AddItem('alt', 'json', pkGETorPOST);
  SRESTRequest.Execute;
  if Assigned(SRESTRequest.Response.JSONValue) then
  begin
    JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

    feed := JSONObject.GetValue('feed') as TJSONObject;
    if not Assigned(feed) then exit;    
    entry := feed.GetValue('entry') as TJSONArray;
    if not Assigned(entry) then exit;    
    
    for i := 0 to entry.Count - 1 do
    begin
      item := entry.Items[i] as TJSONObject;
      if not Assigned(item) then continue;

      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := ExtractCellMetadata(item);
    end;
  end;
end;

function TGoogleAPI.GetCellValue(ACells: TGCells; ARow, ACol: integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(ACells) - 1 do
    if (ACells[i].Row = ARow) and (ACells[i].Col = ACol) then
    begin
      Result := ACells[i].Value;
      break;
    end;
end;

function TGoogleAPI.ExtractCellMetadata(cell: TJSONObject): TGCell;
var
  s: string;
  linklist: TJSONArray;
  j: Integer;
  gscell,
  link: TJSONObject;
begin
  Result.Clear;

  Result.Title := GetSpValue(cell.Get('title').JsonValue);
  s := ReverseString(GetSpValue(cell.Get('id').JsonValue));
  Result.Id := ReverseString(Copy(s, 1, pos('/', s) - 1));

  gscell := cell.Get('gs$cell').JsonValue as TJSONObject;
  if Assigned(gscell) then
  begin
    Result.InputValue := ExtractFromQuotes(gscell.Get('inputValue').JsonValue.ToString);
    Result.Value := ExtractFromQuotes(gscell.Get('$t').JsonValue.ToString);
    Result.Col := StrToIntDef(ExtractFromQuotes(gscell.Get('col').JsonValue.ToString), 0);
    Result.Row := StrToIntDef(ExtractFromQuotes(gscell.Get('row').JsonValue.ToString), 0);
  end;

  s := '';
  linklist := cell.GetValue('link') as TJSONArray;
  if Assigned(linklist) then
    for j := 0 to linklist.Count - 1 do
    begin
      link := linklist.Items[j] as TJSONObject;
      if not Assigned(link) then continue;

      if (link.GetValue('rel').ToString = 'edit') or (link.GetValue('rel').ToString = '"edit"') then
      begin
        s := link.GetValue('href').ToString;
        break;
      end;
    end;

  s := ExtractFromQuotes(ReverseString(s));
  Result.EditTag := ReverseString(Copy(s, 1, pos('/', s) - 1));
end;

function TGoogleAPI.ExtractFromQuotes(s: string): string;
begin
  if (length(s) > 1) and (s[1] = '"') then s := Copy(s, 2, length(s));
  if (length(s) > 1) and (s[length(s)] = '"') then s := Copy(s, 1, length(s) - 1);
  Result := s;
end;

function TGoogleAPI.ExtractMeasurement(cell: TJSONObject): TMeasurement;
var
  s: string;
  linklist: TJSONArray;
  j: Integer;
  link: TJSONObject;
begin
  Result.Clear;

  s := ReverseString(GetSpValue(cell.Get('id').JsonValue));
  Result.Id := ReverseString(Copy(s, 1, pos('/', s) - 1));
  Result.Date := StrToDateTimeDef(GetSpValue(cell.Get('gsx$date').JsonValue), 0);
  Result.InternalDate := StrToIntDef(GetSpValue(cell.Get('gsx$internaldate').JsonValue), 0);
  s := ReplaceStr(GetSpValue(cell.Get('gsx$temperature').JsonValue), ',', FormatSettings.DecimalSeparator);
  Result.Temperature := StrToFloatDef(s, 0);
  s := ReplaceStr(GetSpValue(cell.Get('gsx$humidity').JsonValue), ',', FormatSettings.DecimalSeparator);
  Result.Humidity := StrToFloatDef(s, 0);
  Result.CO2Level := StrToIntDef(GetSpValue(cell.Get('gsx$co2level').JsonValue), 0);

  s := '';
  linklist := cell.GetValue('link') as TJSONArray;
  if Assigned(linklist) then
    for j := 0 to linklist.Count - 1 do
    begin
      link := linklist.Items[j] as TJSONObject;
      if not Assigned(link) then continue;

      if (link.GetValue('rel').ToString = 'edit') or (link.GetValue('rel').ToString = '"edit"') then
      begin
        s := link.GetValue('href').ToString;
        break;
      end;
    end;

  s := ExtractFromQuotes(ReverseString(s));
  Result.EditTag := ReverseString(Copy(s, 1, pos('/', s) - 1));
end;

function TGoogleAPI.ExtractWorksheetMetadata(spreadsheet: TJSONObject): TWorksheet;
var
  s: string;
  linklist: TJSONArray;
  j: Integer;
  link: TJSONObject;
begin
  Result.Clear;

  Result.Title := GetSpValue(spreadsheet.Get('title').JsonValue);
  s := ReverseString(GetSpValue(spreadsheet.Get('id').JsonValue));
  Result.Id := ReverseString(Copy(s, 1, pos('/', s) - 1));
  Result.ColCount := StrToIntDef(GetSpValue(spreadsheet.Get('gs$colCount').JsonValue), 0);
  Result.RowCount := StrToIntDef(GetSpValue(spreadsheet.Get('gs$rowCount').JsonValue), 0);

  s := '';
  linklist := spreadsheet.GetValue('link') as TJSONArray;
  if Assigned(linklist) then
    for j := 0 to linklist.Count - 1 do
    begin
      link := linklist.Items[j] as TJSONObject;
      if not Assigned(link) then continue;

      if (link.GetValue('rel').ToString = 'edit') or (link.GetValue('rel').ToString = '"edit"') then
      begin
        s := link.GetValue('href').ToString;
        break;
      end;
    end;

  s := ExtractFromQuotes(ReverseString(s));
  Result.EditTag := ReverseString(Copy(s, 1, pos('/', s) - 1));
end;

function TGoogleAPI.GetDirectoryID(AParent, ADirName: string): string;
var
  JSONObject: TJSONObject;
  kind: string;
  FileObject: TJSONObject;
  Pair: TJSONPair;
  NextToken: string;
  ListItems: TJSONArray;
  i: Integer;
begin
  Result := '';
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  RESTRequest.Method:=rmGET;
  RESTRequest.Resource:='/files';
  RESTRequest.Params.AddItem('q', 'mimeType="application/vnd.google-apps.folder" and "' + AParent +
    '" in parents and title="' + ADirName +
    '" and trashed = false', TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Execute;

  if Assigned(RESTRequest.Response.JSONValue) then
    begin
      JSONObject := RESTRequest.Response.JSONValue as TJSONObject;

      RESTRequest.Response.GetSimpleValue('kind', kind);
      if kind = 'drive#fileList' then
      begin
        Pair := JSONObject.Get('nextPageToken');
        if Assigned(Pair) then NextToken := Pair.JsonValue.Value;

        ListItems := JSONObject.Get('items').JsonValue as TJSONArray;
        for i := 0 to ListItems.Count - 1 do
        begin
          FileObject := ListItems.Items[i] as TJSONObject;
          Result := FileObject.TryGetValue('id');
          if Result <> '' then break;
        end;
      end;
    end;

end;

function TGoogleAPI.GetFileID(ADir, AFileName: string): string;
var
  JSONObject: TJSONObject;
  kind: string;
  FileObject: TJSONObject;
  ListItems: TJSONArray;
  i: Integer;
begin
  Result := '';
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  RESTRequest.Method:=rmGET;
  RESTRequest.Resource:='/files';
  RESTRequest.Params.AddItem('q', 'mimeType="application/vnd.google-apps.spreadsheet" and "' + ADir +
    '" in parents and title="' + AFileName +
    '" and trashed = false', TRESTRequestParameterKind.pkGETorPOST);
  RESTRequest.Execute;

  if Assigned(RESTRequest.Response.JSONValue) then
    begin
      JSONObject := RESTRequest.Response.JSONValue as TJSONObject;

      RESTRequest.Response.GetSimpleValue('kind', kind);
      if kind = 'drive#fileList' then
      begin
        ListItems := JSONObject.Get('items').JsonValue as TJSONArray;
        for i := 0 to ListItems.Count - 1 do
        begin
          FileObject := ListItems.Items[i] as TJSONObject;
          Result := FileObject.TryGetValue('id');
          if Result <> '' then break;
        end;
      end;
    end;

end;

function TGoogleAPI.GetListRow(AFileID, AWorksheetID,
  AQuery: string): TMeasurements;
var
  JSONObject: TJSONObject;
  feed,
  item: TJSONObject;
  entry: TJSONArray;
  i: integer;
begin
  SetLength(Result, 0);
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  SRESTRequest.Method:=rmGET;
  SRESTRequest.Resource:='/list/' + AFileID + '/' + AWorksheetID + '/private/full';
  if AQuery <> '' then SRESTRequest.Params.AddItem('sq', AQuery, pkGETorPOST);
  SRESTRequest.Params.AddItem('alt', 'json', pkGETorPOST);
  SRESTRequest.Execute;
  if Assigned(SRESTRequest.Response.JSONValue) then
  begin
    JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

    feed := JSONObject.GetValue('feed') as TJSONObject;
    if not Assigned(feed) then exit;    
    entry := feed.GetValue('entry') as TJSONArray;
    if not Assigned(entry) then exit;    
    
    for i := 0 to entry.Count - 1 do
    begin
      item := entry.Items[i] as TJSONObject;
      if not Assigned(item) then continue;

      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := ExtractMeasurement(item);
    end;
  end;
end;

function TGoogleAPI.GetSpValue(val: TJSONValue): string;
var
  obj: TJSONObject;
begin
  Result := '';

  obj := val as TJSONObject;
  if not Assigned(obj) then exit;

  Result := ExtractFromQuotes(obj.GetValue('$t').ToString);
end;

function TGoogleAPI.GetWorksheetList(AFileID: string): TWorksheets;
var
  JSONObject,
  spreadsheet: TJSONObject;
  entry: TJSONArray;
  i: integer;
begin
  SetLength(Result, 0);
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  SRESTRequest.Method:=rmGET;
  SRESTRequest.Resource:='/worksheets/' + AFileID + '/private/full';
  SRESTRequest.Params.AddItem('alt', 'json', pkGETorPOST);
  SRESTRequest.Execute;
  if Assigned(SRESTRequest.Response.JSONValue) then
  begin
    JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

    entry := (JSONObject.GetValue('feed') as TJSONObject).GetValue('entry') as TJSONArray;
    for i := 0 to entry.Count - 1 do
    begin
      spreadsheet := entry.Items[i] as TJSONObject;
      if not Assigned(spreadsheet) then continue;

      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1] := ExtractWorksheetMetadata(spreadsheet);
    end;
  end;
end;

function TGoogleAPI.AddListRow(AFileID, AWorksheetID: string;
  AMes: TMeasurement): TMeasurement;
var
  JSONObject: TJSONObject;
  entry: TJSONObject;
begin
  Result.Clear;
  ClearRESTConnector;
  if not TryAuthenticate then exit;

  SRESTRequest.Method:=rmPOST;
  SRESTRequest.Resource:='/list/' + AFileID + '/' + AWorksheetID + '/private/full?alt=json';

  SRESTRequest.AddBody('<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gsx="http://schemas.google.com/spreadsheets/2006/extended">' +
    ' <gsx:date>' + FormatDateTime('DD.MM.YYYY HH:NN:SS', AMes.Date) + '</gsx:date>' +
    ' <gsx:internaldate>' + IntToStr(AMes.InternalDate) + '</gsx:internaldate>' +
    ' <gsx:temperature>' + FloatToStrF(AMes.Temperature, ffFixed, 20, 2) + '</gsx:temperature>' +
    ' <gsx:humidity>' + FloatToStrF(AMes.Humidity, ffFixed, 20, 2) + '</gsx:humidity>' +
    ' <gsx:co2level>' + IntToStr(AMes.CO2Level) + '</gsx:co2level>' +
    '</entry>',
    TRESTContentType.ctAPPLICATION_ATOM_XML);
  SRESTRequest.Execute;
  if Assigned(SRESTRequest.Response.JSONValue) then
  begin
    JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

    entry := JSONObject.GetValue('entry') as TJSONObject;
    if not Assigned(entry) then exit;
    Result := ExtractMeasurement(entry);
  end;
end;

procedure TGoogleAPI.Authenticate;
var
  wf: Tfrm_OAuthWebForm;
begin
  OAuth2Authenticator.Authenticate(RESTRequest);
  if OAuth2Authenticator.AccessToken = '' then
  begin
    wf := Tfrm_OAuthWebForm.Create(FOwner);
    try
      wf.OnTitleChanged := TitleChanged;
      wf.ShowModalWithURL(OAuth2Authenticator.AuthorizationRequestURI);
    finally
      wf.Release;
    end;
    OAuth2Authenticator.ChangeAuthCodeToAccesToken;
  end;
end;

procedure TGoogleAPI.ClearRESTConnector;
begin
  RESTRequest.ResetToDefaults;
  RESTRequest.Params.Clear;
  RESTRequest.ClearBody;

  SRESTRequest.ResetToDefaults;
  SRESTRequest.Params.Clear;
  SRESTRequest.ClearBody;
end;

function TGoogleAPI.isDirectoryExist(AParent, ADirName: string): boolean;
begin
  Result := GetDirectoryID(AParent, ADirName) <> '';
end;

function TGoogleAPI.SetCell(AFileID, AWorksheetID: string;
  ACell: TGCell): TGCell;
var
  JSONObject: TJSONObject;
  entry: TJSONObject;
  TryCount: integer;
begin
  TryCount := 0;
  if not TryAuthenticate then exit;

  repeat
    TryCount := TryCount + 1;
    Result.Clear;
    ClearRESTConnector;

    SRESTRequest.Method:=rmPUT;
    SRESTRequest.Resource:='/cells/' + AFileID + '/' + AWorksheetID + '/private/full/' + ACell.Id + '/' + ACell.GetUrlEditTag + '?alt=json';

    SRESTRequest.AddBody('<entry xmlns="http://www.w3.org/2005/Atom" xmlns:gs="http://schemas.google.com/spreadsheets/2006">' +
      ' <gs:cell row="' + IntToStr(ACell.Row) + '" col="' + IntToStr(ACell.Col) + '" inputValue="' + ACell.InputValue + '"' + ' />' +
      '</entry>',
      TRESTContentType.ctAPPLICATION_ATOM_XML);
    SRESTRequest.Execute;
    if Assigned(SRESTRequest.Response.JSONValue) then
    begin
      JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

      entry := JSONObject.GetValue('entry') as TJSONObject;
      if not Assigned(entry) then exit;
      Result := ExtractCellMetadata(entry);
    end;

    if SRESTRequest.Response.StatusCode = 409 then ACell.EditTag := Result.EditTag;
  until (TryCount > 1) or (SRESTRequest.Response.StatusCode <> 409);
end;

function TGoogleAPI.SetCells(AFileID, AWorksheetID: string;
  ACells: TGCells): TGCells;
var
  i: Integer;
begin
  SetLength(Result, length(ACells));
  for i := 0 to length(ACells) - 1 do
    Result [i] := SetCell(AFileID, AWorksheetID, ACells[i])
end;

procedure TGoogleAPI.SetCellValue(var ACells: TGCells; ARow, ACol: integer;
  AInputValue: string);
var
  i: Integer;
begin
  for i := 0 to length(ACells) - 1 do
    if (ACells[i].Row = ARow) and (ACells[i].Col = ACol) then
    begin
      ACells[i].InputValue := AInputValue;
      exit;
    end;

  SetLength(ACells, length(ACells) + 1);
  ACells[length(ACells) - 1].Id := 'R' + IntToStr(ARow) + 'C' + IntToStr(ACol);
  ACells[length(ACells) - 1].InputValue := AInputValue;
  ACells[length(ACells) - 1].Row := ARow;
  ACells[length(ACells) - 1].Col := ACol;
end;

{ TWorksheet }

procedure TWorksheet.Clear;
begin
  Title := '';
  Id := '';
  EditTag := '';
  ColCount := 0;
  RowCount := 0;
end;

{ TGCell }

procedure TGCell.Clear;
begin
  Id := '';
  Title := '';
  InputValue := '';
  Value := '';
  EditTag := '';
  Col := 0;
  Row := 0;
end;

function TGCell.GetUrlEditTag: string;
begin
  Result := EditTag;
  if Result = '' then Result := '0';
end;

end.
