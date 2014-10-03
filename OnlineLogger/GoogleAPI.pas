unit GoogleAPI;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XMLIntf, XMLDoc, System.AnsiStrings,
  REST.Types, System.JSON, IPPeerClient,
  REST.Authenticator.OAuth, REST.Authenticator.OAuth.WebForm.Win,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope;

type
  TWorksheet = packed record
    Title,
    Id,
    EditTag: string;
    ColCount,
    RowCount: integer;
  end;
  TWorksheets = array of TWorksheet;

  TGoogleAPI = class
  private
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
  public
    constructor Create(Owner: TComponent; AClientID, AClientSecret: string);

    procedure Authenticate(Owner: TComponent);
    property Authenticated: boolean read GetAuthenticated;

    function isDirectoryExist(AParent, ADirName: string): boolean;
    function CreateDirectory(AParent, ADirName: string): string;
    function GetDirectoryID(AParent, ADirName: string): string;

    function CreateFile(ADir, AFileName: string): string;
    function GetFileID(ADir, AFileName: string): string;

    function GetWorksheetList(AFileID: string): TWorksheets;
    function CreateWorksheet(AFileID, AWorksheetName: string; ARowCount, AColCount: integer): string;
    function EditWorksheetParams(AFileID, AWorksheetID, AWorksheetName: string; ARowCount, AColCount: integer): string;
    function GetCells(AFileID: string; MinRow, MaxRow, MinCol, MaxCol: integer): string;
  end;

implementation

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

constructor TGoogleAPI.Create(Owner: TComponent; AClientID, AClientSecret: string);
begin
  OAuth2Authenticator := TOAuth2Authenticator.Create(Owner);
  OAuth2Authenticator.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth';
  OAuth2Authenticator.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
  OAuth2Authenticator.RedirectionEndpoint := 'urn:ietf:wg:oauth:2.0:oob';
  OAuth2Authenticator.Scope := 'https://www.googleapis.com/auth/drive https://spreadsheets.google.com/feeds/';
  OAuth2Authenticator.ClientID := AClientID;
  OAuth2Authenticator.ClientSecret := AClientSecret;

  RESTClient := TRESTClient.Create(Owner);
  RESTClient.Authenticator := OAuth2Authenticator;
  RESTClient.BaseURL := 'https://www.googleapis.com/drive/v2';

  RESTResponse := TRESTResponse.Create(Owner);

  RESTRequest := TRESTRequest.Create(Owner);
  RESTRequest.Client := RESTClient;
  RESTRequest.Response := RESTResponse;

  SRESTClient := TRESTClient.Create(Owner);
  SRESTClient.Authenticator := OAuth2Authenticator;
  SRESTClient.BaseURL := 'https://spreadsheets.google.com/feeds';

  SRESTResponse := TRESTResponse.Create(Owner);

  SRESTRequest := TRESTRequest.Create(Owner);
  SRESTRequest.Client := SRESTClient;
  SRESTRequest.Response := SRESTResponse;

  Authenticate(Owner);
end;

function TGoogleAPI.CreateDirectory(AParent, ADirName: string): string;
var
  JSONObject: TJSONObject;
begin
  Result := '';
  ClearRESTConnector;

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
      Result := JSONObject.Get('id').JsonValue.Value;
    end;
end;

function TGoogleAPI.CreateFile(ADir, AFileName: string): string;
var
  JSONObject: TJSONObject;
begin
  Result := '';
  ClearRESTConnector;

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
      Result := JSONObject.Get('id').JsonValue.Value;
    end;
end;

function TGoogleAPI.CreateWorksheet(AFileID, AWorksheetName: string; ARowCount,
  AColCount: integer): string;
begin

end;

function TGoogleAPI.EditWorksheetParams(AFileID, AWorksheetID,
  AWorksheetName: string; ARowCount, AColCount: integer): string;
begin

end;

function TGoogleAPI.GetAuthenticated: boolean;
begin
  Result := OAuth2Authenticator.AccessToken <> '';
end;

function TGoogleAPI.GetCells(AFileID: string; MinRow, MaxRow, MinCol,
  MaxCol: integer): string;
begin

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
          Result := FileObject.Get('id').JsonValue.Value;
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
          Result := FileObject.Get('id').JsonValue.Value;
          if Result <> '' then break;
        end;
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

  Result := obj.GetValue('$t').ToString;
  if (length(Result) > 1) and (Result[1] = '"') then Result := Copy(Result, 2, length(Result));
  if (length(Result) > 1) and (Result[length(Result)] = '"') then Result := Copy(Result, 1, length(Result) - 1);
end;

function TGoogleAPI.GetWorksheetList(AFileID: string): TWorksheets;
var
  JSONObject,
  spreadsheet,
  link: TJSONObject;
  linklist,
  entry: TJSONArray;
  sl: TStringList;
  i,
  j: integer;
  s: string;
begin
  SetLength(Result, 0);
  ClearRESTConnector;

  SRESTRequest.Method:=rmGET;
  SRESTRequest.Resource:='worksheets/' + AFileID + '/private/full';
  SRESTRequest.Params.AddItem('alt', 'json', pkGETorPOST);
  SRESTRequest.Execute;
  if Assigned(SRESTRequest.Response.JSONValue) then
  begin
    JSONObject := SRESTRequest.Response.JSONValue as TJSONObject;

    entry := (JSONObject.GetValue('feed') as TJSONObject).GetValue('entry') as TJSONArray;

    sl := TStringList.Create;
    sl.Text := entry.ToString;
    sl.SaveToFile('d:\2.txt');

    for i := 0 to entry.Count - 1 do
    begin
      spreadsheet := entry.Items[i] as TJSONObject;
      if not Assigned(spreadsheet) then continue;

      SetLength(Result, length(Result) + 1);
      Result[length(Result)-1].Title := GetSpValue(spreadsheet.Get('title').JsonValue);
      s := ReverseString(GetSpValue(spreadsheet.Get('id').JsonValue));
      Result[length(Result)-1].Id := ReverseString(Copy(s, 1, pos('/', s) - 1));
      Result[length(Result)-1].ColCount := StrToIntDef(GetSpValue(spreadsheet.Get('gs$colCount').JsonValue), 0);
      Result[length(Result)-1].ColCount := StrToIntDef(GetSpValue(spreadsheet.Get('gs$rowCount').JsonValue), 0);

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
      s := ReverseString(s);
      Result[length(Result)-1].EditTag := ReverseString(Copy(s, 1, pos('/', s) - 1));
    end;
  end;
end;

procedure TGoogleAPI.Authenticate(Owner: TComponent);
var
  wf: Tfrm_OAuthWebForm;
begin
  OAuth2Authenticator.Authenticate(RESTRequest);
  if OAuth2Authenticator.AccessToken = '' then
  begin
    wf := Tfrm_OAuthWebForm.Create(Owner);
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
end;

function TGoogleAPI.isDirectoryExist(AParent, ADirName: string): boolean;
begin
  Result := GetDirectoryID(AParent, ADirName) <> '';
end;

end.
