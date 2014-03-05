unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  CO2Meter;

type
  TfMain = class(TForm)
    edCOM: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    btCheckMem: TButton;
    btGetMem: TButton;
    btClose: TButton;
    lbInfo: TLabel;
    edFileName: TEdit;
    btFile: TButton;
    dgSave: TSaveDialog;
    procedure btFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCheckMemClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btGetMemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.btCheckMemClick(Sender: TObject);
var
  id,
  version: string;
  SamplesCount,
  SamplesRate: cardinal;
  SamplesStartDate: TDateTime;
begin
  with TCO2Meter.Create do
  try
    OpenPort(StrToIntDef(edCOM.Text, 1));

    GetInfo(id, version);
    GetMemoryStat(SamplesCount, SamplesRate, SamplesStartDate);

    lbInfo.Caption :=
      'ID=' + id +
      ' version=' + version + #$0d#$0a +
      'samples=' + IntToStr(SamplesCount) +
      ' rate=' + IntToStr(SamplesRate) + #$0d#$0a +
      'start date=' + DateTimeToStr(SamplesStartDate) +
      ' end date=' + DateTimeToStr(SamplesStartDate + SamplesRate * (SamplesCount / 3 - 1) / SecsPerDay)
    ;
    ClosePort;
  finally
    Free;
  end;
end;

procedure TfMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfMain.btFileClick(Sender: TObject);
begin
  if dgSave.Execute then edFileName.Text := dgSave.FileName;
end;

procedure TfMain.btGetMemClick(Sender: TObject);
var
  fname,
  id,
  version: string;
  SamplesCount,
  SamplesRate: cardinal;
  SamplesStartDate: TDateTime;
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  with TCO2Meter.Create do
  try
    OpenPort(StrToIntDef(edCOM.Text, 1));

    GetInfo(id, version);
    GetSamples(SamplesCount, SamplesRate, SamplesStartDate, sl);

    lbInfo.Caption :=
      'ID=' + id +
      ' version=' + version + #$0d#$0a +
      'samples=' + IntToStr(SamplesCount) +
      ' rate=' + IntToStr(SamplesRate) + #$0d#$0a +
      'start date=' + DateTimeToStr(SamplesStartDate) +
      ' end date=' + DateTimeToStr(SamplesStartDate + SamplesRate * (SamplesCount / 3 - 1) / SecsPerDay)
    ;

    for i := 0 to sl.Count - 1 do
    begin
      sl[i] := StringReplace(sl[i], ' ', ',', [rfReplaceAll]);
      sl[i] := Copy(sl[i], Pos(',', sl[i]) + 1, length(sl[i]));
      sl[i] := '"' + DateTimeToStr(SamplesStartDate + i * integer(SamplesRate) / SecsPerDay) + '",' + sl[i];
    end;

    sl.Insert(0, 'Date, Temperature, CO2level, Humidity');

    fname := edFileName.Text;
    if ExtractFilePath(fname) = '' then
      fname := ExtractFilePath(Application.ExeName) + fname;
    sl.SaveToFile(fname);

    ClosePort;
  finally
    Free;
    sl.Free;
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  dgSave.InitialDir := ExtractFilePath(Application.ExeName);

  // command line parameters
  if ParamCount > 0 then edCOM.Text := ParamStr(1);
  if ParamCount > 1 then edFileName.Text := ParamStr(2);
  if (ParamCount > 2) and ((ParamStr(3) = 'e') or (ParamStr(3) = 'E')) then
  begin
    btGetMem.Click;
    Application.Terminate;
  end;
end;

end.
