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

procedure TfMain.FormCreate(Sender: TObject);
begin
  dgSave.InitialDir := ExtractFilePath(Application.ExeName);
end;

end.
