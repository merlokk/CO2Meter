unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.AnsiStrings, DateUtils, IniFiles, UITypes,
  REST.Authenticator.OAuth.WebForm.Win, Vcl.StdCtrls, Vcl.Buttons,
  MainExecutor, GoogleSender, def, Vcl.ExtCtrls;

type
  TMainFrm = class(TForm)
    BitBtn2: TBitBtn;
    Button1: TButton;
    Label1: TLabel;
    edComPort: TEdit;
    Label2: TLabel;
    edClientID: TEdit;
    Label3: TLabel;
    edClientSecret: TEdit;
    btSave: TButton;
    btReloadServer: TButton;
    Timer1: TTimer;
    lbStatus: TLabel;
    cbExecute: TCheckBox;
    procedure BitBtn2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btReloadServerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    ex: TMainExecutor;
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

uses CO2Meter;

procedure TMainFrm.BitBtn2Click(Sender: TObject);
var
 mes: TMeasurements;
begin
  SetLength(mes, 1);
  mes[0].Date := EncodeDate(2013, 1, 2);
  mes[0].InternalDate := SecondsBetween(mes[0].Date, EncodeDate(2000, 1, 1));
  mes[0].Temperature := 10;
  mes[0].Humidity := 20;
  mes[0].CO2Level := 2000;
  ex.GoogleSender.SendData(mes);
end;

procedure TMainFrm.btReloadServerClick(Sender: TObject);
begin
  if (ex <> nil) and (ex.CO2Meter.GetDataCount > 0) then
    if MessageDlg('There are measurements in queue that will be lost! Do you want to restart?', mtConfirmation, mbYesNo, 0) <> mrYes
    then exit;

  if ex <> nil then
  try
    ex.Free;
  except
    sleep(500);
  end;
  ex := nil;

  try
    ex := TMainExecutor.Create(Self);
    ex.Init(StrToIntDef(edComPort.Text, 0),
      edClientID.Text,
      edClientSecret.Text,
      ExtractFilePath(Application.ExeName) + '\storage.txt');
  except
     on E : Exception do
     begin
       MessageDlg('Can''t init engine. Error:' + E.Message, mtError, [mbOk], 0);
     end;
  end;
end;

procedure TMainFrm.btSaveClick(Sender: TObject);
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + 'logger.ini') do
  try
    WriteString('Port', 'COM', edComPort.Text);
    WriteString('GoogleAPI', 'ClientID', edClientID.Text);
    WriteString('GoogleAPI', 'ClientSecret', edClientSecret.Text);
  finally
    Free;
  end;
end;

procedure TMainFrm.Button1Click(Sender: TObject);
begin
  ex.WorkCycle;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  ex := nil;

  with TIniFile.Create(ExtractFilePath(Application.ExeName) + 'logger.ini') do
  try
    edComPort.Text := ReadString('Port', 'COM', '10');
    edClientID.Text := ReadString('GoogleAPI', 'ClientID', '');
    edClientSecret.Text := ReadString('GoogleAPI', 'ClientSecret', '');
  finally
    Free;
  end;

  btReloadServer.Click;
end;

procedure TMainFrm.Timer1Timer(Sender: TObject);
var
  StartDate: TDateTime;
begin
  Timer1.Enabled := false;
  if ex <> nil then
    try
      // get data statistic
      StartDate := ex.CO2Meter.GetDataStartDate;
      lbStatus.Caption := 'Data count:' + IntToStr(ex.CO2Meter.GetDataCount);
      if StartDate > EncodeDate(2000, 1, 1) then
        lbStatus.Caption := lbStatus.Caption + ' start date:' + DateTimeToStr(StartDate);

      // execute sending data
      if cbExecute.Checked and (ex.CO2Meter.GetDataCount > 0) then
      begin
        ex.WorkCycle;
        lbStatus.Caption := lbStatus.Caption + #$0D#$0A'Data sent...';
      end;

    except
    end;
  Timer1.Enabled := true;
end;

end.
