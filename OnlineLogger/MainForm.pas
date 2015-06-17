unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.AnsiStrings, DateUtils, IniFiles, UITypes,
  REST.Authenticator.OAuth.WebForm.Win, Vcl.StdCtrls, Vcl.Buttons,
  MainExecutor, GoogleSender, def, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TMainFrm = class(TForm)
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    pcMain: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lbCurrentMes: TLabel;
    edComPort: TEdit;
    edClientSecret: TEdit;
    edClientID: TEdit;
    shCOMState: TShape;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btReloadServer: TButton;
    lbStatus: TLabel;
    Button1: TButton;
    cbExecute: TCheckBox;
    BitBtn2: TBitBtn;
    btSave: TButton;
    lbCurrentState: TLabel;
    shGoogleConnect: TShape;
    Label4: TLabel;
    cbGetOfflineData: TCheckBox;
    cbFlushOfflineData: TCheckBox;
    procedure BitBtn2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btReloadServerClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    { Private declarations }
    ex: TMainExecutor;
    FIni: TIniFile;

    isFirstConnect: boolean;
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
      ExtractFilePath(Application.ExeName) + '\storage.txt',
      Fini);
    ex.CO2Meter.GetOfflineData := cbGetOfflineData.Checked;
    ex.CO2Meter.FlushOfflineData := cbFlushOfflineData.Checked;
  except
     on E : Exception do
     begin
       MessageDlg('Can''t init engine. Error:' + E.Message, mtError, [mbOk], 0);
     end;
  end;
end;

procedure TMainFrm.btSaveClick(Sender: TObject);
begin
  with FIni do
  try
    WriteString('Port', 'COM', edComPort.Text);
    WriteString('GoogleAPI', 'ClientID', edClientID.Text);
    WriteString('GoogleAPI', 'ClientSecret', edClientSecret.Text);
    WriteBool('Config', 'Execute', cbExecute.Checked);
    WriteBool('Config', 'GetOfflineData', cbGetOfflineData.Checked);
    WriteBool('Config', 'FlushOfflineData', cbFlushOfflineData.Checked);
  except
  end;
end;

procedure TMainFrm.Button1Click(Sender: TObject);
begin
  ex.WorkCycle;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  ex := nil;
  isFirstConnect := true;

  FIni := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'logger.ini');

  with FIni do
  try
    edComPort.Text := ReadString('Port', 'COM', '10');
    edClientID.Text := ReadString('GoogleAPI', 'ClientID', '');
    edClientSecret.Text := ReadString('GoogleAPI', 'ClientSecret', '');
    cbExecute.Checked := ReadBool('Config', 'Execute', false);
    cbGetOfflineData.Checked := ReadBool('Config', 'GetOfflineData', false);
    cbFlushOfflineData.Checked := ReadBool('Config', 'FlushOfflineData', false);
  except
  end;

  pcMain.ActivePageIndex := 0;

  btReloadServer.Click;
end;

procedure TMainFrm.Timer1Timer(Sender: TObject);
var
  StartDate: TDateTime;
begin
  Timer1.Enabled := false;
  if ex <> nil then
    try
      // com port connect
      if ex.CO2Meter.Connected then
      begin
        shCOMState.Brush.Color := clGreen;

        if isFirstConnect then
        begin
          pcMain.ActivePageIndex := 1;
          isFirstConnect := false;
        end;
      end
      else
        shCOMState.Brush.Color := clRed;

      // google connect state
      if not ex.GoogleSender.Connected then
        shGoogleConnect.Brush.Color := clRed
      else
        if ex.CO2Meter.DataCount < 10 then
          shGoogleConnect.Brush.Color := clGreen
        else
          shGoogleConnect.Brush.Color := clYellow;

      // get current measurement
      if ex.CO2Meter.Connected and
         (ex.CO2Meter.CurrentMeasurement.InternalDate <> 0)
      then
      begin
        lbCurrentState.Caption := 'Date and time: ' +
          FormatDateTime('DD.MM.YYYY HH.NN.SS', ex.CO2Meter.CurrentMeasurement.Date);
        lbCurrentMes.Caption := ex.CO2Meter.CurrentMeasurement.AsString2Lines;
        TrayIcon1.Hint := ex.CO2Meter.CurrentMeasurement.AsString2Lines;
      end
      else
      begin
        lbCurrentState.Caption := '';
        lbCurrentMes.Caption := '';
        TrayIcon1.Hint := 'Logger...';
      end;

      // get data statistic
      StartDate := ex.CO2Meter.DataStartDate;
      lbStatus.Caption := 'Data count:' + IntToStr(ex.CO2Meter.DataCount);
      if StartDate > EncodeDate(2000, 1, 1) then
        lbStatus.Caption := lbStatus.Caption + ' start date:' + DateTimeToStr(StartDate);

      // execute sending data
      if cbExecute.Checked and (ex.CO2Meter.DataCount > 0) then
      begin
        ex.WorkCycle;
        lbStatus.Caption := lbStatus.Caption + #$0D#$0A'Data sent...';
      end;

    except
    end;
  Timer1.Enabled := true;
end;

procedure TMainFrm.TrayIcon1DblClick(Sender: TObject);
begin
  SetForegroundWindow(Self.Handle);
end;

end.
