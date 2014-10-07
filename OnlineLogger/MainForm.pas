unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.AnsiStrings, DateUtils,
  REST.Authenticator.OAuth.WebForm.Win, Vcl.StdCtrls, Vcl.Buttons,
  MainExecutor, GoogleSender, def;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    BitBtn2: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    procedure BitBtn2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    ex: TMainExecutor;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn2Click(Sender: TObject);
var
 sn: TGoogleSender;
 mes: TMeasurements;
begin
  sn := TGoogleSender.Create(Self,
   '945613708754-lh8napt9tngjkslvq49nlm2foivl021s.apps.googleusercontent.com', 'joitopSs5kn2ZZ50vkSUFiAQ');

  SetLength(mes, 1);
  mes[0].Date := now;
  mes[0].InternalDate := SecondsBetween(now, EncodeDate(2000, 1, 1));
  mes[0].Temperature := 10;
  mes[0].Humidity := 20;
  mes[0].CO2Level := 2000;
  sn.SendData(mes);
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  if ex = nil then
  begin
    ex := TMainExecutor.Create(Self);
    ex.Init(10,
      '945613708754-lh8napt9tngjkslvq49nlm2foivl021s.apps.googleusercontent.com',
      'joitopSs5kn2ZZ50vkSUFiAQ',
      ExtractFilePath(Application.ExeName) + '\storage.txt');
  end;

  ex.WorkCycle;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Add('co2connect data count:' + IntToStr(ex.CO2Meter.GetDataCount) + ' start date:' + DateTimeToStr(ex.CO2Meter.GetDataStartDate));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ex := nil;
end;

end.
