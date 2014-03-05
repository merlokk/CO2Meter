unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  CO2Meter;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Button5: TButton;
    Button6: TButton;
    Edit2: TEdit;
    Button7: TButton;
    Edit3: TEdit;
    Button8: TButton;
    Label1: TLabel;
    procedure Button4Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    m: TCO2Meter;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
Var
  ver,
  id: string;
begin
  if m = nil then exit;

  try
    m.GetInfo(id, ver);
    Memo1.Lines.Add('Get info: id=' + id + ' version=' + ver)
  except
    on e : Exception do
      Memo1.Lines.Add('Get info error: ' + e.Message);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
Var
  SamplesCount,
  SamplesRate: cardinal;
  SamplesStartDate: TDateTime;
begin
  if m = nil then exit;

  try
    m.GetMemoryStat(SamplesCount, SamplesRate, SamplesStartDate);
    Memo1.Lines.Add('Get memory stat: cnt=' + IntToStr(SamplesCount) +
                    ' rate=' + IntToStr(SamplesRate) +
                    ' start date=' + DateTimeToStr(SamplesStartDate) +
                    ' end date=' + DateTimeToStr(SamplesStartDate + SamplesRate * (SamplesCount / 3 - 1) / SecsPerDay)
    );
  except
    on e : Exception do
      Memo1.Lines.Add('Get memory stat error: ' + e.Message);
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  SamplesCount,
  SamplesRate: cardinal;
  SamplesStartDate: TDateTime;
  sl: TStringList;
begin
  if m = nil then exit;

  try
    sl := TStringList.Create;
    try
      m.GetSamples(SamplesCount, SamplesRate, SamplesStartDate, sl);
      Memo1.Lines.Add('Get samples ok. length=' + IntToStr(sl.Count) +
                    ' rate=' + IntToStr(SamplesRate) +
                    ' start date=' + DateTimeToStr(SamplesStartDate) +
                    ' end date=' + DateTimeToStr(SamplesStartDate + SamplesRate * (SamplesCount / 3 - 1) / SecsPerDay)
      );
    finally
      sl.Free;
    end;
  except
    on e : Exception do
      Memo1.Lines.Add('Get samples error: ' + e.Message);
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if m = nil then exit;

  try
    m.SetDateTime(Now);
    Memo1.Lines.Add('Set datetime ok');
  except
    on e : Exception do
      Memo1.Lines.Add('Set datetime error: ' + e.Message);
  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if m = nil then exit;

  try
    m.SetId(Edit1.Text);
    Memo1.Lines.Add('Set ID ok');
  except
    on e : Exception do
      Memo1.Lines.Add('Set ID error: ' + e.Message);
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  if m = nil then exit;

  try
    m.SetSamplingRate(StrToIntDef(Edit2.Text, 1));
    Memo1.Lines.Add('Set sampling rate ok');
  except
    on e : Exception do
      Memo1.Lines.Add('Set sampling rate error: ' + e.Message);
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  if m <> nil then m.Free;

  m := TCO2Meter.Create;
  m.OpenPort(StrToIntDef(Edit3.Text, 1));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  m := nil;
end;

end.
