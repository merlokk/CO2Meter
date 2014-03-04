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

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
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
