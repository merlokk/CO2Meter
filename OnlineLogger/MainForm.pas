unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, REST.Types, System.JSON, System.AnsiStrings,
  REST.Authenticator.OAuth.WebForm.Win, Vcl.StdCtrls, Vcl.Buttons, IPPeerClient,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Authenticator.OAuth, GoogleSender, def;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    BitBtn2: TBitBtn;
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
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

  sn.SendData(mes);
end;


end.