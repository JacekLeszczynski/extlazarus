unit lazflogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, LazGradient, ExtMessage;

type

  TTestLoginEvent = procedure(Sender: TObject; aLogin, aPassword: string; aIndex: integer; aItem: string; var IsLogin: boolean) of object;
  TTestMaxLogins = procedure(Sender: TObject; var IsOK: boolean) of object;
  TLazLoginChangeIndexEvent = procedure(aIndex: integer; aItem: string) of object;

  { TLazFormLogin }

  TLazFormLogin = class(TForm)
    anuluj: TBitBtn;
    c2: TComboBox;
    message: TExtMessage;
    haslo: TLabeledEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    c1: TLabel;
    Grad: TLazGradient;
    ok: TBitBtn;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel2: TPanel;
    uzytkownik: TLabeledEdit;
    procedure anulujClick(Sender: TObject);
    procedure c2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure okClick(Sender: TObject);
  private
    { private declarations }
    function test(s11,s22:string):boolean;
  public
    { public declarations }
    FTestLogin: ^TTestLoginEvent;
    FTestMaxLogins: ^TTestMaxLogins;
    FLazLoginChangeIndex: ^TLazLoginChangeIndexEvent;
    HOST,DATABASE: string;
    ADM_LOGIN,ADM_PASSWORD: string;
    sDialogError: string;
    _RES: array [1..6] of boolean;
    _LOGIN,_ITEMRETURN: string;
    ERR: integer;
    sERR: string;
    BB: boolean;
    _INDEX,_INDEXRETURN: integer;
    B_REZYGNUJ: boolean;
  end;

{$IFDEF WINDOWS}
//function PwdVerify(server,baza,LazFormLogin,haslo:PChar;integrated:boolean;programtype,connectiontype:integer):integer; stdcall; external 'PWD_WB.DLL' name 'PwdVerify';
{$ENDIF}

var
  LazFormLogin: TLazFormLogin;

implementation

{$R *.lfm}

{ TLazFormLogin }

procedure TLazFormLogin.okClick(Sender: TObject);
var
  s1,s2: string;
  b: boolean;
begin
  B_REZYGNUJ:=false;
  ERR:=0;
  sERR:='';
  s1:=uzytkownik.Text;
  s2:=haslo.Text;
  if (ADM_LOGIN<>'') and (ADM_PASSWORD<>'') and (s1=ADM_LOGIN) and (s2=ADM_PASSWORD) then BB:=true else BB:=false;
  try
    if not BB then
    begin
      b:=true;
      if Assigned(FTestMaxLogins^) then FTestMaxLogins^(Sender,b);
      if b then if Assigned(FTestLogin^) then FTestLogin^(Sender,s1,s2,c2.ItemIndex,c2.Text,BB) else BB:=test(s1,s2);
    end;
  except on E : Exception do
    begin
      ERR:=1;
      sERR:=E.Message;
      BB:=false;
      Close;
      Exit;
    end;
  end;
  if b and BB then
  begin
    _LOGIN:=s1;
    _INDEXRETURN:=c2.ItemIndex;
    _ITEMRETURN:=c2.Text;
    Close;
  end else begin
    if b then message.ShowInformation('BRAK DOSTĘPU!',sDialogError) else message.ShowWarning('BRAK DOSTĘPU!','Przekroczona liczba dozwolonych stanowisk!'+#13+'Dostęp zabroniony.');
    ActiveControl:=uzytkownik;
  end;
end;

function TLazFormLogin.test(s11, s22: string): boolean;
var
  i: integer;
  b: boolean;
  s1,s2,s3,s4: string;
begin
  b:=false;
  s1:=HOST;
  s2:=DATABASE;
  if s2[1]='[' then delete(s2,1,1);
  if s2[length(s2)]=']' then delete(s2,length(s2),1);
  s3:=s11;
  s4:=s22;
  for i:=1 to 6 do
  begin
    if _RES[i] then
    begin
      {$IFDEF WINDOWS}
      {if PwdVerify(pchar(s1),pchar(s2),pchar(s3),pchar(s4),false,i,3)<>0 then
      begin
        b:=true;
        break;
      end;}
      b:=false;
      break;
      {$ELSE}
      b:=false;
      break;
      {$ENDIF}
    end;
  end;
  result:=b;
end;

procedure TLazFormLogin.anulujClick(Sender: TObject);
begin
  B_REZYGNUJ:=true;
  ERR:=0;
  sERR:='';
  BB:=false;
  _INDEXRETURN:=-1;
  _ITEMRETURN:='';
  Close;
end;

procedure TLazFormLogin.c2Change(Sender: TObject);
begin
  if Assigned(FLazLoginChangeIndex^) then FLazLoginChangeIndex^(c2.ItemIndex,c2.Text);
end;

procedure TLazFormLogin.FormCreate(Sender: TObject);
begin
  LazFormLogin.Height:=193; //220
end;

procedure TLazFormLogin.FormShow(Sender: TObject);
begin
  ERR:=0;
  sERR:='';
  if c2.Items.Count=0 then
  begin
    c1.Visible:=false;
    c2.Visible:=false;
    LazFormLogin.Height:=193;
  end else begin
    c1.Visible:=true;
    c2.Visible:=true;
    LazFormLogin.Height:=220;
    c2.ItemIndex:=_INDEX;
  end;
  if uzytkownik.Text='' then ActiveControl:=uzytkownik else ActiveControl:=haslo;
end;

end.

