unit LazLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, lazflogin, LazGradient;

type

  { Typy wyliczeniowe i ich zbiory }

  TSetRes = (wfFakir, wfKaper, wfMag, wfBest, wfGang, wfAnalizy);
  TRes = set of TSetRes;
  TSetPictureOpt = (poAutoSize, poCenter, poAlignClient, poProportional, poStretch);
  TPictureOpt = set of TSetPictureOpt;
  TLoginBorderStyle = (lbsDialog, lbsNone, lbsToolWindow);
  TReadErrorEvent = procedure(Sender: TObject; N: integer; S: string; DB: boolean) of object;

  { Zdarzenia }

  TReturnEvent = procedure(Sender: TObject; IsLogin: boolean; User: string; vIndex: integer; vItem: string) of object;
  TTestLoginEvent = procedure(Sender: TObject; Login,Password: string; var IsLogin: boolean) of object;
  TTestMaxLogins = procedure(Sender: TObject; var IsOK: boolean) of object;
  TTestUserEvent = procedure(Sender: TObject; Login: string; var IsLogin: boolean) of object;

  { TLazLogin }

  TLazLogin = class(TComponent)
  private
    { Private declarations }
    FBorderStyle: TLoginBorderStyle;
    FButtonHeight: integer;
    FCancelWidth: integer;
    FCCancel: string;
    FCLogin: string;
    FCOK: string;
    FCPassw: string;
    FFontButtons: TFont;
    FOkWidth: integer;
    FScreenCenter: boolean;
    FTestMaxLogins: TTestMaxLogins;
    FTestUser: TTestUserEvent;
    zm_logowania: boolean;
    zm_user: string;
    ERR: integer;
    sERR: string;
    FReturn: TReturnEvent;
    FError: TReadErrorEvent;
    FTestLogin: TTestLoginEvent;
    FHost,FDatabase: string;
    FRes: TRes;
    FNL: boolean;
    FSysName: string;
    FFont,FTextFont: TFont;
    FPicture: TPicture;
    FTextVisible,FPictureVisible: boolean;
    FPictureOpt: TPictureOpt;
    FColorPan1,FColorPan2,FColorPan3,FGradColor: TColor;
    FGradPan2,FGradRounded: boolean;
    FGradOrientation: TOrientation;
    FText,FDialogError,FLCaption,FLastLogin: string;
    FItems: TStrings;
    FItemIndex: integer;
    procedure SetCCancel(AValue: string);
    procedure SetCLogin(AValue: string);
    procedure SetCOK(AValue: string);
    procedure SetCPassw(AValue: string);
    procedure SetDialogError(const AValue: string);
    procedure SetSysName(const AValue: string);
    procedure SetItems(const AValue: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(SuperLogin:string='';SuperPassword:string=''):boolean;
    function ExecuteForceLogin(Login:string):boolean;
    function GetLogin:string;
  published
    { Published declarations }
    property Host: string read FHost write FHost;
    property Database: string read FDatabase write FDatabase;
    property Resources: TRes read FRes write FRes;
    property Caption: string read FSysName write SetSysName;
    property CaptionUser: string read FCLogin write SetCLogin;
    property CaptionPassw: string read FCPassw write SetCPassw;
    property CaptionBtOK: string read FCOK write SetCOK;
    property CaptionBtCancel: string read FCCancel write SetCCancel;
    property WidthOK: integer read FOkWidth write FOkWidth default 83;
    property WidthCancel: integer read FCancelWidth write FCancelWidth default 83;
    property BorderStyle: TLoginBorderStyle read FBorderStyle write FBorderStyle default lbsDialog;
    property AutoTerminate: boolean read FNL write FNL default false;
    property Font: TFont read FFont write FFont;
    property ButtonFont: TFont read FFontButtons write FFontButtons;
    property ButtonHeight: integer read FButtonHeight write FButtonHeight default 30;
    property Text: string read FText write FText;
    property TextVisible: boolean read FTextVisible write FTextVisible default false;
    property TextFont: TFont read FTextFont write FTextFont;
    property Picture: TPicture read FPicture write FPicture;
    property PictureVisible: boolean read FPictureVisible write FPictureVisible default false;
    property PictureOptions: TPictureOpt read FPictureOpt write FPictureOpt default [];
    property ColorPan1: TColor read FColorPan1 write FColorPan1;
    property ColorPan2: TColor read FColorPan2 write FColorPan2;
    property ColorPan3: TColor read FColorPan3 write FColorPan3;
    property GradPan2: boolean read FGradPan2 write FGradPan2 default false;
    property GradRounded: boolean read FGradRounded write FGradRounded;
    property GradColor: TColor read FGradColor write FGradColor;
    property GradOrientation: TOrientation read FGradOrientation write FGradOrientation;
    property DialogError: string read FDialogError write SetDialogError;
    property ListCaption: string read FLCaption write FLCaption;
    property ListItems: TStrings read FItems write SetItems;
    property ListItemIndex: integer read FItemIndex write FItemIndex;
    property LastLogin: string read FLastLogin write FLastLogin;
    property ScreenCenter: boolean read FScreenCenter write FScreenCenter default false;
    //zdarzenia
    property OnAfterExecute: TReturnEvent read FReturn write FReturn;
    property OnError: TReadErrorEvent read FError write FError;
    property OnTestLogin: TTestLoginEvent read FTestLogin write FTestLogin;
    property OnTestUser: TTestUserEvent read FTestUser write FTestUser;
    property OnTestMaxLogins: TTestMaxLogins read FTestMaxLogins write FTestMaxLogins;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('System',[TLazLogin]);
end;

{ TLazLogin }

procedure TLazLogin.SetDialogError(const AValue: string);
begin
  if AValue='' then FDialogError:='<auto>' else FDialogError:=AValue;
end;

procedure TLazLogin.SetCLogin(AValue: string);
begin
  if AValue='' then FCLogin:='<Auto>' else FCLogin:=AValue;
end;

procedure TLazLogin.SetCCancel(AValue: string);
begin
  if AValue='' then FCCancel:='<Auto>' else FCCancel:=AValue;
end;

procedure TLazLogin.SetCOK(AValue: string);
begin
  if AValue='' then FCOK:='<Auto>' else FCOK:=AValue;
end;

procedure TLazLogin.SetCPassw(AValue: string);
begin
  if AValue='' then FCPassw:='<Auto>' else FCPassw:=AValue;
end;

procedure TLazLogin.SetSysName(const AValue: string);
begin
  if AValue='' then FSysName:='<auto>' else FSysName:=AValue;
end;

procedure TLazLogin.SetItems(const AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

constructor TLazLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont:=TFont.Create;
  FFontButtons:=TFont.Create;
  FTextFont:=TFont.Create;
  FPicture:=TPicture.Create;
  FItems:=TStringList.Create;
  //inicjowanie zmiennych
  ERR:=0;
  sERR:='';
  FNL:=false;
  FSysName:='<auto>';
  FTextVisible:=false;
  FText:='';
  FPictureVisible:=false;
  FPictureOpt:=[];
  FColorPan1:=clDefault;
  FColorPan2:=clDefault;
  FColorPan3:=clDefault;
  FGradPan2:=false;
  FGradColor:=clDefault;
  FGradRounded:=false;
  FGradOrientation:=foLeftToRight;
  FDialogError:='<auto>';
  FLCaption:='';
  FItemIndex:=-1;
  FLastLogin:='';
  FBorderStyle:=lbsDialog;
  FCLogin:='<Auto>';
  FCPassw:='<Auto>';
  FCOK:='<Auto>';
  FCCancel:='<Auto>';
  FOkWidth:=83;
  FCancelWidth:=83;
  FButtonHeight:=30;
  FScreenCenter:=false;
end;

destructor TLazLogin.Destroy;
begin
  FFont.Free;
  FFontButtons.Free;
  FTextFont.Free;
  FPicture.Free;
  FItems.Free;
  inherited Destroy;
end;

function TLazLogin.Execute(SuperLogin: string; SuperPassword: string
  ): boolean;
var
  pom: string;
  pom1,pom2: TColor;
  szparka,calosc: integer;
  left,top: integer;
begin
  ERR:=0;
  sERR:='';
  try
    zm_logowania:=false;
    zm_user:='';
    Application.CreateForm(TLazFormLogin, LazFormLogin);
    if FScreenCenter then LazFormLogin.Position:=poScreenCenter;
    application.ProcessMessages;
    { ustawianie atrybutów okna }
    case FBorderStyle of
      lbsDialog:     LazFormLogin.BorderStyle:=bsDialog;
      lbsNone:       LazFormLogin.BorderStyle:=bsNone;
      lbsToolWindow: LazFormLogin.BorderStyle:=bsToolWindow;
    end;
    if FSysName='<auto>' then LazFormLogin.Caption:='Logowanie do systemu' else LazFormLogin.Caption:=FSysName;
    //text
    LazFormLogin.Label1.Visible:=FTextVisible;
    LazFormLogin.Label1.Caption:=FText;
    LazFormLogin.Label1.Font.Assign(FTextFont);
    if FCLogin='<Auto>' then pom:='Użytkownik' else pom:=FCLogin;
    LazFormLogin.uzytkownik.EditLabel.Caption:=pom+':';
    if FCPassw='<Auto>' then pom:='Hasło' else pom:=FCPassw;
    LazFormLogin.haslo.EditLabel.Caption:=pom+':';
    //Buttons
    if FCOK='<Auto>' then pom:='Zatwierdź' else pom:=FCOK;
    LazFormLogin.ok.Caption:=pom;
    if FCCancel='<Auto>' then pom:='Rezygnuj' else pom:=FCCancel;
    LazFormLogin.anuluj.Caption:=pom;
    with LazFormLogin do
    begin
      //szerokość
      ok.Width:=FOkWidth;
      anuluj.Width:=FCancelWidth;
      anuluj.Left:=Width-anuluj.Width-7;
      ok.Left:=Width-ok.Width-anuluj.Width-12;
      Label3.Left:=anuluj.Left+(anuluj.Width-Label3.Width)-3;
      Label2.Left:=ok.Left+(ok.Width-Label2.Width)-3;
      //wysokość
      ok.Height:=FButtonHeight;
      anuluj.Height:=FButtonHeight;
      szparka:=anuluj.Top-Label3.Height-Label3.Top;
      calosc:=ok.Height+Label3.Height+szparka;
      Label2.Top:=round((Panel1.Height-calosc)*0.27);
      ok.Top:=Label2.Top+Label2.Height+szparka;
      anuluj.Top:=ok.Top;
      Label3.Top:=Label2.Top;
    end;
    //panele
    LazFormLogin.Panel1.Font.Assign(FFont);
    LazFormLogin.Panel2.Font.Assign(FFont);
    LazFormLogin.Panel3.Font.Assign(FFont);
    LazFormLogin.ok.Font.Assign(FFontButtons);
    LazFormLogin.anuluj.Font.Assign(FFontButtons);
    LazFormLogin.Panel1.Color:=FColorPan1;
    LazFormLogin.Panel2.Color:=FColorPan2;
    LazFormLogin.Panel3.Color:=FColorPan3;
    //Image
    LazFormLogin.Image1.Visible:=FPictureVisible;
    LazFormLogin.Image1.Picture.Assign(FPicture.Pixmap);
    if poAlignClient in FPictureOpt then
    begin
      LazFormLogin.Image1.AutoSize:=false;
      LazFormLogin.Image1.Top:=0;
      LazFormLogin.Image1.Left:=1;
      LazFormLogin.Image1.Height:=LazFormLogin.Panel1.Height-1;
      LazFormLogin.Image1.Width:=LazFormLogin.Panel1.Width-2;
      LazFormLogin.Image1.Center:=poCenter in FPictureOpt;
    end else begin
      LazFormLogin.Image1.AutoSize:=poAutoSize in FPictureOpt;
      LazFormLogin.Image1.Align:=alNone;
      LazFormLogin.Image1.Center:=false;
    end;
    LazFormLogin.Image1.Proportional:=poProportional in FPictureOpt;
    LazFormLogin.Image1.Stretch:=poStretch in FPictureOpt;
    //ListItems
    if FLCaption='' then LazFormLogin.c1.Caption:='' else LazFormLogin.c1.Caption:=FLCaption+':';
    if FItems.Count=0 then  LazFormLogin.c2.Clear else LazFormLogin.c2.Items.Assign(FItems);
    LazFormLogin._INDEX:=FItemIndex;
    //GradPanel
    LazFormLogin.Grad.Visible:=FGradPan2;
    if FColorPan2=clDefault then pom1:=clBtnFace else pom1:=FColorPan2;
    if FGradColor=clDefault then pom2:=clBtnFace else pom2:=FGradColor;
    LazFormLogin.Grad.BeginColor:=pom1;
    LazFormLogin.Grad.EndColor:=pom2;
    LazFormLogin.Grad.Rounded:=FGradRounded;
    LazFormLogin.Grad.Orientation:=FGradOrientation;
    { reszta }
    LazFormLogin.ADM_LOGIN:=SuperLogin;
    LazFormLogin.ADM_PASSWORD:=SuperPassword;
    LazFormLogin.HOST:=FHost;
    LazFormLogin.DATABASE:=FDatabase;
    LazFormLogin._RES[1]:=wfFakir in FRes;
    LazFormLogin._RES[2]:=wfKaper in FRes;
    LazFormLogin._RES[3]:=wfMag in FRes;
    LazFormLogin._RES[4]:=wfBest in FRes;
    LazFormLogin._RES[5]:=wfGang in FRes;
    LazFormLogin._RES[6]:=wfAnalizy in FRes;
    if FDialogError='<auto>' then LazFormLogin.sDialogError:='Podałeś nieprawidłowe dane użytkownika lub hasło.'+#13+'Po poprawieniu tych danych, możesz spróbować jeszcze raz.' else LazFormLogin.sDialogError:=FDialogError;
    LazFormLogin.uzytkownik.Text:=FLastLogin;
    { procedura alternatywna i testująca }
    LazFormLogin.FTestLogin:=@FTestLogin;
    LazFormLogin.FTestMaxLogins:=@FTestMaxLogins;
    { wywołanie formularza }
    LazFormLogin.ShowModal;
    zm_logowania:=LazFormLogin.BB;
    if ERR=0 then
    begin
      if zm_logowania then zm_user:=LazFormLogin._LOGIN;
      if Assigned(FReturn) then FReturn(self,zm_logowania,zm_user,LazFormLogin._INDEXRETURN,LazFormLogin._ITEMRETURN);
    end else begin
      if Assigned(FError) then FError(self,ERR,sERR,false);
    end;
  finally
    LazFormLogin.Free;
  end;
  if (not zm_logowania) and FNL then halt;
  result:=zm_logowania;
end;

function TLazLogin.ExecuteForceLogin(Login: string): boolean;
var
  b: boolean;
begin
  if Assigned(FTestUser) then
  begin
    FTestUser(self,Login,b);
    if Assigned(FReturn) then FReturn(self,b,Login,-1,'');
  end else b:=false;
  result:=b;
end;

function TLazLogin.GetLogin: string;
begin
  result:=zm_user;
end;

initialization
  {$I lazlogin_icon.lrs}

end.
