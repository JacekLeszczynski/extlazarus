{
INFO:
  Komponent obsługujący piloty służące do prezentacji.
  Na ten czas działa z tym dobrze pilot Gembird.
}

unit Presentation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, ExtCtrls, PointerTab;

type

  { TPresentation }

  TPresentationDevice = (dvCustom,dvGembird);
  TPresentationCustom = procedure(aKey: word; var aButton: integer) of object;
  TPresentationOnClick = procedure(aButton: integer; var aTestDblClick: boolean) of object;
  TPresentationOnClickLong = procedure(aButton: integer; aDblClick: boolean) of object;
  TPresentation = class(TComponent)
  private
    bufor: array [1..2] of word;
    FCustom: TPresentationCustom;
    FDevice: TPresentationDevice;
    key_buf,key_last: word;
    pbufor: TPointerTab;
    timer_bufor,timer_pbufor: TTimer;
    FOnClick: TPresentationOnClick;
    FOnClickLong: TPresentationOnClickLong;
    procedure OnTimerBufor(Sender: TObject);
    procedure OnTimerPBufor(Sender: TObject);
    procedure pbuforCreateElement(Sender: TObject; var AWskaznik: Pointer);
    procedure pbuforDestroyElement(Sender: TObject; var AWskaznik: Pointer);
    procedure pbuforReadElement(Sender: TObject; var AWskaznik: Pointer);
    procedure pbuforWriteElement(Sender: TObject; var AWskaznik: Pointer);
    procedure przycisk_szybki(aNr: integer);
    procedure przycisk_wolny(aNr: integer; aDwuklik: boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(aKey: Word); //Adding to OnkKeyDown method for End-Code!
    procedure ExecuteEx(aPilotKey: Word);
    procedure SendKey(vkey: word);
    procedure SendKeyEx(vkey: word);
    procedure SendKeyEx(vkey1,vkey2: word);
    procedure SendKeyEx(aShift: TShiftState; vkey: word);
  published
    property Device: TPresentationDevice read FDevice write FDevice;
    property OnCustom: TPresentationCustom read FCustom write FCustom;
    //Natychmiastowe zadziałanie przycisku
    property OnClick: TPresentationOnClick read FOnClick write FOnClick;
    //Opóźnione zadziałanie przycisku
    //więc może zostać wykryty dwuklik
    property OnClickLong: TPresentationOnClickLong read FOnClickLong write FOnClickLong;
  end;

procedure Register;

implementation

uses
  MouseAndKeyInput;

type
  TKeyElement = record
    key: word;
  end;
  PKeyElement = ^TKeyElement;

var
  KeyElement: TKeyElement;

procedure Register;
begin
  {$I presentation_icon.lrs}
  RegisterComponents('Multimedia',[TPresentation]);
end;

{ TPresentation }

procedure TPresentation.OnTimerBufor(Sender: TObject);
begin
  timer_bufor.Enabled:=false;
  przycisk_wolny(bufor[1],bufor[1]=bufor[2]);
  key_last:=bufor[1];
  bufor[1]:=0;
  bufor[2]:=0;
end;

procedure TPresentation.OnTimerPBufor(Sender: TObject);
begin
  if pbufor.Read then KeyInput.Press(KeyElement.key) else timer_pbufor.Enabled:=false;
end;

procedure TPresentation.pbuforCreateElement(Sender: TObject;
  var AWskaznik: Pointer);
var
  p: PKeyElement;
begin
  new(p);
  AWskaznik:=p;
end;

procedure TPresentation.pbuforDestroyElement(Sender: TObject;
  var AWskaznik: Pointer);
var
  p: PKeyElement;
begin
  p:=AWskaznik;
  dispose(p);
  AWskaznik:=nil;
end;

procedure TPresentation.pbuforReadElement(Sender: TObject;
  var AWskaznik: Pointer);
var
  p: PKeyElement;
begin
  p:=AWskaznik;
  KeyElement:=p^;
end;

procedure TPresentation.pbuforWriteElement(Sender: TObject;
  var AWskaznik: Pointer);
var
  p: PKeyElement;
begin
  p:=AWskaznik;
  p^:=KeyElement;
end;

procedure TPresentation.przycisk_szybki(aNr: integer);
var
  vTest: boolean;
begin
  vTest:=false;
  if Assigned(FOnClick) then FOnClick(aNr,vTest);
  if vTest then
  begin
    bufor[2]:=bufor[1];
    bufor[1]:=aNr;
    if not timer_bufor.Enabled then timer_bufor.Enabled:=true;
  end;
end;

procedure TPresentation.przycisk_wolny(aNr: integer; aDwuklik: boolean);
begin
  if Assigned(FOnClickLong) then FOnClickLong(aNr,aDwuklik);
end;

constructor TPresentation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDevice:=dvCustom;
  pbufor:=TPointerTab.Create(nil);
  pbufor.Rodzaj:=roKolejka;
  pbufor.OnCreateElement:=@pbuforCreateElement;
  pbufor.OnDestroyElement:=@pbuforDestroyElement;
  pbufor.OnReadElement:=@pbuforReadElement;
  pbufor.OnWriteElement:=@pbuforWriteElement;
  timer_bufor:=TTimer.Create(nil);
  timer_bufor.Enabled:=false;
  timer_bufor.Interval:=250;
  timer_bufor.OnTimer:=@OnTimerBufor;
  timer_pbufor:=TTimer.Create(nil);
  timer_pbufor.Enabled:=false;
  timer_pbufor.Interval:=1;
  timer_pbufor.OnTimer:=@OnTimerPBufor;
end;

destructor TPresentation.Destroy;
begin
  pbufor.Clear;
  pbufor.Free;
  timer_bufor.Free;
  timer_pbufor.Free;
  inherited Destroy;
end;

procedure TPresentation.Execute(aKey: Word);
var
  vButton: integer;
begin
  (* test pilota *)
  if (FDevice=dvCustom) and Assigned(FCustom) then
  begin
    FCustom(aKey,vButton);
    przycisk_szybki(vButton);
  end else
  if FDevice=dvGembird then
  begin
    if (aKey=66) and (key_buf<>17) then przycisk_szybki(1) else
    if aKey=33 then przycisk_szybki(2) else
    if aKey=34 then przycisk_szybki(3) else
    if ((aKey=18) and (key_buf=66)) then przycisk_szybki(4) else
    if aKey=192 then przycisk_szybki(5);
  end;
  (* ending code *)
  if aKey>0 then key_buf:=aKey;
end;

procedure TPresentation.ExecuteEx(aPilotKey: Word);
begin
  if aPilotKey=1 then przycisk_szybki(1) else
  if aPilotKey=2 then przycisk_szybki(2) else
  if aPilotKey=3 then przycisk_szybki(3) else
  if aPilotKey=4 then przycisk_szybki(4) else
  if aPilotKey=5 then przycisk_szybki(5);
end;

procedure TPresentation.SendKey(vkey: word);
var
  i: integer;
begin
  for i:=1 to 40 do
  begin
    KeyElement.key:=vkey;
    pbufor.Add;
  end;
  timer_pbufor.Enabled:=true;
end;

procedure TPresentation.SendKeyEx(vkey: word);
begin
  KeyInput.Press(vkey);
end;

procedure TPresentation.SendKeyEx(vkey1, vkey2: word);
begin
  KeyInput.Down(vkey1);
  KeyInput.Press(vkey2);
  KeyInput.Up(vkey1);
end;

procedure TPresentation.SendKeyEx(aShift: TShiftState; vkey: word);
begin
  KeyInput.Apply(aShift);
  KeyInput.Press(vkey);
  KeyInput.Unapply(aShift);
end;

{ TPresentation }

end.
