unit Polfan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Graphics, netsynwebsocket, WebSocket2;

type
  TPolfanRoomMode = (rmService,rmRoom,rmUser);

type
  TPolfanArchOsInfoElements = (osNone,osWindows86,osWindows64,osLinux86,osLinux64,osFreeBSD86,osFreeBSD64);
  TPolfanOnBeforeConnect = procedure(Sender: TObject; aUser, aFingerPrint, aOS: string; var aAccepted: boolean) of object;
  TPolfanOnClose = procedure(Sender: TObject; aErr: integer; aErrorString: string) of object;
  TPolfanOnSoundRequest = procedure(Sender: TObject; aUser, aRoom: string) of object;
  TPolfanOnRead = procedure(Sender: TObject; aFrame: string) of object;
  TPolfanOnImageFilename = procedure(Sender: TObject; aFilename: string; var aNewFilename: string) of object;
  TPolfanOnDownloadRequest = procedure(Sender: TObject; AFilename, APrive: string) of object;
  TPolfanOnDownloadNow = procedure(Sender: TObject; APrive: string) of object;
  TPolfanOnUserToList = procedure(Sender: TObject; ARoom, AUsername, ADescription: string; aAttr: integer) of object;
  TPolfanOnNewAttr = procedure(Sender: TObject; aAttr: integer) of object;
  TPolfanOnUserNewAttr = procedure(Sender: TObject; ARoom, AUsername: string; aAttr: integer) of object;
  TPolfanOnUserDelFromList = procedure(Sender: TObject; ARoom, AUsername: string) of object;
  TPolfanOnInitUserList = procedure(Sender: TObject; AUsers, ADescriptions, AAttributes: TStrings) of object;
  TPolfanOnBeforeReadFrame = procedure(Sender: TObject; aFrame: string; var aDropNow, aNotLogSrc: boolean) of object;
  TPolfanOnBeforeReadDocument = procedure(Sender: TObject; AName, aNadawca, aAdresat: string; AMode: TPolfanRoomMode; AMessage: string; var aDropNow, aDeleteFromLogSrc: boolean) of object;
  TPolfanOnReadDocument = procedure(Sender: TObject; AName: string; AMode: TPolfanRoomMode; AMessage: string; ADocument: TStrings; ARefresh: boolean) of object;
  TPolfanOnRoomBegin = procedure(Sender: TObject; ARoom, AOpis: string) of object;
  TPolfanOnRoomEnd = procedure(Sender: TObject; ARoom: string) of object;
  TPolfanOnRoomAdd = procedure(Sender: TObject; AUser: string; var AForceSetRoom: boolean) of object;
  TPolfanOnRoomDel = procedure(Sender: TObject; AUser: string; AID: integer) of object;
  TPolfanOnAutoResponseRequest = procedure(Sender: TObject; AUser, AMessage: string) of object;

  { TPolfanRoom }

  TPolfanRoom = class
  private
    FDocument: TStrings;
    FName,FDescription: string;
    FMode: TPolfanRoomMode;
    uzyt,uzyt_opis,uzyt_attr: TStrings;
    function GetIndex(AName: string): integer;
  protected
  public
    constructor Create(AName,ADescription: string; AMode: TPolfanRoomMode);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AText: string);
    procedure AddUser(AName,ADescription: string; AAttr: integer);
    procedure DelUser(AName: string);
    procedure UpdateAttr(AName: string; AAttr: integer);
    procedure Truncate(ACount: integer);
  published
    property Name: string read FName;
    property Description: string read FDescription;
    property Mode: TPolfanRoomMode read FMode;
    property Document: TStrings read FDocument;
    property Users: TStrings read uzyt write uzyt;
    property UsersInfo: TStrings read uzyt_opis write uzyt_opis;
    property UsersAttr: TStrings read uzyt_attr write uzyt_attr;
  end;

  { TPolfanRooms }

  TPolfanRoomsTabs = array of TPolfanRoom;

  TPolfanRooms = class
  private
    FCount: integer;
    FCountRooms,FCountUsers: integer;
    FRooms: TPolfanRoomsTabs;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AName, ADescription: string; AMode: TPolfanRoomMode): integer;
    procedure Delete(AIndex: integer);
    procedure DeleteAll;
    function GetIndex(AName: string): integer;
    function GetIndex(AName: string; AMode: TPolfanRoomMode): integer;
    function GetIndexRoom(AName: string): integer;
    function GetIndexUser(AName: string): integer;
  published
    property Rooms: TPolfanRoomsTabs read FRooms write FRooms;
    property Count: integer read FCount;
    property CountRooms: integer read FCountRooms;
    property CountUsers: integer read FCountUsers;
  end;

  { TPolfan }

  TPolfan = class(TComponent)
  private
    FActive: boolean;
    FAutoResponse: boolean;
    FAutoResponseRequest: TPolfanOnAutoResponseRequest;
    FBaseDirectory: string;
    FDevOn: boolean;
    FFingerPrint: string;
    FIdentify: string;
    FImagesOFF: boolean;
    FImgAltVisible: boolean;
    FMaxLines: integer;
    FOnAfterConnect: TNotifyEvent;
    FOnBeforeConnect: TPolfanOnBeforeConnect;
    FOnBeforeReadDocument: TPolfanOnBeforeReadDocument;
    FOnBeforeReadFrame: TPolfanOnBeforeReadFrame;
    FOnClose: TPolfanOnClose;
    FOnDownloadNow: TPolfanOnDownloadNow;
    FOnDownloadRequest: TPolfanOnDownloadRequest;
    FOnImageFilename: TPolfanOnImageFilename;
    FOnListUserAdd: TPolfanOnUserToList;
    FOnListUserDeInit: TNotifyEvent;
    FOnListUserDel: TPolfanOnUserDelFromList;
    FOnListUsersInit: TPolfanOnInitUserList;
    FOnNewAttr: TPolfanOnNewAttr;
    FOnOpen: TNotifyEvent;
    FOnRead: TPolfanOnRead;
    FOnReadDocument: TPolfanOnReadDocument;
    FOnRoomAdd: TPolfanOnRoomAdd;
    FOnRoomClearRequest: TNotifyEvent;
    FOnRoomDel: TPolfanOnRoomDel;
    FOnRoomDelAll: TNotifyEvent;
    FOnRoomIn: TPolfanOnRoomBegin;
    FOnRoomOut: TPolfanOnRoomEnd;
    FOnSoundRequest: TPolfanOnSoundRequest;
    FOnUserAttr: TPolfanOnUserNewAttr;
    FProgName,FProgVersion: string;
    FSilentMute: boolean;
    FUser,FPassword,FRoom: string;
    FUserStatus: string;
    web: TNetSynWebSocket;
    color_user,color_op: string;
    color_guest: array of string;
    color_guest_count: integer;
    pokoje: TPolfanRooms;
    nick: string;
    time_status: integer;
    src_dump: boolean;
    src,document: TStrings;
    kategorie_emotek: TStringList;
    send_zmiana_pokoju: boolean;
    error_connected: integer;
    error_connected_str: string;
    procedure SetIdentify(AValue: string);
    procedure SetUserStatus(AValue: string);
    procedure WebOpen(ASender: TWebSocketCustomConnection);
    procedure WebClose(aSender: TWebSocketCustomConnection;
      aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure WebRead(aSender: TWebSocketCustomConnection; aFinal, aRes1,
      aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    {$IFDEF WINDOWS}function WindowsOsInfo: string;{$ENDIF}
    function ArchInfo: TPolfanArchOsInfoElements;
    function IsExistImages(aPrive: string; var str: string): boolean;
    function ImagePlusOpis(str: string): string;
    procedure ReadColorsTable(ATable: string);
    procedure ReadKategorieEmotek(AText: string);
    procedure ReadFrame(aFrame: string);
    procedure GoRefresh(aMessage: string = ''; aName: string = ''; aNadawca: string = ''; aAdresat: string = ''; aNadawcaWiadomosci: string = ''; aOdbiorcaWiadomosci: string = ''; aSoundNow: boolean = false);
    procedure StartDownloading(APrive: string);
    function indeks_pokoju(nazwa: string): integer;
    function dodaj_pokoj(nazwa: string): integer;
    procedure usun_pokoj(nazwa: string);
    procedure usun_wszystkie_pokoje;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure InitUserStatus;
    function SendText(AText: string; AZmianaPokoju: boolean = false): boolean;
    function SendTextIgnoreSilentMute(AText: string; AZmianaPokoju: boolean = false): boolean;
    procedure Refresh(AName: string = '');
    procedure RefreshUserList(AName: string);
    function GetLoginNick: string;
    procedure AddNewRoom(ARoom: string);
    procedure DelRoom(ARoom: string);
    procedure AddSourceDump(AText: string);
    function GetSourceDump: TStrings;
    procedure SourceDumpClear;
    procedure RoomClear;
    function IsRoom(AName: string): boolean;
    function IsUser(AName: string): boolean;
    function IsRoomsCount: integer;
    function GetColorUser(aAttr: integer): TColor;
    procedure AddDocument(aText: string; aRoom: string = '');
  published
    property Active: boolean read FActive;
    property ProgName: string read FProgName write FProgName;
    property ProgVersion: string read FProgVersion write FProgVersion;
    property Identify: string read FIdentify write SetIdentify;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Room: string read FRoom write FRoom;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
    property ImgAltVisible: boolean read FImgAltVisible write FImgAltVisible;
    property MaxLines: integer read FMaxLines write FMaxLines default 200;
    property FingerPrint: string read FFingerPrint write FFingerPrint;
    property SilentMute: boolean read FSilentMute write FSilentMute default false;
    {Gdy włączone przed połączeniem,
    będzie można zdampować całą komunikację.}
    property DeveloperCodeOn: boolean read FDevOn write FDevOn default false;
    {Status użytkownika, jeśli niedostępny
     to wpisać tekst powiadomienia.
     Aktywność należy zainicjować przez:
        metodę: InitUserStatus;}
    property UserStatus: string read FUserStatus write SetUserStatus;
    property AutoResponse: boolean read FAutoResponse write FAutoResponse;
    {Wyłącza obsługę obrazków - emotek.}
    property ImagesOFF: boolean read FImagesOFF write FImagesOFF;
    property OnBeforeConnect: TPolfanOnBeforeConnect read FOnBeforeConnect write FOnBeforeConnect;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnAFterConnect: TNotifyEvent read FOnAfterConnect write FOnAfterConnect;
    property OnClose: TPolfanOnClose read FOnClose write FOnClose;
    {Została odebrana ramka tekstu.}
    property OnRead: TPolfanOnRead read FOnRead write FOnRead;
    {Polecenie odebrania powiadomienia
     dźwiękowego.}
    property OnSoundRequest: TPolfanOnSoundRequest read FOnSoundRequest write FOnSoundRequest;
    {Translacja nazwy obrazka jeśli potrzena.}
    property OnImageFilename: TPolfanOnImageFilename read FOnImageFilename write FOnImageFilename;
    {Zapamiętaj brakujące pliki.}
    property OnDownloadRequest: TPolfanOnDownloadRequest read FOnDownloadRequest write FOnDownloadRequest;
    {Ściągnij teraz te pliki.}
    property OnDownloadNow: TPolfanOnDownloadNow read FOnDownloadNow write FOnDownloadNow;
    {Zjawia się nowy użytkownik do pokoju.}
    property OnListUserAdd: TPolfanOnUserToList read FOnListUserAdd write FOnListUserAdd;
    {Wczytanie praw lub zmiana aktualnych.}
    property OnNewAttr: TPolfanOnNewAttr read FOnNewAttr write FOnNewAttr;
    property OnUserAttr: TPolfanOnUserNewAttr read FOnUserAttr write FOnUserAttr;
    {Odchodzi użytkownik z pokoju.}
    property OnListUserDel: TPolfanOnUserDelFromList read FOnListUserDel write FOnListUserDel;
    {Inicjuję początkową listę użytkowników.}
    property OnListUserInit: TPolfanOnInitUserList read FOnListUsersInit write FOnListUsersInit;
    {Usuwa wszystkich użytkowników.}
    property OnListUserDeInit: TNotifyEvent read FOnListUserDeInit write FOnListUserDeInit;
    {Wchodzisz do pokoju.}
    property OnRoomIn: TPolfanOnRoomBegin read FOnRoomIn write FOnRoomIn;
    {Opuszczasz pokój.}
    property OnRoomOut: TPolfanOnRoomEnd read FOnRoomOut write FOnRoomOut;
    property OnRoomClearRequest: TNotifyEvent read FOnRoomClearRequest write FOnRoomClearRequest;
    property OnBeforeReadFrame: TPolfanOnBeforeReadFrame read FOnBeforeReadFrame write FOnBeforeReadFrame;
    property OnBeforeReadDocument: TPolfanOnBeforeReadDocument read FOnBeforeReadDocument write FOnBeforeReadDocument;
    property OnReadDocument: TPolfanOnReadDocument read FOnReadDocument write FOnReadDocument;
    property OnRoomAdd: TPolfanOnRoomAdd read FOnRoomAdd write FOnRoomAdd;
    property OnRoomDel: TPolfanOnRoomDel read FOnRoomDel write FOnRoomDel;
    {Czyści listę użytkowników.}
    property OnRoomDelAll: TNotifyEvent read FOnRoomDelAll write FOnRoomDelAll;
    property OnAutoResponseRequest: TPolfanOnAutoResponseRequest read FAutoResponseRequest write FAutoResponseRequest;
  end;

procedure Register;
function SColorToHtmlColor(Color: TColor): string;
function SHtmlColorToColor(s: string; out Len: integer; Default: TColor): TColor;

implementation

uses
  ecode_unit,
  {$IFDEF WINDOWS}
  dos,
  {$ENDIF}
  synautil, math, fpjson, strutils;

type
  TIm = record
    alt: string;
    index: integer;
  end;

const
  {$IFDEF UNIX}
  _FF = '/';
  {$ELSE}
  _FF = '/';
  {$ENDIF}

const
  STR_HOST = 's1.polfan.pl';
  WORD_PORT = 14080;
  STR_PING = '{"numbers":[1],"strings":[]}';
  STR_PONG = '{"numbers":[2],"strings":[]}';
  STR_LOGIN1 = 'http://polfan.pl?cg=p';
  STR_LOGIN2 = 'nlst=1&nnum=1&jlmsg=true&ignprv=false';

procedure Register;
begin
  {$I polfan_icon.lrs}
  RegisterComponents('lNet',[TPolfan]);
end;

function SColorToHtmlColor(Color: TColor): string;
var
  N: Longint;
begin
  if Color=clNone then
    begin Result:= ''; exit end;
  N:= ColorToRGB(Color);
  Result:= '#'+
    IntToHex(Red(N), 2)+
    IntToHex(Green(N), 2)+
    IntToHex(Blue(N), 2);
end;

function SHtmlColorToColor(s: string; out Len: integer; Default: TColor): TColor;
var
  N1, N2, N3: integer;
  i: integer;
begin
  Result:= Default;
  Len:= 0;
  if (s<>'') and (s[1]='#') then Delete(s, 1, 1);
  if (s='') then exit;

  //delete after first nonword char
  i:= 1;
  while (i<=Length(s)) and IsCharWord(s[i]) do Inc(i);
  Delete(s, i, Maxint);

  //allow only #rgb, #rrggbb
  Len:= Length(s);
  if (Len<>3) and (Len<>6) then exit;

  for i:= 1 to Len do
    if not IsCharHex(s[i]) then exit;

  if Len=6 then
  begin
    N1:= StrToInt('$'+Copy(s, 1, 2));
    N2:= StrToInt('$'+Copy(s, 3, 2));
    N3:= StrToInt('$'+Copy(s, 5, 2));
  end
  else
  begin
    N1:= StrToInt('$'+s[1]+s[1]);
    N2:= StrToInt('$'+s[2]+s[2]);
    N3:= StrToInt('$'+s[3]+s[3]);
  end;

  Result:= RGBToColor(N1, N2, N3);
end;

{ TPolfanRooms }

constructor TPolfanRooms.Create;
begin
  FCount:=0;
  FCountRooms:=0;
  FCountUsers:=0;
end;

destructor TPolfanRooms.Destroy;
begin
  inherited Destroy;
end;

function TPolfanRooms.Add(AName, ADescription: string; AMode: TPolfanRoomMode
  ): integer;
var
  index: integer;
begin
  index:=GetIndex(AName);
  if index=-1 then
  begin
    inc(FCount);
    if AMode=rmRoom then inc(FCountRooms) else inc(FCountUsers);
    SetLength(FRooms,FCount);
    FRooms[FCount-1]:=TPolfanRoom.Create(AName,ADescription,AMode);
    index:=FCount-1;
  end;
  result:=index;
end;

procedure TPolfanRooms.Delete(AIndex: integer);
var
  i: integer;
begin
  if FRooms[AIndex].Mode=rmRoom then dec(FCountRooms) else dec(FCountUsers);
  FRooms[AIndex].Free;
  for i:=AIndex+1 to FCount-1 do FRooms[i-1]:=FRooms[i];
  dec(FCount);
  SetLength(FRooms,FCount);
end;

procedure TPolfanRooms.DeleteAll;
var
  i: integer;
begin
  for i:=FCount-1 downto 0 do Delete(i);
end;

function TPolfanRooms.GetIndex(AName: string): integer;
var
  i,a: integer;
begin
  a:=-1;
  for i:=0 to FCount-1 do if FRooms[i].Name=AName then
  begin
    a:=i;
    break;
  end;
  result:=a;
end;

function TPolfanRooms.GetIndex(AName: string; AMode: TPolfanRoomMode): integer;
var
  i,a: integer;
begin
  a:=-1;
  for i:=0 to FCount-1 do if (FRooms[i].Mode=AMode) and (FRooms[i].Name=AName) then
  begin
    a:=i;
    break;
  end;
  result:=a;
end;

function TPolfanRooms.GetIndexRoom(AName: string): integer;
begin
  result:=GetIndex(AName,rmRoom);
end;

function TPolfanRooms.GetIndexUser(AName: string): integer;
begin
  result:=GetIndex(AName,rmUser);
end;

{ TPolfanRoom }

function TPolfanRoom.GetIndex(AName: string): integer;
var
  a,i: integer;
begin
  a:=-1;
  for i:=0 to uzyt.Count-1 do if uzyt[i]=AName then
  begin
    a:=i;
    break;
  end;
  result:=a;
end;

constructor TPolfanRoom.Create(AName, ADescription: string;
  AMode: TPolfanRoomMode);
begin
  FName:=AName;
  FDescription:=ADescription;
  FMode:=AMode;
  FDocument:=TstringList.Create;
  if AMode=rmRoom then
  begin
    uzyt:=TstringList.Create;
    uzyt_opis:=TstringList.Create;
    uzyt_attr:=TstringList.Create;
  end;
end;

destructor TPolfanRoom.Destroy;
begin
  FDocument.Free;
  if FMode=rmRoom then
  begin
    uzyt.Free;
    uzyt_opis.Free;
    uzyt_attr.Free;
  end;
  inherited Destroy;
end;

procedure TPolfanRoom.Clear;
begin
  FDocument.Clear;
  if FMode=rmRoom then
  begin
    uzyt.Clear;
    uzyt_opis.Clear;
    uzyt_attr.Clear;
  end;
end;

procedure TPolfanRoom.Add(AText: string);
begin
  FDocument.Add(AText);
end;

procedure TPolfanRoom.AddUser(AName, ADescription: string; AAttr: integer);
begin
  uzyt.Add(AName);
  uzyt_opis.Add(ADescription);
  uzyt_attr.Add(IntToStr(AAttr));
end;

procedure TPolfanRoom.DelUser(AName: string);
var
  i,a: integer;
begin
  a:=GetIndex(AName);
  if a>-1 then
  begin
    uzyt.Delete(a);
    uzyt_opis.Delete(a);
    uzyt_attr.Delete(a);
  end;
end;

procedure TPolfanRoom.UpdateAttr(AName: string; AAttr: integer);
var
  a: integer;
begin
  a:=GetIndex(AName);
  uzyt_attr.Delete(a);
  uzyt_attr.Insert(a,IntToStr(AAttr));
end;

procedure TPolfanRoom.Truncate(ACount: integer);
begin
  while FDocument.Count>ACount do FDocument.Delete(0);
end;

{ TPolfan }

procedure TPolfan.WebOpen(ASender: TWebSocketCustomConnection);
var
  s,pom,ver: string;
  vOS: string;
  vAccepted: boolean;
begin
  vAccepted:=true;
  error_connected:=0;
  error_connected_str:='';
  if FProgVersion='' then ver:='' else ver:=' '+FProgVersion;
  sleep(500);
  if (FIdentify<>'<auto>') and (FIdentify<>'') then s:=FIdentify else
  begin
    case ArchInfo of
      osLinux86:   vOS:='Linux i386';
      osLinux64:   vOS:='Linux x86_64';
      osWindows86: vOS:='Windows 32bit';
      osWindows64: vOS:='Windows 64bit';
      osNone:      vOS:='_none_';
      else         vOS:='_none_';
    end;
    {$IFDEF WINDOWS}
    s:=WindowsOsInfo;
    if s<>'' then vOS:=s;
    {$ENDIF}
    s:=FProgName+ver+' ('+vOS+')';
  end;
  if Assigned(FOnBeforeConnect) then FOnBeforeConnect(self,FUser,FFingerPrint,vOS,vAccepted);
  if not vAccepted then
  begin
    if Assigned(FOnClose) then FOnClose(self,500,'Dostęp zabroniony!^Wszelkie skargi i zażalenia proszę kierować do: kontakt@polfan.pl');
    exit;
  end;
  FActive:=true;
  if FDevOn then src.Add('[Sent]: {"numbers": [1400], "strings":["'+FUser+'","*****","","'+FRoom+'","'+STR_LOGIN1+'","'+STR_LOGIN2+'","","'+s+'"]}');
  web.SendText('{"numbers": [1400], "strings":["'+FUser+'","'+FPassword+'","","'+FRoom+'","'+STR_LOGIN1+'","'+STR_LOGIN2+'","","'+s+'"]}');
  if Assigned(FOnOpen) then FOnOpen(self);
end;

procedure TPolfan.SetUserStatus(AValue: string);
begin
  if AValue='' then FUserStatus:='<Available>' else FUserStatus:=AValue;
end;

procedure TPolfan.SetIdentify(AValue: string);
begin
  if AValue='' then FIdentify:='<auto>' else FIdentify:=AValue;
end;

procedure TPolfan.WebClose(aSender: TWebSocketCustomConnection;
  aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
begin
  FActive:=false;
  document.Clear;
  kategorie_emotek.Clear;
  usun_wszystkie_pokoje;
  nick:='';
  if Assigned(FOnListUserDeInit) then FOnListUserDeInit(self);
  if Assigned(FOnClose) then FOnClose(self,error_connected,error_connected_str);
  if Assigned(FOnRoomClearRequest) then FOnRoomClearRequest(self);
end;

procedure TPolfan.WebRead(aSender: TWebSocketCustomConnection; aFinal, aRes1,
  aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
var
  s: string;
  c: TTestWebSocketClientConnection;
  a,kod: integer;
begin
  c:=TTestWebSocketClientConnection(aSender);
  //InfoMemo.Lines.Insert(0, Format('OnRead %d, final: %d, ext1: %d, ext2: %d, ext3: %d, type: %d, length: %d', [aSender.Index, ord(aFinal), ord(aRes1), ord(aRes2), ord(aRes3), aCode, aData.Size]));
  s:=ReadStrFromStream(c.ReadStream,min(c.ReadStream.size,10*1024));
  ReadFrame(s);
  if Assigned(FOnRead) then FOnRead(self,s);
end;

{$IFDEF WINDOWS}
function TPolfan.WindowsOsInfo: string;
var
  Release: string;
  Version: string;
begin
  Release := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
  Version := '';
  if Release = '1.1'  then Version := 'Windows 1.01';
  if Release = '1.2'  then Version := 'Windows 1.02';
  if Release = '1.3'  then Version := 'Windows 1.03';
  if Release = '1.4'  then Version := 'Windows 1.04';
  if Release = '2.3'  then Version := 'Windows 2.03';
  if Release = '2.10' then Version := 'Windows 2.10';
  if Release = '2.11' then Version := 'Windows 2.11';
  if Release = '3.0'  then Version := 'Windows 3.0';
  if Release = '3.10' then Version := 'Windows 3.1 or Windows NT 3.1';
  if Release = '3.11' then Version := 'Windows for Workgroups 3.11';
  if Release = '3.2'  then Version := 'Windows 3.2';
  if Release = '3.50' then Version := 'Windows NT 3.5';
  if Release = '3.51' then Version := 'Windows NT 3.51';
  if Release = '4.0'  then Version := 'Windows 95 or Windows NT 4.0';
  if Release = '4.10' then Version := 'Windows 98';
  if Release = '5.0'  then Version := 'Windows 2000';
  if Release = '4.90' then Version := 'Windows ME';
  if Release = '5.1'  then Version := 'Windows XP';
  if Release = '5.2'  then Version := 'Windows XP x86_64';
  if Release = '6.0'  then Version := 'Windows Vista';
  if Release = '6.1'  then Version := 'Windows 7';
  if Release = '6.2'  then Version := 'Windows 8';
  if Release = '6.3'  then Version := 'Windows 8.1';
  if Release = '10.0' then Version := 'Windows 10';
  result:=Version;
end;
{$ENDIF}

function TPolfan.ArchInfo: TPolfanArchOsInfoElements;
var
  s: string;
begin
  result:=osNone;
  {$IFDEF Windows}
  {$if defined(cpu64)}
  result:=osWindows64;
  {$else}
  result:=osWindows86;
  {$endif}
  {$ENDIF}
  {$if defined(cpu64) and defined(linux) }
  result:=osLinux64;
  {$ENDIF}
  {$if defined(cpu86) and defined(linux)}
  result:=osLinux86;
  {$ENDIF}
  {$IFDEF freebsd}
  {$if defined(cpu64)}
  result:=osFreeBSD64;
  {$else}
  result:=osFreeBSD86;
  {$endif}
  {$ENDIF}
end;

function TPolfan.IsExistImages(aPrive: string; var str: string): boolean;
var
  s,s1,new_filename: string;
  a: integer;
  b: boolean;
begin
  b:=false;
  s:=str;
  new_filename:='';
  s:=StringReplace(s,'\"','"',[rfReplaceAll]);
  while true do
  begin
    a:=pos('<img ',s);
    if a=0 then break;
    delete(s,1,a+4);
    a:=pos('src="',s);
    delete(s,1,a+4);
    a:=pos('"',s);
    s1:=copy(s,1,a-1);
    if Assigned(FOnImageFilename) then
    begin
      FOnImageFilename(self,s1,new_filename);
      if new_filename<>'' then str:=StringReplace(str,s1,new_filename,[]);
    end;
    if new_filename='' then
    begin
      if not FileExists(FBaseDirectory+_FF+s1) then
      begin
        FOnDownloadRequest(self,s1,aPrive);
        b:=true;
      end;
    end else begin
      if not FileExists(FBaseDirectory+_FF+new_filename) then
      begin
        FOnDownloadRequest(self,s1,aPrive);
        b:=true;
      end;
    end;
  end;
  result:=b;
end;

function TPolfan.ImagePlusOpis(str: string): string;
var
  s: string;
  aa,a,licznik,i: integer;
  tab: array of TIm;
begin
  licznik:=0;
  aa:=0;
  s:=StringReplace(str,'"',#9,[rfReplaceAll]);
  while true do
  begin
    a:=pos('<img src=',s);
    if a=0 then break;
    delete(s,1,a);
    inc(aa,a);
    a:=pos('alt=',s);
    delete(s,1,a);
    inc(aa,a);
    a:=pos(#9,s);
    delete(s,1,a);
    inc(aa,a);
    inc(licznik);
    SetLength(tab,licznik);
    tab[licznik-1].alt:=GetLineToStr(s,1,#9);
    a:=pos('>',s);
    tab[licznik-1].index:=aa+a+1;
  end;
  s:=str;
  for i:=licznik-1 downto 0 do insert('<font color=#808080 size=1>['+tab[i].alt+']</font>',s,tab[i].index);
  SetLength(tab,0);
  result:=s;
end;

procedure TPolfan.ReadColorsTable(ATable: string);
var
  pom,s,s1,s2: string;
  i,j: integer;
begin
  //color_user,color_op: string;
  //color_guest: array [1..15] of string;
  //color_user=#000000&color_op=#ff0000&color_guest=#ffffff #0000a6 #a62a2a #008000 #e68a00 #800080 #3366ff #8b4513 #ffcc00 #336699 #ff33ff #f9c000 #66cc66 #7f7f7f #000000 &password_protection=1&room_creation=1&server_version=100.0&pver=2.0.8
  i:=1;
  while true do
  begin
    pom:=trim(GetLineToStr(ATable,i,'&'));
    if pom='' then exit;
    s1:=trim(GetLineToStr(pom,1,'='));
    s2:=trim(GetLineToStr(pom,2,'='));
    if s1='color_user' then color_user:=s2 else
    if s1='color_op' then color_op:=s2 else
    if s1='color_guest' then
    begin
      color_guest_count:=0;
      SetLength(color_guest,color_guest_count);
      j:=1;
      while true do
      begin
        s:=trim(GetLineToStr(s2,j,' '));
        if s='' then break;
        inc(color_guest_count);
        SetLength(color_guest,color_guest_count);
        color_guest[color_guest_count-1]:=s;
        inc(j);
      end;
    end;
    inc(i);
  end;
end;

procedure TPolfan.ReadKategorieEmotek(AText: string);
var
  i: integer;
  s: string;
begin
  i:=1;
  while true do
  begin
    s:=GetLineToStr(AText,i,' ');
    if s='' then break;
    kategorie_emotek.Add(s);
    inc(i);
  end;
end;

procedure TPolfan.ReadFrame(aFrame: string);
var
  jData : TJSONData;
  jObject : TJSONObject;
  jArray : TJSONArray;
  s,sroom,opis,nadawca,adresat,pom,pom2: string;
  i,ii,ile,id: integer;
  kod,kod2,a,b: integer;
  wiadomosc,nadawca_wiadomosci,odbiorca_wiadomosci: string;
  u_nick,u_s: string;
  pom_ss1,pom_ss2,pom_ss3: TStrings;
  vDropNow,vNotLogSrc,do_mnie,sound_now: boolean;
begin
  vDropNow:=false;
  vNotLogSrc:=false;
  if Assigned(FOnBeforeReadFrame) then FOnBeforeReadFrame(self,AFrame,vDropNow,vNotLogSrc);
  if vDropNow then exit;
  if FDevOn and (not vNotLogSrc) then src.Add('[Received]: '+AFrame);
  sroom:='';
  do_mnie:=false;
  sound_now:=false;
  nadawca_wiadomosci:='';
  odbiorca_wiadomosci:='';
  jData:=GetJSON(aFrame);
  jObject:=TJSONObject(jData);
  jArray:=jObject.Arrays['numbers'];
  kod:=jArray[0].AsInteger;
  if jArray.Count>=2 then kod2:=jArray[1].AsInteger else kod2:=-1;
  s:='';

  if kod=610 then {Normalne pakiety rozmów}
  begin
    jArray:=jObject.Arrays['strings'];
    s:=jArray[0].AsString;
    sroom:=jArray[1].AsString;
    (* tu kod sprawdzający czy obrazek istnieje, a jak nie, to ściąga obrazek automatycznie *)
    if Assigned(FOnDownloadRequest) then if IsExistImages('',s) then StartDownloading(sroom);
    if FImgAltVisible then s:=ImagePlusOpis(s);
    u_nick:=upcase(nick);
    u_s:=upcase(s);
    (* testy na dalsze zdarzenia *)
    if Assigned(FOnSoundRequest) or FAutoResponse or ((FUserStatus<>'<Available>') and (FUserStatus<>'')) then
    begin
      do_mnie:=(pos('><B>'+u_nick+'</B></FONT>:',u_s)=0) and (pos('<FONT COLOR=RED>** PRZYCHODZI <B>'+u_nick+'</B>...</FONT>',u_s)=0)
               and (
                 (pos(' '+u_nick+' ',u_s)>0) or
                 (pos(' '+u_nick+'<',u_s)>0) or
                 (pos('>'+u_nick+' ',u_s)>0) or
                 (pos('>'+u_nick+'<',u_s)>0) or
                 (pos('>'+u_nick+'.',u_s)>0) or
                 (pos('>'+u_nick+',',u_s)>0) or
                 (pos('>'+u_nick+'!',u_s)>0) or
                 (pos(' '+u_nick+'.',u_s)>0) or
                 (pos(' '+u_nick+',',u_s)>0) or
                 (pos(' '+u_nick+'!',u_s)>0)
               );
      pom:=s;
      a:=pos('><B>',u_s)+4;
      b:=pos('</B></FONT>:',u_s);
      pom:=copy(s,a,b-a);
    end;
    (* powiadomienie dźwiękowe przy wiadomości nadchodzącej do ciebie *)
    //   '><b>Samusia</b></font>:';
    if Assigned(FOnSoundRequest) then if do_mnie and (pom<>'') then
    begin
      nadawca_wiadomosci:=pom;
      sound_now:=true;
    end;
    (* gdy autoresponse jest aktywne, każda wiadomość wędruje do analizy *)
    if do_mnie and FAutoResponse and (pom<>'') then if Assigned(FAutoResponseRequest) then
    begin
      pom2:=s;
      a:=pos('><b>'+pom+'</b></font>:',pom2);
      delete(pom2,1,a+15+length(pom));
      pom2:=StringReplace(pom2,'</font>','',[rfReplaceAll,rfIgnoreCase]);
      pom2:=StringReplace(pom2,'<font color=#','<#',[rfReplaceAll,rfIgnoreCase]);
      //<img src="img/piwosz.gif" alt="piwosz" />
      while true do begin
        a:=pos('<img src="',pom2);
        if a=0 then break;
        b:=pos('alt="',pom2);
        delete(pom2,a,b-a);
        pom2:=StringReplace(pom2,'alt="','<',[rfIgnoreCase]);
        pom2:=StringReplace(pom2,'" />','>',[rfIgnoreCase]);
      end;
      while true do begin
        a:=pos('&lt;',pom2);
        if a=0 then break;
        if a>0 then delete(pom2,a,4);
        insert('<',pom2,a);
      end;
      while true do begin
        a:=pos('&gt;',pom2);
        if a=0 then break;
        if a>0 then delete(pom2,a,4);
        insert('>',pom2,a);
      end;
      FAutoResponseRequest(self,pom,trim(pom2));
    end;
    (* jeśli nie mogę odpowiedzieć - odpowiedź automatyczna *)
    if (FUserStatus<>'<Available>') and (FUserStatus<>'') then
    begin
      if do_mnie and (time_status+60000<TimeToInteger) then
      begin
        time_status:=TimeToInteger;
        wiadomosc:=StringReplace(FUserStatus,'$',pom,[rfReplaceAll]);
        wiadomosc:=StringReplace(wiadomosc,'<#>','<'+SColorToHtmlColor(clGray)+'>',[rfReplaceAll]);
        web.SendText('{"numbers":[410],"strings":["<'+SColorToHtmlColor(clGray)+'>'+wiadomosc+'", "'+sroom+'"]}');
      end;
    end;
  end else

  if kod=611 then {Wiadomość prywatna}
  begin
    jArray:=jObject.Arrays['strings'];
    ile:=jArray.Count;
    s:=jArray[0].AsString; {wiadomość}
    nadawca:=jArray[1].AsString; {nadawca}
    if ile>=3 then adresat:=jArray[2].AsString else adresat:=''; {adresat}
    if (adresat='') or (adresat=nick) then sroom:=nadawca else
    if (nadawca='') or (nadawca=nick) then sroom:=adresat else
    sroom:='';
    if Assigned(FOnDownloadRequest) then if IsExistImages(sroom,s) then StartDownloading(sroom);
    if FImgAltVisible then s:=ImagePlusOpis(s);
    odbiorca_wiadomosci:=sroom;
    sound_now:=true;
  end else

  if kod=615 then {ZJAWIA SIĘ NOWY UŻYTKOWNIK}
  begin
    jArray:=jObject.Arrays['strings'];
    sroom:=jArray[1].AsString;
    id:=pokoje.GetIndex(sroom);
    pokoje.Rooms[id].AddUser(jArray[0].AsString,jArray[2].AsString,kod2);
    if Assigned(FOnListUserAdd) then FOnListUserAdd(self,sroom,jArray[0].AsString,jArray[2].AsString,kod2);
    if jArray[0].AsString=nick then if Assigned(FOnNewAttr) then FOnNewAttr(self,kod2);
  end else

  if kod=616 then {UŻYTKOWNIK ODCHODZI}
  begin
    jArray:=jObject.Arrays['strings'];
    sroom:=jArray[1].AsString;
    id:=pokoje.GetIndex(sroom);
    pokoje.Rooms[id].DelUser(jArray[0].AsString);
    if Assigned(FOnListUserDel) then FOnListUserDel(self,sroom,jArray[0].AsString);
  end else

  if kod=617 then {INNY UŻYTKOWNIK DOSTAJE AKTUALIZACJĘ UPRAWNIEŃ}
  begin
    jArray:=jObject.Arrays['strings'];
    sroom:=jArray[1].AsString;
    id:=pokoje.GetIndex(sroom);
    pokoje.Rooms[id].UpdateAttr(jArray[0].AsString,kod2);
    if Assigned(FOnUserAttr) then FOnUserAttr(self,sroom,jArray[0].AsString,kod2);
  end else

  if (kod=618) and (kod2=4) then {Jako kto się loguję}
  begin
    jArray:=jObject.Arrays['strings'];
    nick:=jArray[0].AsString;
  end else

  if kod=619 then {ZALOGOWANI LUDZIE}
  begin
    pom_ss1:=TStringList.Create;
    pom_ss2:=TStringList.Create;
    pom_ss3:=TStringList.Create;
    try
      jArray:=jObject.Arrays['numbers'];
      i:=5;
      ii:=jArray.Count;
      while i<ii do
      begin
        pom_ss3.Add(IntToStr(jArray[i].AsInteger));
        inc(i,2);
      end;
      jArray:=jObject.Arrays['strings'];
      sroom:=jArray[0].AsString;
      i:=1;
      ii:=jArray.Count;
      while i<ii do
      begin
        pom_ss1.Add(jArray[i].AsString);
        pom_ss2.Add(jArray[i+1].AsString);
        inc(i,2);
      end;
      id:=pokoje.GetIndex(sroom);
      pokoje.Rooms[id].Users.Assign(pom_ss1);
      pokoje.Rooms[id].UsersInfo.Assign(pom_ss2);
      pokoje.Rooms[id].UsersAttr.Assign(pom_ss3);
      if Assigned(FOnListUsersInit) then FOnListUsersInit(self,pom_ss1,pom_ss2,pom_ss3);
    finally
      pom_ss1.Free;
      pom_ss2.Free;
      pom_ss3.Free;
    end;
  end else

  if (kod=625) and (kod2=0) then {WŁAŚNIE WSZEDŁEŚ DO POKOJU - NAGŁÓWKI POKOJU}
  begin
    jArray:=jObject.Arrays['strings'];
    sroom:=jArray[0].AsString;
    opis:=jArray[1].AsString;
    pokoje.Add(sroom,opis,rmRoom);
    if Assigned(FOnRoomIn) then FOnRoomIn(self,sroom,opis);
  end else

  if kod=626 then {MAPY KOLORÓW i KATEGORIE EMOTKÓW}
  begin
    sroom:='';
    jArray:=jObject.Arrays['strings'];
    ReadColorsTable(jArray[0].AsString);
    if kategorie_emotek.Count=0 then ReadKategorieEmotek(jArray[1].AsString);
    if FPassword='' then nick:='~'+FUser else nick:=FUser; //Na start, potem i tak zostanie zaktualizowany
    if Assigned(FOnNewAttr) then FOnNewAttr(self,0); //Reset wszystkiego, jak będzie trzeba potem się odświeży
  end else

  if kod=630 then {INFORMACJE SERWERA DO UŻYTKOWNIKA}
  begin
    jArray:=jObject.Arrays['strings'];
    s:=jArray[0].AsString;
    sroom:=jArray[1].AsString;
    exit;
  end else

  if kod=631 then {OPUSZCZASZ POKÓJ}
  begin
    jArray:=jObject.Arrays['strings'];
    sroom:=jArray[1].AsString;
    id:=pokoje.GetIndex(sroom);
    pokoje.Delete(id);
    if Assigned(FOnRoomOut) then FOnRoomOut(self,sroom);
    if Assigned(FOnRoomClearRequest) then FOnRoomClearRequest(self);
    exit;
  end else

  if kod=65535 then {ZAMKNIJ POŁĄCZENIE}
  begin
    jArray:=jObject.Arrays['strings'];
    error_connected_str:=jArray[0].AsString;
    if error_connected_str<>'' then error_connected:=100;
    if web.Active then web.Close;
    exit;
  end;

  GoRefresh(s+'<br>',sroom,nadawca,adresat,nadawca_wiadomosci,odbiorca_wiadomosci,sound_now);
end;

procedure TPolfan.GoRefresh(aMessage: string; aName: string; aNadawca: string;
  aAdresat: string; aNadawcaWiadomosci: string; aOdbiorcaWiadomosci: string;
  aSoundNow: boolean);
var
  id: integer;
  vDropNow,vDelFromLogSrc: boolean;
begin
  vDropNow:=false;
  vDelFromLogSrc:=false;
  if aName<>'' then id:=pokoje.GetIndex(aName);
  (* ONLY REFRESH *)
  if aMessage='' then
  begin
    if aName='' then
    begin
      if Assigned(FOnReadDocument) then FOnReadDocument(self,aName,rmService,aMessage,document,true);
    end else begin
      if id=-1 then exit;
      if Assigned(FOnReadDocument) then FOnReadDocument(self,aName,pokoje.Rooms[id].Mode,aMessage,pokoje.Rooms[id].Document,true);
    end;
    exit;
  end;
  (* BEFORE ANALIZE *)
  if Assigned(FOnBeforeReadDocument) and (aName<>'') then FOnBeforeReadDocument(self,aName,aNadawca,aAdresat,rmUser,aMessage,vDropNow,vDelFromLogSrc);
  if vDropNow then
  begin
    if vDelFromLogSrc then src.Delete(src.Count-1);
    exit;
  end;
  (* NOWA WIADOMOŚĆ *)
  if aName='' then
  begin
    (* POKÓJ GŁÓWNY *)
    document.Add(aMessage);
    while document.Count>FMaxLines do document.Delete(0);
    if Assigned(FOnReadDocument) then FOnReadDocument(self,aName,rmService,aMessage,document,false);
  end else begin
    (* ROOM OR USER *)
    if id=-1 then id:=dodaj_pokoj(aName);
    pokoje.Rooms[id].Add(aMessage);
    if FMaxLines>0 then pokoje.Rooms[id].Truncate(FMaxLines);
    if Assigned(FOnReadDocument) then FOnReadDocument(self,aName,pokoje.Rooms[id].Mode,aMessage,pokoje.Rooms[id].Document,false);
    if aSoundNow and Assigned(FOnSoundRequest) then FOnSoundRequest(self,aNadawcaWiadomosci,aOdbiorcaWiadomosci);
  end;
end;

procedure TPolfan.StartDownloading(APrive: string);
begin
  if Assigned(FOnDownloadNow) then FOnDownloadNow(self,APrive);
end;

function TPolfan.indeks_pokoju(nazwa: string): integer;
begin
  result:=pokoje.GetIndex(nazwa);
end;

function TPolfan.dodaj_pokoj(nazwa: string): integer;
var
  id: integer;
begin
  id:=pokoje.GetIndex(nazwa);
  if id=-1 then
  begin
    (* dodaję pokój *)
    id:=pokoje.Add(nazwa,'',rmUser);
    if Assigned(FOnRoomAdd) then FOnRoomAdd(self,nazwa,send_zmiana_pokoju);
  end;
  send_zmiana_pokoju:=false;
  result:=id;
end;

procedure TPolfan.usun_pokoj(nazwa: string);
var
  id: integer;
begin
  id:=pokoje.GetIndex(nazwa);
  if id=-1 then exit;
  pokoje.Delete(id);
  if Assigned(FOnRoomDel) then FOnRoomDel(self,nazwa,id);
end;

procedure TPolfan.usun_wszystkie_pokoje;
var
  i: integer;
begin
  pokoje.DeleteAll;
  if Assigned(FOnRoomDelAll) then FOnRoomDelAll(self);
end;

constructor TPolfan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  pokoje:=TPolfanRooms.Create;
  document:=TStringList.Create;
  src:=TStringList.Create;
  src_dump:=false;
  kategorie_emotek:=TStringList.Create;
  web:=TNetSynWebSocket.Create(self);
  web.Host:=STR_HOST;
  web.PingText:=STR_PING;
  web.PongText:=STR_PONG;
  web.Port:=WORD_PORT;
  web.OnOpen:=@WebOpen;
  web.OnClose:=@WebClose;
  web.OnRead:=@WebRead;
  color_guest_count:=0;
  FDevOn:=false;
  FActive:=false;
  FUserStatus:='<Available>';
  FAutoResponse:=false;
  FIdentify:='<auto>';
  FMaxLines:=200;
  FImagesOFF:=false;
  FFingerPrint:='';
  FSilentMute:=false;
end;

destructor TPolfan.Destroy;
begin
  if web.Active then Disconnect;
  web.Free;
  document.Free;
  src.Free;
  kategorie_emotek.Free;
  usun_wszystkie_pokoje;
  pokoje.Free;
  inherited Destroy;
end;

procedure TPolfan.Connect;
begin
  if not web.Active then
  begin
    src_dump:=FDevOn;
    web.Open;
  end;
end;

procedure TPolfan.Disconnect;
begin
  if web.Active then
  begin
    if nick<>'' then
    begin
      if FDevOn then src.Add('[Sent]: {"numbers":[410],"strings":["/quit", ""]}');
      web.SendText('{"numbers":[410],"strings":["/quit", ""]}');
      sleep(500);
    end;
    web.Close;
  end;
end;

procedure TPolfan.InitUserStatus;
begin
  time_status:=0;
end;

function TPolfan.SendText(AText: string; AZmianaPokoju: boolean): boolean;
begin
  if FDevOn then src.Add('[Sent]: '+AText);
  if FSilentMute then
  begin
    result:=true;
    exit;
  end;
  send_zmiana_pokoju:=AZmianaPokoju;
  result:=web.SendText(AText);
end;

function TPolfan.SendTextIgnoreSilentMute(AText: string; AZmianaPokoju: boolean
  ): boolean;
begin
  if FDevOn then src.Add('[Sent]: '+AText);
  send_zmiana_pokoju:=AZmianaPokoju;
  result:=web.SendText(AText);
end;

procedure TPolfan.Refresh(AName: string);
begin
  GoRefresh('',AName);
end;

procedure TPolfan.RefreshUserList(AName: string);
var
  id: integer;
begin
  id:=pokoje.GetIndex(AName);
  if Assigned(FOnListUsersInit) then FOnListUsersInit(self,pokoje.Rooms[id].Users,pokoje.Rooms[id].UsersInfo,pokoje.Rooms[id].UsersAttr);
end;

function TPolfan.GetLoginNick: string;
begin
  result:=nick;
end;

procedure TPolfan.AddNewRoom(ARoom: string);
begin
  send_zmiana_pokoju:=true;
  dodaj_pokoj(ARoom);
end;

procedure TPolfan.DelRoom(ARoom: string);
begin
  usun_pokoj(ARoom);
end;

procedure TPolfan.AddSourceDump(AText: string);
begin
  if FDevOn then src.Add(AText);
end;

function TPolfan.GetSourceDump: TStrings;
begin
  result:=src;
end;

procedure TPolfan.SourceDumpClear;
begin
  src.Clear;
end;

procedure TPolfan.RoomClear;
begin
  document.Clear;
  if Assigned(FOnRoomClearRequest) then FOnRoomClearRequest(self);
end;

function TPolfan.IsRoom(AName: string): boolean;
var
  id: integer;
begin
  id:=pokoje.GetIndex(AName);
  result:=pokoje.Rooms[id].Mode=rmRoom;
end;

function TPolfan.IsUser(AName: string): boolean;
var
  id: integer;
begin
  id:=pokoje.GetIndex(AName);
  result:=pokoje.Rooms[id].Mode=rmUser;
end;

function TPolfan.IsRoomsCount: integer;
begin
  result:=pokoje.CountRooms;
end;

function TPolfan.GetColorUser(aAttr: integer): TColor;
var
  kolor,dlugosc: integer;
  kolory: array [0..15] of string = ('#000000','#9F0004','#990099','#FF00FF','#000066','#2079FF',
                                     '#00FFFF','#008080','#008000','#00FF00','#B5E61D','#FF6600',
                                     '#F9C000','#FFFF00','#7F7F7F','#000000');
begin
  kolor:=trunc(aAttr div 16);
  if (aAttr and 2) = 2 then result:=SHtmlColorToColor(color_op,dlugosc,clRed) //OP
  else if kolor>0 then result:=SHtmlColorToColor(kolory[kolor],dlugosc,clBlack) //GUEST
  else result:=SHtmlColorToColor(color_user,dlugosc,clBlack); //USER
end;

procedure TPolfan.AddDocument(aText: string; aRoom: string);
var
  id: integer;
begin
  if aRoom='' then
  begin
    document.Add(aText+'<br>');
    Refresh;
  end else begin
    id:=pokoje.GetIndex(aRoom);
    pokoje.Rooms[id].Add(aText+'<br>');
    Refresh(aRoom);
  end;
end;

end.
