unit Polfan;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  netsynwebsocket, WebSocket2;

type
  TPolfanPokoje = record
    nazwa: string;
    doc: TStringList;
    id: integer;
  end;

type
  TPolfanArchOsInfoElements = (osNone,osWindows86,osWindows64,osLinux86,osLinux64,osFreeBSD86,osFreeBSD64);
  TPolfanOnSoundRequest = procedure(Sender: TObject; aUser: string) of object;
  TPolfanOnRead = procedure(Sender: TObject; aRoom, aFrame: string) of object;
  TPolfanOnImageFilename = procedure(Sender: TObject; aFilename: string; var aNewFilename: string) of object;
  TPolfanOnDownloadRequest = procedure(Sender: TObject; AFilename, APrive: string) of object;
  TPolfanOnDownloadNow = procedure(Sender: TObject; APrive: string) of object;
  TPolfanOnUserToList = procedure(Sender: TObject; AUsername, ADescription: string; aAttr: integer) of object;
  TPolfanOnNewAttr = procedure(Sender: TObject; aAttr: integer) of object;
  TPolfanOnUserNewAttr = procedure(Sender: TObject; AUsername: string; aAttr: integer) of object;
  TPolfanOnUserDelFromList = procedure(Sender: TObject; AUsername: string) of object;
  TPolfanOnInitUserList = procedure(Sender: TObject; AUsers, ADescriptions, AAttributes: TStrings) of object;
  TPolfanOnReadDocument = procedure(Sender: TObject; AUser: string; AMessage: string; ADocument: TStrings; ARefresh: boolean) of object;
  TPolfanOnRoomBeginEnd = procedure(Sender: TObject; ARoom: string) of object;
  TPolfanOnRoomAdd = procedure(Sender: TObject; AUser: string; var AID: integer; AForceSetRoom: boolean) of object;
  TPolfanOnRoomDel = procedure(Sender: TObject; AUser: string; AID: integer) of object;

  { TPolfan }

  TPolfan = class(TComponent)
  private
    FActive: boolean;
    FBaseDirectory: string;
    FDevOn: boolean;
    FIdentify: string;
    FImagesOFF: boolean;
    FImgAltVisible: boolean;
    FMaxLines: integer;
    FOnClose: TNotifyEvent;
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
    FOnRoomIn: TPolfanOnRoomBeginEnd;
    FOnRoomOut: TPolfanOnRoomBeginEnd;
    FOnSoundRequest: TPolfanOnSoundRequest;
    FOnUserAttr: TPolfanOnUserNewAttr;
    FProgName,FProgVersion: string;
    FUser,FPassword,FRoom: string;
    FUserStatus: string;
    web: TNetSynWebSocket;
    color_user,color_op: string;
    color_guest: array of string;
    color_guest_count: integer;
    actual_room: string;
    pokoje: array of TPolfanPokoje;
    pokoje_count: integer;
    nick: string;
    time_status: integer;
    src_dump: boolean;
    src,document: TStrings;
    send_zmiana_pokoju: boolean;
    procedure SetIdentify(AValue: string);
    procedure SetUserStatus(AValue: string);
    procedure WebOpen(ASender: TWebSocketCustomConnection);
    procedure WebClose(aSender: TWebSocketCustomConnection;
      aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure WebRead(aSender: TWebSocketCustomConnection; aFinal, aRes1,
      aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    function ArchInfo: TPolfanArchOsInfoElements;
    function IsExistImages(aPrive: string; var str: string): boolean;
    function ImagePlusOpis(str: string): string;
    procedure ReadColorsTable(ATable: string);
    procedure ReadFrame(aFrame: string);
    procedure GoRefresh(aMessage: string = ''; aUser: string = '');
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
    procedure Refresh(AUser: string = '');
    function GetLoginNick: string;
    procedure AddNewRoom(ARoom: string);
    procedure DelRoom(ARoom: string);
    procedure AddSourceDump(AText: string);
    function GetSourceDump: TStrings;
    procedure SourceDumpClear;
    procedure RoomClear;
    function IsRoom: string;
    function GetColorUser(aAttr: integer): TColor;
    procedure AddDocument(aText: string);
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
    {Gdy włączone przed połączeniem,
    będzie można zdampować całą komunikację.}
    property DeveloperCodeOn: boolean read FDevOn write FDevOn default false;
    {Status użytkownika, jeśli niedostępny
     to wpisać tekst powiadomienia.
     Aktywność należy zainicjować przez:
        metodę: InitUserStatus;}
    property UserStatus: string read FUserStatus write SetUserStatus;
    {Wyłącza obsługę obrazków - emotek.}
    property ImagesOFF: boolean read FImagesOFF write FImagesOFF;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
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
    property OnRoomIn: TPolfanOnRoomBeginEnd read FOnRoomIn write FOnRoomIn;
    {Opuszczasz pokój.}
    property OnRoomOut: TPolfanOnRoomBeginEnd read FOnRoomOut write FOnRoomOut;
    property OnRoomClearRequest: TNotifyEvent read FOnRoomClearRequest write FOnRoomClearRequest;
    property OnReadDocument: TPolfanOnReadDocument read FOnReadDocument write FOnReadDocument;
    property OnRoomAdd: TPolfanOnRoomAdd read FOnRoomAdd write FOnRoomAdd;
    property OnRoomDel: TPolfanOnRoomDel read FOnRoomDel write FOnRoomDel;
    {Czyści listę użytkowników.}
    property OnRoomDelAll: TNotifyEvent read FOnRoomDelAll write FOnRoomDelAll;
  end;

procedure Register;
function SColorToHtmlColor(Color: TColor): string;
function SHtmlColorToColor(s: string; out Len: integer; Default: TColor): TColor;

implementation

uses
  synautil, math, fpjson, jsonparser, strutils;

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

function GetLineToStr(s:string;l:integer;separator:char;wynik:string=''):string;
var
  i,ll,dl: integer;
  b: boolean;
begin
  b:=false;
  dl:=length(s);
  ll:=1;
  s:=s+separator;
  for i:=1 to length(s) do
  begin
    if s[i]='"' then b:=not b;
    if (not b) and (s[i]=separator) then inc(ll);
    if ll=l then break;
  end;
  if ll=1 then dec(i);
  delete(s,1,i);
  b:=false;
  for i:=1 to length(s) do
  begin
    if s[i]='"' then b:=not b;
    if (not b) and (s[i]=separator) then break;
  end;
  delete(s,i,dl);
  if (s<>'') and (s[1]='"') then
  begin
    delete(s,1,1);
    delete(s,length(s),1);
  end;
  if s='' then s:=wynik;
  result:=s;
end;

function TimeToInteger(Hour,Minutes,Second,Milisecond: word): integer;
begin
  result:=(Hour*60*60*1000)+(Minutes*60*1000)+(Second*1000)+Milisecond;
end;

function TimeToInteger(Time: TDateTime): integer;
var
  godz,min,sec,milisec: word;
begin
  DecodeTime(Time,godz,min,sec,milisec);
  result:=(godz*60*60*1000)+(min*60*1000)+(sec*1000)+milisec;
end;

function TimeToInteger: integer;
begin
  result:=TimeToInteger(time);
end;

function IsCharWord(ch: char): boolean;
begin
  Result:= ch in ['a'..'z', 'A'..'Z', '_', '0'..'9'];
end;

function IsCharHex(ch: char): boolean;
begin
  Result:= ch in ['0'..'9', 'a'..'f', 'A'..'F'];
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

{ TPolfan }

procedure TPolfan.WebOpen(ASender: TWebSocketCustomConnection);
var
  s,ver: string;
begin
  FActive:=true;
  if FProgVersion='' then ver:='' else ver:=' '+FProgVersion;
  sleep(500);
  if (FIdentify<>'<auto>') and (FIdentify<>'') then s:=FIdentify else
  case ArchInfo of
    osLinux86:   s:=FProgName+ver+' (Linux 32bit)';
    osLinux64:   s:=FProgName+ver+' (Linux 64bit)';
    osWindows86: s:=FProgName+ver+' (Windows 32bit)';
    osWindows64: s:=FProgName+ver+' (Windows 64bit)';
    osNone:      s:=FProgName+ver;
    else         s:=FProgName+ver;
  end;
  actual_room:=FRoom;
  if FDevOn then src.Add('[Sent]: {"numbers": [1400], "strings":["'+FUser+'","*****","","'+actual_room+'","'+STR_LOGIN1+'","'+STR_LOGIN2+'","","'+s+'"]}');
  web.SendText('{"numbers": [1400], "strings":["'+FUser+'","'+FPassword+'","","'+actual_room+'","'+STR_LOGIN1+'","'+STR_LOGIN2+'","","'+s+'"]}');
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
  usun_wszystkie_pokoje;
  nick:='';
  if Assigned(FOnListUserDeInit) then FOnListUserDeInit(self);
  if Assigned(FOnClose) then FOnClose(self);
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
  if Assigned(FOnRead) then FOnRead(self,actual_room,s);
end;

function TPolfan.ArchInfo: TPolfanArchOsInfoElements;
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

procedure TPolfan.ReadFrame(aFrame: string);
var
  jData : TJSONData;
  jObject : TJSONObject;
  jArray : TJSONArray;
  s,nadawca,adresat,uzytkownik,pom: string;
  i,ii,ile: integer;
  kod,kod2,a,b: integer;
  wiadomosc: string;
  u_nick,u_s: string;
  pom_ss1,pom_ss2,pom_ss3: TStrings;
begin
  if FDevOn then src.Add('[Received]: '+AFrame);
  uzytkownik:='';
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
    (* tu kod sprawdzający czy obrazek istnieje, a jak nie, to ściąga obrazek automatycznie *)
    if Assigned(FOnDownloadRequest) then if IsExistImages('',s) then StartDownloading('');
    if FImgAltVisible then s:=ImagePlusOpis(s);
    u_nick:=upcase(nick);
    u_s:=upcase(s);
    (* powiadomienie dźwiękowe przy wiadomości nadchodzącej do ciebie *)
    //   '><b>Samusia</b></font>:';
    if Assigned(FOnSoundRequest) then if (pos('><B>'+u_nick+'</B></FONT>:',u_s)=0) and (pos('<FONT COLOR=RED>** PRZYCHODZI <B>'+u_nick+'</B>...</FONT>',u_s)=0)
    and ((pos(' '+u_nick+' ',u_s)>0) or (pos(' '+u_nick+'<',u_s)>0) or (pos('>'+u_nick+' ',u_s)>0) or (pos('>'+u_nick+'<',u_s)>0)) then
    begin
      pom:=s;
      a:=pos('><B>',u_s)+4;
      b:=pos('</B></FONT>:',u_s);
      pom:=copy(s,a,b-a);
      FOnSoundRequest(self,pom);
    end;
    (* jeśli nie mogę odpowiedzieć - odpowiedź automatyczna *)
    if (FUserStatus<>'<Available>') and (FUserStatus<>'') then
    begin
      if (pos('><B>'+u_nick+'</B></FONT>:',u_s)=0) and (pos('<FONT COLOR=RED>** PRZYCHODZI <B>'+u_nick+'</B>...</FONT>',u_s)=0)
        and ((pos(' '+u_nick+' ',u_s)>0) or (pos(' '+u_nick+'<',u_s)>0) or (pos('>'+u_nick+' ',u_s)>0) or (pos('>'+u_nick+'<',u_s)>0))
          and (time_status+60000<TimeToInteger)
            then
      begin
        time_status:=TimeToInteger;
        wiadomosc:=StringReplace(FUserStatus,'$',pom,[rfReplaceAll]);
        web.SendText('{"numbers":[410],"strings":["<'+SColorToHtmlColor(clGray)+'>'+wiadomosc+'", "'+actual_room+'"]}');
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
    if (adresat='') or (adresat=nick) then uzytkownik:=nadawca else
    if (nadawca='') or (nadawca=nick) then uzytkownik:=adresat else
    uzytkownik:='';
    if Assigned(FOnDownloadRequest) then if IsExistImages(uzytkownik,s) then StartDownloading(uzytkownik);
    if FImgAltVisible then s:=ImagePlusOpis(s);
    if nadawca<>nick then if Assigned(FOnSoundRequest) then FOnSoundRequest(self,'');
  end else

  if kod=615 then {ZJAWIA SIĘ NOWY UŻYTKOWNIK}
  begin
    jArray:=jObject.Arrays['strings'];
    if Assigned(FOnListUserAdd) then FOnListUserAdd(self,jArray[0].AsString,jArray[2].AsString,kod2);
    if jArray[0].AsString=nick then if Assigned(FOnNewAttr) then FOnNewAttr(self,kod2);
  end else

  if kod=616 then {UŻYTKOWNIK ODCHODZI}
  begin
    jArray:=jObject.Arrays['strings'];
    if Assigned(FOnListUserDel) then FOnListUserDel(self,jArray[0].AsString);
  end else

  if kod=617 then {INNY UŻYTKOWNIK DOSTAJE AKTUALIZACJĘ UPRAWNIEŃ}
  begin
    jArray:=jObject.Arrays['strings'];
    if Assigned(FOnUserAttr) then FOnUserAttr(self,jArray[0].AsString,kod2);
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
      i:=1;
      ii:=jArray.Count;
      while i<ii do
      begin
        pom_ss1.Add(jArray[i].AsString);
        pom_ss2.Add(jArray[i+1].AsString);
        inc(i,2);
      end;
      if Assigned(FOnListUsersInit) then FOnListUsersInit(self,pom_ss1,pom_ss2,pom_ss3);
    finally
      pom_ss1.Free;
      pom_ss2.Free;
      pom_ss3.Free;
    end;
  end else

  if kod=625 then {NAGŁÓWKI POKOJU}
  begin
    jArray:=jObject.Arrays['strings'];
    actual_room:=jArray[0].AsString;
    if Assigned(FOnRoomIn) then FOnRoomIn(self,actual_room);
  end else

  if kod=626 then {MAPY KOLORÓW}
  begin
    jArray:=jObject.Arrays['strings'];
    ReadColorsTable(jArray[0].AsString);
    if FPassword='' then nick:='~'+FUser else nick:=FUser; //Na start, potem i tak zostanie zaktualizowany
    if Assigned(FOnNewAttr) then FOnNewAttr(self,0); //Reset wszystkiego, jak będzie trzeba potem się odświeży
  end else

  if kod=630 then {INFORMACJE SERWERA DO UŻYTKOWNIKA - WAŻNE}
  begin
    jArray:=jObject.Arrays['strings'];
    s:=jArray[0].AsString;
  end else

  if kod=631 then {OPUSZCZASZ POKÓJ}
  begin
    jArray:=jObject.Arrays['strings'];
    actual_room:='';
    document.Clear;
    if Assigned(FOnRoomOut) then FOnRoomOut(self,jArray[1].AsString);
    if Assigned(FOnRoomClearRequest) then FOnRoomClearRequest(self);
    jData.Free;
    exit;
  end else

  if kod=65535 then {ZAMKNIJ POŁĄCZENIE}
    if web.Active then web.Close;

  GoRefresh(s+'<br>',uzytkownik);
end;

procedure TPolfan.GoRefresh(aMessage: string; aUser: string);
var
  nazwa: string;
  a,id: integer;
begin
  (* ONLY REFRESH *)
  if aMessage='' then
  begin
    if aUser='' then
    begin
      if Assigned(FOnReadDocument) then FOnReadDocument(self,aUser,aMessage,document,true);
    end else begin
      id:=indeks_pokoju(aUser);
      if id=-1 then exit;
      if Assigned(FOnReadDocument) then FOnReadDocument(self,aUser,aMessage,pokoje[id].doc,true);
    end;
    exit;
  end;
  (* NOWA WIADOMOŚĆ *)
  if aUser='' then
  begin
    (* POKÓJ GŁÓWNY *)
    document.Add(aMessage);
    while document.Count>FMaxLines do document.Delete(0);
    if Assigned(FOnReadDocument) then FOnReadDocument(self,aUser,aMessage,document,false);
  end else begin
    (* PRIVE *)
    id:=indeks_pokoju(aUser);
    if id=-1 then id:=dodaj_pokoj(aUser);
    pokoje[id].doc.Add(aMessage);
    while pokoje[id].doc.Count>200 do pokoje[id].doc.Delete(0);
    if Assigned(FOnReadDocument) then FOnReadDocument(self,aUser,aMessage,pokoje[id].doc,false);
  end;
end;

procedure TPolfan.StartDownloading(APrive: string);
begin
  if Assigned(FOnDownloadNow) then FOnDownloadNow(self,APrive);
end;

function TPolfan.indeks_pokoju(nazwa: string): integer;
var
  i,a: integer;
begin
  a:=-1;
  for i:=0 to pokoje_count-1 do if pokoje[i].nazwa=nazwa then
  begin
    a:=i;
    break;
  end;
  result:=a;
end;

function TPolfan.dodaj_pokoj(nazwa: string): integer;
var
  id,id_pokoju: integer;
begin
  id:=indeks_pokoju(nazwa);
  if id=-1 then
  begin
    (* dodaję pokój *)
    inc(pokoje_count);
    SetLength(pokoje,pokoje_count);
    id:=pokoje_count-1;
    pokoje[id].nazwa:=nazwa;
    pokoje[id].doc:=TStringList.Create;
    if Assigned(FOnRoomAdd) then FOnRoomAdd(self,nazwa,id_pokoju,send_zmiana_pokoju);
    pokoje[id].id:=id_pokoju;
  end;
  send_zmiana_pokoju:=false;
  result:=id;
end;

procedure TPolfan.usun_pokoj(nazwa: string);
var
  id,id_pokoju,i: integer;
begin
  id:=indeks_pokoju(nazwa);
  if id=-1 then exit;
  id_pokoju:=pokoje[id].id;
  pokoje[id].doc.Free;
  for i:=id+1 to pokoje_count-1 do pokoje[i-1]:=pokoje[i];
  dec(pokoje_count);
  SetLength(pokoje,pokoje_count);
  if Assigned(FOnRoomDel) then FOnRoomDel(self,nazwa,id_pokoju);
end;

procedure TPolfan.usun_wszystkie_pokoje;
var
  i: integer;
begin
  if pokoje_count=0 then exit;
  for i:=0 to pokoje_count-1 do pokoje[i].doc.Free;
  pokoje_count:=0;
  SetLength(pokoje,pokoje_count);
  if Assigned(FOnRoomDelAll) then FOnRoomDelAll(self);
end;

constructor TPolfan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  document:=TStringList.Create;
  src:=TStringList.Create;
  src_dump:=false;
  web:=TNetSynWebSocket.Create(self);
  web.Host:=STR_HOST;
  web.PingText:=STR_PING;
  web.PongText:=STR_PONG;
  web.Port:=WORD_PORT;
  web.OnOpen:=@WebOpen;
  web.OnClose:=@WebClose;
  web.OnRead:=@WebRead;
  pokoje_count:=0;
  color_guest_count:=0;
  FDevOn:=false;
  FActive:=false;
  FUserStatus:='<Available>';
  FIdentify:='<auto>';
  FMaxLines:=200;
  FImagesOFF:=false;
end;

destructor TPolfan.Destroy;
begin
  if web.Active then Disconnect;
  web.Free;
  document.Free;
  src.Free;
  usun_wszystkie_pokoje;
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
  send_zmiana_pokoju:=AZmianaPokoju;
  result:=web.SendText(AText);
end;

procedure TPolfan.Refresh(AUser: string);
begin
  GoRefresh('',AUser);
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

function TPolfan.IsRoom: string;
begin
  result:=actual_room;
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

procedure TPolfan.AddDocument(aText: string);
begin
  document.Add(aText+'<br>');
  Refresh;
end;

end.
