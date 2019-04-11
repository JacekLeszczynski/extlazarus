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
  TPolfanOnRead = procedure(Sender: TObject; aRoom, aFrame: string) of object;
  TPolfanOnDownloadRequest = procedure(Sender: TObject; AFilename: string) of object;
  TPolfanOnUserToList = procedure(Sender: TObject; AUsername, ADescription: string; aAttr: integer) of object;
  TPolfanOnUserDelFromList = procedure(Sender: TObject; AUsername: string) of object;
  TPolfanOnInitUserList = procedure(Sender: TObject; AUsers, ADescriptions, AAttributes: TStrings) of object;
  TPolfanOnReadDocument = procedure(Sender: TObject; AUser: string; AMessage: string; ADocument: TStrings; ARefresh: boolean) of object;
  TPolfanOnRoomAdd = procedure(Sender: TObject; AUser: string; var AID: integer; AForceSetRoom: boolean) of object;
  TPolfanOnRoomDel = procedure(Sender: TObject; AUser: string; AID: integer) of object;

  { TPolfan }

  TPolfan = class(TComponent)
  private
    FActive: boolean;
    FBaseDirectory: string;
    FImgAltVisible: boolean;
    FMaxLines: integer;
    FOnClose: TNotifyEvent;
    FOnDownloadNow: TNotifyEvent;
    FOnDownloadRequest: TPolfanOnDownloadRequest;
    FOnListUserAdd: TPolfanOnUserToList;
    FOnListUserDeInit: TNotifyEvent;
    FOnListUserDel: TPolfanOnUserDelFromList;
    FOnListUsersInit: TPolfanOnInitUserList;
    FOnOpen: TNotifyEvent;
    FOnRead: TPolfanOnRead;
    FOnReadDocument: TPolfanOnReadDocument;
    FOnRoomAdd: TPolfanOnRoomAdd;
    FOnRoomDel: TPolfanOnRoomDel;
    FOnRoomDelAll: TNotifyEvent;
    FOnSoundRequest: TNotifyEvent;
    FProgName,FProgVersion: string;
    FUser,FPassword,FRoom: string;
    FUserStatus: string;
    web: TNetSynWebSocket;
    pokoje: array of TPolfanPokoje;
    pokoje_count: integer;
    nick: string;
    time_status: integer;
    document: TStrings;
    send_zmiana_pokoju: boolean;
    procedure SetUserStatus(AValue: string);
    procedure WebOpen(ASender: TWebSocketCustomConnection);
    procedure WebClose(aSender: TWebSocketCustomConnection;
      aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure WebRead(aSender: TWebSocketCustomConnection; aFinal, aRes1,
      aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    function ArchInfo: TPolfanArchOsInfoElements;
    function IsExistImages(str: string): boolean;
    function ImagePlusOpis(str: string): string;
    procedure ReadFrame(aFrame: string);
    procedure GoRefresh(aMessage: string = ''; aUser: string = '');
    procedure StartDownloading;
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
  published
    property Active: boolean read FActive;
    property ProgName: string read FProgName write FProgName;
    property ProgVersion: string read FProgVersion write FProgVersion;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property Room: string read FRoom write FRoom;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
    property ImgAltVisible: boolean read FImgAltVisible write FImgAltVisible;
    property MaxLines: integer read FMaxLines write FMaxLines default 200;
    {Status użytkownika, jeśli niedostępny
     to wpisać tekst powiadomienia.
     Aktywność należy zainicjować przez:
        metodę: InitUserStatus;}
    property UserStatus: string read FUserStatus write SetUserStatus;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    {Została odebrana ramka tekstu.}
    property OnRead: TPolfanOnRead read FOnRead write FOnRead;
    {Polecenie odebrania powiadomienia
     dźwiękowego.}
    property OnSoundRequest: TNotifyEvent read FOnSoundRequest write FOnSoundRequest;
    {Zapamiętaj brakujące pliki.}
    property OnDownloadRequest: TPolfanOnDownloadRequest read FOnDownloadRequest write FOnDownloadRequest;
    {Ściągnij teraz te pliki.}
    property OnDownloadNow: TNotifyEvent read FOnDownloadNow write FOnDownloadNow;
    {Zjawia się nowy użytkownik do pokoju.}
    property OnListUserAdd: TPolfanOnUserToList read FOnListUserAdd write FOnListUserAdd;
    {Odchodzi użytkownik z pokoju.}
    property OnListUserDel: TPolfanOnUserDelFromList read FOnListUserDel write FOnListUserDel;
    {Inicjuję początkową listę użytkowników.}
    property OnListUserInit: TPolfanOnInitUserList read FOnListUsersInit write FOnListUsersInit;
    {Usuwa wszystkich użytkowników.}
    property OnListUserDeInit: TNotifyEvent read FOnListUserDeInit write FOnListUserDeInit;
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
  synautil, math, fpjson, jsonparser;

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
  case ArchInfo of
    osLinux86:   s:=FProgName+ver+' (Linux 32bit)';
    osLinux64:   s:=FProgName+ver+' (Linux 64bit)';
    osWindows86: s:=FProgName+ver+' (Windows 32bit)';
    osWindows64: s:=FProgName+ver+' (Windows 64bit)';
    osNone:      s:=FProgName+ver;
    else         s:=FProgName+ver;
  end;
  web.SendText('{"numbers": [1400], "strings":["'+FUser+'","'+FPassword+'","","'+FRoom+'","'+STR_LOGIN1+'","'+STR_LOGIN2+'","","'+s+'"]}');
  if Assigned(FOnOpen) then FOnOpen(self);
end;

procedure TPolfan.SetUserStatus(AValue: string);
begin
  if AValue='' then FUserStatus:='<Available>' else FUserStatus:=AValue;
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
  if Assigned(FOnRead) then FOnRead(self,FRoom,s);
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

function TPolfan.IsExistImages(str: string): boolean;
var
  s,s1: string;
  a: integer;
  b: boolean;
begin
  b:=false;
  s:=str;
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
    if not FileExists(FBaseDirectory+_FF+s1) then
    begin
      FOnDownloadRequest(self,s1);
      b:=true;
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

procedure TPolfan.ReadFrame(aFrame: string);
var
  jData : TJSONData;
  jObject : TJSONObject;
  jArray : TJSONArray;
  s,nadawca,adresat,uzytkownik: string;
  i,ii,ile: integer;
  kod,kod2: integer;
  wiadomosc: string;
  u_nick,u_s: string;
  pom_ss1,pom_ss2,pom_ss3: TStrings;
begin
  uzytkownik:='';
  jData:=GetJSON(aFrame);
  jObject:=TJSONObject(jData);
  jArray:=jObject.Arrays['numbers'];
  kod:=jArray[0].AsInteger;
  if jArray.Count=2 then kod2:=jArray[1].AsInteger else kod2:=-1;
  s:='';

  if kod=619 then writeln(aFrame);

  if kod=610 then {Normalne pakiety rozmów}
  begin
    jArray:=jObject.Arrays['strings'];
    s:=jArray[0].AsString;
    (* tu kod sprawdzający czy obrazek istnieje, a jak nie, to ściąga obrazek automatycznie *)
    if Assigned(FOnDownloadRequest) then if IsExistImages(s) then StartDownloading;
    if FImgAltVisible then s:=ImagePlusOpis(s);
    u_nick:=upcase(nick);
    u_s:=upcase(s);
    (* powiadomienie dźwiękowe przy wiadomości nadchodzącej do ciebie *)
    if Assigned(FOnSoundRequest) then if (pos('><B>'+u_nick+'</B></FONT>:',u_s)=0) and (pos('<FONT COLOR=RED>** PRZYCHODZI <B>'+u_nick+'</B>...</FONT>',u_s)=0)
    and ((pos(' '+u_nick+' ',u_s)>0) or (pos(' '+u_nick+'<',u_s)>0) or (pos('>'+u_nick+' ',u_s)>0) or (pos('>'+u_nick+'<',u_s)>0)) then FOnSoundRequest(self);
    (* jeśli nie mogę odpowiedzieć - odpowiedź automatyczna *)
    if (FUserStatus<>'<Available>') and (FUserStatus<>'') then
    begin
      if (pos('><B>'+u_nick+'</B></FONT>:',u_s)=0) and (pos('<FONT COLOR=RED>** PRZYCHODZI <B>'+u_nick+'</B>...</FONT>',u_s)=0)
        and ((pos(' '+u_nick+' ',u_s)>0) or (pos(' '+u_nick+'<',u_s)>0) or (pos('>'+u_nick+' ',u_s)>0) or (pos('>'+u_nick+'<',u_s)>0))
          and (time_status+60000<TimeToInteger)
            then
      begin
        time_status:=TimeToInteger;
        wiadomosc:='<u>Info:</u> <i>'+FUserStatus;
        web.SendText('{"numbers":[410],"strings":["<'+SColorToHtmlColor(clGray)+'>'+wiadomosc+'", "'+FRoom+'"]}');
      end;
    end;
  end else

  if kod=611 then {Wiadomość prywatna}
  begin
    jArray:=jObject.Arrays['strings'];
    ile:=jArray.Count;
    s:=jArray[0].AsString; {wiadomość}
    nadawca:=jArray[1].AsString; {nadawca}
    if 2<=ile-1 then adresat:=jArray[2].AsString; {adresat}
    if (adresat='') or (adresat=nick) then uzytkownik:=nadawca else
    if (nadawca='') or (nadawca=nick) then uzytkownik:=adresat else
    uzytkownik:='';
    if Assigned(FOnDownloadRequest) then if IsExistImages(s) then StartDownloading;
    if FImgAltVisible then s:=ImagePlusOpis(s);
    if nadawca<>nick then if Assigned(FOnSoundRequest) then FOnSoundRequest(self);
  end else

  if kod=615 then {ZJAWIA SIĘ NOWY UŻYTKOWNIK}
  begin
    jArray:=jObject.Arrays['strings'];
    if Assigned(FOnListUserAdd) then FOnListUserAdd(self,jArray[0].AsString,jArray[2].AsString,kod2);
  end else

  if kod=616 then {ZNIKA SIĘ NOWY UŻYTKOWNIK}
  begin
    jArray:=jObject.Arrays['strings'];
    if Assigned(FOnListUserDel) then FOnListUserDel(self,jArray[0].AsString);
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
    s:=jArray[0].AsString;
  end else

  if kod=630 then {INFORMACJE SERWERA DO UŻYTKOWNIKA - WAŻNE}
  begin
    jArray:=jObject.Arrays['strings'];
    s:=jArray[0].AsString;
  end else

  if kod=65535 then {ZAMKNIJ POŁĄCZENIE}
    if web.Active then web.Close;

  GoRefresh(s+'<br>',uzytkownik);
  jData.Free;
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

procedure TPolfan.StartDownloading;
begin
  if Assigned(FOnDownloadNow) then FOnDownloadNow(self);
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
  web:=TNetSynWebSocket.Create(self);
  web.Host:=STR_HOST;
  web.PingText:=STR_PING;
  web.PongText:=STR_PONG;
  web.Port:=WORD_PORT;
  web.OnOpen:=@WebOpen;
  web.OnClose:=@WebClose;
  web.OnRead:=@WebRead;
  pokoje_count:=0;
  FActive:=false;
  FUserStatus:='<Available>';
  FMaxLines:=200;
end;

destructor TPolfan.Destroy;
begin
  if web.Active then Disconnect;
  web.Free;
  document.Free;
  usun_wszystkie_pokoje;
  inherited Destroy;
end;

procedure TPolfan.Connect;
begin
  if not web.Active then web.Open;
end;

procedure TPolfan.Disconnect;
begin
  if web.Active then
  begin
    if nick<>'' then
    begin
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

end.
