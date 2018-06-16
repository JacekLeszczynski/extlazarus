unit ZEncodedDBConf;

{
  Komponent zapamiętuje w formie zaszyfrowanej dane do połączenia się. Potrafi
  zapisywać i odczytywać żądane dane. Może również wprowadzać je bezpośrednio
  do komponentu TZConnection z ZeosDBE.

  Autor: Jacek Leszczyński
  E-Mail: tao@bialan.pl

  Wszelkie prawa zastrzeżone (C) Białystok 2011, Polska.


  Instrukcja obsługi:
  1. Ustawiamy właściwości danych do połączenia się z tabelą, lub wczytujemy te wartości
     z pliku wywołując metodę: LOADTOFILE(FILENAME:STRING);
  2. Odczytujemy żądane właściwości, lub zapisujemy je do pliku wykonując metodę:
     SAVETOFILE(FILENAME:STRING);
  3. Gdy mamy ustawione wszyskie właściwości wymagane do połączenia się z bazą danych,
     a właściwość DB_CONNECTION wskazuje na TCONNECTION, metoda CONFTODB przepisze
     całą zawartość potrzebnych wartości do TCONNECTION.
  UWAGA: Używany plik z danymi jest szyfrowany z wykorzystaniem klucza, który możemy
         ustawić we właściwości TOKEN.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  DCPdes, DCPsha1, ZConnection;

type

  TZAlgorithmEncoder = (aeDES, aeDES3);
  TZ_Protocol = (prDefault, prMssql, prFreeTDS, prMySql, prMariaDB5, prPostgresql, prInterbase, prFirebird10, prFirebird15, prFirebird20, prFirebird21, prFirebird25);

  { TZEncodedDBConf }

  TZEncodedDBConf = class(TComponent)
  private
    { Private declarations }
    Des: TDCP_des;
    Des3: TDCP_3des;
    FAEnc: TZAlgorithmEncoder;
    FHost: ShortString;
    FDatabase: ShortString;
    FLogin: ShortString;
    FPassword: ShortString;
    FPort: Integer;
    FProto: TZ_Protocol;
    FToken: String;
    FDB: TZConnection;
    Fzmienna_01: String;
    Fzmienna_02: String;
    Fzmienna_03: String;
    Fzmienna_04: String;
    Fzmienna_05: String;
    Fzmienna_06: String;
    Fzmienna_07: String;
    Fzmienna_08: String;
    Fzmienna_09: String;
    Fzmienna_10: String;
    function GetRandom(q: integer): string;
    function GetLineToStr(s:string;l:integer;separator:char):string;
    function EncryptStr(s:string):string;
    function DecryptStr(s:string):string;
    function ProtoToStr:string;
    function StrToProto(s:string):TZ_Protocol;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(FileName: String);
    function LoadFromFile(FileName: String):boolean;
    procedure SaveToRegistry(RootKey: THandle; vKey, vName: string; InLazServiceRegistry: boolean = false);
    function LoadFromRegistry(RootKey: THandle; vKey, vName: string; InLazServiceRegistry: boolean = false):boolean;
    procedure ConfToDB;
  published
    { Published declarations }
    property Host: ShortString read FHost write FHost;
    property Database: ShortString read FDatabase write FDatabase;
    property Login: ShortString read FLogin write FLogin;
    property Password: ShortString read FPassword write FPassword;
    property Port: Integer read FPort write FPort default 0;
    property Token: String read FToken write FToken;
    property AlgorithmEncoder: TZAlgorithmEncoder read FAEnc write FAEnc default aeDES;
    property DB_Connection: TZConnection read FDB write FDB;
    property Zmienna_01: String read FZmienna_01 write FZmienna_01;
    property Zmienna_02: String read FZmienna_02 write FZmienna_02;
    property Zmienna_03: String read FZmienna_03 write FZmienna_03;
    property Zmienna_04: String read FZmienna_04 write FZmienna_04;
    property Zmienna_05: String read FZmienna_05 write FZmienna_05;
    property Zmienna_06: String read FZmienna_06 write FZmienna_06;
    property Zmienna_07: String read FZmienna_07 write FZmienna_07;
    property Zmienna_08: String read FZmienna_08 write FZmienna_08;
    property Zmienna_09: String read FZmienna_09 write FZmienna_09;
    property Zmienna_10: String read FZmienna_10 write FZmienna_10;
    property Protocol: TZ_Protocol read FProto write FProto default prDefault;
  end;

procedure Register;

implementation

uses
  Registry, Sockets;

const
  MAX_BUFOR = 500;

procedure Register;
begin
  {$I zencodeddbconf_icon.lrs}
  RegisterComponents('Zeos Access',[TZEncodedDBConf]);
end;

{ TEncodedDBConf }

function TZEncodedDBConf.GetRandom(q: integer): string;
var
  s: string;
  i: integer;
begin
  if q=0 then q:=random(11)+5;
  s:='';
  for i:=1 to q do s:=s+Chr(random(90)+33);
  result:=s;
end;

function TZEncodedDBConf.GetLineToStr(s: string; l: integer; separator: char
  ): string;
var
  i,ll,dl: integer;
begin
  dl:=length(s);
  ll:=1;
  s:=s+separator;
  for i:=1 to length(s) do
  begin
    if s[i]=separator then inc(ll);
    if ll=l then break;
  end;
  if ll=1 then dec(i);
  delete(s,1,i);
  for i:=1 to length(s) do if s[i]=separator then break;
  delete(s,i,dl);
  result:=s;
end;

function TZEncodedDBConf.EncryptStr(s: string): string;
var
  pom: string;
begin
  case FAEnc of
    aeDES:  begin
              Des.InitStr(FToken,TDCP_sha1);
              pom:=Des.EncryptString(s);
              Des.Burn;
            end;
    aeDES3: begin
              Des3.InitStr(FToken,TDCP_sha1);
              pom:=Des3.EncryptString(s);
              Des3.Burn;
            end;
  end;
  result:=pom;
end;

function TZEncodedDBConf.DecryptStr(s: string): string;
var
  pom: string;
begin
  case FAEnc of
    aeDES:  begin
              Des.InitStr(FToken,TDCP_sha1);
              pom:=Des.DecryptString(s);
              Des.Burn;
            end;
    aeDES3: begin
              Des3.InitStr(FToken,TDCP_sha1);
              pom:=Des3.DecryptString(s);
              Des3.Burn;
            end;
  end;
  result:=pom;
end;

function TZEncodedDBConf.ProtoToStr: string;
var
  s: string;
begin
  case FProto of
    prDefault:    s:='';
    prMssql:      s:='mssql';
    prMariaDB5:   s:='MariaDB-5';
    prFreeTDS:    s:='FreeTDS_MsSQL>=2005';
    prMySql:      s:='mysql';
    prPostgresql: s:='postgresql';
    prInterbase:  s:='interbase-6';
    prFirebird10: s:='firebird-1.0';
    prFirebird15: s:='firebird-1.5';
    prFirebird20: s:='firebird-2.0';
    prFirebird21: s:='firebird-2.1';
    prFirebird25: s:='firebird-2.5';
  end;
  result:=s;
end;

function TZEncodedDBConf.StrToProto(s: string): TZ_Protocol;
begin
  if s='mssql' then result:=prMssql else
  if s='FreeTDS_MsSQL>=2005' then result:=prFreeTDS else
  if s='mysql' then result:=prMySql else
  if s='MariaDB-5' then result:=prMariaDB5 else
  if s='postgresql' then result:=prPostgresql else
  if s='interbase-6' then result:=prInterbase else
  if s='firebird-1.0' then result:=prFirebird10 else
  if s='firebird-1.5' then result:=prFirebird15 else
  if s='firebird-2.0' then result:=prFirebird20 else
  if s='firebird-2.1' then result:=prFirebird21 else
  if s='firebird-2.5' then result:=prFirebird25 else result:=prDefault;
end;

constructor TZEncodedDBConf.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Des:=TDCP_des.Create(Self);
  Des3:=TDCP_3des.Create(Self);
  FAEnc:=aeDes;
  FProto:=prDefault;
end;

destructor TZEncodedDBConf.Destroy;
begin
  Des3.Free;
  Des.Free;
  inherited Destroy;
end;

procedure TZEncodedDBConf.SaveToFile(FileName: String);
var
  f: textfile;
  s: string;
begin
  Randomize;
  assignfile(f,Filename);
  s:=GetRandom(0)+'~'+FHost+'~'+GetRandom(0)+'~'+IntToStr(FPort)+'~'+GetRandom(0)+'~'+FDatabase+'~'+GetRandom(0)+'~'+FLogin+'~'+GetRandom(0)+'~'+FPassword+'~'+GetRandom(0)+
                  '~'+FZmienna_01+'~'+GetRandom(0)+
                  '~'+FZmienna_02+'~'+GetRandom(0)+
                  '~'+FZmienna_03+'~'+GetRandom(0)+
                  '~'+FZmienna_04+'~'+GetRandom(0)+
                  '~'+FZmienna_05+'~'+GetRandom(0)+
                  '~'+FZmienna_06+'~'+GetRandom(0)+
                  '~'+FZmienna_07+'~'+GetRandom(0)+
                  '~'+FZmienna_08+'~'+GetRandom(0)+
                  '~'+FZmienna_09+'~'+GetRandom(0)+
                  '~'+FZmienna_10+'~'+GetRandom(0)+
                  '~'+ProtoToStr+'~'+GetRandom(0);
  s:=EncryptStr(s);
  rewrite(f);
  writeln(f,s);
  closefile(f);
end;

function TZEncodedDBConf.LoadFromFile(FileName: String): boolean;
var
  f: textfile;
  s: string;
begin
  if not FileExists(FileName) then
  begin
    result:=false;
    Exit;
  end;
  assignfile(f,FileName);
  reset(f);
  readln(f,s);
  s:=DecryptStr(s);
  closefile(f);
  FHost:=GetLineToStr(s,2,'~');
  try FPort:=StrToInt(GetLineToStr(s,4,'~')) except FPort:=0 end;
  FDatabase:=GetLineToStr(s,6,'~');
  FLogin:=GetLineToStr(s,8,'~');
  FPassword:=GetLineToStr(s,10,'~');
  FZmienna_01:=GetLineToStr(s,12,'~');
  FZmienna_02:=GetLineToStr(s,14,'~');
  FZmienna_03:=GetLineToStr(s,16,'~');
  FZmienna_04:=GetLineToStr(s,18,'~');
  FZmienna_05:=GetLineToStr(s,20,'~');
  FZmienna_06:=GetLineToStr(s,22,'~');
  FZmienna_07:=GetLineToStr(s,24,'~');
  FZmienna_08:=GetLineToStr(s,26,'~');
  FZmienna_09:=GetLineToStr(s,28,'~');
  FZmienna_10:=GetLineToStr(s,30,'~');
  FProto:=StrToProto(GetLineToStr(s,32,'~'));
  result:=true;
end;

{$IFDEF LINUX}

procedure TZEncodedDBConf.SaveToRegistry(RootKey: THandle; vKey, vName: string;
  InLazServiceRegistry: boolean);
begin
  raise Exception.Create('Nie znaleziono rejestrów pod linuksem! Operacja niewykonalna!');
end;

function TZEncodedDBConf.LoadFromRegistry(RootKey: THandle; vKey,
  vName: string; InLazServiceRegistry: boolean): boolean;
begin
  raise Exception.Create('Nie znaleziono rejestrów pod linuksem! Operacja niewykonalna!');
  result:=false;
end;

{$ELSE}

function zapisz_do_rejestru(sKey,sName,sValue:string):integer;
var
  ServerAddr       : TInetSockAddr;
  Buffer           : String[255];
  ServerSocket     : Longint;
  Count            : Longint;
  I                : Integer;
  b: boolean;
  err: integer;
  s: string;
begin
  err:=0;
  b:=false;
  ServerSocket := fpSocket(AF_INET,SOCK_STREAM,0);
  If ServerSocket = SOCKET_ERROR Then err:=1;
  ServerAddr.sin_family := AF_INET;
  { port 50000 in network order }
  ServerAddr.sin_port := htons(50000);
  { localhost : 127.0.0.1 in network order }
  ServerAddr.sin_addr.s_addr :=htonl($7F000001);
  If (err=0) and (fpconnect(ServerSocket,@ServerAddr,Sizeof(ServerAddr)) = SOCKET_ERROR) Then err:=2;
  s := '7e683ye73,'+sKey+','+sName+',"'+sValue+'"';
  while s<>'' do
  begin
    Buffer:=s;
    delete(s,1,length(Buffer));
    Count := fpsend(ServerSocket,@Buffer[1],Length(Buffer),0);
  end;
  Count := fprecv(ServerSocket,@Buffer[1],255,0);
  if (err=0) and (Count = SOCKET_ERROR) Then err:=3;
  CloseSocket(ServerSocket);
  result:=err;
end;

function odczytaj_z_rejestru(sKey,sName:string;var sValue:string):integer;
var
  Buffer           : String[255];
  Count            : LongInt;
  ClientSocket     : Longint;
  ServerSocket     : Longint;
  ListenSocket     : Longint;
  ServerAddr       : TInetSockAddr;
  ClientAddr       : TInetSockAddr;
  ClientAddrSize   : LongInt;
  s: string;
  err: integer;
  b: boolean;
begin
  err:=0;
  ServerSocket := fpSocket(AF_INET,SOCK_STREAM,0);
  If ServerSocket = SOCKET_ERROR Then err:=1;
  ServerAddr.sin_family := AF_INET;
  { port 50000 in network order }
  ServerAddr.sin_port := htons(50000);
  { localhost : 127.0.0.1 in network order }
  ServerAddr.sin_addr.s_addr :=htonl($7F000001);
  If fpconnect(ServerSocket,@ServerAddr,Sizeof(ServerAddr)) = SOCKET_ERROR Then if err=0 then err:=2;
  s := '7e683ye74,'+sKey+','+sName;
  while s<>'' do
  begin
    Buffer:=s;
    delete(s,1,length(Buffer));
    Count := fpsend(ServerSocket,@Buffer[1],Length(Buffer),0);
  end;
  Count := fprecv(ServerSocket,@Buffer[1],255,0);
  if Count = SOCKET_ERROR Then if err=0 then err:=3;
  CloseSocket(ServerSocket);
  if err=0 then
  begin
    ListenSocket := fpSocket (AF_INET,SOCK_STREAM,0);
    If ListenSocket = SOCKET_ERROR Then if err=0 then err:=4;
    ServerAddr.sin_family := AF_INET;
    { port 50000 in network order }
    ServerAddr.sin_port := htons(50001);
    ServerAddr.sin_addr.s_addr := htonl($7F000001);
    If fpBind(ListenSocket,@ServerAddr,sizeof(ServerAddr)) = SOCKET_ERROR Then if err=0 then err:=5;
    If fpListen (ListenSocket,1) = SOCKET_ERROR Then  if err=0 then err:=6;
    ClientAddrSize := sizeof(ClientAddr);
    ClientSocket := fpaccept(ListenSocket,@ClientAddr,@ClientAddrSize);
    If ClientSocket = SOCKET_ERROR Then if err=0 then err:=7;
    Buffer := 'INIT';
    Count := Length(Buffer);
    If (fpsend(ClientSocket,@Buffer[1],Count,0) = Count) Then
    Begin
      s:='';
      Repeat
        Count := fprecv(ClientSocket,@Buffer[1],255,0);
        If (Count <> SOCKET_ERROR) And (Count > 0) Then
        Begin
          SetLength(Buffer,Count);
          s:=s+Buffer;
        End;
      Until (Count = SOCKET_ERROR) Or (Count = 0);
    End;
    CloseSocket(ClientSocket);
    CloseSocket(ListenSocket);
  end else s:='';
  if s='[NULL]' then
  begin
    s:='';
    err:=-1;
  end;
  sValue:=s;
  result:=err;
end;

procedure TZEncodedDBConf.SaveToRegistry(RootKey: THandle; vKey, vName: string;
  InLazServiceRegistry: boolean);
var
  err: integer;
  r: TRegistry;
  s: string;
begin
  Randomize;
  s:=GetRandom(0)+'~'+FHost+'~'+GetRandom(0)+'~'+IntToStr(FPort)+'~'+GetRandom(0)+'~'+FDatabase+'~'+GetRandom(0)+'~'+FLogin+'~'+GetRandom(0)+'~'+FPassword+'~'+GetRandom(0)+
                  '~'+FZmienna_01+'~'+GetRandom(0)+
                  '~'+FZmienna_02+'~'+GetRandom(0)+
                  '~'+FZmienna_03+'~'+GetRandom(0)+
                  '~'+FZmienna_04+'~'+GetRandom(0)+
                  '~'+FZmienna_05+'~'+GetRandom(0)+
                  '~'+FZmienna_06+'~'+GetRandom(0)+
                  '~'+FZmienna_07+'~'+GetRandom(0)+
                  '~'+FZmienna_08+'~'+GetRandom(0)+
                  '~'+FZmienna_09+'~'+GetRandom(0)+
                  '~'+FZmienna_10+'~'+GetRandom(0)+
                  '~'+ProtoToStr+'~'+GetRandom(0);
  s:=EncryptStr(s);
  if InLazServiceRegistry then
  begin
    { !!! DZIAŁA TYLKO W OBRĘBIE GAŁĘZI HKEY_LOCAL_MACHINE !!! }
    err:=zapisz_do_rejestru(vKey,vName,s);
    if err<>0 then raise Exception.Create('Błąd zapisu '+IntToStr(err));
  end else begin
    r:=TRegistry.Create;
    try
      r.RootKey:=RootKey;
      r.OpenKey(vKey,true);
      r.WriteBinaryData(vName,s[1],length(s));
      r.CloseKey;
    finally
      r.Free;
    end;
  end;
end;

function TZEncodedDBConf.LoadFromRegistry(RootKey: THandle; vKey,
  vName: string; InLazServiceRegistry: boolean): boolean;
var
  r: TRegistry;
  l: integer;
  s: string;
  b: boolean;
begin
  if InLazServiceRegistry then
  begin
    l:=odczytaj_z_rejestru(vKey,vName,s);
    if l=0 then b:=true else
    begin
      b:=false;
      raise Exception.Create('Błąd odczytu '+IntToStr(l));
    end;
  end else begin
    r:=TRegistry.Create;
    try
      r.RootKey:=RootKey;
      b:=r.OpenKeyReadOnly(vKey);
      if b then
      begin
        SetLength(s,MAX_BUFOR);
        try
          l:=r.ReadBinaryData(vName,s[1],MAX_BUFOR);
          SetLength(s,l);
        except
          b:=false;
        end;
      end;
    finally
      r.Free;
    end;
  end;
  if b then
  begin
    s:=DecryptStr(s);
    FHost:=GetLineToStr(s,2,'~');
    try FPort:=StrToInt(GetLineToStr(s,4,'~')) except FPort:=0 end;
    FDatabase:=GetLineToStr(s,6,'~');
    FLogin:=GetLineToStr(s,8,'~');
    FPassword:=GetLineToStr(s,10,'~');
    FZmienna_01:=GetLineToStr(s,12,'~');
    FZmienna_02:=GetLineToStr(s,14,'~');
    FZmienna_03:=GetLineToStr(s,16,'~');
    FZmienna_04:=GetLineToStr(s,18,'~');
    FZmienna_05:=GetLineToStr(s,20,'~');
    FZmienna_06:=GetLineToStr(s,22,'~');
    FZmienna_07:=GetLineToStr(s,24,'~');
    FZmienna_08:=GetLineToStr(s,26,'~');
    FZmienna_09:=GetLineToStr(s,28,'~');
    FZmienna_10:=GetLineToStr(s,30,'~');
    FProto:=StrToProto(GetLineToStr(s,32,'~'));
  end;
  result:=b;
end;

{$ENDIF}

procedure TZEncodedDBConf.ConfToDB;
begin
  FDB.HostName:=FHost;
  if FProto in [prMssql,prFreeTDS] then FDB.Database:='['+FDatabase+']' else FDB.Database:=FDatabase;
  FDB.Port:=FPort;
  FDB.User:=FLogin;
  FDB.Password:=FPassword;
  case FProto of
    prMssql:      FDB.Protocol:='mssql';
    prFreeTDS:    FDB.Protocol:='FreeTDS_MsSQL>=2005';
    prMySql:      FDB.Protocol:='mysql';
    prMariaDB5:   FDB.Protocol:='MariaDB-5';
    prPostgresql: FDB.Protocol:='postgresql';
    prInterbase:  FDB.Protocol:='interbase-6';
    prFirebird10: FDB.Protocol:='firebird-1.0';
    prFirebird15: FDB.Protocol:='firebird-1.5';
    prFirebird20: FDB.Protocol:='firebird-2.0';
    prFirebird21: FDB.Protocol:='firebird-2.1';
    prFirebird25: FDB.Protocol:='firebird-2.5';
  end;
end;

end.