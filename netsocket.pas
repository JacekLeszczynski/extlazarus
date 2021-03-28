unit NetSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, lNet, lNetComponents, lNetSSL;

type
  TNetSocketMode = (smServer, smClient);
  TNetSocketProto = (spTCP, spUDP);
  TNetSocketSecurity = (ssNone, ssSSL, ssCrypt);
  TNetSocketCommunication = (cmCustom,cmString,cmBinary,cmMixed);
  TNetSocketSSLMethod = TLSSLMethod;
  TNetSocketOnASocket = procedure(aSocket: TLSocket) of object;
  TNetSocketOnCanSend = procedure(aSocket: TLSocket; const aValue: string) of object;
  TNetSocketOnASocketNull = procedure of object;
  TNetSocketOnConstStringASocket = procedure(const aMsg: string; aSocket: TLSocket) of object;
  TNetSocketOnReceiveStringASocket = procedure(aMsg: string; aSocket: TLSocket) of object;
  TNetSocketOnReceiveBinaryASocket = procedure(const outdata; size: longword; aSocket: TLSocket) of object;
  TNetSocketOnStatus = procedure(aActive, aCrypt: boolean) of object;
  TNetSocketOnCryptDecryptString = procedure(var aText: string) of object;
  TNetSocketOnCryptDecryptBinary = procedure(const indata; var outdata; var size: longword) of object;
  TNetSocketOnInteger = procedure(aTimeVector: integer) of object;

  { TNetSocket }

  TNetSocket = class(TComponent)
  private
    FCommunication: TNetSocketCommunication;
    FGoPM: TNetSocketOnASocketNull;
    FOnCryptBinary: TNetSocketOnCryptDecryptBinary;
    FOnDecryptBinary: TNetSocketOnCryptDecryptBinary;
    FOnReceiveBinary: TNetSocketOnReceiveBinaryASocket;
    FOnTimeVector: TNetSocketOnInteger;
    FActive,FCrypt: boolean;
    FHost: string;
    FMode: TNetSocketMode;
    FOnAccept: TNetSocketOnASocket;
    FOnCanSend: TNetSocketOnCanSend;
    FOnConnect: TNetSocketOnASocket;
    FOnCryptString: TNetSocketOnCryptDecryptString;
    FOnDecryptString: TNetSocketOnCryptDecryptString;
    FOnDisconnect: TNetSocketOnASocketNull;
    FOnError: TNetSocketOnConstStringASocket;
    FOnReceive: TNetSocketOnASocket;
    FOnReceiveString: TNetSocketOnReceiveStringASocket;
    FOnSSLAccept: TNetSocketOnASocket;
    FOnSSLConnect: TNetSocketOnASocket;
    FOnStatus: TNetSocketOnStatus;
    FPort: Word;
    FProto: TNetSocketProto;
    FReuseAddress: boolean;
    FSecurity: TNetSocketSecurity;
    FSSLCAFile: string;
    FSSLKeyFile: string;
    FSSLMethod: TNetSocketSSLMethod;
    FSSLPassword: string;
    FTimeout: integer;
    tcp: TLTCPComponent;
    udp: TLUDPComponent;
    ssl: TLSSLSessionComponent;
    ntp_count,ntp_srednia,ntp_t1: integer;
    TabKeys: TStringList;
    TabSocket: TList;
    TabSocketKeys: TStringList;
    function GetCount: integer;
    procedure GetStringReceive(aMsg: string; aSocket: TLSocket);
    procedure _OnAccept(aSocket: TLSocket);
    procedure _OnCanSend(aSocket: TLSocket);
    procedure _OnConnect(aSocket: TLSocket);
    procedure _OnDisconnect(aSocket: TLSocket);
    procedure _OnError(const msg: string; aSocket: TLSocket);
    procedure _OnReceive(aSocket: TLSocket);
    procedure _OnSSLAccept(aSocket: TLSocket);
    procedure _OnSSLConnect(aSocket: TLSocket);
    function _SendString(const aMessage: string; aSocket: TLSocket = nil): integer;
    function _SendBinary(const aBinary; aSize: integer; aSocket: TLSocket = nil): integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: boolean;
    procedure Disconnect(aForce: boolean = false);
    function SendString(const aMessage: string; aSocket: TLSocket = nil): integer;
    function SendBinary(const aBinary; aSize: integer; aSocket: TLSocket = nil): integer;
    procedure GetTimeVector;
    function GetGUID: string;
    function GetGUIDTimeDate: string;
    procedure SendCanSendMessage(aSocket: TLSocket; aValue: string);
    procedure RegisterKey(aKey: string; aSocket: TLSocket = nil);
    function KeyToSocket(aKey: string): TLSocket;
    function KeyExist(aKey: string): boolean;
    procedure Clear;
  published
    property Mode: TNetSocketMode read FMode write FMode;
    property Protocol: TNetSocketProto read FProto write FProto default spTCP;
    property Security: TNetSocketSecurity read FSecurity write FSecurity;
    property Communication: TNetSocketCommunication read FCommunication write FCommunication default cmString;
    {Po połączeniu w kliencie wykonaj:
     -> Application.ProcessMessage <-}
    property Active: boolean read FActive;
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property ReuseAddress: boolean read FReuseAddress write FReuseAddress;
    property Timeout: integer read FTimeout write FTimeout;
    property SSLMethod: TNetSocketSSLMethod read FSSLMethod write FSSLMethod;
    property SSLCAFile: string read FSSLCAFile write FSSLCAFile;
    property SSLKeyFile: string read FSSLKeyFile write FSSLKeyFile;
    property SSLPassword: string read FSSLPassword write FSSLPassword;
    {Działa gdy AutoRegister jest ustawiony.}
    property Count: integer read GetCount;
    {Server: Gdy serwer zaakceptuje połączenie.
     Dostępne: tcp}
    property OnAccept: TNetSocketOnASocket read FOnAccept write FOnAccept;
    {Dostępne: tcp/udp}
    property OnCanSend: TNetSocketOnCanSend read FOnCanSend write FOnCanSend;
    {Wymagana pozycja!
     Adding this:
     -> Application.ProcessMessage <-}
    property OnProcessMessage: TNetSocketOnASocketNull read FGoPM write FGoPM;
    {Client (tcp) / Server (tcp,udp)}
    property OnConnect: TNetSocketOnASocket read FOnConnect write FOnConnect;
    {Dostępne: tcp/udp*}
    property OnDisconnect: TNetSocketOnASocketNull read FOnDisconnect write FOnDisconnect;
    {Dostępne: tcp/udp}
    property OnError: TNetSocketOnConstStringASocket read FOnError write FOnError;
    {Dostępne: tcp/udp}
    property OnReceive: TNetSocketOnASocket read FOnReceive write FOnReceive;
    {Dostępne: tcp/udp}
    property OnReceiveString: TNetSocketOnReceiveStringASocket read FOnReceiveString write FOnReceiveString;
    property OnReceiveBinary: TNetSocketOnReceiveBinaryASocket read FOnReceiveBinary write FOnReceiveBinary;
    property OnStatus: TNetSocketOnStatus read FOnStatus write FOnStatus;
    property OnCryptString: TNetSocketOnCryptDecryptString read FOnCryptString write FOnCryptString;
    property OnDecryptString: TNetSocketOnCryptDecryptString read FOnDecryptString write FOnDecryptString;
    property OnCryptBinary: TNetSocketOnCryptDecryptBinary read FOnCryptBinary write FOnCryptBinary;
    property OnDecryptBinary: TNetSocketOnCryptDecryptBinary read FOnDecryptBinary write FOnDecryptBinary;
    property OnSSLAccept: TNetSocketOnASocket read FOnSSLAccept write FOnSSLAccept;
    property OnSSLConnect: TNetSocketOnASocket read FOnSSLConnect write FOnSSLConnect;
    property OnTimeVector: TNetSocketOnInteger read FOnTimeVector write FOnTimeVector;
  end;

procedure Register;

implementation

uses
  ecode_unit;

procedure Register;
begin
  {$I netsocket_icon.lrs}
  RegisterComponents('lNet',[TNetSocket]);
end;

function IntToSys(liczba, baza: integer): string;
var
  wynik: string;
  n,pom: integer;
begin
  wynik:='';
  n:=liczba;
  repeat
    pom:=n mod baza;
    if pom<10 then wynik:=IntToStr(pom)+wynik else if pom<36 then wynik:=chr(pom+55)+wynik else wynik:=chr(pom+61)+wynik;
    n:=n div baza;
  until n=0;
  result:=wynik;
end;

function HexToDec(Str: string): Integer;
var
  i, M: Integer;
begin
  Result:=0;
  M:=1;
  Str:=AnsiUpperCase(Str);
  for i:=Length(Str) downto 1 do
  begin
    case Str[i] of
      '1'..'9': Result:=Result+(Ord(Str[i])-Ord('0'))*M;
      'A'..'F': Result:=Result+(Ord(Str[i])-Ord('A')+10)*M;
    end;
    M:=M shl 4;
  end;
end;

function StrBase(aValue: string; aBase: integer): string;
var
  s: string;
begin
  s:=aValue;
  while length(s)<aBase do s:='0'+s;
  result:=s;
end;

{ TNetSocket }

procedure TNetSocket._OnAccept(aSocket: TLSocket);
begin
  if Assigned(FOnAccept) then FOnAccept(aSocket);
end;

function TNetSocket.GetCount: integer;
var
  i: integer;
begin
  if FActive then
  begin
    if FProto=spTCP then result:=tcp.Count-1 else result:=udp.Count-1;
  end else result:=0;
end;

procedure TNetSocket.GetStringReceive(aMsg: string; aSocket: TLSocket);
var
  s: string;
  t1,t2,t3,t4,srednia,wektor_czasu: integer;
begin
  s:=aMsg;
  if (FMode=smServer) and (s='{NTP}') then SendString('NTP$'+IntToStr(TimeToInteger(time)),aSocket) else
  if (FMode=smClient) and (GetLineToStr(s,1,'$')='NTP') then
  begin
    t1:=ntp_t1;
    t2:=StrToInt(GetLineToStr(s,2,'$'));
    t3:=t2;
    t4:=TimeToInteger(time);
    srednia:=round(((t2-t1)+(t3-t4))/2);
    ntp_srednia:=ntp_srednia+srednia;
    inc(ntp_count);
    if ntp_count<100 then
    begin
      ntp_t1:=t4;
      SendString('{NTP}');
    end else begin
      wektor_czasu:=round(ntp_srednia/ntp_count);
      if Assigned(FOnTimeVector) then FOnTimeVector(wektor_czasu);
    end;
  end else if Assigned(FOnReceiveString) then FOnReceiveString(s,aSocket);
end;

procedure TNetSocket._OnCanSend(aSocket: TLSocket);
begin
  FOnCanSend(aSocket,'');
end;

procedure TNetSocket._OnConnect(aSocket: TLSocket);
begin
  FOnConnect(aSocket);
end;

procedure TNetSocket._OnDisconnect(aSocket: TLSocket);
begin
  if FActive and (FMode=smClient) then Disconnect;
end;

procedure TNetSocket._OnError(const msg: string; aSocket: TLSocket);
begin
  if Assigned(FOnError) then FOnError(msg,aSocket);
end;

procedure TNetSocket._OnReceive(aSocket: TLSocket);
var
  p: pchar;
  ss,s,pom: string;
  bin,bout: array [0..65535] of char;
  ll,i: integer;
  wsk,bsize,blok,a,b: longword;
begin
  if FCommunication=cmString then
  begin
    if aSocket.GetMessage(ss)>0 then
    begin
      ll:=1;
      while true do
      begin
        s:=GetLineToStr(ss,ll,#10);
        if s='' then break;
        if (FSecurity=ssCrypt) and assigned(FOnDecryptString) then FOnDecryptString(s);
        GetStringReceive(s,aSocket);
        inc(ll);
      end;
    end;
  end else
  if FCommunication=cmBinary then
  begin
    bsize:=aSocket.Get(bin,65535);
    if bsize>0 then
    begin
      if (FSecurity=ssCrypt) and assigned(FOnDecryptBinary) then
      begin
        FOnDecryptBinary(bin,bout,bsize);
        if assigned(FOnReceiveBinary) then FOnReceiveBinary(bout,bsize,aSocket);
      end else begin
        if assigned(FOnReceiveBinary) then FOnReceiveBinary(bin,bsize,aSocket);
      end;
    end;
  end else
  if FCommunication=cmMixed then
  begin
    bsize:=aSocket.Get(bin,65535);
    if bsize>0 then
    begin
      if (FSecurity=ssCrypt) and assigned(FOnDecryptBinary) then
      begin
        wsk:=0;
        blok:=0;
        while wsk+4<bsize do
        begin
          pom:='    ';
          pom[1]:=bin[wsk];
          pom[2]:=bin[wsk+1];
          pom[3]:=bin[wsk+2];
          pom[4]:=bin[wsk+3];
          blok:=HexToDec(pom);
          FOnDecryptBinary(&bin[wsk+4],bout,blok);
          p:=@bout+0;
          s:=p;
          GetStringReceive(s,aSocket);
          wsk:=wsk+4+blok;
        end;
      end else begin
        p:=@bin+0;
        ss:=p;
        ll:=length(ss);
        wsk:=1;
        blok:=0;
        while wsk+4<ll do
        begin
          blok:=HexToDec(copy(ss,wsk,4));
          //if blok=0 then break;
          s:=copy(ss,wsk+4,blok);
          GetStringReceive(s,aSocket);
          wsk:=wsk+4+blok;
        end;
      end;
    end;
  end else begin
    if Assigned(FOnReceive) then FOnReceive(aSocket);
  end;
end;

procedure TNetSocket._OnSSLAccept(aSocket: TLSocket);
begin
  if Assigned(FOnSSLAccept) then FOnSSLAccept(aSocket);
end;

procedure TNetSocket._OnSSLConnect(aSocket: TLSocket);
begin
  if Assigned(FOnSSLConnect) then FOnSSLConnect(aSocket);
end;

function TNetSocket._SendString(const aMessage: string; aSocket: TLSocket
  ): integer;
var
  c: integer;
begin
  c:=0;
  if FProto=spTCP then
  begin
    (* TCP *)
    if FMode=smServer then
    begin
      if aSocket=nil then
      begin
        tcp.IterReset;
        while tcp.IterNext do c:=c+tcp.SendMessage(aMessage,tcp.Iterator);
      end else try if aSocket.ConnectionStatus=scConnected then c:=aSocket.SendMessage(aMessage); except c:=0; end;
    end else c:=tcp.SendMessage(aMessage);
  end else begin
    (* UDP *)
    if aSocket=nil then c:=udp.SendMessage(aMessage,LADDR_BR) else try if aSocket.ConnectionStatus=scConnected then c:=aSocket.SendMessage(aMessage); except c:=0; end;
  end;
  result:=c;
end;

function TNetSocket._SendBinary(const aBinary; aSize: integer; aSocket: TLSocket
  ): integer;
var
  c: integer;
begin
  c:=0;
  if FProto=spTCP then
  begin
    (* TCP *)
    if FMode=smServer then
    begin
      if aSocket=nil then
      begin
        tcp.IterReset;
        while tcp.IterNext do c:=c+tcp.Send(aBinary,aSize,tcp.Iterator);
      end else try if aSocket.ConnectionStatus=scConnected then c:=aSocket.Send(aBinary,aSize); except c:=0; end;
    end else c:=tcp.Send(aBinary,aSize);
  end else begin
    (* UDP *)
    if aSocket=nil then c:=udp.Send(aBinary,aSize,LADDR_BR) else try if aSocket.ConnectionStatus=scConnected then c:=aSocket.Send(aBinary,aSize); except c:=0; end;
  end;
  result:=c;
end;

constructor TNetSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabKeys:=TStringList.Create;
  TabKeys.Sorted:=true;
  TabSocket:=TList.Create;
  TabSocketKeys:=TStringList.Create;
  FActive:=false;
  FCrypt:=false;
  FSecurity:=ssNone;
  FMode:=smServer;
  FCommunication:=cmString;
  FProto:=spTCP;
  FHost:='';
  FPort:=0;
  FReuseAddress:=false;
  FSSLMethod:=msSSLv2or3;
  FTimeout:=0;
end;

destructor TNetSocket.Destroy;
var
  i: integer;
begin
  TabKeys.Free;
  TabSocket.Free;
  TabSocketKeys.Free;
  if FActive then Disconnect(true);
  inherited Destroy;
end;

function TNetSocket.Connect: boolean;
begin
  if FActive then exit;
  (* ustawianie objektów *)
  if FProto=spTCP then
  begin
    tcp:=TLTCPComponent.Create(nil);
    tcp.Timeout:=FTimeout;
    tcp.ReuseAddress:=FReuseAddress;
    tcp.OnAccept:=@_OnAccept;
    if Assigned(FOnCanSend) then tcp.OnCanSend:=@_OnCanSend;
    if Assigned(FOnConnect) then tcp.OnConnect:=@_OnConnect;
    tcp.OnDisconnect:=@_OnDisconnect;
    tcp.OnError:=@_OnError;
    tcp.OnReceive:=@_OnReceive;
  end else begin
    udp:=TLUDPComponent.Create(nil);
    udp.Timeout:=FTimeout;
    udp.ReuseAddress:=FReuseAddress;
    if Assigned(FOnCanSend) then udp.OnCanSend:=@_OnCanSend;
    udp.OnDisconnect:=@_OnDisconnect;
    udp.OnError:=@_OnError;
    udp.OnReceive:=@_OnReceive;
  end;
  (* ustawienia SSL *)
  if FSecurity=ssSSL then
  begin
    ssl:=TLSSLSessionComponent.Create(nil);
    ssl.Method:=FSSLMethod;
    ssl.OnSSLAccept:=@_OnSSLAccept;
    ssl.OnSSLConnect:=@_OnSSLConnect;
    ssl.CAFile:=FSSLCAFile;
    ssl.KeyFile:=FSSLKeyFile;
    ssl.Password:=FSSLPassword;
    ssl.SSLActive:=true;
  end;
  (* ustanowienie połączenia*)
  if FProto=spTCP then
  begin
    if FMode=smServer then FActive:=tcp.Listen(FPort) else
    begin
      FActive:=tcp.Connect(FHost,FPort);
      sleep(250);
      if Assigned(FGoPM) then FGoPM;
      FActive:=tcp.Connected;
    end;
  end else begin
    if FMode=smServer then FActive:=udp.Listen(FPort) else
    begin
      FActive:=udp.Connect(FHost,FPort);
      sleep(250);
      if Assigned(FGoPM) then FGoPM;
      FActive:=udp.Connected;
    end;
  end;
  (* jeśli nie uzyskano połączenia *)
  if not FActive then
  begin
    if FSecurity=ssSSL then ssl.Free;
    if FProto=spTCP then tcp.Free else udp.Free;
  end;
  if FMode=smServer then if FActive then if Assigned(FOnConnect) then FOnConnect(nil);
  FCrypt:=(FSecurity=ssCrypt) and FActive;
  if Assigned(FOnStatus) then FOnStatus(FActive,FCrypt);
  result:=FActive;
end;

procedure TNetSocket.Disconnect(aForce: boolean);
begin
  if not FActive then exit;
  if FProto=spTCP then tcp.Disconnect(aForce) else udp.Disconnect(aForce);
  sleep(250);
  if FSecurity=ssSSL then ssl.Free;
  if FProto=spTCP then tcp.Free else udp.Free;
  FActive:=false;
  FCrypt:=false;
  TabKeys.Clear;
  if Assigned(FOnDisconnect) then FOnDisconnect;
  if Assigned(FOnStatus) then FOnStatus(FActive,FCrypt);
end;

function TNetSocket.SendString(const aMessage: string; aSocket: TLSocket
  ): integer;
var
  s,ss: string;
  l: integer;
  p: pchar;
  o: array [0..65535] of byte;
begin
  if not FActive then exit;
  s:=aMessage;
  if FCommunication=cmMixed then
  begin
    l:=length(s)+1;
    if (FSecurity=ssCrypt) and Assigned(FOnCryptBinary) then
    begin
      FOnCryptBinary(&s[1],&o[4],l);
      ss:=StrBase(IntToSys(l,16),4);
      o[0]:=ord(ss[1]);
      o[1]:=ord(ss[2]);
      o[2]:=ord(ss[3]);
      o[3]:=ord(ss[4]);
      result:=_SendBinary(o,l+4,aSocket);
    end else begin
      ss:=StrBase(IntToSys(l,16),4)+s;
      result:=_SendBinary(&ss[1],l+4,aSocket);
    end;
  end else begin
    if (FSecurity=ssCrypt) and Assigned(FOnCryptString) then FOnCryptString(s);
    result:=_SendString(s+#10,aSocket);
  end;
end;

function TNetSocket.SendBinary(const aBinary; aSize: integer; aSocket: TLSocket
  ): integer;
var
  data: array [0..65535] of byte;
begin
  if not FActive then exit;
  if (FSecurity=ssCrypt) and Assigned(FOnDecryptBinary) then
  begin
    FOnDecryptBinary(aBinary,data,aSize);
    result:=_SendBinary(data,aSize,aSocket);
  end else result:=_SendBinary(aBinary,aSize,aSocket);
end;

procedure TNetSocket.GetTimeVector;
begin
  if FMode=smServer then exit;
  ntp_count:=0;
  ntp_srednia:=0;
  ntp_t1:=TimeToInteger(time);
  SendString('{NTP}');
end;

function TNetSocket.GetGUID: string;
var
  error: integer;
  a: TGUID;
  s: string;
begin
  error:=CreateGUID(a);
  if error=0 then
  begin
    s:=GUIDToString(a);
    result:=s;
  end else result:='';
end;

function TNetSocket.GetGUIDTimeDate: string;
var
  s,s1: string;
  i: integer;
begin
  s:=GetGUID;
  s:=StringReplace(s,'{','',[rfReplaceAll]);
  s:=StringReplace(s,'}','',[rfReplaceAll]);
  s:=StringReplace(s,'-','',[rfReplaceAll]);
  s1:='';
  for i:=0 to 7 do s1:=s1+StrBase(IntToSys(HexToDec(copy(s,i*4+1,4)),62),3);
  result:=s1+IntToSys(DateTimeToFileDate(now),62);
end;

procedure TNetSocket.SendCanSendMessage(aSocket: TLSocket; aValue: string);
begin
  if assigned(FOnCanSend) then FOnCanSend(aSocket,aValue);
end;

procedure TNetSocket.RegisterKey(aKey: string; aSocket: TLSocket);
begin
  TabKeys.Add(aKey);
  TabSocketKeys.Add(aKey);
  TabSocket.Add(aSocket);
end;

function TNetSocket.KeyToSocket(aKey: string): TLSocket;
var
  i: integer;
begin
  result:=nil;
  for i:=0 to TabSocketKeys.Count-1 do if TabSocketKeys[i]=aKey then
  begin
    result:=TLSocket(TabSocket[i]);
    break;
  end;
end;

function TNetSocket.KeyExist(aKey: string): boolean;
var
  index: integer;
begin
  result:=TabKeys.Find(aKey,index);
end;

procedure TNetSocket.Clear;
begin
  TabKeys.Clear;
  TabSocketKeys.Clear;
  TabSocket.Clear;
end;

end.
