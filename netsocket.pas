unit NetSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, lNet, lNetComponents, lNetSSL, Pipes;

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
  TNetSocketOnReceiveStringASocket = procedure(aMsg: string; aSocket: TLSocket; aBinSize: integer; var aReadBin: boolean) of object;
  TNetSocketOnReceiveBinaryASocket = procedure(const outdata; size: longword; aSocket: TLSocket) of object;
  TNetSocketOnStatus = procedure(aActive, aCrypt: boolean) of object;
  TNetSocketOnCryptDecryptString = procedure(var aText: string) of object;
  TNetSocketOnCryptDecryptBinary = procedure(const indata; var outdata; var size: longword) of object;
  TNetSocketOnInteger = procedure(aTimeVector: integer) of object;
  TNetSocketOnMonitorDataSize = procedure(aLevel: integer; const aData; aSize: integer) of object;

  { TNetSocket }

  TNetSocket = class(TComponent)
  private
    FCommunication: TNetSocketCommunication;
    FGoPM: TNetSocketOnASocketNull;
    FMaxBuffer: word;
    FOnCryptBinary: TNetSocketOnCryptDecryptBinary;
    FOnDecryptBinary: TNetSocketOnCryptDecryptBinary;
    FOnMonRecvData: TNetSocketOnMonitorDataSize;
    FOnMonSendData: TNetSocketOnMonitorDataSize;
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
    FUDPBroadcast: boolean;
    tcp: TLTCPComponent;
    udp: TLUDPComponent;
    ssl: TLSSLSessionComponent;
    ntp_count,ntp_srednia,ntp_t1: integer;
    TabKeys: TStringList;
    TabSocket: TList;
    TabSocketKeys: TStringList;
    buffer_client: TMemoryStream;
    pipein : TInputPipeStream;
    pipeout : TOutputPipeStream;
    function GetCount: integer;
    procedure GetStringReceive(aMsg: string; aSocket: TLSocket; aBinSize: integer; var aReadBin: boolean);
    procedure _OnAccept(aSocket: TLSocket);
    procedure _OnCanSend(aSocket: TLSocket);
    procedure _OnConnect(aSocket: TLSocket);
    procedure _OnDisconnect(aSocket: TLSocket);
    procedure _OnError(const msg: string; aSocket: TLSocket);
    procedure _OnReceive(aSocket: TLSocket);
    procedure _OnSSLAccept(aSocket: TLSocket);
    procedure _OnSSLConnect(aSocket: TLSocket);
    function _SendString(const aMessage: string; aSocket: TLSocket = nil): integer; //wysyła dokładnie to co dostaje
    function _SendBinary(const aBinary; aSize: integer; aSocket: TLSocket = nil): integer; //wysyła dokładnie to co dostaje
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: boolean;
    procedure Disconnect(aForce: boolean = false);
    {Wysyłanie danych tekstowych, oraz danych binarnych w trybie Mixed}
    function SendString(const aMessage: string; aBlock: pointer = nil; aBlockSize: integer = 0; aSocket: TLSocket = nil): integer;
    function SendString(const aMessage: string; aSocket: TLSocket): integer;
    {Wysyłanie danych binarnych}
    function SendBinary(const aBinary; aSize: integer; aSocket: TLSocket = nil): integer;
    procedure GetTimeVector;
    function GetGUID: string;
    function GetGUIDTimeDate: string;
    procedure SendCanSendMessage(aSocket: TLSocket; aValue: string);
    procedure RegisterKey(aKey: string; aSocket: TLSocket = nil);
    function KeyToSocket(aKey: string): TLSocket;
    function KeyExist(aKey: string): boolean;
    procedure Clear;
    function IsOpenPort(aIPAdr: string; aPort: word; aSendString: string = ''): boolean;
    function GetAddressIp: string;
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
    {Po włączeniu wszystkie pakiety
     lecą na broadcast, przy wyłączonej
     opcji lecą na adres Hosta}
    property UDPBroadcast: boolean read FUDPBroadcast write FUDPBroadcast default false;
    {Wewnętrzny Timeout}
    property Timeout: integer read FTimeout write FTimeout;
    {Jeśli nie używasz całej pamięci,
     warto tu istawić max używanej}
    property MaxBuffer: word read FMaxBuffer write FMaxBuffer default 65535;
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
    {Dostępne: tcp/udp (String/Mixed)}
    property OnReceiveString: TNetSocketOnReceiveStringASocket read FOnReceiveString write FOnReceiveString;
    {Dostępne: tcp/udp (Binary/Mixed)}
    property OnReceiveBinary: TNetSocketOnReceiveBinaryASocket read FOnReceiveBinary write FOnReceiveBinary;
    property OnStatus: TNetSocketOnStatus read FOnStatus write FOnStatus;
    property OnCryptString: TNetSocketOnCryptDecryptString read FOnCryptString write FOnCryptString;
    property OnDecryptString: TNetSocketOnCryptDecryptString read FOnDecryptString write FOnDecryptString;
    property OnCryptBinary: TNetSocketOnCryptDecryptBinary read FOnCryptBinary write FOnCryptBinary;
    property OnDecryptBinary: TNetSocketOnCryptDecryptBinary read FOnDecryptBinary write FOnDecryptBinary;
    property OnSSLAccept: TNetSocketOnASocket read FOnSSLAccept write FOnSSLAccept;
    property OnSSLConnect: TNetSocketOnASocket read FOnSSLConnect write FOnSSLConnect;
    {Odpowiedź synchronizacji czasu}
    property OnTimeVector: TNetSocketOnInteger read FOnTimeVector write FOnTimeVector;
    {Monitorowanie wyjścia
     Level (poziom warstwy):
     0 - dane na wyjściu
     1 - dane wewnątrz (zawsze jawne)}
    property OnMonSendData: TNetSocketOnMonitorDataSize read FOnMonSendData write FOnMonSendData;
    {Monitorowanie wejścia
     Level (poziom warstwy):
     0 - dane na wejściu
     1 - dane wewnątrz (zawsze jawne)}
    property OnMonRecvData: TNetSocketOnMonitorDataSize read FOnMonRecvData write FOnMonRecvData;
  end;

procedure Register;

implementation

uses
  ecode_unit, blcksock;

type
  TTBuffer256 = array [0..255] of char;
  PPBuffer256 = ^TTBuffer256;

const
  znaczek = #1;

procedure Register;
begin
  {$I netsocket_icon.lrs}
  RegisterComponents('lNet',[TNetSocket]);
end;

function StrBase(aValue: string; aBase: integer): string;
var
  s: string;
begin
  s:=aValue;
  while length(s)<aBase do s:='0'+s;
  result:=s;
end;

function IntToB256(liczba: longword; var buffer; size: integer): integer;
var
  p: PPBuffer256;
  l,i,j: integer;
  n,pom: longword;
begin
  l:=0;
  p:=@buffer;
  n:=liczba;
  repeat
    if l+1>size then
    begin
      result:=-1;
      exit;
    end;
    for i:=l downto 1 do p^[i]:=p^[i-1]; //przesunięcie o jeden bajt w prawo
    pom:=n mod 256;
    p^[0]:=chr(pom);
    n:=n div 256;
    inc(l);
  until n=0;
  for i:=1 to size-l do
  begin
    for j:=l downto 1 do p^[j]:=p^[j-1]; //przesunięcie o jeden bajt w prawo
    p^[0]:=#0;
  end;
  result:=l;
end;

function B256ToInt(const buffer; size: integer): integer;
var
  p: PPBuffer256;
  i, M: Integer;
  b: byte;
begin
  p:=@buffer;
  Result:=0;
  M:=1;
  for i:=size-1 downto 0 do
  begin
    b:=ord(p^[i]);
    Result:=Result+b*M;
    M:=M shl 8;
  end;
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

procedure log(block: pointer; size: integer);
var
  i: integer;
  c: ^byte;
begin
  for i:=0 to size-1 do
  begin
    c:=block+i;
    write(IntToHex(c^,2),' ');
  end;
  writeln;
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

procedure TNetSocket.GetStringReceive(aMsg: string; aSocket: TLSocket;
  aBinSize: integer; var aReadBin: boolean);
var
  s1,s: string;
  t1,t2,t3,t4,srednia,wektor_czasu: integer;
  id: integer;
begin
  s:=aMsg;
  if (FMode=smServer) and (s='{NTP}') then SendString('{NTP}$'+IntToStr(TimeToInteger(time)),aSocket) else
  if (FMode=smClient) and (GetLineToStr(s,1,'$')='{NTP}') then
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
  end else begin
    if Assigned(FOnReceiveString) then FOnReceiveString(s,aSocket,aBinSize,aReadBin);
  end;
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
  ss,s: string;
  bin,bout: array [0..65535] of char;
  l,ll,lb,i: integer;
  bsize,bsize2,blok: longword;
  tab: array [0..1] of char;
  odczyt_bin: boolean;
begin
  if FCommunication=cmString then
  begin

    (* STRING *)
    if aSocket.GetMessage(ss)>0 then
    begin
      if assigned(FOnMonRecvData) then FOnMonRecvData(0,ss,length(ss));
      ll:=1;
      while true do
      begin
        s:=GetLineToStr(ss,ll,#10);
        if s='' then break;
        if (FSecurity=ssCrypt) and assigned(FOnDecryptString) then FOnDecryptString(s);
        GetStringReceive(s,aSocket,0,odczyt_bin);
        inc(ll);
      end;
    end;

  end else
  if FCommunication=cmBinary then
  begin

    (* BINARY *)
    bsize:=aSocket.Get(bin,FMaxBuffer);
    if assigned(FOnMonRecvData) then FOnMonRecvData(0,bin,bsize);
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

    (* MIXED *)
    bsize:=aSocket.Get(bin,FMaxBuffer);
    if assigned(FOnMonRecvData) then FOnMonRecvData(0,bin,bsize);

    (* dane odbierane najpierw buforuję *)
    buffer_client.Position:=buffer_client.Size;
    buffer_client.Write(bin,bsize);

    WHILE TRUE DO
    BEGIN
      if buffer_client.Size<3 then break;
      buffer_client.Position:=0;
      buffer_client.Read(tab,2);
      blok:=B256ToInt(tab,2);
      if blok=0 then
      begin
        buffer_client.Clear;
        break;
      end;
      if buffer_client.Size<blok+2 then break;

      (* czytam pierwszy blok danych *)
      buffer_client.Position:=0;
      bsize:=buffer_client.Read(bin,blok+2);
      (* przenoszę resztę na początek *)
      bsize2:=buffer_client.Read(bout,buffer_client.Size-bsize);
      buffer_client.Clear;
      buffer_client.Write(bout,bsize2);

      if assigned(FOnMonRecvData) then FOnMonRecvData(0,bin,bsize-2);
      if bsize>0 then
      begin
        if (FSecurity=ssCrypt) and assigned(FOnDecryptBinary) then
        begin
          bsize-=2;
          FOnDecryptBinary(&bin[2],bout,bsize);
          l:=B256ToInt(bout,2)-2; //długość: string + binary
          ll:=B256ToInt(&bout[2],2); //długość bloku stringowego
          lb:=l-ll; //długość bloku binarnego
          s:=''; for i:=1 to ll do s:=s+bout[i+3];
          GetStringReceive(s,aSocket,lb,odczyt_bin);
          if (lb>0) and odczyt_bin and assigned(FOnReceiveBinary) then FOnReceiveBinary(&bout[ll+4],lb,aSocket);
        end else begin
          l:=B256ToInt(bin,2)-2; //długość: string + binary
          ll:=B256ToInt(&bin[2],2); //długość bloku stringowego
          lb:=l-ll; //długość bloku binarnego
          s:=''; for i:=1 to ll do s:=s+bin[i+3];
          //writeln('l=',l,' ll=',ll,' lb=',lb);
          //writeln(s);
          GetStringReceive(s,aSocket,lb,odczyt_bin);
          if (lb>0) and odczyt_bin and assigned(FOnReceiveBinary) then FOnReceiveBinary(&bin[ll+4],lb,aSocket);
        end;
      end;
    END;

  end else begin
    (* CUSTOM *)
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
  if assigned(FOnMonSendData) then FOnMonSendData(0,aMessage,length(aMessage));
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
    if aSocket=nil then
    begin               //LADDR_BR
      if FUDPBroadcast then c:=udp.SendMessage(aMessage,LADDR_BR) else c:=udp.SendMessage(aMessage,FHost);
    end else try if aSocket.ConnectionStatus=scConnected then c:=aSocket.SendMessage(aMessage); except c:=0; end;
  end;
  result:=c;
end;

function TNetSocket._SendBinary(const aBinary; aSize: integer; aSocket: TLSocket
  ): integer;
var
  c: integer;
begin
  if assigned(FOnMonSendData) then FOnMonSendData(0,aBinary,aSize);
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
    if aSocket=nil then
    begin
      if FUDPBroadcast then c:=udp.Send(aBinary,aSize,LADDR_BR) else c:=udp.Send(aBinary,aSize,FHost);
    end else try if aSocket.ConnectionStatus=scConnected then c:=aSocket.Send(aBinary,aSize); except c:=0; end;
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
  FUDPBroadcast:=false;
  FMaxBuffer:=65535;
  FSSLMethod:=msSSLv2or3;
  FTimeout:=4;
  buffer_client:=TMemoryStream.Create;
  CreatePipeStreams(pipein,pipeout);
end;

destructor TNetSocket.Destroy;
begin
  pipein.Free;
  pipeout.Free;
  buffer_client.Free;
  TabKeys.Free;
  TabSocket.Free;
  TabSocketKeys.Free;
  if FActive then Disconnect(true);
  inherited Destroy;
end;

function TNetSocket.Connect: boolean;
var
  i: integer;
begin
  if FActive then exit;
  (* ustawianie objektów *)
  if FProto=spTCP then
  begin
    tcp:=TLTCPComponent.Create(nil);
    tcp.Timeout:=0;
    tcp.ReuseAddress:=FReuseAddress;
    tcp.OnAccept:=@_OnAccept;
    if Assigned(FOnCanSend) then tcp.OnCanSend:=@_OnCanSend;
    if Assigned(FOnConnect) then tcp.OnConnect:=@_OnConnect;
    tcp.OnDisconnect:=@_OnDisconnect;
    tcp.OnError:=@_OnError;
    tcp.OnReceive:=@_OnReceive;
  end else begin
    udp:=TLUDPComponent.Create(nil);
    udp.Timeout:=0;
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
      for i:=1 to FTimeout*10 do
      begin
        sleep(100);
        if Assigned(FGoPM) then FGoPM;
        FActive:=tcp.Connected;
        if FActive then break;
      end;
      FActive:=tcp.Connected;
    end;
  end else begin
    if FMode=smServer then FActive:=udp.Listen(FPort) else
    begin
      FActive:=udp.Connect(FHost,FPort);
      for i:=1 to FTimeout*10 do
      begin
        sleep(100);
        if Assigned(FGoPM) then FGoPM;
        FActive:=udp.Connected;
        if FActive then break;
      end;
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

type
  TByteArray = array [0..65535] of byte;
  PByteArray = ^TByteArray;

function TNetSocket.SendString(const aMessage: string; aBlock: pointer;
  aBlockSize: integer; aSocket: TLSocket): integer;
var
  s: string;
  l1,l2: integer;
  l,ll,j: integer;
  p: PByteArray;
  i,o: array [0..65535] of byte;
begin
  if not FActive then exit;
  if FCommunication=cmMixed then
  begin
    (* MIXED *)
    l1:=length(aMessage);
    l2:=aBlockSize;
    ll:=l1+l2+4;
    IntToB256(l1+l2+2,i,2);
    IntToB256(l1,&i[2],2);
    for j:=0 to l1-1 do i[j+4]:=ord(aMessage[j+1]);
    if aBlockSize>0 then
    begin
      p:=aBlock;
      for j:=0 to aBlockSize-1 do i[j+4+l1]:=p^[j];
    end;
    if (FSecurity=ssCrypt) and Assigned(FOnCryptBinary) then
    begin
      ll:=l1+l2+4;
      FOnCryptBinary(&i[0],&o[2],ll);
      IntToB256(ll,o,2);
      result:=_SendBinary(o,ll+2,aSocket);
    end else result:=_SendBinary(i,ll,aSocket);
  end else begin
    (* STRING *)
    s:=aMessage;
    if (FSecurity=ssCrypt) and Assigned(FOnCryptString) then FOnCryptString(s);
    result:=_SendString(s+#10,aSocket);
  end;
end;

function TNetSocket.SendString(const aMessage: string; aSocket: TLSocket
  ): integer;
begin
  result:=SendString(aMessage,nil,0,aSocket);
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

{
function SendString(const aMessage: string; aSocket: TLSocket = nil; aID: integer = -1): integer;
var
  s,ss: string;
  l: integer;
  p: pchar;
  o: array [0..65535] of byte;
begin
  if not FActive then exit;
  if aID=-1 then s:=aMessage else s:=znaczek+IntToStr(aID)+znaczek+aMessage;
  if FCommunication=cmMixed then
  begin
    l:=length(s)+1;
    if (FSecurity=ssCrypt) and Assigned(FOnCryptBinary) then
    begin
      FillChar(o,FMaxBuffer,0);
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
}

function TNetSocket.IsOpenPort(aIPAdr: string; aPort: word; aSendString: string
  ): boolean;
var
  socket: TBlockSocket;
  b: boolean;
var
  s,vSendString: string;
  l1,l2: integer;
  l,ll,j: integer;
  p: PByteArray;
  i,o: array [0..65535] of byte;
begin
  socket:=TTCPBlockSocket.Create;
  try
    socket.Connect(aIPAdr,IntToStr(aPort));
    b:=socket.LastError=0;
    if b and (aSendString<>'') then
    begin
      vSendString:=aSendString;
      if FCommunication=cmMixed then
      begin
        (* MIXED *)
        l1:=length(vSendString);
        l2:=0;
        ll:=l1+l2+4;
        IntToB256(l1+l2+2,i,2);
        IntToB256(l1,&i[2],2);
        for j:=0 to l1-1 do i[j+4]:=ord(vSendString[j+1]);
        if (FSecurity=ssCrypt) and Assigned(FOnCryptBinary) then
        begin
          ll:=l1+l2+4;
          FOnCryptBinary(&i[0],&o[2],ll);
          IntToB256(ll,o,2);
          socket.SendBuffer(@o,ll+2);
        end else socket.SendBuffer(@i,ll);
      end else begin
        (* STRING *)
        s:=aSendString;
        if (FSecurity=ssCrypt) and Assigned(FOnCryptString) then FOnCryptString(s);
        socket.SendString(s+#10);
      end;
    end;
    socket.CloseSocket;
  finally
    socket.Free;
  end;
  result:=b;
end;

function TNetSocket.GetAddressIp: string;
var
  s: string;
begin
  if FProto=spTCP then
  begin
    s:=tcp.Socks[0].LocalAddress;
    s:=s+':'+IntToStr(tcp.Socks[0].LocalPort);
    s:=s+'/'+tcp.Socks[0].PeerAddress;
    s:=s+':'+IntToStr(tcp.Socks[0].PeerPort);
  end else begin
    s:=udp.Socks[0].LocalAddress;
    s:=s+':'+IntToStr(udp.Socks[0].LocalPort);
    s:=s+'/'+udp.Socks[0].PeerAddress;
    s:=s+':'+IntToStr(udp.Socks[0].PeerPort);
  end;
  result:=s;
end;

end.
