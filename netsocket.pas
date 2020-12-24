unit NetSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, lNet, lNetComponents, lNetSSL;

type
  TNetSocketMode = (smServer, smClient);
  TNetSocketSecurity = (ssNone, ssSSL, ssCrypt);
  TNetSocketSSLMethod = TLSSLMethod;
  TNetSocketOnASocket = procedure(aSocket: TLSocket) of object;
  TNetSocketOnASocketNull = procedure of object;
  TNetSocketOnConstStringASocket = procedure(const aMsg: string; aSocket: TLSocket) of object;
  TNetSocketOnReceiveStringASocket = procedure(aMsg: string; aSocket: TLSocket) of object;
  TNetSocketOnStatus = procedure(aActive, aCrypt: boolean) of object;
  TNetSocketOnCryptDecryptString = procedure(var aText: string) of object;
  TNetSocketOnInteger = procedure(aTimeVector: integer) of object;

  { TNetSocket }

  TNetSocket = class(TComponent)
  private
    FGoPM: TNetSocketOnASocketNull;
    FOnTimeVector: TNetSocketOnInteger;
    FActive,FCrypt: boolean;
    FBinary: boolean;
    FHost: string;
    FMode: TNetSocketMode;
    FOnAccept: TNetSocketOnASocket;
    FOnCanSend: TNetSocketOnASocket;
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
    FReuseAddress: boolean;
    FSecurity: TNetSocketSecurity;
    FSSLCAFile: string;
    FSSLKeyFile: string;
    FSSLMethod: TNetSocketSSLMethod;
    FSSLPassword: string;
    FTimeout: integer;
    tcp: TLTCPComponent;
    ssl: TLSSLSessionComponent;
    ntp_count,ntp_srednia,ntp_t1: integer;
    function GetCount: integer;
    procedure _OnAccept(aSocket: TLSocket);
    procedure _OnCanSend(aSocket: TLSocket);
    procedure _OnConnect(aSocket: TLSocket);
    procedure _OnDisconnect(aSocket: TLSocket);
    procedure _OnError(const msg: string; aSocket: TLSocket);
    procedure _OnReceive(aSocket: TLSocket);
    procedure _OnSSLAccept(aSocket: TLSocket);
    procedure _OnSSLConnect(aSocket: TLSocket);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: boolean;
    procedure Disconnect(aForce: boolean = false);
    procedure SendString(const aMessage: string; aSocket: TLSocket = nil);
    function SendBinary(const aBinary; aSize: integer): integer;
    procedure GetTimeVector;
    function GetGUID: string;
  published
    property Mode: TNetSocketMode read FMode write FMode;
    property Security: TNetSocketSecurity read FSecurity write FSecurity;
    property BinaryMode: boolean read FBinary write FBinary;
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
    {Server: Gdy serwer zaakceptuje połączenie.}
    property OnAccept: TNetSocketOnASocket read FOnAccept write FOnAccept;
    property OnCanSend: TNetSocketOnASocket read FOnCanSend write FOnCanSend;
    {Wymagana pozycja!
     Adding this:
     -> Application.ProcessMessage <-}
    property OnProcessMessage: TNetSocketOnASocketNull read FGoPM write FGoPM;
    {Client: Po połączeniu się z serwerem.}
    property OnConnect: TNetSocketOnASocket read FOnConnect write FOnConnect;
    property OnDisconnect: TNetSocketOnASocketNull read FOnDisconnect write FOnDisconnect;
    property OnError: TNetSocketOnConstStringASocket read FOnError write FOnError;
    property OnReceive: TNetSocketOnASocket read FOnReceive write FOnReceive;
    property OnReceiveString: TNetSocketOnReceiveStringASocket read FOnReceiveString write FOnReceiveString;
    property OnStatus: TNetSocketOnStatus read FOnStatus write FOnStatus;
    property OnCryptString: TNetSocketOnCryptDecryptString read FOnCryptString write FOnCryptString;
    property OnDecryptString: TNetSocketOnCryptDecryptString read FOnDecryptString write FOnDecryptString;
    property OnSSLAccept: TNetSocketOnASocket read FOnSSLAccept write FOnSSLAccept;
    property OnSSLConnect: TNetSocketOnASocket read FOnSSLConnect write FOnSSLConnect;
    property OnTimeVector: TNetSocketOnInteger read FOnTimeVector write FOnTimeVector;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I netsocket_icon.lrs}
  RegisterComponents('lNet',[TNetSocket]);
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

function TimeToInteger(aTime: TDateTime): longword;
var
  godz,min,sec,milisec: word;
begin
  DecodeTime(aTime,godz,min,sec,milisec);
  result:=(godz*60*60*1000)+(min*60*1000)+(sec*1000)+milisec;
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
  if FActive then result:=tcp.Count-1 else result:=0;
end;

procedure TNetSocket._OnCanSend(aSocket: TLSocket);
begin
  FOnCanSend(aSocket);
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
  ll: integer;
  t1,t2,t3,t4,srednia,wektor_czasu: integer;
begin
  if Assigned(FOnReceive) then FOnReceive(aSocket);
  if FBinary then exit;
  if Assigned(FOnReceiveString) then
  begin
    if aSocket.GetMessage(ss)>0 then
    begin
      ll:=1;
      while true do
      begin
        s:=GetLineToStr(ss,ll,#10);
        if s='' then break;
        inc(ll);
        if (FSecurity=ssCrypt) and Assigned(FOnDecryptString) then FOnDecryptString(s);
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
        end else FOnReceiveString(s,aSocket);
      end;
    end;
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

constructor TNetSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=false;
  FCrypt:=false;
  FSecurity:=ssNone;
  FMode:=smServer;
  FBinary:=false;
  FHost:='';
  FPort:=0;
  FReuseAddress:=false;
  FSSLMethod:=msSSLv2or3;
  FTimeout:=0;
end;

destructor TNetSocket.Destroy;
begin
  if FActive then Disconnect(true);
  inherited Destroy;
end;

function TNetSocket.Connect: boolean;
begin
  if FActive then exit;
  tcp:=TLTCPComponent.Create(nil);
  tcp.Timeout:=FTimeout;
  tcp.ReuseAddress:=FReuseAddress;
  tcp.OnAccept:=@_OnAccept;
  if Assigned(FOnCanSend) then tcp.OnCanSend:=@_OnCanSend;
  if Assigned(FOnConnect) then tcp.OnConnect:=@_OnConnect;
  tcp.OnDisconnect:=@_OnDisconnect;
  tcp.OnError:=@_OnError;
  tcp.OnReceive:=@_OnReceive;
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
  if FMode=smServer then FActive:=tcp.Listen(FPort) else
  begin
    FActive:=tcp.Connect(FHost,FPort);
    sleep(250);
    if Assigned(FGoPM) then FGoPM;
    FActive:=tcp.Connected;
  end;
  if not FActive then
  begin
    if FSecurity=ssSSL then ssl.Free;
    tcp.Free;
  end;
  FCrypt:=(FSecurity=ssCrypt) and FActive;
  if Assigned(FOnStatus) then FOnStatus(FActive,FCrypt);
  result:=FActive;
end;

procedure TNetSocket.Disconnect(aForce: boolean);
begin
  if not FActive then exit;
  tcp.Disconnect(aForce);
  sleep(250);
  if FSecurity=ssSSL then ssl.Free;
  tcp.Free;
  FActive:=false;
  FCrypt:=false;
  if Assigned(FOnDisconnect) then FOnDisconnect;
  if Assigned(FOnStatus) then FOnStatus(FActive,FCrypt);
end;

procedure TNetSocket.SendString(const aMessage: string; aSocket: TLSocket);
var
  s: string;
begin
  if not FActive then exit;
  s:=aMessage;
  if (FSecurity=ssCrypt) and Assigned(FOnCryptString) then FOnCryptString(s);
  if FMode=smServer then
  begin
    if aSocket=nil then
    begin
      tcp.IterReset;
      while tcp.IterNext do tcp.SendMessage(s+#10,tcp.Iterator);
    end else aSocket.SendMessage(s+#10);
  end else tcp.SendMessage(s+#10);
end;

function TNetSocket.SendBinary(const aBinary; aSize: integer): integer;
begin
  if not FActive then exit;
  if FMode=smServer then
  begin
    tcp.IterReset;
    while tcp.IterNext do result:=tcp.Send(aBinary,aSize,tcp.Iterator);
  end else result:=tcp.Send(aBinary,aSize);
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

end.
