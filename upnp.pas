unit Upnp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

type

  { TUpnp }

  TUpnp = class(TComponent)
  private
    UPNP_OK: boolean;
    FActive: boolean;
    FDiscover: boolean;
    FDeviceIP: string;
    FDevicePort: word;
    FDeviceControlURL: string;
    FLocalIP,FExternalIP: string;
    FTCP,FUDP: TStrings;
    fTitle: string;
    function GetLocalIpAddress: string;
    function IsDiscovered: boolean;
    function Discover: boolean;
    procedure SetTCP(AValue: TStrings);
    procedure SetUDP(AValue: TStrings);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Init: boolean;
    procedure Open;
    procedure Close;
    function AddPortMapping(const aPort: word; aProto: string = 'tcp'; aTitle: string = ''): boolean;
    procedure DeletePortMapping(const aPort: word; aProto: string = 'tcp');
    function GetExternalIP: string;
  published
    {Nazwa pod którą będą widziane otwarte porty}
    property Title: string read fTitle write fTitle;
    property PortsTcp: TStrings read FTCP write SetTCP;
    property PortsUdp: TStrings read FUDP write SetUDP;
  end;

procedure Register;

implementation

uses
  baseunix, blcksock, sockets, synautil, NetSynHTTP, DOM, XMLRead;

const
  EOL = #13#10;
  WAN_IP_CONN_SERVICE = 'WANIPConnection:1';
  WAN_PPP_CONN_SERVICE = 'WANPPPConnection:1';
  WAN_IP_CONN_SERVICE_TYPE = 'urn:schemas-upnp-org:service:WANIPConnection:1';

procedure Register;
begin
  {$I upnp_icon.lrs}
  RegisterComponents('lNet',[TUpnp]);
end;

{ TUpnp }

function TUpnp.GetLocalIpAddress: string;
const
  CN_GDNS_ADDR = '127.0.0.1';
  CN_GDNS_PORT = 53;
var
  sock: longint;
  err: longint;
  UnixAddr: TInetSockAddr;
  HostAddr: TSockAddr;
  len: Integer;
begin
  if FLocalIP='' then
  begin
    err:=0;
    sock:=fpsocket(AF_INET,SOCK_DGRAM,0);
    assert(sock<>-1);
    UnixAddr.sin_family:=AF_INET;
    UnixAddr.sin_port:=htons(CN_GDNS_PORT);
    UnixAddr.sin_addr:=StrToHostAddr(CN_GDNS_ADDR);
    if (fpConnect(sock,@UnixAddr,SizeOf(UnixAddr))=0) then
    begin
      try
        len:=SizeOf(HostAddr);
        if (fpgetsockname(sock,@HostAddr,@len)=0) then result:=NetAddrToStr(HostAddr.sin_addr) else err:=socketError;
      finally
        if (fpclose(sock)<>0) then err:=socketError;
      end;
    end else err:=socketError;
    if (err<>0) then result:='';
  end else result:=FLocalIP;
end;

function TUpnp.IsDiscovered: boolean;
begin
  result:=FDeviceIP<>'';
end;

constructor TUpnp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UPNP_OK:=false;
  FActive:=false;
  FDiscover:=false;
  FDeviceIP:='';
  FDevicePort:=0;
  FExternalIP:='';
  FLocalIP:='';
  FTCP:=TStringList.Create;
  FUDP:=TStringList.Create;
end;

destructor TUpnp.Destroy;
begin
  FTCP.Free;
  FUDP.Free;
  inherited Destroy;
end;

function TUpnp.Init: boolean;
begin
  result:=Discover;
end;

procedure TUpnp.Open;
var
  i: integer;
begin
  if not UPNP_OK then exit;
  if FActive then exit;
  for i:=0 to FTCP.Count-1 do AddPortMapping(StrToInt(FTCP[i]),'tcp',FTitle);
  for i:=0 to FUDP.Count-1 do AddPortMapping(StrToInt(FUDP[i]),'udp',FTitle);
  FActive:=true;
end;

procedure TUpnp.Close;
var
  i: integer;
begin
  if not UPNP_OK then exit;
  if not FActive then exit;
  for i:=0 to FTCP.Count-1 do DeletePortMapping(StrToInt(FTCP[i]),'tcp');
  for i:=0 to FUDP.Count-1 do DeletePortMapping(StrToInt(FUDP[i]),'udp');
  FActive:=false;
end;

function TUpnp.Discover: boolean;
var
  a: integer;
  s,uri,adres,service_type: string;
  prot,user,pass,host,port,path,para: string;
  mem: TMemoryStream;
  LSendStr: string;
  LNet: TUDPBlockSocket;
  LHttp: TNetSynHTTP;
  LResponseStr: string;
  LStartIdx,LCount: Integer;
  LXml: TXMLDocument;
  LNodeList: TDOMNode;

  procedure ProcessNode(Node: TDOMNode);
  var
    pom: string;
    cNode: TDOMNode;
  begin
    if Node=nil then Exit;
    pom:=adres;
    if node.NodeName='#text' then
    begin
      if pos('/serviceList/service/serviceType',adres)>0 then service_type:=node.NodeValue;
      if (service_type=WAN_IP_CONN_SERVICE_TYPE) and (pos('/serviceList/service/controlURL',adres)>0) then FDeviceControlURL:=node.NodeValue;
      //if adres='//device/serviceList/service' then writeln(adres);
    end else adres:=adres+'/'+node.NodeName;
    cNode:=Node.FirstChild;
    while cNode<>nil do
    begin
      ProcessNode(cNode);
      cNode:=cNode.NextSibling;
    end;
    adres:=pom;
  end;

begin
  if FDiscover then exit;
  LSendStr:='M-SEARCH * HTTP/1.1'+EOL+'MX: 2'+EOL+'HOST: 239.255.255.250:1900'+EOL+'MAN: "ssdp:discover"'+EOL+'ST: urn:schemas-upnp-org:service:%s'+EOL+EOL;
  LNet:=TUDPBlockSocket.Create;
  try
    LNet.Connect('239.255.255.250','1900');
    LNet.SendString(Format(LSendStr,[WAN_IP_CONN_SERVICE]));
    LResponseStr:='';
    repeat
      s:=LNet.RecvString(1000);
      if s<>'' then LResponseStr:=LResponseStr+s+EOL;
    until s='';
  finally
    LNet.CloseSocket;
    LNet.Free;
  end;
  if LResponseStr<>'' then
  begin
    LStartIdx:=pos('LOCATION:',LResponseStr);
    if (LStartIdx<>0) then
    begin
      LStartIdx:=LStartIdx+Length('LOCATION:')+1;
      LCount:=pos(EOL,LResponseStr,LStartIdx)-LStartIdx;
      uri:=copy(LResponseStr,LStartIdx,LCount);
      ParseURL(uri,prot,user,pass,host,port,path,para);
      FDeviceIP:=host;
      try FDevicePort:=StrToInt(port) except FDevicePort:=0; end;
      LHttp:=TNetSynHTTP.Create(nil);
      mem:=TMemoryStream.Create;
      try
        a:=LHTtp.execute(uri,mem);
        if a>0 then
        begin
          mem.Position:=0;
          LXml:=TXMLDocument.Create;
          try
            ReadXmlFile(LXml,mem);
            adres:='/';
            LNodeList:=LXml.DocumentElement.FirstChild;
            while LNodeList<>nil do
            begin
              ProcessNode(LNodeList); // Recursive
              LNodeList:=LNodeList.NextSibling;
            end;
          finally
            LXml.Free;
          end;
        end;
      finally
        LHttp.Free;
        mem.Free;
      end;
    end;
    UPNP_OK:=true;
  end else UPNP_OK:=false;
  FDiscover:=true;
  result:=UPNP_OK;
end;

procedure TUpnp.SetTCP(AValue: TStrings);
begin
  if FTCP.Text=AValue.Text then Exit;
  FTCP.Assign(AValue);
end;

procedure TUpnp.SetUDP(AValue: TStrings);
begin
  if FUDP.Text=AValue.Text then Exit;
  FUDP.Assign(AValue);
end;

function TUpnp.AddPortMapping(const aPort: word; aProto: string; aTitle: string
  ): boolean;
var
  n1: string;
  LHttp: TNetSynHTTP;
  LSendData: TStringStream;
  LHeaderStr: string;
  LResponseStr: string;
begin
  if not UPNP_OK then exit;
  if aTitle='' then n1:='NONAME' else n1:=AnsiUpperCase(aTitle);
  LSendData:=TStringStream.Create('');
  try
    LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
    LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    LSendData.WriteString('<s:Body>');
    LSendData.WriteString(Format('<u:AddPortMapping xmlns:u="%s">',[WAN_IP_CONN_SERVICE_TYPE]));
    LSendData.WriteString('<NewRemoteHost></NewRemoteHost>');
    LSendData.WriteString(Format('<NewExternalPort>%d</NewExternalPort>',[aPort]));
    LSendData.WriteString(Format('<NewProtocol>%s</NewProtocol>',[UpperCase(aProto)]));
    LSendData.WriteString(Format('<NewInternalPort>%d</NewInternalPort>',[aPort]));
    LSendData.WriteString(Format('<NewInternalClient>%s</NewInternalClient>',[GetLocalIpAddress]));
    LSendData.WriteString(Format('<NewEnabled>%d</NewEnabled>',[1]));
    LSendData.WriteString(Format('<NewPortMappingDescription>%s</NewPortMappingDescription>',[n1]));
    LSendData.WriteString(Format('<NewLeaseDuration>%d</NewLeaseDuration>',[0]));
    LSendData.WriteString('</u:AddPortMapping>');
    LSendData.WriteString('</s:Body>');
    LSendData.WriteString('</s:Envelope>');
    LHeaderStr:='POST %s HTTP/1.1'+EOL+'HOST: %s:%d'+EOL+'SOAPACTION: "%s"'+EOL+'CONTENT-TYPE: text/xml ; charset="utf-8"'+EOL+'CONTENT-LENGTH: %d'+EOL+EOL;
    LHeaderStr:=Format(LHeaderStr,[FDeviceControlURL,FDeviceIP,FDevicePort,WAN_IP_CONN_SERVICE_TYPE+'#'+'AddPortMapping',LSendData.Size]);
    LHttp:=TNetSynHTTP.Create(nil);
    try
      LHTtp.Method:=mePost;
      LHTtp.Headers.AddText(LHeaderStr);
      LHTtp.UrlData:=LSendData.DataString;
      LHTtp.execute(FDeviceIP+':'+IntToStr(FDevicePort),LResponseStr);
    finally
      LHttp.Free;
    end;
  finally
    LSendData.Free;
  end;
  if (pos('200 OK',LResponseStr)<>0) then result:=true else result:=false;
end;

procedure TUpnp.DeletePortMapping(const aPort: word; aProto: string);
var
  LHttp: TNetSynHTTP;
  LSendData: TStringStream;
  LHeaderStr: string;
  LResponseStr: string;
begin
  if not UPNP_OK then exit;
  LSendData:=TStringStream.Create('');
  try
    LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
    LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
    LSendData.WriteString('<s:Body>');
    LSendData.WriteString(Format('<u:DeletePortMapping xmlns:u="%s">',[WAN_IP_CONN_SERVICE_TYPE]));
    LSendData.WriteString('<NewRemoteHost></NewRemoteHost>');
    LSendData.WriteString(Format('<NewExternalPort>%d</NewExternalPort>',[aPort]));
    LSendData.WriteString(Format('<NewProtocol>%s</NewProtocol>',[UpperCase(aProto)]));
    LSendData.WriteString('</u:DeletePortMapping>');
    LSendData.WriteString('</s:Body>');
    LSendData.WriteString('</s:Envelope>');
    LHeaderStr:='POST %s HTTP/1.1'+EOL+'HOST: %s:%d'+EOL+'SOAPACTION: "%s"'+EOL+'CONTENT-TYPE: text/xml ; charset="utf-8"'+EOL+'CONTENT-LENGTH: %d'+EOL+EOL;
    LHeaderStr:=Format(LHeaderStr,[FDeviceControlURL,FDeviceIP,FDevicePort,WAN_IP_CONN_SERVICE_TYPE+'#'+'DeletePortMapping',LSendData.Size]);
    LHttp:=TNetSynHTTP.Create(nil);
    try
      LHTtp.Method:=mePost;
      LHTtp.Headers.AddText(LHeaderStr);
      LHTtp.UrlData:=LSendData.DataString;
      LHTtp.execute(FDeviceIP+':'+IntToStr(FDevicePort),LResponseStr);
    finally
      LHttp.Free;
    end;
  finally
    LSendData.Free;
  end;
  //if (pos('200 OK',LResponseStr)<>0) then result:=true else result:=false;
end;

function TUpnp.GetExternalIP: string;
var
  a: integer;
  mem: TMemoryStream;
  adres: string;
  LHttp: TNetSynHTTP;
  LSendData: TStringStream;
  LHeaderStr: string;
  LResponseStr: string;
  LXml: TXMLDocument;
  LNodeList: TDOMNode;

  procedure ProcessNode(Node: TDOMNode);
  var
    pom: string;
    cNode: TDOMNode;
  begin
    if Node=nil then Exit;
    pom:=adres;
    if node.NodeName='#text' then
    begin
      if pos('/NewExternalIPAddress',adres)>0 then LResponseStr:=node.NodeValue;
    end else adres:=adres+'/'+node.NodeName;
    cNode:=Node.FirstChild;
    while cNode<>nil do
    begin
      ProcessNode(cNode);
      cNode:=cNode.NextSibling;
    end;
    adres:=pom;
  end;

begin
  if not UPNP_OK then exit;
  result:=FExternalIP;
  if (result='') then
  begin
    LSendData:=TStringStream.Create('');
    mem:=TMemoryStream.Create;
    try
      LSendData.WriteString('<?xml version="1.0" encoding="utf-8"?>');
      LSendData.WriteString('<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/" s:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">');
      LSendData.WriteString('<s:Body>');
      LSendData.WriteString(Format('<u:GetExternalIPAddress xmlns:u="%s">',[WAN_IP_CONN_SERVICE_TYPE]));
      LSendData.WriteString('</u:GetExternalIPAddress>');
      LSendData.WriteString('</s:Body>');
      LSendData.WriteString('</s:Envelope>');
      LHeaderStr:='POST %s HTTP/1.1'+EOL+'HOST: %s:%d'+EOL+'SOAPACTION: "%s"'+EOL+'CONTENT-TYPE: text/xml ; charset="utf-8"'+EOL+'CONTENT-LENGTH: %d'+EOL+EOL;
      LHeaderStr:=Format(LHeaderStr,[FDeviceControlURL,FDeviceIP,FDevicePort,WAN_IP_CONN_SERVICE_TYPE+'#'+'GetExternalIPAddress',LSendData.Size]);
      LHttp:=TNetSynHTTP.Create(nil);
      try
        LHTtp.Method:=mePost;
        LHTtp.Headers.AddText(LHeaderStr);
        LHTtp.UrlData:=LSendData.DataString;
        a:=LHTtp.execute(FDeviceIP+':'+IntToStr(FDevicePort),mem);
      finally
        LHttp.Free;
      end;
      if a>0 then
      begin
        mem.Position:=0;
        LXml:=TXMLDocument.Create;
        try
          ReadXmlFile(LXml,mem);
          adres:='/';
          LNodeList:=LXml.DocumentElement.FirstChild;
          while LNodeList<>nil do
          begin
            ProcessNode(LNodeList); // Recursive
            LNodeList:=LNodeList.NextSibling;
          end;
        finally
          LXml.Free;
        end;
      end;
    finally
      LSendData.Free;
      mem.Free
    end;
    if LResponseStr<>'' then FExternalIP:=LResponseStr;
    result:=FExternalIP;
  end;
end;

end.
