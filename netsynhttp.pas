unit NetSynHTTP;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  httpsend;

type

  { TNetSynHTTP }

  TSynHttpMethode = (meGet,mePost);

  TNetSynHTTP = class(TComponent)
  private
    FFilter: TStrings;
    FFiltering: boolean;
    FHeaders: TStrings;
    FMethod: TSynHttpMethode;
    FMimetype: string;
    FReferrer: string;
    FSesja: boolean;
    //FTimeout: integer;
    FUrlData: string;
    FUserAgent: string;
    procedure SetFilter(AValue: TStrings);
    procedure SetHeaders(AValue: TStrings);
    procedure SetMimetype(AValue: string);
    procedure SetUserAgent(AValue: string);
    function GetUserAgent: string;
    function GetMimetype: string;
    procedure konwersja(var AMemoryStream: TMemoryStream);
  protected
  public
    http: THttpSend;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure OpenSession;
    procedure CloseSession;
    function execute(url: string; var res: TMemoryStream): integer;
    function execute(url: string; var res: string): integer;
    function execute(url: string; var res: TStringList): integer;
    function execute(url: string; var res: TStrings): integer;
    function execute_get_post(url_get,url_post: string; var res: TMemoryStream): integer;
    function execute_get_post(url_get,url_post: string; var res: string): integer;
    function execute_get_post(url_get,url_post: string; var res: TStringList): integer;
    function execute_get_post(url_get,url_post: string; var res: TStrings): integer;
    function execute_get_post(url: string; var res: TMemoryStream): integer;
    function execute_get_post(url: string; var res: string): integer;
    function execute_get_post(url: string; var res: TStringList): integer;
    function execute_get_post(url: string; var res: TStrings): integer;
    function GetText(const URL: string; const Response: TStrings): Boolean;
    function GetBinary(const URL: string; const Response: TStream): Boolean;
    function FilteringNow(aHtml: string): string;
  published
    property Sesja: boolean read FSesja;
    property Method: TSynHttpMethode read FMethod write FMethod;
    property Mimetype: string read FMimetype write SetMimetype; //<auto>: "text/html", <app>: "application/x-www-form-urlencoded"
    property UserAgent: string read FUserAgent write SetUserAgent;
    property UrlData: string read FUrlData write FUrlData; //Dane formularzy w metodzie "POST"
    {Filtruj wszystko co zostanie wczytane}
    property Filtering: boolean read FFiltering write FFiltering;
    {Automatyczne filtrowanie stron}
    { D... - Usuń od początku wraz z tekstem}
    { T... - Usuń tekst i całą resztę}
    property Filter: TStrings read FFilter write SetFilter;
    property Headers: TStrings read FHeaders write SetHeaders;
    property Referrer: string read FReferrer write FReferrer;
  end;

procedure Register;

implementation

uses
  ssl_openssl, ssl_openssl_lib, lconvencoding;
  //, synaicnv, synachar;

procedure Register;
begin
  {$I netsynhttp_icon.lrs}
  RegisterComponents('lNet',[TNetSynHTTP]);
end;

{ TNetSynHTTP }

procedure TNetSynHTTP.SetUserAgent(AValue: string);
begin
  if FUserAgent=AValue then Exit;
  if AValue='' then FUserAgent:='<auto>' else FUserAgent:=AValue;
end;

procedure TNetSynHTTP.SetHeaders(AValue: TStrings);
begin
  FHeaders.Assign(AValue);
end;

procedure TNetSynHTTP.SetFilter(AValue: TStrings);
begin
  FFilter.Assign(AValue);
end;

procedure TNetSynHTTP.SetMimetype(AValue: string);
begin
  if FMimetype=AValue then Exit;
  if AValue='' then FMimetype:='<auto>' else FMimetype:=AValue;
end;

function TNetSynHTTP.GetUserAgent: string;
begin
  if (FUserAgent='') or (FUserAgent='<auto>') then result:='Opera/9.80 (Windows NT 5.1; U; pl) Presto/2.2.15 Version/10.10'
  else result:=FUserAgent;
end;

function TNetSynHTTP.GetMimetype: string;
begin
  if (FMimetype='') or (FMimetype='<auto>') then result:='text/html' else
  if FMimetype='<app>' then result:='application/x-www-form-urlencoded' else
  result:=FMimetype;
end;

procedure TNetSynHTTP.konwersja(var AMemoryStream: TMemoryStream);
var
  m: TStringList;
  i: integer;
  s: string;
begin
  m:=TStringList.Create;
  try
    m.LoadFromStream(AMemoryStream);
    for i:=0 to m.Count-1 do
    begin
      s:=m[i];
      s:=ISO_8859_2ToUTF8(s);
      m.Delete(i);
      m.Insert(i,s);
    end;
    AMemoryStream.Clear;
    m.SaveToStream(AMemoryStream);
  finally
    m.Free;
  end;
end;

constructor TNetSynHTTP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaders:=TStringList.Create;
  FFilter:=TStringList.Create;
  FSesja:=false;
  FFiltering:=false;
  FMethod:=meGet;
  FUserAgent:='<auto>';
  FMimetype:='<auto>';
  FUrlData:='';
  FReferrer:='';
  //FTimeout:=0;
end;

destructor TNetSynHTTP.Destroy;
begin
  if FSesja then http.Free;
  FHeaders.Free;
  FFilter.Free;
  inherited Destroy;
end;

procedure TNetSynHTTP.OpenSession;
var
  i: integer;
begin
  http:=THttpSend.Create;
  if FReferrer<>'' then http.Headers.Add('Referer: '+FReferrer);
  for i:=0 to FHeaders.Count-1 do http.Headers.Add(FHeaders[i]);
  FSesja:=true;
end;

procedure TNetSynHTTP.CloseSession;
begin
  http.Free;
  FSesja:=false;
end;

function TNetSynHTTP.execute(url: string; var res: TMemoryStream): integer;
var
  v_http: THttpSend;
  v_method: string;
  v_ok: boolean;
  result_code: integer;
  i: integer;
begin
  if FMethod=meGet then v_method:='GET' else v_method:='POST';
  if FSesja then
  begin
    v_http:=http;
    v_http.Headers.Clear;
  end else begin
    v_http:=THttpSend.Create;
    if FReferrer<>'' then v_http.Headers.Add('Referer: '+FReferrer);
    for i:=0 to FHeaders.Count-1 do v_http.Headers.Add(FHeaders[i]);
  end;
  try
    v_http.UserAgent:=GetUserAgent;
    v_http.MimeType:=GetMimetype;
    if FUrlData<>'' then v_http.Document.Write(Pointer(FUrlData)^,length(FUrlData));
    v_ok:=v_http.HTTPMethod(v_method,url);
    result_code:=v_http.ResultCode;
    res.LoadFromStream(v_http.Document);
  finally
    if not FSesja then v_http.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute(url: string; var res: string): integer;
var
  str: TMemoryStream;
  list: TStringList;
  result_code: integer;
begin
  str:=TMemoryStream.Create;
  list:=TStringList.Create;
  try
    result_code:=execute(url,str);
    list.LoadFromStream(str);
    res:=list.Text;
  finally
    str.Free;
    list.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute(url: string; var res: TStringList): integer;
var
  str: TMemoryStream;
  result_code: integer;
begin
  str:=TMemoryStream.Create;
  try
    result_code:=execute(url,str);
    res.LoadFromStream(str);
  finally
    str.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute(url: string; var res: TStrings): integer;
var
  str: TMemoryStream;
  result_code: integer;
begin
  str:=TMemoryStream.Create;
  try
    result_code:=execute(url,str);
    res.LoadFromStream(str);
  finally
    str.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute_get_post(url_get, url_post: string;
  var res: TMemoryStream): integer;
var
  v_http: THttpSend;
  v_method: string;
  v_ok: boolean;
  result_code: integer;
begin
  v_http:=THttpSend.Create;
  try

    v_http.UserAgent:=GetUserAgent;
    v_http.MimeType:='text/html';
    v_ok:=v_http.HTTPMethod('GET',url_get);
    result_code:=v_http.ResultCode;

    v_http.UserAgent:=GetUserAgent;
    v_http.MimeType:='application/x-www-form-urlencoded';
    v_http.Document.Write(Pointer(FUrlData)^,length(FUrlData));
    v_ok:=v_http.HTTPMethod('POST',url_get);
    result_code:=v_http.ResultCode;
    res.LoadFromStream(v_http.Document);

  finally
    v_http.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute_get_post(url_get, url_post: string; var res: string
  ): integer;
var
  str: TMemoryStream;
  list: TStringList;
  result_code: integer;
begin
  str:=TMemoryStream.Create;
  list:=TStringList.Create;
  try
    result_code:=execute_get_post(url_get,url_post,str);
    list.LoadFromStream(str);
    res:=list.Text;
  finally
    str.Free;
    list.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute_get_post(url_get, url_post: string;
  var res: TStringList): integer;
var
  str: TMemoryStream;
  result_code: integer;
begin
  str:=TMemoryStream.Create;
  try
    result_code:=execute_get_post(url_get,url_post,str);
    res.LoadFromStream(str);
  finally
    str.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute_get_post(url_get, url_post: string;
  var res: TStrings): integer;
var
  str: TMemoryStream;
  result_code: integer;
begin
  str:=TMemoryStream.Create;
  try
    result_code:=execute_get_post(url_get,url_post,str);
    res.LoadFromStream(str);
  finally
    str.Free;
  end;
  result:=result_code;
end;

function TNetSynHTTP.execute_get_post(url: string; var res: TMemoryStream
  ): integer;
begin
  result:=execute_get_post(url,url,res);
end;

function TNetSynHTTP.execute_get_post(url: string; var res: string): integer;
begin
  result:=execute_get_post(url,url,res);
end;

function TNetSynHTTP.execute_get_post(url: string; var res: TStringList
  ): integer;
begin
  result:=execute_get_post(url,url,res);
end;

function TNetSynHTTP.execute_get_post(url: string; var res: TStrings): integer;
begin
  result:=execute_get_post(url,url,res);
end;

function TNetSynHTTP.GetText(const URL: string; const Response: TStrings
  ): Boolean;
begin
  result:=HttpGetText(URL,Response);
end;

function TNetSynHTTP.GetBinary(const URL: string; const Response: TStream
  ): Boolean;
begin
  result:=HttpGetBinary(URL,Response);
end;

function TNetSynHTTP.FilteringNow(aHtml: string): string;
begin

end;

end.
