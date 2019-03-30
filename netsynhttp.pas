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
    FMethod: TSynHttpMethode;
    FUrlData: string;
    FUserAgent: string;
    http: THttpSend;
    procedure SetUserAgent(AValue: string);
    function GetUserAgent: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function execute(url: string; var res: TMemoryStream): integer;
    function execute(url: string; var res: string): integer;
    function execute(url: string; var res: TStringList): integer;
  published
    property Method: TSynHttpMethode read FMethod write FMethod;
    property UserAgent: string read FUserAgent write SetUserAgent;
    property UrlData: string read FUrlData write FUrlData;
  end;

procedure Register;

implementation

uses
  ssl_openssl, ssl_openssl_lib;

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

function TNetSynHTTP.GetUserAgent: string;
begin
  if (FUserAgent='') or (FUserAgent='<auto>') then result:='Opera/9.80 (Windows NT 5.1; U; pl) Presto/2.2.15 Version/10.10'
  else result:=FUserAgent;
end;

constructor TNetSynHTTP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethod:=meGet;
  FUserAgent:='<auto>';
  FUrlData:='';
end;

destructor TNetSynHTTP.Destroy;
begin
  inherited Destroy;
end;

function TNetSynHTTP.execute(url: string; var res: TMemoryStream): integer;
var
  v_http: THttpSend;
  v_method: string;
  v_ok: boolean;
  result_code: integer;
begin
  if FMethod=meGet then v_method:='GET' else v_method:='POST';
  v_http:=THttpSend.Create;
  try
    v_http.UserAgent:=GetUserAgent;
    v_http.MimeType:='text/html';
    if FUrlData<>'' then
    begin
      v_http.MimeType:='application/x-www-form-urlencoded';
      v_http.Document.Write(Pointer(FUrlData)^,length(FUrlData));
    end;
    v_ok:=v_http.HTTPMethod(v_method,url);
    result_code:=v_http.ResultCode;
    res.LoadFromStream(v_http.Document);
  finally
    v_http.Free;
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

end.
