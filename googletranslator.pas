unit GoogleTranslator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, NetSynHTTP;

type

  { TGoogleTranslator }

  TGoogleTranslator = class(TComponent)
  private
    FLangSource: string;
    FLangTarget: string;
    FOnlyTest: boolean;
    FTextSource: TStrings;
    FTextTranslated: TStrings;
    http: TNetSynHTTP;
    procedure SetTextSource(AValue: TStrings);
    procedure SetTextTranslated(AValue: TStrings);
    procedure ParsujWinik(AText: string);
    function ParsujWinikStr(AText: string): string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    function ExecuteString(aLangFrom,aLangTo,AString: string): string;
  published
    property OnlyTest: boolean read FOnlyTest write FOnlyTest;
    property LangSource: string read FLangSource write FLangSource;
    property LangTarget: string read FLangTarget write FLangTarget;
    property TextSource: TStrings read FTextSource write SetTextSource;
    property TextTranslated: TStrings read FTextTranslated write SetTextTranslated;
  end;

procedure Register;

implementation

uses
  ecode_unit, synacode;

procedure Register;
begin
  {$I googletranslator_icon.lrs}
  RegisterComponents('System',[TGoogleTranslator]);
end;

{ TGoogleTranslator }

procedure TGoogleTranslator.SetTextSource(AValue: TStrings);
begin
  if FTextSource.Text=AValue.Text then Exit;
  FTextSource.Assign(AValue);
end;

procedure TGoogleTranslator.SetTextTranslated(AValue: TStrings);
begin
  if FTextTranslated.Text=AValue.Text then Exit;
  FTextTranslated.Assign(AValue);
end;

procedure TGoogleTranslator.ParsujWinik(AText: string);
var
  s: string;
  a,b: integer;
begin
  FTextTranslated.Clear;
  s:=StringReplace(AText,'\n','',[rfReplaceAll]);
  while true do
  begin
    a:=pos('["',s);
    if a=0 then break;
    delete(s,1,a);
    FTextTranslated.Add(GetLineToStr(s,1,','));
  end;
end;

function TGoogleTranslator.ParsujWinikStr(AText: string): string;
var
  s,pom: string;
  a,b: integer;
begin
  pom:='';
  s:=AText;
  while true do
  begin
    a:=pos('["',s);
    if a=0 then break;
    delete(s,1,a);
    pom:=pom+GetLineToStr(s,1,',');
  end;
  result:=pom;
end;

constructor TGoogleTranslator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextSource:=TStringList.Create;
  FTextTranslated:=TStringList.Create;
  FOnlyTest:=false;
  http:=TNetSynHTTP.Create(nil);
end;

destructor TGoogleTranslator.Destroy;
begin
  FTextSource.Free;
  FTextTranslated.Free;
  http.Free;
  inherited Destroy;
end;

procedure TGoogleTranslator.Execute;
var
  url: string;
  s: string;
begin
  if FOnlyTest then TextTranslated.Assign(TextSource) else
  begin
    url:='https://translate.googleapis.com/translate_a/single?client=gtx&sl='
         +FLangSource+'&tl='+FLangTarget+'&dt=t&q='+EncodeURL(TextSource.Text);
    http.execute(url,s);
    ParsujWinik(DecodeURL(s));
  end;
end;

function TGoogleTranslator.ExecuteString(aLangFrom, aLangTo, AString: string
  ): string;
var
  url: string;
  s: string;
begin
  if FOnlyTest then result:=AString else
  begin
    url:='https://translate.googleapis.com/translate_a/single?client=gtx&sl='+aLangFrom+'&tl='+aLangTo+'&dt=t&q='+EncodeURL(AString);
    http.execute(url,s);
    //url:='https://translate.google.pl/?hl=pl#view=home&op=translate&sl='+aLangFrom+'&tl='+aLangTo+'&text='+EncodeURL(AString);
    //http.execute_get_post(url,s);
    result:=ParsujWinikStr(DecodeURL(s));
  end;
end;

end.
