unit SymfoniaParser;

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFNDEF FPC AND $IFDEF MSWINDOWS}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE LAZARUS}
{$ENDIF}

{$IFDEF LAZARUS}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources;

type
  TBeforeAfterReadEvent = procedure(Sender: TObject) of object;
  TReadEvent = procedure(Sender: TObject; poziom: integer; adres,klucz,zmienna,wartosc: string; var Stopped:boolean) of object;
  TErrorEvent = procedure(Sender: TObject; ERR: integer; sERR: string) of object;
  TOnProgress = procedure(Sender: TObject; vMax, vPos: integer) of object;

  { TSymfoniaParser }

  TSymfoniaParser = class(TComponent)
  private
    FOnProgress: TOnProgress;
    FFilename: string;
    FNULL: boolean;
    FOnAfterRead: TBeforeAfterReadEvent;
    FOnBeforeRead: TBeforeAfterReadEvent;
    FOnError: TErrorEvent;
    FOnRead: TReadEvent;
    function Count: integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean;
  published
    { Published declarations }
    property Filename: string read FFilename write FFilename;
    property LastNullRead: boolean read FNULL write FNULL default false;
    property OnBeforeRead: TBeforeAfterReadEvent read FOnBeforeRead write FOnBeforeRead;
    property OnRead: TReadEvent read FOnRead write FOnRead;
    property OnAfterRead: TBeforeAfterReadEvent read FOnAfterRead write FOnAfterRead;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
  end;

procedure Register;

implementation

uses
  {$IFDEF LAZARUS}
  lconvencoding, parsery_utf8;
  {$ELSE}
  Komunikaty_Delphi;
  {$ENDIF}

var
  zm_stop: boolean;
  _MAX: integer;

procedure Register;
begin
  {$IFDEF LAZARUS}
  {$I symfoniaparser_icon.lrs}
  {$ENDIF}
  RegisterComponents('Misc',[TSymfoniaParser]);
end;

function ConvertISO(s: string): string;
begin
  {$IFDEF LAZARUS}
  result:=CP1250ToUTF8(s);
  {$ELSE}
  result:=s;
  {$ENDIF}
end;

function GetLineToStr(s:string;l:integer;separator:char):string;
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

{ TSymfoniaParser }

constructor TSymfoniaParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSymfoniaParser.Destroy;
begin
  inherited Destroy;
end;

function TSymfoniaParser.Execute: boolean;
var
  f: text;
  zm_count,licznik,pom,i: integer;
  adres,klucz,s,s1,s2: string;
begin
  if Assigned(FOnBeforeRead) then FOnBeforeRead(Self);
  if Assigned(FOnProgress) then
  begin
    _MAX:=Count;
    if _MAX=-1 then
    begin
      result:=false;
      exit;
    end;
    FOnProgress(Self,_MAX,0);
  end;
  (* otwarcie pliku do czytania *)
  AssignFile(f,FFilename);
  try
    reset(f);
  except
    on e:exception do
    begin
      if Assigned(FOnError) then FOnError(Self,1,ConvertISO(e.Message));
      result:=false;
      exit;
    end;
  end;
  zm_stop:=false;
  zm_count:=0;
  licznik:=0;
  adres:='';
  klucz:='';
  (* petla *)
  while not eof(f) do
  begin
    try
      inc(zm_count);
      readln(f,s);
      if Assigned(FOnProgress) then FOnProgress(Self,_MAX,zm_count);
    except
      on e:exception do
      begin
        if Assigned(FOnError) then FOnError(Self,1,ConvertISO(e.Message));
        inc(licznik);
        continue;
      end;
    end;
    if trim(s)='' then continue;
    s:=ConvertISO(s);
    (* analizuje rekord *)
    if pos('{',s)>0 then
    begin
      //dodanie sekcji
      s:=StringReplace(s,'{','',[]);
      klucz:=trim(s);
      adres:=adres+'/'+klucz;
      inc(licznik);
      continue;
    end;
    if pos('}',s)>0 then
    begin
      //usuniecie sekcji
      for i:=length(adres) downto 1 do if adres[i]='/' then
      begin
        delete(adres,i,100);
        break;
      end;
      klucz:='';
      for i:=length(adres) downto 1 do if adres[i]='/' then break else klucz:=adres[i]+klucz;
      dec(licznik);
      continue;
    end;
    //wlasciwy kod
    s1:=trim(GetLineToStr(s,1,'='));
    s2:=trim(GetLineToStr(s,2,'='));
    if Assigned(FOnRead) then FOnRead(self,licznik,adres,klucz,s1,s2,zm_stop);
    if zm_stop then break;
  end;
  (* dodatkowe puste uruchomienie metody OnRead *)
  if (not zm_stop) and FNULL and Assigned(FOnRead) then FOnRead(self,-1,'','','','',zm_stop);
  (* zamkniecie pliku *)
  closefile(f);
  if Assigned(FOnAfterRead) then FOnAfterRead(Self);
  result:=true;
end;

function TSymfoniaParser.Count: integer;
var
  f: text;
  r: integer;
  s: string;
begin
  AssignFile(f,FFilename);
  try
    reset(f);
  except
    on e:exception do
    begin
      if Assigned(FOnError) then FOnError(Self,1,ConvertISO(e.Message));
      result:=-1;
      exit;
    end;
  end;
  r:=0;
  while not eof(f) do
  begin
    readln(f,s);
    inc(r);
  end;
  closefile(f);
  result:=r;
end;

end.
