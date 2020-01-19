unit CsvParser;

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

{$IFDEF DELPHI}
{$R TParsery.dcr}
{$ENDIF}

uses
  Classes, SysUtils,
  {$IFDEF LAZARUS}
  LResources,
  {$ENDIF}
  Forms, Controls, Graphics, Dialogs;

type
  TBeforeAfterReadEvent = procedure(Sender: TObject) of object;
  TReadEvent = procedure(Sender: TObject; NumberRec,PosRec: integer; sName, sValue: string; var Stopped:boolean) of object;
  TErrorEvent = procedure(Sender: TObject; ERR: integer; sERR: string) of object;
  TOnProgress = procedure(Sender: TObject; vMax, vPos: integer) of object;

  { TCsvParser }

  TCsvParser = class(TComponent)
  private
    { Private declarations }
    FFilename: string;
    FFirstRecMenu: boolean;
    FOnProgress: TOnProgress;
    FSeparator,FTextSeparator: char;
    FNULL: boolean;
    FOnBeforeRead: TBeforeAfterReadEvent;
    FOnRead: TReadEvent;
    FOnAfterRead: TBeforeAfterReadEvent;
    FOnError: TErrorEvent;
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
    property FirstRecMenu: boolean read FFirstRecMenu write FFirstRecMenu default false;
    property Separator: char read FSeparator write FSeparator default ',';
    property TextSeparator: char read FTextSeparator write FTextSeparator default '"';
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

procedure Register;
begin
  {$IFDEF LAZARUS}
  {$I csvparser_icon.lrs}
  {$ENDIF}
  RegisterComponents('Misc',[TCsvParser]);
end;

function ConvertISO(s: string): string;
begin
  {$IFDEF LAZARUS}
  result:=s;
  {$ELSE}
  result:=UTF8Encode(s);
  {$ENDIF}
end;

function GetLineToStr(s:string;l:integer;separator,textseparator:char;wynik:string=''):string;
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
    if s[i]=textseparator then b:=not b;
    if (not b) and (s[i]=separator) then inc(ll);
    if ll=l then break;
  end;
  if ll=1 then dec(i);
  delete(s,1,i);
  b:=false;
  for i:=1 to length(s) do
  begin
    if s[i]=textseparator then b:=not b;
    if (not b) and (s[i]=separator) then break;
  end;
  delete(s,i,dl);
  if (s<>'') and (s[1]=textseparator) then
  begin
    delete(s,1,1);
    delete(s,length(s),1);
  end;
  if s='' then s:=wynik;
  result:=s;
end;

function GetLineCount(s: string; separator,textseparator: char): integer;
var
  ll,i,ost: integer;
begin
  //licze separatory by zdiagnozowac maksymalna ilosc sekcji
  ll:=1;
  for i:=1 to length(s) do if s[i]=separator then inc(ll);
  //szukam ostatniej nie bialej zawartosci sekcji
  ost:=0;
  for i:=ll downto 1 do if GetLineToStr(s,i,separator,textseparator)<>'' then
  begin
    ost:=i;
    break;
  end;
  //wyjscie
  result:=ost;
end;

{ TCsvParser }

function TCsvParser.Count: integer;
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
      if Assigned(FOnError) then FOnError(Self,1,{$IFDEF LAZARUS}ConvertISO(e.Message){$ELSE}e.Message{$ENDIF});
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

constructor TCsvParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //init
  FFirstRecMenu:=false;
  FSeparator:=',';
  FTextSeparator:='"';
  FNULL:=false;
end;

destructor TCsvParser.Destroy;
begin
  inherited Destroy;
end;

function TCsvParser.Execute: boolean;
var
  b: boolean;
  f: Text;
  zm_menu,s: string;
  _MAX,zm_count,licznik,i,razem: integer;
begin
  _MAX:=0;
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
  b:=FFirstRecMenu;
  zm_menu:='';
  AssignFile(f,FFilename);
  try
    reset(f);
  except
    on e:exception do
    begin
      if Assigned(FOnError) then {$IFDEF LAZARUS}FOnError(Self,1,ConvertISO(e.Message){$ELSE}FOnError(Self,1,e.Message{$ENDIF} );
      result:=false;
      exit;
    end;
  end;
  zm_stop:=false;
  zm_count:=0;
  licznik:=1;
  while not eof(f) do
  begin
    try
      readln(f,s);
      {$IFDEF LAZARUS}
      s:=ConvertISO(s);
      {$ENDIF}
      inc(zm_count);
      if Assigned(FOnProgress) then FOnProgress(Self,_MAX,zm_count);
    except
      on e:exception do
      begin
        if Assigned(FOnError) then FOnError(Self,1,{$IFDEF LAZARUS}ConvertISO(e.Message){$ELSE}e.Message{$ENDIF});
        inc(licznik);
        inc(zm_count);
        if Assigned(FOnProgress) then FOnProgress(Self,_MAX,zm_count);
        continue;
      end;
    end;
    if b then
    begin
      zm_menu:=s;
      b:=false;
      continue;
    end;
    razem:=GetLineCount(s,FSeparator,FTextSeparator);
    if Assigned(FOnRead) then for i:=1 to razem do
    begin
      FOnRead(self,licznik,i,GetLineToStr(zm_menu,i,FSeparator,FTextSeparator),GetLineToStr(s,i,FSeparator,FTextSeparator),zm_stop);
      if zm_stop then break;
    end;
    if zm_stop then break;
    inc(licznik);
  end;
  if (not zm_stop) and FNULL and Assigned(FOnRead) then FOnRead(self,-1,0,'','',zm_stop);
  closefile(f);
  if Assigned(FOnAfterRead) then FOnAfterRead(Self);
  result:=true;
end;

end.
