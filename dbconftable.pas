unit DBConfTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ZConnection, ZDataset;

type

  { TDBConfTable }

  TDBConfTable = class(TComponent)
  private
    FAutoCreate: boolean;
    FDatabase: TZConnection;
    FInfo: TStrings;
    FNullDelete: boolean;
    FSysPref: TStrings;
    FSysPrefActive: boolean;
    FTableName: string;
    create_db: TZQuery;
    get_db: TZQuery;
    ile_db: TZQuery;
    insert_db: TZQuery;
    update_db: TZQuery;
    delete_db: TZQuery;
    procedure SetSysPref(AValue: TStrings);
    procedure SetTableName(AValue: string);
    procedure InitTableName;
    function TestSysPref(nazwa: string): string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure RecoverySysPrefixes; //Odzyskuje nazwy, które stały się później prefiksami systemowymi.
    procedure Delete(zmienna:string);
    procedure SetString(zmienna:string; wartosc: string);
    procedure SetInteger(zmienna:string; wartosc: integer);
    procedure SetBoolean(zmienna:string; wartosc: boolean);
    procedure SetDate(zmienna:string; wartosc: TDate);
    procedure SetTime(zmienna:string; wartosc: TTime);
    function GetString(zmienna:string; domyslna: string = ''): string;
    function GetInteger(zmienna:string; domyslna: integer = 0): integer;
    function GetBoolean(zmienna:string; domyslna: boolean = false): boolean;
    function GetDate(zmienna:string): TDate;
    function GetTime(zmienna:string): TTime;
  published
    property Info: TStrings read FInfo;
    property Database: TZConnection read FDatabase write FDatabase;
    property TableName: string read FTableName write SetTableName;
    property AutoCreate: boolean read FAutoCreate write FAutoCreate default false;
    property StringNullAutoDelete: boolean read FNullDelete write FNullDelete default false; //Załączenie automatycznie usuwa puste stringi.
    property SysPrefixLabels: TStrings read FSysPref write SetSysPref; //Lista zmiennych, które w każdym systemie będą miały swoją unikalną nazwę.
    property SysPrefixActive: boolean read FSysPrefActive write FSysPrefActive default false;
  end;

procedure Register;

implementation

var
  _sys_pref: string;

procedure Register;
begin
  {$I dbconftable_icon.lrs}
  RegisterComponents('Zeos Access',[TDBConfTable]);
end;

{ TDBConfTable }

procedure TDBConfTable.SetTableName(AValue: string);
begin
  if FTableName=AValue then Exit;
  if AValue='' then FTableName:='<auto>' else FTableName:=AValue;
  InitTableName;
end;

procedure TDBConfTable.SetSysPref(AValue: TStrings);
begin
  FSysPref.Assign(AValue);
end;

procedure TDBConfTable.InitTableName;
var
  s: string;
begin
  if FTableName='<auto>' then s:='ustawienia' else s:=FTableName;
  if s='' then s:='ustawienia';
  create_db.SQL.Clear;
  get_db.SQL.Clear;
  ile_db.SQL.Clear;
  insert_db.SQL.Clear;
  update_db.SQL.Clear;
  create_db.SQL.Add('create table '+s+' (id integer primary key,zmienna varchar(50),wartosc text)');
  delete_db.SQL.Add('delete from '+s+' where zmienna=:zmienna');
  get_db.SQL.Add('select wartosc from '+s+' where zmienna=:zmienna');
  ile_db.SQL.Add('select count(zmienna) as ile from '+s+' where zmienna=:zmienna');
  insert_db.SQL.Add('insert into '+s+' (zmienna,wartosc) values (:zmienna,:wartosc)');
  update_db.SQL.Add('update '+s+' set wartosc=:wartosc where zmienna=:zmienna');
end;

function TDBConfTable.TestSysPref(nazwa: string): string;
var
  i: integer;
  b: boolean;
begin
  (* sprawdzam, czy nazwa występuje na liście prefixów systemowych *)
  b:=false;
  for i:=0 to FSysPref.Count-1 do if FSysPref[i]=nazwa then
  begin
    b:=true;
    break;
  end;
  if b then result:=_sys_pref+nazwa else result:=nazwa;
end;

constructor TDBConfTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInfo:=TStringList.Create;
  FInfo.Add('W celu ręcznego założenia tabeli wykonaj:');
  FInfo.Add('');
  FInfo.Add('create table ustawienia');
  FInfo.Add('(');
  FInfo.Add('  id integer primary key,');
  FInfo.Add('  zmienna varchar(50),');
  FInfo.Add('  wartosc text');
  FInfo.Add(');');
  FSysPrefActive:=false;
  FSysPref:=TStringList.Create;
  FTableName:='<auto>';
  FAutoCreate:=false;
  FNullDelete:=false;
  create_db:=TZQuery.Create(self);
  delete_db:=TZQuery.Create(self);
  get_db:=TZQuery.Create(self);
  ile_db:=TZQuery.Create(self);
  insert_db:=TZQuery.Create(self);
  update_db:=TZQuery.Create(self);
  create_db.Connection:=FDatabase;
  delete_db.Connection:=FDatabase;
  get_db.Connection:=FDatabase;
  ile_db.Connection:=FDatabase;
  insert_db.Connection:=FDatabase;
  update_db.Connection:=FDatabase;
  {$IFDEF LINUX}
  _sys_pref:='$L';
  {$ENDIF}
  {$IFDEF WINDOWS}
  _sys_pref:='$W';
  {$ENDIF}
  {$IFDEF FREEBSD}
  _sys_pref:='$F';
  {$ENDIF}
  InitTableName;
end;

destructor TDBConfTable.Destroy;
begin
  FInfo.Free;
  FSysPref.Free;
  create_db.Free;
  delete_db.Free;
  get_db.Free;
  ile_db.Free;
  insert_db.Free;
  update_db.Free;
  inherited Destroy;
end;

procedure TDBConfTable.Init;
var
  s: string;
  b: boolean;
begin
  create_db.Connection:=FDatabase;
  delete_db.Connection:=FDatabase;
  get_db.Connection:=FDatabase;
  ile_db.Connection:=FDatabase;
  insert_db.Connection:=FDatabase;
  update_db.Connection:=FDatabase;
  InitTableName;
  if FAutoCreate then
  begin
    try
      GetString('$TEST$');
      b:=false;
    except
      b:=true;
    end;
    if b then create_db.ExecSQL;
  end;
end;

procedure TDBConfTable.RecoverySysPrefixes;
var
  s: string;
  query: TZQuery;
  i: integer;
begin
  if not FSysPrefActive then exit;
  query:=TZQuery.Create(self);
  try
    if FTableName='<auto>' then s:='ustawienia' else s:=FTableName;
    if s='' then s:='ustawienia';
    query.Connection:=FDatabase;
    query.SQL.Add('update '+s);
    query.SQL.Add('set zmienna=:nowa_nazwa');
    query.SQL.Add('where zmienna=:stara_nazwa');
    for i:=0 to FSysPref.Count-1 do
    begin
      query.ParamByName('stara_nazwa').AsString:=FSysPref[i];
      query.ParamByName('nowa_nazwa').AsString:=_sys_pref+FSysPref[i];
      query.ExecSQL;
    end;
  finally
    query.Free;
  end;
end;

procedure TDBConfTable.Delete(zmienna: string);
begin
  if FSysPrefActive then zmienna:=TestSysPref(zmienna);
  delete_db.ParamByName('zmienna').AsString:=zmienna;
  delete_db.ExecSQL;
end;

procedure TDBConfTable.SetString(zmienna: string; wartosc: string);
var
  l: integer;
begin
  if FSysPrefActive then zmienna:=TestSysPref(zmienna);
  if (wartosc='') and FNullDelete then
  begin
    Delete(zmienna);
    exit;
  end;
  ile_db.ParamByName('zmienna').AsString:=zmienna;
  ile_db.Open;
  l:=ile_db.FieldByName('ile').AsInteger;
  ile_db.Close;
  if l=0 then
  begin
    insert_db.ParamByName('zmienna').AsString:=zmienna;
    insert_db.ParamByName('wartosc').AsString:=wartosc;
    insert_db.ExecSQL;
  end else begin
    update_db.ParamByName('zmienna').AsString:=zmienna;
    update_db.ParamByName('wartosc').AsString:=wartosc;
    update_db.ExecSQL;
  end;
end;

procedure TDBConfTable.SetInteger(zmienna: string; wartosc: integer);
begin
  SetString(zmienna,IntToStr(wartosc));
end;

procedure TDBConfTable.SetBoolean(zmienna: string; wartosc: boolean);
begin
  if wartosc then SetString(zmienna,'1') else SetString(zmienna,'0');
end;

procedure TDBConfTable.SetDate(zmienna: string; wartosc: TDate);
begin
  SetString(zmienna,FormatDateTime('yyyy-mm-dd',wartosc));
end;

procedure TDBConfTable.SetTime(zmienna: string; wartosc: TTime);
begin
  SetString(zmienna,FormatDateTime('hh:nn:ss',wartosc));
end;

function TDBConfTable.GetString(zmienna: string; domyslna: string): string;
var
  s: string;
begin
  if FSysPrefActive then zmienna:=TestSysPref(zmienna);
  get_db.ParamByName('zmienna').AsString:=zmienna;
  get_db.Open;
  try
    if get_db.FieldByName('wartosc').IsNull then s:=domyslna else s:=get_db.FieldByName('wartosc').AsString;
  except
    s:=domyslna;
  end;
  get_db.Close;
  result:=s;
end;

function TDBConfTable.GetInteger(zmienna: string; domyslna: integer): integer;
begin
  try
    result:=StrToInt(GetString(zmienna,IntToStr(domyslna)));
  except
    result:=domyslna;
  end;
end;

function TDBConfTable.GetBoolean(zmienna: string; domyslna: boolean): boolean;
begin
  result:=GetString(zmienna,'0')='1';
end;

function TDBConfTable.GetDate(zmienna: string): TDate;
var
  FS: TFormatSettings;
begin
  FS.ShortDateFormat:='y/m/d';
  FS.DateSeparator:='-';
  try
    result:=StrToDate(GetString(zmienna),FS);
  except
  end;
end;

function TDBConfTable.GetTime(zmienna: string): TTime;
begin
  try
    result:=StrToTime(GetString(zmienna));
  except
  end;
end;

end.
