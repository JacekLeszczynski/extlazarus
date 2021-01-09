unit ZMasterVersionDB;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Dialogs,
  ZConnection, ZDataset, XmlParser, ZSqlProcessor;

type

  TCreateEvent = procedure(Sender: TObject; TagNo: integer; var Stopped, NegationResult, ForceUpgrade: boolean) of object;
  TUpgradeEvent = procedure(Sender: TObject; TagNo, VerDB: integer; var Stopped: boolean) of object;
  TUpgradeAllEvent = procedure(Sender: TObject; TagNo, VerDB: integer; var LastDB: integer; var Stopped: boolean) of object;
  TAddingCodeEvent = procedure(Sender: TObject; TagNo: integer) of object;
  TAddingCode2Event = procedure(Sender: TObject; TagNo, VerDB, LastDB: integer) of object;
  TUpgradeTestEvent = procedure(Sender: TObject; var Stopped: boolean) of object;

  TEncodingFormat = (eUTF8,eWindows1250);
  TAlgorithmUpgrade = (auOneToStep,auAllToStep,auDES);

  { TZMasterVersionDB }

  TZMasterVersionDB = class(TComponent)
  private
    { Private declarations }
    FDebug: boolean;
    FEncode: TEncodingFormat;
    FDB: TZConnection;
    FNazwaTabeli, FNazwaPola, FSQLWhere, FPName, FPlik, FToken: string;
    FUpgradeTest: TUpgradeTestEvent;
    FVersion, FMasterID: integer;
    FSQLTables, FSQLViews, FSQLProcs, FSQLDrop, FSQLExec: TStrings;
    FCreate: TCreateEvent;
    FUpgrade: TUpgradeEvent;
    FUpgradeAll: TUpgradeAllEvent;
    FAlgorithmUpgrade: TAlgorithmUpgrade;
    FBlockUpgrade: boolean;
    Query1: TZQuery;
    Script: TZSQLProcessor;
    xml: TXmlParser;
    xml_query: TZSQLProcessor;
    FBeforeCreate,FAfterCreate: TAddingCodeEvent;
    FBeforeExecute,FAfterExecute: TAddingCodeEvent;
    FBeforeUpgrade: TAddingCodeEvent;
    FAfterUpgrade: TAddingCode2Event;
    procedure Init;
    procedure SetSQLTables(const AValue: TStrings);
    procedure SetSQLViews(const AValue: TStrings);
    procedure SetSQLProcs(const AValue: TStrings);
    procedure SetSQLDrop(const AValue: TStrings);
    procedure SetSQLExec(const AValue: TStrings);
    procedure UpgradeDES(VerDB:integer; var LastDB:integer; var Stopped:boolean);
    procedure XmlRead(Sender: TObject; poziom: integer; adres, klucz,
      zmienna, wartosc: string; var Stopped: boolean);
    function CreateAll(zm_tag:integer; var fu:boolean):boolean;
    function UpgradeAll(zm_tag,ver:integer):boolean;
    function UpdateVer(act_ver:integer):boolean;
    function ISOConverter(s: string): string;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(TagNo:integer):boolean;
    function Execute:boolean;
    function Drop:boolean;
    procedure GetError(var zm_ERR: integer; var zm_sERR: string);
    function IsError:boolean;
    function Test:boolean;
  published
    { Published declarations }
    property DBEncodeChars: TEncodingFormat read FEncode write FEncode default eUTF8;
    property DB_Connection: TZConnection read FDB write FDB;
    property TableName: string read FNazwaTabeli write FNazwaTabeli;
    property FieldName: string read FNazwaPola write FNazwaPola;
    property Version: integer read FVersion write FVersion;
    property AlgorithmUpgrade: TAlgorithmUpgrade read FAlgorithmUpgrade write FAlgorithmUpgrade default auOneToStep;
    property DESProjectName: string read FPName write FPName;
    property DESMasterID: integer read FMasterID write FMasterID;
    property DESFileName: string read FPlik write FPlik;
    property DESToken: string read FToken write FToken;
    property SQLTables: TStrings read FSQLTables write SetSQLTables;
    property SQLViews: TStrings read FSQLViews write SetSQLViews;
    property SQLProcs: TStrings read FSQLProcs write SetSQLProcs;
    property SQLWhere: string read FSQLWhere write FSQLWhere;
    property SQLDrop: TStrings read FSQLDrop write SetSQLDrop;
    property SQLExec: TStrings read FSQLExec write SetSQLExec;
    property BlockUpgrade: boolean read FBlockUpgrade write FBlockUpgrade default false;
    property Debug: boolean read FDebug write FDebug default false;
    property OnCreate: TCreateEvent read FCreate write FCreate;
    property OnUpgrade: TUpgradeEvent read FUpgrade write FUpgrade;
    property OnUpgradeAll: TUpgradeAllEvent read FUpgradeAll write FUpgradeAll;
    property OnBeforeCreate: TAddingCodeEvent read FBeforeCreate write FBeforeCreate;
    property OnAfterCreate: TAddingCodeEvent read FAfterCreate write FAfterCreate;
    property OnBeforeExecute: TAddingCodeEvent read FBeforeExecute write FBeforeExecute;
    property OnAfterExecute: TAddingCodeEvent read FAfterExecute write FAfterExecute;
    property OnBeforeUpgrade: TAddingCodeEvent read FBeforeUpgrade write FBeforeUpgrade;
    property OnAfterUpgrade: TAddingCode2Event read FAfterUpgrade write FAfterUpgrade;
    property OnPreUpgradeTest: TUpgradeTestEvent read FUpgradeTest write FUpgradeTest;
  end;

procedure Register;

implementation

uses
  ecode_unit, lconvencoding, ZDbcIntfs, ZScriptParser;

var
  ERR: integer;
  sERR: string;
  _B: boolean;
  gpostgres,gmysql,gmariadb,gfirebird: boolean;

var {upgrades}
  upgrade: record
    b_master: boolean;
    master,min,max,post: integer;
    id,id_max,nr: integer;
  end;

procedure Register;
begin
  {$I zmasterversiondb_icon.lrs}
  RegisterComponents('Zeos Access',[TZMasterVersionDB]);
end;

function czy_sekcja(find_str,full_str:string):boolean;
var
  s,pom: string;
  b: boolean;
  i: integer;
begin
  s:=find_str;
  s:=StringReplace(s,'[','',[rfReplaceAll]);
  s:=StringReplace(s,']','',[rfReplaceAll]);
  i:=0;
  b:=false;
  while true do
  begin
    inc(i);
    pom:=GetLineToStr(s,i,',');
    if pom='' then break;
    b:=pos(upcase(pom),upcase(full_str))>0;
    if b then break;
  end;
  result:=b;
end;

procedure normalize_postgres(var s:string);
var
  a: integer;
begin
  (* tinyint to smallint *)
  s:=StringReplace(s,'tinyint','smallint',[rfReplaceAll,rfIgnoreCase]);
  (* blob to bytea *)
  s:=StringReplace(s,'blob','bytea',[rfReplaceAll,rfIgnoreCase]);
  (* remove unsigned *)
  a:=pos('unsigned',s);
  delete(s,a-1,9);
end;

procedure normalize_mysql(var s:string);
var
  a,b: integer;
begin
  (* serial to int autoincrement *)
  a:=pos('serial',s);
  if a>0 then
  begin
    s:=StringReplace(s,'serial','int',[rfReplaceAll,rfIgnoreCase]);
    a:=pos(',',s);
    if a=0 then a:=pos(';',s);
    if a=0 then s:=s+' auto_increment' else insert(' auto_increment',s,a);
  end;
end;

procedure normalize_mariadb(var s:string);
var
  a,b: integer;
begin
  (* serial to int autoincrement *)
  a:=pos('serial',s);
  if a>0 then
  begin
    s:=StringReplace(s,'serial','int',[rfReplaceAll,rfIgnoreCase]);
    a:=pos(',',s);
    if a=0 then a:=pos(';',s);
    if a=0 then s:=s+' auto_increment' else insert(' auto_increment',s,a);
  end;
end;

procedure normalize_firebird(var s:string);
var
  a: integer;
begin
  (* serial to int but NO autoincrement *)
  s:=StringReplace(s,'serial','int',[rfReplaceAll,rfIgnoreCase]);
  (* tinyint to smallint *)
  s:=StringReplace(s,'tinyint','smallint',[rfReplaceAll,rfIgnoreCase]);
  (* remove unsigned *)
  a:=pos('unsigned',s);
  delete(s,a-1,9);
end;

{ TMasterVersionDB }

procedure TZMasterVersionDB.Init;
begin
  FEncode:=eUTF8;
  FVersion:=0;
  FMasterID:=0;
  FAlgorithmUpgrade:=auOneToStep;
  FBlockUpgrade:=false;
  FDebug:=false;
end;

procedure TZMasterVersionDB.SetSQLTables(const AValue: TStrings);
begin
  FSQLTables.Assign(AValue);
end;

procedure TZMasterVersionDB.SetSQLViews(const AValue: TStrings);
begin
  FSQLViews.Assign(AValue);
end;

procedure TZMasterVersionDB.SetSQLProcs(const AValue: TStrings);
begin
  FSQLProcs.Assign(AValue);
end;

procedure TZMasterVersionDB.SetSQLDrop(const AValue: TStrings);
begin
  FSQLDrop.Assign(AValue);
end;

procedure TZMasterVersionDB.SetSQLExec(const AValue: TStrings);
begin
  FSQLExec.Assign(AValue);
end;

procedure TZMasterVersionDB.UpgradeDES(VerDB: integer; var LastDB: integer;
  var Stopped: boolean);
begin
  { sprawdzenie pliku z poprawkami }
  if not FileExists(FPlik) then
  begin
    ERR:=2;
    sERR:='Brak pliku z poprawkami, aktualizacja przerwana, struktury w dalszym ciągu mogą nie być aktualne.';
    LastDB:=VerDB;
    Stopped:=true;
    exit;
  end;
  { wczytuję sekcje }
  gpostgres:=czy_sekcja('postgres',FDB.Protocol);
  gmysql:=czy_sekcja('mysql',FDB.Protocol);
  gmariadb:=czy_sekcja('MariaDB',FDB.Protocol);
  gfirebird:=czy_sekcja('firebird',FDB.Protocol);
  { wygenerowanie poprawek }
  upgrade.master:=FMasterID;
  upgrade.min:=VerDB+1;
  upgrade.max:=LastDB;
  upgrade.post:=VerDB;
  if upgrade.post=0 then inc(upgrade.post);
  upgrade.b_master:=false;
  upgrade.id:=0;
  upgrade.id_max:=0;
  upgrade.nr:=0;
  try
    xml_query:=TZSQLProcessor.Create(nil);
    xml_query.Connection:=FDB;
    xml_query.Script.Clear;
    xml:=TXmlParser.Create(nil);
    xml.Contener:=crDES;
    xml.Encoding:=eAuto;
    xml.Filename:=FPlik;
    xml.Token:=FToken;
    xml.OnRead:=@XmlRead;
    xml.Execute;
    LastDB:=upgrade.post;
  finally
    xml_query.Free;
    xml.Free;
  end;
end;

var
  TRANSACTION: boolean = false;
  global_kopiuj: boolean = true;

procedure TZMasterVersionDB.XmlRead(Sender: TObject; poziom: integer; adres,
  klucz, zmienna, wartosc: string; var Stopped: boolean);
var
  s,pom,cpom: string;
  dl: boolean;
begin
  { zaczynam }
  if (adres='/root') and (zmienna='produkt') and (wartosc<>FPName) then
  begin
    //Plik poprawek jest z innego projektu!
    ERR:=3;
    sERR:='Wykryto niewłaściwy plik poprawek, najprawdopodobniej pochodzi z innego projektu, przerywam aktualizację.';
    Stopped:=true;
    exit;
  end;
  if (adres='/root/zestawienia/nr') and (zmienna='count') and (not upgrade.b_master) then
  begin
    try
      upgrade.id_max:=StrToInt(wartosc);
    except
      upgrade.id_max:=0;
    end;
  end;
  if (adres='/root/zestawienia/nr') and (zmienna='') and (not upgrade.b_master) then upgrade.b_master:=StrToInt(wartosc)=upgrade.master;
  { poprawki }
  if adres='/root/poprawki/master' then
  begin
    if not upgrade.b_master then
    begin
      //Brak gałęzi!
      ERR:=4;
      sERR:='W pliku aktualizacji brakuje wymaganej wersji gałęzi, przerywam aktualizację.';
      Stopped:=true;
      exit;
    end;
    upgrade.id:=StrToInt(wartosc);
  end;
  if adres='/root/poprawki/nr' then upgrade.nr:=StrToInt(wartosc);

  s:=trim(wartosc);
  dl:=pos('delimiter',s)>0;

  if s<>'' then if s[1]='[' then
  begin
    if upcase(s)='[ALL]' then global_kopiuj:=true else global_kopiuj:=czy_sekcja(s,FDB.Protocol);
  end;

  (* dodajemy wiersz do sql-a *)
  if (adres='/root/poprawki/wiersz') and
     (upgrade.id=upgrade.master) and
     (upgrade.nr=upgrade.post+1) and
     (not dl) and
     (s<>'♫') and
     (s<>'GO') and
     (s[1]<>'[') and
     (s<>'TRANSACTION') and global_kopiuj then xml_query.Script.Add(ISOConverter(xml.UnlockString(wartosc,true)));

  if (upgrade.id=upgrade.master) and (upgrade.nr=upgrade.post+1) then
  begin
    if (adres='/root/poprawki/wiersz') and dl then
    begin
      pom:=GetLineToStr(xml.UnlockString(s,true),2,' ');
      xml_query.Delimiter:=pom;
      xml_query.DelimiterType:=dtDelimiter;
    end else if (not TRANSACTION) and (adres='/root/poprawki/wiersz') and (trim(wartosc)='TRANSACTION') then
    begin
      //Daną poprawkę wykonujemy w transakcji
      FDB.TransactIsolationLevel:=tiReadCommitted;
      FDB.StartTransaction;
      TRANSACTION:=true;
    end else if (((adres='/root/poprawki/wiersz') and (trim(wartosc)='GO')) or (adres='/root/poprawki/X')) then
    begin
      //wykonujemy poprawkę!
      if xml_query.Script.Count>0 then
      begin
        try
          { wykonuję konwersję }
          cpom:=xml_query.Script.Text;
          if gpostgres then normalize_postgres(cpom);
          if gmysql then normalize_mysql(cpom);
          if gmariadb then normalize_mariadb(cpom);
          if gfirebird then normalize_firebird(cpom);
          xml_query.Script.Clear;
          xml_query.Script.Add(cpom);
          { konwersja wykonana }
          if FDebug then xml_query.Script.SaveToFile(MyDir('MDB_UPGRADE.SQL'));
          xml_query.Execute;
          xml_query.Delimiter:=';';
          xml_query.DelimiterType:=dtDefault;
        except
          if TRANSACTION then
          begin
            FDB.Rollback;
            FDB.TransactIsolationLevel:=tiNone;
            TRANSACTION:=false;
          end;
          raise;
        end;
      end;
      if klucz='X' then
      begin
        upgrade.post:=upgrade.nr;
        _B:=UpdateVer(upgrade.nr);
        if TRANSACTION then
        begin
          if _B then
          begin
            FDB.Commit;
            FDB.TransactIsolationLevel:=tiNone;
          end else begin
            FDB.Rollback;
            FDB.TransactIsolationLevel:=tiNone;
            Stopped:=true;
          end;
          TRANSACTION:=false;
        end;
      end;
      xml_query.Script.Clear;
      if upgrade.post=upgrade.max then Stopped:=true;
    end;
  end;
end;

function TZMasterVersionDB.CreateAll(zm_tag: integer; var fu: boolean): boolean;
var
  negation_result,stopped,force_upgrade: boolean;
  i: integer;
  sekcja,s: string;
  kopiuj: boolean;
  postgres,mysql,mariadb,firebird: boolean;
begin
  if Assigned(FBeforeCreate) then FBeforeCreate(self,zm_tag);
  stopped:=false;
  negation_result:=false;
  force_upgrade:=fu;
  postgres:=czy_sekcja('postgres',FDB.Protocol);
  mysql:=czy_sekcja('mysql',FDB.Protocol);
  mariadb:=czy_sekcja('MariaDB',FDB.Protocol);
  firebird:=czy_sekcja('firebird',FDB.Protocol);
  if Assigned(FCreate) then FCreate(self,zm_tag,stopped,negation_result,force_upgrade);
  fu:=force_upgrade;
  if not stopped then
  begin
    { zakładam całą strukturę jak leci }
    //SQL_TABLES
    if FSQLTables.Count>0 then
    begin
      Script.Script.Clear;
      sekcja:='[ALL]'; kopiuj:=true;
      for i:=1 to FSQLTables.Count do
      begin
        s:=FSQLTables[i-1];
        if s<>'' then if s[1]='[' then
        begin
          if upcase(s)='[ALL]' then kopiuj:=true else kopiuj:=czy_sekcja(s,FDB.Protocol);
          continue;
        end;
        if postgres then normalize_postgres(s);
        if mysql then normalize_mysql(s);
        if mariadb then normalize_mariadb(s);
        if firebird then normalize_firebird(s);
        if kopiuj then Script.Script.Add(ISOConverter(s));
      end;
      if FDebug then Script.Script.SaveToFile(MyDir('MDB_TABLES.SQL'));
      Script.Execute;
    end;
    //SQL_EXEC
    if FSQLExec.Count>0 then
    begin
      Script.Script.Clear;
      sekcja:='[ALL]'; kopiuj:=true;
      for i:=1 to FSQLExec.Count do
      begin
        s:=FSQLExec[i-1];
        if s<>'' then if s[1]='[' then
        begin
          if upcase(s)='[ALL]' then kopiuj:=true else kopiuj:=czy_sekcja(s,FDB.Protocol);
          continue;
        end;
        if kopiuj then Script.Script.Add(ISOConverter(s));
      end;
      if FDebug then Script.Script.SaveToFile(MyDir('MDB_EXEC.SQL'));
      Script.Execute;
    end;
    //SQL_VIEWS
    if FSQLViews.Count>0 then
    begin
      Script.Script.Clear;
      sekcja:='[ALL]'; kopiuj:=true;
      for i:=1 to FSQLViews.Count do
      begin
        s:=FSQLViews[i-1];
        if s<>'' then if s[1]='[' then
        begin
          if upcase(s)='[ALL]' then kopiuj:=true else kopiuj:=czy_sekcja(s,FDB.Protocol);
          continue;
        end;
        if kopiuj then Script.Script.Add(ISOConverter(s));
      end;
      if FDebug then Script.Script.SaveToFile(MyDir('MDB_VIEWS.SQL'));
      Script.Execute;
    end;
    //SQL_PROCS
    if FSQLProcs.Count>0 then
    begin
      Script.Script.Clear;
      sekcja:='[ALL]'; kopiuj:=true;
      for i:=1 to FSQLProcs.Count do
      begin
        s:=FSQLProcs[i-1];
        if s<>'' then
        begin
          if s[1]='[' then
          begin
            if s='[GO]' then
            begin
              Script.DelimiterType:=dtGo;
              continue;
            end else
            if upcase(s)='[ALL]' then kopiuj:=true else kopiuj:=czy_sekcja(s,FDB.Protocol);
            continue;
          end;
          if pos('delimiter',s)=1 then
          begin
            Script.Delimiter:=GetLineToStr(s,2,' ');
            Script.DelimiterType:=dtDelimiter;
            continue;
          end;
        end;
        if kopiuj then Script.Script.Add(ISOConverter(s));
      end;
      if FDebug then Script.Script.SaveToFile(MyDir('MDB_PROCS.SQL'));
      Script.Execute;
      Script.Delimiter:=';';
      Script.DelimiterType:=dtDefault;
    end;
    { Dołożenie rekordu wersji }
    Query1.SQL.Clear;
    Query1.SQL.Add('INSERT INTO '+FNazwaTabeli);
    if FSQLWhere='' then s:='' else s:=GetLineToStr(FSQLWhere,1,'=')+',';
    Query1.SQL.Add('('+s+FNazwaPola+')');
    Query1.SQL.Add('VALUES');
    if FSQLWhere='' then s:='' else s:=':ID,';
    Query1.SQL.Add('('+s+':VER)');
    if FSQLWhere<>'' then Query1.ParamByName('ID').AsInteger:=StrToInt(GetLineToStr(FSQLWhere,2,'='));
    Query1.ParamByName('VER').AsInteger:=FVersion;
    if FDebug then ShowMessage('Wykonuję poniższy skrypt:'+#13+Query1.SQL.Text);
    Query1.ExecSQL;
  end;
  if negation_result then stopped:=not stopped;
  if Assigned(FAfterCreate) then FAfterCreate(self,zm_tag);
  result:=not stopped;
end;

function TZMasterVersionDB.UpgradeAll(zm_tag, ver: integer): boolean;
var
  b,stopped: boolean;
  i,OutVerDB: integer;
begin
  _B:=true;
  b:=true;
  stopped:=false;
  if Assigned(FBeforeUpgrade) then FBeforeUpgrade(self,zm_tag);
  { Upgrade 'OneToStep' }
  if (not FBlockUpgrade) and (FAlgorithmUpgrade=auOneToStep) and Assigned(FUpgrade) then for i:=ver+1 to FVersion do
  begin
    FUpgrade(self,zm_tag,i,stopped);
    if stopped then break;
    b:=UpdateVer(i);
    if not b then break;
  end;
  { Upgrade 'AllToStep' }
  if (not FBlockUpgrade) and (FAlgorithmUpgrade=auAllToStep) and Assigned(FUpgradeAll) then
  begin
    OutVerDB:=FVersion;
    FUpgradeAll(self,zm_tag,ver,OutVerDB,stopped);
    if (OutVerDB>ver) and (OutVerDB<=FVersion) then b:=UpdateVer(OutVerDB);
    b:=b and (OutVerDB=FVersion) and (not stopped);
  end;
  { Upgrade 'DES' }
  if (not FBlockUpgrade) and (FAlgorithmUpgrade=auDES) then
  begin
    OutVerDB:=FVersion;
    UpgradeDES(ver,OutVerDB,stopped);
    if _B and (OutVerDB>ver) and (OutVerDB<=FVersion) then b:=UpdateVer(OutVerDB);
    b:=b and (OutVerDB=FVersion) and (not stopped);
  end;
  if Assigned(FAfterUpgrade) then FAfterUpgrade(self,zm_tag,OutVerDB,FVersion);
  result:=b;
end;

function TZMasterVersionDB.UpdateVer(act_ver:integer):boolean;
begin
  Query1.SQL.Clear;
  Query1.SQL.Add('update '+FNazwaTabeli);
  Query1.SQL.Add('set '+FNazwaPola+'=:VER');
  if FSQLWhere<>'' then Query1.SQL.Add('where '+FSQLWhere);
  Query1.ParamByName('VER').AsInteger:=act_ver;
  try
    Query1.ExecSQL;
    result:=true;
  except
    ERR:=1;
    sERR:='Błąd aktualizacji wersji.';
    result:=false;
  end;
end;

function TZMasterVersionDB.ISOConverter(s: string): string;
begin
  case FEncode of
    eWindows1250: result:=ConvertEncoding(s,'utf8','cp1250');
    else result:=s;
  end;
end;

constructor TZMasterVersionDB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQLTables:=TStringList.Create;
  FSQLViews:=TStringList.Create;
  FSQLProcs:=TStringList.Create;
  FSQLDrop:=TStringList.Create;
  FSQLExec:=TStringList.Create;
  Query1:=TZQuery.Create(nil);
  Script:=TZSQLProcessor.Create(nil);
  init;
end;

destructor TZMasterVersionDB.Destroy;
begin
  FSQLTables.Free;
  FSQLViews.Free;
  FSQLProcs.Free;
  FSQLDrop.Free;
  FSQLExec.Free;
  Query1.Free;
  Script.Free;
  inherited Destroy;
end;

function TZMasterVersionDB.Execute(TagNo: integer): boolean;
var
  ver: integer;
  b,upg_stopped,fu: boolean;
begin
  if Assigned(FBeforeExecute) then FBeforeExecute(self,TagNo);
  ERR:=0;
  sERR:='';
  Query1.Connection:=FDB;
  Script.Connection:=FDB;
  //1. Sprawdzam aktualny numer wersji bazy danych
  Query1.SQL.Clear;
  Query1.SQL.Add('select '+FNazwaPola+' from '+FNazwaTabeli);
  if FSQLWhere<>'' then Query1.SQL.Add('where '+FSQLWhere);
  try
    Query1.Open;
    if (Query1.IsEmpty) or (Query1.Fields[0].IsNull) then ver:=-1 else ver:=Query1.Fields[0].AsInteger;
    Query1.Close;
  except
    ver:=-1;
  end;
  b:=ver>-1;
  //2. zakładam lub aktualizuję strukturę bazy
  fu:=false;
  if ver=-1 then b:=CreateAll(TagNo,fu);
  if fu then ver:=0;
  if b and ((ver<>-1) and (ver<FVersion)) then
  begin
    upg_stopped:=false;
    if Assigned(FUpgradeTest) then FUpgradeTest(self,upg_stopped);
    if not upg_stopped then b:=UpgradeAll(TagNo,ver) else b:=false;
  end;
  if Assigned(FAfterExecute) then FAfterExecute(self,TagNo);
  result:=b;
end;

function TZMasterVersionDB.Execute: boolean;
begin
  result:=Execute(0);
end;

function TZMasterVersionDB.Drop: boolean;
var
  s,sekcja: string;
  kopiuj: boolean;
  i: integer;
begin
  ERR:=0;
  sERR:='';
  if FSQLDrop.Count>0 then
  begin
    Script.Script.Clear;
    sekcja:='[ALL]'; kopiuj:=true;
    for i:=1 to FSQLDrop.Count do
    begin
      s:=FSQLDrop[i-1];
      if s<>'' then if s[1]='[' then
      begin
        if upcase(s)='[ALL]' then kopiuj:=true else kopiuj:=czy_sekcja(s,FDB.Protocol);
        continue;
      end;
      if kopiuj then Script.Script.Add(ISOConverter(s));
    end;
    Script.Execute;
  end;
end;

procedure TZMasterVersionDB.GetError(var zm_ERR: integer; var zm_sERR: string);
begin
  zm_ERR:=ERR;
  zm_sERR:=sERR;
end;

function TZMasterVersionDB.IsError: boolean;
begin
  result:=ERR>0;
end;

(* Sprawdzam czy struktura jest aktualna *)
function TZMasterVersionDB.Test: boolean;
var
  ver: integer;
begin
  Query1.Connection:=FDB;
  Query1.SQL.Clear;
  Query1.SQL.Add('select '+FNazwaPola+' from '+FNazwaTabeli);
  if FSQLWhere<>'' then Query1.SQL.Add('where '+FSQLWhere);
  try
    Query1.Open;
    if (Query1.IsEmpty) or (Query1.Fields[0].IsNull) then ver:=-1 else ver:=Query1.Fields[0].AsInteger;
    Query1.Close;
  except
    ver:=-1;
  end;
  result:=ver>=FVersion;
end;

end.
