unit DBSchemaSync_postgres;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, DB, ZConnection, ZDataset, ZSqlProcessor;

type
  TNoDropObjects = (nnTables, nnColumns);
  TListNoDropObjects = set of TNoDropObjects;
  TDataSyncOption = (dsNone, dsNormal, dsOnlySql, dsOnlyScript);
  TStatusEvent = procedure(Sender: TObject; ATyp: integer; AOper: char; ATitle, ANazwa, ARodzaj: string; AProgress: boolean; AMax, APosition: integer) of object;
  TDefaultSchemaEvent = procedure(Sender: TObject) of object;
  TSyncDataExecuteEvent = procedure(Sender: TObject; ATag: integer; AQuerySql: string) of object;

  { TDBSchemaSyncPostgres }

  TDBSchemaSyncPostgres = class(TComponent)
  private
    { Private declarations }
    dbsql: string;
    FAfterSave: TDefaultSchemaEvent;
    FAfterSync: TDefaultSchemaEvent;
    FBeforeSave: TDefaultSchemaEvent;
    FBeforeSync: TDefaultSchemaEvent;
    FDataSync: TStrings;
    FDataSyncFooter: TStrings;
    FDataSyncHeader: TStrings;
    FDataSyncOption: TDataSyncOption;
    FGrantRemember: boolean;
    FNoDropObjects: TListNoDropObjects;
    FRequest: TStrings;
    FStatusEvent: TStatusEvent;
    FSToSqlite: boolean;
    FSyncDataExecute: TSyncDataExecuteEvent;
    FSyncLog: boolean;
    proto: byte; //1=mysql, 2=postgresql
    FDB,sdb,sdb2: TZConnection;
    FZnacznikCzasu: boolean;
    q1,q2,qq: TZQuery;
    sq1,sq2,sq22: TZQuery;
    list,gens,przywileje1,przywileje2,lastindexes: TStringList;
    procedure DeleteStrings(zrodlo,cel: TStrings);
    procedure AppendStrings(zrodlo,cel: TStrings;obj:string;opis:string='';ignore:boolean=false);
    procedure AppendStrings(cel: TStrings;opis:string);
    procedure AppendStrings(opis:string);
    procedure SetDataSync(AValue: TStrings);
    procedure SetDataSyncFooter(AValue: TStrings);
    procedure SetDataSyncHeader(AValue: TStrings);
    procedure SetRequest(AValue: TStrings);
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    procedure StartTransaction2;
    procedure Commit2;
    procedure Rollback2;
    function odczytaj_znacznik_czasu(s: string; var czas: TDateTime): boolean;
    function sync_delete:boolean;
    function sync_table:boolean;
    function test_ref(nazwa:string;cialo:TStrings):boolean;
    function test_table(indeks:integer;nazwa:string;cialo,sekwencje:TStrings):boolean;
    function akt_table(nazwa:string;s1,s2:TStrings):boolean;
    function akt_indeksy_delete_postgres(nazwa:string):boolean;
    function akt_indeksy_create_postgres(nazwa:string):boolean;
    function akt_widoki_delete_postgres:boolean;
    function akt_widoki_create_postgres:boolean;
    function sync_event:boolean;
    procedure usun_zaleznosci(tabela,kolumna:string);
    procedure get_alter_table_postgres(nazwa,s1,s2:string;var s_pole,s_null,s_default:string);
    procedure postgres_TableStructAppendKeys(s1,s2:string;schema:TStrings);
    (* tools *)
    procedure ustaw_plik_skryptu(FilenameDB:string='');
    procedure czytaj_przywileje_postgres(tabele,przywileje:TStrings;tabela:string='');
    procedure zapisz_przywileje_postgres(tabele,przywileje:TStrings;tabela:string='');
    procedure export_data_table(tabela:string);
    procedure import_data_table;
    function FuncPostgresToLanguage(definicja:string):string;
    function PostgresIndexToFullName(s1,s2:string):string;
    procedure ZapiszSkryptDoPliku(filename: string; SyncLogTest: boolean = false);
    procedure last_indeksy_create_postgres;
  protected
    { Protected declarations }
  public
    { Public declarations }
    err: integer;
    error: string;
    log: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure init;
    procedure ShowLanguagesPostgres(schema,cialo: TStrings);
    procedure ShowSchemasPostgres(schema,cialo: TStrings);
    procedure ShowTypesPostgres(nazwa,cialo: TStrings);
    procedure ShowTables(tables: TStrings);
    procedure ShowTablesPostgres(tables: TStrings);
    procedure ShowViewsPostgres(tables: TStrings);
    procedure ShowTriggers(nazwa,cialo: TStrings);
    procedure ShowTriggersPostgres(nazwa,cialo: TStrings);
    procedure ShowFunctions(nazwa,cialo: TStrings);
    procedure ShowFunctionsPostgres(nazwa,nazwa2,cialo: TStrings; szukaj_schema: string=''; szukaj_name: string=''; szukaj_args: string='');
    procedure ShowProcedures(nazwa,cialo: TStrings);
    procedure ShowAggregatesPostgres(nazwa,cialo: TStrings);
    procedure ShowCreateTable(table: string; schema: TStrings);
    procedure ShowCreateTable(table: string; var schema: string);
    procedure ShowCreateTablePostgres(table: string; schema: TStrings);
    procedure ShowCreateTablePostgres(table: string; var schema: string);
    procedure ShowCreateViewPostgres(table: string; schema: TStrings);
    procedure ShowCreateViewPostgres(table: string; var schema: string);
    procedure ShowIndexesPostgres(table: string; nazwa,cialo: TStrings);
    procedure ShowIndexesPostgres(table: string; nazwa,nazwa_pelna,cialo: TStrings);
    procedure ShowSequencesPostgres(nazwa,cialo: TStrings; obiekty: TStrings = nil);
    procedure SaveSchema;
    function SyncSchema(SyncLogFilename:string='';SyncLogTest:boolean=false):boolean;
    function GenerateScript(Filename: string): boolean;
  published
    { Published declarations }
    property DB_Connection: TZConnection read FDB write FDB;
    property StructFileName: string read dbsql write dbsql; (*Nazwa pliku ze strukturą*)
    property ScriptToSqlite: boolean read FSToSqlite write FSToSqlite default false;
    property ZnacznikCzasu: boolean read FZnacznikCzasu write FZnacznikCzasu default false;
    property GrantsRemember: boolean read FGrantRemember write FGrantRemember default false;
    property ObjectsForRequest: TStrings read FRequest write SetRequest;
    property DataSync: TStrings read FDataSync write SetDataSync;
    property DataSyncSqlHeader: TStrings read FDataSyncHeader write SetDataSyncHeader;
    property DataSyncSqlFooter: TStrings read FDataSyncFooter write SetDataSyncFooter;
    property NoDropObjects: TListNoDropObjects read FNoDropObjects write FNoDropObjects default [];
    property SyncLog: boolean read FSyncLog write FSyncLog default false;
    property DataSyncOption: TDataSyncOption read FDataSyncOption write FDataSyncOption default dsNone;
    property OnStatus: TStatusEvent read FStatusEvent write FStatusEvent;
    property OnBeforeSave: TDefaultSchemaEvent read FBeforeSave write FBeforeSave;
    property OnAfterSave: TDefaultSchemaEvent read FAfterSave write FAfterSave;
    property OnBeforeSync: TDefaultSchemaEvent read FBeforeSync write FBeforeSync;
    property OnAfterSync: TDefaultSchemaEvent read FAfterSync write FAfterSync;
    property OnDataExecute: TSyncDataExecuteEvent read FSyncDataExecute write FSyncDataExecute;
  end;

procedure Register;

implementation

uses
  ecode_unit, ZDbcIntfs, ZScriptParser;

var
  ogens_opis: string = '';
  ogens: boolean = false;
  script_filename: string = '';

procedure Register;
begin
  {$I dbschemasync_postgres_icon.lrs}
  RegisterComponents('System',[TDBSchemaSyncPostgres]);
end;

procedure StrToListItems(s:string;list:TStrings);
var
  i: integer;
  pom: string;
begin
  list.Clear;
  pom:='';
  for i:=1 to length(s) do
  begin
    if s[i]=#10 then
    begin
      list.Add(pom);
      pom:='';
      continue;
    end;
    if s[i]=#13 then continue;
    pom:=pom+s[i];
  end;
  if pom<>'' then list.Add(pom);
end;

function StringToItemIndex(slist: TStrings; kod: string; wart_domyslna: integer; only_first_param: boolean = false): integer;
var
  i,a: integer;
  s: string;
begin
  a:=wart_domyslna;
  for i:=0 to slist.Count-1 do
  begin
    if only_first_param then s:=GetLineToStr(slist[i],1,' ') else s:=slist[i];
    if s=kod then
    begin
      a:=i;
      break;
    end;
  end;
  result:=a;
end;

procedure _postgres_str_2_schema_table(str: string; var schema,table: string);
var
  a: integer;
begin
  a:=pos('.',str);
  if a=0 then
  begin
    schema:='public';
    table:=str;
  end else begin
    schema:=copy(str,1,a-1);
    table:=copy(str,a+1,1000);
  end;
end;

{ TDBSchemaSyncPostgres }

procedure TDBSchemaSyncPostgres.DeleteStrings(zrodlo, cel: TStrings);
var
  a: integer;
begin
  exit; //NARAZIE WYŁĄCZONA!
  a:=StringToItemIndex(cel,zrodlo.Text,-1);
  if a>-1 then cel.Delete(a);
end;

procedure TDBSchemaSyncPostgres.AppendStrings(zrodlo, cel: TStrings; obj: string;
  opis: string; ignore: boolean);
var
  i: integer;
  s: string;
  schemat,nazwa: string;
  a: integer;
begin
  TRY
    if ogens_opis<>'' then
    begin
      cel.Add('');
      cel.Add('--');
      cel.Add('-- '+ogens_opis);
      cel.Add('--');
      cel.Add('');
      ogens_opis:='';
    end;
    if opis<>'' then
    begin
      cel.Add('');
      cel.Add('--');
      cel.Add('-- '+opis);
      cel.Add('--');
      cel.Add('');
    end;
    for i:=0 to zrodlo.Count-1 do
    begin
      s:=zrodlo[i];
      if i=zrodlo.Count then if s[length(s)]<>';' then s:=s+';';
      if ignore then s:='-- '+s;
      cel.Add(s);
      if sdb2.Connected then
      begin
        _postgres_str_2_schema_table(obj,schemat,nazwa);
        a:=pos('ON ',schemat);
        if a>0 then delete(schemat,1,a+2);
        sq22.ParamByName('schemat').AsString:=schemat;
        sq22.ParamByName('definicja').AsString:=s;
        sq22.ExecSQL;
      end;
    end;
  EXCEPT
    cel.Add('!!! ERROR LOGS !!!');
  END;
end;

procedure TDBSchemaSyncPostgres.AppendStrings(cel: TStrings; opis: string);
begin
  ogens_opis:=opis;
end;

procedure TDBSchemaSyncPostgres.AppendStrings(opis: string);
begin
  ogens_opis:=opis;
end;

procedure TDBSchemaSyncPostgres.SetDataSync(AValue: TStrings);
begin
  FDataSync.Assign(AValue);
end;

procedure TDBSchemaSyncPostgres.SetDataSyncFooter(AValue: TStrings);
begin
  FDataSyncFooter.Assign(AValue);
end;

procedure TDBSchemaSyncPostgres.SetDataSyncHeader(AValue: TStrings);
begin
  FDataSyncHeader.Assign(AValue);
end;

procedure TDBSchemaSyncPostgres.SetRequest(AValue: TStrings);
begin
  FRequest.Assign(AValue);
end;

procedure TDBSchemaSyncPostgres.StartTransaction;
begin
  sdb.TransactIsolationLevel:=tiReadCommitted;
  sdb.StartTransaction;
end;

procedure TDBSchemaSyncPostgres.Commit;
begin
  sdb.Commit;
  sdb.TransactIsolationLevel:=tiNone;
end;

procedure TDBSchemaSyncPostgres.Rollback;
begin
  sdb.Rollback;
  sdb.TransactIsolationLevel:=tiNone;
end;

procedure TDBSchemaSyncPostgres.StartTransaction2;
begin
  sdb2.TransactIsolationLevel:=tiReadCommitted;
  sdb2.StartTransaction;
end;

procedure TDBSchemaSyncPostgres.Commit2;
begin
  sdb2.Commit;
  sdb2.TransactIsolationLevel:=tiNone;
end;

procedure TDBSchemaSyncPostgres.Rollback2;
begin
  sdb2.Rollback;
  sdb2.TransactIsolationLevel:=tiNone;
end;

function TDBSchemaSyncPostgres.odczytaj_znacznik_czasu(s: string; var czas: TDateTime
  ): boolean;
var
  a: integer;
  pom: string;
  rok,miesiac,dzien: word;
  godz,min,sec,mili: word;
begin
  if not FZnacznikCzasu then
  begin
    result:=false;
    exit;
  end;
  a:=pos('-- {$ZC=',s);
  if a=0 then result:=false else
  begin
    (* wczytuje ciag interesujacych mnie znakow *)
    pom:=copy(s,a+8,12);
    (* wczytuje czesci daty *)
    rok:=StrToInt(copy(pom,1,4));
    miesiac:=StrToInt(copy(pom,5,2));
    dzien:=StrToInt(copy(pom,7,2));
    (* wczytuje czesci czasu *)
    godz:=StrToInt(copy(pom,9,2));
    min:=StrToInt(copy(pom,11,2));
    (* sekundy i milisekundy ustawiam na 0 *)
    sec:=0;
    mili:=0;
    (* wszystko pcham do zmiennej daty i czasu *)
    czas:=EncodeDate(rok,miesiac,dzien)+EncodeTime(godz,min,sec,mili);
    result:=true;
  end;
end;

function TDBSchemaSyncPostgres.sync_delete: boolean;
var
  i: integer;
  v1,v2,v3: TStringList;
  s,s1,s2: string;
begin
  (* usuwamy ze struktury objekty nie istniejące w bazie wzorcowej *)
  v1:=TStringList.Create;
  v2:=TStringList.Create;
  v3:=TStringList.Create;
  try
    (* wyzwalacze *)
    case proto of
      1: ShowTriggers(v1,v2);
      2: ShowTriggersPostgres(v1,v2);
    end;
    if ogens then AppendStrings(gens,'Usunięcie nie istniejących wyzwalaczy');
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=3');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      if Assigned(FStatusEvent) then FStatusEvent(self,3,'d','Synchro Schema Delete',v1[i],'Trigger',false,0,0);
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsInteger=0 then
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,3,'D','Synchro Schema Delete',v1[i],'Trigger',false,0,0);
        qq.SQL.Clear;
        qq.SQL.Add('drop trigger '+v1[i]+';');
        try
          if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
          begin
            qq.ExecSQL;
            if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
          end;
        except
          on E: Exception do
          begin
            log.Add('SYNC-DELETE: Błąd podczas usuwania wyzwalacza: ['+v1[i]+']:'+#13#10+qq.SQL.Text+#13#10#13#10+E.Message);
            result:=false;
            exit;
          end;
        end;
      end;
      sq1.Close;
    end;
    (* widoki *)
    case proto of
      1: ShowTables(v1);
      2: ShowTablesPostgres(v1);
    end;
    if ogens then AppendStrings(gens,'Usunięcie nieistniejących widoków');
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=2');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      case proto of
        1: ShowCreateTable(v1[i],s);
        2: ShowCreateTablePostgres(v1[i],s);
      end;
      if pos('CREATE VIEW',s)=1 then
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,2,'d','Synchro Schema Delete',v1[i],'View',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,2,'D','Synchro Schema Delete',v1[i],'View',false,0,0);
          qq.SQL.Clear;
          qq.SQL.Add('drop view '+v1[i]+';');
          try
            if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
            begin
              qq.ExecSQL;
              if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
            end;
          except
            log.Add('SYNC-DELETE: Błąd podczas usuwania podglądu: ['+v1[i]+']:'+#13#10+qq.SQL.Text);
            result:=false;
            exit;
          end;
        end;
        sq1.Close;
      end;
    end;
    (* procedury *)
    if proto=1 then
    begin
      if ogens then AppendStrings(gens,'Usunięcie nieistniejących procedur');
      ShowProcedures(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=5');
      sq1.Prepare;
      for i:=0 to v1.Count-1 do
      begin
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          qq.SQL.Clear;
          qq.SQL.Add('drop procedure '+v1[i]);
          try
            if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
            begin
              qq.ExecSQL;
              if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
            end;
          except
            log.Add('SYNC-DELETE: Błąd podczas usuwania procedury: ['+v1[i]+']:'+#13#10+qq.SQL.Text);
            result:=false;
            exit;
          end;
        end;
        sq1.Close;
      end;
    end;
    (* funkcje *)
    case proto of
      1: ShowFunctions(v1,v2);
      2: ShowFunctionsPostgres(v1,v3,v2); //v3 - nazwa rozszerzone z parametrami
    end;
    if ogens then AppendStrings(gens,'Usunięcie nieistniejących funkcji');
    sq1.SQL.Clear;
    sq1.SQL.Add('select nazwa,nazwa2 from tabele where nazwa=:nazwa and typ=4');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      if Assigned(FStatusEvent) then FStatusEvent(self,4,'d','Synchro Function Delete',v1[i],'Function',false,0,0);
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.IsEmpty or (v3[i]<>sq1.Fields[1].AsString) then
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,4,'D','Synchro Function Delete',v1[i],'Function',false,0,0);
        qq.SQL.Clear;
        //qq.SQL.Add('drop function '+v1[i]+';');
        qq.SQL.Add('drop function '+v1[i]+' cascade;');
        try
          if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
          begin
            qq.ExecSQL;
            if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
          end;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania funkcji: ['+v1[i]+']:'+#13#10+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* tabele *)
    case proto of
      1: ShowTables(v1);
      2: ShowTablesPostgres(v1);
    end;
    if ogens then AppendStrings(gens,'Usunięcie nieistniejących tabel');
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=1');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      case proto of
        1: ShowCreateTable(v1[i],v2);
        2: ShowCreateTablePostgres(v1[i],v2);
      end;
      if pos('CREATE TABLE',v2.Text)=1 then
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,1,'d','Synchro Schema Delete',v1[i],'Table',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,1,'D','Synchro Schema Delete',v1[i],'Table',false,0,0);
          qq.SQL.Clear;
          qq.SQL.Add('drop table '+v1[i]+';');
          try
            if nnTables in FNoDropObjects then
            begin
              if ogens then AppendStrings(qq.SQL,gens,v1[i],'',true) else
                if FSyncLog then AppendStrings(qq.SQL,gens,v1[i],'',true);
            end else begin
              if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
              begin
                qq.ExecSQL;
                if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
              end;
            end;
          except
            log.Add('SYNC-DELETE: Błąd podczas usuwania tabeli: ['+v1[i]+']:'+#13#10+qq.SQL.Text);
            result:=false;
            exit;
          end;
        end;
        sq1.Close;
      end;
    end;
    (* sekwencje *)
    if proto=2 then
    begin
      if ogens then AppendStrings(gens,'Usunięcie nieistniejących sekwencji');
      ShowSequencesPostgres(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=12');
      sq1.Prepare;
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,12,'d','Synchro Schema Delete',v1[i],'Sequention',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,12,'D','Synchro Schema Delete',v1[i],'Sequention',false,0,0);
          qq.SQL.Clear;
          qq.SQL.Add('drop sequence '+v1[i]+';');
          try
            if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
            begin
              qq.ExecSQL;
              if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
            end;
          except
            if (nnTables in FNoDropObjects) or (nnColumns in FNoDropObjects) then
            begin
              log.Add('SYNC-DELETE: Błąd podczas usuwania sekwencji: ['+v1[i]+']:'+#13#10+qq.SQL.Text+' (ZIGNOROWANY)');
              if FSyncLog then AppendStrings(qq.SQL,gens,v1[i],'',true);
              result:=true;
            end else begin
              log.Add('SYNC-DELETE: Błąd podczas usuwania sekwencji: ['+v1[i]+']:'+#13#10+qq.SQL.Text);
              result:=false;
              exit;
            end;
          end;
        end;
        sq1.Close;
      end;
    end;
    (* types in postgres *)
    if proto=2 then
    begin
      if ogens then AppendStrings(gens,'Usunięcie nieistniejących typów');
      ShowTypesPostgres(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=20');
      sq1.Prepare;
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,20,'d','Synchro Schema Delete',v1[i],'Type',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,20,'D','Synchro Schema Delete',v1[i],'Type',false,0,0);
          qq.SQL.Clear;
          qq.SQL.Add('drop type '+v1[i]);
          try
            if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
            begin
              qq.ExecSQL;
              if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
            end;
          except
            log.Add('SYNC-DELETE: Błąd podczas usuwania typu: ['+v1[i]+']:'+#13#10+qq.SQL.Text);
            result:=false;
            exit;
          end;
        end;
        sq1.Close;
      end;
    end;
    (* schematy *)
    if proto=2 then
    begin
      if ogens then AppendStrings(gens,'Usunięcie nieistniejących schematów');
      ShowSchemasPostgres(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=0');
      sq1.Prepare;
      for i:=0 to v1.Count-1 do
      begin
        if v1[i]='public' then continue; //zabezpieczenie przed usunięciem głównego schematu
        if Assigned(FStatusEvent) then FStatusEvent(self,0,'d','Synchro Schema Delete',v1[i],'Schema',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,0,'D','Synchro Schema Delete',v1[i],'Schema',false,0,0);
          qq.SQL.Clear;
          qq.SQL.Add('drop schema '+v1[i]);
          try
            if ogens then AppendStrings(qq.SQL,gens,v1[i]) else
            begin
              qq.ExecSQL;
              if FSyncLog then AppendStrings(qq.SQL,gens,v1[i]);
            end;
          except
            log.Add('SYNC-DELETE: Błąd podczas usuwania schematu: ['+v1[i]+']:'+#13#10+qq.SQL.Text);
            result:=false;
            exit;
          end;
        end;
        sq1.Close;
      end;
    end;
  finally
    v1.Free;
    v2.Free;
    v3.Free;
  end;
  result:=true;
end;

function TDBSchemaSyncPostgres.sync_table: boolean;
var
  i: integer;
  v1,v2,defs,vs: TStringList;
  nazwa,def: string;
  b: boolean;
  lc: integer;
  q: TZSQLProcessor;
begin
  (* aktualizacja schamatów, sekwencji i tabel *)
  b:=true;
  list.Clear;
  v1:=TStringList.Create;
  v2:=TStringList.Create;
  vs:=TStringList.Create;
  defs:=TStringList.Create;
  try
    (* languages and schematy *)
    if proto=2 then
    begin
      (* languages *)
      if ogens then AppendStrings(gens,'Dodanie nowych języków proceduralnych');
      ShowLanguagesPostgres(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=181 order by id');
      sq1.Open;
      q:=TZSQLProcessor.Create(self);
      q.Connection:=FDB;
      q.DelimiterType:=dtDelimiter;
      q.Delimiter:=';;';
      q.ParamCheck:=false;
      try
        while not sq1.EOF do
        begin
          nazwa:=sq1.FieldByName('nazwa').AsString;
          def:=sq1.FieldByName('definicja').AsString;
          if Assigned(FStatusEvent) then FStatusEvent(self,181,'c','Synchro Schema Create',nazwa,'Language',false,0,0);
          if StringToItemIndex(v1,nazwa,-1)=-1 then
          begin
            if Assigned(FStatusEvent) then FStatusEvent(self,181,'C','Synchro Schema Create',nazwa,'Language',false,0,0);
            q.Script.Clear;
            q.Script.Add(StringReplace(def,';',';;',[rfReplaceAll]));
            try
              if ogens then AppendStrings(q.Script,gens,nazwa) else
              begin
                q.Execute;
                if FSyncLog then AppendStrings(q.Script,gens,nazwa);
              end;
            except
              log.Add('SYNC-ALTER: Błąd podczas dodawania procedural language: ['+nazwa+']:'+#13#10+q.Script.Text);
              exit;
            end;
          end;
          sq1.Next;
        end;
      finally
        q.Free;
      end;
      sq1.Close;
      (* schematy *)
      if ogens then AppendStrings(gens,'Dodanie nowych schematów');
      ShowSchemasPostgres(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=0 order by id');
      sq1.Open;
      while not sq1.EOF do
      begin
        nazwa:=sq1.FieldByName('nazwa').AsString;
        def:=sq1.FieldByName('definicja').AsString;
        if Assigned(FStatusEvent) then FStatusEvent(self,0,'c','Synchro Schema Create',nazwa,'Schema',false,0,0);
        if StringToItemIndex(v1,nazwa,-1)=-1 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,0,'C','Synchro Schema Create',nazwa,'Schema',false,0,0);
          q1.SQL.Clear;
          q1.SQL.Add(def);
          try
            if ogens then AppendStrings(q1.SQL,gens,nazwa) else
            begin
              q1.ExecSQL;
              if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
            end;
          except
            log.Add('SYNC-ALTER: Błąd podczas dodawania schematu: ['+nazwa+']:'+#13#10+q1.SQL.Text);
            exit;
          end;
        end;
        sq1.Next;
      end;
      sq1.Close;
    end;
    (* types *)
    if proto=2 then
    begin
      if ogens then AppendStrings(gens,'Dodanie nowych typów');
      ShowTypesPostgres(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=20 order by id');
      sq1.Open;
      while not sq1.EOF do
      begin
        nazwa:=sq1.FieldByName('nazwa').AsString;
        def:=sq1.FieldByName('definicja').AsString;
        if Assigned(FStatusEvent) then FStatusEvent(self,20,'c','Synchro Schema Create',nazwa,'Type',false,0,0);
        if StringToItemIndex(v1,nazwa,-1)=-1 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,20,'C','Synchro Schema Create',nazwa,'Type',false,0,0);
          q1.SQL.Clear;
          q1.SQL.Add(def);
          try
            if ogens then AppendStrings(q1.SQL,gens,nazwa) else
            begin
              q1.ExecSQL;
              if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
            end;
          except
            log.Add('SYNC-ALTER: Błąd podczas dodawania typu: ['+nazwa+']:'+#13#10+q1.SQL.Text);
            exit;
          end;
        end;
        sq1.Next;
      end;
      sq1.Close;
    end;
    (* sekwencje *)
    if proto=2 then
    begin
      vs.Clear;
      if ogens then AppendStrings(gens,'Dodanie nowych sekwencji');
      ShowSequencesPostgres(v1,v2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=12 order by id');
      sq1.Open;
      while not sq1.EOF do
      begin
        nazwa:=sq1.FieldByName('nazwa').AsString;
        def:=sq1.FieldByName('definicja').AsString;
        if Assigned(FStatusEvent) then FStatusEvent(self,12,'c','Synchro Schema Create',nazwa,'Sequention',false,0,0);
        if StringToItemIndex(v1,nazwa,-1)=-1 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,12,'C','Synchro Schema Create',nazwa,'Sequention',false,0,0);
          q1.SQL.Clear;
          q1.SQL.Add(def);
          try
            if ogens then AppendStrings(q1.SQL,gens,nazwa) else
            begin
              q1.ExecSQL;
              vs.Add(nazwa);
              if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
            end;
          except
            log.Add('SYNC-ALTER: Błąd podczas dodawania sekwencji: ['+nazwa+']:'+#13#10+q1.SQL.Text);
            exit;
          end;
        end;
        sq1.Next;
      end;
      sq1.Close;
    end;
    (* widoki *)
    if proto=2 then akt_widoki_delete_postgres;
    (* tabele *)
    if Assigned(FStatusEvent) then FStatusEvent(self,-1,'I','Synchro Schema Table Preparing','','',false,0,0);
    case proto of
      1: ShowTables(v1);
      2: ShowTablesPostgres(v1);
    end;
    if ogens then AppendStrings(gens,'Aktualizacja tabel i indeksów');
    sq1.SQL.Clear;
    sq1.SQL.Add('select nazwa,definicja from tabele where typ=1 order by id desc');
    sq1.Open;
    while not sq1.EOF do
    begin
      list.Add(sq1.FieldByName('nazwa').AsString);
      sq1.Next;
    end;
    sq1.Close;
    (* lecimy po tabelach dopóki lista nie będzie pusta *)
    sq1.SQL.Clear;
    sq1.SQL.Add('select definicja from tabele where typ=1 and nazwa=:nazwa');
    sq1.Prepare;
    while list.Count>0 do
    begin
      lc:=list.Count;
      for i:=list.Count-1 downto 0 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,1,'I','Synchro Tables Upgrading Now!',list[i],'Table',false,0,0);
        nazwa:=list[i];
        sq1.ParamByName('nazwa').AsString:=nazwa;
        sq1.Open;
        def:=sq1.FieldByName('definicja').AsString;
        sq1.Close;
        StrToListItems(def,defs);
        (* sprawdzamy referencje i wykonujemy aktualizację jeśli trzeba *)
        if test_ref(nazwa,defs) then
        begin
          if proto=2 then akt_indeksy_delete_postgres(nazwa);
          b:=test_table(i,nazwa,defs,vs);
          if (proto=2) and b then akt_indeksy_create_postgres(nazwa);
        end;
      end;
      if lc=list.Count then break;
    end;
    if vs.Count>0 then
    begin
      (* kojarzę wszystkie nowo dodane sekwencje jeśli trzeba *)
      if ogens then AppendStrings(gens,'Aktualizacja nowych sekwencji');
      sq1.SQL.Clear;
      sq1.SQL.Add('select obiekt from tabele where typ=12 and nazwa=:nazwa');
      sq1.Prepare;
      for i:=0 to vs.Count-1 do
      begin
        sq1.ParamByName('nazwa').AsString:=vs[i];
        sq1.Open;
        if (not sq1.FieldByName('obiekt').IsNull) and (sq1.FieldByName('obiekt').AsString<>'') then
        begin
          q1.SQL.Clear;
          q1.SQL.Add('ALTER SEQUENCE '+vs[i]+' OWNED BY '+sq1.FieldByName('obiekt').AsString+';');
          try
            if ogens then AppendStrings(q1.SQL,gens,vs[i]) else
            begin
              q1.ExecSQL;
              if FSyncLog then AppendStrings(q1.SQL,gens,vs[i]);
            end;
          except
            log.Add('SYNC-ALTER: Błąd podczas dodawania sekwencji: ['+nazwa+']:'+#13#10+q1.SQL.Text);
          end;
        end;
        sq1.Close;
      end;
    end;
  finally
    v1.Free;
    v2.Free;
    vs.Free;
    defs.Free;
  end;
  result:=b;
end;

function TDBSchemaSyncPostgres.test_ref(nazwa: string; cialo: TStrings): boolean;
var
  b: boolean;
  i,j,a1,a2,a3: integer;
  s: string;
begin
  b:=true;
  for i:=0 to cialo.Count-1 do
  begin
    s:=cialo[i];
    a1:=pos('CONSTRAINT',s);
    a2:=pos('FOREIGN KEY',s);
    a3:=pos('REFERENCES',s);
    if (a1>0) and (a2>0) and (a3>0) then
    begin
      delete(s,1,a3+10);
      j:=pos(' ',s);
      delete(s,j,255);
      if StringToItemIndex(list,s,-1)<>-1 then
      begin
        b:=false;
        break;
      end;
    end;
  end;
  result:=b;
end;

function TDBSchemaSyncPostgres.test_table(indeks: integer; nazwa: string; cialo,
  sekwencje: TStrings): boolean;
var
  ss: TStringList;
  b: boolean;
begin
  b:=true;
  ss:=TStringList.Create;
  try
    case proto of
      1: ShowCreateTable(nazwa,ss);
      2: ShowCreateTablePostgres(nazwa,ss);
    end;
    if ogens then AppendStrings(gens,'Dodanie nowych tabel');
    if cialo.Text<>ss.Text then b:=akt_table(nazwa,cialo,ss);
  finally
    ss.Free;
  end;
  if b then list.Delete(indeks);
  result:=b;
end;

function nazwa_pola(pole:string):string;
var
  s: string;
  a: integer;
begin
  s:=pole;
  a:=pos(' ',s);
  delete(s,a,255);
  result:=s;
end;

function nazwa_indeksu(pole:string):string;
var
  s: string;
  a: integer;
begin
  s:=pole;
  if pos('KEY',s)=1 then delete(s,1,4);
  if pos('UNIQUE KEY',s)=1 then delete(s,1,11);
  a:=pos(' ',s);
  delete(s,a,255);
  result:=s;
end;

function nazwa_klucza_obcego(pole:string):string;
var
  s: string;
  a: integer;
begin
  s:=pole;
  delete(s,1,11);
  a:=pos(' ',s);
  delete(s,a,255);
  result:=s;
end;

procedure pola_to_indeksy(proto:integer;cialo,cel:TStrings;typ:integer;pp:boolean=false);
var
  i: integer;
  s: string;
  tt: integer;
  znacznik: integer;
begin
  (* legenda: 0-pole, 1-klucz pierwszy, 2-indeks, 3-klucz obcy *)
  case proto of
    1: znacznik:=2;
    2: znacznik:=1;
  end;
  cel.Clear;
  for i:=1 to cialo.Count-znacznik do
  begin
    s:=trimleft(cialo[i]);
    if s=');' then continue;
    if s[length(s)]=';' then delete(s,length(s),1);
    if s[length(s)]=')' then delete(s,length(s),1);
    if s[length(s)]=',' then delete(s,length(s),1);
    if pos('PRIMARY KEY',s)=1 then tt:=1 else
    if (pos('KEY',s)=1) or (pos('UNIQUE KEY',s)=1) then tt:=2 else
    if (pos('CONSTRAINT',s)=1) and (pos('FOREIGN KEY',s)>0) then tt:=3 else tt:=0;
    if tt=typ then case typ of
      0: if pp then cel.Add(s) else cel.Add(nazwa_pola(s));
      1: cel.Add(s);
      2: cel.Add(s);
      3: if proto=2 then cel.Add(s+')') else cel.Add(s);
    end;
  end;
end;

function usun_pierwszy_wiersz(s: string):string;
var
  a: integer;
begin
  a:=pos(' ',s);
  delete(s,1,a);
  result:=s;
end;

function TDBSchemaSyncPostgres.akt_table(nazwa: string; s1, s2: TStrings): boolean;
var
  p1,p2,pp,kd: TStringList; (* kd - kolumny dodane *)
  i,a,aa: integer;
  s,o1,o2,o3,pom: string;
begin
  {try
    q1.ExecSQL;
  except
    on E: Exception do log.Add('SYNC-ADD: Błąd podczas usuwania indeksu z tabeli: ['+w1[i]+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
  end;}
  result:=true;
  if Assigned(FStatusEvent) then FStatusEvent(self,1,'i','Synchro Schema SyncTable',nazwa,'Table',false,0,0);
  if s2.Count=0 then
  begin
    (* tej tabeli nie ma - tworzę ją *)
    s:=s1.Text;
    //s:=StringReplace(s,':','::',[rfReplaceAll]);
    q1.SQL.Clear;
    q1.SQL.Add(s);
    try
      if ogens then AppendStrings(s1,gens,nazwa) else
      begin
        try
          q1.ParamCheck:=false;
          q1.ExecSQL;
        finally
          q1.ParamCheck:=true;
        end;
        if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
      end;
      result:=true;
    except
      on E: Exception do
      begin
        log.Add('SYNC-CREATE: Błąd podczas tworzenia tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
        result:=false;
      end;
    end;
    exit;
  end;
  result:=false;
  pp:=TStringList.Create;
  p1:=TStringList.Create;
  p2:=TStringList.Create;
  kd:=TStringList.Create;
  try
    (* --- USUWAM WSZYSTKIE OBJEKTY KTÓRYCH JUŻ NIE MA --- *)
    (* usuwam gotowe do usunięcia klucze obce *)
    pola_to_indeksy(proto,s1,p1,3);
    pola_to_indeksy(proto,s2,p2,3);
    for i:=0 to p2.Count-1 do
    begin
      aa:=StringToItemIndex(p1,p2[i],-1);
      if aa=-1 then
      begin
        pom:=nazwa_klucza_obcego(p2[i]);
        q1.SQL.Clear;
        case proto of
          1: q1.SQL.Add('ALTER TABLE '+nazwa+' DROP FOREIGN KEY '+pom+';');
          //2: q1.SQL.Add('ALTER TABLE '+nazwa+' DROP FOREIGN KEY '+pom+';');
          2: q1.SQL.Add('ALTER TABLE '+nazwa+' DROP CONSTRAINT '+pom+';');
        end;
        try
          if ogens then AppendStrings(q1.SQL,gens,nazwa) else
          begin
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
          end;
        except
          on E: Exception do
          begin
            log.Add('SYNC-ALTER: Błąd podczas usuwania klucza obcego w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
            exit;
          end;
        end;
      end;
    end;
    (* usuwam gotowe do usunięcia indeksy *)
    pola_to_indeksy(proto,s1,p1,2);
    pola_to_indeksy(proto,s2,p2,2);
    for i:=0 to p2.Count-1 do if StringToItemIndex(p1,p2[i],-1)=-1 then
    begin
      pom:=nazwa_indeksu(p2[i]);
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' DROP INDEX '+pom+';');
      try
        if ogens then AppendStrings(q1.SQL,gens,nazwa) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
        end;
      except
        on E: Exception do
        begin
          log.Add('SYNC-ALTER: Błąd podczas usuwania indeksu w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
          exit;
        end;
      end;
    end;
    (* usuwam gotowe do usunięcia klucze główne *)
    pola_to_indeksy(proto,s1,p1,1);
    pola_to_indeksy(proto,s2,p2,1);
    if p2.Count=1 then if (p1.Count=0) or ((p1.Count=1) and (p1[0]<>p2[0])) then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' DROP PRIMARY KEY;');
      try
        if ogens then AppendStrings(q1.SQL,gens,nazwa) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
        end;
      except
        on E: Exception do
        begin
          log.Add('SYNC-ALTER: Błąd podczas usuwania klucza nadrzędnego tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
          exit;
        end;
      end;
    end;
    (* usuwam gotowe do usunięcia pola tabeli *)
    pola_to_indeksy(proto,s1,p1,0);
    pola_to_indeksy(proto,s2,p2,0);
    for i:=0 to p2.Count-1 do if StringToItemIndex(p1,p2[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' DROP COLUMN '+p2[i]+';');
      try
        if nnColumns in FNoDropObjects then
        begin
          if ogens then AppendStrings(q1.SQL,gens,nazwa,'',true) else
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa,'',true);
        end else begin
          if ogens then AppendStrings(q1.SQL,gens,nazwa) else
          begin
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
          end;
        end;
      except
        on E: Exception do
        begin
          log.Add('SYNC-ALTER: Błąd podczas usuwania kolumny z tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
          exit;
        end;
      end;
    end;
    (* odtwarzam nową wersję struktury i sprawdzam czy struktury są już takie same *)
    case proto of
      1: ShowCreateTable(nazwa,s2);
      2: ShowCreateTablePostgres(nazwa,s2);
    end;
    if s1=s2 then
    begin
      result:=true;
      exit;
    end;
    (* --- DODAJĘ WSZYSTKIE OBJEKTY KTÓRYCH BRAKUJE --- *)
    (* dodaję nowe pola tabeli *)
    pola_to_indeksy(proto,s1,pp,0,true);
    pola_to_indeksy(proto,s1,p1,0);
    pola_to_indeksy(proto,s2,p2,0);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      case proto of
        1: if i=0 then s:='ALTER TABLE '+nazwa+' ADD COLUMN '+pp[i]+' FIRST'
                  else s:='ALTER TABLE '+nazwa+' ADD COLUMN '+pp[i]+' AFTER '+p1[i-1];
        2: s:='ALTER TABLE '+nazwa+' ADD COLUMN '+pp[i]+';';
      end;
      if pos('AUTO_INCREMENT',s)>0 then s:=StringReplace(s,'AUTO_INCREMENT','AUTO_INCREMENT PRIMARY KEY',[rfIgnoreCase]);
      try
        q1.SQL.Add(s);
        if ogens then AppendStrings(q1.SQL,gens,nazwa) else begin
          try
            q1.ParamCheck:=false;
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
            kd.Add(nazwa_pola(pp[i]));
          finally
            q1.ParamCheck:=true;
          end;
        end;
      except
        on E: Exception do
        begin
          log.Add('SYNC-ADDING: Błąd podczas dodawania kolumny do tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10+#13#10+E.Message);
          result:=false;
          exit;
        end;
      end;
    end;
    (* aktualizuję pola tabeli, które się różnią *)
    pola_to_indeksy(proto,s1,pp,0);
    pola_to_indeksy(proto,s1,p1,0,true);
    pola_to_indeksy(proto,s2,p2,0,true);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      (* MYSQL *)
      if proto=1 then
      begin
        q1.SQL.Clear;
        if i=0 then q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' FIRST')
               else q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' AFTER '+pp[i-1]);
        try
          if ogens then AppendStrings(q1.SQL,gens,nazwa) else
          begin
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
          end;
        except
          on E: Exception do
          begin
            log.Add('SYNC-CHANGE: Błąd podczas edycji kolumny w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
            exit;
          end;
        end;
      end;
      (* POSTGRESQL *)
      if proto=2 then
      begin
        pom:=nazwa_pola(pp[i]);
        if (pom<>'CONSTRAINT') and (StringToItemIndex(kd,pom,-1)=-1) then
        begin
          get_alter_table_postgres(nazwa,pp[i],p1[i],o1,o2,o3);
          if o1<>'' then
          begin
            q1.SQL.Clear;
            q1.SQL.Add(o1+';');
            try
              if ogens then AppendStrings(q1.SQL,gens,nazwa) else
              begin
                try
                  q1.ParamCheck:=false;
                  q1.ExecSQL;
                finally
                  q1.ParamCheck:=true;
                end;
                if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
              end;
            except
              on E: Exception do
              begin
                log.Add('SYNC-CHANGE: Błąd podczas edycji kolumny w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
                exit;
              end;
            end;
          end;
          if o2<>'' then
          begin
            q1.SQL.Clear;
            q1.SQL.Add(o2+';');
            try
              if ogens then AppendStrings(q1.SQL,gens,nazwa) else
              begin
                try
                  q1.ParamCheck:=false;
                  q1.ExecSQL;
                finally
                  q1.ParamCheck:=true;
                end;
                if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
              end;
            except
              on E: Exception do
              begin
                log.Add('SYNC-CHANGE: Błąd podczas edycji kolumny w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
                exit;
              end;
            end;
          end;
          if o3<>'' then
          begin
            q1.SQL.Clear;
            q1.SQL.Add(o3+';');
            try
              if ogens then AppendStrings(q1.SQL,gens,nazwa) else
              begin
                try
                  q1.ParamCheck:=false;
                  q1.ExecSQL;
                finally
                  q1.ParamCheck:=true;
                end;
                if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
              end;
            except
              on E: Exception do
              begin
                log.Add('SYNC-CHANGE: Błąd podczas edycji kolumny w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
                exit;
              end;
            end;
          end;
        end;
      end; //if postgres
    end;
    (* aktualizuję kolejność pól tabeli, jeśli istnieje inna kolejność - tylko mysql *)
    if proto=1 then
    begin
      pola_to_indeksy(proto,s1,pp,0);
      pola_to_indeksy(proto,s1,p1,0,true);
      pola_to_indeksy(proto,s2,p2,0,true);
      for i:=0 to p1.Count-1 do if p1[i]<>p2[i] then
      begin
        q1.SQL.Clear;
        if i=0 then q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' FIRST')
               else q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' AFTER '+pp[i-1]);
        try
          if ogens then AppendStrings(q1.SQL,gens,nazwa) else
          begin
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
          end;
        except
          on E: Exception do
          begin
            log.Add('SYNC-SORT: Błąd podczas przesuwania kolumny w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
            exit;
          end;
        end;
        case proto of
          1: ShowCreateTable(nazwa,s2);
          2: ShowCreateTablePostgres(nazwa,s2);
        end;
        pola_to_indeksy(proto,s2,p2,0,true);
      end;
    end;
    (* dodaję brakujący indeks pierwszy *)
    pola_to_indeksy(proto,s1,p1,1);
    pola_to_indeksy(proto,s2,p2,1);
    if (p1.Count=1) and (p2.Count=0) then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' ADD '+p1[0]+';');
      try
        if ogens then AppendStrings(q1.SQL,gens,nazwa) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
        end;
      except
        on E: Exception do
        begin
          log.Add('SYNC-ADD: Błąd podczas dodawania indeksu podstawowego w tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
          exit;
        end;
      end;
    end;
    (* dodaję brakujące indeksy *)
    pola_to_indeksy(proto,s1,p1,2);
    pola_to_indeksy(proto,s2,p2,2);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' ADD '+p1[i]+';');
      try
        if ogens then AppendStrings(q1.SQL,gens,nazwa) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
        end;
      except
        on E: Exception do
        begin
          log.Add('SYNC-ADD: Błąd podczas dodawania indeksu do tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
          exit;
        end;
      end;
    end;
    (* dodaję brakujące klucze obce *)
    pola_to_indeksy(proto,s1,p1,3);
    pola_to_indeksy(proto,s2,p2,3);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' ADD '+p1[i]+';');
      try
        if ogens then AppendStrings(q1.SQL,gens,nazwa) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
        end;
      except
        on E: Exception do
        begin
          log.Add('SYNC-ADD: Błąd podczas dodawania indeksu obcego do tabeli: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
          exit;
        end;
      end;
    end;
  finally
    pp.Free;
    p1.Free;
    p2.Free;
    kd.Free;
  end;
  result:=true;
end;

function TDBSchemaSyncPostgres.akt_indeksy_delete_postgres(nazwa: string): boolean;
var
  w1,w2,ww: TStringList;
  i: integer;
  usuniecie: boolean;
  s: string;
  ppom: string;
begin
  sq2.SQL.Clear;
  sq2.SQL.Add('select nazwa,nazwa2,definicja from tabele where typ=11 and nazwa2=:nazwa');
  sq2.Prepare;
  w1:=TStringList.Create;
  w2:=TStringList.Create;
  ww:=TStringList.Create;
  try
    ShowIndexesPostgres(nazwa,w1,ww,w2);
    for i:=w1.Count-1 downto 0 do
    begin
      usuniecie:=false;
      sq2.ParamByName('nazwa').AsString:=ww[i];
      sq2.Open;
      if sq2.RecordCount=0 then usuniecie:=true else if sq2.FieldByName('definicja').AsString<>w2[i] then usuniecie:=true;
      sq2.Close;
      if usuniecie then
      begin
        (* usuwam indeks *)
        q1.SQL.Clear;
        if pos('CONSTRAINT',w2[i])>0 then begin ppom:=w1[i]; q1.SQL.Add('drop constraint '+w1[i]+';'); end
                                     else begin ppom:=ww[i]; q1.SQL.Add('drop index '+ww[i]+';'); end;
        if ogens then AppendStrings(q1.SQL,gens,ppom) else
        begin
          try
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,ppom);
          except
            on E: Exception do log.Add('SYNC-ADD: Błąd podczas usuwania indeksu z tabeli: ['+w1[i]+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
          end;
        end;
      end;
    end;
  finally
    w1.Free;
    w2.Free;
    ww.Free;
  end;
  result:=true;
end;

function TDBSchemaSyncPostgres.akt_indeksy_create_postgres(nazwa: string): boolean;
var
  w1,w2,ww: TStringList;
  s1,s2,naz,naz2,def: string;
  i: integer;
begin
  _postgres_str_2_schema_table(nazwa,s1,s2);
  sq2.SQL.Clear;
  sq2.SQL.Add('select nazwa,nazwa2,definicja from tabele where typ=11 and (definicja like ''ALTER TABLE '+s1+'.'+s2+' ADD CONSTRAINT%'' or definicja like ''%ON '+s1+'.'+s2+' USING%'') order by id');
  w1:=TStringList.Create;
  w2:=TStringList.Create;
  ww:=TStringList.Create;
  try
    ShowIndexesPostgres(nazwa,w1,ww,w2);
    sq2.Open;
    while not sq2.EOF do
    begin
      naz:=sq2.FieldByName('nazwa').AsString;
      naz2:=sq2.FieldByName('nazwa2').AsString;
      def:=sq2.FieldByName('definicja').AsString;
      if StringToItemIndex(ww,naz2,-1)=-1 then
      begin
        {if pos('tgeapi_transakcje_mytimestamp_kod_instrumentu',naz2)>0 then
        begin
          writeln('INDEKS DO DODANIA:');
          for i:=0 to ww.Count-1 do writeln('  >'+ww[i]);
          writeln('nazwa:     '+naz);
          writeln('nazwa2:    '+naz2);
          writeln('definicja: '+def);
        end;}
        (* zakładam indeks *)
        q1.SQL.Clear;
        q1.SQL.Add(def);
        q1.ParamCheck:=false;
        try
          try
            if ogens then AppendStrings(q1.SQL,gens,naz) else
            begin
              q1.ExecSQL;
              if FSyncLog then AppendStrings(q1.SQL,gens,naz);
            end;
          except
            on E: Exception do
            begin
              lastindexes.Add(q1.SQL.Text);
              log.Add('SYNC-CREATE: Błąd podczas tworzenia indeksu: ['+naz+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
              result:=false;
            end;
          end;
        finally
          q1.ParamCheck:=true;
        end;
      end;
      sq2.Next;
    end;
    sq2.Close;
  finally
    w1.Free;
    w2.Free;
    ww.Free;
  end;
  result:=true;
end;

function TDBSchemaSyncPostgres.akt_widoki_delete_postgres: boolean;
var
  w1,w2: TStringList;
  i: integer;
  usuniecie: boolean;
begin
  if Assigned(FStatusEvent) then FStatusEvent(self,2,'d','Synchro Views Drop','','View',false,0,0);
  if ogens then AppendStrings(gens,'Usunięcie nie istniejących lub nieaktualnych widoków');
  sq1.SQL.Clear;
  sq1.SQL.Add('select nazwa,definicja from tabele where typ=2 and nazwa=:nazwa');
  sq1.Prepare;
  w1:=TStringList.Create;
  w2:=TStringList.Create;
  try
    ShowViewsPostgres(w1);
    for i:=w1.Count-1 downto 0 do
    begin
      usuniecie:=false;
      ShowCreateViewPostgres(w1[i],w2);
      if w2.Count=0 then continue;
      sq1.ParamByName('nazwa').AsString:=w1[i];
      sq1.Open;
      if sq1.RecordCount=0 then usuniecie:=true else if sq1.FieldByName('definicja').AsString<>w2.Text then usuniecie:=true;
      sq1.Close;
      if usuniecie then
      begin
        (* usuwam widok *)
        if Assigned(FStatusEvent) then FStatusEvent(self,2,'D','Synchro Views Drop',w1[i],'View',false,0,0);
        q1.SQL.Clear;
        q1.SQL.Add('drop view '+w1[i]+' cascade;');
        if ogens then AppendStrings(q1.SQL,gens,w1[i]) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,w1[i]);
        end;
      end;
    end;
  finally
    w1.Free;
    w2.Free;
  end;
  result:=true;
end;

function TDBSchemaSyncPostgres.akt_widoki_create_postgres: boolean;
var
  p1,p2,p3: TStringList; //lista objektów do aktualizacji
  w1,ww1,ww2: TStringList; //lista widoków istniejących w systemie
  i,a,b,aa: integer;
  max,ee: integer;
  pom: string;
  o_typ: integer;
  o_rodzaj,o_nazwa: string;
begin
  a:=0; b:=0; ee:=0;
  if ogens then AppendStrings(gens,'Stworzenie nowych aktualnych widoków');
  sq1.SQL.Clear;
  sq1.SQL.Add('select nazwa,definicja,typ from tabele where typ in (2,4) order by id');
  p1:=TStringList.Create;
  p2:=TStringList.Create;
  p3:=TStringList.Create;
  w1:=TStringList.Create;
  ww1:=TStringList.Create;
  ww2:=TStringList.Create;
  try
    (* wczytuje wszystkie widoki *)
    sq1.Open;
    while not sq1.EOF do
    begin
      p1.Add(sq1.FieldByName('nazwa').AsString);
      p2.Add(sq1.FieldByName('definicja').AsString);
      p3.Add(sq1.FieldByName('typ').AsString);
      sq1.Next;
    end;
    sq1.Close;
    max:=p1.Count;
    if Assigned(FStatusEvent) then FStatusEvent(self,-1,'c','Create Funcs & Views in Postgres','','',true,max,p1.Count);
    (* aktualizuję i udane usuwam z listy - przelatuję tak dopóki nie oczyszczę całkowicie listy widoków *)
    ShowViewsPostgres(w1);
    ShowFunctionsPostgres(ww1,nil,ww2);
    q1.ParamCheck:=false;
    while p1.Count>0 do
    begin
      a:=p1.Count;
      for i:=p1.Count-1 downto 0 do
      begin

        if p3[i]='2' then
        begin
          o_typ:=2;
          o_rodzaj:='View';
          o_nazwa:=p1[i];
          (* WIDOKI *)
          if StringToItemIndex(w1,p1[i],-1)=-1 then
          begin
            (* zakładam widok *)
            q1.SQL.Clear;
            pom:=p2[i];
            //pom:=StringReplace(pom,':','::',[rfReplaceAll]);
            q1.SQL.Add(pom);
            try
              if ogens then AppendStrings(q1.SQL,gens,o_nazwa) else
              begin
                q1.ExecSQL;
                if FSyncLog then AppendStrings(q1.SQL,gens,o_nazwa);
              end;
              p1.Delete(i);
              p2.Delete(i);
              p3.Delete(i);
            except
              on E: Exception do
              begin
                //if pos('nie istnieje',E.Message)=0 then log.Add('SYNC-CREATE: Błąd podczas tworzenia widoku: ['+p1[i]+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
                log.Add('SYNC-CREATE: Błąd podczas tworzenia widoku: ['+p1[i]+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
                result:=false;
              end;
            end;
          end else begin
            (* usuwam istniejące - tych widoków nie trzeba zakładać *)
            p1.Delete(i);
            p2.Delete(i);
            p3.Delete(i);
          end;
        end else begin
          o_typ:=4;
          o_rodzaj:='Function';
          o_nazwa:=p1[i];
          (* FUNKCJE *)
          aa:=StringToItemIndex(ww1,p1[i],-1);

          //if aa=-1 then log.Add('BRAKUJE FUNKCJI: '+o_nazwa);

          if aa>-1 then if ww2[aa]<>p2[i] then
          begin
            //log.Add('RÓŻNICA FUNKCJI: '+o_nazwa);
            //log.Add(ww2[aa]);
            //log.Add(p2[i]);
            aa:=-1;
          end;

          if aa=-1 then
          begin
            (* zakładam funkcję *)
            q1.SQL.Clear;
            pom:=p2[i];
            q1.SQL.Add(pom);
            try
              if ogens then AppendStrings(q1.SQL,gens,o_nazwa) else
              begin
                q1.ExecSQL;
                q1.SQL.Add(''); //do oddzielenia funkcji pustą linią w samym skrypcie
                if FSyncLog then AppendStrings(q1.SQL,gens,o_nazwa);
              end;
              p1.Delete(i);
              p2.Delete(i);
              p3.Delete(i);
            except
              on E: Exception do
              begin
                log.Add('SYNC-CREATE: Błąd podczas tworzenia funkcji: ['+p1[i]+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
                result:=false;
              end;
            end;
            //q1.ParamChar:=':';

          end else begin
            (* usuwam istniejące - tych funkcji nie trzeba zakładać *)
            p1.Delete(i);
            p2.Delete(i);
            p3.Delete(i);
          end;
        end;

        if Assigned(FStatusEvent) then FStatusEvent(self,o_typ,'C','Create Funcs & Views in Postgres',o_nazwa,o_rodzaj,true,0,p1.Count);

      end;
      b:=p1.Count;
      if (b>0) and (a=b) then
      begin
        inc(ee);
        if ee>=4 then
        begin
          log.Add('');
          log.Add('Nie zostały zaktualizowane poniższe funkcje/widoki:');
          for i:=0 to p1.Count-1 do
          begin
            case StrToInt(p3[i]) of
              2: log.Add('Widok:    '+p1[i]);
              4: log.Add('Funkcja:  '+p1[i]);
            end;
          end;
          break;
        end;
      end;
    end;
  finally
    p1.Free;
    p2.Free;
    p3.Free;
    w1.Free;
    ww1.Free;
    ww2.Free;
    q1.ParamCheck:=true;
  end;
  result:=true;
end;

function TDBSchemaSyncPostgres.sync_event: boolean;
var
  p1,p2: TStringList;
  nazwa,def,s: string;
  a: integer;
  b: boolean;
  q: TZSQLProcessor;
begin
  (* aktualizacja events - czyli funkcje, procedury, widoki i wyzwalacze *)
  result:=true;
  b:=true;
  list.Clear;
  q:=TZSQLProcessor.Create(nil);
  q.Connection:=FDB;
  q.DelimiterType:=dtDelimiter;
  q.Delimiter:=';;';
  q.ParamCheck:=false;
  p1:=TStringList.Create;
  p2:=TStringList.Create;
  try
    if proto=2 then
    begin
      akt_widoki_create_postgres; //FUNKCJE I WIDOKI
      (* AGREGATY *)
      if Assigned(FStatusEvent) then FStatusEvent(self,21,'C','Create Aggregates in Postgres','','',false,0,0);
      ShowAggregatesPostgres(p1,p2);
      if ogens then AppendStrings(gens,'Aktualizacja agregatów');
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=21 order by id');
      sq1.Open;
      while not sq1.EOF do
      begin
        nazwa:=sq1.FieldByName('nazwa').AsString;
        if Assigned(FStatusEvent) then FStatusEvent(self,21,'c','Create Aggregates in Postgres',nazwa,'Aggregate',false,0,0);
        def:=sq1.FieldByName('definicja').AsString;
        a:=StringToItemIndex(p1,nazwa,-1);
        if a>-1 then s:=p2[a];
        if a<>-1 then if p2[a]<>def then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,21,'D','Create Aggregates in Postgres',nazwa,'Aggregate',false,0,0);
          q1.SQL.Clear;
          q1.SQL.Add('drop aggregate '+nazwa);
          if ogens then AppendStrings(q1.SQL,gens,nazwa) else
          begin
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
          end;
          a:=-1;
        end;
        if a=-1 then
        begin
          if Assigned(FStatusEvent) then FStatusEvent(self,21,'C','Create Aggregates in Postgres',nazwa,'Aggregate',false,0,0);
          q.Script.Clear;
          q.Script.Add(def+';;');
          try
            if ogens then AppendStrings(q.Script,gens,nazwa) else
            begin
              q.Execute;
              if FSyncLog then AppendStrings(q.Script,gens,nazwa);
            end;
          except
            on E: Exception do
            begin
              log.Add('SYNC-CREATE: Błąd podczas tworzenia agregatu: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
              result:=false;
            end;
          end;
        end;
        sq1.Next;
      end;
      sq1.Close;
    end;
    if proto=1 then
    begin
      (* funkcje *)
      ShowFunctions(p1,p2);
      if ogens then AppendStrings(gens,'Aktualizacja funkcji');
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=4 order by id');
      sq1.Open;
      while not sq1.EOF do
      begin
        nazwa:=sq1.FieldByName('nazwa').AsString;
        def:=sq1.FieldByName('definicja').AsString;
        a:=StringToItemIndex(p1,nazwa,-1);
        if a>-1 then s:=p2[a];
        if a<>-1 then if p2[a]<>def then
        begin
          q1.SQL.Clear;
          q1.SQL.Add('drop function '+nazwa+';');
          if ogens then AppendStrings(q1.SQL,gens,nazwa) else
          begin
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
          end;
          a:=-1;
        end;
        if a=-1 then
        begin
          q.Script.Clear;
          q.Script.Add(def+';;');
          try
            if ogens then AppendStrings(q.Script,gens,nazwa) else
            begin
              q.Execute;
              if FSyncLog then AppendStrings(q.Script,gens,nazwa);
            end;
          except
            on E: Exception do
            begin
              log.Add('SYNC-CREATE: Błąd podczas tworzenia funkcji: ['+nazwa+']:'+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
              result:=false;
            end;
          end;
        end;
        sq1.Next;
      end;
      sq1.Close;
    end;
    (* procedury *)
    if proto=1 then
    begin
      if ogens then AppendStrings(gens,'Aktualizacja procedur');
      ShowProcedures(p1,p2);
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=5 order by id');
      sq1.Open;
      while not sq1.EOF do
      begin
        nazwa:=sq1.FieldByName('nazwa').AsString;
        def:=sq1.FieldByName('definicja').AsString;
        a:=StringToItemIndex(p1,nazwa,-1);
        if a<>-1 then if p2[a]<>def then
        begin
          q1.SQL.Clear;
          q1.SQL.Add('drop procedure '+nazwa);
          if ogens then AppendStrings(q1.SQL,gens,nazwa) else
          begin
            q1.ExecSQL;
            if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
          end;
          a:=-1;
        end;
        if a=-1 then
        begin
          q.Script.Clear;
          q.Script.Add(def+';;');
          if ogens then AppendStrings(q.Script,gens,nazwa) else
          begin
            q.Execute;
            if FSyncLog then AppendStrings(q.Script,gens,nazwa);
          end;
        end;
        sq1.Next;
      end;
      sq1.Close;
    end;
    (* widoki *)
    if proto=1 then
    begin
      if ogens then AppendStrings(gens,'Aktualizacja widoków');
      ShowTables(p1);
      sq1.SQL.Clear;
      sq1.SQL.Add('select nazwa,definicja from tabele where typ=2 order by id');
      sq1.Open;
      while not sq1.EOF do
      begin
        nazwa:=sq1.FieldByName('nazwa').AsString;
        def:=sq1.FieldByName('definicja').AsString;
        while (def[length(def)]=#10) or (def[length(def)]=#13) do delete(def,length(def),1);
        a:=StringToItemIndex(p1,nazwa,-1);
        if a<>-1 then
        begin
          ShowCreateTable(nazwa,s);
          if s<>def then
          begin
            q1.SQL.Clear;
            q1.SQL.Add('drop view '+nazwa+';');
            if ogens then AppendStrings(q1.SQL,gens,nazwa) else
            begin
              q1.ExecSQL;
              if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
            end;
            a:=-1;
          end;
        end;
        if a=-1 then
        begin
          q.Script.Clear;
          q.Script.Add(def+';;');
          if ogens then AppendStrings(q.Script,gens,nazwa) else
          begin
            q.Execute;
            if FSyncLog then AppendStrings(q.Script,gens,nazwa);
          end;
        end;
        sq1.Next;
      end;
      sq1.Close;
    end;
    (* wyzwalacze *)
    case proto of
      1: ShowTriggers(p1,p2);
      2: ShowTriggersPostgres(p1,p2);
    end;
    if ogens then AppendStrings(gens,'Aktualizacja wyzwalaczy');
    sq1.SQL.Clear;
    sq1.SQL.Add('select nazwa,definicja from tabele where typ=3 order by id');
    sq1.Open;
    while not sq1.EOF do
    begin
      nazwa:=sq1.FieldByName('nazwa').AsString;
      def:=sq1.FieldByName('definicja').AsString;
      a:=StringToItemIndex(p1,nazwa,-1);
      if a<>-1 then if p2[a]<>def then
      begin
        q1.SQL.Clear;
        q1.SQL.Add('drop trigger '+nazwa+';');
        if ogens then AppendStrings(q1.SQL,gens,nazwa) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,nazwa);
        end;
        a:=-1;
      end;
      if a=-1 then
      begin
        q.Script.Clear;
        q.Script.Add(def+';;');
        if ogens then AppendStrings(q.Script,gens,nazwa) else
        begin
          q.Execute;
          if FSyncLog then AppendStrings(q.Script,gens,nazwa);
        end;
      end;
      sq1.Next;
    end;
    sq1.Close;
  finally
    q.Free;
    p1.Free;
    p2.Free;
  end;
  result:=b;
end;

procedure TDBSchemaSyncPostgres.usun_zaleznosci(tabela, kolumna: string);
var
  q: TZQuery;
  s1,s2: string;
begin
  _postgres_str_2_schema_table(tabela,s1,s2);
  q:=TZQuery.Create(nil);
  q.Connection:=FDB;
  q.SQL.Add('select distinct dependee.relname from pg_depend');
  q.SQL.Add('join pg_rewrite on pg_depend.objid=pg_rewrite.oid');
  q.SQL.Add('join pg_class as dependee on pg_rewrite.ev_class=dependee.oid');
  q.SQL.Add('join pg_class as dependent on pg_depend.refobjid=dependent.oid');
  q.SQL.Add('join pg_attribute on pg_depend.refobjid=pg_attribute.attrelid and pg_depend.refobjsubid=pg_attribute.attnum');
  q.SQL.Add('where dependent.relname=:tabela');
  q.SQL.Add('  and pg_attribute.attnum>0');
  q.SQL.Add('  and pg_attribute.attname=:kolumna');
  q.ParamByName('tabela').AsString:=s2;
  q.ParamByName('kolumna').AsString:=kolumna;
  try
    q.Open;
    while not q.EOF do
    begin
      q1.SQL.Clear;
      q1.SQL.Add('drop view if exists '+s1+'.'+q.Fields[0].AsString+' cascade;');
      try
        if ogens then AppendStrings(q1.SQL,gens,s1+'.'+q.Fields[0].AsString) else
        begin
          q1.ExecSQL;
          if FSyncLog then AppendStrings(q1.SQL,gens,s1+'.'+q.Fields[0].AsString);
        end;
      except end; //ignorujemy błędy
      q.Next;
    end;
    q.Close;
  finally
    q.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.get_alter_table_postgres(nazwa, s1, s2: string;
  var s_pole, s_null, s_default: string);
var
  a,b,c,space: integer;
  pom: string;
begin
  usun_zaleznosci(nazwa,s1);
  space:=pos(' ',s2);
  a:=pos('NOT NULL',s2);
  b:=pos('NULL',s2);
  c:=pos('DEFAULT',s2);
  if c>0 then pom:=copy(s2,space+1,c-space-2) else
  if a>0 then pom:=copy(s2,space+1,a-space-2) else
  if b>0 then pom:=copy(s2,space+1,b-space-2) else pom:=usun_pierwszy_wiersz(s2);
  s_pole:=trim('ALTER TABLE '+nazwa+' ALTER COLUMN '+s1+' TYPE '+pom);
  if a>0 then s_null:='ALTER TABLE '+nazwa+' ALTER COLUMN '+s1+' SET NOT NULL'
         else s_null:='ALTER TABLE '+nazwa+' ALTER COLUMN '+s1+' DROP NOT NULL';
  if c>0 then
  begin
    if a>0 then pom:=copy(s2,c,a-c-1) else if b>0 then pom:=copy(s2,c,b-c-1);
    a:=pos(' NOT',pom);
    if a>0 then delete(pom,a,4);
    s_default:=trim('ALTER TABLE '+nazwa+' ALTER COLUMN '+s1+' SET '+pom);
  end else s_default:='ALTER TABLE '+nazwa+' ALTER COLUMN '+s1+' DROP DEFAULT';
end;

procedure TDBSchemaSyncPostgres.postgres_TableStructAppendKeys(s1, s2: string;
  schema: TStrings);
var
  ilosc,ile: integer;
  s: string;
begin
  q1.SQL.Clear;
  q1.SQL.Add('SELECT');
  q1.SQL.Add('  ns.nspname schemat,');
  q1.SQL.Add('  cl.relname tabela,');
  q1.SQL.Add('  r.conname klucz,');
  q1.SQL.Add('  r.contype rodzaj,');
  q1.SQL.Add('  pg_catalog.pg_get_constraintdef(r.oid, true) as condef');
  q1.SQL.Add('FROM pg_catalog.pg_constraint r');
  q1.SQL.Add('join pg_class cl on cl.oid = r.conrelid');
  q1.SQL.Add('join pg_namespace ns on ns.oid = cl.relnamespace');
  q1.SQL.Add('where contype in (''p'',''f'')');
  q1.SQL.Add('  and ns.nspname=:schema');
  q1.SQL.Add('  and cl.relname=:table');
  q1.SQL.Add('order by ns.nspname,cl.relname,r.contype desc');
  q1.ParamByName('schema').AsString:=s1;
  q1.ParamByName('table').AsString:=s2;
  q1.Open;
  ilosc:=q1.RecordCount;
  if ilosc>0 then
  begin
    (*  usuwamy ostatnią linię ze znakami ");" *)
    schema.Delete(schema.Count-1);
    (* teraz nowa linijka ostatnia dodajemy znaczek "," *)
    s:=schema[schema.Count-1];
    schema.Delete(schema.Count-1);
    schema.Add(s+',');
    (* dodajemy klucze *)
    ile:=0;
    while not q1.EOF do
    begin
      inc(ile);
      if ile<ilosc then
        schema.Add('  CONSTRAINT '+q1.FieldByName('klucz').AsString+' '+q1.FieldByName('condef').AsString+',')
      else
        schema.Add('  CONSTRAINT '+q1.FieldByName('klucz').AsString+' '+q1.FieldByName('condef').AsString);
      q1.Next;
    end;
    schema.Add(');');
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ustaw_plik_skryptu(FilenameDB: string);
var
  b: boolean;
  tekst: TStringList;
  s: string;
begin
  b:=false;
  if ogens then b:=true else if FSyncLog then b:=true;
  if b then
  begin
    AppendStrings(gens,'Dane inicjujące');
    tekst:=TStringList.Create;
    try
      tekst.Add('SET client_encoding = ''UTF8'';');
      AppendStrings(tekst,gens,'');
    finally
      tekst.Free;
    end;
  end;
  AppendStrings(gens,'Treść skryptu');
  if FSToSqlite and (FilenameDB<>'') then
  begin
    s:=StringReplace(FilenameDB,'.sql','.sqlite',[rfReplaceAll]);
    if FileExists(s) then DeleteFile(s);
    sdb2.Protocol:='sqlite-3';
    sdb2.Database:=s;
    sdb2.Connect;
    (* tabele i widoki *)
    sq22.SQL.Clear;
    sq22.SQL.Add('create table lista (id integer primary key autoincrement,schemat text,definicja blob)');
    sq22.ExecSQL;
    sq22.SQL.Clear;
    sq22.SQL.Add('insert into lista (schemat,definicja) values (:schemat,:definicja)');
    sq22.Prepare;
    StartTransaction2;
  end else sdb2.Database:='';
end;

//procedura uzupelnia kontrolki o kolejne uprawnienia
//jesli nie zostanie podana tabela - zostana wczytane uprawnienia wszystkich objektów w bazie
procedure TDBSchemaSyncPostgres.czytaj_przywileje_postgres(tabele,
  przywileje: TStrings; tabela: string);
var
  q: TZQuery;
  s: string;
  s1,s2,s3: string;
  z1: string;
begin
  if Assigned(FStatusEvent) then FStatusEvent(self,-1,'I','Synchro Schema Prepare','Czytam przywileje','',false,0,0);
  z1:='';
  q:=TZQuery.Create(nil);
  q.Connection:=FDB;
  q.SQL.Add('select concat(table_schema,''.'',table_name),grantee,string_agg(privilege_type,'','') from information_schema.role_table_grants');
  if tabela='' then q.SQL.Add('where table_schema not in (''pg_catalog'',''information_schema'')')
               else q.SQL.Add('where table_schema not in (''pg_catalog'',''information_schema'') and table_schema=:schema and table_name=:table');
  q.SQL.Add('group by concat(table_schema,''.'',table_name),grantee');
  try
    q.Open;
    while not q.EOF do
    begin
      s1:=q.Fields[0].AsString; //tabela
      s2:=q.Fields[1].AsString; //uzytkownik
      s3:=q.Fields[2].AsString; //uprawnienia
      if z1<>s1 then
      begin
        if (z1<>'') and (s<>'') then
        begin
          tabele.Add(z1);
          przywileje.Add(s);
          s:='';
        end;
        z1:=s1;
        s:=s2+#9+s3;
      end else s:=s+#9+s2+#9+s3;
      q.Next;
    end;
    if s<>'' then
    begin
      tabele.Add(z1);
      przywileje.Add(s);
    end;
    q.Close;
  finally
    q.Free;
  end;
end;

//procedura odtwarza uprawnienia
//jesli nie zostanie podana tabela - zostana odtworzone uprawnienia na wszystkich objektach w bazie
procedure TDBSchemaSyncPostgres.zapisz_przywileje_postgres(tabele,
  przywileje: TStrings; tabela: string);
var
  q: TZQuery;
  i,j: integer;
  s1,s2,z1,z2: string;
begin
  if Assigned(FStatusEvent) then FStatusEvent(self,-1,'I','Synchro Schema Prepare','Odtwarzam przywileje','',false,0,0);
  q:=TZQuery.Create(nil);
  q.Connection:=FDB;
  try
    for i:=0 to tabele.Count-1 do if (tabela='') or ((tabela<>'') and (tabele[i]=tabela)) then
    begin
      s1:=tabele[i];
      s2:=przywileje[i];
      (* odtworzenie uprawnień *)
      j:=0;
      while true do
      begin
        inc(j); z1:=GetLineToStr(s2,j,#9);
        inc(j); z2:=GetLineToStr(s2,j,#9);
        if (z1='') and (z2='') then break;
        q.SQL.Clear;
        q.SQL.Add('grant '+z2+' on '+s1+' to '+z1+';');
        if ogens then AppendStrings(q.SQL,gens,'Odtworzenie uprawnień') else
        begin
          q.ExecSQL;
          if FSyncLog then AppendStrings(q.SQL,gens,s1);
        end;
      end;
    end;
  finally
    q.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.export_data_table(tabela: string);
var
  q: TZQuery;
  tab,flagi,s,s1,pom: string;
  i: integer;
begin
  if FDataSyncOption=dsNone then exit;
  if tabela[1]='^' then
  begin
    tab:=tabela;
    delete(tab,1,1);
    if tab[length(tab)]<>';' then tab:=tab+';';
    flagi:='E';
  end else begin
    tab:=GetLineToStr(tabela,1,' ');
    flagi:=GetLineToStr(tabela,2,' ');
  end;
  q:=TZQuery.Create(nil);
  q.Connection:=FDB;
  try
    if pos('E',flagi)>0 then
    begin
      (* EXECUTE *)
      sq1.ParamByName('nazwa').AsString:='ExeSql';
      sq1.ParamByName('typ').AsInteger:=50;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=tab;
      sq1.ParamByName('jezyk').Clear;
      sq1.ParamByName('obiekt').Clear;
      sq1.ExecSQL;
    end else if pos('S',flagi)>0 then
    begin
      (* SQUENCES *)
      q.SQL.Add('select last_value from '+tab);
      try
        q.Open;
        s:='select setval('''+tab+''','+IntToStr(q.Fields[0].AsLargeInt)+',true);';
        q.Open;
        sq1.ParamByName('nazwa').AsString:=tab;
        sq1.ParamByName('typ').AsInteger:=50;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=s;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      except
      end;
    end else begin
      (* DEFAULT SELECT TABLES *)
      q.SQL.Add('select * from '+tab);
      try
        q.Open;
        (* wykonanie flag *)
        if pos('D',flagi)>0 then
        begin
          sq1.ParamByName('nazwa').AsString:=tab;
          sq1.ParamByName('typ').AsInteger:=50;
          sq1.ParamByName('z_czasu').Clear;
          sq1.ParamByName('definicja').AsString:='delete from '+tab+';';
          sq1.ParamByName('jezyk').Clear;
          sq1.ParamByName('obiekt').Clear;
          sq1.ExecSQL;
        end;
        (* export danych *)
        while not q.EOF do
        begin
          s1:='';
          for i:=0 to q.Fields.Count-1 do
          begin
            if q.Fields[i].IsNull then pom:='NULL' else pom:=''''+StringReplace(q.Fields[i].AsString,'''','''''',[rfReplaceAll])+'''';
            if s1='' then s1:=pom else s1:=s1+','+pom;
          end;
          s:='insert into '+tab+' values ('+s1+');';
          sq1.ParamByName('nazwa').AsString:=tab;
          sq1.ParamByName('typ').AsInteger:=50;
          sq1.ParamByName('z_czasu').Clear;
          sq1.ParamByName('definicja').AsString:=s;
          sq1.ParamByName('jezyk').Clear;
          sq1.ParamByName('obiekt').Clear;
          sq1.ExecSQL;
          q.Next;
        end;
        q.Close;
      except
      end;
    end;
  finally
    q.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.import_data_table;
var
  q: TZQuery;
  blok: TZSQLProcessor;
  i: integer;
begin
  if FDataSyncOption=dsNone then exit;
  (* część nagłówka *)
  if FDataSyncHeader.Count>0 then
  begin
    if ogens or FSyncLog then AppendStrings('Odtworzenie danych - część wstępna');
    blok:=TZSQLProcessor.Create(self);
    try
      for i:=0 to FDataSyncHeader.Count-1 do blok.Script.Add(FDataSyncHeader[i]);
      if ogens then AppendStrings(blok.Script,gens,'') else
      begin
        if (FDataSyncOption=dsNormal) or (FDataSyncOption=dsOnlySql) then try blok.Execute except end;
        if FSyncLog and ((FDataSyncOption=dsNormal) or (FDataSyncOption=dsOnlyScript)) then AppendStrings(blok.Script,gens,'');
        if Assigned(FSyncDataExecute) then FSyncDataExecute(self,self.Tag,trim(blok.Script.Text));
      end;
    finally
      blok.Free;
    end;
  end;
  (* część główna *)
  if ogens or FSyncLog then AppendStrings('Odtworzenie danych - część główna');
  sq1.SQL.Clear;
  sq1.SQL.Add('select nazwa,definicja from tabele where typ=50 order by id');
  sq1.Open;
  if sq1.RecordCount>0 then
  begin
    q:=TZQuery.Create(nil);
    q.Connection:=FDB;
    q.ParamCheck:=false;
    try
      (* import danych *)
      while not sq1.EOF do
      begin
        q.SQL.Clear;
        q.SQL.Add(sq1.FieldByName('definicja').AsString);
        if ogens then AppendStrings(q.SQL,gens,sq1.FieldByName('nazwa').AsString) else
        begin
          if (FDataSyncOption=dsNormal) or (FDataSyncOption=dsOnlySql) then try q.ExecSQL except end;
          if FSyncLog and ((FDataSyncOption=dsNormal) or (FDataSyncOption=dsOnlyScript)) then AppendStrings(q.SQL,gens,sq1.FieldByName('nazwa').AsString);
          if Assigned(FSyncDataExecute) then FSyncDataExecute(self,self.Tag,trim(q.SQL.Text));
        end;
        sq1.Next;
      end;
    finally
      q.Free;
    end;
  end;
  sq1.Close;
  (* część stopki *)
  if FDataSyncFooter.Count>0 then
  begin
    if ogens or FSyncLog then AppendStrings('Odtworzenie danych - część kończąca');
    blok:=TZSQLProcessor.Create(self);
    try
      for i:=0 to FDataSyncFooter.Count-1 do blok.Script.Add(FDataSyncFooter[i]);
      if ogens then AppendStrings(blok.Script,gens,'') else
      begin
        if (FDataSyncOption=dsNormal) or (FDataSyncOption=dsOnlySql) then try blok.Execute except end;
        if FSyncLog and ((FDataSyncOption=dsNormal) or (FDataSyncOption=dsOnlyScript)) then AppendStrings(blok.Script,gens,'');
        if Assigned(FSyncDataExecute) then FSyncDataExecute(self,self.Tag,trim(blok.Script.Text));
      end;
    finally
      blok.Free;
    end;
  end;
end;

function TDBSchemaSyncPostgres.FuncPostgresToLanguage(definicja: string): string;
var
  s: string;
  a: integer;
begin
  s:=StringReplace(definicja,#13#10,' ',[rfReplaceAll]);
  s:=StringReplace(s,#13,' ',[rfReplaceAll]);
  a:=pos('LANGUAGE',s);
  delete(s,1,a+8);
  a:=pos(' ',s);
  delete(s,a,length(s));
  result:=trim(s);
end;

function TDBSchemaSyncPostgres.PostgresIndexToFullName(s1, s2: string): string;
var
  s: string;
  a: integer;
begin
  s:=s2;
  a:=pos(' ON ',s);
  delete(s,1,a+3);
  a:=pos('.',s);
  result:=trim(copy(s,1,a-1))+'.'+s1;
end;

procedure TDBSchemaSyncPostgres.ZapiszSkryptDoPliku(filename: string;
  SyncLogTest: boolean);
var
  strumien: TMemoryStream;
  plik: TFileStream;
  bufor: string[255];
  ile: integer;
begin
  (* zapisuję skrypt do pliku *)
  //gens.SaveToFile(filename);
  //exit;

  if SyncLogTest then
  begin
    gens.Insert(0,'BEGIN;');
    gens.Add('');
    gens.Add('ROLLBACK;');
  end;

  (* zapisuję skrypt aklualizując zawartość w locie *)
  //if FileExists(filename) then DeleteFile(filename);
  strumien:=TMemoryStream.Create;
  plik:=TFileStream.Create(filename,fmCreate);
  try
    gens.SaveToStream(strumien);
    strumien.Position:=0;
    while true do
    begin
      ile:=strumien.Read(bufor[1],255);
      if ile=0 then break;
      SetLength(bufor,ile);
      bufor:=StringReplace(bufor,#13#10,#10,[rfReplaceAll]);
      plik.Write(bufor[1],length(bufor));
    end;
  finally
    plik.Free;
    strumien.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.last_indeksy_create_postgres;
var
  i: integer;
begin
  for i:=0 to lastindexes.Count-1 do
  begin
    (* wykonuję wszystkie instrukcje krok po kroku *)
    q1.SQL.Clear;
    q1.SQL.Add(lastindexes[i]);
    q1.ParamCheck:=false;
    try
      try
        if ogens then AppendStrings(q1.SQL,gens,'') else
        begin
          q1.ExecSQL;
          if FSyncLog then
          begin
            DeleteStrings(q1.SQL,gens);
            AppendStrings(q1.SQL,gens,'');
          end;
        end;
      except
        on E: Exception do log.Add('SYNC-CREATE-LAST: Błąd podczas tworzenia indeksu: '+#13#10+q1.SQL.Text+#13#10#13#10+E.Message);
      end;
    finally
      q1.ParamCheck:=true;
    end;
  end;
end;

constructor TDBSchemaSyncPostgres.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRequest:=TStringList.Create;
  FDataSync:=TStringList.Create;
  FDataSyncFooter:=TStringList.Create;
  FDataSyncHeader:=TStringList.Create;
  list:=TStringList.Create;
  gens:=TStringList.Create;
  log:=TStringList.Create;
  lastindexes:=TStringList.Create;
  przywileje1:=TStringList.Create;
  przywileje2:=TStringList.Create;
  sdb:=TZConnection.Create(nil);
  sdb2:=TZConnection.Create(nil);
  q1:=TZQuery.Create(nil);
  q2:=TZQuery.Create(nil);
  qq:=TZQuery.Create(nil);
  sq1:=TZQuery.Create(nil);
  sq2:=TZQuery.Create(nil);
  sq22:=TZQuery.Create(nil);
  sq1.Connection:=sdb;
  sq2.Connection:=sdb;
  sq22.Connection:=sdb2;
  FZnacznikCzasu:=false;
  FGrantRemember:=false;
  FNoDropObjects:=[];
  FSyncLog:=false;
  FDataSyncOption:=dsNone;
  FSToSqlite:=false;
end;

destructor TDBSchemaSyncPostgres.Destroy;
begin
  FRequest.Free;
  FDataSync.Free;
  FDataSyncFooter.Free;
  FDataSyncHeader.Free;
  list.Free;
  gens.Free;
  log.Free;
  lastindexes.Free;
  przywileje1.Free;
  przywileje2.Free;
  q1.Free;
  q2.Free;
  qq.Free;
  sq1.Free;
  sq2.Free;
  sq22.Free;
  sdb.Free;
  sdb2.Free;
  inherited Destroy;
end;

procedure TDBSchemaSyncPostgres.init;
begin
  (* ustalamy połączenia *)
  q1.Connection:=FDB;
  q2.Connection:=FDB;
  qq.Connection:=FDB;
  (* ustawiamy protokół połączenia *)
  if pos('MYSQL',uppercase(FDB.Protocol))>0 then proto:=1 else
  if pos('POSTGRES',uppercase(FDB.Protocol))>0 then proto:=2 else
  proto:=0;
end;

procedure TDBSchemaSyncPostgres.ShowLanguagesPostgres(schema, cialo: TStrings);
var
  s: string;
begin
  schema.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select lanname,lanowner,lanispl,lanpltrusted,lanacl from pg_language where lanname not in (''internal'',''c'',''sql'')');
  q1.Open;
  while not q1.EOF do
  begin
    s:=q1.Fields[0].AsString;
    schema.Add(s);
    if s='plpgsql' then
      cialo.Add('CREATE EXTENSION IF NOT EXISTS '+s+' WITH SCHEMA pg_catalog;'+#13#10+'COMMENT ON EXTENSION '+s+' IS ''PL/pgSQL procedural language'';')
    else
      cialo.Add('CREATE OR REPLACE PROCEDURAL LANGUAGE '+s+';'+#13#10+'ALTER PROCEDURAL LANGUAGE '+s+' OWNER TO postgres;');
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowSchemasPostgres(schema, cialo: TStrings);
var
  s: string;
begin
  schema.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select schema_name from information_schema.schemata where catalog_name=:database and schema_name not like ''pg_%'' and schema_name<>''information_schema'' and schema_name<>''public''');
  q1.ParamByName('database').AsString:=FDB.Database;
  q1.Open;
  while not q1.EOF do
  begin
    s:=q1.Fields[0].AsString;
    schema.Add(s);
    cialo.Add('CREATE SCHEMA '+s+';');
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowTypesPostgres(nazwa, cialo: TStrings);
var
  r1,r2: TZQuery;
  schema,type_name: string;
  s1,s2: string;
begin
  nazwa.Clear;
  cialo.Clear;
  r1:=TZQuery.Create(nil);
  r2:=TZQuery.Create(nil);
  r1.Connection:=FDB;
  r2.Connection:=FDB;
  r1.SQL.Add('SELECT');
  r1.SQL.Add('  distinct');
  r1.SQL.Add('  aa.udt_schema,aa.udt_name,concat(aa.udt_schema,''.'',aa.udt_name) as nazwa');
  r1.SQL.Add('FROM pg_class c');
  r1.SQL.Add('JOIN pg_namespace n on c.relnamespace=n.oid');
  r1.SQL.Add('JOIN pg_attribute a ON c.oid = a.attrelid');
  r1.SQL.Add('JOIN pg_type t ON a.atttypid = t.oid');
  r1.SQL.Add('JOIN information_schema.attributes aa ON aa.udt_name=c.relname');
  r1.SQL.Add('                                     and aa.ordinal_position=a.attnum');
  r1.SQL.Add('                                     and aa.attribute_name=a.attname');
  r1.SQL.Add('                                     and aa.udt_schema=n.nspname');
  r1.SQL.Add('WHERE aa.udt_catalog = :database');
  r1.SQL.Add('order by aa.udt_schema,aa.udt_name');
  r2.SQL.Add('SELECT');
  r2.SQL.Add('  concat(aa.udt_schema,''.'', aa.udt_name) nazwa,');
  r2.SQL.Add('  c.relname, a.attname, a.attnum, t.typname,');
  r2.SQL.Add('  aa.attribute_default,');
  r2.SQL.Add('  aa.is_nullable,');
  r2.SQL.Add('  aa.data_type,');
  r2.SQL.Add('  aa.character_maximum_length,');
  r2.SQL.Add('  aa.numeric_precision,');
  r2.SQL.Add('  aa.numeric_precision_radix,');
  r2.SQL.Add('  aa.numeric_scale,');
  r2.SQL.Add('  aa.attribute_udt_name,');
  r2.SQL.Add('  aa.dtd_identifier');
  r2.SQL.Add('FROM pg_class c');
  r2.SQL.Add('JOIN pg_namespace n on c.relnamespace=n.oid');
  r2.SQL.Add('JOIN pg_attribute a ON c.oid = a.attrelid');
  r2.SQL.Add('JOIN pg_type t ON a.atttypid = t.oid');
  r2.SQL.Add('JOIN information_schema.attributes aa ON aa.udt_name=c.relname');
  r2.SQL.Add('                                     and aa.ordinal_position=a.attnum');
  r2.SQL.Add('                                     and aa.attribute_name=a.attname');
  r2.SQL.Add('                                     and aa.udt_schema=n.nspname');
  r2.SQL.Add('WHERE aa.udt_catalog = :database and aa.udt_schema=:schema and aa.udt_name=:name');
  r2.SQL.Add('order by attnum');
  r1.ParamByName('database').AsString:=FDB.Database;
  r2.ParamByName('database').AsString:=FDB.Database;
  r1.Prepare;
  r2.Prepare;
  try
    r1.Open;
    while not r1.EOF do
    begin
      schema:=r1.Fields[0].AsString;
      type_name:=r1.Fields[1].AsString;
      nazwa.Add(schema+'.'+type_name);
      s1:='CREATE TYPE '+schema+'.'+type_name+' AS (';
      s2:='';
      r2.ParamByName('schema').AsString:=schema;
      r2.ParamByName('name').AsString:=type_name;
      r2.Open;
      while not r2.EOF do
      begin
        if s2<>'' then
        begin
          s2:=s2+',';
          s1:=s1+#13#10+'  '+s2;
          s2:='';
        end;
        if r2.FieldByName('data_type').AsString='numeric' then
          s2:=r2.FieldByName('attname').AsString+' '+r2.FieldByName('typname').AsString+'('+r2.FieldByName('numeric_precision').AsString+','+r2.FieldByName('numeric_scale').AsString+')'
        else
        if (r2.FieldByName('typname').AsString='varchar') or (r2.FieldByName('typname').AsString='char') then
          s2:=r2.FieldByName('attname').AsString+' '+r2.FieldByName('typname').AsString+'('+r2.FieldByName('character_maximum_length').AsString+')'
        else
          s2:=r2.FieldByName('attname').AsString+' '+r2.FieldByName('data_type').AsString;
        r2.Next;
      end;
      if s2<>'' then
      begin
        s1:=s1+#13#10+'  '+s2;
        s2:='';
      end;
      s1:=s1+#13#10+');';
      s1:=StringReplace(s1,'(,)','',[rfReplaceAll]);
      s1:=StringReplace(s1,'()','',[rfReplaceAll]);
      cialo.Add(s1);
      r2.Close;
      r1.Next;
    end;
    r1.Close;
  finally
    r1.Free;
    r2.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.ShowTables(tables: TStrings);
begin
  tables.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('show tables');
  q1.Open;
  while not q1.EOF do
  begin
    tables.Add(q1.Fields[0].AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowTablesPostgres(tables: TStrings);
var
  s: string;
begin
  tables.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select concat(table_schema,''.'',table_name) from information_schema.tables where table_catalog=:database and table_schema<>''pg_catalog'' and table_schema<>''information_schema'' and table_type=''BASE TABLE''');
  q1.ParamByName('database').AsString:=FDB.Database;
  q1.Open;
  while not q1.EOF do
  begin
    s:=q1.Fields[0].AsString;
    tables.Add(s);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowViewsPostgres(tables: TStrings);
var
  s: string;
begin
  //q1.ParamChar:=':';
  tables.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select concat(table_schema,''.'',table_name) from information_schema.tables where table_catalog=:database and table_schema<>''pg_catalog'' and table_schema<>''information_schema'' and table_type=''VIEW''');
  q1.ParamByName('database').AsString:=FDB.Database;
  q1.Open;
  while not q1.EOF do
  begin
    s:=q1.Fields[0].AsString;
    tables.Add(s);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowTriggers(nazwa, cialo: TStrings);
begin
  nazwa.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('show triggers');
  q1.Open;
  while not q1.EOF do
  begin
    nazwa.Add(q1.FieldByName('Trigger').AsString);
    list.Clear;
    list.Add('CREATE TRIGGER '+q1.FieldByName('Trigger').AsString+' '+q1.FieldByName('Timing').AsString+' '+q1.FieldByName('Event').AsString+' ON '+q1.FieldByName('Table').AsString+' FOR EACH ROW');
    list.Add(q1.FieldByName('Statement').AsString);
    cialo.Add(list.Text);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowTriggersPostgres(nazwa, cialo: TStrings);
begin
  nazwa.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select');
  q1.SQL.Add('  ns.nspname as schema,');
  q1.SQL.Add('  cl.relname as tabela,');
  q1.SQL.Add('  t.tgname as trigger,');
  q1.SQL.Add('  pg_get_triggerdef(t.oid) as body');
  q1.SQL.Add('from pg_trigger t');
  q1.SQL.Add('join pg_class cl on cl.oid = t.tgrelid');
  q1.SQL.Add('join pg_namespace ns on ns.oid = cl.relnamespace');
  q1.SQL.Add('where tgisinternal=false');
  q1.Open;
  while not q1.EOF do
  begin
    nazwa.Add(q1.FieldByName('trigger').AsString+' ON '+q1.FieldByName('schema').AsString+'.'+q1.FieldByName('tabela').AsString);
    list.Clear;
    list.Add(q1.FieldByName('body').AsString+';');
    cialo.Add(q1.FieldByName('body').AsString+';');
    q1.Next;
  end;
  q1.Close;
end;

function NormalizeCialoFunkcji(cialo:string):string;
var
  s: string;
  a,b,c: integer;
begin
  s:=StringReplace(cialo,'`','',[rfReplaceAll]);
  a:=pos('CREATE DEFINER=',s);
  if a>0 then
  begin
    b:=pos(' FUNCTION ',s);
    delete(s,a+6,b-a-6);
  end;
  a:=pos(' CHARSET ',s);
  if a>0 then
  begin
    b:=pos(#10,s);
    c:=pos(#13,s);
    if (b=0) and (c>0) then b:=c;
    if (b<>c) and (b>0) and (c>0) then if b>c then b:=c;
    if b>0 then delete(s,a,b-a);
  end;
  result:=s;
end;

procedure TDBSchemaSyncPostgres.ShowFunctions(nazwa, cialo: TStrings);
begin
  nazwa.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('show function status where db=database()');
  q1.Open;
  while not q1.EOF do
  begin
    nazwa.Add(q1.FieldByName('Name').AsString);
    q2.SQL.Clear;
    q2.SQL.Add('show create function '+q1.FieldByName('Name').AsString);
    q2.Open;
    cialo.Add(NormalizeCialoFunkcji(q2.FieldByName('Create Function').AsString));
    q2.Close;
    q1.Next;
  end;
  q1.Close;
end;

function FunctionNamePostgresToExtendedName(name,definicja:string):string;
var
  s: string;
  a,b: integer;
begin
  a:=pos('(',name);
  s:=copy(name,1,a-1);
  a:=pos('(',definicja);
  b:=pos('RETURNS',definicja);
  result:=s+trim(copy(definicja,a,b-a));
end;

procedure TDBSchemaSyncPostgres.ShowFunctionsPostgres(nazwa, nazwa2,
  cialo: TStrings; szukaj_schema: string; szukaj_name: string;
  szukaj_args: string);
var
  qq1: TZQuery;
  s_name,e_name,definicja: string;
begin
  nazwa.Clear;
  if nazwa2<>nil then nazwa2.Clear;
  cialo.Clear;

  qq1:=TZQuery.Create(self);
  qq1.Connection:=FDB;

  qq1.SQL.Add('SELECT');
  qq1.SQL.Add('  format(''%I.%I(%s)'', ns.nspname, p.proname, oidvectortypes(p.proargtypes))');
  qq1.SQL.Add('  ,  pg_get_functiondef(p.oid)');
  qq1.SQL.Add('FROM pg_proc p INNER JOIN pg_namespace ns ON p.pronamespace = ns.oid');
  qq1.SQL.Add('WHERE /*probin is null and*/ not p.proisagg and ns.nspname<>''pg_catalog'' and ns.nspname<>''information_schema''');
  if szukaj_schema<>'' then qq1.SQL.Add('  and ns.nspname='''+szukaj_schema+'''');
  if szukaj_name<>'' then qq1.SQL.Add('  and p.proname='''+szukaj_name+'''');
  if szukaj_args<>'' then qq1.SQL.Add('  and oidvectortypes(p.proargtypes)='''+szukaj_args+'''');
  qq1.SQL.Add('order by p.oid');

  try

    qq1.Open;
    while not qq1.EOF do
    begin
      s_name:=qq1.Fields[0].AsString;
      definicja:=StringReplace(StringReplace(qq1.Fields[1].AsString,#13#10,#10,[rfReplaceAll]),#10,#13#10,[rfReplaceAll])+';';
      nazwa.Add(s_name);
      if nazwa2<>nil then
      begin
        e_name:=FunctionNamePostgresToExtendedName(s_name,definicja);
        nazwa2.Add(e_name);
      end;
      cialo.Add(definicja);
//      cialo.Add('');
      qq1.Next;
    end;
    qq1.Close;

  finally
    qq1.Free;
  end;
end;

function NormalizeCialoProcedury(cialo:string):string;
var
  s: string;
  a,b,c: integer;
begin
  s:=StringReplace(cialo,'`','',[rfReplaceAll]);
  a:=pos('CREATE DEFINER=',s);
  if a>0 then
  begin
    b:=pos(' PROCEDURE ',s);
    delete(s,a+6,b-a-6);
  end;
  result:=s;
end;

procedure TDBSchemaSyncPostgres.ShowProcedures(nazwa, cialo: TStrings);
begin
  nazwa.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('show procedure status where db=database()');
  q1.Open;
  while not q1.EOF do
  begin
    nazwa.Add(q1.FieldByName('Name').AsString);
    q2.SQL.Clear;
    q2.SQL.Add('show create procedure '+q1.FieldByName('Name').AsString);
    q2.Open;
    cialo.Add(NormalizeCialoProcedury(q2.FieldByName('Create Procedure').AsString));
    q2.Close;
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowAggregatesPostgres(nazwa, cialo: TStrings);
var
  schema,naz,pelna: string;
  s,pom1,pom2: string;
begin
  nazwa.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select ns.nspname as schema, p.proname as nazwa, oidvectortypes(p.proargtypes) as argumenty');
  q1.SQL.Add('from pg_proc p');
  q1.SQL.Add('inner join pg_namespace ns on p.pronamespace=ns.oid');
  q1.SQL.Add('where p.proisagg and ns.nspname<>''pg_catalog'' and ns.nspname<>''information_schema''');
  q1.SQL.Add('order by p.oid');
  q2.SQL.Clear;
  q2.SQL.Add('SELECT');
  q2.SQL.Add('  tpn.nspname AS tfnnsp, tp.proname AS aggtransfn, ttn.nspname AS tnsname,');
  q2.SQL.Add('  tt.typname AS ttype, fp.proname AS aggfinalfn, fpn.nspname AS ffnnsp, a.agginitval');
  q2.SQL.Add('FROM pg_aggregate a');
  q2.SQL.Add('INNER JOIN pg_type tt ON a.aggtranstype = tt.oid');
  q2.SQL.Add('INNER JOIN pg_namespace ttn ON tt.typnamespace = ttn.oid');
  q2.SQL.Add('INNER JOIN pg_proc p ON a.aggfnoid = p.oid');
  q2.SQL.Add('INNER JOIN pg_namespace n ON n.oid = p.pronamespace');
  q2.SQL.Add('LEFT OUTER JOIN pg_proc tp ON a.aggtransfn = tp.oid');
  q2.SQL.Add('LEFT OUTER JOIN pg_namespace tpn ON tpn.oid = tp.pronamespace');
  q2.SQL.Add('LEFT OUTER JOIN pg_proc fp ON a.aggfinalfn = fp.oid');
  q2.SQL.Add('LEFT OUTER JOIN pg_namespace fpn ON fpn.oid = fp.pronamespace');
  q2.SQL.Add('LEFT OUTER JOIN pg_description ds ON p.oid = ds.objoid');
  q2.SQL.Add('LEFT OUTER JOIN pg_operator op ON a.aggsortop = op.oid');
  q2.SQL.Add('LEFT OUTER JOIN pg_namespace opn ON opn.oid = op.oprnamespace');
  q2.SQL.Add('WHERE p.proname = :nazwa AND n.nspname = :schema');
  q2.SQL.Add('ORDER BY n.nspname, p.proname, tt.typname');
  q2.Prepare;
  q1.Open;
  while not q1.EOF do
  begin
    schema:=q1.FieldByName('schema').AsString;
    naz:=q1.FieldByName('nazwa').AsString;
    pelna:=schema+'.'+naz+'('+q1.FieldByName('argumenty').AsString+')';
    q2.ParamByName('schema').AsString:=schema;
    q2.ParamByName('nazwa').AsString:=naz;
    q2.Open;
    nazwa.Add(pelna);
    pom1:=q2.FieldByName('tfnnsp').AsString+'.';
    pom2:=q2.FieldByName('tnsname').AsString+'.';
    if pom1='pg_catalog.' then pom1:='';
    if pom2='pg_catalog.' then pom2:='';
    if q2.FieldByName('aggfinalfn').IsNull then
      s:='CREATE AGGREGATE '+pelna+' (SFUNC = '+pom1+q2.FieldByName('aggtransfn').AsString+', STYPE = '+pom2+q2.FieldByName('ttype').AsString+', INITCOND = "'+q2.FieldByName('agginitval').AsString+'");'
    else
      s:='CREATE AGGREGATE '+pelna+' (SFUNC = '+pom1+q2.FieldByName('aggtransfn').AsString+', STYPE = '+pom2+q2.FieldByName('ttype').AsString+', FINALFUNC = '+q2.FieldByName('ffnnsp').AsString+'.'+q2.FieldByName('aggfinalfn').AsString+', INITCOND = "'+q2.FieldByName('agginitval').AsString+'");';
    cialo.Add(s);
    q2.Close;
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncPostgres.ShowCreateTable(table: string; schema: TStrings);
var
  s: string;
  a,b: integer;
begin
  q1.SQL.Clear;
  q1.SQL.Add('show create table '+table);
  try
    q1.Open;
    if q1.IsEmpty then s:='' else s:=q1.Fields[1].AsString;
  except
    s:='';
  end;
  q1.Close;
  if s='' then
  begin
    schema.Clear;
    exit;
  end;
  s:=StringReplace(s,'`','',[rfReplaceAll]);
  s:=StringReplace(s,'int(10)','int',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'int(11)','int',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'tinyint(4)','tinyint',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'tinyint(5)','tinyint',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'smallint(5)','smallint',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'smallint(6)','smallint',[rfReplaceAll,rfIgnoreCase]);
//  s:=StringReplace(s,'null default null','null',[rfReplaceAll,rfIgnoreCase]);
//  s:=StringReplace(s,'default null','null',[rfReplaceAll,rfIgnoreCase]);
  a:=pos('CREATE ALGORITHM=',s);
  if a>0 then
  begin
    b:=pos(' VIEW ',s);
    delete(s,a+6,b-a-6);
  end;
  StrToListItems(s,schema);
  if pos(') ENGINE=',schema[schema.Count-1])>0 then
  begin
    schema.Delete(schema.Count-1);
    schema.Add(')');
  end;
end;

procedure TDBSchemaSyncPostgres.ShowCreateTable(table: string; var schema: string);
var
  s: string;
  a,b: integer;
begin
  q1.SQL.Clear;
  q1.SQL.Add('show create table '+table);
  q1.Open;
  if q1.IsEmpty then s:='' else s:=q1.Fields[1].AsString;
  q1.Close;
  if s='' then
  begin
    schema:='';
    exit;
  end;
  s:=StringReplace(s,'`','',[rfReplaceAll]);
  s:=StringReplace(s,'int(10)','int',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'int(11)','int',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'tinyint(4)','tinyint',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'tinyint(5)','tinyint',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'smallint(5)','smallint',[rfReplaceAll,rfIgnoreCase]);
  s:=StringReplace(s,'smallint(6)','smallint',[rfReplaceAll,rfIgnoreCase]);
//  s:=StringReplace(s,'null default null','null',[rfReplaceAll,rfIgnoreCase]);
//  s:=StringReplace(s,'default null','null',[rfReplaceAll,rfIgnoreCase]);
  a:=pos('CREATE ALGORITHM=',s);
  if a>0 then
  begin
    b:=pos(' VIEW ',s);
    delete(s,a+6,b-a-6);
  end;
  a:=pos(') ENGINE=',s);
  if a>0 then delete(s,a+1,255);
  schema:=s;
end;

{ERROR - W tej procedurze select nie zwraa dla wszystkich schematów!}
procedure TDBSchemaSyncPostgres.ShowCreateTablePostgres(table: string; schema: TStrings
  );
var
  s,s1,s2,s3,pom: string;
  mm: integer;
begin
  schema.Clear;
  _postgres_str_2_schema_table(table,s1,s2);
  q1.SQL.Clear;
  q1.SQL.Add('SELECT');
  q1.SQL.Add('  distinct');
  q1.SQL.Add('  n.nspname as schema_name,');
  q1.SQL.Add('  c.relname as table_name,');
  q1.SQL.Add('  f.attname AS column_name,');
  q1.SQL.Add('  pg_catalog.format_type(f.atttypid,f.atttypmod) AS column_type,');
  q1.SQL.Add('  CASE f.atthasdef WHEN ''t'' THEN ''DEFAULT '' || substring(pg_catalog.pg_get_expr(d.adbin,d.adrelid) for 128) END AS column_default_value,');
  q1.SQL.Add('  CASE f.attnotnull WHEN true THEN ''NOT NULL'' ELSE ''NULL'' END AS column_not_null,');
  q1.SQL.Add('  f.attnum,');
  q1.SQL.Add('  f.attnum as number');
  q1.SQL.Add('  --CASE p.contype WHEN ''p'' THEN ''t'' ELSE ''f'' END AS primarykey,');
  q1.SQL.Add('  --CASE p.contype WHEN ''u'' THEN ''t'' ELSE ''f'' END AS uniquekey,');
  q1.SQL.Add('  --CASE p.contype WHEN ''f'' THEN g.relname END AS foreignkey,');
  q1.SQL.Add('  --CASE p.contype WHEN ''f'' THEN p.confkey END AS foreignkey_fieldnum,');
  q1.SQL.Add('  --CASE p.contype WHEN ''f'' THEN g.relname END AS foreignkey,');
  q1.SQL.Add('  --CASE p.contype WHEN ''f'' THEN p.conkey END AS foreignkey_connnum');
  q1.SQL.Add('FROM pg_attribute f');
  q1.SQL.Add('    JOIN pg_class c ON c.oid = f.attrelid');
  q1.SQL.Add('    JOIN pg_type t ON t.oid = f.atttypid');
  q1.SQL.Add('    LEFT JOIN pg_attrdef d ON d.adrelid = c.oid AND d.adnum = f.attnum');
  q1.SQL.Add('    LEFT JOIN pg_namespace n ON n.oid = c.relnamespace');
  q1.SQL.Add('    LEFT JOIN pg_constraint p ON p.conrelid = c.oid AND f.attnum = ANY (p.conkey)');
  q1.SQL.Add('    LEFT JOIN pg_class AS g ON p.confrelid = g.oid');
  q1.SQL.Add('WHERE c.relkind = ''r''::::char');
  q1.SQL.Add('    AND n.nspname = :schema  -- Replace with Schema name');
  q1.SQL.Add('    AND c.relname = :table  -- Replace with table name');
  q1.SQL.Add('    AND f.attnum > 0');
  q1.SQL.Add('ORDER BY number');
  q1.ParamByName('schema').AsString:=s1;
  q1.ParamByName('table').AsString:=s2;
  q1.Open;
  while not q1.EOF do
  begin
    mm:=q1.FieldByName('number').AsInteger;
    q1.Next;
  end;
  q1.First;
  while not q1.EOF do
  begin
    pom:=q1.FieldByName('schema_name').AsString;
    s3:=q1.FieldByName('column_default_value').AsString;
    s3:=StringReplace(s3,#13#10,#13,[rfReplaceAll]);
    s3:=StringReplace(s3,#13,'\n',[rfReplaceAll]);
    if pom='' then pom:=s1;
    if q1.FieldByName('attnum').AsInteger=1 then
      schema.Add('CREATE TABLE '+pom+'.'+q1.FieldByName('table_name').AsString+' (');
    s:='  "'+q1.FieldByName('column_name').AsString+'" '+q1.FieldByName('column_type').AsString+' '+s3+' '+q1.FieldByName('column_not_null').AsString;
    if q1.FieldByName('number').AsInteger=mm then
    begin
      schema.Add(s);
      schema.Add(');');
    end else begin
      s:=s+',';
      schema.Add(s);
    end;
    q1.Next;
  end;
  q1.Close;
  postgres_TableStructAppendKeys(s1,s2,schema);
end;

procedure TDBSchemaSyncPostgres.ShowCreateTablePostgres(table: string;
  var schema: string);
var
  pom: TStringList;
begin
  pom:=TStringList.Create;
  try
    ShowCreateTablePostgres(table,pom);
    schema:=pom.Text;
  finally
    pom.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.ShowCreateViewPostgres(table: string; schema: TStrings);
var
  s,s1,s2: string;
begin
  schema.Clear;
  s:='CREATE VIEW $NAME$ AS $BODY$';
  _postgres_str_2_schema_table(table,s1,s2);
  s:=StringReplace(s,'$NAME$',s1+'.'+s2,[]);
  q1.SQL.Clear;
  q1.SQL.Add('select definition from pg_views where schemaname=:schema and viewname=:table');
  q1.ParamByName('schema').AsString:=s1;
  q1.ParamByName('table').AsString:=s2;
  q1.Open;
  if q1.RecordCount=0 then s:='' else s:=StringReplace(s,'$BODY$',q1.FieldByName('definition').AsString,[]);
  q1.Close;
  //s:=StringReplace(s,':','::',[rfReplaceAll]);
  if s<>'' then schema.Add(s);
end;

procedure TDBSchemaSyncPostgres.ShowCreateViewPostgres(table: string; var schema: string
  );
var
  pom: TStringList;
begin
  pom:=TStringList.Create;
  try
    ShowCreateViewPostgres(table,pom);
    schema:=pom.Text;
  finally
    pom.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.ShowIndexesPostgres(table: string; nazwa,
  cialo: TStrings);
var
  s,s1,s2: string;
begin
  _postgres_str_2_schema_table(table,s1,s2);
  nazwa.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('SELECT c.relname, pg_get_indexdef(c.oid), a.attname, i.indisprimary, i.indisunique');
  q1.SQL.Add('FROM pg_index AS i, pg_class AS c, pg_attribute AS a');
  q1.SQL.Add('WHERE i.indexrelid = c.oid AND i.indexrelid = a.attrelid AND i.indisprimary=false AND i.indrelid = '''+s1+'.'+s2+'''::::regclass');
  q1.SQL.Add('ORDER BY c.oid, a.attnum');
  try
    q1.Open;
    while not q1.EOF do
    begin
      nazwa.Add(q1.Fields[0].AsString);
      s:=q1.Fields[1].AsString;
      s:=StringReplace(s,'ON '+s2+' USING','ON '+s1+'.'+s2+' USING',[])+';';
      cialo.Add(s);
      {if pos('tgeapi_transakcje_mytimestamp_kod_instrumentu',q1.Fields[0].AsString)>0 then
      begin
        writeln('INDEX: '+q1.Fields[0].AsString);
        writeln('CIALO: '+s);
      end;}
      q1.Next;
    end;
    q1.Close;
  except
  end;
end;

procedure TDBSchemaSyncPostgres.ShowIndexesPostgres(table: string; nazwa, nazwa_pelna,
  cialo: TStrings);
var
  s,s1,s2,ss: string;
begin
  _postgres_str_2_schema_table(table,s1,s2);
  nazwa.Clear;
  nazwa_pelna.Clear;
  cialo.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('SELECT c.relname, pg_get_indexdef(c.oid), a.attname, i.indisprimary, i.indisunique');
  q1.SQL.Add('FROM pg_index AS i, pg_class AS c, pg_attribute AS a');
  q1.SQL.Add('WHERE i.indexrelid = c.oid AND i.indexrelid = a.attrelid AND i.indisprimary=false AND i.indrelid = '''+s1+'.'+s2+'''::::regclass');
  q1.SQL.Add('ORDER BY c.oid, a.attnum');
  try
    q1.Open;
    while not q1.EOF do
    begin
      nazwa.Add(q1.Fields[0].AsString);
      s:=q1.Fields[1].AsString;
      s:=StringReplace(s,'ON '+s2+' USING','ON '+s1+'.'+s2+' USING',[])+';';
      nazwa_pelna.Add(PostgresIndexToFullName(q1.Fields[0].AsString,s));
      cialo.Add(s);
      {if (s1='tgeapi') and (s2='transakcje') then
      begin
        writeln('INDEX: '+q1.Fields[0].AsString);
        writeln('CIALO: '+s);
        writeln('FULLNAME: '+PostgresIndexToFullName(q1.Fields[0].AsString,s));
      end;}
      q1.Next;
    end;
    q1.Close;
  except
  end;
end;

procedure TDBSchemaSyncPostgres.ShowSequencesPostgres(nazwa, cialo: TStrings;
  obiekty: TStrings);
var
  s1,s2: string;
  q: TZQuery;
  schemat,sekwencja,obiekt: string;
begin
  nazwa.Clear;
  cialo.Clear;
  if obiekty<>nil then obiekty.Clear;
  q:=TZQuery.Create(self);
  try
    (* zapytanie o skojarzony obiekt z sekwencją *)
    if obiekty<>nil then
    begin
      q.Connection:=q1.Connection;
      q.SQL.Add('select s.relname as seq, n.nspname as sch, t.relname as tab, a.attname as col');
      q.SQL.Add('from pg_class s');
      q.SQL.Add('join pg_depend d on d.objid=s.oid and d.classid=''pg_class''::::regclass and d.refclassid=''pg_class''::::regclass');
      q.SQL.Add('join pg_class t on t.oid=d.refobjid');
      q.SQL.Add('join pg_namespace n on n.oid=t.relnamespace');
      q.SQL.Add('join pg_attribute a on a.attrelid=t.oid and a.attnum=d.refobjsubid');
      q.SQL.Add('where s.relkind=''S'' and d.deptype=''a''');
      q.SQL.Add('  and n.nspname=:schemat and s.relname=:sekwencja');
    end;
    (* zapytanie o sekwencje *)
    q1.SQL.Clear;
    q1.SQL.Add('select * from information_schema.sequences');
    q1.Open;
    while not q1.EOF do
    begin
      schemat:=q1.FieldByName('sequence_schema').AsString;
      sekwencja:=q1.FieldByName('sequence_name').AsString;
      if obiekty<>nil then
      begin
        q.ParamByName('schemat').AsString:=schemat;
        q.ParamByName('sekwencja').AsString:=sekwencja;
        q.Open;
        obiekt:=schemat+'.'+q.FieldByName('tab').AsString+'.'+q.FieldByName('col').AsString;
        q.Close;
      end;
      s1:=schemat+'.'+sekwencja;
      s2:='CREATE SEQUENCE '+s1+' INCREMENT '+q1.FieldByName('increment').AsString+' MINVALUE '+q1.FieldByName('minimum_value').AsString+' MAXVALUE '+q1.FieldByName('maximum_value').AsString+' START '+q1.FieldByName('start_value').AsString+' CACHE 1;';
      nazwa.Add(s1);
      cialo.Add(s2);
      if obiekty<>nil then obiekty.Add(obiekt);
      q1.Next;
    end;
    q1.Close;
  finally
    if obiekty<>nil then q.Free;
  end;
end;

procedure TDBSchemaSyncPostgres.SaveSchema;
var
  i,j,w: integer;
  pom,vv,v1,v2,v3: TStringList;
  czas: TDateTime;
  local_name: string;
begin
  if Assigned(FBeforeSave) then FBeforeSave(self);
  if Assigned(FStatusEvent) then FStatusEvent(self,-1,'r','Save Schema Prepare','','',false,0,0);
  czas:=now;
  pom:=TStringList.Create;
  vv:=TStringList.Create;
  v1:=TStringList.Create;
  v2:=TStringList.Create;
  v3:=TStringList.Create;
  try
    if FileExists(dbsql) then DeleteFile(dbsql);
    sdb.Protocol:='sqlite-3';
    sdb.Database:=dbsql;
    sdb.Connect;
    (* tabele i widoki *)
    sq1.SQL.Clear;
    sq1.SQL.Add('create table tabele (id integer primary key autoincrement,nazwa text,nazwa2 text,typ integer,znacznik_czasu datetime,definicja blob,jezyk text,obiekt text)');
    sq1.ExecSQL;
    sq1.SQL.Clear;
    sq1.SQL.Add('insert into tabele (nazwa,nazwa2,typ,znacznik_czasu,definicja,jezyk,obiekt) values (:nazwa,:nazwa2,:typ,:z_czasu,:definicja,:jezyk,:obiekt)');
    sq1.Prepare;
    StartTransaction;
    (* ODCZYT LANGUAGES AND SCHEMAS AND TYPES AND AGGREGATES ONLY POSTGRES! *)
    if proto=2 then
    begin
      ShowLanguagesPostgres(v1,v2);
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,0,'r','Save Schema',v1[i],'Language',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=181;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=v2[i];;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
      ShowSchemasPostgres(v1,v2);
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,0,'r','Save Schema',v1[i],'Schema',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=0;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=v2[i];;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
      ShowTypesPostgres(v1,v2);
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,20,'r','Save Schema',v1[i],'Type',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=20;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=v2[i];;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
      ShowAggregatesPostgres(v1,v2);
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,21,'r','Save Schema',v1[i],'Aggregate',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=21;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=v2[i];;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
    end;
    (* MYSQL *)
    if proto=1 then
    begin
      ShowTables(vv);
      for i:=0 to vv.Count-1 do
      begin
        ShowCreateTable(vv[i],pom);
        if pos('CREATE VIEW ',pom.Text)=1 then
        begin
          v1.Add(vv[i]);
          v2.Add(pom.Text);
          continue;
        end;
        sq1.ParamByName('nazwa').AsString:=vv[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=1;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=pom.Text;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
      for i:=0 to v1.Count-1 do
      begin
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=2;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=v2[i];
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
    end;
    (* POSTGRESQL *)
    if proto=2 then
    begin
      (* sekwencje *)
      ShowSequencesPostgres(v1,v2,vv);
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,12,'r','Save Schema',v1[i],'Sequenction',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=12;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=v2[i];
        sq1.ParamByName('jezyk').Clear;
        if vv[i]='' then sq1.ParamByName('obiekt').Clear else sq1.ParamByName('obiekt').AsString:=vv[i];
        sq1.ExecSQL;
      end;
      (* tables *)
      ShowTablesPostgres(vv);
      for i:=0 to vv.Count-1 do
      begin
        (* table - metadata show create table *)
        if Assigned(FStatusEvent) then FStatusEvent(self,1,'r','Save Schema',vv[i],'Table',false,0,0);
        ShowCreateTablePostgres(vv[i],pom);
        sq1.ParamByName('nazwa').AsString:=vv[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=1;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=pom.Text;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
        (* table - indeksy *)
        ShowIndexesPostgres(vv[i],v1,v3,v2);
        for j:=0 to v1.Count-1 do
        begin
          sq1.ParamByName('nazwa').AsString:=v1[j];
          sq1.ParamByName('nazwa2').AsString:=v3[j];
          sq1.ParamByName('typ').AsInteger:=11;
          sq1.ParamByName('z_czasu').Clear;
          sq1.ParamByName('definicja').AsString:=v2[j];
          sq1.ParamByName('jezyk').Clear;
          sq1.ParamByName('obiekt').Clear;
          sq1.ExecSQL;
        end;
      end;
      (* views *)
      ShowViewsPostgres(vv);
      for i:=0 to vv.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,2,'r','Save Schema',vv[i],'View',false,0,0);
        ShowCreateViewPostgres(vv[i],pom);
        sq1.ParamByName('nazwa').AsString:=vv[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=2;
        sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=pom.Text;
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
    end;
    (* wyzwalacze *)
    v1.Clear;
    v2.Clear;
    case proto of
      1: ShowTriggers(v1,v2);
      2: ShowTriggersPostgres(v1,v2);
    end;
    for i:=0 to v1.Count-1 do
    begin
      if Assigned(FStatusEvent) then FStatusEvent(self,3,'r','Save Schema',v1[i],'Trigger',false,0,0);
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.ParamByName('nazwa2').Clear;
      sq1.ParamByName('typ').AsInteger:=3;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=v2[i];
      sq1.ParamByName('jezyk').Clear;
      sq1.ParamByName('obiekt').Clear;
      sq1.ExecSQL;
    end;
    (* funkcje *)
    v1.Clear;
    v2.Clear;
    case proto of
      1: ShowFunctions(v1,v2);
      2: ShowFunctionsPostgres(v1,v3,v2);
    end;
    for i:=0 to v1.Count-1 do
    begin
      if Assigned(FStatusEvent) then FStatusEvent(self,4,'r','Save Schema',v1[i],'Function',false,0,0);
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.ParamByName('nazwa2').AsString:=v3[i];
      sq1.ParamByName('typ').AsInteger:=4;
      if odczytaj_znacznik_czasu(v2[i],czas) then sq1.ParamByName('z_czasu').AsDateTime:=czas else sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=v2[i];
      sq1.ParamByName('jezyk').AsString:=FuncPostgresToLanguage(v2[i]);
      sq1.ParamByName('obiekt').Clear;
      sq1.ExecSQL;
    end;
    (* procedury - ONLY MYSQL! *)
    if proto=1 then
    begin
      v1.Clear;
      v2.Clear;
      ShowProcedures(v1,v2);
      for i:=0 to v1.Count-1 do
      begin
        if Assigned(FStatusEvent) then FStatusEvent(self,5,'r','Save Schema',v1[i],'Procedure',false,0,0);
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.ParamByName('nazwa2').Clear;
        sq1.ParamByName('typ').AsInteger:=5;
        if odczytaj_znacznik_czasu(v2[i],czas) then sq1.ParamByName('z_czasu').AsDateTime:=czas else sq1.ParamByName('z_czasu').Clear;
        sq1.ParamByName('definicja').AsString:=v2[i];
        sq1.ParamByName('jezyk').Clear;
        sq1.ParamByName('obiekt').Clear;
        sq1.ExecSQL;
      end;
    end;
    (* dane wybranych tabel *)
    for i:=0 to FDataSync.Count-1 do export_data_table(FDataSync[i]);
    Commit;
    sdb.Disconnect;
  finally
    pom.Free;
    vv.Free;
    v1.Free;
    v2.Free;
    v3.Free;
  end;
  if Assigned(FAfterSave) then FAfterSave(self);
end;

//Funkcja syncronizuje zmiany nanosząc je bezpośrednio na bazie danych
function TDBSchemaSyncPostgres.SyncSchema(SyncLogFilename: string; SyncLogTest: boolean
  ): boolean;
var
  b: boolean;
  ee: integer;
begin
  if not FileExists(dbsql) then
  begin
    err:=1;
    error:='Brak struktury wzorcowej, synchronizacja anulowana.';
    result:=false;
    exit;
  end;
  if SyncLogFilename<>'' then script_filename:=SyncLogFilename;
  if Assigned(FBeforeSync) then FBeforeSync(self);
  if Assigned(FStatusEvent) then FStatusEvent(self,-1,'I','Synchro Schema Prepare','','',false,0,0);
  przywileje1.Clear;
  przywileje2.Clear;
  ogens:=false;
  sdb.Protocol:='sqlite-3';
  sdb.Database:=dbsql;
  sdb.Connect;
  ustaw_plik_skryptu(SyncLogFilename);
  try
    if (proto=2) and FGrantRemember then czytaj_przywileje_postgres(przywileje1,przywileje2);
    ee:=101;
    b:=sync_delete;
    ee:=102;
    lastindexes.Clear;
    if b then b:=sync_table;
    ee:=103;
    if b then b:=sync_event;
    if (proto=2) and (lastindexes.Count>0) then last_indeksy_create_postgres;
    if (proto=2) and FGrantRemember then zapisz_przywileje_postgres(przywileje1,przywileje2);
    import_data_table;
  except
    err:=ee;
    b:=false;
  end;
  sdb.Disconnect;
  if sdb2.Connected then begin commit2; sdb2.Disconnect; end;
  if b then
  begin
    if FSyncLog then
    begin
      try if FileExists(script_filename) then DeleteFile(script_filename); except end;
      if gens.Count>0 then ZapiszSkryptDoPliku(script_filename,SyncLogTest);
    end;
    err:=0;
    error:='';
  end;
  if Assigned(FAfterSync) then FAfterSync(self);
  result:=b;
end;

//funkcja generuje skrypt synchronizujący bez nanoszenia zmian
//skrypt można sprawdzić i wykonać go ręcznie
function TDBSchemaSyncPostgres.GenerateScript(Filename: string): boolean;
var
  b: boolean;
  ee: integer;
begin
  if not FileExists(dbsql) then
  begin
    err:=1;
    error:='Brak struktury wzorcowej, synchronizacja anulowana.';
    result:=false;
    exit;
  end;
  ogens:=true;
  gens.Clear;
  ogens_opis:='';
  script_filename:=Filename;
  sdb.Protocol:='sqlite-3';
  sdb.Database:=dbsql;
  sdb.Connect;
  ustaw_plik_skryptu(Filename);
  try
    if (proto=2) and FGrantRemember then czytaj_przywileje_postgres(przywileje1,przywileje2);
    ee:=101;
    b:=sync_delete;
    ee:=102;
    if b then b:=sync_table;
    ee:=103;
    if b then b:=sync_event;
    if (proto=2) and FGrantRemember then zapisz_przywileje_postgres(przywileje1,przywileje2);
    import_data_table;
  except
    err:=ee;
    b:=false;
  end;
  sdb.Disconnect;
  if sdb2.Connected then begin commit2; sdb2.Disconnect; end;
  if b then
  begin
    gens.SaveToFile(script_filename);
    err:=0;
    error:='';
  end;
  result:=b;
end;

end.
