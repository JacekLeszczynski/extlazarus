unit DBSchemaSync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, DB, ZConnection, ZDataset, ZSqlProcessor;

type

  { TDBSchemaSync }

  TDBSchemaSync = class(TComponent)
  private
    { Private declarations }
    dbsql: string;
    FDB,sdb: TZConnection;
    FDictTables: TStrings;
    FZnacznikCzasu: boolean;
    q1,q2,qq: TZQuery;
    sq1,sq2: TZQuery;
    list: TStringList;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    //procedure SetAutoincrement;
    function odczytaj_znacznik_czasu(s: string; var czas: TDateTime): boolean;
    function sync_delete:boolean;
    function sync_delete_sqlite:boolean;
    function sync_delete_sqlite_2:boolean;
    function sync_table:boolean;
    function test_ref(nazwa:string;cialo:TStrings):boolean;
    function test_table(indeks:integer;nazwa:string;cialo:TStrings):boolean;
    function akt_table(nazwa:string;s1,s2:TStrings):boolean;
    function sync_event:boolean;
    function sync_slow:boolean;
    (* do sqlite *)
    function PragmaForeignKeys: boolean;
    procedure PragmaForeignKeys(aOn: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    err: integer;
    error: string;
    log: TStringList;
    sqlite: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure init;
    procedure ShowTables(tables: TStrings);
    procedure ShowTablesSqlite(aBody: TStrings);
    procedure ShowViewsSqlite(aBody: TStrings);
    procedure ShowIndexesSqlite(aBody: TStrings);
    procedure ShowTriggers(nazwa,cialo: TStrings);
    procedure ShowFunctions(nazwa,cialo: TStrings);
    procedure ShowProcedures(nazwa,cialo: TStrings);
    procedure ShowCreateTable(table: string; schema: TStrings);
    procedure ShowCreateTable(table: string; var schema: string);
    procedure ShowCreateViewSqlite(aView: string; var aWektor: string; aBody: TStrings);
    procedure ShowCreateViewSqlite(aView: string; aBody: TStrings);
    procedure SaveSchema;
    function SyncSchema:boolean;
  published
    { Published declarations }
    property DB_Connection: TZConnection read FDB write FDB;
    property StructFileName: string read dbsql write dbsql;
    property ZnacznikCzasu: boolean read FZnacznikCzasu write FZnacznikCzasu default false;
    //Podajemy nazwy tabel słownikowych w formacie:
    //tabela[:indeks]
    property DictionaryTables: TStrings read FDictTables write FDictTables;
  end;

procedure Register;

implementation

uses
  ecode_unit, ZDbcIntfs, ZScriptParser, ZCompatibility;

procedure Register;
begin
  {$I dbschemasync_icon.lrs}
  RegisterComponents('System',[TDBSchemaSync]);
end;

{ TDBSchemaSync }

procedure TDBSchemaSync.StartTransaction;
begin
  sdb.TransactIsolationLevel:=tiReadCommitted;
  sdb.StartTransaction;
end;

procedure TDBSchemaSync.Commit;
begin
  sdb.Commit;
  sdb.TransactIsolationLevel:=tiNone;
end;

procedure TDBSchemaSync.Rollback;
begin
  sdb.Rollback;
  sdb.TransactIsolationLevel:=tiNone;
end;

{procedure TDBSchemaSync.SetAutoincrement;
var
  q: TZQuery;
begin
  q:=TZQuery.Create(self);
  q.Connection:=qdata.Connection;
  q.SQL.Add('ALTER TABLE '+io_tablename+' AUTO_INCREMENT=:ai');
  q.ParamByName('ai').AsLargeInt:=SpinEdit1.Value;
  try
    try
      q.ExecSQL;
    except
      mess.ShowInformation('Ustawienie odrzucone z powodu błędu.');
      GetAutoIncrement;
    end;
  finally
    q.Free;
  end;
end;}

function TDBSchemaSync.odczytaj_znacznik_czasu(s: string; var czas: TDateTime
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

function TDBSchemaSync.sync_delete: boolean;
var
  i: integer;
  v1,v2: TStringList;
  s: string;
begin
  (* usuwamy ze struktury objekty nie istniejące w bazie wzorcowej *)
  v1:=TStringList.Create;
  v2:=TStringList.Create;
  try
    (* wyzwalacze *)
    ShowTriggers(v1,v2);
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=3');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsInteger=0 then
      begin
        qq.SQL.Clear;
        qq.SQL.Add('drop trigger '+v1[i]);
        try
          qq.ExecSQL;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania wyzwalacza: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* widoki *)
    ShowTables(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=2');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      ShowCreateTable(v1[i],s);
      if pos('CREATE VIEW',s)=1 then
      begin
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          qq.SQL.Clear;
          qq.SQL.Add('drop view '+v1[i]);
          try
            qq.ExecSQL;
          except
            log.Add('SYNC-DELETE: Błąd podczas usuwania podglądu: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
            result:=false;
            exit;
          end;
        end;
        sq1.Close;
      end;
    end;
    (* procedury *)
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
          qq.ExecSQL;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania procedury: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* funkcje *)
    ShowFunctions(v1,v2);
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=4');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsInteger=0 then
      begin
        qq.SQL.Clear;
        qq.SQL.Add('drop function '+v1[i]);
        try
          qq.ExecSQL;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania funkcji: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* tabele *)
    ShowTables(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=1');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      ShowCreateTable(v1[i],v2);
      if pos('CREATE TABLE',v2.Text)=1 then
      begin
        sq1.ParamByName('nazwa').AsString:=v1[i];
        sq1.Open;
        if sq1.Fields[0].AsInteger=0 then
        begin
          qq.SQL.Clear;
          qq.SQL.Add('drop table '+v1[i]);
          try
            qq.ExecSQL;
          except
            log.Add('SYNC-DELETE: Błąd podczas usuwania tabeli: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
            result:=false;
            exit;
          end;
        end;
        sq1.Close;
      end;
    end;
  finally
    v1.Clear;
    v2.Clear;
  end;
  result:=true;
end;

function TDBSchemaSync.sync_delete_sqlite: boolean;
var
  i: integer;
  v1: TStringList;
  s: string;
begin
  (* usuwamy ze struktury objekty nie istniejące w bazie wzorcowej *)
  v1:=TStringList.Create;
  try
    (* widoki *)
    ShowViewsSqlite(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=3');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsInteger=0 then
      begin
        qq.SQL.Clear;
        qq.SQL.Add('drop view '+v1[i]);
        try
          qq.ExecSQL;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania podglądu: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* indeksy *)
    ShowIndexesSqlite(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=2');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsInteger=0 then
      begin
        qq.SQL.Clear;
        qq.SQL.Add('drop index '+v1[i]);
        try
          qq.ExecSQL;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania indeksu: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* tabele *)
    ShowTables(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select count(id) from tabele where nazwa=:nazwa and typ=1');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsInteger=0 then
      begin
        qq.SQL.Clear;
        qq.SQL.Add('drop table '+v1[i]);
        try
          qq.ExecSQL;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania tabeli: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* WSZYSTKO *)
  finally
    v1.Free;
  end;
  result:=true;
end;

function TDBSchemaSync.sync_delete_sqlite_2: boolean;
var
  i: integer;
  v1,v2: TStringList;
  s: string;
begin
  (* usuwamy ze struktury objekty nie istniejące w bazie wzorcowej *)
  v1:=TStringList.Create;
  v2:=TStringList.Create;
  try
    (* widoki *)
    ShowViewsSqlite(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select definicja from tabele where nazwa=:nazwa and typ=3');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      ShowCreateViewSqlite(v1[i],v2);
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsString<>v2.Text then
      begin
        qq.SQL.Clear;
        qq.SQL.Add('drop view '+v1[i]);
        try
          qq.ExecSQL;
        except
          log.Add('SYNC-DELETE: Błąd podczas usuwania podglądu: ['+v1[i]+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Close;
    end;
    (* WSZYSTKO *)
  finally
    v1.Free;
    v2.Free;
  end;
  result:=true;
end;

function TDBSchemaSync.sync_table: boolean;
var
  i: integer;
  v1,v2,defs: TStringList;
  nazwa,def: string;
  b: boolean;
begin
  (* aktualizacja tabel *)
  b:=true;
  list.Clear;
  v1:=TStringList.Create;
  v2:=TStringList.Create;
  defs:=TStringList.Create;
  try
    (* tabele *)
    ShowTables(v1);
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
      for i:=list.Count-1 downto 0 do
      begin
        nazwa:=list[i];
        sq1.ParamByName('nazwa').AsString:=nazwa;
        sq1.Open;
        def:=sq1.FieldByName('definicja').AsString;
        sq1.Close;
        StrToListItems(def,defs);
        (* sprawdzamy referencje i wykonujemy aktualizację jeśli trzeba *)
        if test_ref(nazwa,defs) then b:=test_table(i,nazwa,defs);
        if not b then break;
      end;
      if not b then break;
    end;
  finally
    v1.Free;
    v2.Free;
    defs.Free;
  end;
  result:=b;
end;

function TDBSchemaSync.test_ref(nazwa: string; cialo: TStrings): boolean;
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

function TDBSchemaSync.test_table(indeks: integer; nazwa: string;
  cialo: TStrings): boolean;
var
  ss: TStringList;
  b: boolean;
begin
  b:=true;
  ss:=TStringList.Create;
  try
    ShowCreateTable(nazwa,ss);
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

procedure pola_to_indeksy(cialo,cel:TStrings;typ:integer;pp:boolean=false);
var
  i: integer;
  s: string;
  tt: integer;
begin
  (* legenda: 0-pole, 1-klucz pierwszy, 2-indeks, 3-klucz obcy *)
  cel.Clear;
  for i:=1 to cialo.Count-2 do
  begin
    s:=trimleft(cialo[i]);
    if s[length(s)]=',' then delete(s,length(s),1);
    if pos('PRIMARY KEY',s)=1 then tt:=1 else
    if (pos('KEY',s)=1) or (pos('UNIQUE KEY',s)=1) then tt:=2 else
    if (pos('CONSTRAINT',s)=1) and (pos('FOREIGN KEY',s)>0) then tt:=3 else tt:=0;
    if tt=typ then case typ of
      0: if pp then cel.Add(s) else cel.Add(nazwa_pola(s));
      1: cel.Add(s);
      2: cel.Add(s);
      3: cel.Add(s);
    end;
  end;
end;

function TDBSchemaSync.akt_table(nazwa: string; s1, s2: TStrings): boolean;
var
  p1,p2,pp: TStringList;
  i,a: integer;
  s: string;
begin
  result:=true;
  if s2.Count=0 then
  begin
    (* tej tabeli nie ma - tworzę ją *)
    q1.SQL.Assign(s1);
    try
      q1.ExecSQL;
      result:=true;
    except
      log.Add('SYNC-CREATE: Błąd podczas tworzenia tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
      result:=false;
    end;
    exit;
  end;
  result:=false;
  pp:=TStringList.Create;
  p1:=TStringList.Create;
  p2:=TStringList.Create;
  try
    (* --- USUWAM WSZYSTKIE OBJEKTY KTÓRYCH JUŻ NIE MA --- *)
    (* usuwam gotowe do usunięcia klucze obce *)
    pola_to_indeksy(s1,p1,3);
    pola_to_indeksy(s2,p2,3);
    for i:=0 to p2.Count-1 do if StringToItemIndex(p1,p2[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' DROP FOREIGN KEY '+nazwa_klucza_obcego(p2[i]));
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-ALTER: Błąd podczas usuwania klucza obcego w tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* usuwam gotowe do usunięcia indeksy *)
    pola_to_indeksy(s1,p1,2);
    pola_to_indeksy(s2,p2,2);
    for i:=0 to p2.Count-1 do if StringToItemIndex(p1,p2[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' DROP INDEX '+nazwa_indeksu(p2[i]));
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-ALTER: Błąd podczas usuwania indeksu w tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* usuwam gotowe do usunięcia indeks pierwszy *)
    pola_to_indeksy(s1,p1,1);
    pola_to_indeksy(s2,p2,1);
    if p2.Count=1 then if (p1.Count=0) or ((p1.Count=1) and (p1[0]<>p2[0])) then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' DROP PRIMARY KEY');
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-ALTER: Błąd podczas usuwania klucza nadrzędnego tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* usuwam gotowe do usunięcia pola tabeli *)
    pola_to_indeksy(s1,p1,0);
    pola_to_indeksy(s2,p2,0);
    for i:=0 to p2.Count-1 do if StringToItemIndex(p1,p2[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' DROP COLUMN '+p2[i]);
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-ALTER: Błąd podczas usuwania kolumny z tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* odtwarzam nową wersję struktury i sprawdzam czy struktury są już takie same *)
    ShowCreateTable(nazwa,s2);
    if s1=s2 then
    begin
      result:=true;
      exit;
    end;
    (* --- DODAJĘ WSZYSTKIE OBJEKTY KTÓRYCH BRAKUJE --- *)
    (* dodaję nowe pola tabeli *)
    pola_to_indeksy(s1,pp,0,true);
    pola_to_indeksy(s1,p1,0);
    pola_to_indeksy(s2,p2,0);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      if i=0 then s:='ALTER TABLE '+nazwa+' ADD COLUMN '+pp[i]+' FIRST'
             else s:='ALTER TABLE '+nazwa+' ADD COLUMN '+pp[i]+' AFTER '+p1[i-1];
      if pos('AUTO_INCREMENT',s)>0 then s:=StringReplace(s,'AUTO_INCREMENT','AUTO_INCREMENT PRIMARY KEY',[rfIgnoreCase]);
      try
        q1.SQL.Add(s);
        q1.ExecSQL;
      except
        log.Add('SYNC-ADDING: Błąd podczas dodawania kolumny do tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* aktualizuję pola tabeli, które się różnią *)
    pola_to_indeksy(s1,pp,0);
    pola_to_indeksy(s1,p1,0,true);
    pola_to_indeksy(s2,p2,0,true);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      if i=0 then q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' FIRST')
             else q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' AFTER '+pp[i-1]);
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-CHANGE: Błąd podczas edycji kolumny w tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* aktualizuję kolejność pól tabeli, jeśli istnieje inna kolejność *)
    pola_to_indeksy(s1,pp,0);
    pola_to_indeksy(s1,p1,0,true);
    pola_to_indeksy(s2,p2,0,true);
    for i:=0 to p1.Count-1 do if p1[i]<>p2[i] then
    begin
      q1.SQL.Clear;
      if i=0 then q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' FIRST')
             else q1.SQL.Add('ALTER TABLE '+nazwa+' CHANGE COLUMN '+pp[i]+' '+p1[i]+' AFTER '+pp[i-1]);
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-SORT: Błąd podczas przesuwania kolumny w tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
      ShowCreateTable(nazwa,s2);
      pola_to_indeksy(s2,p2,0,true);
    end;
    (* dodaję brakujący indeks pierwszy *)
    pola_to_indeksy(s1,p1,1);
    pola_to_indeksy(s2,p2,1);
    if (p1.Count=1) and (p2.Count=0) then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' ADD '+p1[0]);
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-ADD: Błąd podczas dodawania indeksu podstawowego w tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* dodaję brakujące indeksy *)
    pola_to_indeksy(s1,p1,2);
    pola_to_indeksy(s2,p2,2);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' ADD '+p1[i]);
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-ADD: Błąd podczas dodawania indeksu do tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
    (* dodaję brakujące klucze obce *)
    pola_to_indeksy(s1,p1,3);
    pola_to_indeksy(s2,p2,3);
    for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' ADD '+p1[i]);
      try
        q1.ExecSQL;
      except
        log.Add('SYNC-ADD: Błąd podczas dodawania indeksu obcego do tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        exit;
      end;
    end;
  finally
    pp.Free;
    p1.Free;
    p2.Free;
  end;
  result:=true;
end;

function TDBSchemaSync.sync_event: boolean;
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
    (* funkcje *)
    ShowFunctions(p1,p2);
    sq1.SQL.Clear;
    sq1.SQL.Add('select nazwa,definicja from tabele where typ=4 order by id');
    sq1.Open;
    while not sq1.EOF do
    begin
      nazwa:=sq1.FieldByName('nazwa').AsString;
      def:=sq1.FieldByName('definicja').AsString;
      a:=StringToItemIndex(p1,nazwa,-1);
      if a<>-1 then if p2[a]<>def then
      begin
        q1.SQL.Clear;
        q1.SQL.Add('drop function '+nazwa);
        q1.ExecSQL;
        a:=-1;
      end;
      if a=-1 then
      begin
        q.Script.Clear;
        q.Script.Add(def+';;');
        q.Execute;
      end;
      sq1.Next;
    end;
    sq1.Close;
    (* procedury *)
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
        q1.ExecSQL;
        a:=-1;
      end;
      if a=-1 then
      begin
        q.Script.Clear;
        q.Script.Add(def+';;');
        q.Execute;
      end;
      sq1.Next;
    end;
    sq1.Close;
    (* widoki *)
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
          q1.SQL.Add('drop view '+nazwa);
          q1.ExecSQL;
          a:=-1;
        end;
      end;
      if a=-1 then
      begin
        q.Script.Clear;
        q.Script.Add(def+';;');
        q.Execute;
      end;
      sq1.Next;
    end;
    sq1.Close;
    (* wyzwalacze *)
    ShowTriggers(p1,p2);
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
        q1.SQL.Add('drop trigger '+nazwa);
        q1.ExecSQL;
        a:=-1;
      end;
      if a=-1 then
      begin
        q.Script.Clear;
        q.Script.Add(def+';;');
        q.Execute;
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

function _StrToDate(s: string): TDate;
var
  fs: TFormatSettings;
begin
  fs.ShortDateFormat:='y/m/d';
  fs.DateSeparator:='-';
  result:=StrToDate(s,fs);
end;

function _StrToTime(str: string): TDate;
var
  h,m,s,ms: word;
begin
  str:=trim(str);
  h:=StrToInt(copy(str,1,2));
  m:=StrToInt(copy(str,4,2));
  s:=StrToInt(copy(str,7,2));
  ms:=0;
  result:=EncodeTime(h,m,s,ms);
end;

function _StrToDateTime(str: string): TDate;
var
  yy,mm,dd: word;
  h,m,s,ms: word;
begin
  str:=trim(str);
  yy:=StrToInt(copy(str,1,4));
  mm:=StrToInt(copy(str,6,2));
  dd:=StrToInt(copy(str,9,2));
  h:=StrToInt(copy(str,12,2));
  m:=StrToInt(copy(str,15,2));
  s:=StrToInt(copy(str,18,2));
  ms:=0;
  result:=EncodeDate(yy,mm,dd)+EncodeTime(h,m,s,ms);
end;

function TDBSchemaSync.sync_slow: boolean;
var
  nazwa,indeks,definicja,wartosc: string;
  indeksy: TStringList;
  i,max,j: integer;
  s,w: string;
  b: boolean;
begin
  indeksy:=TStringList.Create;
  try
    (* dodanie/aktualizacja istniejących rekordów *)
    sq1.SQL.Clear;
    sq1.SQL.Add('select nazwa,indeks,definicja from slowniki');
    sq1.SQL.Add('order by id');
    sq1.Open;
    while not sq1.EOF do
    begin
      indeksy.Clear;
      nazwa:=sq1.FieldByName('nazwa').AsString;
      indeks:=sq1.FieldByName('indeks').AsString;
      definicja:=trim(sq1.FieldByName('definicja').AsString);
      (* BEGIN *)
      q1.SQL.Clear;
      q1.SQL.Add('select count(*) from '+nazwa);
      q1.Open;
      b:=q1.Fields[0].AsInteger=0;
      q1.Close;
      if b then
      begin
        q1.SQL.Clear;
        q1.SQL.Add('select * from '+nazwa);
        //if indeks<>'' then q1.SQL.Add('order by '+indeks);
        q1.Open;
        max:=GetLineCount(definicja,';');

        q2.SQL.Clear;
        q2.SQL.Add('insert into '+nazwa);
        q2.SQL.Add('(');
        for i:=0 to q1.Fields.Count-1 do
        begin
          if i>0 then q2.SQL.Add(',');
          q2.SQL.Add(q1.Fields[i].FieldName);
        end;
        q2.SQL.Add(')');
        q2.SQL.Add('values');
        q2.SQL.Add('(');
        for i:=0 to q1.Fields.Count-1 do
        begin
          if i>0 then q2.SQL.Add(',');
          q2.SQL.Add(':'+q1.Fields[i].FieldName);
        end;
        q2.SQL.Add(')');
        q2.Prepare;
        q1.Close;

        for i:=1 to max do
        begin
          s:=GetLineToStr(definicja,i,';');
          if s='' then continue;

          for j:=0 to q2.Params.Count-1 do
          begin
            w:=GetLineToStr(s,j+1,',');
            if w='' then continue;
            w:=StringReplace(w,'{$1}',',',[rfReplaceAll]);
            w:=StringReplace(w,'{$2}',';',[rfReplaceAll]);
            //if q1.Params[j].Name=indeks then wartosc:=w;
            try
              if q2.Params[j].DataType=ftDate then q2.Params[j].AsDate:=_StrToDate(w) else
              if q2.Params[j].DataType=ftTime then q2.Params[j].AsTime:=_StrToTime(w) else
              if q2.Params[j].DataType=ftDateTime then q2.Params[j].AsDateTime:=_StrToDateTime(w) else
              if q2.Params[j].DataType=ftTimeStamp then q2.Params[j].AsDateTime:=_StrToDateTime(w) else
              q2.Params[j].AsString:=w;
            except
              on E: Exception do log.Add('j = '+IntToStr(j)+' w = '+w+' Error = '+E.Message);
            end;
          end;
          try
            q2.ExecSQL;
          except
            on E: Exception do log.Add('ExecSQL Error = '+E.Message);
          end;

          {if q1.FieldByName(indeks).AsString<>wartosc then
          begin
            q1.Edit;
            q1.FieldByName(indeks).AsString:=wartosc;
            q1.Post;
            q2.SQL.Clear;
            q2.SQL.Add('ALTER TABLE '+nazwa+' AUTO_INCREMENT=:ai');
            q2.ParamByName('ai').AsLargeInt:=StrToInt(wartosc)+1;
            q2.ExecSQL;
          end;}
        end;
        //q1.Close;
      end;
      (* END *)
      sq1.Next;
    end;
    sq1.Close;
    (* usunięcie rekordów usuniętych *)
  finally
    indeksy.Free;
  end;
  result:=true;
end;

function TDBSchemaSync.PragmaForeignKeys: boolean;
var
  a: integer;
begin
  q1.SQL.Clear;
  q1.SQL.Add('PRAGMA foreign_keys');
  q1.Open;
  a:=q1.Fields[0].AsInteger;
  q1.Close;
  result:=a=1;
end;

procedure TDBSchemaSync.PragmaForeignKeys(aOn: boolean);
begin
  q1.SQL.Clear;
  if aOn then q1.SQL.Add('PRAGMA foreign_keys = on')
         else q1.SQL.Add('PRAGMA foreign_keys = off');
  q1.ExecSQL;
end;

constructor TDBSchemaSync.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDictTables:=TStringList.Create;
  list:=TStringList.Create;
  log:=TStringList.Create;
  sdb:=TZConnection.Create(nil);
  q1:=TZQuery.Create(nil);
  q2:=TZQuery.Create(nil);
  qq:=TZQuery.Create(nil);
  sq1:=TZQuery.Create(nil);
  sq2:=TZQuery.Create(nil);
  sq1.Connection:=sdb;
  sq2.Connection:=sdb;
  FZnacznikCzasu:=false;
  sqlite:=false;
end;

destructor TDBSchemaSync.Destroy;
begin
  FDictTables.Free;
  list.Free;
  log.Free;
  q1.Free;
  q2.Free;
  qq.Free;
  sq1.Free;
  sq2.Free;
  sdb.Free;
  inherited Destroy;
end;

procedure TDBSchemaSync.init;
begin
  sqlite:=(FDB.Protocol='sqlite-3') or (FDB.Protocol='sqlite');
  q1.Connection:=FDB;
  q2.Connection:=FDB;
  qq.Connection:=FDB;
end;

procedure TDBSchemaSync.ShowTables(tables: TStrings);
var
  s: string;
begin
  tables.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('show tables');
  q1.Open;
  while not q1.EOF do
  begin
    s:=q1.Fields[0].AsString;
    tables.Add(s);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSync.ShowTablesSqlite(aBody: TStrings);
begin
  aBody.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('SHOW FULL TABLES WHERE table_type = :typ');
  q1.ParamByName('typ').AsString:='BASE TABLE';
  q1.Open;
  while not q1.EOF do
  begin
    aBody.Add(q1.Fields[0].AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSync.ShowViewsSqlite(aBody: TStrings);
begin
  aBody.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('SHOW FULL TABLES WHERE table_type = :typ');
  q1.ParamByName('typ').AsString:='VIEW';
  q1.Open;
  while not q1.EOF do
  begin
    aBody.Add(q1.Fields[0].AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSync.ShowIndexesSqlite(aBody: TStrings);
begin
  aBody.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select name from sqlite_master where type=:typ and sql is not null and sql<>:pusty');
  q1.ParamByName('typ').AsString:='index';
  q1.ParamByName('pusty').AsString:='';
  q1.Open;
  while not q1.EOF do
  begin
    aBody.Add(q1.Fields[0].AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSync.ShowTriggers(nazwa, cialo: TStrings);
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

procedure TDBSchemaSync.ShowFunctions(nazwa, cialo: TStrings);
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

procedure TDBSchemaSync.ShowProcedures(nazwa, cialo: TStrings);
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

procedure TDBSchemaSync.ShowCreateTable(table: string; schema: TStrings);
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

procedure TDBSchemaSync.ShowCreateTable(table: string; var schema: string);
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

procedure TDBSchemaSync.ShowCreateViewSqlite(aView: string;
  var aWektor: string; aBody: TStrings);
begin
  aBody.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select tbl_name,sql from sqlite_master where type=:typ and name=:name');
  q1.ParamByName('typ').AsString:='view';
  q1.ParamByName('name').AsString:=aView;
  q1.Open;
  aWektor:=q1.FieldByName('tbl_name').AsString;
  while not q1.EOF do
  begin
    aBody.Add(q1.FieldByName('sql').AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSync.ShowCreateViewSqlite(aView: string; aBody: TStrings);
var
  wektor: string;
begin
  ShowCreateViewSqlite(aView,wektor,aBody);
end;

procedure TDBSchemaSync.SaveSchema;
var
  i,j,max: integer;
  pom,vv,v1,v2: TStringList;
  czas: TDateTime;
  s,s1,spom,s2,s3: string;
begin
  if sqlite then exit;
  pom:=TStringList.Create;
  vv:=TStringList.Create;
  v1:=TStringList.Create;
  v2:=TStringList.Create;
  try
    if FileExists(dbsql) then DeleteFile(dbsql);
    sdb.Protocol:='sqlite-3';
    sdb.ClientCodepage:='UTF-8';
    sdb.ControlsCodePage:=cCP_UTF8;
    sdb.Database:=dbsql;
    sdb.Connect;
    (* tabele i widoki *)
    sq1.SQL.Clear;
    sq1.SQL.Add('create table tabele (id integer primary key autoincrement,nazwa text,typ integer,znacznik_czasu datetime,definicja blob)');
    sq1.ExecSQL;
    sq1.SQL.Clear;
    sq1.SQL.Add('create table slowniki (id integer primary key autoincrement,nazwa text,indeks text,definicja blob)');
    sq1.ExecSQL;
    sq1.SQL.Clear;
    sq1.SQL.Add('insert into tabele (nazwa,typ,znacznik_czasu,definicja) values (:nazwa,:typ,:z_czasu,:definicja)');
    sq1.Prepare;
    ShowTables(vv);
    StartTransaction;
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
      sq1.ParamByName('typ').AsInteger:=1;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=pom.Text;
      sq1.ExecSQL;
    end;
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.ParamByName('typ').AsInteger:=2;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=v2[i];
      sq1.ExecSQL;
    end;
    (* wyzwalacze *)
    v1.Clear;
    v2.Clear;
    ShowTriggers(v1,v2);
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.ParamByName('typ').AsInteger:=3;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=v2[i];
      sq1.ExecSQL;
    end;
    (* funkcje *)
    v1.Clear;
    v2.Clear;
    ShowFunctions(v1,v2);
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.ParamByName('typ').AsInteger:=4;
      if odczytaj_znacznik_czasu(v2[i],czas) then sq1.ParamByName('z_czasu').AsDateTime:=czas else sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=v2[i];
      sq1.ExecSQL;
    end;
    (* procedury *)
    v1.Clear;
    v2.Clear;
    ShowProcedures(v1,v2);
    for i:=0 to v1.Count-1 do
    begin
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.ParamByName('typ').AsInteger:=5;
      if odczytaj_znacznik_czasu(v2[i],czas) then sq1.ParamByName('z_czasu').AsDateTime:=czas else sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=v2[i];
      sq1.ExecSQL;
    end;
    (* Dictionary Tables - Tabele Słownikowe *)
    sq1.SQL.Clear;
    sq1.SQL.Add('insert into slowniki (nazwa,indeks,definicja) values (:nazwa,:indeks,:definicja)');
    sq1.Prepare;
    for i:=0 to FDictTables.Count-1 do
    begin
      s:=FDictTables[i];
      s1:=trim(GetLineToStr(s,1,':'));
      spom:=trim(GetLineToStr(s,2,':'));
      s2:=trim(GetLineToStr(spom,1,'='));
      s3:=trim(GetLineToStr(spom,2,'='));
      //writeln('TABELA = ',s1,' INDEX = ',s2);
      sq1.ParamByName('nazwa').AsString:=s1;
      sq1.ParamByName('indeks').AsString:=s2;
      q1.SQL.Clear;
      q1.SQL.Add('select * from '+s1);
      if s3<>'' then q1.SQL.Add('where '+s2+'=:wartosc');
      if s2<>'' then q1.SQL.Add('order by '+s2);
      if s3<>'' then q1.ParamByName('wartosc').AsString:=s3;
      s:='';
      q1.Open;
      while not q1.EOF do
      begin
        max:=q1.Fields.Count-1;
        for j:=0 to max do
        begin
          if q1.Fields[j].DataType=ftDate then s1:=FormatDateTime('yyyy-mm-dd',q1.Fields[j].AsDateTime) else
          if q1.Fields[j].DataType=ftTime then s1:=FormatDateTime('hh:nn:ss',q1.Fields[j].AsDateTime) else
          if q1.Fields[j].DataType=ftDateTime then s1:=FormatDateTime('yyyy-mm-dd hh:nn:ss',q1.Fields[j].AsDateTime) else
          if q1.Fields[j].DataType=ftTimeStamp then s1:=FormatDateTime('yyyy-mm-dd hh:nn:ss',q1.Fields[j].AsDateTime) else
          s1:=q1.Fields[j].AsString;
          s1:=StringReplace(s1,',','{$1}',[rfReplaceAll]);
          s1:=StringReplace(s1,';','{$2}',[rfReplaceAll]);
          if j=max then s:=s+s1 else s:=s+s1+',';
        end;
        s:=s+';';
        q1.Next;
      end;
      sq1.ParamByName('definicja').AsString:=s;
      sq1.ExecSQL;
      q1.Close;
    end;
    Commit;
    sdb.Disconnect;
  finally
    pom.Free;
    vv.Clear;
    v1.Clear;
    v2.Clear;
  end;
end;

function TDBSchemaSync.SyncSchema: boolean;
var
  b,pragma_fk: boolean;
  ee: integer;
begin
  if not FileExists(dbsql) then
  begin
    err:=1;
    error:='Brak struktury wzorcowej, synchronizacja anulowana.';
    result:=false;
    exit;
  end;
  sdb.Protocol:='sqlite-3';
  sdb.Database:=dbsql;
  sdb.Connect;
  try

    try
      if sqlite then
      begin
        pragma_fk:=PragmaForeignKeys;
        if pragma_fk then PragmaForeignKeys(false);
      end else pragma_fk:=false;
      if sqlite then
      begin
        ee:=101;
        b:=sync_delete_sqlite;
        ee:=102;
        b:=sync_delete_sqlite_2;
      end else begin
        ee:=103;
        b:=sync_delete;
      end;
      ee:=104;
      if b then b:=sync_table;
      ee:=105;
      if b then b:=sync_event;
      ee:=106;
      if b then b:=sync_slow;
    finally
      if pragma_fk then PragmaForeignKeys(true);
    end;

  except
    on E: Exception do
    begin
      if error='' then error:=E.Message;
      err:=ee;
      b:=false;
    end;
  end;
  sdb.Disconnect;
  if b then
  begin
    err:=0;
    error:='';
  end;
  result:=b;
end;

end.
