unit DBSchemaSyncSqlite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, DB, ZConnection, ZDataset, ZSqlProcessor;

type

  { TDBSchemaSyncSqlite }

  TDBSchemaSyncSqlite = class(TComponent)
  private
    dbsql: string;
    //list: TStringList;
    FDB: TZConnection;
    sdb,sdb2: TZConnection;
    sq1,sq2: TZQuery;
    q1,q2,qq: TZQuery;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function CalculateDiffPool(aBody1,aBody2: TStrings): string;
    procedure wewn_normalize_create_table(aSrc,aDest: TStrings);
    function wewn_delete_dolars(aCialo: string): string;
    procedure wewn_delete_dolars(aCialo: TStrings);
    function sync_delete:boolean;
    function sync_delete2:boolean;
    function sync_table:boolean;
    function sync_event:boolean;
    function akt_table(nazwa:string;s1,s2:TStrings):boolean;
    function akt_restrukturyzacja(cialo,stare:TStrings;nazwa:string): boolean;
    function PragmaForeignKeys: boolean;
    procedure PragmaForeignKeys(aOn: boolean);
  protected
  public
    err: integer;
    error: string;
    log: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure init;
    procedure ShowTables(aBody: TStrings);
    procedure ShowIndexes(aBody: TStrings);
    function ShowAutoIndexName(aTable: string; aNr: integer): string;
    procedure ShowViews(aBody: TStrings);
    procedure ShowCreateTable(aTable: string; aBody: TStrings);
    procedure ShowCreateIndex(aIndex: string; var aWektor: string; aBody: TStrings);
    procedure ShowCreateIndex(aIndex: string; aBody: TStrings);
    procedure ShowCreateView(aView: string; var aWektor: string; aBody: TStrings);
    procedure ShowCreateView(aView: string; aBody: TStrings);
    procedure SaveSchema;
    function SyncSchema:boolean;
  published
    property DB_Connection: TZConnection read FDB write FDB;
    property StructFileName: string read dbsql write dbsql;
  end;

procedure Register;

implementation

uses
  ecode_unit, ZDbcIntfs, ZScriptParser;

procedure Register;
begin
  {$I dbschemasyncsqlite_icon.lrs}
  RegisterComponents('System',[TDBSchemaSyncSqlite]);
end;

procedure StrToListItems(s: string; list: TStrings);
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

function StringToItemIndex(slist: TStrings; kod: string; wart_domyslna: integer = -1): integer;
var
  i,a: integer;
begin
   a:=wart_domyslna;
   for i:=0 to slist.Count-1 do if slist[i]=kod then
   begin
     a:=i;
     break;
   end;
   result:=a;
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
    while pos('  ',s)>0 do s:=StringReplace(s,'  ',' ',[rfReplaceAll]);
    if s[length(s)]=',' then delete(s,length(s),1);
    if pos('PRIMARY KEY',uppercase(s))>0 then tt:=1 else
    if pos('UNIQUE (',uppercase(s))>0 then tt:=2 else
    if pos('FOREIGN KEY',uppercase(s))>0 then tt:=3 else tt:=0;
    if tt=typ then case typ of
      0: if pp then cel.Add(s) else cel.Add(nazwa_pola(s));
      1: cel.Add(s);
      2: cel.Add(s);
      3: cel.Add(s);
    end;
  end;
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

{ TDBSchemaSyncSqlite }

procedure TDBSchemaSyncSqlite.StartTransaction;
begin
  sdb.TransactIsolationLevel:=tiReadCommitted;
  sdb.StartTransaction;
end;

procedure TDBSchemaSyncSqlite.Commit;
begin
  sdb.Commit;
  sdb.TransactIsolationLevel:=tiNone;
end;

procedure TDBSchemaSyncSqlite.Rollback;
begin
  sdb.Rollback;
  sdb.TransactIsolationLevel:=tiNone;
end;

function TDBSchemaSyncSqlite.CalculateDiffPool(aBody1, aBody2: TStrings
  ): string;
var
  s1,s2,s,pom: string;
  t1,t2,t3: TStringList;
  i,a,b: integer;
begin
  s1:=StringReplace(aBody1.Text,#13,'',[rfReplaceAll]);
  s2:=StringReplace(aBody2.Text,#13,'',[rfReplaceAll]);
  s1:=StringReplace(s1,#10,' ',[rfReplaceAll]);
  s2:=StringReplace(s2,#10,' ',[rfReplaceAll]);
  while pos('  ',s1)>0 do s1:=StringReplace(s1,'  ',' ',[rfReplaceAll]);
  while pos('  ',s2)>0 do s2:=StringReplace(s2,'  ',' ',[rfReplaceAll]);
  s1:=StringReplace(s1,'( ','(',[rfReplaceAll]);
  s2:=StringReplace(s2,'( ','(',[rfReplaceAll]);
  s1:=StringReplace(s1,' )',')',[rfReplaceAll]);
  s2:=StringReplace(s2,' )',')',[rfReplaceAll]);
  s1:=StringReplace(s1,', ',',',[rfReplaceAll]);
  s2:=StringReplace(s2,', ',',',[rfReplaceAll]);
  s1:=StringReplace(s1,' ,',',',[rfReplaceAll]);
  s2:=StringReplace(s2,' ,',',',[rfReplaceAll]);
  s1:=trim(s1);
  s2:=trim(s2);
  t1:=TStringList.Create;
  t2:=TStringList.Create;
  t3:=TStringList.Create;
  try
    a:=pos('(',s1);
    delete(s1,1,a);
    a:=pos('(',s2);
    delete(s2,1,a);
    (* s1 *)
    while true do
    begin
      s:=GetLineToStr(s1,1,',');
      if s='' then break;
      if pos('PRIMARY KEY',s)=1 then break;
      pom:=GetLineToStr(s,1,' ');
      t1.Add(pom);
      a:=pos(',',s1);
      if a=0 then break;
      delete(s1,1,a);
    end;
    (* s2 *)
    while true do
    begin
      s:=GetLineToStr(s2,1,',');
      if s='' then break;
      if pos('PRIMARY KEY',s)=1 then break;
      pom:=GetLineToStr(s,1,' ');
      t2.Add(pom);
      a:=pos(',',s2);
      if a=0 then break;
      delete(s2,1,a);
    end;
    t1.Sort;
    t2.Sort;
    for i:=0 to t1.Count-1 do if StringToItemIndex(t2,t1[i])>-1 then t3.Add(t1[i]);
    for i:=0 to t2.Count-1 do if StringToItemIndex(t1,t2[i])>-1 then if StringToItemIndex(t3,t2[i])=-1 then t3.Add(t2[i]);
    s:='';
    for i:=0 to t3.Count-1 do if s='' then s:=t3[i] else s:=s+','+t3[i];
  finally
    t1.Free;
    t2.Free;
    t3.Free;
  end;
  result:=s;
end;

procedure TDBSchemaSyncSqlite.wewn_normalize_create_table(aSrc, aDest: TStrings);
var
  s,ss: string;
  a,b: integer;
  nazwa: string;
  nr: integer;
begin
  aDest.Clear;
  ss:=aSrc.Text;
  (* create table [nazwa] ( *)
  a:=pos('(',ss);
  s:=trim(copy(ss,1,a));
  aDest.Add(s);
  delete(ss,1,a);
  (* pobranie nazwy *)
  nazwa:=s;
  a:=pos('CREATE',uppercase(nazwa));
  delete(nazwa,a,6);
  a:=pos('TABLE',uppercase(nazwa));
  delete(nazwa,a,5);
  a:=pos('(',nazwa);
  delete(nazwa,a,1);
  nazwa:=trim(nazwa);
  nr:=0;
  (* pętla *)
  while true do
  begin
    a:=pos(',',ss);
    b:=pos('UNIQUE',uppercase(ss));
    if a>0 then
    begin
      s:=trim(copy(ss,1,a));
      aDest.Add(' '+s);
      delete(ss,1,a);
      continue;
    end;
    a:=pos('UNIQUE',uppercase(ss));
    b:=pos('FOREIGN',uppercase(ss));
    if (a>0) and ((b=0) or (a<b)) then
    begin
      a:=pos(')',ss);
      s:=trim(copy(ss,1,a));
      //aDest.Add(' '+s+'$'+ShowAutoIndexName(nazwa,nr)+'$');
      aDest.Add(' '+s);
      delete(ss,1,a);
      inc(nr);
      continue;
    end;
    if (b>0) and ((a=0) or (b<a)) then
    begin
      a:=pos('REFERENCES',uppercase(ss));
      s:=trim(copy(ss,1,a));
      delete(ss,1,a);
      a:=pos(')',ss);
      s:=s+trim(copy(ss,1,a));
      aDest.Add(' '+s);
      delete(ss,1,a);
      continue;
    end;
    break;
  end;
  a:=pos(')',ss);
  if a>1 then
  begin
    s:=trim(copy(ss,1,a-1));
    if s<>'' then aDest.Add(' '+s);
    delete(ss,1,a-1);
  end;
  aDest.Add(trim(ss));
end;

function TDBSchemaSyncSqlite.wewn_delete_dolars(aCialo: string): string;
var
  s: string;
  a,b: integer;
begin
  s:=aCialo;
  while true do
  begin
    a:=pos('$',s);
    if a=0 then break;
    delete(s,a,1);
    b:=pos('$',s);
    delete(s,a,b-a);
  end;
  result:=s;
end;

procedure TDBSchemaSyncSqlite.wewn_delete_dolars(aCialo: TStrings);
var
  i: integer;
  s: string;
begin
  for i:=0 to aCialo.Count-1 do
  begin
    s:=aCialo[i];
    if pos('$',s)>0 then
    begin
      s:=wewn_delete_dolars(s);
      aCialo.Delete(i);
      aCialo.Insert(i,s);
    end;
  end;
end;

function TDBSchemaSyncSqlite.sync_delete: boolean;
var
  i: integer;
  v1: TStringList;
  s: string;
begin
  (* usuwamy ze struktury objekty nie istniejące w bazie wzorcowej *)
  v1:=TStringList.Create;
  try
    (* widoki *)
    ShowViews(v1);
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
          writeln('drop view '+v1[i]);
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
    ShowIndexes(v1);
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
          writeln('drop index '+v1[i]);
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
          writeln('drop table '+v1[i]);
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

function TDBSchemaSyncSqlite.sync_delete2: boolean;
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
    ShowViews(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select definicja from tabele where nazwa=:nazwa and typ=3');
    sq1.Prepare;
    for i:=0 to v1.Count-1 do
    begin
      ShowCreateView(v1[i],v2);
      sq1.ParamByName('nazwa').AsString:=v1[i];
      sq1.Open;
      if sq1.Fields[0].AsString<>v2.Text then
      begin
        qq.SQL.Clear;
        qq.SQL.Add('drop view '+v1[i]);
        try
          writeln('(2) drop view '+v1[i]);
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

function TDBSchemaSyncSqlite.sync_table: boolean;
var
  i: integer;
  list,defs,v1: TStringList;
  nazwa,def: string;
  b: boolean;
begin
  (* aktualizacja tabel *)
  b:=true;
  list:=TStringList.Create;
  defs:=TStringList.Create;
  v1:=TStringList.Create;
  try
    (* tabele *)
    sq1.SQL.Clear;
    sq1.SQL.Add('select nazwa from tabele where typ=1 order by id');
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
    for i:=0 to list.Count-1 do
    begin
      nazwa:=list[i];
      sq1.ParamByName('nazwa').AsString:=nazwa;
      sq1.Open;
      def:=sq1.FieldByName('definicja').AsString;
      sq1.Close;
      StrToListItems(def,defs);
      (* sprawdzamy referencje i wykonujemy aktualizację jeśli trzeba *)
      ShowCreateTable(nazwa,v1);
      if v1.Text<>defs.Text then b:=akt_table(nazwa,defs,v1);
      //b:=akt_table(nazwa,defs,v1);
      //if v1.Text<>defs.Text then writeln('DO ZMIANY!');
    end;
  finally
    list.Free;
    defs.Free;
    v1.Free;
  end;
  result:=b;
end;

function TDBSchemaSyncSqlite.sync_event: boolean;
var
  a: integer;
  v1: TStringList;
  s: string;
begin
  (* dodaję do struktury brakujące objekty istniejące w bazie wzorcowej *)
  v1:=TStringList.Create;
  try
    (* widoki *)
    ShowViews(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select id,nazwa,wektor,definicja from tabele where typ=3');
    sq1.Open;
    while not sq1.EOF do
    begin
      a:=StringToItemIndex(v1,sq1.FieldByName('nazwa').AsString);
      if a=-1 then
      begin
        (* dodaję brakujący widok *)
        qq.SQL.Clear;
        qq.SQL.Add(sq1.FieldByName('definicja').AsString);
        try
          writeln('create view '+sq1.FieldByName('nazwa').AsString);
          qq.ExecSQL;
        except
          log.Add('SYNC-CREATE-VIEW: Błąd podczas dodawania widoku: ['+sq1.FieldByName('nazwa').AsString+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Next;
    end;
    sq1.Close;
    (* indeksy *)
    ShowIndexes(v1);
    sq1.SQL.Clear;
    sq1.SQL.Add('select id,nazwa,wektor,definicja from tabele where typ=2');
    sq1.Open;
    while not sq1.EOF do
    begin
      a:=StringToItemIndex(v1,sq1.FieldByName('nazwa').AsString);
      if a=-1 then
      begin
        (* dodaję brakujący indeks *)
        qq.SQL.Clear;
        qq.SQL.Add(sq1.FieldByName('definicja').AsString);
        try
          writeln('create index '+sq1.FieldByName('nazwa').AsString);
          qq.ExecSQL;
        except
          log.Add('SYNC-CREATE-INDEX: Błąd podczas dodawania indeksu: ['+sq1.FieldByName('nazwa').AsString+']:'+#10#13+qq.SQL.Text);
          result:=false;
          exit;
        end;
      end;
      sq1.Next;
    end;
    sq1.Close;
    (* WSZYSTKO *)
  finally
    v1.Free;
  end;
  result:=true;
end;

function TDBSchemaSyncSqlite.akt_table(nazwa: string; s1, s2: TStrings
  ): boolean;
var
  b_restrukturyzacja: boolean;
  p1,p2,pp: TStringList;
  i,a: integer;
  s: string;
  d1,d2,e1,e2: string;
  tabela_ok: boolean;
begin
  result:=true;
  tabela_ok:=false;
  if s2.Count=0 then
  begin
    (* tej tabeli nie ma - tworzę ją *)
    q1.SQL.Assign(s1);
    try
      writeln('create table '+nazwa);
      q1.ExecSQL;
      tabela_ok:=true;
    except
      log.Add('SYNC-CREATE: Błąd podczas tworzenia tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
      result:=false;
      exit;
    end;
  end;
  (* zmiany dot. zmiany nazw kolumn jeśli istnieją różnice tylko w nazwach *)
  if not tabela_ok then for i:=0 to s1.Count-1 do
  begin
    d1:=s1[i];
    d2:=s2[i];
    if d1=d2 then continue;
    e1:=GetLineToStr(d1,1,' ');
    e2:=GetLineToStr(d2,1,' ');
    d2:=StringReplace(d2,e2,e1,[]);
    if d1=d2 then
    begin
      q1.SQL.Clear;
      q1.SQL.Add('ALTER TABLE '+nazwa+' RENAME COLUMN '+e2+' TO '+e1);
      try
        writeln('alter table '+nazwa+' RENAME COLUMN '+e2+' TO '+e1);
        q1.ExecSQL;
      except
        log.Add('SYNC-ALTER: Błąd podczas zmiany nazwy kolumny z tabeli: ['+nazwa+']:'+#10#13+q1.SQL.Text);
        result:=false;
        exit;
      end;
    end else break;
  end;
  (* cokolwiek jest inaczej z kolumnami - wykonujemy restrukturyzację *)
  if not tabela_ok then for i:=0 to s1.Count-1 do
  begin
    if s1[i]<>s2[i] then
    begin
      b_restrukturyzacja:=true;
      break;
    end;
  end;
  if not tabela_ok then for i:=0 to s2.Count-1 do
  begin
    if s1[i]<>s2[i] then
    begin
      b_restrukturyzacja:=true;
      break;
    end;
  end;
  (* Jeśli zarządano restrukturyzacji *)
  if b_restrukturyzacja then if akt_restrukturyzacja(s1,s2,nazwa) then
  begin
    result:=true;
    tabela_ok:=true;
  end else begin
    log.Add('SYNC-ALTER: Błąd wykonania restrukturyzacji, tabela: ['+nazwa+']:'+#10#13);
    result:=false;
    exit;
  end;

  (* tabela jest ok - teraz czas na indeksy *)
  exit;

  result:=false;
  b_restrukturyzacja:=false;
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
      b_restrukturyzacja:=true;
      break;
    end;
    (* usuwam gotowe do usunięcia indeksy definiowane wewnątrz tabeli *)
    if not b_restrukturyzacja then
    begin
      pola_to_indeksy(s1,p1,2);
      pola_to_indeksy(s2,p2,2);
      for i:=0 to p2.Count-1 do if StringToItemIndex(p1,p2[i],-1)=-1 then
      begin
        b_restrukturyzacja:=true;
        break;
      end;
    end;
    (* usuwam gotowe do usunięcia indeks pierwszy *)
    if not b_restrukturyzacja then
    begin
      pola_to_indeksy(s1,p1,1);
      pola_to_indeksy(s2,p2,1);
      if p2.Count=1 then if (p1.Count=0) or ((p1.Count=1) and (p1[0]<>p2[0])) then b_restrukturyzacja:=true;
    end;
    (* usuwam gotowe do usunięcia pola tabeli *)
    if not b_restrukturyzacja then
    begin
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
    end;
    (* Jeśli zarządano restrukturyzacji *)
    if b_restrukturyzacja then if akt_restrukturyzacja(s1,s2,nazwa) then
    begin
      result:=true;
      exit;
    end else begin
      log.Add('SYNC-ALTER: Błąd wykonania restrukturyzacji, tabela: ['+nazwa+']:'+#10#13);
      result:=false;
      exit;
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
    {pola_to_indeksy(s1,pp,0);
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
    end;}
    (* dodaję brakujący indeks pierwszy *)
    pola_to_indeksy(s1,p1,1);
    pola_to_indeksy(s2,p2,1);
    if not b_restrukturyzacja then if (p1.Count=1) and (p2.Count=0) then b_restrukturyzacja:=true;
    (* dodaję brakujące indeksy *)
    if not b_restrukturyzacja then
    begin
      pola_to_indeksy(s1,p1,2);
      pola_to_indeksy(s2,p2,2);
      for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
      begin
        begin
          b_restrukturyzacja:=true;
          break;
        end;
      end;
    end;
    (* dodaję brakujące klucze obce *)
    if not b_restrukturyzacja then
    begin
      pola_to_indeksy(s1,p1,3);
      pola_to_indeksy(s2,p2,3);
      for i:=0 to p1.Count-1 do if StringToItemIndex(p2,p1[i],-1)=-1 then
      begin
        begin
          b_restrukturyzacja:=true;
          break;
        end;
      end;
    end;
    (* Jeśli zarządano restrukturyzacji *)
    if b_restrukturyzacja then if akt_restrukturyzacja(s1,s2,nazwa) then
    begin
      result:=true;
      exit;
    end else begin
      log.Add('SYNC-ALTER: Błąd wykonania restrukturyzacji, tabela: ['+nazwa+']:'+#10#13);
      result:=false;
      exit;
    end;
    (* Dodaję brakujące indeksy zewnętrzne *)
    {TU MA BYĆ TEN KOD}
  finally
    pp.Free;
    p1.Free;
    p2.Free;
  end;
  result:=true;
end;

function TDBSchemaSyncSqlite.akt_restrukturyzacja(cialo, stare: TStrings;
  nazwa: string): boolean;
var
  s: string;
begin
  writeln('restrukturyzacja tabeli: '+nazwa);
  s:=CalculateDiffPool(cialo,stare);
  try
    q1.SQL.Clear;
    q1.SQL.Add('alter table '+nazwa+' rename to '+nazwa+'_old');
    q1.ExecSQL;

    q1.SQL.Assign(cialo);
    q1.ExecSQL;

    q1.SQL.Clear;
    q1.SQL.Add('insert into '+nazwa+' ('+s+')');
    q1.SQL.Add('select '+s+' from '+nazwa+'_old');
    q1.ExecSQL;

    q1.SQL.Clear;
    q1.SQL.Add('drop table '+nazwa+'_old');
    q1.ExecSQL;
    result:=true;
  except
    result:=false;
  end;
end;

function TDBSchemaSyncSqlite.PragmaForeignKeys: boolean;
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

procedure TDBSchemaSyncSqlite.PragmaForeignKeys(aOn: boolean);
begin
  q1.SQL.Clear;
  if aOn then q1.SQL.Add('PRAGMA foreign_keys = on')
         else q1.SQL.Add('PRAGMA foreign_keys = off');
  q1.ExecSQL;
end;

constructor TDBSchemaSyncSqlite.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  log:=TStringList.Create;
  sdb:=TZConnection.Create(nil);
  sdb2:=TZConnection.Create(nil);
  q1:=TZQuery.Create(nil);
  q2:=TZQuery.Create(nil);
  qq:=TZQuery.Create(nil);
  sq1:=TZQuery.Create(nil);
  sq2:=TZQuery.Create(nil);
  sq1.Connection:=sdb;
  sq2.Connection:=sdb;
end;

destructor TDBSchemaSyncSqlite.Destroy;
begin
  log.Free;
  q1.Free;
  q2.Free;
  qq.Free;
  sq1.Free;
  sq2.Free;
  sdb.Free;
  sdb2.Free;
  inherited Destroy;
end;

procedure TDBSchemaSyncSqlite.init;
begin
  q1.Connection:=FDB;
  q2.Connection:=FDB;
  qq.Connection:=FDB;
end;

procedure TDBSchemaSyncSqlite.ShowTables(aBody: TStrings);
begin
  aBody.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select name from sqlite_master where type=:typ');
  q1.ParamByName('typ').AsString:='table';
  q1.Open;
  while not q1.EOF do
  begin
    aBody.Add(q1.Fields[0].AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncSqlite.ShowIndexes(aBody: TStrings);
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

function TDBSchemaSyncSqlite.ShowAutoIndexName(aTable: string; aNr: integer): string;
var
  s: string;
  i: integer;
begin
  q1.SQL.Clear;
  q1.SQL.Add('select name from sqlite_master where type=:typ and tbl_name=:table order by name');
  q1.ParamByName('typ').AsString:='index';
  q1.ParamByName('table').AsString:=aTable;
  q1.Open;
  for i:=1 to aNr do q1.Next;
  result:=q1.Fields[0].AsString;
  q1.Close;
end;

procedure TDBSchemaSyncSqlite.ShowViews(aBody: TStrings);
begin
  aBody.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select name from sqlite_master where type=:typ');
  q1.ParamByName('typ').AsString:='view';
  q1.Open;
  while not q1.EOF do
  begin
    aBody.Add(q1.Fields[0].AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncSqlite.ShowCreateTable(aTable: string; aBody: TStrings);
var
  s: string;
  ss: TStringList;
  b: boolean;
begin
  b:=false;
  aBody.Clear;
  ss:=TStringList.Create;
  try
    q1.SQL.Clear;
    q1.SQL.Add('select sql from sqlite_master where type=:typ and name=:name');
    q1.ParamByName('typ').AsString:='table';
    q1.ParamByName('name').AsString:=aTable;
    q1.Open;
    if not q1.IsEmpty then
    begin
      while not q1.EOF do
      begin
        s:=StringReplace(q1.Fields[0].AsString,#9,' ',[rfReplaceAll]);
        s:=StringReplace(s,'"','',[rfReplaceAll]);
        ss.Add(s);
        q1.Next;
      end;
      b:=true;
    end;
    q1.Close;
    if b then wewn_normalize_create_table(ss,aBody);
  finally
    ss.Free;
  end;
end;

procedure TDBSchemaSyncSqlite.ShowCreateIndex(aIndex: string;
  var aWektor: string; aBody: TStrings);
begin
  aBody.Clear;
  q1.SQL.Clear;
  q1.SQL.Add('select tbl_name,sql from sqlite_master where type=:typ and name=:name');
  q1.ParamByName('typ').AsString:='index';
  q1.ParamByName('name').AsString:=aIndex;
  q1.Open;
  aWektor:=q1.FieldByName('tbl_name').AsString;
  while not q1.EOF do
  begin
    aBody.Add(q1.FieldByName('sql').AsString);
    q1.Next;
  end;
  q1.Close;
end;

procedure TDBSchemaSyncSqlite.ShowCreateIndex(aIndex: string; aBody: TStrings);
var
  wektor: string;
begin
  ShowCreateIndex(aIndex,wektor,aBody);
end;

procedure TDBSchemaSyncSqlite.ShowCreateView(aView: string;
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

procedure TDBSchemaSyncSqlite.ShowCreateView(aView: string; aBody: TStrings);
var
  wektor: string;
begin
  ShowCreateView(aView,wektor,aBody);
end;

procedure TDBSchemaSyncSqlite.SaveSchema;
var
  i: integer;
  pom,vv: TStringList;
  czas: TDateTime;
  wektor: string;
begin
  pom:=TStringList.Create;
  vv:=TStringList.Create;
  try
    if FileExists(dbsql) then DeleteFile(dbsql);
    sdb.Protocol:='sqlite-3';
    sdb.Database:=dbsql;
    sdb.Connect;
    (* przygotowanie bazy do zapisu struktury *)
    sq1.SQL.Clear;
    sq1.SQL.Add('create table tabele (id integer primary key autoincrement,nazwa text,wektor text,typ integer,znacznik_czasu datetime,definicja blob)');
    sq1.ExecSQL;
    sq1.SQL.Clear; sq1.SQL.Add('create index tabele_index_nazwa on tabele(nazwa)'); sq1.ExecSQL;
    sq1.SQL.Clear; sq1.SQL.Add('create index tabele_index_wektor on tabele(wektor)'); sq1.ExecSQL;
    sq1.SQL.Clear; sq1.SQL.Add('create index tabele_index_typ on tabele(typ)'); sq1.ExecSQL;
    sq1.SQL.Clear;
    sq1.SQL.Add('insert into tabele (nazwa,wektor,typ,znacznik_czasu,definicja) values (:nazwa,:wektor,:typ,:z_czasu,:definicja)');
    sq1.Prepare;
    (* tabele *)
    ShowTables(vv);
    StartTransaction;
    for i:=0 to vv.Count-1 do
    begin
      ShowCreateTable(vv[i],pom);
      sq1.ParamByName('nazwa').AsString:=vv[i];
      sq1.ParamByName('wektor').Clear;
      sq1.ParamByName('typ').AsInteger:=1;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=pom.Text;
      sq1.ExecSQL;
    end;
    (* indeksy *)
    ShowIndexes(vv);
    for i:=0 to vv.Count-1 do
    begin
      ShowCreateIndex(vv[i],wektor,pom);
      sq1.ParamByName('nazwa').AsString:=vv[i];
      sq1.ParamByName('wektor').AsString:=wektor;
      sq1.ParamByName('typ').AsInteger:=2;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=pom.Text;
      sq1.ExecSQL;
    end;
    (* widoki *)
    ShowViews(vv);
    for i:=0 to vv.Count-1 do
    begin
      ShowCreateView(vv[i],wektor,pom);
      sq1.ParamByName('nazwa').AsString:=vv[i];
      sq1.ParamByName('wektor').AsString:=wektor;
      sq1.ParamByName('typ').AsInteger:=3;
      sq1.ParamByName('z_czasu').Clear;
      sq1.ParamByName('definicja').AsString:=pom.Text;
      sq1.ExecSQL;
    end;
    Commit;
    sdb.Disconnect;
  finally
    pom.Free;
    vv.Clear;
  end;
end;

function TDBSchemaSyncSqlite.SyncSchema: boolean;
var
  pragma_fk: boolean;
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
  sdb.Protocol:='sqlite-3';
  sdb.Database:=dbsql;
  sdb.Connect;
  try
    try
      pragma_fk:=PragmaForeignKeys;
      if pragma_fk then PragmaForeignKeys(false);
      (* usuwam obiekty nieistniejące w bazie *)
      ee:=101;
      b:=sync_delete;
      (* usuwam obiekty istniejące w bazie, lecz tylko te o zmienionej definicji (dot. widoków i indeksów) *)
      ee:=102;
      if b then b:=sync_delete2;
      (* aktualizuję struktury samych tabel *)
      ee:=103;
      if b then b:=sync_table;
      (* aktualizę/dodaję obiekty, których brakuje *)
      ee:=104;
      if b then b:=sync_event;
    finally
      if pragma_fk then PragmaForeignKeys(true);
    end;
  except
    err:=ee;
    b:=false;
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
