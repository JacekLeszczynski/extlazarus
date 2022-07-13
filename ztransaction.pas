unit ZTransaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, ZConnection, ZDbcIntfs, ZDataset;

type

  { TZTransaction }

  TZTransaction = class(TComponent)
  private
    ac: boolean;
    FDatabase: TZConnection;
    FTransactIsolationLevel: TZTransactIsolationLevel;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    procedure State(Sender: TObject; var aActive,aNoEmpty,aEdited: boolean);
    function SqlitePragmaForeignKeys: boolean;
    procedure SqlitePragmaForeignKeys(aOn: boolean);
    function GetLastId: integer;
  published
    property Database: TZConnection read FDatabase write FDatabase;
    property IsolationLevel: TZTransactIsolationLevel read FTransactIsolationLevel write FTransactIsolationLevel default tiReadCommitted;
  end;

procedure Register;

implementation

uses
  db;

var
  przed_transakcja: TZTransactIsolationLevel;

procedure Register;
begin
  {$I ztransaction_icon.lrs}
  RegisterComponents('Zeos Access',[TZTransaction]);
end;

{ TZTransaction }

constructor TZTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransactIsolationLevel:=tiReadCommitted;
end;

destructor TZTransaction.Destroy;
begin
  inherited Destroy;
end;

procedure TZTransaction.Init;
begin
  FDatabase.TransactIsolationLevel:=tiNone;
end;

procedure TZTransaction.StartTransaction;
begin
  ac:=FDatabase.AutoCommit;
  przed_transakcja:=FDatabase.TransactIsolationLevel;
  FDatabase.TransactIsolationLevel:=FTransactIsolationLevel;
  FDatabase.StartTransaction;
end;

procedure TZTransaction.Commit;
begin
  FDatabase.Commit;
  FDatabase.TransactIsolationLevel:=przed_transakcja;
  FDatabase.AutoCommit:=ac;
end;

procedure TZTransaction.Rollback;
begin
  FDatabase.Rollback;
  FDatabase.TransactIsolationLevel:=przed_transakcja;
  FDatabase.AutoCommit:=ac;
end;

procedure TZTransaction.State(Sender: TObject; var aActive, aNoEmpty,
  aEdited: boolean);
var
  a: TDataSource;
  b: TDataSet;
begin
  a:=TDataSource(Sender);
  b:=a.DataSet;
  aActive:=b.Active and (not (a.State in [dsEdit,dsInsert]));
  aNoEmpty:=aActive and (not b.IsEmpty);
  aEdited:=b.Active and (a.State in [dsEdit,dsInsert]);
end;

function TZTransaction.SqlitePragmaForeignKeys: boolean;
var
  q1: TZQuery;
  a: integer;
begin
  q1:=TZQuery.Create(nil);
  q1.Connection:=FDatabase;
  try
    q1.SQL.Add('PRAGMA foreign_keys');
    q1.Open;
    a:=q1.Fields[0].AsInteger;
    q1.Close;
  finally
    q1.Free;
  end;
  result:=a=1;
end;

procedure TZTransaction.SqlitePragmaForeignKeys(aOn: boolean);
var
  q1: TZQuery;
begin
  q1:=TZQuery.Create(nil);
  q1.Connection:=FDatabase;
  try
    if aOn then q1.SQL.Add('PRAGMA foreign_keys = on')
           else q1.SQL.Add('PRAGMA foreign_keys = off');
    q1.ExecSQL;
  finally
    q1.Free;
  end;
end;

function TZTransaction.GetLastId: integer;
var
  q1: TZQuery;
  a: integer;
begin
  if pos('sqlite',FDatabase.Protocol)=0 then
  begin
    result:=0;
    exit;
  end;
  q1:=TZQuery.Create(nil);
  q1.Connection:=FDatabase;
  q1.SQL.Add('select last_insert_rowid()');
  try
    q1.Open;
    if q1.IsEmpty then a:=0 else a:=q1.Fields[0].AsLargeInt;
    q1.Close;
  finally
    q1.Free;
  end;
  result:=a;
end;

{ TZTransaction }

end.
