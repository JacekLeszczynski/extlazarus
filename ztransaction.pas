unit ZTransaction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ZConnection, ZDbcIntfs;

type

  { TZTransaction }

  TZTransaction = class(TComponent)
  private
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
  published
    property Database: TZConnection read FDatabase write FDatabase;
    property IsolationLevel: TZTransactIsolationLevel read FTransactIsolationLevel write FTransactIsolationLevel default tiReadCommitted;
  end;

procedure Register;

implementation

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
  przed_transakcja:=FDatabase.TransactIsolationLevel;
  FDatabase.TransactIsolationLevel:=FTransactIsolationLevel;
  FDatabase.StartTransaction;
end;

procedure TZTransaction.Commit;
begin
  FDatabase.Commit;
  FDatabase.TransactIsolationLevel:=przed_transakcja;
end;

procedure TZTransaction.Rollback;
begin
  FDatabase.Rollback;
  FDatabase.TransactIsolationLevel:=przed_transakcja;
end;

{ TZTransaction }

{

function TForm1.TimeToInteger(Time: TDateTime): integer;
var
  godz,min,sec,milisec: word;
begin
  DecodeTime(Time,godz,min,sec,milisec);
  result:=(godz*60*60*1000)+(min*60*1000)+(sec*1000)+milisec;
end;

function TForm1.TimeToInteger: integer;
begin
  result:=TimeToInteger(time);
end;

function TForm1.IntegerToTime(czas: integer): TDateTime;
var
  c: integer;
  godz,min,sec,milisec: word;
begin
  c:=czas;
  godz:=c div (60*60*1000);
  c:=c-(godz*(60*60*1000));
  min:=c div (60*1000);
  c:=c-(min*(60*1000));
  sec:=c div 1000;
  milisec:=c-(sec*1000);
  result:=EncodeTime(godz,min,sec,milisec);
end;

}

end.
