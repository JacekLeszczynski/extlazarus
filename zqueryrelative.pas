unit ZQueryRelative;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ZDataset, ExtCtrls, DB;

type

  { TZQueryRelative }

  TBeforeScrollMasterEvent = procedure(DataSet: TDataSet) of object;

  TZQueryRelative = class(TZQuery)
  private
    { Private declarations }
    (* deklaracje properties *)
    FLoopDelay: boolean;
    (* deklaracje zmiennych dodatkowych *)
    tim: TIdleTimer;
    FBeforeScrollMaster: TBeforeScrollMasterEvent;
    __block_timer: boolean;
    __TIMER: integer;
    __DataSource: TDataSource;
    procedure timTimer(Sender: TObject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* procedury zdarzeń *)
    procedure ProcedureBeforeOpen(DataSet: TDataSet);
    procedure ProcedureBeforeScroll(DataSet: TDataSet);
  published
    { Published declarations }
    property LoopDelay: boolean read FLoopDelay write FLoopDelay default true;
    property BeforeScroollMaster: TBeforeScrollMasterEvent read FBeforeScrollMaster write FBeforeScrollMaster;
end;

procedure Register;

implementation

procedure Register;
begin
  {$I zqueryrelative_icon.lrs}
  RegisterComponents('Zeos Access',[TZQueryRelative]);
end;

{ TZQueryRelative }

constructor TZQueryRelative.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoopDelay:=true;
  __TIMER:=-1;
  __block_timer:=false;
  tim:=TIdleTimer.Create(nil);
  tim.Enabled:=false;
  tim.AutoStartEvent:=itaOnUserInput;
  tim.Interval:=80;
  tim.OnTimer:=@timTimer;
  self.BeforeOpen:=@ProcedureBeforeOpen;
end;

destructor TZQueryRelative.Destroy;
begin
  tim.Free;
  inherited Destroy;
end;

procedure TZQueryRelative.ProcedureBeforeOpen(DataSet: TDataSet);
begin
  self.DataSource.DataSet.BeforeScroll:=@ProcedureBeforeScroll;
  if Assigned(FBeforeScrollMaster) then FBeforeScrollMaster(self.DataSource.DataSet);
end;

procedure TZQueryRelative.ProcedureBeforeScroll(DataSet: TDataSet);
begin
  if not FLoopDelay then exit;
  if (__TIMER=-1) and self.Active then
  begin
    __TIMER:=0;
    self.DisableControls;
    __DataSource:=self.DataSource;
    self.DataSource:=nil;
    tim.Enabled:=true;
  end else if __TIMER>0 then __TIMER:=0;
end;

procedure TZQueryRelative.timTimer(Sender: TObject);
begin
  if not FLoopDelay then exit;
  if __block_timer then exit;
  if __Timer>2 then
  begin
    tim.Enabled:=false;
    //pozycje.Active:=true;
    self.DataSource:=__DataSource;
    self.EnableControls;
    __TIMER:=-1;
  end else inc(__TIMER);
end;

end.
