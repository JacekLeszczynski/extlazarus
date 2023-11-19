unit LiveTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

type

  { TLiveTimer }

  TLiveTimer = class(TComponent)
  private
    FActive: boolean;
    czas_start: integer;
    FCorrection: integer;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    function Start(time_index_start: integer = 0): integer;
    function Stop: integer;
    function GetIndexTime: integer;
    function GetIndexStartTime: integer;
    procedure SetIndexStartTime(aTimeIndexStart: integer);
  published
    property Active: boolean read FActive default false;
    property Correction: integer read FCorrection write FCorrection default 0;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

procedure Register;

implementation

uses
  ecode_unit;

procedure Register;
begin
  {$I livetimer_icon.lrs}
  RegisterComponents('System',[TLiveTimer]);
end;

{ TLiveTimer }

constructor TLiveTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=false;
  FCorrection:=0;
end;

function TLiveTimer.Start(time_index_start: integer): integer;
begin
  if FActive then exit;
  if time_index_start=0 then czas_start:=TimeToInteger else czas_start:=time_index_start;
  FActive:=true;
  if assigned(FOnStart) then FOnStart(self);
  result:=czas_start;
end;

function TLiveTimer.Stop: integer;
begin
  if not FActive then exit;
  czas_start:=0;
  FActive:=false;
  if assigned(FOnStop) then FOnStop(self);
  result:=czas_start;
end;

function TLiveTimer.GetIndexTime: integer;
var
  a: integer;
begin
  if not FActive then exit;
  a:=TimeToInteger+FCorrection;
  if a<0 then czas_start:=czas_start+(24*60*60*1000);
  while a<czas_start do czas_start:=czas_start-(24*60*60*1000);
  result:=a-czas_start;
end;

function TLiveTimer.GetIndexStartTime: integer;
begin
  result:=czas_start;
end;

procedure TLiveTimer.SetIndexStartTime(aTimeIndexStart: integer);
begin
  czas_start:=aTimeIndexStart;
end;

end.
