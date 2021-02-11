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
  protected
  public
    constructor Create(AOwner: TComponent); override;
    //destructor Destroy; override;
    procedure Start(time_index_start: integer = 0);
    procedure Stop;
    function GetIndexTime: integer;
    function GetIndexStartTime: integer;
    procedure SetIndexStartTime(aTimeIndexStart: integer);
  published
    property Active: boolean read FActive default false;
    property Correction: integer read FCorrection write FCorrection default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I livetimer_icon.lrs}
  RegisterComponents('System',[TLiveTimer]);
end;

function TimeToInteger(Hour, Minutes, Second, Milisecond: word): longword;
begin
  result:=(Hour*60*60*1000)+(Minutes*60*1000)+(Second*1000)+Milisecond;
end;

function TimeToInteger(Time: TDateTime): longword;
var
  godz,min,sec,milisec: word;
begin
  DecodeTime(Time,godz,min,sec,milisec);
  result:=(godz*60*60*1000)+(min*60*1000)+(sec*1000)+milisec;
end;

function TimeToInteger: longword;
begin
  result:=TimeToInteger(time);
end;

function IntegerToTime(czas: longword; no_milisecond: boolean): TDateTime;
var
  c: longword;
  godz,min,sec,milisec: word;
begin
  c:=czas;
  godz:=c div 3600000;
  c:=c-(godz*3600000);
  min:=c div 60000;
  c:=c-(min*60000);
  sec:=c div 1000;
  if no_milisecond then milisec:=0 else milisec:=c-(sec*1000);
  result:=EncodeTime(godz,min,sec,milisec);
end;

{ TLiveTimer }

constructor TLiveTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=false;
  FCorrection:=0;
end;

procedure TLiveTimer.Start(time_index_start: integer);
begin
  if FActive then exit;
  if time_index_start=0 then czas_start:=TimeToInteger else czas_start:=time_index_start;
  FActive:=true;
end;

procedure TLiveTimer.Stop;
begin
  if not FActive then exit;
  czas_start:=0;
  FActive:=false;
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
