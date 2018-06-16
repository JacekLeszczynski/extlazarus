unit UOSPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, UOSEngine;

type

  { TUOSPlayer }

  TUOSPlayerMode = (moPlay, moRecord);

  TUOSPlayer = class(TComponent)
  private
    FAfterMute: TNotifyEvent;
    FAfterStart: TNotifyEvent;
    FAfterStop: TNotifyEvent;
    FBeforeMute: TNotifyEvent;
    FBeforeStart: TNotifyEvent;
    FBeforeStop: TNotifyEvent;
    FBusy: boolean;
    FDevEngine: TUOSEngine;
    FFileName: string;
    FDevIndex: cardinal;
    FListenMic: boolean;
    FMode: TUOSPlayerMode;
    FMute: boolean;
    FOnStop: TNotifyEvent;
    FPause: boolean;
    FVolume: integer;
    InIndex, outIndex : integer;
    procedure SetMode(AValue: TUOSPlayerMode);
    procedure SetMute(AValue: boolean);
    procedure SetVolume(AValue: integer);
    procedure _init;
    procedure ClosePlayer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Replay;
    function GetStatus: integer;
  published
    property DeviceEngine: TUOSEngine read FDevEngine write FDevEngine;
    property Busy: boolean read FBusy default false;
    property Paused: boolean read FPause default false;
    property FileName: string read FFileName write FFileName;
    property Mode: TUOSPlayerMode read FMode write SetMode default moPlay;
    property ListenMic: boolean read FListenMic write FListenMic default false;
    property DeviceIndex: cardinal read FDevIndex write FDevIndex default 0;
    property Volume: integer read FVolume write SetVolume default 100;
    property Mute: boolean read FMute write SetMute default false; //Nie wycisza głośników, wycisza nagrywanie!
    property BeforeStart: TNotifyEvent read FBeforeStart write FBeforeStart;
    property AfterStart: TNotifyEvent read FAfterStart write FAfterStart;
    property BeforeStop: TNotifyEvent read FBeforeStop write FBeforeStop;
    property AfterStop: TNotifyEvent read FAfterStop write FAfterStop;
    property BeforeMute: TNotifyEvent read FBeforeMute write FBeforeMute;
    property AfterMute: TNotifyEvent read FAfterMute write FAfterMute;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
  end;

procedure Register;

implementation

uses
  uos_flat;

procedure Register;
begin
  {$I uosplayer_icon.lrs}
  RegisterComponents('Multimedia',[TUOSPlayer]);
end;

{ TUOSPlayer }

procedure TUOSPlayer._init;
begin
  FBusy:=false;
  FPause:=false;
  FDevIndex:=0;
  FMode:=moPlay;
  FListenMic:=false;
  FVolume:=100;
  FMute:=false;
end;

procedure TUOSPlayer.ClosePlayer;
begin
  FBusy:=false;
  FPause:=false;
  if Assigned(FOnStop) then FOnStop(self);
end;

procedure TUOSPlayer.SetMode(AValue: TUOSPlayerMode);
begin
  if FBusy or (FMode=AValue) then Exit;
  FMode:=AValue;
end;

procedure TUOSPlayer.SetMute(AValue: boolean);
var
  a: integer;
begin
  if Assigned(FBeforeMute) then FBeforeMute(self);
  if FMute=AValue then Exit;
  FMute:=AValue;
  if FBusy then
  begin
    if FMute then a:=0 else a:=FVolume;
    uos_InputSetDSPVolume(FDevIndex,InIndex,a/100,a/100,True);
  end;
  if Assigned(FAfterMute) then FAfterMute(self);
end;

procedure TUOSPlayer.SetVolume(AValue: integer);
begin
  if FVolume=AValue then Exit;
  FVolume:=AValue;
  if FVolume<0 then FVolume:=0;
  if FVolume>100 then FVolume:=100;
  if FBusy and (not FMute) then uos_InputSetDSPVolume(FDevIndex,InIndex,FVolume/100,FVolume/100,True);
end;

constructor TUOSPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  _init;
end;

destructor TUOSPlayer.Destroy;
begin
  if FBusy then Stop;
  inherited Destroy;
end;

procedure TUOSPlayer.Start;
var
  a: integer;
begin
  if Assigned(FBeforeStart) then FBeforeStart(Self);
  if not FDevEngine.Loaded then exit;
  if FBusy then exit;
  uos_CreatePlayer(FDevIndex);
  if FMute then a:=0 else a:=FVolume;
  if FMode=moPlay then
  begin
    (* PLAY *)
    InIndex:=uos_AddFromFile(FDevIndex,Pchar(FFileName));
    uos_AddIntoDevOut(FDevIndex,-1,-1,uos_InputGetSampleRate(FDevIndex,InIndex),uos_InputGetChannels(FDevIndex,InIndex),-1,-1,-1);
    uos_InputAddDSP1ChanTo2Chan(FDevIndex,InIndex);
    uos_InputAddDSPVolume(FDevIndex,InIndex,1,1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,a/100,a/100,True); /// Set volume
    uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex);  /////// everything is ready to play...
  end else begin
    (* RECORD *)
    uos_AddIntoFile(FDevIndex,Pchar(FFileName));
    outindex:=uos_AddIntoDevOut(FDevIndex);
    uos_outputsetenable(FDevIndex,outindex,FListenMic);
    InIndex:=uos_AddFromDevIn(FDevIndex);  /// add Input from mic into IN device with default parameters
    uos_InputAddDSPVolume(FDevIndex,InIndex, 1, 1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,a/100,a/100,True); /// Set volume
    uos_Play(FDevIndex);  /////// everything is ready to play...
  end;
  FBusy:=true;
  if Assigned(FAfterStart) then FAfterStart(Self);
end;

procedure TUOSPlayer.Stop;
begin
  if Assigned(FBeforeStop) then FBeforeStop(Self);
  if not FDevEngine.Loaded then exit;
  if not FBusy then exit;
  uos_Stop(FDevIndex);
  FBusy:=false;
  if Assigned(FOnStop) then FOnStop(self);
  if Assigned(FAfterStop) then FAfterStop(Self);
end;

procedure TUOSPlayer.Pause;
begin
  if not FBusy then exit;
  if FPause then exit;
  uos_Pause(FDevIndex);
  FPause:=true;
end;

procedure TUOSPlayer.Replay;
begin
  if not FBusy then exit;
  if not FPause then exit;
  uos_RePlay(FDevIndex);
  FPause:=false;
end;

function TUOSPlayer.GetStatus: integer;
begin
  result:=uos_GetStatus(FDevIndex);
end;

end.
