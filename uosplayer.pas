unit UOSPlayer;

{$mode objfpc}{$H+}
{.$define SOUTH}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, UOSEngine;

type

  { TUOSPlayer }

  {$IFDEF SHOUT}
  TUOSPlayerMode = (moPlay, moRecord, moShout);
  {$ELSE}
  TUOSPlayerMode = (moPlay, moRecord, moInfo, moURL);
  {$ENDIF}

  TIDTag = record
    Title,Artist,Album: string[30];
    Year: string[4];
    Comment: string[30];
    Genre: Byte;
  end;

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
    //FIntoPlayer: TUOSPlayer;
    FListenMic: boolean;
    FMeter: boolean;
    FMode: TUOSPlayerMode;
    FMute: boolean;
    FOnStop: TNotifyEvent;
    FPause: boolean;
    FPosition: boolean;
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
    procedure GetMeter(var ALeft,ARight: double);
    procedure GetMeter(var ALeft,ARight: integer);
    procedure SetVolume(AVolume: double);
    function GetLength: longint;
    function GetLengthSeconds: single;
    function GetLengthTime: TTime;
    function Position: longint;
    function PositionSeconds: single;
    function PositionTime: TTime;
    procedure Seek(ASample: longint);
    procedure SeekSeconds(ASeconds: single);
    procedure SeekTime(ATime: TTime);
    function GetTag(Filename: TFileName; var aTag: TIDTag): boolean;
    {$IFDEF SHOUT}
    procedure ServerSet(server: string; port: word; password: string);
    {$ENDIF}
  published
    property DeviceEngine: TUOSEngine read FDevEngine write FDevEngine;
    property Busy: boolean read FBusy default false;
    property Paused: boolean read FPause default false;
    property FileName: string read FFileName write FFileName;
    property Mode: TUOSPlayerMode read FMode write SetMode;
    property ListenMic: boolean read FListenMic write FListenMic default false;
    property DeviceIndex: cardinal read FDevIndex write FDevIndex default 0;
    //property IntoDeviceIndex: TUOSPlayer read FIntoPlayer write FIntoPlayer;
    property Volume: integer read FVolume write SetVolume default 100;
    property Mute: boolean read FMute write SetMute default false; //Nie wycisza głośników, wycisza nagrywanie!
    property CalcMeter: boolean read FMeter write FMeter default false;
    property CalcPosition: boolean read FPosition write FPosition default false;
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

var
  server_data: record
    server: string;
    port: word;
    password: string;
  end;

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
  FMeter:=false;
  FPosition:=false;
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
  if FMode=moPlay then       //moShout
  begin
    (* PLAY *)
    InIndex:=uos_AddFromFile(FDevIndex,Pchar(FFileName));
    uos_AddIntoDevOut(FDevIndex,-1,-1,uos_InputGetSampleRate(FDevIndex,InIndex),uos_InputGetChannels(FDevIndex,InIndex),-1,-1,-1);
    uos_InputAddDSP1ChanTo2Chan(FDevIndex,InIndex);
    uos_InputAddDSPVolume(FDevIndex,InIndex,1,1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,a/100,a/100,true); /// Set volume
    if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(FDevIndex,InIndex,1);
    uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex);  /////// everything is ready to play...
  end else if FMode=moURL then
  begin
    (* PLAY FOR INTERNET STREAMING *)
    inindex:=uos_AddFromURL(FDevIndex,PChar(FFileName),-1,-1,-1,-1,true);
    //outindex:=uos_AddIntoDevOut(FDevIndex);
    outindex:=uos_AddIntoDevOut(FDevIndex,-1,-1,uos_InputGetSampleRate(FDevIndex,inindex),uos_InputGetChannels(FDevIndex,inindex),-1,1024,-1);
    uos_OutputAddDSPVolume(FDevIndex,outindex,1,1);
    uos_OutputSetDSPVolume(FDevIndex,outindex,a/100,a/100,true);
    //uos_AddIntoFile(FDevIndex, PChar('/home/tao/test.wav'));
    //if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    //if FPosition then uos_InputSetPositionEnable(FDevIndex,InIndex,1);
    //uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex);
  end else if FMode=moRecord then
  begin
    (* RECORD *)
    uos_AddIntoFile(FDevIndex,Pchar(FFileName));
    outindex:=uos_AddIntoDevOut(FDevIndex);
    uos_outputsetenable(FDevIndex,outindex,FListenMic);
    InIndex:=uos_AddFromDevIn(FDevIndex);  /// add Input from mic into IN device with default parameters
    uos_InputAddDSPVolume(FDevIndex,InIndex, 1, 1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,a/100,a/100,True); /// Set volume
    if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    uos_Play(FDevIndex);  /////// everything is ready to play...
  end else if FMode=moInfo then
  begin
    (* INFO *)
    (* Tu nic nie wykonuję ... *)
  end else begin
    {$IFDEF SHOUT}
    (* SHOUT *)
    uos_AddIntoIceServer(FDevIndex,44100,2,0,0,server_data.port,pchar(server_data.server),nil,pchar(server_data.password),nil);
    uos_InputAddDSP1ChanTo2Chan(FDevIndex,InIndex);
    uos_InputAddDSPVolume(FDevIndex,InIndex,1,1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,a/100,a/100,True); /// Set volume
    if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(FDevIndex,InIndex,1);
    uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex);  /////// everything is ready to play...
    {$ENDIF}
  end;
  FBusy:=true;
  if Assigned(FAfterStart) then FAfterStart(Self);
end;

procedure TUOSPlayer.Stop;
begin
  if Assigned(FBeforeStop) then FBeforeStop(Self);
  if not FDevEngine.Loaded then exit;
  if not FBusy then exit;
  if FMode=moInfo then
  begin
    uos_FreePlayer(FDevIndex);
  end else begin
    uos_Stop(FDevIndex);
  end;
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

procedure TUOSPlayer.GetMeter(var ALeft, ARight: double);
begin
  if FMode=moRecord then
  begin
    ALeft:=uos_InputGetLevelLeft(FDevIndex,InIndex);
    ARight:=uos_InputGetLevelRight(FDevIndex,InIndex);
  end else begin
    ALeft:=uos_InputGetLevelLeft(FDevIndex,OutIndex)*(FVolume/100);
    ARight:=uos_InputGetLevelRight(FDevIndex,OutIndex)*(FVolume/100);
  end;
end;

procedure TUOSPlayer.GetMeter(var ALeft, ARight: integer);
begin
  if FMode=moRecord then
  begin
    ALeft:=round(uos_InputGetLevelLeft(FDevIndex,InIndex)*105);
    ARight:=round(uos_InputGetLevelRight(FDevIndex,InIndex)*105);
  end else begin
    ALeft:=round(uos_InputGetLevelLeft(FDevIndex,OutIndex)*100*(FVolume/100));
    ARight:=round(uos_InputGetLevelRight(FDevIndex,OutIndex)*100*(FVolume/100));
  end;
end;

procedure TUOSPlayer.SetVolume(AVolume: double);
begin
  FVolume:=Round(AVolume*100);
  if FVolume<0 then FVolume:=0;
  if FVolume>100 then FVolume:=100;
  if FBusy and (not FMute) then uos_InputSetDSPVolume(FDevIndex,InIndex,AVolume,AVolume,True);
end;

function TUOSPlayer.GetLength: longint;
begin
  result:=uos_InputLength(FDevIndex,InIndex);
end;

function TUOSPlayer.GetLengthSeconds: single;
begin
  result:=uos_InputLengthSeconds(FDevIndex,InIndex);
end;

function TUOSPlayer.GetLengthTime: TTime;
begin
  result:=uos_InputLengthTime(FDevIndex,InIndex);
end;

function TUOSPlayer.Position: longint;
begin
  result:=uos_InputPosition(FDevIndex,InIndex);
end;

function TUOSPlayer.PositionSeconds: single;
begin
  result:=uos_InputPositionSeconds(FDevIndex,InIndex);
end;

function TUOSPlayer.PositionTime: TTime;
begin
  result:=uos_InputPositionTime(FDevIndex,InIndex);
end;

procedure TUOSPlayer.Seek(ASample: longint);
begin
  uos_InputSeek(FDevIndex,InIndex,ASample);
end;

procedure TUOSPlayer.SeekSeconds(ASeconds: single);
begin
  uos_InputSeekSeconds(FDevIndex,InIndex,ASeconds);
end;

procedure TUOSPlayer.SeekTime(ATime: TTime);
begin
  uos_InputSeekTime(FDevIndex,InIndex,ATime);
end;

function TUOSPlayer.GetTag(Filename: TFileName; var aTag: TIDTag): boolean;
var
  s: string;
  ext: string;
begin
  if (FMode=moInfo) and FBusy then
  begin
    InIndex:=uos_AddFromFile(FDevIndex,Pchar(Filename));
    ext:=upcase(ExtractFileExt(Filename));
    s:=strpas(uos_InputGetTagTag(FDevIndex,InIndex));
    setlength(s,3);
    writeln(s);
    if ((ext='.MP3') and (s='TAG')) or (ext<>'.MP3') then
    begin
      aTag.Title:=strpas(uos_InputGetTagTitle(FDevIndex,InIndex));
      aTag.Artist:=strpas(uos_InputGetTagArtist(FDevIndex,InIndex));
      aTag.Album:=strpas(uos_InputGetTagAlbum(FDevIndex,InIndex));
      result:=true;
    end else result:=false;
  end else result:=false;
end;

{$IFDEF SHOUT}
procedure TUOSPlayer.ServerSet(server: string; port: word; password: string);
begin
  server_data.server:=server;
  server_data.port:=port;
  server_data.password:=password;
end;
{$ENDIF}

end.
