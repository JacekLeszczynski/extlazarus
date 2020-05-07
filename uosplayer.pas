unit UOSPlayer;

{$mode objfpc}{$H+}
{.$define SOUTH}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  UOSEngine;

type

  { TUOSPlayer }

  {$IFDEF SHOUT}
  TUOSPlayerMode = (moPlay, moRecord, moShout);
  {$ELSE}
  TUOSPlayerMode = (moPlay, moPlayLoop, moRecord, moInfo, moURL);
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
    FCalcLoop: boolean;
    FDevEngine: TUOSEngine;
    FFileName: string;
    FDevIndex: cardinal;
    //FIntoPlayer: TUOSPlayer;
    FListenMic: boolean;
    FMeter: boolean;
    FMode: TUOSPlayerMode;
    FMute: boolean;
    FOnLoop: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FPause,FPausing: boolean;
    FPauseing: boolean;
    FPosition: boolean;
    FPPS: boolean;
    FVolume: double;
    FVolumeGlobal: double;
    InIndex,OutIndex : integer;
    QPOS: TTime;
    QMEM: TMemoryStream;
    QFORCE,QFORCEEXIT,QFORCEFPPS: boolean;
    QTimer: TTimer;
    QVolume: double;
    QTT: integer;
    procedure SetMode(AValue: TUOSPlayerMode);
    procedure SetMute(AValue: boolean);
    function GetMixVolume: double;
    procedure SetVolume(AValue: double);
    procedure SetVolumeGlobal(AValue: double);
    procedure _init;
    procedure ClosePlayer;
    procedure LoopPlayer;
    procedure QPetla(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start(aMemoryStream: TMemoryStream = nil);
    procedure Stop(aForce: boolean = false);
    procedure Pause;
    procedure Replay;
    function GetStatus: integer;
    procedure GetMeter(var ALeft,ARight: single);
    procedure GetMeter(var ALeft,ARight: double);
    procedure GetMeter(var ALeft,ARight: integer);
    function GetLength: longword;
    function GetLengthSeconds: single;
    function GetLengthTime: TTime;
    function Position: longword;
    function PositionSeconds: single;
    function PositionTime: TTime;
    procedure Seek(ASample: longword);
    procedure SeekSeconds(ASeconds: single);
    procedure SeekTime(ATime: TTime);
    function GetTag(Filename: TFileName; var aTag: TIDTag): boolean;
    {$IFDEF SHOUT}
    procedure ServerSet(server: string; port: word; password: string);
    {$ENDIF}
  published
    property DeviceEngine: TUOSEngine read FDevEngine write FDevEngine; //Engine Component
    property Busy: boolean read FBusy default false; //Is Active Player
    property Paused: boolean read FPause default false; //Player zatrzymany
    property Pausing: boolean read FPauseing default false; //Player w trakcie zatrzymywania
    property FileName: string read FFileName write FFileName; //Plik lub adres URL
    //Tryb działania kontrolki
    //Przy moPlayLoop wymagany CalcPosition!
    property Mode: TUOSPlayerMode read FMode write SetMode;
    property ListenMic: boolean read FListenMic write FListenMic default false; //Mik idzie na głośniki gdy włączone
    property DeviceIndex: cardinal read FDevIndex write FDevIndex default 0; //Dla każdej kontrolki wartość unikatowa
    //property IntoDeviceIndex: TUOSPlayer read FIntoPlayer write FIntoPlayer;
    property Volume: double read FVolume write SetVolume; //Sterowanie głośnością
    property VolumeGlobal: double read FVolumeGlobal write SetVolumeGlobal; //Sterowanie głośnością wyższego poziomu
    property Mute: boolean read FMute write SetMute default false; //Nie wycisza głośników, wycisza nagrywanie!
    property CalcLoop: boolean read FCalcLoop write FCalcLoop default false; //Uruchamia wyzwalacz do sterowania wskaźnikami wysterowania czy pozycji
    property CalcMeter: boolean read FMeter write FMeter default false; //Uruchamia rejestrowanie wysterowania
    property CalcPosition: boolean read FPosition write FPosition default false; //Uruchamia rejestrowanie pozycji
    //Wyciszaj automatycznie zanim zatrzymasz utwór wewnątrz
    //i rób to samo, gdy wykonujesz Replay wewnątrz utworu!
    property AutoVolPPS: boolean read FPPS write FPPS default false;
    property BeforeStart: TNotifyEvent read FBeforeStart write FBeforeStart;
    property AfterStart: TNotifyEvent read FAfterStart write FAfterStart;
    property BeforeStop: TNotifyEvent read FBeforeStop write FBeforeStop;
    property AfterStop: TNotifyEvent read FAfterStop write FAfterStop;
    property BeforeMute: TNotifyEvent read FBeforeMute write FBeforeMute;
    property AfterMute: TNotifyEvent read FAfterMute write FAfterMute;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;  //Wyzwalacz do wyświetlania pozycji i poziomu sygnału
  end;

procedure Register;

implementation

uses
  uos_flat, math;

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
  FPauseing:=false;
  FDevIndex:=0;
  FMode:=moPlay;
  FListenMic:=false;
  FVolume:=1.0;
  FVolumeGlobal:=1.0;
  FCalcLoop:=false;
  FMute:=false;
  FMeter:=false;
  FPosition:=false;
  FPPS:=false;
end;

procedure TUOSPlayer.ClosePlayer;
begin
  if QFORCE then exit;
  FBusy:=false;
  FPause:=false;
  FPauseing:=false;
  if Assigned(FOnStop) then FOnStop(self);
end;

procedure TUOSPlayer.LoopPlayer;
begin
  if Assigned(FOnLoop) then FOnLoop(self);
end;

procedure TUOSPlayer.QPetla(Sender: TObject);
var
  bstop: boolean;
begin
  bstop:=false;
  if QTT=1 then
  begin
    QVolume:=QVolume+0.0005;
    if QVolume>=1 then
    begin
      QVolume:=1;
      bstop:=true;
    end;
    SetVolume(-1);
    if bstop then QTimer.Enabled:=false;
  end else begin
    QVolume:=QVolume-0.0005;
    if QVolume<=0 then
    begin
      QVolume:=0;
      bstop:=true;
    end;
    SetVolume(-1);
    if bstop then QTimer.Enabled:=false;
  end;
  if bstop then
  begin
    QFORCEFPPS:=QTT>1;
    case QTT of
      2: Pause;
      3: Stop;
    end;
    QTT:=0;
  end;
end;

procedure TUOSPlayer.SetMode(AValue: TUOSPlayerMode);
begin
  if FBusy or (FMode=AValue) then Exit;
  FMode:=AValue;
end;

procedure TUOSPlayer.SetMute(AValue: boolean);
var
  a,b: double;
begin
  if Assigned(FBeforeMute) then FBeforeMute(self);
  if FMute=AValue then Exit;
  FMute:=AValue;
  if FBusy then
  begin
    if FMode=moRecord then b:=FVolume else b:=GetMixVolume;
    if FMute then a:=0 else a:=b;
    uos_InputSetDSPVolume(FDevIndex,InIndex,a,a,True);
  end;
  if Assigned(FAfterMute) then FAfterMute(self);
end;

function TUOSPlayer.GetMixVolume: double;
begin
  result:=FVolume*FVolumeGlobal*QVolume;
end;

procedure TUOSPlayer.SetVolume(AValue: double);
var
  a: double;
begin
  if FVolume=AValue then Exit;
  if AValue>-0.5 then FVolume:=AValue;
  if FVolume<0 then FVolume:=0;
  if FVolume>1 then FVolume:=1;
  if FMode=moRecord then a:=FVolume else a:=GetMixVolume;
  if FBusy and (not FMute) then uos_InputSetDSPVolume(FDevIndex,InIndex,a,a,True);
end;

procedure TUOSPlayer.SetVolumeGlobal(AValue: double);
var
  a: double;
begin
  if FVolumeGlobal=AValue then Exit;
  if AValue>-0.5 then FVolumeGlobal:=AValue;
  if FVolumeGlobal<0 then FVolumeGlobal:=0;
  if FVolumeGlobal>1 then FVolumeGlobal:=1;
  if FMode=moRecord then a:=FVolume else a:=GetMixVolume;
  if FBusy and (not FMute) then uos_InputSetDSPVolume(FDevIndex,InIndex,a,a,True);
end;

constructor TUOSPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  _init;
  QTimer:=TTimer.Create(nil);
  QTimer.Enabled:=false;
  QTimer.Interval:=1;
  QTimer.OnTimer:=@QPetla;
end;

destructor TUOSPlayer.Destroy;
begin
  QFORCEEXIT:=true;
  if FBusy then Stop;
  QTimer.Free;
  inherited Destroy;
end;

procedure TUOSPlayer.Start(aMemoryStream: TMemoryStream);
var
  a: double;
begin
  if Assigned(FBeforeStart) then FBeforeStart(Self);
  if not FDevEngine.Loaded then exit;
  if ((FMode<>moPlayLoop) and FBusy) or ((FMode=moPlayLoop) and FBusy and (not FPause)) then exit;
  QFORCE:=false;
  QFORCEEXIT:=false;
  QFORCEFPPS:=false;
  uos_CreatePlayer(FDevIndex);
  if FMute then a:=0 else a:=GetMixVolume;
  if FMode=moPlay then       //moShout
  begin
    (* PLAY *)
    QVolume:=1;
    QTT:=0;
    if aMemoryStream=nil then InIndex:=uos_AddFromFile(FDevIndex,Pchar(FFileName))
    else InIndex:=uos_AddFromMemoryStream(FDevIndex,aMemoryStream,-1,-1,-1,-1);

    OutIndex:=uos_AddIntoDevOut(FDevIndex,-1,-1,uos_InputGetSampleRate(FDevIndex,InIndex),uos_InputGetChannels(FDevIndex,InIndex),-1,-1,-1);
    uos_InputAddDSP1ChanTo2Chan(FDevIndex,InIndex);
    uos_InputAddDSPVolume(FDevIndex,InIndex,1,1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,a,a,true); /// Set volume
    if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(FDevIndex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(FDevIndex,InIndex,@LoopPlayer);
    uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex);  /////// everything is ready to play...
  end else if FMode=moPlayLoop then
  begin
    (* PLAY LOOP *)
    if FBusy and FPPS then QVolume:=0 else QVolume:=1;
    QMEM:=aMemoryStream;
    if aMemoryStream=nil then InIndex:=uos_AddFromFile(FDevIndex,Pchar(FFileName))
    else InIndex:=uos_AddFromMemoryStream(FDevIndex,aMemoryStream,-1,-1,-1,-1);

    OutIndex:=uos_AddIntoDevOut(FDevIndex,-1,-1,uos_InputGetSampleRate(FDevIndex,InIndex),uos_InputGetChannels(FDevIndex,InIndex),-1,-1,-1);
    uos_InputAddDSP1ChanTo2Chan(FDevIndex,InIndex);
    uos_InputAddDSPVolume(FDevIndex,InIndex,1,1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,a,a,true); /// Set volume
    if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(FDevIndex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(FDevIndex,InIndex,@LoopPlayer);
    uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex,-1);  /////// everything is ready to play...
    if FPPS then
    begin
      SetVolume(-1);
      QTT:=1;
      QTimer.Enabled:=true;
    end;
  end else if FMode=moURL then
  begin
    (* PLAY FOR INTERNET STREAMING *)
    inindex:=uos_AddFromURL(FDevIndex,PChar(FFileName),-1,-1,-1,-1,true);
    //outindex:=uos_AddIntoDevOut(FDevIndex);
    outindex:=uos_AddIntoDevOut(FDevIndex,-1,-1,uos_InputGetSampleRate(FDevIndex,inindex),uos_InputGetChannels(FDevIndex,inindex),-1,1024,-1);
    uos_OutputAddDSPVolume(FDevIndex,outindex,1,1);
    uos_OutputSetDSPVolume(FDevIndex,outindex,a,a,true);
    //uos_AddIntoFile(FDevIndex, PChar('/home/tao/test.wav'));
    if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(FDevIndex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(FDevIndex,InIndex,@LoopPlayer);
    uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex);
  end else if FMode=moRecord then
  begin
    (* RECORD *)
    uos_AddIntoFile(FDevIndex,Pchar(FFileName));
    outindex:=uos_AddIntoDevOut(FDevIndex);
    uos_outputsetenable(FDevIndex,outindex,FListenMic);
    InIndex:=uos_AddFromDevIn(FDevIndex);  /// add Input from mic into IN device with default parameters
    uos_InputAddDSPVolume(FDevIndex,InIndex, 1, 1);
    uos_InputSetDSPVolume(FDevIndex,InIndex,FVolume,FVolume,True); /// Set volume
    if FMeter then uos_InputSetLevelEnable(FDevIndex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(FDevIndex,InIndex,@LoopPlayer);
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
    if FCalcLoop then uos_LoopProcIn(FDevIndex,InIndex,@LoopPlayer);
    uos_EndProc(FDevIndex,@ClosePlayer);
    uos_Play(FDevIndex);  /////// everything is ready to play...
    {$ENDIF}
  end;
  FBusy:=true;
  if Assigned(FAfterStart) then FAfterStart(Self);
end;

procedure TUOSPlayer.Stop(aForce: boolean);
begin
  if (not aForce) and (not FPause) and (not QFORCEEXIT) and (not QFORCEFPPS) and FPPS then
  begin
    QTT:=3;
    QTimer.Enabled:=true;
    exit;
  end;
  QTimer.Enabled:=false;
  if Assigned(FBeforeStop) then FBeforeStop(Self);
  if not FDevEngine.Loaded then exit;
  if not FBusy then exit;
  if FMode=moInfo then
  begin
    uos_FreePlayer(FDevIndex);
  end else
  if FMode=moPlayLoop then
  begin
    QMEM:=nil;
    QPOS:=0;
    QFORCE:=false;
    if not FPause then uos_Stop(FDevIndex);
  end else begin
    uos_Stop(FDevIndex);
  end;
  FBusy:=false;
  if Assigned(FOnStop) then FOnStop(self);
  if Assigned(FAfterStop) then FAfterStop(Self);
  QFORCEFPPS:=false;
end;

procedure TUOSPlayer.Pause;
begin
  FPauseing:=true;
  if not FBusy then exit;
  if FPause then exit;
  if (not QFORCEFPPS) and FPPS then
  begin
    QTT:=2;
    QTimer.Enabled:=true;
    exit;
  end;
  QTimer.Enabled:=false;
  if FMode=moPlayLoop then
  begin
    QPOS:=PositionTime;
    QFORCE:=true;
    uos_Stop(FDevIndex);
  end else uos_Pause(FDevIndex);
  FPause:=true;
  FPauseing:=true;
  QFORCEFPPS:=false;
end;

procedure TUOSPlayer.Replay;
begin
  if not FPauseing then exit;
  QTT:=1;
  FPauseing:=false;
  if not FBusy then exit;
  if not FPause then exit;
  if FMode=moPlayLoop then
  begin
    QFORCE:=false;
    QTT:=1;
    if not QTimer.Enabled then
    begin
      Start(QMEM);
      SeekTime(QPOS);
    end;
  end else begin
    if FPPS then
    begin
      SetVolume(-1);
      QTT:=1;
      QTimer.Enabled:=true;
    end;
    uos_RePlay(FDevIndex);
  end;
  FPause:=false;
  FPauseing:=false;
end;

function TUOSPlayer.GetStatus: integer;
begin
  result:=uos_GetStatus(FDevIndex);
end;

procedure TUOSPlayer.GetMeter(var ALeft, ARight: single);
begin
  if FMode=moRecord then
  begin
    ALeft:=uos_InputGetLevelLeft(FDevIndex,InIndex);
    ARight:=uos_InputGetLevelRight(FDevIndex,InIndex);
  end else begin
    ALeft:=uos_InputGetLevelLeft(FDevIndex,OutIndex)*FVolume;
    ARight:=uos_InputGetLevelRight(FDevIndex,OutIndex)*FVolume;
  end;
end;

procedure TUOSPlayer.GetMeter(var ALeft, ARight: double);
begin
  if FMode=moRecord then
  begin
    ALeft:=uos_InputGetLevelLeft(FDevIndex,InIndex);
    ARight:=uos_InputGetLevelRight(FDevIndex,InIndex);
  end else begin
    ALeft:=uos_InputGetLevelLeft(FDevIndex,OutIndex)*FVolume;
    ARight:=uos_InputGetLevelRight(FDevIndex,OutIndex)*FVolume;
  end;
end;

procedure TUOSPlayer.GetMeter(var ALeft, ARight: integer);
begin
  if FMode=moRecord then
  begin
    ALeft:=round(uos_InputGetLevelLeft(FDevIndex,InIndex)*105);
    ARight:=round(uos_InputGetLevelRight(FDevIndex,InIndex)*105);
  end else begin
    ALeft:=round(uos_InputGetLevelLeft(FDevIndex,OutIndex)*100*FVolume);
    ARight:=round(uos_InputGetLevelRight(FDevIndex,OutIndex)*100*FVolume);
  end;
end;

function TUOSPlayer.GetLength: longword;
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

function TUOSPlayer.Position: longword;
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

procedure TUOSPlayer.Seek(ASample: longword);
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
    InIndex:=uos_AddFromFile(FDevIndex,Pchar(ExtractShortPathName(Filename)));
    ext:=upcase(ExtractFileExt(Filename));
    s:=strpas(uos_InputGetTagTag(FDevIndex,InIndex));
    setlength(s,3);
    if ((ext='.MP3') and (s='TAG')) or (ext<>'.MP3') then
    begin
      aTag.Title:=strpas(uos_InputGetTagTitle(FDevIndex,InIndex));
      aTag.Artist:=strpas(uos_InputGetTagArtist(FDevIndex,InIndex));
      aTag.Album:=strpas(uos_InputGetTagAlbum(FDevIndex,InIndex));
      if trim(aTag.Title)='' then result:=false else result:=true;
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
