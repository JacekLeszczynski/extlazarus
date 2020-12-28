unit UOSPlayer;

{$mode objfpc}{$H+}
{.$define SOUTH}
{$DEFINE CRQ}

interface

uses
  Classes, SysUtils, LResources, ExtCtrls, UOSEngine;

type

  { TUOSPlayer }

  {$IFDEF SHOUT}
  TUOSPlayerMode = (moPlay, moRecord, moShout);
  {$ELSE}
  TUOSPlayerMode = (moPlay, moPlayLoop, moRecord, moInfo, moURL);
  {$ENDIF}

  TUOSPlauerOnStop = procedure(Sender: TObject; aBusy,aPlaying,aPauseing,aPause: boolean) of object;
  TUOSPlauerOnNull = procedure of object;

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
    FNoFreeOnStop: boolean;
    FOnLoop: TNotifyEvent;
    FOnStop: TUOSPlauerOnStop;
    FPause,FPausing: boolean;
    FPauseing: boolean;
    FPlayRawMode: boolean;
    FPosition: boolean;
    FPPS: boolean;
    FProcessMessage: TUOSPlauerOnNull;
    FSleepForPlay: integer;
    FVolume: double;
    FVolumeGlobal: double;
    InIndex,OutIndex : integer;
    QPOS: TTime;
    QMEM: TMemoryStream;
    QFORCE,QFORCEEXIT,QFORCEFPPS: boolean;
    QTimer: TTimer;
    QVolume: double;
    QTT: integer;
    xindex: integer;
    FStarting: boolean;
    procedure SetDevIndex(AValue: cardinal);
    procedure SetMode(AValue: TUOSPlayerMode);
    procedure SetMute(AValue: boolean);
    function GetMixVolume: double;
    procedure SetVolume(AValue: double);
    procedure SetVolumeGlobal(AValue: double);
    procedure _init;
    procedure ClosePlayer;
    procedure LoopPlayer;
    procedure QPetla(Sender: TObject);
    procedure wewnStart(aMemoryStream: TMemoryStream = nil);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start(aMemoryStream: TMemoryStream = nil);
    function Starting: boolean;
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
    property DeviceIndex: cardinal read FDevIndex write SetDevIndex default 0; //Dla każdej kontrolki wartość unikalna
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
    //Nie zwalniaj strumienia po zatrzymaniu
    property NoFreeOnStop: boolean read FNoFreeOnStop write FNoFreeOnStop default false;
    //Odtwarzaj strumienie w locie
    property PlayRawMode: boolean read FPlayRawMode write FPlayRawMode default false;
    //Opóźnienie startu odtwarzania
    //Wymaga metody OnProcessMessage!
    property SleepForPlay: integer read FSleepForPlay write FSleepForPlay default 0;
    property BeforeStart: TNotifyEvent read FBeforeStart write FBeforeStart;
    property AfterStart: TNotifyEvent read FAfterStart write FAfterStart;
    property BeforeStop: TNotifyEvent read FBeforeStop write FBeforeStop;
    property AfterStop: TNotifyEvent read FAfterStop write FAfterStop;
    property BeforeMute: TNotifyEvent read FBeforeMute write FBeforeMute;
    property AfterMute: TNotifyEvent read FAfterMute write FAfterMute;
    property OnStop: TUOSPlauerOnStop read FOnStop write FOnStop;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;  //Wyzwalacz do wyświetlania pozycji i poziomu sygnału
    //Wykonaj Application.ProcessMessage
    // - jeśli potrzebne.
    property OnProcessMessage: TUOSPlauerOnNull read FProcessMessage write FProcessMessage;
  end;

procedure Register;

implementation

uses
  uos, uos_flat, math;

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
  FStarting:=false;
  FBusy:=false;
  FPause:=false;
  FPauseing:=false;
  FNoFreeOnStop:=false;
  FPlayRawMode:=false;
  FSleepForPlay:=0;
  FDevIndex:=0;
  xindex:=0;
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
var
  a: integer;
begin
  if QFORCE then exit;
  a:=uos_GetStatus(xindex);
  FBusy:=a>0;
  FPause:=a=2;
  FPauseing:=a=2;
  if Assigned(FOnStop) then FOnStop(self,FBusy,not FPause,FPauseing,FPause);
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

procedure TUOSPlayer.wewnStart(aMemoryStream: TMemoryStream);
var
  a: double;
  x: TMemoryStream;
  Bufferinfos: Tuos_bufferinfos;
  cc: integer;
begin
  if not FDevEngine.Loaded then exit;
  if ((FMode<>moPlayLoop) and FBusy) or ((FMode=moPlayLoop) and FBusy and (not FPause)) then exit;
  QFORCE:=false;
  QFORCEEXIT:=false;
  QFORCEFPPS:=false;
  FStarting:=true;

  if FSleepForPlay>0 then
  begin
    for cc:=1 to FSleepForPlay*100 do
    begin
      sleep(10);
      if assigned(FProcessMessage) then FProcessMessage;
    end;
  end;

  uos_CreatePlayer(xindex);
  if FMode=moPlay then       //moShout
  begin
    (* PLAY *)
    QVolume:=1;
    if FMute then a:=0 else a:=GetMixVolume;
    QTT:=0;

    if aMemoryStream=nil then InIndex:=uos_AddFromFile(xindex,Pchar(FFileName)) else
    if FPlayRawMode then
    begin
      uos_CustBufferInfos(Bufferinfos,44100,2,2,-1);
      InIndex:=uos_AddFromMemoryStreamDec(xindex,aMemoryStream,Bufferinfos,-1,-1);
    end else InIndex:=uos_AddFromMemoryStream(xindex,aMemoryStream,-1,-1,-1,-1);

    OutIndex:=uos_AddIntoDevOut(xindex,-1,-1,uos_InputGetSampleRate(xindex,InIndex),uos_InputGetChannels(xindex,InIndex),-1,-1,-1);
    uos_InputAddDSP1ChanTo2Chan(xindex,InIndex);
    uos_InputAddDSPVolume(xindex,InIndex,1,1);
    uos_InputSetDSPVolume(xindex,InIndex,a,a,true); /// Set volume
    if FMeter then uos_InputSetLevelEnable(xindex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(xindex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(xindex,InIndex,@LoopPlayer);
    uos_EndProc(xindex,@ClosePlayer);

    if FNoFreeOnStop then uos_PlayNoFree(xindex) else uos_Play(xindex);  /////// everything is ready to play...
  end else if FMode=moPlayLoop then
  begin
    (* PLAY LOOP *)
    if FBusy and FPPS then QVolume:=0 else QVolume:=1;
    if FMute then a:=0 else a:=GetMixVolume;

    if aMemoryStream=nil then
    begin
      {$IFDEF CRQ}
      if QMEM.Size>0 then
      begin
        x:=TMemoryStream.Create;
        x.LoadFromStream(QMEM);
        aMemoryStream:=x;
      end;
      {$ELSE}
      if QMEM<>nil then
      begin
        x:=TMemoryStream.Create;
        x.LoadFromStream(QMEM);
        aMemoryStream:=x;
      end;
      {$ENDIF}
    end else begin
      {$IFDEF CRQ}
      QMEM.LoadFromStream(aMemoryStream);
      {$ELSE}
      QMEM:=TMemoryStream.Create;
      QMEM.LoadFromStream(aMemoryStream);
      {$ENDIF}
    end;

    if aMemoryStream=nil then InIndex:=uos_AddFromFile(xindex,Pchar(FFileName))
    else InIndex:=uos_AddFromMemoryStream(xindex,aMemoryStream,-1,-1,-1,-1);

    OutIndex:=uos_AddIntoDevOut(xindex,-1,-1,uos_InputGetSampleRate(xindex,InIndex),uos_InputGetChannels(xindex,InIndex),-1,-1,-1);
    uos_InputAddDSP1ChanTo2Chan(xindex,InIndex);
    uos_InputAddDSPVolume(xindex,InIndex,1,1);
    uos_InputSetDSPVolume(xindex,InIndex,a,a,true); /// Set volume
    if FMeter then uos_InputSetLevelEnable(xindex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(xindex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(xindex,InIndex,@LoopPlayer);
    uos_EndProc(xindex,@ClosePlayer);
    if FNoFreeOnStop then uos_PlayNoFree(xindex,-1) else uos_Play(xindex,-1);  /////// everything is ready to play...
    if FPPS then
    begin
//      SetVolume(-1);
      QTT:=1;
      QTimer.Enabled:=true;
    end;
  end else if FMode=moURL then
  begin
    (* PLAY FOR INTERNET STREAMING *)
    QVolume:=1;
    if FMute then a:=0 else a:=GetMixVolume;
    inindex:=uos_AddFromURL(xindex,PChar(FFileName),-1,-1,-1,-1,true);
    //outindex:=uos_AddIntoDevOut(xindex);
    outindex:=uos_AddIntoDevOut(xindex,-1,-1,uos_InputGetSampleRate(xindex,inindex),uos_InputGetChannels(xindex,inindex),-1,1024,-1);
    uos_OutputAddDSPVolume(xindex,outindex,1,1);
    uos_OutputSetDSPVolume(xindex,outindex,a,a,true);
    //uos_AddIntoFile(xindex, PChar('/home/tao/test.wav'));
    if FMeter then uos_InputSetLevelEnable(xindex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(xindex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(xindex,InIndex,@LoopPlayer);
    uos_EndProc(xindex,@ClosePlayer);
    if FNoFreeOnStop then uos_PlayNoFree(xindex) else uos_Play(xindex);
  end else if FMode=moRecord then
  begin
    (* RECORD *)
    QVolume:=1;
    if FMute then a:=0 else a:=GetMixVolume;

    if aMemoryStream=nil then uos_AddIntoFile(xindex,Pchar(FFileName))
    else uos_AddIntoMemoryStream(xindex,aMemoryStream,-1,-1,-1,-1);
    {function uos_AddIntoMemoryStream(PlayerIndex: cint32; MemoryStream: TMemoryStream; SampleRate: LongInt;
           SampleFormat: LongInt ; Channels: LongInt; FramesCount: LongInt): LongInt;
     Add a Output into TMemoryStream
     Parameters:
       2. MemoryStream : the TMemoryStream to use to store memory.
       3. SampleRate : delault : -1 (44100)
       4. SampleFormat : default : -1 (2:Int16) ( 1:Int32, 2:Int16)
       5. Channels : delault : -1 (2:stereo) (0: no channels, 1:mono, 2:stereo, ...)
       6. FramesCount : default : -1 (= 4096)
    }

    outindex:=uos_AddIntoDevOut(xindex);
    uos_outputsetenable(xindex,outindex,FListenMic);
    InIndex:=uos_AddFromDevIn(xindex);  /// add Input from mic into IN device with default parameters
    uos_InputAddDSPVolume(xindex,InIndex, 1, 1);
    uos_InputSetDSPVolume(xindex,InIndex,FVolume,FVolume,True); /// Set volume
    if FMeter then uos_InputSetLevelEnable(xindex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(xindex,InIndex,@LoopPlayer);
    if FNoFreeOnStop then uos_PlayNoFree(xindex) else uos_Play(xindex);  /////// everything is ready to play...
  end else if FMode=moInfo then
  begin
    (* INFO *)
    (* Tu nic nie wykonuję ... *)
  end else begin
    {$IFDEF SHOUT}
    (* SHOUT *)
    QVolume:=1;
    if FMute then a:=0 else a:=GetMixVolume;
    uos_AddIntoIceServer(xindex,44100,2,0,0,server_data.port,pchar(server_data.server),nil,pchar(server_data.password),nil);
    uos_InputAddDSP1ChanTo2Chan(xindex,InIndex);
    uos_InputAddDSPVolume(xindex,InIndex,1,1);
    uos_InputSetDSPVolume(xindex,InIndex,a/100,a/100,True); /// Set volume
    if FMeter then uos_InputSetLevelEnable(xindex,InIndex,1);
    if FPosition then uos_InputSetPositionEnable(xindex,InIndex,1);
    if FCalcLoop then uos_LoopProcIn(xindex,InIndex,@LoopPlayer);
    uos_EndProc(xindex,@ClosePlayer);
    uos_Play(xindex);  /////// everything is ready to play...
    {$ENDIF}
  end;
  FBusy:=true;
  FPause:=false;
  FPauseing:=false;
  FStarting:=false;
end;

procedure TUOSPlayer.SetMode(AValue: TUOSPlayerMode);
begin
  if FBusy or (FMode=AValue) then Exit;
  FMode:=AValue;
end;

procedure TUOSPlayer.SetDevIndex(AValue: cardinal);
begin
  if FDevIndex=AValue then Exit;
  FDevIndex:=AValue;
  xindex:=FDevIndex;
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
    uos_InputSetDSPVolume(xindex,InIndex,a,a,True);
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
  if FBusy and (not FMute) then uos_InputSetDSPVolume(xindex,InIndex,a,a,True);
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
  if FBusy and (not FMute) then uos_InputSetDSPVolume(xindex,InIndex,a,a,True);
end;

constructor TUOSPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF CRQ}
  QMEM:=TMemoryStream.Create;
  {$ELSE}
  QMEM:=nil;
  {$ENDIF}
  _init;
  QTimer:=TTimer.Create(nil);
  QTimer.Enabled:=false;
  QTimer.Interval:=1;
  QTimer.OnTimer:=@QPetla;
end;

destructor TUOSPlayer.Destroy;
begin
  {$IFDEF CRQ}
  QMEM.Free;
  {$ENDIF}
  QFORCEEXIT:=true;
  if FBusy then Stop;
  QTimer.Free;
  inherited Destroy;
end;

procedure TUOSPlayer.Start(aMemoryStream: TMemoryStream);
begin
  if FBusy then exit;
  if Assigned(FBeforeStart) then FBeforeStart(Self);
  if FMode=moPlayLoop then
  begin
    {$IFDEF CRQ}
    QMEM.Clear;
    {$ELSE}
    if QMEM<>nil then
    begin
      QMEM.Free;
      QMEM:=nil;
    end;
    {$ENDIF}
  end;
  wewnStart(aMemoryStream);
  if Assigned(FAfterStart) then FAfterStart(Self);
end;

function TUOSPlayer.Starting: boolean;
begin
  result:=FStarting;
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
    uos_FreePlayer(xindex);
  end else
  if FMode=moPlayLoop then
  begin
    {$IFDEF CRQ}
    QMEM.Clear;
    {$ELSE}
    if QMEM<>nil then
    begin
      QMEM.Free;
      QMEM:=nil;
    end;
    {$ENDIF}
    QPOS:=0;
    QFORCE:=false;
    if not FPause then uos_Stop(xindex);
  end else begin
    uos_Stop(xindex);
  end;
  FBusy:=false;
  if xindex<100 then inc(xindex,100) else dec(xindex,100);
  if Assigned(FOnStop) then FOnStop(self,FBusy,not FPause,FPauseing,FPause);
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
    uos_Stop(xindex);
  end else uos_Pause(xindex);
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
      wewnStart;
      SeekTime(QPOS);
    end;
  end else begin
    if FPPS then
    begin
      SetVolume(-1);
      QTT:=1;
      QTimer.Enabled:=true;
    end;
    uos_RePlay(xindex);
  end;
  FPause:=false;
  FPauseing:=false;
end;

function TUOSPlayer.GetStatus: integer;
begin
  result:=uos_GetStatus(xindex);
end;

procedure TUOSPlayer.GetMeter(var ALeft, ARight: single);
begin
  if FMode=moRecord then
  begin
    ALeft:=uos_InputGetLevelLeft(xindex,InIndex);
    ARight:=uos_InputGetLevelRight(xindex,InIndex);
  end else begin
    ALeft:=uos_InputGetLevelLeft(xindex,OutIndex)*FVolume;
    ARight:=uos_InputGetLevelRight(xindex,OutIndex)*FVolume;
  end;
end;

procedure TUOSPlayer.GetMeter(var ALeft, ARight: double);
begin
  if FMode=moRecord then
  begin
    ALeft:=uos_InputGetLevelLeft(xindex,InIndex);
    ARight:=uos_InputGetLevelRight(xindex,InIndex);
  end else begin
    ALeft:=uos_InputGetLevelLeft(xindex,OutIndex)*FVolume;
    ARight:=uos_InputGetLevelRight(xindex,OutIndex)*FVolume;
  end;
end;

procedure TUOSPlayer.GetMeter(var ALeft, ARight: integer);
begin
  if FMode=moRecord then
  begin
    ALeft:=round(uos_InputGetLevelLeft(xindex,InIndex)*105);
    ARight:=round(uos_InputGetLevelRight(xindex,InIndex)*105);
  end else begin
    ALeft:=round(uos_InputGetLevelLeft(xindex,OutIndex)*100*FVolume);
    ARight:=round(uos_InputGetLevelRight(xindex,OutIndex)*100*FVolume);
  end;
end;

function TUOSPlayer.GetLength: longword;
begin
  result:=uos_InputLength(xindex,InIndex);
end;

function TUOSPlayer.GetLengthSeconds: single;
begin
  result:=uos_InputLengthSeconds(xindex,InIndex);
end;

function TUOSPlayer.GetLengthTime: TTime;
begin
  result:=uos_InputLengthTime(xindex,InIndex);
end;

function TUOSPlayer.Position: longword;
begin
  result:=uos_InputPosition(xindex,InIndex);
end;

function TUOSPlayer.PositionSeconds: single;
begin
  result:=uos_InputPositionSeconds(xindex,InIndex);
end;

function TUOSPlayer.PositionTime: TTime;
begin
  result:=uos_InputPositionTime(xindex,InIndex);
end;

procedure TUOSPlayer.Seek(ASample: longword);
begin
  uos_InputSeek(xindex,InIndex,ASample);
end;

procedure TUOSPlayer.SeekSeconds(ASeconds: single);
begin
  uos_InputSeekSeconds(xindex,InIndex,ASeconds);
end;

procedure TUOSPlayer.SeekTime(ATime: TTime);
begin
  uos_InputSeekTime(xindex,InIndex,ATime);
end;

function TUOSPlayer.GetTag(Filename: TFileName; var aTag: TIDTag): boolean;
var
  s: string;
  ext: string;
begin
  if (FMode=moInfo) and FBusy then
  begin
    InIndex:=uos_AddFromFile(xindex,Pchar(ExtractShortPathName(Filename)));
    ext:=upcase(ExtractFileExt(Filename));
    s:=strpas(uos_InputGetTagTag(xindex,InIndex));
    setlength(s,3);
    if ((ext='.MP3') and (s='TAG')) or (ext<>'.MP3') then
    begin
      aTag.Title:=strpas(uos_InputGetTagTitle(xindex,InIndex));
      aTag.Artist:=strpas(uos_InputGetTagArtist(xindex,InIndex));
      aTag.Album:=strpas(uos_InputGetTagAlbum(xindex,InIndex));
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
