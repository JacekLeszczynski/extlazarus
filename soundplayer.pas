unit SoundPlayer;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Classes, SysUtils, LResources, FileUtil, mmsystem;
  {$ELSE}
  Classes, SysUtils, LResources, FileUtil, AsyncProcess, Process;
  {$ENDIF}

type
  TProcessOption = (poAsync,poSync);
  TSoundPlayerMode = (smPlay,smRecord);

  { TSoundPlayer }

  TSoundPlayer = class(TComponent)
  private
    FAfterStart: TNotifyEvent;
    FAfterStop: TNotifyEvent;
    FBeforeStart: TNotifyEvent;
    FBeforeStop: TNotifyEvent;
    {$IFNDEF WINDOWS}
    SoundPlayerAsyncProcess: Tasyncprocess;
    SoundPlayerSyncProcess: Tprocess;
    {$ENDIF}
    FFileName: string;
    FMode: TSoundPlayerMode;
    FPlayCommand,DPlayCommand: string;
    FPO: TProcessOption;
    FRecCommand,DRecCommand: string;
    procedure SetPlayCommand(AValue: string);
    procedure SetRecCommand(AValue: string);
  protected
    procedure PlaySound(const plik: string); virtual;
    procedure RecordSound(const plik: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function Busy: boolean;
  published
    property FileName: string read FFileName write FFileName;
    property ProcessOption: TProcessOption read FPO write FPO default poASync;
    property Mode: TSoundPlayerMode read FMode write FMode default smPlay;
    property PlayCommand: string read FPlayCommand write SetPlayCommand;
    property RecordCommand: string read FRecCommand write SetRecCommand;
    property BeforeStart: TNotifyEvent read FBeforeStart write FBeforeStart;
    property AfterStart: TNotifyEvent read FAfterStart write FAfterStart;
    property BeforeStop: TNotifyEvent read FBeforeStop write FBeforeStop;
    property AfterStop: TNotifyEvent read FAfterStop write FAfterStop;
  end;

procedure Register;

implementation

uses
  LazFileUtils;

procedure Register;
begin
  {$I soundplayer_icon.lrs}
  RegisterComponents('Multimedia',[TSoundPlayer]);
end;

function GetNonWindowsPlayCommand:String;
begin
  if FindDefaultExecutablePath('play')<>'' then result:='play' else
  if FindDefaultExecutablePath('aplay')<>'' then result:='aplay -q' else
  if FindDefaultExecutablePath('paplay')<>'' then result:='paplay' else
  if FindDefaultExecutablePath('mplayer')<>'' then result:='mplayer -really-quiet' else
  if FindDefaultExecutablePath('CMus')<>'' then result:='CMus' else
  if FindDefaultExecutablePath('pacat')<>'' then result:='pacat -p' else
  if FindDefaultExecutablePath('ffplay')<>'' then result:='ffplay -autoexit -nodisp' else
  if FindDefaultExecutablePath('cvlc')<>'' then result:='cvlc -q --play-and-exit' else
  if FindDefaultExecutablePath('canberra-gtk-play')<>'' then result:='canberra-gtk-play -c never -f' else
  if FindDefaultExecutablePath('afplay')<>'' then result:='afplay' else
  result:='';
end;

function GetNonWindowsRecCommand:String;
begin
  if FindDefaultExecutablePath('ffmpeg')<>'' then result:='ffmpeg -f pulse -i default' else
  result:='';
end;

{ TSoundPlayer }

procedure TSoundPlayer.SetPlayCommand(AValue: string);
begin
  if AValue='' then FPlayCommand:='<auto>' else FPlayCommand:=AValue;
end;

procedure TSoundPlayer.SetRecCommand(AValue: string);
begin
  if AValue='' then FRecCommand:='<auto>' else FRecCommand:=AValue;
end;

procedure TSoundPlayer.PlaySound(const plik: string);
var
  {$IFDEF WINDOWS}
  flags: word;
  {$ELSE}
  L: TStrings;
  i: Integer;
  playCmd: String;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  if FPO=poASync then flags:=SND_ASYNC or SND_NODEFAULT else flags:=SND_SYNC or SND_NODEFAULT;
  try
    sndPlaySound(PChar(plik),flags);
  except
    //ShowMessage('Unable to play '+plik);
  end;
  {$ELSE}
  if FPlayCommand='<auto>' then playCmd:=DPlayCommand else playCmd:=FPlayCommand;
  if (playCmd<>'') then
  begin
    L:=TStringList.Create;
    try
      L.Delimiter:=' ';
      L.DelimitedText:=playCmd;
      if FPO=poASync then
      begin
        if SoundPlayerAsyncProcess=nil then SoundPlayerAsyncProcess:=TASyncProcess.Create(nil);
        SoundPlayerAsyncProcess.CurrentDirectory:=ExtractFileDir(plik);
        SoundPlayerAsyncProcess.Executable:=FindDefaultExecutablePath(L[0]);
        SoundPlayerAsyncProcess.Parameters.Clear;
        for i:=1 to L.Count-1 do SoundPlayerAsyncProcess.Parameters.Add(L[i]);
        SoundPlayerAsyncProcess.Parameters.Add(plik);
        try
          SoundPlayerAsyncProcess.Execute;
        except
          on E: Exception do E.CreateFmt('Playstyle=paASync: Unable to play %s Message:%s',[plik,E.Message]);
        end;
      end else begin
        if SoundPlayerSyncProcess=nil then SoundPlayerSyncProcess:=TProcess.Create(nil);
        SoundPlayerSyncProcess.CurrentDirectory:=ExtractFileDir(plik);
        SoundPlayerSyncProcess.Executable:=FindDefaultExecutablePath(L[0]);
        SoundPlayersyncProcess.Parameters.Clear;
        for i:=1 to L.Count-1 do SoundPlayerSyncProcess.Parameters.Add(L[i]);
        SoundPlayerSyncProcess.Parameters.Add(plik);
        try
          SoundPlayerSyncProcess.Execute;
          SoundPlayersyncProcess.WaitOnExit;
        except
          on E: Exception do E.CreateFmt('Playstyle=paSync: Unable to play %s Message:%s',[plik,E.Message]);
        end;
      end;
    finally
      L.Free;
    end;
  end else raise Exception.CreateFmt('The play command %s does not work on your system',[fPlayCommand]);
  {$ENDIF}
end;

procedure TSoundPlayer.RecordSound(const plik: string);
var
  {$IFDEF WINDOWS}
  flags: word;
  {$ELSE}
  L: TStrings;
  i: Integer;
  recCmd: String;
  {$ENDIF}
begin
  if FileExists(plik) then DeleteFile(plik);
  {$IFDEF WINDOWS}
  if FPO=poASync then flags:=SND_ASYNC or SND_NODEFAULT else flags:=SND_SYNC or SND_NODEFAULT;
  try
    //sndRecSound(PChar(plik),flags);
  except
    //ShowMessage('Unable to record '+plik);
  end;
  {$ELSE}
  if FRecCommand='<auto>' then recCmd:=DRecCommand else recCmd:=FRecCommand;
  if (recCmd<>'') then
  begin
    L:=TStringList.Create;
    try
      L.Delimiter:=' ';
      L.DelimitedText:=recCmd;
      if FPO=poASync then
      begin
        if SoundPlayerAsyncProcess=nil then SoundPlayerAsyncProcess:=TASyncProcess.Create(nil);
        SoundPlayerAsyncProcess.CurrentDirectory:=ExtractFileDir(plik);
        SoundPlayerAsyncProcess.Executable:=FindDefaultExecutablePath(L[0]);
        SoundPlayerAsyncProcess.Parameters.Clear;
        for i:=1 to L.Count-1 do SoundPlayerAsyncProcess.Parameters.Add(L[i]);
        SoundPlayerAsyncProcess.Parameters.Add(plik);
        try
          SoundPlayerAsyncProcess.Execute;
        except
          on E: Exception do E.CreateFmt('Recstyle=paASync: Unable to record %s Message:%s',[plik,E.Message]);
        end;
      end else begin
        if SoundPlayerSyncProcess=nil then SoundPlayerSyncProcess:=TProcess.Create(nil);
        SoundPlayerSyncProcess.CurrentDirectory:=ExtractFileDir(plik);
        SoundPlayerSyncProcess.Executable:=FindDefaultExecutablePath(L[0]);
        SoundPlayersyncProcess.Parameters.Clear;
        for i:=1 to L.Count-1 do SoundPlayerSyncProcess.Parameters.Add(L[i]);
        SoundPlayerSyncProcess.Parameters.Add(plik);
        try
          SoundPlayerSyncProcess.Execute;
          SoundPlayersyncProcess.WaitOnExit;
        except
          on E: Exception do E.CreateFmt('Recstyle=paSync: Unable to record %s Message:%s',[plik,E.Message]);
        end;
      end;
    finally
      L.Free;
    end;
  end else raise Exception.CreateFmt('The record command %s does not work on your system',[fRecCommand]);
  {$ENDIF}
end;

constructor TSoundPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  (* drivery *)
  //{$IFNDEF WINDOWS}
  //SoundPlayerAsyncProcess:=Tasyncprocess.Create(nil);
  //SoundPlayerSyncProcess:=Tprocess.Create(nil);
  //{$ENDIF}
  (* reszta *)
  {$IFDEF WINDOWS}
  DPlayCommand:='sndPlaySound';
  DRecCommand:='sndRecSound';
  {$ELSE}
  DPlayCommand:=GetNonWindowsPlayCommand; //Linux, FreeBSD, Mac etc.
  DRecCommand:=GetNonWindowsRecCommand; //Linux, FreeBSD, Mac etc.
  {$ENDIF}
  FPO:=poAsync;
  FMode:=smPlay;
  FPlayCommand:='<auto>';
  FRecCommand:='<auto>';
end;

destructor TSoundPlayer.Destroy;
begin
  {$IFNDEF WINDOWS}
  FreeAndNil(SoundPlayerSyncProcess);
  FreeAndNil(SoundPlayerAsyncProcess);
  {$ENDIF}
  inherited Destroy;
end;

procedure TSoundPlayer.Start;
begin
  if Assigned(FBeforeStart) then FBeforeStart(self);
  if FMode=smPlay then
  begin
    if not FileExists(FFileName) then exit;
    try
      PlaySound(FFileName);
    except
      on E: Exception do E.CreateFmt('Unable to play %s Message: %s', [FFileName,E.Message]);
    end;
  end else begin
    try
      RecordSound(FFileName);
    except
      on E: Exception do E.CreateFmt('Unable to record %s Message: %s', [FFileName,E.Message]);
    end;
  end;
  if Assigned(FAfterStart) then FAfterStart(self);
end;

procedure TSoundPlayer.Stop;
begin
  if Assigned(FBeforeStop) then FBeforeStop(self);
  {$IFDEF WINDOWS}
  sndPlaySound(nil,0);
  {$ELSE}
  if SoundPlayerSyncProcess<>nil then SoundPlayerSyncProcess.Terminate(1);
  if SoundPlayerAsyncProcess<>nil then SoundPlayerAsyncProcess.Terminate(1);
  {$ENDIF}
  if Assigned(FAfterStop) then FAfterStop(self);
end;

function TSoundPlayer.Busy: boolean;
begin
  {$IFDEF WINDOWS}
  result:=false;
  {$ELSE}
  case FPO of
    poAsync: if SoundPlayerAsyncProcess<>nil then result:=SoundPlayerAsyncProcess.Running else result:=false;
    poSync : if SoundPlayerSyncProcess<>nil  then result:=SoundPlayerSyncProcess.Running else result:=false;
  end;
  {$ENDIF}
end;

{ TSoundPlayer }

end.
