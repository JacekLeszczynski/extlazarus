unit UOSEngine;

{$mode objfpc}{$H+}
{.$define SOUTH}

interface

uses
  Classes, SysUtils, LResources;

type

  { TUosEngine }

  TUOSEngineOsInfo = (osNone,osWindows86,osWindows64,osLinux86,osLinux64,osFreeBSD86,osFreeBSD64);
  TUOSEngineDriversLoad = (dlPortAudio,dlSndAudio,dlMpg123,dlMp4ff,dlFaad,dlOpus);
  TUOSEngineDriversLoadSet = set of TUOSEngineDriversLoad;
  {$IFDEF SOUTH}
  TUOSEngineDriversServerLoad = (slShout,slOpus);
  TUOSEngineDriversServerLoadSet = set of TUOSEngineDriversServerLoad;
  {$ENDIF}

  TUOSEngine = class(TComponent)
  private
    FDrivers: TUOSEngineDriversLoadSet;
    FLibDirectory: string;
    FLoaded: boolean;
    FOSInfo: TUOSEngineOsInfo;
    {$IFDEF SOUTH}
    FLoadedServers: boolean;
    FServers: TUOSEngineDriversServerLoadSet;
    {$ENDIF}
    procedure SetLibDirectory(AValue: string);
    procedure _init;
    function TestOSInfo: TUOSEngineOsInfo;
    function GetAutoDir: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadLibrary: boolean;
    procedure UnLoadLibrary;
    function GetErrorStr: string;
  published
    property OSInfo: TUOSEngineOsInfo read FOSInfo; //Information from OS System.
    property LibDirectory: string read FLibDirectory write SetLibDirectory;
    {Drivers List:
       PortAudio - for dealing with audio devices of sound card,
       SndFile - for decoding wav, flac, org audio files,
       Mpg123 - for decoding mp3 audio files,
       Faad and Mp4ff - for decoding m4a audio files,
       OpusFile - for decoding opus audio files
    } property DriversLoad: TUOSEngineDriversLoadSet read FDrivers write FDrivers;
    property Loaded: boolean read FLoaded default false; //Library is Loaded!
    {$IFDEF SOUTH}
    property ServersLoad: TUOSEngineDriversServerLoadSet read FServers write FServers;
    property LoadedServers: boolean read FLoadedServers default false; //Library is Loaded!
    {$ENDIF}
  end;

procedure Register;

implementation

uses
  uos_flat;

var
  _FF: string;
  libPortAudio,libSndDFile,Mpg123FileName,libMp4ff,libFaad,libOpus: string;
  libShoutServer,libOpusServer: string;

procedure Register;
begin
  {$I uosengine_icon.lrs}
  RegisterComponents('Multimedia',[TUOSEngine]);
end;

{ TUosEngine }

procedure TUOSEngine._init;
begin
  FOSInfo:=TestOSInfo;
  FLibDirectory:='<auto>';
  FLoaded:=false;
  {$IFDEF SOUTH}
  FLoadedServers:=false;
  {$ENDIF}
  FDrivers:=[dlPortAudio,dlSndAudio];
end;

procedure TUOSEngine.SetLibDirectory(AValue: string);
begin
  if AValue='' then FLibDirectory:='<auto>' else FLibDirectory:=AValue;
end;

function TUOSEngine.TestOSInfo: TUOSEngineOsInfo;
begin
  result:=osNone;
  {$IFDEF UNIX}
  _FF:='/';
  {$ELSE}
  _FF:='\';
  {$ENDIF}
  {$IFDEF Windows}
  {$if defined(cpu64)}
  libPortAudio:='LibPortaudio-64.dll';
  libSndDFile:='LibSndFile-64.dll';
  Mpg123FileName:='LibMpg123-64.dll';
  libMp4ff:='';
  libFaad:='';
  libOpus:='';
  libShoutServer:='';
  libOpusServer:='';
  result:=osWindows64;
  {$else}
  libPortAudio:='LibPortaudio-32.dll';
  libSndDFile:='LibSndFile-32.dll';
  Mpg123FileName:='LibMpg123-32.dll';
  libMp4ff:='LibMp4ff-32.dll';
  libFaad:='LibFaad2-32.dll';
  libOpus:='LibOpusFile-32.dll';
  libShoutServer:='';
  libOpusServer:='libopus-0.dll';
  result:=osWindows86;
  {$endif}
  {$ENDIF}
  {$if defined(cpu64) and defined(linux) }
  libPortAudio:='LibPortaudio-64.so';
  libSndDFile:='LibSndFile-64.so';
  Mpg123FileName:='LibMpg123-64.so';
  libMp4ff:='LibMp4ff-64.so';
  libFaad:='LibFaad2-64.so';
  libOpus:='LibOpusFile-64.so';
  libShoutServer:='LibShout-64.so';
  libOpusServer:='libopus.so';
  result:=osLinux64;
  {$ENDIF}
  {$if defined(cpu86) and defined(linux)}
  libPortAudio:='LibPortaudio-32.so';
  libSndDFile:='LibSndFile-32.so';
  Mpg123FileName:='LibMpg123-32.so';
  libMp4ff:='LibMp4ff-32.so';
  libFaad:='LibFaad2-32.so';
  libOpus:='';
  libShoutServer:='';
  libOpusServer:='';
  result:=osLinux86;
  {$ENDIF}
  {$IFDEF freebsd}
  {$if defined(cpu64)}
  libPortAudio:='libportaudio-64.so';
  libSndDFile:='libsndfile-64.so';
  Mpg123FileName:='libmpg123-64.so';
  libMp4ff:='libmp4ff-64.so';
  libFaad:='libfaad2-64.so';
  libOpus:='libopusfile-64.so';
  libShoutServer:='libshout-64.so';
  libOpusServer:='libopus.so';
  result:=osFreeBSD64;
  {$else}
  libPortAudio:='libportaudio-32.so';
  libSndDFile:='libsndfile-32.so';
  Mpg123FileName:='libmpg123-32.so';
  libMp4ff:='';
  libFaad:='';
  libOpus:='';
  libShoutServer:='';
  libOpusServer:='';
  result:=osFreeBSD86;
  {$endif}
  {$ENDIF}
end;

function TUOSEngine.GetAutoDir: string;
var
  s,md,snd: string;
begin
  md:=ExtractFilePath(ParamStr(0));
  {$IFDEF Windows}
    {$if defined(cpu64)}
      snd:='LibSndFile-64.dll';
    {$else}
      snd:='LibSndFile-32.dll';
    {$endif}
  {$ENDIF}
  {$if defined(cpu64) and defined(linux) }
    snd:='LibSndFile-64.so';
  {$ENDIF}
  {$if defined(cpu86) and defined(linux)}
    snd:='LibSndFile-32.so';
  {$ENDIF}
  {$IFDEF freebsd}
    {$if defined(cpu64)}
      snd:='libsndfile-64.so';
    {$else}
      snd:='libsndfile-32.so';
    {$endif}
  {$ENDIF}
  {$IFDEF UNIX}
    if FileExists('/usr/lib/uos/'+snd) then s:='/usr/lib/uos' else
    if FileExists('/usr/local/lib/uos/'+snd) then s:='/usr/local/lib/uos' else
    if FileExists(md+'/uos/'+snd) then s:=md+'/uos' else
    s:=md;
  {$ELSE}
    if FileExists('c:\windows\uos\'+snd) then s:='c:\windows\uos' else
    if FileExists('c:\windows\system\uos\'+snd) then s:='c:\windows\system\uos' else
    if FileExists('c:\windows\system32\uos\'+snd) then s:='c:\windows\system32\uos' else
    if FileExists('c:\windows\system64\uos\'+snd) then s:='c:\windows\system64\uos' else
    if FileExists(md+'\uos\'+snd) then s:=md+'\uos' else
    s:=md;
  {$ENDIF}
  result:=s;
end;

constructor TUOSEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  _init;
end;

destructor TUOSEngine.Destroy;
begin
  UnLoadLibrary;
  uos_free;
  inherited Destroy;
end;

function TUOSEngine.LoadLibrary: boolean;
var
  s: string;
  s1,s2,s3,s4,s5,s6: string;
  p1,p2,p3,p4,p5,p6: pchar;
  {$IFDEF SOUTH}
  s11,s22: string;
  p11,p22: pchar;
  err: integer;
  {$ENDIF}
begin
  result:=FLoaded;
  if FLoaded then exit;
  s:=FLibDirectory;
  if (s='<auto>') or (s='.') then s:=StringReplace(GetAutoDir+_FF,_FF+_FF,_FF,[rfReplaceAll]) else s:=StringReplace(s+_FF,_FF+_FF,_FF,[rfReplaceAll]);
  s1:=s+libPortAudio;
  s2:=s+libSndDFile;
  s3:=s+Mpg123FileName;
  s4:=s+libMp4ff;
  s5:=s+libFaad;
  s6:=s+libOpus;
  {$IFDEF SOUTH}
  s11:=s+libShoutServer;
  s22:=s+libOpusServer;
  {$ENDIF}
  if (dlPortAudio in FDrivers) and (s1<>'') then p1:=pchar(s1) else p1:=nil;
  if (dlSndAudio in FDrivers) and (s2<>'') then p2:=pchar(s2) else p2:=nil;
  if (dlMpg123 in FDrivers) and (s3<>'') then p3:=pchar(s3) else p3:=nil;
  if (dlMp4ff in FDrivers) and (s4<>'') then p4:=pchar(s4) else p4:=nil;
  if (dlFaad in FDrivers) and (s5<>'') then p5:=pchar(s5) else p5:=nil;
  if (dlOpus in FDrivers) and (s6<>'') then p6:=pchar(s6) else p6:=nil;
  if uos_LoadLib(p1,p2,p3,p4,p5,p6)=0 then FLoaded:=true else FLoaded:=false;
  {$IFDEF SOUTH}
  if (slShout in FServers) and (s11<>'') then p11:=pchar(s11) else p11:=nil;
  if (slOpus in FServers) and (s22<>'') then p22:=pchar(s22) else p22:=nil;
  err:=uos_LoadServerLib(p11,p22);
  if err=0 then FLoadedServers:=true else
  begin
    FLoadedServers:=false;
    writeln('Podczas ładowania ServerShout wystąpił błąd: ',err);
    if not FileExists(s11) then writeln('Biblioteka ',s11,' - nie została znaleziona!');
    if not FileExists(s22) then writeln('Biblioteka ',s22,' - nie została znaleziona!');
  end;
  {$ENDIF}
  result:=FLoaded;
end;

procedure TUOSEngine.UnLoadLibrary;
begin
  if FLoaded then
  begin
    uos_unloadlib;
    FLoaded:=false;
  end;
  {$IFDEF SOUTH}
  if FLoadedServers then
  begin
    uos_unloadServerLib;
    FLoadedServers:=false;
  end;
  {$ENDIF}
end;

function TUOSEngine.GetErrorStr: string;
begin
  result:=GetLoadErrorStr;
end;

end.
