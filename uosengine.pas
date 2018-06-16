unit UOSEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TUosEngine }

  TUOSEngineOsInfo = (osNone,osWindows86,osWindows64,osLinux86,osLinux64,osFreeBSD86,osFreeBSD64);

  TUOSEngine = class(TComponent)
  private
    FLibDirectory: string;
    FLoaded: boolean;
    FOSInfo: TUOSEngineOsInfo;
    procedure SetLibDirectory(AValue: string);
    procedure _init;
    function TestOSInfo: TUOSEngineOsInfo;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadLibrary: boolean;
    procedure UnLoadLibrary;
  published
    property OSInfo: TUOSEngineOsInfo read FOSInfo; //Information from OS System.
    property LibDirectory: string read FLibDirectory write SetLibDirectory;
    property Loaded: boolean read FLoaded default false; //Library is Loaded!
  end;

procedure Register;

implementation

uses
  uos_flat;

var
  _FF,libPortAudio,libSndDFile: string;

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
  result:=osWindows64;
  {$else}
  libPortAudio:='LibPortaudio-32.dll';
  libSndDFile:='LibSndFile-32.dll';
  result:=osWindows86;
  {$endif}
  {$ENDIF}
  {$if defined(cpu64) and defined(linux) }
  libPortAudio:='LibPortaudio-64.so';
  libSndDFile:='LibSndFile-64.so';
  result:=osLinux64;
  {$ENDIF}
  {$if defined(cpu86) and defined(linux)}
  libPortAudio:='LibPortaudio-32.so';
  libSndDFile:='LibSndFile-32.so';
  result:=osLinux86;
  {$ENDIF}
  {$IFDEF freebsd}
  {$if defined(cpu64)}
  libPortAudio:='libportaudio-64.so';
  libSndDFile:='libsndfile-64.so';
  result:=osFreeBSD64;
  {$else}
  libPortAudio:='libportaudio-32.so';
  libSndDFile:='libsndfile-32.so';
  result:=osFreeBSD86;
  {$endif}
  {$ENDIF}
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
  s1,s2: string;
begin
  result:=FLoaded;
  if FLoaded then exit;
  s:=FLibDirectory;
  if (s='<auto>') or (s='.') then s:=ExtractFilePath(ParamStr(0)) else s:=s+_FF;
  s1:=s+libPortAudio;
  s2:=s+libSndDFile;
  if uos_LoadLib(Pchar(s1),pchar(s2),nil,nil,nil,nil)=0 then FLoaded:=true else FLoaded:=false;
  result:=FLoaded;
end;

procedure TUOSEngine.UnLoadLibrary;
begin
  if FLoaded then
  begin
    uos_unloadlib;
    FLoaded:=false;
  end;
end;

end.
