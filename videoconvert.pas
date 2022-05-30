unit VideoConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Process, AsyncProcess;

type

  { TVideoConvert }

  TVideoConvertThreadsRefreshCountEvent = procedure of object;
  TVideoConvertThreadsFileOutEvent = procedure(aSourceFileName,aDestinationFileName: string) of object;
  TVideoConvertThreadsCount = procedure(aCount: integer) of object;
  TVideoConvert = class(TComponent)
  private
    FOnFileRendered: TVideoConvertThreadsFileOutEvent;
    FOnThreadsCount: TVideoConvertThreadsCount;
    FThreadCount: integer;
    FThreads: boolean;
    proc: TAsyncProcess;
    procedure OdswiezIloscWatkow;
    procedure PlikZostalWygenerowany(aSourceFile,aDestinationFile: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderOgg(aSourceFileName: string; aQuality: integer = -2; aChannels: integer = 0);
  published
    property ThreadsCount: integer read FThreadCount default 0;
    property ThreadsOn: boolean read FThreads write FThreads default false;
    property OnThreadsCount: TVideoConvertThreadsCount read FOnThreadsCount write FOnThreadsCount;
    property OnFileRendered: TVideoConvertThreadsFileOutEvent read FOnFileRendered write FOnFileRendered;
  end;

  { TVideoConvertRenderOgg }

  TVideoConvertRenderOgg = class(TThread)
  private
    FOnExit: TVideoConvertThreadsFileOutEvent;
    FOnRefresh1: TVideoConvertThreadsRefreshCountEvent;
    sender: TVideoConvert;
    sfilename: string;
    squality,schannels: integer;
    procedure go_inc;
    procedure go_dec;
    procedure Execute; override;
  protected
  public
    constructor Create(aSender: TVideoConvert; aSourceFileName: string; aQuality: integer = -2; aChannels: integer = 0);
    destructor Destroy; override;
  published
    property OnRefreshThreadCount: TVideoConvertThreadsRefreshCountEvent read FOnRefresh1 write FOnRefresh1;
    property OnExit: TVideoConvertThreadsFileOutEvent read FOnExit write FOnExit;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I videoconvert_icon.lrs}
  RegisterComponents('Multimedia',[TVideoConvert]);
end;

procedure _RenderOgg(aSourceFileName: string;
  aQuality: integer; aChannels: integer);
var
  dir,f1,f2: string;
  proc: TAsyncProcess;
begin
  dir:=ExtractFilePath(aSourceFileName);
  f1:=ExtractFileName(aSourceFileName);
  f2:=ChangeFileExt(f1,'.ogg');
  proc:=TAsyncProcess.Create(nil);
  proc.CurrentDirectory:=dir;
  proc.Executable:='ffmpeg';
  //proc.Options:=[poWaitOnExit,poUsePipes,poNoConsole];
  proc.Options:=[poWaitOnExit,poNoConsole];
  proc.Priority:=ppNormal;
  proc.ShowWindow:=swoNone;
  //proc.OnReadData:=@YTReadData;
  //-i madonna.mp4 -vn -acodec libvorbis -q:a 5 -y madonna.ogg
  proc.Parameters.Add('-i');
  proc.Parameters.Add(f1);
  if aChannels>0 then
  begin
    proc.Parameters.Add('-ac');
    proc.Parameters.Add(IntToStr(aChannels));
  end;
  proc.Parameters.Add('-vn');
  proc.Parameters.Add('-acodec');
  proc.Parameters.Add('libvorbis');
  if aQuality>-2 then
  begin
    proc.Parameters.Add('-q:a');
    proc.Parameters.Add(IntToStr(aQuality));
  end;
  proc.Parameters.Add('-y');
  proc.Parameters.Add(f2);
  try
    proc.Execute;
  finally
    proc.Terminate(0);
    proc.Free;
  end;
end;

{ TVideoConvertRenderOgg }

procedure TVideoConvertRenderOgg.go_inc;
var
  a: integer;
begin
  a:=sender.FThreadCount;
  inc(a);
  sender.FThreadCount:=a;
  if assigned(FOnRefresh1) then FOnRefresh1;
end;

procedure TVideoConvertRenderOgg.go_dec;
var
  a: integer;
  s: string;
begin
  a:=sender.FThreadCount;
  dec(a);
  sender.FThreadCount:=a;
  if assigned(FOnRefresh1) then FOnRefresh1;
  if assigned(FOnExit) then
  begin
    s:=ChangeFileExt(s,'.ogg');
    FOnExit(sfilename,s);
  end;
end;

procedure TVideoConvertRenderOgg.Execute;
begin
  synchronize(@go_inc);
  _RenderOgg(sfilename,squality,schannels);
  synchronize(@go_dec);
end;

constructor TVideoConvertRenderOgg.Create(aSender: TVideoConvert;
  aSourceFileName: string; aQuality: integer; aChannels: integer);
begin
  FreeOnTerminate:=true;
  sender:=aSender;
  sfilename:=aSourceFileName;
  squality:=aQuality;
  schannels:=aChannels;
  inherited Create(true);
end;

destructor TVideoConvertRenderOgg.Destroy;
begin
  inherited Destroy;
end;

{ TVideoConvert }

procedure TVideoConvert.OdswiezIloscWatkow;
begin
  if assigned(FOnThreadsCount) then FOnThreadsCount(FThreadCount);
end;

procedure TVideoConvert.PlikZostalWygenerowany(aSourceFile,
  aDestinationFile: string);
begin
  if assigned(FOnFileRendered) then FOnFileRendered(aSourceFile,aDestinationFile);
end;

constructor TVideoConvert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadCount:=0;
  FThreads:=false;
end;

destructor TVideoConvert.Destroy;
begin
  inherited Destroy;
end;

procedure TVideoConvert.RenderOgg(aSourceFileName: string;
  aQuality: integer; aChannels: integer);
var
  a: TVideoConvertRenderOgg;
begin
  if FThreads then
  begin
    a:=TVideoConvertRenderOgg.Create(self,aSourceFileName,aQuality,aChannels);
    a.OnRefreshThreadCount:=@OdswiezIloscWatkow;
    a.OnExit:=@PlikZostalWygenerowany;
    a.Start;
  end else _RenderOgg(aSourceFileName,aQuality,aChannels);
end;

end.
