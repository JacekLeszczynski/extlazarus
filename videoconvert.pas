unit VideoConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Process, AsyncProcess;

type

  { TVideoConvert }

  TVideoConvertThreadsRefreshCountEvent = procedure of object;
  TVideoConvertThreadsFileOutEvent = procedure(aId: integer; aSourceFileName,aDestinationFileName: string) of object;
  TVideoConvertThreadsCount = procedure(aCount: integer) of object;
  TVideoConvert = class(TComponent)
  private
    FOnFileRendered: TVideoConvertThreadsFileOutEvent;
    FOnThreadsCount: TVideoConvertThreadsCount;
    FThreadCount,licznik: integer;
    FThreads: boolean;
    proc: TAsyncProcess;
    procedure OdswiezIloscWatkow;
    procedure PlikZostalWygenerowany(aId: integer; aSourceFile,aDestinationFile: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RenderWav(aId: integer; aSourceFileName: string; aChannels: integer = 0);
    procedure RenderWav(aSourceFileName: string; aChannels: integer = 0);
    procedure RenderOgg(aId: integer; aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2);
    procedure RenderOgg(aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2);
  published
    property ThreadsCount: integer read FThreadCount default 0;
    property ThreadsOn: boolean read FThreads write FThreads default false;
    property OnThreadsCount: TVideoConvertThreadsCount read FOnThreadsCount write FOnThreadsCount;
    property OnFileRendered: TVideoConvertThreadsFileOutEvent read FOnFileRendered write FOnFileRendered;
  end;

  { TVideoConvertRenderFile }

  TVideoConvertRenderFile = class(TThread)
  private
    stop: boolean;
    typ,id: integer;
    FOnExit: TVideoConvertThreadsFileOutEvent;
    FOnRefresh1: TVideoConvertThreadsRefreshCountEvent;
    sender: TVideoConvert;
    sfilename: string;
    squality,schannels: integer;
    procedure go_inc;
    procedure go_dec;
    procedure go_test;
    procedure Execute; override;
  protected
  public
    constructor Create(aSender: TVideoConvert; aType: integer; aId: integer; aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2);
    destructor Destroy; override;
  published
    property OnRefreshThreadCount: TVideoConvertThreadsRefreshCountEvent read FOnRefresh1 write FOnRefresh1;
    property OnExit: TVideoConvertThreadsFileOutEvent read FOnExit write FOnExit;
  end;

  { TVideoConvertRenderList }

  TVideoConvertRenderList = class(TThread)
  protected
  public
    constructor Create(aSender: TVideoConvert; aType: integer; aId: integer; aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2);
    destructor Destroy; override;
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I videoconvert_icon.lrs}
  RegisterComponents('Multimedia',[TVideoConvert]);
end;

procedure _RenderWav(aSourceFileName: string; aChannels: integer = 0);
var
  dir,f1,f2: string;
  proc: TAsyncProcess;
begin
  dir:=ExtractFilePath(aSourceFileName);
  f1:=ExtractFileName(aSourceFileName);
  f2:=ChangeFileExt(f1,'.wav');
  proc:=TAsyncProcess.Create(nil);
  proc.CurrentDirectory:=dir;
  proc.Executable:='ffmpeg';
  //proc.Options:=[poWaitOnExit,poUsePipes,poNoConsole];
  proc.Options:=[poWaitOnExit,poNoConsole];
  proc.Priority:=ppNormal;
  proc.ShowWindow:=swoNone;
  //proc.OnReadData:=@YTReadData;
  //-i madonna.mp4 -vn -y madonna.wav
  proc.Parameters.Add('-i');
  proc.Parameters.Add(f1);
  if aChannels>0 then
  begin
    proc.Parameters.Add('-ac');
    proc.Parameters.Add(IntToStr(aChannels));
  end;
  proc.Parameters.Add('-vn');
  proc.Parameters.Add('-y');
  proc.Parameters.Add(f2);
  try
    proc.Execute;
  finally
    proc.Terminate(0);
    proc.Free;
  end;
end;

procedure _RenderOgg(aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2);
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

{ TVideoConvertRenderList }

constructor TVideoConvertRenderList.Create(aSender: TVideoConvert;
  aType: integer; aId: integer; aSourceFileName: string; aChannels: integer;
  aQuality: integer);
begin

end;

destructor TVideoConvertRenderList.Destroy;
begin
  inherited Destroy;
end;

{ TVideoConvertRenderFile }

procedure TVideoConvertRenderFile.go_inc;
var
  a,b: integer;
begin
  a:=sender.FThreadCount;
  b:=sender.licznik;
  inc(a);
  sender.FThreadCount:=a;
  if assigned(FOnRefresh1) then FOnRefresh1;
  stop:=b>2;
  if not stop then
  begin
    inc(b);
    sender.licznik:=b;
  end;
end;

procedure TVideoConvertRenderFile.go_dec;
var
  a,b: integer;
  s: string;
begin
  a:=sender.FThreadCount;
  b:=sender.licznik;
  dec(a);
  dec(b);
  sender.FThreadCount:=a;
  sender.licznik:=b;
  if assigned(FOnRefresh1) then FOnRefresh1;
  if assigned(FOnExit) then
  begin
    case typ of
      0: s:=ChangeFileExt(sfilename,'.wav');
      1: s:=ChangeFileExt(sfilename,'.ogg');
    end;
    FOnExit(id,sfilename,s);
  end;
end;

procedure TVideoConvertRenderFile.go_test;
var
  b: integer;
begin
  b:=sender.licznik;
  stop:=b>2;
  if not stop then
  begin
    inc(b);
    sender.licznik:=b;
  end;
end;

procedure TVideoConvertRenderFile.Execute;
begin
  synchronize(@go_inc);
  while stop do
  begin
    sleep(1000);
    synchronize(@go_test);
  end;
  case typ of
    0: _RenderWav(sfilename,schannels);
    1: _RenderOgg(sfilename,schannels,squality);
  end;
  synchronize(@go_dec);
end;

constructor TVideoConvertRenderFile.Create(aSender: TVideoConvert;
  aType: integer; aId: integer; aSourceFileName: string; aChannels: integer;
  aQuality: integer);
begin
  FreeOnTerminate:=true;
  sender:=aSender;
  typ:=aType; //0-Wav, 1-Ogg
  id:=aId;
  sfilename:=aSourceFileName;
  squality:=aQuality;
  schannels:=aChannels;
  inherited Create(true);
end;

destructor TVideoConvertRenderFile.Destroy;
begin
  inherited Destroy;
end;

{ TVideoConvert }

procedure TVideoConvert.OdswiezIloscWatkow;
begin
  if assigned(FOnThreadsCount) then FOnThreadsCount(FThreadCount);
end;

procedure TVideoConvert.PlikZostalWygenerowany(aId: integer; aSourceFile,
  aDestinationFile: string);
begin
  if assigned(FOnFileRendered) then FOnFileRendered(aId,aSourceFile,aDestinationFile);
end;

constructor TVideoConvert.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  licznik:=0;
  FThreadCount:=0;
  FThreads:=false;
end;

destructor TVideoConvert.Destroy;
begin
  inherited Destroy;
end;

procedure TVideoConvert.RenderWav(aId: integer; aSourceFileName: string;
  aChannels: integer);
var
  a: TVideoConvertRenderFile;
begin
  if FThreads then
  begin
    a:=TVideoConvertRenderFile.Create(self,0,aId,aSourceFileName,aChannels);
    a.OnRefreshThreadCount:=@OdswiezIloscWatkow;
    a.OnExit:=@PlikZostalWygenerowany;
    a.Start;
  end else _RenderWav(aSourceFileName,aChannels);
end;

procedure TVideoConvert.RenderWav(aSourceFileName: string; aChannels: integer);
begin
  RenderWav(0,aSourceFileName,aChannels);
end;

procedure TVideoConvert.RenderOgg(aId: integer; aSourceFileName: string;
  aChannels: integer; aQuality: integer);
var
  a: TVideoConvertRenderFile;
begin
  if FThreads then
  begin
    a:=TVideoConvertRenderFile.Create(self,1,aId,aSourceFileName,aChannels,aQuality);
    a.OnRefreshThreadCount:=@OdswiezIloscWatkow;
    a.OnExit:=@PlikZostalWygenerowany;
    a.Start;
  end else _RenderOgg(aSourceFileName,aChannels,aQuality);
end;

procedure TVideoConvert.RenderOgg(aSourceFileName: string; aChannels: integer;
  aQuality: integer);
begin
  RenderOgg(0,aSourceFileName,aChannels,aQuality);
end;

end.
