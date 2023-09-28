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
  TVideoConvertTagEngineMp3 = (teMp3_none,teMp3_lltag,teMp3_id3v2,teMp3_id3tool);
  TVideoConvertTagEngineOgg = (teOgg_none,teOgg_lltag,teOgg_vorbiscomment);
  TVideoConvertTagEngineFlac = (teFlac_none,teFlac_lltag,teFlac_vorbiscomment);
  TVideoConvert = class(TComponent)
  private
    FOnFileRendered: TVideoConvertThreadsFileOutEvent;
    FOnThreadsCount: TVideoConvertThreadsCount;
    FTagFlacEngine: TVideoConvertTagEngineFlac;
    FTagMp3Engine: TVideoConvertTagEngineMp3;
    FTagOggEngine: TVideoConvertTagEngineOgg;
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
    procedure RenderOgg(aId: integer; aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2; aTitle: string = ''; aArtist: string = ''; aAlbum: string = '');
    procedure RenderOgg(aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2; aTitle: string = ''; aArtist: string = ''; aAlbum: string = '');
    function GetOGGFileInfo(const filename: string; var title, artist, album: string): Boolean;
    function SetOGGFileInfo(const filename, title, artist, album: string): Boolean;
  published
    property ThreadsCount: integer read FThreadCount default 0;
    property ThreadsOn: boolean read FThreads write FThreads default false;
    property OnThreadsCount: TVideoConvertThreadsCount read FOnThreadsCount write FOnThreadsCount;
    property OnFileRendered: TVideoConvertThreadsFileOutEvent read FOnFileRendered write FOnFileRendered;
  published
    property TagMp3Engine: TVideoConvertTagEngineMp3 read FTagMp3Engine write FTagMp3Engine default teMp3_none;
    property TagOggEngine: TVideoConvertTagEngineOgg read FTagOggEngine write FTagOggEngine default teOgg_none;
    property TagFlacEngine: TVideoConvertTagEngineFlac read FTagFlacEngine write FTagFlacEngine default teFlac_none;
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
    OggEngine: TVideoConvertTagEngineOgg;
    title,artist,album: string;
    procedure go_inc;
    procedure go_dec;
    procedure go_test;
    procedure Execute; override;
  protected
  public
    constructor Create(aSender: TVideoConvert; aOggEngine: TVideoConvertTagEngineOgg; aType: integer; aId: integer; aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2; aTitle: string = ''; aArtist: string = ''; aAlbum: string = '');
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

uses
  ecode_unit;

procedure Register;
begin
  {$I videoconvert_icon.lrs}
  RegisterComponents('Multimedia',[TVideoConvert]);
end;

function _GetOGGFileInfo(EngineOgg: TVideoConvertTagEngineOgg; const filename: string; var title, artist, album: string): Boolean;
var
  p: TAsyncProcess;
  ss: TStringList;
  i: integer;
  s,s1,s2: string;
begin
  title:='';
  artist:='';
  album:='';
  if EngineOgg=teOgg_lltag then
  begin
    p:=TAsyncProcess.Create(nil);
    p.CurrentDirectory:=ExtractFilePath(filename);
    p.Executable:='lltag';
    p.Options:=[poWaitOnExit,poUsePipes,poNoConsole];
    p.Priority:=ppNormal;
    p.ShowWindow:=swoNone;
    //-S nazwa_pliku.ogg
    p.Parameters.Add('-S');
    p.Parameters.Add(ExtractFileName(filename));
    try
      p.Execute;
      if p.NumBytesAvailable>0 then
      begin
        ss:=TStringList.Create;
        try
          ss.LoadFromStream(p.Output);
          for i:=0 to ss.Count-1 do
          begin
            s:=trim(ss[i]);
            if s='' then continue;
            if s[length(s)]=':' then continue;
            s1:=GetLineToStr(s,1,'=');
            s2:=GetLineToStr(s,2,'=');
{
-a, --ARTIST  <val>    Add explicit value <val> for ARTIST
-t, --TITLE   <val>    Add explicit value <val> for TITLE
-A, --ALBUM   <val>    Add explicit value <val> for ALBUM
-n, --NUMBER  <val>    Add explicit value <val> for NUMBER
-g, --GENRE   <val>    Add explicit value <val> for GENRE
-d, --DATE    <val>    Add explicit value <val> for DATE
-c, --COMMENT <val>    Add explicit value <val> for COMMENT
}
            if s1='TITLE' then title:=s2 else
            if s1='ARTIST' then artist:=s2 else
            if s1='ALBUM' then album:=s2;
          end;
        finally
          ss.Free;
        end;
      end;
    finally
      p.Terminate(0);
      p.Free;
    end;
  end;
  result:=(title<>'') or (artist<>'') or (album<>'');
end;

function _SetOGGFileInfo(EngineOgg: TVideoConvertTagEngineOgg; const filename, title, artist, album: string): Boolean;
var
  p: TAsyncProcess;
begin
  if not FileExists(filename) then
  begin
    result:=false;
    exit;
  end;
  if EngineOgg=teOgg_lltag then
  begin
    p:=TAsyncProcess.Create(nil);
    p.CurrentDirectory:=ExtractFilePath(filename);
    p.Executable:='lltag';
    p.Options:=[poWaitOnExit,poUsePipes,poNoConsole];
    p.Priority:=ppNormal;
    p.ShowWindow:=swoNone;
    //--yes -t title -a artist -A album nazwa_pliku.ogg
    p.Parameters.Add('--yes');
    p.Parameters.Add('-t');
    p.Parameters.Add(trim(title));
    p.Parameters.Add('-a');
    p.Parameters.Add(trim(artist));
    p.Parameters.Add('-A');
    p.Parameters.Add(trim(album));
    p.Parameters.Add(ExtractFileName(filename));
    try
      p.Execute;
    finally
      p.Terminate(0);
      p.Free;
    end;
    result:=true;
  end else result:=false;
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

procedure _RenderOgg(EngineOgg: TVideoConvertTagEngineOgg; aSourceFileName: string; aChannels: integer = 0; aQuality: integer = -2);
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
    1: begin
         _RenderOgg(OggEngine,sfilename,schannels,squality);
         if (title<>'') or (artist<>'') or (album<>'') then _SetOGGFileInfo(OggEngine,ChangeFileExt(sfilename,'.ogg'),title,artist,album);
       end;
  end;
  synchronize(@go_dec);
end;

constructor TVideoConvertRenderFile.Create(aSender: TVideoConvert;
  aOggEngine: TVideoConvertTagEngineOgg; aType: integer; aId: integer;
  aSourceFileName: string; aChannels: integer; aQuality: integer;
  aTitle: string; aArtist: string; aAlbum: string);
begin
  FreeOnTerminate:=true;
  sender:=aSender;
  OggEngine:=aOggEngine;
  typ:=aType; //0-Wav, 1-Ogg
  id:=aId;
  sfilename:=aSourceFileName;
  squality:=aQuality;
  schannels:=aChannels;
  title:=aTitle;
  artist:=aArtist;
  album:=aAlbum;
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
  FTagMp3Engine:=teMp3_none;
  FTagOggEngine:=teOgg_none;
  FTagFlacEngine:=teFlac_none;
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
    a:=TVideoConvertRenderFile.Create(self,teOgg_none,0,aId,aSourceFileName,aChannels);
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
  aChannels: integer; aQuality: integer; aTitle: string; aArtist: string;
  aAlbum: string);
var
  a: TVideoConvertRenderFile;
begin
  if FThreads then
  begin
    a:=TVideoConvertRenderFile.Create(self,FTagOggEngine,1,aId,aSourceFileName,aChannels,aQuality,aTitle,aArtist,aAlbum);
    a.OnRefreshThreadCount:=@OdswiezIloscWatkow;
    a.OnExit:=@PlikZostalWygenerowany;
    a.Start;
  end else begin
    _RenderOgg(FTagOggEngine,aSourceFileName,aChannels,aQuality);
    if (aTitle<>'') or (aArtist<>'') or (aAlbum<>'') then _SetOGGFileInfo(FTagOggEngine,ChangeFileExt(aSourceFileName,'.ogg'),aTitle,aArtist,aAlbum);
  end;
end;

procedure TVideoConvert.RenderOgg(aSourceFileName: string; aChannels: integer;
  aQuality: integer; aTitle: string; aArtist: string; aAlbum: string);
begin
  RenderOgg(0,aSourceFileName,aChannels,aQuality,aTitle,aArtist,aAlbum);
end;

function TVideoConvert.GetOGGFileInfo(const filename: string; var title,
  artist, album: string): Boolean;
begin
  result:=_GetOGGFileInfo(FTagOggEngine,filename,title,artist,album);
end;

function TVideoConvert.SetOGGFileInfo(const filename, title, artist,
  album: string): Boolean;
begin
  result:=_SetOGGFileInfo(FTagOggEngine,filename,title,artist,album);
end;

end.
