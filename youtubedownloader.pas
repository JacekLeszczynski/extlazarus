unit YoutubeDownloader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  AsyncProcess, Process, NetSynHTTP;

type

  { ZDARZENIA }

  TYoutubeDownloaderEngine = (enDefault,enDefBoost,enDefPlus);
  TYoutubeDownloaderType = (ytAudio,ytVideo,ytAll);
  TYoutubeDownloaderReadPozInfo = procedure(aType: TYoutubeDownloaderType; aFormatCode: integer; aExtension, aResolution: string; aRes,aPrzeplywnosc,aFPS,aSampleRate: integer; aSize: int64; aDASH: boolean) of object;
  TYoutubeDownloaderDlStart = procedure(aLink,aDir: string; aTag: integer) of object;
  TYoutubeDownloaderDlGetFileName = procedure(aFileName,aDir: string; aTag: integer) of object;
  TYoutubeDownloaderDlGetPosition = procedure(aPosition: integer; aSpeed: int64; aTag: integer) of object;
  TYoutubeDownloaderDlFinish = procedure(aLink,aFileName,aDir: string; aTag: integer) of object;

  { TYoutubeDownloader }

  TYoutubeDownloader = class(TComponent)
  private
    mem_yt_cookie: string;
    http: TNetSynHTTP;
    FAutoSelect: boolean;
    FMaxAudioBitRate,FMaxAudioSampleRate,FMaxVideoBitRate,FMaxVideoQuality,FMinAudioSampleRate: integer;
    FBoolReadPozInfo: boolean;
    FCookieFile: TFilename;
    FDirYtDl: string;
    FDirAria: string;
    FDirYtDlp: string;
    FDlFileName: TYoutubeDownloaderDlGetFileName;
    FDlPosition: TYoutubeDownloaderDlGetPosition;
    FDlFinish: TYoutubeDownloaderDlFinish;
    FEngine: TYoutubeDownloaderEngine;
    FReadPozInfo: TYoutubeDownloaderReadPozInfo;
    FStart,FStop: TNotifyEvent;
    FDlStart: TYoutubeDownloaderDlStart;
    FWatki: boolean;
    linki,katalogi,audio,video,tagi,automatyka: TStrings;
    proces: pointer;
    nazwa_linku,nazwa_pliku,nazwa_katalogu: string;
    nazwa_linku2,nazwa_pliku2,nazwa_katalogu2: string;
    itag,itag2: integer;
    pozycja: integer;
    predkosc_str: string;
    kod: array [0..9] of boolean;
    function GetDirAria: string;
    function GetDirYtDlp: string;
    function GetDirYtDl: string;
    procedure SetDirAria(AValue: string);
    procedure SetDirYtDlp(AValue: string);
    procedure SetDirYtDl(AValue: string);
    procedure AddCode(aCode: integer);
    procedure ReceivedVerbose(ASender: TObject);
    procedure TerminateAria;
  protected
  public
    watek_timer: TTimer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DownloadInfo(aLink: string; aAudio: TStrings = nil; aVideo: TStrings = nil);
    procedure GetInformationsForAll(aLink: string; var aTitle,aDescription,aKeywords: string);
    procedure GetInformationsForYoutube(aLink: string; var aTitle,aDescription,aKeywords: string);
    procedure GetInformationsForRumble(aLink: string; var aTitle,aDescription: string);
    procedure AddLink(aLink,aDir: string; aAudioNr: integer = 0; aVideoNr: integer = 0; aTag: integer = 0);
    procedure Clear;
    procedure Terminate;
  published
    {Engine:
      enDefault  - youtube-dl
      enDefBoost - youtube-dl + aria2
      enDefPlus  - yt-dlp}
    property Engine: TYoutubeDownloaderEngine read FEngine write FEngine default enDefault;
    {Wykonywanie kodu w wątkach:
        TRUE - ściąganie w wątkach
       FALSE - ściąganie w procesie głównym}
    property ThreadsProcess: boolean read FWatki default true;
    {Path to Directory executed program:
       youtube-dl | youtube-dl.exe
     OR flags:
     <auto> - System Path Search Program's
     <curr> - Current Program Directory}
    property DirectoryYoutubeDl: string read FDirYtDl write SetDirYtDl;
    {Path to Directory executed program:
       aria2c | aria2c.exe
     OR flags:
     <auto> - System Path Search Program's
     <curr> - Current Program Directory}
    property DirectoryAria2: string read FDirAria write SetDirAria;
    {Path to Directory executed program:
       yt-dlp | yt-dlp.exe
     OR flags:
     <auto> - System Path Search Program's
     <curr> - Current Program Directory}
    property DirectoryYtDlp: string read FDirYtDlp write SetDirYtDlp;
    {Path to Cookie File in format:
       Netscape HTTP File Cookie}
    property PathToCookieFile: TFilename read FCookieFile write FCookieFile;
    {Reading records on execute:
       DownloadInfo(link)}
    property ReadPozInfo: boolean read FBoolReadPozInfo write FBoolReadPozInfo default false;
    {Auto select file in values:
       Min... and Max... Positions.
     Aby to działało kody audio i video muszą być ustawione na 0,
     w przeciwnym wypadku zadziała manualnie!}
    property AutoSelect: boolean read FAutoSelect write FAutoSelect default false;
    {Max audio bitrate in kbit.
     Examples: 51,66,138}
    property MaxAudioBitRate: integer read FMaxAudioBitRate write FMaxAudioBitRate default 0;
    {Min audio samplerate in HZ.
     Examples: 11025,22050,44100,48000}
    property MinAudioSampleRate: integer read FMinAudioSampleRate write FMinAudioSampleRate default 0;
    {Max audio samplerate in HZ.
     Examples: 11025,22050,44100,48000}
    property MaxAudioSampleRate: integer read FMaxAudioSampleRate write FMaxAudioSampleRate default 0;
    {Max video quality in p.
     Examples: 144,240,360,480,720,1080,
               or 0..maxint}
    property MaxVideoQuality: integer read FMaxVideoQuality write FMaxVideoQuality default 0;
    {Max video bitrate in kbit.
     Examples: 18,60,70,156,307}
    property MaxVideoBitRate: integer read FMaxVideoBitRate write FMaxVideoBitRate default 0;
    {Reading records on execute:
       DownloadInfo(link)}
    property OnReadPozInfo: TYoutubeDownloaderReadPozInfo read FReadPozInfo write FReadPozInfo;
    {Wątek wystartował}
    property OnStart: TNotifyEvent read FStart write FStart;
    {Wątek się zamyka}
    property OnStop: TNotifyEvent read FStop write FStop;
    {Wątek zaczyna pobierać plik}
    property OnDlStart: TYoutubeDownloaderDlStart read FDlStart write FDlStart;
    {Pobieram plik - dostałem nazwę pobieranego pliku}
    property OnDlFileName: TYoutubeDownloaderDlGetFileName read FDlFileName write FDlFileName;
    {Pobieram plik - dostałem pozycję ściąganego pliku
     Zakres: 0 - 1000}
    property OnDlPosition: TYoutubeDownloaderDlGetPosition read FDlPosition write FDlPosition;
    {Plik został pobrany}
    property OnDlFinish: TYoutubeDownloaderDlFinish read FDlFinish write FDlFinish;
  end;

  { TYoutubeDownloaderWatekYoutube }

  TYoutubeDownloaderWatekYoutube = class(TThread)
  private
    sender: TYoutubeDownloader;
    engine: TYoutubeDownloaderEngine;
    auto_select: boolean;
    YTData: TAsyncProcess;
    dir_youtubedl,dir_aria2c: string;
    cookiesfile: string;
    tag,tag2: integer;
    automatyka: string;
    kod_verbose: integer;
    link,nazwa_pliku,directory: string;
    link2,nazwa_pliku2,directory2: string;
    audio,video: integer;
    plik1,plik2: string;
    zrobione: boolean;
    pozycja: integer;
    predkosc_str: string;
    sciagam: boolean;
    czas_reakcji: integer;

    //film: integer;
    fs: TFormatSettings;
    procedure verbose; //żądanie wykonania kodu po stronie aplikacji!
    procedure wykonaj;
    procedure pobierz;
    procedure Execute; override;
    procedure YTReadData(aSender: TObject);
  protected
  public
    constructor Create(aSender: TYoutubeDownloader; aEngine: TYoutubeDownloaderEngine; aBinaryDir,aBinaryAria2c,aCookieFile: string);
    destructor Destroy; override;
  published
  end;

procedure Register;

//procedure local_DownloadInfo(aLink: string; aDirBin, aCookieFile: string;
//  maxAPrzeplywnosc, minASampleRate, maxASampleRate, maxVRes, maxVPrzeplywnosc: integer;
//  var outAudioCode,outVideoCode: integer);

implementation

uses
  ecode_unit, lcltype;

procedure Register;
begin
  {$I youtubedownloader_icon.lrs}
  RegisterComponents('lNet',[TYoutubeDownloader]);
end;

procedure local_del(ss: TStringList; col: integer);
var
  i,a,max: integer;
  s: string;
begin
  max:=0;
  (* szukam największego *)
  for i:=0 to ss.Count-1 do
  begin
    s:=ss[i];
    a:=StrToInt(GetLineToStr(s,col,','));
    if a>max then max:=a;
  end;
  (* usuwam mniejsze *)
  for i:=ss.Count-1 downto 0 do
  begin
    s:=ss[i];
    a:=StrToInt(GetLineToStr(s,col,','));
    if a<max then ss.Delete(i);
  end;
end;

procedure local_DownloadInfo(aLink: string; aDirBin, aCookieFile: string;
  maxAPrzeplywnosc, minASampleRate, maxASampleRate, maxVRes, maxVPrzeplywnosc: integer;
  var outAudioCode,outVideoCode: integer);
var
  YTData: TProcess;
  str: TStringList;
  l,i,j: integer;
  ss: array [1..6] of string;
  p,s,reszta,s1: string;
  ba,bv,bav: boolean;
  vFormatCode: integer;
  vExtension,vResolution: string;
  vRes,vPrzeplywnosc,vFPS,vSampleRate: integer;
  vSize: int64;
  d: double;
  vType: TYoutubeDownloaderType;
  vDASH: boolean;
  audio,video,video2: TStringList;
begin
  (* sprawdzam jaki serwis i przekazuję wywołanie dalej *)
  if (pos('//www.youtube.com/',aLink)=0) and (pos('//youtu.be/',aLink)=0) and (pos('//youtube.com/',aLink)=0) then
  begin
    outAudioCode:=0;
    outVideoCode:=0;
    exit;
  end;
  (* reszta *)
  YTData:=TProcess.Create(nil);
  YTData.Executable:='youtube-dl';
  YTData.CurrentDirectory:=aDirBin;
  YTData.Options:=[poUsePipes,poNoConsole,poWaitOnExit];
  YTData.Priority:=ppNormal;
  YTData.ShowWindow:=swoNone;
  YTData.PipeBufferSize:=2*1024;
  try
    if aCookieFile<>'' then
    begin
      YTData.Parameters.Add('--cookies');
      YTData.Parameters.Add(aCookieFile);
    end;
    YTData.Parameters.Add('-F');
    YTData.Parameters.Add(aLink);
    YTData.CurrentDirectory:='';
    YTData.Execute;
    str:=TStringList.Create;
    audio:=TStringList.Create;
    video:=TStringList.Create;
    video2:=TStringList.Create;
    try
      if YTData.Output.NumBytesAvailable>0 then
      begin
        str.LoadFromStream(YTData.Output);
        for i:=0 to str.Count-1 do
        begin
          ba:=true;
          bv:=true;
          s:=str[i];
          if s='' then continue;
          s:=StringReplace(s,'audio only','audio_only',[rfReplaceAll]);
          s:=StringReplace(s,'video only','video_only',[rfReplaceAll]);
          vDASH:=pos('DASH',s)>0;
          s:=StringReplace(s,'DASH audio','tiny',[rfReplaceAll]);
          s:=StringReplace(s,'DASH video','0p',[rfReplaceAll]);
          s:=StringReplace(s,' , ',', ',[rfReplaceAll]);
          if pos('audio_only',s)>0 then bv:=false else if pos('video_only',s)>0 then ba:=false;
          bav:=ba and bv;
          if bav then vType:=ytAll else if ba then vType:=ytAudio else vType:=ytVideo;
          while pos('  ',s)>0 do s:=StringReplace(s,'  ',' ',[rfReplaceAll]);
          s1:=GetLineToStr(s,1,' ');
          if (s1='[youtube]') or (s1='[info]') or (s1='format') then continue;

          (* czytam części między przecinkami *)
          ss[1]:=trim(GetLineToStr(s,1,','));
          ss[2]:=trim(GetLineToStr(s,2,','));
          ss[3]:=trim(GetLineToStr(s,3,','));
          ss[4]:=trim(GetLineToStr(s,4,','));
          ss[5]:=trim(GetLineToStr(s,5,','));
          ss[6]:=trim(GetLineToStr(s,6,','));
          (* czytam wartości *)
          vFormatCode:=StrToInt(trim(GetLineToStr(ss[1],1,' ')));
          vExtension:=trim(GetLineToStr(ss[1],2,' '));
          for j:=6 downto 5 do
          begin
            p:=trim(GetLineToStr(ss[1],j,' '));
            l:=length(p);
            if l=0 then continue;
            if p[l]='k' then delete(p,l,1);
            break;
          end;
          vPrzeplywnosc:=StrToInt(p);
          if bv then
          begin
            vResolution:=trim(GetLineToStr(ss[1],3,' '));
            p:=trim(GetLineToStr(ss[1],4,' '));
            //l:=length(p);
            //if p[l]='p' then delete(p,l,1);
            //vRes:=StrToInt(p);
            vRes:=StrToL(p,reszta,0);
            if bav then p:=trim(ss[3]) else p:=trim(ss[4]);
            p:=StringReplace(p,'fps','',[]);
            vFPS:=StrToInt(p);
            if bav then
            begin
              p:=trim(ss[4]);
              p:=StringReplace(p,') (',',',[]);
              p:=StringReplace(p,'(',',',[]);
              p:=StringReplace(p,')',',',[]);
              p:=GetLineToStr(p,2,',');
              p:=StringReplace(p,'Hz','',[]);
              p:=trim(p);
              vSampleRate:=StrToInt(p);
              p:=trim(ss[5]);
              if p='' then vSize:=0 else
              begin
                p:=upcase(p);
                d:=StrToD(p,reszta);
                vSize:=trunc(d*1024);
                if reszta='MIB' then vSize:=vSize*1024 else
                if reszta ='KIB' then vSize:=vSize;
              end;
            end else begin
              vSampleRate:=0;
              p:=trim(ss[6]);
              if p='' then vSize:=0 else
              begin
                p:=upcase(p);
                d:=StrToD(p,reszta);
                vSize:=trunc(d*1024);
                if reszta='MIB' then vSize:=vSize*1024 else
                if reszta ='KIB' then vSize:=vSize;
              end;
            end;
            if vDash and (vRes=0) then vRes:=StrToInt(GetLineToStr(vResolution,2,'x'));
          end else begin
            vResolution:='';
            vRes:=0;
            vFPS:=0;
            p:=trim(ss[3]);
            p:=StringReplace(p,'(',',',[]);
            p:=StringReplace(p,')',',',[]);
            p:=GetLineToStr(p,2,',');
            p:=StringReplace(p,'Hz','',[]);
            p:=trim(p);
            vSampleRate:=StrToInt(p);
            p:=trim(ss[4]);
            if p='' then vSize:=0 else
            begin
              p:=upcase(p);
              d:=StrToD(p,reszta);
              vSize:=trunc(d*1024);
              if reszta='MIB' then vSize:=vSize*1024 else
              if reszta ='KIB' then vSize:=vSize;
            end;
          end;
          if (maxAPrzeplywnosc>0) and (vType=ytAudio) and (vPrzeplywnosc>maxAPrzeplywnosc) then continue;
          if (minASampleRate>0) and (vType=ytAudio) and (vSampleRate<minASampleRate) then continue;
          if (maxASampleRate>0) and (vType=ytAudio) and (vSampleRate>maxASampleRate) then continue;
          if (maxVRes>0) and ((vType=ytVideo) or (vType=ytAll)) and (vRes>maxVRes) then continue;
          if (maxVPrzeplywnosc>0) and ((vType=ytVideo) or (vType=ytAll)) and (vPrzeplywnosc>maxVPrzeplywnosc) then continue;
          if vType=ytAudio then audio.Add(IntToStr(vFormatCode)+','+IntToStr(vPrzeplywnosc)+','+IntToStr(vSampleRate)) else
          if vType=ytVideo then video.Add(IntToStr(vFormatCode)+','+IntToStr(vRes)+','+IntToStr(vPrzeplywnosc)+','+IntToStr(vFPS)) else
                                video2.Add(IntToStr(vFormatCode)+','+IntToStr(vRes)+','+IntToStr(vPrzeplywnosc)+','+IntToStr(vFPS));
          //writeln('Type=',vType,', Code=',vFormatCode,', Ext=',vExtension,', Resolution=',vResolution,', Res=',vRes,', Przepływ=',vPrzeplywnosc,', FPS=',vFPS,', SR=',vSampleRate,', Size=',vSize,', DASH=',vDASH);

        end;
        local_del(audio,3);
        local_del(video,4);
        local_del(video2,4);

        (* wybieram pozycje do ściągnięcia *)
        if (audio.Count>0) or (video.Count>0) then
        begin
          outAudioCode:=StrToInt(GetLineToStr(audio[audio.Count-1],1,','));
          outVideoCode:=StrToInt(GetLineToStr(video[video.Count-1],1,','));
        end else
        if video2.Count>0 then
        begin
          outAudioCode:=0;
          outVideoCode:=StrToInt(GetLineToStr(video2[video2.Count-1],1,','));
        end else begin
          outAudioCode:=0;
          outVideoCode:=0;
        end;

      end;
    finally
      str.Free;
      audio.Free;
      video.Free;
      video2.Free;
    end;
  finally
    YTData.Free;
  end;
end;

{ TYoutubeDownloader }

procedure TYoutubeDownloader.SetDirYtDl(AValue: string);
begin
  if FDirYtDl=AValue then Exit;
  if (AValue='') and (FDirYtDl='<auto>') then exit;
  if AValue='' then FDirYtDl:='<auto>' else FDirYtDl:=AValue;
end;

procedure TYoutubeDownloader.AddCode(aCode: integer);
begin
  kod[aCode]:=true;
  watek_timer.Enabled:=true;
end;

function StrToLinuxInt64(aStr: string): int64;
var
  s: string;
  d: double;
  reszta: string;
  vSize: int64;
begin
  s:=upcase(aStr);
  d:=StrToD(s,reszta);
  vSize:=trunc(d*1024);
  if (reszta='MIB') or (reszta='MIB/S') then vSize:=vSize*1024 else
  if (reszta='KIB') or (reszta='KIB/S') then vSize:=vSize;
  result:=vSize;
end;

procedure TYoutubeDownloader.ReceivedVerbose(ASender: TObject);
var
  i: integer;
begin
  watek_timer.Enabled:=false;
  if kod[0] then
  begin
    proces:=nil;
    if assigned(FStop) then FStop(self); //wątek się zamyka
    for i:=0 to 8 do kod[i]:=false;
  end;
  if kod[1] then
  begin
    if assigned(FStart) then FStart(self); //wątek wystartował
    kod[1]:=false;
  end;
  if kod[2] then
  begin
    if assigned(FDlStart) then FDlStart(nazwa_linku,nazwa_katalogu,itag); //ściąganie pliku - dostałem link
    kod[2]:=false;
  end;
  if kod[3] then
  begin
    if assigned(FDlFileName) then FDlFileName(nazwa_pliku,nazwa_katalogu,itag); //ściąganie pliku - dostałem nazwę ściąganego pliku
    kod[3]:=false;
  end;
  if kod[4] then
  begin
    if assigned(FDlPosition) then FDlPosition(pozycja,StrToLinuxInt64(predkosc_str),itag); //ściąganie pliku - dostałem pozycję ściąganego pliku
    kod[4]:=false;
  end;
  if kod[9] then
  begin
    if assigned(FDlFinish) then FDlFinish(nazwa_linku2,nazwa_pliku2,nazwa_katalogu2,itag2); //plik został pobrany
    kod[9]:=false;
  end;
end;

procedure TYoutubeDownloader.TerminateAria;
var
  a: TProcess;
begin
  a:=TAsyncProcess.Create(self);
  try
    a.Executable:='killall';
    a.Options:=[poWaitOnExit,poNoConsole];
    a.Priority:=ppNormal;
    a.ShowWindow:=swoNone;
    a.Parameters.Add('aria2c');
    a.Execute;
  finally
    a.Terminate(0);
    a.Free;
  end;
end;

function TYoutubeDownloader.GetDirYtDl: string;
var
  s: string;
begin
  if FDirYtDl='<auto>' then result:='' else
  if FDirYtDl='<curr>' then
  begin
    s:=ExtractFilePath(ParamStr(0));
    delete(s,length(s),1);
    result:=s;
  end else
  result:=FDirYtDl;
end;

function TYoutubeDownloader.GetDirAria: string;
var
  s: string;
begin
  if FDirAria='<auto>' then result:='' else
  if FDirAria='<curr>' then
  begin
    s:=ExtractFilePath(ParamStr(0));
    delete(s,length(s),1);
    result:=s;
  end else
  result:=FDirAria;
end;

function TYoutubeDownloader.GetDirYtDlp: string;
var
  s: string;
begin
  if FDirYtDlp='<auto>' then result:='' else
  if FDirYtDlp='<curr>' then
  begin
    s:=ExtractFilePath(ParamStr(0));
    delete(s,length(s),1);
    result:=s;
  end else
  result:=FDirYtDlp;
end;

procedure TYoutubeDownloader.SetDirAria(AValue: string);
begin
  if FDirAria=AValue then Exit;
  if (AValue='') and (FDirAria='<auto>') then exit;
  if AValue='' then FDirAria:='<auto>' else FDirAria:=AValue;
end;

procedure TYoutubeDownloader.SetDirYtDlp(AValue: string);
begin
  if FDirYtDlp=AValue then Exit;
  if (AValue='') and (FDirYtDlp='<auto>') then exit;
  if AValue='' then FDirYtDlp:='<auto>' else FDirYtDlp:=AValue;
end;

constructor TYoutubeDownloader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  (* obiekty *)
  proces:=nil;
  linki:=TStringList.Create;
  katalogi:=TStringList.Create;
  audio:=TStringList.Create;
  video:=TStringList.Create;
  tagi:=TStringList.Create;
  automatyka:=TStringList.Create;
  watek_timer:=TTimer.Create(self);
  watek_timer.Enabled:=false;
  watek_timer.Interval:=1;
  watek_timer.OnTimer:=@ReceivedVerbose;
  http:=TNetSynHTTP.Create(self);
  (* zmienne *)
  mem_yt_cookie:='';
  FEngine:=enDefault;
  FWatki:=true;
  FDirYtDl:='<auto>';
  FDirAria:='<auto>';
  FDirYtDlp:='<auto>';
  FBoolReadPozInfo:=false;
  FAutoSelect:=false;
  FMaxAudioBitRate:=0;
  FMaxAudioSampleRate:=0;
  FMaxVideoBitRate:=0;
  FMaxVideoQuality:=0;
  FMinAudioSampleRate:=0;
end;

destructor TYoutubeDownloader.Destroy;
begin
  if proces<>nil then
  begin
    TYoutubeDownloaderWatekYoutube(proces).Terminate;
    if FEngine=enDefBoost then TerminateAria;
  end;
  linki.Free;
  katalogi.Free;
  audio.Free;
  video.Free;
  tagi.Free;
  automatyka.Free;
  watek_timer.Free;
  http.Free;
  inherited Destroy;
end;

procedure TYoutubeDownloader.DownloadInfo(aLink: string; aAudio: TStrings;
  aVideo: TStrings);
var
  YTData: TProcess;
  str: TStringList;
  l,i,j: integer;
  ss: array [1..6] of string;
  p,pom,s,reszta,s1: string;
  ba,bv,bav: boolean;
  vFormatCode: integer;
  vExtension,vResolution: string;
  vRes,vPrzeplywnosc,vFPS,vSampleRate: integer;
  vSize: int64;
  d: double;
  vType: TYoutubeDownloaderType;
  vDASH: boolean;
begin
  if aAudio<>nil then aAudio.Clear;
  if aVideo<>nil then aVideo.Clear;
  YTData:=TProcess.Create(nil);
  YTData.Executable:='youtube-dl';
  YTData.CurrentDirectory:=GetDirYtDl;
  YTData.Options:=[poUsePipes,poNoConsole,poWaitOnExit];
  YTData.Priority:=ppNormal;
  YTData.ShowWindow:=swoNone;
  YTData.PipeBufferSize:=2*1024;
  try
    if FCookieFile<>'' then
    begin
      YTData.Parameters.Add('--cookies');
      YTData.Parameters.Add(FCookieFile);
    end;
    YTData.Parameters.Add('-F');
    YTData.Parameters.Add(aLink);
    YTData.CurrentDirectory:='';
    YTData.Execute;
    str:=TStringList.Create;
    try
      if YTData.Output.NumBytesAvailable>0 then
      begin
        str.LoadFromStream(YTData.Output);
        for i:=0 to str.Count-1 do
        begin
          ba:=true;
          bv:=true;
          s:=str[i];
          if s='' then continue;
          pom:=s;
          s:=StringReplace(s,'audio only','audio_only',[rfReplaceAll]);
          s:=StringReplace(s,'video only','video_only',[rfReplaceAll]);
          vDASH:=pos('DASH',s)>0;
          s:=StringReplace(s,'DASH audio','tiny',[rfReplaceAll]);
          s:=StringReplace(s,'DASH video','0p',[rfReplaceAll]);
          s:=StringReplace(s,' , ',', ',[rfReplaceAll]);
          if pos('audio_only',s)>0 then bv:=false else if pos('video_only',s)>0 then ba:=false;
          bav:=ba and bv;
          if bav then vType:=ytAll else if ba then vType:=ytAudio else vType:=ytVideo;
          while pos('  ',s)>0 do s:=StringReplace(s,'  ',' ',[rfReplaceAll]);
          s1:=GetLineToStr(s,1,' ');
          if (s1='[youtube]') or (s1='[info]') or (s1='format') then continue;
          if bv then
          begin
            if aVideo<>nil then aVideo.Add(pom);
          end else begin
            if aAudio<>nil then aAudio.Add(pom);
          end;

          if FBoolReadPozInfo then
          begin
            (* czytam części między przecinkami *)
            ss[1]:=trim(GetLineToStr(s,1,','));
            ss[2]:=trim(GetLineToStr(s,2,','));
            ss[3]:=trim(GetLineToStr(s,3,','));
            ss[4]:=trim(GetLineToStr(s,4,','));
            ss[5]:=trim(GetLineToStr(s,5,','));
            ss[6]:=trim(GetLineToStr(s,6,','));
            (* czytam wartości *)
            vFormatCode:=StrToInt(trim(GetLineToStr(ss[1],1,' ')));
            vExtension:=trim(GetLineToStr(ss[1],2,' '));
            for j:=6 downto 5 do
            begin
              p:=trim(GetLineToStr(ss[1],j,' '));
              l:=length(p);
              if l=0 then continue;
              if p[l]='k' then delete(p,l,1);
              break;
            end;
            vPrzeplywnosc:=StrToInt(p);
            if bv then
            begin
              vResolution:=trim(GetLineToStr(ss[1],3,' '));
              p:=trim(GetLineToStr(ss[1],4,' '));
              //l:=length(p);
              //if p[l]='p' then delete(p,l,1);
              //vRes:=StrToInt(p);
              vRes:=StrToL(p,reszta,0);
              if bav then p:=trim(ss[3]) else p:=trim(ss[4]);
              p:=StringReplace(p,'fps','',[]);
              vFPS:=StrToInt(p);
              if bav then
              begin
                p:=trim(ss[4]);
                p:=StringReplace(p,') (',',',[]);
                p:=StringReplace(p,'(',',',[]);
                p:=StringReplace(p,')',',',[]);
                p:=GetLineToStr(p,2,',');
                p:=StringReplace(p,'Hz','',[]);
                p:=trim(p);
                vSampleRate:=StrToInt(p);
                p:=trim(ss[5]);
                if p='' then vSize:=0 else
                begin
                  p:=upcase(p);
                  d:=StrToD(p,reszta);
                  vSize:=trunc(d*1024);
                  if reszta='MIB' then vSize:=vSize*1024 else
                  if reszta ='KIB' then vSize:=vSize;
                end;
              end else begin
                vSampleRate:=0;
                p:=trim(ss[6]);
                if p='' then vSize:=0 else
                begin
                  p:=upcase(p);
                  d:=StrToD(p,reszta);
                  vSize:=trunc(d*1024);
                  if reszta='MIB' then vSize:=vSize*1024 else
                  if reszta ='KIB' then vSize:=vSize;
                end;
              end;
              if vDash and (vRes=0) then vRes:=StrToInt(GetLineToStr(vResolution,2,'x'));
            end else begin
              vResolution:='';
              vRes:=0;
              vFPS:=0;
              p:=trim(ss[3]);
              p:=StringReplace(p,'(',',',[]);
              p:=StringReplace(p,')',',',[]);
              p:=GetLineToStr(p,2,',');
              p:=StringReplace(p,'Hz','',[]);
              p:=trim(p);
              vSampleRate:=StrToInt(p);
              p:=trim(ss[4]);
              if p='' then vSize:=0 else
              begin
                p:=upcase(p);
                d:=StrToD(p,reszta);
                vSize:=trunc(d*1024);
                if reszta='MIB' then vSize:=vSize*1024 else
                if reszta ='KIB' then vSize:=vSize;
              end;
            end;
            if assigned(FReadPozInfo) then FReadPozInfo(vType,vFormatCode,vExtension,vResolution,vRes,vPrzeplywnosc,vFPS,vSampleRate,vSize,vDASH);
          end;

        end;
      end;
    finally
      str.Free;
    end;
  finally
    YTData.Free;
  end;
end;

procedure TYoutubeDownloader.GetInformationsForAll(aLink: string; var aTitle,
  aDescription, aKeywords: string);
begin
  aTitle:='';
  aDescription:='';
  aKeywords:='';
  (* sprawdzam jaki serwis i przekazuję wywołanie dalej *)
  if pos('//rumble.com/',aLink)>0 then GetInformationsForRumble(aLink,aTitle,aDescription) else
  if pos('//www.youtube.com/',aLink)>0 then GetInformationsForYoutube(aLink,aTitle,aDescription,aKeywords) else
  if pos('//youtu.be/',aLink)>0 then GetInformationsForYoutube(aLink,aTitle,aDescription,aKeywords) else
  if pos('//youtube.com/',aLink)>0 then GetInformationsForYoutube(aLink,aTitle,aDescription,aKeywords);
end;

procedure TYoutubeDownloader.GetInformationsForYoutube(aLink: string;
  var aTitle, aDescription, aKeywords: string);
var
  ss: TStrings;
  s,s1,s2,cookie: string;
  a,i: integer;
begin
  http.Headers.Clear;
  if FCookieFile<>'' then
  begin
    if mem_yt_cookie='' then
    begin
      ss:=TStringList.Create;
      try
        (* dołączam dane cookies do połączenia jeśli istnieją*)
        if FileExists(FCookieFile) then
        begin
          cookie:='';
          ss.LoadFromFile(FCookieFile);
          for i:=0 to ss.Count-1 do
          begin
            s:=ss[i];
            s1:=GetLineToStr(s,6,#9);
            s2:=GetLineToStr(s,7,#9);
            if cookie='' then cookie:='cookie: '+s1+'='+s2 else cookie:=cookie+'; '+s1+'='+s2;
          end;
          mem_yt_cookie:=cookie
        end;
      finally
        ss.Free;
      end;
    end;
    http.Headers.Add(cookie);
  end;

  http.execute(aLink,s);

  http.StrDeleteStart(s,'meta name="title"');
  http.StrDeleteStart(s,'content="');
  a:=pos('"',s);
  aTitle:=copy(s,1,a-1);
  aTitle:=DecodeHTMLAmp(aTitle);

  http.StrDeleteStart(s,'meta name="description"');
  http.StrDeleteStart(s,'content="');
  a:=pos('"',s);
  aDescription:=copy(s,1,a-1);
  aDescription:=DecodeHTMLAmp(aDescription);

  http.StrDeleteStart(s,'meta name="keywords"');
  http.StrDeleteStart(s,'content="');
  a:=pos('"',s);
  aKeywords:=copy(s,1,a-1);
  aKeywords:=DecodeHTMLAmp(aKeywords);
end;

procedure TYoutubeDownloader.GetInformationsForRumble(aLink: string;
  var aTitle, aDescription: string);
var
  s,s1: string;
  a: integer;
begin
  http.Headers.Clear;
  http.execute(aLink,s);
  s1:=s;

  http.StrDeleteStart(s,'property=og:title');
  http.StrDeleteStart(s,'content="');
  a:=pos('"',s);
  aTitle:=copy(s,1,a-1);
  aTitle:=DecodeHTMLAmp(aTitle);

  http.StrDeleteStart(s,'meta name=description');
  http.StrDeleteStart(s,'content="');
  a:=pos('"',s);
  aDescription:=copy(s,1,a-1);
  aDescription:=DecodeHTMLAmp(aDescription);

  {http.StrDeleteStart(s,'meta name="keywords"');
  http.StrDeleteStart(s,'content="');
  a:=pos('"',s);
  aKeywords:=copy(s,1,a-1);
  aKeywords:=DecodeHTMLAmp(aKeywords);}

end;

procedure TYoutubeDownloader.AddLink(aLink, aDir: string; aAudioNr: integer;
  aVideoNr: integer; aTag: integer);
var
  a: TYoutubeDownloaderWatekYoutube;
  i: integer;
begin
  linki.Add(aLink);
  katalogi.Add(aDir);
  audio.Add(IntToStr(aAudioNr));
  video.Add(IntToStr(aVideoNr));
  tagi.Add(IntToStr(aTag));
  automatyka.Add('');
  if proces=nil then
  begin
    for i:=0 to 9 do kod[i]:=false;
    case FEngine of
      enDefault:  a:=TYoutubeDownloaderWatekYoutube.Create(self,FEngine,GetDirYtDl,'',FCookieFile);
      enDefBoost: a:=TYoutubeDownloaderWatekYoutube.Create(self,FEngine,GetDirYtDl,GetDirAria,FCookieFile);
      enDefPlus:  a:=TYoutubeDownloaderWatekYoutube.Create(self,FEngine,GetDirYtDlp,'',FCookieFile);
    end;
    proces:=a;
  end;
end;

procedure TYoutubeDownloader.Clear;
begin
  linki.Clear;
  katalogi.Clear;
  audio.Clear;
  video.Clear;
end;

procedure TYoutubeDownloader.Terminate;
begin
  if proces<>nil then TYoutubeDownloaderWatekYoutube(proces).Terminate;
  if FEngine=enDefBoost then TerminateAria;
end;

{ TYoutubeDownloaderWatekYoutube }

procedure TYoutubeDownloaderWatekYoutube.verbose;
begin
  sender.itag:=tag;
  if kod_verbose=2 then
  begin
    sender.nazwa_linku:=link;
    sender.nazwa_katalogu:=directory;
  end else
  if kod_verbose=3 then sender.nazwa_pliku:=nazwa_pliku else
  if kod_verbose=4 then
  begin
    sender.pozycja:=pozycja;
    sender.predkosc_str:=predkosc_str;
  end else
  if kod_verbose=9 then
  begin
    sender.itag2:=tag2;
    sender.nazwa_linku2:=link2;
    sender.nazwa_pliku2:=nazwa_pliku2;
    sender.nazwa_katalogu2:=directory2;
  end;
  sender.AddCode(kod_verbose);
end;

procedure TYoutubeDownloaderWatekYoutube.wykonaj;
var
  s,s1: string;
begin
  zrobione:=false;
  plik1:='';
  plik2:='';
  s:='';
  kod_verbose:=2;
  synchronize(@verbose);
  YTData.Parameters.Clear;
  if (audio>0) and (video>0) then
  begin
    if video<audio then s:=IntToStr(video)+'+'+IntToStr(audio) else s:=IntToStr(audio)+'+'+IntToStr(video);
  end else
  if (audio=0) and (video>0) then s:=IntToStr(video) else
  if (audio>0) and (video=0) then s:=IntToStr(audio);
  if s<>'' then
  begin
    YTData.Parameters.Add('-f');
    YTData.Parameters.Add(s);
  end;
  if cookiesfile<>'' then
  begin
    YTData.Parameters.Add('--cookies');
    YTData.Parameters.Add(cookiesfile);
  end;
  YTData.Parameters.Add(link);
  if engine=enDefBoost then
  begin
    s:=' --summary-interval=1';
    s1:=dir_aria2c;
    {$IFDEF UNIX}
    if s1<>'' then s1:=s1+'/';
    {$ELSE}
    if s1<>'' then s1:=s1+'\';
    {$ENDIF}
    YTData.Parameters.Add('--external-downloader='+s1+'aria2c');
    YTData.Parameters.Add('--external-downloader-args');
    if cookiesfile='' then
      YTData.Parameters.Add('--min-split-size=1M --max-connection-per-server=16 --max-concurrent-downloads=16 --split=16'+s)
    else
      YTData.Parameters.Add('--min-split-size=1M --max-connection-per-server=16 --max-concurrent-downloads=16 --split=16 --load-cookies='+cookiesfile+s);
  end;
  YTData.CurrentDirectory:=directory;
  YTData.Execute;

  czas_reakcji:=TimeToInteger;
  //while (czas_reakcji+10000>TimeToInteger) and (YTData.ExitStatus<>0) and YTData.Running and (not zrobione) and (not self.Terminated) do sleep(500);
  while (czas_reakcji+10000>TimeToInteger) and (YTData.ExitStatus<>0) and YTData.Running and (not self.Terminated) do sleep(500);
  if YTData.Running then
  begin
    sleep(1000);
    YTData.Terminate(0);
  end;

  if sciagam then
  begin
    sciagam:=false;
    link2:=link;
    nazwa_pliku2:=nazwa_pliku;
    directory2:=directory;
    tag2:=tag;
    kod_verbose:=9;
    synchronize(@verbose);
  end;
end;

procedure TYoutubeDownloaderWatekYoutube.pobierz;
begin
  {pobieram parametry pliku do pobrania}
  if sender.linki.Count=0 then
  begin
    auto_select:=false;
    link:='';
    directory:='';
    audio:=0;
    video:=0;
    tag:=0;
    automatyka:='';
  end else begin
    auto_select:=sender.FAutoSelect;
    link:=sender.linki[0];
    directory:=sender.katalogi[0];
    if directory='' then directory:=dir_youtubedl;
    audio:=StrToInt(sender.audio[0]);
    video:=StrToInt(sender.video[0]);
    tag:=StrToInt(sender.tagi[0]);
    automatyka:=sender.automatyka[0];
    sender.linki.Delete(0);
    sender.katalogi.Delete(0);
    sender.audio.Delete(0);
    sender.video.Delete(0);
    sender.tagi.Delete(0);
    sender.automatyka.Delete(0);
  end;
end;

(*
  kod_verbose:
    1 - wątek odpalony
    0 - wątek wygaszony
  kody pracy:
    2 - rozpoczynam ściąganie pliku
    3 - ściągam plik - dostałem nazwę pliku
    4 - ściągam plik - dostałem postęp pobierania
*)

procedure TYoutubeDownloaderWatekYoutube.Execute;
begin
  kod_verbose:=1; //wątek odpalony - zaczynam pracę!
  synchronize(@verbose);
  sleep(10);
  {pobieram dane}
  while true do
  begin
    sciagam:=false;
    synchronize(@pobierz);
    if link='' then break;
    if self.Terminated then break;
    if (audio=0) and (video=0) and auto_select then local_DownloadInfo(link,dir_youtubedl,cookiesfile,sender.MaxAudioBitRate,sender.MinAudioSampleRate,sender.MaxAudioSampleRate,sender.MaxVideoQuality,sender.MaxVideoBitRate,audio,video);
    wykonaj;
    sleep(10);
  end;
  kod_verbose:=0; //wychodzę!
  synchronize(@verbose);
end;

procedure TYoutubeDownloaderWatekYoutube.YTReadData(aSender: TObject);
var
  s,s1: string;
  str: TStringList;
  i,a: integer;
begin
  czas_reakcji:=TimeToInteger;
  str:=TStringList.Create;
  try
    if YTData.Output.NumBytesAvailable>0 then
    begin
      str.LoadFromStream(YTData.Output);
      for i:=0 to str.Count-1 do
      begin
        s:=str[i];
        if s='' then continue;
        if engine=enDefBoost then
        begin
          if pos('FILE:',s)>0 then continue;
          if pos('========',s)>0 then continue;
          if pos('***',s)>0 then continue;
          if pos('--------',s)>0 then continue;
          if pos('NOTICE',s)>0 then continue;
        end;
        //writeln(s);
        if pos('already been downloaded and merged',s)>0 then
        begin
          (* plik pobrany już wcześniej *)
          zrobione:=true;
        end else
        if pos('Resuming download at',s)>0 then
        begin
          (* kontynuowanie pobierania - tu nie robię nic *)
        end else
        if pos('has already been downloaded',s)>0 then
        begin
          (* kontynuowanie pobierania - tu nie robię nic *)
        end else
        if pos('[download] Destination:',s)>0 then
        begin
          (* dostajemy nazwę pliku *)
          sciagam:=true;
          delete(s,1,23);
          s:=trim(StringReplace(s,'"','',[rfReplaceAll]));
          nazwa_pliku:=s;
          kod_verbose:=3;
          synchronize(@verbose);
          if plik1='' then plik1:=s else if plik2='' then plik2:=s;
        end else
        if pos('[download]',s)>0 then
        begin
          delete(s,1,10);
          s:=trim(StringReplace(s,'"','',[rfReplaceAll]));
          s1:=s;
          a:=pos('%',s);
          delete(s,a,maxint);
          pozycja:=round(StrToFloat(s,fs)*10);
          a:=pos('at',s1);
          delete(s1,1,a+1);
          a:=pos('ETA',s1);
          delete(s1,a,maxint);
          predkosc_str:=trim(s1);
          kod_verbose:=4;
          synchronize(@verbose);
        end else
        if (engine=enDefBoost) and (pos('%)',s)>0) and (pos('ETA:',s)>0) then
        begin
          s1:=s;
          a:=pos('(',s);
          delete(s,1,a);
          a:=pos('%',s);
          delete(s,a,maxint);
          //pozycja:=round(StrToFloat(s,fs)*10);
          pozycja:=StrToInt(s)*10;
          a:=pos('DL:',s1);
          delete(s1,1,a+2);
          a:=pos('ETA:',s1);
          delete(s1,a,maxint);
          predkosc_str:=trim(s1);
          kod_verbose:=4;
          synchronize(@verbose);
        end else
        if (pos('[ffmpeg] Merging formats into',s)>0) or (pos('[Merger] Merging formats into',s)>0) then
        begin
          delete(s,1,29);
          link2:=link;
          nazwa_pliku2:=trim(StringReplace(s,'"','',[rfReplaceAll]));
          directory2:=directory;
          tag2:=tag;
        end else
        if pos('Deleting original file',s)>0 then
        begin
          sciagam:=false;
          delete(s,1,22);
          s:=StringReplace(s,'"','',[rfReplaceAll]);
          a:=pos('(pass -k to keep)',s);
          delete(s,a,maxint);
          s:=trim(s);
          if plik1=s then plik1:='';
          if plik2=s then plik2:='';
          if (plik1='') and (plik2='') then zrobione:=true;
          kod_verbose:=9;
          synchronize(@verbose);
        end;
      end;
    end;
  finally
    str.Free;
  end;
end;

constructor TYoutubeDownloaderWatekYoutube.Create(aSender: TYoutubeDownloader;
  aEngine: TYoutubeDownloaderEngine; aBinaryDir, aBinaryAria2c,
  aCookieFile: string);
begin
  (* przekazuję zmienne *)
  sender:=aSender;
  engine:=aEngine;
  dir_youtubedl:=aBinaryDir;
  dir_aria2c:=aBinaryAria2c;
  cookiesfile:=aCookieFile;
  (* obsługa wątku *)
  FreeOnTerminate:=true;
  YTData:=TAsyncProcess.Create(nil);
  YTData.CurrentDirectory:=dir_youtubedl;
  case engine of
    enDefault,enDefBoost: YTData.Executable:='youtube-dl';
    enDefPlus:            YTData.Executable:='yt-dlp';
  end;
  YTData.Options:=[poUsePipes,poNoConsole];
  YTData.Priority:=ppNormal;
  YTData.ShowWindow:=swoNone;
  YTData.OnReadData:=@YTReadData;
  fs.DecimalSeparator:='.';
  inherited Create(false);
end;

destructor TYoutubeDownloaderWatekYoutube.Destroy;
begin
  if YTData.Running then YTData.Terminate(0);
  YTData.Free;
  inherited Destroy;
end;

end.
