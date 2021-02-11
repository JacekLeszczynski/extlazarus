{ LCL control for playing videos using mplayer under gtk2

  Copyright (C) 2009 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

Changes:
  2014-03-24  Changes for Microsoft Windows Compatibility and added Events
              for Mouse Actions/ Michael Koecher aka six1

  2014-06-21  Added OnFeedback/OnError events
              Added OnPlay, OnStop, OnPlaying events
              Expanded Timer code to track state of Player
              Added pausing_keep_force to all commands
                - simply requesting state information was enough to resume paused video
                - adding pausing_keep was insufficent
              Added Duration, Position
              Replaced StrToCmdParam with AnsiQuotedStr in Play
                - StrToCmdParam didn't work under Windows - wrapped filename in ', Windows needed "
              Persisted FCanvas outside of IDE to prevent painting issues when no file playing
              / Mike Thompson

  2014-06-24  Added FindMPlayerPath (Refactored code from Play)
  2014-06-28  Extended FindMPlayer to also look for mplayer in a subfolder of the exe
              Fixed painting issues when playing audio files (introduces a flicker on
                resize when playing video :-( )...
              Fixed repeated requests for volume in files that don't support volme
              Changed TProcessUTF8 population code in .Play from .CommandLine to
                use .Executable & .Parameters
                - incidently removed the need to use AnsiQuotedStr around Filename under Windows
              Added Rate (Fast Forward only, mplayer doesn't support rewind)
              Only request position updates every ON_PLAYING_INTERVAL
              Set Volume on Play
              Added GrabImage and OnGrabImage (delay before mplayer grabs image)
                - doesn't work well with some renderers (-glnosw for instance,
                  also inconsistently on -vo X11)
                - Capturing failed attempts in code will be hard, for now I'll
                  just ensure this is documented on the wiki (recommend -vo direct3d under win)
              / Mike Thompson
  2014-07-01  Discovered -identify to load stats (including Start Time)
              Moved set volume on play to the parameters
              Refactored TimerEvent to ensure OnPlay & OnPlaying are broadcast in correct sequence
              Added VideoInfo and AudioInfo (load values from -identify)
              Fixed Position for videos with embedded Start_Time
              Deprecated PlayerProcess (no need for it to be exposed anymore)
              Realised no need for StepForward/StepBack - can be implemented externally via Position
              Exposed OnMouseWheel and implemented wheelmouse scrolling through video in FullFeatured
                demo
              / Mike Thompson

TODO
              EXTENSIVE TESTING UNDER LINUX
                - Tested under Linus Mint 16 (MATE) with mplayer installed (not mplayer2)
              Consider descending control from TGraphicControl (instead of creating FCanvas)

NOTES
  2014-06-29  TProcessUTF8 is a thin wrapper over TProcess.  TProcess on Windows
                is not unicode aware, so there is currently an issue playing unicode
                filenames under windows.
                No easy apparent solution other than upgrading TProcess (win\process.inc).
}
unit MPlayerCtrl;

{$mode objfpc}{$H+}

{$ifdef Linux}
 {$ifndef LCLgtk2}
 {$error this unit only supports LCL under gtk2}
 {$endif}
{$endif}

interface

uses
  Classes, SysUtils, Controls, WSLCLClasses, LCLProc, LCLType, InterfaceBase,
  LResources, LMessages, Graphics, ExtCtrls, FileUtil, Process, AsyncProcess,
  LazFileUtils
  {$ifdef Linux}
  , gtk2int, gtk2, glib2, gdk2x, Gtk2WSControls, GTK2Proc, Gtk2Def
  {$endif}
  ;

type
  TMPlayerCtrlEngine = (meMplayer,meMPV);
  TVideoInfo = record
    Codec: string;
    Format: string;
    Width, Height: Integer;
    FPS: Single;
    Bitrate: Integer;
  end;

  TAudioInfo = record
    Codec: string;
    Format: string;
    Bitrate: Single;
    Channels: Integer;
    SampleRate: Integer;    // Hz
  end;

  { TCustomMPlayerControl }

  TMplayerCtrlModeMPV = (mmNone, mmCPlayer, mmPseudoGui);
  TMplayerCtrlScreenShotFormat = (ssNone,ssJPG,ssPNG);
  TMplayerCtrlOnFeedback = procedure(ASender: TObject; AStrings: TStringList) of object;
  TMplayerCtrlOnError = procedure(ASender: TObject; AStrings: TStringList) of object;
  TMplayerCtrlOnBeforePlay = procedure(ASender: TObject; AFilename: string) of object;
  TMplayerCtrlOnPlaying = procedure(ASender: TObject; APosition,ADuration: single) of object;
  TMplayerCtrlOnGrabImage = procedure(ASender: TObject; AFilename: String) of object;
  TMplayerCtrlOnICYRadio = procedure(ASender: TObject; AName,AGenre,AWebsite: string; APublic: boolean; ABitrate, AStreamTitle, AStreamURL: string) of object;
  TMplayerCtrlOnCaptureDump = procedure(ASender: TObject; ACapture: boolean) of object;
  TMplayerCtrlOnString = procedure(ASender: TObject; AText: String) of object;
  TMplayerCtrlProcessPriority = (mpHigh,mpIdle,mpNormal,mpRealTime);

  TCustomMPlayerControl = class(TWinControl)
  private
    FCache: integer;
    FCacheMin: integer;
    FMpvIpcDevFile: string;
    FMpvIpcServer: boolean;
    FMPVNoOsc: boolean;
    FOnBeforePlay: TMplayerCtrlOnBeforePlay;
    FOnTimerDump: TMplayerCtrlOnString;
    FTimerDump: boolean;
    FUniqueKey: string;
    icyName,icyGenre,icyWebsite: string;
    icyPublic: boolean;
    icyBitrate,icyStreamTitle,icyStreamURL: string;
    vIsDump: boolean;
    FActiveTimer: boolean;
    FCapture: boolean;
    FEngine: TMPlayerCtrlEngine;
    FFilename: string;
    FImagePath: string;
    FLastImageFilename: string;
    FNoSound: boolean;
    FOnCapture: TMplayerCtrlOnCaptureDump;
    FOnGrabImage: TMplayerCtrlOnGrabImage;
    FOnICYRadio: TMplayerCtrlOnICYRadio;
    FRadio: boolean;
    FRate: single;
    FStartParam:string;
    FLoop: integer;
    FMPlayerPath: string;
    FMPVPath: string;
    FPaused: boolean;
    FPlayerProcess: TAsyncProcess;
    FTimer: TTimer;
    FVolume: integer;
    FCanvas: TCanvas;
    FPosition: Single;
    FLastPosition: string;
    FRequestingPosition: boolean;
    FLastTimer: TDateTime;
    FRequestVolume: boolean;
    FStartTime: single;
    FDuration: single;
    FOnError: TMplayerCtrlOnError;
    FOnFeedback: TMplayerCtrlOnFeedback;
    FOnPlay: TNotifyEvent;
    FOnPlaying: TMplayerCtrlOnPlaying;
    FOnStop: TNotifyEvent;
    FOutList: TStringList;
    FVideoInfo: TVideoInfo;
    FAudioInfo: TAudioInfo;
    FormatSettings: TFormatSettings;
    function GetFileMpvSocket: string;
    function GetPosition: single;
    function GetDuration: single;
    function GetRate: single;
    procedure SetImagePath(AValue: string);
    procedure SetMpvIpcDevFile(AValue: string);
    procedure SetPosition(AValue: single);
    procedure SetFilename(const AValue: string);
    procedure SetLoop(const AValue: integer);
    procedure SetMPlayerPath(const AValue: string);
    procedure SetPaused(const AValue: boolean);
    procedure SetRate(AValue: single);
    procedure SetVolume(const AValue: integer);
    procedure SetStartParam(const AValue: string);
    procedure TimerEvent(Sender: TObject);
    procedure PlayerProcessReadData(Sender: TObject);
  private
    FAccel: string;
    FPLAYF: boolean;
    FBostVolume: boolean;
    FModeMPV: TMplayerCtrlModeMPV;
    FOnBeforeStop: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FOnReplay: TNotifyEvent;
    FOnSetPosition: TNotifyEvent;
    FPP: TMplayerCtrlProcessPriority;
    FscDir: string;
    FSSFormat: TMplayerCtrlScreenShotFormat;
    function ExecuteSockProcess(command: string; device_file: string = ''): string;
    procedure SetAccel(AValue: string);
    procedure SetscDir(AValue: string);
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
	
    procedure InitialiseInfo;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetMpvIpcDevFile: string;
    procedure SendMPlayerCommand(Cmd: string); // see: mplayer -input cmdlist and http://www.mplayerhq.hu/DOCS/tech/slave.txt
    function Running: boolean;
    procedure Play(sBaseDirectory: string = '');
    procedure Stop;
    function Playing: boolean;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Capturing;
    procedure Pause;
    procedure Replay;
    function GetPositionOnlyRead: single;
    procedure SetOSDLevel(aLevel: word);
    procedure SetMute(aMute: boolean = true);
    procedure SetChannels(aChannels: word);
    function GetAudioSamplerate: integer;
    procedure SetAudioSamplerate(aSamplerate: integer);
    procedure SetAudioEQ(s: string = '');
    procedure SetAF(aValue: string = '');
    function SingleMpToTime(AValue: single): TTime;
    function SingleMpToInteger(AValue: single): integer;
    function TimeToSingleMp(AValue: TTime): single;
    function IntegerToSingleMp(AValue: integer): single;
    procedure SetPositionEx(aPosition,aMax: integer);
  public
    function FindMPlayerPath : Boolean;
    function FindMPVPath : Boolean;

    procedure GrabImage;
    property ScreenshotDirectory: string read FscDir write SetscDir;
    property ScreenshotFormat: TMplayerCtrlScreenShotFormat read FSSFormat write FSSFormat default ssNone;
    property LastImageFilename: String read FLastImageFilename;

    property ProcessPriority: TMplayerCtrlProcessPriority read FPP write FPP default mpNormal;
    property AccelType: string read FAccel write SetAccel;
    property ModeMPV: TMplayerCtrlModeMPV read FModeMPV write FModeMPV default mmNone;
    property UniqueKey: string read FUniqueKey; //Unikalny kod, można wykorzystać do tworzenia plików rurek
    property MpvIpcServer: boolean read FMpvIpcServer write FMpvIpcServer default false;
    property MpvIpcDevFile: string read FMpvIpcDevFile write SetMpvIpcDevFile; //'{$UniqueKey}' zamieniany jest na UniqueKey
    property Engine: TMPlayerCtrlEngine read FEngine write FEngine;
    property NoSound: boolean read FNoSound write FNoSound default false;
    property Cache: integer read FCache write FCache default 0; //0=default
    property CacheMin: integer read FCacheMin write FCacheMin default 0; //0=default
    property Filename: string read FFilename write SetFilename;
    property StartParam: string read FStartParam write SetStartParam;
    property MPlayerPath: string read FMPlayerPath write SetMPlayerPath;
    property PlayerProcess: TAsyncProcess read fPlayerProcess;  deprecated;
    property Paused: boolean read FPaused write SetPaused;
    property Loop: integer read FLoop write SetLoop; // -1 no, 0 forever, 1 once, 2 twice, ...
    property Volume: integer read FVolume write SetVolume;
    property BostVolume: boolean read FBostVolume write FBostVolume default false;
    property ICYRadio: boolean read FRadio write FRadio;
    property CaptureDump: boolean read FCapture write FCapture;
    property ActiveTimer: boolean read FActiveTimer write FActiveTimer;
    property TimerDump: boolean read FTimerDump write FTimerDump default false;
    property MpvNoOSC: boolean read FMPVNoOsc write FMPVNoOsc default false;

    property ImagePath: string read FImagePath write SetImagePath;

    property Rate: single read GetRate write SetRate; // mplayer only supports 0.1 to 100
    property Duration: single read GetDuration; // seconds
    property Position: single read GetPosition write SetPosition; // seconds

    property VideoInfo: TVideoInfo read FVideoInfo; // this isn't fully populated until OnPlay recieved
    property AudioInfo: TAudioInfo read FAudioInfo; // this isn't fully populated until OnPlay received

    property OnFeedback: TMplayerCtrlOnFeedback read FOnFeedback write FOnFeedback;
    property OnError: TMplayerCtrlOnError read FOnError write FOnError;
    property OnPlaying: TMplayerCtrlOnPlaying read FOnPlaying write FOnPlaying;
    property OnBeforePlay: TMplayerCtrlOnBeforePlay read FOnBeforePlay write FOnBeforePlay;
    property OnBeforeStop: TNotifyEvent read FOnBeforeStop write FOnBeforeStop;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnReplay: TNotifyEvent read FOnReplay write FOnReplay;
    property OnGrabImage: TMplayerCtrlOnGrabImage read FOnGrabImage write FOnGrabImage;
    property OnICYRadio: TMplayerCtrlOnICYRadio read FOnICYRadio write FOnICYRadio;
    property OnCapture: TMplayerCtrlOnCaptureDump read FOnCapture write FOnCapture;
    property OnTimerDump: TMplayerCtrlOnString read FOnTimerDump write FOnTimerDump;
    property OnSetPosition: TNotifyEvent read FOnSetPosition write FOnSetPosition;
  end;

  TMPlayerControl = class(TCustomMPlayerControl)
  published
    property ProcessPriority;
    property AccelType;
    property ModeMPV;
    property ScreenshotDirectory;
    property ScreenshotFormat;
    property UniqueKey;
    property MpvIpcServer;
    property MpvIpcDevFile;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property Engine;
    property NoSound;
    property Cache;
    property CacheMin;
    property Filename;
    property Loop;
    property StartParam;
    property MPlayerPath;
    property ICYRadio;
    property CaptureDump;
    property ActiveTimer;
    property TimerDump;
    property MpvNoOSC;
    property OnChangeBounds;
    property OnConstrainedResize;
    property OnResize;
    property OnClick;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseWheel;
    property Visible;
    property Volume;     // 0 to 100 and -1 is deactivated
    property BostVolume;
    property OnFeedback; // Provides standard console output from mplayer
    property OnError;    // Provides stderr console output from mplayer
    property OnPlaying;  // When not paused, an event every 250ms to 500ms with Position
    property OnBeforePlay;
    property OnBeforeStop;
    property OnPlay;     // Sent after mplayer initialises the current video file
    property OnPause;
    property OnReplay;
    property OnStop;     // Sent sometime (up to approx 250ms) after mplayer finishes current video
    property OnGrabImage; // Fired when mplayer reports the filename of the image grab
    property OnICYRadio;
    property OnCapture;
    property OnTimerDump;
    property OnSetPosition;
  end;

  { TWSMPlayerControl }

  {$ifdef Linux}
  TWSMPlayerControl = class(TGtk2WSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;
  {$endif}

Const
  ON_PLAYING_INTERVAL = 500 / (24*60*60*1000);

procedure Register;

implementation

Uses
  ecode_unit, Forms, fpjson, jsonparser;

procedure Register;
begin
  RegisterComponents('Multimedia',[TMPlayerControl]);
end;

// returns the value from "ANS_PropertyName=Value" strings
function ExtractAfter(AInput, AIdentifier: string): string; inline;
begin
  AInput := Lowercase(AInput);
  AIdentifier := Lowercase(AIdentifier);

  Result := Copy(AInput, Length(AIdentifier) + 1, Length(AInput) - Length(AIdentifier));
end;

{ TCustomMPlayerControl }

procedure TCustomMPlayerControl.TimerEvent(Sender: TObject);
var
  a,b: single;
begin
  if FPlayerProcess<>nil then
  begin
    if FEngine=meMplayer then
    begin
      If Running And ((Now-FLastTimer)>ON_PLAYING_INTERVAL) Then
      begin
        if Assigned(FOnPlaying) and not FPaused then
        begin
          SendMPlayerCommand('get_time_pos');
          FRequestingPosition:=True;
        end;
        if FRequestVolume then SendMPlayerCommand('get_property volume');
      end;
    end else begin
      //if Running and ((Now-FLastTimer)>ON_PLAYING_INTERVAL) then
      if Playing then
      begin
        if Assigned(FOnPlaying) then
        begin
          b:=GetDuration;
          if b>-1 then
          begin
            a:=GetPosition;
            FOnPlaying(self,a,b);
          end;
        end;
      end else if FPLAYF then stop;
    end;
  end;
end;

procedure TCustomMPlayerControl.PlayerProcessReadData(Sender: TObject);
var
  ErrList: TStringList;
  i,j: integer;
  temp,sTemp,s,pom,s1,s2: string;
  str: TStringList;
  iPosEquals, iPosAfterUS: SizeInt;
  sValue: string;
  sProperty: string;
  iError: Integer;
  b1,b2: boolean;
  a,b: integer;
  bPostOnPlay, bPostOnStop, bPostOnPlaying: boolean;
begin
  bPostOnPlay:=False;
  bPostOnStop:=False;
  bPostOnPlaying:=true;
  FLastTimer := Now;

  if FPlayerProcess.Output.NumBytesAvailable > 0 then
  begin
    FOutList.LoadFromStream(FPlayerProcess.Output);

    // Look for responses to injected commands...
    // or for standard issued information
    for i:=FOutList.Count-1 downto 0 do
    begin
      temp:=FOutList[i];
      if Assigned(FOnTimerDump) and FTimerDump then FOnTimerDump(self,temp);
      sTemp:=Lowercase(temp);
      if FEngine=meMPV then
      begin
        if pos('playing:',sTemp)=1 then bPostOnPlay:=true;
        if sTemp='exiting... (end of file)' then bPostOnStop:=true;
      end;
      iPosEquals:=Pos('=',sTemp);

      (* CAPTURING *)
      if FCapture and Assigned(FOnCapture) then
        if pos('capturing:',sTemp)>0 then FOnCapture(Self,pos('enabled',sTemp)>0);

      (* ICYRADIO *)
      if FRadio then
      begin
        b2:=pos(':',sTemp)>0;
        b1:=pos('name',sTemp)=1;
        if b1 and b2 then icyName:=trim(GetLineToStr(temp,2,':'));
        b1:=pos('genre',sTemp)=1;
        if b1 and b2 then icyGenre:=trim(GetLineToStr(temp,2,':'));
        b1:=pos('website',sTemp)=1;
        if b1 and b2 then
        begin
          s:=temp;
          a:=pos(':',s);
          delete(s,1,a);
          icyWebsite:=trim(s);
        end;
        b1:=pos('public',sTemp)=1;
        if b1 and b2 then icyPublic:=trim(GetLineToStr(temp,2,':'))='yes';
        b1:=pos('bitrate',sTemp)=1;
        if b1 and b2 then icyBitrate:=trim(GetLineToStr(temp,2,':'));
        b1:=pos('icy info:',sTemp)=1;
        if b1 then
        begin
          s:=temp;
          a:=pos(':',s);
          delete(s,1,a);
          s:=trim(s);
          str:=TStringList.Create;
          try
            a:=1;
            while true do
            begin
              pom:=trim(GetLineToStr(s,a,';'));
              if pom='' then break;
              str.Add(pom);
              inc(a);
            end;
            for j:=0 to str.Count-1 do
            begin
              s:=str[j];
              s1:=GetLineToStr(s,1,'=');
              s2:=GetLineToStr(s,2,'=');
              if s2[1]='''' then delete(s2,1,1);
              if s2[length(s2)]='''' then delete(s2,length(s2),1);
              if s1='StreamTitle' then icyStreamTitle:=s2 else
              if s1='StreamUrl' then icyStreamURL:=s2;
            end;
          finally
            str.Free;
          end;
        end;
      end;

      // Identify requests look like ID_Property=Value
      // Property requests look like ANS_Property=Value
      if (iPosEquals>1) and ((Pos('ans_', sTemp)=1) or (Pos('id_', sTemp)=1)) then
      begin
        iPosAfterUS := Pos('_', sTemp)+1;
        sValue := Copy(sTemp, iPosEquals + 1, Length(sTemp) - iPosEquals);
        sProperty := Copy(sTemp, iPosAfterUS, iPosEquals - iPosAfterUS);

        if Assigned(FOnPlaying) and (FRequestingPosition) and (sProperty = 'time_position') then
        begin
          // Are we paused by any chance?
          if sValue = FLastPosition then
            SendMPlayerCommand('get_property pause');

          FLastPosition := sValue;

          FPosition := StrToFloatDef(sValue,0,FormatSettings)-FStartTime;

          // Don't remove any further ANS_Time_Positions, they're not ours...
          FRequestingPosition := False;

          // clear this response from the queue
          FOutList.Delete(i);
        end
        else
          case sProperty Of
            'volume' :
              begin
                FVolume := Trunc(0.5 + StrToFloatDef(sValue,100,FormatSettings));
                FRequestVolume := False;

                // clear this response from the queue
                FOutList.Delete(i);
               end;
            'length'       : FDuration := StrToFloatDef(sValue,-1,FormatSettings);
            'pause'        : FPaused := (sValue = 'yes');
            'video_codec'  : FVideoInfo.Codec:=sValue;
            'video_format' : FVideoInfo.Format:=sValue;
            'video_bitrate': FVideoInfo.Bitrate:=StrToIntDef(sValue, 0);
            'video_width'  : FVideoInfo.Width:=StrToIntDef(sValue, 0);
            'video_height' : FVideoInfo.Height:=StrToIntDef(sValue, 0);
            'video_fps'    : FVideoInfo.FPS:=StrToFloatDef(sValue,0,FormatSettings);
            'start_time'   : FStartTime:=StrToFloatDef(sValue,0,FormatSettings);
            //'seekable'     : FSeekable:=(sValue='1');
            'audio_codec'  : FAudioInfo.Codec:=sValue;
            'audio_format' : FAudioInfo.Format:=sValue;
            'audio_bitrate': FAudioInfo.Bitrate:=StrToIntDef(sValue, 0);
            'audio_rate'   : FAudioInfo.SampleRate:=StrToIntDef(sValue, 0);
            'audio_nch'    : FAudioInfo.Channels:=StrToIntDef(sValue, 0);
            'exit'         : bPostOnStop:=True;
        end;
      end // ID_ or ANS_
      else if Assigned(FOnPlay) and (sTemp = 'starting playback...') then  //starting playback...
        bPostOnPlay:=True
      else if (Pos('*** screenshot', sTemp)=1) Then
      begin
        //  result looks like *** screenshot 'shot0002.png' ***
        FLastImageFilename:=IncludeTrailingBackslash(GetCurrentDirUTF8) + Copy(sTemp, 17, Pos('.', sTemp)-17+4);

        if assigned(FOnGrabImage) And FileExistsUTF8(FLastImageFilename) then
          FOnGrabImage(Self, FLastImageFilename);

        // clear this response from the queue
        FOutList.Delete(i);
      end
      else if sTemp='sending vfctrl_screenshot!' then
        FOutList.Delete(i);
    end;

    if Assigned(FOnFeedback) and (FOutlist.Count > 0) then
      FOnFeedback(Self, FOutlist);
  end;

  if FPlayerProcess.StdErr.NumBytesAvailable > 0 then
  begin
    ErrList := TStringList.Create;
    try
      ErrList.LoadFromStream(FPlayerProcess.Stderr);

      // Catch error retrieving volume
      If FRequestVolume Then
      begin
        iError := ErrList.IndexOf('Failed to get value of property ''volume''.');
        If iError<>-1 Then
        begin
          Errlist.Delete(iError);

          // Prevent further requests for volume
          FVolume := 0;
          FRequestVolume := False;
        end;
      end;

      if Assigned(FOnError) then
        FOnError(Self, ErrList);
    finally
      ErrList.Free;
    end;
  end;

  // don't post the OnPlay until all the data above is processed
  if Assigned(FOnPlay) and bPostOnPlay then FOnPlay(Self);
  If Assigned(FOnPlaying) and bPostOnPlaying then FOnPlaying(Self,FPosition,FDuration);
  if Assigned(FOnICYRadio) then FOnICYRadio(Self,icyName,icyGenre,icyWebsite,icyPublic,icyBitrate,icyStreamTitle,icyStreamURL);
  If (not Running) Or bPostOnStop Then Stop;
end;

function TCustomMPlayerControl.ExecuteSockProcess(command: string;
  device_file: string): string;
var
  SockProcess: TAsyncProcess;
  oo: TStringList;
  s: string;
begin
  result:='';
  if not FMpvIpcServer then exit;
  if device_file='' then s:=GetFileMpvSocket else s:=device_file;
  SockProcess:=TAsyncProcess.Create(nil);
  oo:=TStringList.Create;
  try
    SockProcess.Executable:='/bin/sh';
    SockProcess.Parameters.Add('-c');
    SockProcess.Parameters.Add('echo '''+command+''' | socat - '+s);
    SockProcess.Options:=[poWaitOnExit,poUsePipes];
    SockProcess.Execute;
    oo.LoadFromStream(SockProcess.Output);
    result:=oo.Text;
    SockProcess.Terminate(0);
  finally
    SockProcess.Free;
    oo.Free;
  end;
end;

procedure TCustomMPlayerControl.SetAccel(AValue: string);
begin
  if AValue='' then FAccel:='<auto>' else FAccel:=AValue;
end;

procedure TCustomMPlayerControl.SetscDir(AValue: string);
var
  s: string;
begin
  if AValue='' then s:='<auto>' else s:=AValue;
  if FscDir=s then Exit;
  FscDir:=s;
end;

procedure TCustomMPlayerControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  if (csDesigning in ComponentState) and (FCanvas<>nil) then begin
    with FCanvas do begin
      if Message.DC <> 0 then
        Handle := Message.DC;
      Brush.Color:=clLtGray;
      Pen.Color:=clRed;
      Rectangle(0,0,Self.Width-1,Self.Height-1);
      MoveTo(0,0);
      LineTo(Self.Width,Self.Height);
      MoveTo(0,Self.Height);
      LineTo(Self.Width,0);
      if Message.DC <> 0 then
        Handle := 0;
    end;
  end;
  Exclude(FControlState, csCustomPaint);
end;

procedure TCustomMPlayerControl.WMSize(var Message: TLMSize);
begin
  if (Message.SizeType and Size_SourceIsInterface)>0 then
    DoOnResize;
end;

procedure TCustomMPlayerControl.SetStartParam(const AValue: string);
begin
  if FStartParam=AValue then exit;
  FStartParam:=AValue;
end;

procedure TCustomMPlayerControl.SetFilename(const AValue: string);
  // Copied from win\process.inc
  // mplayer uses identical params under linux, so this is safe
  Function MaybeQuoteIfNotQuoted(Const S : String) : String;
  begin
    If (Pos(' ',S)<>0) and (pos('"',S)=0) then
      Result:='"'+S+'"'
    else
       Result:=S;
  end;
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  if Running then
    SendMPlayerCommand('loadfile '+MaybeQuoteIfNotQuoted(Filename));
end;

procedure TCustomMPlayerControl.SetLoop(const AValue: integer);
begin
  if FLoop=AValue then exit;
  FLoop:=AValue;
  if Running then
    SendMPlayerCommand('loop '+IntToStr(FLoop));
end;

procedure TCustomMPlayerControl.SetMPlayerPath(const AValue: string);
begin
  if FMPlayerPath=AValue then exit;
  FMPlayerPath:=AValue;
end;

procedure TCustomMPlayerControl.SetPaused(const AValue: boolean);
begin
  if FPaused=AValue then exit;
  if Running then begin
    FPaused:=AValue;
    SendMPlayerCommand('pause');
  end;
end;

procedure TCustomMPlayerControl.SetRate(AValue: single);
begin
  if FRate=AValue then Exit;
  if (FRate<0.1) or (FRate>100) then Exit;
  if Running then begin
    FRate:=AValue;
    SendMPlayerCommand(Format('set_property speed %.3f', [FRate]));
  end;
end;

procedure TCustomMPlayerControl.SetVolume(const AValue: integer);
var
  s: string;
begin
  if FVolume=AValue then exit;
  FVolume:=AValue;
  if FVolume=-1 then exit;
  if Running then
  begin
    if FEngine=meMplayer then
    begin
      {mplayer}
      SendMPlayerCommand('volume ' + IntToStr(FVolume) + ' 1');
      FRequestVolume := True;
    end else begin
      {mpv}
      s:=ExecuteSockProcess('{ "command": ["set_property", "volume", '+IntToStr(AValue)+'] }');
    end;
  end;
end;

constructor TCustomMPlayerControl.Create(TheOwner: TComponent);
var
  vKey: TGuid;
begin
  inherited Create(TheOwner);
  FPP:=mpNormal;
  FAccel:='<auto>';
  FPLAYF:=false;
  FormatSettings.DecimalSeparator:='.';
  CreateGUID(vKey);
  FModeMPV:=mmNone;
  FscDir:='<auto>';
  FSSFormat:=ssNone;
  FMpvIpcServer:=false;
  FMpvIpcDevFile:='<auto>';
  FBostVolume:=false;
  FUniqueKey:=StringReplace(GUIDToString(vKey),'{','',[rfReplaceAll]);
  FUniqueKey:=StringReplace(FUniqueKey,'}','',[rfReplaceAll]);
  FUniqueKey:=StringReplace(FUniqueKey,'-','',[rfReplaceAll]);
  FEngine:=meMplayer;
  FNoSound:=false;
  FRadio:=false;
  FCapture:=false;
  FVolume:=-1;
  FActiveTimer:=false;
  FTimerDump:=false;
  FMPVNoOsc:=false;
  ControlStyle:=ControlStyle-[csSetCaption];
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  SetInitialBounds(0, 0, 160, 90);

  FOutlist := TStringList.Create;

  FMPlayerPath := 'mplayer' + GetExeExt;
  FMPVPath := 'mpv' + GetExeExt;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 250;
  FTimer.OnTimer := @TimerEvent;
end;

destructor TCustomMPlayerControl.Destroy;
begin
  Stop;
  FreeAndNil(FCanvas);
  FreeAndNil(FTimer);
  FreeAndNil(FOutList);
  inherited Destroy;
end;

function TCustomMPlayerControl.GetMpvIpcDevFile: string;
begin
  result:=GetFileMpvSocket;
end;

procedure TCustomMPlayerControl.SendMPlayerCommand(Cmd: string);
begin
  if Cmd='' then exit;
  if not Running then exit;

  if Pos('paus', Lowercase(Cmd)) <> 1 then
    Cmd := 'pausing_keep_force ' + Cmd;
  if Cmd[length(Cmd)] <> LineEnding then
    Cmd := Cmd + LineEnding;

  FPlayerProcess.Input.Write(Cmd[1], length(Cmd));
end;

function TCustomMPlayerControl.Running: boolean;
begin
  Result:=(fPlayerProcess<>nil) and fPlayerProcess.Running;
end;

function TCustomMPlayerControl.FindMPlayerPath: Boolean;
var
  ExePath: string;
  MPlayerExe: String;
begin
  result := FileExistsUTF8(FMPlayerPath);

  If not result then
  begin
    MPlayerExe:='mplayer'+GetExeExt;
    if FMPlayerPath='' then
      FMPlayerPath:=MPlayerExe;
    ExePath:=FMPlayerPath;
    // Is mplayer installed in the environment path?
    if not FilenameIsAbsolute(ExePath) then
      ExePath:=FindDefaultExecutablePath(ExePath);
    // is mplayer in a folder under the application folder?
    if Not FileExistsUTF8(ExePath) then
      ExePath := IncludeTrailingBackSlash(ExtractFileDir(Application.ExeName))+
        IncludeTrailingBackslash('mplayer') + MPlayerExe;
    // did we find it?
    if FileExistsUTF8(ExePath) then
    begin
      FMPlayerPath:=ExePath;
      result := true;
    end;
  end;
end;

function TCustomMPlayerControl.FindMPVPath: Boolean;
var
  ExePath: string;
  MPlayerExe: String;
begin
  result := FileExistsUTF8(FMPVPath);

  If not result then
  begin
    MPlayerExe:='mpv'+GetExeExt;
    if FMPVPath='' then
      FMPVPath:=MPlayerExe;
    ExePath:=FMPVPath;
    // Is mplayer installed in the environment path?
    if not FilenameIsAbsolute(ExePath) then
      ExePath:=FindDefaultExecutablePath(ExePath);
    // is mplayer in a folder under the application folder?
    if Not FileExistsUTF8(ExePath) then
      ExePath := IncludeTrailingBackSlash(ExtractFileDir(Application.ExeName))+
        IncludeTrailingBackslash('mpv') + MPlayerExe;
    // did we find it?
    if FileExistsUTF8(ExePath) then
    begin
      FMPVPath:=ExePath;
      result := true;
    end;
  end;
end;

procedure TCustomMPlayerControl.GrabImage;
begin
  if Running and (FSSFormat<>ssNone) then
  begin
    if FEngine=meMplayer then SendMPlayerCommand('screenshot 0') else
    begin
      ExecuteSockProcess('{ "command": ["screenshot"] }');
      if Assigned(FOnGrabImage) then FOnGrabImage(self,FscDir);
    end;
  end;
end;

procedure TCustomMPlayerControl.Play(sBaseDirectory: string);
var
  CurWindowID: PtrUInt;
  slStartParams: TStringList;
  i: integer;
  s: string;
begin
  if (csDesigning in ComponentState) then exit;

  if Running and Paused then begin
    Paused:=false;
    exit;
  end;

  if Playing then begin
    if FRate<>1 Then
      Rate := 1;
    exit;
  end;

  if Assigned(FOnBeforePlay) then FOnBeforePlay(self,FFilename);

  {$IFDEF Linux}
  if (not HandleAllocated) then exit;
  {$IFDEF DEBUG}
  DebugLn(['TCustomMPlayerControl.Play ']);
  {$ENDIF}
  {$endif}

  if fPlayerProcess<>nil then
    FreeAndNil(fPlayerProcess);
//    raise Exception.Create('TCustomMPlayerControl.Play fPlayerProcess still exists');

  icyName:=''; icyGenre:=''; icyWebsite:='';
  icyPublic:=false;
  icyBitrate:=''; icyStreamTitle:=''; icyStreamURL:='';

  if FEngine=meMplayer then
  begin
    FDuration:=-1;
    if not FindMPlayerPath then
      raise Exception.Create(MPlayerPath+' not found');
  end else begin
    FDuration:=-1;
    if not FindMPVPath then
      raise Exception.Create(FMPVPath+' not found');
  end;

  {$IFDEF Linux}
    CurWindowID := GDK_WINDOW_XWINDOW({%H-}PGtkWidget(PtrUInt(Handle))^.window);
  {$else}
    CurWindowID := Handle;
  {$ENDIF}

  FPlayerProcess := TAsyncProcess.Create(Self);
  FPlayerProcess.OnReadData:=@PlayerProcessReadData;
  FPlayerProcess.Options := FPlayerProcess.Options + [poUsePipes, poNoConsole];
  FPlayerProcess.CurrentDirectory:=sBaseDirectory;

  // -really-quiet       : DONT USE: causes the video player to not connect to -wid.  Odd...
  // -noconfig all       : stop mplayer from reading commands from a text file
  // -zoom -fs           : Unsure:  Only perceptible difference is background drawn black not green
  // -vo direct3d        : uses Direct3D renderer (recommended under windows)
  // -vo gl_nosw         : uses OpenGL no software renderer
  case FEngine of
    meMplayer: FPlayerProcess.Executable:=FMPlayerPath;
    meMPV:     FPlayerProcess.Executable:=FMPVPath;
  end;
  if FEngine=meMplayer then
  begin
    FPlayerProcess.Parameters.Add('-softvol');   // allow us to soft volume!
    FPlayerProcess.Parameters.Add('-slave');     // allow us to control mplayer
    FPlayerProcess.Parameters.Add('-identify');  // Request stats on playing file
    if FCapture then FPlayerProcess.Parameters.Add('-capture');
    FPlayerProcess.Parameters.Add('-vf');
    FPlayerProcess.Parameters.Add('screenshot'); // (with -vf) Allow frame grab
    if FCache>0 then
    begin
      FPlayerProcess.Parameters.Add('-cache');
      FPlayerProcess.Parameters.Add(IntToStr(FCache));
      if FCacheMin>0 then
      begin
        FPlayerProcess.Parameters.Add('-cache-min');
        FPlayerProcess.Parameters.Add(IntToStr(FCacheMin));
      end;
    end;
  end;
  if FEngine=meMPV then
  begin
    if FSSFormat<>ssNone then
    begin
      if (FscDir<>'') and (FscDir<>'<auto>') then FPlayerProcess.Parameters.Add('--screenshot-directory='+FscDir);
      case FSSFormat of
        ssJPG: FPlayerProcess.Parameters.Add('--screenshot-format=jpg');
        ssPNG: FPlayerProcess.Parameters.Add('--screenshot-format=png');
      end;
    end;
    if not FNoSound then
    begin
      if FBostVolume then FPlayerProcess.Parameters.Add('--af=lavfi=[acompressor=24]');
      if FVolume>-1 then
      begin
        FPlayerProcess.Parameters.Add('-volume');    // Set initial volume
        FPlayerProcess.Parameters.Add(IntToStr(FVolume));
      end;
    end;
    if FModeMPV=mmCPlayer then FPlayerProcess.Parameters.Add('-player-operation-mode=cplayer') else
    if FModeMPV=mmPseudoGui then FPlayerProcess.Parameters.Add('-player-operation-mode=pseudo-gui');
    if FMPVNoOsc then FPlayerProcess.Parameters.Add('-no-osc');
    FPlayerProcess.Parameters.Add('--script-opts=timetotal=yes,timems=yes');
    //FPlayerProcess.Parameters.Add('--input-test');
    //FPlayerProcess.Parameters.Add('--idle');
    if FMpvIpcServer then FPlayerProcess.Parameters.Add('--input-ipc-server='+GetFileMpvSocket);
  end;
  FPlayerProcess.Parameters.Add('-quiet');     // supress most messages
  case FEngine of
    meMplayer: if FNoSound then FPlayerProcess.Parameters.Add('-nosound');
    meMPV:     if FNoSound then begin FPlayerProcess.Parameters.Add('-mute'); FPlayerProcess.Parameters.Add('yes'); end;
  end;

  FPlayerProcess.Parameters.Add('-wid');       // sets Window ID (display video in our control)
  FPlayerProcess.Parameters.Add(IntToStr(CurWindowID));

  if FAccel='<auto>' then
  begin
    FPlayerProcess.Parameters.Add('-vo');
    {$IFDEF LINUX}
    FPlayerProcess.Parameters.Add('xv');
    {$ELSE}
    FPlayerProcess.Parameters.Add('directx');
    //FPlayerProcess.Parameters.Add('direct3d');
    {$ENDIF}
  end else begin
    FPlayerProcess.Parameters.Add('-vo');
    FPlayerProcess.Parameters.Add(FAccel);
  end;

  // Add the user defined start params
  if (Trim(FStartParam)<>'') then
  begin
    slStartParams := TStringList.Create;
    try
      CommandToList(StartParam, slStartParams);
      if FNoSound then for i:=slStartParams.Count-1 downto 0 do
      begin
        s:=slStartParams[i];
        if s='--mute=no' then slStartParams.Delete(i);
      end;
      FPlayerProcess.Parameters.AddStrings(slStartParams);
    finally
      slStartParams.Free;
    end;
  end;

  FPlayerProcess.Parameters.Add(FFilename);

  FPlayerProcess.Parameters.Delimiter:=' ';
  {$IFDEF DEBUG}
  DebugLn(['TCustomMPlayerControl.Play ', FPlayerProcess.Parameters.DelimitedText]);
  {$ENDIF}

  // Normally I'd be careful to only use FOutList in the
  // Timer event, but here I'm confident the timer isn't running...
  if assigned(FOnFeedback) then
  begin
    FOutlist.Clear;
    FOutlist.Add(FPlayerProcess.Executable + ' ' + FPlayerProcess.Parameters.DelimitedText);
    FOutlist.Add('');
    FonFeedback(Self, FOutlist);
  end;

  // Populate defaults
  InitialiseInfo;

  case FPP of
    mpHigh:     FPlayerProcess.Priority:=ppHigh;
    mpIdle:     FPlayerProcess.Priority:=ppIdle;
    mpNormal:   FPlayerProcess.Priority:=ppNormal;
    mpRealTime: FPlayerProcess.Priority:=ppRealTime;
  end;
  FPlayerProcess.Execute;

  // Start the timer that handles feedback from mplayer
  FPLAYF:=true;
  FTimer.Enabled:=FActiveTimer;
  if FEngine=meMPV then if FPLayerProcess.Running and Assigned(FOnPlay) then FOnPlay(Self); //to nie jest potrzebne...
end;

procedure TCustomMPlayerControl.Stop;
var
  s: string;
begin
  if FPlayerProcess=nil then
    exit;

  if Assigned(FOnBeforeStop) then FOnBeforeStop(self);

  {$IFDEF DEBUG}
  DebugLn(Format('ExitStatus=%d',[fPlayerProcess.ExitStatus]));
  DebugLn(Format('ExitCode=%d',[fPlayerProcess.ExitCode]));
  {$ENDIF}
  FPaused:=False;
  FDuration:=-1;
  FTimer.Enabled:=False;
  FPLAYF:=false;

  SendMPlayerCommand('quit');

  sleep(250);
  FPlayerProcess.Terminate(0);

  FreeAndNil(FPlayerProcess);

  icyName:=''; icyGenre:=''; icyWebsite:='';
  icyPublic:=false;
  icyBitrate:=''; icyStreamTitle:=''; icyStreamURL:='';

  if FMpvIpcServer then
  begin
    s:=GetFileMpvSocket;
    if FileExists(s) then DeleteFile(s);
  end;

  if Assigned(FOnPlaying) then FOnPlaying(self,0,0);
  if Assigned(FOnStop) then FOnStop(Self);

  // repaint the control
  Refresh;
end;

function TCustomMPlayerControl.Playing: boolean;
begin
  Result := Running and (not Paused);
end;

procedure TCustomMPlayerControl.Invalidate;
begin
  if csCustomPaint in FControlState then exit;
  inherited Invalidate;
end;

procedure TCustomMPlayerControl.EraseBackground(DC: HDC);
begin
  if (FCanvas <> nil) then
    with FCanvas do
    begin
      if DC <> 0 then
        Handle := DC;
      Brush.Color := clLtGray;
      Rectangle(0, 0, Self.Width, Self.Height);
      if DC <> 0 then
        Handle := 0;
    end;
end;

procedure TCustomMPlayerControl.Capturing;
begin
  vIsDump:=not vIsDump;
  SendMPlayerCommand('capturing');
end;

procedure TCustomMPlayerControl.Pause;
var
  s: string;
begin
  if FEngine=meMplayer then
  begin
    if Running and Playing then
    begin
      FPaused:=true;
      SendMPlayerCommand('pause');
    end;
  end else if FEngine=meMPV then
  begin
    if Running and Playing then
    begin
      FPLAYF:=false;
      s:=ExecuteSockProcess('{ "command": ["set_property", "pause", true] }');
      FPaused:=pos('"success"',s)>0;
    end;
  end;
  if Assigned(FOnPause) then FOnPause(self);
end;

procedure TCustomMPlayerControl.Replay;
var
  s: string;
begin
  if FEngine=meMplayer then
  begin
    if Running and Playing then
    begin
      FPaused:=false;
      SendMPlayerCommand('pause');
    end;
  end else if FEngine=meMPV then
  begin
    //if Running and Playing then
    if FPaused then
    begin
      FPaused:=false;
      s:=ExecuteSockProcess('{ "command": ["set_property", "pause", false] }');
      FPaused:=not pos('"success"',s)>0;
      FPLAYF:=true;
    end;
  end;
  if Assigned(FOnReplay) then FOnReplay(self);
end;

function TCustomMPlayerControl.GetPositionOnlyRead: single;
begin
  result:=FPosition;
end;

procedure TCustomMPlayerControl.SetOSDLevel(aLevel: word);
begin
  ExecuteSockProcess('{ "command": ["set_property", "osd-level", '+IntToStr(aLevel)+'] }');
end;

procedure TCustomMPlayerControl.SetMute(aMute: boolean);
var
  s: string;
begin
  if FNosound then exit;
  if aMute then s:='yes' else s:='no';
  ExecuteSockProcess('{ "command": ["set_property", "mute", "'+s+'"] }');
end;

procedure TCustomMPlayerControl.SetChannels(aChannels: word);
begin
  if FNosound then exit;
  case aChannels of
    0: SetMute;
    1: begin
         ExecuteSockProcess('{ "command": ["set_property", "audio-channels", "mono"] }');
         SetMute(false);
       end;
    2: begin
         ExecuteSockProcess('{ "command": ["set_property", "audio-channels", "stereo"] }');
         SetMute(false);
       end;
  end;
end;

function TCustomMPlayerControl.GetAudioSamplerate: integer;
var
  s,s2: string;
  jData: TJSONData;
  jObject: TJSONObject;
begin
  if FNosound then exit;
  s:=ExecuteSockProcess('{ "command": ["get_property", "audio-samplerate"] }');
  jData:=GetJSON(s);
  jObject:=TJSONObject(jData);
  try s2:=jObject.Strings['error']; except s2:=''; end;
  if s2='success' then
  begin
    jObject.Floats['data'];
    result:=jObject.Integers['data'];
  end else result:=0;
end;

procedure TCustomMPlayerControl.SetAudioSamplerate(aSamplerate: integer);
begin
  if FNosound then exit;
  ExecuteSockProcess('{ "command": ["set_property", "audio-samplerate", '+IntToStr(aSamplerate)+'] }');
end;

procedure TCustomMPlayerControl.SetAudioEQ(s: string);
begin
  if FNosound then exit;
  if s='' then
    ExecuteSockProcess('{ "command": ["set_property", "af", "superequalizer"] }')
  else
    ExecuteSockProcess('{ "command": ["set_property", "af", "superequalizer='+s+'"] }');
end;

procedure TCustomMPlayerControl.SetAF(aValue: string);
begin
  if FNosound then exit;
  if aValue='' then
    ExecuteSockProcess('{ "command": ["set_property", "af", ""] }')
  else
    ExecuteSockProcess('{ "command": ["set_property", "af", "'+aValue+'"] }');
end;

function TCustomMPlayerControl.SingleMpToTime(AValue: single): TTime;
begin
  result:=AValue/SecsPerDay;
end;

function TCustomMPlayerControl.SingleMpToInteger(AValue: single): integer;
begin
  result:=TimeToInteger(SingleMpToTime(AValue));
end;

function TCustomMPlayerControl.TimeToSingleMp(AValue: TTime): single;
begin
  result:=SecsPerDay*AValue;
end;

function TCustomMPlayerControl.IntegerToSingleMp(AValue: integer): single;
begin
  result:=TimeToSingleMp(IntegerToTime(AValue));
end;

procedure TCustomMPlayerControl.SetPositionEx(aPosition, aMax: integer);
begin
  SetPosition(round(FDuration*aPosition/aMax));
end;

procedure TCustomMPlayerControl.InitialiseInfo;
begin
  FLastPosition := '';
  FPosition := 0;
  FRequestVolume := False;
  FStartTime := 0;
  FRate := 1;
  FDuration := -1;

  with FVideoInfo Do
  begin
    Format := '';
    Width := 0;
    Height := 0;
    FPS := 0;
    Bitrate := 0;
  end;

  With FAudioInfo Do
  begin
    Format := '';
    Bitrate := 0;
  end;
end;

function TCustomMPlayerControl.GetFileMpvSocket: string;
var
  s: string;
begin
  if (MpvIpcDevFile='') or (MpvIpcDevFile='<auto>') then
    s:='/tmp/mpv-'+FUniqueKey+'.socket'
  else
    s:=MpvIpcDevFile;
  s:=StringReplace(s,'{$UniqueKey}',FUniqueKey,[]);
  result:=s;
end;

function TCustomMPlayerControl.GetPosition: single;
var
  s,s2: string;
  jData: TJSONData;
  jObject: TJSONObject;
begin
  if FEngine=meMplayer then
  begin
    {$IFDEF DEBUG}
    DebugLn(Format('Get Position %.3f', [FPosition]));
    {$ENDIF}
    Result := FPosition;
  end else begin
    if Running then
    begin
      s:=ExecuteSockProcess('{ "command": ["get_property", "playback-time"] }');
      jData:=GetJSON(s);
      jObject:=TJSONObject(jData);
      try s2:=jObject.Strings['error']; except s2:=''; end;
      if s2='success' then
      begin
        jObject.Floats['data'];
        FPosition:=jObject.Floats['data'];
      end else begin
        result:=0;
        exit;
      end;
    end;
    result:=FPosition;
  end;
end;

function TCustomMPlayerControl.GetDuration: single;
var
  s,s2: string;
  jData: TJSONData;
  jObject: TJSONObject;
begin
  if FEngine=meMplayer then
  begin
    {$IFDEF DEBUG}
    DebugLn(Format('Get Position %.3f', [FDuration]));
    {$ENDIF}
    Result := FDuration;
  end else begin
    if FDuration=-1 then
    begin
      s:=ExecuteSockProcess('{ "command": ["get_property", "duration"] }');
      jData:=GetJSON(s);
      jObject:=TJSONObject(jData);
      try s2:=jObject.Strings['error']; except s2:=''; end;
      if s2='success' then
      begin
        jObject.Floats['data'];
        FDuration:=jObject.Floats['data'];
      end else begin
        result:=0;
        exit;
      end;
    end;
    result:=FDuration;
  end;
end;

function TCustomMPlayerControl.GetRate: single;
begin
  Result := FRate;

  //If not Running Then
  //  Result := FRate
  //Else
  //  Result := StrToFloatDef(DoCommand('get_property speed', 'ans_speed='), 1)
end;

procedure TCustomMPlayerControl.SetImagePath(AValue: string);
begin
  if DirectoryExistsUTF8(AValue) then
  begin
    FImagePath:=AValue;
    SetCurrentDirUTF8(AValue);
  end;
end;

procedure TCustomMPlayerControl.SetMpvIpcDevFile(AValue: string);
begin
  if FMpvIpcDevFile=AValue then Exit;
  if AValue='' then FMpvIpcDevFile:='<auto>' else FMpvIpcDevFile:=AValue;
end;

procedure TCustomMPlayerControl.SetPosition(AValue: single);
var
  s: string;
begin
  if Running then
  begin
    if FEngine=meMplayer then
    begin
      if AValue>0 Then
        FPosition := AValue
      Else
        FPosition := 0;

      {$IFDEF DEBUG}
      DebugLn(Format('Set Position to  %.3f', [FPosition]));
      {$ENDIF}
      SendMPlayerCommand(Format('pausing_keep seek %.3f 2', [FPosition]));
    end else begin
      s:=StringReplace(FormatFloat('0.000000',AValue),',','.',[]);
      ExecuteSockProcess('{ "command": ["set_property", "playback-time", '+s+'] }');
    end;
    if Assigned(FOnSetPosition) then FOnSetPosition(Self);
  end;
end;

{$ifdef Linux}
function MPLayerWidgetDestroyCB(Widget: PGtkWidget; {%H-}data: gPointer): GBoolean; cdecl;
begin
  FreeWidgetInfo(Widget); // created in TWSMPlayerControl.CreateHandle
  Result:=false;
end;

{ TWSMPlayerControl }

class function TWSMPlayerControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  NewWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  if csDesigning in AWinControl.ComponentState then
    Result:=inherited CreateHandle(AWinControl,AParams)
  else begin
    NewWidget:=gtk_event_box_new;

    WidgetInfo := GetWidgetInfo(NewWidget,true); // destroyed in MPLayerWidgetDestroyCB
    WidgetInfo^.LCLObject := AWinControl;
    WidgetInfo^.Style := AParams.Style;
    WidgetInfo^.ExStyle := AParams.ExStyle;
    WidgetInfo^.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);

    // set allocation
    Allocation.X := AParams.X;
    Allocation.Y := AParams.Y;
    Allocation.Width := AParams.Width;
    Allocation.Height := AParams.Height;
    gtk_widget_size_allocate(NewWidget, @Allocation);

    if csDesigning in AWinControl.ComponentState then begin
      // at designtime setup normal handlers
      TGtk2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    end else begin
      // at runtime
      g_signal_connect(GPointer(NewWidget), 'destroy',
                       TGTKSignalFunc(@MPLayerWidgetDestroyCB), WidgetInfo);
    end;
    Result:=HWND({%H-}PtrUInt(Pointer(NewWidget)));
    {$IFDEF DEBUG}
    DebugLn(['TWSMPlayerControl.CreateHandle ',dbgs(NewWidget)]);
    {$ENDIF}
  end;
end;

class procedure TWSMPlayerControl.DestroyHandle(const AWinControl: TWinControl
  );
begin
  inherited DestroyHandle(AWinControl);
end;
{$endif}

initialization
  {$ifdef Linux}
  RegisterWSComponent(TCustomMPlayerControl,TWSMPlayerControl);
  {$endif}

  {$I mplayerctrl.lrs}
end.

