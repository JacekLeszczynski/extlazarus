unit LiveChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, ExtCtrls, Process, AsyncProcess;

type

  { TLiveChat }

  TLiveChatService = (svYoutube, svRumble);
  TLiveOnReceive = procedure(aTime: TDateTime; aNick,aMessage: string) of object;
  TLiveOnIsActive = procedure(aIsActive: boolean) of object;
  TLiveOnOpenIsError = procedure(IsError: boolean; aClassNameError,aMessageError: string) of object;
  TLiveOnError = procedure(aErrors: TStrings; var aStopNow, aRestartNow: boolean) of object;
  TLiveChat = class(TComponent)
  private
    FOnError: TLiveOnError;
    FOnRestarted: TNotifyEvent;
    FPipeBufferSize: cardinal;
    FRunning: boolean;
    FOnAfterRecv: TNotifyEvent;
    FOnAfterStart: TLiveOnOpenIsError;
    FOnAfterStop: TLiveOnIsActive;
    FOnBeforeRecv: TNotifyEvent;
    FOnBeforeStart: TNotifyEvent;
    FOnBeforeStop: TNotifyEvent;
    FOnReceive: TLiveOnReceive;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
    FReStarting: boolean;
    FService: TLiveChatService;
    FVideoID: string;
    czas: TDateTime;
    nick,message: string;
    proc: TAsyncProcess;
    timer: TTimer;
    bufor: TStringList;
    last_czas: TDateTime;
    last_nick: TStringList;
    restart_now: boolean;
    procedure _OnReadData(Sender: TObject);
    procedure _OnTimer(Sender: TObject);
    procedure _Restart;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Restart;
  published
    //Działanie komponentu.
    property Running: boolean read FRunning default false;
    //Wybór serwisu.
    property Service: TLiveChatService read FService write FService default svYoutube;
    //W razie błędu nastąpi automatyczna reaktywacja.
    property ReStarting: boolean read FReStarting write FReStarting default false;
    //VideoID wklejany z paska adresu filmu.
    property VideoID: string read FVideoID write FVideoID;
    property PipeBufferSize : cardinal read FPipeBufferSize write FPipeBufferSize default 1024;
    //Gdy pobrano linię czatu.
    property OnReceive: TLiveOnReceive read FOnReceive write FOnReceive;
    property OnBeforeRecv: TNotifyEvent read FOnBeforeRecv write FOnBeforeRecv;
    property OnAfterRecv: TNotifyEvent read FOnAfterRecv write FOnAfterRecv;
    property OnBeforeStart: TNotifyEvent read FOnBeforeStart write FOnBeforeStart;
    property OnBeforeStop: TNotifyEvent read FOnBeforeStop write FOnBeforeStop;
    //Gdy otwarcie się nie uda, zwrócone
    //zostaną informacje o błędzie.
    property OnAfterStart: TLiveOnOpenIsError read FOnAfterStart write FOnAfterStart;
    property OnAfterStop: TLiveOnIsActive read FOnAfterStop write FOnAfterStop;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnRestarted: TNotifyEvent read FOnRestarted write FOnRestarted;
    property OnError: TLiveOnError read FOnError write FOnError;
  end;

procedure Register;

implementation

uses
  ecode_unit;

procedure Register;
begin
  {$I livechat_icon.lrs}
  RegisterComponents('lNet',[TLiveChat]);
end;

{ TLiveChat }

procedure TLiveChat._OnReadData(Sender: TObject);
var
  s,pom: string;
  sdate,stime: string;
  i,a,indeks: integer;
  b_dodaj: boolean;
  ss: TStringList;
  bstop,brestart: boolean;
begin
  //errors messages
  if proc.Stderr.NumBytesAvailable>0 then
  begin
    bstop:=false;
    brestart:=false;
    ss:=TStringList.Create;
    try
      ss.LoadFromStream(proc.Stderr);
      if assigned(FOnError) then FOnError(ss,bstop,brestart);
    finally
      ss.Free;
    end;
    if bstop then
    begin
      Stop;
      exit;
    end else
    if brestart then
    begin
      _Restart;
      exit;
    end;
  end;
  //chat messages
  if proc.Output.NumBytesAvailable>0 then
  begin
    if assigned(FOnBeforeRecv) then FOnBeforeRecv(self);
    bufor.LoadFromStream(proc.Output);
    //proc.Stderr;
    for i:=0 to bufor.Count-1 do
    begin
      s:=trim(bufor[i]);
      if s='========== live ===========' then continue;
      //writeln('BUFOR: ',s);
      if i mod 2 = 0 then
      begin
        //2023-03-01 12:47:23 Zeeshan hdjshdjsd
        a:=pos(' ',s);
        pom:=copy(s,1,a-1);
        if pom='--->' then
        begin
          delete(s,1,a);
          a:=pos(' ',s);
          sdate:=copy(s,1,a-1);
          delete(s,1,a);
          a:=pos(' ',s);
          stime:=copy(s,1,a-1);
          delete(s,1,a);
          nick:=s;
          czas:=StrToDateTime(sdate+' '+stime);
        end else nick:=s;
      end else begin
        message:=s;
        //---> 2023-03-01 12:47:23 Zeeshan hdjshdjsd
        if restart_now then
        begin
          if czas<last_czas then b_dodaj:=false else
          if czas=last_czas then b_dodaj:=not last_nick.Find(FormatDateTime('yyyymmddhhnnss',czas)+nick,indeks);
          if b_dodaj then
          begin
            restart_now:=false;
            if assigned(FOnRestarted) then FOnRestarted(self);
          end;
        end else begin
          last_czas:=czas;
          last_nick.Add(FormatDateTime('yyyymmddhhnnss',czas)+nick);
          if last_nick.Count>10 then last_nick.Delete(0);
          b_dodaj:=true;
        end;
        if (nick<>'') and (message<>'') and b_dodaj then if assigned(FOnReceive) then FOnReceive(czas,nick,message);
      end;
    end;
    if assigned(FOnAfterRecv) then FOnAfterRecv(self);
  end;
end;

procedure TLiveChat._OnTimer(Sender: TObject);
begin
  if proc.ExitStatus<>-1 then if FReStarting then
  begin
    restart_now:=true;
    proc.Execute;
  end else Stop;
end;

procedure TLiveChat._Restart;
begin
  proc.Terminate(0);
  restart_now:=true;
  proc.Execute;
end;

constructor TLiveChat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRunning:=false;
  FService:=svYoutube;
  FReStarting:=false;
  FVideoID:='';
  FPipeBufferSize:=1024;
  nick:='';
  message:='';
  bufor:=TStringList.Create;
  last_nick:=TStringList.Create;
  last_nick.Sorted:=true;
  restart_now:=false;
  timer:=TTimer.Create(self);
  timer.Enabled:=false;
  timer.Interval:=500;
  timer.OnTimer:=@_OnTimer;
  proc:=TAsyncProcess.Create(self);
  proc.Executable:='yttool';
  proc.Options:=[poUsePipes,poNoConsole];
  proc.PipeBufferSize:=FPipeBufferSize;
  proc.OnReadData:=@_OnReadData;
end;

destructor TLiveChat.Destroy;
begin
  proc.Free;
  bufor.Free;
  last_nick.Free;
  inherited Destroy;
end;

procedure TLiveChat.Start;
var
  error_cn,error_message: string;
begin
  if FRunning then exit;
  if assigned(FOnBeforeStart) then FOnBeforeStart(self);
  try
    try
      last_czas:=0;
      last_nick.Clear;
      restart_now:=false;
      proc.PipeBufferSize:=FPipeBufferSize;
      proc.Parameters.Clear;
      proc.Parameters.Add('--livechat');
      proc.Parameters.Add(FVideoID);
      proc.Execute;
      FRunning:=true;
      timer.Enabled:=true;
      if assigned(FOnStart) then FOnStart(self);
    except
      on E: Exception do
      begin
        error_cn:=E.ClassName;
        error_message:=E.Message;
        FRunning:=false;
      end;
    end;
  finally
    if assigned(FOnAfterStart) then FOnAfterStart(not FRunning,error_cn,error_message);
  end;
end;

procedure TLiveChat.Stop;
begin
  if not FRunning then exit;
  if assigned(FOnBeforeStop) then FOnBeforeStop(self);
  timer.Enabled:=false;
  proc.Terminate(0);
  FRunning:=false;
  if assigned(FOnStop) then FOnStop(self);
  proc.Parameters.Clear;
  last_czas:=0;
  last_nick.Clear;
  restart_now:=false;
  if assigned(FOnAfterStop) then FOnAfterStop(FRunning);
end;

procedure TLiveChat.Restart;
begin
  if not FRunning then exit;
  _Restart;
end;

end.
