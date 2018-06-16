unit ConsMixer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  FileUtil, AsyncProcess, Process;

type

  { TConsMixer }

  TConsMixer = class(TComponent)
  private
    FPlayMuted: boolean;
    FRecMuted: boolean;
    run: Tasyncprocess;
    run2: TProcess;
    FAfterPlayMute: TNotifyEvent;
    FAfterPlayUnmute: TNotifyEvent;
    FAfterRecMute: TNotifyEvent;
    FAfterRecUnmute: TNotifyEvent;
    FBeforePlayMute: TNotifyEvent;
    FBeforePlayUnmute: TNotifyEvent;
    FBeforeRecMute: TNotifyEvent;
    FBeforeRecUnmute: TNotifyEvent;
    FDefCommands: TStrings;
    FPlayMuteCmd: string;
    FPlayUnmuteCmd: string;
    FRecMuteCmd: string;
    FRecUnmuteCmd: string;
    procedure SetDefCommands(AValue: TStrings);
  protected
    procedure Execute(polecenie: string);
    procedure ExecuteWait(polecenie: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure PlayMute;
    procedure PlayUnmute;
    procedure RecMute;
    procedure RecUnmute;
  published
    property PlayMuted: boolean read FPlayMuted default false;
    property RecMuted: boolean read FRecMuted default false;
    property DefaultCommands: TStrings read FDefCommands write SetDefCommands;
    property RecMuteCmd: string read FRecMuteCmd write FRecMuteCmd;
    property RecUnmuteCmd: string read FRecUnmuteCmd write FRecUnmuteCmd;
    property PlayMuteCmd: string read FPlayMuteCmd write FPlayMuteCmd;
    property PlayUnmuteCmd: string read FPlayUnmuteCmd write FPlayUnmuteCmd;
    property BeforePlayMute: TNotifyEvent read FBeforePlayMute write FBeforePlayMute;
    property AfterPlayMute: TNotifyEvent read FAfterPlayMute write FAfterPlayMute;
    property BeforeRecMute: TNotifyEvent read FBeforeRecMute write FBeforeRecMute;
    property AfterRecMute: TNotifyEvent read FAfterRecMute write FAfterRecMute;
    property BeforePlayUnmute: TNotifyEvent read FBeforePlayUnmute write FBeforePlayUnmute;
    property AfterPlayUnmute: TNotifyEvent read FAfterPlayUnmute write FAfterPlayUnmute;
    property BeforeRecUnmute: TNotifyEvent read FBeforeRecUnmute write FBeforeRecUnmute;
    property AfterRecUnmute: TNotifyEvent read FAfterRecUnmute write FAfterRecUnmute;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I consmixer_icon.lrs}
  RegisterComponents('Multimedia',[TConsMixer]);
end;

{ TConsMixer }

procedure TConsMixer.SetDefCommands(AValue: TStrings);
begin
  FDefCommands.Assign(AValue);
end;

procedure TConsMixer.Execute(polecenie: string);
var
  L: TStrings;
  i: Integer;
begin
  L:=TStringList.Create;
  try
    L.Delimiter:=' ';
    L.DelimitedText:=polecenie;
    run.Executable:=FindDefaultExecutablePath(L[0]);
    run.Parameters.Clear;
    for i:=1 to L.Count-1 do run.Parameters.Add(L[i]);
    try
      run.Execute;
    except
      on E: Exception do E.CreateFmt('Unable to execute command %s Message: %s',[polecenie,E.Message]);
    end;
  finally
    L.Free;
  end;
end;

procedure TConsMixer.ExecuteWait(polecenie: string);
var
  L: TStrings;
  i: Integer;
begin
  L:=TStringList.Create;
  try
    L.Delimiter:=' ';
    L.DelimitedText:=polecenie;
    run2.Executable:=FindDefaultExecutablePath(L[0]);
    run2.Parameters.Clear;
    for i:=1 to L.Count-1 do run2.Parameters.Add(L[i]);
    try
      run2.Execute;
      run2.WaitOnExit;
    except
      on E: Exception do E.CreateFmt('Unable to execute command %s Message: %s',[polecenie,E.Message]);
    end;
  finally
    L.Free;
  end;
end;

constructor TConsMixer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefCommands:=TStringList.Create;
  run:=Tasyncprocess.Create(nil);
  FPlayMuted:=false;
  FRecMuted:=false;
end;

destructor TConsMixer.Destroy;
begin
  FDefCommands.Free;
  run.Free;
  inherited Destroy;
end;

procedure TConsMixer.Init;
var
  i: integer;
begin
  run2:=TProcess.Create(nil);
  try
    for i:=0 to FDefCommands.Count-1 do ExecuteWait(FDefCommands[i]);
  finally
    run2.Free;
  end;
end;

procedure TConsMixer.PlayMute;
begin
  if FPlayMuteCmd='' then exit;
  if Assigned(FBeforePlayMute) then FBeforePlayMute(self);
  Execute(FPlayMuteCmd);
  FPlayMuted:=true;
  if Assigned(FAfterPlayMute) then FAfterPlayMute(self);
end;

procedure TConsMixer.PlayUnmute;
begin
  if FPlayUnmuteCmd='' then exit;
  if Assigned(FBeforePlayUnmute) then FBeforePlayUnmute(self);
  Execute(FPlayUnmuteCmd);
  FPlayMuted:=false;
  if Assigned(FAfterPlayUnmute) then FAfterPlayUnmute(self);
end;

procedure TConsMixer.RecMute;
begin
  if FRecMuteCmd='' then exit;
  if Assigned(FBeforeRecMute) then FBeforeRecMute(self);
  Execute(FRecMuteCmd);
  FRecMuted:=true;
  if Assigned(FAfterRecMute) then FAfterRecMute(self);
end;

procedure TConsMixer.RecUnmute;
begin
  if FRecUnmuteCmd='' then exit;
  if Assigned(FBeforeRecUnmute) then FBeforeRecUnmute(self);
  Execute(FRecUnmuteCmd);
  FRecMuted:=false;
  if Assigned(FAfterRecUnmute) then FAfterRecUnmute(self);
end;

end.
