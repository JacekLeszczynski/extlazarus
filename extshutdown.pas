unit ExtShutdown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

type

  { TExtShutdown }

  TExtShutdownOnShutdownMode = (smNone,smQDbusKDE,smGnome,smShutdownP1,smShutdownP2,smWindows,smCustom);
  TExtShutdownOnMonitorMode = (mmGetMode, mmOn, mmOff, mmStandby, mmSuspend);
  TExtShutdown = class(TComponent)
  private
    FCustomCommand: string;
    FMode: TExtShutdownOnShutdownMode;
    FOnAfterShutdown: TNotifyEvent;
    FOnBeforeShutdown: TNotifyEvent;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure execute;
    procedure monitor(var aMonitorMode: TExtShutdownOnMonitorMode);
    function monitor: boolean;
  published
    {Mode:}
    { KDE    - shutdown KDE mode}
    { Gnome  - gnome-session-quit --power-off --force}
    { P1     - execute: "shutdown -p now"}
    { P2     - execute: "shutdown -P now"}
    { Custom - execute custom command}
    property Mode: TExtShutdownOnShutdownMode read FMode write FMode;
    {Execute command is Mode = Custom}
    property CustomCommand: string read FCustomCommand write FCustomCommand;
    property OnBeforeShutdown: TNotifyEvent read FOnBeforeShutdown write FOnBeforeShutdown;
    property OnAfterShutdown: TNotifyEvent read FOnAfterShutdown write FOnAfterShutdown;
  end;

procedure Register;

implementation

uses
  {$IFDEF MSWINDOWS}
  windows,
  {$ENDIF}
  process;

procedure Register;
begin
  {$I extshutdown_icon.lrs}
  RegisterComponents('System',[TExtShutdown]);
end;

{
  EWX_FORCE       - Wymusza zamknięcie wszystkich procesów. Jeżeli ta flaga jest ustawiona, system nie wysyła komunikatów WM_QUERYENDSESSION i WM_ENDSESSION do aktualnie uruchomionych aplikacji, czyli nie pyta ich o ewentualne zapisanie danych. Dlatego powinieneś używać tej flagi w nagłych wypadkach.
  EWX_FORCEIFHUNG - Wymusza zamknięcie wszystkich procesów jeśli nie odpowiadają one na komunikaty WM_QUERYENDSESSION i WM_ENDSESSION. Flaga ta jest ignorowana, jeśli zostanie użyta flaga EWX_FORCE.
  EWX_LOGOFF      - Zamyka wszystkie uruchomione aplikacje i wylogowywuje użytkownika.
  EWX_POWEROFF    - Zamyka system i wyłącza komputer.
  EWX_REBOOT      - Zamyka a następnie restartuje system.
  EWX_SHUTDOWN    - Zamyka system do momentu kiedy będzie można bezpiecznie wyłączyć komputer, wszystkie tymczasowe pliki (bufory, pliki wymiany) zostają wyczyszczone (ewentualnie usunięte) a uruchomione procesy zakończone.
  źródło: https://4programmers.net/Delphi/Artyku%C5%82y/Zamykanie_systemu_w_Delphi
}

{$IFDEF MSWINDOWS}
function LazarusExitWindows(Flags: Word): Boolean;
var
  iVersionInfo: TOSVersionInfo;
  iToken: THandle;
  iPriveleg: TTokenPrivileges;
  iaresult: DWord;
begin
  Result:=False;
  FillChar(iPriveleg,SizeOf(iPriveleg),#0);
  iVersionInfo.dwOSVersionInfoSize:=SizeOf(TOSVersionInfo);
  GetVersionEx(iVersionInfo);
  if iVersionInfo.dwPlatformId<>VER_PLATFORM_WIN32_NT then
    Result:=ExitWindowsEx(Flags,0)
  else
    if OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,iToken) then
      if LookupPrivilegeValue(NIL,'SeShutdownPrivilege',iPriveleg.Privileges[0].Luid) then
      begin
        iPriveleg.PrivilegeCount:=1;
        iPriveleg.Privileges[0].Attributes:=SE_PRIVILEGE_ENABLED;
        if AdjustTokenPrivileges(iToken,False,iPriveleg,Sizeof(iPriveleg),iPriveleg,iaresult) then Result:=ExitWindowsEx(Flags,0);
      end;
end;
{$ENDIF}

{ TExtShutdown }

constructor TExtShutdown.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF UNIX}
  FMode:=smNone;
  {$ENDIF}
  {$IFDEF WINDOWS}
  FMode:=smWindows;
  {$ENDIF}
end;

destructor TExtShutdown.Destroy;
begin
  inherited Destroy;
end;

procedure TExtShutdown.execute;
var
  proc: TProcess;
begin
  if Assigned(FOnBeforeShutdown) then FOnBeforeShutdown(self);
  if FMode<>smNone then
  begin
    {$IFDEF UNIX}
    proc:=TProcess.Create(self);
    try //(smQDbusKDE,smShutdownP1,smShutdownP2,smWindows,smCustom)
      case FMode of
        smQDbusKDE: proc.CommandLine:='qdbus org.kde.ksmserver /KSMServer logout 0 2 2';
        smGnome: proc.CommandLine:='gnome-session-quit --power-off --force --no-prompt';
        smShutdownP1: proc.CommandLine:='shutdown -p now';
        smShutdownP2: proc.CommandLine:='shutdown -P now';
        smCustom: proc.CommandLine:=FCustomCommand;
      end;
      proc.Execute;
    finally
      proc.Free;
    end;
    if FMode=smGnome then
    begin
      try
        proc:=TProcess.Create(self);
        proc.CommandLine:='gnome-session-quit --power-off --force --no-prompt';
        proc.Execute;
      finally
        proc.Free;
      end;
    end;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    if FMode=smWindows then LazarusExitWindows(EWX_FORCE or EWX_POWEROFF) else
    begin
      proc:=TProcess.Create(self);
      try
        proc.CommandLine:=FCustomCommand;
        proc.Execute;
      finally
        proc.Free;
      end;
    end;
    {$ENDIF}
  end;
  if Assigned(FOnAfterShutdown) then FOnAfterShutdown(self);
end;

procedure TExtShutdown.monitor(var aMonitorMode: TExtShutdownOnMonitorMode);
var
  proc: TProcess;
  ss: TStringList;
begin
  {$IFDEF UNIX}
  proc:=TProcess.Create(self);
  try
    if aMonitorMode=mmGetMode then
    begin
      (* odczytanie mode monitora *)
      proc.Options:=[poWaitOnExit,poUsePipes];
      proc.CommandLine:='xset -q';
      proc.Execute;
      ss.LoadFromStream(proc.Output);
      if pos('Monitor is On',ss.Text)>0 then aMonitorMode:=mmOn else
      if pos('Monitor is Off',ss.Text)>0 then aMonitorMode:=mmOff else
      if pos('Monitor is Standby',ss.Text)>0 then aMonitorMode:=mmStandby else
      if pos('Monitor is Suspend',ss.Text)>0 then aMonitorMode:=mmSuspend else
      aMonitorMode:=mmOn;
    end else begin
      (* ustawienie mode monitora *)
      case aMonitorMode of
        mmOn      : proc.CommandLine:='xset -display :0.0 dpms force on';
        mmOff     : proc.CommandLine:='xset -display :0.0 dpms force off';
        mmStandby : proc.CommandLine:='xset -display :0.0 dpms force standby';
        mmSuspend : proc.CommandLine:='xset -display :0.0 dpms force suspend';
      end;
      proc.Execute;
    end;
  finally
    proc.Free;
  end;
  {$ENDIF}
end;

function TExtShutdown.monitor: boolean;
var
  a: TExtShutdownOnMonitorMode;
begin
  a:=mmGetMode;
  monitor(a);
  result:=a=mmOn;
end;

end.
