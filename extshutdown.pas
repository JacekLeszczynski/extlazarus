unit ExtShutdown;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TExtShutdown }

  TExtShutdown = class(TComponent)
  private

  protected

  public
    procedure execute;
  published

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

procedure TExtShutdown.execute;
var
  proc: TProcess;
begin
  {$IFDEF LINUX}
  proc:=TProcess.Create(self);
  try
    proc.CommandLine:='qdbus org.kde.ksmserver /KSMServer logout 0 2 2';
    proc.Execute;
  finally
    proc.Free;
  end;
  {$ENDIF}
  {$IFDEF FREEBSD}
  proc:=TProcess.Create(self);
  try
    proc.CommandLine:='shutdown -p now';
    proc.Execute;
  finally
    proc.Free;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  LazarusExitWindows(EWX_FORCE or EWX_POWEROFF);
  {$ENDIF}
end;

end.
