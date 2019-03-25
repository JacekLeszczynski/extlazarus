unit ArchitectureOSInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TArchitectureOsInfoElements = (osNone,osWindows86,osWindows64,osLinux86,osLinux64,osFreeBSD86,osFreeBSD64);

  { TArchitectureOSInfo }

  TArchitectureOSInfo = class(TComponent)
  private
    FOsInfo: TArchitectureOsInfoElements;
    function TestOsInfo: TArchitectureOsInfoElements;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OsInfo: TArchitectureOsInfoElements read FOsInfo; //Architecture Information.
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I architectureosinfo_icon.lrs}
  RegisterComponents('System',[TArchitectureOSInfo]);
end;

{ TArchitectureOSInfo }

function TArchitectureOSInfo.TestOsInfo: TArchitectureOsInfoElements;
begin
  result:=osNone;
  {$IFDEF Windows}
  {$if defined(cpu64)}
  result:=osWindows64;
  {$else}
  result:=osWindows86;
  {$endif}
  {$ENDIF}
  {$if defined(cpu64) and defined(linux) }
  result:=osLinux64;
  {$ENDIF}
  {$if defined(cpu86) and defined(linux)}
  result:=osLinux86;
  {$ENDIF}
  {$IFDEF freebsd}
  {$if defined(cpu64)}
  result:=osFreeBSD64;
  {$else}
  result:=osFreeBSD86;
  {$endif}
  {$ENDIF}
end;

constructor TArchitectureOSInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOsInfo:=TestOsInfo;
end;

end.
