unit ExtEventLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Eventlog, LogStringGrid;

type

  { TExtEventLog }

  TExtEventLog = class(TEventLog)
  private
    FRegEvents: TEventTypes;
    FRegEventsLSG: TEventTypes;
    FLogStringGrid: TLogStringGrid;
    { Private declarations }
    function IsExecute(EventType: TEventType): boolean;
    function IsExecuteLSG(EventType: TEventType): boolean;
    procedure EventToStringGrid(EventType: TEventType; Msg: String);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    Procedure Log (EventType : TEventType; const Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Log (EventType : TEventType; const Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Log (const Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Log (const Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Warning (const Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Warning (const Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Error (const Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Error (const Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Debug (const Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Debug (const Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Info (const Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Info (const Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
  published
    { Published declarations }
    property LogStringGrid: TLogStringGrid read FLogStringGrid write FLogStringGrid;
    property RegisterEvents: TEventTypes read FRegEvents write FRegEvents;
    property RegisterEventsLSG: TEventTypes read FRegEventsLSG write FRegEventsLSG;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I exteventlog_icon.lrs}
  RegisterComponents('System',[TExtEventLog]);
end;

{ TExtEventLog }

function TExtEventLog.IsExecute(EventType: TEventType): boolean;
begin
  if EventType in FRegEvents then result:=true else result:=false;
end;

function TExtEventLog.IsExecuteLSG(EventType: TEventType): boolean;
begin
  if (FLogStringGrid<>nil) and (EventType in FRegEventsLSG) then result:=true else result:=false;
end;

procedure TExtEventLog.EventToStringGrid(EventType: TEventType; Msg: String);
begin
  {case EventType of
    etCustom:  FLogStringGrid.Login(etCustom,Msg);
    etInfo:    FLogStringGrid.LoginInfo(Msg);
    etWarning: FLogStringGrid.Login(etWarning,Msg);
    etError:   FLogStringGrid.LoginError(Msg);
    etDebug:   FLogStringGrid.Login(etDebug,Msg);
  end;}
  FLogStringGrid.Login(EventType,Msg);
end;

constructor TExtEventLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegEvents:=[etCustom,etDebug];
  FRegEventsLSG:=[etError,etInfo,etWarning];
end;

procedure TExtEventLog.Log(EventType: TEventType; const Msg: String);
begin
  if IsExecuteLSG(EventType) then EventToStringGrid(EventType,Msg);
  if not IsExecute(EventType) then exit;
  Inherited Log(EventType,Msg);
end;

procedure TExtEventLog.Log(EventType: TEventType; const Fmt: String;
  Args: array of const);
begin
  if IsExecuteLSG(EventType) then EventToStringGrid(EventType,Format(Fmt,Args));
  if not IsExecute(EventType) then exit;
  Inherited Log(EventType,Fmt);
end;

procedure TExtEventLog.Log(const Msg: String);
begin
  Log(DefaultEventType,msg);
end;

procedure TExtEventLog.Log(const Fmt: String; Args: array of const);
begin
  Log(Format(Fmt,Args));
end;

procedure TExtEventLog.Warning(const Msg: String);
begin
  Log(etWarning,Msg);
end;

procedure TExtEventLog.Warning(const Fmt: String; Args: array of const);
begin
  Warning(Format(Fmt,Args));
end;

procedure TExtEventLog.Error(const Msg: String);
begin
  Log(etError,Msg);
end;

procedure TExtEventLog.Error(const Fmt: String; Args: array of const);
begin
  Error(Format(Fmt,Args));
end;

procedure TExtEventLog.Debug(const Msg: String);
begin
  Log(etDebug,Msg);
end;

procedure TExtEventLog.Debug(const Fmt: String; Args: array of const);
begin
  Debug(Format(Fmt,Args));
end;

procedure TExtEventLog.Info(const Msg: String);
begin
  Log(etInfo,Msg);
end;

procedure TExtEventLog.Info(const Fmt: String; Args: array of const);
begin
  Info(Format(Fmt,Args));
end;

end.
