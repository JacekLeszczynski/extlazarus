unit StringsList;

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFNDEF FPC AND $IFDEF MSWINDOWS}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE LAZARUS}
{$ENDIF}

{$IFDEF LAZARUS}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TStringsList }

  TStringsListOnExecute = procedure(aName: string; aStrings: TStrings) of object;
  TStringsList = class(TComponent)
  private
    fnazwy: TStringList;
    FOnAfterExecute: TNotifyEvent;
    FOnBeforeExecute: TNotifyEvent;
    FOnExecute: TStringsListOnExecute;
    fpola: TList;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Add(aName,aStr: string): integer;
    procedure Execute;
  published
    property OnExecute: TStringsListOnExecute read FOnExecute write FOnExecute;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF LAZARUS}
  {$I stringslist_icon.lrs}
  {$ENDIF}
  RegisterComponents('Misc',[TStringsList]);
end;

{ TStringsList }

constructor TStringsList.Create(AOwner: TComponent);
begin
  fnazwy:=TStringList.Create;
  fpola:=TList.Create;
  inherited Create(AOwner);
end;

destructor TStringsList.Destroy;
begin
  fnazwy.Free;
  fpola.Free;
  inherited Destroy;
end;

procedure TStringsList.Clear;
var
  i: integer;
begin
  for i:=0 to fpola.Count-1 do TStringList(fpola[i]).Free;
  fnazwy.Clear;
  fpola.Clear;
end;

function TStringsList.Count: integer;
begin
  result:=fnazwy.Count;
end;

function TStringsList.Add(aName, aStr: string): integer;
var
  a,i: integer;
begin
  a:=-1;
  (* sprawdzam czy żądana lista już istnieje *)
  for i:=0 to fnazwy.Count-1 do
  begin
    if fnazwy[i]=aName then
    begin
      a:=i;
      break;
    end;
  end;
  (* jeśli lista nie istnieje - tworzę ją *)
  if a=-1 then
  begin
    a:=fnazwy.Add(aName);
    fpola.Add(TStringList.Create);
  end;
  (* dodaję zawartość *)
  TStringList(fpola[a]).Add(aStr);
  result:=a;
end;

procedure TStringsList.Execute;
var
  i: integer;
begin
  if not assigned(FOnExecute) then exit;
  if assigned(FOnBeforeExecute) then FOnBeforeExecute(Self);
  for i:=0 to fnazwy.Count-1 do FOnExecute(fnazwy[i],TStringList(fpola[i]));
  if assigned(FOnAfterExecute) then FOnAfterExecute(Self);
end;

end.
