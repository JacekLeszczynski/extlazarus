unit zqueryplus;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ZDataset;
  Classes, SysUtils, LResources, ZDataset;

type

  { TZQueryPlus }

  TZQueryPlus = class(TZQuery)
  private
    FBeforeOpenII: TNotifyEvent;
    FSQLDef: TStrings;
    FSQLScript: TStrings;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoBeforeOpen; override;
    procedure ClearDefs;
    procedure AddDef(aDef,aValue: string);
  published
    property BeforeOpenII: TNotifyEvent read FBeforeOpenII write FBeforeOpenII;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I zqueryplus_icon.lrs}
  RegisterComponents('Zeos Access',[TZQueryPlus]);
end;

{ TZQueryPlus }

constructor TZQueryPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQLScript:=TStringList.Create;
  FSQLDef:=TStringList.Create;
end;

destructor TZQueryPlus.Destroy;
begin
  FSQLScript.Free;
  FSQLDef.Free;
  inherited Destroy;
end;

procedure TZQueryPlus.DoBeforeOpen;
var
  pom,s,s1,s2: string;
  i: integer;
  a,b: integer;
begin
  if FSqlScript.Count=0 then FSqlScript.Assign(SQL);
  inherited DoBeforeOpen;
  pom:=FSQLScript.Text;
  for i:=0 to FSQLDef.Count-1 do
  begin
    s:=FSQLDef[i];
    a:=pos('=',s);
    if a=0 then continue;
    s1:=s; system.delete(s1,a,1000);
    s2:=s; system.delete(s2,1,a);
    pom:=StringReplace(pom,s1,s2,[rfReplaceAll,rfIgnoreCase]);
  end;
  SQL.Clear;
  SQL.AddText(pom);
  if assigned(FBeforeOpenII) then FBeforeOpenII(self);
end;

procedure TZQueryPlus.ClearDefs;
begin
  FSQLDef.Clear;
end;

procedure TZQueryPlus.AddDef(aDef, aValue: string);
begin
  FSQLDef.Add(aDef+'='+aValue);
end;

end.
