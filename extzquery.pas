unit extzquery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ZDataset;

type

  { TZQueryPlus }

  TZQueryPlus = class(TZQuery)
  private
    FSQLDef: TStrings;
    FSQLScript: TStrings;
    procedure SetSQLDef(AValue: TStrings);
    procedure SetSQLScript(AValue: TStrings);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoBeforeOpen; override;
    procedure ClearDefs;
    procedure AddDef(aDef,aValue: string);
  published
  end;

procedure Register;

implementation

const
  textseparator='"';

procedure Register;
begin
  {$I extzquery_icon.lrs}
  RegisterComponents('Zeos Access',[TZQueryPlus]);
end;

function GetLineToStr(s:string;l:integer;separator:char;wynik:string=''):string;
var
  i,ll,dl: integer;
  b: boolean;
begin
  b:=false;
  dl:=length(s);
  ll:=1;
  s:=s+separator;
  for i:=1 to length(s) do
  begin
    if s[i]=textseparator then b:=not b;
    if (not b) and (s[i]=separator) then inc(ll);
    if ll=l then break;
  end;
  if ll=1 then dec(i);
  delete(s,1,i);
  b:=false;
  for i:=1 to length(s) do
  begin
    if s[i]=textseparator then b:=not b;
    if (not b) and (s[i]=separator) then break;
  end;
  delete(s,i,dl);
  if (s<>'') and (s[1]=textseparator) then
  begin
    delete(s,1,1);
    delete(s,length(s),1);
  end;
  if s='' then s:=wynik;
  result:=s;
end;

{ TZQueryPlus }

procedure TZQueryPlus.SetSQLScript(AValue: TStrings);
begin
  if FSQLScript.Text=AValue.Text then Exit;
  FSQLScript.Assign(AValue);
end;

procedure TZQueryPlus.SetSQLDef(AValue: TStrings);
begin
  if FSQLDef.Text=AValue.Text then Exit;
  FSQLDef.Assign(AValue);
end;

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
