unit DBGridPlus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, DBGrids;

type

  { TDBGridPlus }

  TDBGridPlus = class(TDBGrid)
  private
    FBlockAutoResizeCols: string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AutoScaleColumns;
  published
    {Wyłączenie wybranych kolumn z metody:
       "AutoScaleColumn"
     Wypisz ich indeksy oddzielając
     przecinkami, np: 0,1,4
     }
    property BlockAutoResizeCols: string read FBlockAutoResizeCols write FBlockAutoResizeCols;
  end;

procedure Register;

implementation

const
  textseparator='"';

procedure Register;
begin
  {$I dbgridplus_icon.lrs}
  RegisterComponents('Data Controls',[TDBGridPlus]);
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

{ TExtDBGrid }

constructor TDBGridPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBlockAutoResizeCols:='';
end;

destructor TDBGridPlus.Destroy;
begin
  inherited Destroy;
end;

procedure TDBGridPlus.AutoScaleColumns;
var
  pominiete,max,i,a: integer;
  s,pom: string;
begin
  pominiete:=0; max:=0;
  (* suma wszystkich pominiętych kolumn *)
  i:=0;
  while true do
  begin
    inc(i);
    s:=GetLineToStr(FBlockAutoResizeCols,i,',');
    if s='' then break;
    a:=StrToInt(s);
    if not Columns[a].Visible then continue;
    pominiete:=pominiete+Columns[a].Width;
  end;
  (* suma wszystkich kolumn *)
  for i:=0 to Columns.Count-1 do if Columns[i].Visible then max:=max+Columns[i].Width;
  (* ustawienie kolumn *)
  pom:=','+FBlockAutoResizeCols+',';
  for i:=0 to Columns.Count-1 do
  begin
    if not Columns[i].Visible then continue;
    if pos(','+IntToStr(i)+',',pom)>0 then continue;
    Columns[i].Width:=round(Columns[i].Width*(Width-pominiete-28)/(max-pominiete));
  end;
end;

end.
