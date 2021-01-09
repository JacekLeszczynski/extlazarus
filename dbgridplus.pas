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

uses
  ecode_unit;

procedure Register;
begin
  {$I dbgridplus_icon.lrs}
  RegisterComponents('Data Controls',[TDBGridPlus]);
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
