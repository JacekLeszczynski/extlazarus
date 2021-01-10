unit DBGridPlus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, DBGrids, list_unit;

type

  { TDBGridPlus }

  TDBGridPlus = class(TDBGrid)
  private
    FAutoScaleCols: boolean;
    FBlockAutoScaleCols: string;
    wWidth: integer;
    list: TListOfInt;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AutoScaleColumns;
  published
    {Włączenie autoskalowania, metoda:
      "AutoScaleColumns" - staje się aktywna.}
    property AutoScaleCols: boolean read FAutoScaleCols write FAutoScaleCols default false;
    {Wyłączenie wybranych kolumn z autoskalowania,
     wypisz ich indeksy oddzielając przecinkami.
     Przykład użycia: 0,1,4
     }
    property BlockAutoScaleCols: string read FBlockAutoScaleCols write FBlockAutoScaleCols;
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

procedure TDBGridPlus.Paint;
begin
  inherited Paint;
  AutoScaleColumns;
end;

constructor TDBGridPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  list:=TListOfInt.Create;
  wWidth:=-1;
  FAutoScaleCols:=false;
  FBlockAutoScaleCols:='';
end;

destructor TDBGridPlus.Destroy;
begin
  list.Clear;
  list.Free;
  inherited Destroy;
end;

procedure TDBGridPlus.AutoScaleColumns;
var
  pominiete,max,i,a: integer;
  s,pom: string;
  ind: integer;
begin
  if wWidth=Width then exit;
  if not FAutoScaleCols then exit;
  wWidth:=Width;
  pominiete:=0; max:=0;
  if list.isEmpty then for i:=0 to Columns.Count-1 do list.Add(Columns[i].Width);
  if dgIndicator in self.Options then ind:=25 else ind:=0;
  (* suma wszystkich pominiętych kolumn *)
  i:=0;
  while true do
  begin
    inc(i);
    s:=GetLineToStr(FBlockAutoScaleCols,i,',');
    if s='' then break;
    a:=StrToInt(s);
    if not Columns[a].Visible then continue;
    //pominiete:=pominiete+Columns[a].Width;
    pominiete:=pominiete+list.getItem(a);
  end;
  (* suma wszystkich kolumn *)
  //for i:=0 to Columns.Count-1 do if Columns[i].Visible then max:=max+Columns[i].Width;
  for i:=0 to Columns.Count-1 do if Columns[i].Visible then max:=max+list.getItem(i);
  (* ustawienie kolumn *)
  pom:=','+FBlockAutoScaleCols+',';
  for i:=0 to Columns.Count-1 do
  begin
    if not Columns[i].Visible then continue;
    if pos(','+IntToStr(i)+',',pom)>0 then continue;
    //Columns[i].Width:=round(Columns[i].Width*(Width-pominiete-ind)/(max-pominiete));
    Columns[i].Width:=round(list.getItem(i)*(Width-pominiete-ind)/(max-pominiete));
  end;
end;

end.
