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
    FAutoScaleVector: integer;
    FBlockAutoScaleCols: string;
    wWidth: integer;
    list: TListOfInt;
    procedure SetAutoScaleCols(AValue: boolean);
    procedure AutoScaleStart;
    procedure AutoScaleStop;
    procedure SetAutoScaleVector(AValue: integer);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AutoScaleColumns;
  published
    {Włączenie autoskalowania, metoda:
      "AutoScaleColumns" - staje się aktywna.}
    property AutoScaleCols: boolean read FAutoScaleCols write SetAutoScaleCols default false;
    {Wektor przesunięcia, w razie gdyby kolumna
     była źle ustawiana}
    property AutoScaleVector: integer read FAutoScaleVector write SetAutoScaleVector;
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

procedure TDBGridPlus.SetAutoScaleCols(AValue: boolean);
begin
  if FAutoScaleCols=AValue then Exit;
  FAutoScaleCols:=AValue;
  if FAutoScaleCols then AutoScaleColumns else AutoScaleStop;
end;

procedure TDBGridPlus.AutoScaleStart;
var
  i: integer;
begin
  if list.Count>0 then AutoScaleStop;
  for i:=0 to Columns.Count-1 do list.Add(Columns[i].Width);
end;

procedure TDBGridPlus.AutoScaleStop;
var
  a,b,i: integer;
begin
  a:=Columns.Count;
  b:=list.Count;
  if b<a then a:=b;
  for i:=0 to a-1 do Columns[i].Width:=list.getItem(i);
  list.Clear;
  wWidth:=-1;
end;

procedure TDBGridPlus.SetAutoScaleVector(AValue: integer);
begin
  if FAutoScaleVector=AValue then Exit;
  FAutoScaleVector:=AValue;
  AutoscaleColumns;
end;

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
  FAutoScaleVector:=0;
  FAutoScaleCols:=false;
  FBlockAutoScaleCols:='';
end;

destructor TDBGridPlus.Destroy;
begin
  list.Free;
  inherited Destroy;
end;

procedure TDBGridPlus.AutoScaleColumns;
var
  pominiete,max,i,a: integer;
  s,pom: string;
  ind: integer;
begin
  //{ $IFDEF LAZARUSIDE}
  //exit;
  //{ $ENDIF}
  if wWidth=Width+FAutoScaleVector then exit;
  if not FAutoScaleCols then exit;
  wWidth:=Width;
  pominiete:=0; max:=0;
  if list.Count<>Columns.Count then AutoScaleStart;
  if dgIndicator in self.Options then ind:=25+FAutoScaleVector else ind:=15+FAutoScaleVector;
  (* suma wszystkich pominiętych kolumn *)
  i:=0;
  while true do
  begin
    inc(i);
    s:=GetLineToStr(FBlockAutoScaleCols,i,',');
    if s='' then break;
    a:=StrToInt(s);
    if not Columns[a].Visible then continue;
    pominiete:=pominiete+list.getItem(a);
  end;
  (* suma wszystkich kolumn *)
  for i:=0 to Columns.Count-1 do if Columns[i].Visible then max:=max+list.getItem(i);
  (* ustawienie kolumn *)
  pom:=','+FBlockAutoScaleCols+',';
  for i:=0 to Columns.Count-1 do
  begin
    if not Columns[i].Visible then continue;
    if pos(','+IntToStr(i)+',',pom)>0 then continue;
    Columns[i].Width:=round(list.getItem(i)*(Width-pominiete-ind)/(max-pominiete));
  end;
end;

end.
