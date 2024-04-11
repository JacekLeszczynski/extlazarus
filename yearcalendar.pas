unit YearCalendar;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  ExtCtrls, Controls, Graphics;

type

  { TYearCalendar }

  TYearCalendarOnDrawClick = procedure(aSender: TObject; aX,aY: integer; IsDate: boolean; aDate: TDate; aText: string; aColor: TColor) of object;
  TYearCalendarOnDrawColor = procedure(aSender: TObject; aX,aY: integer; aDate: TDate; var aColor: TColor) of object;
  TYearCalendarOnSelectCell = procedure(aSender: TObject; aX,aY: integer) of object;
  TYearCalendarOnSelectDate = procedure(aSender: TObject; aDate: TDate) of object;
  TYearCalendarOnYearChanged = procedure(aSender: TObject; aYear: word) of object;
  TYearCalendar = class(TComponent)
  private
    FBKColor: TColor;
    FBKColorHeaders: TColor;
    FFontColorDaysOff: TColor;
    FNotDrawLines: boolean;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnSelectCell: TYearCalendarOnSelectCell;
    FOnSelectDate: TYearCalendarOnSelectDate;
    FOnYearChanged: TYearCalendarOnYearChanged;
    v_zaznaczenie_x,v_zaznaczenie_y: integer;
    FOnDrawClick: TYearCalendarOnDrawClick;
    FOnDrawColor: TYearCalendarOnDrawColor;
    FPanel: TPanel;
    cal: TPaintBox;
    FRok: word;
    procedure ClearCanvas;
    procedure InicjujWartosci;
    procedure RysujSiatke;
    procedure RysujTlo(aX,aY: integer; aColor: TColor);
    procedure SetRok(AValue: word);
    procedure WypelnijDni;
    procedure WypelnijDzien(aX,aY: integer);
    procedure Pisz(aX,aY: integer; aStr: string; aVecX: integer = 0; aVecY: integer = 0; aColor: TColor = clBlack);
    procedure Zaznacz(aX,aY: integer);
    procedure _OnClick(Sender: TObject);
    procedure _OnDblClick(Sender: TObject);
    procedure _OnPaint(Sender: TObject);
    procedure _OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    {$IFDEF MSWINDOWS}
    procedure wLine(StartX,StartY,StopX,StopY: integer);
    {$ENDIF}
  protected
    procedure CreateObjects;
    procedure DestroyObjects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init;
    {$IFDEF MSWINDOWS}
    procedure DeInit;
    {$ENDIF}
    procedure Refresh;
    procedure SetColor(aX,aY: integer; aColor: TColor); overload;
    procedure SetColor(aDate: TDate; aColor: TColor); overload;
    procedure SetYear(aDate: TDate);
  published
    property Panel: TPanel read FPanel write FPanel;
    property Year: word read FRok write SetRok;
    property BKColor: TColor read FBKColor write FBKColor default clWhite;
    property BKColorHeaders: TColor read FBKColorHeaders write FBKColorHeaders default clCream;
    property FontColorDaysOff: TColor read FFontColorDaysOff write FFontColorDaysOff default clRed;
    property NotDrawLines: boolean read FNotDrawLines write FNotDrawLines default false;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDrawClick: TYearCalendarOnDrawClick read FOnDrawClick write FOnDrawClick;
    property OnDrawColor: TYearCalendarOnDrawColor read FOnDrawColor write FOnDrawColor;
    property OnSelectCell: TYearCalendarOnSelectCell read FOnSelectCell write FOnSelectCell;
    property OnSelectDate: TYearCalendarOnSelectDate read FOnSelectDate write FOnSelectDate;
    property OnYearChanged: TYearCalendarOnYearChanged read FOnYearChanged write FOnYearChanged;
  end;

procedure Register;

implementation

uses
  DateUtils, Types;

type
  TRec = record
    dzien: integer;
    color_background: TColor;
    text: string;
  end;

const
  C_COLS = 37;
  C_ROWS = 12;
  C_COL_WIDTH = 80;
  C_ROW_HEIGHT = 50;
  C_DNI: array [1..7] of string = ('po','wt','śr','cz','pi','so','ni');

var
  xWidth,yHeight: double;
  rec_rows: array [0..C_ROWS+1] of integer;
  rec_cols: array [0..C_COLS+1] of integer;
  rec: array [0..C_COLS,0..C_ROWS] of TRec;

procedure Register;
begin
  {$IFDEF FPC}
  {$I yearcalendar_icon.lrs}
  RegisterComponents('Misc',[TYearCalendar]);
  {$ELSE}
  RegisterComponents('NobleSecurities',[TYearCalendar]);
  {$ENDIF}
end;

{ TYearCalendar }

procedure TYearCalendar.ClearCanvas;
begin
  cal.Canvas.Brush.Color:=FBKColor;
  cal.Canvas.FillRect(cal.ClientRect);
end;

procedure TYearCalendar.InicjujWartosci;
var
  i: integer;
begin
  ClearCanvas;
  xWidth:=(cal.Width-C_COL_WIDTH)/(C_COLS);
  yHeight:=(cal.Height-C_ROW_HEIGHT)/(C_ROWS);
  rec_cols[0]:=0;
  rec_rows[0]:=0;
  for i:=0 to C_COLS do rec_cols[i+1]:=round(i*xWidth)+C_COL_WIDTH;
  for i:=0 to C_ROWS do rec_rows[i+1]:=round(i*yHeight)+C_ROW_HEIGHT;
end;

procedure TYearCalendar.Refresh;
begin
  _OnPaint(self);
end;

procedure TYearCalendar.RysujSiatke;
var
  i,j,w1,w2: integer;
  pom,c: TColor;
begin
  pom:=cal.Canvas.Brush.Color;
  cal.Canvas.Pen.Color:=clGray;
  if FNotDrawLines then
  begin
    w1:=rec_rows[1];
    w2:=rec_cols[1];
  end else begin
    w1:=cal.Width;
    w2:=cal.Width;
  end;
  {$IFDEF FPC}
  for i:=0 to 1 do cal.Canvas.Line(rec_cols[i],0,rec_cols[i],cal.Width);
  for i:=2 to C_COLS+1 do cal.Canvas.Line(rec_cols[i],0,rec_cols[i],w1);
  for i:=0 to 1 do cal.Canvas.Line(0,rec_rows[i],cal.Width,rec_rows[i]);
  for i:=2 to C_ROWS+1 do cal.Canvas.Line(0,rec_rows[i],w2,rec_rows[i]);
  {$ELSE}
  for i:=0 to 1 do wLine(rec_cols[i],0,rec_cols[i],cal.Width);
  for i:=2 to C_COLS+1 do wLine(rec_cols[i],0,rec_cols[i],w1);
  for i:=0 to 1 do wLine(0,rec_rows[i],cal.Width,rec_rows[i]);
  for i:=2 to C_ROWS+1 do wLine(0,rec_rows[i],w2,rec_rows[i]);
  {$ENDIF}
  (* HEADERS *)
  for i:=0 to C_COLS do RysujTlo(i,0,FBKColorHeaders);
  for i:=1 to C_ROWS do RysujTlo(0,i,FBKColorHeaders);
  cal.Canvas.Brush.Color:=FBKColorHeaders;
  pisz(0,0,'2024',4);
  j:=0;
  for i:=1 to c_cols do
  begin
    inc(j);
    if j>7 then j:=1;
    if j<6 then c:=clBlack else c:=FFontColorDaysOff;
    pisz(i,0,C_DNI[j],4,0,c);
  end;
  pisz(0,1,'Styczeń',4);
  pisz(0,2,'Luty',4);
  pisz(0,3,'Marzec',4);
  pisz(0,4,'Kwiecień',4);
  pisz(0,5,'Maj',4);
  pisz(0,6,'Czerwiec',4);
  pisz(0,7,'Lipiec',4);
  pisz(0,8,'Sierpień',4);
  pisz(0,9,'Wrzesień',4);
  pisz(0,10,'Październik',4);
  pisz(0,11,'Listopad',4);
  pisz(0,12,'Grudzień',4);
  cal.Canvas.Brush.Color:=pom;
  WypelnijDni;
end;

procedure TYearCalendar.RysujTlo(aX, aY: integer; aColor: TColor);
var
  pom: TColor;
  a,b,c,d: integer;
  {$IFDEF MSWINDOWS}
  Rect: TRect;
  {$ENDIF}
begin
  rec[aX,aY].color_background:=aColor;
  a:=rec_cols[aX]+1;
  b:=rec_rows[aY]+1;
  c:=rec_cols[aX+1];
  d:=rec_rows[aY+1];
  pom:=cal.Canvas.Brush.Color;
  cal.Canvas.Brush.Color:=aColor;
  {$IFDEF MSWINDOWS}
  Rect.Left:=a;
  Rect.Top:=b;
  Rect.Right:=c;
  Rect.Bottom:=d;
  cal.Canvas.FillRect(Rect);
  {$ELSE}
  cal.Canvas.FillRect(a,b,c,d);
  {$ENDIF}
  cal.Canvas.Brush.Color:=pom;
end;

procedure TYearCalendar.SetRok(AValue: word);
begin
  if FRok=AValue then Exit;
  FRok:=AValue;
  if cal<>nil then Init;
  if assigned(FOnYearChanged) then FOnYearChanged(self,FRok);
end;

procedure TYearCalendar.SetYear(aDate: TDate);
var
  r,m,d: Word;
begin
  DecodeDate(aDate,r,m,d);
  SetRok(r);
end;

{$IFDEF MSWINDOWS}
procedure TYearCalendar.wLine(StartX, StartY, StopX, StopY: integer);
var
  a: TPoint;
begin
  a.X:=StartX;
  a.Y:=StartY;
  cal.Canvas.PenPos:=a;
  cal.Canvas.LineTo(StopX,StopY);
end;
{$ENDIF}

procedure TYearCalendar.WypelnijDni;
var
  i,j: integer;
  data,dt1,dt2: TDate;
  dzien: word;
  pom,color: TColor;
begin
  for i:=0 to c_rows do for j:=0 to c_cols do rec[j,i].dzien:=-1;
  for i:=1 to c_rows do for j:=1 to c_cols do rec[j,i].color_background:=FBKColor;
  for i:=1 to c_rows do
  begin
    dt1:=EncodeDate(FRok,i,1);
    if i=12 then dt2:=EncodeDate(FRok+1,1,1)-1 else dt2:=EncodeDate(FRok,i+1,1)-1;
    dzien:=DayOfTheWeek(dt1);
    for j:=1 to trunc(dt2-dt1+1) do
    begin
      color:=FBKColor;
      data:=EncodeDate(FRok,i,j);
      if assigned(FOnDrawColor) then FOnDrawColor(self,j+dzien-1,i,data,color);
      if color<>FBKColor then
      begin
        pom:=cal.Canvas.Brush.Color;
        cal.Canvas.Brush.Color:=color;
        RysujTlo(j+dzien-1,i,color);
        rec[j+dzien-1,i].dzien:=j;
        pisz(j+dzien-1,i,IntToStr(j),6,0,clBlack);
        rec[j+dzien-1,i].color_background:=color;
        cal.Canvas.Brush.Color:=pom;
      end else begin
        rec[j+dzien-1,i].dzien:=j;
        pisz(j+dzien-1,i,IntToStr(j),6,0,clBlack);
        rec[j+dzien-1,i].color_background:=FBKColor;
      end;
    end;
  end;
end;

procedure TYearCalendar.WypelnijDzien(aX, aY: integer);
var
  pom: TColor;
begin
  pom:=cal.Canvas.Brush.Color;
  cal.Canvas.Brush.Color:=rec[aX,aY].color_background;
  pisz(aX,aY,IntToStr(rec[aX,aY].dzien),6,0,clBlack);
  cal.Canvas.Brush.Color:=pom;
end;

procedure TYearCalendar.Pisz(aX, aY: integer; aStr: string; aVecX: integer;
  aVecY: integer; aColor: TColor);
var
  c: TColor;
  a,b: integer;
begin
  rec[aX,aY].text:=aStr;
  a:=rec_cols[aX]+aVecX+2;
  b:=rec_rows[aY]+aVecY+2;
  c:=cal.Canvas.Font.Color;
  cal.Canvas.Font.Color:=aColor;
  cal.Canvas.TextOut(a,b,aStr);
  cal.Canvas.Font.Color:=c;
end;

procedure TYearCalendar.Zaznacz(aX, aY: integer);
var
  a,b,c,d: integer;
  pom: TColor;
begin
  (* odznaczenie *)
  if (v_zaznaczenie_x>-1) and (v_zaznaczenie_y>-1) then
  begin
    RysujTlo(v_zaznaczenie_x,v_zaznaczenie_y,rec[v_zaznaczenie_x,v_zaznaczenie_y].color_background);
    WypelnijDzien(v_zaznaczenie_x,v_zaznaczenie_y);
    v_zaznaczenie_x:=-1;
    v_zaznaczenie_y:=-1;
  end;
  (* zaznaczenie *)
  a:=rec_cols[aX]+1;
  b:=rec_rows[aY]+1;
  c:=rec_cols[aX+1];
  d:=rec_rows[aY+1];
  pom:=cal.Canvas.Brush.Color;
  cal.Canvas.Brush.Color:=rec[aX,aY].color_background;
  cal.Canvas.Rectangle(a,b,c,d);
  pisz(aX,aY,IntToStr(rec[aX,aY].dzien),6,0,clBlack);
  cal.Canvas.Brush.Color:=pom;
  v_zaznaczenie_x:=aX;
  v_zaznaczenie_y:=aY;
end;

procedure TYearCalendar._OnClick(Sender: TObject);
begin
  if assigned(FOnClick) then FOnClick(self);
end;

procedure TYearCalendar._OnDblClick(Sender: TObject);
begin
  if assigned(FOnDblClick) then FOnDblClick(self);
end;

procedure TYearCalendar._OnPaint(Sender: TObject);
begin
  InicjujWartosci;
  RysujSiatke;
end;

procedure TYearCalendar._OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vx,vy: integer;
  dt: TDate;
  i,d: integer;
  b: boolean;
begin
  if Button=mbLeft then
  begin
    b:=true;
    vx:=-1;
    for i:=0 to c_cols do if (X>=rec_cols[i]) and (X<rec_cols[i+1]) then
    begin
      vx:=i;
      break;
    end;
    vy:=-1;
    for i:=0 to c_rows do if (Y>=rec_rows[i]) and (Y<rec_rows[i+1]) then
    begin
      vy:=i;
      break;
    end;
    d:=rec[vx,vy].dzien;
    if d=-1 then b:=false;
    if b then
    begin
      zaznacz(vx,vy);
      if b then dt:=EncodeDate(FRok,vy,d);
      if assigned(FOnSelectDate) then FOnSelectDate(self,dt);
    end;
    if assigned(FOnSelectCell) then FOnSelectCell(self,vx,vy);
    if assigned(FOnDrawClick) then FOnDrawClick(self,vx,vy,b,dt,rec[vx,vy].text,rec[vx,vy].color_background);
  end;
end;

procedure TYearCalendar.CreateObjects;
begin
  if cal<>nil then exit;
  cal:=TPaintBox.Create(FPanel);
  cal.Parent:=FPanel;
  cal.Align:=alClient;
  {$IFDEF FPC}
  cal.OnClick:=@_OnClick;
  cal.OnDblClick:=@_OnDblClick;
  cal.OnPaint:=@_OnPaint;
  cal.OnMouseDown:=@_OnMouseDown;
  {$ELSE}
  cal.OnClick:=_OnClick;
  cal.OnDblClick:=_OnDblClick;
  cal.OnPaint:=_OnPaint;
  cal.OnMouseDown:=_OnMouseDown;
  {$ENDIF}
end;

procedure TYearCalendar.DestroyObjects;
begin
  cal.Free;
  cal:=nil;
end;

constructor TYearCalendar.Create(AOwner: TComponent);
var
  r,m,d: Word;
begin
  inherited Create(AOwner);
  cal:=nil;
  DecodeDate(date,r,m,d);
  FRok:=r;
  FBKColor:=clWhite;
  FBKColorHeaders:=clCream;
  FFontColorDaysOff:=clRed;
  FNotDrawLines:=false;
  v_zaznaczenie_x:=-1;
  v_zaznaczenie_y:=-1;
end;

destructor TYearCalendar.Destroy;
begin
  inherited Destroy;
  if cal<>nil then DestroyObjects;
end;

procedure TYearCalendar.Init;
begin
  CreateObjects;
  InicjujWartosci;
  RysujSiatke;
end;

{$IFDEF MSWINDOWS}
procedure TYearCalendar.DeInit;
begin
  DestroyObjects;
end;
{$ENDIF}

procedure TYearCalendar.SetColor(aX, aY: integer; aColor: TColor);
begin
  RysujTlo(aX,aY,aColor);
  rec[aX,aY].color_background:=aColor;
  WypelnijDzien(aX,aY);
  if (aX=v_zaznaczenie_x) and (aY=v_zaznaczenie_y) then zaznacz(aX,aY);
end;

procedure TYearCalendar.SetColor(aDate: TDate; aColor: TColor);
var
  r,m,d,dzien: Word;
  x,y: integer;
  dt: TDate;
begin
  DecodeDate(aDate,r,m,d);
  if r<>FRok then exit;
  dt:=EncodeDate(r,m,1);
  dzien:=DayOfTheWeek(aDate);
  SetColor(d+dzien-1,m,aColor);
end;

end.
