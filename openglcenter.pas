(*
  Autor: Jacek Leszczyński
  Licencja: GPL (wersja 3)

  Opis:
  Komponent do prostego rysowania na ekranie OpenGL.
  Musowo ekran trzeba zainicjować, metoda Init(Obiekt_który_ma_być_ekranem).
  Następnie definiujemy tekstury oraz fonty, wystarczy to zrobić raz.
  Do każdego takiego obiektu możemy zczytać adres typu POINTER, który możemy potem wykorzystywać.
  Następnie wykonujemy rysowanie, umieszczanie objektów.
  Ogólnie obiekty rysowane są po jeden raz na każdy zdefiniowany objekt,
  aby to ominąć musimy skorzystać z dodatkowych metod i naszego adresu POINTER,
  pozwoli to na tworzenie wielu kopii objektu na ekranie.

  Póki co, nie miałem większych potrzeb, ale można by się nad tym komponentem pobawić.
  Może w przyszłości coś takiego się stanie.
*)

unit OpenGLCenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics,
  PointerTab, OpenGLContext, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TOpenGLCenter }
  TNotifyPaintAfterTexture = procedure(Sender: TObject; aNazwa: string; aX,aY,aWielkosc: single) of object;

  TOpenGLCenter = class(TComponent)
  private
    FMouseMove,FMouseScale,FMouseScaleReverse: boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnPaint: TNotifyEvent;
    FOnPaintAfterTexture: TNotifyPaintAfterTexture;
    FProcent: double;
    FScaleFollow: boolean;
    FVX,FVY,FScale: single;
    tab: TPointerTab;
    OpenGLControl: TOpenGLControl;
    tex: IBGLTexture;
    texfont: IBGLFont;
    vcenter: pointer;
    procedure TabCreateElement(Sender: TObject; var AWskaznik: Pointer);
    procedure TabDestroyElement(Sender: TObject; var AWskaznik: Pointer);
    procedure TabReadElement(Sender: TObject; var AWskaznik: Pointer);
    procedure TabWriteElement(Sender: TObject; var AWskaznik: Pointer);
    procedure Paint(Sender: TObject);
    procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OpenGLControlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LoadTextureFromFile(nazwa: string; filename: string; drugi_plan: boolean = false): pointer;
    function LoadTextureFromResource(nazwa: string; ResourceName: string; wielkosc: single = 0; drugi_plan: boolean = false): pointer;
    function LoadTextureFromResource(nazwa: string; wielkosc: single = 0; drugi_plan: boolean = false): pointer;
    function LoadTextureFromResource(nazwa: string; ResourceName: string; drugi_plan: boolean = false): pointer;
    function LoadTextureFromResource(nazwa: string; drugi_plan: boolean = false): pointer;
    function CreateFont(nazwa: string; rodzaj: string; wielkosc: integer; kolor: TColor; drugi_plan: boolean = false): pointer;
    procedure BlockTextureData(nazwa: string);
    procedure BlockFontData(nazwa: string);
    procedure SetTextureData(nazwa: string; x,y: single);
    procedure SetTextureData(nazwa: string; x,y,wielkosc: single);
    procedure SetFontData(nazwa: string; x,y: single);
    procedure SetFontData(nazwa,text: string; x,y: single);
    procedure SetFontData(nazwa: string; x,y: single; text: string);
    procedure SetFontData(nazwa,text: string);
    procedure Init(ParentControl: TWinControl);
    procedure Paint;
    procedure PaintTexture(aX,aY,aWielkosc: single);
    procedure PaintTexture(aObject: pointer; aX,aY,aWielkosc: single);
    procedure PaintText(aX,aY: single; const aText: string);
    procedure PaintText(aObject: pointer; aX,aY: single; const aText: string);
    procedure Invalidate;
  published
    property MouseMove: boolean read FMouseMove write FMouseMove default false; //Przesuwanie ekranu myszą
    property MouseScale: boolean read FMouseScale write FMouseScale default false; //Skalowanie ekranu za pomocą kółka myszy
    property MouseScaleReverse: boolean read FMouseScaleReverse write FMouseScaleReverse default false; //Kierunek skalowania
    property Scale: single read FScale write FScale default 1; //Skala domyślna
    property ScalePercent: double read FProcent write FProcent; //Moc skalowania ekranu
    property ScaleFollow: boolean read FScaleFollow write FScaleFollow default false; //Skalując ekran podążaj za kursorem myszy
    property vector_x: single read FVX write FVX default 0; //Wektor przesunięcia w poziomie
    property vector_y: single read FVY write FVY default 0; //Wektor przesunięcia w pionie
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint; //Kod rysowania po ekranie
    property OnPaintAfterTexture: TNotifyPaintAfterTexture read FOnPaintAfterTexture write FOnPaintAfterTexture; //Kod rysowania - dodatkowy
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    //procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    //procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    //procedure OnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  end;

procedure Register;

implementation

uses
  LCLType;

type
  TRodzajElementu = (reNull,reTextura,reFont);
  TElement = record
    nazwa: string;
    rodzaj: TRodzajElementu;
    tex: ^IBGLTexture;
    font: ^IBGLFont;
    text: string;
    x,y: single;
    wielkosc: single;
    drugi_plan: boolean;
    blocked: boolean;
  end;
  PElement = ^TElement;

var
  element: TElement;
  pp: PElement;
  MovingOrigin: TPointF;
  WheelOrigin: integer;

procedure Register;
begin
  {$I openglcenter_icon.lrs}
  RegisterComponents('OpenGL',[TOpenGLCenter]);
end;

{ TOpenGLCenter }

procedure TOpenGLCenter.TabCreateElement(Sender: TObject; var AWskaznik: Pointer
  );
begin
  new(pp);
  pp^.rodzaj:=reNull;
  pp^.drugi_plan:=false;
  AWskaznik:=pp;
end;

procedure TOpenGLCenter.TabDestroyElement(Sender: TObject;
  var AWskaznik: Pointer);
begin
  pp:=AWskaznik;
  case pp^.rodzaj of
    reTextura: dispose(pp^.tex);
    reFont: dispose(pp^.font);
  end;
  dispose(pp);
end;

procedure TOpenGLCenter.TabReadElement(Sender: TObject; var AWskaznik: Pointer);
begin
  pp:=AWskaznik;
  element:=pp^;
end;

procedure TOpenGLCenter.TabWriteElement(Sender: TObject; var AWskaznik: Pointer
  );
begin
  pp^:=element;
  AWskaznik:=pp;
end;

constructor TOpenGLCenter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OpenGLControl:=TOpenGLControl.Create(self);
  OpenGLControl.Align:=alClient;
  OpenGLControl.AutoResizeViewport:=true;
  OpenGLControl.OnPaint:=@Paint;
  OpenGLControl.OnMouseDown:=@OpenGLControlMouseDown;
  OpenGLControl.OnMouseMove:=@OpenGLControlMouseMove;
  OpenGLControl.OnMouseWheel:=@OpenGLControlMouseWheel;
  tab:=TPointerTab.Create(self);
  tab.OnCreateElement:=@TabCreateElement;
  tab.OnDestroyElement:=@TabDestroyElement;
  tab.OnReadElement:=@TabReadElement;
  tab.OnWriteElement:=@TabWriteElement;
  FScale:=1;
  FProcent:=5;
  FScaleFollow:=false;
  FVX:=0;
  FVY:=0;
  FMouseMove:=false;
  FMouseScale:=false;
  FMouseScaleReverse:=false;
end;

destructor TOpenGLCenter.Destroy;
begin
  OpenGLControl.Free;
  tab.Free;
  inherited Destroy;
end;

function TOpenGLCenter.LoadTextureFromFile(nazwa: string; filename: string;
  drugi_plan: boolean): pointer;
var
  bmp: TBGLBitmap;
begin
  element.nazwa:=nazwa;
  element.rodzaj:=reTextura;
  new(element.tex);
  result:=element.tex;
  bmp:=TBGLBitmap.Create(ExtractFilePath(Application.ExeName)+filename);
  bmp.ResampleFilter:=rfBestQuality;
  element.tex^:=bmp.MakeTextureAndFree;
  element.drugi_plan:=drugi_plan;
  element.blocked:=true;
  tab.Add;
end;

function TOpenGLCenter.LoadTextureFromResource(nazwa: string;
  ResourceName: string; wielkosc: single; drugi_plan: boolean): pointer;
var
  s: string;
  bmp: TBGLBitmap;
  res: TResourceStream;
begin
  if ResourceName='' then s:=nazwa else s:=ResourceName;
  element.nazwa:=nazwa;
  element.rodzaj:=reTextura;
  element.wielkosc:=wielkosc;
  element.drugi_plan:=drugi_plan;
  new(element.tex);
  result:=element.tex;
  try
    res:=TResourceStream.Create(hInstance,s,RT_RCDATA);
    bmp:=TBGLBitmap.Create(res);
    bmp.ResampleFilter:=rfBestQuality;
    element.tex^:=bmp.MakeTextureAndFree;
  finally
    res.Free;
  end;
  element.blocked:=true;
  tab.Add;
end;

function TOpenGLCenter.LoadTextureFromResource(nazwa: string; wielkosc: single;
  drugi_plan: boolean): pointer;
begin
  result:=LoadTextureFromResource(nazwa,nazwa,wielkosc,drugi_plan);
end;

function TOpenGLCenter.LoadTextureFromResource(nazwa: string;
  ResourceName: string; drugi_plan: boolean): pointer;
begin
  result:=LoadTextureFromResource(nazwa,ResourceName,0,drugi_plan);
end;

function TOpenGLCenter.LoadTextureFromResource(nazwa: string;
  drugi_plan: boolean): pointer;
begin
  result:=LoadTextureFromResource(nazwa,nazwa,0,drugi_plan);
end;

function TOpenGLCenter.CreateFont(nazwa: string; rodzaj: string;
  wielkosc: integer; kolor: TColor; drugi_plan: boolean): pointer;
begin
  element.nazwa:=nazwa;
  element.rodzaj:=reFont;
  element.wielkosc:=wielkosc;
  element.drugi_plan:=drugi_plan;
  new(element.font);
  result:=element.font;
  element.font^:=BGLFont(rodzaj,wielkosc,kolor);
  element.blocked:=true;
  tab.Add;
end;

procedure TOpenGLCenter.BlockTextureData(nazwa: string);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reTextura) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.blocked:=true;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.BlockFontData(nazwa: string);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reFont) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.blocked:=true;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.SetTextureData(nazwa: string; x, y: single);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reTextura) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.x:=x;
    element.y:=y;
    element.blocked:=false;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.SetTextureData(nazwa: string; x, y, wielkosc: single);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reTextura) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.x:=x;
    element.y:=y;
    element.wielkosc:=wielkosc;
    element.blocked:=false;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.SetFontData(nazwa: string; x, y: single);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reFont) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.x:=x;
    element.y:=y;
    element.blocked:=false;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.SetFontData(nazwa, text: string; x, y: single);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reFont) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.text:=text;
    element.x:=x;
    element.y:=y;
    element.blocked:=false;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.SetFontData(nazwa: string; x, y: single; text: string);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reFont) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.text:=text;
    element.x:=x;
    element.y:=y;
    element.blocked:=false;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.SetFontData(nazwa, text: string);
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to tab.Count do
  begin
    tab.Read(i);
    if (element.rodzaj=reFont) and (element.nazwa=nazwa) then
    begin
      b:=true;
      break;
    end;
  end;
  if b then
  begin
    element.text:=text;
    element.blocked:=false;
    tab.Edit(i);
  end;
end;

procedure TOpenGLCenter.Init(ParentControl: TWinControl);
begin
  OpenGLControl.Parent:=ParentControl;
end;

procedure TOpenGLCenter.Paint(Sender: TObject);
var
  i: integer;
  x,y,w: single;
  mx,my: single;
begin
  BGLViewPort(OpenGLControl.Width,OpenGLControl.Height,BGRABlack);
  if Assigned(FOnPaint) then FOnPaint(Sender);
  mx:=OpenGLControl.Width/2;
  my:=OpenGLControl.Height/2;
  for i:=0 to tab.Count-1 do
  begin
    tab.Read(i);
    if element.blocked then continue;
    if element.rodzaj=reTextura then tex:=element.tex^ else
    if element.rodzaj=reFont then texfont:=element.font^;
    if element.drugi_plan then
    begin
      x:=element.x;
      y:=element.y;
      w:=element.wielkosc;
    end else begin
      x:=(element.x-FVX)*FScale;
      y:=(element.y-FVY)*FScale;
      w:=element.wielkosc*FScale;
      x:=x+mx;
      y:=y+my;
    end;
    case element.rodzaj of
      reTextura: begin
                   if (x<OpenGLControl.Width+100) and (y<OpenGLControl.Height+100) and (x>-100) and (y>-100) then
                     tex.StretchDrawAngle(x,y,w,w/tex.Width*tex.Height,0,PointF(tex.Width/2,tex.Height/2),False);
                   if Assigned(FOnPaintAfterTexture) then FOnPaintAfterTexture(self,element.nazwa,element.x,element.y,element.wielkosc);
                 end;
      reFont: element.font^.TextOut(x,y,Utf8ToAnsi(element.text));
    end;
  end;
  OpenGLControl.SwapBuffers;
end;

procedure TOpenGLCenter.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(self,Button,Shift,X,Y);
  if Button<>mbLeft then exit;
  MovingOrigin:=PointF(X,Y);
end;

procedure TOpenGLCenter.OpenGLControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  mousePos: TPointF;
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(self,Shift,X,Y);
  if not FMouseMove then exit;
  if ssLeft in Shift then
  begin
    mousePos:=PointF(X,Y);
    FVX-=(mousePos.x-MovingOrigin.x)/FScale;
    FVY-=(mousePos.y-MovingOrigin.y)/FScale;
    OpenGLControl.Invalidate;
    MovingOrigin:=mousePos;
  end;
end;

procedure TOpenGLCenter.OpenGLControlMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  a: integer;
  x2,y2,xx,yy,b,z: single;
begin
  if Assigned(FOnMouseWheel) then FOnMouseWheel(self,Shift,WheelDelta,MousePos,Handled);
  if not FMouseScale then exit;
  b:=FScale;
  if WheelDelta>0 then a:=1 else a:=-1;
  if FMouseScaleReverse then FScale-=FScale*FProcent/100*a else FScale+=FScale*FProcent/100*a;
  if FScale<0 then FScale:=0;
  if FScaleFollow then
  begin
    x2:=OpenGLControl.Width/2;
    y2:=OpenGLControl.Height/2;
    xx:=MousePos.X-x2;
    yy:=MousePos.Y-y2;
    z:=(FScale-b)/2;
    FVX+=xx/z/OpenGLControl.Width;
    FVY+=yy/z/OpenGLControl.Height;
  end;
  OpenGLControl.Invalidate;
end;

procedure TOpenGLCenter.Paint;
begin
  OpenGLControl.Paint;
end;

procedure TOpenGLCenter.PaintTexture(aX, aY, aWielkosc: single);
var
  x,y,w: single;
  mx,my: single;
begin
  mx:=OpenGLControl.Width/2;
  my:=OpenGLControl.Height/2;

  x:=(aX-FVX)*FScale;
  y:=(aY-FVY)*FScale;
  w:=aWielkosc*FScale;
  x:=x+mx;
  y:=y+my;

  if (x<OpenGLControl.Width+100) and (y<OpenGLControl.Height+100) and (x>-100) and (y>-100) then
    tex.StretchDrawAngle(x,y,w,w/tex.Width*tex.Height,0,PointF(tex.Width/2,tex.Height/2),False);
end;

procedure TOpenGLCenter.PaintTexture(aObject: pointer; aX, aY, aWielkosc: single
  );
var
  o: ^IBGLTexture;
  x,y,w: single;
  mx,my: single;
begin
  o:=aObject;
  mx:=OpenGLControl.Width/2;
  my:=OpenGLControl.Height/2;

  x:=(aX-FVX)*FScale;
  y:=(aY-FVY)*FScale;
  w:=aWielkosc*FScale;
  x:=x+mx;
  y:=y+my;

  if (x<OpenGLControl.Width+100) and (y<OpenGLControl.Height+100) and (x>-100) and (y>-100) then
    o^.StretchDrawAngle(x,y,w,w/o^.Width*o^.Height,0,PointF(o^.Width/2,o^.Height/2),False);
end;

procedure TOpenGLCenter.PaintText(aX, aY: single; const aText: string);
var
  x,y: single;
  mx,my: single;
begin
  mx:=OpenGLControl.Width/2;
  my:=OpenGLControl.Height/2;

  x:=(aX-FVX)*FScale;
  y:=(aY-FVY)*FScale;
  x:=x+mx;
  y:=y+my;

  if (x<OpenGLControl.Width+100) and (y<OpenGLControl.Height+100) and (x>-100) and (y>-100) then
    texfont.TextOut(x,y,Utf8ToAnsi(aText));
end;

procedure TOpenGLCenter.PaintText(aObject: pointer; aX, aY: single;
  const aText: string);
var
  x,y: single;
  mx,my: single;
  f: ^IBGLFont;
begin
  f:=aObject;
  mx:=OpenGLControl.Width/2;
  my:=OpenGLControl.Height/2;

  x:=(aX-FVX)*FScale;
  y:=(aY-FVY)*FScale;
  x:=x+mx;
  y:=y+my;

  if (x<OpenGLControl.Width+100) and (y<OpenGLControl.Height+100) and (x>-100) and (y>-100) then
    f^.TextOut(x,y,Utf8ToAnsi(aText));
end;

procedure TOpenGLCenter.Invalidate;
begin
  OpenGLControl.Invalidate;
end;

end.
