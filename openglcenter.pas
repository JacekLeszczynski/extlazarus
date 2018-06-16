unit OpenGLCenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  PointerTab, OpenGLContext, BGRABitmap, BGRABitmapTypes, BGRAOpenGL;

type

  { TOpenGLCenter }
  TNotifyPaintAfterTexture = procedure(Sender: TObject; nazwa: string; x,y,wielkosc: single) of object;

  TOpenGLCenter = class(TComponent)
  private
    FMouseMove,FMouseScale,FMouseScaleReverse: boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnPaint: TNotifyEvent;
    FOnPaintAfterTexture: TNotifyPaintAfterTexture;
    FVX,FVY,FScale: single;
    tab: TPointerTab;
    OpenGLControl: TOpenGLControl;
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
    procedure LoadTextureFromFile(nazwa: string; filename: string; drugi_plan: boolean = false);
    procedure LoadTextureFromResource(nazwa: string; ResourceName: string; wielkosc: single = 0; drugi_plan: boolean = false);
    procedure LoadTextureFromResource(nazwa: string; wielkosc: single = 0; drugi_plan: boolean = false);
    procedure LoadTextureFromResource(nazwa: string; ResourceName: string; drugi_plan: boolean = false);
    procedure LoadTextureFromResource(nazwa: string; drugi_plan: boolean = false);
    procedure CreateFont(nazwa: string; rodzaj: string; wielkosc: integer; kolor: TColor; drugi_plan: boolean = false);
    procedure SetTextureData(nazwa: string; x,y: single);
    procedure SetTextureData(nazwa: string; x,y,wielkosc: single);
    procedure SetFontData(nazwa: string; x,y: single);
    procedure SetFontData(nazwa,text: string; x,y: single);
    procedure SetFontData(nazwa: string; x,y: single; text: string);
    procedure SetFontData(nazwa,text: string);
    procedure Init(ParentControl: TWinControl);
    procedure Paint;
    procedure Invalidate;
  published
    property MouseMove: boolean read FMouseMove write FMouseMove default false;
    property MouseScale: boolean read FMouseScale write FMouseScale default false;
    property MouseScaleReverse: boolean read FMouseScaleReverse write FMouseScaleReverse default false;
    property Scale: single read FScale write FScale default 1;
    property vector_x: single read FVX write FVX default 0;
    property vector_y: single read FVY write FVY default 0;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint; //Server code
    property OnPaintAfterTexture: TNotifyPaintAfterTexture read FOnPaintAfterTexture write FOnPaintAfterTexture;
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

procedure TOpenGLCenter.LoadTextureFromFile(nazwa: string; filename: string;
  drugi_plan: boolean);
var
  bmp: TBGLBitmap;
begin
  element.nazwa:=nazwa;
  element.rodzaj:=reTextura;
  new(element.tex);
  bmp:=TBGLBitmap.Create(ExtractFilePath(Application.ExeName)+filename);
  bmp.ResampleFilter:=rfBestQuality;
  element.tex^:=bmp.MakeTextureAndFree;
  element.drugi_plan:=drugi_plan;
  tab.Add;
end;

procedure TOpenGLCenter.LoadTextureFromResource(nazwa: string;
  ResourceName: string; wielkosc: single; drugi_plan: boolean);
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
  try
    res:=TResourceStream.Create(hInstance,s,RT_RCDATA);
    bmp:=TBGLBitmap.Create(res);
    bmp.ResampleFilter:=rfBestQuality;
    element.tex^:=bmp.MakeTextureAndFree;
  finally
    res.Free;
  end;
  tab.Add;
end;

procedure TOpenGLCenter.LoadTextureFromResource(nazwa: string;
  wielkosc: single; drugi_plan: boolean);
begin
  LoadTextureFromResource(nazwa,nazwa,wielkosc,drugi_plan);
end;

procedure TOpenGLCenter.LoadTextureFromResource(nazwa: string;
  ResourceName: string; drugi_plan: boolean);
begin
  LoadTextureFromResource(nazwa,ResourceName,0,drugi_plan);
end;

procedure TOpenGLCenter.LoadTextureFromResource(nazwa: string;
  drugi_plan: boolean);
begin
  LoadTextureFromResource(nazwa,nazwa,0,drugi_plan);
end;

procedure TOpenGLCenter.CreateFont(nazwa: string; rodzaj: string;
  wielkosc: integer; kolor: TColor;  drugi_plan: boolean);
begin
  element.nazwa:=nazwa;
  element.rodzaj:=reFont;
  element.wielkosc:=wielkosc;
  element.drugi_plan:=drugi_plan;
  new(element.font);
  element.font^:=BGLFont(rodzaj,wielkosc,kolor);
  tab.Add;
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
  tex: IBGLTexture;
  x,y,w: single;
  mx,my: single;
begin
  if Assigned(FOnPaint) then FOnPaint(Sender);
  mx:=OpenGLControl.Width/2;
  my:=OpenGLControl.Height/2;
  BGLViewPort(OpenGLControl.Width,OpenGLControl.Height,BGRABlack);
  for i:=0 to tab.Count-1 do
  begin
    tab.Read(i);
    if element.rodzaj=reTextura then tex:=element.tex^;
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
begin
  if Assigned(FOnMouseWheel) then FOnMouseWheel(self,Shift,WheelDelta,MousePos,Handled);
  if not FMouseScale then exit;
  if FMouseScaleReverse then FScale-=WheelDelta/1000 else FScale+=WheelDelta/1000;
  if FScale<0 then FScale:=0;
  OpenGLControl.Invalidate;
end;

procedure TOpenGLCenter.Paint;
begin
  OpenGLControl.Paint;
end;

procedure TOpenGLCenter.Invalidate;
begin
  OpenGLControl.Invalidate;
end;

end.
