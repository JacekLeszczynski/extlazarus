{

  Copyright (C) 2007 Laurent Jacques

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit ColorProgress;

{$MODE objfpc}{$H+}

interface

uses
  LResources, Classes, SysUtils, Controls, Graphics, FPCanvas;

type

  TColorProgressKind = (ckText, ckHorizontalBar, ckHorRoundBar, ckVerticalBar,
    ckVerRoundBar, ckPie, ckBitmap);

  { TColorProgress }

  TColorProgress = class(TGraphicControl)
  private
    { Private declarations }
    FBackColor: TColor;
    FBorderStyle: TBorderStyle;
    FCurValue: longint;
    FForeColor: TColor;
    FForeStyle: TFPBrushStyle;
    FForeImage: TBitmap;
    FKind:     TColorProgressKind;
    FMaxValue: longint;
    FMinValue: longint;
    FShowText: boolean;
    procedure DrawRoundBar;
    procedure DrawBar;
    procedure DrawText;
    procedure DrawPie;
    procedure DrawBitmap;
    function GetPercentDone: longint;
    procedure SetBackColor(AValue: TColor);
    procedure SetForeColor(AValue: TColor);
    procedure SetForeStyle(AValue: TFPBrushStyle);
    procedure SetForeImage(AValue: TBitmap);
    procedure SetGaugeKind(const AValue: TColorProgressKind);
    procedure SetMaxValue(AValue: longint);
    procedure SetMinValue(AValue: longint);
    procedure SetProgress(AValue: longint);
    procedure SetShowText(AValue: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddProgress(AValue: longint);
    procedure Paint; override;
    property PercentDone: longint Read GetPercentDone;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BackColor: TColor Read FBackColor Write SetBackColor default clWhite;
    property Color;
    property Constraints;
    property Enabled;
    property ForeColor: TColor Read FForeColor Write SetForeColor default clBlack;
    property ForeStyle: TFPBrushStyle Read FForeStyle Write SetForeStyle default bsSolid;
    property ForeImage: TBitmap Read FForeImage Write SetForeImage;
    property Font;
    property Kind: TColorProgressKind Read FKind Write SetGaugeKind default
      ckHorizontalBar;
    property MinValue: longint Read FMinValue Write SetMinValue default 0;
    property MaxValue: longint Read FMaxValue Write SetMaxValue default 100;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Progress: longint Read FCurValue Write SetProgress;
    property ShowHint;
    property ShowText: boolean Read FShowText Write SetShowText default True;
    property Visible;
  end;

procedure Register;

implementation

uses Math;

procedure Register;
begin
  RegisterComponents('Common Controls', [TColorProgress]);
end;

constructor TColorProgress.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  FMinValue  := 0;
  FMaxValue  := 100;
  FCurValue  := 0;
  FBorderStyle := bsSingle;
  FForeColor := clBlack;
  FBackColor := clWhite;
  FShowText  := True;
  FKind      := ckHorizontalBar;
  Width      := 100;
  Height     := 20;
  FForeImage := TBitmap.Create;
end;

destructor TColorProgress.Destroy;
begin
  FForeImage.Free;
  inherited Destroy;
end;

procedure TColorProgress.Paint;
begin
  case Kind of
    ckText: DrawText;
    ckHorizontalBar,
    ckVerticalBar: DrawBar;
    ckHorRoundBar,
    ckVerRoundBar: DrawroundBar;
    ckPie: DrawPie;
    ckBitmap: DrawBitmap;
  end;
  if ShowText and (Kind <> ckText) then
    DrawText;
  inherited Paint;
end;

procedure TColorProgress.SetForeColor(AValue: TColor);
begin
  if AValue <> FForeColor then
  begin
    FForeColor := AValue;
    Refresh;
  end;
end;

procedure TColorProgress.DrawRoundBar;
var
  FillSize: longint;
  MinSize:  longint;
begin
  with Canvas do
  begin
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    MinSize     := Min(self.Width, self.Height);
    Pen.Color   := clGray;
    RoundRect(0, 0, self.Width - 1, self.Height - 1, MinSize div 4, MinSize div 4);
    Pen.Color := clSilver;
    RoundRect(1, 1, self.Width - 2, self.Height - 2, MinSize div 4, MinSize div 4);
    Brush.Style := ForeStyle;
    Brush.Color := ForeColor;
    if percentdone > 0 then
      case Kind of
        ckHorRoundBar:
        begin
          FillSize := Trunc((self.Width - 2) * (PercentDone / 100));
          RoundRect(Rect(1, 1, FillSize, self.Height - 2), MinSize div
            4, MinSize div 4);
        end;
        ckVerRoundBar:
        begin
          FillSize := Trunc((self.Height - 3) * (PercentDone / 100));
          RoundRect(Rect(1, Self.Height - 2 - FillSize, Self.Width - 2, Self.Height - 2),
            MinSize div 4, MinSize div 4);
        end;
      end;
  end;
end;

procedure TColorProgress.DrawBar;
var
  FillSize: longint;
begin
  with Canvas do
  begin
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    Pen.Color   := clGray;
    Rectangle(0, 0, self.Width, self.Height);
    Pen.Color := clSilver;
    Rectangle(1, 1, self.Width - 1, self.Height - 1);
    Brush.Style := ForeStyle;
    Brush.Color := ForeColor;
    if percentdone > 0 then
      case Kind of
        ckHorizontalBar:
        begin
          FillSize := Trunc((self.Width - 4) * (PercentDone / 100));
          FillRect(Rect(2, 2, FillSize + 2, self.Height - 2));
        end;
        ckVerticalBar:
        begin
          FillSize := Trunc((self.Height - 4) * (PercentDone / 100));
          FillRect(Rect(2, Self.Height - 2 - FillSize, Self.Width - 2, Self.Height - 2));
        end;
      end;
  end;
end;

procedure TColorProgress.DrawText;
var
  X, Y: integer;
  S:    string;
begin
  with Canvas do
  begin
    if Kind = ckText then
    begin
      Brush.Color := BackColor;
      Brush.Style := bsSolid;
      Pen.Color   := clGray;
      Rectangle(0, 0, self.Width, self.Height);
      Pen.Color := clSilver;
      Rectangle(1, 1, self.Width - 1, self.Height - 1);
    end;
    Font := Self.Font;
    S    := format('%d%%', [PercentDone]);
    Y    := self.Height div 2 - TextHeight(S) div 2;
    X    := self.Width div 2 - TextWidth(S) div 2;
    TextRect(self.ClientRect, X, Y, S);
  end;
end;

procedure TColorProgress.DrawPie;
var
  MiddleX, MiddleY: integer;
  Angle: double;
begin
  with Canvas do
  begin
    Brush.Color := BackColor;
    Brush.Style := bsSolid;
    Pen.Color   := clGray;
    Ellipse(0, 0, self.Width - 1, self.Height - 1);
    Pen.Color := clSilver;
    Ellipse(1, 1, self.Width - 2, self.Height - 2);
    Brush.Style := ForeStyle;
    Brush.Color := ForeColor;
    if PercentDone > 0 then
    begin
      MiddleX := (self.Width - 2) div 2;
      MiddleY := (self.Height - 2) div 2;
      Angle   := (Pi * ((PercentDone / 50) + 0.5));
      Pie(1, 1, self.Width - 2, self.Height - 2,
        integer(Round(MiddleX * (1 - Cos(Angle)))),
        integer(Round(MiddleY * (1 - Sin(Angle)))), MiddleX + 1, 1);
    end;
  end;
end;

procedure TColorProgress.DrawBitmap;
var
  FillSize: longint;
  MinSize:  longint;
  SrcRect, DstRect: TRect;
  bmp:      TBitmap;
begin
  with Canvas do
  begin
    bmp := TBitmap.Create;
    Bmp.Width := self.Width;
    Bmp.Height := Self.Height;
    Bmp.Canvas.StretchDraw(rect(0, 0, self.Width, self.Height), FForeImage);
    Brush.Style := bsclear;
    MinSize     := Min(self.Width + 2, self.Height + 2);
    FillSize    := Trunc((self.Width - 4) * (PercentDone / 100));
    DstRect     := Rect(2, 2, FillSize + 2, self.Height + 2);
    CopyMode    := cmSrcCopy;
    CopyRect(DstRect, bmp.Canvas, DstRect);
    Pen.Color := clGray;
    RoundRect(0, 0, self.Width - 1, self.Height - 1, MinSize div 4, MinSize div 4);
    Pen.Color := clSilver;
    RoundRect(1, 1, self.Width - 2, self.Height - 2, MinSize div 4, MinSize div 4);
    bmp.Free;
  end;
end;

function TColorProgress.GetPercentDone: longint;
begin
  Result := trunc(100.0 * (FCurValue / FMaxValue));
end;

procedure TColorProgress.SetBackColor(AValue: TColor);
begin
  if AValue <> FBackColor then
  begin
    FBackColor := AValue;
    Refresh;
  end;
end;

procedure TColorProgress.SetMinValue(AValue: longint);
begin
  if AValue <> FMinValue then
  begin
    FMinValue := AValue;
    Refresh;
  end;
end;

procedure TColorProgress.SetMaxValue(AValue: longint);
begin
  if AValue <> FMaxValue then
  begin
    FMaxValue := AValue;
    Refresh;
  end;
end;

procedure TColorProgress.SetShowText(AValue: boolean);
begin
  if AValue <> FShowText then
  begin
    FShowText := AValue;
    Refresh;
  end;
end;

procedure TColorProgress.SetForeStyle(AValue: TFPBrushStyle);
begin
  if AValue <> FForeStyle then
  begin
    FForeStyle := AValue;
    Refresh;
  end;
end;

procedure TColorProgress.SetForeImage(AValue: TBitmap);
var
  NewBitmap: TBitmap;
begin
  if AValue <> FForeImage then
  begin
    NewBitmap := TBitmap.Create;
    NewBitmap.Assign(AValue);
    FForeImage.Height := self.Height;
    FForeImage.Width  := self.Width;
    FForeImage.Canvas.StretchDraw(Rect(0, 0, Width, Height), NewBitmap);
    NewBitmap.Free;
    Refresh;
  end;
end;

procedure TColorProgress.SetGaugeKind(const AValue: TColorProgressKind);
begin
  if AValue <> FKind then
  begin
    FKind := AValue;
    Refresh;
  end;
end;


procedure TColorProgress.SetProgress(AValue: longint);
begin
  if AValue < FMinValue then
    AValue := FMinValue
  else if AValue > FMaxValue then
    AValue := FMaxValue;
  if FCurValue <> AValue then
  begin
    FCurValue := AValue;
    Refresh;
  end;
end;

procedure TColorProgress.AddProgress(AValue: longint);
begin
  Progress := FCurValue + AValue;
  Refresh;
end;

initialization
{$I colorprogress.lrs}

end.
