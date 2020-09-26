{
  Copyright (C) 2008 Laurent Jacques

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit lazgradient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics;

type
  TOrientation = (foLeftToRight, fdTopToBottom);

  { TGradient }

  TLazGradient = class(TGraphicControl)
  private
    { Private declarations }
    FAlignment: TAlignment;
    FBeginColor: TColor;
    FEndColor: TColor;
    FOrientation: TOrientation;
    FRounded: boolean;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetBeginColor(const AValue: TColor);
    procedure SetEndColor(const AValue: TColor);
    procedure SetOrientation(const AValue: TOrientation);
    procedure SetRounded(const AValue: boolean);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure RealSetText(const Value: TCaption); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property Anchors;
    property BorderSpacing;
    property BeginColor: TColor read FBeginColor write SetBeginColor;
    property Caption;
    property Constraints;
    property EndColor: TColor read FEndColor write SetEndColor;
    property Font;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property OnClick;
    property OnDblClick;
    property OnTripleClick;
    property OnQuadClick;
    property Rounded: boolean read FRounded write SetRounded;
    property Visible;
  end;

procedure Register;

implementation
uses lcltype, LCLIntf;

procedure Register;
begin
  RegisterComponents('Additional',[TLazGradient]);
end;

{ TLazGradient }

procedure TLazGradient.SetBeginColor(const AValue: TColor);
begin
  if AValue = FBeginColor then exit;
  FBeginColor:= AValue;
  Invalidate;
end;

procedure TLazGradient.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  Invalidate;
end;

procedure TLazGradient.SetEndColor(const AValue: TColor);
begin
  if AValue = FEndColor then exit;
  FEndColor:= AValue;
  Invalidate;
end;

procedure TLazGradient.SetOrientation(const AValue: TOrientation);
begin
  if AValue = FOrientation then exit;
  FOrientation:= AValue;
  Invalidate;
end;

procedure TLazGradient.SetRounded(const AValue: boolean);
begin
 if FRounded=AValue then exit;
 FRounded:=AValue;
 Invalidate;
end;

procedure TLazGradient.Paint;
var
  aBand : TRect;
  aBandToo : TRect;
  i     : Integer;
  FBeginRGB   : array[0..2] of Byte;
  FCurrentRGB : array[0..2] of Byte;
  FDeltaRGB   : array[0..2] of Integer;
  ARect: TRect;
  TS : TTextStyle;
  HalfSize: Integer;
begin
  If Visible then
  begin
    FBeginRGB[0] := red( colortorgb( FBeginColor ) );
    FBeginRGB[1] := green( colortorgb( FBeginColor ) );
    FBeginRGB[2] := blue( colortorgb( FBeginColor ) );
    FDeltaRGB[0] := red( colortorgb( FEndColor )) - FBeginRGB[0];
    FDeltaRGB[1] := green( colortorgb( FEndColor )) - FBeginRGB[1];
    FDeltaRGB[2] := blue( colortorgb( FEndColor )) - FBeginRGB[2];

    if (Orientation = fdTopToBottom) then begin
      aBand.Left :=0;
      aBand.Right:=self.Width;
      aBandToo.Left :=0;
      aBandToo.Right:=self.Width;
      HalfSize:= self.Height div 2;
    end
    else begin
      aBand.Top :=0;
      aBand.Bottom:=self.Height;
      aBandToo.Top :=0;
      aBandToo.Bottom:=self.Height;
      HalfSize:= self.Width div 2;
    end;
    canvas.Lock;
    with Canvas Do
    begin
      Pen.Style:=psSolid;
      Pen.Mode:=pmCopy;
      for i:= 0 to 255 do
      begin
        if (Orientation = fdTopToBottom) then begin
         if Rounded then begin
           aBand.Top := MulDiv( i , HalfSize, 256 );
           aBand.Bottom := MulDiv( i+1 , HalfSize, 256 );
           aBandToo.Top:= MulDiv( 255-i , HalfSize, 256 )+HalfSize;
           aBandToo.Bottom:= MulDiv( 255-(i-1) , HalfSize, 256 )+HalfSize;
         end
         else begin
           aBand.Top := MulDiv( i , self.Height, 256 );
           aBand.Bottom := MulDiv( i+1 , self.Height, 256 );
         end;
        end
        else begin
         if Rounded then begin
           aBand.Left := MulDiv( i , HalfSize , 256 );
           aBand.Right := MulDiv( i+1 , HalfSize , 256 );
           aBandToo.Left := MulDiv( 255-i , HalfSize , 256 )+HalfSize;
           aBandToo.Right := MulDiv( 255-(i-1) , HalfSize , 256 )+HalfSize;
         end
         else begin
           aBand.Left := MulDiv( i , self.Width , 256 );
           aBand.Right := MulDiv( i+1 , self.Width , 256 );
         end;
        end;
        FCurrentRGB[0] := (FBeginRGB[0] + MulDiv( i , FDeltaRGB[0] , 255 )) ;
        FCurrentRGB[1] := (FBeginRGB[1] + MulDiv( i , FDeltaRGB[1] , 255 )) ;
        FCurrentRGB[2] := (FBeginRGB[2] + MulDiv( i , FDeltaRGB[2] , 255 )) ;
        Brush.color:=RGBToColor(FCurrentRGB[0],FCurrentRGB[1],FCurrentRGB[2]);
        FillRect(aBand);
        if Rounded then FillRect(aBandToo);
      end;
      canvas.Unlock;
    end;
  end;
  ARect := GetClientRect;
  if Caption <> '' then begin
    TS := Canvas.TextStyle;
    TS.Alignment:= Alignment;
    TS.Layout:= tlCenter;
    TS.Opaque:= false;
    TS.Clipping:= false;
    TS.SystemFont:=Canvas.Font.IsDefault;
    If not Enabled then begin
      Canvas.Font.Color := clBtnHighlight;
      OffsetRect(ARect, 1, 1);
      Canvas.TextRect(ARect, ARect.Left, ARect.Top, Caption, TS);
      Canvas.Font.Color := clBtnShadow;
      OffsetRect(ARect, -1, -1);
    end
    else Canvas.Font.Color := Font.Color;

    Canvas.TextRect(ARect,ARect.Left,ARect.Top, Caption, TS);
  end;
  inherited Paint;
end;

procedure TLazGradient.RealSetText(const Value: TCaption);
begin
  inherited RealSetText(Value);
  Invalidate;
end;

constructor TLazGradient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BeginColor:= clBlue;
  EndColor:= clWhite;
  Orientation:= foLeftToRight;
  Align:= alClient;
  Alignment := taCenter;
  Rounded:= False;
end;
initialization
  {$I lazgradient.lrs}

end.
