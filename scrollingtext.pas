{ Scrolling Text component

  Copyright (C)2014 Gordon Bamber minesadorada@charcodelvalle.com

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

unit ScrollingText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, LCLIntf,LCLTranslator,AboutScrolltextunit;

const
  C_TEXTFILENAME = 'scrolling.txt';
  C_TEXTRESOURCENAME = 'scrolltext'; //Note: LResources unit needed
  C_VERSION = '1.0.2.0';

type
  TTextSource = (stStringlist, stTextfile, stResource);

  { TScrollingText }

  TScrollingText = class(TAboutScrollText)
  private
    FActive: boolean;
    FActiveLine: integer;   //the line over which the mouse hovers
    FAddingNullLines: integer;
    FBuffer: TBitmap;
    FEndLine: integer;
    FFormated: boolean;
    FInterval: integer;
    FLineHeight: integer;
    FLines: TStrings;
    FLoop: boolean;
    FNumLines: integer;
    FOffset: integer;
    FOnAfterActivate: TNotifyEvent;
    FOnAfterDeactivate: TNotifyEvent;
    FOnBeforeActivate: TNotifyEvent;
    FOnBeforeDeactivate: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FSleep,FPause: boolean;
    FStartLine: integer;
    FStartTextShow: boolean;
    FStepSize: integer;
    FTimer: TTimer;
    FFont: TFont;
    FLinkFont:TFont;
    FBackColor: TColor;
    fTextFileName: string;
    fResourceName: string;
    fVersionString: string;
    fTextSource: TTextSource;
    FWordWrap: boolean;
    function ActiveLineIsURL: boolean;
    procedure DoTimer(Sender: TObject);
    procedure SetActive(const AValue: boolean);
    procedure Init;
    procedure DrawScrollingText(Sender: TObject);
    procedure SetInterval(AValue: integer);
    procedure SetLines(AValue: TStrings);
    procedure SetFont(AValue: TFont);
    procedure SetLinkFont(AValue: TFont);
    procedure wPaint(aOnlyPaint: boolean = false);
  protected
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
  published
    property OnClick;
    // Can be set in design mode. Note URL links are inactive in design mode
    property Active: boolean read FActive write SetActive;
    // Inherited property
    property Align;
    // Inherited property
    property Anchors;
    // Inherited property
    property BiDiMode;
    // Inherited property
    property Constraints;
    // Inherited property
    property Enabled;
    // Inherited property
    property Borderspacing;
    // Can be set in design or runtime mode. (TextSource=stStringlist)
    property Lines: TStrings read FLines write SetLines;
    // Sets the background color of the window
    property BackColor: TColor read fBackColor write fBackColor default clWindow;
    // Sets the font properties of the scrolling text
    property Font: TFont read fFont write SetFont;
    // Sets the font properties of links in the scrolling text
    property LinkFont: TFont read fLinkFont write SetLinkFont;
    // Source of the text to display.
    // If TextSource=stTextfile 'scrolling.txt' should be in the deployed app folder
    // if TextSource=stResource be sure to add LResources to your uses clause
    property TextSource: TTextSource read fTextSource write fTextSource default
      stStringlist;
    // Read-only property to remind you of the correct file name
    property TextFileName: string read fTextFileName;
    // Read-only property to remind you of the correct resource name
    property TextResourceName: string read fResourceName;
    // Version number of this component
    property Version: string read fVersionString;
    // Moje właściwości
    {Alternatywny tryb działania}
    property Formatted: boolean read FFormated write FFormated default false;
    {Działa tylko gdy Formated = Atcive
     Poprawka na dodatkowe puste linie}
    property AddingNullLines: integer read FAddingNullLines write FAddingNullLines default 0;
    {Działa tylko gdy Formated = Atcive}
    property WordWrap: boolean read FWordWrap write FWordWrap default false;
    {Prędkość przesuwania tekstu:
     czym mniejsza liczba tym szybciej.}
    property Interval: integer read FInterval write SetInterval default 30;
    {Początek animacji:
       False: Tekst schowany
       True:  Tekst pokazany}
    property StartTextShow: boolean read FStartTextShow write FStartTextShow default false;
    {Startuj w uśpieniu}
    property Sleep: boolean read FSleep write FSleep default false;
    {Wykonuj animację w pętli}
    property Loop: boolean read FLoop write FLoop default true;
    property OnBeforeActivate: TNotifyEvent read FOnBeforeActivate write FOnBeforeActivate;
    property OnAfterActivate: TNotifyEvent read FOnAfterActivate write FOnAfterActivate;
    property OnBeforeDeactivate: TNotifyEvent read FOnBeforeDeactivate write FOnBeforeDeactivate;
    property OnAfterDeactivate: TNotifyEvent read FOnAfterDeactivate write FOnAfterDeactivate;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
  end;


procedure Register;

implementation

procedure Register;
begin
  {$I scrollingtext_icon.lrs}
  RegisterComponents('Additional', [TScrollingText]);
end;

procedure TScrollingText.SetFont(AValue: TFont);
begin
  fFont.Assign(AValue);
end;

procedure TScrollingText.SetLinkFont(AValue: TFont);
begin
  fLinkFont.Assign(AValue);
end;

procedure TScrollingText.wPaint(aOnlyPaint: boolean);
var
  w: integer;
  s: string;
  i: integer;
  r: TRect;
  style: TTextStyle;
begin
  if not aOnlyPaint then Dec(FOffset, FStepSize);

  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));

  //reset buffer font
  if FFormated then
  begin
    FBuffer.Canvas.Font := fFont;
    FBuffer.Canvas.Font.Style := fFont.Style;
    FBuffer.Canvas.Font.Color := fFont.Color;
    r.Top:=FOffset;
    r.Left:=0;
    r.Height:=Height-FOffset;
    r.Width:=Width;
    style.SingleLine:=false;
    style.Wordbreak:=FWordWrap;
    FBuffer.Canvas.TextRect(r,0,FOffset,FLines.Text,style);
  end else begin
    if FOffSet < 0 then FStartLine := -FOffset div FLineHeight else FStartLine := 0;
    FEndLine := FStartLine + FNumLines + 1;
    if FEndLine > FLines.Count - 1 then FEndLine := FLines.Count - 1;
    for i := FEndLine downto FStartLine do
    begin
      s := Trim(FLines[i]);

      //reset buffer font
      FBuffer.Canvas.Font := fFont;
      FBuffer.Canvas.Font.Style := fFont.Style;
      FBuffer.Canvas.Font.Color := fFont.Color;

      //skip empty lines
      if Length(s) > 0 then
      begin
        //check for url
        if (Pos('http://', s) <> 0)
        OR (Pos('https://', s) <> 0)
        OR  (Pos('mailto:', s) <> 0) then
        begin
          FBuffer.Canvas.Font := FLinkFont;
          if i = FActiveLine then FBuffer.Canvas.Font.Style := FBuffer.Canvas.Font.Style+[fsUnderline];
        end;
        //check for bold format token
        if s[1] = '#' then
        begin
          s := copy(s, 2, Length(s) - 1);
          FBuffer.Canvas.Font.Style := [fsBold];
        end;
        w := FBuffer.Canvas.TextWidth(s);
        FBuffer.Canvas.TextOut((FBuffer.Width - w) div 2, FOffset + i * FLineHeight, s);
      end;
    end;
  end;

  //start showing the list from the start
  if FFormated then
  begin
    if FOffset < 0-FBuffer.Height-(FAddingNullLines*FLineHeight) then
      if FLoop then FOffset := FBuffer.Height else begin
        FTimer.Enabled:=false;
        if assigned(FOnPause) then FOnPause(self);
      end;
  end else begin
    if FStartLine > FLines.Count - 1 then
      if FLoop then FOffset := FBuffer.Height else begin
        FTimer.Enabled:=false;
        if assigned(FOnPause) then FOnPause(self);
      end;
  end;
  Invalidate;
end;

procedure TScrollingText.SetLines(AValue: TStrings);
begin
  fLines.Assign(AValue);
end;

procedure TScrollingText.SetActive(const AValue: boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  if FActive then
  begin
    if assigned(FOnBeforeActivate) then FOnBeforeActivate(self);
  end else begin
    if assigned(FOnBeforeDeactivate) then FOnBeforeDeactivate(self);
  end;
  FTimer.Enabled:=FActive;
  if FActive then
  begin
    FPause:=false;
    Init;
    if FStartTextShow then FOffset:=0 else FOffset:=FBuffer.Height; // Start at the bottom of the window
  end;
  if FActive then
  begin
    if assigned(FOnAfterActivate) then FOnAfterActivate(self);
  end else begin
    if assigned(FOnAfterDeactivate) then FOnAfterDeactivate(self);
  end;
end;

procedure TScrollingText.Init;
var
  r: TLResource;
begin
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  FLineHeight := FBuffer.Canvas.TextHeight('X');
  FNumLines := FBuffer.Height div FLineHeight;

  if FOffset = -1 then FOffset := FBuffer.Height;

  with FBuffer.Canvas do
  begin
    Brush.Color := fBackColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;
  if (fTextSource = stTextfile) then
    if FileExists('scrolling.txt') then
    begin
      fLines.Clear;
      fLines.LoadFromFile('scrolling.txt');
    end
    else
    begin
      fLines.Clear;
      fLines.Add('The file ''' + C_TEXTFILENAME + ''' is missing.');
      fLines.Add('It should be in the same folder as your application');
    end;
  if (fTextSource = stResource) then

  // Load text from resource string
  begin
    r := LazarusResources.Find(fResourceName);
    if r = nil then
      raise Exception.CreateFmt('Resource ''%s'' is missing',[fResourceName])
    else
    begin
      fLines.Clear;
      fLines.Add(r.Value);
    end;
  end;
  // Are there any lines in the Stringlist?
  if (fLines.Count = 0) then
  begin
    fLines.Add('This is the ScrollingText scrolling window.');
    fLines.Add(' ');
    fLines.Add('This default text is showing because you either:');
    fLines.Add(' ');
    fLines.Add('1) Haven''t set any text in the Lines property. or');
    fLines.Add('2) TextSource is set to stTextfile and the file');
    fLines.Add('''' + C_TEXTFILENAME + ''' is empty.');
    fLines.Add(' ');
    fLines.Add('Note that URL links such as');
    fLines.Add('http://wiki.lazarus.freepascal.org/Main_Page');
    fLines.Add('mailto:bill_gates@microsoft.com');
    fLines.Add('are clickable by the user');
    fLines.Add('(each link should be a single line of text)');
    fLines.Add(' ');
    fLines.Add('TScrollingText is released under the GPL license (See About)');
    fLines.Add('Code is modified from the Lazarus ''AboutFrm'' code');
    fLines.Add(' ');
    fLines.Add('The standalone visual component TScrollingText is available at:');
    fLines.Add('https://sourceforge.net/p/lazarus-ccr/svn/HEAD/tree/components/');
    fLines.Add(' ');
    fLines.Add('Sep 2015');
  end;
end;

procedure TScrollingText.DrawScrollingText(Sender: TObject);
begin
  if Active then
    Canvas.Draw(0,0, FBuffer);
end;

procedure TScrollingText.SetInterval(AValue: integer);
begin
  if FInterval=AValue then Exit;
  FInterval:=AValue;
  FTimer.Interval:=FInterval;
end;

procedure TScrollingText.DoTimer(Sender: TObject);
begin
  if not FActive then Exit;
  wPaint;
  if FSleep and (not FPause) then
  begin
    FTimer.Enabled:=false;
    FPause:=true;
  end;
end;

function TScrollingText.ActiveLineIsURL: boolean;
begin
  if (FActiveLine > 0) and (FActiveLine < FLines.Count) then
    Result := (Pos('http://', FLines[FActiveLine]) <> 0)
    OR (Pos('https:', FLines[FActiveLine]) <> 0)
    OR (Pos('mailto:', FLines[FActiveLine]) <> 0)
  else
    Result := False;
end;

procedure TScrollingText.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  Init;
end;

procedure TScrollingText.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  s:string;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ActiveLineIsURL and
  ((Pos('http://', FLines[FActiveLine]) OR (Pos('https://', FLines[FActiveLine]))<> 0)) then
  begin
    s:=FLines[FActiveLine];
    if (Pos(' ',s))=0 then s:=Copy(s,Pos('http://',s),MaxInt)
    else begin
      if Pos(' ',s)<Pos('http://',s) then s:=Copy(s,Pos('http://',s),MaxInt);
      if (Pos(' ',s))=0 then s:=Copy(s,Pos('http://',s),MaxInt)
      else s:=Copy(s,Pos('http://',s),(Pos(' ',s)-Pos('http://',s)));
    end;
    if (Pos(' ',s))=0 then s:=Copy(s,Pos('https://',s),MaxInt)
    else begin
      if Pos(' ',s)<Pos('https://',s) then s:=Copy(s,Pos('https://',s),MaxInt);
      if (Pos(' ',s))=0 then s:=Copy(s,Pos('https://',s),MaxInt)
      else s:=Copy(s,Pos('https://',s),(Pos(' ',s)-Pos('https://',s)));
    end;
    OpenURL(s);
  end
  else if ActiveLineIsURL and (Pos('mailto:', FLines[FActiveLine]) <> 0) then begin
    s:=FLines[FActiveLine];
    if (Pos(' ',s))=0 then s:=Copy(s,Pos('mailto:',s),MaxInt)
    else begin
      if Pos(' ',s)<Pos('mailto:',s) then s:=Copy(s,Pos('mailto:',s),MaxInt);
      if (Pos(' ',s))=0 then s:=Copy(s,Pos('mailto:',s),MaxInt)
      else s:=Copy(s,Pos('mailto:',s),(Pos(' ',s)-Pos('mailto:',s)));
    end;
    OpenURL(s);
  end;
end;

procedure TScrollingText.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);

  //calculate what line is clicked from the mouse position
  FActiveLine := (Y - FOffset) div FLineHeight;

  Cursor := crDefault;

  if (FActiveLine >= 0) and (FActiveLine < FLines.Count) and ActiveLineIsURL then
    Cursor := crHandPoint;
end;

constructor TScrollingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  OnPaint := @DrawScrollingText;
  FSleep:=false;
  FWordWrap:=false;
  FFormated:=false;
  FLoop:=true;
  FAddingNullLines:=0;
  FInterval:=30;
  FStartTextShow:=false;
  FLines := TStringList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled:=false;
  FTimer.OnTimer := @DoTimer;
  FTimer.Interval := FInterval;
  FBuffer := TBitmap.Create;
  FFont := TFont.Create;
  FLinkFont := TFont.Create;
  FFont.Size := 10;
  FLinkFont.Size := 10;
  fBackColor := clWindow;

  FStepSize := 1;
  FStartLine := 0;
  FOffset := -1;
  Width := 100;
  Height := 100;
  fTextFileName := C_TEXTFILENAME;
  fResourceName := C_TEXTRESOURCENAME;
  fVersionString := C_VERSION;
  fTextSource := stStringlist;
  SendToBack;

    // About dialog
  AboutBoxComponentName:='ScrollingText component';
  AboutBoxWidth:=400;
//  AboutBoxHeight (integer)
  AboutBoxDescription:='Component that shows a scrolling window.' + LineEnding +
  'Use Lines property to set text and Active=True' + LineEnding +
  'to use the component';
  AboutBoxBackgroundColor:=clWindow;
  AboutBoxFontName:='Arial';
  AboutBoxFontSize:=10;
  AboutBoxVersion:=C_VERSION;
  AboutBoxAuthorname:='Gordon Bamber and Andrey Gunenko';
  AboutBoxOrganisation:='Public Domain';
  AboutBoxAuthorEmail:='minesadorada@charcodelvalle.com';
  AboutBoxLicenseType:='MODIFIEDGPL';

end;

destructor TScrollingText.Destroy;
begin
  FLines.Free;
  FTimer.Free;
  FBuffer.Free;
  FFont.Free;
  FLinkFont.Free;
  inherited Destroy;
end;

procedure TScrollingText.Start;
begin
  FTimer.Enabled:=true;
end;

end.
