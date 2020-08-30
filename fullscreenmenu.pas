unit FullscreenMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TFullscreenMenu }

  TFullscreenMenuOnBefore = procedure(aItemIndex: integer) of object;
  TFullscreenMenuOnExecute = procedure(aItemIndex: integer; aResult: integer) of object;
  TFullscreenMenuOnAfter = procedure(aItemIndex: integer) of object;
  TFullscreenMenu = class(TComponent)
  private
    cctimer,cctimer_opt: integer;
    cPanel: TPanel;
    cLabels: array of TLabel;
    FActive: boolean;
    FBGColor: TColor;
    FColor,FActiveColor: TColor;
    FControl: TWinControl;
    FCursorOff: boolean;
    FFont: TFont;
    FIndex,FCount,FItemIndex: integer;
    FItems: TStrings;
    FOnAfter: TFullscreenMenuOnAfter;
    FOnBefore: TFullscreenMenuOnBefore;
    FOnExecute: TFullscreenMenuOnExecute;
    FUpcase: boolean;
    FTimer: TTimer;
    procedure SetFont(AValue: TFont);
    procedure _OnStartTimer(Sender: TObject);
    procedure _OnStopTimer(Sender: TObject);
    procedure _OnTimer(Sender: TObject);
    procedure SetItems(AValue: TStrings);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(aItemIndex: integer = -1);
    procedure Click;
  published
    property Active: boolean read FActive;
    property DefControl: TWinControl read FControl write FControl;
    property Font: TFont read FFont write SetFont;
    property Items: TStrings read FItems write SetItems;
    property DefaultIndex: integer read FIndex write FIndex;
    property Color: TColor read FColor write FColor;
    property ActiveColor: TColor read FActiveColor write FActiveColor;
    property ActiveColorBG: TColor read FBGColor write FBGColor;
    property UpcaseSet: boolean read FUpcase write FUpcase;
    property CursorOff: boolean read FCursorOff write FCursorOff;
    property OnExecute: TFullscreenMenuOnExecute read FOnExecute write FOnExecute;
    property OnBefore: TFullscreenMenuOnBefore read FOnBefore write FOnBefore;
    property OnAfter: TFullscreenMenuOnAfter read FOnAfter write FOnAfter;
  end;

procedure Register;

implementation

uses
  ecode;

procedure Register;
begin
  {$I fullscreenmenu_icon.lrs}
  RegisterComponents('Additional',[TFullscreenMenu]);
end;

{ TFullscreenMenu }

procedure TFullscreenMenu._OnStartTimer(Sender: TObject);
begin
  cctimer:=0;
  cctimer_opt:=0;
end;

procedure TFullscreenMenu.SetFont(AValue: TFont);
begin
  if FFont.IsEqual(AValue) then exit;
  FFont.Assign(AValue);
end;

procedure TFullscreenMenu._OnStopTimer(Sender: TObject);
var
  i: integer;
begin
  FActive:=false;
  for i:=0 to FCount-1 do cLabels[i].Free;
  SetLength(cLabels,0);
  cPanel.Free;
  if assigned(FOnExecute) then FOnExecute(FItemIndex,cctimer_opt);
  if assigned(FOnAfter) then FOnAfter(FItemIndex);
end;

procedure TFullscreenMenu._OnTimer(Sender: TObject);
var
  a,b: integer;
begin
  a:=cctimer div 30;
  b:=cctimer mod 30;
  if b=0 then
  begin
    cctimer_opt:=a;
    cLabels[a].Font.Color:=FActiveColor;
    cLabels[a].Color:=FBGColor;
    if a>0 then
    begin
      cLabels[a-1].Font.Assign(FFont);
      cLabels[a-1].Color:=FColor;
    end;
  end;
  if (a=FCount-1) and (cctimer>(FCount-1)*30+10) then FTimer.Enabled:=false;
  inc(cctimer);
end;

procedure TFullscreenMenu.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

constructor TFullscreenMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=false;
  FIndex:=-1;
  FFont:=TFont.Create;
  FItems:=TStringList.Create;
  FTimer:=TTimer.Create(nil);
  FTimer.Enabled:=false;
  FTimer.Interval:=50;
  FTimer.OnStartTimer:=@_OnStartTimer;
  FTimer.OnStopTimer:=@_OnStopTimer;
  FTimer.OnTimer:=@_OnTimer;
  FColor:=clBlue;
  FActiveColor:=clBlack;
  FBGColor:=clYellow;
  FUpcase:=false;
  FCursorOff:=false;
end;

destructor TFullscreenMenu.Destroy;
begin
  FFont.Free;
  FItems.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TFullscreenMenu.Execute(aItemIndex: integer);
var
  s: string;
  i: integer;
begin
  cctimer_opt:=0;
  if aItemIndex=-1 then FItemIndex:=FIndex else FItemIndex:=aItemIndex;
  if assigned(FOnBefore) then FOnBefore(FItemIndex);
  FActive:=true;
  s:=FItems[FItemIndex];
  FCount:=GetLineCount(s,',');
  (* tworzę kontrolki *)
  cPanel:=TPanel.Create(FControl);
  cPanel.Parent:=FControl;
  cPanel.Color:=FColor;
  if FCursorOff then cPanel.Cursor:=crNone else cPanel.Cursor:=crDefault;
  SetLength(cLabels,FCount);
  cPanel.Width:=1000;
  for i:=0 to FCount-1 do
  begin
    cLabels[i]:=TLabel.Create(cPanel);
    cLabels[i].Parent:=cPanel;

    //if i=cctimer_opt then cLabels[i].Font.Color:=FActiveColor else cLabels[i].Font.Color:=FCancelColor;
    if i=cctimer_opt then cLabels[i].Color:=FBGColor else cLabels[i].Color:=FColor;

    if FUpcase then cLabels[i].Caption:=ansiuppercase(GetLineToStr(s,i+1,',')) else cLabels[i].Caption:=GetLineToStr(s,i+1,',');
    cLabels[i].Font.Assign(FFont);
    cLabels[i].Left:=32;
    cLabels[i].Top:=round(((cLabels[0].Height*(FCount+1))/FCount)*i+(cLabels[i].Height/2));
    cLabels[i].Alignment:=taCenter;
    cLabels[i].AutoSize:=false;
    cLabels[i].Width:=cPanel.Width-64;
  end;
  cPanel.Height:=cLabels[0].Height*(FCount+2)-round(cLabels[0].Height/2);
  cPanel.Top:=round((FControl.Height/2)-(cPanel.Height/2));
  cPanel.Left:=round((FControl.Width/2)-(cPanel.Width/2));
  (* proces *)
  FTimer.Enabled:=true;
end;

procedure TFullscreenMenu.Click;
begin
  FTimer.Enabled:=false;
end;

end.
