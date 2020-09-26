unit FullscreenMenu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Controls, Graphics, StdCtrls, ExtCtrls;

type

  { TFullscreenMenu }

  TFullscreenMenuMode = (fmNone,fmAutomatic,fmManual);
  TFullscreenMenuOnBefore = procedure(aItemIndex: integer) of object;
  TFullscreenMenuOnExecute = procedure(aItemIndex: integer; aResult: integer) of object;
  TFullscreenMenuOnAfter = procedure(aItemIndex: integer) of object;
  TFullscreenMenu = class(TComponent)
  private
    cctimer,cctimer_opt: integer;
    cPanel: TPanel;
    cLabels: array of TLabel;
    cmode: TFullscreenMenuMode;
    FActive: boolean;
    FAlarmColor: TColor;
    FBGColor: TColor;
    FColor,FActiveColor: TColor;
    FControl: TWinControl;
    FCursorOff: boolean;
    FFont: TFont;
    FIndex,FCount,FItemIndex: integer;
    FItems: TStrings;
    FMode: TFullscreenMenuMode;
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
    procedure SetMenuPosition(aIndex: integer);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(aItemIndex: integer = -1; aMode: TFullscreenMenuMode = fmNone);
    function IsManual: boolean;
    procedure Click;
    procedure Cancel;
    procedure Next;
    procedure Prior;
  published
    {Ustawione, gdy kontrolka działa}
    property Active: boolean read FActive;
    {Kontrolka na której ma być wyświetlane menu}
    property DefControl: TWinControl read FControl write FControl;
    {Domyślny tryb działania kontrolki:}
    { fmNone      - tryb nie jest wybrany domyślnie}
    { fmAutomatic - tryb pracy automatycznej}
    { fmManual    - tryb pracy manualnej}
    property Mode: TFullscreenMenuMode read FMode write FMode;
    {Domyślny font (kolor dot. nieaktywnych)}
    property Font: TFont read FFont write SetFont;
    {Zestawy menu oddzielone przecinkami}
    property Items: TStrings read FItems write SetItems;
    {Domyślny zestaw menu, gdy nie podany w parametrze}
    property DefaultIndex: integer read FIndex write FIndex;
    {Kolor tła menu}
    property Color: TColor read FColor write FColor;
    {Kolor fontu pozycji aktywnych}
    property ActiveColor: TColor read FActiveColor write FActiveColor;
    {Kolor fontu pozycji alarmowej}
    property AlarmColor: TColor read FAlarmColor write FAlarmColor;
    {Kolor tła fontu pozycji aktywnych}
    property ActiveColorBG: TColor read FBGColor write FBGColor;
    {Wyświetlanie pozycji menu dużymi literami}
    property UpcaseSet: boolean read FUpcase write FUpcase;
    {Ukrywaj kursor myszy po najechaniu na menu}
    property CursorOff: boolean read FCursorOff write FCursorOff;
    {Programowanie funkcji menu}
    property OnExecute: TFullscreenMenuOnExecute read FOnExecute write FOnExecute;
    {Kod wykonywany przed odpaleniem kontrolki}
    property OnBefore: TFullscreenMenuOnBefore read FOnBefore write FOnBefore;
    {Kod wykonywany po zakończeniu pracy kontrolki}
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
    SetmenuPosition(a);
  end;
  if (a=FCount-1) and (cctimer>(FCount-1)*30+10) then FTimer.Enabled:=false;
  inc(cctimer);
end;

procedure TFullscreenMenu.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

procedure TFullscreenMenu.SetMenuPosition(aIndex: integer);
var
  i: integer;
  alarm: boolean;
begin
  (* wyłączam wszystkie pozycje *)
  for i:=0 to FCount-1 do
  begin
    alarm:=cLabels[i].Font.Color=FAlarmColor;
    if not alarm then cLabels[i].Font.Assign(FFont);
    cLabels[i].Color:=FColor;
  end;
  (* włączam pożądaną pozycję *)
  if cLabels[aIndex].Font.Color<>FAlarmColor then cLabels[aIndex].Font.Color:=FActiveColor;
  cLabels[aIndex].Color:=FBGColor;
end;

constructor TFullscreenMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=false;
  FMode:=fmNone;
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
  FAlarmColor:=clRed;
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

procedure TFullscreenMenu.Execute(aItemIndex: integer;
  aMode: TFullscreenMenuMode);
var
  s,s1: string;
  i: integer;
  alarm: boolean;
begin
  if aMode=fmNone then cmode:=FMode else cmode:=aMode;
  if cmode=fmNone then exit;
  if aItemIndex=-1 then FItemIndex:=FIndex else FItemIndex:=aItemIndex;
  if FItemIndex=-1 then exit;
  cctimer_opt:=0;
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
    if i=cctimer_opt then cLabels[i].Color:=FBGColor else cLabels[i].Color:=FColor;
    if FUpcase then s1:=ansiuppercase(GetLineToStr(s,i+1,',')) else s1:=GetLineToStr(s,i+1,',');
    alarm:=pos('$',s1)>0;
    s1:=StringReplace(s1,'$','',[rfReplaceAll]);
    cLabels[i].Caption:=s1;
    cLabels[i].Font.Assign(FFont);
    if alarm then cLabels[i].Font.Color:=FAlarmColor;
    cLabels[i].Left:=32;
    cLabels[i].Top:=round(((cLabels[0].Height*(FCount+1))/FCount)*i+(cLabels[i].Height/2))-15;
    cLabels[i].Alignment:=taCenter;
    cLabels[i].AutoSize:=false;
    cLabels[i].Width:=cPanel.Width-64;
  end;
  cPanel.Height:=cLabels[0].Height*(FCount+2)-round(cLabels[0].Height/2);
  cPanel.Top:=round((FControl.Height/2)-(cPanel.Height/2));
  cPanel.Left:=round((FControl.Width/2)-(cPanel.Width/2));
  (* proces *)
  if cmode=fmAutomatic then FTimer.Enabled:=true else SetMenuPosition(cctimer_opt);
end;

function TFullscreenMenu.IsManual: boolean;
begin
  result:=FActive and (cmode=fmManual);
end;

procedure TFullscreenMenu.Click;
begin
  if cmode=fmAutomatic then FTimer.Enabled:=false else _OnStopTimer(self);
end;

procedure TFullscreenMenu.Cancel;
begin
  if cmode=fmAutomatic then FTimer.Enabled:=false else
  begin
    cctimer_opt:=-1;
    _OnStopTimer(self);
  end;
end;

procedure TFullscreenMenu.Next;
begin
  inc(cctimer_opt);
  if cctimer_opt>=FCount then cctimer_opt:=0;
  SetMenuPosition(cctimer_opt);
end;

procedure TFullscreenMenu.Prior;
begin
  dec(cctimer_opt);
  if cctimer_opt<0 then cctimer_opt:=FCount-1;
  SetMenuPosition(cctimer_opt);
end;

end.
