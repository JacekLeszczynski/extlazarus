unit ExtMessage;

{by Jacek Leszczyñski}

{$IFNDEF FPC AND $IFDEF MSWINDOWS}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE LAZARUS}
{$ENDIF}

{$IFDEF LAZARUS}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$IFDEF DELPHI}
{$R TExtMessage.dcr}
{$ENDIF}

uses
  Classes, SysUtils, ExtLazarusTypes,
  {$IFDEF DELPHI}
  Windows, Messages,
  {$ELSE}
  LResources, Controls, Graphics,
  {$ENDIF}
  Forms, Dialogs;

  { TExtMessage }

type
  TExtMessageCalculateTextEvent = procedure(Sender: TObject; var AText: string) of object;

  TExtMessage = class(TComponent)
  private
    FAutoEncoding: TAutoEncoding;
    FButtons: TMsgDlgButtons;
    FCaption: string;
    FDefButton: TDefMsgDlgBtn;
    FHelpCtx: integer;
    FLeft: integer;
    FMessage: TStrings;
    FPosition: TPositionDlg;
    FResult: TResultDlgBtn;
    FResultButton: TResultDlgBtn;
    FTop: integer;
    FType: TWewnMsgDlgType;
    FS2CR: string;
    FSplashForm: boolean;
    FOnBeforeShow: TNotifyEvent;
    FOnAfterShow: TNotifyEvent;
    FOnCalculate: TExtMessageCalculateTextEvent;
    FStrReplace: TStrings;
    FStrDelete: TStrings;
    procedure SetCaption(AValue: string);
    procedure SetMessage(AValue: TStrings);
    {$IFDEF LAZARUS}
    function GetDefaultButton(button: TDefMsgDlgBtn): TMsgDlgBtn;
    {$ENDIF}
    function GetMsgDlgType(aType: TWewnMsgDlgType): TMsgDlgType;
    function ConvIsoTest(s:string):string;
    procedure SetStrReplace(const Value: TStrings);
    procedure SetStrDelete(const Value: TStrings);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean; overload;
    function Execute(sMessage: string): boolean; overload;
    procedure ShowMessage(sMessage: string = ''); overload;
    procedure ShowMessage(sCaption,sMessage: string); overload;
    procedure ShowInformation(sMessage: string = ''); overload;
    procedure ShowInformation(sCaption,sMessage: string); overload;
    procedure ShowConfirmation(sMessage: string = ''); overload;
    procedure ShowConfirmation(sCaption,sMessage: string); overload;
    procedure ShowWarning(sMessage: string = ''); overload;
    procedure ShowWarning(sCaption,sMessage: string); overload;
    procedure ShowError(sMessage: string = ''); overload;
    procedure ShowError(sCaption,sMessage: string); overload;
    function ShowMessageYesNo(sMessage: string = ''):boolean; overload;
    function ShowMessageYesNo(sCaption,sMessage: string):boolean; overload;
    function ShowInformationYesNo(sMessage: string = ''):boolean; overload;
    function ShowInformationYesNo(sCaption,sMessage: string):boolean; overload;
    function ShowConfirmationYesNo(sMessage: string = ''):boolean; overload;
    function ShowConfirmationYesNo(sCaption,sMessage: string):boolean; overload;
    function ShowWarningYesNo(sMessage: string = ''):boolean; overload;
    function ShowWarningYesNo(sCaption,sMessage: string):boolean; overload;
    function ShowErrorYesNo(sMessage: string = ''):boolean; overload;
    function ShowErrorYesNo(sCaption,sMessage: string):boolean; overload;
    function ShowMessageOkCancel(sMessage: string = ''):boolean; overload;
    function ShowMessageOkCancel(sCaption,sMessage: string):boolean; overload;
    function ShowInformationOkCancel(sMessage: string = ''):boolean; overload;
    function ShowInformationOkCancel(sCaption,sMessage: string):boolean; overload;
    function ShowConfirmationOkCancel(sMessage: string = ''):boolean; overload;
    function ShowConfirmationOkCancel(sCaption,sMessage: string):boolean; overload;
    function ShowWarningOkCancel(sMessage: string = ''):boolean; overload;
    function ShowWarningOkCancel(sCaption,sMessage: string):boolean; overload;
    function ShowErrorOkCancel(sMessage: string = ''):boolean; overload;
    function ShowErrorOkCancel(sCaption,sMessage: string):boolean; overload;
    procedure ShowInfo(Caption: string = '');
    procedure HideInfo;
    procedure SetDefault;
  published
    property AutoEncoding: TAutoEncoding read FAutoEncoding write FAutoEncoding default isoDefault;
    property Caption: string read FCaption write SetCaption;
    property Message: TStrings read FMessage write SetMessage;
    property StrReplace: TStrings read FStrReplace write SetStrReplace;
    property StrDelete: TStrings read FStrDelete write SetStrDelete;
    property DlgType: TWewnMsgDlgType read FType write FType default wmtInformation;
    property Buttons: TMsgDlgButtons read FButtons write FButtons default [mbOK];
    property HelpContext: integer read FHelpCtx write FHelpCtx;
    property DefaultButton: TDefMsgDlgBtn read FDefButton write FDefButton default dbNone;
    property Position: TPositionDlg read FPosition write FPosition default psScreenCenter;
    property PosTop: integer read FTop write FTop;
    property PosLeft: integer read FLeft write FLeft;
    property ResultButton: TResultDlgBtn read FResultButton write FResultButton default rsOK;
    property Result: TResultDlgBtn read FResult;
    property StringToCR: string read FS2CR write FS2CR;
    property SplashForm: boolean read FSplashForm default false;
    property OnBeforeShow: TNotifyEvent read FOnBeforeShow write FOnBeforeShow;
    property OnAfterShow: TNotifyEvent read FOnAfterShow write FOnAfterShow;
    property OnCalculate: TExtMessageCalculateTextEvent read FOnCalculate write FOnCalculate;
  end;

procedure Register;

implementation

uses
  eCode,
{$IFDEF LAZARUS}
  lconvencoding, extlazarus_utf8,
{$ELSE}
  ext_lazarus_komunikaty_delphi,
{$ENDIF}
  extmessage_form;

var
  FExtMessageForm: TFExtMessageForm;

procedure Register;
begin
{$IFDEF LAZARUS}
  {$I extmessage_icon.lrs}
{$ENDIF}
  RegisterComponents('Dialogs', [TExtMessage]);
end;

{ TExtMessage }

procedure TExtMessage.SetMessage(AValue: TStrings);
begin
  FMessage.Assign(AValue);
end;

procedure TExtMessage.SetStrDelete(const Value: TStrings);
begin
  FStrDelete.Assign(Value);
end;

procedure TExtMessage.SetStrReplace(const Value: TStrings);
begin
  FStrReplace.Assign(Value);
end;

{$IFDEF LAZARUS}
function TExtMessage.GetDefaultButton(button: TDefMsgDlgBtn): TMsgDlgBtn;
begin
  case button of
    dbYes:      result:=mbYes;
    dbNo:       result:=mbNo;
    dbOK:       result:=mbOK;
    dbCancel:   result:=mbCancel;
    dbAbort:    result:=mbAbort;
    dbRetry:    result:=mbRetry;
    dbIgnore:   result:=mbIgnore;
    dbAll:      result:=mbAll;
    dbNoToAll:  result:=mbNoToAll;
    dbYesToAll: result:=mbYesToAll;
    dbHelp:     result:=mbHelp;
    {$IFDEF LAZARUS}
    dbClose:    result:=mbClose;
    {$ENDIF}
  end;
end;
{$ENDIF}

function TExtMessage.GetMsgDlgType(aType: TWewnMsgDlgType): TMsgDlgType;
begin
  case aType of
    wmtWarning:      result:=mtWarning;
    wmtError:        result:=mtError;
    wmtInformation:  result:=mtInformation;
    wmtConfirmation: result:=mtConfirmation;
    else result:=mtCustom;
  end;
end;

procedure TExtMessage.HideInfo;
begin
  if not FSplashForm then exit;
  FExtMessageForm.Hide;
  FExtMessageForm.Free;
  FSplashForm:=false;
end;

function TExtMessage.ConvIsoTest(s: string): string;
const
  tab: array [1..18] of char = (#202,#211,#165,#140,#163,#175,#143,#198,#209,
                                #234,#243,#156,#191,#159,#230,#241,#185,#179);
var
  a,i: integer;
  pom: string;
begin
  case FAutoEncoding of
    iso1250: {$IFDEF LAZARUS} pom:=ConvertEncoding(s,'cp1250','utf8'); {$ELSE} pom:=s; {$ENDIF}
    isoAUTO: begin
               for i:=1 to 17 do
               begin
                 a:=pos(tab[i],s);
                 if a>0 then break;
               end;
               if a>0 then {$IFDEF LAZARUS} pom:=ConvertEncoding(s,'cp1250','utf8') {$ELSE} pom:=s {$ENDIF} else pom:=s;
             end;
    else pom:=s;
  end;
  result:=pom;
end;

procedure TExtMessage.SetCaption(AValue: string);
begin
  if FCaption=AValue then Exit;
  if AValue='' then FCaption:='<Auto>' else FCaption:=AValue;
end;

constructor TExtMessage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessage:=TStringList.Create;
  FStrReplace:=TStringList.Create;
  FStrDelete:=TStringList.Create;
  FS2CR:='';
  FSplashForm:=false;
  SetDefault;
end;

destructor TExtMessage.Destroy;
begin
  if FSplashForm then HideInfo;
  FMessage.Free;
  FStrReplace.Free;
  FStrDelete.Free;
  inherited Destroy;
end;

function TExtMessage.Execute: boolean;
begin
  result:=Execute(FMessage.Text);
end;

function TExtMessage.Execute(sMessage: string): boolean;
var
  s,s2,ss: string;
  r: integer;
  dialog: TForm;
  i: integer;
begin
  if Assigned(FOnBeforeShow) then FOnBeforeShow(self);
  ss:=sMessage;
  if Assigned(FOnCalculate) then FOnCalculate(self,ss);
  if FS2CR<>'' then ss:=StringReplace(ss,FS2CR,#13#10,[rfReplaceAll]);
  for i:=0 to FStrReplace.Count-1 do ss:=StringReplace(ss,GetLineToStr(FStrReplace[i],1,','),GetLineToStr(FStrReplace[i],2,','),[rfReplaceAll]);
  for i:=0 to FStrDelete.Count-1 do ss:=StringReplace(ss,FStrDelete[i],'',[rfReplaceAll]);
  if FType=wmtMessage then
  begin
    (* niekonfigurowalna wiadomosc *)
    ShowMessage(ConvIsoTest(ss));
    FResult:=rsNone;
    result:=true;
  end else begin
    (* konfigurowalna wiadomosc *)
    s:=FCaption;
    if s='<Auto>' then
    begin
      case FType of
        wmtWarning:      s:=com_1;
        wmtError:        s:=com_2;
        wmtInformation:  s:=com_3;
        wmtConfirmation: s:=com_4;
        wmtCustom:       s:=com_5;
      end;
    end else s:=ConvIsoTest(s);
    s2:=ConvIsoTest(ss);
    case FPosition of
      {$IFDEF LAZARUS}
      psScreenCenter:   r:=MessageDlg(s,s2,GetMsgDlgType(FType),FButtons,FHelpCtx,GetDefaultButton(FDefButton));
      {$ENDIF}
      psTopLeft:        r:=MessageDlgPos(s2,GetMsgDlgType(FType),FButtons,FHelpCtx,FLeft,FTop);
      else begin
        dialog:=CreateMessageDialog(s2,GetMsgDlgType(FType),FButtons);
        try
          dialog.Caption:=s;
          case FPosition of
            psScreenCenter:    dialog.Position:=poScreenCenter;
            psTopLeft:         begin
                                 dialog.Position:=poDesigned;
                                 dialog.Left:=FLeft;
                                 dialog.Top:=FTop;
                               end;
            psMainFormCenter:  dialog.Position:=poMainFormCenter;
            psOwnerFormCenter: dialog.Position:=poOwnerFormCenter;
          end;
          dialog.HelpContext:=FHelpCtx;
          dialog.ShowModal;
          r:=dialog.ModalResult;
        finally
          dialog.Free;
        end;
      end;
    end;
    case r of
       1: FResult:=rsOK;
       2: FResult:=rsCancel;
       3: FResult:=rsAbort;
       4: FResult:=rsRetry;
       5: FResult:=rsIgnore;
       6: FResult:=rsYes;
       7: FResult:=rsNo;
       8: FResult:=rsAll;
       9: FResult:=rsNoToAll;
      10: FResult:=rsYesToAll;
    end;
    result:=FResult=FResultButton;
  end;
  if Assigned(FOnAfterShow) then FOnAfterShow(self);
end;

procedure TExtMessage.ShowMessage(sMessage: string);
begin
  ShowInformation(sMessage);
end;

procedure TExtMessage.ShowMessage(sCaption, sMessage: string);
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  ShowInformation(sMessage);
  FCaption:=pom;
end;

procedure TExtMessage.ShowInformation(sMessage: string);
begin
  FButtons:=[mbOK];
  FType:=wmtInformation;
  Execute(sMessage);
end;

procedure TExtMessage.ShowInfo(Caption: string);
var
  s: string;
begin
  if FSplashForm then exit;
  FSplashForm:=true;
  FExtMessageForm:=TFExtMessageForm.Create(self);
  case FPosition of
    psScreenCenter: FExtMessageForm.Position:=poScreenCenter;
    psMainFormCenter: FExtMessageForm.Position:=poMainFormCenter;
    psOwnerFormCenter: FExtMessageForm.Position:=poOwnerFormCenter;
    psTopLeft: begin
                 FExtMessageForm.Position:=poDesigned;
                 FExtMessageForm.Left:=FLeft;
                 FExtMessageForm.Top:=FTop;
               end;
  end;
  if Caption='' then s:=FCaption else s:=Caption;
  if FS2CR<>'' then s:=StringReplace(s,FS2CR,#13#10,[rfReplaceAll]);
  FExtMessageForm.StrCaption:=s;
  FExtMessageForm.Label1.Color:=clBlue;
  FExtMessageForm.Label1.Font.Name:='Arial';
  FExtMessageForm.Label1.Font.Color:=clWhite;
  //FExtMessageForm.Label1.Font.Style:=[fsBold];
  FExtMessageForm.Label1.Font.Size:=14;
  FExtMessageForm.Show;
  FExtMessageForm.Update;
end;

procedure TExtMessage.ShowInformation(sCaption, sMessage: string);
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  ShowInformation(sMessage);
  FCaption:=pom;
end;

procedure TExtMessage.ShowConfirmation(sMessage: string);
begin
  FButtons:=[mbOK];
  FType:=wmtConfirmation;
  Execute(sMessage);
end;

procedure TExtMessage.ShowConfirmation(sCaption, sMessage: string);
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  ShowConfirmation(sMessage);
  FCaption:=pom;
end;

procedure TExtMessage.ShowWarning(sMessage: string);
begin
  FButtons:=[mbOK];
  FType:=wmtWarning;
  Execute(sMessage);
end;

procedure TExtMessage.ShowWarning(sCaption, sMessage: string);
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  ShowWarning(sMessage);
  FCaption:=pom;
end;

procedure TExtMessage.ShowError(sMessage: string);
begin
  FButtons:=[mbOK];
  FType:=wmtError;
  Execute(sMessage);
end;

procedure TExtMessage.ShowError(sCaption, sMessage: string);
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  ShowError(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowMessageYesNo(sMessage: string): boolean;
begin
  result:=ShowInformationYesNo(sMessage);
end;

function TExtMessage.ShowMessageYesNo(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowMessageYesNo(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowInformationYesNo(sMessage: string): boolean;
begin
  FButtons:=[mbYes,mbNo];
  FType:=wmtInformation;
  FResultButton:=rsYes;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowInformationYesNo(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowInformationYesNo(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowConfirmationYesNo(sMessage: string): boolean;
begin
  FButtons:=[mbYes,mbNo];
  FType:=wmtConfirmation;
  FResultButton:=rsYes;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowConfirmationYesNo(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowConfirmationYesNo(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowWarningYesNo(sMessage: string): boolean;
begin
  FButtons:=[mbYes,mbNo];
  FType:=wmtWarning;
  FResultButton:=rsYes;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowWarningYesNo(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowWarningYesNo(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowErrorYesNo(sMessage: string): boolean;
begin
  FButtons:=[mbYes,mbNo];
  FType:=wmtError;
  FResultButton:=rsYes;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowErrorYesNo(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowErrorYesNo(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowMessageOkCancel(sMessage: string): boolean;
begin
  result:=ShowInformationOkCancel(sMessage);
end;

function TExtMessage.ShowMessageOkCancel(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowMessageOkCancel(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowInformationOkCancel(sMessage: string): boolean;
begin
  FButtons:=[mbOK,mbCancel];
  FType:=wmtInformation;
  FResultButton:=rsOK;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowInformationOkCancel(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowInformationOkCancel(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowConfirmationOkCancel(sMessage: string): boolean;
begin
  FButtons:=[mbOK,mbCancel];
  FType:=wmtConfirmation;
  FResultButton:=rsOK;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowConfirmationOkCancel(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowConfirmationOkCancel(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowWarningOkCancel(sMessage: string): boolean;
begin
  FButtons:=[mbOK,mbCancel];
  FType:=wmtWarning;
  FResultButton:=rsOK;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowWarningOkCancel(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowWarningOkCancel(sMessage);
  FCaption:=pom;
end;

function TExtMessage.ShowErrorOkCancel(sMessage: string): boolean;
begin
  FButtons:=[mbOK,mbCancel];
  FType:=wmtError;
  FResultButton:=rsOK;
  result:=Execute(sMessage);
end;

function TExtMessage.ShowErrorOkCancel(sCaption, sMessage: string): boolean;
var
  pom: string;
begin
  pom:=FCaption;
  FCaption:=sCaption;
  result:=ShowErrorOkCancel(sMessage);
  FCaption:=pom;
end;

procedure TExtMessage.SetDefault;
begin
  FAutoEncoding:=isoDefault;
  FButtons:=[mbOK];
  FCaption:='<Auto>';
  FDefButton:=dbNone;
  FHelpCtx:=0;
  FType:=wmtInformation;
  FLeft:=0;
  FTop:=0;
  Fposition:=psScreenCenter;
  FResult:=rsNone;
  FResultButton:=rsOK;
end;

end.
