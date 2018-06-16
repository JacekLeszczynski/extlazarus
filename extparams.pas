unit ExtParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TExtParams }

  TExtParams = class(TComponent)
  private
    FAfterExecute: TNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FVP: TStrings;
    { Private declarations }
    list1,list2: TStrings;
    procedure SetVP(AValue: TStrings);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    function Count: integer;
    function GetParam(Index: integer): string;
    function GetValue(Index: integer): string;
    function IsParam(par1: string; par2: string = ''; par3: string = ''; par4: string = ''; par5: string = ''; par6: string = ''): boolean;
    function GetValue(par1: string; par2: string = ''; par3: string = ''; par4: string = ''; par5: string = ''; par6: string = ''): string;
    function IsNull(wartosc,domyslna: string):string;
  published
    { Published declarations }
    property ParamsForValues: TStrings read FVP write SetVP;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I extparams_icon.lrs}
  RegisterComponents('System',[TExtParams]);
end;

function StringToItemIndex(slist: TStrings; kod: string; wart_domyslna: integer): integer;
var
  i,a: integer;
begin
   a:=wart_domyslna;
   for i:=0 to slist.Count-1 do if slist[i]=kod then
   begin
     a:=i;
     break;
   end;
   result:=a;
end;

{ TExtParams }

procedure TExtParams.SetVP(AValue: TStrings);
begin
  FVP.Assign(AValue);
end;

constructor TExtParams.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVP:=TStringList.Create;
  list1:=TStringList.Create;
  list2:=TStringList.Create;
end;

destructor TExtParams.Destroy;
begin
  FVP.Free;
  list1.Free;
  list2.Free;
  inherited Destroy;
end;

(* procedura wczytuje automatycznie parametry do tablic *)
procedure TExtParams.Execute;
var
  i,a: integer;
  s,w: string;
  licznik: integer;
  blokada: boolean;
begin
  if Assigned(FBeforeExecute) then FBeforeExecute(self);
  list1.Clear;
  list2.Clear;
  i:=0;
  licznik:=1;
  while true do
  begin
    inc(i);
    if ParamCount<i then break;
    s:=ParamStr(i);
    (* sprawdzam czy jest to parametr zaczynający się na '-' lub '--' *)
    if s[1]='-' then
    begin
      blokada:=false;
      while s[1]='-' do delete(s,1,1); //pozbywam się pierwszych znaków '-'
      if s='' then continue;
      a:=pos('=',s);
      if a>0 then
      begin
        (* w parametrze istnieje również wartość *)
        w:=copy(s,a+1,length(s)-a);
        s:=copy(s,1,a-1);
        list1.Add(s);
        if (FVP.Count=0) or (StringToItemIndex(FVP,s,-1)<>-1) then list2.Add(w);
      end else begin
        (* następny parametr jest wartością lub nie *)
        list1.Add(s);
        if (i+1<=ParamCount) and ((FVP.Count=0) or (StringToItemIndex(FVP,s,-1)<>-1)) then
        begin
          w:=ParamStr(i+1);
          if w[1]<>'-' then
          begin
            list2.Add(w);
            inc(i);
          end else list2.Add('');
        end else list2.Add('');
      end;
    end else begin
      list1.Add('_'+IntToStr(licznik));
      list2.Add(s);
      inc(licznik);
    end;
  end;
  if Assigned(FAfterExecute) then FAfterExecute(self);
end;

function TExtParams.Count: integer;
begin
  result:=list1.Count;
end;

function TExtParams.GetParam(Index: integer): string;
begin
  result:=list1[Index];
end;

function TExtParams.GetValue(Index: integer): string;
begin
  result:=list2[Index];
end;

function _IsParam(list: TStrings; par: string):boolean;
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to list.Count-1 do if list[i]=par then
  begin
    b:=true;
    break;
  end;
  result:=b;
end;

function TExtParams.IsParam(par1: string; par2: string; par3: string;
  par4: string; par5: string; par6: string): boolean;
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=1 to 6 do
  begin
    case i of
      1: b:=_IsParam(list1,par1);
      2: b:=_IsParam(list1,par2);
      3: b:=_IsParam(list1,par3);
      4: b:=_IsParam(list1,par4);
      5: b:=_IsParam(list1,par5);
      6: b:=_IsParam(list1,par6);
    end;
    if b then break;
  end;
  result:=b;
end;

function _GetValue(list1,list2: TStrings; param: string): string;
var
  i: integer;
  s: string;
begin
  if param='' then result:='' else
  begin
    s:='';
    for i:=0 to list1.Count-1 do if list1[i]=param then
    begin
      s:=list2[i];
      break;
    end;
    result:=s;
  end;
end;

function TExtParams.GetValue(par1: string; par2: string; par3: string;
  par4: string; par5: string; par6: string): string;
var
  i: integer;
  s: string;
begin
  s:='';
  for i:=1 to 6 do
  begin
    case i of
      1: s:=_GetValue(list1,list2,par1);
      2: s:=_GetValue(list1,list2,par2);
      3: s:=_GetValue(list1,list2,par3);
      4: s:=_GetValue(list1,list2,par4);
      5: s:=_GetValue(list1,list2,par5);
      6: s:=_GetValue(list1,list2,par6);
    end;
    if s<>'' then break;
  end;
  result:=s;
end;

function TExtParams.IsNull(wartosc, domyslna: string): string;
begin
  if wartosc='' then result:=domyslna else result:=wartosc;
end;

end.
