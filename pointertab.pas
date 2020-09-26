unit PointerTab;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$IFDEF DELPHI}
{$R TPointerTab.dcr}
{$ENDIF}

uses
  Classes, SysUtils, LResources;

type
  t_wskaznik = ^t_element;
  t_element = record
    wsk: t_wskaznik;
    body: pointer;
  end;

type

  TPointerTabRodzaj = (roStos, roLista, roKolejka);
  TPointerTabOperationEvent = procedure(Sender: TObject; var AWskaznik: Pointer) of object;
  TPointerTabSortedEvent = procedure(Sender: TObject; AWsk1,AWsk2: Pointer; var AWieksze: byte) of object; //0:AWsk1=AWsk2, 1:Awsk1>Awsk2, 2:Awsk1<Awsk2

  { TPointerTab }

  TPointerTab = class(TComponent)
  private
    ww,koniec: t_wskaznik;
    FOnCreateElement: TPointerTabOperationEvent;
    FOnDestroyElement: TPointerTabOperationEvent;
    FOnReadElement: TPointerTabOperationEvent;
    FOnSorted: TPointerTabSortedEvent;
    FOnWriteElement: TPointerTabOperationEvent;
    FRodzaj: TPointerTabRodzaj;
    FCount: integer;
    { Private declarations }
    procedure clear_stos;
    procedure wartosci_domyslne;
    procedure na_stos;
    function ze_stosu: boolean;
    function ze_stosu_clear: boolean;
    procedure do_kolejki;
    function z_kolejki: boolean;
    function pobierz_adres_elementu(index: integer): pointer;
    function pobierz_adres_wartosci(index: integer): pointer;
    (* kod do sortowania szybkiego *)
    function a_mniejsze_b(a,b: integer): boolean;
    procedure sort_babelkowe(L,R: integer);
    procedure quicksort(L,R: integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Add;
    function Read: boolean; //stosuje się tylko do stosu i kolejki
    function Read(index: integer): boolean; //stosuje się tylko do listy
    function Delete(index: integer): boolean; //usuwa element listy
    function Insert(index: integer): boolean; //dodaję element do wskazanego indeksu przesuwając resztę elementów o jeden dalej
    function Edit(index: integer): boolean; //edytuje element o wskazanym indeksie, stara zawartość jest zamieniana nową
    function Replace(i1,i2: integer): boolean; //zamienia wskaźniki w ten sposób, że i1 staje się i2, zaś i2 wchodzi na miejsce i1
    procedure Sort;
  published
    { Published declarations }
    property Count: integer read FCount;
    property Rodzaj: TPointerTabRodzaj read FRodzaj write FRodzaj default roLista;
    property OnCreateElement: TPointerTabOperationEvent read FOnCreateElement write FOnCreateElement;
    property OnDestroyElement: TPointerTabOperationEvent read FOnDestroyElement write FOnDestroyElement;
    property OnReadElement: TPointerTabOperationEvent read FOnReadElement write FOnReadElement;
    property OnWriteElement: TPointerTabOperationEvent read FOnWriteElement write FOnWriteElement;
    property OnSorted: TPointerTabSortedEvent read FOnSorted write FOnSorted;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF FPC}
  {$I pointertab_icon.lrs}
  RegisterComponents('System',[TPointerTab]);
  {$ELSE}
  RegisterComponents('NobleSecurities',[TPointerTab]);
  {$ENDIF}
end;

{ TPointerTab }

procedure TPointerTab.clear_stos;
begin
  while ze_stosu_clear do;
end;

procedure TPointerTab.wartosci_domyslne;
begin
  FCount:=0;
  FRodzaj:=roLista;
end;

procedure TPointerTab.na_stos;
var
  w: t_wskaznik;
begin
  if (not Assigned(FOnCreateElement)) or (not Assigned(FOnWriteElement)) then exit;
  new(w);
  FOnCreateElement(self,w^.body);
  FOnWriteElement(self,w^.body);
  if ww=nil then w^.wsk:=nil else w^.wsk:=ww;
  ww:=w;
  inc(FCount);
end;

function TPointerTab.ze_stosu: boolean;
var
  w: t_wskaznik;
begin
  //if (FCount=0) or (ww=nil) then
  if ww=nil then
  begin
    result:=false;
    exit;
  end;
  if (not Assigned(FOnDestroyElement)) or (not Assigned(FOnReadElement)) then exit;
  FOnReadElement(self,ww^.body);
  FOnDestroyElement(self,ww^.body);
  w:=ww;
  ww:=w^.wsk;
  dispose(w);
  if ww=nil then koniec:=nil;
  dec(FCount);
  result:=true;
end;

function TPointerTab.ze_stosu_clear: boolean;
var
  w: t_wskaznik;
begin
  if ww=nil then
  begin
    result:=false;
    exit;
  end;
  if not Assigned(FOnDestroyElement) then exit;
  FOnDestroyElement(self,ww^.body);
  w:=ww;
  ww:=w^.wsk;
  dispose(w);
  if ww=nil then koniec:=nil;
  dec(FCount);
  result:=true;
end;

procedure TPointerTab.do_kolejki;
var
  w: t_wskaznik;
begin
  (* tworzę stos, z tą różnicą, że nowe elementy dodaję na sam koniec tego stosu *)
  (* w tej sposób tworzy się kolejka, a czytanie jest dokładnie takie jak ze stosu *)
  if (not Assigned(FOnCreateElement)) or (not Assigned(FOnWriteElement)) then exit;
  new(w);
  FOnCreateElement(self,w^.body);
  FOnWriteElement(self,w^.body);
  w^.wsk:=nil;
  if koniec<>nil then koniec^.wsk:=w;
  koniec:=w;
  if ww=nil then ww:=koniec;
  inc(FCount);
end;

function TPointerTab.z_kolejki: boolean;
begin
  (* usuwając element z listy traktuję ją jako stos *)
  result:=ze_stosu;
end;

function TPointerTab.pobierz_adres_elementu(index: integer): pointer;
var
  w: t_wskaznik;
  i: integer;
begin
  w:=ww;
  for i:=1 to index do w:=w^.wsk;
  result:=w;
end;

function TPointerTab.pobierz_adres_wartosci(index: integer): pointer;
var
  w: t_wskaznik;
  i: integer;
begin
  w:=ww;
  for i:=1 to index do w:=w^.wsk;
  result:=w^.body;
end;

function TPointerTab.a_mniejsze_b(a, b: integer): boolean;
var
  wynik: byte;
begin
  FOnSorted(self,pobierz_adres_wartosci(a),pobierz_adres_wartosci(b),wynik);
  result:=wynik=2;
end;

procedure TPointerTab.sort_babelkowe(L, R: integer);
var
  b: boolean;
  i,j: integer;
  a1,a2: integer;
begin
  if L<R then begin a1:=L; a2:=R; end else begin a1:=R; a2:=L; end;
  for i:=a1 to a2 do
  begin
    b:=true;
    for j:=a1 to a2-1 do if a_mniejsze_b(j+1,j) then begin Replace(j,j+1); b:=false; end;
    if b then break;
  end;
end;

procedure TPointerTab.quicksort(L,R: integer);
var
  i, j, x, temp : Integer;
begin
  i:=L;
  j:=R;
  x:=((L+R+1) div 2)-1;
  repeat
    while a_mniejsze_b(i,x) do inc(i);
    while a_mniejsze_b(x,j) do dec(j);
    if i<=j Then
    begin
      Replace(i,j);
      inc(i);
      dec(j);
    end;
  until i>j;
  if L<j then quicksort(L,j);
  if i<R then quicksort(i,R);
end;

constructor TPointerTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  wartosci_domyslne;
  ww:=nil;
  koniec:=nil;
end;

destructor TPointerTab.Destroy;
begin
  clear_stos;
  inherited Destroy;
end;

procedure TPointerTab.Clear;
begin
  (* założenie jest takie, że wszystko trzymane jest jako stos, tak więc używamy zawsze tej samej procedury czyszczącej *)
  clear_stos;
end;

procedure TPointerTab.Add;
begin
  case FRodzaj of
    roStos: na_stos;
    roKolejka: do_kolejki;
    roLista: do_kolejki;
  end;
end;

function TPointerTab.Read: boolean;
begin
  if FRodzaj=roLista then
  begin
    result:=false;
    exit;
  end;
  case FRodzaj of
    roStos: result:=ze_stosu;
    roKolejka: result:=z_kolejki;
  end;
end;

function TPointerTab.Read(index: integer): boolean;
var
  i: integer;
  w: t_wskaznik;
begin
  if (FRodzaj<>roLista) or (index>=FCount) or (index<0) then
  begin
    result:=false;
    exit;
  end;
  (* ustawiam się na pierwszym elemencie listy, indeks=0 *)
  w:=ww;
  (* ustawiam się na właściwym elemencie listy *)
  for i:=1 to index do w:=w^.wsk;
  (* czytam wartość elementu *)
  FOnReadElement(self,w^.body);
  result:=true;
end;

function TPointerTab.Delete(index: integer): boolean;
var
  i: integer;
  poprzedni,w: t_wskaznik;
begin
  if (FRodzaj<>roLista) or (index>=FCount) or (index<0) then
  begin
    result:=false;
    exit;
  end;
  (* ustawiam się na pierwszym elemencie listy, indeks=0 *)
  poprzedni:=nil;
  w:=ww;
  (* ustawiam się na właściwym elemencie listy *)
  for i:=1 to index do
  begin
    poprzedni:=w;
    w:=w^.wsk;
  end;
  (* usuwam wartość elementu i cały element z listy *)
  if poprzedni=nil then ww:=w^.wsk else poprzedni^.wsk:=w^.wsk;
  if w^.wsk=nil then koniec:=poprzedni;
  FOnDestroyElement(self,w^.body);
  dispose(w);
  dec(FCount);
  result:=true;
end;

function TPointerTab.Insert(index: integer): boolean;
var
  i: integer;
  nowy,poprzedni,w: t_wskaznik;
begin
  if (FRodzaj<>roLista) or (index>=FCount) or (index<0) then
  begin
    result:=false;
    exit;
  end;
  (* tworzę nowy element i ustawiam mu wartość *)
  new(nowy);
  FOnCreateElement(self,nowy^.body);
  FOnWriteElement(self,nowy^.body);
  (* ustawiam się na pierwszym elemencie listy, indeks=0 *)
  poprzedni:=nil;
  w:=ww;
  (* ustawiam się na właściwym elemencie listy *)
  for i:=1 to index do
  begin
    poprzedni:=w;
    w:=w^.wsk;
  end;
  (* dodaję nowy element w to miejsce przesuwając resztę dalej o te jedno miejsce *)
  nowy^.wsk:=w;
  if poprzedni=nil then ww:=nowy else poprzedni^.wsk:=nowy;
  inc(FCount);
  result:=true;
end;

function TPointerTab.Edit(index: integer): boolean;
var
  i: integer;
  w: t_wskaznik;
begin
  if (FRodzaj<>roLista) or (index>=FCount) or (index<0) then
  begin
    result:=false;
    exit;
  end;
  (* ustawiam się na pierwszym elemencie listy, indeks=0 *)
  w:=ww;
  (* ustawiam się na właściwym elemencie listy *)
  for i:=1 to index do w:=w^.wsk;
  (* ustawiam w nim nową zawartość *)
  FOnWriteElement(self,w^.body);
  result:=true;
end;

function TPointerTab.Replace(i1, i2: integer): boolean;
var
  i,ii: integer;
  wartownik,w1,w2: t_wskaznik;
  p: pointer;
begin
  //jeśli nie jest to lista lub oba indeksy są tym samym indeksem, lub któryś z indeksów jest poza zakresem nie robię nic i wychodzę
  if (FRodzaj<>roLista) or (i1=i2) or (i1<0) or (i1>=FCount) or (i2<0) or (i2>=FCount) then
  begin
    result:=false;
    exit;
  end;
  //ustawiam indeksy tak, by pierwszy był mniejszy od drugiego
  ii:=i1;
  if i2>i1 then ii:=i2;
  //ustawiam teraz oba wskaźniki tak by wskazywały na elementy o pożądanych indeksach
  w1:=nil;
  w2:=nil;
  wartownik:=ww;
  for i:=0 to ii do
  begin
    if i=i1 then w1:=wartownik;
    if i=i2 then w2:=wartownik;
    wartownik:=wartownik^.wsk;
  end;
  //zamieniam wskaźniki ze sobą
  p:=w1^.body;
  w1^.body:=w2^.body;
  w2^.body:=p;
end;

procedure TPointerTab.Sort;
begin
  //sort_babelkowe(0,FCount-1);
  quicksort(0,FCount-1);
end;

end.
