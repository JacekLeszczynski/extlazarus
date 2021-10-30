unit ecode_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GetLineToStr(s:string;l:integer;separator:char;wynik:string=''):string;
function GetLineCount(s: string; separator:char):integer;
function GetLineCount(s: string; separator,textseparator: char): integer;
function ConvOdczyt(s: string): string;
function ConvZapis(s: string): string;
function MyDir(Filename:string):string;
function IsCharWord(ch: char): boolean;
function IsCharHex(ch: char): boolean;
{ ----------------------- KOD CZASU ----------------------------- }
function SecToTime(aSec: longword): double;
function SecToInteger(aSec: longword): integer;
function MiliSecToTime(aMiliSec: longword): double;
function MiliSecToInteger(aMiliSec: longword): longword;
function TimeTruncate(Time: longword): longword;
function TimeTruncate(Time: TDateTime): TDateTime;
function TimeToInteger(Hour,Minutes,Second,Milisecond: word): longword;
function TimeToInteger(Time: TDateTime): longword;
function TimeToInteger: longword;
function IntegerToTime(czas: longword; no_milisecond: boolean = false): TDateTime;
{ ----------------------- KOD 02 ------------------------------- }
function IsWhiteChar(c:char):boolean;
function IsDigit(c:char):boolean;
function StrToD(s1:string; var s2:string; znak_dziesietny: char = '.'):double;
function StrToDCurr(s1:string; var s2:string; znak_dziesietny: char = '.'):double;
function StrToL(s1:string; var s2:string; baza:integer):longint;
function AToI(s:string):integer;
function GToS(d:double;l:integer):string;
procedure QSort(adr:pointer;size_elementu:longint;adres:pointer;ile,ilosc_elementow:longint);

implementation

const
  textseparator = '"';

//Funkcja zwraca n-ty (l) ciag stringu (s), o wskazanym separatorze.
function GetLineToStr(s:string;l:integer;separator:char;wynik:string=''):string;
var
  i,ll,dl: integer;
  b: boolean;
begin
  b:=false;
  dl:=length(s);
  ll:=1;
  s:=s+separator;
  for i:=1 to length(s) do
  begin
    if s[i]=textseparator then b:=not b;
    if (not b) and (s[i]=separator) then inc(ll);
    if ll=l then break;
  end;
  if ll=1 then dec(i);
  delete(s,1,i);
  b:=false;
  for i:=1 to length(s) do
  begin
    if s[i]=textseparator then b:=not b;
    if (not b) and (s[i]=separator) then break;
  end;
  delete(s,i,dl);
  if (s<>'') and (s[1]=textseparator) then
  begin
    delete(s,1,1);
    delete(s,length(s),1);
  end;
  if s='' then s:=wynik;
  result:=s;
end;

function GetLineCount(s: string; separator:char):integer;
var
  ll,i,ost: integer;
begin
  //liczę separatory by zdiagnozować maksymalną ilość sekcji
  ll:=1;
  for i:=1 to length(s) do if s[i]=separator then inc(ll);
  //szukam ostatniej nie białej zawartości sekcji
  ost:=0;
  for i:=ll downto 1 do if GetLineToStr(s,i,separator,textseparator)<>'' then
  begin
    ost:=i;
    break;
  end;
  //wyjście
  result:=ost;
end;

function GetLineCount(s: string; separator,textseparator: char): integer;
var
  ll,i,ost: integer;
begin
  //licze separatory by zdiagnozowac maksymalna ilosc sekcji
  ll:=1;
  for i:=1 to length(s) do if s[i]=separator then inc(ll);
  //szukam ostatniej nie bialej zawartosci sekcji
  ost:=0;
  for i:=ll downto 1 do if GetLineToStr(s,i,separator,textseparator)<>'' then
  begin
    ost:=i;
    break;
  end;
  //wyjscie
  result:=ost;
end;

function ConvOdczyt(s: string): string;
var
  pom: string;
begin
  {$IFDEF LAZARUS}
  pom:=ConvertEncoding(s,'cp1250','utf8');
  {$ELSE}
  pom:=UTF8Encode(s);
  {$ENDIF}
  result:=pom;
end;

function ConvZapis(s: string): string;
var
  pom: string;
begin
  {$IFDEF LAZARUS}
  pom:=ConvertEncoding(s,'utf8','cp1250');
  {$ELSE}
  pom:=UTF8Decode(s);
  {$ENDIF}
  result:=pom;
end;

function MyDir(Filename:string):string;
var
  s: string;
begin
  s:=ExtractFilePath(ParamStr(0));
  delete(s,length(s),1);
  {$IFDEF WINDOWS}
  if Filename<>'' then s:=StringReplace(s+'\'+Filename,'/','\',[rfReplaceAll]);
  {$ELSE}
  if Filename<>'' then s:=StringReplace(s+'/'+Filename,'\','/',[rfReplaceAll]);
  {$ENDIF}
  result:=s;
end;

function IsCharWord(ch: char): boolean;
begin
  Result:= ch in ['a'..'z', 'A'..'Z', '_', '0'..'9'];
end;

function IsCharHex(ch: char): boolean;
begin
  Result:= ch in ['0'..'9', 'a'..'f', 'A'..'F'];
end;

function SecToTime(aSec: longword): double;
begin
  result:=aSec/SecsPerDay;
end;

function SecToInteger(aSec: longword): integer;
begin
  result:=TimeToInteger(aSec/SecsPerDay);
end;

function MiliSecToTime(aMiliSec: longword): double;
begin
  result:=aMiliSec/1000/SecsPerDay;
end;

function MiliSecToInteger(aMiliSec: longword): longword;
begin
  result:=TimeToInteger(aMiliSec/1000/SecsPerDay);
end;

function TimeTruncate(Time: longword): longword;
begin
  result:=TimeToInteger(IntegerToTime(Time,true));
end;

function TimeTruncate(Time: TDateTime): TDateTime;
begin
  result:=IntegerToTime(TimeToInteger(Time),true);
end;

function TimeToInteger(Hour, Minutes, Second, Milisecond: word): longword;
begin
  result:=(Hour*60*60*1000)+(Minutes*60*1000)+(Second*1000)+Milisecond;
end;

function TimeToInteger(Time: TDateTime): longword;
var
  godz,min,sec,milisec: word;
begin
  DecodeTime(Time,godz,min,sec,milisec);
  result:=(godz*60*60*1000)+(min*60*1000)+(sec*1000)+milisec;
end;

function TimeToInteger: longword;
begin
  result:=TimeToInteger(time);
end;

function IntegerToTime(czas: longword; no_milisecond: boolean): TDateTime;
var
  c: longword;
  godz,min,sec,milisec: word;
begin
  c:=czas;
  godz:=c div 3600000;
  c:=c-(godz*3600000);
  min:=c div 60000;
  c:=c-(min*60000);
  sec:=c div 1000;
  if no_milisecond then milisec:=0 else milisec:=c-(sec*1000);
  result:=EncodeTime(godz,min,sec,milisec);
end;

{ ----------------------- KOD 02 ------------------------------- }

//Sprawdź, czy dany znak należy do takzwanych znaków białych
function IsWhiteChar(c:char):boolean;
begin
  case c of
    ' ',#9: result:=true;
    else result:=false;
  end;
end;

//Sprawdź, czy dany znak jest cyfrą
function IsDigit(c:char):boolean;
begin
  if (c>='0') and (c<='9') then result:=true else result:=false;
end;

//Konwersja stringu do liczby rzeczywistej
//1. Pierwsze znaki biale sa pomijane
//2. Liczba jest konwertowana
//3. Reszta jest wrzucana do s2
function StrToD(s1:string; var s2:string; znak_dziesietny: char = '.'):double;
var
  p,pom: string;
  i,ii,error: integer;
  d: double;
  zm_kropka,plus,minus: boolean;
  c: char;
  _TFS: TFormatSettings;
begin
  error:=0;
  _TFS.DecimalSeparator:=znak_dziesietny;
  p:=s1;
  pom:='';
  zm_kropka:=false;
  plus:=false;
  minus:=false;
  //wywalam wszystkie początkowe znaki białe
  while (length(p)>0) and IsWhiteChar(p[1]) do delete(p,1,1);
  //jeśli string jest pusty
  if p='' then
  begin
    pom:='';
    result:=0;
    exit;
  end;
  //szukam miejsca błędu
  for i:=1 to length(p) do
  begin
    if (p[i]='+') and (not plus) then
    begin
      plus:=true;
      continue;
    end;
    if (p[i]='-') and (not minus) then
    begin
      minus:=true;
      continue;
    end;
    if (p[i]=znak_dziesietny) and (not zm_kropka) then
    begin
      zm_kropka:=true;
      continue;
    end;
    if (not isdigit(p[i])) and (p[i]<>'e') and (p[i]<>'E') then
    begin
      error:=i;
      break;
    end;
  end;
  //przesuwam błąd
  if error<>0 then
  begin
    pom:=p;
    delete(p,error,1024);
    delete(pom,1,error-1);
  end;
  //błąd e
  if p='' then c:=' ' else c:=p[length(p)];
  if (c='e') or (c='E') then
  begin
    delete(p,length(p),1);
    pom:=c+pom;
  end;
  //próbuję zamienić liczbę
  if p<>'' then
  begin
    try
      d:=StrToFloat(p,_TFS);
    except
      ii:=pos(znak_dziesietny,p);
      delete(p,ii,1000);
      d:=StrToFloat(p,_TFS);
    end;
  end else d:=0;
  //oddaję wyniki
  s2:=pom;
  result:=d;
end;

function StrToDCurr(s1: string; var s2: string; znak_dziesietny: char): double;
var
  p,pom: string;
  i,ii,error: integer;
  d: double;
  zm_kropka,plus,minus: boolean;
  c: char;
  _TFS: TFormatSettings;
begin
  error:=0;
  _TFS.DecimalSeparator:=znak_dziesietny;
  p:=s1;
  pom:='';
  zm_kropka:=false;
  plus:=false;
  minus:=false;
  //wywalam wszystkie początkowe znaki białe
  while (length(p)>0) and IsWhiteChar(p[1]) do delete(p,1,1);
  //jeśli string jest pusty
  if p='' then
  begin
    pom:='';
    result:=0;
    exit;
  end;
  //szukam miejsca błędu
  for i:=1 to length(p) do
  begin
    if (p[i]='+') and (not plus) then
    begin
      plus:=true;
      continue;
    end;
    if (p[i]='-') and (not minus) then
    begin
      minus:=true;
      continue;
    end;
    if (p[i]=znak_dziesietny) and (not zm_kropka) then
    begin
      zm_kropka:=true;
      continue;
    end;
    if (not isdigit(p[i])) and (p[i]<>'e') and (p[i]<>'E') then
    begin
      error:=i;
      break;
    end;
  end;
  //przesuwam błąd
  if error<>0 then
  begin
    pom:=p;
    delete(p,error,1024);
    delete(pom,1,error-1);
  end;
  //błąd e
  if p='' then c:=' ' else c:=p[length(p)];
  if (c='e') or (c='E') then
  begin
    delete(p,length(p),1);
    pom:=c+pom;
  end;
  //próbuję zamienić liczbę
  if p<>'' then
  begin
    try
      d:=StrToCurr(p,_TFS);
    except
      ii:=pos(znak_dziesietny,p);
      delete(p,ii,1000);
      d:=StrToCurr(p,_TFS);
    end;
  end else d:=0;
  //oddaję wyniki
  s2:=pom;
  result:=d;
end;

//Konwersja stringu do liczby calkowitej
//1. Pierwsze znaki biale sa pomijane
//2. Liczba jest konwertowana
//3. Reszta jest wrzucana do s2
//gdzie: Baza jest polem informującym system liczbowy
//w tej chwili jest to pole ignorowane i zawsze przyjmowany jest
//system dziesiętny (wartosci 0 i 10)
function StrToL(s1:string; var s2:string; baza:integer):longint;
var
  p,pom: string;
  i,error: integer;
  d: longint;
  plus,minus: boolean;
begin
  error:=0;
  p:=s1;
  pom:='';
  plus:=false;
  minus:=false;
  //wywalam wszystkie początkowe znaki białe
  while (length(p)>0) and IsWhiteChar(p[1]) do delete(p,1,1);
  //jeśli string jest pusty
  if p='' then
  begin
    pom:='';
    result:=0;
    exit;
  end;
  //szukam miejsca błędu
  for i:=1 to length(p) do
  begin
    if (p[i]='+') and (not plus) then
    begin
      plus:=true;
      continue;
    end;
    if (p[i]='-') and (not minus) then
    begin
      minus:=true;
      continue;
    end;
    if not isdigit(p[i]) then
    begin
      error:=i;
      break;
    end;
  end;
  //przesuwam błąd
  if error<>0 then
  begin
    pom:=p;
    delete(p,error,1024);
    delete(pom,1,error-1);
  end;
  //próbuję zamienić liczbę
  if p<>'' then d:=StrToInt(p) else d:=0;
  //oddaję wyniki
  s2:=pom;
  result:=d;
end;

//Konwersja stringu do integer
//Funkcja kompatybilnosci z C++
function AToI(s:string):integer;
begin
  result:=StrToInt(s);
end;

//Konwersja liczby rzeczywistej do stringu
//Funkcja kompatybilnosci z C++
//dodatkowo funkcja zostala przystosowana do realiów języka Pascal
function GToS(d:double;l:integer):string;
var
  i: integer;
  s: string;
begin
  if l=0 then s:='0' else s:='0.';
  for i:=1 to l do s:=s+'#';
  result:=FormatFloat(s,d);
end;

//Bardzo wyjatkowa funkcja, w C jest to tak zwany automat do sortowania
//w pamieci, jest ona troszke bardziej rozbudowana niż funkcja poniżej.
//W oryginale uzytkownik sam buduje mechanizm sortowania, zas tu -
//Wszystko jest zautomatyzowane! Na razie wykorzystywane jest najprostsze
//sortowanie "bąbelkowe", ale z czasem to poprawię i zastosuję szybszy algorytm.
//Dziala to bardzo prosto. Podajemy w argumentach dane na temat bloku pamięci,
//oraz dane częsci bloku wg którego ma by wszystko sortowane.
//Wiecej informacji w helpie.
procedure QSort(adr:pointer;size_elementu:longint;adres:pointer;ile,ilosc_elementow:longint);
var
  ii,i,j,k,l: integer;
  p1,p2,p11,p22: ^byte;
  b: byte;
  q: integer;
begin
  for ii:=1 to ilosc_elementow {*ilosc_elementow} do for i:=0 to ilosc_elementow-2 do
  begin
    j:=i+1;
    p1:=adres;
    inc(p1,i*size_elementu);
    p2:=adres;
    inc(p2,j*size_elementu);
    for k:=ile-1 downto 0 do
    begin
      { przeprowadzam porównanie }
      p11:=p1; inc(p11,k);
      p22:=p2; inc(p22,k);
      q:=0;
      if p11^<p22^ then q:=1;
      if p11^>p22^ then q:=2;
      if q=0 then continue;
      if q=1 then break;
      { zamieniam strony }
      for l:=0 to size_elementu-1 do
      begin
        p11:=adr; inc(p11,i*size_elementu+l);
        p22:=adr; inc(p22,(i+1)*size_elementu+l);
        b:=p11^;
        p11^:=p22^;
        p22^:=b;
      end;
      break;
    end;
  end;
end;

end.

