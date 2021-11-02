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
function DecodeHTMLAmp(str: string): string;
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

function DecodeHTMLAmp(str: string): string;
var
  i: integer;
  s: string;
begin
  s:=str;
  s:=StringReplace(s,'&quot;','"',[rfReplaceAll]);
  s:=StringReplace(s,'&amp;amp;','&',[rfReplaceAll]);
  s:=StringReplace(s,'&amp;','&',[rfReplaceAll]);
  s:=StringReplace(s,'&#260;','Ą',[rfReplaceAll]);
  s:=StringReplace(s,'&#261;','ą',[rfReplaceAll]);
  s:=StringReplace(s,'&#280;','Ę',[rfReplaceAll]);
  s:=StringReplace(s,'&#281;','ę',[rfReplaceAll]);
  s:=StringReplace(s,'&#211;','Ó',[rfReplaceAll]);
  s:=StringReplace(s,'&#243;','ó',[rfReplaceAll]);
  s:=StringReplace(s,'&oacute;','ó',[rfReplaceAll]);
  s:=StringReplace(s,'&#262;','Ć',[rfReplaceAll]);
  s:=StringReplace(s,'&#263;','ć',[rfReplaceAll]);
  s:=StringReplace(s,'&#321;','Ł',[rfReplaceAll]);
  s:=StringReplace(s,'&#322;','ł',[rfReplaceAll]);
  s:=StringReplace(s,'&#323;','Ń',[rfReplaceAll]);
  s:=StringReplace(s,'&#324;','ń',[rfReplaceAll]);
  s:=StringReplace(s,'&#346;','Ś',[rfReplaceAll]);
  s:=StringReplace(s,'&#347;','ś',[rfReplaceAll]);
  s:=StringReplace(s,'&#377;','Ź',[rfReplaceAll]);
  s:=StringReplace(s,'&#378;','ź',[rfReplaceAll]);
  s:=StringReplace(s,'&#379;','Ż',[rfReplaceAll]);
  s:=StringReplace(s,'&#380;','ż',[rfReplaceAll]);
  s:=StringReplace(s,'&laquo;','«',[rfReplaceAll]);
  s:=StringReplace(s,'&raquo;','»',[rfReplaceAll]);
  s:=StringReplace(s,'&rsquo;','’',[rfReplaceAll]);
  s:=StringReplace(s,'&#x2019;','’',[rfReplaceAll]);
  s:=StringReplace(s,'&#8217;','’',[rfReplaceAll]);
  s:=StringReplace(s,'&apos;','''',[rfReplaceAll]);
  s:=StringReplace(s,'&#x27;','''',[rfReplaceAll]);
  s:=StringReplace(s,'&#39;','''',[rfReplaceAll]);
  s:=StringReplace(s,'&ntilde;','ñ',[rfReplaceAll]);
  s:=StringReplace(s,'&nbsp;',' ',[rfReplaceAll]);
  s:=StringReplace(s,'&Agrave;','À',[rfReplaceAll]);
  s:=StringReplace(s,'&Aacute;','Á',[rfReplaceAll]);
  s:=StringReplace(s,'&Acirc;','Â',[rfReplaceAll]);
  s:=StringReplace(s,'&Atilde;','Ã',[rfReplaceAll]);
  s:=StringReplace(s,'&Auml;','Ä',[rfReplaceAll]);
  s:=StringReplace(s,'&Aring;','Å',[rfReplaceAll]);
  s:=StringReplace(s,'&agrave;','à',[rfReplaceAll]);
  s:=StringReplace(s,'&aacute;','á',[rfReplaceAll]);
  s:=StringReplace(s,'&acirc;','â',[rfReplaceAll]);
  s:=StringReplace(s,'&atilde;','ã',[rfReplaceAll]);
  s:=StringReplace(s,'&auml;','ä',[rfReplaceAll]);
  s:=StringReplace(s,'&aring;','å',[rfReplaceAll]);
  s:=StringReplace(s,'&AElig;','Æ',[rfReplaceAll]);
  s:=StringReplace(s,'&aelig;','æ',[rfReplaceAll]);
  s:=StringReplace(s,'&szlig;','ß',[rfReplaceAll]);
  s:=StringReplace(s,'&Ccedil;','Ç',[rfReplaceAll]);
  s:=StringReplace(s,'&ccedil;','ç',[rfReplaceAll]);
  s:=StringReplace(s,'&Egrave;','È',[rfReplaceAll]);
  s:=StringReplace(s,'&Eacute;','É',[rfReplaceAll]);
  s:=StringReplace(s,'&Ecirc;','Ê',[rfReplaceAll]);
  s:=StringReplace(s,'&Euml;','Ë',[rfReplaceAll]);
  s:=StringReplace(s,'&egrave;','è',[rfReplaceAll]);
  s:=StringReplace(s,'&eacute;','é',[rfReplaceAll]);
  s:=StringReplace(s,'&ecirc;','ê',[rfReplaceAll]);
  s:=StringReplace(s,'&euml;','ë',[rfReplaceAll]);
  s:=StringReplace(s,'&#131;','ƒ',[rfReplaceAll]);
  s:=StringReplace(s,'&Igrave;','Ì',[rfReplaceAll]);
  s:=StringReplace(s,'&Iacute;','Í',[rfReplaceAll]);
  s:=StringReplace(s,'&Icirc;','Î',[rfReplaceAll]);
  s:=StringReplace(s,'&Iuml;','Ï',[rfReplaceAll]);
  s:=StringReplace(s,'&igrave;','ì',[rfReplaceAll]);
  s:=StringReplace(s,'&iacute;','í',[rfReplaceAll]);
  s:=StringReplace(s,'&icirc;','î',[rfReplaceAll]);
  s:=StringReplace(s,'&iuml;','ï',[rfReplaceAll]);
  s:=StringReplace(s,'&Ntilde;','Ñ',[rfReplaceAll]);
  s:=StringReplace(s,'&ntilde;','ñ',[rfReplaceAll]);
  s:=StringReplace(s,'&Ograve;','Ò',[rfReplaceAll]);
  s:=StringReplace(s,'&Oacute;','Ó',[rfReplaceAll]);
  s:=StringReplace(s,'&Ocirc;','Ô',[rfReplaceAll]);
  s:=StringReplace(s,'&Otilde;','Õ',[rfReplaceAll]);
  s:=StringReplace(s,'&Ouml;','Ö',[rfReplaceAll]);
  s:=StringReplace(s,'&ograve;','ò',[rfReplaceAll]);
  s:=StringReplace(s,'&oacute;','ó',[rfReplaceAll]);
  s:=StringReplace(s,'&ocirc;','ô',[rfReplaceAll]);
  s:=StringReplace(s,'&otilde;','õ',[rfReplaceAll]);
  s:=StringReplace(s,'&ouml;','ö',[rfReplaceAll]);
  s:=StringReplace(s,'&Oslash;','Ø',[rfReplaceAll]);
  s:=StringReplace(s,'&oslash;','ø',[rfReplaceAll]);
  s:=StringReplace(s,'&#140;','Œ',[rfReplaceAll]);
  s:=StringReplace(s,'&#156;','œ',[rfReplaceAll]);
  s:=StringReplace(s,'&#138;','Š',[rfReplaceAll]);
  s:=StringReplace(s,'&#154;','š',[rfReplaceAll]);
  s:=StringReplace(s,'&Ugrave;','Ù',[rfReplaceAll]);
  s:=StringReplace(s,'&Uacute;','Ú',[rfReplaceAll]);
  s:=StringReplace(s,'&Ucirc;','Û',[rfReplaceAll]);
  s:=StringReplace(s,'&Uuml;','Ü',[rfReplaceAll]);
  s:=StringReplace(s,'&ugrave;','ù',[rfReplaceAll]);
  s:=StringReplace(s,'&uacute;','ú',[rfReplaceAll]);
  s:=StringReplace(s,'&ucirc;','û',[rfReplaceAll]);
  s:=StringReplace(s,'&uuml;','ü',[rfReplaceAll]);
  s:=StringReplace(s,'&#181;','µ',[rfReplaceAll]);
  s:=StringReplace(s,'&#215;','×',[rfReplaceAll]);
  s:=StringReplace(s,'&Yacute;','Ý',[rfReplaceAll]);
  s:=StringReplace(s,'&#159;','Ÿ',[rfReplaceAll]);
  s:=StringReplace(s,'&yacute;','ý',[rfReplaceAll]);
  s:=StringReplace(s,'&yuml;','ÿ',[rfReplaceAll]);
  s:=StringReplace(s,'&#176;','°',[rfReplaceAll]);
  s:=StringReplace(s,'&#134;','†',[rfReplaceAll]);
  s:=StringReplace(s,'&#135;','‡',[rfReplaceAll]);
  s:=StringReplace(s,'&lt;','<',[rfReplaceAll]);
  s:=StringReplace(s,'&gt;','>',[rfReplaceAll]);
  s:=StringReplace(s,'&#177;','±',[rfReplaceAll]);
  s:=StringReplace(s,'&#171;','«',[rfReplaceAll]);
  s:=StringReplace(s,'&#187;','»',[rfReplaceAll]);
  s:=StringReplace(s,'&#191;','¿',[rfReplaceAll]);
  s:=StringReplace(s,'&#161;','¡',[rfReplaceAll]);
  s:=StringReplace(s,'&#183;','·',[rfReplaceAll]);
  s:=StringReplace(s,'&#149;','•',[rfReplaceAll]);
  s:=StringReplace(s,'&#153;','™',[rfReplaceAll]);
  s:=StringReplace(s,'&copy;','©',[rfReplaceAll]);
  s:=StringReplace(s,'&reg;','®',[rfReplaceAll]);
  s:=StringReplace(s,'&#167;','§',[rfReplaceAll]);
  s:=StringReplace(s,'&#182;','¶',[rfReplaceAll]);
  s:=StringReplace(s,'&ndash;','á',[rfReplaceAll]);
  s:=StringReplace(s,'&ldquo;','“',[rfReplaceAll]);
  s:=StringReplace(s,'&rdquo;','”',[rfReplaceAll]);
  s:=StringReplace(s,'&bdquo;','„',[rfReplaceAll]);
  s:=StringReplace(s,'&iquest;','¿',[rfReplaceAll]);
  s:=StringReplace(s,'&iexcl;','¡',[rfReplaceAll]);
  s:=StringReplace(s,'&sup2;','²',[rfReplaceAll]);
  s:=StringReplace(s,'&Scaron;','Š',[rfReplaceAll]);
  s:=StringReplace(s,'&scaron;','š',[rfReplaceAll]);
  s:=StringReplace(s,'&deg;','°',[rfReplaceAll]);
  s:=StringReplace(s,'&eth;','ð',[rfReplaceAll]);
  s:=StringReplace(s,'&middot;','·',[rfReplaceAll]);
  s:=StringReplace(s,'&ordm;','º',[rfReplaceAll]);
  s:=StringReplace(s,'&THORN;','Þ',[rfReplaceAll]);
  s:=StringReplace(s,'&thorn;','þ',[rfReplaceAll]);
  s:=StringReplace(s,'&Delta;','Δ',[rfReplaceAll]);
  s:=StringReplace(s,'&delta;','δ',[rfReplaceAll]);
  s:=StringReplace(s,'&lsquo;','‘',[rfReplaceAll]);
  s:=StringReplace(s,'&rsquo;','’',[rfReplaceAll]);
  s:=StringReplace(s,'&times;','×',[rfReplaceAll]);
  s:=StringReplace(s,'&frac12;','½',[rfReplaceAll]);
  s:=StringReplace(s,'&hellip;','…',[rfReplaceAll]);
  s:=StringReplace(s,'&mdash;','—',[rfReplaceAll]);
  s:=StringReplace(s,'&ndash;','–',[rfReplaceAll]);
  s:=StringReplace(s,'&hearts;','♥',[rfReplaceAll]);
  s:=StringReplace(s,'&OElig;','Œ',[rfReplaceAll]);
  s:=StringReplace(s,'&oelig;','œ',[rfReplaceAll]);
  result:=s;
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

