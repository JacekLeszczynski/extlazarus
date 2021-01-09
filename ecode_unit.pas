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

end.

