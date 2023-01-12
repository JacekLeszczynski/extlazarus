unit ecode_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  _FF: string[1];
  textseparator: char = '"';

function GetLineToStr(const S: string; N: Integer; const Delims: Char; const wynik: string = ''): string;
function GetLineCount(aStr: string; separator: char): integer;
function GetLineCount(aStr: string; separator,forcetextseparator: char): integer;
function MyDir(Filename:string='';AddingExeExtension:boolean=false):string;
function IsCharWord(ch: char): boolean;
function IsCharHex(ch: char): boolean;
function DecodeHTMLAmp(str:string):string; cdecl; external 'libecode.so' name 'DecodeHTMLAmp';
function NormalizeB(aFormat: string; aWielkoscBajtowa: int64): string; cdecl; external 'libecode.so' name 'NormalizeB';
{ ----------------------- KOD CZASU ----------------------------- }
function SecToTime(aSec: longword): double; cdecl; external 'libecode.so' name 'SecToTime';
function SecToInteger(aSec: longword): integer; cdecl; external 'libecode.so' name 'SecToInteger';
function MiliSecToTime(aMiliSec: longword): double; cdecl; external 'libecode.so' name 'MiliSecToTime';
function MiliSecToInteger(aMiliSec: longword): longword; cdecl; external 'libecode.so' name 'MiliSecToInteger';
function TimeTruncate(Time: longword): longword; cdecl; external 'libecode.so' name 'TimeTruncateInt';
function TimeTruncate(Time: TDateTime): TDateTime; cdecl; external 'libecode.so' name 'TimeTruncateDT';
function TimeToInteger(Hour,Minutes,Second,Milisecond: word): longword; cdecl; external 'libecode.so' name 'DecodeTimeToInteger';
function TimeToInteger(Time: TDateTime): longword; cdecl; external 'libecode.so' name 'TimeToInteger';
function TimeToInteger: longword; cdecl; external 'libecode.so' name 'NowToInteger';
function IntegerToTime(czas: longword; no_milisecond: boolean = false): TDateTime; cdecl; external 'libecode.so' name 'IntegerToTime';
{ ----------------------- KOD 02 ------------------------------- }
function IsWhiteChar(c:char):boolean; cdecl; external 'libecode.so' name 'IsWhiteChar';
function IsDigit(c:char):boolean; cdecl; external 'libecode.so' name 'IsDigit';
function StrToD(s1:string; var s2:string; znak_dziesietny: char = '.'):double; cdecl; external 'libecode.so' name 'StrToD';
function StrToDCurr(s1:string; var s2:string; znak_dziesietny: char = '.'):double; cdecl; external 'libecode.so' name 'StrToDCurr';
function StrToL(s1:string; var s2:string; baza:integer):longint; cdecl; external 'libecode.so' name 'StrToL';
function AToI(s:string):integer; cdecl; external 'libecode.so' name 'AToI';
function GToS(d:double;l:integer):string; cdecl; external 'libecode.so' name 'GToS';
procedure QSort(adr:pointer;size_elementu:longint;adres:pointer;ile,ilosc_elementow:longint); cdecl; external 'libecode.so' name 'QSort';

implementation

var
  ppp: pchar;
  MyDirectory: string = '';

function GET_FF: char; cdecl; external 'libecode.so' name '_FF';
function fGetLineToStr(aStr: Pchar; l: integer; separator,textseparator: char; wynik: pchar; var wartosc: pchar): integer; cdecl; external 'libecode_c';
function fGetLineCount(aStr: Pchar; separator,textseparator: char): integer; cdecl; external 'libecode_c';

function GetLineToStr(const S: string; N: Integer; const Delims: Char;
  const wynik: string): string;
var
  len: SizeInt;
begin
  len:=fGetLineToStr(pchar(S),N,Delims,textseparator,pchar(wynik),&ppp);
  SetString(result,ppp,len);
end;

function GetLineCount(aStr: string; separator: char): integer;
begin
  result:=fGetLineCount(pchar(aStr),separator,textseparator);
end;

function GetLineCount(aStr: string; separator, forcetextseparator: char
  ): integer;
begin
  result:=fGetLineCount(pchar(aStr),separator,forcetextseparator);
end;

function MyDir(Filename: string; AddingExeExtension: boolean): string;
var
  s,s2: string;
  l: integer;
begin
  if MyDirectory='' then
  begin
    if pos('!',Filename)=1 then
    begin
      s:=ExtractFilePath(ParamStr(0));
      delete(s,1,1);
    end else s:=GetCurrentDir;
    l:=length(s);
    if s[l]=_FF then delete(s,l,1);
  end else s:=MyDirectory;
  {$IFDEF WINDOWS}
  if AddingExeExtension then s2:='.exe' else s2:='';
  if Filename<>'' then s:=StringReplace(s+'\'+Filename+s2,'/','\',[rfReplaceAll]);
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

initialization
  ppp:=nil;
  _FF:=GET_FF;
finalization
  if ppp<>nil then StrDispose(ppp);
end.

