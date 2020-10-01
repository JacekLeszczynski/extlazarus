unit ExtDiff;

(*
Moduł przepisany z wersji pod Delphi, przepisałem ja Jacek L.
Niżej pod moim komentarzem dane autora tego modułu.

A tu historia moich zmian:
  22.09.2020 - przepisanie kodu do modułu pod Lazarusa i testy pod linuksem,
               i optymalizacja kodu.
  26.09.2020 - dodanie metod tworzących łatki kodu (pliki i katalogi).
*)

(*******************************************************************************
* Component         TDiff                                                      *
* Version:          3.1                                                        *
* Date:             7 November 2009                                            *
* Compilers:        Delphi 7 - Delphi2009                                      *
* Author:           Angus Johnson - angusj-AT-myrealbox-DOT-com                *
* Copyright:        © 2001-200( Angus Johnson                                  *
*                                                                              *
* Licence to use, terms and conditions:                                        *
*                   The code in the TDiff component is released as freeware    *
*                   provided you agree to the following terms & conditions:    *
*                   1. the copyright notice, terms and conditions are          *
*                   left unchanged                                             *
*                   2. modifications to the code by other authors must be      *
*                   clearly documented and accompanied by the modifier's name. *
*                   3. the TDiff component may be freely compiled into binary  *
*                   format and no acknowledgement is required. However, a      *
*                   discrete acknowledgement would be appreciated (eg. in a    *
*                   program's 'About Box').                                    *
*                                                                              *
* Description:      Component to list differences between two integer arrays   *
*                   using a "longest common subsequence" algorithm.            *
*                   Typically, this component is used to diff 2 text files     *
*                   once their individuals lines have been hashed.             *
*                                                                              *
* Acknowledgements: The key algorithm in this component is based on:           *
*                   "An O(ND) Difference Algorithm and its Variations"         *
*                   By E Myers - Algorithmica Vol. 1 No. 2, 1986, pp. 251-266  *
*                   http://www.cs.arizona.edu/people/gene/                     *
*                   http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps       *
*                                                                              *
*******************************************************************************)


(*******************************************************************************
* History:                                                                     *
* 13 December 2001 - Original Release                                          *
* 22 April 2008    - Complete rewrite to greatly improve the code and          *
*                    provide a much simpler view of differences through a new  *
*                    'Compares' property.                                      *
* 7 November 2009  - Updated so now compiles in newer versions of Delphi.      *
*******************************************************************************)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

const
  //Maximum realistic deviation from centre diagonal vector ...
  MAX_DIAGONAL = $FFFFFF; //~16 million

type
  {$IFDEF UNICODE}
    P8Bits = PByte;
  {$ELSE}
    P8Bits = PAnsiChar;
  {$ENDIF}

  PINT = PInteger;

  PDiags = ^TDiags;
  TDiags = array [-MAX_DIAGONAL .. MAX_DIAGONAL] of int64;

  PIntArray = ^TIntArray;
  TIntArray = array[0 .. MAXINT div sizeof(int64) -1] of int64;
  PChrArray = ^TChrArray;
  TChrArray = array[0 .. MAXINT div sizeof(char) -1] of Char;

  TChangeKind = (ckNone, ckAdd, ckDelete, ckModify);

  PCompareRec = ^TCompareRec;
  TCompareRec = record
    Kind      : TChangeKind;
    oldIndex1,
    oldIndex2 : integer;
    case boolean of
      false   : (chr1, chr2 : Char);
      true    : (int1, int2 : integer);
  end;

  PDiffVars = ^TDiffVars;
  TDiffVars = record
    offset1 : integer;
    offset2 : integer;
    len1    : integer;
    len2    : integer;
  end;

  TDiffStats = record
    matches  : integer;
    adds     : integer;
    deletes  : integer;
    modifies : integer;
  end;

  { TFilesList }

  TFilesList = class
  private
    FFiles: TList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Add(aPatchToFile: string; aDateTime, aMiliSeconds: qword): integer;
    procedure Delete(aIndex: integer);
    procedure Get(aIndex: integer; var aPatchToFile: string; var aDateTime, aMiliSeconds: qword);
  published
  end;

  { TExtDiff }

  TDiffAlgorithm = (daDifference,daSeqComparison);
  TDiffFileEngine = (fePascal,feMemory);
  TDiffRequestFileEvent = procedure(Sender: TObject; aVersion: integer; aFileName: string) of object;
  TDiffRequestFileAttributesEvent = procedure(Sender: TObject; aVersion: integer; aFileName: string; var aFileDate: qword; var aMilisecond: qword) of object;
  TDiffRequestFileBinaryEvent = procedure(Sender: TObject; aVersion: integer; aFileName: string; var aIsBinary: boolean) of object;
  TDiffRequestFileBodyEvent = procedure(Sender: TObject; aVersion: integer; aFileName: string; aBody: TStrings) of object;
  TDiffRequestDiffBinFilesEvent = procedure(Sender: TObject; aBinFile1,aBinFile2: string; var aDifferend: boolean) of object;

  TExtDiff = class(TComponent)
  private
    FAlg: TDiffAlgorithm;
    FBinDiff: TDiffRequestFileEvent;
    FBinPatch: TDiffRequestFileEvent;
    fDiffList: TList;      //this TList circumvents the need for recursion
    fCompareInts: boolean; //ie are we comparing integer arrays or char arrays
    fCancelled: boolean;
    fDiffStats: TDiffStats;
    fCompareList: TList;
    fExecuting: boolean;
    fDiagBuffer, bDiagBuffer: pointer;
    Chrs1, Chrs2: PChrArray;
    FFileEngine: TDiffFileEngine;
    FOnProcSys: TNotifyEvent;
    FRequestDeleteFile: TDiffRequestFileEvent;
    FRequestDiffBinFiles: TDiffRequestDiffBinFilesEvent;
    FRequestFileAttrib: TDiffRequestFileAttributesEvent;
    FRequestFileIsBin: TDiffRequestFileBinaryEvent;
    FRequestFileBody: TDiffRequestFileBodyEvent;
    FRequestSaveFile: TDiffRequestFileBodyEvent;
    Ints1, Ints2: PIntArray;
    LastCompareRec: TCompareRec;
    fDiag, bDiag: PDiags;
    procedure PushDiff(offset1, offset2, len1, len2: integer);
    function  PopDiff: boolean;
    procedure DiffInt(offset1, offset2, len1, len2: integer);
    procedure DiffChr(offset1, offset2, len1, len2: integer);
    function SnakeChrF(k,offset1,offset2,len1,len2: integer): boolean;
    function SnakeChrB(k,offset1,offset2,len1,len2: integer): boolean;
    function SnakeIntF(k,offset1,offset2,len1,len2: integer): boolean;
    function SnakeIntB(k,offset1,offset2,len1,len2: integer): boolean;
    procedure AddChangeChr(offset1, range: integer; ChangeKind: TChangeKind);
    procedure AddChangeInt(offset1, range: integer; ChangeKind: TChangeKind);
    procedure InitDiagArrays(MaxOscill, len1, len2: integer);
    procedure InitDiagArrays(len1, len2: integer);
    //nb: To optimize speed, separate functions are called for either
    //integer or character compares ...
    procedure RecursiveDiffChr(offset1, offset2, len1, len2: integer);
    procedure AddChangeChrs(offset1, range: integer; ChangeKind: TChangeKind);
    procedure RecursiveDiffInt(offset1, offset2, len1, len2: integer);
    procedure AddChangeInts(offset1, range: integer; ChangeKind: TChangeKind);
    function GetCompare(index: integer): TCompareRec;
    function GetCompareCount: integer;
    function _ile_pozycji_o_tej_samej_fladze(aIndex: integer): integer;
    procedure dane_pliku(aStr: string; var aFileName: string; var aDT: TDateTime);
    procedure dane_pliku(aStr: string; var aStart,aCount,bStart,bCount: integer);

    function DiffFileInfo(aFlaga: char; aFileName: string; aDT,aMS: qword): string;
    function DiffFileInfo(aFlaga: char; aFileName: string; aDateTime: TDateTime): string;
    function DiffAnalize(pp: TList; aDiff: TStrings): integer;
    function DiffSzukajPierwszejZmiany(aOd: integer; pp: TList): integer;
    procedure DiffNewFile(aFileNotFound,aFile: string; aDiff: TStrings; aExecuteRequest: boolean = false);
    procedure DiffFileDeleted(aFile,aFileDeleted: string; aDiff: TStrings; aExecuteRequest: boolean = false);

  protected
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function GetHashFile(const sFile: TFileName): string;
    function GetHashFile(aBuffer: TMemoryStream): string;
    function IsTextFile(const sFile: TFileName; const aForceAll: boolean = false): boolean;
    function IsTextFile(aMemoryStream: TMemoryStream; const aForceAll: boolean = false): boolean;
    function IsBinaryFile(const sFile: TFileName; const aForceAll: boolean = false): boolean;
    function IsBinaryFile(aMemoryStream: TMemoryStream; const aForceAll: boolean = false): boolean;
    function NormalizeFilePath(aPatchFile: string; aForceLinux: boolean = false): string;
    //compare either and array of characters or an array of integers ...
    function Execute(pints1, pints2: PINT; len1, len2: integer): boolean;
    function Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean;
    //Cancel allows interrupting excessively prolonged comparisons
    procedure Cancel;
    procedure Clear;
    procedure FileToHashList(aFile: string; aHashList: TList; aTrimSpace: boolean = false; aIgnoreSpace: boolean = false; aIgnoreCase: boolean = false);
    procedure StringsToHashList(aStr: TStrings; aHashList: TList; aTrimSpace: boolean = false; aIgnoreSpace: boolean = false; aIgnoreCase: boolean = false);
    procedure StringsToHashList(aStr: TStringList; aHashList: TList; aTrimSpace: boolean = false; aIgnoreSpace: boolean = false; aIgnoreCase: boolean = false);
    function HashLine(const aLine: string; aTrimSpace: boolean = false; aIgnoreSpace: boolean = false; aIgnoreCase: boolean = false): pointer;
    function Diff(aFile1, aFile2: TStrings; aFName1, aFName2: string; aDTime1, aMSec1, aDTime2, aMSec2: qword; aDiff: TStrings): integer;
    function Diff(aFileOld,aFileNew: string; aDiff: TStrings): integer;
    procedure DiffDirectory(aDir1,aDir2: TStrings; aDiff: TStrings);
    procedure DiffDirectory(aDirOld,aDirNew: string; aDiff: TStrings);
    procedure Patch(aFileDiff,aFile: TStrings);
    procedure Patch(aFileDiff,aFile: string);
    procedure PatchDirectory(aFileDiff: TStrings);
    procedure PatchDirectory(aFileDiff,aDir: string);

    property Cancelled: boolean read fCancelled;
    property Count: integer read GetCompareCount;
    property Compares[index: integer]: TCompareRec read GetCompare; default;
    property DiffStats: TDiffStats read fDiffStats;
  published
    {Algorytm działania:
      daDifference    - algorytm różnicowy Meyer'sa
      daSeqComparison - algorytm porównywania sekwencji}
    property Algorithm: TDiffAlgorithm read FAlg write FAlg default daDifference;
    {Tryb czytania plików (jeśli wykorzystywane):
      fePascal - pascalowe czytanie plików,
      feMemory - wczytanie całego pliku do pamięci.}
    property FileEngine: TDiffFileEngine read FFileEngine write FFileEngine default feMemory;
    {Jeśli potrzebujesz użyć metody:
     *** Application.ProcessMessage ***}
    property OnProcSys: TNotifyEvent read FOnProcSys write FOnProcSys;
    {Gdy algorytm Diff trafi na plik binarny
     tu możesz go dodatkowo obsłużyć w swoim programie.}
    property OnBinaryFileDiff: TDiffRequestFileEvent read FBinDiff write FBinDiff;
    {Gdy algorytm Patch trafi na plik binarny
     tu możesz go dodatkowo obsłużyć w swoim programie.}
    property OnBinaryFilePatch: TDiffRequestFileEvent read FBinPatch write FBinPatch;
    {Żądanie podania dokładnego czasu ostatniej edycji pliku.}
    property OnRequestFileAttrib: TDiffRequestFileAttributesEvent read FRequestFileAttrib write FRequestFileAttrib;
    {Żądanie sprawdzenia, czy plik jest plikiem binarnym?}
    property OnRequestFileIsBin: TDiffRequestFileBinaryEvent read FRequestFileIsBin write FRequestFileIsBin;
    {Żądanie podania zawartości pliku tekstowego.}
    property OnRequestFileBody: TDiffRequestFileBodyEvent read FRequestFileBody write FRequestFileBody;
    {Żądanie porównania dwóch plików binarnych,
     by stwierdzić czy się różnią!}
    property OnRequestDiffBinFiles: TDiffRequestDiffBinFilesEvent read FRequestDiffBinFiles write FRequestDiffBinFiles;
    {żądanie usunięcia pliku!}
    property OnRequestDeleteFile: TDiffRequestFileEvent read FRequestDeleteFile write FRequestDeleteFile;
    {żądanie aktualizacji pliku!}
    property OnRequestSaveFile: TDiffRequestFileBodyEvent read FRequestSaveFile write FRequestSaveFile;
  end;

procedure Register;

implementation

uses
  {$IFDEF UNIX}
  Math, crc, md5, BaseUnix, strutils, lazfileutils, fileutil;
  {$ELSE}
  Math, crc, md5, strutils, lazfileutils, fileutil;
  {$ENDIF}

type
  PElement = ^TElement;
  TElement = record
    flaga: char;
    s: string;
    i1,i2: integer;
  end;

const
  textseparator = '"';

procedure Register;
begin
  {$I diff_icon.lrs}
  RegisterComponents('Misc',[TExtDiff]);
end;

function CrcString(const mystring: string): longword;
var
  crcvalue: longword;
begin
  crcvalue:=crc32(0,nil,0);
  result:=crc32(crcvalue,@mystring[1],length(mystring));
end;

function ElementAdd(aList: TList; aElement: TElement): integer;
var
  p: PElement;
begin
  new(p);
  p^:=aElement;
  result:=aList.Add(p);
end;

procedure ElementClear(aList: TList);
var
  i: integer;
begin
  for i:=0 to aList.Count-1 do dispose(PElement(aList[i]));
end;

function StringToItemIndex(slist: TStrings; kod: string; aIndeksOd: integer = 0; aIndeksDo: integer = -1; wart_domyslna: integer = -1): integer;
var
  i,a,max: integer;
begin
   a:=wart_domyslna;
   if aIndeksDo=-1 then max:=slist.Count-1 else max:=aIndeksDo;
   for i:=aIndeksOd to max do if slist[i]=kod then
   begin
     a:=i;
     break;
   end;
   result:=a;
end;

function StringToItemIndex(slist: TStringList; kod: string; aIndeksOd: integer = 0; aIndeksDo: integer = -1; wart_domyslna: integer = -1): integer;
var
  i,a,max: integer;
begin
   a:=wart_domyslna;
   if aIndeksDo=-1 then max:=slist.Count-1 else max:=aIndeksDo;
   for i:=aIndeksOd to max do if slist[i]=kod then
   begin
     a:=i;
     break;
   end;
   result:=a;
end;

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

function pierwsza_nazwa(aFileName: string): string;
var
  s: string;
  a: integer;
begin
  s:=aFileName;
  {$IFDEF UNIX}
  a:=pos('/',s);
  {$ELSE}
  a:=pos('\',s);
  {$ENDIF}
  if a>0 then delete(s,a,maxint);
  result:=s;
end;

function druga_nazwa(aFileName: string): string;
var
  s: string;
  a: integer;
begin
  s:=aFileName;
  {$IFDEF UNIX}
  a:=pos('/',s);
  {$ELSE}
  a:=pos('\',s);
  {$ENDIF}
  if a>0 then delete(s,1,a);
  result:=s;
end;

function druga_nazwa(aKat1,aKat2,aStr: string; aReverse: boolean = false): string;
var
  s: string;
  i: integer;
begin
  s:=aStr;
  if aReverse then
  begin
    for i:=1 to length(aKat2) do delete(s,1,1);
    s:=aKat1+s;
  end else begin
    for i:=1 to length(aKat1) do delete(s,1,1);
    s:=aKat2+s;
  end;
  result:=s;
end;

procedure patch_go(d,f: TStrings; start,a1,a2,b1,b2: integer; var wektor: integer);
var
  i: integer;
  s,s1: string;
begin
  for i:=start to d.Count-1 do
  begin
    s:=d[i];
    if pos('diff -ruN',s)=1 then break;
    if pos('---',s)=1 then break;
    if pos('@@',s)=1 then break;
    if pos('B',s)=1 then break;
    s1:=s;
    delete(s1,1,1);
    if s[1]=' ' then continue;
    if s[1]='-' then
    begin
      f.Delete(a1+i+wektor-start-1);
      dec(wektor);
    end;
    if s[1]='+' then
    begin
      if (a1=0) and (a2=0) then f.Add(s1) else f.Insert(a1+i+wektor-start-1,s1);
      inc(wektor);
    end;
  end;
end;

function IsDirectory(aFileName: string): boolean;
var
  f: longint;
begin
  f:=FileGetAttr(aFileName);
  result:=(f and faDirectory)<>0;
end;

{ TFilesList }

type
  PFilesListFiles = ^TFilesListFiles;
  TFilesListFiles = record
    PatchToFile : string;
    DateTime    : qword;
    MiliSeconds : qword;
  end;

constructor TFilesList.Create;
begin
  FFiles:=TList.Create;
end;

destructor TFilesList.Destroy;
begin
  Clear;
  FFiles.Free;
  inherited Destroy;
end;

procedure TFilesList.Clear;
var
  i: integer;
begin
  for i:=0 to FFiles.Count-1 do Dispose(PFilesListFiles(FFiles[i]));
  FFiles.Clear;
end;

function TFilesList.Count: integer;
begin
  result:=FFiles.Count;
end;

function TFilesList.Add(aPatchToFile: string; aDateTime, aMiliSeconds: qword
  ): integer;
var
  p: PFilesListFiles;
begin
  new(p);
  p^.PatchToFile:=aPatchToFile;
  p^.DateTime:=aDateTime;
  p^.MiliSeconds:=aMiliSeconds;
  result:=FFiles.Add(p);
end;

procedure TFilesList.Delete(aIndex: integer);
begin
  Dispose(PFilesListFiles(FFiles[aIndex]));
  FFiles.Delete(aIndex);
end;

procedure TFilesList.Get(aIndex: integer; var aPatchToFile: string;
  var aDateTime, aMiliSeconds: qword);
var
  p: PFilesListFiles;
begin
  p:=FFiles[aIndex];
  aPatchToFile:=p^.PatchToFile;
  aDateTime:=p^.DateTime;
  aMiliSeconds:=p^.MiliSeconds;
end;

{ TDiff }

procedure TExtDiff.PushDiff(offset1, offset2, len1, len2: integer);
var
  DiffVars: PDiffVars;
begin
  new(DiffVars);
  DiffVars^.offset1:=offset1;
  DiffVars^.offset2:=offset2;
  DiffVars^.len1:=len1;
  DiffVars^.len2:=len2;
  fDiffList.Add(DiffVars);
end;

function TExtDiff.PopDiff: boolean;
var
  DiffVars: PDiffVars;
  idx: integer;
begin
  idx:=fDiffList.Count-1;
  result:=idx>=0;
  if not result then exit;
  DiffVars:=PDiffVars(fDiffList[idx]);
  with DiffVars^ do if fCompareInts then DiffInt(offset1,offset2,len1,len2) else DiffChr(offset1,offset2,len1,len2);
  Dispose(DiffVars);
  fDiffList.Delete(idx);
end;

procedure TExtDiff.DiffInt(offset1, offset2, len1, len2: integer);
var
  p,k,delta: integer;
begin
  //trim matching bottoms ...
  while (len1>0) and (len2>0) and (Ints1^[offset1]=Ints2^[offset2]) do
  begin
    inc(offset1);
    inc(offset2);
    dec(len1);
    dec(len2);
  end;
  //trim matching tops ...
  while (len1>0) and (len2>0) and (Ints1^[offset1+len1-1]=Ints2^[offset2+len2-1]) do
  begin
    dec(len1);
    dec(len2);
  end;

  //stop diff'ing if minimal conditions reached ...
  if (len1=0) then
  begin
    AddChangeInt(offset1,len2,ckAdd);
    exit;
  end else
  if (len2=0) then
  begin
    AddChangeInt(offset1,len1,ckDelete);
    exit;
  end else
  if (len1=1) and (len2=1) then
  begin
    AddChangeInt(offset1,1,ckDelete);
    AddChangeInt(offset1,1,ckAdd);
    exit;
  end;

  p:=-1;
  delta:=len2-len1;
  InitDiagArrays(len1,len2);
  if delta<0 then
  begin
    repeat
      inc(p);
      if (p mod 1024)=1023 then
      begin
        if Assigned(FOnProcSys) then FOnProcSys(self);
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k:=p downto delta+1 do if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k:=-p+delta to delta-1 do if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k:=delta-p to -1 do if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      for k:=p downto 1 do if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      if SnakeIntF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeIntB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end else begin
    repeat
      inc(p);
      if (p mod 1024)=1023 then
      begin
        if Assigned(FOnProcSys) then FOnProcSys(self);
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k:=-p to delta-1 do if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k:=p+delta downto delta+1 do if SnakeIntF(k,offset1,offset2,len1,len2) then exit;
      for k:=delta+p downto 1 do if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      for k:=-p to -1 do if SnakeIntB(k,offset1,offset2,len1,len2) then exit;
      if SnakeIntF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeIntB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end;
end;

procedure TExtDiff.DiffChr(offset1, offset2, len1, len2: integer);
var
  p,k,delta: integer;
begin
  //trim matching bottoms ...
  while (len1>0) and (len2>0) and (Chrs1^[offset1]=Chrs2^[offset2]) do
  begin
    inc(offset1);
    inc(offset2);
    dec(len1);
    dec(len2);
  end;
  //trim matching tops ...
  while (len1>0) and (len2>0) and
    (Chrs1^[offset1+len1-1]=Chrs2^[offset2+len2-1]) do
  begin
    dec(len1);
    dec(len2);
  end;

  //stop diff'ing if minimal conditions reached ...
  if (len1=0) then
  begin
    AddChangeChr(offset1,len2,ckAdd);
    exit;
  end
  else if (len2=0) then
  begin
    AddChangeChr(offset1,len1,ckDelete);
    exit;
  end
  else if (len1=1) and (len2=1) then
  begin
    AddChangeChr(offset1,1,ckDelete);
    AddChangeChr(offset1,1,ckAdd);
    exit;
  end;

  p:=-1;
  delta:=len2-len1;
  InitDiagArrays(len1,len2);
  if delta<0 then
  begin
    repeat
      inc(p);
      if (p mod 1024=1023) then
      begin
        if Assigned(FOnProcSys) then FOnProcSys(self);
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k:=p downto delta+1 do if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k:=-p+delta to delta-1 do if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k:=delta-p to -1 do if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      for k:=p downto 1 do if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      if SnakeChrF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeChrB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end else begin
    repeat
      inc(p);
      if (p mod 1024=1023) then
      begin
        if Assigned(FOnProcSys) then FOnProcSys(self);
        if fCancelled then exit;
      end;
      //nb: the Snake order is important here
      for k:=-p to delta -1 do if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k:=p+delta downto delta +1 do if SnakeChrF(k,offset1,offset2,len1,len2) then exit;
      for k:=delta+p downto 1 do if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      for k:=-p to -1 do if SnakeChrB(k,offset1,offset2,len1,len2) then exit;
      if SnakeChrF(delta,offset1,offset2,len1,len2) then exit;
      if SnakeChrB(0,offset1,offset2,len1,len2) then exit;
    until(false);
  end;
end;

function TExtDiff.SnakeChrF(k, offset1, offset2, len1, len2: integer): boolean;
var
  x,y: integer;
begin
  if fDiag^[k+1]>fDiag^[k-1] then y:=fDiag^[k+1] else y:=fDiag^[k-1]+1;
  x:=y-k;
  while (x<len1-1) and (y<len2-1) and (Chrs1^[offset1+x+1]=Chrs2^[offset2+y+1]) do
  begin
    inc(x);
    inc(y);
  end;
  fDiag^[k]:=y;
  result:=(fDiag^[k]>=bDiag^[k]);
  if not result then exit;

  inc(x);
  inc(y);
  PushDiff(offset1+x,offset2+y,len1-x,len2-y);
  PushDiff(offset1,offset2,x,y);
end;

function TExtDiff.SnakeChrB(k, offset1, offset2, len1, len2: integer): boolean;
var
  x,y: integer;
begin
  if bDiag^[k-1]<bDiag^[k+1] then y:=bDiag^[k-1] else y:=bDiag^[k+1]-1;
  x:=y-k;
  while (x>=0) and (y>=0) and (Chrs1^[offset1+x]=Chrs2^[offset2+y]) do
  begin
    dec(x);
    dec(y);
  end;
  bDiag^[k]:=y;
  result:=bDiag^[k]<=fDiag^[k];
  if not result then exit;

  inc(x);
  inc(y);
  PushDiff(offset1+x,offset2+y,len1-x,len2-y);
  PushDiff(offset1,offset2,x,y);
end;

function TExtDiff.SnakeIntF(k, offset1, offset2, len1, len2: integer): boolean;
var
  x,y: integer;
begin
  if fDiag^[k+1]>fDiag^[k-1] then y:=fDiag^[k+1] else y:=fDiag^[k-1]+1;
  x:=y-k;
  while (x<len1-1) and (y<len2-1) and (Ints1^[offset1+x+1]=Ints2^[offset2+y+1]) do
  begin
    inc(x);
    inc(y);
  end;
  fDiag^[k]:=y;
  result:=(fDiag^[k]>=bDiag^[k]);
  if not result then exit;

  inc(x);
  inc(y);
  PushDiff(offset1+x,offset2+y,len1-x,len2-y);
  PushDiff(offset1,offset2,x,y);
end;

function TExtDiff.SnakeIntB(k, offset1, offset2, len1, len2: integer): boolean;
var
  x,y: integer;
begin
  if bDiag^[k-1]<bDiag^[k+1] then y:=bDiag^[k-1] else y:=bDiag^[k+1]-1;
  x:=y-k;
  while (x>=0) and (y>=0) and (Ints1^[offset1+x]=Ints2^[offset2+y]) do
  begin
    dec(x);
    dec(y);
  end;
  bDiag^[k]:=y;
  result:=bDiag^[k]<=fDiag^[k];
  if not result then exit;

  inc(x);
  inc(y);
  PushDiff(offset1+x,offset2+y,len1-x,len2-y);
  PushDiff(offset1,offset2,x,y);
end;

procedure TExtDiff.AddChangeChr(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (LastCompareRec.oldIndex1<offset1-1) do
  begin
    with LastCompareRec do
    begin
      Kind:=ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      chr1:=Chrs1^[oldIndex1];
      chr2:=Chrs2^[oldIndex2];
    end;
    New(compareRec);
    compareRec^:=LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckNone: for i:=1 to range do
            begin
              with LastCompareRec do
              begin
                Kind:=ckNone;
                inc(oldIndex1);
                inc(oldIndex2);
                chr1:=Chrs1^[oldIndex1];
                chr2:=Chrs2^[oldIndex2];
              end;
              New(compareRec);
              compareRec^:=LastCompareRec;
              fCompareList.Add(compareRec);
              inc(fDiffStats.matches);
            end;
    ckAdd:  begin
              for i:=1 to range do
              begin
                with LastCompareRec do
                begin

                  //check if a range of adds are following a range of deletes
                  //and convert them to modifies ...
                  if Kind=ckDelete then
                  begin
                    j:=fCompareList.Count-1;
                    while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckDelete) do dec(j);
                    PCompareRec(fCompareList[j])^.Kind:=ckModify;
                    dec(fDiffStats.deletes);
                    inc(fDiffStats.modifies);
                    inc(LastCompareRec.oldIndex2);
                    PCompareRec(fCompareList[j])^.oldIndex2:=LastCompareRec.oldIndex2;
                    PCompareRec(fCompareList[j])^.chr2:=Chrs2^[oldIndex2];
                    if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                    continue;
                  end;

                  Kind:=ckAdd;
                  chr1:=#0;
                  inc(oldIndex2);
                  chr2:=Chrs2^[oldIndex2]; //ie what we added
                end;
                New(compareRec);
                compareRec^:=LastCompareRec;
                fCompareList.Add(compareRec);
                inc(fDiffStats.adds);
              end;
            end;
    ckDelete: begin
                for i:=1 to range do
                begin
                  with LastCompareRec do
                  begin

                    //check if a range of deletes are following a range of adds
                    //and convert them to modifies ...
                    if Kind=ckAdd then
                    begin
                      j:=fCompareList.Count-1;
                      while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckAdd) do dec(j);
                      PCompareRec(fCompareList[j])^.Kind:=ckModify;
                      dec(fDiffStats.adds);
                      inc(fDiffStats.modifies);
                      inc(LastCompareRec.oldIndex1);
                      PCompareRec(fCompareList[j])^.oldIndex1:=LastCompareRec.oldIndex1;
                      PCompareRec(fCompareList[j])^.chr1:=Chrs1^[oldIndex1];
                      if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                      continue;
                    end;

                    Kind:=ckDelete;
                    chr2:=#0;
                    inc(oldIndex1);
                    chr1:=Chrs1^[oldIndex1]; //ie what we deleted
                  end;
                  New(compareRec);
                  compareRec^:=LastCompareRec;
                  fCompareList.Add(compareRec);
                  inc(fDiffStats.deletes);
                end;
              end;
  end;
end;

procedure TExtDiff.AddChangeInt(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (LastCompareRec.oldIndex1<offset1-1) do
  begin
    with LastCompareRec do
    begin
      Kind:=ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      int1:=Ints1^[oldIndex1];
      int2:=Ints2^[oldIndex2];
    end;
    New(compareRec);
    compareRec^:=LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckNone: for i:=1 to range do
            begin
              with LastCompareRec do
              begin
                Kind:=ckNone;
                inc(oldIndex1);
                inc(oldIndex2);
                int1:=Ints1^[oldIndex1];
                int2:=Ints2^[oldIndex2];
              end;
              New(compareRec);
              compareRec^:=LastCompareRec;
              fCompareList.Add(compareRec);
              inc(fDiffStats.matches);
            end;
    ckAdd: begin
             for i:=1 to range do
             begin
               with LastCompareRec do
               begin

                 //check if a range of adds are following a range of deletes
                 //and convert them to modifies ...
                 if Kind=ckDelete then
                 begin
                   j:=fCompareList.Count-1;
                   while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckDelete) do dec(j);
                   PCompareRec(fCompareList[j])^.Kind:=ckModify;
                   dec(fDiffStats.deletes);
                   inc(fDiffStats.modifies);
                   inc(LastCompareRec.oldIndex2);
                   PCompareRec(fCompareList[j])^.oldIndex2:=LastCompareRec.oldIndex2;
                   PCompareRec(fCompareList[j])^.int2:=Ints2^[oldIndex2];
                   if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                   continue;
                 end;

                 Kind:=ckAdd;
                 int1:=$0;
                 inc(oldIndex2);
                 int2:=Ints2^[oldIndex2]; //ie what we added
               end;
               New(compareRec);
               compareRec^:=LastCompareRec;
               fCompareList.Add(compareRec);
               inc(fDiffStats.adds);
             end;
           end;
    ckDelete: begin
                for i:=1 to range do
                begin
                  with LastCompareRec do
                  begin

                   //check if a range of deletes are following a range of adds
                   //and convert them to modifies ...
                   if Kind=ckAdd then
                   begin
                     j:=fCompareList.Count-1;
                     while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckAdd) do dec(j);
                     PCompareRec(fCompareList[j])^.Kind:=ckModify;
                     dec(fDiffStats.adds);
                     inc(fDiffStats.modifies);
                     inc(LastCompareRec.oldIndex1);
                     PCompareRec(fCompareList[j])^.oldIndex1:=LastCompareRec.oldIndex1;
                     PCompareRec(fCompareList[j])^.int1:=Ints1^[oldIndex1];
                     if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                     continue;
                   end;

                   Kind:=ckDelete;
                   int2:=$0;
                   inc(oldIndex1);
                   int1:=Ints1^[oldIndex1]; //ie what we deleted
                 end;
                 New(compareRec);
                 compareRec^:=LastCompareRec;
                 fCompareList.Add(compareRec);
                 inc(fDiffStats.deletes);
               end;
             end;
  end;
end;

procedure TExtDiff.InitDiagArrays(MaxOscill, len1, len2: integer);
var
  i: integer;
begin
  inc(maxOscill); //for the extra diag at each end of the arrays ...
  P8Bits(fDiag):=P8Bits(fDiagBuffer)-sizeof(int64)*(MAX_DIAGONAL-maxOscill);
  P8Bits(bDiag):=P8Bits(bDiagBuffer)-sizeof(int64)*(MAX_DIAGONAL-maxOscill);
  //initialize Diag arrays (assumes 0 based arrays) ...
  for i:=-maxOscill to maxOscill do fDiag^[i]:=-MAXINT;
  fDiag^[0]:=-1;
  for i:=-maxOscill to maxOscill do bDiag^[i]:=MAXINT;
  bDiag^[len1-len2]:=len1-1;
end;

procedure TExtDiff.InitDiagArrays(len1, len2: integer);
var
  i: integer;
begin
  //assumes that top and bottom matches have been excluded
  P8Bits(fDiag):=P8Bits(fDiagBuffer)-sizeof(int64)*(MAX_DIAGONAL-(len1+1));
  for i:=-(len1+1) to (len2+1) do fDiag^[i]:=-MAXINT;
  fDiag^[1]:=-1;
  P8Bits(bDiag):=P8Bits(bDiagBuffer)-sizeof(int64)*(MAX_DIAGONAL-(len1+1));
  for i:=-(len1+1) to (len2+1) do bDiag^[i]:=MAXINT;
  bDiag^[len2-len1+1]:=len2;
end;

procedure TExtDiff.RecursiveDiffChr(offset1, offset2, len1, len2: integer);
var
  diag,lenDelta,Oscill,maxOscill,x1,x2: integer;
begin
  //nb: the possible depth of recursion here is most unlikely to cause
  //    problems with stack overflows.
  if Assigned(FOnProcSys) then FOnProcSys(self);
  if fCancelled then exit;

  if (len1=0) then
  begin
    AddChangeChrs(offset1,len2,ckAdd);
    exit;
  end else
  if (len2=0) then
  begin
    AddChangeChrs(offset1,len1,ckDelete);
    exit;
  end else
  if (len1=1) and (len2=1) then
  begin
    AddChangeChrs(offset1,1,ckDelete);
    AddChangeChrs(offset1,1,ckAdd);
    exit;
  end;

  maxOscill:=min(max(len1,len2),MAX_DIAGONAL);
  InitDiagArrays(MaxOscill,len1,len2);
  lenDelta:=len1-len2;

  Oscill:=1; //ie assumes prior filter of top and bottom matches
  while Oscill<=maxOscill do
  begin

    if (Oscill mod 200)=0 then
    begin
      if Assigned(FOnProcSys) then FOnProcSys(self);
      if fCancelled then exit;
    end;

    //do forward oscillation (keeping diag within assigned grid)...
    diag := Oscill;
    while diag>len1 do dec(diag,2);
    while diag>=max(-Oscill,-len2) do
    begin
      if fDiag^[diag-1]<fDiag^[diag+1] then x1:=fDiag^[diag+1] else x1:=fDiag^[diag-1]+1;
      x2:=x1-diag;
      while (x1<len1-1) and (x2<len2-1) and (Chrs1^[offset1+x1+1]=Chrs2^[offset2+x2+1]) do
      begin
        inc(x1);
        inc(x2);
      end;
      fDiag^[diag]:=x1;

      //nb: (fDiag[diag] is always < bDiag[diag]) here when NOT odd(lenDelta) ...
      if odd(lenDelta) and (fDiag^[diag]>=bDiag^[diag]) then
      begin
        inc(x1);
        inc(x2);
        //save x1 & x2 for second recursive_diff() call by reusing no longer
        //needed variables (ie minimize variable allocation in recursive fn) ...
        diag:=x1;
        Oscill:=x2;
        while (x1>0) and (x2>0) and (Chrs1^[offset1+x1-1]=Chrs2^[offset2+x2-1]) do
        begin
          dec(x1);
          dec(x2);
        end;
        RecursiveDiffChr(offset1,offset2,x1,x2);
        x1:=diag;
        x2:=Oscill;
        RecursiveDiffChr(offset1+x1,offset2+x2,len1-x1,len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    //do backward oscillation (keeping diag within assigned grid)...
    diag:=lenDelta+Oscill;
    while diag>len1 do dec(diag,2);
    while diag>=max(lenDelta-Oscill,-len2) do
    begin
      if bDiag^[diag-1]<bDiag^[diag+1] then x1:=bDiag^[diag-1] else x1:=bDiag^[diag+1]-1;
      x2:=x1-diag;
      while (x1>-1) and (x2>-1) and (Chrs1^[offset1+x1]=Chrs2^[offset2+x2]) do
      begin
        dec(x1);
        dec(x2);
      end;
      bDiag^[diag]:=x1;

      if bDiag^[diag]<=fDiag^[diag] then
      begin
        //flag return value then ...
        inc(x1);
        inc(x2);
        RecursiveDiffChr(offset1,offset2,x1,x2);
        while (x1<len1) and (x2<len2) and (Chrs1^[offset1+x1]=Chrs2^[offset2+x2]) do
        begin
          inc(x1);
          inc(x2);
        end;
        RecursiveDiffChr(offset1+x1,offset2+x2,len1-x1,len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    inc(Oscill);
  end; //while Oscill <= maxOscill

  raise Exception.create('oops - error in RecursiveDiffChr()');
end;

procedure TExtDiff.AddChangeChrs(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (LastCompareRec.oldIndex1<offset1-1) do
  begin
    with LastCompareRec do
    begin
      Kind:=ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      chr1:=Chrs1^[oldIndex1];
      chr2:=Chrs2^[oldIndex2];
    end;
    New(compareRec);
    compareRec^:=LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckAdd: begin
             for i:=1 to range do
             begin
               with LastCompareRec do
               begin

                 //check if a range of adds are following a range of deletes
                 //and convert them to modifies ...
                 if Kind = ckDelete then
                 begin
                   j:=fCompareList.Count-1;
                   while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckDelete) do dec(j);
                   PCompareRec(fCompareList[j])^.Kind:=ckModify;
                   dec(fDiffStats.deletes);
                   inc(fDiffStats.modifies);
                   inc(LastCompareRec.oldIndex2);
                   PCompareRec(fCompareList[j])^.oldIndex2:=LastCompareRec.oldIndex2;
                   PCompareRec(fCompareList[j])^.chr2:=Chrs2^[oldIndex2];
                   if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                   continue;
                 end;

                 Kind:=ckAdd;
                 chr1:=#0;
                 inc(oldIndex2);
                 chr2:=Chrs2^[oldIndex2]; //ie what we added
               end;
               New(compareRec);
               compareRec^:=LastCompareRec;
               fCompareList.Add(compareRec);
               inc(fDiffStats.adds);
             end;
           end;
    ckDelete: begin
                for i:=1 to range do
                begin
                  with LastCompareRec do
                  begin

                    //check if a range of deletes are following a range of adds
                    //and convert them to modifies ...
                    if Kind=ckAdd then
                    begin
                      j:=fCompareList.Count-1;
                      while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckAdd) do dec(j);
                      PCompareRec(fCompareList[j])^.Kind:=ckModify;
                      dec(fDiffStats.adds);
                      inc(fDiffStats.modifies);
                      inc(LastCompareRec.oldIndex1);
                      PCompareRec(fCompareList[j])^.oldIndex1:=LastCompareRec.oldIndex1;
                      PCompareRec(fCompareList[j])^.chr1:=Chrs1^[oldIndex1];
                      if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                      continue;
                    end;

                    Kind:=ckDelete;
                    chr2:=#0;
                    inc(oldIndex1);
                    chr1:=Chrs1^[oldIndex1]; //ie what we deleted
                  end;
                  New(compareRec);
                  compareRec^:=LastCompareRec;
                  fCompareList.Add(compareRec);
                  inc(fDiffStats.deletes);
                end;
              end;
  end;
end;

procedure TExtDiff.RecursiveDiffInt(offset1, offset2, len1, len2: integer);
var
  diag,lenDelta,Oscill,maxOscill,x1,x2: integer;
begin
  //nb: the possible depth of recursion here is most unlikely to cause
  //    problems with stack overflows.
  if Assigned(FOnProcSys) then FOnProcSys(self);
  if fCancelled then exit;

  if (len1=0) then
  begin
    assert(len2>0,'oops!');
    AddChangeInts(offset1,len2,ckAdd);
    exit;
  end else
  if (len2=0) then
  begin
    AddChangeInts(offset1,len1,ckDelete);
    exit;
  end else
  if (len1=1) and (len2=1) then
  begin
    assert(Ints1^[offset1]<>Ints2^[offset2],'oops!');
    AddChangeInts(offset1,1,ckDelete);
    AddChangeInts(offset1,1,ckAdd);
    exit;
  end;

  maxOscill:=min(max(len1,len2),MAX_DIAGONAL);
  InitDiagArrays(MaxOscill,len1,len2);
  lenDelta:=len1-len2;

  Oscill:=1; //ie assumes prior filter of top and bottom matches
  while Oscill<=maxOscill do
  begin

    if (Oscill mod 200)=0 then
    begin
      if Assigned(FOnProcSys) then FOnProcSys(self);
      if fCancelled then exit;
    end;

    //do forward oscillation (keeping diag within assigned grid)...
    diag:=Oscill;
    while diag>len1 do dec(diag,2);
    while diag>=max(-Oscill,-len2) do
    begin
      if fDiag^[diag-1]<fDiag^[diag+1] then x1:=fDiag^[diag+1] else x1:=fDiag^[diag-1]+1;
      x2:=x1-diag;
      while (x1<len1-1) and (x2<len2-1) and (Ints1^[offset1+x1+1]=Ints2^[offset2+x2+1]) do
      begin
        inc(x1);
        inc(x2);
      end;
      fDiag^[diag]:=x1;

      //nb: (fDiag[diag] is always < bDiag[diag]) here when NOT odd(lenDelta) ...
      if odd(lenDelta) and (fDiag^[diag]>=bDiag^[diag]) then
      begin
        inc(x1);
        inc(x2);
        //save x1 & x2 for second recursive_diff() call by reusing no longer
        //needed variables (ie minimize variable allocation in recursive fn) ...
        diag:=x1;
        Oscill:=x2;
        while (x1>0) and (x2>0) and (Ints1^[offset1+x1-1]=Ints2^[offset2+x2-1]) do
        begin
          dec(x1);
          dec(x2);
        end;
        RecursiveDiffInt(offset1,offset2,x1,x2);
        x1:=diag;
        x2:=Oscill;
        RecursiveDiffInt(offset1+x1,offset2+x2,len1-x1,len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    //do backward oscillation (keeping diag within assigned grid)...
    diag:=lenDelta+Oscill;
    while diag>len1 do dec(diag,2);
    while diag>=max(lenDelta-Oscill,-len2) do
    begin
      if bDiag^[diag-1]<bDiag^[diag+1] then x1:=bDiag^[diag-1] else x1:=bDiag^[diag+1]-1;
      x2:=x1-diag;
      while (x1>-1) and (x2>-1) and (Ints1^[offset1+x1]=Ints2^[offset2+x2]) do
      begin
        dec(x1);
        dec(x2);
      end;
      bDiag^[diag]:=x1;

      if bDiag^[diag]<=fDiag^[diag] then
      begin
        //flag return value then ...
        inc(x1);
        inc(x2);
        RecursiveDiffInt(offset1,offset2,x1,x2);
        while (x1<len1) and (x2<len2) and (Ints1^[offset1+x1]=Ints2^[offset2+x2]) do
        begin
          inc(x1);
          inc(x2);
        end;
        RecursiveDiffInt(offset1+x1,offset2+x2,len1-x1,len2-x2);
        exit; //ALL DONE
      end;
      dec(diag,2);
    end;

    inc(Oscill);
  end; //while Oscill <= maxOscill

  raise Exception.create('oops - error in RecursiveDiffInt()');
end;

procedure TExtDiff.AddChangeInts(offset1, range: integer; ChangeKind: TChangeKind);
var
  i,j: integer;
  compareRec: PCompareRec;
begin
  //first, add any unchanged items into this list ...
  while (LastCompareRec.oldIndex1<offset1-1) do
  begin
    with LastCompareRec do
    begin
      Kind:=ckNone;
      inc(oldIndex1);
      inc(oldIndex2);
      int1:=Ints1^[oldIndex1];
      int2:=Ints2^[oldIndex2];
    end;
    New(compareRec);
    compareRec^:=LastCompareRec;
    fCompareList.Add(compareRec);
    inc(fDiffStats.matches);
  end;

  case ChangeKind of
    ckAdd: begin
             for i:=1 to range do
             begin
               with LastCompareRec do
               begin

                 //check if a range of adds are following a range of deletes
                 //and convert them to modifies ...
                 if Kind=ckDelete then
                 begin
                   j:=fCompareList.Count-1;
                   while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckDelete) do dec(j);
                   PCompareRec(fCompareList[j])^.Kind:=ckModify;
                   dec(fDiffStats.deletes);
                   inc(fDiffStats.modifies);
                   inc(LastCompareRec.oldIndex2);
                   PCompareRec(fCompareList[j])^.oldIndex2:=LastCompareRec.oldIndex2;
                   PCompareRec(fCompareList[j])^.int2:=Ints2^[oldIndex2];
                   if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                   continue;
                 end;

                 Kind:=ckAdd;
                 int1:=$0;
                 inc(oldIndex2);
                 int2:=Ints2^[oldIndex2]; //ie what we added
               end;
               New(compareRec);
               compareRec^:=LastCompareRec;
               fCompareList.Add(compareRec);
               inc(fDiffStats.adds);
             end;
           end;
    ckDelete: begin
                for i := 1 to range do
                begin
                  with LastCompareRec do
                  begin

                    //check if a range of deletes are following a range of adds
                    //and convert them to modifies ...
                    if Kind=ckAdd then
                    begin
                      j:=fCompareList.Count-1;
                      while (j>0) and (PCompareRec(fCompareList[j-1])^.Kind=ckAdd) do dec(j);
                      PCompareRec(fCompareList[j])^.Kind:=ckModify;
                      dec(fDiffStats.adds);
                      inc(fDiffStats.modifies);
                      inc(LastCompareRec.oldIndex1);
                      PCompareRec(fCompareList[j])^.oldIndex1:=LastCompareRec.oldIndex1;
                      PCompareRec(fCompareList[j])^.int1:=Ints1^[oldIndex1];
                      if j=fCompareList.Count-1 then LastCompareRec.Kind:=ckModify;
                      continue;
                    end;

                    Kind:=ckDelete;
                    int2:=$0;
                    inc(oldIndex1);
                    int1:=Ints1^[oldIndex1]; //ie what we deleted
                  end;
                  New(compareRec);
                  compareRec^:=LastCompareRec;
                  fCompareList.Add(compareRec);
                  inc(fDiffStats.deletes);
                end;
              end;
  end;
end;

function TExtDiff.GetCompare(index: integer): TCompareRec;
begin
  result:=PCompareRec(fCompareList[index])^;
end;

function TExtDiff.GetCompareCount: integer;
begin
  result:=fCompareList.count;
end;

function TExtDiff._ile_pozycji_o_tej_samej_fladze(aIndex: integer): integer;
var
  cc: TCompareRec;
  k: TChangeKind;
  i,l: integer;
  flaga: string;
begin
  l:=0;
  k:=self.Compares[aIndex].Kind;
  for i:=aIndex to self.Count-1 do
  begin
    cc:=self.Compares[i];
    if cc.Kind<>k then break;
    inc(l);
  end;
  result:=l;
end;

procedure TExtDiff.dane_pliku(aStr: string; var aFileName: string; var aDT: TDateTime);
var
  s: string;
  pom,s3,s4,s5,s6: string;
  fs: TFormatSettings;
begin
  s:=StringReplace(aStr,#9,' ',[]);
  aFileName:=NormalizeFilePath(GetLineToStr(s,2,' '));
  s3:=GetLineToStr(s,3,' '); //date
  pom:=GetLineToStr(s,4,' '); //time + milisekund
  s4:=GetLineToStr(pom,1,'.'); //time
  s5:=GetLineToStr(pom,2,'.'); //milisekund
  s6:=GetLineToStr(s,5,' '); //strefa
  fs.DateSeparator:='-';
  fs.ShortDateFormat:='y/m/d';
  fs.TimeSeparator:=':';
  aDT:=StrToDateTime(s3+' '+s4,fs);
end;

procedure TExtDiff.dane_pliku(aStr: string; var aStart, aCount, bStart, bCount: integer);
var
  s: string;
begin
  {A}
  s:=GetLineToStr(aStr,2,' ');
  if (s[1]='-') or (s[1]='+') then delete(s,1,1);
  if s='1' then
  begin
    aStart:=0;
    aCount:=0;
  end else begin
    aStart:=StrToInt(GetLineToStr(s,1,','));
    aCount:=StrToInt(GetLineToStr(s,2,','));
  end;
  {B}
  s:=GetLineToStr(aStr,3,' ');
  if (s[1]='-') or (s[1]='+') then delete(s,1,1);
  bStart:=StrToInt(GetLineToStr(s,1,','));
  bCount:=StrToInt(GetLineToStr(s,2,','));
end;

function TExtDiff.DiffFileInfo(aFlaga: char; aFileName: string; aDT, aMS: qword
  ): string;
var
  i: integer;
  strefa,s,pom: string;
begin
  i:=round((GetLocalTimeOffset*-1)/60);
  if i>0 then strefa:='+' else strefa:='-';
  if i<10 then strefa:=strefa+'0'+IntToStr(i) else strefa:=strefa+IntToStr(i);
  strefa:=strefa+'00';
  (* buduję string *)
  if aFlaga='+' then s:='+++' else s:='---';
  s:=s+' '+NormalizeFilePath(aFileName,true);
  if aDT=0 then s:=s+#9+'1970-01-01 01:00:00.000000000 +0100' else
  begin
    s:=s+#9+FormatDateTime('yyyy-mm-dd hh:nn:ss',FileDateToDateTime(aDT));
    pom:=FormatFloat('000000000',aMS);
    if length(pom)>9 then SetLength(pom,9);
    s:=s+'.'+pom;
    s:=s+' '+strefa;
  end;
  result:=s;
end;

function TExtDiff.DiffFileInfo(aFlaga: char; aFileName: string;
  aDateTime: TDateTime): string;
var
  i: integer;
  strefa,s,pom: string;
  hh,mm,ss,ms: word;
begin
  i:=round((GetLocalTimeOffset*-1)/60);
  if i>0 then strefa:='+' else strefa:='-';
  if i<10 then strefa:=strefa+'0'+IntToStr(i) else strefa:=strefa+IntToStr(i);
  strefa:=strefa+'00';
  (* buduję string *)
  if aFlaga='+' then s:='+++' else s:='---';
  s:=s+' '+NormalizeFilePath(aFileName,true);
  if aDateTime=0 then s:=s+#9+'1970-01-01 01:00:00.000000000 +0100' else
  begin
    DecodeTime(aDateTime,hh,mm,ss,ms);
    s:=s+#9+FormatDateTime('yyyy-mm-dd hh:nn:ss',aDateTime);
    pom:=FormatFloat('000000000',ms);
    if length(pom)>9 then SetLength(pom,9);
    s:=s+'.'+pom;
    s:=s+' '+strefa;
  end;
  result:=s;
end;

function TExtDiff.DiffAnalize(pp: TList; aDiff: TStrings): integer;
var
  element: TElement;
  ile,max,indeks: integer;
  x,a,x1,x2,x3,x4: integer;
  zab: integer;
  s: string;
begin
  ile:=0;
  max:=pp.Count-1;
  x:=0;
  (* analiza *)
  while true do
  begin
    a:=DiffSzukajPierwszejZmiany(x,pp);
    if a=-1 then break;

    a:=a-3;
    if a<0 then a:=0;

    indeks:=aDiff.Add('@@ -$A,$B +$C,$D @@');
    x1:=0; x2:=0; x3:=0; x4:=0; zab:=0;
    while true do
    begin
      if a>max then break;
      element:=PElement(pp[a])^;
      if (x1=0) and (element.flaga<>'A') then x1:=element.i1+1;
      if (x2=0) and (element.flaga<>'D') then x2:=element.i2+1;
      if element.flaga<>'A' then x3:=element.i1+1;
      if element.flaga<>'D' then x4:=element.i2+1;
      if element.flaga<>'N' then zab:=0;
      aDiff.Add(element.s);
      inc(ile);
      inc(a);
      inc(zab);
      if zab>3 then break;
    end;

    s:=aDiff[indeks];
    s:=StringReplace(s,'$A',IntToStr(x1),[]);
    s:=StringReplace(s,'$B',IntToStr(x3-x1+1),[]);
    s:=StringReplace(s,'$C',IntToStr(x2),[]);
    s:=StringReplace(s,'$D',IntToStr(x4-x2+1),[]);
    aDiff.Delete(indeks);
    if ile>0 then aDiff.Insert(indeks,s);

    x:=a+1;

  end;
  result:=ile;
end;

function TExtDiff.DiffSzukajPierwszejZmiany(aOd: integer; pp: TList): integer;
var
  element: TElement;
  i,a: integer;
begin
  a:=-1;
  for i:=aOd to pp.Count-1 do
  begin
    element:=PElement(pp[i])^;
    if element.flaga<>'N' then
    begin
      a:=i;
      break;
    end;
  end;
  result:=a;
end;

procedure TExtDiff.DiffNewFile(aFileNotFound, aFile: string; aDiff: TStrings;
  aExecuteRequest: boolean);
var
  ff: TStringList;
  {$IFDEF UNIX}
  info: stat;
  {$ENDIF}
  i: integer;
  dtime,msec: qword;
begin
  aDiff.Add(DiffFileInfo('-',aFileNotFound,0,0));
  if aExecuteRequest then
  begin
    if assigned(FRequestFileAttrib) then FRequestFileAttrib(self,2,aFile,dtime,msec);
  end else begin
    {$IFDEF UNIX}
    fpstat(aFile,info);
    dtime:=info.st_mtime;
    msec:=info.st_mtime_nsec;
    {$ELSE}
    dtime:=FileAge(aFile);
    msec:=0;
    {$ENDIF}
  end;
  aDiff.Add(DiffFileInfo('+',aFile,dtime,msec));
  ff:=TStringList.Create;
  try
    if aExecuteRequest then
    begin
      if assigned(FRequestFileBody) then FRequestFileBody(self,2,aFile,ff);
    end else ff.LoadFromFile(aFile);
    aDiff.Add('@@ -0,0 +1,'+IntToStr(ff.Count)+' @@');
    (* przelatuję dane *)
    for i:=0 to ff.Count-1 do aDiff.Add('+'+ff[i]);
  finally
    ff.Free;
  end;
end;

procedure TExtDiff.DiffFileDeleted(aFile, aFileDeleted: string;
  aDiff: TStrings; aExecuteRequest: boolean);
var
  ff: TStringList;
  {$IFDEF UNIX}
  info: stat;
  {$ENDIF}
  i: integer;
  dtime,msec: qword;
begin
  if aExecuteRequest then
  begin
    if assigned(FRequestFileAttrib) then FRequestFileAttrib(self,1,aFile,dtime,msec);
  end else begin
    {$IFDEF UNIX}
    fpstat(aFile,info);
    dtime:=info.st_mtime;
    msec:=info.st_mtime_nsec;
    {$ELSE}
    dtime:=FileAge(aFile);
    msec:=0;
    {$ENDIF}
  end;
  aDiff.Add(DiffFileInfo('-',aFile,dtime,msec));
  aDiff.Add(DiffFileInfo('+',aFileDeleted,0,0));
  ff:=TStringList.Create;
  try
    if aExecuteRequest then
    begin
      if assigned(FRequestFileBody) then FRequestFileBody(self,1,aFile,ff);
    end else ff.LoadFromFile(aFile);
    aDiff.Add('@@ -1 +0,0 @@');
    (* przelatuję dane *)
    for i:=0 to ff.Count-1 do aDiff.Add('-'+ff[i]);
  finally
    ff.Free;
  end;
end;

constructor TExtDiff.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAlg:=daDifference;
  FFileEngine:=feMemory;
  fCompareList:=TList.create;
  fDiffList:=TList.Create;
end;

destructor TExtDiff.Destroy;
begin
  Clear; fCompareList.free;
  fDiffList.Free;
  inherited Destroy;
end;

function TExtDiff.GetHashFile(const sFile: TFileName): string;
begin
  result:=MD5Print(MD5File(sFile));
end;

function TExtDiff.GetHashFile(aBuffer: TMemoryStream): string;
var
  buf: pchar;
begin
  GetMem(buf,aBuffer.Size+1);
  try
    aBuffer.Position:=0;
    aBuffer.ReadBuffer(buf^,aBuffer.Size);
    result:=MD5Print(MD5Buffer(buf^,aBuffer.Size));
  finally
    FreeMem(buf,aBuffer.Size+1);
  end;
end;

function TExtDiff.IsTextFile(const sFile: TFileName; const aForceAll: boolean
  ): boolean;
//Created By Marcelo Castro - from Brazil
var
  oIn: TFileStream;
  iRead, iMaxRead: int64;
  iData: Byte;
  dummy:string;
begin
  result:=true;
  dummy:='';
  oIn:=TFileStream.Create(sFile,fmOpenRead or fmShareDenyNone);
  try
    iMaxRead:=oIn.Size;  //only text the first 1000 bytes
    if (not aForceAll) and (iMaxRead>1000) then iMaxRead:=1000;
    for iRead:=1 to iMaxRead do
    begin
      iData:=oIn.ReadByte;
      //oIn.Read(iData,1);
      if idata>127 then
      begin
        result:=false;
        break;
      end;
    end;
  finally
    FreeAndNil(oIn);
  end;
end;

function TExtDiff.IsTextFile(aMemoryStream: TMemoryStream;
  const aForceAll: boolean): boolean;
var
  i,max: int64;
  b: byte;
begin
  result:=true;
  aMemoryStream.Position:=0;
  max:=aMemoryStream.Size;
  if (not aForceAll) and (max>1000) then max:=1000;
  for i:=1 to max do
  begin
    b:=aMemoryStream.ReadByte;
    if b>127 then
    begin
      result:=false;
      break;
    end;
  end;
end;

function TExtDiff.IsBinaryFile(const sFile: TFileName; const aForceAll: boolean
  ): boolean;
begin
  result:=not IsTextFile(sFile);
end;

function TExtDiff.IsBinaryFile(aMemoryStream: TMemoryStream;
  const aForceAll: boolean): boolean;
begin
  result:=not IsTextFile(aMemoryStream);
end;

function TExtDiff.NormalizeFilePath(aPatchFile: string; aForceLinux: boolean
  ): string;
var
  s: string;
begin
  s:=aPatchFile;
  {$IFDEF UNIX}
  s:=StringReplace(s,'\','/',[rfReplaceAll]);
  {$ELSE}
  if aForceLinux then s:=StringReplace(s,'\','/',[rfReplaceAll])
                 else s:=StringReplace(s,'/','\',[rfReplaceAll]);
  {$ENDIF}
  result:=s;
end;

function TExtDiff.Execute(pints1, pints2: PINT; len1, len2: integer): boolean;
var
  maxOscill,x1,x2,savedLen: integer;
  compareRec: PCompareRec;
var
  i,Len1Minus1: integer;
begin
  if FAlg=daDifference then
  begin
    result:=not fExecuting;
    if not result then exit;
    fExecuting:=true;
    fCancelled:=false;
    try
      Clear;

      //setup the character arrays ...
      Ints1:=pointer(pints1);
      Ints2:=pointer(pints2);

      //save first string length for later (ie for any trailing matches) ...
      savedLen:=len1-1;

      //ignore top matches ...
      x1:=0;
      x2:=0;
      while (len1>0) and (len2>0) and (Ints1^[len1-1]=Ints2^[len2-1]) do
      begin
        dec(len1);
        dec(len2);
      end;

      //if something doesn't match ...
      if (len1<>0) or (len2<>0) then
      begin

        //ignore bottom of matches too ...
        while (len1>0) and (len2>0) and (Ints1^[x1]=Ints2^[x2]) do
        begin
          dec(len1);
          dec(len2);
          inc(x1);
          inc(x2);
        end;

        maxOscill:=min(max(len1,len2),MAX_DIAGONAL);
        fCompareList.Capacity:=len1+len2;

        //nb: the Diag arrays are extended by 1 at each end to avoid testing
        //for array limits. Hence '+3' because will also includes Diag[0] ...
        GetMem(fDiagBuffer,sizeof(int64)*(maxOscill*2+3));
        GetMem(bDiagBuffer,sizeof(int64)*(maxOscill*2+3));
        try
          RecursiveDiffInt(x1,x2,len1,len2);
        finally
          freeMem(fDiagBuffer);
          freeMem(bDiagBuffer);
        end;
      end;

      if fCancelled then
      begin
        result:=false;
        Clear;
        exit;
      end;

      //finally, append any trailing matches onto compareList ...
      while (LastCompareRec.oldIndex1<savedLen) do
      begin
        with LastCompareRec do
        begin
          Kind:=ckNone;
          inc(oldIndex1);
          inc(oldIndex2);
          int1:=Ints1^[oldIndex1];
          int2:=Ints2^[oldIndex2];
        end;
        New(compareRec);
        compareRec^:=LastCompareRec;
        fCompareList.Add(compareRec);
        inc(fDiffStats.matches);
      end;
    finally
      fExecuting:=false;
    end;
  end else begin
    result:=not fExecuting;
    if not result then exit;
    fCancelled:=false;
    fExecuting:=true;
    try
      Clear;

      Len1Minus1:=len1-1;
      fCompareList.Capacity:=len1+len2;
      fCompareInts:=true;

      GetMem(fDiagBuffer,sizeof(int64)*(len1+len2+3));
      GetMem(bDiagBuffer,sizeof(int64)*(len1+len2+3));
      Ints1:=pointer(pints1);
      Ints2:=pointer(pints2);
      try
        PushDiff(0,0,len1,len2);
        while PopDiff do;
      finally
        freeMem(fDiagBuffer);
        freeMem(bDiagBuffer);
      end;

      if fCancelled then
      begin
        result:=false;
        Clear;
        exit;
      end;

      //correct the occasional missed match ...
      for i:=1 to count-1 do
        with PCompareRec(fCompareList[i])^ do
          if (Kind=ckModify) and (int1=int2) then
          begin
            Kind:=ckNone;
            Dec(fDiffStats.modifies);
            Inc(fDiffStats.matches);
          end;

      //finally, append any trailing matches onto compareList ...
      with LastCompareRec do AddChangeInt(oldIndex1,len1Minus1-oldIndex1,ckNone);
    finally
      fExecuting:=false;
    end;
  end;
end;

function TExtDiff.Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean;
var
  maxOscill,x1,x2,savedLen: integer;
  compareRec: PCompareRec;
var
  i, Len1Minus1: integer;
begin
  if FAlg=daDifference then
  begin
    result:=not fExecuting;
    if not result then exit;
    fExecuting:=true;
    fCancelled:=false;
    try
      Clear;

      //save first string length for later (ie for any trailing matches) ...
      savedLen:=len1-1;

      //setup the character arrays ...
      Chrs1:=pointer(pchrs1);
      Chrs2:=pointer(pchrs2);

      //ignore top matches ...
      x1:=0;
      x2:=0;
      while (len1>0) and (len2>0) and (Chrs1^[len1-1]=Chrs2^[len2-1]) do
      begin
        dec(len1);
        dec(len2);
      end;

      //if something doesn't match ...
      if (len1<>0) or (len2<>0) then
      begin
        //ignore bottom of matches too ...
        while (len1>0) and (len2>0) and (Chrs1^[x1]=Chrs2^[x2]) do
        begin
          dec(len1);
          dec(len2);
          inc(x1);
          inc(x2);
        end;

        maxOscill:=min(max(len1,len2),MAX_DIAGONAL);
        fCompareList.Capacity:=len1+len2;

        //nb: the Diag arrays are extended by 1 at each end to avoid testing
        //for array limits. Hence '+3' because will also includes Diag[0] ...
        GetMem(fDiagBuffer,sizeof(int64)*(maxOscill*2+3));
        GetMem(bDiagBuffer,sizeof(int64)*(maxOscill*2+3));
        try
          RecursiveDiffChr(x1,x2,len1,len2);
        finally
          freeMem(fDiagBuffer);
          freeMem(bDiagBuffer);
        end;
      end;

      if fCancelled then
      begin
        result:=false;
        Clear;
        exit;
      end;

      //finally, append any trailing matches onto compareList ...
      while (LastCompareRec.oldIndex1<savedLen) do
      begin
        with LastCompareRec do
        begin
          Kind:=ckNone;
          inc(oldIndex1);
          inc(oldIndex2);
          chr1:=Chrs1^[oldIndex1];
          chr2:=Chrs2^[oldIndex2];
        end;
        New(compareRec);
        compareRec^:=LastCompareRec;
        fCompareList.Add(compareRec);
        inc(fDiffStats.matches);
      end;
    finally
      fExecuting:=false;
    end;
  end else begin
    result:=not fExecuting;
    if not result then exit;
    fCancelled:=false;
    fExecuting:=true;
    try
      Clear;

      Len1Minus1:=len1-1;
      fCompareList.Capacity:=len1+len2;
      fDiffList.Capacity:=1024;
      fCompareInts:=false;

      GetMem(fDiagBuffer,sizeof(int64)*(len1+len2+3));
      GetMem(bDiagBuffer,sizeof(int64)*(len1+len2+3));
      Chrs1:=pointer(pchrs1);
      Chrs2:=pointer(pchrs2);
      try
        PushDiff(0,0,len1,len2);
        while PopDiff do;
      finally
        freeMem(fDiagBuffer);
        freeMem(bDiagBuffer);
      end;

      if fCancelled then
      begin
        result:=false;
        Clear;
        exit;
      end;

      //correct the occasional missed match ...
      for i:=1 to count-1 do with PCompareRec(fCompareList[i])^ do if (Kind=ckModify) and (chr1=chr2) then
      begin
        Kind:=ckNone;
        Dec(fDiffStats.modifies);
        Inc(fDiffStats.matches);
      end;

      //finally, append any trailing matches onto compareList ...
      with LastCompareRec do AddChangeChr(oldIndex1,len1Minus1-oldIndex1,ckNone);
    finally
      fExecuting:=false;
    end;
  end;
end;

procedure TExtDiff.Cancel;
begin
  fCancelled:=true;
end;

procedure TExtDiff.Clear;
var
  i: integer;
begin
  for i:=0 to fCompareList.Count-1 do dispose(PCompareRec(fCompareList[i]));
  fCompareList.clear;
  LastCompareRec.Kind:=ckNone;
  LastCompareRec.oldIndex1:=-1;
  LastCompareRec.oldIndex2:=-1;
  fDiffStats.matches:=0;
  fDiffStats.adds:=0;
  fDiffStats.deletes:=0;
  fDiffStats.modifies:=0;
  Chrs1:=nil;
  Chrs2:=nil;
  Ints1:=nil;
  Ints2:=nil;
end;

procedure TExtDiff.FileToHashList(aFile: string; aHashList: TList;
  aTrimSpace: boolean; aIgnoreSpace: boolean; aIgnoreCase: boolean);
var
  f: Text;
  s: string;
  t: TStringList;
  i: integer;
begin
  aHashList.Clear;
  if FFileEngine=fePascal then
  begin
    assignfile(f,aFile);
    while not eof(f) do
    begin
      readln(f,s);
      aHashList.Add(HashLine(s,aTrimSpace,aIgnoreSpace,aIgnoreCase));
    end;
    closefile(f);
  end else begin
    t:=TStringList.Create;
    try
      t.LoadFromFile(aFile);
      for i:=0 to t.Count-1 do aHashList.Add(HashLine(t[i],aTrimSpace,aIgnoreSpace,aIgnoreCase));
    finally
      t.Free;
    end;
  end;
end;

procedure TExtDiff.StringsToHashList(aStr: TStrings; aHashList: TList;
  aTrimSpace: boolean; aIgnoreSpace: boolean; aIgnoreCase: boolean);
var
  i: integer;
begin
  aHashList.Clear;
  for i:=0 to aStr.Count-1 do aHashList.Add(HashLine(aStr[i],aTrimSpace,aIgnoreSpace,aIgnoreCase));
end;

procedure TExtDiff.StringsToHashList(aStr: TStringList; aHashList: TList;
  aTrimSpace: boolean; aIgnoreSpace: boolean; aIgnoreCase: boolean);
var
  i: integer;
begin
  aHashList.Clear;
  for i:=0 to aStr.Count-1 do aHashList.Add(HashLine(aStr[i],aTrimSpace,aIgnoreSpace,aIgnoreCase));
end;

function TExtDiff.HashLine(const aLine: string; aTrimSpace: boolean;
  aIgnoreSpace: boolean; aIgnoreCase: boolean): pointer;
var
  i: integer;
  s: string;
begin
  s:=aLine;
  (* usuwam wszystkie powtarzające się spacje *)
  if aTrimSpace then while pos('  ',s)>0 do s:=StringReplace(s,'  ',' ',[rfReplaceAll]);
  (* usuwam wszystkie spacje i znaki tabulacji *)
  if aIgnoreSpace then
  begin
    s:='';
    for i:=1 to length(aLine) do if not (aLine[i] in [#9,#32]) then s:=s+aLine[i];
  end;
  (* ignoruję wielkości liter *)
  if aIgnoreCase then s:=AnsiLowerCase(s);
  //result:=pointer(CalcCRC32(pchar(s),length(s)));
  result:=pointer(CrcString(s));
end;

function TExtDiff.Diff(aFile1, aFile2: TStrings; aFName1, aFName2: string;
  aDTime1, aMSec1, aDTime2, aMSec2: qword; aDiff: TStrings): integer;
var
  ll1,ll2: TList;
  cc: TCompareRec;
  flaga: string[1];
  i,i1,i2,a,x: integer;
  strefa: string;
  pp: TList;
  element: TElement;
begin
  i:=round((GetLocalTimeOffset*-1)/60);
  if i>0 then strefa:='+' else strefa:='-';
  if i<10 then strefa:=strefa+'0'+IntToStr(i) else strefa:=strefa+IntToStr(i);
  strefa:=strefa+'00';

  ll1:=TList.Create;
  ll2:=TList.Create;
  pp:=TList.Create;
  try
    (* obliczam hashe *)
    StringsToHashList(aFile1,ll1);
    StringsToHashList(aFile2,ll2);
    (* wykonuję analizę *)
    execute(PINT(ll1.List),PINT(ll2.List),ll1.Count,ll2.Count);
    (* przelatuję dane *)
    for i:=0 to self.Count-1 do
    begin
      cc:=self.Compares[i];
      i1:=cc.oldIndex1;
      i2:=cc.oldIndex2;
      case cc.Kind of
        ckNone:   flaga:='N';
        ckAdd:    flaga:='A';
        ckDelete: flaga:='D';
        ckModify: flaga:='M';
      end;
      element.flaga:=flaga[1];
      element.i1:=i1;
      element.i2:=i2;
      case cc.Kind of
        ckNone:   begin element.s:=' '+aFile1[i1]; ElementAdd(pp,element); end;
        ckAdd:    begin element.s:='+'+aFile2[i2]; ElementAdd(pp,element); end;
        ckDelete: begin element.s:='-'+aFile1[i1]; ElementAdd(pp,element); end;
        ckModify: begin
                    element.s:='-'+aFile1[i1]; ElementAdd(pp,element);
                    element.s:='+'+aFile2[i2]; ElementAdd(pp,element);
                  end;
      end;
    end;
    x:=aDiff.Add(DiffFileInfo('-',aFName1,aDTime1,aMSec1));
    aDiff.Add(DiffFileInfo('+',aFName2,aDTime2,aMSec2));
    a:=DiffAnalize(pp,aDiff);
    if a=0 then
    begin
      result:=0;
      aDiff.Delete(x);
      aDiff.Delete(x);
    end else result:=a;
  finally
    ll1.Free;
    ll2.Free;
    ElementClear(pp);
    pp.Free;
  end;
end;

function TExtDiff.Diff(aFileOld, aFileNew: string; aDiff: TStrings): integer;
var
  ff1,ff2: TStringList;
  {$IFDEF UNIX}
  info1,info2: stat;
  {$ENDIF}
begin
  ff1:=TStringList.Create;
  ff2:=TStringList.Create;
  try
    ff1.LoadFromFile(aFileOld);
    ff2.LoadFromFile(aFileNew);
    {$IFDEF UNIX}
    fpstat(aFileOld,info1);
    fpstat(aFileNew,info2);
    result:=Diff(ff1,ff2,aFileOld,aFileNew,info1.st_mtime,info1.st_mtime_nsec,info2.st_mtime,info2.st_mtime_nsec,aDiff);
    {$ELSE}
    result:=Diff(ff1,ff2,aFileOld,aFileNew,FileAge(aFileOld),0,FileAge(aFileNew),0,aDiff);
    {$ENDIF}
  finally
    ff1.Free;
    ff2.Free;
  end;
end;

procedure TExtDiff.DiffDirectory(aDir1, aDir2: TStrings; aDiff: TStrings);
var
  d1,d2: string;
  list1,list2,bin,f1,f2: TStringList;
  s1,s2: string;
  a,x: integer;
  b1,b2,bb: boolean;
  dtime1,msec1,dtime2,msec2: qword;
begin
  if (aDir1.Count=0) or (aDir2.Count=0) then exit;
  d1:=pierwsza_nazwa(aDir1[0]);
  d2:=pierwsza_nazwa(aDir2[0]);
  list1:=TStringList.Create;
  list2:=TStringList.Create;
  f1:=TStringList.Create;
  f2:=TStringList.Create;
  bin:=TStringList.Create;
  try
    list1.Assign(aDir1);
    list2.Assign(aDir2);
    (* pliki z listy 2 *)
    while list2.Count>0 do
    begin
      s2:=list2[0];
      s1:=druga_nazwa(d1,d2,s2,true);
      list2.Delete(0);
      a:=StringToItemIndex(list1,s1);
      if a=-1 then
      begin
        b2:=false;
        if assigned(FRequestFileIsBin) then FRequestFileIsBin(self,2,s2,b2);
        if b2 then
        begin
          bin.Add('Binarne pliki '+NormalizeFilePath(s1,true)+' i '+NormalizeFilePath(s2,true)+' różnią się');
          if assigned(FBinDiff) then FBinDiff(self,2,s2);
        end else begin
          aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2));
          DiffNewFile(s1,s2,aDiff,true);
        end;
      end else begin
        list1.Delete(a);
        if assigned(FRequestFileIsBin) then
        begin
          b1:=false;
          b2:=false;
          FRequestFileIsBin(self,1,s1,b1);
          FRequestFileIsBin(self,2,s2,b2);
        end;
        if b1 or b2 then
        begin
          bb:=false;
          if assigned(FRequestDiffBinFiles) then FRequestDiffBinFiles(self,s1,s2,bb);
          if bb then
          begin
            bin.Add('Binarne pliki '+NormalizeFilePath(s1,true)+' i '+NormalizeFilePath(s2,true)+' różnią się');
            if assigned(FBinDiff) then FBinDiff(self,2,s2);
          end;
        end else begin
          x:=aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2,true));
          if assigned(FRequestFileBody) then
          begin
            FRequestFileBody(self,1,s1,f1);
            FRequestFileBody(self,2,s2,f2);
          end;
          if assigned(FRequestFileAttrib) then
          begin
            FRequestFileAttrib(self,1,s1,dtime1,msec1);
            FRequestFileAttrib(self,2,s2,dtime2,msec2);
          end;
          if Diff(f1,f2,s1,s2,dtime1,msec1,dtime2,msec2,aDiff)=0 then aDiff.Delete(x);
        end;
      end;
    end;
    (* pliki z listy 1 *)
    while list1.Count>0 do
    begin
      s1:=list1[0];
      s2:=druga_nazwa(d1,d2,s1);
      list1.Delete(0);
      a:=StringToItemIndex(list2,s2);
      if a=-1 then
      begin

        b1:=false;
        if assigned(FRequestFileIsBin) then FRequestFileIsBin(self,1,s1,b1);
        if b1 then
        begin
          bin.Add('Binarne pliki '+NormalizeFilePath(s1,true)+' i '+NormalizeFilePath(s2,true)+' różnią się');
          if assigned(FBinDiff) then FBinDiff(self,1,s1);
        end else begin
          aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2,true));
          DiffFileDeleted(s1,s2,aDiff,true);
        end;

      end else begin
        list2.Delete(a);
        b1:=false;
        b2:=false;
        FRequestFileIsBin(self,1,s1,b1);
        FRequestFileIsBin(self,2,s2,b2);
        if b1 or b2 then
        begin
          bb:=false;
          if assigned(FRequestDiffBinFiles) then FRequestDiffBinFiles(self,s1,s2,bb);
          if bb then
          begin
            bin.Add('Binarne pliki '+NormalizeFilePath(s1,true)+' i '+NormalizeFilePath(s2,true)+' różnią się');
            if assigned(FBinDiff) then FBinDiff(self,2,s2);
          end;
        end else begin
          x:=aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2,true));
          if assigned(FRequestFileBody) then
          begin
            FRequestFileBody(self,1,s1,f1);
            FRequestFileBody(self,2,s2,f2);
          end;
          if assigned(FRequestFileAttrib) then
          begin
            FRequestFileAttrib(self,1,s1,dtime1,msec1);
            FRequestFileAttrib(self,2,s2,dtime2,msec2);
          end;
          if Diff(f1,f2,s1,s2,dtime1,msec1,dtime2,msec2,aDiff)=0 then aDiff.Delete(x);
        end;
      end;
    end;
    for a:=0 to bin.Count-1 do aDiff.Add(bin[a]);
  finally
    list1.Free;
    list2.Free;
    f1.Free;
    f2.Free;
    bin.Free;
  end;
end;

procedure TExtDiff.DiffDirectory(aDirOld, aDirNew: string; aDiff: TStrings);
var
  list1,list2,bin: TStringList;
  s1,s2: string;
  a: integer;
  b1,b2: boolean;
begin
  list1:=TStringList.Create;
  list2:=TStringList.Create;
  bin:=TStringList.Create;
  try
    FindAllFiles(list1,aDirOld);
    FindAllFiles(list2,aDirNew);
    (* pliki z listy 2 *)
    while list2.Count>0 do
    begin
      s2:=list2[0];
      s1:=druga_nazwa(aDirOld,aDirNew,s2,true);
      list2.Delete(0);
      a:=StringToItemIndex(list1,s1);
      if a=-1 then
      begin
        b2:=IsBinaryFile(s2);
        if b2 then
        begin
          bin.Add('Binarne pliki '+NormalizeFilePath(s1,true)+' i '+NormalizeFilePath(s2,true)+' różnią się');
          if assigned(FBinDiff) then FBinDiff(self,2,s2);
        end else begin
          aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2,true));
          DiffNewFile(s1,s2,aDiff);
        end;
      end else begin
        b1:=IsBinaryFile(s1);
        b2:=IsBinaryFile(s2);
        if b1 or b2 then
        begin
          if GetHashFile(s1)<>GetHashFile(s2) then
          begin
            bin.Add('Binarne pliki '+NormalizeFilePath(s1,true)+' i '+NormalizeFilePath(s2,true)+' różnią się');
            if assigned(FBinDiff) then FBinDiff(self,2,s2);
          end;
        end else begin
          list1.Delete(a);
          aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2,true));
          Diff(s1,s2,aDiff);
        end;
      end;
    end;
    (* pliki z listy 1 *)
    while list1.Count>0 do
    begin
      s1:=list1[0];
      s2:=druga_nazwa(aDirOld,aDirNew,s1);
      list1.Delete(0);
      a:=StringToItemIndex(list2,s2);
      if a=-1 then
      begin
        aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2,true));
        DiffFileDeleted(s1,s2,aDiff);
      end else begin
        b1:=IsBinaryFile(s1);
        b2:=IsBinaryFile(s2);
        if b1 or b2 then
        begin
          if GetHashFile(s1)<>GetHashFile(s2) then
          begin
            bin.Add('Binarne pliki '+NormalizeFilePath(s1,true)+' i '+NormalizeFilePath(s2,true)+' różnią się');
            if assigned(FBinDiff) then FBinDiff(self,2,s2);
          end;
        end else begin
          list2.Delete(a);
          aDiff.Add('diff -ruN '+NormalizeFilePath(s1,true)+' '+NormalizeFilePath(s2,true));
          Diff(s1,s2,aDiff);
        end;
      end;
    end;
    for a:=0 to bin.Count-1 do aDiff.Add(bin[a]);
  finally
    list1.Free;
    list2.Free;
    bin.Free;
  end;
end;

procedure TExtDiff.Patch(aFileDiff, aFile: TStrings);
var
  f1_name,f2_name: string;
  f1_dt,f2_dt: TDateTime;
  kompozyt: boolean;
  i,j,wektor: integer;
  s: string;
  a1,a2,b1,b2: integer;
begin
  wektor:=0;
  kompozyt:=pos('diff -ruN',aFileDiff[0])>0;
  for i:=0 to aFileDiff.Count-1 do
  begin
    s:=aFileDiff[i];
    if pos('---',s)=1 then dane_pliku(s,f1_name,f1_dt) else
    if pos('+++',s)=1 then dane_pliku(s,f2_name,f2_dt) else
    if pos('@@',s)=1 then
    begin
      dane_pliku(s,a1,a2,b1,b2);
      patch_go(aFileDiff,aFile,i+1,a1,a2,b1,b2,wektor);
    end;
  end;
end;

procedure TExtDiff.Patch(aFileDiff, aFile: string);
var
  d,f: TStringList;
begin
  d:=TStringList.Create;
  f:=TStringList.Create;
  try
    d.LoadFromFile(aFileDiff);
    f.LoadFromFile(aFile);
    Patch(d,f);
    f.SaveToFile(aFile);
  finally
    d.Free;
    f.Free;
  end;
end;

procedure TExtDiff.PatchDirectory(aFileDiff: TStrings);
var
  f: TStringList;
  i,wektor: integer;
  s,pom: string;
  kompozyt: boolean;
  plik,plik2: string;
  f1_name,f2_name: string;
  f1_dt,f2_dt: TDateTime;
  a1,a2,b1,b2: integer;
begin
  f:=TStringList.Create;
  try
    for i:=0 to aFileDiff.Count-1 do
    begin
      s:=aFileDiff[i];
      kompozyt:=pos('diff -ruN',s)=1;
      if kompozyt then
      begin
        if f.Count>0 then
        begin
          if assigned(FRequestSaveFile) then FRequestSaveFile(self,2,plik2,f);
          f.Clear;
        end;
        plik:=NormalizeFilePath(GetLineToStr(s,3,' '));
        plik2:=NormalizeFilePath(GetLineToStr(s,4,' '));
        wektor:=0;
        continue;
      end;
      if pos('---',s)=1 then dane_pliku(s,f1_name,f1_dt) else
      if pos('+++',s)=1 then dane_pliku(s,f2_name,f2_dt) else
      if pos('@@',s)=1 then
      begin
        dane_pliku(s,a1,a2,b1,b2);
        f.Clear;
        if assigned(FRequestFileBody) then FRequestFileBody(self,2,plik2,f);
        if (a1=0) and (a2=0) and (b1=0) and (b2=0) then
        begin
          if assigned(FRequestDeleteFile) then FRequestDeleteFile(self,2,plik2);
          f.Clear;
        end else patch_go(aFileDiff,f,i+1,a1,a2,b1,b2,wektor);
      end else
      if pos('B',s)=1 then
      begin
        pom:=NormalizeFilePath(GetLineToStr(s,5,' '));
        if assigned(FBinPatch) then FBinPatch(self,2,pom);
      end;
    end;
    if f.Count>0 then if assigned(FRequestSaveFile) then FRequestSaveFile(self,2,plik2,f);
  finally
    f.Free;
  end;
end;

procedure TExtDiff.PatchDirectory(aFileDiff, aDir: string);
var
  d,f: TStringList;
  i,wektor: integer;
  s,pom,pom2,pom3: string;
  kompozyt: boolean;
  plik,plik2: string;
  f1_name,f2_name: string;
  f1_dt,f2_dt: TDateTime;
  a1,a2,b1,b2: integer;
begin
  d:=TStringList.Create;
  f:=TStringList.Create;
  try
    d.LoadFromFile(aFileDiff);
    for i:=0 to d.Count-1 do
    begin
      s:=d[i];
      kompozyt:=pos('diff -ruN',s)=1;
      if kompozyt then
      begin
        if f.Count>0 then
        begin
          f.SaveToFile(plik2);
          f.Clear;
        end;
        plik:=NormalizeFilePath(GetLineToStr(s,3,' '));
        plik:=druga_nazwa(plik);
        {$IFDEF UNIX}
        plik2:=aDir+'/'+plik;
        plik2:=StringReplace(plik2,'//','/',[]);
        {$ELSE}
        plik2:=aDir+'\'+plik;
        plik2:=StringReplace(plik2,'\\','\',[]);
        {$ENDIF}
        plik2:=NormalizeFilePath(plik2); //na wszelki wypadek
        wektor:=0;
        continue;
      end;
      if pos('---',s)=1 then dane_pliku(s,f1_name,f1_dt) else
      if pos('+++',s)=1 then dane_pliku(s,f2_name,f2_dt) else
      if pos('@@',s)=1 then
      begin
        dane_pliku(s,a1,a2,b1,b2);
        if FileExists(plik2) then f.LoadFromFile(plik2);
        if (a1=0) and (a2=0) and (b1=0) and (b2=0) then
        begin
          DeleteFile(plik2);
          f.Clear;
        end else patch_go(d,f,i+1,a1,a2,b1,b2,wektor);
      end else
      if pos('B',s)=1 then
      begin
        pom:=NormalizeFilePath(GetLineToStr(s,5,' '));
        pom2:=druga_nazwa(pom);
        {$IFDEF UNIX}
        pom3:=aDir+'/'+pom2;
        pom3:=StringReplace(pom3,'//','/',[]);
        {$ELSE}
        pom3:=aDir+'\'+pom2;
        pom3:=StringReplace(pom3,'\\','\',[]);
        {$ENDIF}
        pom3:=NormalizeFilePath(pom3); //tak na wszelki wypadek
        if assigned(FBinPatch) then FBinPatch(self,2,pom3);
      end;
    end;
    if f.Count>0 then
    begin
      f.SaveToFile(plik2);
      f.Clear;
    end;
  finally
    d.Free;
    f.Free;
  end;
end;

end.
