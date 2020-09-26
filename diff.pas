unit diff;

(*
Moduł przepisany z wersji pod Delphi, przepisałem ja Jacek L.
Niżej pod moim komentarzem dane autora tego modułu.

A tu historia moich zmian:
  22.09.2020 - przepisanie kodu do modułu pod Lazarusa i testy pod linuksem.
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

  TDiffAlgorithm = (daDifference,daSeqComparison);
  TDiffFileEngine = (fePascal,feMemory);

  { TDiff }

  TDiff = class(TComponent)
  private
    FAlg: TDiffAlgorithm;
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

    function DiffFileInfo(aFlaga: char; aFileName: string; aDT,aMS: qword): string;
    procedure DiffAnalize(pp: TList; aDiff: TStrings);
    function DiffSzukajPierwszejZmiany(aOd: integer; pp: TList): integer;
    procedure DiffNewFile(aFileNotFound,aFile: string; aDiff: TStrings);
    procedure DiffFileDeleted(aFile,aFileDeleted: string; aDiff: TStrings);

  protected
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure Diff(aFileOld,aFileNew: string; aDiff: TStrings);
    procedure DiffDirectories(aDirOld,aDirNew: string; aDiff: TStrings);

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
  end;

procedure Register;

implementation

uses
  Math, crc, BaseUnix, strutils, lazfileutils, fileutil;

type
  PElement = ^TElement;
  TElement = record
    flaga: char;
    s: string;
    i1,i2: integer;
  end;

procedure Register;
begin
  {$I diff_icon.lrs}
  RegisterComponents('Misc',[TDiff]);
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

function StringToItemIndex(slist: TStringList; kod: string; wart_domyslna: integer = -1): integer;
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

{ TDiff }

procedure TDiff.PushDiff(offset1, offset2, len1, len2: integer);
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

function TDiff.PopDiff: boolean;
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

procedure TDiff.DiffInt(offset1, offset2, len1, len2: integer);
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

procedure TDiff.DiffChr(offset1, offset2, len1, len2: integer);
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

function TDiff.SnakeChrF(k, offset1, offset2, len1, len2: integer): boolean;
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

function TDiff.SnakeChrB(k, offset1, offset2, len1, len2: integer): boolean;
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

function TDiff.SnakeIntF(k, offset1, offset2, len1, len2: integer): boolean;
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

function TDiff.SnakeIntB(k, offset1, offset2, len1, len2: integer): boolean;
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

procedure TDiff.AddChangeChr(offset1, range: integer; ChangeKind: TChangeKind);
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

procedure TDiff.AddChangeInt(offset1, range: integer; ChangeKind: TChangeKind);
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

procedure TDiff.InitDiagArrays(MaxOscill, len1, len2: integer);
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

procedure TDiff.InitDiagArrays(len1, len2: integer);
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

procedure TDiff.RecursiveDiffChr(offset1, offset2, len1, len2: integer);
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

procedure TDiff.AddChangeChrs(offset1, range: integer; ChangeKind: TChangeKind);
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

procedure TDiff.RecursiveDiffInt(offset1, offset2, len1, len2: integer);
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

procedure TDiff.AddChangeInts(offset1, range: integer; ChangeKind: TChangeKind);
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

function TDiff.GetCompare(index: integer): TCompareRec;
begin
  result:=PCompareRec(fCompareList[index])^;
end;

function TDiff.GetCompareCount: integer;
begin
  result:=fCompareList.count;
end;

function TDiff._ile_pozycji_o_tej_samej_fladze(aIndex: integer): integer;
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

function TDiff.DiffFileInfo(aFlaga: char; aFileName: string; aDT, aMS: qword
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
  s:=s+' '+aFileName;
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

procedure TDiff.DiffAnalize(pp: TList; aDiff: TStrings);
var
  element: TElement;
  max,indeks: integer;
  x,a,x1,x2,x3,x4: integer;
  zab: integer;
  s: string;
begin
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
    aDiff.Insert(indeks,s);

    x:=a+1;

  end;
end;

function TDiff.DiffSzukajPierwszejZmiany(aOd: integer; pp: TList): integer;
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

procedure TDiff.DiffNewFile(aFileNotFound, aFile: string; aDiff: TStrings);
var
  ff: TStringList;
  info: stat;
  i: integer;
begin
  fpstat(aFile,info);
  aDiff.Add(DiffFileInfo('-',aFileNotFound,0,0));
  aDiff.Add(DiffFileInfo('+',aFile,info.st_mtime,info.st_mtime_nsec));
  ff:=TStringList.Create;
  try
    ff.LoadFromFile(aFile);
    aDiff.Add('@@ -0,0 +1,'+IntToStr(ff.Count)+' @@');
    (* przelatuję dane *)
    for i:=0 to ff.Count-1 do aDiff.Add('+'+ff[i]);
  finally
    ff.Free;
  end;
end;

procedure TDiff.DiffFileDeleted(aFile, aFileDeleted: string; aDiff: TStrings);
var
  ff: TStringList;
  info: stat;
  strefa: string;
  i: integer;
begin
  fpstat(aFile,info);
  aDiff.Add(DiffFileInfo('-',aFile,info.st_mtime,info.st_mtime_nsec));
  aDiff.Add(DiffFileInfo('+',aFileDeleted,0,0));
  ff:=TStringList.Create;
  try
    ff.LoadFromFile(aFile);
    aDiff.Add('@@ -1 +0,0 @@');
    (* przelatuję dane *)
    for i:=0 to ff.Count-1 do aDiff.Add('-'+ff[i]);
  finally
    ff.Free;
  end;
end;

constructor TDiff.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAlg:=daDifference;
  FFileEngine:=feMemory;
  fCompareList:=TList.create;
  fDiffList:=TList.Create;
end;

destructor TDiff.Destroy;
begin
  Clear;
  fCompareList.free;
  fDiffList.Free;
  inherited Destroy;
end;

function TDiff.Execute(pints1, pints2: PINT; len1, len2: integer): boolean;
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

function TDiff.Execute(pchrs1, pchrs2: PChar; len1, len2: integer): boolean;
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

procedure TDiff.Cancel;
begin
  fCancelled:=true;
end;

procedure TDiff.Clear;
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

procedure TDiff.FileToHashList(aFile: string; aHashList: TList;
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

procedure TDiff.StringsToHashList(aStr: TStrings; aHashList: TList;
  aTrimSpace: boolean; aIgnoreSpace: boolean; aIgnoreCase: boolean);
var
  i: integer;
begin
  aHashList.Clear;
  for i:=0 to aStr.Count-1 do aHashList.Add(HashLine(aStr[i],aTrimSpace,aIgnoreSpace,aIgnoreCase));
end;

procedure TDiff.StringsToHashList(aStr: TStringList; aHashList: TList;
  aTrimSpace: boolean; aIgnoreSpace: boolean; aIgnoreCase: boolean);
var
  i: integer;
begin
  aHashList.Clear;
  for i:=0 to aStr.Count-1 do aHashList.Add(HashLine(aStr[i],aTrimSpace,aIgnoreSpace,aIgnoreCase));
end;

function TDiff.HashLine(const aLine: string; aTrimSpace: boolean;
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

procedure TDiff.Diff(aFileOld, aFileNew: string; aDiff: TStrings);
var
  ff1,ff2: TStringList;
  ll1,ll2: TList;
  cc: TCompareRec;
  flaga: string[1];
  i,i1,i2: integer;
  info1,info2: stat;
  strefa: string;
  pp: TList;
  element: TElement;
begin
  i:=round((GetLocalTimeOffset*-1)/60);
  if i>0 then strefa:='+' else strefa:='-';
  if i<10 then strefa:=strefa+'0'+IntToStr(i) else strefa:=strefa+IntToStr(i);
  strefa:=strefa+'00';
  fpstat(aFileOld,info1);
  fpstat(aFileNew,info2);
  ff1:=TStringList.Create;
  ff2:=TStringList.Create;
  ll1:=TList.Create;
  ll2:=TList.Create;
  pp:=TList.Create;
  try
    ff1.LoadFromFile(aFileOld);
    ff2.LoadFromFile(aFileNew);
    (* obliczam hashe *)
    StringsToHashList(ff1,ll1);
    StringsToHashList(ff2,ll2);
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
        ckNone:   begin element.s:=' '+ff1[i1]; ElementAdd(pp,element); end;
        ckAdd:    begin element.s:='+'+ff2[i2]; ElementAdd(pp,element); end;
        ckDelete: begin element.s:='-'+ff1[i1]; ElementAdd(pp,element); end;
        ckModify: begin
                    element.s:='-'+ff1[i1]; ElementAdd(pp,element);
                    element.s:='+'+ff2[i2]; ElementAdd(pp,element);
                  end;
      end;
    end;
    aDiff.Add(DiffFileInfo('-',aFileOld,info1.st_mtime,info1.st_mtime_nsec));
    aDiff.Add(DiffFileInfo('+',aFileNew,info2.st_mtime,info2.st_mtime_nsec));
    DiffAnalize(pp,aDiff);
  finally
    ff1.Free;
    ff2.Free;
    ll1.Free;
    ll2.Free;
    ElementClear(pp);
    pp.Free;
  end;
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

procedure TDiff.DiffDirectories(aDirOld, aDirNew: string; aDiff: TStrings);
var
  list1,list2: TStringList;
  s1,s2: string;
  a: integer;
begin
  list1:=TStringList.Create;
  list2:=TStringList.Create;
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
        aDiff.Add('diff -ruN '+s1+' '+s2);
        DiffNewFile(s1,s2,aDiff);
        //DiffFileDeleted(s1,s2,aDiff);
      end else begin
        list1.Delete(a);
        aDiff.Add('diff -ruN '+s1+' '+s2);
        Diff(s1,s2,aDiff);
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
        aDiff.Add('diff -ruN '+s1+' '+s2);
        DiffFileDeleted(s1,s2,aDiff);
      end else begin
        list2.Delete(a);
        aDiff.Add('diff -ruN '+s1+' '+s2);
        Diff(s1,s2,aDiff);
      end;
    end;
  finally
    list1.Free;
    list2.Free;
  end;
end;

end.
