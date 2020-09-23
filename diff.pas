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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

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

    property Cancelled: boolean read fCancelled;
    property Count: integer read GetCompareCount;
    property Compares[index: integer]: TCompareRec read GetCompare; default;
    property DiffStats: TDiffStats read fDiffStats;
  published
    {Algorytm działania:
      daDifference    - algorytm różnicowy Meyer'sa
      daSeqComparison - algorytm porównywania sekwencji}
    property Algorithm: TDiffAlgorithm read FAlg write FAlg default daDifference;
  end;

procedure Register;

implementation

uses
  Math;

procedure Register;
begin
  {$I diff_icon.lrs}
  RegisterComponents('Misc',[TDiff]);
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
        Application.ProcessMessages;
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
        Application.ProcessMessages;
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
        Application.ProcessMessages;
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
        Application.ProcessMessages;
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
  application.processmessages;
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
      application.processmessages;
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
  application.processmessages;
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
      application.processmessages;
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

constructor TDiff.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FAlg:=daDifference;
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

end.