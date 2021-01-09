unit DirectoryPack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

type
  TDirectoryPackMode = (pmServer, pmClient);
  TDirectoryPackOnExecute = procedure(Sender: TObject; AFolders, AFiles: TStringList; ASignature: string) of object;
  TDirectoryPackOnPackRecord = procedure(Sender: TObject; APackRecord, ASignature: string) of object;

  { TDirectoryPack }

  TDirectoryPack = class(TComponent)
  private
    __F: char;
    FMode: TDirectoryPackMode;
    FOnExecute: TDirectoryPackOnExecute;
    FOnPackRecord: TDirectoryPackOnPackRecord;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetSeparator(aSeparator: char = #0);
    procedure Execute(aPath, aFilter: string; aSignature: string = '');
    procedure Execute(aPackRecord: string; aSignature: string = '');
    procedure Execute(aPath, aFilter: string; var aKatalogi,aPliki: TStrings);
    procedure ExecuteDirs(aPath, aFilter: string; var aKatalogi: TStrings);
    procedure ExecuteFiles(aPath, aFilter: string; var aPliki: TStrings);
  published
    property Mode: TDirectoryPackMode read FMode write FMode;
    property OnExecute: TDirectoryPackOnExecute read FOnExecute write FOnExecute;
    property OnPackRecord: TDirectoryPackOnPackRecord read FOnPackRecord write FOnPackRecord;
  end;

procedure Register;

implementation

uses
  ecode_unit;

const
  {$IFDEF WINDOWS}
  ALL = '*.*';
  _FF = '\';
  {$ELSE}
  ALL = '*';
  _FF = '/';
  {$ENDIF}

procedure Register;
begin
  {$I directorypack_icon.lrs}
  RegisterComponents('Common Controls',[TDirectoryPack]);
end;

{ TDirectoryPack }

constructor TDirectoryPack.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetSeparator;
  FMode:=pmServer;
end;

destructor TDirectoryPack.Destroy;
begin
  inherited Destroy;
end;

procedure TDirectoryPack.SetSeparator(aSeparator: char);
begin
  if aSeparator=#0 then __F:=_FF else __F:=aSeparator;
end;

procedure TDirectoryPack.Execute(aPath, aFilter: string; aSignature: string);
var
  katalogi,pliki: TStringList;
  SR: TSearchRec;
  found: Integer;
  s,s1,s2,spack: string;
  i: integer;
begin
  katalogi:=TStringList.Create;
  pliki:=TStringList.Create;
  try
    s1:=aPath;
    {katalogi}
    found:=FindFirst(s1+__F+ALL,faDirectory,SR);
    while found=0 do
    begin
      if (SR.Name<>'.') and (SR.Attr=48) then katalogi.Add(SR.Name);
      found:=FindNext(SR);
    end;
    FindClose(SR);
    {pliki}
    i:=1;
    while true do
    begin
      s2:=GetLineToStr(aFilter,i,';');
      if s2='' then break;
      s:=s1+__F+s2;
      found:=FindFirst(s,faNormal,SR);
      while found=0 do
      begin
        pliki.Add(SR.Name);
        found:=FindNext(SR);
      end;
      FindClose(SR);
      inc(i);
    end;
    if Assigned(FOnExecute) then FOnExecute(self,katalogi,pliki,aSignature);
    if Assigned(FOnPackRecord) then
    begin
      spack:=aPath+#9+aFilter+#9+IntToStr(katalogi.Count)+#9+IntToStr(pliki.Count);
      for i:=0 to katalogi.Count-1 do spack:=spack+#9+katalogi[i];
      for i:=0 to pliki.Count-1 do spack:=spack+#9+pliki[i];
      FOnPackRecord(self,spack,aSignature);
    end;
  finally
    katalogi.Free;
    pliki.Free;
  end;
end;

procedure TDirectoryPack.Execute(aPackRecord: string; aSignature: string);
var
  katalogi,pliki: TStringList;
  pom,s1,s2: string;
  a,b,i: integer;
begin
  s1:=GetLineToStr(aPackRecord,1,#9); //sciezka
  s2:=GetLineToStr(aPackRecord,2,#9); //filtr
  pom:=GetLineToStr(aPackRecord,3,#9);
  if pom='' then
  begin
    Execute(s1,s2,aSignature);
    exit;
  end;
  a:=StrToInt(pom); //ilość folderów
  b:=StrToInt(GetLineToStr(aPackRecord,4,#9)); //ilość plików

  katalogi:=TStringList.Create;
  pliki:=TStringList.Create;
  try
    for i:=5 to a+5-1 do katalogi.Add(GetLineToStr(aPackRecord,i,#9));
    for i:=5+a to a+b+5-1 do pliki.Add(GetLineToStr(aPackRecord,i,#9));
    if Assigned(FOnExecute) then FOnExecute(self,katalogi,pliki,aSignature);
  finally
    katalogi.Free;
    pliki.Free;
  end;
end;

procedure TDirectoryPack.Execute(aPath, aFilter: string; var aKatalogi,
  aPliki: TStrings);
begin
  ExecuteDirs(aPath,aFilter,aKatalogi);
  ExecuteFiles(aPath,aFilter,aPliki);
end;

procedure TDirectoryPack.ExecuteDirs(aPath, aFilter: string;
  var aKatalogi: TStrings);
var
  katalogi: TStrings;
  SR: TSearchRec;
  found: Integer;
  s,s1,spack: string;
  i: integer;
begin
  katalogi:=TStringList.Create;
  try
    s1:=aPath;
    {katalogi}
    s:=s1+__F+ALL;
    s:=StringReplace(s,__F+__F,__F,[]);
    found:=FindFirst(s,faDirectory,SR);
    while found=0 do
    begin
      if (SR.Name<>'.') and (SR.Attr=48) then katalogi.Add(SR.Name);
      found:=FindNext(SR);
    end;
    FindClose(SR);
    aKatalogi.Assign(katalogi);
  finally
    katalogi.Free;
  end;
end;

procedure TDirectoryPack.ExecuteFiles(aPath, aFilter: string;
  var aPliki: TStrings);
var
  pliki: TStringList;
  SR: TSearchRec;
  found: Integer;
  s,s1,s2,spack: string;
  i: integer;
begin
  pliki:=TStringList.Create;
  try
    s1:=aPath;
    {pliki}
    i:=1;
    while true do
    begin
      s2:=GetLineToStr(aFilter,i,';');
      if s2='' then break;
      s:=s1+__F+s2;
      s:=StringReplace(s,__F+__F,__F,[]);
      found:=FindFirst(s,faNormal,SR);
      while found=0 do
      begin
        pliki.Add(SR.Name);
        found:=FindNext(SR);
      end;
      FindClose(SR);
      inc(i);
    end;
    aPliki.Assign(pliki);
  finally
    pliki.Free;
  end;
end;

end.
