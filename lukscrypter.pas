unit LuksCrypter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

type

  { TLuksCrypter }

  TLuksCrypterUnits = (ucBajt,ucKiloBajt,ucMegaBajt,ucGigaBajt,ucTeraBajt);
  TLuksCrypterOperations = (ocOpen,ocClose,ocMount,ocUmount,ocOpenMount,ocUmountClose);
  TLuksCrypterOnBeforeAfterExec = procedure(aSender: TObject; aOperation: TLuksCrypterOperations) of object;
  TLuksCrypter = class(TComponent)
  private
    FOnBeforeExec,FOnAfterExec: TLuksCrypterOnBeforeAfterExec;
    licznik: integer;
    nazwy,opisy,pliki,katalogi: TStrings;
    keyfile: string;
    function ImageToIndex(aName: string): integer;
    function AddImage(aName,aOpis,aFileName: string): integer;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DestroyAll(const aSudoPassword: string): boolean;
    function Count: integer;
    function OpisToNazwa(aOpis: string): string;
    function FileToImage(aFileName: string): string;
    function DirectoryToImage(aDirectory: string): string;
    procedure PassOpen(const aPassword: string);
    procedure PassClose;
    function AddDirectory(aName,aDirectory: string): boolean;
    function AddDirectory(aIndex: integer; aDirectory: string): boolean;
    function DelDirectory(aName: string): boolean;
    function DelDirectory(aIndex: integer): boolean;
    procedure DeleteImage(aName: string);
    function IsImage(aName,aFileName: string): boolean;
    function IsFileName(aFileName: string): boolean;
    function CreateFile(aFileName: string; aSize: cardinal; aUnit: TLuksCrypterUnits): boolean;
    function FormatFile(aFileName: string; const aSudoPassword: string): boolean;
    function Open(aName,aOpis,aFileName: string; const aSudoPassword: string): boolean;
    function Close(aName: string; const aSudoPassword: string): boolean;
    function FormatImage(aName,aFsType: string; const aSudoPassword: string): boolean;
    function Mount(aName,aDirectory: string; const aSudoPassword: string): boolean;
    function Umount(aName: string; const aSudoPassword: string): boolean;
    function OpenAndMount(aName,aOpis,aFileName,aDirectory: string; const aSudoPassword: string): boolean;
    function UmountAndClose(aName: string; const aSudoPassword: string): boolean;
    function AddVirtualIndex(aName,aFileName,aDirectory: string): integer;
    function AddVirtualIndex(aName,aOpis,aFileName,aDirectory: string): integer;
  published
    property OnBeforeExec: TLuksCrypterOnBeforeAfterExec read FOnBeforeExec write FOnBeforeExec;
    property OnAfterExec: TLuksCrypterOnBeforeAfterExec read FOnAfterExec write FOnAfterExec;
  end;

procedure Register;

implementation

uses
  Process;

procedure Register;
begin
  {$I lukscrypter_icon.lrs}
  RegisterComponents('System',[TLuksCrypter]);
end;

{ TLuksCrypter }

function TLuksCrypter.ImageToIndex(aName: string): integer;
var
  i: integer;
begin
  result:=-1;
  for i:=0 to nazwy.Count-1 do if nazwy[i]=aName then
  begin
    result:=i;
    break;
  end;
end;

constructor TLuksCrypter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  licznik:=0;
  keyfile:='';
  nazwy:=TStringList.Create;
  opisy:=TStringList.Create;
  pliki:=TStringList.Create;
  katalogi:=TStringList.Create;
end;

destructor TLuksCrypter.Destroy;
begin
  PassClose;
  nazwy.Free;
  opisy.Free;
  pliki.Free;
  katalogi.Free;
  inherited Destroy;
end;

function TLuksCrypter.DestroyAll(const aSudoPassword: string): boolean;
var
  b: boolean;
  i: integer;
  nazwa,katalog: string;
begin
  b:=nazwy.Count>0;
  for i:=nazwy.Count-1 downto 0 do
  begin
    nazwa:=nazwy[i];
    katalog:=katalogi[i];
    if katalog<>'' then Umount(nazwa,aSudoPassword);
    Close(nazwa,aSudoPassword);
  end;
  result:=b;
end;

function TLuksCrypter.Count: integer;
begin
  result:=nazwy.Count;
end;

function TLuksCrypter.OpisToNazwa(aOpis: string): string;
var
  i: integer;
begin
  result:='';
  for i:=0 to opisy.Count-1 do if opisy[i]=aOpis then
  begin
    result:=nazwy[i];
    break;
  end;
end;

function TLuksCrypter.FileToImage(aFileName: string): string;
var
  i: integer;
begin
  result:='';
  for i:=0 to nazwy.Count-1 do if pliki[i]=aFileName then
  begin
    result:=nazwy[i];
    break;
  end;
end;

function TLuksCrypter.DirectoryToImage(aDirectory: string): string;
var
  i: integer;
begin
  result:='';
  for i:=0 to nazwy.Count-1 do if katalogi[i]=aDirectory then
  begin
    result:=nazwy[i];
    break;
  end;
end;

procedure TLuksCrypter.PassOpen(const aPassword: string);
var
  vKey: TGuid;
  ss: TStringList;
begin
  if keyfile<>'' then PassClose;
  CreateGUID(vKey);
  keyfile:='/tmp/'+GUIDToString(vKey)+'.dat';
  ss:=TStringList.Create;
  try
    ss.Add(aPassword);
    ss.SaveToFile(keyfile);
  finally
    ss.Free;
  end;
end;

procedure TLuksCrypter.PassClose;
var
  i: integer;
  ss: TStringList;
  s: string;
begin
  if keyfile='' then exit;
  if FileExists(keyfile) then
  begin
    ss:=TStringList.Create;
    try
      s:='';
      for i:=1 to length(keyfile) do s:=s+#0;
      ss.Add(s);
      ss.SaveToFile(keyfile);
    finally
      ss.Free
    end;
    DeleteFile(keyfile);
  end;
  FillChar(keyfile[1],length(keyfile),0);
  keyfile:='';
end;

function TLuksCrypter.AddImage(aName, aOpis, aFileName: string): integer;
var
  a: integer;
begin
  a:=nazwy.Add(aName);
  opisy.Add(aOpis);
  pliki.Add(aFileName);
  katalogi.Add('');
  result:=a;
end;

function TLuksCrypter.AddDirectory(aName, aDirectory: string): boolean;
var
  a: integer;
begin
  a:=ImageToIndex(aName);
  if a=-1 then result:=false else
  begin
    katalogi.Delete(a);
    katalogi.Insert(a,aDirectory);
    result:=true;
  end;
end;

function TLuksCrypter.AddDirectory(aIndex: integer; aDirectory: string
  ): boolean;
begin
  if aIndex>nazwy.Count-1 then result:=false else
  begin
    katalogi.Delete(aIndex);
    katalogi.Insert(aIndex,aDirectory);
    result:=true;
  end;
end;

function TLuksCrypter.DelDirectory(aName: string): boolean;
var
  a: integer;
begin
  a:=ImageToIndex(aName);
  if a=-1 then result:=false else
  begin
    katalogi.Delete(a);
    katalogi.Insert(a,'');
    result:=true;
  end;
end;

function TLuksCrypter.DelDirectory(aIndex: integer): boolean;
begin
  if aIndex>nazwy.Count-1 then result:=false else
  begin
    katalogi.Delete(aIndex);
    katalogi.Insert(aIndex,'');
    result:=true;
  end;
end;

procedure TLuksCrypter.DeleteImage(aName: string);
var
  i,a: integer;
begin
  a:=-1;
  for i:=0 to nazwy.Count-1 do if nazwy[i]=aName then
  begin
    a:=i;
    break;
  end;
  if a>-1 then
  begin
    nazwy.Delete(a);
    opisy.Delete(a);
    pliki.Delete(a);
    katalogi.Delete(a);
  end;
end;

function TLuksCrypter.IsImage(aName, aFileName: string): boolean;
var
  i: integer;
  b1,b2: boolean;
begin
  b1:=false;
  b2:=false;
  for i:=0 to nazwy.Count-1 do if nazwy[i]=aName then
  begin
    b1:=true;
    break;
  end;
  for i:=0 to pliki.Count-1 do if pliki[i]=aFileName then
  begin
    b2:=true;
    break;
  end;
  result:=b1 or b2;
end;

function TLuksCrypter.IsFileName(aFileName: string): boolean;
var
  i: integer;
  b: boolean;
begin
  b:=false;
  for i:=0 to pliki.Count-1 do if pliki[i]=aFileName then
  begin
    b:=true;
    break;
  end;
  result:=b;
end;

function TLuksCrypter.CreateFile(aFileName: string; aSize: cardinal;
  aUnit: TLuksCrypterUnits): boolean;
var
  p: TProcess;
  b: boolean;
  s: string;
begin
  case aUnit of
    ucBajt: s:='';
    ucKiloBajt: s:='KB';
    ucMegaBajt: s:='MB';
    ucGigaBajt: s:='GB';
    ucTeraBajt: s:='TB';
  end;
  p:=TProcess.Create(nil);
  try
    p.Options:=[poWaitOnExit];
    p.Executable:='/bin/sh';
    p.Parameters.Add('-c');    //sudo fallocate -l 1GB kontener.luks
    p.Parameters.Add('fallocate -l '+IntToStr(aSize)+s+' '+aFileName);
    p.Execute;
    b:=p.ExitStatus=0;
  finally
    p.Terminate(0);
    p.Free;
  end;
  sleep(200);
  result:=b and FileExists(aFileName);
end;

function TLuksCrypter.FormatFile(aFileName: string; const aSudoPassword: string
  ): boolean;
var
  p: TProcess;
  b: boolean;
begin
  result:=false;
  if keyfile='' then exit;
  p:=TProcess.Create(nil);
  try
    p.Options:=[poWaitOnExit];
    p.Executable:='/bin/sh';
    p.Parameters.Add('-c');
    p.Parameters.Add('echo '+aSudoPassword+' | sudo -S cryptsetup luksFormat '+aFileName+' --key-file '+keyfile);
    p.Execute;
    b:=p.ExitStatus=0;
  finally
    p.Terminate(0);
    p.Free;
  end;
  sleep(200);
  result:=b;
end;

function TLuksCrypter.Open(aName, aOpis, aFileName: string;
  const aSudoPassword: string): boolean;
var
  wpis: string;
  p: TProcess;
  b: boolean;
begin
  if licznik=0 then if assigned(FOnBeforeExec) then FOnBeforeExec(self,ocOpen);
  inc(licznik);
  try
    result:=false;
    if keyfile='' then exit;
    wpis:=aName+','+aFileName;
    if IsImage(aName,aFileName) then exit;
    p:=TProcess.Create(nil);
    try
      p.Options:=[poWaitOnExit];
      p.Executable:='/bin/sh';
      p.Parameters.Add('-c');
      p.Parameters.Add('echo '+aSudoPassword+' | sudo -S cryptsetup luksOpen '+aFileName+' '+aName+' --key-file '+keyfile);
      p.Execute;
      b:=p.ExitStatus=0;
    finally
      p.Terminate(0);
      p.Free;
    end;
    if b then AddImage(aName,aOpis,aFileName);
    result:=b;
  finally
    dec(licznik);
    if licznik=0 then if assigned(FOnAfterExec) then FOnAfterExec(self,ocOpen);
  end;
end;

function TLuksCrypter.Close(aName: string; const aSudoPassword: string
  ): boolean;
var
  p: TProcess;
  b: boolean;
begin
  if licznik=0 then if assigned(FOnBeforeExec) then FOnBeforeExec(self,ocClose);
  inc(licznik);
  try
    p:=TProcess.Create(nil);
    try
      p.Options:=[poWaitOnExit];
      p.Executable:='/bin/sh';
      p.Parameters.Add('-c');
      p.Parameters.Add('echo '+aSudoPassword+' | sudo -S cryptsetup luksClose '+aName);
      p.Execute;
      b:=p.ExitStatus=0;
    finally
      p.Terminate(0);
      p.Free;
    end;
    if b then DeleteImage(aName);
    result:=b;
  finally
    dec(licznik);
    if licznik=0 then if assigned(FOnAfterExec) then FOnAfterExec(self,ocClose);
  end;
end;

function TLuksCrypter.FormatImage(aName, aFsType: string;
  const aSudoPassword: string): boolean;
var
  p: TProcess;
  b: boolean;
begin
  if licznik=0 then if assigned(FOnBeforeExec) then FOnBeforeExec(self,ocClose);
  inc(licznik);
  try
    p:=TProcess.Create(nil);
    try
      p.Options:=[poWaitOnExit];
      p.Executable:='/bin/sh';
      p.Parameters.Add('-c');
      p.Parameters.Add('echo '+aSudoPassword+' | sudo -S mkfs.'+aFsType+' /dev/mapper/'+aName);
      p.Execute;
      b:=p.ExitStatus=0;
    finally
      p.Terminate(0);
      p.Free;
    end;
    result:=b;
  finally
    dec(licznik);
    if licznik=0 then if assigned(FOnAfterExec) then FOnAfterExec(self,ocClose);
  end;
end;

function TLuksCrypter.Mount(aName, aDirectory: string;
  const aSudoPassword: string): boolean;
var
  p: TProcess;
  index: integer;
  b: boolean;
begin
  if licznik=0 then if assigned(FOnBeforeExec) then FOnBeforeExec(self,ocMount);
  inc(licznik);
  try
    index:=ImageToIndex(aName);
    if index=-1 then exit;
    p:=TProcess.Create(nil);
    try
      p.Options:=[poWaitOnExit];
      p.Executable:='/bin/sh';
      p.Parameters.Add('-c');
      p.Parameters.Add('echo '+aSudoPassword+' | sudo -S mount /dev/mapper/'+aName+' '+aDirectory);
      p.Execute;
      b:=p.ExitStatus=0;
    finally
      p.Terminate(0);
      p.Free;
    end;
    if b then AddDirectory(index,aDirectory);
    result:=b;
  finally
    dec(licznik);
    if licznik=0 then if assigned(FOnAfterExec) then FOnAfterExec(self,ocMount);
  end;
end;

function TLuksCrypter.Umount(aName: string; const aSudoPassword: string
  ): boolean;
var
  p: TProcess;
  ss: TStringList;
  index: integer;
  katalog: string;
begin
  if licznik=0 then if assigned(FOnBeforeExec) then FOnBeforeExec(self,ocUmount);
  inc(licznik);
  try
    result:=false;
    index:=ImageToIndex(aName);
    if index=-1 then exit;
    katalog:=katalogi[index];
    (* exec tools *)
    p:=TProcess.Create(nil);
    ss:=TStringList.Create;
    try
      p.Options:=[poWaitOnExit,poUsePipes];
      p.Executable:='/bin/sh';
      p.Parameters.Add('-c');
      p.Parameters.Add('echo '+aSudoPassword+' | sudo -S umount '+katalog);
      p.Execute;
      if not p.ExitStatus=0 then exit;
      if p.Stderr.NumBytesAvailable>0 then
      begin
        ss.LoadFromStream(p.Stderr);
        if pos('cel jest zajęty',ss.Text)>0 then
        begin
          result:=false;
          exit;
        end;
      end else begin
        DelDirectory(index);
        result:=true;
      end;
    finally
      ss.Free;
      p.Terminate(0);
      p.Free;
    end;
  finally
    dec(licznik);
    if licznik=0 then if assigned(FOnAfterExec) then FOnAfterExec(self,ocUmount);
  end;
end;

function TLuksCrypter.OpenAndMount(aName, aOpis, aFileName, aDirectory: string;
  const aSudoPassword: string): boolean;
var
  b: boolean;
begin
  if licznik=0 then if assigned(FOnBeforeExec) then FOnBeforeExec(self,ocOpenMount);
  inc(licznik);
  try
    b:=Open(aName,aOpis,aFileName,aSudoPassword);
    if b then b:=Mount(aName,aDirectory,aSudoPassword);
    result:=b;
  finally
    dec(licznik);
    if licznik=0 then if assigned(FOnAfterExec) then FOnAfterExec(self,ocOpenMount);
  end;
end;

function TLuksCrypter.UmountAndClose(aName: string; const aSudoPassword: string
  ): boolean;
var
  b: boolean;
begin
  if licznik=0 then if assigned(FOnBeforeExec) then FOnBeforeExec(self,ocUmountClose);
  inc(licznik);
  try
    Umount(aName,aSudoPassword);
    b:=Close(aName,aSudoPassword);
    result:=b;
  finally
    dec(licznik);
    if licznik=0 then if assigned(FOnAfterExec) then FOnAfterExec(self,ocUmountClose);
  end;
end;

function TLuksCrypter.AddVirtualIndex(aName, aFileName, aDirectory: string
  ): integer;
var
  a: integer;
begin
  a:=nazwy.Add(aName);
  opisy.Add('');
  pliki.Add(aFileName);
  katalogi.Add(aDirectory);
  result:=a;
end;

function TLuksCrypter.AddVirtualIndex(aName, aOpis, aFileName,
  aDirectory: string): integer;
var
  a: integer;
begin
  a:=nazwy.Add(aName);
  opisy.Add(aOpis);
  pliki.Add(aFileName);
  katalogi.Add(aDirectory);
  result:=a;
end;

end.
