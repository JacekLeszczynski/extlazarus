unit XmlParser;

{
  Komponent przelatuje caly plik XML i wyrzuca dane w odpowiedniej formie
  do przeczytania ze zdarzenia. Ewentualne bledy sa wyrzucane w drugim zdarzeniu.

  Autor: Jacek Leszczynski
  E-Mail: tao@bialan.pl

  Wszelkie prawa zastrzezone (C) Bialystok 2011, Polska.


  Instrukcja obslugi:
  1. Ustawiamy wlasciwosci FILENAME, ma wskazywac na plik XML do wczytania.
  2. Wypelniamy zdarzenie ONREAD odpowiednia wartoscia.
  3. Wypelniamy zdarzenie ONERROR odpowiednia wartoscia (zalecane).
  4. Uruchamiamy funckje EXECUTE, ktora zwroci TRUE jesli sie wykona, gdy wyjda
     jakies bledy, zostanie zwrocona wartosc FALSE!
}

{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}

{$IFNDEF FPC AND $IFDEF MSWINDOWS}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE LAZARUS}
{$ENDIF}

{$IFDEF LAZARUS}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  {$IFDEF LAZARUS}
  LResources,
  {$ENDIF}
  {$IFDEF LAZARUS}
  DOM, XMLRead, Zipper,
  {$ELSE}
  XmlDOM, XmlIntf, XmlDoc,
  {$ENDIF}
  DCPdes, DCPsha1;

type
  { zdarzenia }
  TBeforeAfterReadEvent = procedure(Sender: TObject) of object;
  TReadEvent = procedure(Sender: TObject; poziom: integer; adres,klucz,zmienna,wartosc: string; var Stopped:boolean) of object;
  TBranchEvent = procedure(Sender: TObject; poziom: integer; adres: string; var Stopped:boolean) of object;
  TErrorEvent = procedure(Sender: TObject; ERR: integer; ERROR: string) of object;
  TOnProgress = procedure(Sender: TObject; vMax, vPos: integer) of object;

  { typy wyliczeniowe }
  TContener = (crNone,crDES,crZIP);
  TEncodingFormat = (eAuto,eUTF8,eWindows1250,eISO_8859_2);

  { TXmlParser }

  TXmlParser = class(TComponent)
  private
    FContener: TContener;
    FOnProgress: TOnProgress;
    { Private declarations }
    strumien,strumien2,strumien_source: TMemoryStream;
    doc: TXMLDocument;
    temp: {$IFDEF LAZARUS} TDOMNode {$ELSE} IDOMNode {$ENDIF} ;
    __ERROR: integer;
    zm_filename: string;
    FES: TEncodingFormat;
    FNULL: boolean;
    FToken: String;
    FTest: boolean;
    FOnBeforeRead: TBeforeAfterReadEvent;
    FOnRead: TReadEvent;
    FOnBranchIn: TBranchEvent;
    FOnBranchBlock: TBranchEvent;
    FOnBranchOut: TBranchEvent;
    FOnAfterRead: TBeforeAfterReadEvent;
    FOnError: TErrorEvent;
    Des: TDCP_des;
    function OkreslStroneKodowa(str:TStream):integer;
    function EncryptStream(s_in,s_out:TStream;size:longword):longword;
    function DecryptStream(s_in,s_out:TStream;size:longword):longword;
    {$IFDEF LAZARUS}
    procedure proc_open(Sender: TObject; var AStream: TStream);
    procedure proc1(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure proc2(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    {$ENDIF}
  protected
    { Protected declarations }
    procedure ParseXML(level: integer; node: {$IFDEF LAZARUS} TDOMNode {$ELSE} IDOMNode {$ENDIF});
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetAlg01;
    procedure SetAlg02;
    function Execute: boolean; overload;
    function Execute(Stream:TStream): boolean; overload;
    function LockString(s:string;spaces:boolean=false):string;
    function UnlockString(s:string;spaces:boolean=false):string;
    function EncodeXML(XMLFile:string;EncodeFile:string=''):boolean; {$IFDEF DELPHI}overload;{$ENDIF}
    function EncodeXML:boolean; {$IFDEF DELPHI}overload;{$ENDIF}
    function DecodeXML(DecodeFile:string;XMLFile:string=''):boolean; {$IFDEF DELPHI}overload;{$ENDIF}
    function DecodeXML:boolean; {$IFDEF DELPHI}overload;{$ENDIF}
  published
    { Published declarations }
    property Contener: TContener read FContener write FContener default crNone;
    property Encoding: TEncodingFormat read FES write FES default eAuto;
    property Filename: string read zm_filename write zm_filename;
    property LastNullRead: boolean read FNULL write FNULL default false;
    property Token: String read FToken write FToken;
    property HeaderTest: boolean read FTest write FTest default true;
    property OnBeforeRead: TBeforeAfterReadEvent read FOnBeforeRead write FOnBeforeRead;
    property OnRead: TReadEvent read FOnRead write FOnRead;
    property OnAfterRead: TBeforeAfterReadEvent read FOnAfterRead write FOnAfterRead;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnBranchIn: TBranchEvent read FOnBranchIn write FOnBranchIn;
    property OnBranchBlock: TBranchEvent read FOnBranchBlock write FOnBranchBlock;
    property OnBranchOut: TBranchEvent read FOnBranchOut write FOnBranchOut;
  end;

procedure Register;

implementation

uses
  ecode_unit,
  {$IFDEF LAZARUS}
  lconvencoding, parsery_utf8;
  {$ELSE}
  windows, Komunikaty_Delphi, ZipMstr;
  {$ENDIF}

type
  _import = record
              level: integer;
              klucz: string;
              adres: string;
            end;

var
  import: _import;
  zm_stop: boolean;
  istrumien: integer;
  stary_algorytm: boolean = false;

procedure Register;
begin
  {$IFDEF LAZARUS}
  {$I xmlparser_icon.lrs}
  {$ENDIF}
  RegisterComponents('Misc',[TXmlParser]);
end;

{ TXmlParser }

procedure AktualizujAdres(klucz: string; level: integer);
var
  i,b: integer;
  s: string;
begin
  if klucz='#text' then exit;

  //gdy poczatek
  if level=-1 then
  begin
    import.adres:='/'+klucz;
    import.level:=level;
    import.klucz:=klucz;
    exit;
  end;

  //czy mamy cos do dodania?
  if import.level<level then import.adres:=import.adres+'/'+klucz;

  //czy mamy cos do usuniecia?
  if (import.level>level) or ((import.level=level) and (GetLineToStr(import.adres,level,'/')<>klucz)) then
  begin
    if klucz='' then b:=2 else b:=1;
    for i:=1 to level+b do if i=1 then s:=GetLineToStr(import.adres,i,'/') else s:=s+'/'+GetLineToStr(import.adres,i,'/');
    if b=1 then s:=s+'/'+klucz;
    import.adres:=s;
  end;

  if import.adres[length(import.adres)]='/' then delete(import.adres,length(import.adres),1);
  import.level:=level;
  import.klucz:=klucz;
end;

function CzyToJestXML(s:shortstring):boolean;
begin
  //sprawdzam ciag i porownuje go do: "<?xml"
  result:=(s[1]='<') and (s[2]='?') and
          ((s[3]='X') or (s[3]='x')) and
          ((s[4]='M') or (s[4]='m')) and
          ((s[5]='L') or (s[5]='l'));
end;

function TXmlParser.OkreslStroneKodowa(str: TStream): integer;
var
  e: file of char;
  f: textfile;
  c: char;
  s: string;
  a: integer;
  b: boolean;
begin
  (* Najpierw sprawdze, czy plik nie jest zakodowany *)
  str.Position:=0;
  SetLength(s,255);
  a:=str.Read(s[1],255);
  SetLength(s,a);
  str.Position:=0;
  //sprawdze jeszcze tylko, czy to jest XML
  b:=CzyToJestXML(s);
  //jesli to nie jest xml - wychodze
  if not b then
  begin
    result:=10;
    exit;
  end;
  (* Chyba mamy do czynienia z plikiem XML, sprawdzam kodowanie *)
  //sprawdzam kodowanie
  a:=pos('encoding=',lowercase(s));
  delete(s,1,a);
  s:=GetLineToStr(s,2,'"');
  if (UpCase(s)='UTF8') or (UpCase(s)='UTF-8') then a:=1 else
  if pos('1250',s)>0 then a:=2 else
  if pos('8859-2',s)>0 then a:=3 else
  a:=0;
  result:=a;
end;

function TXmlParser.EncryptStream(s_in, s_out: TStream; size: longword
  ): longword;
begin
  Des.InitStr(FToken,TDCP_sha1);
  result:=Des.EncryptStream(s_in,s_out,size);
  Des.Burn;
end;

function TXmlParser.DecryptStream(s_in, s_out: TStream; size: longword
  ): longword;
begin
  Des.InitStr(FToken,TDCP_sha1);
  result:=Des.DecryptStream(s_in,s_out,size);
  Des.Burn;
end;

procedure TXmlParser.ParseXML(level: integer; node: {$IFDEF LAZARUS} TDOMNode {$ELSE} IDOMNode {$ENDIF} );
var
  cNode: {$IFDEF LAZARUS} TDOMNode {$ELSE} IDOMNode {$ENDIF};
  i: integer;
  s: string;
  b: boolean;
begin
  if zm_stop or (Node=nil) then Exit;

  if (level>import.level) and (import.level>=0) and Assigned(FOnBranchIn) then
  begin
    if stary_algorytm then AktualizujAdres(Node.NodeName,level);
    s:=import.adres;
    delete(s,1,10);
    {$IFDEF LAZARUS}
    FOnBranchIn(self,import.level,import.adres,zm_stop);
    {$ELSE}
    FOnBranchIn(self,import.level,s,zm_stop);
    {$ENDIF}
    if not stary_algorytm then AktualizujAdres(Node.NodeName,level);
  end else AktualizujAdres(Node.NodeName,level);

  if Assigned(FOnRead) then
  begin
    //ATRYBUTY
    try if {$IFDEF LAZARUS} Node.HasAttributes and {$ENDIF} (Node.Attributes.Length>0) then b:=true else b:=false; except b:=false; end;
    if b then
    begin
      s:=import.adres;
      delete(s,1,10);
      for i:=0 to Node.Attributes.Length-1 do
      begin
        {$IFDEF LAZARUS}
        FOnRead(self,level,import.adres,Node.NodeName,Node.Attributes[i].NodeName,UTF8Encode(Node.Attributes[i].NodeValue),zm_stop);
        {$ELSE}
        FOnRead(self,level,s,Node.NodeName,Node.Attributes[i].NodeName,Node.Attributes[i].NodeValue,zm_stop);
        {$ENDIF}
      end;
    end;
    //WARTOSCI KLUCZY
    if Node.NodeValue<>'' then
    begin
      s:=import.adres;
      delete(s,1,10);
      {$IFDEF LAZARUS}
      FOnRead(self,import.level,import.adres,import.klucz,'',UTF8Encode(Node.NodeValue),zm_stop);
      {$ELSE}
      FOnRead(self,import.level,s,import.klucz,'',Node.NodeValue,zm_stop);
      {$ENDIF}
    end;
  end;

  if Assigned(FOnProgress) then case istrumien of
    0: FOnProgress(self,strumien.Size,strumien.Position);
    2: FOnProgress(self,strumien2.Size,strumien2.Position);
  end;

  cNode:=Node.FirstChild;
  while cNode<>nil do
  begin
    inc(level);
    ParseXML(level,cNode);
    cNode:=cNode.NextSibling;
    dec(level);
  end;

  if (import.level=level) and Assigned(FOnBranchBlock) then
  begin
    s:=import.adres;
    delete(s,1,10);
    {$IFDEF LAZARUS}
    FOnBranchBlock(self,import.level,import.adres,zm_stop);
    {$ELSE}
    FOnBranchBlock(self,import.level,s,zm_stop);
    {$ENDIF}
  end;

  if (level<import.level) and Assigned(FOnBranchOut) then
  begin
    s:=import.adres;
    delete(s,1,10);
    {$IFDEF LAZARUS}
    FOnBranchOut(self,import.level,import.adres,zm_stop);
    {$ELSE}
    FOnBranchOut(self,import.level,s,zm_stop);
    {$ENDIF}
    AktualizujAdres(Node.NodeName,level);
  end;
end;

constructor TXmlParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FContener:=crNone;
  FES:=eAuto;
  FNULL:=false;
  FTest:=true;
  strumien:=TMemoryStream.Create;
  strumien2:=TMemoryStream.Create;
  Des:=TDCP_des.Create(Self);
end;

destructor TXmlParser.Destroy;
begin
  strumien.Free;
  strumien2.Free;
  DES.Free;
  inherited Destroy;
end;

procedure TXmlParser.SetAlg01;
begin
  stary_algorytm:=true;
end;

procedure TXmlParser.SetAlg02;
begin
  stary_algorytm:=false;
end;

function TXmlParser.Execute: boolean;
var
  plik: string;
  stream: TMemoryStream;
  b: boolean;
begin
  plik:=zm_filename;
  if (plik='') or (not FileExists(plik)) then
  begin
    __ERROR:=2;
    if Assigned(FOnError) then FOnError(Self, 2, com_1);
    result:=false;
    exit;
  end;
  stream:=TMemoryStream.Create;
  stream.LoadFromFile(plik);
  try
    b:=Execute(stream);
  finally
    stream.Free;
  end;
  result:=b;
end;

{$IFDEF LAZARUS}

procedure TXmlParser.proc_open(Sender: TObject; var AStream: TStream);
begin
  AStream:=TMemoryStream.Create;
  AStream.CopyFrom(strumien_source,strumien_source.Size);
  AStream.Position:=0;
end;

procedure TXmlParser.proc1(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream:=TMemoryStream.Create;
end;

procedure TXmlParser.proc2(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream.Position:=0;
  strumien.LoadFromStream(Astream);
  Astream.Free;
end;

{$ENDIF}

function TXmlParser.Execute(Stream: TStream): boolean;
var
  plik: string;
  bufor: shortstring;
  kodowanie,a,b,buf: integer;
  b_przekodowanie,zm_result: boolean;
  {$IFDEF LAZARUS}
  ZipMaster: TUnzipper;
  instream,outstream: TMemoryStream;
  lista: TStrings;
  {$ELSE}
  ZipMaster: TZipMaster;
  {$ENDIF}
begin
  plik:=zm_filename;
  if Assigned(FOnBeforeRead) then FOnBeforeRead(Self);
  b_przekodowanie:=false;
  zm_stop:=false;
  strumien.Clear;
  strumien2.Clear;
  (* wczytanie strumienia z automatycznym odkodowaniem/rozpakowaniem jesli trzeba *)
  case FContener of
    crNone: strumien.LoadFromStream(stream);
    crDES:  begin
              strumien2.LoadFromStream(stream);
              DecryptStream(strumien2,strumien,strumien2.Size);
              strumien2.Clear;
            end;
    crZIP:  begin
              {$IFDEF LAZARUS}
              strumien_source:=TMemoryStream.Create;
              strumien_source.LoadFromStream(stream);
              ZipMaster:=TUnzipper.Create;
              lista:=TStringList.Create;
              try
                ZipMaster.OnOpenInputStream:=@proc_open;
                ZipMaster.OnCreateStream:=@proc1;
                ZipMaster.OnDoneStream:=@proc2;
                ZipMaster.Examine;
                lista.Add(ZipMaster.Entries.FullEntries[0].ArchiveFileName);
                strumien_source.Position:=0;
                ZipMaster.UnZipFiles(lista);
              finally
                lista.Free;
                ZipMaster.Free;
                strumien_source.Free;
              end;
              {$ELSE}
              ZipMaster:=TZipMaster.Create(nil);
              try
                ZipMaster.ZipStream.LoadFromStream(stream);
                strumien.LoadFromStream(ZipMaster.ExtractFileToStream(ZipMaster.DirEntry[0].Filename));
              finally
                ZipMaster.Free;
              end;
              {$ENDIF}
            end;
  end;
  strumien.Position:=0;
  (* w razie potrzeby przekodowujemy do utf8 *)
  if FES=eAuto then kodowanie:=OkreslStroneKodowa(strumien) else kodowanie:=0;
  strumien.Position:=0;
  try
    __ERROR:=0;
    import.adres:='';
    import.level:=-1;
    //przekodowanie - jesli trzeba
    if (FES=eWindows1250) or (FES=eISO_8859_2) or ((FES=eAuto) and (kodowanie>1)) then
    begin
      a:=0;
      while true do
      begin
        buf:=strumien.Read(bufor[1],200);
        SetLength(bufor,buf);
        {$IFDEF LAZARUS}
        if (FES=eWindows1250) or (kodowanie=2) then
          bufor:=ConvertEncoding(bufor,'cp1250','utf8') else
        if (FES=eISO_8859_2) or (kodowanie=3) then
          bufor:=ConvertEncoding(bufor,'iso_8859_2','utf8');
        {$ELSE}
        bufor:=UTF8Encode(bufor);
        {$ENDIF}
        buf:=length(bufor);
        if (a=0) and (pos('<?',bufor)>0) or (a=1) then
        begin
          if a=0 then strumien2.Write('<?xml version="1.0" encoding="utf-8"?>',38);
          b:=pos('?>',bufor);
          if b>0 then
          begin
            delete(bufor,1,b+1);
            dec(buf,b+1);
            a:=2;
          end else begin
            a:=1;
            continue;
          end;
        end;
        if buf=0 then break;
        strumien2.Write(bufor[1],buf);
      end;
      strumien2.Position:=0;
      b_przekodowanie:=true;
      strumien.Clear;
    end;
    //ewentualny test poprawnosci pliku XML
    if FTest then
    begin
      if b_przekodowanie then
      begin
        strumien2.Position:=0;
        buf:=strumien2.Read(bufor[1],5);
        SetLength(bufor,buf);
        strumien2.Position:=0;
      end else begin
        strumien.Position:=0;
        buf:=strumien.Read(bufor[1],5);
        SetLength(bufor,buf);
        strumien.Position:=0;
      end;
      if not CzyToJestXML(bufor) then
      begin
        (* rejestruje blad i przerywam zadanie *)
        __ERROR:=4;
        if b_przekodowanie then strumien2.Clear else strumien.Clear;
        if Assigned(FOnError) then FOnError(Self, 4, com_2);
        result:=false;
        exit;
      end;
    end;
    //parser
    {$IFDEF LAZARUS}
    doc:=TXMLDocument.Create;
    {$ELSE}
    doc:=TXMLDocument.Create(nil);
    {$ENDIF}
    if b_przekodowanie then
    begin
      istrumien:=2;
      if Assigned(FOnProgress) then FOnProgress(Self,strumien2.Size,0);
      {$IFDEF LAZARUS}
      ReadXMLFile(doc,strumien2); (* strumien przekodowany, lub zdeszyfrowany *)
      {$ELSE}
      doc.LoadFromStream(strumien2,xetUTF_8); (* strumien przekodowany, lub zdeszyfrowany *)
      {$ENDIF}
    end else begin
      istrumien:=0;
      if Assigned(FOnProgress) then FOnProgress(Self,strumien.Size,0);
      {$IFDEF LAZARUS}
      ReadXMLFile(doc,strumien); (* strumien domyslny *)
      {$ELSE}
      doc.LoadFromStream(strumien,xetUTF_8); (* strumien domyslny *)
      {$ENDIF}
    end;
    {$IFDEF LAZARUS}
    temp:=doc.DocumentElement;
    {$ELSE}
    temp:=doc.DOMDocument;
    {$ENDIF}
    ParseXML(0,temp);
    if (not zm_stop) and FNULL and Assigned(FOnRead) then FOnRead(self,-1,'','','','',zm_stop);
  finally
    doc.Free;
    if b_przekodowanie then strumien2.Clear else strumien.Clear;
  end;
  if __ERROR=0 then zm_result:=true else
  begin
    if Assigned(FOnError) then FOnError(Self, 2, com_3);
    zm_result:=false;
  end;
  if Assigned(FOnAfterRead) then FOnAfterRead(Self);
  result:=zm_result;
end;

function TXmlParser.LockString(s: string; spaces: boolean): string;
var
  pom: string;
begin
  pom:=s;
  pom:=StringReplace(pom,'<',com_5,[rfReplaceAll]);
  pom:=StringReplace(pom,'>',com_6,[rfReplaceAll]);
  if spaces then pom:=StringReplace(pom,' ',com_7,[rfReplaceAll]);
  result:=pom;
end;

function TXmlParser.UnlockString(s: string; spaces: boolean): string;
var
  pom: string;
begin
  pom:=s;
  (* w celu zachowania wersji wstecz *)
  pom:=StringReplace(pom,com_5,'<',[rfReplaceAll]);
  pom:=StringReplace(pom,com_6,'>',[rfReplaceAll]);
  if spaces then pom:=StringReplace(pom,com_7,' ',[rfReplaceAll]);
  //pom:=StringReplace(pom,#9668,'<',[rfReplaceAll]);
  //pom:=StringReplace(pom,#9658,'>',[rfReplaceAll]);
  //if spaces then pom:=StringReplace(pom,#9834,' ',[rfReplaceAll]);
  result:=pom;
end;

function TXmlParser.EncodeXML(XMLFile: string; EncodeFile: string): boolean;
var
  plik: string;
  bufor: shortstring;
  kodowanie,a,b,buf: integer;
  zm_result: boolean;
begin
  plik:=XMLFile;
  if (plik='') or (not FileExists(plik)) then
  begin
    __ERROR:=3;
    if Assigned(FOnError) then FOnError(Self, 2, com_1);
    result:=false;
    exit;
  end;
  try
    __ERROR:=0;
    strumien.Clear;
    strumien.LoadFromFile(plik);
    //XMLToDES
    strumien2.Clear;
    EncryptStream(strumien,strumien2,strumien.Size);
    if EncodeFile<>'' then plik:=EncodeFile;
    strumien2.SaveToFile(plik);
  finally
    strumien.Clear;
    strumien2.Clear;
  end;
  if __ERROR=0 then zm_result:=true else
  begin
    if Assigned(FOnError) then FOnError(Self, 2, com_3);
    zm_result:=false;
  end;
  result:=zm_result;
end;

function TXmlParser.EncodeXML: boolean;
begin
  result:=EncodeXML(zm_filename);
end;

function TXmlParser.DecodeXML(DecodeFile: string; XMLFile: string): boolean;
var
  plik: string;
  bufor: shortstring;
  kodowanie,a,b,buf: integer;
  zm_result: boolean;
begin
  plik:=DecodeFile;
  if (plik='') or (not FileExists(plik)) then
  begin
    __ERROR:=3;
    if Assigned(FOnError) then FOnError(Self, 2, com_4);
    result:=false;
    exit;
  end;
  kodowanie:=3;
  try
    __ERROR:=0;
    strumien.Clear;
    strumien.LoadFromFile(plik);
    //DESToXML
    strumien2.Clear;
    DecryptStream(strumien,strumien2,strumien.Size);
    if XMLFile<>'' then plik:=XMLFile;
    strumien2.SaveToFile(plik);
  finally
    strumien.Clear;
    strumien2.Clear;
  end;
  if __ERROR=0 then zm_result:=true else
  begin
    if Assigned(FOnError) then FOnError(Self, 2, com_3);
    zm_result:=false;
  end;
  result:=zm_result;
end;

function TXmlParser.DecodeXML: boolean;
begin
  result:=DecodeXML(zm_filename);
end;

end.
