unit compressionfly;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources;

const
  BUFFER_SIZE = 100000;
  BUFFER_SIZE_COMPRESSED = 65536;

type

  TCompressionFlyAlgorithm = (acNone, acDeflate,acLZBRRC,acBRRC);
  TCompressionFlyMode = (moCompress,moDecompress);

type
  TBufferNetwork = array [0..BUFFER_SIZE-1] of byte;

  { TCompressionFly }

  TCompressionFly = class(TComponent)
  private
    fAlg: TCompressionFlyAlgorithm;
    fAutoClear: boolean;
    fCount: integer;
    fMode: TCompressionFlyMode;
    fNag: boolean;
    stream: TMemoryStream;
    function wCompress(const iBuffer; iCount: integer; out oBuffer; oMaxCount: integer): integer;
    function wDecompress(const iBuffer; iCount: integer; out oBuffer; oMaxCount: integer): integer;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    function Add(iStream: TStream; iCount: integer = 0): integer;
    function Add(const iBuffer; iCount: integer): integer;
    function Test: boolean;
    function Execute(oStream: TStream): integer;
    function Execute(out oBuffer): integer;
  published
    property Mode: TCompressionFlyMode read fMode write fMode default moCompress;
    property Taging: boolean read fNag write fNag default false;
    property Algorithm: TCompressionFlyAlgorithm read fAlg write fAlg default acNone;
    property AutoClear: boolean read fAutoClear write fAutoClear default true;
  end;

procedure Register;

implementation

uses
  ecode_unit, RNL;

procedure Register;
begin
  {$I compressionfly_icon.lrs}
  RegisterComponents('System',[TCompressionFly]);
end;

{ TCompressionFly }

function TCompressionFly.wCompress(const iBuffer; iCount: integer; out oBuffer;
  oMaxCount: integer): integer;
var
  c1: TRNLCompressorDeflate;
  c2: TRNLCompressorLZBRRC;
  c3: TRNLCompressorBRRC;
  n: integer;
begin
  if fAlg=acDeflate then begin c1:=TRNLCompressorDeflate.Create; try n:=c1.Compress(@iBuffer,iCount,@oBuffer,oMaxCount) finally c1.Free end; end else
  if fAlg=acLZBRRC then begin c2:=TRNLCompressorLZBRRC.Create; try n:=c2.Compress(@iBuffer,iCount,@oBuffer,oMaxCount) finally c2.Free end; end else
  if fAlg=acBRRC then begin c3:=TRNLCompressorBRRC.Create; try n:=c3.Compress(@iBuffer,iCount,@oBuffer,oMaxCount) finally c3.Free end; end;
  result:=n;
end;

function TCompressionFly.wDecompress(const iBuffer; iCount: integer; out
  oBuffer; oMaxCount: integer): integer;
var
  c1: TRNLCompressorDeflate;
  c2: TRNLCompressorLZBRRC;
  c3: TRNLCompressorBRRC;
  n: integer;
begin
  if fAlg=acDeflate then begin c1:=TRNLCompressorDeflate.Create; try n:=c1.Decompress(@iBuffer,iCount,@oBuffer,oMaxCount) finally c1.Free end; end else
  if fAlg=acLZBRRC then begin c2:=TRNLCompressorLZBRRC.Create; try n:=c2.Decompress(@iBuffer,iCount,@oBuffer,oMaxCount) finally c2.Free end; end else
  if fAlg=acBRRC then begin c3:=TRNLCompressorBRRC.Create; try n:=c3.Decompress(@iBuffer,iCount,@oBuffer,oMaxCount) finally c3.Free end; end;
  result:=n;
end;

constructor TCompressionFly.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  stream:=TMemoryStream.Create;
  fMode:=moCompress;
  fNag:=false;
  fAlg:=acNone;
  fCount:=0;
  fAutoClear:=true;
end;

destructor TCompressionFly.Destroy;
begin
  stream.Free;
  inherited Destroy;
end;

function TCompressionFly.Count: integer;
begin
  result:=fCount;
end;

procedure TCompressionFly.Clear;
begin
  stream.Clear;
  fCount:=0;
end;

function TCompressionFly.Add(iStream: TStream; iCount: integer): integer;
var
  buf: TBufferNetwork;
  c,n: integer;
begin
  stream.Position:=stream.Size;
  if iCount=0 then c:=iStream.Size else c:=iCount;
  n:=iStream.Read(buf,c);
  fCount:=fCount+stream.Write(buf,n);
  result:=fCount;
end;

function TCompressionFly.Add(const iBuffer; iCount: integer): integer;
begin
  stream.Position:=stream.Size;
  fCount:=fCount+stream.Write(iBuffer,iCount);
  result:=fCount;
end;

function TCompressionFly.Test: boolean;
var
  a: integer;
  s: string;
begin
  if fNag then
  begin
    SetLength(s,5);
    stream.Read(s[1],5);
    a:=HexToDec(s);
    result:=stream.Size>=a;
  end else result:=true;
end;

function TCompressionFly.Execute(oStream: TStream): integer;
var
  buf,buf2: TBufferNetwork;
  a,n,n2: integer;
  s: string;
begin
  if fAlg=acNone then
  begin
    stream.Position:=0;
    result:=oStream.CopyFrom(stream,stream.Size);
    if fAutoClear then Clear;
    exit;
  end;
  stream.Position:=0;
  n:=stream.Read(buf,stream.Size);
  if fMode=moCompress then
  begin
    n2:=wCompress(buf,n,buf2,BUFFER_SIZE_COMPRESSED);
    if fNag then
    begin
      s:=IntToHex(n2+5,5);
      oStream.Write(s[1],5);
    end;
    oStream.Write(buf2,n2);
    if fAutoClear then Clear;
    if fNag then result:=n2+5 else result:=n2;
  end else begin
    if fNag then
    begin
      SetLength(s,5);
      s[1]:=chr(buf[0]);
      s[2]:=chr(buf[1]);
      s[3]:=chr(buf[2]);
      s[4]:=chr(buf[3]);
      s[5]:=chr(buf[4]);
      a:=HexToDec(s);
      if a>n then
      begin
        result:=-1;
        exit;
      end;
      n2:=wDecompress(buf[5],a-5,buf2,BUFFER_SIZE);
      if n>a then
      begin
        stream.Clear;
        stream.Write(buf[a],n-a);
      end else if fAutoClear then Clear;
      oStream.Write(buf2,n2);
    end else begin
      n2:=wDecompress(buf,n,buf2,BUFFER_SIZE);
      oStream.Write(buf2,n2);
    end;
    result:=n2;
  end;
end;

function TCompressionFly.Execute(out oBuffer): integer;
var
  buf: TBufferNetwork;
  a,n,n2: integer;
  s: string;
begin
  if fAlg=acNone then
  begin
    stream.Position:=0;
    result:=stream.Read(oBuffer,BUFFER_SIZE_COMPRESSED);
    if fAutoClear then Clear;
    exit;
  end;
  stream.Position:=0;
  n:=stream.Read(buf,stream.Size);
  if fMode=moCompress then
  begin
    if fNag then
    begin
      n2:=wCompress(buf,n,TBufferNetwork(oBuffer)[5],BUFFER_SIZE_COMPRESSED);
      s:=IntToHex(n2+5,5);
      TBufferNetwork(oBuffer)[0]:=ord(s[1]);
      TBufferNetwork(oBuffer)[1]:=ord(s[2]);
      TBufferNetwork(oBuffer)[2]:=ord(s[3]);
      TBufferNetwork(oBuffer)[3]:=ord(s[4]);
      TBufferNetwork(oBuffer)[4]:=ord(s[5]);
    end else n2:=wCompress(buf,n,oBuffer,BUFFER_SIZE_COMPRESSED);
    if fAutoClear then Clear;
    if fNag then result:=n2+5 else result:=n2;
  end else begin
    if fNag then
    begin
      SetLength(s,5);
      s[1]:=chr(buf[0]);
      s[2]:=chr(buf[1]);
      s[3]:=chr(buf[2]);
      s[4]:=chr(buf[3]);
      s[5]:=chr(buf[4]);
      a:=HexToDec(s);
      if a>n then
      begin
        result:=-1;
        exit;
      end;
      n2:=wDecompress(buf[5],a-5,oBuffer,BUFFER_SIZE);
      if n>a then
      begin
        stream.Clear;
        stream.Write(buf[a],n-a);
      end else if fAutoClear then Clear;
    end else n2:=wDecompress(buf,n,oBuffer,BUFFER_SIZE);
    result:=n2;
  end;
end;

end.
