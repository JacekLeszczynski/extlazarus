unit NetSynWebSocket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  websocket2;

type

  { TTestWebSocketClientConnection }

  TTestWebSocketClientConnection = class(TWebSocketClientConnection)
  private
    fFramedText: string;
    fFramedStream: TMemoryStream;
    fPing: string;
    fPong: string;
    procedure ProcessText(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); override;
    procedure ProcessTextContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: string); override;
    procedure ProcessStream(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); override;
    procedure ProcessStreamContinuation(aFinal, aRes1, aRes2, aRes3: boolean; aData: TMemoryStream); override;
    procedure ProcessPing(aData: string); override;
    procedure ProcessPong(aData: string); override;
    procedure SyncTextFrame;
    procedure SyncBinFrame;
    procedure SyncPing;
    procedure SyncPong;
  public
    constructor Create(aHost, aPort, aResourceName: string;
    aOrigin: string = '-'; aProtocol: string = '-'; aExtension: string = '-';
    aCookie: string = '-'; aVersion: integer = 8); override;
    destructor Destroy; override;
    property ReadFinal: boolean read fReadFinal;
    property ReadRes1: boolean read fReadRes1;
    property ReadRes2: boolean read fReadRes2;
    property ReadRes3: boolean read fReadRes3;
    property ReadCode: integer read fReadCode;
    property ReadStream: TMemoryStream read fReadStream;
    property WriteFinal: boolean read fWriteFinal;
    property WriteRes1: boolean read fWriteRes1;
    property WriteRes2: boolean read fWriteRes2;
    property WriteRes3: boolean read fWriteRes3;
    property WriteCode: integer read fWriteCode;
    property WriteStream: TMemoryStream read fWriteStream;
  end;

  { TNetSynWebSocket }

  TNetSynWebSocket = class(TComponent)
  private
    FActive: boolean;
    fClient: TWebSocketClientConnection;
    fFrameString: string;
    FHost: string;
    FKeepConnection: boolean;
    FPingText: string;
    FTimer: TTimer;
    fOnClose: TWebSocketConnectionClose;
    fOnOpen: TWebSocketConnectionEvent;
    fOnRead: TWebSocketConnectionData;
    fOnWrite: TWebSocketConnectionData;
    FPort: word;
    FSSL: boolean;
    procedure OnReadPrivate(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    procedure OnWritePrivate(aSender: TWebSocketCustomConnection; aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
    procedure OnClosePrivate(aSender: TWebSocketCustomConnection; aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
    procedure OnOpenPrivate(aSender: TWebSocketCustomConnection);
    procedure SetKeepConnection(AValue: boolean);
    procedure TimerEvent(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function SendText(sText: string): boolean;
  published
    property Host: string read FHost write FHost;
    property Port: word read FPort write FPort;
    property SSL: boolean read FSSL write FSSL;
    property Active: boolean read FActive;
    property KeepConnection: boolean read FKeepConnection write SetKeepConnection;
    property PingText: string read FPingText write FPingText;
    property OnRead: TWebSocketConnectionData read fOnRead write fOnRead;
    property OnWrite: TWebSocketConnectionData read fOnWrite write fOnWrite;
    property OnClose: TWebSocketConnectionClose read fOnClose write fOnClose;
    property OnOpen: TWebSocketConnectionEvent read fOnOpen write fOnOpen;
  end;

procedure Register;

implementation

uses
  synachar;

procedure Register;
begin
  {$I netsynwebsocket_icon.lrs}
  RegisterComponents('lNet',[TNetSynWebSocket]);
end;

{ TTestWebSocketClientConnection }

procedure TTestWebSocketClientConnection.ProcessText(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: string);
begin
  inherited ProcessText(aFinal, aRes1, aRes2, aRes3, aData);
  fFramedText:=aData;
end;

procedure TTestWebSocketClientConnection.ProcessTextContinuation(aFinal, aRes1,
  aRes2, aRes3: boolean; aData: string);
begin
  inherited ProcessTextContinuation(aFinal, aRes1, aRes2, aRes3, aData);
  fFramedText:=fFramedText+aData;
  if (aFinal) then Synchronize(@SyncTextFrame);
end;

procedure TTestWebSocketClientConnection.ProcessStream(aFinal, aRes1, aRes2,
  aRes3: boolean; aData: TMemoryStream);
begin
  inherited ProcessStream(aFinal, aRes1, aRes2, aRes3, aData);
  fFramedStream.Size:=0;
  fFramedStream.CopyFrom(aData,aData.Size);
  //MainForm.InfoMemo.Lines.Insert(0,Format('ProcessStream %d, %d, %d, %d, %d, %d ',[Index,ord(aFinal),ord(aRes1),ord(aRes2),ord(aRes3),aData.Size]));
  if (aFinal) then Synchronize(@SyncBinFrame);
end;

procedure TTestWebSocketClientConnection.ProcessStreamContinuation(aFinal,
  aRes1, aRes2, aRes3: boolean; aData: TMemoryStream);
begin
  inherited ProcessStreamContinuation(aFinal, aRes1, aRes2, aRes3, aData);
  fFramedStream.CopyFrom(aData,aData.Size);
  //MainForm.InfoMemo.Lines.Insert(0,Format('ProcessStreamContinuation %d, %d, %d, %d, %d, %d ',[Index,ord(aFinal),ord(aRes1),ord(aRes2),ord(aRes3),aData.Size]));
  if (aFinal) then Synchronize(@SyncBinFrame);
end;

procedure TTestWebSocketClientConnection.ProcessPing(aData: string);
begin
  inherited ProcessPing(aData);
  Pong(aData);
  fPing:=aData;
  Synchronize(@SyncPing);
end;

procedure TTestWebSocketClientConnection.ProcessPong(aData: string);
begin
  inherited ProcessPong(aData);
  fPong:=aData;
  Synchronize(@SyncPong);
end;

procedure TTestWebSocketClientConnection.SyncTextFrame;
begin
  //MainForm.FrameReceiveMemo.Text:=CharsetConversion(fFramedText,UTF_8,GetCurCP);
end;

procedure TTestWebSocketClientConnection.SyncBinFrame;
var
  png: TPortableNetworkGraphic;
begin
//  MainForm.InfoMemo.Lines.Insert(0,Format('SyncBinFrame %d',[fFramedStream.Size]));
  png:=TPortableNetworkGraphic.Create;
  fFramedStream.Position:=0;
  png.LoadFromStream(fFramedStream);
//  MainForm.Image1.Picture.Assign(png);
  png.Free;
end;

procedure TTestWebSocketClientConnection.SyncPing;
begin
  //MainForm.InfoMemo.Lines.Insert(0,Format('SyncPing %s',[fPing]));
end;

procedure TTestWebSocketClientConnection.SyncPong;
begin
  //MainForm.InfoMemo.Lines.Insert(0,Format('SyncPong %s',[fPong]));
end;

constructor TTestWebSocketClientConnection.Create(aHost, aPort,
  aResourceName: string; aOrigin: string; aProtocol: string;
  aExtension: string; aCookie: string; aVersion: integer);
begin
  inherited Create(aHost, aPort, aResourceName, aOrigin, aProtocol, aExtension,
    aCookie, aVersion);
  fFramedText:='';
  fFramedStream:=TMemoryStream.Create;
end;

destructor TTestWebSocketClientConnection.Destroy;
begin
  fFramedStream.free;
  inherited Destroy;
end;

{ TNetSynWebSocket }

procedure TNetSynWebSocket.OnReadPrivate(aSender: TWebSocketCustomConnection;
  aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
begin
  if Assigned(FOnRead) then FOnRead(aSender,aFinal,aRes1,aRes2,aRes3,aCode,aData);
end;

procedure TNetSynWebSocket.OnWritePrivate(aSender: TWebSocketCustomConnection;
  aFinal, aRes1, aRes2, aRes3: boolean; aCode: integer; aData: TMemoryStream);
begin
  if Assigned(FOnWrite) then FOnWrite(aSender,aFinal,aRes1,aRes2,aRes3,aCode,aData);
end;

procedure TNetSynWebSocket.OnClosePrivate(aSender: TWebSocketCustomConnection;
  aCloseCode: integer; aCloseReason: string; aClosedByPeer: boolean);
begin
  FTimer.Enabled:=false;
  FActive:=false;
  if Assigned(FOnClose) then FOnClose(aSender,aCloseCode,aCloseReason,aClosedByPeer);
end;

procedure TNetSynWebSocket.OnOpenPrivate(aSender: TWebSocketCustomConnection);
begin
  FActive:=true;
  if FKeepConnection then FTimer.Enabled:=true;
  if Assigned(FOnOpen) then FOnOpen(aSender);
end;

procedure TNetSynWebSocket.SetKeepConnection(AValue: boolean);
begin
  if FKeepConnection=AValue then Exit;
  FKeepConnection:=AValue;
  if FActive then FTimer.Enabled:=AValue;
end;

procedure TNetSynWebSocket.TimerEvent(Sender: TObject);
begin
  //fClient.SendText(CharsetConversion('{"numbers":[2],"strings":[]}',GetCurCP,UTF_8));
  fClient.SendText(CharsetConversion(PingText,GetCurCP,UTF_8));
end;

constructor TNetSynWebSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer:=TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.Interval:=30000;
  FTimer.OnTimer:=@TimerEvent;
end;

destructor TNetSynWebSocket.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TNetSynWebSocket.Open;
begin
  fClient:=TTestWebSocketClientConnection.Create(FHost,IntToStr(FPort),'/');

  fClient.OnRead:=@OnReadPrivate;
  fClient.OnWrite:=@OnWritePrivate;
  fClient.OnClose:=@OnClosePrivate;
  fClient.OnOpen:=@OnOpenPrivate;
  //fClient.Socket.OnSyncStatus := OnConnectionSocket;

  fClient.SSL:=FSSL;
  fClient.Start;
end;

procedure TNetSynWebSocket.Close;
begin
  //fClient.TerminateThread;
  fClient.Close(wsCloseNormal,'bye bye');
  //sleep(2000);
end;

function TNetSynWebSocket.SendText(sText: string): boolean;
begin
  fClient.SendText(CharsetConversion(sText,GetCurCP,UTF_8));
end;

end.
