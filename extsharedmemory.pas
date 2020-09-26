unit ExtSharedMemory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, simpleipc;

type
  TExtSharedMemoryMessageEvent = procedure(Sender: TObject; AMessage: string) of object;
  TExtSharedMemorySendMessageEvent = procedure(Sender: TObject; var AMessage: string) of object;

  { TExtSharedMemory }

  TExtSharedMemory = class(TComponent)
  private
    { Private declarations }
    FActive: boolean;
    FAppKey: string;
    FOnClient: TNotifyEvent;
    FOnMessage: TExtSharedMemoryMessageEvent;
    FOnSendMessage: TExtSharedMemorySendMessageEvent;
    FOnServer: TNotifyEvent;
    ss: TSimpleIPCServer;
    cc: TSimpleIPCClient;
    procedure init;
  protected
    { Protected declarations }
    procedure _MessageQueued(Sender: TObject);
    procedure _Message(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean; //Wykonaj całą procedurę! Zaleca się wykonać jeden raz zaraz po uruchomieniu aplikacji.
    procedure Uninstall;
  published
    { Published declarations }
    property ApplicationKey: string read FAppKey write FAppKey; //Unique Key Application
    property Active: boolean read FActive;
    property OnMessage: TExtSharedMemoryMessageEvent read FOnMessage write FOnMessage; //Message received
    property OnSendMessage: TExtSharedMemorySendMessageEvent read FOnSendMessage write FOnSendMessage; //Message to send
    property OnClient: TNotifyEvent read FOnClient write FOnClient; //Client code
    property OnServer: TNotifyEvent read FOnServer write FOnServer; //Server code
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I extsharedmemory_icon.lrs}
  RegisterComponents('System',[TExtSharedMemory]);
end;

{ TExtSharedMemory }

procedure TExtSharedMemory.init;
var
  vKey: TGuid;
begin
  CreateGUID(vKey);
  FActive:=false;
  FAppKey:=GUIDToString(vKey);
  ss.Global:=true;
end;

procedure TExtSharedMemory._MessageQueued(Sender: TObject);
begin
  ss.ReadMessage;
end;

procedure TExtSharedMemory._Message(Sender: TObject);
begin
  if Assigned(FOnMessage) then FOnMessage(self,ss.StringMessage);
end;

constructor TExtSharedMemory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ss:=TSimpleIPCServer.Create(self);
  cc:=TSimpleIPCClient.Create(self);
  ss.OnMessageQueued:=@_MessageQueued;
  ss.OnMessage:=@_Message;
  init;
end;

destructor TExtSharedMemory.Destroy;
begin
  if cc.Active then cc.Disconnect;
  if ss.Active then ss.StopServer;
  cc.Free;
  ss.Free;
  inherited Destroy;
end;

function TExtSharedMemory.Execute: boolean;
var
  mess: string = '';
begin
  if FActive then exit;
  ss.ServerID:=FAppKey;
  cc.ServerID:=FAppKey;
  (* sprawdzam czy odpalony jest już serwer *)
  try
    cc.Connect;
    if Assigned(FOnSendMessage) then
    begin
      FOnSendMessage(self,mess);
      if mess<>'' then cc.SendStringMessage(mess);
    end;
    if Assigned(FOnClient) then FOnClient(self);
    result:=false;
  except
    ss.StartServer(true);
    if Assigned(FOnServer) then FOnServer(self);
    result:=true;
  end;
  FActive:=true;
end;

procedure TExtSharedMemory.Uninstall;
begin
  if cc.Active then cc.Disconnect;
  if ss.Active then ss.StopServer;
end;

end.
