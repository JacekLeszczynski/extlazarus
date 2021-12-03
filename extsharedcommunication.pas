unit ExtSharedCommunication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, ExtSharedMemory;

type

  { TExtSharedCommunication }

  TExtSharedCommunicationMessageEvent = procedure(Sender: TObject; AMessage: string) of object;
  TExtSharedCommunication = class(TComponent)
  private
    FAppKey1: string;
    FAppKey2: string;
    FOnMessage: TExtSharedCommunicationMessageEvent;
    shared1,shared2: TExtSharedMemory;
    SSS,CCC: integer;
    procedure w_shared1Client(Sender: TObject);
    procedure w_shared2Client(Sender: TObject);
    procedure w_shared1Server(Sender: TObject);
    procedure w_shared2Server(Sender: TObject);
    procedure w_shared1Message(Sender: TObject; AMessage: string);
    procedure w_shared2Message(Sender: TObject; AMessage: string);
    procedure SetAppKey1(AValue: string);
    procedure SetAppKey2(AValue: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure SendMessage(aMessage: string);
  published
    property AppKey1: string read FAppKey1 write SetAppKey1; //Unique Key Application 1
    property AppKey2: string read FAppKey2 write SetAppKey2; //Unique Key Application 2
    property OnMessage: TExtSharedCommunicationMessageEvent read FOnMessage write FOnMessage; //Message received
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I extsharedcommunication_icon.lrs}
  RegisterComponents('System',[TExtSharedCommunication]);
end;

{ TExtSharedCommunication }

procedure TExtSharedCommunication.w_shared1Client(Sender: TObject);
begin
  CCC:=1;
end;

procedure TExtSharedCommunication.w_shared2Client(Sender: TObject);
begin
  CCC:=2;
end;

procedure TExtSharedCommunication.w_shared1Server(Sender: TObject);
begin
  SSS:=1;
end;

procedure TExtSharedCommunication.w_shared2Server(Sender: TObject);
begin
  SSS:=2;
end;

procedure TExtSharedCommunication.w_shared1Message(Sender: TObject;
  AMessage: string);
begin
  if AMessage='{wewn_STOP}' then
  begin
    shared2.Uninstall;
    CCC:=0;
  end else
  if AMessage='{wewn_START}' then shared2.Execute else
  if assigned(FOnMessage) then FOnMessage(self,AMessage);
end;

procedure TExtSharedCommunication.w_shared2Message(Sender: TObject;
  AMessage: string);
begin
  if AMessage='{wewn_STOP}' then
  begin
    shared1.Uninstall;
    CCC:=0;
  end else
  if AMessage='{wewn_START}' then shared1.Execute else
  if assigned(FOnMessage) then FOnMessage(self,AMessage);
end;

procedure TExtSharedCommunication.SetAppKey1(AValue: string);
begin
  if FAppKey1=AValue then Exit;
  FAppKey1:=AValue;
  shared1.ApplicationKey:=FAppKey1;
end;

procedure TExtSharedCommunication.SetAppKey2(AValue: string);
begin
  if FAppKey2=AValue then Exit;
  FAppKey2:=AValue;
  shared2.ApplicationKey:=FAppKey2;
end;

constructor TExtSharedCommunication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SSS:=0;
  CCC:=0;
  shared1:=TExtSharedMemory.Create(self);
  shared2:=TExtSharedMemory.Create(self);
  shared1.OnClient:=@w_shared1Client;
  shared2.OnClient:=@w_shared2Client;
  shared1.OnServer:=@w_shared1Server;
  shared2.OnServer:=@w_shared2Server;
  shared1.OnMessage:=@w_shared1Message;
  shared2.OnMessage:=@w_shared2Message;
  FAppKey1:=shared1.ApplicationKey;
  FAppKey2:=shared2.ApplicationKey;
end;

destructor TExtSharedCommunication.Destroy;
begin
  shared1.Free;
  shared2.Free;
  inherited Destroy;
end;

procedure TExtSharedCommunication.Start;
begin
  shared1.Execute;
  if CCC=1 then
  begin
    shared2.Execute;
    shared1.SendMessage('{wewn_START}');
  end else
  if SSS=1 then
  begin
    shared2.Execute;
    if sss=2 then
    begin
      shared2.Uninstall;
      SSS:=1;
    end else
    if CCC=2 then shared2.SendMessage('{wewn_START}');
  end;
end;

procedure TExtSharedCommunication.Stop;
begin
  if CCC=1 then shared1.SendMessage('{wewn_STOP}') else
  if CCC=2 then shared2.SendMessage('{wewn_STOP}');
  shared1.Uninstall;
  shared2.Uninstall;
end;

procedure TExtSharedCommunication.SendMessage(aMessage: string);
begin
  case CCC of
    1: shared1.SendMessage(aMessage);
    2: shared2.SendMessage(aMessage);
  end;
end;

end.
