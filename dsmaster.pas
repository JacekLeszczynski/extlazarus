unit DSMaster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DB;

type

  { TDSMaster }

  TDSMaster = class(TComponent)
  private
    FDataSource: TDataSource;
    FItems: TStrings;
    procedure SetItems(AValue: TStrings);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
  published
    //Źródło nadrzędne
    property DataSource: TDataSource read FDataSource write FDataSource;
    //Lista źródeł w kolejności otwierania
    //Zamykanie nastąpi w odwrotnej kolejności
    property Items: TStrings read FItems write SetItems;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I dsmaster_icon.lrs}
  RegisterComponents('Data Access',[TDSMaster]);
end;

{ TDSMaster }

procedure TDSMaster.SetItems(AValue: TStrings);
begin
  FItems.Assign(AValue);
end;

constructor TDSMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems:=TStringList.Create;
end;

destructor TDSMaster.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TDSMaster.Open;
var
  i,j: integer;
  ds: TDataSource;
begin
  FDataSource.DataSet.Open;
  for i:=0 to FItems.Count-1 do
  begin
    for j:=0 to Owner.ComponentCount-1 do
    begin
      if Owner.Components[j].ClassType.ClassName<>'TDataSource' then continue;
      if Owner.Components[j].Name=FItems[i] then
      begin
        ds:=TDataSource(Owner.Components[j]);
        ds.DataSet.Open;
        break;
      end;
    end;
  end;
end;

procedure TDSMaster.Close;
var
  i,j: integer;
  ds: TDataSource;
begin
  for i:=FItems.Count-1 downto 0 do
  begin
    for j:=0 to Owner.ComponentCount-1 do
    begin
      if Owner.Components[j].ClassType.ClassName<>'TDataSource' then continue;
      if Owner.Components[j].Name=FItems[i] then
      begin
        ds:=TDataSource(Owner.Components[j]);
        ds.DataSet.Close;
        break;
      end;
    end;
  end;
  FDataSource.DataSet.Close;
end;

end.
