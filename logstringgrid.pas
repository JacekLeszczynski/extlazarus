unit LogStringGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids;

type

  { TLogStringGrid }

  TLogStringGrid = class(TStringGrid)
  private
    { Private declarations }
    czas_start,czas_zero: TDateTime;
    FBoldTitle: boolean;
    procedure init;
    procedure LogDblClick(Sender:TObject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    errors: integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Prepare;
    procedure LogReset;
    procedure LogReset(ForcedTime:TDateTime);
    procedure Login(EventType:TEventType;Description:string;StrType:string=''); {EventType: etCustom,etInfo,etWarning,etError,etDebug}
    procedure LoginInfo(Description:string;StrType:string='');
    procedure LoginError(Description:string;StrType:string='');
  published
    { Published declarations }
    property BoldTitle: boolean read FBoldTitle write FBoldTitle default false;
  end;

procedure Register;

implementation

uses
  extlazarus_utf8;

procedure Register;
begin
  RegisterComponents('System',[TLogStringGrid]);
end;

{ TLogStringGrid }

procedure TLogStringGrid.init;
begin
  self.OnDblClick:=@LogDblClick;
  self.FixedCols:=0;
  self.RowCount:=5;
  self.ColCount:=4;
  self.ColWidths[0]:=50;
  self.ColWidths[1]:=50;
  self.ColWidths[2]:=39;
  self.ColWidths[3]:=485;
  self.Cells[0,0]:='Lp.';
  self.Cells[1,0]:='Czas';
  self.Cells[2,0]:='Typ';
  self.Cells[3,0]:='Opis';
  self.Cells[3,1]:=com_6;
  self.Cells[3,2]:=com_7;
  self.Cells[3,3]:=com_8;
  self.Cells[3,4]:=com_9;
  FBoldTitle:=false;
end;

procedure TLogStringGrid.LogDblClick(Sender: TObject);
begin
  ShowMessage(trim(self.Cells[self.Col,self.Row]));
end;

constructor TLogStringGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  init;
end;

destructor TLogStringGrid.Destroy;
begin
  inherited Destroy;
end;

procedure TLogStringGrid.Prepare;
begin
  self.Columns.Clear;
  self.Columns.Add;
  self.Columns.Add;
  self.Columns.Add;
  self.Columns.Add;
  self.Columns[0].Title.Caption:='Lp.';
  self.Columns[0].Width:=50;
  if FBoldTitle then self.Columns[0].Title.Font.Style:=[fsBold];
  self.Columns[0].Alignment:=taRightJustify;
  self.Columns[1].Title.Caption:='Czas';
  self.Columns[1].Width:=50;
  if FBoldTitle then self.Columns[1].Title.Font.Style:=[fsBold];
  self.Columns[2].Title.Caption:='Typ';
  self.Columns[2].Width:=39;
  if FBoldTitle then self.Columns[2].Title.Font.Style:=[fsBold];
  self.Columns[2].Alignment:=taCenter;
  self.Columns[2].Font.Color:=clRed;
  if FBoldTitle then self.Columns[2].Font.Style:=[fsBold];
  self.Columns[3].Title.Caption:='Opis';
  self.Columns[3].Width:=485;
  if FBoldTitle then self.Columns[3].Title.Font.Style:=[fsBold];
  LogReset;
end;

procedure TLogStringGrid.LogReset;
begin
  errors:=0;
  czas_start:=now;
  czas_zero:=czas_start;
  self.Clean([gzNormal]);
  self.RowCount:=1;
end;

procedure TLogStringGrid.LogReset(ForcedTime: TDateTime);
begin
  errors:=0;
  czas_start:=ForcedTime;
  czas_zero:=ForcedTime;
  self.Clean([gzNormal]);
  self.RowCount:=1;
end;

procedure TLogStringGrid.Login(EventType: TEventType; Description: string;
  StrType: string);
var
  td: TDateTime;
  i: integer;
  s: string;
begin
  td:=now-czas_start;
  if (EventType=etError) or (EventType=etDebug) then inc(errors);
  {EventType: etCustom,etInfo,etWarning,etError,etDebug}
  if StrType='' then case EventType of
                       etCustom:  s:='C';
                       etInfo:    s:='';
                       etWarning: s:='W';
                       etError:   s:='E';
                       etDebug:   s:='D';
                     end else s:=StrType;
  i:=self.RowCount;
  self.InsertColRow(False,i);
  self.Cells[0,i]:=IntToStr(i)+'.';
  self.Cells[1,i]:=FormatDateTime('nn:ss',td);
  self.Cells[2,i]:=s;
  self.Cells[3,i]:=Description;
  Application.ProcessMessages;
end;

procedure TLogStringGrid.LoginInfo(Description: string; StrType: string);
begin
  Login(etInfo,Description,StrType);
end;

procedure TLogStringGrid.LoginError(Description: string; StrType: string);
begin
  Login(etError,Description,StrType);
end;

initialization
  {$I logstringgrid_images.lrs}

end.
