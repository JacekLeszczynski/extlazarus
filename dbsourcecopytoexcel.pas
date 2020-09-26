unit DBSourceCopyToExcel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, DB, ClipBrd;

type

  { TDBSourceCopyToExcel }

  TDBSourceCopyToExcel = class(TComponent)
  private
    FData: TDataSource;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Execute;
  published
    { Published declarations }
    property DataSource: TDataSource read FData write FData;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I dbsourcecopytoexcel_icon.lrs}
  RegisterComponents('Data Controls',[TDBSourceCopyToExcel]);
end;

{ TDBSourceCopyToExcel }

procedure TDBSourceCopyToExcel.Execute;
var
  mem: TStringList;
  t: TBookmark;
  znak: string;
  i,ostatni: integer;
  s: string;
begin
  znak:='"'+#9+'"';
  mem:=TStringList.Create;
  try
    mem.Clear;

    s:='"';
    ostatni:=FData.DataSet.Fields.Count-1;
    for i:=0 to ostatni do
    begin
      if i=ostatni then s:=s+FData.DataSet.Fields[i].DisplayLabel+'"'
                   else s:=s+FData.DataSet.Fields[i].DisplayLabel+znak;
    end;
    mem.Add(s);

    FData.DataSet.DisableControls;
    t:=FData.DataSet.GetBookmark;
    FData.DataSet.First;
    while not FData.DataSet.EOF do
    begin
      s:='"';
      for i:=0 to ostatni do
      begin
        if i=ostatni then s:=s+FData.DataSet.Fields[i].AsString+'"'
                     else s:=s+FData.DataSet.Fields[i].AsString+znak;
      end;
      mem.Add(s);
      FData.DataSet.Next;
    end;
    FData.DataSet.GotoBookmark(t);
    FData.DataSet.EnableControls;

    Clipboard.AsText:=mem.Text;
  finally
    mem.Free;
  end;
end;

end.
