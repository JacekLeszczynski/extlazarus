unit frReportPlus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LR_Class;

type

  { TfrReportPlus }

  TfrReportPlus = class(TfrReport)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure SaveToBin(sFile:string);
    procedure LoadFromBin(sFile:string);
    procedure LoadFromResource(sName:string);
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses
  LCLType;

procedure Register;
begin
  {$I extlazreport_icon.lrs}
  RegisterComponents('LazReport',[TfrReportPlus]);
end;

{ TfrReportPlus }

procedure TfrReportPlus.SaveToBin(sFile: string);
var
  f: TMemoryStream;
begin
  try
    f:=TMemoryStream.Create;
    self.SaveToStream(f);
    f.SaveToFile(sFile);
  finally
    f.Free;
  end;
end;

procedure TfrReportPlus.LoadFromBin(sFile: string);
var
  f: TMemoryStream;
begin
  try
    f:=TMemoryStream.Create;
    f.LoadFromFile(sFile);
    self.LoadFromStream(f);
  finally
    f.Free;
  end;
end;

procedure TfrReportPlus.LoadFromResource(sName: string);
var
  res: TResourceStream;
begin
  try
    res:=TResourceStream.Create(hInstance,sName,RT_RCDATA);
    self.LoadFromStream(res);
  finally
    res.Free;
  end;
end;

end.
