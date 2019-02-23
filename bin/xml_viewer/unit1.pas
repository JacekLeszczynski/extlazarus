unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Grids, Buttons, XMLPropStorage, ComCtrls, XmlParser, CsvParser,
  SymfoniaParser, ExtMessage;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    csv: TCsvParser;
    Edit1: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    message: TExtMessage;
    not_filter: TCheckBox;
    FileNameEdit1: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    cc: TProgressBar;
    sg: TStringGrid;
    sparser: TSymfoniaParser;
    SpeedButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    xml: TXmlParser;
    XMLPropStorage1: TXMLPropStorage;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure csvRead(Sender: TObject; NumberRec,PosRec: integer; sName,
      sValue: string; var Stopped: boolean);
    procedure FileNameEdit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sparserRead(Sender: TObject; poziom: integer; adres, klucz,
      zmienna, wartosc: string; var Stopped: boolean);
    procedure xmlBranchBlock(Sender: TObject; poziom: integer; adres: string;
      var Stopped: boolean);
    procedure xmlBranchIn(Sender: TObject; poziom: integer; adres: string;
      var Stopped: boolean);
    procedure xmlBranchOut(Sender: TObject; poziom: integer; adres: string;
      var Stopped: boolean);
    procedure xmlError(Sender: TObject; ERR: integer; ERROR: string);
    procedure xmlProgress(Sender: TObject; vMax, vPos: integer);
    procedure xmlRead(Sender: TObject; poziom: integer; adres, klucz, zmienna,
      wartosc: string; var Stopped: boolean);
  private
    MSSQL1,MSSQL2: integer;
    CZYTAJ: boolean;
    list: TStringList;
    global_licznik: integer;
    { private declarations }
  public
    { public declarations }
  end; 

  { TRun }

  TRun = class(TThread)
  private
    { private declarations }
    str: TStringList;
    s: string;
  protected
    procedure Execute; override;
    procedure GetL1;
    procedure GetL2;
    procedure GetL3;
  public
    Constructor Create;
  end;

const
  spij = 10;

var
  Form1: TForm1;
  tab: TRun;

implementation

uses
  tools;

{ TRun }

procedure TRun.Execute;
var
  i: integer;
begin
  try
    str:=TStringList.Create;
    Synchronize(@GetL1);
    for i:=1 to str.Count do
    begin
      s:=str[i-1];
      Synchronize(@GetL2);
      if Terminated then break;
    end;
    Synchronize(@GetL3);
  finally
    str.Free;
  end;
end;

procedure TRun.GetL1;
begin
  Form1.StatusBar1.SimpleText:='Displaying Data 2/2... (please wait or plug in button STOP or EXIT)';
  Form1.BitBtn2.Enabled:=true;
  str.Assign(Form1.list);
  Form1.cc.Max:=str.Count;
  Form1.cc.Position:=0;
end;

procedure TRun.GetL2;
begin
  Form1.sg.InsertColRow(False,Form1.sg.RowCount);
  Form1.sg.Rows[Form1.sg.RowCount-1].Text:=s;
  Form1.cc.StepBy(1);
end;

procedure TRun.GetL3;
begin
  Form1.StatusBar1.SimpleText:='';
  Form1.list.Clear;
  Form1.FileNameEdit1.Enabled:=true;
  Form1.BitBtn2.Enabled:=false;
  Form1.cc.Position:=0;
end;

constructor TRun.Create;
begin
  FreeOnTerminate:=true;
  inherited Create(true);
  resume;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if BitBtn2.Enabled then tab.Terminate;
  Close;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  BitBtn2.Enabled:=false;
  tab.Terminate;
end;

procedure TForm1.csvRead(Sender: TObject; NumberRec,PosRec: integer; sName,
  sValue: string; var Stopped: boolean);
var
  s: string;
begin
  s:=sName;
  if s='' then s:=' ';
  list.Add(IntToStr(NumberRec)+#13#10+IntToStr(PosRec)+#13#10+' '+#13#10+s+#13#10+sValue);
end;

procedure TForm1.FileNameEdit1Change(Sender: TObject);
var
  plik,ext: string;
  i,j: integer;
begin
  global_licznik:=0;
  FileNameEdit1.InitialDir:=ExtractFilePath(FileNameEdit1.FileName);
  FileNameEdit1.Enabled:=false;
  StatusBar1.SimpleText:='Reading XML 1/2...';
  Application.ProcessMessages;
  MSSQL1:=0;
  MSSQL2:=0;
  plik:=AnsiToUTF8(FileNameEdit1.FileName);
  if FileExists(plik) then
  begin
    if CheckBox1.Checked then xml.SetAlg01 else xml.SetAlg02;
    ext:=ExtractFileExt(plik);
    sg.Clean([gzNormal]);
    sg.RowCount:=1;
    if (UpCase(ext)='.XML') or (UpCase(ext)='.ZIP') then
    begin
      xml.Filename:=plik;
      case ComboBox1.ItemIndex of
        0: xml.Encoding:=eAuto;
        1: xml.Encoding:=eUTF8;
        2: xml.Encoding:=eWindows1250;
        3: xml.Encoding:=eISO_8859_2;
      end;
      case ComboBox2.ItemIndex of
        0: xml.Contener:=crNone;
        1: xml.Contener:=crZIP;
        2: xml.Contener:=crDES;
      end;
      try
        xml.Execute;
      except
        message.ShowError('Błąd odczytu, prawdopodobnie jest źle oznakowana strona kodowa pliku XML.');
        FileNameEdit1.Enabled:=true;
        exit;
      end;
      if MSSQL1=10 then
      begin
        sg.Clean([gzNormal]);
        sg.RowCount:=1;
        try
          xml.Execute;
        except
          message.ShowError('Błąd odczytu, prawdopodobnie jest źle oznakowana strona kodowa pliku XML.');
          FileNameEdit1.Enabled:=true;
          exit;
        end;
      end;
      not_filter.Enabled:=MSSQL1=10;
    end;
    if UpCase(ext)='.CSV' then
    begin
      csv.Filename:=plik;
      csv.Execute;
    end;
    if UpCase(ext)='.TXT' then
    begin
      sparser.Filename:=plik;
      sparser.Execute;
    end;
  end;
  //odpalenie wątku
  tab:=TRun.Create;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s: string;
begin
  if FileExists(MyDir('local_xml_viewer.xml')) then Form1.Position:=poDesigned;
  s:=ParamStr(1);
  //message.ShowMessage(s);
  FileNameEdit1.InitialDir:=s;
  List:=TStringList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  list.Free;
end;

procedure TForm1.sparserRead(Sender: TObject; poziom: integer; adres, klucz,
  zmienna, wartosc: string; var Stopped: boolean);
var
  s: string;
begin
  s:=zmienna;
  if s='' then s:=' ';
  CZYTAJ:=false;
  list.Add(IntToStr(poziom)+#13#10+adres+#13#10+klucz+#13#10+s+#13#10+wartosc);
end;

procedure TForm1.xmlBranchBlock(Sender: TObject; poziom: integer;
  adres: string; var Stopped: boolean);
begin
  list.Add(IntToStr(poziom)+#13#10+adres+#13#10+'<BLOCK>'+#13#10+' '+#13#10+' ');
end;

procedure TForm1.xmlBranchIn(Sender: TObject; poziom: integer; adres: string;
  var Stopped: boolean);
begin
  list.Add(IntToStr(poziom)+#13#10+adres+#13#10+'<IN>'+#13#10+' '+#13#10+' ');
end;

procedure TForm1.xmlBranchOut(Sender: TObject; poziom: integer; adres: string;
  var Stopped: boolean);
begin
  list.Add(IntToStr(poziom)+#13#10+adres+#13#10+'<OUT>'+#13#10+' '+#13#10+' ');
end;

procedure TForm1.xmlError(Sender: TObject; ERR: integer; ERROR: string);
begin
  ShowMessage(ERROR);
end;

procedure TForm1.xmlProgress(Sender: TObject; vMax, vPos: integer);
begin
  cc.Max:=vMax;
  cc.Position:=vPos;
  cc.Refresh;
end;

procedure TForm1.xmlRead(Sender: TObject; poziom: integer; adres, klucz,
  zmienna, wartosc: string; var Stopped: boolean);
var
  s: string;
begin
  inc(global_licznik);
  if CheckBox2.Checked and (global_licznik>2000) then Stopped:=true;
  s:=zmienna;
  if s='' then s:=' ';
  //test MSSQL
  if (MSSQL1<3) then
  begin
    inc(MSSQL1);
    if (adres='/TraceData') and (zmienna='xmlns') and (wartosc='http://tempuri.org/TracePersistence.xsd') then inc(MSSQL2);
    if (adres='/TraceData/Header/TraceProvider') and (zmienna='name') and (wartosc='Microsoft SQL Server') then inc(MSSQL2);
    if MSSQL2>1 then
    begin
      MSSQL1:=10;
      Stopped:=true;
      CZYTAJ:=false;
    end;
  end;
  if (MSSQL1<>10) or ((MSSQL1=10) and CZYTAJ) or ((MSSQL1=10) and (not_filter.Checked)) then
  begin
    //wczytuję rekord
    CZYTAJ:=false;
    list.Add(IntToStr(poziom)+#13#10+adres+#13#10+klucz+#13#10+s+#13#10+wartosc);
  end;
  if (MSSQL1=10) and (not CZYTAJ) and (not not_filter.Checked) then
  begin
    //filtruję MSSQL TraceData
    CZYTAJ:=
      (adres='/TraceData/Events/Event/Column') and (zmienna='name') and (wartosc='TextData');
  end;
end;

end.
