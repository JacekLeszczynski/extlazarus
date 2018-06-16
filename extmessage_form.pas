unit extmessage_form;

interface

uses
  {$IFNDEF FPC AND $IFDEF MSWINDOWS}
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;
  {$ELSE}
  Classes, SysUtils, ExtLazarusTypes,
  LResources, Controls, Graphics,
  Forms, Dialogs, StdCtrls, ExtCtrls;
  {$ENDIF}

type
  TFExtMessageForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    StrCaption: string;
  end;

implementation

{$R *.dfm}

procedure TFExtMessageForm.FormShow(Sender: TObject);
begin
  Label1.Caption:=StrCaption;
end;

end.
