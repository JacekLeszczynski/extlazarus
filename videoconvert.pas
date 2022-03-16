unit VideoConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type
  TVideoConvert = class(TComponent)
  private

  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I videoconvert_icon.lrs}
  RegisterComponents('Multimedia',[TVideoConvert]);
end;

end.
