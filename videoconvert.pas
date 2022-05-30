unit VideoConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Process, AsyncProcess;

type

  { TVideoConvert }

  TVideoConvert = class(TComponent)
  private
    proc: TAsyncProcess;
  protected
  public
    procedure ConvertToOgg(aSourceFileName: string; aQuality: integer = -2; aChannels: integer = 0);
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I videoconvert_icon.lrs}
  RegisterComponents('Multimedia',[TVideoConvert]);
end;

{ TVideoConvert }

procedure TVideoConvert.ConvertToOgg(aSourceFileName: string;
  aQuality: integer; aChannels: integer);
var
  dir,f1,f2: string;
begin
  dir:=ExtractFilePath(aSourceFileName);
  f1:=ExtractFileName(aSourceFileName);
  f2:=ChangeFileExt(f1,'.ogg');
  proc:=TAsyncProcess.Create(self);
  proc.CurrentDirectory:=dir;
  proc.Executable:='ffmpeg';
  //proc.Options:=[poWaitOnExit,poUsePipes,poNoConsole];
  proc.Options:=[poWaitOnExit,poNoConsole];
  proc.Priority:=ppNormal;
  proc.ShowWindow:=swoNone;
  //proc.OnReadData:=@YTReadData;
  //-i madonna.mp4 -vn -acodec libvorbis -q:a 5 -y madonna.ogg
  proc.Parameters.Add('-i');
  proc.Parameters.Add(f1);
  if aChannels>0 then
  begin
    proc.Parameters.Add('-ac');
    proc.Parameters.Add(IntToStr(aChannels));
  end;
  proc.Parameters.Add('-vn');
  proc.Parameters.Add('-acodec');
  proc.Parameters.Add('libvorbis');
  if aQuality>-2 then
  begin
    proc.Parameters.Add('-q:a');
    proc.Parameters.Add(IntToStr(aQuality));
  end;
  proc.Parameters.Add('-y');
  proc.Parameters.Add(f2);
  try
    proc.Execute;
  finally
    proc.Terminate(0);
    proc.Free;
  end;
end;

end.
