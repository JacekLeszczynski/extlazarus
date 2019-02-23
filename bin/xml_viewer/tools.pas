unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function MyDir(Filename:string):string;

const
  _OFF1 = false;

var
  _sciezka: string;

implementation

//Funkcja zwraca aktualny katalog z którego uruchamiany jest program.
function MyDir(Filename:string):string;
var
  s: string;
begin
  s:=ExtractFilePath(ParamStr(0));
  if Filename='' then delete(s,length(s),1) else s:=s+Filename;
  result:=s;
end;

end.

