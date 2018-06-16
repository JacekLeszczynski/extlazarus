{
   File generated automatically by Lazarus Package Manager

   fpmake.pp for ExtLazarus 0.0

   This file was generated on 14.06.2018
}

{$ifndef ALLPACKAGES} 
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_ExtLazarus(const ADirectory: string);

var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
    P:=AddPackage('extlazarus');
    P.Version:='0.0';

    P.Directory:=ADirectory;

    P.Flags.Add('LazarusDsgnPkg');

    P.Dependencies.Add('lazopenglcontext');
    P.Dependencies.Add('bgrabitmappack');
    P.Dependencies.Add('zcomponent');
    P.Dependencies.Add('lazreport');
    P.Dependencies.Add('dcpcrypt');
    P.Dependencies.Add('lcl');
    P.Dependencies.Add('fcl');
    P.Options.Add('-MObjFPC');
    P.Options.Add('-Scghi');
    P.Options.Add('-O1');
    P.Options.Add('-g');
    P.Options.Add('-gl');
    P.Options.Add('-l');
    P.Options.Add('-vewnhibq');
    P.Options.Add('-dLCL');
    P.Options.Add('-dLCL$(LCLWidgetType)');
    P.UnitPath.Add('../../lib');
    P.UnitPath.Add('.');
    T:=P.Targets.AddUnit('extlazarus.pas');
    t.Dependencies.AddUnit('xmlparser');
    t.Dependencies.AddUnit('csvparser');
    t.Dependencies.AddUnit('symfoniaparser');
    t.Dependencies.AddUnit('exteventlog');
    t.Dependencies.AddUnit('logstringgrid');
    t.Dependencies.AddUnit('extmessage');
    t.Dependencies.AddUnit('extparams');
    t.Dependencies.AddUnit('pointertab');
    t.Dependencies.AddUnit('extlazarustypes');
    t.Dependencies.AddUnit('extmessage_form');
    t.Dependencies.AddUnit('frreportplus');
    t.Dependencies.AddUnit('dbschemasync');
    t.Dependencies.AddUnit('zencodeddbconf');
    t.Dependencies.AddUnit('zmasterversiondb');
    t.Dependencies.AddUnit('zqueryrelative');
    t.Dependencies.AddUnit('lazgradient');
    t.Dependencies.AddUnit('lazflogin');
    t.Dependencies.AddUnit('lazlogin');
    t.Dependencies.AddUnit('colorprogress');
    t.Dependencies.AddUnit('dbschemasync_postgres');
    t.Dependencies.AddUnit('dbsourcecopytoexcel');
    t.Dependencies.AddUnit('gradpan');
    t.Dependencies.AddUnit('extsharedmemory');
    t.Dependencies.AddUnit('openglcenter');
    t.Dependencies.AddUnit('ztransaction');
    t.Dependencies.AddUnit('dbconftable');
    t.Dependencies.AddUnit('uosengine');
    t.Dependencies.AddUnit('uosplayer');
    t.Dependencies.AddUnit('soundplayer');
    t.Dependencies.AddUnit('consmixer');
    t.Dependencies.AddUnit('extshutdown');
    t.Dependencies.AddUnit('netsynhttp');
    t.Dependencies.AddUnit('asn1util');

    T:=P.Targets.AddUnit('xmlparser.pas');
    T:=P.Targets.AddUnit('csvparser.pas');
    T:=P.Targets.AddUnit('symfoniaparser.pas');
    T:=P.Targets.AddUnit('exteventlog.pas');
    T:=P.Targets.AddUnit('logstringgrid.pas');
    T:=P.Targets.AddUnit('extmessage.pas');
    T:=P.Targets.AddUnit('extparams.pas');
    T:=P.Targets.AddUnit('pointertab.pas');
    T:=P.Targets.AddUnit('extlazarustypes.pas');
    T:=P.Targets.AddUnit('extmessage_form.pas');
    T:=P.Targets.AddUnit('frreportplus.pas');
    T:=P.Targets.AddUnit('dbschemasync.pas');
    T:=P.Targets.AddUnit('zencodeddbconf.pas');
    T:=P.Targets.AddUnit('zmasterversiondb.pas');
    T:=P.Targets.AddUnit('zqueryrelative.pas');
    T:=P.Targets.AddUnit('lazgradient.pas');
    T:=P.Targets.AddUnit('lazflogin.pas');
    T:=P.Targets.AddUnit('lazlogin.pas');
    T:=P.Targets.AddUnit('colorprogress.pas');
    P.Targets.AddImplicitUnit('dbschemasync_postgres.pas');
    T:=P.Targets.AddUnit('dbsourcecopytoexcel.pas');
    T:=P.Targets.AddUnit('gradpan.pas');
    T:=P.Targets.AddUnit('extsharedmemory.pas');
    T:=P.Targets.AddUnit('openglcenter.pas');
    T:=P.Targets.AddUnit('ztransaction.pas');
    T:=P.Targets.AddUnit('dbconftable.pas');
    T:=P.Targets.AddUnit('uosengine.pas');
    T:=P.Targets.AddUnit('uosplayer.pas');
    T:=P.Targets.AddUnit('soundplayer.pas');
    T:=P.Targets.AddUnit('consmixer.pas');
    T:=P.Targets.AddUnit('extshutdown.pas');
    T:=P.Targets.AddUnit('netsynhttp.pas');
    T:=P.Targets.AddUnit('asn1util.pas');

    // copy the compiled file, so the IDE knows how the package was compiled
    P.InstallFiles.Add('ExtLazarus.compiled',AllOSes,'$(unitinstalldir)');

    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_ExtLazarus('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
