{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ExtLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  XmlParser, CsvParser, SymfoniaParser, ExtEventLog, LogStringGrid, 
  ExtMessage, ExtParams, PointerTab, ExtLazarusTypes, extmessage_form, 
  frReportPlus, DBSchemaSync, ZEncodedDBConf, ZMasterVersionDB, 
  ZQueryRelative, lazgradient, lazflogin, LazLogin, ColorProgress, 
  DBSchemaSync_postgres, DBSourceCopyToExcel, Gradpan, ExtSharedMemory, 
  OpenGLCenter, ZTransaction, DBConfTable, UOSEngine, UOSPlayer, SoundPlayer, 
  ConsMixer, ExtShutdown, NetSynHTTP, LiveTimer, MPlayerCtrl, 
  ArchitectureOSInfo, NetSynWebSocket, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('XmlParser', @XmlParser.Register);
  RegisterUnit('CsvParser', @CsvParser.Register);
  RegisterUnit('SymfoniaParser', @SymfoniaParser.Register);
  RegisterUnit('ExtEventLog', @ExtEventLog.Register);
  RegisterUnit('LogStringGrid', @LogStringGrid.Register);
  RegisterUnit('ExtMessage', @ExtMessage.Register);
  RegisterUnit('ExtParams', @ExtParams.Register);
  RegisterUnit('PointerTab', @PointerTab.Register);
  RegisterUnit('frReportPlus', @frReportPlus.Register);
  RegisterUnit('DBSchemaSync', @DBSchemaSync.Register);
  RegisterUnit('ZEncodedDBConf', @ZEncodedDBConf.Register);
  RegisterUnit('ZMasterVersionDB', @ZMasterVersionDB.Register);
  RegisterUnit('ZQueryRelative', @ZQueryRelative.Register);
  RegisterUnit('lazgradient', @lazgradient.Register);
  RegisterUnit('LazLogin', @LazLogin.Register);
  RegisterUnit('ColorProgress', @ColorProgress.Register);
  RegisterUnit('DBSchemaSync_postgres', @DBSchemaSync_postgres.Register);
  RegisterUnit('DBSourceCopyToExcel', @DBSourceCopyToExcel.Register);
  RegisterUnit('Gradpan', @Gradpan.Register);
  RegisterUnit('ExtSharedMemory', @ExtSharedMemory.Register);
  RegisterUnit('OpenGLCenter', @OpenGLCenter.Register);
  RegisterUnit('ZTransaction', @ZTransaction.Register);
  RegisterUnit('DBConfTable', @DBConfTable.Register);
  RegisterUnit('UOSEngine', @UOSEngine.Register);
  RegisterUnit('UOSPlayer', @UOSPlayer.Register);
  RegisterUnit('SoundPlayer', @SoundPlayer.Register);
  RegisterUnit('ConsMixer', @ConsMixer.Register);
  RegisterUnit('ExtShutdown', @ExtShutdown.Register);
  RegisterUnit('NetSynHTTP', @NetSynHTTP.Register);
  RegisterUnit('LiveTimer', @LiveTimer.Register);
  RegisterUnit('MPlayerCtrl', @MPlayerCtrl.Register);
  RegisterUnit('ArchitectureOSInfo', @ArchitectureOSInfo.Register);
  RegisterUnit('NetSynWebSocket', @NetSynWebSocket.Register);
end;

initialization
  RegisterPackage('ExtLazarus', @Register);
end.
