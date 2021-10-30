{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ExtLazarus;

{$warn 5023 off : no warning about unused units}
interface

uses
  XmlParser, CsvParser, SymfoniaParser, ExtEventLog, LogStringGrid, 
  ExtMessage, ExtParams, PointerTab, ExtLazarusTypes, extmessage_form, 
  frReportPlus, ZEncodedDBConf, ZMasterVersionDB, ZQueryRelative, lazgradient, 
  lazflogin, LazLogin, ColorProgress, DBSchemaSync_postgres, DBSchemaSync, 
  DBSchemaSyncSqlite, DBSourceCopyToExcel, Gradpan, ExtSharedMemory, 
  OpenGLCenter, ZTransaction, DBConfTable, UOSEngine, UOSPlayer, SoundPlayer, 
  ConsMixer, ExtShutdown, NetSynHTTP, LiveTimer, MPlayerCtrl, 
  ArchitectureOSInfo, NetSocket, NetSynWebSocket, Polfan, GoogleTranslator, 
  DirectoryPack, Presentation, DSMaster, FullscreenMenu, ExtDiff, 
  compressionfly, RNL, DBGridPlus, ZQueryPlus, ecode_unit, list_unit, Upnp, 
  YoutubeDownloader, LazarusPackageIntf;

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
  RegisterUnit('ZEncodedDBConf', @ZEncodedDBConf.Register);
  RegisterUnit('ZMasterVersionDB', @ZMasterVersionDB.Register);
  RegisterUnit('ZQueryRelative', @ZQueryRelative.Register);
  RegisterUnit('lazgradient', @lazgradient.Register);
  RegisterUnit('LazLogin', @LazLogin.Register);
  RegisterUnit('ColorProgress', @ColorProgress.Register);
  RegisterUnit('DBSchemaSync_postgres', @DBSchemaSync_postgres.Register);
  RegisterUnit('DBSchemaSync', @DBSchemaSync.Register);
  RegisterUnit('DBSchemaSyncSqlite', @DBSchemaSyncSqlite.Register);
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
  RegisterUnit('NetSocket', @NetSocket.Register);
  RegisterUnit('NetSynWebSocket', @NetSynWebSocket.Register);
  RegisterUnit('Polfan', @Polfan.Register);
  RegisterUnit('GoogleTranslator', @GoogleTranslator.Register);
  RegisterUnit('DirectoryPack', @DirectoryPack.Register);
  RegisterUnit('Presentation', @Presentation.Register);
  RegisterUnit('DSMaster', @DSMaster.Register);
  RegisterUnit('FullscreenMenu', @FullscreenMenu.Register);
  RegisterUnit('ExtDiff', @ExtDiff.Register);
  RegisterUnit('compressionfly', @compressionfly.Register);
  RegisterUnit('DBGridPlus', @DBGridPlus.Register);
  RegisterUnit('ZQueryPlus', @ZQueryPlus.Register);
  RegisterUnit('Upnp', @Upnp.Register);
  RegisterUnit('YoutubeDownloader', @YoutubeDownloader.Register);
end;

initialization
  RegisterPackage('ExtLazarus', @Register);
end.
