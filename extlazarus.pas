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
  ExtSharedCommunication, OpenGLCenter, ZTransaction, DBConfTable, UOSEngine, 
  UOSPlayer, SoundPlayer, ConsMixer, ExtShutdown, NetSynHTTP, LiveTimer, 
  MPlayerCtrl, ArchitectureOSInfo, NetSocket, NetSynWebSocket, Polfan, 
  GoogleTranslator, DirectoryPack, Presentation, DSMaster, FullscreenMenu, 
  ExtDiff, compressionfly, RNL, DBGridPlus, ZQueryPlus, list_unit, Upnp, 
  YoutubeDownloader, StringsList, VideoConvert, ScrollingText, 
  AboutScrolltextunit, LiveChat, ChatGPT, LuksCrypter, YearCalendar, 
  LazarusPackageIntf;

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
  RegisterUnit('ExtSharedCommunication', @ExtSharedCommunication.Register);
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
  RegisterUnit('StringsList', @StringsList.Register);
  RegisterUnit('VideoConvert', @VideoConvert.Register);
  RegisterUnit('ScrollingText', @ScrollingText.Register);
  RegisterUnit('AboutScrolltextunit', @AboutScrolltextunit.Register);
  RegisterUnit('LiveChat', @LiveChat.Register);
  RegisterUnit('ChatGPT', @ChatGPT.Register);
  RegisterUnit('LuksCrypter', @LuksCrypter.Register);
  RegisterUnit('YearCalendar', @YearCalendar.Register);
end;

initialization
  RegisterPackage('ExtLazarus', @Register);
end.
