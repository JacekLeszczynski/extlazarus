unit ExtLazarusTypes;

{$IFNDEF FPC AND $IFDEF MSWINDOWS}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE LAZARUS}
{$ENDIF}

{$IFDEF LAZARUS}
{$mode objfpc}{$H+}
{$ENDIF}

interface

type
  TConvertString = (csLowerCase, csDeleteSpaces, csDelUnderlines, csDelMinuses, csDelSlashes);
  TSetConvertString = set of TConvertString;
  TAutoEncoding   = (isoDefault, iso1250, isoAUTO);
  TWewnMsgDlgType = (wmtMessage, wmtWarning, wmtError, wmtInformation, wmtConfirmation, wmtCustom);
  TPositionDlg    = (psScreenCenter, psMainFormCenter, psOwnerFormCenter, psTopLeft);
  TDefMsgDlgBtn   = (dbNone, dbYes, dbNo, dbOK, dbCancel, dbAbort, dbRetry, dbIgnore,
                     dbAll, dbNoToAll, dbYesToAll, dbHelp {$IFDEF LAZARUS}, dbClose {$ENDIF});
  TResultDlgBtn   = (rsNone, rsYes, rsNo, rsOK, rsCancel, rsAbort, rsRetry, rsIgnore,
                     rsAll, rsNoToAll, rsYesToAll, rsHelp {$IFDEF LAZARUS}, rsClose {$ENDIF});
  TOptionExecute = (oeShow,oeShowModal);
  TPositionForm  = (ppDefault, ppDefaultPosOnly, ppDefaultSizeOnly, ppDesigned, ppDesktopCenter, ppMainFormCenter, ppOwnerFormCenter, ppScreenCenter);

implementation

end.
