//==============================================================================
// Update form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 15/03/2003
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Update;

//==============================================================================

Interface

//==============================================================================

Uses
   ComCtrls, Controls, StdCtrls, ExtCtrls, Classes, Forms, IdComponent, Generic;

//==============================================================================

Type
   TUpdateForm = Class(TGenericForm)
    CancelBtn: TButton;
    ElapsedLab: TLabel;
    ElapsedVal: TLabel;
    RemainingLab: TLabel;
    RemainingVal: TLabel;
    TransferRateLab: TLabel;
    TransferRateVal: TLabel;
    CurrentVersionLab: TLabel;
    CurrentVersionVal: TLabel;
    NewVersionLab: TLabel;
    NewVersionVal: TLabel;
    Timer: TTimer;
    ProgressBar: TProgressBar;
    procedure CancelBtnClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
      Private
         Startup: Boolean;
         StopDownloadingUpdate: Boolean;

         UpdateFile: TFileStream;

         StartingTime: Int64;
         ElapsedTime: Int64;

         Procedure IdHTTPWorkBegin(aSender: TObject; aWorkMode: TWorkMode; aWorkCountMax: Integer);
         Procedure IdHTTPWork(aSender: TObject; aWorkMode: TWorkMode; aWorkCount: Integer);
         Procedure IdHTTPWorkEnd(aSender: TObject; aWorkMode: TWorkMode);

      Public
         Constructor Create(aOwner: TComponent; Const aStartup: Boolean; Const aLatestCORVersion: String); Reintroduce;
         Destructor Destroy; Override;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   Windows, SysUtils, DateUtils, Consts, Common, CORCommon;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TUpdateForm.Create(aOwner: TComponent; Const aStartup: Boolean;
                               Const aLatestCORVersion: String);
Begin
   Inherited Create(aOwner);

   Caption := COR_NAME+' Update';

   Startup := aStartup;

   // No need to waste time checking whether the URL file really exists,
   // because if it did NOT, we wouldn't be here...

   CurrentVersionVal.Caption := COR_VERSION;
   NewVersionVal.Caption     := aLatestCORVersion;

   // Handle some events

   With IdHTTP Do Begin
      OnWorkBegin := IdHTTPWorkBegin;
      OnWork      := IdHTTPWork;
      OnWorkEnd   := IdHTTPWorkEnd;
   End;

   // Prepare the local update file

   UpdateFile := TFileStream.Create(UPDATED_COR_FILENAME, fmCreate);
End;

//==============================================================================

Destructor TUpdateForm.Destroy;
Begin
   UpdateFile.Free;

   If (StopDownloadingUpdate) Then
      // We stopped the download, so...
      // Note: this can only be done after "UpdateFile" has been freed, since
      //       it "owns" the file...

      DeleteFile(UPDATED_COR_FILENAME);

   With IdHTTP Do Begin
      OnWorkBegin := Nil;
      OnWork      := Nil;
      OnWorkEnd   := Nil;
   End;

   Inherited;
End;

//==============================================================================

Procedure TUpdateForm.IdHTTPWorkBegin(aSender: TObject; aWorkMode: TWorkMode;
                                      aWorkCountMax: Integer);
Begin
   // Use the size of the file as the maximum position for the progress bar

   ProgressBar.Max := aWorkCountMax;

   // Get ready for the statistics

   ElapsedTime := 0;

   StartingTime := TimerVal;
End;

//==============================================================================

Procedure TUpdateForm.IdHTTPWork(aSender: TObject; aWorkMode: TWorkMode;
                                 aWorkCount: Integer);
Var
   NbOfBytesPerSec: Integer;
   ElapseTimeInSeconds: Int64;
Begin
   If (StopDownloadingUpdate) Then
      IdHTTP.Disconnect
   Else Begin
      ElapsedTime := ElapsedTime+TimerVal-StartingTime;

      ElapseTimeInSeconds := Round(TimerValToSec(ElapsedTime));

      ProgressBar.Position := aWorkCount;

      ElapsedVal.Caption := FormatDateTime('hh:mm:ss', IncSecond(0, ElapseTimeInSeconds));

      RemainingVal.Caption := FormatDateTime('hh:mm:ss', IncSecond(0, Round(ElapseTimeInSeconds*((ProgressBar.Max-ProgressBar.Position)/ProgressBar.Position))));

      NbOfBytesPerSec := Round(1000*(ProgressBar.Position/TimerValToMSec(ElapsedTime)));

      TransferRateVal.Caption := ConvertBytes(NbOfBytesPerSec)+'/s';

      // Make sure that the new values are updated...

      Application.ProcessMessages;

      StartingTime := TimerVal;
   End;
End;

//==============================================================================

Procedure TUpdateForm.IdHTTPWorkEnd(aSender: TObject; aWorkMode: TWorkMode);
Var
   TextFileID: TextFile;
   OldCORFileName: String;
Begin
   If (Not StopDownloadingUpdate) Then Begin
      // Generate the updater

      AssignFile(TextFileID, UPDATE_COR_FILENAME);

      Rewrite(TextFileID);

      WriteLn(TextFileID, '@Echo Off');
      WriteLn(TextFileID);
      WriteLn(TextFileID, 'If Not Exist "'+UPDATED_COR_FILENAME+'" Goto End');
      WriteLn(TextFileID);

      // We swap the new and old COR files. Ideally, one would erase the old
      // one, but the fact is that Windows XP/Vista doesn't seem to like that...
      // Not nice, since the user will have to delete that file manually, but we
      // don't really have a choice, because we must update the COR file, so...
      // However, we can keep track of the fat that we have move the old COR
      // file in the temporary directory, so that we can delete it the next time
      // COR is run... Nice trick, eh?! :)

      OldCORFileName := TempDir+COR_NAME+'-'+COR_VERSION+'.exe';

      WriteLn(TextFileID, 'Move "'+Application.ExeName+'" "'+OldCORFileName+'"');
      WriteLn(TextFileID);
      WriteLn(TextFileID, 'Move "'+UPDATED_COR_FILENAME+'" "'+Application.ExeName+'"');
      WriteLn(TextFileID);

      If (Startup) Then Begin
         WriteLn(TextFileID, '"'+Application.ExeName+'"');
         WriteLn(TextFileID);
      End;

      WriteLn(TextFileID, ':End');
      WriteLn(TextFileID);
      Write(TextFileID, 'Del %0');

      CloseFile(TextFileID);

      CORUpdateAvailable := True;

      TrackOldCOR(OldCORFileName);

      Tag := UPDATE_MUST_RESTART_COR;
   End;
End;

//==============================================================================

Procedure TUpdateForm.TimerTimer(Sender: TObject);
Begin
   // The only way to start the download seems to be through the use of a timer,
   // so...

   Timer.Enabled := False;

   Try
      IdHTTP.Get(COR_UPDATE_EXE, UpdateFile);
      
      IdHTTP.Disconnect;
   Finally
      Close;
   End;
End;

//==============================================================================

Procedure TUpdateForm.CancelBtnClick(Sender: TObject);
Begin
   StopDownloadingUpdate := True;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

