//==============================================================================
// COR project
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://COR.physiol.ox.ac.uk/
//
// Copyright 2002-2009
//------------------------------------------------------------------------------
// Date of Creation: 27/04/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// Note: do NOT add new files to the project via Delphi's IDE. That will
//       otherwise mess everything up! Do it by hand instead...
//==============================================================================
// JCL_DEBUG_EXPERT_INSERTJDBG ON
//==============================================================================

Program COR;

//==============================================================================

{$R 'Res\Resources.res' 'Res\Resources.rc'}

//==============================================================================

Uses
   FastMM4,

   FastMove,
   FastCode,

   SysUtils, Forms,

   IdGlobal, IdThreadSafe,

   JclAppInst,
   
   ExceptDlgMail,

   CellMLAPI In 'Src\CellML\CellMLAPI.pas',
   CellMLAPIEngine In 'Src\CellML\CellMLAPIEngine.pas',
   CellMLAPIExportEngine In 'Src\CellML\CellMLAPIExportEngine.pas',
   CellMLAPIToCellMLASCIIEngine In 'Src\CellML\CellMLAPIToCellMLASCIIEngine.pas',
   CellMLAPIToCellMLFileEngine In 'Src\CellML\CellMLAPIToCellMLFileEngine.pas',
   CellMLAPIToCFileEngine In 'Src\CellML\CellMLAPIToCFileEngine.pas',
   CellMLAPIToCMCFileEngine In 'Src\CellML\CellMLAPIToCMCFileEngine.pas',
   CellMLAPIToCPPFileEngine In 'Src\CellML\CellMLAPIToCPPFileEngine.pas',
   CellMLAPIToDelphiForWin32FileEngine In 'Src\CellML\CellMLAPIToDelphiForWin32FileEngine.pas',
   CellMLAPIToFortran77FileEngine In 'Src\CellML\CellMLAPIToFortran77FileEngine.pas',
   CellMLAPIToJavaFileEngine In 'Src\CellML\CellMLAPIToJavaFileEngine.pas',
   CellMLAPIToMATLABFileEngine In 'Src\CellML\CellMLAPIToMATLABFileEngine.pas',
   CellMLAPIToMCEngine In 'Src\CellML\CellMLAPIToMCEngine.pas',
   CellMLAPIToMSWordFileEngine In 'Src\CellML\CellMLAPIToMSWordFileEngine.pas',
   CellMLAPIToPascalFileEngine In 'Src\CellML\CellMLAPIToPascalFileEngine.pas',
   CellMLAPIToTeXFileEngine In 'Src\CellML\CellMLAPIToTeXFileEngine.pas',
   CellMLASCIIToCellMLAPIEngine In 'Src\CellML\CellMLASCIIToCellMLAPIEngine.pas',
   CellMLFileToCellMLAPIEngine In 'Src\CellML\CellMLFileToCellMLAPIEngine.pas',
   CellMLScannerEngine In 'Src\CellML\CellMLScannerEngine.pas',

   Cell In 'Src\Common\Cell.pas',
   Common In 'Src\Common\Common.pas',
   CORCommon In 'Src\Common\CORCommon.pas',
   Engine In 'Src\Common\Engine.pas',
{$IFDEF OPT_MATH}
   OptMath In 'Src\Common\OptMath.pas',
{$ENDIF}

   About In 'Src\GUI\About.pas' {AboutForm},
   Computation In 'Src\GUI\Computation.pas' {ComputationFrame: TFrame},
   Console In 'Src\GUI\Console.pas' {ConsoleFrame: TFrame},
   Contents In 'Src\GUI\Contents.pas' {ContentsFrame: TFrame},
   Dockable In 'Src\GUI\Dockable.pas' {DockableFrame: TFrame},
   DockSites In 'Src\GUI\DockSites.pas',
   Editor In 'Src\GUI\Editor.pas' {EditorFrame: TFrame},
   CommandViewer In 'Src\GUI\CommandViewer.pas' {CommandViewerFrame: TFrame},
   Generic In 'Src\GUI\Generic.pas' {GenericForm},
   GraphPanel In 'Src\GUI\GraphPanel.pas' {GraphPanelFrame: TFrame},
   Msg In 'Src\GUI\Msg.pas' {MsgForm},
   Msgs In 'Src\GUI\Msgs.pas' {MsgsFrame: TFrame},
   Main In 'Src\GUI\Main.pas' {MainForm},
   Options In 'Src\GUI\Options.pas' {OptionsForm},
   Password In 'Src\GUI\Password.pas' {PasswordForm},
   Properties In 'Src\GUI\Properties.pas' {PropertiesFrame},
   SplashScreen In 'Src\GUI\SplashScreen.pas' {SplashScreenForm},
   Update In 'Src\GUI\Update.pas' {UpdateForm},
   VersionInfo In 'Src\GUI\VersionInfo.pas' {VersionInfoForm},

   ODE2ndOrderRungeKuttaIntegrator In 'Src\ODE\ODE2ndOrderRungeKuttaIntegrator.pas',
   ODE4thOrderRungeKuttaIntegrator In 'Src\ODE\ODE4thOrderRungeKuttaIntegrator.pas',
   ODECVODEIntegrator In 'Src\ODE\ODECVODEIntegrator.pas',
   ODEFixedTimeStepIntegrator In 'Src\ODE\ODEFixedTimeStepIntegrator.pas',
   ODEForwardEulerIntegrator In 'Src\ODE\ODEForwardEulerIntegrator.pas',
   ODEIntegrator In 'Src\ODE\ODEIntegrator.pas',

   CmdGraph In 'Src\VCL\CmdGraph.pas',
   OGL In 'Src\VCL\OGL.pas',
   OGLGraphPanel In 'Src\VCL\OGLGraphPanel.pas',
   RippledBitmap In 'Src\VCL\RippledBitmap.pas',
   SyntaxEdit In 'Src\VCL\SyntaxEdit.pas',
   VSTListBox In 'Src\VCL\VSTListBox.pas';

//==============================================================================

{$R *.res}

//==============================================================================

Begin
   // Register two known, but expected, leaks with Indy. They are located in the
   // finalisation section of IdStack, which leaks an instance of
   // "TIdCriticalSection", and IdThread, which leaks an instance of
   // "TIdThreadSafeInteger" (and therefore also one of "TIdCriticalSection").
   // To ensure that the reporting always makes sense, we include those units in
   // the project file, so that their initialisation section is executed and the
   // leaks really occur, no matter what...

{$IFNDEF COR_RELEASE}
   RegisterExpectedMemoryLeak(TIdThreadSafeInteger, 1);
   RegisterExpectedMemoryLeak(TIdCriticalSection, 2);
{$ENDIF}

   // Set the decimal and thousand separators and prevent Windows from resetting
   // them
   // Note: this is very important, since some countries use different
   //       conventions, so...

   DecimalSeparator  := '.';
   ThousandSeparator := ',';

   Application.UpdateFormatSettings := False;

   // Delete any previous version of COR that is still in the temporary
   // directory

   DeleteOldCOR;

   If (CORAlreadyRunning) Then
      // An instance of COR is already running, switch to it and pass it any
      // parameter that we may have...

      SwitchToAlreadyRunningCOR
   Else Begin
      // If we arrive here, then it means that either no arguments were given to
      // COR and/or that no instance of COR exists. Either way, we can go ahead
      // and this means that we can find out whether there is a new version of
      // COR available or not (assuming the user wants to check it for it at
      // startup)...

      Application.Title := COR_NAME+' '+COR_SHORT_VERSION;

      Application.Initialize;

      Application.MainFormOnTaskbar := True;

{$IFDEF COR_RELEASE}
      // Display the splash screen

      SplashScreenForm := TSplashScreenForm.Create(Nil);
      // Note: it gets hidden by the splash screen's timer and freed within
      //       "OnDestroy"...

      Application.ProcessMessages;
      // Ensure that the splash screen appears straight away
{$ENDIF}

      // Load the general options
      // Note: it cannot only be done when generating a release of COR,
      //       otherwise when running COR in debug mode we will lose the
      //       internet settings from the registry...

      LoadGeneralOptions;

{$IFDEF COR_RELEASE}
      // Check for a new version of COR, if allowed...

      If (GeneralOptions.CheckForNewVersionAtStartup) Then Begin
         CORUpdate(True);

         If (CORUpdateAvailable) Then
            // A new version of COR has been downloaded, so install it and
            // restart COR

            If (UpdateCOR) Then
               // The update went ok, so we can exit...

               Exit;
      End;

      SplashScreenForm.FormStyle := fsStayOnTop;
      // Note: we don't want the splash screen to be originally on top, as this
      //       will not work well with an update of COR, so...
{$ENDIF}

      Try
         // Create the main form and start COR itself

         Application.CreateForm(TMainForm, MainForm);

         Application.Run;
      Except
         On E: Exception Do
            ExceptionDialogMailClass.ShowException(E, Nil);
            // Note: for the exception dialog to actually work, we need the main
            //       form to be created, hence we only have the
            //       "Try...Except...End" statement around
            //       "Application.CreateForm" and "Application.Run" and not the
            //       whole main code...
      End;
   End;
End.

//==============================================================================
// End of file
//==============================================================================

