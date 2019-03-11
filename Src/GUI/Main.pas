//==============================================================================
// Main form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 27/04/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Main;

//==============================================================================

Interface

//==============================================================================

Uses
   Messages, Classes, Forms, Computation, Editor, DDEMan, Controls, Contents,
   ExtCtrls, Generic, HTMLHelpViewer;

//==============================================================================

Type
   TMainForm = Class(TGenericForm)
    EditorFrame: TEditorFrame;
    ComputationFrame: TComputationFrame;
    DDEServerConv: TDdeServerConv;
    SplashScreenTimer: TTimer;
    GUITimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure DDEServerConvExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure SplashScreenTimerTimer(Sender: TObject);
    procedure GUITimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
      Private
         AppArgs: String;

         FFormCreating: Boolean;
         FFormCloseQuerying: Boolean;

         FMode: TContentsFrameMode;

         FResetAll: Boolean;

         CanSaveComputationalSettings: Boolean;

         Procedure AnalyseAppArgs;

         Procedure ApplicationHint(aSender: TObject); Inline;

      Protected
         Procedure CreateParams(Var aParams: TCreateParams); Override;
         Procedure WndProc(Var aMsg: TMessage); Override;

         Procedure WMSyscommand(Var aMessage: TWmSysCommand); Message WM_SYSCOMMAND;

         Procedure WMActivate(Var aMsg: TWMActivate); Message WM_ACTIVATE;
         Procedure WMDropFiles(Var aMsg: TWMDropFiles); Message WM_DROPFILES;

      Public
         Procedure SaveDimensions; Inline;

         Procedure GoIntoMode(Const aMode: TContentsFrameMode);

      Published
         Property FormCreating: Boolean Read FFormCreating;
         Property FormCloseQuerying: Boolean Read FFormCloseQuerying;

         Property Mode: TContentsFrameMode Read FMode;

         Property ResetAll: Boolean Read FResetAll Write FResetAll;
   End;

//==============================================================================

Var
   MainForm: TMainForm;

//==============================================================================

Implementation

//==============================================================================

Uses
   Windows, SysUtils, Graphics, ShellAPI, JclAppInst, SyntaxEdit, Common, DeCAL,
   CORCommon, Properties, Registry, SynEdit, ODEIntegrator, ODECVODEIntegrator,
   CellMLAPIToMCEngine;

//==============================================================================

{$R *.dfm}

//==============================================================================

Procedure TMainForm.FormCreate(Sender: TObject);
   Procedure SetSystemFonts(Const aForm: TForm);
   Var
      NCM: TNonClientMetrics;
      PLF: PLogFont;
      Color: TColor;
      Height: Integer;
      Style: TFontStyles;
   Begin
      // Based on the code found at
      // http://groups.google.com/group/borland.public.delphi.non-technical/msg/1f645945067331cc?dmode=source

      NCM.cbSize := SizeOf(TNonClientMetrics);

      If (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NCM, 0)) Then Begin
         PLF := @NCM.lfMessageFont;

         Color  := aForm.Font.Color;
         Height := aForm.Font.Height;
         Style  := aForm.Font.Style;

         aForm.Font.Handle := CreateFontIndirect(PLF^);
         // Note: the font handle will automatically be detroyed by
         //       "aForm.Font", so no need to keep track of the font handle and
         //       call "DeleteObject" when shutting down...

         aForm.Font.Color  := Color;
         aForm.Font.Height := Height;
         aForm.Font.Style  := Style;
      End;
   End;
   Procedure LoadSettings;
      Function LoadControlSettings(Const aDirectory: String;
                                   Const aControl: TControl;
                                   Const aDockHosts: Array Of TWinControl;
                                   Const aManualDock: Boolean;
                                   Var aVisible: Boolean;
                                   Const aForceVisible: Boolean = False): Boolean;
      Var
         DockHost: Integer;
         CtrlTop: Integer;
         CtrlLeft: Integer;
         CtrlWidth: Integer;
         CtrlHeight: Integer;
      Begin
         Result := True;

         With TRegistryIniFile.Create(aDirectory) Do Begin
            DockHost := ReadInteger(aControl.Name, 'DockHost', 1);

            CtrlTop    := ReadInteger(aControl.Name, 'Top', 0);
            CtrlLeft   := ReadInteger(aControl.Name, 'Left', 0);
            CtrlWidth  := ReadInteger(aControl.Name, 'Width', 0);
            CtrlHeight := ReadInteger(aControl.Name, 'Height', 0);

            // Dock the control or make it floating

            If (DockHost = 0) Then
               FloatingControlList.Add([TFloatingControl.Create(aControl, CtrlTop, CtrlLeft, CtrlWidth, CtrlHeight)])
            Else If (SectionExists(aControl.Name)) Then Begin
               aControl.Parent := aDockHosts[DockHost-1];

               aControl.SetBounds(CtrlLeft, CtrlTop, CtrlWidth, CtrlHeight);

               If (aManualDock) Then
                  aControl.ManualDock(aDockHosts[DockHost-1], Nil, alClient);
            End Else   // There were no settings saved for the control, so...
                       // Note: important to know for the controls that must be
                       //       docked the very first time COR is used (see
                       //       "LoadSettings")
               Result := False;

            If (aForceVisible) Then
               aControl.Visible := True
            Else Begin
               aVisible := ReadBool(aControl.Name, 'Visible', True);

               aControl.Visible := aVisible;
            End;

            Free;
         End;
      End;
      Procedure LoadDockSiteSettings(Const aDirectory: String; aPanel: TPanel;
                                     Var aDockSiteSize: Integer);
      Const
         DividerForDockSite = 5;
      Var
         MemoryStream: TMemoryStream;
      Begin
         With TRegistryIniFile.Create(aDirectory) Do Begin
            MemoryStream := TMemoryStream.Create;

            Try
               ReadBinaryStream(aPanel.Name, 'Settings', MemoryStream);

               If ((MemoryStream.Memory <> Nil) And (MemoryStream.Size <> 0)) Then
                  aPanel.DockManager.LoadFromStream(MemoryStream);

               If ((aPanel.Align = alTop) Or (aPanel.Align = alBottom)) Then Begin
                  aPanel.Height := ReadInteger(aPanel.Name, 'Height', MainForm.EditorFrame.ClientHeight Div DividerForDockSite);

                  aDockSiteSize := aPanel.Height;
               End Else Begin
                  aPanel.Width := ReadInteger(aPanel.Name, 'Width', MainForm.EditorFrame.ClientWidth Div DividerForDockSite);

                  aDockSiteSize := aPanel.Width;
               End;

               // Note: "EditorFrame" and "ComputationFrame" have the same
               //       dimensions, so to use one or the other is the same...
            Finally
               MemoryStream.Free;
            End;

            Free;
         End;
      End;
   Const
      TOOLBAR_MULT = 5;
      TOOLBAR_DIV  = 9;
   Var
      DummyVisible: Boolean;
      NbOfOpenedFiles: Integer;
      Element: TEditorColElemOptions;
      I, Iter: Integer;
      NewFontStyle: TFontStyles;
      NeedUpdatingLeftDockSiteWidth: Boolean;
   Begin
      With TRegistryIniFile.Create(COR_REGISTRY) Do Begin
         // Main form's settings

         MainFormMaximised := ReadBool(GUI, 'Maximised', False);

         MainFormTop    := ReadInteger(GUI, 'Top', UNDEFINED);
         MainFormLeft   := ReadInteger(GUI, 'Left', UNDEFINED);
         MainFormWidth  := ReadInteger(GUI, 'Width', UNDEFINED);
         MainFormHeight := ReadInteger(GUI, 'Height', UNDEFINED);

         // General options
         // Note: they are loaded in the main COR source file, so...

         // Editor options

         With EditorOptions Do Begin
            // General

            With GeneralOptions Do Begin
               InsertMode         := ReadBool(OPTIONS_EDITOR_GENERAL, 'InsertMode', True);
               GroupUndo          := ReadBool(OPTIONS_EDITOR_GENERAL, 'GroupUndo', True);
               ScrollPastEOF      := ReadBool(OPTIONS_EDITOR_GENERAL, 'ScrollPastEOF', False);
               AutoIndentMode     := ReadBool(OPTIONS_EDITOR_GENERAL, 'AutoIndentMode', True);
               SmartTabs          := ReadBool(OPTIONS_EDITOR_GENERAL, 'SmartTabs', True);
               BackspaceUnindents := ReadBool(OPTIONS_EDITOR_GENERAL, 'BackspaceUnindents', True);
               ShowScrollHint     := ReadBool(OPTIONS_EDITOR_GENERAL, 'ShowScrollHint', True);
               WordWrap           := ReadBool(OPTIONS_EDITOR_GENERAL, 'WordWrap', False);

               View := TEditorView(ReadInteger(OPTIONS_EDITOR_GENERAL, 'View', Integer(evCOR)));

               InsertCaret    := TSynEditCaretType(ReadInteger(OPTIONS_EDITOR_GENERAL, 'InsertCaret', 0));
               OverwriteCaret := TSynEditCaretType(ReadInteger(OPTIONS_EDITOR_GENERAL, 'OverwriteCaret', 3));

               TabIndent := ReadInteger(OPTIONS_EDITOR_GENERAL, 'TabIndent', 3);
            End;

            // Display

            With DisplayOptions Do Begin
               ShowRightMargin := ReadBool(OPTIONS_EDITOR_DISPLAY, 'ShowRightMargin', True);
               RightMargin     := ReadInteger(OPTIONS_EDITOR_DISPLAY, 'RightMargin', 80);

               ShowGutter  := ReadBool(OPTIONS_EDITOR_DISPLAY, 'ShowGutter', True);
               GutterWidth := ReadInteger(OPTIONS_EDITOR_DISPLAY, 'GutterWidth', 17);

               FontName := ReadString(OPTIONS_EDITOR_DISPLAY, 'FontName', 'Lucida Console');
               FontSize := ReadInteger(OPTIONS_EDITOR_DISPLAY, 'FontSize', 10);
            End;

            // Colour

            With ColourOptions Do
               For Iter := 0 To Elements.Size-1 Do Begin
                  Element := TEditorColElemOptions(Elements.At(Iter).VObject);

                  With Element Do Begin
                     If (ValueExists(OPTIONS_EDITOR_COLOUR, Name+'BackgroundColor')) Then
                        BackgroundColor := ReadInteger(OPTIONS_EDITOR_COLOUR, Name+'BackgroundColor', clWindow);

                     If (ValueExists(OPTIONS_EDITOR_COLOUR, Name+'ForegroundColor')) Then
                        ForegroundColor := ReadInteger(OPTIONS_EDITOR_COLOUR, Name+'ForegroundColor', clWindowText);

                     NewFontStyle := FontStyle;

                     If (ValueExists(OPTIONS_EDITOR_COLOUR, Name+'FontStyleBold')) Then Begin
                        If (ReadBool(OPTIONS_EDITOR_COLOUR, Name+'FontStyleBold', False)) Then
                           NewFontStyle := NewFontStyle+[fsBold]
                        Else
                           NewFontStyle := NewFontStyle-[fsBold];
                     End;

                     If (ValueExists(OPTIONS_EDITOR_COLOUR, Name+'FontStyleItalic')) Then Begin
                        If (ReadBool(OPTIONS_EDITOR_COLOUR, Name+'FontStyleItalic', False)) Then
                           NewFontStyle := NewFontStyle+[fsItalic]
                        Else
                           NewFontStyle := NewFontStyle-[fsItalic];
                     End;

                     If (ValueExists(OPTIONS_EDITOR_COLOUR, Name+'FontStyleUnderline')) Then Begin
                        If (ReadBool(OPTIONS_EDITOR_COLOUR, Name+'FontStyleUnderline', False)) Then
                           NewFontStyle := NewFontStyle+[fsUnderline]
                        Else
                           NewFontStyle := NewFontStyle-[fsUnderline];
                     End;

                     FontStyle := NewFontStyle;

                     If (Name = LOCKED_FILE) then
                        Alpha := ReadInteger(OPTIONS_EDITOR_COLOUR, Name+'Alpha', 5)
                     Else
                        Alpha := ReadInteger(OPTIONS_EDITOR_COLOUR, Name+'Alpha', 100);
                  End;
               End;

            // Command viewer

            With CommandViewerOptions Do Begin
               OptimisedFontSize := ReadBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'OptimisedFontSize', True);
               UnderscoreForSub  := ReadBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'UnderscoreForSub', True);
               GreekSymbols      := ReadBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'GreekSymbols', True);
               DigitGrouping     := ReadBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'DigitGrouping', True);
               FontName          := ReadString(OPTIONS_EDITOR_COMMAND_VIEWER, 'FontName', 'Times New Roman');
               FontSize          := ReadInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'FontSize', 10);
               SubFont           := ReadInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'SubFont', 50);

               BackgroundColor := ReadInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'BackgroundColor', clWindow);
               ForegroundColor := ReadInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'ForegroundColor', clWindowText);
            End;
         End;

         // Computation options

         With ComputationOptions Do Begin
            // General

            With GeneralOptions Do Begin
               Duration := ReadFloat(OPTIONS_COMPUTATION_GENERAL, 'Duration', 1000);
               Output   := ReadFloat(OPTIONS_COMPUTATION_GENERAL, 'Output', 1);

               DebugMode := ReadBool(OPTIONS_COMPUTATION_GENERAL, 'DebugMode', False);

               Compiler         := TCellMLModelRuntimeCompiler(ReadInteger(OPTIONS_COMPUTATION_GENERAL, 'Compiler', Integer(cInternal)));
               CompilerLocation := ReadString(OPTIONS_COMPUTATION_GENERAL, 'CompilerLocation', '');
            End;

            // Integration

            With IntegrationOptions Do Begin
               Integrator := TODEIntegratorType(ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Integrator', Integer(itCVODE)));

               TimeStep := ReadFloat(OPTIONS_COMPUTATION_INTEGRATION, 'TimeStep', 0.01);

               MaxTimeStep := ReadFloat(OPTIONS_COMPUTATION_INTEGRATION, 'MaxTimeStep', 0);

               MaxNbOfSteps := ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'MaxNbOfSteps', 500);

               Method := TODECVODEIntegratorMethod(ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Method', Integer(mBDF)-1)+1);
               // Note: "-1" and "+1" because "TODECVODEIntegratorMethod" is
               //       1-based, as opposed to 0-based...

               Iterator           := TODECVODEIntegratorIterator(ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Iterator', Integer(iNewton)-1)+1);
               // Note: "-1" and "+1" because "TODECVODEIntegratorIterator" is
               //       1-based, as opposed to 0-based...
               LinearSolver       := TODECVODEIntegratorLinearSolver(ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'LinearSolver', Integer(lsDense)));
               Preconditioner     := TODECVODEIntegratorPreconditioner(ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Preconditioner', Integer(pBanded)));
               UpperHalfBandwidth := ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'UpperHalfBandwidth', 1);
               LowerHalfBandwidth := ReadInteger(OPTIONS_COMPUTATION_INTEGRATION, 'LowerHalfBandwidth', 1);

               RelTol := ReadFloat(OPTIONS_COMPUTATION_INTEGRATION, 'RelTol', 1e-7);
               AbsTol := ReadFloat(OPTIONS_COMPUTATION_INTEGRATION, 'AbsTol', 1e-9);
            End;

            // Graphical panel

            With GraphicalPanelOptions Do Begin
               UseGradientForTraces := ReadBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'UseGradientForTraces', True);

               ShowAxes      := ReadBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'ShowAxes', True);
               ShowGridlines := ReadBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'ShowGridlines', True);
               ShowLabels    := ReadBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'ShowLabels', True);

               BackgroundColor := ReadInteger(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'BackgroundColor', clWhite);
               AxesColor       := ReadInteger(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'AxesColor', clBlack);
               GridlinesColor  := ReadInteger(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'GridlinesColor', clGray);
            End;
         End;

         // Editorial frame

         With EditorFrame Do Begin
            LoadControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarMenuToolBar, [ControlBar], False, DummyVisible, True);

            If (Not LoadControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarFileMenuToolBar, [ControlBar], False, EditorFrameControlBarFileMenuToolBarVisible)) Then
               // No settings saved for the file menu toolbar, so manually
               // initialise its left position
               // Note: important to do as otherwise it might not be set to what
               //       we want, because of the toolbar being inherited from the
               //       contents frame and that it is not the far left and that
               //       the other inherited toolbars may "originally" have a
               //       width which is different from their "final" one, hence
               //       some potential problem

               ControlBarFileMenuToolBar.Left := 0;

            If (Not LoadControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarEditMenuToolBar, [ControlBar], False, EditorFrameControlBarEditMenuToolBarVisible)) Then
               // No settings saved for the edit menu toolbar, so manually
               // initialise its left position
               // Note: see the note above for the file menu toolbar

               ControlBarEditMenuToolBar.Left := ControlBarFileMenuToolBar.Left+
                                                 TOOLBAR_MULT*ControlBarFileMenuToolBar.Width Div TOOLBAR_DIV;

            If (Not LoadControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarRunMenuToolBar, [ControlBar], False, EditorFrameControlBarRunMenuToolBarVisible)) Then
               // No settings saved for the run menu toolbar, so manually
               // initialise its left position
               // Note: see the note above for the file menu toolbar

               ControlBarRunMenuToolBar.Left := ControlBarFileMenuToolBar.Left+
                                                ControlBarFileMenuToolBar.Width+
                                                TOOLBAR_MULT*ControlBarEditMenuToolBar.Width Div TOOLBAR_DIV;

            LoadDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, TopDockSite, EditorFrameTopDockSiteSize);
            LoadDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, LeftDockSite, EditorFrameLeftDockSiteSize);
            LoadDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, BottomDockSite, EditorFrameBottomDockSiteSize);
            LoadDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, RightDockSite, EditorFrameRightDockSiteSize);

            If (Not LoadControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, CommandViewerFrame, [TopDockSite, LeftDockSite, BottomDockSite, RightDockSite], True, EditorFrameCommandViewerFrameVisible)) Then Begin
               // No settings saved for the command viewer, so manually dock it
               // to the top dock site

               CommandViewerFrame.ManualDock(TopDockSite, Nil, alClient);

               // Make sure that the command viewer frame has a reasonable
               // starting height

               TopDockSite.Height := 5*ClientHeight Div 11;
            End;

            If (Not LoadControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, MsgsFrame, [TopDockSite, LeftDockSite, BottomDockSite, RightDockSite], True, EditorFrameMsgsFrameVisible)) Then Begin
               // No settings saved for the messages view, so manually dock it
               // to the bottom dock site

               MsgsFrame.ManualDock(BottomDockSite, Nil, alClient);

               // Make sure that the messages frame has a reasonable starting
               // height

               BottomDockSite.Height := 3*ClientHeight Div 11;
            End;

            // If there are no docked frames, then "hide" the dock site and its
            // associated splitter... (useful when first starting COR)

            If (TopDockSite.VisibleDockClientCount = 0) Then Begin
               TopDockSite.Height := 0;
               TopSplitter.Height := 0;
            End;

            If (LeftDockSite.VisibleDockClientCount = 0) Then Begin
               LeftDockSite.Width := 0;
               LeftSplitter.Width := 0;
            End;

            If (BottomDockSite.VisibleDockClientCount = 0) Then Begin
               BottomDockSite.Height := 0;
               BottomSplitter.Height := 0;
            End;

            If (RightDockSite.VisibleDockClientCount = 0) Then Begin
               RightDockSite.Width := 0;
               RightSplitter.Width := 0;
            End;

            // Opened files

            NbOfOpenedFiles := ReadInteger(OPENED_FILES, 'NbOfOpenedFiles', 0);

            ObjFree(EditorFrameOpenedFiles);

            EditorFrameOpenedFiles.Clear;

            If (NbOfOpenedFiles <> 0) Then
               For I := 0 To NbOfOpenedFiles-1 Do
                  EditorFrameOpenedFiles.PushBack([TEditorFrameOpenedFile.Create(ReadString(OPENED_FILES, 'OpenedFile'+IntToStr(I), ''),
                                                                                 TEditorView(ReadInteger(OPENED_FILES, 'OpenedFile'+IntToStr(I)+'View', Integer(EditorOptions.GeneralOptions.View))))]);

            EditorFrameSelectedFile := ReadInteger(OPENED_FILES, 'SelectedFile', 0);

            // Most recently opened files

            For I := 0 To MAX_NB_OF_MRU_FILES Do
               EditorFrameMRUFiles[I] := ReadString(MRU_FILES, IntToStr(I), '');

            NewCellMLFileID := 1;
            NewFileID       := 1;
         End;

         // Computational frame

         With ComputationFrame Do Begin
            LoadControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarMenuToolBar, [ControlBar], False, DummyVisible, True);

            If (Not LoadControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarFileMenuToolBar, [ControlBar], False, ComputationFrameControlBarFileMenuToolBarVisible)) Then
               // No settings saved for the file menu toolbar, so manually
               // initialise its left position
               // Note: see the note above for the file menu toolbar from the
               //       editor frame

               ControlBarFileMenuToolBar.Left := 0;

            If (Not LoadControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarPropertiesMenuToolBar, [ControlBar], False, ComputationFrameControlBarPropertiesMenuToolBarVisible)) Then
               // No settings saved for the file menu toolbar, so manually
               // initialise its left position
               // Note: see the note above for the file menu toolbar from the
               //       editor frame

               ControlBarPropertiesMenuToolBar.Left := ControlBarFileMenuToolBar.Left+
                                                       TOOLBAR_MULT*ControlBarFileMenuToolBar.Width Div TOOLBAR_DIV;

            If (Not LoadControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarRunMenuToolBar, [ControlBar], False, ComputationFrameControlBarRunMenuToolBarVisible)) Then
               // No settings saved for the run menu toolbar, so manually
               // initialise its left position
               // Note: see the note above for the file menu toolbar from the
               //       editor frame

               ControlBarRunMenuToolBar.Left := ControlBarFileMenuToolBar.Left+
                                                ControlBarFileMenuToolBar.Width+
                                                TOOLBAR_MULT*ControlBarPropertiesMenuToolBar.Width Div TOOLBAR_DIV;

            If (Not LoadControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarToolsMenuToolBar, [ControlBar], False, ComputationFrameControlBarToolsMenuToolBarVisible)) Then
               // No settings saved for the tools menu toolbar, so manually
               // initialise its left position
               // Note: see the note above for the file menu toolbar from the
               //       editor frame

               ControlBarToolsMenuToolBar.Left := ControlBarFileMenuToolBar.Left+
                                                  ControlBarFileMenuToolBar.Width+
                                                  ControlBarPropertiesMenuToolBar.Width+
                                                  TOOLBAR_MULT*ControlBarRunMenuToolBar.Width Div TOOLBAR_DIV;

            LoadDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, TopDockSite, ComputationFrameTopDockSiteSize);
            LoadDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, LeftDockSite, ComputationFrameLeftDockSiteSize);
            LoadDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, BottomDockSite, ComputationFrameBottomDockSiteSize);
            LoadDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, RightDockSite, ComputationFrameRightDockSiteSize);

            If (Not LoadControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, PropertiesFrame, [TopDockSite, LeftDockSite, BottomDockSite, RightDockSite], True, ComputationFramePropertiesFrameVisible)) Then Begin
               // No settings saved for the properties view, so manually dock it
               // to the left dock site

               PropertiesFrame.ManualDock(LeftDockSite, Nil, alClient);

               NeedUpdatingLeftDockSiteWidth := True;
            End Else
               NeedUpdatingLeftDockSiteWidth := False;

            // Width of the different properties' columns

            PropertiesFrame.VirtualStringTree.Header.Columns[0].Width := ReadInteger(COMPUTATION_FRAME+'\'+PropertiesFrame.Name, 'Col0Width', 169);
            PropertiesFrame.VirtualStringTree.Header.Columns[1].Width := ReadInteger(COMPUTATION_FRAME+'\'+PropertiesFrame.Name, 'Col1Width', 69);
            PropertiesFrame.VirtualStringTree.Header.Columns[2].Width := ReadInteger(COMPUTATION_FRAME+'\'+PropertiesFrame.Name, 'Col2Width', 69);

            // Make sure that the properties frame has a reasonable starting
            // width

            If (NeedUpdatingLeftDockSiteWidth) Then
               LeftDockSite.Width := PropertiesFrame.VirtualStringTree.Header.Columns[0].Width+
                                     PropertiesFrame.VirtualStringTree.Header.Columns[1].Width+
                                     PropertiesFrame.VirtualStringTree.Header.Columns[2].Width+
                                     GetSystemMetrics(SM_CYHSCROLL)+
                                     2;
               // Note: "GetSystemMetrics(SM_CYHSCROLL)" in case of a vertical
               //       scrollbar and "2" because that seems required (!!)...

            If (Not LoadControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ConsoleFrame, [TopDockSite, LeftDockSite, BottomDockSite, RightDockSite], True, ComputationFrameConsoleFrameVisible)) Then Begin
               // No settings saved for the console view, so manually dock it to
               // the bottom dock site

               ConsoleFrame.ManualDock(BottomDockSite, Nil, alClient);

               // Make sure that the console frame has a reasonable starting
               // height

               BottomDockSite.Height := 2*ClientHeight Div 11;
            End;

            // If there are no docked frames, then "hide" the dock site and its
            // associated splitter... (useful when first starting COR)

            If (TopDockSite.VisibleDockClientCount = 0) Then Begin
               TopDockSite.Height := 0;
               TopSplitter.Height := 0;
            End;

            If (LeftDockSite.VisibleDockClientCount = 0) Then Begin
               LeftDockSite.Width := 0;
               LeftSplitter.Width := 0;
            End;

            If (BottomDockSite.VisibleDockClientCount = 0) Then Begin
               BottomDockSite.Height := 0;
               BottomSplitter.Height := 0;
            End;

            If (RightDockSite.VisibleDockClientCount = 0) Then Begin
               RightDockSite.Width := 0;
               RightSplitter.Width := 0;
            End;
         End;

         Free;
      End;

      // Finalise things with the floating controls list

      FloatingControlList.TrimToSize;

      Sort(FloatingControlList);
   End;
Var
   I: Integer;
Begin
   Inherited;

   FFormCreating := True;

   // Set the system fonts

   SetSystemFonts(Self);

   // Load the settings

   LoadSettings;

   // Assign the help file to the application

   Application.HelpFile := CORHelpFileName;

   // An event handler for the application

   Application.OnHint := ApplicationHint;

   // Dimensions of the main form

   If ((MainFormTop <> UNDEFINED)   And (MainFormLeft <> UNDEFINED) And
       (MainFormWidth <> UNDEFINED) And (MainFormHeight <> UNDEFINED)) Then Begin
      Top    := MainFormTop;
      Left   := MainFormLeft;
      Width  := MainFormWidth;
      Height := MainFormHeight;
   End;

   If (MainFormMaximised) Then
      ShowWindow(Handle, SW_MAXIMIZE);

   // Initialise the different frames

   EditorFrame.FrameCreate;
   ComputationFrame.FrameCreate;

   // Set things up for the two different modes (very important and so is the
   // order!)

   CanSaveComputationalSettings := False;

   GoIntoMode(mComputational);
   GoIntoMode(mEditorial);

   // Enable the splash screen's timer

   SplashScreenTimer.Enabled := True;

   // Get the arguments passed to COR

   AppArgs := '';

   If (ParamCount > 0) Then Begin
      For I := 1 To ParamCount Do
         AppArgs := AppArgs+ParamStr(I)+CRLF;

      AppArgs := Copy(AppArgs, 1, Length(AppArgs)-Length(CRLF));
   End;

   // Analyse the arguments passed to COR

   AnalyseAppArgs;

   FFormCreating := False;
End;

//==============================================================================

Procedure TMainForm.FormDestroy(Sender: TObject);
Begin
   // Destroy the frames themselves

   EditorFrame.FrameDestroy;
   ComputationFrame.FrameDestroy;

{$IFDEF COR_RELEASE}
   // Release the splash screen

   SplashScreenForm.Free;
{$ENDIF}

   // Launch the COR updater, if an update has been downloaded

   If (CORUpdateAvailable) Then
      UpdateCOR;
End;

//==============================================================================

Procedure TMainForm.FormShow(Sender: TObject);
Begin
   // Select the mode in which we want to start
   // Note: this must be done here to ensure that all floating controls are
   //       always displayed over the main form...

   GoIntoMode(mEditorial);
End;

//==============================================================================

Procedure TMainForm.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
   Procedure SaveSettings; 
      Procedure SaveControlSettings(Const aDirectory: String;
                                    Const aControl: TControl;
                                    Const aVisible: Boolean);
      Var
         PosControl, DimControl: TControl;
      Begin
         With TRegistryIniFile.Create(aDirectory) Do Begin
            If (aControl.Floating) Then Begin
               WriteInteger(aControl.Name, 'DockHost', 0);

               PosControl := aControl.Parent;

               If (CompareStr(aControl.ClassName, 'TToolBar') = 0) Then
                  DimControl := aControl
               Else   // Panel
                  DimControl := aControl.Parent;
            End Else Begin
               WriteInteger(aControl.Name, 'DockHost', aControl.Parent.Tag);

               PosControl := aControl;
               DimControl := aControl;
            End;

            WriteInteger(aControl.Name, 'Top', PosControl.Top);
            WriteInteger(aControl.Name, 'Left', PosControl.Left);
            WriteInteger(aControl.Name, 'Width', DimControl.Width);
            WriteInteger(aControl.Name, 'Height', DimControl.Height);

            WriteBool(aControl.Name, 'Visible', aVisible);

            If (aControl.Parent Is TPanel) Then
               WriteInteger(aControl.Name, 'Orientation', Ord(aControl.DockOrientation));

            Free;
         End;
      End;
      Procedure SaveDockSiteSettings(Const aDirectory: String; aPanel: TPanel;
                                     Const aDockSiteSize: Integer);
      Var
         MemoryStream: TMemoryStream;
      Begin
         With TRegistryIniFile.Create(aDirectory) Do Begin
            MemoryStream := TMemoryStream.Create;

            Try
               aPanel.DockManager.SaveToStream(MemoryStream);

               MemoryStream.Seek(0, soFromBeginning);

               WriteBinaryStream(aPanel.Name, 'Settings', MemoryStream);

               If ((aPanel.Align = alTop) Or (aPanel.Align = alBottom)) Then Begin
                  If (aPanel.VisibleDockClientCount <> 0) Then
                     WriteInteger(aPanel.Name, 'Height', aPanel.Height)
                  Else
                     WriteInteger(aPanel.Name, 'Height', aDockSiteSize);
               End Else Begin
                  If (aPanel.VisibleDockClientCount <> 0) Then
                     WriteInteger(aPanel.Name, 'Width', aPanel.Width)
                  Else
                     WriteInteger(aPanel.Name, 'Width', aDockSiteSize);
               End;
            Finally
               MemoryStream.Free;
            End;

            Free;
         End;
      End;
   Var
      I, Iter: Integer;
   Begin
      With TRegistryIniFile.Create(COR_REGISTRY) Do Begin
         // Main form's settings

         WriteBool(GUI, 'Maximised', MainFormMaximised);

         WriteInteger(GUI, 'Top', MainFormTop);
         WriteInteger(GUI, 'Left', MainFormLeft);
         WriteInteger(GUI, 'Width', MainFormWidth);
         WriteInteger(GUI, 'Height', MainFormHeight);

         // General options

         With GeneralOptions Do Begin
            WriteBool(OPTIONS_GENERAL, 'CheckForNewVersionAtStartup', CheckForNewVersionAtStartup);

            WriteBool(OPTIONS_GENERAL, 'ConnectThroughProxyServer', ConnectThroughProxyServer);

            WriteString(OPTIONS_GENERAL, 'ProxyServer', ProxyServer);
            WriteInteger(OPTIONS_GENERAL, 'ProxyPort', ProxyPort);

            WriteString(OPTIONS_GENERAL, 'ProxyAuthenticationMethod', ProxyAuthenticationMethod);

            WriteString(OPTIONS_GENERAL, 'ProxyAuthenticationUsername', ProxyAuthenticationUsername);
            WriteString(OPTIONS_GENERAL, 'ProxyAuthenticationPassword', ProxyAuthenticationPassword);

            WriteBool(OPTIONS_GENERAL, 'ProxyAuthenticationAskForPassword', ProxyAuthenticationAskForPassword);
         End;

         // Editor options

         With EditorOptions Do Begin
            // General

            With GeneralOptions Do Begin
               WriteBool(OPTIONS_EDITOR_GENERAL, 'InsertMode', InsertMode);
               WriteBool(OPTIONS_EDITOR_GENERAL, 'GroupUndo', GroupUndo);
               WriteBool(OPTIONS_EDITOR_GENERAL, 'ScrollPastEOF', ScrollPastEOF);
               WriteBool(OPTIONS_EDITOR_GENERAL, 'AutoIndentMode', AutoIndentMode);
               WriteBool(OPTIONS_EDITOR_GENERAL, 'SmartTabs', SmartTabs);
               WriteBool(OPTIONS_EDITOR_GENERAL, 'BackspaceUnindents', BackspaceUnindents);
               WriteBool(OPTIONS_EDITOR_GENERAL, 'ShowScrollHint', ShowScrollHint);
               WriteBool(OPTIONS_EDITOR_GENERAL, 'WordWrap', WordWrap);

               WriteInteger(OPTIONS_EDITOR_GENERAL, 'View', Integer(View));

               WriteInteger(OPTIONS_EDITOR_GENERAL, 'InsertCaret', Integer(InsertCaret));
               WriteInteger(OPTIONS_EDITOR_GENERAL, 'OverwriteCaret', Integer(OverwriteCaret));

               WriteInteger(OPTIONS_EDITOR_GENERAL, 'TabIndent', TabIndent);
            End;

            // Display

            With DisplayOptions Do Begin
               WriteBool(OPTIONS_EDITOR_DISPLAY, 'ShowRightMargin', ShowRightMargin);
               WriteInteger(OPTIONS_EDITOR_DISPLAY, 'RightMargin', RightMargin);

               WriteBool(OPTIONS_EDITOR_DISPLAY, 'ShowGutter', ShowGutter);
               WriteInteger(OPTIONS_EDITOR_DISPLAY, 'GutterWidth', GutterWidth);

               WriteString(OPTIONS_EDITOR_DISPLAY, 'FontName', FontName);
               WriteInteger(OPTIONS_EDITOR_DISPLAY, 'FontSize', FontSize);
            End;

            // Colour

            With ColourOptions Do
               For Iter := 0 To Elements.Size-1 Do
                  With TEditorColElemOptions(Elements.At(Iter).VObject) Do Begin
                     WriteInteger(OPTIONS_EDITOR_COLOUR, Name+'BackgroundColor', BackgroundColor);
                     WriteInteger(OPTIONS_EDITOR_COLOUR, Name+'ForegroundColor', ForegroundColor);

                     WriteBool(OPTIONS_EDITOR_COLOUR, Name+'FontStyleBold', fsBold In FontStyle);
                     WriteBool(OPTIONS_EDITOR_COLOUR, Name+'FontStyleItalic', fsItalic In FontStyle);
                     WriteBool(OPTIONS_EDITOR_COLOUR, Name+'FontStyleUnderline', fsUnderline In FontStyle);

                     WriteInteger(OPTIONS_EDITOR_COLOUR, Name+'Alpha', Alpha);
                  End;

            // Command viewer

            With CommandViewerOptions Do Begin
               WriteBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'OptimisedFontSize', OptimisedFontSize);
               WriteBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'UnderscoreForSub', UnderscoreForSub);
               WriteBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'GreekSymbols', GreekSymbols);
               WriteBool(OPTIONS_EDITOR_COMMAND_VIEWER, 'DigitGrouping', DigitGrouping);
               WriteString(OPTIONS_EDITOR_COMMAND_VIEWER, 'FontName', FontName);
               WriteInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'FontSize', FontSize);
               WriteInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'SubFont', SubFont);

               WriteInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'BackgroundColor', BackgroundColor);
               WriteInteger(OPTIONS_EDITOR_COMMAND_VIEWER, 'ForegroundColor', ForegroundColor);
            End;
         End;

         // Computation options

         With ComputationOptions Do Begin
            // General

            With GeneralOptions Do Begin
               WriteFloat(OPTIONS_COMPUTATION_GENERAL, 'Duration', Duration);
               WriteFloat(OPTIONS_COMPUTATION_GENERAL, 'Output', Output);

               WriteBool(OPTIONS_COMPUTATION_GENERAL, 'DebugMode', DebugMode);

               WriteInteger(OPTIONS_COMPUTATION_GENERAL, 'Compiler', Integer(Compiler));
               WriteString(OPTIONS_COMPUTATION_GENERAL, 'CompilerLocation', CompilerLocation);
            End;

            // Integration

            With IntegrationOptions Do Begin
               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Integrator', Integer(Integrator));

               WriteFloat(OPTIONS_COMPUTATION_INTEGRATION, 'TimeStep', TimeStep);

               WriteFloat(OPTIONS_COMPUTATION_INTEGRATION, 'MaxTimeStep', MaxTimeStep);

               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'MaxNbOfSteps', MaxNbOfSteps);

               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Method', Integer(Method)-1);
               // Note: "-1" because "TODECVODEIntegratorMethod" is 1-based, as
               //       opposed to 0-based...

               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Iterator', Integer(Iterator)-1);
               // Note: "-1" because "TODECVODEIntegratorIterator" is 1-based,
               //       as opposed to 0-based...
               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'LinearSolver', Integer(LinearSolver));
               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'Preconditioner', Integer(Preconditioner));
               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'UpperHalfBandwidth', UpperHalfBandwidth);
               WriteInteger(OPTIONS_COMPUTATION_INTEGRATION, 'LowerHalfBandwidth', LowerHalfBandwidth);

               WriteFloat(OPTIONS_COMPUTATION_INTEGRATION, 'RelTol', RelTol);
               WriteFloat(OPTIONS_COMPUTATION_INTEGRATION, 'AbsTol', AbsTol);
            End;

            // Graphical panel

            With GraphicalPanelOptions Do Begin
               WriteBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'UseGradientForTraces', UseGradientForTraces);

               WriteBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'ShowAxes', ShowAxes);
               WriteBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'ShowGridlines', ShowGridlines);
               WriteBool(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'ShowLabels', ShowLabels);

               WriteInteger(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'BackgroundColor', BackgroundColor);
               WriteInteger(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'AxesColor', AxesColor);
               WriteInteger(OPTIONS_COMPUTATION_GRAPHICAL_PANEL, 'GridlinesColor', GridlinesColor);
            End;
         End;

         // Editorial frame

         With EditorFrame Do Begin
            SaveControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarMenuToolBar, True);

            SaveControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarFileMenuToolBar, EditorFrameControlBarFileMenuToolBarVisible);
            SaveControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarEditMenuToolBar, EditorFrameControlBarEditMenuToolBarVisible);
            SaveControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, ControlBarRunMenuToolBar, EditorFrameControlBarRunMenuToolBarVisible);

            SaveControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, CommandViewerFrame, EditorFrameCommandViewerFrameVisible);
            SaveControlSettings(COR_REGISTRY+'\'+EDITOR_FRAME, MsgsFrame, EditorFrameMsgsFrameVisible);

            SaveDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, TopDockSite, EditorFrameTopDockSiteSize);
            SaveDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, LeftDockSite, EditorFrameLeftDockSiteSize);
            SaveDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, BottomDockSite, EditorFrameBottomDockSiteSize);
            SaveDockSiteSettings(COR_REGISTRY+'\'+EDITOR_FRAME, RightDockSite, EditorFrameRightDockSiteSize);

            // Opened files

            WriteInteger(OPENED_FILES, 'NbOfOpenedFiles', EditorFrameOpenedFiles.Size);

            For Iter := 0 To EditorFrameOpenedFiles.Size-1 Do
               With TEditorFrameOpenedFile(EditorFrameOpenedFiles.At(Iter).VObject) Do Begin
                  WriteString(OPENED_FILES, 'OpenedFile'+IntToStr(Iter), FileName);
                  WriteInteger(OPENED_FILES, 'OpenedFile'+IntToStr(Iter)+'View', Integer(View));
               End;

            WriteInteger(OPENED_FILES, 'SelectedFile', EditorFrameSelectedFile);

            // Most recently opened files

            For I := 0 To MAX_NB_OF_MRU_FILES Do
               WriteString(MRU_FILES, IntToStr(I), EditorFrameMRUFiles[I]);
         End;

         // Computational frame

         If (CanSaveComputationalSettings) Then
            With ComputationFrame Do Begin
               SaveControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarMenuToolBar, True);

               SaveControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarFileMenuToolBar, ComputationFrameControlBarFileMenuToolBarVisible);
               SaveControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarPropertiesMenuToolBar, ComputationFrameControlBarPropertiesMenuToolBarVisible);
               SaveControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarRunMenuToolBar, ComputationFrameControlBarRunMenuToolBarVisible);
               SaveControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ControlBarToolsMenuToolBar, ComputationFrameControlBarToolsMenuToolBarVisible);

               SaveControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, PropertiesFrame, ComputationFramePropertiesFrameVisible);
               SaveControlSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, ConsoleFrame, ComputationFrameConsoleFrameVisible);

               SaveDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, TopDockSite, ComputationFrameTopDockSiteSize);
               SaveDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, LeftDockSite, ComputationFrameLeftDockSiteSize);
               SaveDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, BottomDockSite, ComputationFrameBottomDockSiteSize);
               SaveDockSiteSettings(COR_REGISTRY+'\'+COMPUTATION_FRAME, RightDockSite, ComputationFrameRightDockSiteSize);

               // Width of the different properties' columns

               WriteInteger(COMPUTATION_FRAME+'\'+PropertiesFrame.Name, 'Col0Width', PropertiesFrame.VirtualStringTree.Header.Columns[0].Width);
               WriteInteger(COMPUTATION_FRAME+'\'+PropertiesFrame.Name, 'Col1Width', PropertiesFrame.VirtualStringTree.Header.Columns[1].Width);
               WriteInteger(COMPUTATION_FRAME+'\'+PropertiesFrame.Name, 'Col2Width', PropertiesFrame.VirtualStringTree.Header.Columns[2].Width);
            End;

         Free;
      End;
   End;
Begin
   FFormCloseQuerying := True;

   // Close all the files in the editor, if any, but only if in editor mode

   If (FMode = mEditorial) Then
      EditorFrame.FrameCloseQuery(CanClose)
   Else Begin
      ComputationFrame.ActionFileExitExecute(Sender);

      CanClose := False;
   End;

   If (CanClose) Then Begin
      // Hide the whole application, since "HHCloseAll" takes some time and may
      // therefore give the impression that COR is slow, so...

      Hide;

      // Save the settings
      // Note: ideally, this should be done within "OnDestroy", but as it
      //       happens some components (e.g. toolbars) get destroy before we
      //       reach the "OnDestroy" handler, so...

      If (Not FResetAll) Then
         SaveSettings;
   End;

   FFormCloseQuerying := False;
End;

//==============================================================================

Procedure TMainForm.SaveDimensions;
Begin
   If (Not MainFormMaximised) Then Begin
      // The application is not maximised, so we can backup its dimensions

      MainFormTop    := Top;
      MainFormLeft   := Left;
      MainFormWidth  := Width;
      MainFormHeight := Height;
   End;
End;

//==============================================================================

Procedure TMainForm.AnalyseAppArgs;
   Function AppArg(Var aAppArgs: String): String; 
   Var
      CRLFPos: Integer;
   Begin
      CRLFPos := Pos(CRLF, aAppArgs);

      If ((CRLFPos = 0) And (Length(aAppArgs) <> 0)) Then Begin
         Result := aAppArgs;

         aAppArgs := '';
      End Else Begin
         Result := Copy(aAppArgs, 1, CRLFPos-1);

         aAppArgs := Copy(aAppArgs, CRLFPos+Length(CRLF), Length(aAppArgs)-CRLFPos-Length(CRLF)+1);
      End;
   End;
Var
   LocalAppArgs, AppArgVal: String;
Begin
   If (CompareStr(AppArgs, '') <> 0) Then Begin
      // Find out whether the first argument is an option or not

      LocalAppArgs := AppArgs;

      AppArgVal := UpperCase(AppArg(LocalAppArgs));

      // If the first argument is the open option, then get back all of the
      // arguments including the first one

      If (CompareStr(AppArgVal, '/OPEN') <> 0) Then
         LocalAppArgs := AppArgs;

      // Treat the different arguments, as If they were filenames to open

      AppArgVal := AppArg(LocalAppArgs);

      While (CompareStr(AppArgVal, '') <> 0) Do Begin
         EditorFrame.OpenFile(AppArgVal, EditorOptions.GeneralOptions.View);

         AppArgVal := AppArg(LocalAppArgs);
      End;
   End;
End;

//==============================================================================

Procedure TMainForm.ApplicationHint(aSender: TObject);
Begin
   If (FMode = mEditorial) Then
      EditorFrame.StatusBarInfoHint
   Else
      ComputationFrame.StatusBarInfoHint;
End;

//==============================================================================

Procedure TMainForm.CreateParams(Var aParams: TCreateParams);
Begin
   Inherited;

   aParams.ExStyle := (aParams.ExStyle And Not WS_EX_TOOLWINDOW) Or WS_EX_APPWINDOW;
End;

//==============================================================================

Procedure TMainForm.WndProc(Var aMsg: TMessage);
Begin
   // Interprocess communication handler

   Try
      If (HandleAllocated And Not (csDestroying In ComponentState)) Then
         Case ReadMessageCheck(aMsg, Handle) Of
            AppInstCmdLineDataKind: Begin
               // We have just received some COR's arguments, so deal with them...

               ReadMessageString(aMsg, AppArgs);

               AnalyseAppArgs;
            End;
         Else
            Inherited;
         End
      Else
         Inherited;
   Except
      Inherited;
   End;
End;

//==============================================================================

Procedure TMainForm.WMSyscommand(Var aMessage: TWmSysCommand);
Begin
   Case (aMessage.CmdType And $FFF0) Of
      SC_MAXIMIZE: Begin
         ShowWindow(Handle, SW_MAXIMIZE);

         MainFormMaximised := True;

         aMessage.Result := 0;
      End;
      SC_MINIMIZE: Begin
         ShowWindow(Handle, SW_MINIMIZE);

         aMessage.Result := 0;
      End;
      SC_RESTORE: Begin
         ShowWindow(Handle, SW_RESTORE);

         MainFormMaximised := False;

         aMessage.Result := 0;
      End;
   Else
      Inherited;
   End;
End;

//==============================================================================

Procedure TMainForm.WMActivate(Var aMsg: TWMActivate);
Begin
   Inherited;

   // Note: OnActivate doesn't seem to be working fine, so we have to map the
   //       original Windows message instead

   If (FMode = mEditorial) Then
      EditorFrame.WMActivate(aMsg);
End;

//==============================================================================

Procedure TMainForm.WMDropFiles(Var aMsg: TWMDropFiles);
Begin
   Inherited;

   EditorFrame.WMDropFiles(aMsg);

   DragFinish(aMsg.Drop);
End;

//==============================================================================

Procedure TMainForm.GoIntoMode(Const aMode: TContentsFrameMode);
   Procedure InitFrameVar(Const aMode: TContentsFrameMode;
                          Var aFrame: TContentsFrame);
   Begin
      Case aMode Of
         mEditorial:
            aFrame := EditorFrame;
         mComputational:
            aFrame := ComputationFrame;
     End;
   End;
Var
   OldMode: TContentsFrameMode;
   OldFrame, NewFrame: TContentsFrame;
   CanContinue: Boolean;
Begin
   // Note: the cases where we test "FFormCreating" are very important. To
   //       remove them could generate some exceptions in some cases, as some
   //       things may not yet be initialised, etc. so don't remove them!

   // Determine whether we are going into computational mode (once COR is up and
   // running)

   CanSaveComputationalSettings := CanSaveComputationalSettings Or
                                   ((aMode = mComputational) And Not FFormCreating);

   // Determine the old and new frames

   InitFrameVar(FMode, OldFrame);
   InitFrameVar(aMode, NewFrame);

   // Set the new mode

   OldMode := FMode;
   FMode   := aMode;

   If (OldFrame <> NewFrame) Then Begin
      // Keep track of the editor's active control

      If ((FMode = mComputational) And Not FFormCreating) Then
         OldFrame.ActiveControl := ActiveControl;

      // Specific clean up for the old frame

      If ((FMode = mEditorial) Or Not FFormCreating) Then
         OldFrame.ExitMode;

      // Backup various toolbars and views for the old frame

      OldFrame.BackupToolBarsAndViews;
   End;

   // Specific stuff for the new frame

   If ((FMode = mEditorial) Or Not FFormCreating) Then
      CanContinue := NewFrame.EnterMode
   Else
      CanContinue := True;

   // Were we able to go into the new mode?

   If (Not CanContinue) Then Begin
      // We couldn't go into the new mode, so revert what we just did...

      FMode := OldMode;

      NewFrame.ExitMode;
      OldFrame.EnterMode;

      Exit;
   End;

   // Update and show the right frame, while hide the others

   NewFrame.Visible := True;

   If (FMode = mEditorial) Then
      ComputationFrame.Visible := False
   Else
      EditorFrame.Visible := False;

   // Restore various toolbars and views for the new frame

   If (Not FFormCreating) Then
      NewFrame.RestoreToolBarsAndViews;

   // Does allow (or not) files to be dropped onto the form

   If (FMode = mEditorial) Then
      DragAcceptFiles(Handle, True)
   Else
      DragAcceptFiles(Handle, False);

   // Give the focus to the new frame

   ActiveControl := NewFrame.ActiveControl;

   // Update the new frame's caption

   NewFrame.UpdateCaption;

   // Update the new frame's status bar

   If (FMode = mEditorial) Then Begin
      If (EditorFrame.PageCtrl.ActivePage <> Nil) Then
         EditorFrame.StatusBarInfoEdit(EditorFrame.PageCtrl.ActivePage.Controls[0] As TSyntaxEdit)
      Else
         EditorFrame.StatusBarInfoHint;
   End Else
      NewFrame.StatusBarInfoHint;

   // Process any pending message

   Application.ProcessMessages;
   // Note: this is to ensure that the GUI looks responsive...
End;

//==============================================================================

Procedure TMainForm.GUITimerTimer(Sender: TObject);
Var
   SyntaxEdit: TSyntaxEdit;
Begin
   If (EditorFrame.PageCtrl.ActivePage <> Nil) Then Begin
      SyntaxEdit := EditorFrame.PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

      If (EditorFrame.SetFocusTo(SyntaxEdit)) Then
         GUITimer.Enabled := False;
   End;
End;

//==============================================================================

Procedure TMainForm.DDEServerConvExecuteMacro(Sender: TObject; Msg: TStrings);
Var
   I, J: Integer;
Begin
   // The user tried to open or run some files from within Windows Explorer, so
   // thanks to the DDE connection, we are told about it and can deal with the
   // files...

   If (Msg.Count > 0) Then Begin
      For I := 0 To Msg.Count-1 Do Begin
         J := Pos(' ', Msg[I]);

         // Get the option

         If (I = 0) Then
            AppArgs := Copy(Msg[I], 1, J-1);

         // Filename

         AppArgs := AppArgs+CRLF+Copy(Msg[I], J+2, Length(Msg[I])-J-2);
      End;

      AnalyseAppArgs;
   End;
End;

//==============================================================================

Procedure TMainForm.SplashScreenTimerTimer(Sender: TObject);
Begin
   SplashScreenTimer.Enabled := False;

{$IFDEF COR_RELEASE}
   SplashScreenForm.Hide;
{$ENDIF}
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

