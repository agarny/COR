//==============================================================================
// Options form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 01/08/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Options;

//==============================================================================

Interface

//==============================================================================

Uses
   SynEditHighlighter, SynHighlighterCellML, StdCtrls, SynEdit, SyntaxEdit,
   ExtCtrls, Controls, ComCtrls, Classes, Forms, CmdGraph, JvExStdCtrls, JvEdit,
   JvValidateEdit, CORCommon, VirtualTrees, VSTListBox, Graphics, Generic, OGL,
   OGLGraphPanel;

//==============================================================================

Const
   GENERAL_OPTIONS     = 0;
   EDITOR_OPTIONS      = 1;
   COMPUTATION_OPTIONS = 2;

   COMMAND_VIEWER_SUB_OPTIONS = 3;

   GRAPHICAL_PANEL_SUB_OPTIONS = 2;

//==============================================================================

Type
   TOptionsForm = Class(TGenericForm)
    PageCtrl: TPageControl;
    PageCtrlGen: TTabSheet;
    PageCtrlGenSep: TBevel;
    PageCtrlGenProxyFrame: TBevel;
    PageCtrlGenProxyServerLab: TLabel;
    PageCtrlGenProxyPortLab: TLabel;
    PageCtrlGenProxyAuthenticationMethodLab: TLabel;
    PageCtrlGenProxyAuthFrame: TBevel;
    PageCtrlGenProxyAuthPasswordLab: TLabel;
    PageCtrlGenProxyAuthUsernameLab: TLabel;
    PageCtrlGenCheckForNewVersionAtStartupCB: TCheckBox;
    PageCtrlGenConnectThroughProxyServerCB: TCheckBox;
    PageCtrlGenProxyAuthAskForPasswordCB: TCheckBox;
    PageCtrlGenProxyAuthenticationMethodVal: TComboBox;
    PageCtrlEditor: TTabSheet;
    PageCtrlEditorPageCtrl: TPageControl;
    PageCtrlEditorPageCtrlGen: TTabSheet;
    PageCtrlEditorPageCtrlGenTabIndentLab: TLabel;
    PageCtrlEditorPageCtrlGenInsertCaretLab: TLabel;
    PageCtrlEditorPageCtrlGenOverwriteCaretLab: TLabel;
    PageCtrlEditorPageCtrlGenInsertModeCB: TCheckBox;
    PageCtrlEditorPageCtrlGenGrouUndoCB: TCheckBox;
    PageCtrlEditorPageCtrlGenScrollPastEOFCB: TCheckBox;
    PageCtrlEditorPageCtrlGenAutoIndentCB: TCheckBox;
    PageCtrlEditorPageCtrlGenShowScrollHintCB: TCheckBox;
    PageCtrlEditorPageCtrlGenSmartTabsCB: TCheckBox;
    PageCtrlEditorPageCtrlGenBackspaceUnindentsCB: TCheckBox;
    PageCtrlEditorPageCtrlGenInsertCaretVal: TComboBox;
    PageCtrlEditorPageCtrlGenOverwriteCaretVal: TComboBox;
    PageCtrlEditorPageCtrlDisp: TTabSheet;
    PageCtrlEditorPageCtrlDispSampleLab: TLabel;
    PageCtrlEditorPageCtrlDispFontLab: TLabel;
    PageCtrlEditorPageCtrlDispSizeLab: TLabel;
    PageCtrlEditorPageCtrlDispSep: TBevel;
    PageCtrlEditorPageCtrlDispRightMarginLab: TLabel;
    PageCtrlEditorPageCtrlDispGutterWidthLab: TLabel;
    PageCtrlEditorPageCtrlDispFontVal: TComboBox;
    PageCtrlEditorPageCtrlDispSizeVal: TComboBox;
    PageCtrlEditorPageCtrlDispShowRightMarginCB: TCheckBox;
    PageCtrlEditorPageCtrlDispShowGutterCB: TCheckBox;
    PageCtrlEditorPageCtrlCol: TTabSheet;
    PageCtrlEditorPageCtrlColElementLab: TLabel;
    PageCtrlEditorPageCtrlColBackgroundLab: TLabel;
    PageCtrlEditorPageCtrlColForegroundLab: TLabel;
    PageCtrlEditorPageCtrlColBackgroundVal: TColorBox;
    PageCtrlEditorPageCtrlColForegroundVal: TColorBox;
    PageCtrlEditorPageCtrlColBoldCB: TCheckBox;
    PageCtrlEditorPageCtrlColItalicCB: TCheckBox;
    PageCtrlEditorPageCtrlColUnderlineCB: TCheckBox;
    PageCtrlEditorPageCtrlColEditorSample: TSyntaxEdit;
    PageCtrlEditorPageCtrlCmdView: TTabSheet;
    PageCtrlEditorPageCtrlCmdViewSizeLab: TLabel;
    PageCtrlEditorPageCtrlCmdViewSubFontLab: TLabel;
    PageCtrlEditorPageCtrlCmdViewSubFontVal: TLabel;
    PageCtrlEditorPageCtrlCmdViewCommentLab: TLabel;
    PageCtrlEditorPageCtrlCmdViewBackgroundLab: TLabel;
    PageCtrlEditorPageCtrlCmdViewForegroundLab: TLabel;
    PageCtrlEditorPageCtrlCmdViewCommentVal: TLabel;
    PageCtrlEditorPageCtrlCmdViewSubFontPB: TScrollBar;
    PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB: TCheckBox;
    PageCtrlEditorPageCtrlCmdViewSizeVal: TComboBox;
    PageCtrlEditorPageCtrlCmdViewUnderscoreForSubCB: TCheckBox;
    PageCtrlEditorPageCtrlCmdViewGreekSymbolsCB: TCheckBox;
    PageCtrlEditorPageCtrlCmdViewBackgroundVal: TColorBox;
    PageCtrlEditorPageCtrlCmdViewForegroundVal: TColorBox;
    PageCtrlEditorPageCtrlCmdViewPanel: TPanel;
    PageCtrlEditorPageCtrlCmdViewPanelCmdGraph: TCmdGraph;
    PageCtrlComp: TTabSheet;
    PageCtrlCompPageCtrl: TPageControl;
    PageCtrlCompPageCtrlGen: TTabSheet;
    PageCtrlCompPageCtrlGenDurationLab: TLabel;
    PageCtrlCompPageCtrlGenDurationUnitLab: TLabel;
    PageCtrlCompPageCtrlInt: TTabSheet;
    PageCtrlCompPageCtrlIntIntegratorLab: TLabel;
    PageCtrlCompPageCtrlIntIntegratorVal: TComboBox;
    OKBtn: TButton;
    CancelBtn: TButton;
    SynCellMLSyn: TSynCellMLSyn;
    PageCtrlEditorPageCtrlDispSampleVal: TSyntaxEdit;
    PageCtrlGenProxyServerVal: TJvValidateEdit;
    PageCtrlGenProxyPortVal: TJvValidateEdit;
    PageCtrlGenProxyAuthUsernameVal: TJvValidateEdit;
    PageCtrlGenProxyAuthPasswordVal: TJvValidateEdit;
    PageCtrlEditorPageCtrlGenTabIndentVal: TJvValidateEdit;
    PageCtrlEditorPageCtrlDispRightMarginVal: TJvValidateEdit;
    PageCtrlEditorPageCtrlDispGutterWidthVal: TJvValidateEdit;
    PageCtrlCompPageCtrlGenDurationVal: TJvValidateEdit;
    PageCtrlCompPageCtrlGenOutputLab: TLabel;
    PageCtrlCompPageCtrlGenOutputVal: TJvValidateEdit;
    PageCtrlCompPageCtrlGenOutputUnitLab: TLabel;
    PageCtrlEditorPageCtrlCmdViewDigitGroupingCB: TCheckBox;
    PageCtrlEditorPageCtrlCmdViewFontLab: TLabel;
    PageCtrlEditorPageCtrlCmdViewFontVal: TComboBox;
    PageCtrlEditorPageCtrlGenWordWrapCB: TCheckBox;
    PageCtrlCompPageCtrlIntFixedMaxTimeStepLab: TLabel;
    PageCtrlCompPageCtrlIntFixedTimeStepVal: TJvValidateEdit;
    PageCtrlCompPageCtrlIntFixedTimeStepUnitAndSepLab: TLabel;
    PageCtrlCompPageCtrlIntMaxTimeStepVal: TJvValidateEdit;
    PageCtrlCompPageCtrlIntMaxTimeStepUnitLab: TLabel;
    PageCtrlCompPageCtrlIntMaxNbOfStepsVal: TJvValidateEdit;
    PageCtrlCompPageCtrlIntMaxNbOfStepsLab: TLabel;
    PageCtrlCompPageCtrlIntMethodVal: TComboBox;
    PageCtrlCompPageCtrlIntMethodLab: TLabel;
    PageCtrlCompPageCtrlIntIteratorLab: TLabel;
    PageCtrlCompPageCtrlIntIteratorVal: TComboBox;
    PageCtrlCompPageCtrlIntRelTolVal: TJvValidateEdit;
    PageCtrlCompPageCtrlIntRelAbsTolLab: TLabel;
    PageCtrlCompPageCtrlIntAbsTolVal: TJvValidateEdit;
    PageCtrlCompPageCtrlIntRelAbsTolSepLab: TLabel;
    PageCtrlCompPageCtrlIntLinearSolverLab: TLabel;
    PageCtrlCompPageCtrlIntLinearSolverVal: TComboBox;
    PageCtrlCompPageCtrlIntUpperLowerHalfBandwidthLab: TLabel;
    PageCtrlCompPageCtrlIntUpperHalfBandwidthVal: TJvValidateEdit;
    PageCtrlCompPageCtrlIntLowerHalfBandwidthVal: TJvValidateEdit;
    PageCtrlCompPageCtrlIntUpperLowerHalfBandwidthSepLab: TLabel;
    PageCtrlCompPageCtrlIntPreconditionerLab: TLabel;
    PageCtrlCompPageCtrlIntPreconditionerVal: TComboBox;
    PageCtrlCompPageCtrlGenSep: TBevel;
    PageCtrlCompPageCtrlGenCompilerLab: TLabel;
    PageCtrlCompPageCtrlGenCompilerVal: TComboBox;
    PageCtrlCompPageCtrlGenCompilerFrame: TBevel;
    PageCtrlCompPageCtrlGenCompilerLocationLab: TLabel;
    PageCtrlCompPageCtrlGenCompilerLocationVal: TJvValidateEdit;
    PageCtrlCompPageCtrlGenCompilerNoteLab: TLabel;
    PageCtrlCompPageCtrlGenCompilerNoteVal: TLabel;
    PageCtrlCompPageCtrlGenCompilerLocationBtn: TButton;
    PageCtrlEditorPageCtrlColAlphaLab: TLabel;
    PageCtrlEditorPageCtrlColAlphaSB: TScrollBar;
    PageCtrlEditorPageCtrlColAlphaVal: TLabel;
    PageCtrlEditorPageCtrlColAlphaS: TShape;
    PageCtrlEditorPageCtrlColElementVal: TVSTListBox;
    PageCtrlCompPageCtrlGenDebugModeCB: TCheckBox;
    PageCtrlCompPageCtrlGraphPanel: TTabSheet;
    PageCtrlCompPageCtrlGraphPanelPanel: TPanel;
    PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel: TOGLGraphPanel;
    PageCtrlCompPageCtrlGraphPanelUseGradientForTracesCB: TCheckBox;
    PageCtrlCompPageCtrlGraphPanelShowAxesCB: TCheckBox;
    PageCtrlCompPageCtrlGraphPanelBackgroundLab: TLabel;
    PageCtrlCompPageCtrlGraphPanelBackgroundVal: TColorBox;
    PageCtrlCompPageCtrlGraphPanelAxesVal: TColorBox;
    PageCtrlCompPageCtrlGraphPanelAxesLab: TLabel;
    PageCtrlCompPageCtrlGraphPanelGridlinesLab: TLabel;
    PageCtrlCompPageCtrlGraphPanelGridlinesVal: TColorBox;
    PageCtrlCompPageCtrlGraphPanelShowGridlinesCB: TCheckBox;
    PageCtrlCompPageCtrlGraphPanelShowLabelsCB: TCheckBox;
    PageCtrlEditorPageCtrlGenViewLab: TLabel;
    PageCtrlEditorPageCtrlGenViewVal: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure UpdateFontSample(Sender: TObject);
    procedure UpdateIntElementAndEditorSample(Sender: TObject);
    procedure PageCtrlEditorPageCtrlDispShowRightMarginCBClick(Sender: TObject);
    procedure PageCtrlEditorPageCtrlDispShowGutterCBClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateExtElementAndEditorSample(Sender: TObject);
    procedure UpdateCmdGraphSample(Sender: TObject);
    procedure PageCtrlGenProxySettingsClick(Sender: TObject);
    procedure PageCtrlEditorPageCtrlDispRightMarginValChange(
      Sender: TObject);
    procedure PageCtrlEditorPageCtrlDispGutterWidthValChange(
      Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PageCtrlCompPageCtrlGenCompilerValChange(Sender: TObject);
    procedure PageCtrlCompPageCtrlGenCompilerLocationBtnClick(Sender: TObject);
    procedure PageCtrlCompPageCtrlGenCompilerLocationValExit(Sender: TObject);
    procedure PageCtrlEditorPageCtrlColAlphaSBChange(Sender: TObject);
    procedure PageCtrlEditorPageCtrlColElementValChange(
      Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure PageCtrlCompPageCtrlGraphPanelBackgroundValChange(Sender: TObject);
    procedure PageCtrlCompPageCtrlGraphPanelAxesValChange(Sender: TObject);
    procedure PageCtrlCompPageCtrlGraphPanelGridlinesValChange(Sender: TObject);
    procedure PageCtrlCompPageCtrlGraphPanelShowAxesCBClick(Sender: TObject);
    procedure PageCtrlCompPageCtrlGraphPanelShowGridlinesCBClick(Sender: TObject);
    procedure PageCtrlCompPageCtrlGraphPanelShowLabelsCBClick(Sender: TObject);
      Private
         FormCreating: Boolean;

         Elements: TEditorColElemOptionsList;

         CmdBinTree: TCmdBinTree;

         Procedure UpdateGeneral;

         Procedure UpdateCompiler;

         Procedure InitElement(Const aElement: String); Inline;

         Procedure UpdateIntElement;
         Procedure UpdateExtElement;

         Procedure UpdateEditorSample;

         Procedure UpdateAlpha;

         Procedure CMDialogKey(Var aMsg: TCMDialogKey); Message CM_DIALOGKEY;
      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent; Const aOptionsType: Integer = -1; Const aSubOptionsType: Integer = -1); Reintroduce;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   Windows, SysUtils, ODEIntegrator, ODECVODEIntegrator, DeCAL, Common, ShlObj,
   CellMLAPIToMCEngine, JclSecurity;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TOptionsForm.Create(aOwner: TComponent;
                                Const aOptionsType, aSubOptionsType: Integer);
Begin
   Inherited Create(aOwner);

   If (aOptionsType <> UNDEFINED) Then Begin
      PageCtrl.ActivePageIndex := aOptionsType;

      If (aSubOptionsType <> UNDEFINED) Then
         Case aOptionsType Of
            EDITOR_OPTIONS:
               PageCtrlEditorPageCtrl.ActivePageIndex := aSubOptionsType;
            COMPUTATION_OPTIONS:
               PageCtrlCompPageCtrl.ActivePageIndex := aSubOptionsType;
         End;
   End;
End;

//==============================================================================

Procedure TOptionsForm.FormCreate(Sender: TObject);
Var
   I, Iter: Integer;
   Font: TFont;
   DC: HDC;
   OldObject: HGDIOBJ;
   TextMetric: TTextMetric;
Begin
   Inherited;

   FormCreating := True;

   // GUI stuff...

   PageCtrlEditorPageCtrlCmdViewCommentLab.Font.Style := [fsUnderline];

   // General options

   EditFieldForIPAddress(PageCtrlGenProxyServerVal);
   EditFieldForRangeInteger(PageCtrlGenProxyPortVal);
   EditFieldForUsername(PageCtrlGenProxyAuthUsernameVal);
   EditFieldForPassword(PageCtrlGenProxyAuthPasswordVal);

   With GeneralOptions Do Begin
      If (IsWinVistaOrAboveOrAdministrator) Then
         PageCtrlGenCheckForNewVersionAtStartupCB.Checked := CheckForNewVersionAtStartup
      Else
         PageCtrlGenCheckForNewVersionAtStartupCB.Enabled := False;

      PageCtrlGenConnectThroughProxyServerCB.Checked := ConnectThroughProxyServer;

      PageCtrlGenProxyServerVal.Text := ProxyServer;
      PageCtrlGenProxyPortVal.Text   := SimplifyNb(ProxyPort);

      For I := 0 To PageCtrlGenProxyAuthenticationMethodVal.Items.Count-1 Do
         If (CompareStr(ProxyAuthenticationMethod, PageCtrlGenProxyAuthenticationMethodVal.Items[I]) = 0) Then Begin
            PageCtrlGenProxyAuthenticationMethodVal.ItemIndex := I;

            Break;
         End;

      PageCtrlGenProxyAuthUsernameVal.Text := ProxyAuthenticationUsername;
      PageCtrlGenProxyAuthPasswordVal.Text := Decrypt(ProxyAuthenticationPassword);

      PageCtrlGenProxyAuthAskForPasswordCB.Checked := ProxyAuthenticationAskForPassword;
   End;

   UpdateGeneral;

   // Editor options

   // Initialise the font's list

   Font := TFont.Create;

   For I := 0 To Screen.Fonts.Count-1 Do Begin
      // Only want fixed fonts, so...

      Font.Name := Screen.Fonts[I];

      // Get a device context, so we can select the font and check whether it
      // is a fixed one or not

      DC := GetDC(Handle);

      OldObject := SelectObject(DC, Font.Handle);   // Select the font we want
                                                    // to test

      GetTextMetrics(DC, TextMetric);

      SelectObject(DC, OldObject);   // Reset the original font

      ReleaseDC(Handle, DC);

      // Fonts for the editor

      If (((TextMetric.tmPitchAndFamily And TMPF_FIXED_PITCH) = 0) And
          (CompareStr(Copy(Font.Name, 1, 4), 'WST_') <> 0)) Then Begin
         // Note: we skip the "WST_*" fonts as they are not suitable for text
         //       editing

         PageCtrlEditorPageCtrlDispFontVal.Items.Add(Font.Name);

         If (CompareStr(Font.Name, EditorOptions.DisplayOptions.FontName) = 0) Then
            PageCtrlEditorPageCtrlDispFontVal.ItemIndex := PageCtrlEditorPageCtrlDispFontVal.Items.Count-1;
      End;

      // Fonts for the command viewer
      // Note: we just make all the fonts available to the user, except the
      //       Adobe one(s), as they cause problems (is that ok?)

      If (Pos('Adobe', Font.Name) = 0) Then
         PageCtrlEditorPageCtrlCmdViewFontVal.Items.Add(Font.Name);

      If (CompareStr(Font.Name, EditorOptions.CommandViewerOptions.FontName) = 0) Then
         PageCtrlEditorPageCtrlCmdViewFontVal.ItemIndex := PageCtrlEditorPageCtrlCmdViewFontVal.Items.Count-1;
   End;

   Font.Free;

   // Initialise the size's lists

   For I := MIN_FONT_SIZE To MAX_FONT_SIZE Do Begin
      PageCtrlEditorPageCtrlDispSizeVal.Items.Add(IntToStr(I));

      PageCtrlEditorPageCtrlCmdViewSizeVal.Items.Add(IntToStr(I));
   End;

   // Initialise the elements

   Elements := TEditorColElemOptionsList.Create;

   InitElement(DEFAULT);
   InitElement(RESERVED_WORD);
   InitElement(EXTRA_INFO);
   InitElement(ERROR_LINE);
   InitElement(SELECTED_TEXT);
   InitElement(RIGHT_MARGIN);
   InitElement(GUTTER_AREA);
   InitElement(SCROLL_HINT);
   InitElement(LOCKED_FILE);

   // Editor options

   With EditorOptions Do Begin
      // General tab

      EditFieldForStrictlyPositiveInteger(PageCtrlEditorPageCtrlGenTabIndentVal);

      With GeneralOptions Do Begin
         PageCtrlEditorPageCtrlGenInsertModeCB.Checked         := InsertMode;
         PageCtrlEditorPageCtrlGenGrouUndoCB.Checked           := GroupUndo;
         PageCtrlEditorPageCtrlGenScrollPastEOFCB.Checked      := ScrollPastEOF;
         PageCtrlEditorPageCtrlGenAutoIndentCB.Checked         := AutoIndentMode;
         PageCtrlEditorPageCtrlGenSmartTabsCB.Checked          := SmartTabs;
         PageCtrlEditorPageCtrlGenBackspaceUnindentsCB.Checked := BackspaceUnindents;
         PageCtrlEditorPageCtrlGenShowScrollHintCB.Checked     := ShowScrollHint;
         PageCtrlEditorPageCtrlGenWordWrapCB.Checked           := WordWrap;

         PageCtrlEditorPageCtrlGenInsertCaretVal.ItemIndex    := Integer(InsertCaret);
         PageCtrlEditorPageCtrlGenOverwriteCaretVal.ItemIndex := Integer(OverwriteCaret);

         PageCtrlEditorPageCtrlGenTabIndentVal.Text := SimplifyNb(TabIndent);

         For Iter := 0 To Views.Size-1 Do
            PageCtrlEditorPageCtrlGenViewVal.Items.Add(String(Views.At(Iter).VString));

         For I := 0 To PageCtrlEditorPageCtrlGenViewVal.Items.Count-1 Do
            If (CompareStr(String(Views.At(Integer(View)).VString), PageCtrlEditorPageCtrlGenViewVal.Items[I]) = 0) Then Begin
               PageCtrlEditorPageCtrlGenViewVal.ItemIndex := I;

               Break;
            End;
      End;

      // Display tab

      EditFieldForPositiveInteger(PageCtrlEditorPageCtrlDispRightMarginVal);
      EditFieldForPositiveInteger(PageCtrlEditorPageCtrlDispGutterWidthVal);

      With DisplayOptions Do Begin
         PageCtrlEditorPageCtrlDispRightMarginVal.Text := SimplifyNb(RightMargin);

         PageCtrlEditorPageCtrlDispShowRightMarginCB.Checked := ShowRightMargin;

         PageCtrlEditorPageCtrlDispGutterWidthVal.Text := SimplifyNb(GutterWidth);

         PageCtrlEditorPageCtrlDispShowGutterCB.Checked := ShowGutter;

         PageCtrlEditorPageCtrlDispFontVal.Text      := FontName;
         PageCtrlEditorPageCtrlDispSizeVal.ItemIndex := FontSize-MIN_FONT_SIZE;
      End;

      // Colour tab

      With DisplayOptions Do Begin
         PageCtrlEditorPageCtrlDispSampleVal.Font.Name   := FontName;
         PageCtrlEditorPageCtrlColEditorSample.Font.Name := FontName;

         PageCtrlEditorPageCtrlDispSampleVal.Font.Size   := FontSize;
         PageCtrlEditorPageCtrlColEditorSample.Font.Size := FontSize;

         If (ShowRightMargin) Then Begin
            PageCtrlEditorPageCtrlDispSampleVal.RightEdge   := RightMargin;
            PageCtrlEditorPageCtrlColEditorSample.RightEdge := RightMargin;
         End Else Begin
            PageCtrlEditorPageCtrlDispSampleVal.RightEdge   := 0;
            PageCtrlEditorPageCtrlColEditorSample.RightEdge := 0;
         End;

         PageCtrlEditorPageCtrlDispSampleVal.Gutter.Visible   := ShowGutter;
         PageCtrlEditorPageCtrlColEditorSample.Gutter.Visible := ShowGutter;

         PageCtrlEditorPageCtrlDispSampleVal.Gutter.Width   := GutterWidth;
         PageCtrlEditorPageCtrlColEditorSample.Gutter.Width := GutterWidth;
      End;

      // Command viewer tab

      With CommandViewerOptions Do Begin
         PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB.Checked := OptimisedFontSize;
         PageCtrlEditorPageCtrlCmdViewUnderscoreForSubCB.Checked  := UnderscoreForSub;
         PageCtrlEditorPageCtrlCmdViewGreekSymbolsCB.Checked      := GreekSymbols;
         PageCtrlEditorPageCtrlCmdViewDigitGroupingCB.Checked     := DigitGrouping;
         PageCtrlEditorPageCtrlCmdViewFontVal.Text                := FontName;
         PageCtrlEditorPageCtrlCmdViewSizeVal.ItemIndex           := FontSize-MIN_FONT_SIZE;
         PageCtrlEditorPageCtrlCmdViewSubFontPB.Position          := SubFont;

         PageCtrlEditorPageCtrlCmdViewBackgroundVal.Selected := BackgroundColor;
         PageCtrlEditorPageCtrlCmdViewForegroundVal.Selected := ForegroundColor;
      End;
   End;

   // Update the font and editor samples

   UpdateFontSample(Self);

   // Go through the elements to initialise the editor sample and then select
   // the whitespace element

   For I := PageCtrlEditorPageCtrlColElementVal.Count-1 DownTo 0 Do Begin
      PageCtrlEditorPageCtrlColElementVal.NodeIndex := I;

      UpdateExtElementAndEditorSample(Self);
   End;

   // Initialise a sample command, which is in fact an equation

   CmdBinTree := TCmdBinTree.Create(itEq);

   CmdBinTree.Left := TCmdBinTree.Create(itDiff);

      CmdBinTree.Left.Left := TCmdBinTree.Create(itSym, 't');

      CmdBinTree.Left.Right := TCmdBinTree.Create(itSym, 'y');

   CmdBinTree.Right := TCmdBinTree.Create(itDivide);

      CmdBinTree.Right.Left := TCmdBinTree.Create(itSym, '1234.56789');

      CmdBinTree.Right.Right := TCmdBinTree.Create(itMinus);

         CmdBinTree.Right.Right.Left := TCmdBinTree.Create(itSym, 'y_alpha');

         CmdBinTree.Right.Right.Right := TCmdBinTree.Create(itSym, 'y_beta');

   PageCtrlEditorPageCtrlCmdViewPanelCmdGraph.CmdBinTree := CmdBinTree;

   UpdateCmdGraphSample(Self);

   // Computation options

   With ComputationOptions Do Begin
      // General

      EditFieldForStrictlyPositiveFloat(PageCtrlCompPageCtrlGenDurationVal);
      EditFieldForStrictlyPositiveFloat(PageCtrlCompPageCtrlGenOutputVal);

      EditFieldForFolderPath(PageCtrlCompPageCtrlGenCompilerLocationVal);

      With GeneralOptions Do Begin
         PageCtrlCompPageCtrlGenDurationVal.Text := SimplifyNb(Duration);
         PageCtrlCompPageCtrlGenOutputVal.Text   := SimplifyNb(Output);

         PageCtrlCompPageCtrlGenDebugModeCB.Checked := DebugMode;

         // Add the different possible compilers
         // Note: non-Intel processors canNOT be offered the Intel C++
         //       compiler...

         For Iter := 0 To Compilers.Size-1 Do
            If ((CPUVendor = cvIntel) Or
                (TCellMLModelRuntimeCompiler(Iter) <> cIntelCPP)) Then
               PageCtrlCompPageCtrlGenCompilerVal.Items.Add(String(Compilers.At(Iter).VString));

         For I := 0 To PageCtrlCompPageCtrlGenCompilerVal.Items.Count-1 Do
            If (CompareStr(String(Compilers.At(Integer(Compiler)).VString), PageCtrlCompPageCtrlGenCompilerVal.Items[I]) = 0) Then Begin
               PageCtrlCompPageCtrlGenCompilerVal.ItemIndex := I;

               Break;
            End;

         PageCtrlCompPageCtrlGenCompilerLocationVal.Text := CompilerLocation;
      End;

      // Integration

      EditFieldForStrictlyPositiveFloat(PageCtrlCompPageCtrlIntFixedTimeStepVal);
      EditFieldForPositiveFloat(PageCtrlCompPageCtrlIntMaxTimeStepVal);

      EditFieldForStrictlyPositiveInteger(PageCtrlCompPageCtrlIntMaxNbOfStepsVal);

      EditFieldForPositiveInteger(PageCtrlCompPageCtrlIntUpperHalfBandwidthVal);
      EditFieldForPositiveInteger(PageCtrlCompPageCtrlIntLowerHalfBandwidthVal);

      EditFieldForStrictlyPositiveFloat(PageCtrlCompPageCtrlIntRelTolVal);
      EditFieldForStrictlyPositiveFloat(PageCtrlCompPageCtrlIntAbsTolVal);

      With IntegrationOptions Do Begin
         For Iter := 0 To ODEIntegrators.Size-1 Do
            PageCtrlCompPageCtrlIntIntegratorVal.Items.Add(String(ODEIntegrators.At(Iter).VString));

         PageCtrlCompPageCtrlIntIntegratorVal.ItemIndex := Integer(Integrator);

         PageCtrlCompPageCtrlIntFixedTimeStepVal.Text := SimplifyNb(TimeStep);

         PageCtrlCompPageCtrlIntMaxTimeStepVal.Text := SimplifyNb(MaxTimeStep);

         PageCtrlCompPageCtrlIntMaxNbOfStepsVal.Text := SimplifyNb(MaxNbOfSteps);

         For Iter := 0 To ODECVODEIntegratorMethods.Size-1 Do
            PageCtrlCompPageCtrlIntMethodVal.Items.Add(String(ODECVODEIntegratorMethods.At(Iter).VString));

         PageCtrlCompPageCtrlIntMethodVal.ItemIndex := Integer(Method)-1;
         // Note: "-1" because "TODECVODEIntegratorMethod" is 1-based, as
         //       opposed to 0-based...

         For Iter := 0 To ODECVODEIntegratorIterators.Size-1 Do
            PageCtrlCompPageCtrlIntIteratorVal.Items.Add(String(ODECVODEIntegratorIterators.At(Iter).VString));

         PageCtrlCompPageCtrlIntIteratorVal.ItemIndex := Integer(Iterator)-1;
         // Note: "-1" because "TODECVODEIntegratorIterator" is 1-based, as
         //       opposed to 0-based...

         For Iter := 0 To ODECVODEIntegratorLinearSolvers.Size-1 Do
            PageCtrlCompPageCtrlIntLinearSolverVal.Items.Add(String(ODECVODEIntegratorLinearSolvers.At(Iter).VString));

         PageCtrlCompPageCtrlIntLinearSolverVal.ItemIndex := Integer(LinearSolver);

         For Iter := 0 To ODECVODEIntegratorPreconditioners.Size-1 Do
            PageCtrlCompPageCtrlIntPreconditionerVal.Items.Add(String(ODECVODEIntegratorPreconditioners.At(Iter).VString));

         PageCtrlCompPageCtrlIntPreconditionerVal.ItemIndex := Integer(Preconditioner);

         PageCtrlCompPageCtrlIntUpperHalfBandwidthVal.Text := SimplifyNb(UpperHalfBandwidth);
         PageCtrlCompPageCtrlIntLowerHalfBandwidthVal.Text := SimplifyNb(LowerHalfBandwidth);

         PageCtrlCompPageCtrlIntRelTolVal.Text := SimplifyNb(RelTol);
         PageCtrlCompPageCtrlIntAbsTolVal.Text := SimplifyNb(AbsTol);
      End;

      // Graphical panel

      With GraphicalPanelOptions Do Begin
         PageCtrlCompPageCtrlGraphPanelUseGradientForTracesCB.Checked := UseGradientForTraces;

         PageCtrlCompPageCtrlGraphPanelShowAxesCB.Checked      := ShowAxes;
         PageCtrlCompPageCtrlGraphPanelShowGridlinesCB.Checked := ShowGridlines;
         PageCtrlCompPageCtrlGraphPanelShowLabelsCB.Checked    := ShowLabels;

         PageCtrlCompPageCtrlGraphPanelBackgroundVal.Selected := BackgroundColor;
         PageCtrlCompPageCtrlGraphPanelAxesVal.Selected       := AxesColor;
         PageCtrlCompPageCtrlGraphPanelGridlinesVal.Selected  := GridlinesColor;

         PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.Enabled := False;

         PageCtrlCompPageCtrlGraphPanelShowAxesCBClick(Self);
         PageCtrlCompPageCtrlGraphPanelShowGridlinesCBClick(Self);
         PageCtrlCompPageCtrlGraphPanelShowLabelsCBClick(Self);

         PageCtrlCompPageCtrlGraphPanelBackgroundValChange(Self);
         PageCtrlCompPageCtrlGraphPanelAxesValChange(Self);
         PageCtrlCompPageCtrlGraphPanelGridlinesValChange(Self);

         PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.Enabled := True;

         PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.ForceOGLRepaint;
      End;
   End;

   UpdateCompiler;

   FormCreating := False;
End;

//==============================================================================

Procedure TOptionsForm.FormDestroy(Sender: TObject);
Begin
   Elements.Free;

   CmdBinTree.Free;
End;

//==============================================================================

Procedure TOptionsForm.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Var
   I, Iter: Integer;
Begin
   If (ModalResult = mrOk) Then Begin
      // Make sure that everything is valid

      If ((PageCtrlGenProxyServerVal.Enabled And Not CheckEditFieldForIPAddressValidity(PageCtrlGenProxyServerVal)) Or
          (PageCtrlGenProxyServerVal.Enabled And Not CheckEditFieldForRangeIntegerValidity(PageCtrlGenProxyPortVal, 0, 65535)) Or
          (PageCtrlGenProxyAuthUsernameVal.Enabled And Not CheckEditFieldForUsernameValidity(PageCtrlGenProxyAuthUsernameVal)) Or
          Not CheckEditFieldForStrictlyPositiveIntegerValidity(PageCtrlEditorPageCtrlGenTabIndentVal) Or
          Not CheckEditFieldForPositiveIntegerValidity(PageCtrlEditorPageCtrlDispRightMarginVal) Or
          Not CheckEditFieldForPositiveIntegerValidity(PageCtrlEditorPageCtrlDispGutterWidthVal) Or
          Not CheckEditFieldForStrictlyPositiveFloatValidity(PageCtrlCompPageCtrlGenDurationVal) Or
          Not CheckEditFieldForStrictlyPositiveFloatValidity(PageCtrlCompPageCtrlGenOutputVal) Or
          (PageCtrlCompPageCtrlGenCompilerLocationVal.Enabled And Not CheckEditFieldForFolderPathValidity(PageCtrlCompPageCtrlGenCompilerLocationVal)) Or
          Not CheckEditFieldForStrictlyPositiveFloatValidity(PageCtrlCompPageCtrlIntFixedTimeStepVal) Or
          Not CheckEditFieldForPositiveFloatValidity(PageCtrlCompPageCtrlIntMaxTimeStepVal) Or
          Not CheckEditFieldForStrictlyPositiveIntegerValidity(PageCtrlCompPageCtrlIntMaxNbOfStepsVal) Or
          Not CheckEditFieldForPositiveIntegerValidity(PageCtrlCompPageCtrlIntUpperHalfBandwidthVal) Or
          Not CheckEditFieldForPositiveIntegerValidity(PageCtrlCompPageCtrlIntLowerHalfBandwidthVal) Or
          Not CheckEditFieldForStrictlyPositiveFloatValidity(PageCtrlCompPageCtrlIntRelTolVal) Or
          Not CheckEditFieldForStrictlyPositiveFloatValidity(PageCtrlCompPageCtrlIntAbsTolVal)) Then Begin
         // Something is not valid, so...

         CanClose := False;

         Exit;
      End;

      // General options

      With GeneralOptions Do Begin
         If (IsWinVistaOrAboveOrAdministrator) Then
            CheckForNewVersionAtStartup := PageCtrlGenCheckForNewVersionAtStartupCB.Checked;

         ConnectThroughProxyServer := PageCtrlGenConnectThroughProxyServerCB.Checked;

         ProxyServer := PageCtrlGenProxyServerVal.Text;

         If (CompareStr(PageCtrlGenProxyPortVal.Text, '') <> 0) Then
            ProxyPort := StrToInt(PageCtrlGenProxyPortVal.Text)
         Else
            ProxyPort := 8080;

         ProxyAuthenticationMethod := PageCtrlGenProxyAuthenticationMethodVal.Items[PageCtrlGenProxyAuthenticationMethodVal.ItemIndex];

         ProxyAuthenticationUsername := PageCtrlGenProxyAuthUsernameVal.Text;
         ProxyAuthenticationPassword := Encrypt(PageCtrlGenProxyAuthPasswordVal.Text);

         ProxyAuthenticationAskForPassword := PageCtrlGenProxyAuthAskForPasswordCB.Checked;
      End;

      // Editor options

      With EditorOptions Do Begin
         // General tab

         With GeneralOptions Do Begin
            InsertMode         := PageCtrlEditorPageCtrlGenInsertModeCB.Checked;
            GroupUndo          := PageCtrlEditorPageCtrlGenGrouUndoCB.Checked;
            ScrollPastEOF      := PageCtrlEditorPageCtrlGenScrollPastEOFCB.Checked;
            AutoIndentMode     := PageCtrlEditorPageCtrlGenAutoIndentCB.Checked;
            SmartTabs          := PageCtrlEditorPageCtrlGenSmartTabsCB.Checked;
            BackspaceUnindents := PageCtrlEditorPageCtrlGenBackspaceUnindentsCB.Checked;
            ShowScrollHint     := PageCtrlEditorPageCtrlGenShowScrollHintCB.Checked;
            WordWrap           := PageCtrlEditorPageCtrlGenWordWrapCB.Checked;

            InsertCaret    := TSynEditCaretType(PageCtrlEditorPageCtrlGenInsertCaretVal.ItemIndex);
            OverwriteCaret := TSynEditCaretType(PageCtrlEditorPageCtrlGenOverwriteCaretVal.ItemIndex);

            TabIndent := StrToInt(PageCtrlEditorPageCtrlGenTabIndentVal.Text);

            For Iter := 0 To Views.Size-1 Do
               If (CompareStr(PageCtrlEditorPageCtrlGenViewVal.Text, String(Views.At(Iter).VString)) = 0) Then Begin
                  View := TEditorView(Iter);

                  Break;
               End;
         End;

         // Display tab

         With DisplayOptions Do Begin
            ShowRightMargin := PageCtrlEditorPageCtrlDispShowRightMarginCB.Checked;
            RightMargin     := StrToInt(PageCtrlEditorPageCtrlDispRightMarginVal.Text);

            ShowGutter  := PageCtrlEditorPageCtrlDispShowGutterCB.Checked;
            GutterWidth := StrToInt(PageCtrlEditorPageCtrlDispGutterWidthVal.Text);

            FontName := PageCtrlEditorPageCtrlDispFontVal.Text;
            FontSize := MIN_FONT_SIZE+PageCtrlEditorPageCtrlDispSizeVal.ItemIndex;
         End;

         // Colour tab

         With ColourOptions Do
            For I := 0 To PageCtrlEditorPageCtrlColElementVal.Count-1 Do
               Elements.Get(PageCtrlEditorPageCtrlColElementVal.Get(I)).Copy(Self.Elements.Get(PageCtrlEditorPageCtrlColElementVal.Get(I)));

         // Command viewer tab

         With CommandViewerOptions Do Begin
            OptimisedFontSize := PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB.Checked;
            UnderscoreForSub  := PageCtrlEditorPageCtrlCmdViewUnderscoreForSubCB.Checked;
            GreekSymbols      := PageCtrlEditorPageCtrlCmdViewGreekSymbolsCB.Checked;
            DigitGrouping     := PageCtrlEditorPageCtrlCmdViewDigitGroupingCB.Checked;
            FontName          := PageCtrlEditorPageCtrlCmdViewFontVal.Text;
            FontSize          := MIN_FONT_SIZE+PageCtrlEditorPageCtrlCmdViewSizeVal.ItemIndex;
            SubFont           := PageCtrlEditorPageCtrlCmdViewSubFontPB.Position;

            BackgroundColor := PageCtrlEditorPageCtrlCmdViewBackgroundVal.Selected;
            ForegroundColor := PageCtrlEditorPageCtrlCmdViewForegroundVal.Selected;
         End;
      End;

      // Computation options

      With ComputationOptions Do Begin
         // General

         With GeneralOptions Do Begin
            Duration := StrToFloat(PageCtrlCompPageCtrlGenDurationVal.Text);
            Output   := StrToFloat(PageCtrlCompPageCtrlGenOutputVal.Text);

            DebugMode := PageCtrlCompPageCtrlGenDebugModeCB.Checked;

            For Iter := 0 To Compilers.Size-1 Do
               If (CompareStr(PageCtrlCompPageCtrlGenCompilerVal.Text, String(Compilers.At(Iter).VString)) = 0) Then Begin
                  Compiler := TCellMLModelRuntimeCompiler(Iter);

                  Break;
               End;

            CompilerLocation := PageCtrlCompPageCtrlGenCompilerLocationVal.Text;
         End;

         // Integration

         With IntegrationOptions Do Begin
            Integrator := TODEIntegratorType(PageCtrlCompPageCtrlIntIntegratorVal.ItemIndex);

            TimeStep := StrToFloat(PageCtrlCompPageCtrlIntFixedTimeStepVal.Text);

            MaxTimeStep := StrToFloat(PageCtrlCompPageCtrlIntMaxTimeStepVal.Text);

            MaxNbOfSteps := StrToInt(PageCtrlCompPageCtrlIntMaxNbOfStepsVal.Text);

            Method := TODECVODEIntegratorMethod(PageCtrlCompPageCtrlIntMethodVal.ItemIndex+1);
            // Note: "+1" because "TODECVODEIntegratorMethod" is 1-based, as
            //       opposed to 0-based...

            Iterator           := TODECVODEIntegratorIterator(PageCtrlCompPageCtrlIntIteratorVal.ItemIndex+1);
            // Note: "+1" because "TODECVODEIntegratorIterator" is 1-based, as
            //       opposed to 0-based...
            LinearSolver       := TODECVODEIntegratorLinearSolver(PageCtrlCompPageCtrlIntLinearSolverVal.ItemIndex);
            Preconditioner     := TODECVODEIntegratorPreconditioner(PageCtrlCompPageCtrlIntPreconditionerVal.ItemIndex);
            UpperHalfBandwidth := StrToInt(PageCtrlCompPageCtrlIntUpperHalfBandwidthVal.Text);
            LowerHalfBandwidth := StrToInt(PageCtrlCompPageCtrlIntLowerHalfBandwidthVal.Text);

            RelTol := StrToFloat(PageCtrlCompPageCtrlIntRelTolVal.Text);
            AbsTol := StrToFloat(PageCtrlCompPageCtrlIntAbsTolVal.Text);
         End;

         // Graphical panel

         With GraphicalPanelOptions Do Begin
            UseGradientForTraces := PageCtrlCompPageCtrlGraphPanelUseGradientForTracesCB.Checked;

            ShowAxes      := PageCtrlCompPageCtrlGraphPanelShowAxesCB.Checked;
            ShowGridlines := PageCtrlCompPageCtrlGraphPanelShowGridlinesCB.Checked;
            ShowLabels    := PageCtrlCompPageCtrlGraphPanelShowLabelsCB.Checked;

            BackgroundColor := PageCtrlCompPageCtrlGraphPanelBackgroundVal.Selected;
            AxesColor       := PageCtrlCompPageCtrlGraphPanelAxesVal.Selected;
            GridlinesColor  := PageCtrlCompPageCtrlGraphPanelGridlinesVal.Selected;
         End;
      End;
   End;
End;

//==============================================================================

Procedure TOptionsForm.UpdateGeneral;
Var
   NeedExplicitAuthentication: Boolean;
Begin
   PageCtrlGenProxyServerLab.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked;
   PageCtrlGenProxyServerVal.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked;

   PageCtrlGenProxyPortLab.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked;
   PageCtrlGenProxyPortVal.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked;

   PageCtrlGenProxyAuthenticationMethodLab.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked;
   PageCtrlGenProxyAuthenticationMethodVal.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked;

   NeedExplicitAuthentication := CompareStr(PageCtrlGenProxyAuthenticationMethodVal.Items[PageCtrlGenProxyAuthenticationMethodVal.ItemIndex], 'Basic') = 0;

   PageCtrlGenProxyAuthUsernameLab.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked And NeedExplicitAuthentication;
   PageCtrlGenProxyAuthUsernameVal.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked And NeedExplicitAuthentication;

   PageCtrlGenProxyAuthPasswordLab.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked And NeedExplicitAuthentication And Not PageCtrlGenProxyAuthAskForPasswordCB.Checked;
   PageCtrlGenProxyAuthPasswordVal.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked And NeedExplicitAuthentication And Not PageCtrlGenProxyAuthAskForPasswordCB.Checked;

   PageCtrlGenProxyAuthAskForPasswordCB.Enabled := PageCtrlGenConnectThroughProxyServerCB.Checked And NeedExplicitAuthentication;
End;

//==============================================================================

Procedure TOptionsForm.UpdateCompiler;
Var
   ExtCompilerEnabled: Boolean;
Begin
   ExtCompilerEnabled := PageCtrlCompPageCtrlGenCompilerVal.ItemIndex <> 0;

   PageCtrlCompPageCtrlGenCompilerLocationLab.Enabled := ExtCompilerEnabled;
   PageCtrlCompPageCtrlGenCompilerLocationVal.Enabled := ExtCompilerEnabled;
   PageCtrlCompPageCtrlGenCompilerLocationBtn.Enabled := ExtCompilerEnabled;

   PageCtrlCompPageCtrlGenCompilerNoteLab.Enabled := ExtCompilerEnabled;
   PageCtrlCompPageCtrlGenCompilerNoteVal.Enabled := ExtCompilerEnabled;
End;

//==============================================================================

Procedure TOptionsForm.InitElement(Const aElement: String);
Begin
   PageCtrlEditorPageCtrlColElementVal.Add(aElement);

   Elements.Get(aElement).Copy(EditorOptions.ColourOptions.Elements.Get(aElement));
End;

//==============================================================================

Procedure TOptionsForm.UpdateIntElement;
Begin
   With Elements.Get(PageCtrlEditorPageCtrlColElementVal.Get(PageCtrlEditorPageCtrlColElementVal.NodeIndex)) Do Begin
      BackgroundColor := PageCtrlEditorPageCtrlColBackgroundVal.Selected;
      ForegroundColor := PageCtrlEditorPageCtrlColForegroundVal.Selected;

      If (PageCtrlEditorPageCtrlColBoldCB.Checked) Then
         FontStyle := FontStyle+[fsBold]
      Else
         FontStyle := FontStyle-[fsBold];

      If (PageCtrlEditorPageCtrlColItalicCB.Checked) Then
         FontStyle := FontStyle+[fsItalic]
      Else
         FontStyle := FontStyle-[fsItalic];

      If (PageCtrlEditorPageCtrlColUnderlineCB.Checked) Then
         FontStyle := FontStyle+[fsUnderline]
      Else
         FontStyle := FontStyle-[fsUnderline];

      Alpha := PageCtrlEditorPageCtrlColAlphaSB.Position;
   End;
End;

//==============================================================================

Procedure TOptionsForm.UpdateExtElement;
Begin
   // Make sure that the options don't trigger any event when being modified

   PageCtrlEditorPageCtrlColBackgroundVal.OnChange := Nil;
   PageCtrlEditorPageCtrlColForegroundVal.OnChange := Nil;

   PageCtrlEditorPageCtrlColBoldCB.OnClick      := Nil;
   PageCtrlEditorPageCtrlColItalicCB.OnClick    := Nil;
   PageCtrlEditorPageCtrlColUnderlineCB.OnClick := Nil;

   PageCtrlEditorPageCtrlColAlphaSB.OnChange := Nil;

   // Update the options

   With Elements.Get(PageCtrlEditorPageCtrlColElementVal.Get(PageCtrlEditorPageCtrlColElementVal.NodeIndex)) Do Begin
      PageCtrlEditorPageCtrlColBackgroundVal.Selected := BackgroundColor;
      PageCtrlEditorPageCtrlColForegroundVal.Selected := ForegroundColor;

      PageCtrlEditorPageCtrlColBoldCB.Checked      := fsBold In FontStyle;
      PageCtrlEditorPageCtrlColItalicCB.Checked    := fsItalic In FontStyle;
      PageCtrlEditorPageCtrlColUnderlineCB.Checked := fsUnderline In FontStyle;

      PageCtrlEditorPageCtrlColAlphaSB.Position := Alpha;

      // Enable/disable some options

      PageCtrlEditorPageCtrlColBackgroundLab.Enabled := CompareStr(Name, RIGHT_MARGIN) <> 0;
      PageCtrlEditorPageCtrlColBackgroundVal.Enabled := PageCtrlEditorPageCtrlColBackgroundLab.Enabled;

      PageCtrlEditorPageCtrlColForegroundLab.Enabled := (CompareStr(Name, GUTTER_AREA) <> 0) And (CompareStr(Name, SCROLL_HINT) <> 0) And (CompareStr(Name, LOCKED_FILE) <> 0);
      PageCtrlEditorPageCtrlColForegroundVal.Enabled := PageCtrlEditorPageCtrlColForegroundLab.Enabled;

      PageCtrlEditorPageCtrlColBoldCB.Enabled      := (CompareStr(Name, DEFAULT) = 0) Or (CompareStr(Name, RESERVED_WORD) = 0) Or (CompareStr(Name, EXTRA_INFO) = 0) Or (CompareStr(Name, ERROR_LINE) = 0);
      PageCtrlEditorPageCtrlColItalicCB.Enabled    := PageCtrlEditorPageCtrlColBoldCB.Enabled;
      PageCtrlEditorPageCtrlColUnderlineCB.Enabled := PageCtrlEditorPageCtrlColBoldCB.Enabled;

      PageCtrlEditorPageCtrlColAlphaLab.Enabled := CompareStr(Name, LOCKED_FILE) = 0;
      PageCtrlEditorPageCtrlColAlphaVal.Enabled := PageCtrlEditorPageCtrlColAlphaLab.Enabled;
      PageCtrlEditorPageCtrlColAlphaS.Enabled   := PageCtrlEditorPageCtrlColAlphaLab.Enabled;
      PageCtrlEditorPageCtrlColAlphaSB.Enabled  := PageCtrlEditorPageCtrlColAlphaLab.Enabled;

      // Update the alpha shape

      UpdateAlpha;
   End;

   // Allow the options to trigger an event

   PageCtrlEditorPageCtrlColBackgroundVal.OnChange := UpdateIntElementAndEditorSample;
   PageCtrlEditorPageCtrlColForegroundVal.OnChange := UpdateIntElementAndEditorSample;

   PageCtrlEditorPageCtrlColBoldCB.OnClick      := UpdateIntElementAndEditorSample;
   PageCtrlEditorPageCtrlColItalicCB.OnClick    := UpdateIntElementAndEditorSample;
   PageCtrlEditorPageCtrlColUnderlineCB.OnClick := UpdateIntElementAndEditorSample;

   PageCtrlEditorPageCtrlColAlphaSB.OnChange := PageCtrlEditorPageCtrlColAlphaSBChange;
End;

//==============================================================================

Procedure TOptionsForm.UpdateEditorSample;
Var
   Element: TEditorColElemOptions;
Begin
   Element := Elements.Get(PageCtrlEditorPageCtrlColElementVal.Get(PageCtrlEditorPageCtrlColElementVal.NodeIndex));

   // If we modified the default element, then must update the background and
   // foreground colour of the editor sample

   With Element Do
      If (CompareStr(Name, DEFAULT) = 0) Then Begin
         PageCtrlEditorPageCtrlDispSampleVal.Color   := BackgroundColor;
         PageCtrlEditorPageCtrlColEditorSample.Color := BackgroundColor;

         PageCtrlEditorPageCtrlDispSampleVal.Font.Color   := Foregroundcolor;
         PageCtrlEditorPageCtrlColEditorSample.Font.Color := ForegroundColor;
      End Else If (CompareStr(Name, RIGHT_MARGIN) = 0) Then Begin
         PageCtrlEditorPageCtrlDispSampleVal.RightEdgeColor   := ForegroundColor;
         PageCtrlEditorPageCtrlColEditorSample.RightEdgeColor := ForegroundColor;
      End Else If (CompareStr(Name, GUTTER_AREA) = 0) Then Begin
         PageCtrlEditorPageCtrlDispSampleVal.Gutter.Color   := BackgroundColor;
         PageCtrlEditorPageCtrlColEditorSample.Gutter.Color := BackgroundColor;
      End;

   // Update the syntax highlighting settings for the current element

   UpdateCellMLSynHighlighter(SynCellMLSyn, Element);
End;

//==============================================================================

Procedure TOptionsForm.UpdateAlpha;
Var
   LC, C: TRGB;
   Alpha, One_Alpha: Double;
Begin
   PageCtrlEditorPageCtrlColAlphaVal.Caption := IntToStr(PageCtrlEditorPageCtrlColAlphaSB.Position)+'%';

   If (CompareStr(Elements.Get(PageCtrlEditorPageCtrlColElementVal.Get(PageCtrlEditorPageCtrlColElementVal.NodeIndex)).Name, LOCKED_FILE) = 0) Then
      LC := RGBToRGBValues(ColorToRGB(PageCtrlEditorPageCtrlColBackgroundVal.Selected))
   Else
      LC := RGBToRGBValues(ColorToRGB(clWindow));

   C := RGBToRGBValues(ColorToRGB(clWindow));

   Alpha     := 0.01*PageCtrlEditorPageCtrlColAlphaSB.Position;
   One_Alpha := 1-Alpha;

   PageCtrlEditorPageCtrlColAlphaS.Brush.Color := RGB(Round(Alpha*LC.R+One_Alpha*C.R),
                                                      Round(Alpha*LC.G+One_Alpha*C.G),
                                                      Round(Alpha*LC.B+One_Alpha*C.B));
End;

//==============================================================================

Procedure TOptionsForm.UpdateFontSample(Sender: TObject);
Begin
   PageCtrlEditorPageCtrlDispSampleVal.Font.Name   := PageCtrlEditorPageCtrlDispFontVal.Text;
   PageCtrlEditorPageCtrlColEditorSample.Font.Name := PageCtrlEditorPageCtrlDispFontVal.Text;

   PageCtrlEditorPageCtrlDispSampleVal.Font.Size   := StrToInt(PageCtrlEditorPageCtrlDispSizeVal.Text);
   PageCtrlEditorPageCtrlColEditorSample.Font.Size := PageCtrlEditorPageCtrlDispSampleVal.Font.Size;
End;

//==============================================================================

Procedure TOptionsForm.UpdateExtElementAndEditorSample(Sender: TObject);
Begin
   UpdateExtElement;

   UpdateEditorSample;
End;

//==============================================================================

Procedure TOptionsForm.UpdateIntElementAndEditorSample(Sender: TObject);
Begin
   If ((Sender = PageCtrlEditorPageCtrlColBackgroundVal) And
       (CompareStr(Elements.Get(PageCtrlEditorPageCtrlColElementVal.Get(PageCtrlEditorPageCtrlColElementVal.NodeIndex)).Name, LOCKED_FILE) = 0)) Then
      UpdateAlpha;

   UpdateIntElement;

   UpdateEditorSample;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlEditorPageCtrlDispShowRightMarginCBClick(Sender: TObject);
Begin
   If (PageCtrlEditorPageCtrlDispShowRightMarginCB.Checked) Then Begin
      PageCtrlEditorPageCtrlDispRightMarginLab.Enabled := True;
      PageCtrlEditorPageCtrlDispRightMarginVal.Enabled := True;

      PageCtrlEditorPageCtrlDispSampleVal.RightEdge   := StrToInt(PageCtrlEditorPageCtrlDispRightMarginVal.Text);
      PageCtrlEditorPageCtrlColEditorSample.RightEdge := PageCtrlEditorPageCtrlDispSampleVal.RightEdge;
   End Else Begin
      PageCtrlEditorPageCtrlDispRightMarginLab.Enabled := False;
      PageCtrlEditorPageCtrlDispRightMarginVal.Enabled := False;

      PageCtrlEditorPageCtrlDispSampleVal.RightEdge   := 0;
      PageCtrlEditorPageCtrlColEditorSample.RightEdge := 0;
   End;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlEditorPageCtrlDispShowGutterCBClick(Sender: TObject);
Begin
   If (PageCtrlEditorPageCtrlDispShowGutterCB.Checked) Then Begin
      PageCtrlEditorPageCtrlDispGutterWidthLab.Enabled := True;
      PageCtrlEditorPageCtrlDispGutterWidthVal.Enabled := True;

      PageCtrlEditorPageCtrlDispSampleVal.Gutter.Visible   := True;
      PageCtrlEditorPageCtrlColEditorSample.Gutter.Visible := True;
   End Else Begin
      PageCtrlEditorPageCtrlDispGutterWidthLab.Enabled := False;
      PageCtrlEditorPageCtrlDispGutterWidthVal.Enabled := False;

      PageCtrlEditorPageCtrlDispSampleVal.Gutter.Visible   := False;
      PageCtrlEditorPageCtrlColEditorSample.Gutter.Visible := False;
   End;
End;

//==============================================================================

Procedure TOptionsForm.UpdateCmdGraphSample(Sender: TObject);
Begin
   If (Sender = PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB) Then Begin
      PageCtrlEditorPageCtrlCmdViewSizeLab.Enabled := Not PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB.Checked;
      PageCtrlEditorPageCtrlCmdViewSizeVal.Enabled := Not PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB.Checked;
   End;

   PageCtrlEditorPageCtrlCmdViewSubFontVal.Caption := IntToStr(PageCtrlEditorPageCtrlCmdViewSubFontPB.Position)+'%';

   With PageCtrlEditorPageCtrlCmdViewPanelCmdGraph Do Begin
      OptimisedFontSize := PageCtrlEditorPageCtrlCmdViewOptimisedFontSizeCB.Checked;
      UnderscoreForSub  := PageCtrlEditorPageCtrlCmdViewUnderscoreForSubCB.Checked;
      GreekSymbols      := PageCtrlEditorPageCtrlCmdViewGreekSymbolsCB.Checked;
      DigitGrouping     := PageCtrlEditorPageCtrlCmdViewDigitGroupingCB.Checked;
      Font.Name         := PageCtrlEditorPageCtrlCmdViewFontVal.Text;
      Font.Size         := MIN_FONT_SIZE+PageCtrlEditorPageCtrlCmdViewSizeVal.ItemIndex;
      SubFont           := PageCtrlEditorPageCtrlCmdViewSubFontPB.Position;

      Color      := PageCtrlEditorPageCtrlCmdViewBackgroundVal.Selected;
      Font.Color := PageCtrlEditorPageCtrlCmdViewForegroundVal.Selected;
   End;
End;

//==============================================================================

Procedure TOptionsForm.CMDialogKey(Var aMsg: TCMDialogKey);
Var
   Control: TWinControl;
Begin
   // Allow to scroll through the tabs of a sub-page control...

   With aMsg Do
      If (CharCode = VK_TAB) And (GetKeyState(VK_CONTROL) < 0) Then Begin
         Control:= ActiveControl;

         While Assigned(Control) Do
            If (Control Is TPageControl) Then Begin
               Control.Perform(CM_DIALOGKEY, CharCode, KeyData);

               Exit;
            End Else
               Control := Control.Parent;
      End;

   Inherited;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlGenProxySettingsClick(Sender: TObject);
Begin
   UpdateGeneral;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlEditorPageCtrlDispRightMarginValChange(Sender: TObject);
Begin
   If (CompareStr(PageCtrlEditorPageCtrlDispRightMarginVal.Text, '') <> 0) Then Begin
      PageCtrlEditorPageCtrlDispSampleVal.RightEdge   := StrToInt(PageCtrlEditorPageCtrlDispRightMarginVal.Text);
      PageCtrlEditorPageCtrlColEditorSample.RightEdge := PageCtrlEditorPageCtrlDispSampleVal.RightEdge;
   End Else Begin
      PageCtrlEditorPageCtrlDispSampleVal.RightEdge   := 0;
      PageCtrlEditorPageCtrlColEditorSample.RightEdge := 0;
   End;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGenCompilerLocationBtnClick(Sender: TObject);
Var
   Location: String;
Begin
   Location := PageCtrlCompPageCtrlGenCompilerLocationVal.Text;

   If (SelectFolderDialog('Please select the location of the '+PageCtrlCompPageCtrlGenCompilerVal.Text+' compiler:', Location, BIF_NEWDIALOGSTYLE Or BIF_NONEWFOLDERBUTTON)) Then
      PageCtrlCompPageCtrlGenCompilerLocationVal.Text := Location;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGenCompilerLocationValExit(Sender: TObject);
Begin
   If (DirectoryExists(PageCtrlCompPageCtrlGenCompilerLocationVal.Text)) Then
      PageCtrlCompPageCtrlGenCompilerLocationVal.Text := IncludeTrailingPathDelimiter(PageCtrlCompPageCtrlGenCompilerLocationVal.Text);
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGraphPanelBackgroundValChange(Sender: TObject);
Begin
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.Color := PageCtrlCompPageCtrlGraphPanelBackgroundVal.Selected;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGraphPanelAxesValChange(Sender: TObject);
Begin
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.LineSpecs.Color := PageCtrlCompPageCtrlGraphPanelAxesVal.Selected;
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.LineSpecs.Color := PageCtrlCompPageCtrlGraphPanelAxesVal.Selected;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGraphPanelGridlinesValChange(Sender: TObject);
Begin
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.Grid.LineSpecs.Color := PageCtrlCompPageCtrlGraphPanelGridlinesVal.Selected;
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.Grid.LineSpecs.Color := PageCtrlCompPageCtrlGraphPanelGridlinesVal.Selected;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGraphPanelShowAxesCBClick(Sender: TObject);
Begin
   PageCtrlCompPageCtrlGraphPanelShowGridlinesCB.Enabled := PageCtrlCompPageCtrlGraphPanelShowAxesCB.Checked;
   PageCtrlCompPageCtrlGraphPanelShowLabelsCB.Enabled    := PageCtrlCompPageCtrlGraphPanelShowAxesCB.Checked;

   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.LineSpecs.Enabled := PageCtrlCompPageCtrlGraphPanelShowAxesCB.Checked;
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.LineSpecs.Enabled := PageCtrlCompPageCtrlGraphPanelShowAxesCB.Checked;

   PageCtrlCompPageCtrlGraphPanelShowLabelsCBClick(Sender);
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGraphPanelShowGridlinesCBClick(Sender: TObject);
Begin
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.Grid.LineSpecs.Enabled := PageCtrlCompPageCtrlGraphPanelShowGridlinesCB.Checked;
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.Grid.LineSpecs.Enabled := PageCtrlCompPageCtrlGraphPanelShowGridlinesCB.Checked;

   PageCtrlCompPageCtrlGraphPanelShowLabelsCBClick(Sender);
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGraphPanelShowLabelsCBClick(Sender: TObject);
Begin
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.CoordsSpecs.Enabled      := PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.LineSpecs.Enabled And
                                                                                      PageCtrlCompPageCtrlGraphPanelShowLabelsCB.Checked;
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.Grid.CoordsSpecs.Enabled := PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.LineSpecs.Enabled      And
                                                                                      PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.XAxis.Grid.LineSpecs.Enabled And
                                                                                      PageCtrlCompPageCtrlGraphPanelShowLabelsCB.Checked;

   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.CoordsSpecs.Enabled      := PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.LineSpecs.Enabled And
                                                                                      PageCtrlCompPageCtrlGraphPanelShowLabelsCB.Checked;
   PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.Grid.CoordsSpecs.Enabled := PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.LineSpecs.Enabled      And
                                                                                      PageCtrlCompPageCtrlGraphPanelPanelOGLGraphPanel.YAxis.Grid.LineSpecs.Enabled And
                                                                                      PageCtrlCompPageCtrlGraphPanelShowLabelsCB.Checked;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlCompPageCtrlGenCompilerValChange(Sender: TObject);
Begin
   UpdateCompiler;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlEditorPageCtrlColAlphaSBChange(Sender: TObject);
Begin
   UpdateAlpha;

   UpdateIntElement;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlEditorPageCtrlColElementValChange(Sender: TBaseVirtualTree;
                                                                 Node: PVirtualNode);
Begin
   If (Not FormCreating) Then Begin
      PageCtrlEditorPageCtrlColElementVal.NodeIndex := PageCtrlEditorPageCtrlColElementVal.FocusedNode.Index;

      UpdateExtElementAndEditorSample(Sender);
   End;
End;

//==============================================================================

Procedure TOptionsForm.PageCtrlEditorPageCtrlDispGutterWidthValChange(Sender: TObject);
Begin
   If (CompareStr(PageCtrlEditorPageCtrlDispGutterWidthVal.Text, '') <> 0) Then Begin
      PageCtrlEditorPageCtrlDispSampleVal.Gutter.Width   := StrToInt(PageCtrlEditorPageCtrlDispGutterWidthVal.Text);
      PageCtrlEditorPageCtrlColEditorSample.Gutter.Width := PageCtrlEditorPageCtrlDispSampleVal.Gutter.Width;
   End Else Begin
      PageCtrlEditorPageCtrlDispSampleVal.Gutter.Width   := 0;
      PageCtrlEditorPageCtrlColEditorSample.Gutter.Width := 0;
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

