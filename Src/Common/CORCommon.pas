//==============================================================================
// COR common unit
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 03/05/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CORCommon;

//==============================================================================

Interface

//==============================================================================

Uses
   SysUtils, IdGlobal, DeCAL, SynEdit, Graphics, CmdGraph, Controls, Classes,
   VirtualTrees, ODEIntegrator, ODECVODEIntegrator, IdHTTP, SynHighlighterXML,
   SynHighlighterCellML, SplashScreen, JvValidateEdit, CellMLAPI,
   CellMLAPIToMCEngine;

//==============================================================================

{$M+}

//==============================================================================

Type
   TMemoryInfo = Packed Record
      // Note: we should normally use DWord, but if we want to get the total
      //       memory available through the local network, then that won't be
      //       sufficient (4 GB maximum would be allowed), so...

      Available: Int64;   // B
      Total: Int64;       // B
   End;
   TComputer = Class
      Private
         // Private representation of published properties

         FName: String;
         FCPUSpeed: Int64;   // Hz
         FIPAddress: String;
         FPhysicalMemory: TMemoryInfo;
         FWindowsVersion: String;
         FCORShortVersion: String;
         FCORVersion: String;

         // Methods used for internal purposes

         Procedure UpdateCPUSpeed; Inline;
         Procedure UpdateMemory; Inline;

      Public
         // Constructor & Destructor

         Constructor Create(Const aBytes: TBytes = Nil);

         // User's methods

         Class Function Compare(Const aObj1, aObj2: DObject): Integer; Inline;

         Procedure Update;

         Function AsBytes: TBytes;

      Published
         // Published properties

         Property Name: String Read FName;
         Property CPUSpeed: Int64 Read FCPUSpeed;
         Property IPAddress: String Read FIPAddress;
         Property PhysicalMemory: TMemoryInfo Read FPhysicalMemory;
         Property WindowsVersion: String Read FWindowsVersion;
         Property CORShortVersion: String Read FCORShortVersion;
         Property CORVersion: String Read FCORVersion;
   End;
   TGeneralOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetCheckForNewVersionAtStartup(Const aValue: Boolean); Inline;

         Procedure SetConnectThroughProxyServer(Const aValue: Boolean); Inline;

         Procedure SetProxyServer(Const aValue: String); Inline;
         Procedure SetProxyPort(Const aValue: Integer); Inline;

         Procedure SetProxyAuthenticationMethod(Const aValue: String); Inline;

         Procedure SetProxyAuthenticationUsername(Const aValue: String); Inline;
         Procedure SetProxyAuthenticationPassword(Const aValue: String); Inline;

         Procedure SetProxyAuthenticationAskForPassword(Const aValue: Boolean); Inline;

      Protected
         // Private representation of published properties

         FCheckForNewVersionAtStartup: Boolean;

         FConnectThroughProxyServer: Boolean;

         FProxyServer: String;
         FProxyPort: Integer;

         FProxyAuthenticationMethod: String;

         FProxyAuthenticationUsername: String;
         FProxyAuthenticationPassword: String;

         FProxyAuthenticationAskForPassword: Boolean;

      Public
         // User's methods

         Procedure Copy(Const aGeneralOptions: TGeneralOptions);

      Published
         // Published properties

         Property CheckForNewVersionAtStartup: Boolean Read FCheckForNewVersionAtStartup Write SetCheckForNewVersionAtStartup Default True;

         Property ConnectThroughProxyServer: Boolean Read FConnectThroughProxyServer Write SetConnectThroughProxyServer Default False;

         Property ProxyServer: String Read FProxyServer Write SetProxyServer;
         Property ProxyPort: Integer Read FProxyPort Write SetProxyPort Default 8080;

         Property ProxyAuthenticationMethod: String Read FProxyAuthenticationMethod Write SetProxyAuthenticationMethod;

         Property ProxyAuthenticationUsername: String Read FProxyAuthenticationUsername Write SetProxyAuthenticationUsername;
         Property ProxyAuthenticationPassword: String Read FProxyAuthenticationPassword Write SetProxyAuthenticationPassword;

         Property ProxyAuthenticationAskForPassword: Boolean Read FProxyAuthenticationAskForPassword Write SetProxyAuthenticationAskForPassword Default False;
   End;
   TEditorView = (evCOR, evRaw);
   TEditorGeneralOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetInsertMode(Const aValue: Boolean); Inline;
         Procedure SetGroupUndo(Const aValue: Boolean); Inline;
         Procedure SetScrollPastEOF(Const aValue: Boolean); Inline;
         Procedure SetAutoIndentMode(Const aValue: Boolean); Inline;
         Procedure SetSmartTabs(Const aValue: Boolean); Inline;
         Procedure SetBackspaceUnindents(Const aValue: Boolean); Inline;
         Procedure SetShowScrollHint(Const aValue: Boolean); Inline;
         Procedure SetWordWrap(Const aValue: Boolean); Inline;

         Procedure SetView(Const aValue: TEditorView); Inline;

         Procedure SetInsertCaret(Const aValue: TSynEditCaretType); Inline;
         Procedure SetOverwriteCaret(Const aValue: TSynEditCaretType); Inline;

         Procedure SetTabIndent(Const aValue: Integer); Inline;

      Protected
         // Private representation of published properties

         FInsertMode: Boolean;
         FGroupUndo: Boolean;
         FScrollPastEOF: Boolean;
         FAutoIndentMode: Boolean;
         FSmartTabs: Boolean;
         FBackspaceUnindents: Boolean;
         FShowScrollHint: Boolean;
         FWordWrap: Boolean;

         FView: TEditorView;

         FInsertCaret: TSynEditCaretType;
         FOverwriteCaret: TSynEditCaretType;

         FTabIndent: Integer;

      Public
         // User's methods

         Procedure Copy(Const aEditorGeneralOptions: TEditorGeneralOptions);

      Published
         // Published properties

         Property InsertMode: Boolean Read FInsertMode Write SetInsertMode Default True;
         Property GroupUndo: Boolean Read FGroupUndo Write SetGroupUndo Default True;
         Property ScrollPastEOF: Boolean Read FScrollPastEOF Write SetScrollPastEOF Default False;
         Property AutoIndentMode: Boolean Read FAutoIndentMode Write SetAutoIndentMode Default True;
         Property SmartTabs: Boolean Read FSmartTabs Write SetSmartTabs Default True;
         Property BackspaceUnindents: Boolean Read FBackspaceUnindents Write SetBackspaceUnindents Default True;
         Property ShowScrollHint: Boolean Read FShowScrollHint Write SetShowScrollHint Default True;
         Property WordWrap: Boolean Read FWordWrap Write SetWordWrap Default True;

         Property View: TEditorView Read FView Write SetView Default evCOR;

         Property InsertCaret: TSynEditCaretType Read FInsertCaret Write SetInsertCaret Default ctVerticalLine;
         Property OverwriteCaret: TSynEditCaretType Read FOverwriteCaret Write SetOverwriteCaret Default ctBlock;

         Property TabIndent: Integer Read FTabIndent Write SetTabIndent Default 3;
   End;
   TEditorDisplayOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetShowRightMargin(Const aValue: Boolean); Inline;
         Procedure SetRightMargin(Const aValue: Integer); Inline;

         Procedure SetShowGutter(Const aValue: Boolean); Inline;
         Procedure SetGutterWidth(Const aValue: Integer); Inline;

         Procedure SetFontName(Const aValue: String); Inline;
         Procedure SetFontSize(Const aValue: Integer); Inline;

      Protected
         // Private representation of published properties

         FShowRightMargin: Boolean;
         FRightMargin: Integer;

         FShowGutter: Boolean;
         FGutterWidth: Integer;

         FFontName: String;
         FFontSize: Integer;

      Public
         // User's methods

         Procedure Copy(Const aEditorDisplayOptions: TEditorDisplayOptions);

      Published
         // Published properties

         Property ShowRightMargin: Boolean Read FShowRightMargin Write SetShowRightMargin Default True;
         Property RightMargin: Integer Read FRightMargin Write SetRightMargin Default 80;

         Property ShowGutter: Boolean Read FShowGutter Write SetShowGutter Default True;
         Property GutterWidth: Integer Read FGutterWidth Write SetGutterWidth Default 17;

         Property FontName: String Read FFontName Write SetFontName;
         Property FontSize: Integer Read FFontSize Write SetFontSize Default 10;
   End;
   TEditorColElemOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetName(Const aValue: String); Inline;

         Procedure SetBackgroundColor(Const aValue: TColor); Inline;
         Procedure SetForegroundColor(Const aValue: TColor); Inline;

         Procedure SetFontStyle(Const aValue: TFontStyles); Inline;

         Procedure SetAlpha(Const aValue: Byte); Inline;

      Protected
         // Private representation of published properties

         FName: String;

         FBackgroundColor: TColor;
         FForegroundColor: TColor;

         FFontStyle: TFontStyles;

         FAlpha: Byte;

      Public
         // Constructor & Destructor

         Constructor Create(Const aName: String;
                            Const aBackgroundColor: TColor = clWindow;
                            Const aForegroundColor: TColor = clWindowText;
                            Const aFontStyle: TFontStyles = [];
                            Const aAlpha: Byte = 100);

         // User's methods

         Procedure Copy(Const aEditorColElemOptions: TEditorColElemOptions);

      Published
         // Published properties

         Property Name: String Read FName Write SetName;

         Property BackgroundColor: TColor Read FBackgroundColor Write SetBackgroundColor;
         Property ForegroundColor: TColor Read FForegroundColor Write SetForegroundColor;

         Property FontStyle: TFontStyles Read FFontStyle Write SetFontStyle;

         Property Alpha: Byte Read FAlpha Write SetAlpha;
   End;
   TEditorColElemOptionsList = Class(DArray)
      Private
         // Methods used for internal purposes

         Function Compare(Const aObj1, aObj2: DObject): Integer; Inline;

      Public
         // Constructor & Destructor

         Constructor Create; Override;
         Destructor Destroy; Override;

         // User's methods

         Function Get(Const aName: String): TEditorColElemOptions;
   End;
   TEditorColourOptions = Class
      Protected
         // Private representation of published properties

         FElements: TEditorColElemOptionsList;

      Public
         // Constructor & Destructor

         Constructor Create;
         Destructor Destroy; Override;

         // User's methods

         Procedure Copy(Const aEditorColourOptions: TEditorColourOptions);

      Published
         // Published properties

         Property Elements: TEditorColElemOptionsList Read FElements;
   End;
   TEditorOptions = Class
      Protected
         // Private representation of published properties

         FGeneralOptions: TEditorGeneralOptions;
         FDisplayOptions: TEditorDisplayOptions;
         FColourOptions: TEditorColourOptions;
         FCommandViewerOptions: TCmdGraphOptions;

      Public
         // Constructor & Destructor

         Constructor Create;
         Destructor Destroy; Override;

         // User's methods

         Procedure Copy(Const aEditorOptions: TEditorOptions);

      Published
         // Published properties

         Property GeneralOptions: TEditorGeneralOptions Read FGeneralOptions;
         Property DisplayOptions: TEditorDisplayOptions Read FDisplayOptions;
         Property ColourOptions: TEditorColourOptions Read FColourOptions;
         Property CommandViewerOptions: TCmdGraphOptions Read FCommandViewerOptions;
   End;
   TComputationGeneralOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetDuration(Const aValue: Double); Inline;
         Procedure SetOutput(Const aValue: Double); Inline;

         Procedure SetDebugMode(Const aValue: Boolean); Inline;

         Procedure SetCompiler(Const aValue: TCellMLModelRuntimeCompiler); Inline;
         Procedure SetCompilerLocation(Const aValue: String); Inline;

      Protected
         // Private representation of published properties

         FDuration: Double;
         FOutput: Double;

         FDebugMode: Boolean;

         FCompiler: TCellMLModelRuntimeCompiler;
         FCompilerLocation: String;

      Public
         // User's methods

         Procedure Copy(Const aComputationGeneralOptions: TComputationGeneralOptions);

      Published
         // Published properties

         Property Duration: Double Read FDuration Write SetDuration;
         Property Output: Double Read FOutput Write SetOutput;

         Property DebugMode: Boolean Read FDebugMode Write SetDebugMode Default False;

         Property Compiler: TCellMLModelRuntimeCompiler Read FCompiler Write SetCompiler;
         Property CompilerLocation: String Read FCompilerLocation Write SetCompilerLocation;
   End;
   TComputationIntegrationOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetIntegrator(Const aValue: TODEIntegratorType); Inline;

         Procedure SetTimeStep(Const aValue: Double); Inline;

         Procedure SetMaxTimeStep(Const aValue: Double); Inline;

         Procedure SetMaxNbOfSteps(Const aValue: Integer); Inline;

         Procedure SetMethod(Const aValue: TODECVODEIntegratorMethod); Inline;

         Procedure SetIterator(Const aValue: TODECVODEIntegratorIterator); Inline;
         Procedure SetLinearSolver(Const aValue: TODECVODEIntegratorLinearSolver); Inline;
         Procedure SetPreconditioner(Const aValue: TODECVODEIntegratorPreconditioner); Inline;
         Procedure SetUpperHalfBandwidth(Const aValue: Integer); Inline;
         Procedure SetLowerHalfBandwidth(Const aValue: Integer); Inline;

         Procedure SetRelTol(Const aValue: Double); Inline;
         Procedure SetAbsTol(Const aValue: Double); Inline;

      Protected
         // Private representation of published properties

         FIntegrator: TODEIntegratorType;

         FTimeStep: Double;

         FMaxTimeStep: Double;

         FMaxNbOfSteps: Integer;

         FMethod: TODECVODEIntegratorMethod;

         FIterator: TODECVODEIntegratorIterator;
         FLinearSolver: TODECVODEIntegratorLinearSolver;
         FPreconditioner: TODECVODEIntegratorPreconditioner;
         FUpperHalfBandwidth: Integer;
         FLowerHalfBandwidth: Integer;

         FRelTol: Double;
         FAbsTol: Double;

      Public
         // User's methods

         Procedure Copy(Const aComputationIntegrationOptions: TComputationIntegrationOptions);

      Published
         // Published properties

         Property Integrator: TODEIntegratorType Read FIntegrator Write SetIntegrator;

         Property TimeStep: Double Read FTimeStep Write SetTimeStep;

         Property MaxTimeStep: Double Read FMaxTimeStep Write SetMaxTimeStep;

         Property MaxNbOfSteps: Integer Read FMaxNbOfSteps Write SetMaxNbOfSteps;

         Property Method: TODECVODEIntegratorMethod Read FMethod Write SetMethod;

         Property Iterator: TODECVODEIntegratorIterator Read FIterator Write SetIterator;
         Property LinearSolver: TODECVODEIntegratorLinearSolver Read FLinearSolver Write SetLinearSolver;
         Property Preconditioner: TODECVODEIntegratorPreconditioner Read FPreconditioner Write SetPreconditioner;
         Property UpperHalfBandwidth: Integer Read FUpperHalfBandwidth Write SetUpperHalfBandwidth;
         Property LowerHalfBandwidth: Integer Read FLowerHalfBandwidth Write SetLowerHalfBandwidth;

         Property RelTol: Double Read FRelTol Write SetRelTol;
         Property AbsTol: Double Read FAbsTol Write SetAbsTol;
   End;
   TComputationGraphicalPanelOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetUseGradientForTraces(Const aValue: Boolean); Inline;

         Procedure SetShowAxes(Const aValue: Boolean); Inline;
         Procedure SetShowGridlines(Const aValue: Boolean); Inline;
         Procedure SetShowLabels(Const aValue: Boolean); Inline;

         Procedure SetBackgroundColor(Const aValue: TColor); Inline;
         Procedure SetAxesColor(Const aValue: TColor); Inline;
         Procedure SetGridlinesColor(Const aValue: TColor); Inline;

      Protected
         // Private representation of published properties

         FUseGradientForTraces: Boolean;

         FShowAxes: Boolean;
         FShowGridlines: Boolean;
         FShowLabels: Boolean;

         FBackgroundColor: TColor;
         FAxesColor: TColor;
         FGridlinesColor: TColor;

      Public
         // User's methods

         Procedure Copy(Const aComputationGraphicalPanelOptions: TComputationGraphicalPanelOptions);

      Published
         // Published properties

         Property UseGradientForTraces: Boolean Read FUseGradientForTraces Write SetUseGradientForTraces Default True;

         Property ShowAxes: Boolean Read FShowAxes Write SetShowAxes Default True;
         Property ShowGridlines: Boolean Read FShowGridlines Write SetShowGridlines Default True;
         Property ShowLabels: Boolean Read FShowLabels Write SetShowLabels Default True;

         Property BackgroundColor: TColor Read FBackgroundColor Write SetBackgroundColor;
         Property AxesColor: TColor Read FAxesColor Write SetAxesColor;
         Property GridlinesColor: TColor Read FGridlinesColor Write SetGridlinesColor;
   End;
   TComputationOptions = Class
      Protected
         // Private representation of published properties

         FGeneralOptions: TComputationGeneralOptions;
         FIntegrationOptions: TComputationIntegrationOptions;
         FGraphicalPanelOptions: TComputationGraphicalPanelOptions;

      Public
         // Constructor & Destructor

         Constructor Create;
         Destructor Destroy; Override;

         // User's methods

         Procedure Copy(Const aComputationOptions: TComputationOptions);

      Published
         // Published properties

         Property GeneralOptions: TComputationGeneralOptions Read FGeneralOptions;
         Property IntegrationOptions: TComputationIntegrationOptions Read FIntegrationOptions;
         Property GraphicalPanelOptions: TComputationGraphicalPanelOptions Read FGraphicalPanelOptions;
   End;
   TFloatingControl = Class
      Protected
         // Private representation of published properties

         FControl: TControl;

         FTop: Integer;
         FLeft: Integer;
         FWidth: Integer;
         FHeight: Integer;

      Public
         // Constructor & Destructor

         Constructor Create(Const aControl: TControl;
                            Const aTop: Integer = 0; Const aLeft: Integer = 0; Const aWidth: Integer = 0; Const aHeight: Integer = 0);

      Published
         // Published properties

         Property Control: TControl Read FControl;

         Property Top: Integer Read FTop;
         Property Left: Integer Read FLeft;
         Property Width: Integer Read FWidth;
         Property Height: Integer Read FHeight;
   End;
   TFloatingControlList = Class(DArray)
      Private
         // Methods used for internal purposes

         Function Compare(Const aObj1, aObj2: DObject): Integer; Inline;

      Public
         // Constructor & Destructor

         Constructor Create; Override;
         Destructor Destroy; Override;

         // User's methods

         Function Pop(Const aControl: TControl): TFloatingControl;
   End;
   TEditorFrameOpenedFile = Class
      Protected
         // Private representation of published properties

         FFileName: String;
         FView: TEditorView;

      Public
         // Constructor & Destructor

         Constructor Create(Const aFileName: String; Const aView: TEditorView);

      Published
         // Published properties

         Property FileName: String Read FFileName;
         Property View: TEditorView Read FView;
   End;

//==============================================================================

Const
   UPDATE_MUST_RESTART_COR = 1;

   COR_NAME = 'COR';

   SOFTWARE_REGISTRY = 'Software';
   COR_REGISTRY      = SOFTWARE_REGISTRY+'\'+COR_NAME;

   COR_URL = 'http://cor.physiol.ox.ac.uk/';

   COR_EMAIL = 'cor@physiol.ox.ac.uk';

   COR_UPDATE_URL  = COR_URL+'Update/';
   COR_UPDATE_INFO = COR_UPDATE_URL+'COR.xml';
   COR_UPDATE_EXE  = COR_UPDATE_URL+COR_NAME+'.exe';


   GUI = 'GUI';
   EDITOR_FRAME = GUI+'\EditorFrame';
   COMPUTATION_FRAME = GUI+'\ComputationFrame';

   OLD = 'Old';

   OPTIONS_GENERAL = 'Options\General';

   OPTIONS_EDITOR = 'Options\Editor';
   OPTIONS_EDITOR_GENERAL = OPTIONS_EDITOR+'\General';
   OPTIONS_EDITOR_DISPLAY = OPTIONS_EDITOR+'\Display';
   OPTIONS_EDITOR_COLOUR = OPTIONS_EDITOR+'\Colour';
   OPTIONS_EDITOR_COMMAND_VIEWER = OPTIONS_EDITOR+'\CommandViewer';

   OPTIONS_COMPUTATION = 'Options\Computation';
   OPTIONS_COMPUTATION_GENERAL = OPTIONS_COMPUTATION+'\General';
   OPTIONS_COMPUTATION_INTEGRATION = OPTIONS_COMPUTATION+'\Integration';
   OPTIONS_COMPUTATION_GRAPHICAL_PANEL = OPTIONS_COMPUTATION+'\GraphicalPanel';

   OPENED_FILES = 'Files\Opened';
   MRU_FILES = 'Files\MRU';

   MAX_NB_OF_MRU_FILES = 8;

   FILTER_SEP = '|';

   CELLML_FILE_DEFEXT = 'cellml';
   CELLML_FILE_FILTER = 'CellML file (*.cellml)|*.cellml';
   CELLML_FILE_EXT = '.'+CELLML_FILE_DEFEXT;

   ANY_FILE_FILTER = 'Any file (*.*)|*.*';

   GRID_SPACING = 8;
   HALF_GRID_SPACING = GRID_SPACING Div 2;
   MIN_FONT_SIZE = 7;
   MAX_FONT_SIZE = 50;

   DEFAULT       = 'Default';
   RESERVED_WORD = 'Reserved word';
   EXTRA_INFO    = 'Extra info';
   ERROR_LINE    = 'Error line';
   SELECTED_TEXT = 'Selected text';
   RIGHT_MARGIN  = 'Right margin';
   GUTTER_AREA   = 'Gutter';
   SCROLL_HINT   = 'Scroll hint';
   LOCKED_FILE   = 'Locked file';

//==============================================================================

Var
   COR_SHORT_VERSION: String;
   COR_VERSION: String;

   COR_COPYRIGHT: String;
   COR_FULL_COPYRIGHT: String;

   INFO_COR_FILENAME: String;
   UPDATED_COR_FILENAME: String;
   UPDATE_COR_FILENAME: String;

   COR_PATH: String;
   COR_MODELS_PATH: String;

   CORComputer: TComputer;

   CORUpdateAvailable: Boolean;

   MainFormMaximised: Boolean;

   MainFormTop: Integer;
   MainFormLeft: Integer;
   MainFormWidth: Integer;
   MainFormHeight: Integer;

   IdHTTP: TIdHTTP;
   ProxyErrorMsg: String;

   GeneralOptions: TGeneralOptions;
   EditorOptions: TEditorOptions;
   ComputationOptions: TComputationOptions;

   CORHelpFileName: String;

{$IFDEF OPT_MATH}
   OptMathDLLHandle: HMODULE;
{$ENDIF}
   ODEDLLHandle: HMODULE;

   SynXMLSyn: TSynXMLSyn;
   SynCellMLSyn: TSynCellMLSyn;

   EditorFrameOpenedFiles: DList;

   EditorFrameSelectedFile: Integer;

   EditorFrameMRUFiles: Array[0..MAX_NB_OF_MRU_FILES] Of String;

   NewCellMLFileID: Integer;
   NewFileID: Integer;

   EditorFrameTopDockSiteSize: Integer;
   EditorFrameLeftDockSiteSize: Integer;
   EditorFrameBottomDockSiteSize: Integer;
   EditorFrameRightDockSiteSize: Integer;

   EditorFrameControlBarFileMenuToolBarVisible: Boolean;
   EditorFrameControlBarEditMenuToolBarVisible: Boolean;
   EditorFrameControlBarRunMenuToolBarVisible: Boolean;

   EditorFrameCommandViewerFrameVisible: Boolean;
   EditorFrameMsgsFrameVisible: Boolean;

   ComputationFrameTopDockSiteSize: Integer;
   ComputationFrameLeftDockSiteSize: Integer;
   ComputationFrameBottomDockSiteSize: Integer;
   ComputationFrameRightDockSiteSize: Integer;

   ComputationFrameControlBarFileMenuToolBarVisible: Boolean;
   ComputationFrameControlBarPropertiesMenuToolBarVisible: Boolean;
   ComputationFrameControlBarRunMenuToolBarVisible: Boolean;
   ComputationFrameControlBarToolsMenuToolBarVisible: Boolean;

   ComputationFramePropertiesFrameVisible: Boolean;
   ComputationFrameConsoleFrameVisible: Boolean;

   FloatingControlList: TFloatingControlList;

   SplashScreenForm: TSplashScreenForm;

   CellMLMessages: DArray;

   Views: DArray;

//==============================================================================

Function CORAlreadyRunning: Boolean;
Procedure SwitchToAlreadyRunningCOR;

Function UpdateCOR: Boolean;

Function IsWinVistaOrAbove: Boolean; Inline;
Function IsWinVistaOrAboveOrAdministrator: Boolean; Inline;

Function ExecProg(Const aProg: String; Const aAsAdmin: Boolean = False; Const aMsg: String = ''): Boolean;
Function ExecProgAndWait(Const aProg: String; Const aAsAdmin: Boolean = False; Const aMsg: String = ''): Boolean;

Function ExtractCellMLFileName(Const aFileName: String): String;

Function CellMLFile(Const aFileName: String): Boolean;

Procedure CORShellExecute(Const aCmd: String); Inline;

Function NewVersion(Const aNewVersion, aOldVersion: String): Boolean;
Function ValidVersion(Const aVersion: String): Boolean;

Procedure CORUpdate(Const aStartup: Boolean = False);

Procedure TrackOldCOR(Const aOldCORFileName: String);
Procedure DeleteOldCOR;

Procedure LoadGeneralOptions;

Procedure UpdateCellMLSynHighlighter(Const aSynCellMLSyn: TSynCellMLSyn; Const aElement: TEditorColElemOptions; Const aLocked: Boolean = False);

Procedure UpdateEditorBackgroundColorAndCORAndCellMLSynHighlighters(Const aSyntaxEdit: TObject);

Procedure EditFieldForIPAddress(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForIPAddressValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForUsername(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForUsernameValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForPassword(Var aEditField: TJvValidateEdit);

Procedure EditFieldForFolderPath(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForFolderPathValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForFloat(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForFloatValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForStrictlyPositiveFloat(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForStrictlyPositiveFloatValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForPositiveFloat(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForPositiveFloatValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForPositiveInteger(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForPositiveIntegerValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForStrictlyPositiveInteger(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForStrictlyPositiveIntegerValidity(Const aEditField: TJvValidateEdit; Const aHighlightEditField: Boolean = True): Boolean;

Procedure EditFieldForRangeInteger(Var aEditField: TJvValidateEdit);
Function CheckEditFieldForRangeIntegerValidity(Const aEditField: TJvValidateEdit; Const aMin, aMax: Integer; Const aHighlightEditField: Boolean = True): Boolean;

Function ExecSaveDialog(Const aFileName: String; Const aDefaultExt: String = ''; Const aFilter: String = ''): Boolean;

Procedure RestoreControl(Const aControl: TControl; Const aVisible: Boolean);

Function OptionsForm(Const aOptionsType: Integer = -1; Const aSubOptionsType: Integer = -1): Boolean;

Procedure MathMLCmdToCmdBinTreeConv(aMathMLCommandBinTree: TMathMLCommandBinTree; Var aCmdBinTree: TCmdBinTree);

Function CanOverwriteFile(Const aFileName: Array Of String): Boolean; Overload;
Function CanOverwriteFile(Const aFileName: String): Boolean; Overload; Inline;

Function CellMLFileToCellMLAPI(Const aFileName: String; Const aCellMLModel: TCellMLModel): Boolean;
Function CellMLAPIToCellMLFile(Const aCellMLModel: TCellMLModel; Const aFileName: String): Boolean;

Function CellMLAPIToCellMLASCII(Const aCellMLModel: TCellMLModel; Const aSyntaxEdit: Pointer): Boolean;

Function CellMLASCIIToCellMLAPI(Const aSyntaxEdit: Pointer; Const aCellMLModel: TCellMLModel): Boolean; Overload;
Function CellMLASCIIToCellMLAPI(Const aCmd: String; Var aMathMLCommandBinTree: TMathMLCommandBinTree): Boolean; Overload;

Procedure UpdateCellMLAPI(Const aCellMLModel: TCellMLModel; Const aVirtualStringTree: TVirtualStringTree);

Function CellMLModelIsValid(Const aCellMLModel: TCellMLModel; Const aReset: Boolean = False): Boolean;

Function FileMD5(Const aFileName: String): String;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ENDIF}
   Forms, IdAuthentication, IdHeaderList, Windows, IdStack, Main, StrUtils,
   Common, JclAppInst, ShellAPI, Password, VersionInfo, Update, Math, Registry,
   ExceptDlgMail, ComCtrls, Options, SyntaxEdit, SynEditHighlighter, Dialogs,
   Engine, Properties, IdAntiFreeze, JclSysInfo, JclSecurity, WCrypt2, XMLIntf,
   XMLDoc, CellMLFileToCellMLAPIEngine, CellMLAPIToCellMLFileEngine,
   CellMLAPIToCellMLASCIIEngine, CellMLASCIIToCellMLAPIEngine,
   ODEForwardEulerIntegrator, ODE2ndOrderRungeKuttaIntegrator,
   ODE4thOrderRungeKuttaIntegrator;

//==============================================================================

Type
   TDummyClass = Class
      Public
         Procedure IdHTTPSelectProxyAuthorization(aSender: TObject; Var aAuthenticationClass: TIdAuthenticationClass; aAuthInfo: TIdHeaderList);
   End;

//==============================================================================

Const
   MD5_BUFFER_SIZE = 32768;

//==============================================================================

Var
   Major, Minor, Release, Build: Integer;
   FileVersionInfoSize: DWord;
   FileVersionInfo: PChar;
   FileInfo: ^VS_FIXEDFILEINFO;
   InfoSize: DWord;
   Dummy: THandle;

   CORAlreadyRunningVal: Integer;

   DummyClass: TDummyClass;

   CORUpdateURL: String;

   CORPath: String;

{$IFDEF OPT_MATH}
   OptMathDLLFileName: String;
{$ENDIF}
   ODEDLLFileName: String;

   MD5Valid: Boolean;
   MD5CryptProv: HCRYPTPROV;
   MD5Buffer: Pointer;

//==============================================================================

Constructor TComputer.Create(Const aBytes: TBytes);
Var
   Len, BytesShift: Integer;
Begin
   If (aBytes = Nil) Then Begin
      // Determine the computer's details

      // Name of the computer

      FName := GetLocalComputerName;

      // Speed of the CPU

      UpdateCPUSpeed;

      // IP address

      FIPAddress := GStack.LocalAddress;

      // Memory information

      UpdateMemory;

      // Version of Windows

      FWindowsVersion := ReplaceStr(GetWindowsVersionString+' '+NTProductTypeString+' '+GetWindowsServicePackVersionString, '  ', ' ');

      // Version of COR

      FCORShortVersion := COR_SHORT_VERSION;
      FCORVersion      := COR_VERSION;
   End Else Begin
      // Retrieve the computer's details from a series of bytes

      BytesShift := 0;

      // Name of the computer

      Len := BytesToInteger(aBytes, BytesShift);

      Inc(BytesShift, SizeOf(Integer));

      FName := BytesToString(aBytes, BytesShift, Len);

      Inc(BytesShift, Len);

      // Speed of the CPU

      FCPUSpeed := BytesToInt64(aBytes, BytesShift);

      Inc(BytesShift, SizeOf(Int64));

      // IP address

      Len := BytesToInteger(aBytes, BytesShift);

      Inc(BytesShift, SizeOf(Integer));

      FIPAddress := BytesToString(aBytes, BytesShift, Len);

      Inc(BytesShift, Len);

      // Memory information

      With FPhysicalMemory Do Begin
         Available := BytesToInt64(aBytes, BytesShift);

         Inc(BytesShift, SizeOf(Int64));

         Total := BytesToInt64(aBytes, BytesShift);

         Inc(BytesShift, SizeOf(Int64));
      End;

      // Version of Windows

      Len := BytesToInteger(aBytes, BytesShift);

      Inc(BytesShift, SizeOf(Integer));

      FWindowsVersion := BytesToString(aBytes, BytesShift, Len);

      Inc(BytesShift, Len);

      // Version of COR

      Len := BytesToInteger(aBytes, BytesShift);

      Inc(BytesShift, SizeOf(Integer));

      FCORVersion := BytesToString(aBytes, BytesShift, Len);

//      Inc(BytesShift, Len);

      // Note: we do NOT care about the short version of COR, since we don't
      //       make use of it...
   End;
End;

//==============================================================================

Class Function TComputer.Compare(Const aObj1, aObj2: DObject): Integer;
Begin
   Result := CompareStr(TComputer(aObj1.VObject).IPAddress,
                        TComputer(aObj2.VObject).IPAddress);
End;

//==============================================================================

Procedure TComputer.UpdateCPUSpeed;
Var
   FreqInfo: TFreqInfo;
Begin
   JclSysInfo.GetCPUSpeed(FreqInfo);

   FCPUSpeed := 1000000*FreqInfo.RawFreq;   // Hz
End;

//==============================================================================

Procedure TComputer.UpdateMemory;
Var
   MemoryStatus: TMemoryStatus;
Begin
   MemoryStatus.dwLength := SizeOf(TMemoryStatus);

   GlobalMemoryStatus(MemoryStatus);

   With FPhysicalMemory, MemoryStatus Do Begin
      Available := dwAvailPhys;   // B
      Total     := dwTotalPhys;   // B
   End;
End;

//==============================================================================

Procedure TComputer.Update;
Begin
   // Update the speed of the CPU

   UpdateCPUSpeed;

   // Update the memory

   UpdateMemory;
End;

//==============================================================================

Function TComputer.AsBytes: TBytes;
Begin
   // Update the computer's details, just in case...

   Update;

   // Name of the computer

   Result := ToBytes(Length(FName));

   AppendBytes(Result, ToBytes(FName));

   // Speed of the CPU

   AppendBytes(Result, ToBytes(FCPUSpeed));

   // IP address

   AppendBytes(Result, ToBytes(Length(FIPAddress)));

   AppendBytes(Result, ToBytes(FIPAddress));

   // Memory information

   With FPhysicalMemory Do Begin
      AppendBytes(Result, ToBytes(Available));
      AppendBytes(Result, ToBytes(Total));
   End;

   // Version of Windows

   AppendBytes(Result, ToBytes(Length(FWindowsVersion)));

   AppendBytes(Result, ToBytes(FWindowsVersion));

   // Version of COR

   AppendBytes(Result, ToBytes(Length(FCORVersion)));

   AppendBytes(Result, ToBytes(FCORVersion));
End;

//==============================================================================

Procedure TGeneralOptions.Copy(Const aGeneralOptions: TGeneralOptions);
Begin
   FCheckForNewVersionAtStartup := aGeneralOptions.CheckForNewVersionAtStartup;

   FConnectThroughProxyServer := aGeneralOptions.ConnectThroughProxyServer;

   FProxyServer := aGeneralOptions.ProxyServer;
   FProxyPort   := aGeneralOptions.ProxyPort;

   FProxyAuthenticationMethod := aGeneralOptions.ProxyAuthenticationMethod;

   FProxyAuthenticationUsername := aGeneralOptions.ProxyAuthenticationUsername;
   FProxyAuthenticationPassword := aGeneralOptions.ProxyAuthenticationPassword;

   FProxyAuthenticationAskForPassword := aGeneralOptions.ProxyAuthenticationAskForPassword;
End;

//==============================================================================

Procedure TGeneralOptions.SetCheckForNewVersionAtStartup(Const aValue: Boolean);
Begin
   If (aValue <> FCheckForNewVersionAtStartup) Then
      FCheckForNewVersionAtStartup := aValue;
End;

//==============================================================================

Procedure TGeneralOptions.SetConnectThroughProxyServer(Const aValue: Boolean);
Begin
   If (aValue <> FConnectThroughProxyServer) Then
      FConnectThroughProxyServer := aValue;
End;

//==============================================================================

Procedure TGeneralOptions.SetProxyServer(Const aValue: String);
Begin
   If (CompareStr(aValue, FProxyServer) <> 0) Then
      FProxyServer := aValue;
End;

//==============================================================================

Procedure TGeneralOptions.SetProxyPort(Const aValue: Integer);
Begin
   If ((aValue <> FProxyPort) And (aValue >= 0) And (aValue <= 65535)) Then
      FProxyPort := aValue;
End;

//==============================================================================

Procedure TGeneralOptions.SetProxyAuthenticationMethod(Const aValue: String);
Begin
   If (CompareStr(aValue, FProxyAuthenticationMethod) <> 0) Then
      FProxyAuthenticationMethod := aValue;
End;

//==============================================================================

Procedure TGeneralOptions.SetProxyAuthenticationUsername(Const aValue: String);
Begin
   If (CompareStr(aValue, FProxyAuthenticationUsername) <> 0) Then
      FProxyAuthenticationUsername := aValue;
End;

//==============================================================================

Procedure TGeneralOptions.SetProxyAuthenticationPassword(Const aValue: String);
Begin
   If (CompareStr(aValue, FProxyAuthenticationPassword) <> 0) Then
      FProxyAuthenticationPassword := aValue;
End;

//==============================================================================

Procedure TGeneralOptions.SetProxyAuthenticationAskForPassword(Const aValue: Boolean);
Begin
   If (aValue <> FProxyAuthenticationAskForPassword) Then
      FProxyAuthenticationAskForPassword := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.Copy(Const aEditorGeneralOptions: TEditorGeneralOptions);
Begin
   FInsertMode         := aEditorGeneralOptions.InsertMode;
   FGroupUndo          := aEditorGeneralOptions.GroupUndo;
   FScrollPastEOF      := aEditorGeneralOptions.ScrollPastEOF;
   FAutoIndentMode     := aEditorGeneralOptions.AutoIndentMode;
   FSmartTabs          := aEditorGeneralOptions.SmartTabs;
   FBackspaceUnindents := aEditorGeneralOptions.BackspaceUnindents;
   FShowScrollHint     := aEditorGeneralOptions.ShowScrollHint;
   FWordWrap           := aEditorGeneralOptions.WordWrap;

   FView := aEditorGeneralOptions.View;

   FInsertCaret    := aEditorGeneralOptions.InsertCaret;
   FOverwriteCaret := aEditorGeneralOptions.OverwriteCaret;

   FTabIndent := aEditorGeneralOptions.TabIndent;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetInsertMode(Const aValue: Boolean);
Begin
   If (aValue <> FInsertMode) Then
      FInsertMode := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetGroupUndo(Const aValue: Boolean);
Begin
   If (aValue <> FGroupUndo) Then
      FGroupUndo := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetScrollPastEOF(Const aValue: Boolean);
Begin
   If (aValue <> FScrollPastEOF) Then
      FScrollPastEOF := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetAutoIndentMode(Const aValue: Boolean);
Begin
   If (aValue <> FAutoIndentMode) Then
      FAutoIndentMode := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetSmartTabs(Const aValue: Boolean);
Begin
   If (aValue <> FSmartTabs) Then
      FSmartTabs := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetBackspaceUnindents(Const aValue: Boolean);
Begin
   If (aValue <> FBackspaceUnindents) Then
      FBackspaceUnindents := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetShowScrollHint(Const aValue: Boolean);
Begin
   If (aValue <> FShowScrollHint) Then
      FShowScrollHint := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetWordWrap(Const aValue: Boolean);
Begin
   If (aValue <> FWordWrap) Then
      FWordWrap := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetView(Const aValue: TEditorView);
Begin
   If (aValue <> FView) Then
      FView := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetInsertCaret(Const aValue: TSynEditCaretType);
Begin
   If (aValue <> FInsertCaret) Then
      FInsertCaret := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetOverwriteCaret(Const aValue: TSynEditCaretType);
Begin
   If (aValue <> FOverwriteCaret) Then
      FOverwriteCaret := aValue;
End;

//==============================================================================

Procedure TEditorGeneralOptions.SetTabIndent(Const aValue: Integer);
Begin
   If (aValue <> FTabIndent) Then
      FTabIndent := aValue;
End;

//==============================================================================

Procedure TEditorDisplayOptions.Copy(Const aEditorDisplayOptions: TEditorDisplayOptions);
Begin
   FShowRightMargin := aEditorDisplayOptions.ShowRightMargin;
   FRightMargin     := aEditorDisplayOptions.RightMargin;

   FShowGutter  := aEditorDisplayOptions.ShowGutter;
   FGutterWidth := aEditorDisplayOptions.GutterWidth;

   FFontName := aEditorDisplayOptions.FontName;
   FFontSize := aEditorDisplayOptions.FontSize;
End;

//==============================================================================

Procedure TEditorDisplayOptions.SetShowRightMargin(Const aValue: Boolean);
Begin
   If (aValue <> FShowRightMargin) Then
      FShowRightMargin := aValue;
End;

//==============================================================================

Procedure TEditorDisplayOptions.SetRightMargin(Const aValue: Integer);
Begin
   If (aValue <> FRightMargin) Then
      FRightMargin := aValue;
End;

//==============================================================================

Procedure TEditorDisplayOptions.SetShowGutter(Const aValue: Boolean);
Begin
   If (aValue <> FShowGutter) Then
      FShowGutter := aValue;
End;

//==============================================================================

Procedure TEditorDisplayOptions.SetGutterWidth(Const aValue: Integer);
Begin
   If (aValue <> FGutterWidth) Then
      FGutterWidth := aValue;
End;

//==============================================================================

Procedure TEditorDisplayOptions.SetFontName(Const aValue: String);
Begin
   If (CompareStr(aValue, FFontName) <> 0) Then
      FFontName := aValue;
End;

//==============================================================================

Procedure TEditorDisplayOptions.SetFontSize(Const aValue: Integer);
Begin
   If (aValue <> FFontSize) Then
      FFontSize := aValue;
End;

//==============================================================================

Constructor TEditorColElemOptions.Create(Const aName: String;
                                         Const aBackgroundColor, aForegroundColor: TColor;
                                         Const aFontStyle: TFontStyles;
                                         Const aAlpha: Byte);
Begin
   FName := aName;

   FBackgroundColor := aBackgroundColor;
   FForegroundColor := aForegroundColor;

   FFontStyle := aFontStyle;

   FAlpha := aAlpha;
End;

//==============================================================================

Procedure TEditorColElemOptions.Copy(Const aEditorColElemOptions: TEditorColElemOptions);
Begin
   FBackgroundColor := aEditorColElemOptions.BackgroundColor;
   FForegroundColor := aEditorColElemOptions.ForegroundColor;

   FFontStyle := aEditorColElemOptions.FontStyle;

   FAlpha := aEditorColElemOptions.Alpha;
End;

//==============================================================================

Procedure TEditorColElemOptions.SetName(Const aValue: String);
Begin
   If (CompareStr(aValue, FName) <> 0) Then
      FName := aValue;
End;

//==============================================================================

Procedure TEditorColElemOptions.SetBackgroundColor(Const aValue: TColor);
Begin
   If (aValue <> FBackgroundColor) Then
      FBackgroundColor := aValue;
End;

//==============================================================================

Procedure TEditorColElemOptions.SetForegroundColor(Const aValue: TColor);
Begin
   If (aValue <> FForegroundColor) Then
      FForegroundColor := aValue;
End;

//==============================================================================

Procedure TEditorColElemOptions.SetFontStyle(Const aValue: TFontStyles);
Begin
   If (aValue <> FFontStyle) Then
      FFontStyle := aValue;
End;

//==============================================================================

Procedure TEditorColElemOptions.SetAlpha(Const aValue: Byte);
Begin
   If ((aValue <> FAlpha) And (aValue <= 100)) Then
      FAlpha := aValue;
End;

//==============================================================================

Function TEditorColElemOptionsList.Compare(Const aObj1, aObj2: DObject): Integer;
Begin
   Result := CompareStr(TEditorColElemOptions(aObj1.VObject).Name,
                        TEditorColElemOptions(aObj2.VObject).Name);
End;

//==============================================================================

Constructor TEditorColElemOptionsList.Create;
Begin
   Inherited CreateWith(Compare);

   Add([TEditorColElemOptions.Create(DEFAULT, clWindow, clWindowText, [])]);
   Add([TEditorColElemOptions.Create(RESERVED_WORD, clWindow, clWindowText, [fsBold])]);
   Add([TEditorColElemOptions.Create(EXTRA_INFO, clWindow, clNavy, [fsItalic])]);
   Add([TEditorColElemOptions.Create(ERROR_LINE, clMaroon, clWhite, [])]);
   Add([TEditorColElemOptions.Create(SELECTED_TEXT, clHighlight, clHighlightText)]);
   Add([TEditorColElemOptions.Create(RIGHT_MARGIN, clWindow, clRed)]);
   Add([TEditorColElemOptions.Create(GUTTER_AREA, clBtnFace, clWindowText)]);
   Add([TEditorColElemOptions.Create(SCROLL_HINT, clInfoBk, clWindowText)]);
   Add([TEditorColElemOptions.Create(LOCKED_FILE, clRed, clWindowText, [], 5)]);

   TrimToSize;

   Sort(Self);
End;

//==============================================================================

Destructor TEditorColElemOptionsList.Destroy;
Begin
   ObjFree(Self);

   Inherited;
End;

//==============================================================================

Function TEditorColElemOptionsList.Get(Const aName: String): TEditorColElemOptions;
Var
   ElementOptions: TEditorColElemOptions;
Begin
   // Note: we assume that our calls to "Get" are ALWAYS valid... and therefore
   //       ALWAYS have a valid "aName" and ALWAYS return valid options for an
   //       element... This should be the case, since we use "Get" in some very
   //       specific occasions...

   ElementOptions := TEditorColElemOptions.Create(aName);

   Result := TEditorColElemOptions(DeCAL.GetObject(BinarySearch(Self, [ElementOptions])));

   ElementOptions.Free;
End;

//==============================================================================

Constructor TEditorColourOptions.Create;
Begin
   FElements := TEditorColElemOptionsList.Create;
End;

//==============================================================================

Destructor TEditorColourOptions.Destroy;
Begin
   FElements.Free;
End;

//==============================================================================

Procedure TEditorColourOptions.Copy(Const aEditorColourOptions: TEditorColourOptions);
Var
   Iter: Integer;
Begin
   For Iter := 0 To FElements.Size-1 Do
      With TEditorColElemOptions(FElements.At(Iter).VObject) Do
         Copy(aEditorColourOptions.Elements.Get(Name));
End;

//==============================================================================

Constructor TEditorOptions.Create;
Begin
   FGeneralOptions       := TEditorGeneralOptions.Create;
   FDisplayOptions       := TEditorDisplayOptions.Create;
   FColourOptions        := TEditorColourOptions.Create;
   FCommandViewerOptions := TCmdGraphOptions.Create;
End;

//==============================================================================

Destructor TEditorOptions.Destroy;
Begin
   FGeneralOptions.Free;
   FDisplayOptions.Free;
   FColourOptions.Free;
   FCommandViewerOptions.Free;
End;

//==============================================================================

Procedure TEditorOptions.Copy(Const aEditorOptions: TEditorOptions);
Begin
   FGeneralOptions.Copy(aEditorOptions.GeneralOptions);
   FDisplayOptions.Copy(aEditorOptions.DisplayOptions);
   FColourOptions.Copy(aEditorOptions.ColourOptions);
   FCommandViewerOptions.Copy(aEditorOptions.CommandViewerOptions);
End;

//==============================================================================

Procedure TComputationGeneralOptions.Copy(Const aComputationGeneralOptions: TComputationGeneralOptions);
Begin
   FDuration := aComputationGeneralOptions.Duration;
   FOutput   := aComputationGeneralOptions.Output;

   FDebugMode := aComputationGeneralOptions.DebugMode;

   FCompiler         := aComputationGeneralOptions.Compiler;
   FCompilerLocation := aComputationGeneralOptions.CompilerLocation;
End;

//==============================================================================

Procedure TComputationGeneralOptions.SetDuration(Const aValue: Double);
Begin
   If ((aValue <> FDuration) And (aValue > 0)) Then
      FDuration := aValue;
End;

//==============================================================================

Procedure TComputationGeneralOptions.SetOutput(Const aValue: Double);
Begin
   If ((aValue <> FOutput) And (aValue > 0)) Then
      FOutput := aValue;
End;

//==============================================================================

Procedure TComputationGeneralOptions.SetDebugMode(Const aValue: Boolean);
Begin
   If (aValue <> FDebugMode) Then
      FDebugMode := aValue;
End;

//==============================================================================

Procedure TComputationGeneralOptions.SetCompiler(Const aValue: TCellMLModelRuntimeCompiler);
Begin
   If (aValue <> FCompiler) Then
      FCompiler := aValue;
End;

//==============================================================================

Procedure TComputationGeneralOptions.SetCompilerLocation(Const aValue: String);
Begin
   If (CompareStr(aValue, FCompilerLocation) <> 0) Then
      FCompilerLocation := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.Copy(Const aComputationIntegrationOptions: TComputationIntegrationOptions);
Begin
   FIntegrator := aComputationIntegrationOptions.Integrator;

   FTimeStep := aComputationIntegrationOptions.TimeStep;

   FMaxTimeStep := aComputationIntegrationOptions.MaxTimeStep;

   FMaxNbOfSteps := aComputationIntegrationOptions.MaxNbOfSteps;

   FMethod := aComputationIntegrationOptions.Method;

   FIterator           := aComputationIntegrationOptions.Iterator;
   FLinearSolver       := aComputationIntegrationOptions.LinearSolver;
   FPreconditioner     := aComputationIntegrationOptions.Preconditioner;
   FUpperHalfBandwidth := aComputationIntegrationOptions.UpperHalfBandwidth;
   FLowerHalfBandwidth := aComputationIntegrationOptions.LowerHalfBandwidth;

   FRelTol := aComputationIntegrationOptions.RelTol;
   FAbsTol := aComputationIntegrationOptions.AbsTol;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetIntegrator(Const aValue: TODEIntegratorType);
Begin
   If (aValue <> FIntegrator) Then
      FIntegrator := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetTimeStep(Const aValue: Double);
Begin
   If ((aValue <> FTimeStep) And (aValue > 0)) Then
      FTimeStep := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetMaxTimeStep(Const aValue: Double);
Begin
   If ((aValue <> FMaxTimeStep) And (aValue >= 0)) Then
      FMaxTimeStep := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetMaxNbOfSteps(Const aValue: Integer);
Begin
   If ((aValue <> FMaxNbOfSteps) And (aValue >= 1)) Then
      FMaxNbOfSteps := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetMethod(Const aValue: TODECVODEIntegratorMethod);
Begin
   If (aValue <> FMethod) Then
      FMethod := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetIterator(Const aValue: TODECVODEIntegratorIterator);
Begin
   If (aValue <> FIterator) Then
      FIterator := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetLinearSolver(Const aValue: TODECVODEIntegratorLinearSolver);
Begin
   If (aValue <> FLinearSolver) Then
      FLinearSolver := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetPreconditioner(Const aValue: TODECVODEIntegratorPreconditioner);
Begin
   If (aValue <> FPreconditioner) Then
      FPreconditioner := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetUpperHalfBandwidth(Const aValue: Integer);
Begin
   If (aValue <> FUpperHalfBandwidth) Then
      FUpperHalfBandwidth := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetLowerHalfBandwidth(Const aValue: Integer);
Begin
   If (aValue <> FLowerHalfBandwidth) Then
      FLowerHalfBandwidth := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetRelTol(Const aValue: Double);
Begin
   If ((aValue <> FRelTol) And (aValue > 0)) Then
      FRelTol := aValue;
End;

//==============================================================================

Procedure TComputationIntegrationOptions.SetAbsTol(Const aValue: Double);
Begin
   If ((aValue <> FAbsTol) And (aValue > 0)) Then
      FAbsTol := aValue;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.Copy(Const aComputationGraphicalPanelOptions: TComputationGraphicalPanelOptions);
Begin
   FUseGradientForTraces := aComputationGraphicalPanelOptions.UseGradientForTraces;

   FShowAxes      := aComputationGraphicalPanelOptions.ShowAxes;
   FShowGridlines := aComputationGraphicalPanelOptions.ShowGridlines;
   FShowLabels    := aComputationGraphicalPanelOptions.ShowLabels;

   FBackgroundColor := aComputationGraphicalPanelOptions.BackgroundColor;
   FAxesColor       := aComputationGraphicalPanelOptions.AxesColor;
   FGridlinesColor  := aComputationGraphicalPanelOptions.GridlinesColor;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.SetUseGradientForTraces(Const aValue: Boolean);
Begin
   If (aValue <> FUseGradientForTraces) Then
      FUseGradientForTraces := aValue;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.SetShowAxes(Const aValue: Boolean);
Begin
   If (aValue <> FShowAxes) Then
      FShowAxes := aValue;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.SetShowGridlines(Const aValue: Boolean);
Begin
   If (aValue <> FShowGridlines) Then
      FShowGridlines := aValue;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.SetShowLabels(Const aValue: Boolean);
Begin
   If (aValue <> FShowLabels) Then
      FShowLabels := aValue;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.SetBackgroundColor(Const aValue: TColor);
Begin
   If (aValue <> FBackgroundColor) Then
      FBackgroundColor := aValue;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.SetAxesColor(Const aValue: TColor);
Begin
   If (aValue <> FAxesColor) Then
      FAxesColor := aValue;
End;

//==============================================================================

Procedure TComputationGraphicalPanelOptions.SetGridlinesColor(Const aValue: TColor);
Begin
   If (aValue <> FGridlinesColor) Then
      FGridlinesColor := aValue;
End;

//==============================================================================

Constructor TComputationOptions.Create;
Begin
   FGeneralOptions         := TComputationGeneralOptions.Create;
   FIntegrationOptions     := TComputationIntegrationOptions.Create;
   FGraphicalPanelOptions := TComputationGraphicalPanelOptions.Create;
End;

//==============================================================================

Destructor TComputationOptions.Destroy;
Begin
   FGeneralOptions.Free;
   FIntegrationOptions.Free;
   FGraphicalPanelOptions.Free;
End;

//==============================================================================

Procedure TComputationOptions.Copy(Const aComputationOptions: TComputationOptions);
Begin
   FGeneralOptions.Copy(aComputationOptions.GeneralOptions);
   FIntegrationOptions.Copy(aComputationOptions.IntegrationOptions);
   FGraphicalPanelOptions.Copy(aComputationOptions.GraphicalPanelOptions);
End;

//==============================================================================

Constructor TFloatingControl.Create(Const aControl: TControl;
                                    Const aTop, aLeft, aWidth, aHeight: Integer);
Begin
   FControl := aControl;

   FTop    := aTop;
   FLeft   := aLeft;
   FWidth  := aWidth;
   FHeight := aHeight;
End;

//==============================================================================

Function TFloatingControlList.Compare(Const aObj1, aObj2: DObject): Integer;
Var
   Obj1, Obj2: TFloatingControl;
Begin
   Obj1 := TFloatingControl(aObj1.VObject);
   Obj2 := TFloatingControl(aObj2.VObject);

   Result := CompareStr(Obj1.Control.Owner.Name+'_'+Obj1.Control.Name,
                        Obj2.Control.Owner.Name+'_'+Obj2.Control.Name);
End;

//==============================================================================

Constructor TFloatingControlList.Create;
Begin
   Inherited CreateWith(Compare);
End;

//==============================================================================

Destructor TFloatingControlList.Destroy;
Begin
   ObjFree(Self);

   Inherited;
End;

//==============================================================================

Function TFloatingControlList.Pop(Const aControl: TControl): TFloatingControl;
Var
   FloatingControl: TFloatingControl;
   FloatingControlIter: DIterator;
Begin
   FloatingControl := TFloatingControl.Create(aControl);

   FloatingControlIter := BinarySearch(Self, [FloatingControl]);

   If (Not AtEnd(FloatingControlIter)) Then Begin
      Result := TFloatingControl(DeCAL.GetObject(FloatingControlIter));

      If (Result <> Nil) Then
         Remove([Result]);
   End Else
      Result := Nil;

   FloatingControl.Free;
End;

//==============================================================================

Constructor TEditorFrameOpenedFile.Create(Const aFileName: String;
                                          Const aView: TEditorView);
Begin
   FFileName := aFileName;
   FView     := aView;
End;

//==============================================================================

Procedure TDummyClass.IdHTTPSelectProxyAuthorization(aSender: TObject;
                                                     Var aAuthenticationClass: TIdAuthenticationClass;
                                                     aAuthInfo: TIdHeaderList);
Var
   AuthMethod: String;
Begin
   If (aAuthenticationClass = Nil) Then Begin
      // There doesn't exist any authentication method that we can handle, so...

      AuthMethod := Copy(aAuthInfo.Text, 1, Pos(' ', aAuthInfo.Text)-1);

      If (CompareStr(AuthMethod, '') = 0) Then Begin
         // Couldn't extract the authentication method information, so get the
         // whole thing...

         AuthMethod := aAuthInfo.Text;

         // Remove any trailing CR or LF

         While (AuthMethod[Length(AuthMethod)] = CR) Or
               (AuthMethod[Length(AuthMethod)] = LF) Do
            AuthMethod := Copy(AuthMethod, 1, Length(AuthMethod)-1);
      End;

      ProxyErrorMsg := 'Your proxy server uses an authentication method ('+AuthMethod+') that cannot be handled by '+COR_NAME+'.';
   End;
End;

//==============================================================================

Function CORAlreadyRunning: Boolean;
Const
   COR_NOT_ALREADY_RUNNING = 0;
   COR_ALREADY_RUNNING     = 1;
Begin
   If (CORAlreadyRunningVal = UNDEFINED) Then Begin
      If (Not JcLAppInstances('COR - Cellular Open Resource').CheckInstance(1)) Then
         // Note: the parameter MUST be unique! It ensures that only ONE instance
         // of COR can be run, no matter how many copies of COR exist on one's
         // hard drive. Indeed, a COR developer might have the "official"
         // version of COR installed, as well as have his/her "development"
         // version. In that case, should there be no parameter to the call to
         // "JcLAppInstances", then calling these two "different" versions of
         // COR would result in two instances of the program. This problem can
         // be avoided by having a unique parameter as is the case here...

         CORAlreadyRunningVal := COR_ALREADY_RUNNING
      Else
         CORAlreadyRunningVal := COR_NOT_ALREADY_RUNNING;
   End;

   Result := CORAlreadyRunningVal = COR_ALREADY_RUNNING;
End;

//==============================================================================

Procedure SwitchToAlreadyRunningCOR;
Begin
   With JcLAppInstances Do Begin
      // Bring up the instance of COR

      SwitchTo(0);

      // Send the list of arguments to the instance of COR, if any...

      If (ParamCount > 0) Then
         SendCmdLineParams('TMainForm', 0);

      // Kill ourself

      KillInstance;
   End;
End;

//==============================================================================

Function UpdateCOR: Boolean;
Begin
   Result := ExecProg(UPDATE_COR_FILENAME, Not DirectoryIsWritable(COR_PATH), COR_NAME+' is now going to run a script that requires your permission in order for '+COR_NAME+' to update itself.');

   If (Not Result) Then Begin
      // The user cancelled the update or didn't provide the right
      // administrative privileges so delete the update file...

      DeleteFile(PChar(UPDATE_COR_FILENAME));

      CORUpdateAvailable := False;   // Not available anymore indeed...
   End;
End;

//==============================================================================

Function IsWinVistaOrAbove: Boolean;
Begin
   Result := IsWinVista Or IsWinServer2008 Or IsWin7 Or IsWinServer2008R2;
End;

//==============================================================================

Function IsWinVistaOrAboveOrAdministrator: Boolean;
Begin
   Result := IsWinVistaOrAbove Or IsAdministrator;
End;

//==============================================================================

Function ExecProgWithOrWithoutWait(Const aProg: String; Const aAsAdmin: Boolean;
                                   Const aMsg: String;
                                   Const aWait: Boolean): Boolean;
Var
   ShellExecuteInfo: TShellExecuteInfo;
   Proc: DWORD;
   NeedConsent: Boolean;
Begin
   // Start from a clean "ShellExecuteInfo" record

   FillChar(ShellExecuteInfo, SizeOf(ShellExecuteInfo), 0);

   // Populate the record with the information that will allow us to run the
   // program as an admin

   With ShellExecuteInfo Do Begin
      cbSize := SizeOf(ShellExecuteInfo);
      fMask  := SEE_MASK_FLAG_DDEWAIT Or SEE_MASK_FLAG_NO_UI;

      If (aWait) Then
         fMask := fMask Or SEE_MASK_NOCLOSEPROCESS;

      If (aAsAdmin And Not IsAdministrator) Then
         lpVerb := 'RunAs';

      lpFile := PChar(aProg);
      nShow  := SW_HIDE;
   End;

   // Provide the user with a message, if necessary and required

   If (IsWinVistaOrAbove) Then Begin
      With TRegistry.Create Do Begin
         RootKey := HKEY_LOCAL_MACHINE;

         If (OpenKeyReadOnly('SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System')) Then
            NeedConsent := ReadInteger('ConsentPromptBehaviorAdmin') <> 0
         Else
            // Cannot open the key, so better be safe than sorry...
            
            NeedConsent := True;

         Free;
      End;
   End Else
      NeedConsent := False;

   If (aAsAdmin And Not IsAdministrator And (aMsg <> '') And NeedConsent) Then
      MessageDlg(aMsg, mtInformation, [mbOK], 0);

   // Execute the program itself

   Result := ShellExecuteEx(@ShellExecuteInfo);

   // Wait, if required

   If (Result And aWait) Then Begin
      Proc := ShellExecuteInfo.hProcess;

      While WaitForSingleObject(ShellExecuteInfo.hProcess, 10) <> WAIT_OBJECT_0 Do
         Application.ProcessMessages;

      CloseHandle(Proc);
   End;
End;

//==============================================================================

Function ExecProg(Const aProg: String; Const aAsAdmin: Boolean;
                  Const aMsg: String): Boolean;
Begin
   Result := ExecProgWithOrWithoutWait(aProg, aAsAdmin, aMsg, False);
End;

//==============================================================================

Function ExecProgAndWait(Const aProg: String; Const aAsAdmin: Boolean;
                         Const aMsg: String): Boolean;
Begin
   Result := ExecProgWithOrWithoutWait(aProg, aAsAdmin, aMsg, True);
End;

//==============================================================================

Function ExtractCellMLFileName(Const aFileName: String): String;
Begin
   Result := ExtractFileName(aFileName);

   If (CellMLFile(Result)) Then
      Result := Copy(Result, 1, Length(Result)-Length(ExtractFileExt(aFileName)));
End;

//==============================================================================

Function CellMLFile(Const aFileName: String): Boolean;
Begin
   Result := CompareStr(LowerCase(ExtractFileExt(aFileName)), CELLML_FILE_EXT) = 0;
End;

//==============================================================================

Procedure CORShellExecute(Const aCmd: String);
Begin
   ShellExecute(Application.Handle, Nil, PChar(aCmd), Nil, Nil, SW_SHOWNORMAL);
End;

//==============================================================================

Function ConvertVersion(Const aVersion: String;
                        Var aMajor, aMinor, aRelease, aBuild: Integer): Boolean;
   Function VersionVal(Var aVersion: String; Var aVersionPart: Integer): Boolean;
   Var
      DotPos: Integer;
   Begin
      If (CompareStr(aVersion, '') = 0) Then
         Result := False
      Else Begin
         DotPos := Pos('.', aVersion);

         If (DotPos <> 0) Then Begin
            Result := TryStrToInt(Copy(aVersion, 1, DotPos-1), aVersionPart);

            If (Result) Then
               aVersion := Copy(aVersion, DotPos+1, Length(aVersion)-DotPos);
         End Else Begin
            Result := TryStrToInt(aVersion, aVersionPart);

            If (Result) Then
               aVersion := '';
         End;
      End;
   End;
Var
   Version: String;
Begin
   Version := aVersion;

   Result := VersionVal(Version, aMajor)   And
             VersionVal(Version, aMinor)   And
             VersionVal(Version, aRelease) And
             VersionVal(Version, aBuild);
End;

//==============================================================================

Function NewVersion(Const aNewVersion, aOldVersion: String): Boolean;
Var
   NewMajor, NewMinor, NewRelease, NewBuild: Integer;
   OldMajor, OldMinor, OldRelease, OldBuild: Integer;
Begin
   ConvertVersion(aNewVersion, NewMajor, NewMinor, NewRelease, NewBuild);
   ConvertVersion(aOldVersion, OldMajor, OldMinor, OldRelease, OldBuild);

   Result := False;

   If (OldMajor < NewMajor) Then
      Result := True
   Else If (OldMajor = NewMajor) Then Begin
      If (OldMinor < NewMinor) Then
         Result := True
      Else If (OldMinor = NewMinor) Then Begin
         If (OldRelease < NewRelease) Then
            Result := True
         Else If (OldRelease = NewRelease) Then
            Result := OldBuild < NewBuild;
      End;
   End;
End;

//==============================================================================

Function ValidVersion(Const aVersion: String): Boolean;
Var
   Major, Minor, Release, Build: Integer;
Begin
   Result := ConvertVersion(aVersion, Major, Minor, Release, Build);
End;

//==============================================================================

Procedure CORUpdate(Const aStartup: Boolean);
   Procedure ShowProxyErrorMsg;
   Begin
      If (IdHTTP.ResponseCode = -1) Then
         MessageDlg('You must be connected to the Internet when trying to update '+COR_NAME+'.', mtWarning, [mbOK], 0)
      Else If (CompareStr(ProxyErrorMsg, '') <> 0) Then
         MessageDlg(ProxyErrorMsg+' Please check your Internet settings.', mtWarning, [mbOK], 0)
      Else
         MessageDlg('A problem occurred when trying to update '+COR_NAME+': '+IdHTTP.ResponseText, mtWarning, [mbOK], 0);
   End;
Var
   InfoFile: TFileStream;
   I, J: Integer;
   VersionInfo: TStrings;
   FileSize: Integer;
   NeedShowProxyErrorMsg: Boolean;
   XMLDocument: IXMLDocument;
Begin
   // Set up the Internet connection

   IdHTTP.Request.Clear;
   IdHTTP.ProxyParams.Clear;

   IdHTTP.Response.Clear;

   If (GeneralOptions.ConnectThroughProxyServer) Then
      With IdHTTP.ProxyParams Do Begin
         ProxyServer := GeneralOptions.ProxyServer;
         ProxyPort   := GeneralOptions.ProxyPort;

         BasicAuthentication := CompareStr(GeneralOptions.ProxyAuthenticationMethod, 'Basic') = 0;

         If (BasicAuthentication) Then Begin
            ProxyUsername := GeneralOptions.ProxyAuthenticationUsername;

            If (GeneralOptions.ProxyAuthenticationAskForPassword) Then
               With TPasswordForm.Create(Application.MainForm, 'Proxy server &password') Do Begin
                  ShowModal;

                  If (ModalResult = mrOk) Then
                     ProxyPassword := PasswordVal.Text;

                  Free;
               End
            Else
               ProxyPassword := Decrypt(GeneralOptions.ProxyAuthenticationPassword);
         End;
      End;

   // Check for an update

   InfoFile := TFileStream.Create(INFO_COR_FILENAME, fmCreate);

   ProxyErrorMsg := '';

   Try
      // Get the XML file that contains the versions information for COR

      IdHTTP.Get(COR_UPDATE_INFO, InfoFile);

      IdHTTP.Disconnect;

      InfoFile.Free;
   Except
      // Cannot get the version info for COR from the COR web site

      If (Not aStartup) Then Begin
         NeedShowProxyErrorMsg := True;

         If (IdHTTP.ResponseCode = -1) Then
            // Is it really that the user is not connected to the Internet or
            // that the COR website is down? To find out, we check the BBC
            // website, since it always ought to be up

            Try
               IdHTTP.Get('http://www.bbc.co.uk/', InfoFile);

               MessageDlg('Sorry, but the '+COR_NAME+' website appears to be down. Please try again later.', mtWarning, [mbOK], 0);

               NeedShowProxyErrorMsg := False;
            Except
               // It still fails, so if anything the user is not connected to
               // the Internet
            End;

         If (NeedShowProxyErrorMsg) Then Begin
            If (GeneralOptions.ConnectThroughProxyServer) Then
               Case IdHTTP.ResponseCode Of
                  200:
                     ProxyErrorMsg := 'Your proxy server cannot handle '+GeneralOptions.ProxyAuthenticationMethod+' authentication.';
                  407:
                     ProxyErrorMsg := 'Your proxy server requires explicit authentication.';
               End;

            ShowProxyErrorMsg;
         End;
      End;

      InfoFile.Free;

      DeleteFile(PChar(INFO_COR_FILENAME));
   End;

   // Extract the various versions information

   If (FileExists(INFO_COR_FILENAME)) Then Begin
      XMLDocument := TXMLDocument.Create(Nil);
      VersionInfo := TStringList.Create;

      Try
         XMLDocument.FileName := INFO_COR_FILENAME;

         XMLDocument.Active := True;

         Try
            If ((XMLDocument.Node.ChildNodes.Count = 1) And
                (CompareStr(XMLDocument.Node.ChildNodes[0].NodeName, 'COR') = 0)) Then
               For I := 0 To XMLDocument.Node.ChildNodes[0].ChildNodes.Count-1 Do
                  If (CompareStr(XMLDocument.Node.ChildNodes[0].ChildNodes[I].NodeName, 'Version') = 0) Then
                     With XMLDocument.Node.ChildNodes[0].ChildNodes[I] Do Begin
                        VersionInfo.Add(AttributeNodes.Nodes['Value'].Text);

                        For J := 0 To ChildNodes.Count-1 Do
                           With ChildNodes[J] Do
                              If (CompareStr(NodeName, 'Description') = 0) Then
                                 VersionInfo.Add(AttributeNodes.Nodes['Value'].Text);
                     End;

            // Check against the current version of COR

            If (NewVersion(VersionInfo.Strings[0], COR_VERSION)) Then
               Try
                  IdHTTP.Head(COR_UPDATE_EXE);

                  FileSize := IdHTTP.Response.ContentLength;

                  IdHTTP.Disconnect;

                  With TVersionInfoForm.Create(Application.MainForm, VersionInfo, FileSize) Do Begin
                     ShowModal;

                     If (ModalResult = mrYes) Then
                        With TUpdateForm.Create(Application.MainForm, aStartup, VersionInfo.Strings[0]) Do Begin
                           ShowModal;

                           If (Not aStartup And (Tag = UPDATE_MUST_RESTART_COR)) Then
                              MessageDlg('The latest version of '+COR_NAME+' has been downloaded and will be active the next time you start '+COR_NAME+'.', mtInformation, [mbOK], 0);

                           Free;
                        End;

                     Free;
                  End
               Except
                  If (Not aStartup) Then
                     ShowProxyErrorMsg;
               End
            Else If (Not aStartup) Then
               MessageDlg('You are already using the latest version of '+COR_NAME+' ('+COR_VERSION+').', mtInformation, [mbOK], 0);
         Except
            // Something wrong with the XML file

            On E: Exception Do
               If (Not aStartup) Then
                  ExceptionDialogMailClass.ShowException(E, Nil);
         End;

         XMLDocument.Active := False;
      Finally
         VersionInfo.Free;

         XMLDocument := Nil;
      End;

      // Delete the info file

      DeleteFile(PChar(INFO_COR_FILENAME));
   End;
End;

//==============================================================================

Procedure TrackOldCOR(Const aOldCORFileName: String);
Begin
   With TRegistryIniFile.Create(COR_REGISTRY) Do Begin
      WriteString(OLD, 'OldCORFileName', aOldCORFileName);

      Free;
   End;
End;

//==============================================================================

Procedure DeleteOldCOR;
Var
   OldCORFileName: String;
Begin
   With TRegistryIniFile.Create(COR_REGISTRY) Do Begin
      If (SectionExists(OLD)) Then Begin
         OldCORFileName := ReadString(OLD, 'OldCORFileName', '???');

         If (FileExists(OldCORFileName)) Then
            DeleteFile(PChar(OldCORFileName));

         EraseSection(OLD);
      End;

      Free;
   End;
End;

//==============================================================================

Procedure LoadGeneralOptions;
Begin
   With TRegistryIniFile.Create(COR_REGISTRY) Do Begin
      With GeneralOptions Do Begin
         If (IsWinVistaOrAboveOrAdministrator) Then
            CheckForNewVersionAtStartup := ReadBool(OPTIONS_GENERAL, 'CheckForNewVersionAtStartup', True)
         Else
            CheckForNewVersionAtStartup := False;

         ConnectThroughProxyServer := ReadBool(OPTIONS_GENERAL, 'ConnectThroughProxyServer', False);

         ProxyServer := ReadString(OPTIONS_GENERAL, 'ProxyServer', '');
         ProxyPort   := ReadInteger(OPTIONS_GENERAL, 'ProxyPort', 8080);

         ProxyAuthenticationMethod := ReadString(OPTIONS_GENERAL, 'ProxyAuthenticationMethod', 'Basic');

         ProxyAuthenticationUsername := ReadString(OPTIONS_GENERAL, 'ProxyAuthenticationUsername', '');
         ProxyAuthenticationPassword := ReadString(OPTIONS_GENERAL, 'ProxyAuthenticationPassword', '');

         ProxyAuthenticationAskForPassword := ReadBool(OPTIONS_GENERAL, 'ProxyAuthenticationAskForPassword', False);
      End;

      Free;
   End;
End;


//==============================================================================

Function LockColor(Const aColor: TColor): TColor;
Var
   LC, C: TRGB;
   AlphaVal, One_AlphaVal: Double;
Begin
   C := RGBToRGBValues(ColorToRGB(aColor));

   With EditorOptions.ColourOptions.Elements.Get(LOCKED_FILE) Do Begin
      LC := RGBToRGBValues(ColorToRGB(BackgroundColor));

      AlphaVal     := 0.01*Alpha;
      One_AlphaVal := 1-AlphaVal;

      Result := RGB(Round(AlphaVal*LC.R+One_AlphaVal*C.R),
                    Round(AlphaVal*LC.G+One_AlphaVal*C.G),
                    Round(AlphaVal*LC.B+One_AlphaVal*C.B));
   End;
End;

//==============================================================================

Procedure UpdateHighlighterAttributes(Const aSynHighlighterAttributes: TSynHighlighterAttributes;
                                      Const aElement: TEditorColElemOptions;
                                      Const aLocked: Boolean);
Begin
   If (aLocked) Then Begin
      aSynHighlighterAttributes.Background := LockColor(aElement.BackgroundColor)
   End Else
      aSynHighlighterAttributes.Background := aElement.BackgroundColor;

   aSynHighlighterAttributes.Foreground := aElement.ForegroundColor;
   aSynHighlighterAttributes.Style      := aElement.FontStyle;
End;

//==============================================================================

Procedure UpdateCellMLSynHighlighter(Const aSynCellMLSyn: TSynCellMLSyn;
                                     Const aElement: TEditorColElemOptions;
                                     Const aLocked: Boolean);
Begin
   If (CompareStr(aElement.Name, DEFAULT) = 0) Then
      UpdateHighlighterAttributes(aSynCellMLSyn.IdentifierAttri, aElement, aLocked)
   Else If (CompareStr(aElement.Name, RESERVED_WORD) = 0) Then
      UpdateHighlighterAttributes(aSynCellMLSyn.KeyAttri, aElement, aLocked)
   Else If (CompareStr(aElement.Name, EXTRA_INFO) = 0) Then
      UpdateHighlighterAttributes(aSynCellMLSyn.ExtraInfoAttri, aElement, aLocked);
End;

//==============================================================================

Procedure UpdateEditorBackgroundColorAndCORAndCellMLSynHighlighters(Const aSyntaxEdit: TObject);
Var
   Iter: Integer;
   Locked: Boolean;
   RealColor: TColor;
Begin
   With aSyntaxEdit As TSyntaxEdit Do Begin
      Locked := ReadOnly;

      RealColor := EditorOptions.ColourOptions.Elements.Get(DEFAULT).BackgroundColor;

      If (Locked) Then
         Color := LockColor(RealColor)
      Else
         Color := RealColor;

      If (CellMLFile(FileName)) Then
         For Iter := 0 To EditorOptions.ColourOptions.Elements.Size-1 Do
            UpdateCellMLSynHighlighter(SynCellMLSyn, TEditorColElemOptions(EditorOptions.ColourOptions.Elements.At(Iter).VObject), Locked);

      Repaint;
      // Note: make sure that the changes are immediately taken into account             
   End;
End;

//==============================================================================

Procedure HighlightEditField(Const aEditField: TJvValidateEdit;
                             Const aErrorMsg: String);
   Procedure SetFocus(Const aWinControl: TWinControl);
   Begin
      If (aWinControl.Parent Is TWinControl) Then
         SetFocus(aWinControl.Parent);

      If (aWinControl.Parent Is TPageControl) Then
         // The parent of the current control is a page control, so that has to
         // make the current control a tab sheet, hence...

         (aWinControl.Parent As TPageControl).ActivePage := aWinControl As TTabSheet
      Else If (aWinControl.Visible) Then
         aWinControl.SetFocus;
   End;
Begin
   // Go as high as the form to set the focus on the right field. That's because
   // a form may contain page controls and tab sheets and that of the field we
   // want to highlight may not be selected, so...

   MessageDlg('Please enter a valid '+aErrorMsg+'.', mtError, [mbOK], 0);

   If (aEditField.Visible) Then
      SetFocus(aEditField);

   aEditField.SelectAll;
End;

//==============================================================================

Procedure EditFieldForIPAddress(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCheckChars;

   aEditField.CheckChars := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890.';

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForIPAddressValidity(Const aEditField: TJvValidateEdit;
                                            Const aHighlightEditField: Boolean): Boolean;
   Function ValidIPNb(Const aIPNb: Integer): Boolean;
   Begin
      Result := (aIPNb >= 0) And (aIPNb <= 255);
   End;
   Function ExtractIPNb(Var aStr: String): Boolean;
   Var
      I: Integer;
      IPNb: Integer;
   Begin
      I := Pos('.', aStr);

      If (TryStrToInt(Copy(aStr, 1, I-1), IPNb) And ValidIPNb(IPNb)) Then Begin
         aStr := Copy(aStr, I+1, Length(aStr)-I);

         Result := True;
      End Else
         Result := False;
   End;
Var
   IPAddress: String;
   IPNb: Integer;
Begin
   // Check whether the given value is empty, or starts or ends with a '.'

   If (CompareStr(aEditField.Text, '') = 0) Or ((aEditField.Text[1] = '.') Or (aEditField.Text[Length(aEditField.Text)] = '.')) Then
      Result := False
   Else Begin
      // So far so good, so extract W, X, Y and Z, all of which having to be
      // between 0 and 255

      IPAddress := aEditField.Text;

      Result := ExtractIPNb(IPAddress) And
                ExtractIPNb(IPAddress) And
                ExtractIPNb(IPAddress) And
                TryStrToInt(IPAddress, IPNb) And ValidIPNb(IPNb);
   End;

   If (Not Result And aHighlightEditField) Then
      HighlightEditField(aEditField, 'IP address, i.e. of the format "W.X.Y.Z" with W, X, Y and Z a number between 0 and 255');
End;

//==============================================================================

Procedure EditFieldForUsername(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfAlphaNumeric;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForUsernameValidity(Const aEditField: TJvValidateEdit;
                                           Const aHighlightEditField: Boolean): Boolean;
Begin
   Result := CompareStr(aEditField.Text, '') <> 0;

   If (Not Result and aHighlightEditField) Then
      HighlightEditField(aEditField, 'username');
End;

//==============================================================================

Procedure EditFieldForPassword(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCustom;

   aEditField.Text := '';

   aEditField.PasswordChar := #149;
End;

//==============================================================================

Procedure EditFieldForFolderPath(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfNone;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForFolderPathValidity(Const aEditField: TJvValidateEdit;
                                             Const aHighlightEditField: Boolean): Boolean;
Begin
   Result := DirectoryExists(aEditField.Text);

   If (Not Result and aHighlightEditField) Then
      HighlightEditField(aEditField, 'folder path');
End;

//==============================================================================

Procedure EditFieldForFloat(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCheckChars;

   aEditField.CheckChars := '0123456789e.-';

   aEditField.AutoAlignment := False;
   aEditField.Alignment     := taRightJustify;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForFloatValidity(Const aEditField: TJvValidateEdit;
                                        Const aHighlightEditField: Boolean): Boolean;
Var
   DummyDblNbVal: Double;
Begin
   Result := (CompareStr(aEditField.Text, '') <> 0) And TryStrToFloat(aEditField.Text, DummyDblNbVal);

   If (Not Result And aHighlightEditField) Then
      HighlightEditField(aEditField, 'float number');
End;

//==============================================================================

Procedure EditFieldForStrictlyPositiveFloat(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCheckChars;

   aEditField.CheckChars := '0123456789e.-';

   aEditField.AutoAlignment := False;
   aEditField.Alignment     := taRightJustify;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForStrictlyPositiveFloatValidity(Const aEditField: TJvValidateEdit;
                                                        Const aHighlightEditField: Boolean): Boolean;
Var
   DummyDblNbVal: Double;
Begin
   Result := (CompareStr(aEditField.Text, '') <> 0) And TryStrToFloat(aEditField.Text, DummyDblNbVal) And (DummyDblNbVal > 0);

   If (Not Result And aHighlightEditField) Then
      HighlightEditField(aEditField, 'strictly positive float number');
End;

//==============================================================================

Procedure EditFieldForPositiveFloat(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCheckChars;

   aEditField.CheckChars := '0123456789e.-';

   aEditField.AutoAlignment := False;
   aEditField.Alignment     := taRightJustify;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForPositiveFloatValidity(Const aEditField: TJvValidateEdit;
                                                Const aHighlightEditField: Boolean): Boolean;
Var
   DummyDblNbVal: Double;
Begin
   Result := (CompareStr(aEditField.Text, '') <> 0) And TryStrToFloat(aEditField.Text, DummyDblNbVal) And (DummyDblNbVal >= 0);

   If (Not Result And aHighlightEditField) Then
      HighlightEditField(aEditField, 'positive float number');
End;

//==============================================================================

Procedure EditFieldForPositiveInteger(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCheckChars;

   aEditField.CheckChars := '0123456789e.-';

   aEditField.AutoAlignment := False;
   aEditField.Alignment     := taRightJustify;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForPositiveIntegerValidity(Const aEditField: TJvValidateEdit;
                                                  Const aHighlightEditField: Boolean): Boolean;
Var
   DummyIntNbVal: Integer;
Begin
   Result := (CompareStr(aEditField.Text, '') <> 0) And TryStrToInt(aEditField.Text, DummyIntNbVal) And (DummyIntNbVal >= 0);

   If (Not Result And aHighlightEditField) Then
      HighlightEditField(aEditField, 'positive integer number');
End;

//==============================================================================

Procedure EditFieldForStrictlyPositiveInteger(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCheckChars;

   aEditField.CheckChars := '0123456789e.-';

   aEditField.AutoAlignment := False;
   aEditField.Alignment     := taRightJustify;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForStrictlyPositiveIntegerValidity(Const aEditField: TJvValidateEdit;
                                                          Const aHighlightEditField: Boolean): Boolean;
Var
   DummyIntNbVal: Integer;
Begin
   Result := (CompareStr(aEditField.Text, '') <> 0) And TryStrToInt(aEditField.Text, DummyIntNbVal) And (DummyIntNbVal > 0);

   If (Not Result And aHighlightEditField) Then
      HighlightEditField(aEditField, 'strictly positive integer number');
End;

//==============================================================================

Procedure EditFieldForRangeInteger(Var aEditField: TJvValidateEdit);
Begin
   aEditField.DisplayFormat := dfCheckChars;

   aEditField.CheckChars := '0123456789e.-';

   aEditField.AutoAlignment := False;
   aEditField.Alignment     := taRightJustify;

   aEditField.Text := '';
End;

//==============================================================================

Function CheckEditFieldForRangeIntegerValidity(Const aEditField: TJvValidateEdit;
                                               Const aMin, aMax: Integer;
                                               Const aHighlightEditField: Boolean): Boolean;
Var
   DummyIntNbVal: Integer;
Begin
   Result := (CompareStr(aEditField.Text, '') <> 0) And TryStrToInt(aEditField.Text, DummyIntNbVal) And (DummyIntNbVal >= aMin) And (DummyIntNbVal <= aMax);

   If (Not Result And aHighlightEditField) Then
      HighlightEditField(aEditField, 'integer number between '+IntToStr(aMin)+' and '+IntToStr(aMax));
End;

//==============================================================================

Function ExecSaveDialog(Const aFileName, aDefaultExt, aFilter: String): Boolean;
Begin
   With SaveDialog Do Begin
      FileName := aFileName;

      If ((CompareStr(aDefaultExt, '') <> 0) And (CompareStr(aFilter, '') <> 0)) Then Begin
         DefaultExt := aDefaultExt;
         Filter     := aFilter;
      End Else Begin
         DefaultExt := CELLML_FILE_FILTER;
         Filter     := CELLML_FILE_FILTER+FILTER_SEP+ANY_FILE_FILTER;
      End;

      Result := Execute;

      If (Result And (CompareStr(ChangeFileExt(ExtractFileName(FileName), ''), '') = 0)) Then Begin
         MessageDlg('You must provide a valid filename.', mtWarning, [mbOK], 0);

         Result := False;
      End;
   End;
End;

//==============================================================================

Procedure RestoreControl(Const aControl: TControl; Const aVisible: Boolean);
Var
   FloatingControl: TFloatingControl;
Begin
   If (aVisible) Then Begin
      FloatingControl := FloatingControlList.Pop(aControl);

      If (FloatingControl <> Nil) Then
         With FloatingControl Do Begin
            Control.ManualFloat(Rect(0, 0, Width, Height));
            // Note #1: to provide the coordinates and dimensions to
            //          "ManualFloat" doesn't make the control host dock to be
            //          located in the right place and have the right
            //          dimensions, so...
            // Note #2: unfortunately the above command together with the
            //          following one means that there is potential for some
            //          flickering... :(
            // Note #3: the width and height in the above command are necessary
            //          in case of a toolbar...

            Control.Parent.SetBounds(Left, Top, Width, Height);

            Free;
         End
      Else
         // The control has already been restored, assuming it needed to be,
         // so...

         aControl.Show;
   End;
End;

//==============================================================================

Function OptionsForm(Const aOptionsType, aSubOptionsType: Integer): Boolean;
Var
   I: Integer;
Begin
   With TOptionsForm.Create(Application.MainForm, aOptionsType, aSubOptionsType) Do Begin
      If (ShowModal = mrOk) Then Begin
         // Update the options of the different editors

         For I := 0 To MainForm.EditorFrame.PageCtrl.PageCount-1 Do
            With MainForm.EditorFrame.PageCtrl.Pages[I].Controls[0] As TSyntaxEdit Do Begin
               SetOptions(EditorOptions);

               // Update colours and font settings

               If (MainForm.EditorFrame.PageCtrl.Pages[I] = MainForm.EditorFrame.PageCtrl.ActivePage) Then
                  UpdateEditorBackgroundColorAndCORAndCellMLSynHighlighters(MainForm.EditorFrame.PageCtrl.Pages[I].Controls[0]);

               Repaint;   // Necessary for things like error lines (see
                          // "SetOptions")
            End;

         // Update the options of the command viewer

         MainForm.EditorFrame.CommandViewerFrame.CmdGraph.SetOptions(EditorOptions.CommandViewerOptions);

         // Update the different graph panels

         For I := 0 To MainForm.ComputationFrame.GraphPanelsSize-1 Do
            MainForm.ComputationFrame.UpdateOGLGraphPanelSettings(MainForm.ComputationFrame.GraphPanels[I].OGLGraphPanel);
      End;

      Result := ModalResult = mrOk;

      Free;
   End;
End;

//==============================================================================

Procedure MathMLCmdToCmdBinTreeConv(aMathMLCommandBinTree: TMathMLCommandBinTree;
                                    Var aCmdBinTree: TCmdBinTree);
Var
   DummyDblNbVal: Double;
   PowCmdBinTree: TCmdBinTree;
   ePos: Integer;
Begin
   If (aMathMLCommandBinTree = Nil) Then
      Exit;

   Case aMathMLCommandBinTree.ItemType Of
      mitConcatenate:
         aCmdBinTree := TCmdBinTree.Create(itConcatenate);
      mitUnit:
         aCmdBinTree := TCmdBinTree.Create(itUnit, aMathMLCommandBinTree.Str);
      mitProperties:
         aCmdBinTree := TCmdBinTree.Create(itProperties);
      mitProperty:
         aCmdBinTree := TCmdBinTree.Create(itProperty);
      mitPiecewise:
         aCmdBinTree := TCmdBinTree.Create(itPiecewise);
      mitPiece:
         aCmdBinTree := TCmdBinTree.Create(itPiece);
      mitOtherwise:
         aCmdBinTree := TCmdBinTree.Create(itOtherwise);
      mitEq,
      mitEqEq:
         aCmdBinTree := TCmdBinTree.Create(itEq);
      mitNEq:
         aCmdBinTree := TCmdBinTree.Create(itNEq);
      mitLT:
         aCmdBinTree := TCmdBinTree.Create(itLT);
      mitGT:
         aCmdBinTree := TCmdBinTree.Create(itGT);
      mitLEq:
         aCmdBinTree := TCmdBinTree.Create(itLEq);
      mitGEq:
         aCmdBinTree := TCmdBinTree.Create(itGEq);
      mitPlus:
         aCmdBinTree := TCmdBinTree.Create(itPlus);
      mitMinus:
         aCmdBinTree := TCmdBinTree.Create(itMinus);
      mitTimes:
         aCmdBinTree := TCmdBinTree.Create(itTimes);
      mitDivide:
         aCmdBinTree := TCmdBinTree.Create(itDivide);
      mitPow:
         aCmdBinTree := TCmdBinTree.Create(itPow);
      mitRoot:
         aCmdBinTree := TCmdBinTree.Create(itRoot);
      mitAbs:
         aCmdBinTree := TCmdBinTree.Create(itAbs);
      mitExp:
         aCmdBinTree := TCmdBinTree.Create(itExp);
      mitLN:
         aCmdBinTree := TCmdBinTree.Create(itLN);
      mitLog:
         aCmdBinTree := TCmdBinTree.Create(itLog);
      mitCeil:
         aCmdBinTree := TCmdBinTree.Create(itCeil);
      mitFloor:
         aCmdBinTree := TCmdBinTree.Create(itFloor);
      mitFact:
         aCmdBinTree := TCmdBinTree.Create(itFact);
      mitAnd:
         aCmdBinTree := TCmdBinTree.Create(itAnd);
      mitOr:
         aCmdBinTree := TCmdBinTree.Create(itOr);
      mitXOr:
         aCmdBinTree := TCmdBinTree.Create(itXOr);
      mitNot:
         aCmdBinTree := TCmdBinTree.Create(itNot);
      mitDiff:
         aCmdBinTree := TCmdBinTree.Create(itDiff);
      mitDegree:
         aCmdBinTree := TCmdBinTree.Create(itDegree);
      mitLogBase:
         aCmdBinTree := TCmdBinTree.Create(itLogBase);
      mitSin:
         aCmdBinTree := TCmdBinTree.Create(itSin);
      mitCos:
         aCmdBinTree := TCmdBinTree.Create(itCos);
      mitTan:
         aCmdBinTree := TCmdBinTree.Create(itTan);
      mitSec:
         aCmdBinTree := TCmdBinTree.Create(itSec);
      mitCsc:
         aCmdBinTree := TCmdBinTree.Create(itCsc);
      mitCot:
         aCmdBinTree := TCmdBinTree.Create(itCot);
      mitSinH:
         aCmdBinTree := TCmdBinTree.Create(itSinH);
      mitCosH:
         aCmdBinTree := TCmdBinTree.Create(itCosH);
      mitTanH:
         aCmdBinTree := TCmdBinTree.Create(itTanH);
      mitSecH:
         aCmdBinTree := TCmdBinTree.Create(itSecH);
      mitCscH:
         aCmdBinTree := TCmdBinTree.Create(itCscH);
      mitCotH:
         aCmdBinTree := TCmdBinTree.Create(itCotH);
      mitASin:
         aCmdBinTree := TCmdBinTree.Create(itASin);
      mitACos:
         aCmdBinTree := TCmdBinTree.Create(itACos);
      mitATan:
         aCmdBinTree := TCmdBinTree.Create(itATan);
      mitASec:
         aCmdBinTree := TCmdBinTree.Create(itASec);
      mitACsc:
         aCmdBinTree := TCmdBinTree.Create(itACsc);
      mitACot:
         aCmdBinTree := TCmdBinTree.Create(itACot);
      mitASinH:
         aCmdBinTree := TCmdBinTree.Create(itASinH);
      mitACosH:
         aCmdBinTree := TCmdBinTree.Create(itACosH);
      mitATanH:
         aCmdBinTree := TCmdBinTree.Create(itATanH);
      mitASecH:
         aCmdBinTree := TCmdBinTree.Create(itASecH);
      mitACscH:
         aCmdBinTree := TCmdBinTree.Create(itACscH);
      mitACotH:
         aCmdBinTree := TCmdBinTree.Create(itACotH);
   Else
      If (TryStrToFloat(aMathMLCommandBinTree.Str, DummyDblNbVal)) Then Begin
         // A number, so check whether it's something written in scientific
         // notation, in which case it's to be rendered as such (i.e. "3e5" is
         // to be converted into "3*pow(10, 5)")

         ePos := Pos('E', UpperCase(aMathMLCommandBinTree.Str));

         If (ePos <> 0) Then Begin
            PowCmdBinTree := TCmdBinTree.Create(itPow);

            PowCmdBinTree.Left := TCmdBinTree.Create(itSym, '10');
            PowCmdBinTree.Right := TCmdBinTree.Create(itSym, SimplifyNbStr(Copy(aMathMLCommandBinTree.Str, ePos+1, Length(aMathMLCommandBinTree.Str)-ePos)));

            aCmdBinTree := TCmdBinTree.Create(itTimes);

            aCmdBinTree.Left  := TCmdBinTree.Create(itSym, SimplifyNbStr(Copy(aMathMLCommandBinTree.Str, 1, ePos-1)));
            aCmdBinTree.Right := PowCmdBinTree;
         End Else
            // Just a "normal" number, so...

            aCmdBinTree := TCmdBinTree.Create(itSym, SimplifyNbStr(aMathMLCommandBinTree.Str));
      End Else
         aCmdBinTree := TCmdBinTree.Create(itSym, aMathMLCommandBinTree.Str);
   End;

   MathMLCmdToCmdBinTreeConv(aMathMLCommandBinTree.Left, aCmdBinTree.Left);
   MathMLCmdToCmdBinTreeConv(aMathMLCommandBinTree.Right, aCmdBinTree.Right);
End;

//==============================================================================

Function CanOverwriteFile(Const aFileName: Array Of String): Boolean;
Var
   Msg: String;
   I: Integer;
   LengthOfLastMsg: Integer;
   NbOfFiles: Integer;
   FileHandle: Integer;
   CanWriteAccessAllFiles: Boolean;
Begin
   Msg := '';

   LengthOfLastMsg := 0;
   NbOfFiles := 0;

   CanWriteAccessAllFiles := True;

   For I := 0 To High(aFileName) Do
      If (FileExists(aFileName[I])) Then Begin
         LengthOfLastMsg := Length(Msg);

         Msg := Msg+''''+aFileName[I]+''', ';

         If (CanWriteAccessAllFiles) Then Begin
            FileHandle := FileOpen(aFileName[I], fmOpenWrite);

            If (FileHandle < 0) Then
               CanWriteAccessAllFiles := False
            Else
               FileClose(FileHandle);
         End;

         Inc(NbOfFiles);
      End;

   Case NbOfFiles Of
      0:
         Result := True;
      1:
         If (CanWriteAccessAllFiles) Then
            Result := MessageDlg('The file '+Copy(Msg, 1, Length(Msg)-2)+' already exists. Do you want to overwrite it?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
         Else Begin
            MessageDlg('The file '+Copy(Msg, 1, Length(Msg)-2)+' already exists, but it cannot be accessed.', mtInformation, [mbOK], 0);

            Result := False;
         End;
   Else
      If (CanWriteAccessAllFiles) Then
         Result := MessageDlg('The files '+Copy(Msg, 1, LengthOfLastMsg-2)+' and '+Copy(Msg, LengthOfLastMsg+1, Length(Msg)-LengthOfLastMsg-2)+' already exist. Do you want to overwrite them?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
      Else Begin
         MessageDlg('The files '+Copy(Msg, 1, LengthOfLastMsg-2)+' and '+Copy(Msg, LengthOfLastMsg+1, Length(Msg)-LengthOfLastMsg-2)+' already exist, but at least one of them cannot be accessed.', mtInformation, [mbOK], 0);

         Result := False;
      End;
   End;
End;

//==============================================================================

Function CanOverwriteFile(Const aFileName: String): Boolean;
Var
   FileName: Array[0..0] Of String;
Begin
   FileName[0] := aFileName;

   Result := CanOverwriteFile(FileName);
End;

//==============================================================================

Procedure AddCellMLMessages;
Begin
   // Effectively add the different messages, if any, but first sort them and
   // remove the duplicates (just in case)

   TEngineMsg.SortAndRemoveDuplicates(CellMLMessages);

   MainForm.EditorFrame.AddEngineMsgs(CellMLMessages);

   // Clear the messages

   ObjFree(CellMLMessages);

   CellMLMessages.Clear;
End;

//==============================================================================

Function CellMLFileToCellMLAPI(Const aFileName: String;
                               Const aCellMLModel: TCellMLModel): Boolean;
Const
   LINE_STR = 'Line: ';
Var
   Msg: String;
   Line, LineNbPos, LineNbEnd: Integer;
Begin
   MainForm.EditorFrame.ClearSpecMsgs(aFileName);

   With TCellMLFileToCellMLAPIEngine.Create(aFileName, aCellMLModel, Msg) Do Begin
      Result := Execute;

      If (Not Result) Then Begin
         // There is a problem with the CellML file. Most likely, it is an issue
         // with the CellML code itself, in which case we might be able to
         // retrieve the line number where the problem is. If so, then we need
         // to look for "... Line: XXX ..."

         Line := UNDEFINED;

         LineNbPos := Pos(LINE_STR, Msg);

         If (LineNbPos <> 0) Then Begin
            Inc(LineNbPos, Length(LINE_STR));

            LineNbEnd := Pos(CRLF, Copy(Msg, LineNbPos, Length(Msg)-LineNbPos+1));

            If (LineNbEnd <> 0) Then
               Line := StrToInt(Copy(Msg, LineNbPos, LineNbEnd-1));
         End;

         MainForm.EditorFrame.AddMsg(mtError, aFileName, Line, UNDEFINED, Msg, False);
      End;

      Free;
   End
End;

//==============================================================================

Function CellMLAPIToCellMLFile(Const aCellMLModel: TCellMLModel;
                               Const aFileName: String): Boolean;
Var
   Message: String;
Begin
   MainForm.EditorFrame.ClearSpecMsgs(aFileName);

   With TCellMLAPIToCellMLFileEngine.Create(aCellMLModel, aFileName, Message) Do Begin
      Result := Execute;

      If (Not Result) Then
         MainForm.EditorFrame.AddMsg(mtError, aFileName, UNDEFINED, UNDEFINED, Message, False);

      Free;
   End
End;

//==============================================================================

Function CellMLAPIToCellMLASCII(Const aCellMLModel: TCellMLModel;
                                Const aSyntaxEdit: Pointer): Boolean;
Var
   ASCII: String;
Begin
   With TCellMLAPIToCellMLASCIIEngine.Create(aCellMLModel, ASCII) Do Begin
      Result := Execute;

      Free;

      If (Result) Then
         With TSyntaxEdit(aSyntaxEdit) Do Begin
            Text := ASCII;

            // Initialise the extra information

            Extra.Init(aCellMLModel.Extra);
         End;
   End;
End;

//==============================================================================

Function CellMLASCIIToCellMLAPI(Const aSyntaxEdit: Pointer;
                                Const aCellMLModel: TCellMLModel): Boolean;
Var
   SyntaxEdit: TSyntaxEdit;
Begin
   SyntaxEdit := TSyntaxEdit(aSyntaxEdit);

   MainForm.EditorFrame.ClearSpecMsgs(SyntaxEdit.FileName);

   With TCellMLASCIIToCellMLAPIEngine.Create(SyntaxEdit.FileName, SyntaxEdit.Text, aCellMLModel, CellMLMessages) Do Begin
      Result := Execute;

      AddCellMLMessages;

      Free;

      If (Result) Then
         // Initialise the extra information

         aCellMLModel.Extra.Init(SyntaxEdit.Extra);
   End;
End;

//==============================================================================

Function CellMLASCIIToCellMLAPI(Const aCmd: String;
                                Var aMathMLCommandBinTree: TMathMLCommandBinTree): Boolean;
Begin
   With TCellMLASCIIToCellMLAPIEngine.Create(aCmd, aMathMLCommandBinTree) Do Begin
      Result := Execute;

      Free;
   End;
End;

//==============================================================================

Procedure UpdateCellMLAPI(Const aCellMLModel: TCellMLModel;
                          Const aVirtualStringTree: TVirtualStringTree);
   Procedure UpdateCellMLAPIFromVirtualStringTreeNode(Const aNode: PVirtualNode;
                                                      Const aComponent: TCellMLComponent = Nil);
   Var
      Component: TCellMLComponent;
   Begin
      // Show the check box of the property that is either associated to the
      // active graph panel or that is not associated to any graph panel at all

      With PPropertyData(aVirtualStringTree.GetNodeData(aNode))^ Do
         If (CompareStr(Owner, '') = 0) Then
            // Component

            Component := aCellMLModel.FindComponent(Name)
         Else Begin
            // Parameter

            Component := aComponent;   // Keep track of the component

            If (PropType In [ptStateVar, ptCst]) Then
               // Update the variable's initial value

               Component.FindVariable(Name).InitialValue := SimplifyNb(TPropertyParameter(Data).Value^);
         End;

      If (aNode^.ChildCount <> 0) Then Begin
         UpdateCellMLAPIFromVirtualStringTreeNode(aNode^.FirstChild, Component);

         // Reset the order of the list of variables of the component to that of
         // their creation

         Component.VariableList.Comparator := MakeComparator(LineCompare);

         Sort(Component.VariableList);
      End;

      If (aNode^.NextSibling <> Nil) Then
         UpdateCellMLAPIFromVirtualStringTreeNode(aNode^.NextSibling, Component);
   End;
Begin
   UpdateCellMLAPIFromVirtualStringTreeNode(aVirtualStringTree.RootNode.FirstChild.NextSibling);
   // Note: we go straight for the second child, because we know that the first
   //       one contains information about the simulation settings, so we don't
   //       need that...

   // Reset the order of the components to that of their creation

   aCellMLModel.ComponentList.Comparator := MakeComparator(LineCompare);

   Sort(aCellMLModel.ComponentList);
End;

//==============================================================================

Function CellMLModelIsValid(Const aCellMLModel: TCellMLModel;
                            Const aReset: Boolean): Boolean;
Begin
   // Clear the messages related to the CellML model

   MainForm.EditorFrame.ClearSpecMsgs(aCellMLModel.FileName);

   // Check whether the CellML model is valid or not

   Result := aCellMLModel.IsValid(CellMLMessages, aReset);

   // Effectively add the different messages, if any

   AddCellMLMessages;
End;

//==============================================================================

Function FileMD5(Const aFileName: String): String;
Const
   HASH_LEN: DWord = 16;
Var
   CryptHash: HCRYPTHASH;
   Hash: Array[0..127] Of Byte;
   FileID, NbOfBytesRead: Integer;
Begin
   Result := '';

   If (MD5Valid) Then Begin
      FileID:= FileOpen(aFileName, fmShareDenyWrite);

      Try
         CryptCreateHash(MD5CryptProv, CALG_MD5, 0, 0, @CryptHash);

         Repeat
            NbOfBytesRead := FileRead(FileID, MD5Buffer^, MD5_BUFFER_SIZE);

            If (NbOfBytesRead > 0) Then
               CryptHashData(CryptHash, MD5Buffer, NbOfBytesRead, 0);
         Until NbOfBytesRead < MD5_BUFFER_SIZE;

         CryptGetHashParam(CryptHash, HP_HASHVAL, @Hash[0], @HASH_LEN, 0);

         CryptDestroyHash(CryptHash);

         Result := IntToHex(Hash[ 0], 2)+IntToHex(Hash[ 1], 2)+IntToHex(Hash[ 2], 2)+IntToHex(Hash[ 3], 2)+
                   IntToHex(Hash[ 4], 2)+IntToHex(Hash[ 5], 2)+IntToHex(Hash[ 6], 2)+IntToHex(Hash[ 7], 2)+
                   IntToHex(Hash[ 8], 2)+IntToHex(Hash[ 9], 2)+IntToHex(Hash[10], 2)+IntToHex(Hash[11], 2)+
                   IntToHex(Hash[12], 2)+IntToHex(Hash[13], 2)+IntToHex(Hash[14], 2)+IntToHex(Hash[15], 2);
      Finally
         FileClose(FileID);
      End;
   End;
End;

//==============================================================================

Initialization

//==============================================================================

// Version of COR

Major   := 0;
Minor   := 0;
Release := 0;
Build   := 0;

FileVersionInfoSize := GetFileVersionInfoSize(Pointer(Application.ExeName), Dummy);

If (FileVersionInfoSize > 0) Then Begin
   // There are some information available, so...

   GetMem(FileVersionInfo, FileVersionInfoSize);

   If (GetFileVersionInfo(Pointer(Application.ExeName), 0, FileVersionInfoSize, FileVersionInfo)) Then
      If (VerQueryValue(FileVersionInfo, '\', Pointer(FileInfo), InfoSize)) Then
         If (InfoSize > 0) Then Begin
            Major   := HIWORD(FileInfo.dwFileVersionMS);
            Minor   := LOWORD(FileInfo.dwFileVersionMS);
            Release := HIWORD(FileInfo.dwFileVersionLS);
            Build   := LOWORD(FileInfo.dwFileVersionLS);
         End;

   FreeMem(FileVersionInfo);
End;

COR_SHORT_VERSION := IntToStr(Major)+'.'+IntToStr(Minor);

If (Build = 0) Then Begin
   If (Release = 0) Then
      COR_VERSION := COR_SHORT_VERSION
   Else
      COR_VERSION := COR_SHORT_VERSION+'.'+IntToStr(Release);
End Else
   COR_VERSION  := COR_SHORT_VERSION+'.'+IntToStr(Release)+'.'+IntToStr(Build);

// Miscellaneous

COR_COPYRIGHT      := 'Copyright 2002-'+IntToStr(CurrentYear);
COR_FULL_COPYRIGHT := COR_COPYRIGHT+' Dr Alan Garny';

INFO_COR_FILENAME := TempDir+ExtractFileName(ChangeFileExt(Application.ExeName, '.xml'));

UPDATED_COR_FILENAME := TempDir+'Updated'+ExtractFileName(Application.ExeName);
// Note: the default name cannot be that of the application itself, in case the
//       temporary directory is that of the application... which means that if
//       we had to update COR, we couldn't... hence "Updated" in front of the
//       name, just to be on the safe side...

UPDATE_COR_FILENAME := TempDir+ExtractFileName(ChangeFileExt(Application.ExeName, '.bat'));

COR_PATH        := ExtractFilePath(Application.ExeName);
COR_MODELS_PATH := COR_PATH+'Models\';

// Check that the paths exist otherwise revert to COR's directory

If (Not DirectoryExists(COR_MODELS_PATH)) Then
   COR_MODELS_PATH := COR_PATH;

// Initialisation of different variables

CORAlreadyRunningVal := UNDEFINED;

DummyClass := TDummyClass.Create;
// Note: it is indirectly required by IdHTTP, so...

// HTTP service
// Note: it has to be created as soon as possible because of Indy intialising
//       stuff that we need later on, so...

IdHTTP := TIdHTTP.Create(Nil);

IdHTTP.OnSelectProxyAuthorization := DummyClass.IdHTTPSelectProxyAuthorization;

// COR Computer

CORComputer := TComputer.Create;

// Options

GeneralOptions    := TGeneralOptions.Create;
EditorOptions      := TEditorOptions.Create;
ComputationOptions := TComputationOptions.Create;

If (Not CORAlreadyRunning) Then Begin
   // COR help

   CORHelpFileName := TempDir+'COR.chm';
   // Note: we don't use a temporary name for the help file, as this will reset
   //       the location of the help window every time we start COR and we don't
   //       really want that, so...

   // Extract the COR help

   ExtractResource('CORHELP', CORHelpFileName);

{$IFDEF OPT_MATH}
   If (CPUVendor = cvIntel) Then Begin
      // Optimised Math DLL

      OptMathDLLFileName := TempDir+'IntelMath.dll';

      // Extract the Optimised Math DLL

      ExtractResource('INTELMATHDLL', OptMathDLLFileName);
   End Else Begin
      // Optimised Math DLL

      OptMathDLLFileName := TempDir+'MicrosoftMath.dll';

      // Extract the Optimised Math DLL

      ExtractResource('MICROSOFTMATHDLL', OptMathDLLFileName);
   End;

   // Load the Optimised Math DLL and its different functions

   OptMathDLLHandle := LoadLibrary(PChar(OptMathDLLFileName));

   LoadOptMathFunctions;
{$ENDIF}

   If (CPUVendor = cvIntel) Then Begin
      // ODE DLL

      ODEDLLFileName := TempDir+'IntelODE.dll';

      // Extract the ODE DLL

      ExtractResource('INTELODEDLL', ODEDLLFileName);
   End Else Begin
      // ODE DLL

      ODEDLLFileName := TempDir+'MicrosoftODE.dll';

      // Extract the ODE DLL

      ExtractResource('MICROSOFTODEDLL', ODEDLLFileName);
   End;

   // Load the ODE DLL and its different sets of functions

   ODEDLLHandle := LoadLibrary(PChar(ODEDLLFileName));

   LoadODEForwardEulerFunction;
   LoadODE2ndOrderRungeKuttaFunction;
   LoadODE4thOrderRungeKuttaFunction;
   LoadODECVODEFunctions;
End;

// Syntax highlighters

SynXMLSyn    := TSynXMLSyn.Create(Application);
SynCellMLSyn := TSynCellMLSyn.Create(Application);

// List of opened files

EditorFrameOpenedFiles := DList.Create;

// List of floating controls

FloatingControlList := TFloatingControlList.Create;

// UDP server/client

If (Not CORAlreadyRunning) Then
   TIdAntiFreeze.Create(Application);

// Open/save dialog box

OpenDialog.DefaultExt := CELLML_FILE_FILTER;
OpenDialog.Filter     := CELLML_FILE_FILTER+FILTER_SEP+ANY_FILE_FILTER;

// COR or CellML messages

CellMLMessages := DArray.CreateWith(MakeComparator(EngineMsgCompare));

// Precision with which calculations are to be done

SetPrecisionMode(pmDouble);   // Note: may not be that critical anymore, but it
                              //       surely doesn't harm, so...

// Views

Views := DArray.Create;

With Views Do Begin
   Add(['COR']);
   Add(['Raw']);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

// MD5

MD5Valid := CryptAcquireContext(@MD5CryptProv, Nil, MS_ENHANCED_PROV, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT Or CRYPT_MACHINE_KEYSET);

If (MD5Valid) Then
   GetMem(MD5Buffer, MD5_BUFFER_SIZE);

//==============================================================================

Finalization

//==============================================================================

DummyClass.Free;

IdHTTP.Free;

CORComputer.Free;

GeneralOptions.Free;
EditorOptions.Free;
ComputationOptions.Free;

If (Not CORAlreadyRunning) Then Begin
   SysUtils.DeleteFile(CORHelpFileName);

{$IFDEF OPT_MATH}
   FreeLibrary(OptMathDLLHandle);

   SysUtils.DeleteFile(OptMathDLLFileName);
{$ENDIF}

   FreeLibrary(ODEDLLHandle);

   SysUtils.DeleteFile(ODEDLLFileName);
End;

//SynXMLSyn.Free;
//SynCORSyn.Free;
//SynCellMLSyn.Free;
// Note: no need to free them, since they belong to Application, which got rid
//       of them, so...

FreeAndClear(EditorFrameOpenedFiles);

FloatingControlList.Free;

// Note: no need to free the Indy controls, since they belong to Application,
//       which got rid of them, so...

CellMLMessages.Free;   // Note: ok to just free it, since there is no object in
                       //       it

// Note: we must NOT free the objects held by "Views", for they are strings!

Views.Free;

// MD5

CryptReleaseContext(MD5CryptProv, 0);

FreeMem(MD5Buffer);

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

