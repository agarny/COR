//==============================================================================
// Computation frame
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 23/05/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// Note: all actions should initially be disabled, except for the one that
//       allows to run a CellML file. This is to give the user the impression
//       that things are ready, while may still be doing things that take time
//       (e.g. initialising all the nodes in the properties frame). Indeed, in
//       such a case, we don't want to have for instance the stop action enabled
//       while there are still things to be done. Having said all that, the
//       things to be done shouldn't take that long, so it should be kind of
//       transparent to the user...
//==============================================================================

Unit Computation;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, Classes, Controls, Forms, Contents, Menus, ActnList, ComCtrls, Cell,
   ExtCtrls, ToolWin, Console, Dockable, ImgList, CellMLAPIToMCEngine, Graphics, 
   GraphPanel, OGLGraphPanel, ODEIntegrator, Properties, DeCAL, VirtualTrees,
   CORCommon, VSTListBox, CellMLAPI;

//==============================================================================

Type
   TComputingEngineState = (cesStarted, cesPaused, cesStopped);
   TResetVariablesType = (rptStateVars, rptCsts, rptAll);
   TDebugData = Record
      NeedCellCompute: Boolean;
      Result: Boolean;
   End;
   TOGLGraphPanelData = Record
      OGLGraphPanel: TOGLGraphPanel;
      NbOfVariables: Integer;
   End;
   TComputationFrame = Class(TContentsFrame)
    ActionRunStop: TAction;
    ActionViewConsoleViewer: TAction;
    MenuViewConsole: TMenuItem;
    MenuRunStop: TMenuItem;
    ControlBarMenuToolBarFileMenu: TToolButton;
    ControlBarMenuToolBarViewMenu: TToolButton;
    ControlBarMenuToolBarRunMenu: TToolButton;
    ControlBarMenuToolBarHelpMenu: TToolButton;
    ControlBarRunMenuToolBarRunBtn: TToolButton;
    ControlBarFileMenuToolBarExitBtn: TToolButton;
    ConsoleFrame: TConsoleFrame;
    ControlBarMenuToolBarToolsMenu: TToolButton;
    ControlBarRunMenuToolBarStopBtn: TToolButton;
    ActionViewPropertiesViewer: TAction;
    MenuViewProperties: TMenuItem;
    CentralDockSite: TPanel;
    ActionToolsResetVariablesDummy: TAction;
    ActionToolsClearAllGraphsInAllGraphPanels: TAction;
    PropertiesImageList: TImageList;
    ActionToolsClearAllGraphsInCurrentGraphPanel: TAction;
    ClearAllGraphsInAllGraphPanelsOrInCurrentGraphPanelPopupMenu: TPopupMenu;
    ClearAllGraphsPopupMenuInAllGraphPanels: TMenuItem;
    ClearAllGraphsPopupMenuInCurrentGraphPanel: TMenuItem;
    ActionToolsClearAllGraphsDummy: TAction;
    ActionToolsClearAllGraphs: TAction;
    ActionToolsExportToCSVDummy: TAction;
    ActionToolsExportToCSV: TAction;
    ActionToolsExportToCSVAllGraphPanels: TAction;
    ActionToolsExportToCSVCurrentGraphPanel: TAction;
    MenuToolsSep2: TMenuItem;
    MenuToolsResetVariables: TMenuItem;
    MenuToolsSep3: TMenuItem;
    MenuToolsClearGraphPanel: TMenuItem;
    MenuToolsClearGraphPanelCurrent: TMenuItem;
    MenuToolsClearGraphPanelAll: TMenuItem;
    MenuToolsSep4: TMenuItem;
    MenuToolsExportToCSV: TMenuItem;
    MenuToolsExportToCSVCurrent: TMenuItem;
    MenuToolsExportToCSVAll: TMenuItem;
    ControlBarToolsMenuToolBar: TToolBar;
    ControlBarToolsMenuToolBarResetVariablesBtn: TToolButton;
    ControlBarToolsMenuToolBarAddGraphPanelBtn: TToolButton;
    ControlBarToolsMenuToolBarRemoveGraphPanelBtn: TToolButton;
    ControlBarToolsMenuToolBarClearAllGraphsBtn: TToolButton;
    ControlBarToolsMenuToolBarSep1: TToolButton;
    ControlBarToolsMenuToolBarExportToCSVBtn: TToolButton;
    ActionViewToolsToolBar: TAction;
    ControlBarPopupMenuToolsMenuItem: TMenuItem;
    MenuViewToolbarsTools: TMenuItem;
    ActionToolsAddGraphPanel: TAction;
    ActionToolsRemoveGraphPanelDummy: TAction;
    ActionToolsRemoveGraphPanel: TAction;
    ActionToolsRemoveGraphPanelAll: TAction;
    ActionToolsRemoveGraphPanelCurrent: TAction;
    RemoveAllGraphPanelsOrCurrentGraphPanelPopupMenu: TPopupMenu;
    ExportToCSVAllGraphPanelsOrCurrentGraphPanelPopupMenu: TPopupMenu;
    RemoveAllGraphPanelsOrCurrentGraphPanelPopupMenuAll: TMenuItem;
    RemoveAllGraphPanelsOrCurrentGraphPanelPopupMenuCurrent: TMenuItem;
    ExportToCSVPopupMenuAllGraphsPanels: TMenuItem;
    ExportToCSVPopupMenuCurrentGraphPanel: TMenuItem;
    MenuToolsAddGraphPanel: TMenuItem;
    MenuToolsRemoveGraphPanel: TMenuItem;
    MenuToolsRemoveGraphPanelAll: TMenuItem;
    MenuToolsRemoveGraphPanelCurrent: TMenuItem;
    GraphPanelPopupMenu: TPopupMenu;
    GraphPanelPopupMenuSep1: TMenuItem;
    GraphPanelPopupMenuAddGraphPanel: TMenuItem;
    GraphPanelPopupMenuRemoveGraphPanel: TMenuItem;
    GraphPanelPopupMenuRemoveGraphPanelCurrent: TMenuItem;
    GraphPanelPopupMenuRemoveGraphPanelAll: TMenuItem;
    GraphPanelPopupMenuClearAllGraphs: TMenuItem;
    GraphPanelPopupMenuClearAllGraphsCurrent: TMenuItem;
    GraphPanelPopupMenuClearAllGraphsAll: TMenuItem;
    GraphPanelPopupMenuExportToCSV: TMenuItem;
    GraphPanelPopupMenuExportToCSVCurrent: TMenuItem;
    GraphPanelPopupMenuExportToCSVAll: TMenuItem;
    ResetStateVarsOrCstsOrAllPopupMenu: TPopupMenu;
    ResetStateVarsOrCstsOrAllPopupMenuStateVars: TMenuItem;
    ResetStateVarsOrCstsOrAllPopupMenuCsts: TMenuItem;
    ActionToolsResetVariablesStateVars: TAction;
    ActionToolsResetVariablesCsts: TAction;
    ActionToolsResetVariablesAll: TAction;
    ActionToolsResetVariables: TAction;
    ResetStateVarsOrCstsOrAllPopupMenuSep: TMenuItem;
    ResetStateVarsOrCstsOrAllPopupMenuAll: TMenuItem;
    MenuToolsResetVariablesStateVars: TMenuItem;
    MenuToolsResetVariablesCsts: TMenuItem;
    MenuToolsResetVariablesSep: TMenuItem;
    MenuToolsResetVariablesAll: TMenuItem;
    MenuPropertiesVariables: TMenuItem;
    ActionPropertiesVariablesStateVariables: TAction;
    ActionPropertiesVariablesConstants: TAction;
    ActionPropertiesVariablesComputedVariables: TAction;
    MenuPropertiesVariablesStateVariables: TMenuItem;
    MenuPropertiesVariablesConstants: TMenuItem;
    MenuPropertiesVariablesComputedVariables: TMenuItem;
    ControlBarToolsMenuToolBarSep2: TToolButton;
    ControlBarPropertiesMenuToolBarPropertiesVariablesBtn: TToolButton;
    ActionPropertiesVariables: TAction;
    ShowOrHideSpecificPropertiesPopupMenu: TPopupMenu;
    ShowOrHideSpecificPropertiesPopupMenuStateVars: TMenuItem;
    ShowOrHideSpecificPropertiesPopupMenuCsts: TMenuItem;
    ShowOrHideSpecificPropertiesPopupMenuCompVars: TMenuItem;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    MenuFileSep1: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    ControlBarFileMenuToolBarSep: TToolButton;
    ControlBarFileMenuToolBarSaveBtn: TToolButton;
    ControlBarFileMenuToolBarSaveAsBtn: TToolButton;
    PropertiesFrame: TPropertiesFrame;
    ControlBarRunMenuToolBarSep: TToolButton;
    ControlBarRunMenuToolBarDebugBtn: TToolButton;
    ActionRunDebugMode: TAction;
    MenuRunSep: TMenuItem;
    MenuRunDebug: TMenuItem;
    ActionToolsGraphicalPanelOptions: TAction;
    GraphPanelPopupMenuSep2: TMenuItem;
    GraphPanelPopupMenuGraphicalPanelOptions: TMenuItem;
    ActionToolsExportToCSVAllGraphPanelsInOneCSVFile: TAction;
    MenuToolsExportToCSVAllInOneCSVFile: TMenuItem;
    ExportToCSVPopupMenuAllGraphsPanelsInOneCSVFile: TMenuItem;
    GraphPanelPopupMenuExportToCSVAllInOneCSVFile: TMenuItem;
    ActionPropertiesExpand: TAction;
    ActionPropertiesCollapse: TAction;
    MenuProperties: TMenuItem;
    ControlBarMenuToolBarPropertiesMenu: TToolButton;
    ControlBarPropertiesMenuToolBar: TToolBar;
    MenuViewToolbarsProperties: TMenuItem;
    ActionViewPropertiesToolBar: TAction;
    ControlBarPopupMenuPropertiesMenuItem: TMenuItem;
    ControlBarPropertiesMenuToolBarPropertiesExpandBtn: TToolButton;
    ControlBarPropertiesMenuToolBarPropertiesCollapseBtn: TToolButton;
    ToolButton5: TToolButton;
    MenuPropertiesSep: TMenuItem;
    MenuPropertiesCollapse: TMenuItem;
    MenuPropertiesExpand: TMenuItem;
    procedure ActionToolsResetVariablesCstsUpdate(Sender: TObject);
    procedure ActionToolsResetVariablesStateVarsUpdate(Sender: TObject);
    procedure ActionToolsClearAllGraphsExecute(Sender: TObject);
    procedure ActionToolsRemoveGraphPanelExecute(Sender: TObject);
    procedure ActionToolsResetVariablesExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionViewExecute(Sender: TObject);
    procedure ActionViewUpdate(Sender: TObject);
    procedure ActionRunRunExecute(Sender: TObject);
    procedure ActionRunStopExecute(Sender: TObject);
    procedure ActionRunRunUpdate(Sender: TObject);
    procedure PropertiesFrameVirtualStringTreeInitChildren(
      Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure PropertiesFrameVirtualStringTreeInitNode(
      Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure PropertiesFrameVirtualStringTreeEdited(
      Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure PropertiesFrameVirtualStringTreeGetText(
      Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: WideString);
    procedure PropertiesFrameVirtualStringTreeGetImageIndex(
      Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure ActionViewToolBarsExecute(Sender: TObject);
    procedure ActionViewToolBarsUpdate(Sender: TObject);
    procedure ActionToolsAddGraphPanelExecute(Sender: TObject);
    procedure CentralDockSiteGetSiteInfo(Sender: TObject;
      DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionToolsClearAllGraphsOrExportToCSVCurrentUpdate(
      Sender: TObject);
    procedure PropertiesFrameVirtualStringTreeChecked(
      Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ActionToolsExportToCSVAllGraphPanelsExecute(Sender: TObject);
    procedure ActionToolsExportToCSVCurrentGraphPanelExecute(
      Sender: TObject);
    procedure ActionViewVariablesTypeExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure ActionToolsResetVariablesAllUpdate(Sender: TObject);
    procedure ActionRunStopUpdate(Sender: TObject);
    procedure ActionToolsRemoveGraphPanelUpdate(Sender: TObject);
    procedure ActionFileSaveUpdate(Sender: TObject);
    procedure ActionFileSaveAsUpdate(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionFileSaveAsExecute(Sender: TObject);
    procedure ActionRunDebugModeExecute(Sender: TObject);
    procedure ActionToolsGraphicalPanelOptionsExecute(Sender: TObject);
    procedure ActionToolsClearAllGraphsOrExportToCSVUpdate(Sender: TObject);
    procedure ActionPropertiesExpandExecute(Sender: TObject);
    procedure ActionPropertiesCollapseExecute(Sender: TObject);
    procedure PropertiesFrameVirtualStringTreeExpandedOrCollapsed(
      Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ActionPropertiesExpandUpdate(Sender: TObject);
    procedure ActionPropertiesCollapseUpdate(Sender: TObject);
    procedure PropertiesFrameVirtualStringTreeGetHint(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: WideString);
      Private
         RootComponents: Array Of TCellMLModelRuntimeComponent;

         StateVarsModified: Boolean;
         CstsModified: Boolean;

         RunNb: Integer;

         Duration: Double;
         Output: Double;

         Cell: TCell;
         InitStateVars: PADouble;

         OutputNb: Integer;

         tCrt, tEnd: Double;

         StatusBarStep: Double;

         DurationNode: PVirtualNode;
         OutputNode: PVirtualNode;

         IntegratorNode: PVirtualNode;

         TimeStepNode: PVirtualNode;

         MaxTimeStepNode: PVirtualNode;

         MaxNbOfStepsNode: PVirtualNode;

         MethodNode: PVirtualNode;

         IteratorNode: PVirtualNode;
         LinearSolverNode: PVirtualNode;
         PreconditionerNode: PVirtualNode;
         UpperHalfBandwidthNode: PVirtualNode;
         LowerHalfBandwidthNode: PVirtualNode;

         RelTolNode: PVirtualNode;
         AbsTolNode: PVirtualNode;

         PropertiesFrameVirtualStringTreeCheckedEnabled: Boolean;

         NodeList: Array Of PVirtualNode;

         NodeListSize: Integer;

         ComputingEngineState: TComputingEngineState;

         MCNbOfSteps, VSTStepNb, VSTNbOfSteps: Integer;

         One_VFrequency: Double;

         PropertiesLevel, MaxPropertiesLevel: Integer;

         AtLeastOneNodeNotVisible: Boolean;

         Function CreateCell(Const aCellMLModelRuntime: TCellMLModelRuntime = Nil): Boolean;
         Function DestroyCell: Boolean;

         Procedure SaveFileAndUpdateCell(Const aSaveAs: Boolean);

         Procedure AddConsoleMsg(Const aConsoleMsg: String); Inline;
         Procedure ClearConsoleMsgs; Inline;

         Procedure AddNode(Const aNode: PVirtualNode);
         Procedure RemoveNode(Const aNode: PVirtualNode);

         Procedure ClearAllGraphs(Const aGraphPanel: TOGLGraphPanel);
         Procedure CreateNecessaryGraphs(Const aGraphPanel: TOGLGraphPanel); 

         Procedure ShowLevelOfProperties(Const aLevel: Integer);

         Procedure NbOfVariablesAssociatedToGraphPanel(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);

         Procedure EnableCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);
         Procedure DisableCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);
         Procedure ShowCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);
         Procedure UncheckCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);
         Procedure FreeAdditionalDataInVirtualStringTree(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);
         Procedure UpdateChildrenOfVirtualStringTree(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);
         Procedure DebugDataFromVirtualStringTree(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);

         Procedure UpdateGeneralProperties;
         Procedure UpdateCellProperties;

         Procedure InitCellParams(Const aResetVariablesType: TResetVariablesType = rptAll);

         Procedure AddGraphPanel;
         Procedure RemoveAllGraphPanels(Const aExitMode: Boolean = False);

         Function HasQuickPaintGraphPanels: Boolean;
         Procedure QuickPaintGraphPanels;

         Procedure ExecCellMLFile;

         Procedure UpdateGraphPanelsConnection;

         Procedure ClearAllGraphsInGraphPanel(Const aGraphPanel: TOGLGraphPanel);

         Procedure ClearAllNodeGraphs(Const aNode: PVirtualNode);

         Function RunColor(Const aColor: TColor; Const aRunNb: Integer): TColor;

         Procedure CreateGraph(Const aPropertyData: PPropertyData; Const aPropertyParameter: TPropertyParameter);

         Procedure ExportToCSV(Const aAllGraphPanels: Boolean; Const aOneCSVFile: Boolean = False);

         Function AtLeastOneGraphPresent: Boolean;

         Procedure UpdateTriStateCheckBoxes;

         Procedure DoCellMLFileToMCProgress;

         Procedure FinishAnyEditing; Inline;

         Procedure UpdateCellData; Inline;

         // Methods to modify the different published properties

         Procedure SetActiveGraphPanel(Const aValue: TGraphPanelFrame);

      Protected
         FActiveGraphPanel: TGraphPanelFrame;

         Function GetDockSiteSize(aDockSite: TPanel): Pointer; Override;

      Public
         CellMLModel: TCellMLModel;

         TimeScale, TimeShift: Double;
         TimeUnit: String;

         GraphPanels: Array Of TGraphPanelFrame;

         GraphPanelsSize: Integer;

         Constructor Create(aOwner: TComponent); Override;

         Procedure FrameCreate; Override;

         Procedure BackupToolBarsAndViews; Override;
         Procedure RestoreToolBarsAndViews; Override;

         Function EnterMode: Boolean; Override;
         Function ExitMode: Boolean; Override;

         Procedure RemoveGraphPanel(Const aGraphPanel: TGraphPanelFrame; Const aFree: Boolean);

         Procedure CellMLFileToMCBegin(Const aNbOfSteps: Integer); Inline;
         Procedure CellMLFileToMCProgress(Const aStepNb, aNbOfSteps: Integer); Inline;

         Procedure UpdateOGLGraphPanelSettings(Const aOGLGraphPanel: TOGLGraphPanel);

      Published
         // Published properties

         Property ActiveGraphPanel: TGraphPanelFrame Read FActiveGraphPanel Write SetActiveGraphPanel;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ENDIF}
   Math, SysUtils, DockSites, SyntaxEdit, Main, Msg, Consts, Common, Options,
   Dialogs, ODEForwardEulerIntegrator, ODE2ndOrderRungeKuttaIntegrator,
   ODE4thOrderRungeKuttaIntegrator, ODECVODEIntegrator;

//==============================================================================

Const
   CSV_FORMAT_DEFEXT = 'csv';
   CSV_FORMAT_FILTER = 'Comma Separated Values file (*.csv)|*.csv';

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TComputationFrame.Create(aOwner: TComponent);
Var
   DummyDC: THandle;
Begin
   Inherited;

   // Assign the right dock class for the toolbars and views

   ControlBarPropertiesMenuToolBar.FloatingDockSiteClass := TToolBarDockForm;
   ControlBarToolsMenuToolBar.FloatingDockSiteClass      := TToolBarDockForm;

   // Assign the right dock class for the toolbars and views

   PropertiesFrame.FloatingDockSiteClass := TDockForm;
   ConsoleFrame.FloatingDockSiteClass    := TDockForm;

   // Initial state of the engine

   ComputingEngineState := cesStopped;

   // Miscellaneous

   CellMLModel := Nil;

   SetLength(GraphPanels, 1);

   GraphPanelsSize := 0;

   SetLength(NodeList, 1);

   NodeListSize := 0;

   DummyDC := GetDC(0);

   One_VFrequency := 1/GetDeviceCaps(DummyDC, VREFRESH);   // s

   ReleaseDC(0, DummyDC);
End;

//==============================================================================

Procedure TComputationFrame.AddConsoleMsg(Const aConsoleMsg: String);
Begin
   ConsoleFrame.Console.Add(aConsoleMsg);

   ConsoleFrame.Console.NodeIndex := ConsoleFrame.Console.Count-1;
End;

//==============================================================================

Procedure TComputationFrame.ClearConsoleMsgs;
Begin
   ConsoleFrame.Console.Clear;
End;

//==============================================================================

Procedure TComputationFrame.AddNode(Const aNode: PVirtualNode);
Begin
   If (NodeListSize+1 > Length(NodeList)) Then
      // Not enough space, so make some...

      SetLength(NodeList, 2*Length(NodeList));

   NodeList[NodeListSize] := aNode;

   Inc(NodeListSize);
End;

//==============================================================================

Procedure TComputationFrame.RemoveNode(Const aNode: PVirtualNode);
Var
   I, NodeIter: Integer;
Begin
   NodeIter := -1;

   For I := 0 To NodeListSize-1 Do
      If (Integer(aNode) = Integer(NodeList[I])) Then Begin
         NodeIter := I;

         Break;
      End;

   If (NodeIter <> -1) Then Begin
      For I := NodeIter+1 To NodeListSize-1 Do
         NodeList[I-1] := NodeList[I];

      Dec(NodeListSize);
   End;
End;

//==============================================================================

Procedure TComputationFrame.FrameCreate;
Begin
   Inherited;

   // Toolbars and views

   SetToolBarWidth(ControlBarMenuToolBar);

   SetToolBarWidth(ControlBarFileMenuToolBar);
   SetToolBarWidth(ControlBarPropertiesMenuToolBar);
   SetToolBarWidth(ControlBarRunMenuToolBar);
   SetToolBarWidth(ControlBarToolsMenuToolBar);
End;

//==============================================================================

Procedure TComputationFrame.BackupToolBarsAndViews;
Begin
   // Backup the settings of the computational frame's toolbars and views

   If (Not MainForm.FormCreating) Then Begin
      ComputationFrameControlBarFileMenuToolBarVisible       := ControlBarFileMenuToolBar.Visible;
      ComputationFrameControlBarPropertiesMenuToolBarVisible := ControlBarPropertiesMenuToolBar.Visible;
      ComputationFrameControlBarRunMenuToolBarVisible        := ControlBarRunMenuToolBar.Visible;
      ComputationFrameControlBarToolsMenuToolBarVisible      := ControlBarToolsMenuToolBar.Visible;

      ComputationFramePropertiesFrameVisible := PropertiesFrame.Visible;
      ComputationFrameConsoleFrameVisible    := ConsoleFrame.Visible;
   End;

   // Make the computational frame's toolbars and views invisible (VERY
   // important to do if they are floating!)

   If (ControlBarFileMenuToolBar.Floating) Then
      ControlBarFileMenuToolBar.Visible := False;

   If (ControlBarPropertiesMenuToolBar.Floating) Then
      ControlBarPropertiesMenuToolBar.Visible := False;

   If (ControlBarRunMenuToolBar.Floating) Then
      ControlBarRunMenuToolBar.Visible := False;

   If (ControlBarToolsMenuToolBar.Floating) Then
      ControlBarToolsMenuToolBar.Visible := False;

   If (PropertiesFrame.Floating) Then
      PropertiesFrame.Visible := False;

   If (ConsoleFrame.Floating) Then
      ConsoleFrame.Visible := False;
End;

//==============================================================================

Procedure TComputationFrame.RestoreToolBarsAndViews;
Begin
   // Restore the settings of the computational frame's toolbars and views

   RestoreControl(ControlBarFileMenuToolBar, ComputationFrameControlBarFileMenuToolBarVisible);
   RestoreControl(ControlBarPropertiesMenuToolBar, ComputationFrameControlBarPropertiesMenuToolBarVisible);
   RestoreControl(ControlBarRunMenuToolBar, ComputationFrameControlBarRunMenuToolBarVisible);
   RestoreControl(ControlBarToolsMenuToolBar, ComputationFrameControlBarToolsMenuToolBarVisible);

   RestoreControl(PropertiesFrame, ComputationFramePropertiesFrameVisible);
   RestoreControl(ConsoleFrame, ComputationFrameConsoleFrameVisible);
End;

//==============================================================================

Procedure TComputationFrame.CellMLFileToMCBegin(Const aNbOfSteps: Integer);
Begin
   MCNbOfSteps := aNbOfSteps;

   MainForm.EditorFrame.StatusBarInfoProgBar;
   // Note: the status bar will be indirectly reset in "TMainForm.GoIntoMode"...
End;

//==============================================================================

Procedure TComputationFrame.CellMLFileToMCProgress(Const aStepNb, aNbOfSteps: Integer);
Var
   Shift: Integer;
Begin
   If (aStepNb <> VSTStepNb) Then
      Shift := 0
   Else
      Shift := 50;

   MainForm.EditorFrame.ProgressBar.Position := Shift+Round(50*aStepNb/aNbOfSteps);

   Application.ProcessMessages;
End;

//==============================================================================

Procedure TComputationFrame.DoCellMLFileToMCProgress;
Begin
   Inc(VSTStepNb);

   CellMLFileToMCProgress(VSTStepNb, VSTNbOfSteps);
End;

//==============================================================================

Function TComputationFrame.CreateCell(Const aCellMLModelRuntime: TCellMLModelRuntime): Boolean;
Var
   RealCellMLModelRuntime: TCellMLModelRuntime;
   FVUnitElement: TCellMLUnit;
   Dummy: Double;
   I: Integer;
Begin
   // Convert the CellML file to machine code

   If (aCellMLModelRuntime <> Nil) Then
      RealCellMLModelRuntime := aCellMLModelRuntime
   Else Begin
      RealCellMLModelRuntime := TCellMLModelRuntime.Create;
      // Note: this will be held by "Cell" and "Cell.Runtime" will be released
      //       when destroying the cell, so no need to keep track of
      //       "RealCellMLModelRuntime"...

      With TCellMLAPIToMCEngine.Create(CellMLModel, RealCellMLModelRuntime, ComputationOptions.GeneralOptions.Compiler, ComputationOptions.GeneralOptions.CompilerLocation, CellMLFileToMCBegin, CellMLFileToMCProgress) Do Begin
         Execute;

         Free;
      End;

      // Name of the units in which time is expressed

      TimeUnit := CellMLModel.FreeVariableUnits.Name;

      // Retrieve the attributes of the units in which time is expressed

      CellMLModel.FreeVariableUnits.CanonicalForm;   // Just to be on the safe
                                                     // side

      FVUnitElement := CellMLModel.FreeVariableUnits.GetUnitElement(0);

      FVUnitElement.ConvUnitElemAttr(Dummy, Dummy, TimeScale, TimeShift);

      // Determine the real time scale and shift for converting the free
      // variable from whatever units it is in to milliseconds

      TimeScale := 0.001/TimeScale;
      // Note: "0.001", because we want to convert from milliseconds to whatever
      //       units time is expressed in
   End;

   // Create the virtual cell

   Cell := TCell.Create(RealCellMLModelRuntime);

   // Initialise the virtual cell's parameters

   Try
      InitCellParams;
   Except
      // Cannot initialise the cell, so...

      Result := False;

      // Reset the progress bar

      MainForm.EditorFrame.ProgressBar.Position := 0;

      If (Cell.Runtime.UseMCMethods) Then
         MessageDlg('The CellML file cannot be run. The most likely reason is that DEP (Data Execution Prevention) is turned on. Please turn it off (at least for '+COR_NAME+') and restart '+COR_NAME+'.', mtInformation, [mbOK], 0)
      Else
         MessageDlg('The CellML file cannot be run. There seems to be an issue with the '+String(Compilers.At(Integer(ComputationOptions.GeneralOptions.Compiler)).VString)+' compiler. Please try '+COR_NAME+'''s internal compiler.', mtInformation, [mbOK], 0);

      Exit;
   End;

   // Back up the initial value of its state variables in case (see
   // ExecCellMLFile)

   GetMem(InitStateVars, RealCellMLModelRuntime.NbOfStateVars*SizeOf(Double));

   For I := 0 To Cell.Runtime.NbOfStateVars-1 Do
      InitStateVars[I] := Cell.Y[I];

   Result := True;
End;

//==============================================================================

Function TComputationFrame.DestroyCell: Boolean;
Begin
   // Release previously allocated memory

   If (Cell <> Nil) Then Begin   // Just to be on the safe side...
      Cell.Runtime.Free;   // Has to be released, since it has been created
                           // solely for running this CellML file, so...
      Cell.Integrator.Free;   // Has to be released if we left the computational
                              // mode while still running a simulation, so...

      FreeAndNil(Cell);
      FreeAndNil(CellMLModel);

      Result := True;
   End Else
      Result := False;

   // Release the memory for the model's constants and initial state variables

   FreeMem(InitStateVars);
End;

//==============================================================================

Procedure TComputationFrame.FinishAnyEditing;
Begin
   PropertiesFrame.VirtualStringTree.EndEditNode;

   Application.ProcessMessages;
End;

//==============================================================================

Procedure TComputationFrame.UpdateCellData;
Begin
   Cell.Runtime.Compute(tCrt, Cell.Y, Cell.dY, Cell.Csts, Cell.CompVars);
End;

//==============================================================================

Procedure TComputationFrame.SaveFileAndUpdateCell(Const aSaveAs: Boolean);
   Procedure UpdateVirtualStringTreeInitParams(Const aNode: PVirtualNode);
   Var
      ChildNode: PVirtualNode;
   Begin
      With PropertiesFrame.VirtualStringTree Do
         If (aNode^.ChildCount <> 0) Then Begin
            // A node that has children, so go to its first child

            ChildNode := aNode^.FirstChild;

            Repeat
               If ((aNode <> RootNode) Or (aNode^.FirstChild <> ChildNode)) Then
                  // Not the general properties, so can deal with it...

                  UpdateVirtualStringTreeInitParams(ChildNode);

               ChildNode := ChildNode^.NextSibling;
            Until ChildNode = Nil;
         End Else
            // A child, so update it if it is a state variable or a contant

            With PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode))^ Do
               Case PropType Of
                  ptStateVar:
                     TPropertyParameter(Data).Value := @Cell.Y[Index];
                  ptCst:
                     TPropertyParameter(Data).Value := @Cell.Csts[Index];
               End;
   End;
Var
   CellMLModelRuntime: TCellMLModelRuntime;
Begin
   // Finish any editing of a property, just in case...

   FinishAnyEditing;

   // Update the cell model

   CellMLModelRuntime := TCellMLModelRuntime.Create;

   If ((MainForm.EditorFrame.PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).SaveFile(aSaveAs, PropertiesFrame.VirtualStringTree, CellMLModelRuntime)) Then Begin
      DestroyCell;
      CreateCell(CellMLModelRuntime);

      UpdateCaption;

      UpdateVirtualStringTreeInitParams(PropertiesFrame.VirtualStringTree.RootNode);
   End Else
      CellMLModelRuntime.Free;

   PropertiesFrame.VirtualStringTree.Repaint;   // To refresh the computed
                                                // variables...
End;

//==============================================================================

Function TComputationFrame.EnterMode: Boolean;
Var
   NbOfRootNodes: Integer;
   Iter: Integer;
   CrtComp: TCellMLModelRuntimeComponent;
Begin
   // Clean up the console

   ClearConsoleMsgs;

   If (MainForm.EditorFrame.PageCtrl.ActivePage <> Nil) Then Begin
      If (Not CreateCell) Then Begin
         // The cell couldn't be created (because it uses machine code and the
         // machine code cannot be executed due to Data Execution Protection
         // being turned on)

         Result := False;

         Exit;
      End;

      // Determine the number of root nodes and number of components and
      // variables to initialise

      VSTStepNb    := 0;
      VSTNbOfSteps := 0;
      // Note: very important to initialise it in case the user goes into
      //       computational mode and exit straight away (one would have to be
      //       dumb to do that, but eh!), so...

      NbOfRootNodes := 1;   // For the general node

      For Iter := 0 To Cell.Runtime.ComponentsList.Size-1 Do
         With Cell.Runtime.ComponentsList.At(Iter) Do Begin
            CrtComp := TCellMLModelRuntimeComponent(VObject);

            If (CrtComp.Owner = Nil) Then Begin
               // The current component is one of the top ones within the
               // containment hierarchy, so that's one more root node...

               SetLength(RootComponents, NbOfRootNodes);

               RootComponents[High(RootComponents)] := TCellMLModelRuntimeComponent(VObject);

               Inc(NbOfRootNodes);
            End;

            Inc(VSTNbOfSteps, CrtComp.VariablesList.Size+1);
            // Note: "+1" is for the component itself
         End;

      // Get the virtual string tree to populate itself

      PropertiesFrame.VirtualStringTree.RootNodeCount := NbOfRootNodes;

      PropertiesFrame.VirtualStringTree.ValidateNode(PropertiesFrame.VirtualStringTree.RootNode, True);

      // Add a default graph panel

      AddGraphPanel;
   End;

   // Debug mode

   ActionRunDebugMode.Checked := ComputationOptions.GeneralOptions.DebugMode;

   Result := True;
End;

//==============================================================================

Procedure TComputationFrame.FreeAdditionalDataInVirtualStringTree(aSender: TBaseVirtualTree;
                                                                  aNode: PVirtualNode;
                                                                  aData: Pointer;
                                                                  Var aAbort: Boolean);
Begin
   // Release the memory used by the property's data

   With PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode))^ Do
      Case PropType Of
         ptPickStr:
            FreeAndNil(TPropertyDropDownList(Data));
         ptRangeInteger:
            FreeAndNil(TPropertyRangeInteger(Data));
         ptStateVar, ptCst, ptCompVar:
            FreeAndNil(TPropertyParameter(Data));
      End;
End;

//==============================================================================

Function TComputationFrame.ExitMode: Boolean;
Begin
   DestroyCell;

   // Remove all the graph panels

   RemoveAllGraphPanels(True);

   // Remove the properties after ensuring that the data they contain have been
   // released

   With PropertiesFrame.VirtualStringTree Do Begin
      IterateSubtree(Nil, FreeAdditionalDataInVirtualStringTree, Nil);

      Clear;
   End;

   // Go back to a default status bar
   // Note: we also do that in "ExcelCellMLFile", but it may be that the user
   //       decided to go straight back into editorial mode rather than stopping
   //       the simulation and then going into editorial mode, so when reaching
   //       that point in "ExcelCellMLFile" the frame wouldn't be visible
   //       anymore and the status bar wouldn't be properly reset, so...

   StatusBarInfoHint;

   Result := True;
End;

//==============================================================================

Procedure TComputationFrame.ActionRunRunUpdate(Sender: TObject);
Const
   RUN_CAPTION        = 'Run';
   RUN_HINT           = 'Run|Run the current simulation';
   RUN_IMAGE_INDEX    = 3;
   PAUSE_CAPTION      = 'Pause';
   PAUSE_HINT         = 'Pause|Pause the current simulation';
   PAUSE_IMAGE_INDEX  = 4;
   RESUME_CAPTION     = 'Resume';
   RESUME_HINT        = 'Resume|Resume the current simulation';
   RESUME_IMAGE_INDEX = 3;
Begin
   If (Cell <> Nil) Then
      With Sender As TAction Do Begin
         Case ComputingEngineState Of
            cesPaused: Begin
               Caption := RESUME_CAPTION;

               Hint := RESUME_HINT;

               ImageIndex := RESUME_IMAGE_INDEX;
            End;
            cesStopped: Begin
               Caption := RUN_CAPTION;

               Hint := RUN_HINT;

               ImageIndex := RUN_IMAGE_INDEX;
            End;
         Else   // cesStarted
            Caption := PAUSE_CAPTION;

            Hint := PAUSE_HINT;

            ImageIndex := PAUSE_IMAGE_INDEX;
         End;

         Enabled := (MainForm.Mode = mComputational) And
                    (VSTStepNb = VSTNbOfSteps) And PropertiesFrame.ValidProperty(False);
         // Note: to test "VSTStepNb" is very important, as someone might, for
         //       instance, quickly press F9 twice in a row from the editor,
         //       which would mean going into computation mode and running the
         //       cell model straight away... something that will raise an
         //       error, since not everything has been initialised yet...
      End
   Else
      (Sender As TAction).Enabled := False;
End;

//==============================================================================

Procedure TComputationFrame.InitCellParams(Const aResetVariablesType: TResetVariablesType);
Begin
   // Initialise the state variables and/or constants

   Case aResetVariablesType Of
      rptStateVars: Begin
         Cell.InitStateVars;

         StateVarsModified := False;
      End;
      rptCsts: Begin
         Cell.InitCsts;

         CstsModified := False;
      End;
   Else
      Cell.Init;

      StateVarsModified := False;
      CstsModified      := False;
   End;

   // Reset the computed variables
   // Note #1: it's not critical, but at least they will look ok when going into
   //          computational mode, as they would otherwise get the value of the
   //          previously run model...
   // Note #2: we don't care about the "dY" array, since we don't show those
   //          values to the user...

   Cell.InitCompVars;

   // Execute the code that needs to be computed once

   Cell.ComputeOnce;

   // Initialise the integrator, should it be the CVODE integrator

   If (Cell.Integrator Is TODECVODEIntegrator) Then
      (Cell.Integrator As TODECVODEIntegrator).Init(tCrt);
End;

//==============================================================================

Procedure TComputationFrame.ActionFileExitExecute(Sender: TObject);
Begin
   // Stop the simulation, if necessary, and go back into editorial mode

   ActionRunStopExecute(Sender);

   MainForm.GoIntoMode(mEditorial);
End;

//==============================================================================

Procedure TComputationFrame.ActionFileSaveUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  Not (MainForm.EditorFrame.PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ReadOnly And
                                  (StateVarsModified Or CstsModified);
End;

//==============================================================================

Procedure TComputationFrame.ActionFileSaveAsUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := MainForm.EditorFrame.PageCtrl.ActivePage <> Nil;
End;

//==============================================================================

Procedure TComputationFrame.ActionFileSaveExecute(Sender: TObject);
Begin
   SaveFileAndUpdateCell(False);
End;

//==============================================================================

Procedure TComputationFrame.ActionFileSaveAsExecute(Sender: TObject);
Begin
   SaveFileAndUpdateCell(True);
End;

//==============================================================================

Procedure TComputationFrame.ActionViewExecute(Sender: TObject);
Begin
   If (Sender = ActionViewPropertiesViewer) Then
      ShowHideControl(PropertiesFrame)
   Else
      ShowHideControl(ConsoleFrame);
End;

//==============================================================================

Procedure TComputationFrame.ActionViewVariablesTypeExecute(Sender: TObject);
   Function ShowOrHidePropertiesInVirtualStringTree(aNode: PVirtualNode): Boolean;
   Var
      ChildNode: PVirtualNode;
   Begin
      With PropertiesFrame.VirtualStringTree Do
         If (aNode^.ChildCount <> 0) Then Begin
            // A node that has children, so go to its first child

            Result := False;   // By default

            ChildNode := aNode^.FirstChild;

            Repeat
               If ((aNode = RootNode) And (aNode^.FirstChild = ChildNode)) Then
                  // One of the General properties' nodes, so...

                  Result := True
               Else
                  Result := ShowOrHidePropertiesInVirtualStringTree(ChildNode) Or Result;
                  // Note: it is VERY important to have the "Or" test in that
                  //       way, so that we are sure that we test all the
                  //       children...

               ChildNode := ChildNode^.NextSibling;
            Until ChildNode = Nil;

            If (aNode <> RootNode) Then
               IsVisible[aNode] := Result;
         End Else Begin
            // A child, so only show it if requested

            Case PPropertyData(GetNodeData(aNode))^.PropType Of
               ptStateVar:
                  IsVisible[aNode] := ActionPropertiesVariablesStateVariables.Checked;
               ptCst:
                  IsVisible[aNode] := ActionPropertiesVariablesConstants.Checked;
               ptCompVar:
                  IsVisible[aNode] := ActionPropertiesVariablesComputedVariables.Checked;
            End;

            Result := IsVisible[aNode];
         End;
   End;
Begin
   // (Un)Check the action

   (Sender As TAction).Checked := Not (Sender As TAction).Checked;

   // Finish any editing of a property, as it may otherwise be disastrous...

   FinishAnyEditing;

   // Show/hide the various properties by using a recursive solution, because we
   // need to know whether a parent node ought to be visible or not (depending
   // on whether one or several of its children is shown or not). This cannot be
   // achieved using "IterateSubtree", so...

   With PropertiesFrame.VirtualStringTree Do Begin
      BeginUpdate;
         ShowOrHidePropertiesInVirtualStringTree(RootNode);
      EndUpdate;
   End;

   // Update the tri-state check boxes, so they show their right state, based
   // on the check boxes / properties that are visible

   UpdateTriStateCheckBoxes;

   // Make sure that the focused node is centered, if need be

   With PropertiesFrame.VirtualStringTree Do
      ScrollIntoView(FocusedNode, True);
End;

//==============================================================================

Procedure TComputationFrame.ActionViewUpdate(Sender: TObject);
Begin
   If (Sender = ActionViewPropertiesViewer) Then
      ActionViewPropertiesViewer.Checked := PropertiesFrame.Visible
   Else
      ActionViewConsoleViewer.Checked := ConsoleFrame.Visible;
End;

//==============================================================================

Procedure TComputationFrame.ActionRunRunExecute(Sender: TObject);
Begin
   // Finish any editing of a property, just in case...

   FinishAnyEditing;

   // Start, pause or resume the simulation

   Case ComputingEngineState Of
      cesPaused:
         ComputingEngineState := cesStarted;
      cesStopped:
         // Execute the CellML file

         ExecCellMLFile;
   Else   // cesStarted
      // Compute the model one more time, just to be on the safe side...
      // Note: this is to ensure that all the cell parameters are correct in the
      //       treeview (see note #1 in "OutputData")...

      UpdateCellData;

      If (HasQuickPaintGraphPanels) Then
         QuickPaintGraphPanels;   // Quickly update the graph panels, just to be
                                  // up to date

      ComputingEngineState := cesPaused;
   End;

   PropertiesFrame.VirtualStringTree.Repaint;
End;

//==============================================================================

Procedure TComputationFrame.ActionRunStopExecute(Sender: TObject);
Begin
   // Compute the model one more time, just to be on the safe side...
   // Note: this is to ensure that all the cell parameters are correct in the
   //       treeview (see note #1 in "OutputData")...

   UpdateCellData;

   // Stop the simulation

   ComputingEngineState := cesStopped;
End;

//==============================================================================

Procedure TComputationFrame.ActionRunStopUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  (ComputingEngineState <> cesStopped);
End;

//==============================================================================

Procedure TComputationFrame.ActionRunDebugModeExecute(Sender: TObject);
Begin
   // Do nothing on purpose (otherwise the debug action will be disabled!)
End;

//==============================================================================

Function TComputationFrame.GetDockSiteSize(aDockSite: TPanel): Pointer;
Begin
   If (aDockSite = TopDockSite) Then
      Result := @ComputationFrameTopDockSiteSize
   Else If (aDockSite = LeftDockSite) Then
      Result := @ComputationFrameLeftDockSiteSize
   Else If (aDockSite = BottomDockSite) Then
      Result := @ComputationFrameBottomDockSiteSize
   Else
      Result := @ComputationFrameRightDockSiteSize;
End;

//==============================================================================

Procedure TComputationFrame.NbOfVariablesAssociatedToGraphPanel(aSender: TBaseVirtualTree;
                                                                aNode: PVirtualNode;
                                                                aData: Pointer;
                                                                Var aAbort: Boolean);
Begin
   // Check whether the current variable is associated to the selected graph
   // panel or not

   With PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode))^ Do
      If (PropType In [ptStateVar, ptCst, ptCompVar]) Then
         With TPropertyParameter(Data) Do
            If ((GraphPanel <> Nil) And (GraphPanel = TOGLGraphPanelData(aData^).OGLGraphPanel)) Then
               Inc(TOGLGraphPanelData(aData^).NbOfVariables);
End;

//==============================================================================

Procedure TComputationFrame.UpdateOGLGraphPanelSettings(Const aOGLGraphPanel: TOGLGraphPanel);
   Function ResetGraphColor(Const aColor: TColor): TColor;
   Var
      HSL: THSL;
   Begin
      HSL := ColorToHSL(aColor);

      HSL.L := 60;   // Luminosity for clNavy, clMaroon and clGreen which are
                     // the colours we use for the graphs

      Result := HSLToColor(HSL);
   End;
Var
   Iter: Integer;
   GraphNb, TempRunNb: Integer;
   OGLGraphPanelData: TOGLGraphPanelData;
Begin
   With aOGLGraphPanel Do Begin
      Enabled := False;

      // Background colour

      Color := ComputationOptions.GraphicalPanelOptions.BackgroundColor;

      // Axes colour

      XAxis.LineSpecs.Color := ComputationOptions.GraphicalPanelOptions.AxesColor;
      YAxis.LineSpecs.Color := ComputationOptions.GraphicalPanelOptions.AxesColor;

      // Gridlines colour

      XAxis.Grid.LineSpecs.Color := ComputationOptions.GraphicalPanelOptions.GridlinesColor;
      YAxis.Grid.LineSpecs.Color := ComputationOptions.GraphicalPanelOptions.GridlinesColor;

      // Show axes

      XAxis.LineSpecs.Enabled := ComputationOptions.GraphicalPanelOptions.ShowAxes;
      YAxis.LineSpecs.Enabled := ComputationOptions.GraphicalPanelOptions.ShowAxes;

      // Show gridlines

      XAxis.Grid.LineSpecs.Enabled := ComputationOptions.GraphicalPanelOptions.ShowGridlines;
      YAxis.Grid.LineSpecs.Enabled := ComputationOptions.GraphicalPanelOptions.ShowGridlines;

      // Show labels

      XAxis.CoordsSpecs.Enabled      := XAxis.LineSpecs.Enabled And
                                        ComputationOptions.GraphicalPanelOptions.ShowLabels;
      XAxis.Grid.CoordsSpecs.Enabled := XAxis.LineSpecs.Enabled      And
                                        XAxis.Grid.LineSpecs.Enabled And
                                        ComputationOptions.GraphicalPanelOptions.ShowLabels;

      YAxis.CoordsSpecs.Enabled      := YAxis.LineSpecs.Enabled And
                                        ComputationOptions.GraphicalPanelOptions.ShowLabels;
      YAxis.Grid.CoordsSpecs.Enabled := YAxis.LineSpecs.Enabled      And
                                        YAxis.Grid.LineSpecs.Enabled And
                                        ComputationOptions.GraphicalPanelOptions.ShowLabels;

      // Determine the number of variables associated to the graph panel

      OGLGraphPanelData.OGLGraphPanel := aOGLGraphPanel;
      OGLGraphPanelData.NbOfVariables := 0;

      PropertiesFrame.VirtualStringTree.IterateSubtree(Nil, NbOfVariablesAssociatedToGraphPanel, @OGLGraphPanelData);
      // Note: there is no need for "BeginUpdate" and "EndUpdate", since we are
      //       not affecting the GUI part of the virtual string tree

      // Graphs colour

      If (OGLGraphPanelData.NbOfVariables <> 0) Then Begin
         If (ComputationOptions.GraphicalPanelOptions.UseGradientForTraces) Then Begin
            GraphNb   := 0;
            TempRunNb := 1;

            For Iter := 0 To GraphsListSize-1 Do Begin
               GraphsList[Iter].LineSpecs.Color := RunColor(GraphsList[Iter].LineSpecs.Color, TempRunNb);

               Inc(GraphNb);

               If (GraphNb = OGLGraphPanelData.NbOfVariables) Then Begin
                  GraphNb := 0;

                  Inc(TempRunNb);
               End;
            End;
         End Else
            For Iter := 0 To GraphsListSize-1 Do
               GraphsList[Iter].LineSpecs.Color := ResetGraphColor(GraphsList[Iter].LineSpecs.Color);
      End;

      Enabled := True;

      ForceOGLRepaint;
   End;
End;

//==============================================================================

Procedure TComputationFrame.AddGraphPanel;
Begin
   // Create a panel

   If (GraphPanelsSize+1 > Length(GraphPanels)) Then
      // Not enough space, so make some...

      SetLength(GraphPanels, 2*Length(GraphPanels));

   GraphPanels[GraphPanelsSize] := TGraphPanelFrame.Create(Application);

   Inc(GraphPanelsSize);

   With GraphPanels[GraphPanelsSize-1] Do Begin
      With OGLGraphPanel Do Begin
         Legend := 'Time ('+TimeUnit+')';

         PopupMenu := GraphPanelPopupMenu;

         // Initialise the x axis' max/length values

         Enabled := False;

         If (GraphPanelsSize > 1) Then Begin
            XAxis.Max    := GraphPanels[0].OGLGraphPanel.XAxis.Max;
            XAxis.Length := GraphPanels[0].OGLGraphPanel.XAxis.Length;
         End Else Begin
            XAxis.Max    := Duration;
            XAxis.Length := Duration;
         End;

         Enabled := True;
      End;

      UpdateOGLGraphPanelSettings(OGLGraphPanel);
      // Note: this will force the component to refresh itself

      ActiveGraphPanel := GraphPanels[GraphPanelsSize-1];

      UpdateGraphPanelsConnection;

      ManualDock(CentralDockSite, Nil, alBottom);
   End;

   UpdateCellProperties;
End;

//==============================================================================

Procedure TComputationFrame.RemoveAllGraphPanels(Const aExitMode: Boolean);
Var
   I: Integer;
Begin
   // Hide the central docking site, as this would otheriwse make it graphically
   // unappealing to see all the graph panels being removed one after the other

   CentralDockSite.Visible := False;

   // First: uncheck all parameters that are associated to the graph panels

   PropertiesFrameVirtualStringTreeCheckedEnabled := True;

   With PropertiesFrame.VirtualStringTree Do Begin
      BeginUpdate;
         For I := 0 To GraphPanelsSize-1 Do
            IterateSubtree(Nil, UncheckCheckBoxesInVirtualStringTree, @GraphPanels[I].OGLGraphPanel);
      EndUpdate;
   End;

   PropertiesFrameVirtualStringTreeCheckedEnabled := False;

   // Second: remove the graph panels themselves

   For I := 0 To GraphPanelsSize-1 Do
      GraphPanels[I].Free;

   GraphPanelsSize := 0;

   // No graph panel left, so...

   ActiveGraphPanel := Nil;

   // Show the central docking site back

   CentralDockSite.Visible := True;

   If (Not aExitMode) Then
      // Not about to exit the computational mode, so can update the cell's
      // properties, since we have access to *all* the cell's information...

      UpdateCellProperties;
End;

//==============================================================================

Procedure TComputationFrame.UncheckCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree;
                                                                 aNode: PVirtualNode;
                                                                 aData: Pointer;
                                                                 Var aAbort: Boolean);
Begin
   With PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode))^ Do
      If (CheckType = ctCheckBox) Then
         With TPropertyParameter(Data) Do
            If (GraphPanel = TOGLGraphPanel(aData^)) Then Begin
               aNode^.CheckState := csUncheckedNormal;

               // Ensure that whatever needs to be done when a property is
               // unchecked is actually done

               PropertiesFrameVirtualStringTreeChecked(aSender, aNode);
               // Note: "PropertiesFrameVirtualStringTreeCheckedEnabled" has
               //       been set to "True", so we are fine...
            End;
End;

//==============================================================================

Procedure TComputationFrame.RemoveGraphPanel(Const aGraphPanel: TGraphPanelFrame;
                                             Const aFree: Boolean);
Var
   ActiveGP, NextActiveGP: TGraphPanelFrame;
   IsActiveGP: Boolean;
   I, Shift: Integer;
Begin
   // Active the graph panel that is to be removed
   // Note: this is very important to ensure that all the parameters that are
   //       associated to it are completely "released"...

   ActiveGP := ActiveGraphPanel;

   IsActiveGP := ActiveGP = aGraphPanel;

   ActiveGraphPanel := aGraphPanel;

   // First: uncheck all parameters that are associated to the graph panel

   PropertiesFrameVirtualStringTreeCheckedEnabled := True;

   With PropertiesFrame.VirtualStringTree Do Begin
      BeginUpdate;
         IterateSubtree(Nil, UncheckCheckBoxesInVirtualStringTree, @aGraphPanel.OGLGraphPanel);
      EndUpdate;
   End;

   PropertiesFrameVirtualStringTreeCheckedEnabled := False;

   // Second: remove the graph panel itself

   If (aFree) Then
      aGraphPanel.Free;

   Shift := 0;

   NextActiveGP := Nil;

   For I := 0 To GraphPanelsSize-1 Do
      If (GraphPanels[I] = aGraphPanel) Then Begin
         Shift := -1;

         If (IsActiveGP And (GraphPanelsSize > 1)) Then Begin
            If (I = 0) Then
               NextActiveGP := GraphPanels[1]
            Else
               NextActiveGP := GraphPanels[I-1];
         End;
      End Else
         GraphPanels[I+Shift] := GraphPanels[I];

   Dec(GraphPanelsSize);

   // Set the new active graph panel
   // Note: by doing so, we call "UpdateTriStateCheckBoxesW, so no need to call
   //       it here...

   If (IsActiveGP) Then
      ActiveGraphPanel := NextActiveGP
   Else
      ActiveGraphPanel := ActiveGP;

   UpdateGraphPanelsConnection;

   UpdateCellProperties;
End;

//==============================================================================

Procedure TComputationFrame.UpdateGeneralProperties;
Var
   LinearSolverIndex: Integer;
   CVODEIntegrator: Boolean;
   CVODENewtonIterator: Boolean;
   CVODEBandedLinearSolver: Boolean;
   CVODEKrylovLinearSolver: Boolean;
   CVODEBandedPreconditioner: Boolean;
Begin
   With PropertiesFrame.VirtualStringTree Do Begin
      LinearSolverIndex := TPropertyDropDownList(PPropertyData(GetNodeData(LinearSolverNode))^.Data).Index;

      CVODEIntegrator := TPropertyDropDownList(PPropertyData(GetNodeData(IntegratorNode))^.Data).Index = Integer(itCVODE);
      CVODENewtonIterator := CVODEIntegrator And (TPropertyDropDownList(PPropertyData(GetNodeData(IteratorNode))^.Data).Index = Integer(iNewton)-1);
      // Note: "-1" because "TODECVODEIntegratorIterator" is 1-based, as opposed
      //       to 0-based...
      CVODEBandedLinearSolver := CVODENewtonIterator And (LinearSolverIndex = Integer(lsBanded));
      CVODEKrylovLinearSolver := CVODENewtonIterator And ((LinearSolverIndex = Integer(lsGMRES)) Or (LinearSolverIndex = Integer(lsBiCGStab)) Or (LinearSolverIndex = Integer(lsTFQMR)));
      CVODEBandedPreconditioner := CVODEKrylovLinearSolver And (TPropertyDropDownList(PPropertyData(GetNodeData(PreconditionerNode))^.Data).Index = Integer(pBanded));

      IsVisible[TimeStepNode] := Not CVODEIntegrator;

      IsVisible[MaxTimeStepNode] := CVODEIntegrator;

      IsVisible[MaxNbOfStepsNode] := CVODEIntegrator;

      IsVisible[MethodNode] := CVODEIntegrator;

      IsVisible[IteratorNode]           := CVODEIntegrator;
      IsVisible[LinearSolverNode]       := CVODENewtonIterator;
      IsVisible[PreconditionerNode]     := CVODEKrylovLinearSolver;
      IsVisible[UpperHalfBandwidthNode] := CVODEBandedLinearSolver Or CVODEBandedPreconditioner;
      IsVisible[LowerHalfBandwidthNode] := CVODEBandedLinearSolver Or CVODEBandedPreconditioner;

      IsVisible[RelTolNode] := CVODEIntegrator;
      IsVisible[AbsTolNode] := CVODEIntegrator;
   End;
End;

//==============================================================================

Procedure TComputationFrame.ClearAllGraphs(Const aGraphPanel: TOGLGraphPanel);
Var
   Iter: Integer;
Begin
   // Note: wherever we call this procedure, we normally would have a call to
   //       something like:
   //
   //       PropertiesFrame.VirtualStringTree.IterateSubtree(Nil, ClearAllGraphsForVirtualStringTree, @aGraphPanel);
   //
   //       However, such a solution is time consuming, as it would go through
   //       all the nodes of VirtualStringTree, while here we only go through
   //       the nodes that actually need a graph...

   For Iter := 0 To NodeListSize-1 Do
      If (TPropertyParameter(PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(NodeList[Iter]))^.Data).GraphPanel = aGraphPanel) Then
         ClearAllNodeGraphs(NodeList[Iter]);
End;

//==============================================================================

Procedure TComputationFrame.CreateNecessaryGraphs(Const aGraphPanel: TOGLGraphPanel);
Var
   Iter: Integer;
   PropertyData: PPropertyData;
   PropertyParameter: TPropertyParameter;
Begin
   // Note: wherever we call this procedure, we normally would have a call to
   //       something like:
   //
   //       PropertiesFrame.VirtualStringTree.IterateSubtree(Nil, CreateNecessaryGraphsForVirtualStringTree, Nil);
   //
   //       However, such a solution is time consuming, as it would go through
   //       all the nodes of VirtualStringTree, while here we only go through
   //       the nodes that actually need a graph...

   For Iter := 0 To NodeListSize-1 Do Begin
      PropertyData      := PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(NodeList[Iter]));
      PropertyParameter := TPropertyParameter(PropertyData^.Data);

      If ((PropertyParameter.GraphPanel <> Nil) And
          ((aGraphPanel = Nil) Or (PropertyParameter.GraphPanel = aGraphPanel))) Then
         CreateGraph(PropertyData, PropertyParameter);
   End;
End;

//==============================================================================

Procedure TComputationFrame.EnableCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree;
                                                                aNode: PVirtualNode;
                                                                aData: Pointer;
                                                                Var aAbort: Boolean);
Begin
   aNode^.CheckType := PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode))^.CheckType;
End;

//==============================================================================

Procedure TComputationFrame.DisableCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree;
                                                                 aNode: PVirtualNode;
                                                                 aData: Pointer;
                                                                 Var aAbort: Boolean);
Begin
   aNode^.CheckType := ctNone
End;

//==============================================================================

Procedure TComputationFrame.UpdateCellProperties;
Begin
   // Enable or disable all the check boxes, depending on the number of graph
   // panels. Note that we only test for zero or one graph panel, as there is
   // no point in testing for two or more graph panels (the check boxes will
   // already be enabled in those cases)...

   Case GraphPanelsSize Of
      0: Begin
         // There are no graph panels, so disable all the check boxes

         CentralDockSite.PopupMenu := GraphPanelPopupMenu;

         ActiveGraphPanel := Nil;

         With PropertiesFrame.VirtualStringTree Do Begin
            BeginUpdate;
               IterateSubtree(Nil, DisableCheckBoxesInVirtualStringTree, Nil);
            EndUpdate;
         End;
      End;
      1: Begin
         // There is one graph panel, so enable all the check boxes

         CentralDockSite.PopupMenu := Nil;

         With PropertiesFrame.VirtualStringTree Do Begin
            BeginUpdate;
               IterateSubtree(Nil, EnableCheckBoxesInVirtualStringTree, Nil);
            EndUpdate;
         End;
      End;
   End;
End;

//==============================================================================

Function TComputationFrame.HasQuickPaintGraphPanels;
Var
   I: Integer;
Begin
   Result := False;

   For I := 0 To GraphPanelsSize-1 Do
      If (GraphPanels[I].OGLGraphPanel.HasQuickGraphs) Then Begin
         Result := True;

         Exit;
      End;
End;

//==============================================================================

Procedure TComputationFrame.QuickPaintGraphPanels;
Var
   I: Integer;
Begin
   For I := 0 To GraphPanelsSize-1 Do
      GraphPanels[I].OGLGraphPanel.QuickPaint;
End;

//==============================================================================

Procedure TComputationFrame.DebugDataFromVirtualStringTree(aSender: TBaseVirtualTree;
                                                           aNode: PVirtualNode;
                                                           aData: Pointer;
                                                           Var aAbort: Boolean);
Var
   ValueIsNaN, ValueIsInfinite: Boolean;
   ErrorMsg: String;
Begin
   // Release the memory used by the property's data

   With PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode))^ Do
      If (PropType In [ptStateVar, ptCst, ptCompVar]) Then Begin
         If (TDebugData(aData^).NeedCellCompute And (PropType = ptCompVar)) Then Begin
            UpdateCellData;

            TDebugData(aData^).NeedCellCompute := False;
         End;

         With TPropertyParameter(Data) Do Begin
            ValueIsNaN      := IsNaN(Value^);
            ValueIsInfinite := IsInfinite(Value^);

            If (Not ValueIsNaN And Not ValueIsInfinite) Then Begin
               If (CheckState = csCheckedNormal) Then
                  Graphs[GraphsSize-1].AddPt(Value^);
            End Else Begin
               ErrorMsg := '[Warning] The ';

               Case PropType Of
                  ptCst:
                     ErrorMsg := ErrorMsg+'constant';
                  ptCompVar:
                     ErrorMsg := ErrorMsg+'computed variable';
               Else
                  // State variable

                  ErrorMsg := ErrorMsg+'state variable';
               End;

               ErrorMsg := ErrorMsg+' '''+Name+''' defined in '''+Owner+''' has';

               If (ValueIsNaN) Then
                  ErrorMsg := ErrorMsg+' a value of NaN.'
               Else
                  ErrorMsg := ErrorMsg+' an infinite value.';

               AddConsoleMsg(ErrorMsg);

               TDebugData(aData^).Result := False;
            End;
         End;
      End;
End;

//==============================================================================

Procedure TComputationFrame.ExecCellMLFile;
Var
   OldFlushTime: Int64;
   FlushTimeThreshold: Double;
   Function ConvTimeToFVUnits(Const aTime: Double): Double;
   Begin
      // Convert from milliseconds to whatever units time is expressed in the
      // model
      // Note: from Jonathan Cooper in DOI: 10.1002/spe.828, to convert an
      //       expression e with units u to an expression with units v, we must
      //       use the following formula: m[v*u^-1](e[u]-o_u[u])+o_v[v], where
      //       m = m_u/m_v, m_x the multiplier of units x, and o_x the offset of
      //       units x. Now, it happens that units u is milliseconds, so m_u =
      //       0.001 and o_u = 0. Also, when determining TimeScale, we took into
      //       account m_u, so TimeScale is m above, while TimeShift is o_v,
      //       hence the formula below... 

      Result := TimeScale*aTime+TimeShift;
   End;
   Procedure UpdateGUI;
   Begin
      // Note: we have to manually ask for the following actions to be updated.
      //       Things just go too fast, so the system never gets around updating
      //       them...

      ActionRunRun.OnUpdate(ActionRunRun);
      ActionRunStop.OnUpdate(ActionRunStop);

      ActionToolsResetVariables.OnUpdate(ActionToolsResetVariables);
      ActionToolsResetVariablesStateVars.OnUpdate(ActionToolsResetVariablesStateVars);
      ActionToolsResetVariablesCsts.OnUpdate(ActionToolsResetVariablesCsts);
      ActionToolsResetVariablesAll.OnUpdate(ActionToolsResetVariablesAll);

      ActionToolsRemoveGraphPanel.OnUpdate(ActionToolsRemoveGraphPanel);
      ActionToolsRemoveGraphPanelAll.OnUpdate(ActionToolsRemoveGraphPanelAll);
      ActionToolsRemoveGraphPanelCurrent.OnUpdate(ActionToolsRemoveGraphPanelCurrent);

      ActionToolsClearAllGraphs.OnUpdate(ActionToolsClearAllGraphs);
      ActionToolsClearAllGraphsInCurrentGraphPanel.OnUpdate(ActionToolsClearAllGraphsInCurrentGraphPanel);
      ActionToolsClearAllGraphsInAllGraphPanels.OnUpdate(ActionToolsClearAllGraphsInAllGraphPanels);

      ActionToolsExportToCSV.OnUpdate(ActionToolsExportToCSV);
      ActionToolsExportToCSVCurrentGraphPanel.OnUpdate(ActionToolsExportToCSVCurrentGraphPanel);
      ActionToolsExportToCSVAllGraphPanels.OnUpdate(ActionToolsExportToCSVAllGraphPanels);

      // Process any pending message

      Application.ProcessMessages;
   End;
   Function OutputData: Boolean;
   Var
      NeedCellCompute: Boolean;
      Iter: Integer;
      CrtFlushTime: Int64;
      DebugData: TDebugData;
   Begin
      // Compute the model
      // Note #1: when outputting the data, we know for sure that the state
      //          variables are up to date, but we cannot be sure about computed
      //          variables. Indeed, say that you have a stimulus current that
      //          starts at 0 ms and finishes at 0.5 ms and that you are
      //          outputting data every 0.1 ms. When you will first come here,
      //          the model won't have been executed at all and, therefore the
      //          stimulus current will be equal to zero, while it shouldn't
      //          since the stimulus current starts at 0 ms. Similarly, when
      //          computing the data at 0.5 ms, the value of the current
      //          stimulus will be that at 0.5 ms minus the time step used by
      //          the integrator, hence having to compute the model to ensure
      //          that all the computed parameters are up to date...
      // Note #2: this will obviously slow things down a tiny bit, hence we try
      //          to minimise the overhead by only computing the cell model if
      //          required...

      // Go through the nodes that need outputting and output their latest value

      If (ActionRunDebugMode.Checked) Then Begin
         With DebugData Do Begin
            NeedCellCompute := True;

            Result := True;
         End;

         PropertiesFrame.VirtualStringTree.IterateSubtree(Nil, DebugDataFromVirtualStringTree, @DebugData);

         Result := DebugData.Result;
      End Else Begin
         NeedCellCompute := True;

         For Iter := 0 To NodeListSize-1 Do
            With PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(NodeList[Iter]))^ Do Begin
               If (NeedCellCompute And (PropType = ptCompVar)) Then Begin
                  UpdateCellData;

                  NeedCellCompute := False;
               End;

               With TPropertyParameter(Data) Do
                  Graphs[GraphsSize-1].AddPt(Value^)
            End;

         Result := True;
      End;

      // Quickly update the graph panels, but only if necessary

      If (HasQuickPaintGraphPanels) Then Begin
         CrtFlushTime := TimerVal;

         If ((CrtFlushTime-OldFlushTime >= FlushTimeThreshold) Or
             (ActionRunDebugMode.Checked And Not Result))Then Begin
            // Note: force the quick update in case of a problem with the model

            QuickPaintGraphPanels;

            OldFlushTime := CrtFlushTime;
         End;
      End;

      // Update the status bar

      StatusBarInfoProgBar(Round(tCrt*StatusBarStep));
   End;
Var
   SimulationStartingTime: Int64;
   I, Iter: Integer;
   OldXAxisMax, OldXAxisLength: Double;
   TimeStep, MaxTimeStep, RelTol, AbsTol: Double;
   MaxNbOfSteps: Integer;
   Integrator: TODEIntegratorType;
   Method: TODECVODEIntegratorMethod;
   Iterator: TODECVODEIntegratorIterator;
   LinearSolver: TODECVODEIntegratorLinearSolver;
   Preconditioner: TODECVODEIntegratorPreconditioner;
   UpperHalfBandwidth, LowerHalfBandwidth: Integer;
   ErrorMsg: String;
Begin
   // Everything's ok, so get the parameters for the single cell model, after
   // disabling the general group box

   With PropertiesFrame.VirtualStringTree Do Begin
      IsVisible[RootNode.FirstChild] := False;

      Duration := StrToFloat(PPropertyData(GetNodeData(DurationNode))^.Value);
      Output   := StrToFloat(PPropertyData(GetNodeData(OutputNode))^.Value);

      Integrator := TODEIntegratorType(TPropertyDropDownList(PPropertyData(GetNodeData(IntegratorNode))^.Data).Index);

      TimeStep := StrToFloat(PPropertyData(GetNodeData(TimeStepNode))^.Value);

      MaxTimeStep := StrToFloat(PPropertyData(GetNodeData(MaxTimeStepNode))^.Value);

      MaxNbOfSteps := StrToInt(PPropertyData(GetNodeData(MaxNbOfStepsNode))^.Value);

      Method := TODECVODEIntegratorMethod(TPropertyDropDownList(PPropertyData(GetNodeData(MethodNode))^.Data).Index+1);
      // Note: "+1" because "TODECVODEIntegratorMethod" is 1-based, as opposed
      //       to 0-based...

      Iterator           := TODECVODEIntegratorIterator(TPropertyDropDownList(PPropertyData(GetNodeData(IteratorNode))^.Data).Index+1);
      // Note: "+1" because "TODECVODEIntegratorIterator" is 1-based, as opposed
      //       to 0-based...
      LinearSolver       := TODECVODEIntegratorLinearSolver(TPropertyDropDownList(PPropertyData(GetNodeData(LinearSolverNode))^.Data).Index);
      Preconditioner     := TODECVODEIntegratorPreconditioner(TPropertyDropDownList(PPropertyData(GetNodeData(PreconditionerNode))^.Data).Index);
      UpperHalfBandwidth := StrToInt(PPropertyData(GetNodeData(UpperHalfBandwidthNode))^.Value);
      LowerHalfBandwidth := StrToInt(PPropertyData(GetNodeData(LowerHalfBandwidthNode))^.Value);

      RelTol := StrToFloat(PPropertyData(GetNodeData(RelTolNode))^.Value);
      AbsTol := StrToFloat(PPropertyData(GetNodeData(AbsTolNode))^.Value);
   End;

   // Convert the duration of the simulation to whatever time unit the cell
   // model uses, as well as determine the number of steps and initialise the
   // step number

   Duration := ConvTimeToFVUnits(Duration);

{$IFDEF OPT_MATH}
   Output := OptMinD(ConvTimeToFVUnits(Output), Duration);
{$ELSE}
   Output := Min(ConvTimeToFVUnits(Output), Duration);
{$ENDIF}

   FlushTimeThreshold := 0.5*QPCFrequency*One_VFrequency;
   // Note: "0.5" because we want to refresh faster than the actual vertical
   //       frequency

   // Get the graph panel's x axis' length right

   If (Not AtLeastOneGraphPresent) Then
      RunNb := 0;

   For I := 0 To GraphPanelsSize-1 Do
      With GraphPanels[I].OGLGraphPanel Do Begin
         OldXAxisMax    := XAxis.Max;
         OldXAxisLength := XAxis.Length;

         Enabled := False;

         If (RunNb = 0) Then
            XAxis.Max := Duration
         Else
{$IFDEF OPT_MATH}
            XAxis.Max := OptMaxD(XAxis.Max, Duration);
{$ELSE}
            XAxis.Max := Max(XAxis.Max, Duration);
{$ENDIF}

         XAxis.Length := Duration;

         Enabled := True;

         // Refresh the graph panel, but only if really necessary as it may take
         // time (when there are already lots of long graphs on it for instance)

         If ((OldXAxisMax <> XAxis.Max) Or (OldXAxisLength <> XAxis.Length)) Then
            ForceOGLRepaint;
      End;

   // Clear the console

   ConsoleFrame.Console.Clear;

   // Setup the integrator

   Case Integrator Of
      itForwardEuler:
         Cell.Integrator := TODEForwardEulerIntegrator.Create(ConvTimeToFVUnits(TimeStep));
      it2ndOrderRungeKutta:
         Cell.Integrator := TODE2ndOrderRungeKuttaIntegrator.Create(Cell.Runtime.NbOfStateVars, ConvTimeToFVUnits(TimeStep));
      it4thOrderRungeKutta:
         Cell.Integrator := TODE4thOrderRungeKuttaIntegrator.Create(Cell.Runtime.NbOfStateVars, ConvTimeToFVUnits(TimeStep));
   Else
      // itCVODE

      Cell.Integrator := TODECVODEIntegrator.Create(@Cell, ConvTimeToFVUnits(MaxTimeStep), MaxNbOfSteps, Method, Iterator, LinearSolver, Preconditioner, UpperHalfBandwidth, LowerHalfBandwidth, RelTol, AbsTol);
   End;

   // Initialise the progress bar

   StatusBarInfoProgBar;

   StatusBarStep := 100/Duration;

   // Some general initialisations

   tCrt := 0;

   OutputNb := 0;

   // Get ready for the run, i.e. increment the number of runs and create new
   // graphs

   Inc(RunNb);

   // Create the necessary graphs

   CreateNecessaryGraphs(Nil);

   // We are all set, so execute the CellML file

   ComputingEngineState := cesStarted;

   SimulationStartingTime := TimerVal;

   Try
      // Initialise the cell and the integrator

      Cell.InitCompVars;
      // Note: this is to ensure that the computed variables are reset to zero,
      //       in case a previous simulation failed because of one or several
      //       computed variable ending up with a value of "NAN"

      Cell.ComputeOnce;
      // Note: because of the previous call, we also have to make this one...

      If (Cell.Integrator Is TODECVODEIntegrator) Then
         (Cell.Integrator As TODECVODEIntegrator).Init(tCrt);

      // First output of the data

      If (OutputData) Then Begin
         // Compute the model

         Inc(OutputNb);

         Repeat
            UpdateGUI;

            If (ComputingEngineState = cesStarted) Then Begin
{$IFDEF OPT_MATH}
               tEnd := OptMinD(OutputNb*Output, Duration);
{$ELSE}
               tEnd := Min(OutputNb*Output, Duration);
{$ENDIF}
               // Note: to have something like "tEnd := tCrt+Output;" is not
               //       good enough because of floating point accuracy issues,
               //       so...

               Inc(OutputNb);   // Ready for the next time step (very important
                                // for when creating new graphs in the middle of
                                // a simulation)

               Cell.Compute(tCrt, tEnd);

               StateVarsModified := True;

               If (Not OutputData) Then Begin
                  UpdateGUI;

                  Break;
               End;
            End;
         Until (tCrt = Duration) Or (ComputingEngineState = cesStopped);
      End;
   Except
      On E: CVODEException Do
         ErrorMsg := 'Problem with the CVODE integrator: '+E.Message;
      On CellException Do
         ErrorMsg := 'Problem with the integrator: you may want to reduce its time step.';
   End;

   If (Visible) Then Begin
      // Only do the following if we are still in computational mode (see
      // "ExitMode")... 

      // Display the simulation time

      AddConsoleMsg('Simulation time: '+SimplifyNb(0.001*Round(TimerValToMSec(TimerVal-SimulationStartingTime)))+' s');

      If (CompareStr(ErrorMsg, '') <> 0) Then Begin
         // There has been a problem, so display the error message. We also need
         // to know whether the state variables have been modified or not. It's
         // true that StateVarsModified is modified in the Repeat...Until loop
         // above, but only if we can compute the model at least once, so if it
         // crashes straightaway we are doomed, so...

         If (Not StateVarsModified) Then
            // It would seem that the state variables haven't been modified, but
            // is that really the case?...

            For I := 0 To Cell.Runtime.NbOfStateVars-1 Do
               If (Cell.Y[I] <> InitStateVars[I]) Then Begin
                  StateVarsModified := True;

                  Break;
               End;

         AddConsoleMsg(ErrorMsg);
      End;

      // Compute the model one more time, just to be on the safe side...
      // Note: this is to ensure that all the cell parameters are correct in the
      //       treeview (see note #1 in "OutputData")...

      UpdateCellData;

      // Kind of disable graphs, if any, so that the next run can be as quick as
      // any previous one. Indeed, to have "QuickPaintEnabled" set to true will
      // make the rendering of the graph panels slower the more runs there
      // are...

      If (HasQuickPaintGraphPanels) Then
         QuickPaintGraphPanels;   // To be up to date

      For I := 0 To GraphPanelsSize-1 Do
         With GraphPanels[I].OGLGraphPanel Do Begin
            For Iter := 0 To GraphsListSize-1 Do
               GraphsList[Iter].QuickPaintEnabled := False;

            FinaliseQuickPaint;
         End;

      // Release the integrator, wether or not the simulation has been completed

      Cell.Integrator.Free;

      Cell.Integrator := Nil;   // To avoid problems later on when destroying
                                // the cell...

      // The simulation  has completed, so...

      ComputingEngineState := cesStopped;

      // Finish any editing of a property

      FinishAnyEditing;

      // Re-enable the general group box

      Try
         PropertiesFrame.VirtualStringTree.IsVisible[PropertiesFrame.VirtualStringTree.RootNode.FirstChild] := True;
      Except
      End;

      // Go back to a default status bar

      StatusBarInfoHint;
   End;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsResetVariablesExecute(Sender: TObject);
Begin
   // Finish any editing of a property, as it will otherwise not be reset...

   FinishAnyEditing;

   // Reset the required parameters

   If (Sender = ActionToolsResetVariablesStateVars) Then
      InitCellParams(rptStateVars)
   Else If (Sender = ActionToolsResetVariablesCsts) Then
      InitCellParams(rptCsts)
   Else
      InitCellParams;

   PropertiesFrame.VirtualStringTree.Repaint;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsResetVariablesAllUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  StateVarsModified Or CstsModified;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsResetVariablesStateVarsUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  StateVarsModified;
End;

//==============================================================================

Procedure TComputationFrame.ShowLevelOfProperties(Const aLevel: Integer);
   Procedure ShowLevelInVirtualStringTree(aNode: PVirtualNode;
                                          Const aCrtLevel, aTargetLevel: Integer);
   Var
      ChildNode: PVirtualNode;
   Begin
      With PropertiesFrame.VirtualStringTree Do
         If (aCrtLevel < aTargetLevel) Then Begin
            If (aNode <> RootNode) Then
               FullExpand(aNode);

            If (aNode^.ChildCount <> 0) Then Begin
               // A node that has children, so go to its first child

               ChildNode := aNode^.FirstChild;

               Repeat
                  If ((aNode <> RootNode) Or (aNode^.FirstChild <> ChildNode)) Then
                     ShowLevelInVirtualStringTree(ChildNode, aCrtLevel+1, aTargetLevel);

                  ChildNode := ChildNode^.NextSibling;
               Until ChildNode = Nil;
            End;
         End Else If (aCrtLevel = aTargetLevel) Then
            FullCollapse(aNode);
   End;
Begin
   // Check whether a given component should be expanded or collapsed based on
   // its level. Note that we would normally use "IterateSubtree", but we can't
   // because we need to determine the level of a component, something that
   // cannot be (easily?) done using "IterateSubtree", hence we use a recursive
   // approach instead...

   With PropertiesFrame.VirtualStringTree Do Begin
      BeginUpdate;
         ShowLevelInVirtualStringTree(RootNode, 0, aLevel);
      EndUpdate;
   End;

   PropertiesLevel := aLevel;
End;

//==============================================================================

Procedure TComputationFrame.ActionPropertiesExpandExecute(Sender: TObject);
Begin
{$IFDEF OPT_MATH}
   ShowLevelOfProperties(OptMinI(MaxPropertiesLevel, PropertiesLevel+1));
{$ELSE}
   ShowLevelOfProperties(Min(MaxPropertiesLevel, PropertiesLevel+1));
{$ENDIF}
End;

//==============================================================================

Procedure TComputationFrame.ActionPropertiesCollapseExecute(Sender: TObject);
Begin
   ShowLevelOfProperties(PropertiesLevel-1);
End;

//==============================================================================

Procedure TComputationFrame.ActionPropertiesExpandUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := ((PropertiesLevel < MaxPropertiesLevel) Or AtLeastOneNodeNotVisible) And
                                  (ActionPropertiesVariablesStateVariables.Checked Or ActionPropertiesVariablesConstants.Checked Or ActionPropertiesVariablesComputedVariables.Checked);
End;

//==============================================================================

Procedure TComputationFrame.ActionPropertiesCollapseUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (PropertiesLevel > 1) And
                                  (ActionPropertiesVariablesStateVariables.Checked Or ActionPropertiesVariablesConstants.Checked Or ActionPropertiesVariablesComputedVariables.Checked);
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeExpandedOrCollapsed(Sender: TBaseVirtualTree;
                                                                                Node: PVirtualNode);
   Procedure DetermineLevelInVirtualStringTree(aNode: PVirtualNode;
                                               Const aCrtLevel: Integer;
                                               Var aLevel, aMaxLevel: Integer);
   Var
      ChildNode: PVirtualNode;
   Begin
      With PropertiesFrame.VirtualStringTree Do Begin
         If (aNode^.ChildCount <> 0) Then Begin
            // A node that has children, so go to its first child

            ChildNode := aNode^.FirstChild;

            Repeat
               If ((aNode <> RootNode) Or (aNode^.FirstChild <> ChildNode)) Then
                  // Not the general properties, so can deal with it...

                  DetermineLevelInVirtualStringTree(ChildNode, aCrtLevel+1, aLevel, aMaxLevel);

               ChildNode := ChildNode^.NextSibling;
            Until ChildNode = Nil;
         End;

         If (FullyVisible[aNode]) Then
{$IFDEF OPT_MATH}
            aLevel := OptMaxI(aLevel, aCrtLevel)
{$ELSE}
            aLevel := Max(aLevel, aCrtLevel)
{$ENDIF}
         Else
            AtLeastOneNodeNotVisible := True;

{$IFDEF OPT_MATH}
         aMaxLevel := OptMaxI(aMaxLevel, aCrtLevel);
{$ELSE}
         aMaxLevel := Max(aMaxLevel, aCrtLevel);
{$ENDIF}
      End;
   End;
Begin
   // Determine the level at which the properties are listed

   PropertiesLevel    := 0;
   MaxPropertiesLevel := 0;

   AtLeastOneNodeNotVisible := False;

   DetermineLevelInVirtualStringTree(PropertiesFrame.VirtualStringTree.RootNode, 0, PropertiesLevel, MaxPropertiesLevel);
End;

//==============================================================================

Procedure TComputationFrame.ActionUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational);
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsResetVariablesCstsUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  CstsModified;
End;

//==============================================================================

Procedure TComputationFrame.ClearAllNodeGraphs(Const aNode: PVirtualNode);
Var
   I: Integer;
   PropertyData: PPropertyData;
   PropertyParameter: TPropertyParameter;
Begin
   PropertyData      := PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode));
   PropertyParameter := TPropertyParameter(PropertyData^.Data);

   If (PropertyParameter <> Nil) Then
      With PropertyParameter Do Begin
         For I := 0 To GraphsSize-1 Do
            GraphPanel.RemoveGraph(Graphs[I].Name);

         GraphsSize := 0;

         // Remove the property data from the list, but only if the parameter in
         // question really shouldn't have graphs associated to it

         If (PropertyData^.CheckState <> csCheckedNormal) Then
            RemoveNode(aNode);
      End;
End;

//==============================================================================

Function TComputationFrame.RunColor(Const aColor: TColor;
                                    Const aRunNb: Integer): TColor;
Const
   MAX_RUN_NB = 3;
Var
   HSL: THSL;
   RealRunNb: Integer;
Begin
   If (ComputationOptions.GraphicalPanelOptions.UseGradientForTraces) Then Begin
      HSL := ColorToHSL(aColor);

      RealRunNb := aRunNb Mod MAX_RUN_NB;

      If (RealRunNb = 0) Then
         RealRunNb := MAX_RUN_NB;

      Case RealRunNb Of
         2: HSL.L := 160;
         3: HSL.L := 192;
      End;

      Result := HSLToColor(HSL);
   End Else
      Result := aColor;
End;

//==============================================================================

Procedure TComputationFrame.CreateGraph(Const aPropertyData: PPropertyData;
                                        Const aPropertyParameter: TPropertyParameter);
Var
   FullName: String;
   RunNbStr: String;
Begin
   With aPropertyParameter Do Begin
      If (GraphsSize+1 > Length(Graphs)) Then
         // Not enough space, so make some...

         SetLength(Graphs, 2*Length(Graphs));

      FullName := aPropertyData^.Owner+'|'+aPropertyData^.Name;

{$IFDEF OPT_MATH}
      RunNbStr := StringOfChar('0', OptFloor(OptLog10(MaxInt))-OptFloor(OptLog10(RunNb)))+IntToStr(RunNb);
{$ELSE}
      RunNbStr := StringOfChar('0', Floor(Log10(MaxInt))-Floor(Log10(RunNb)))+IntToStr(RunNb);
{$ENDIF}

      Graphs[GraphsSize] := GraphPanel.AddGraph(RunNbStr+'_'+FullName,
                                                'Run #'+RunNbStr+' - '+FullName+' ('+aPropertyData^.Units+')', OutputNb, Duration, Output);

      Inc(GraphsSize);

      // Specify the colour of the graph
      // Note: to do so will repaint the graph panel, which we don't want as it
      //       may be time consuming (if there are already lots of graphs on
      //       it), so we have to disable before specifying and re-enable it
      //       afterwards...

      GraphPanel.Enabled := False;

      Case aPropertyData^.PropType Of
         ptStateVar:
            Graphs[GraphsSize-1].LineSpecs.Color := RunColor(clNavy, RunNb);
         ptCst:
            Graphs[GraphsSize-1].LineSpecs.Color := RunColor(clMaroon, RunNb);
         ptCompVar:
            Graphs[GraphsSize-1].LineSpecs.Color := RunColor(clGreen, RunNb);
      End;

      GraphPanel.Enabled := True;
   End;
End;

//==============================================================================

Procedure TComputationFrame.ClearAllGraphsInGraphPanel(Const aGraphPanel: TOGLGraphPanel);
Begin
   With aGraphPanel Do Begin
      // Remove all the graphs from the graph panel

      ClearAllGraphs(aGraphPanel);

      // Reset the Y axis (in case a NaN/Infinite data point was plotted)

      With aGraphPanel.YAxis Do Begin
         Min := -1;
         Max := 1;

         Start  := Min;
         Length := Max-Min;
      End;

      // Recreate the graphs, if necessary

      If (ComputingEngineState <> cesStopped) Then
         CreateNecessaryGraphs(aGraphPanel);

      // Repaint the graph panel

      ForceOGLRepaint;
   End;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsClearAllGraphsExecute(Sender: TObject);
Var
   I: Integer;
Begin
   // Clear the graph panel(s)

   If (Sender = ActionToolsClearAllGraphsInCurrentGraphPanel) Then
      ClearAllGraphsInGraphPanel(ActiveGraphPanel.OGLGraphPanel)
   Else
      For I := 0 To GraphPanelsSize-1 Do
         ClearAllGraphsInGraphPanel(GraphPanels[I].OGLGraphPanel);
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeInitChildren(Sender: TBaseVirtualTree;
                                                                         Node: PVirtualNode;
                                                                         Var ChildCount: Cardinal);
Var
   Data: PPropertyData;
Begin
   Data := Sender.GetNodeData(Node);

   If (Node.Parent = Sender.RootNode) Then Begin
      If (Node.Index = 0) Then
         // General properties

         ChildCount := 3
      Else
         // Root component's variables/sub-components

         With RootComponents[Node.Index-1] Do
            ChildCount := VariablesList.Size+ComponentsList.Size;
   End Else If ((Node.Parent.Parent <> Nil) And (Node.Parent.Parent = Sender.RootNode) And
                (Node.Parent.Index = 0) And (Node.Index = 2)) Then
      // Integrator's properties

      ChildCount := 12
   Else
      // A component's variables/sub-components

      With TCellMLModelRuntimeComponent(Data^.Data) Do
         ChildCount := VariablesList.Size+ComponentsList.Size;
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeInitNode(Sender: TBaseVirtualTree;
                                                                     ParentNode, Node: PVirtualNode;
                                                                     Var InitialStates: TVirtualNodeInitStates);
   Function InitPickStr(Const aNode: PVirtualNode; Const aData: PPropertyData;
                        Const aName: String; Const aStringArray: DArray;
                        Const aIndex: Integer): PVirtualNode;
   Var
      Iter: Integer;
   Begin
      Result := aNode;

      With aData^ Do Begin
         PropType := ptPickStr;

         Name := aName;

         Data := TPropertyDropDownList.Create;

         With TPropertyDropDownList(Data) Do Begin
            SetLength(Items, aStringArray.Size);

            For Iter := 0 To aStringArray.Size-1 Do
               Items[Iter] := String(aStringArray.At(Iter).VString);

            Index := aIndex;

            Value := Items[Index];
         End;
      End;
   End;
   Function InitStrictPosFloat(Const aNode: PVirtualNode;
                               Const aData: PPropertyData;
                               Const aName, aValue: String;
                               Const aUnit: String = ''): PVirtualNode; 
   Begin
      Result := aNode;

      With aData^ Do Begin
         PropType := ptStrictPosFloat;

         Name  := aName;
         Value := aValue;
         Units := aUnit;
      End;
   End;
   Function InitPosFloat(Const aNode: PVirtualNode; Const aData: PPropertyData;
                         Const aName, aValue: String;
                         Const aUnit: String = ''): PVirtualNode; 
   Begin
      Result := aNode;

      With aData^ Do Begin
         PropType := ptPosFloat;

         Name  := aName;
         Value := aValue;
         Units := aUnit;
      End;
   End;
   Function InitRangeInteger(Const aNode: PVirtualNode;
                             Const aData: PPropertyData;
                             Const aName, aValue: String;
                             Const aMin, aMax: Integer;
                             Const aUnit: String = ''): PVirtualNode;
   Var
      RealValue: Integer;
   Begin
      Result := aNode;

      With aData^ Do Begin
         PropType := ptRangeInteger;

{$IFDEF OPT_MATH}
         RealValue := OptMinI(StrToInt(aValue), aMax);
{$ELSE}
         RealValue := Min(StrToInt(aValue), aMax);
{$ENDIF}

         Name  := aName;
         Value := IntToStr(RealValue);
         Units := aUnit;

         Data := TPropertyRangeInteger.Create;

         With TPropertyRangeInteger(Data) Do Begin
            Min := aMin;
            Max := aMax;
         End;
      End;
   End;
   Function InitStrictPosInteger(Const aNode: PVirtualNode;
                                 Const aData: PPropertyData;
                                 Const aName, aValue: String;
                                 Const aUnit: String = ''): PVirtualNode; 
   Begin
      Result := aNode;

      With aData^ Do Begin
         PropType := ptStrictPosInteger;

         Name  := aName;
         Value := aValue;
         Units := aUnit;
      End;
   End;
   Procedure InitVar(Const aNode: PVirtualNode; Const aData: PPropertyData;
                     Const aVar: TCellMLModelRuntimeVariable); 
   Begin
      aNode^.CheckType := ctCheckBox;

      With aData^ Do Begin
         Case aVar.State Of
            vsState: Begin
               PropType := ptStateVar;
               Index    := aVar.Index;

               Data := TPropertyParameter.Create(@Cell.Y[aVar.Index]);
            End;
            vsConstant: Begin
               PropType := ptCst;
               Index    := aVar.Index;

               Data := TPropertyParameter.Create(@Cell.Csts[aVar.Index]);
            End;
            vsComputed: Begin
               PropType := ptCompVar;

               Data := TPropertyParameter.Create(@Cell.CompVars[aVar.Index]);
            End;
         End;

         CheckType := ctCheckBox;
         Owner     := aVar.Owner.Name;
         Name      := aVar.Name;
         Units     := aVar.Units;
      End;

      DoCellMLFileToMCProgress;
   End;
   Procedure InitComp(Const aNode: PVirtualNode;
                      Var aInitialStates: TVirtualNodeInitStates;
                      Const aData: PPropertyData;
                      Const aComp: TCellMLModelRuntimeComponent);
   Begin
      aNode^.CheckType := ctTriStateCheckBox;

      aInitialStates := aInitialStates+[ivsHasChildren, ivsExpanded];

      With aData^ Do Begin
         CheckType := ctTriStateCheckBox;
         Name      := aComp.Name;
         Data      := aComp;
      End;

      DoCellMLFileToMCProgress;
   End;
Var
   Data: PPropertyData;
Begin
   Data := Sender.GetNodeData(Node);

   If (ParentNode = Nil) Then Begin
      // Root node

      InitialStates := InitialStates+[ivsHasChildren, ivsExpanded];

      If (Node.Index = 0) Then
         Data^.Name := 'General'
      Else Begin
         // Root component

         Node.CheckType := ctTriStateCheckBox;

         With Data^ Do Begin
            CheckType := ctTriStateCheckBox;
            Name      := RootComponents[Node.Index-1].Name;
            Data      := RootComponents[Node.Index-1];
         End;

         DoCellMLFileToMCProgress;
      End;
   End Else Begin
      If ((ParentNode.Parent = Sender.RootNode) And (ParentNode.Index = 0)) Then
         // General properties

         Case Node.Index Of
            0:
               DurationNode := InitStrictPosFloat(Node, Data, 'Duration', SimplifyNb(ComputationOptions.GeneralOptions.Duration), 'ms');
            1:
               OutputNode := InitStrictPosFloat(Node, Data, 'Output', SimplifyNb(ComputationOptions.GeneralOptions.Output), 'ms');
            2: Begin
               InitialStates := InitialStates+[ivsHasChildren, ivsExpanded];

               Data^.Name := 'Integrator';
            End;
         End
      Else If ((ParentNode.Parent.Parent <> Nil) And
               (ParentNode.Parent.Parent = Sender.RootNode) And (ParentNode.Parent.Index = 0) And
               (ParentNode.Index = 2)) Then
         // Integrator's properties

         Case Node.Index Of
            0:
               IntegratorNode := InitPickStr(Node, Data, 'Integrator', ODEIntegrators, Integer(ComputationOptions.IntegrationOptions.Integrator));
            1:
               TimeStepNode := InitStrictPosFloat(Node, Data, 'Time step', SimplifyNb(ComputationOptions.IntegrationOptions.TimeStep), 'ms');
            2:
               MaxTimeStepNode := InitPosFloat(Node, Data, 'Maximum time step', SimplifyNb(ComputationOptions.IntegrationOptions.MaxTimeStep), 'ms');
            3:
               MaxNbOfStepsNode := InitStrictPosInteger(Node, Data, 'Maximum number of steps', SimplifyNb(ComputationOptions.IntegrationOptions.MaxNbOfSteps));
            4:
               MethodNode := InitPickStr(Node, Data, 'Method', ODECVODEIntegratorMethods, Integer(ComputationOptions.IntegrationOptions.Method)-1);
               // Note: "-1" because "TODECVODEIntegratorMethod" is 1-based, as
               //       opposed to 0-based...
            5:
               IteratorNode := InitPickStr(Node, Data, 'Iterator', ODECVODEIntegratorIterators, Integer(ComputationOptions.IntegrationOptions.Iterator)-1);
               // Note: "-1" because "TODECVODEIntegratorIterator" is 1-based,
               //       as opposed to 0-based...
            6:
               LinearSolverNode := InitPickStr(Node, Data, 'Linear solver', ODECVODEIntegratorLinearSolvers, Integer(ComputationOptions.IntegrationOptions.LinearSolver));
            7:
               PreconditionerNode := InitPickStr(Node, Data, 'Preconditioner', ODECVODEIntegratorPreconditioners, Integer(ComputationOptions.IntegrationOptions.Preconditioner));
            8:
               UpperHalfBandwidthNode := InitRangeInteger(Node, Data, 'Upper half-bandwidth', SimplifyNb(ComputationOptions.IntegrationOptions.UpperHalfBandwidth), 0, Cell.Runtime.NbOfStateVars-1);
            9:
               LowerHalfBandwidthNode := InitRangeInteger(Node, Data, 'Lower half-bandwidth', SimplifyNb(ComputationOptions.IntegrationOptions.LowerHalfBandwidth), 0, Cell.Runtime.NbOfStateVars-1);
            10:
               RelTolNode := InitStrictPosFloat(Node, Data, 'Relative tolerance', SimplifyNb(ComputationOptions.IntegrationOptions.RelTol));
            11: Begin
               AbsTolNode := InitStrictPosFloat(Node, Data, 'Absolute tolerance', SimplifyNb(ComputationOptions.IntegrationOptions.AbsTol));

               // We are done with the general properties, so can tell it which
               // of its children should be visible or not

               UpdateGeneralProperties;
            End;
         End
      Else Begin
         // The model's properties

         With TCellMLModelRuntimeComponent(PPropertyData(Sender.GetNodeData(ParentNode))^.Data) Do
            If (Integer(Node.Index) < VariablesList.Size) Then
               InitVar(Node, Data, TCellMLModelRuntimeVariable(VariablesList.At(Node.Index).VObject))
            Else
               InitComp(Node, InitialStates, Data, TCellMLModelRuntimeComponent(ComponentsList.At(Node.Index-Cardinal(VariablesList.Size)).VObject));
      End;
   End;
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeEdited(Sender: TBaseVirtualTree;
                                                                   Node: PVirtualNode;
                                                                   Column: TColumnIndex);
Var
   NewValue: Double;
Begin
   // Note: one would think that having "Inherited" at the beginning would call
   //       "TPropertiesFrame.VirtualStringTreeEdited", but it does not (!!), so
   //       we just do what "Inherited" should have done... as well as what we
   //       want to do on top of it...

   If ((Node = IntegratorNode) Or (Node = IteratorNode) Or
       (Node = LinearSolverNode) Or (Node = PreconditionerNode)) Then
      // We just edited the type of integrator, type of iterator, linear solver
      // or preconditioner, so update the general properties

      UpdateGeneralProperties
   Else
      With PPropertyData(Sender.GetNodeData(Node))^ Do
         If (PropType In [ptStateVar, ptCst]) Then Begin
            NewValue := StrToFloat(PPropertyData(Sender.GetNodeData(Node))^.Value);

            If (TPropertyParameter(Data).Value^ <> NewValue) Then Begin
               // The edited node is a state variable or constant, so update its
               // real value...

               TPropertyParameter(Data).Value^ := NewValue;

               If (PropType = ptStateVar) Then
                  StateVarsModified := True
               Else
                  CstsModified := True;

               // Recompute the code that needs to be computed once, since we
               // modified some parameters...

               Cell.ComputeOnce;
            End;
         End;
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeGetText(Sender: TBaseVirtualTree;
                                                                    Node: PVirtualNode;
                                                                    Column: TColumnIndex;
                                                                    TextType: TVSTTextType;
                                                                    Var CellText: WideString);
Begin
   // Note: one would think that having "Inherited" at the beginning would call
   //       "TPropertiesFrame.VirtualStringTreeGetText", but it does not (!!),
   //       so we just do what "Inherited" should have done... as well as what
   //       we want to do on top of it...

   If (TextType = ttNormal) Then
      Case Column Of
         0:   // Name
            CellText := PPropertyData(Sender.GetNodeData(Node))^.Name;
         1:   // Value
            With PPropertyData(Sender.GetNodeData(Node))^ Do
               If (PropType In [ptStateVar, ptCst, ptCompVar]) Then
                  CellText := SimplifyNb(TPropertyParameter(Data).Value^)
               Else
                  CellText := Value;
         2:   // Unit
            CellText := PPropertyData(Sender.GetNodeData(Node))^.Units;
      End;
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeGetHint(Sender: TBaseVirtualTree;
                                                                    Node: PVirtualNode;
                                                                    Column: TColumnIndex;
                                                                    Var LineBreakStyle: TVTTooltipLineBreakStyle;
                                                                    Var HintText: WideString);
Begin
   If (Node = MaxTimeStepNode) Then
      HintText := 'Please note that a value of 0 ms means that CVODE will use whatever time step it sees fit.'+CRLF+
                  'However, in some cases (e.g. a cardiac electrophysiological model that implements a stimulus current),'+CRLF+
                  'a more suitable value (e.g. the duration of the stimulus current) will be required (to ensure that the'+CRLF+
                  'stimulus current is ''picked up'' by CVODE).'
   Else If (Node = MaxNbOfStepsNode) Then
      HintText := 'Please note that a value of 500 steps should be big enough.'+CRLF+
                  'However, in some cases (e.g. a particularly stiff model),'+CRLF+
                  'you might want to increase that value.';
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeGetImageIndex(Sender: TBaseVirtualTree;
                                                                          Node: PVirtualNode;
                                                                          Kind: TVTImageKind;
                                                                          Column: TColumnIndex;
                                                                          Var Ghosted: Boolean;
                                                                          Var ImageIndex: Integer);
Begin
   If ((Kind In [ikNormal, ikSelected]) And (Column = 0)) Then Begin
      If (Node.Parent = Sender.RootNode) Then
         ImageIndex := -1   // Root node, so no image
      Else
         Case PPropertyData(Sender.GetNodeData(Node))^.PropType Of
            ptStateVar:
               ImageIndex := 0;
            ptCst:
               ImageIndex := 1;
            ptCompVar:
               ImageIndex := 2;
         Else
            ImageIndex := -1;
         End;
   End;
End;

//==============================================================================

Procedure TComputationFrame.ActionViewToolBarsExecute(Sender: TObject);
Begin
   Inherited;

   // Show/hide a particular toolbar

   If ((Sender As TAction) = ActionViewPropertiesToolBar) Then
      ShowHideControl(ControlBarPropertiesMenuToolBar)
   Else If ((Sender As TAction) = ActionViewToolsToolBar) Then
      ShowHideControl(ControlBarToolsMenuToolBar);
End;

//==============================================================================

Procedure TComputationFrame.ActionViewToolBarsUpdate(Sender: TObject);
Begin
   Inherited;

   // Update the menu of a particular toolbar

   If ((Sender As TAction) = ActionViewPropertiesToolBar) Then
      ActionViewPropertiesToolBar.Checked := ControlBarPropertiesMenuToolBar.Visible
   Else If ((Sender As TAction) = ActionViewToolsToolBar) Then
      ActionViewToolsToolBar.Checked := ControlBarToolsMenuToolBar.Visible;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsAddGraphPanelExecute(Sender: TObject);
Begin
   // Add a graph panel

   AddGraphPanel;
End;

//==============================================================================

Procedure TComputationFrame.CentralDockSiteGetSiteInfo(Sender: TObject;
                                                       DockClient: TControl;
                                                       Var InfluenceRect: TRect;
                                                       MousePos: TPoint;
                                                       Var CanDock: Boolean);
Begin
   CanDock := DockClient.Tag = 3;   // Only accept graphs and the like...
End;

//==============================================================================

Procedure TComputationFrame.ShowCheckBoxesInVirtualStringTree(aSender: TBaseVirtualTree;
                                                              aNode: PVirtualNode;
                                                              aData: Pointer;
                                                              Var aAbort: Boolean);
Begin
   // Show the check box of the property that is either associated to the active
   // graph panel or that is not associated to any graph panel at all

   With PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(aNode))^ Do
      If (CheckType = ctCheckBox) Then
         With TPropertyParameter(Data) Do Begin
            If ((GraphPanel = Nil) Or (GraphPanel = ActiveGraphPanel.OGLGraphPanel)) Then Begin
               // The property is either associated to the active graph panel or
               // not associated to any at all

               If (aNode^.CheckType = ctNone) Then Begin
                  // The property's check box is not visible, so show it

                  aNode^.CheckType  := CheckType;
                  aNode^.CheckState := CheckState;
               End;
            End Else If (aNode^.CheckType <> ctNone) Then Begin
               // The property is associated to another graph panel and its
               // check box is visible, so hide it

               CheckState := aNode^.CheckState;

               aNode^.CheckType := ctNone;
            End;
         End;
End;

//==============================================================================

Procedure TComputationFrame.SetActiveGraphPanel(Const aValue: TGraphPanelFrame);
Var
   I: Integer;
Begin
   If (aValue <> FActiveGraphPanel) Then Begin
      FActiveGraphPanel := aValue;

      // Activate the new currently active graph panel

      For I := 0 To GraphPanelsSize-1 Do
         GraphPanels[I].Active := GraphPanels[I] = FActiveGraphPanel;

      // Update the properties by showing the check box of the properties that
      // are either associated to the newly active graph panel or that are not
      // associated to any graph panel at all

      With PropertiesFrame.VirtualStringTree Do Begin
         BeginUpdate;
            IterateSubtree(Nil, ShowCheckBoxesInVirtualStringTree, Nil);
         EndUpdate;
      End;

      // Update the tri-state check boxes, so they show their right state, based
      // on the check boxes / properties that are visible

      UpdateTriStateCheckBoxes;
   End;
End;

//==============================================================================

Procedure TComputationFrame.UpdateGraphPanelsConnection;
Var
   I: Integer;
Begin
   // Update the connection between the different graph panels

   If (GraphPanelsSize > 1) Then Begin
      For I := 0 To GraphPanelsSize-2 Do
         GraphPanels[I].OGLGraphPanel.ConnectedTo := GraphPanels[I+1].OGLGraphPanel;

      // Deal with the last graph panel

      GraphPanels[GraphPanelsSize-1].OGLGraphPanel.ConnectedTo := GraphPanels[0].OGLGraphPanel;
   End Else If (GraphPanelsSize = 1) Then
      GraphPanels[0].OGLGraphPanel.ConnectedTo := Nil;
End;

//==============================================================================

Procedure TComputationFrame.ActionDummyExecute(Sender: TObject);
Begin
   // Do nothing, and that's exactly what we want! This allows File|New to work,
   // since there must a handler for "OnExecute" as otherwise the action will
   // simply be disabled...
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsRemoveGraphPanelUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  (GraphPanelsSize <> 0);
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsRemoveGraphPanelExecute(Sender: TObject);
Begin
   // Remove the graph panel(s)

   If (Sender = ActionToolsRemoveGraphPanelAll) Then
      RemoveAllGraphPanels
   Else
      RemoveGraphPanel(ActiveGraphPanel, True);
End;

//==============================================================================

Function TComputationFrame.AtLeastOneGraphPresent: Boolean;
Var
   I: Integer;
Begin
   If (GraphPanelsSize = 0) Then
      Result := False
   Else Begin
      Result := False;

      For I := 0 To GraphPanelsSize-1 Do
         If (GraphPanels[I].OGLGraphPanel.NbOfGraphs <> 0) Then Begin
            Result := True;

            Break;
         End;
   End;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsClearAllGraphsOrExportToCSVUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  AtLeastOneGraphPresent;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsClearAllGraphsOrExportToCSVCurrentUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mComputational) And
                                  (ActiveGraphPanel <> Nil) And
                                  (ActiveGraphPanel.OGLGraphPanel.NbOfGraphs <> 0);
End;

//==============================================================================

Procedure TComputationFrame.UpdateChildrenOfVirtualStringTree(aSender: TBaseVirtualTree;
                                                              aNode: PVirtualNode;
                                                              aData: Pointer;
                                                              Var aAbort: Boolean);
Begin
   // Check or uncheck any property that has a normal check box and this as long
   // as it is visible

   If (PropertiesFrame.VirtualStringTree.IsVisible[aNode] And
       (aNode^.CheckType <> ctNone)) Then Begin
      aNode^.CheckState := TCheckState(aData^);

      // Do what should be done, had the check box been directly checked or
      // unchecked

      PropertiesFrameVirtualStringTreeChecked(aSender, aNode);
      // Note: "PropertiesFrameVirtualStringTreeCheckedEnabled" has been set
      //       to "True", so the above vall will be definitely be properly
      //       executed...
   End;
End;

//==============================================================================

Procedure TComputationFrame.UpdateTriStateCheckBoxes;
   Type
      TCheckBoxState = (cbsUndefined, cbsUnchecked, cbsChecked, cbsMixed);
   Function UpdateTriStateCheckBox(aNode: PVirtualNode): TCheckBoxState;
   Var
      ChildNode: PVirtualNode;
   Begin
      If (aNode^.ChildCount <> 0) Then Begin
         // A node that has children, so go to its first child

         ChildNode := aNode^.FirstChild;

         Result := UpdateTriStateCheckBox(ChildNode);   // By default

         ChildNode := ChildNode^.NextSibling;

         While ChildNode <> Nil Do Begin
            // Update Result, based on the value of the child node. Note that we
            // don't do anything if the result is undefined

            Case UpdateTriStateCheckBox(ChildNode) Of
               cbsUnchecked:
                  Case Result Of
                     cbsUndefined:
                        Result := cbsUnchecked;
                     cbsChecked:
                        Result := cbsMixed;
                  End;
               cbsChecked:
                  Case Result Of
                     cbsUndefined:
                        Result := cbsChecked;
                     cbsUnchecked:
                        Result := cbsMixed;
                  End;
               cbsMixed:
                  Result := cbsMixed;
            End;

            ChildNode := ChildNode^.NextSibling;
         End;

         Case Result Of
            cbsUnchecked: Begin
               aNode^.CheckType  := ctTriStateCheckBox;
               aNode^.CheckState := csUncheckedNormal;
            End;
            cbsChecked: Begin
               aNode^.CheckType  := ctTriStateCheckBox;
               aNode^.CheckState := csCheckedNormal;
            End;
            cbsMixed: Begin
               aNode^.CheckType  := ctTriStateCheckBox;
               aNode^.CheckState := csMixedNormal;
            End;
         Else
            // cbsUndefined, so hide the check box

            aNode^.CheckType := ctNone;
         End;
      End Else
         // A child, so return its check state, but only if it is visible and
         // its check box is visible

         If (PropertiesFrame.VirtualStringTree.IsVisible[aNode] And
             (aNode^.CheckType = ctCheckBox)) Then Begin
            If (aNode^.CheckState = csCheckedNormal) Then
               Result := cbsChecked
            Else
               Result := cbsUnchecked;
         End Else
            Result := cbsUndefined;
   End;
Begin
   // Get the tri-state check boxes right. Indeed, depending on whether a
   // property is visible and/or associated to a particular graph panel, then
   // a tri-state check may have to be hidden or not. If it is shown, then we
   // have to determine whether it should be checked, unchecked or mixed. In
   // order to do that, we cannot use "IterateSubtree", because we need to know
   // about a tri-state check box's children to determine its state, hence we
   // use a recursive approach instead...

   With PropertiesFrame.VirtualStringTree Do Begin
      BeginUpdate;
         UpdateTriStateCheckBox(RootNode);
      EndUpdate;
   End;
End;

//==============================================================================

Procedure TComputationFrame.PropertiesFrameVirtualStringTreeChecked(Sender: TBaseVirtualTree;
                                                                    Node: PVirtualNode);
   Procedure NormalCheckBoxCheckedOrUnchecked;
   Var
      PropertyData: PPropertyData;
      PropertyParameter: TPropertyParameter;
   Begin
      // Note: we may come here, not because the current node is the active one,
      //       but because it's one of the active node's children, in which case
      //       we have to keep that in mind, e.g. when having to refresh a graph
      //       panel

      // Keep track of the state of the check box

      PropertyData := PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(Node));

      If (PropertyData^.CheckState <> Node.CheckState) Then Begin
         PropertyData^.CheckState := Node.CheckState;

         If (Node.CheckState = csCheckedNormal) Then Begin
            // The property is checked, so add it to the list of nodes to keep
            // track of, as well as create a graph in the currently active graph
            // panel

            PropertyParameter := TPropertyParameter(PropertyData^.Data);

            PropertyParameter.GraphPanel := ActiveGraphPanel.OGLGraphPanel;

            // Add the property data to the list, but only if we are creating
            // the first graph associated to it

            If (PropertyParameter.GraphsSize = 0) Then
               AddNode(Node);

            // Create the graph, but only if we are currently running a
            // simulation, as it will otherwise be created when actually
            // needed...

            If (ComputingEngineState <> cesStopped) Then
               CreateGraph(PropertyData, PropertyParameter);
         End Else Begin
            // The property is unchecked, so clear all the graphs associated to
            // the current node

            ClearAllNodeGraphs(Node);

            // Refresh the graph panel that used to contain the graphs and then
            // forget about it, since the current node is not associated to it
            // anymore

            With TPropertyParameter(PPropertyData(PropertiesFrame.VirtualStringTree.GetNodeData(Node))^.Data) Do Begin
               If (Not PropertiesFrameVirtualStringTreeCheckedEnabled) Then
                  GraphPanel.ForceOGLRepaint;

               GraphPanel := Nil;
               // Note: to reset "GraphPanel" is very important as some code
               //       relies on its value being "Nil" in some cases...
            End;
         End;
      End;
   End;
Var
   ChildNewState: TCheckState;
   I: Integer;
Begin
   // We only want to deal with the node which check box's state is directly
   // being modified, i.e. if we un/check a tri-state check box, then we only
   // want to catch that one and not any of its children. In the case of
   // un/checking a normal check box, there is no problem, as we will only come
   // here once and for that normal check box. Still, to account for both
   // situations, we have to check that the caught node is indeed the active
   // one, so...

   // Note: when dealing with a tri-state check box, we will need to deal with
   //       its children. That is done by making use of
   //       "PropertiesFrameVirtualStringTreeCheckedEnabled"...

   With PropertiesFrame.VirtualStringTree Do
      If (Node = PropertiesFrame.ActiveNode) Then Begin
         // The caught node is the active one, so we can do what needs to be
         // done and which depends on whether we are dealing with a tri-state
         // check box or a normal one

         PropertiesFrame.ActiveNode := Nil;   // Just to avoid any potential
                                              // problem...

         Case PPropertyData(GetNodeData(Node))^.CheckType Of
            ctTriStateCheckBox: Begin
               // This is a tri-state check box, so we must update the state of
               // every single children's normal check box (as long as it is
               // visible), as well as refresh the graph panels, if need be

               // Determine the state in which the children nodes should be

               Case Node.CheckState Of
                  csCheckedNormal:
                     ChildNewState := csCheckedNormal;
               Else
                  ChildNewState := csUncheckedNormal;
               End;

               // Deal with the children nodes

               PropertiesFrameVirtualStringTreeCheckedEnabled := True;

               BeginUpdate;
                  IterateSubtree(Node, UpdateChildrenOfVirtualStringTree, @ChildNewState);
               EndUpdate;

               PropertiesFrameVirtualStringTreeCheckedEnabled := False;

               // Determine whether we need to repaint all the graphs, something
               // that would be necessary after having unchecked all the
               // children nodes

               If (ChildNewstate = csUncheckedNormal) Then
                  For I := 0 To GraphPanelsSize-1 Do
                     GraphPanels[I].OGLGraphPanel.ForceOGLRepaint;
            End;
            ctCheckBox:
               // A normal check box, so either create a graph or clear all the
               // graphs associated to the property, depending on the case

               NormalCheckBoxCheckedOrUnchecked;
         End;

         // Update the tri-state check boxes, so they show their right state,
         // based on the check boxes / properties that are visible

         UpdateTriStateCheckBoxes;
      End Else If (PropertiesFrameVirtualStringTreeCheckedEnabled And
                   (PPropertyData(GetNodeData(Node))^.CheckType = ctCheckBox)) Then 
         // Dealing indirectly with one of the children, i.e. after we clicked
         // on one of the tri-state check boxes

         NormalCheckBoxCheckedOrUnchecked;
End;

//==============================================================================

Procedure TComputationFrame.ExportToCSV(Const aAllGraphPanels, aOneCSVFile: Boolean);
Var
   FileName: String;
   CSVFileName: Array Of String;
   NbOfExportableGraphPanels, NbOfCSVFiles, NbOfChars: Integer;
   GraphCounter: Integer;
   I, J: Integer;
   CanSave: Boolean;
   MsgForm: TMsgForm;
   OGLGraphPanels: Array Of TOGLGraphPanel;
Begin
   FileName := ChangeFileExt((MainForm.EditorFrame.PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName, '.'+CSV_FORMAT_DEFEXT);

   If (ExecSaveDialog(FileName, CSV_FORMAT_DEFEXT, CSV_FORMAT_FILTER)) Then Begin
      Application.ProcessMessages;
      // Note: it ensures that the Save dialog doesn't remain half shown...

      // Determine the number of graph panels that can be exported to CSV and
      // generate the CSV filenames, if necessary

      FileName := SaveDialog.FileName;

      If (aAllGraphPanels) Then Begin
         // Determine the number of graph panels to be exported to CSV

         NbOfExportableGraphPanels := 0;

         For I := 0 To GraphPanelsSize-1 Do
            If (GraphPanels[I].OGLGraphPanel.NbOfGraphs <> 0) Then
               Inc(NbOfExportableGraphPanels);
      End Else
         NbOfExportableGraphPanels := 1;

      If (aAllGraphPanels And (NbOfExportableGraphPanels <> 1)) Then Begin
         If (aOneCSVFile) Then Begin
            NbOfCSVFiles := 1;

            // The different graph panels

            SetLength(OGLGraphPanels, NbOfExportableGraphPanels);

            J := 0;

            For I := 0 To GraphPanelsSize-1 Do
               If (GraphPanels[I].OGLGraphPanel.NbOfGraphs <> 0) Then Begin
                  OGLGraphPanels[J] := GraphPanels[I].OGLGraphPanel;

                  Inc(J);
               End;
         End Else Begin
            // Generate the CSV filenames

{$IFDEF OPT_MATH}
            NbOfChars := OptFloor(OptLog10(NbOfExportableGraphPanels));
{$ELSE}
            NbOfChars := Floor(Log10(NbOfExportableGraphPanels));
{$ENDIF}

            SetLength(CSVFileName, GraphPanelsSize);

            J := 0;

            GraphCounter := 1;   // Because it's used in a one-based manner

            For I := 0 To GraphPanelsSize-1 Do
               If (GraphPanels[I].OGLGraphPanel.NbOfGraphs <> 0) Then Begin
{$IFDEF OPT_MATH}
                  CSVFileName[J] := ChangeFileExt(FileName, ' - Graph panel #'+StringOfChar('0', NbOfChars-OptFloor(OptLog10(GraphCounter)))+IntToStr(GraphCounter)+'.'+CSV_FORMAT_DEFEXT);
{$ELSE}
                  CSVFileName[J] := ChangeFileExt(FileName, ' - Graph panel #'+StringOfChar('0', NbOfChars-Floor(Log10(GraphCounter)))+IntToStr(GraphCounter)+'.'+CSV_FORMAT_DEFEXT);
{$ENDIF}

                  Inc(J);
                  Inc(GraphCounter);
               End;

            NbOfCSVFiles := NbOfExportableGraphPanels;

            SetLength(OGLGraphPanels, 1);
         End;
      End Else Begin
         // Only one graph panel to be exported to CSV, so...

         SetLength(OGLGraphPanels, 1);

         OGLGraphPanels[0] := ActiveGraphPanel.OGLGraphPanel;

         NbOfExportableGraphPanels := 1;

         NbOfCSVFiles := 1;
      End;

      If (NbOfCSVFiles > 1) Then
         CanSave := CanOverwriteFile(CSVFileName)
      Else
         CanSave := CanOverwriteFile(FileName);

      If (CanSave) Then Begin
         // Export the graph panel(s), based on what we want to export and what
         // can be exported

         MsgForm := TMsgForm.Create('Exporting to CSV...', MAX_FONT_SIZE);

         Application.ProcessMessages;
         // Note: it ensures that the message window appears straight away...

         If (aAllGraphPanels And (NbOfExportableGraphPanels <> 1)) Then Begin
            If (aOneCSVFile) Then
               TOGLGraphPanel.ExportToCSV(OGLGraphPanels, FileName)
            Else Begin
               GraphCounter := 0;   // Because it's used in a zero-based manner

               For I := 0 To GraphPanelsSize-1 Do
                  If (GraphPanels[I].OGLGraphPanel.NbOfGraphs <> 0) Then Begin
                     OGLGraphPanels[0] := GraphPanels[I].OGLGraphPanel;

                     TOGLGraphPanel.ExportToCSV(OGLGraphPanels, CSVFileName[GraphCounter]);

                     Inc(GraphCounter);
                  End;
            End;
         End Else
            TOGLGraphPanel.ExportToCSV(OGLGraphPanels, FileName);

         MsgForm.Free;
      End;
   End;
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsExportToCSVAllGraphPanelsExecute(Sender: TObject);
Begin
   // Export to CSV

   ExportToCSV(True, Sender = ActionToolsExportToCSVAllGraphPanelsInOneCSVFile);
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsExportToCSVCurrentGraphPanelExecute(Sender: TObject);
Begin
   ExportToCSV(False);
End;

//==============================================================================

Procedure TComputationFrame.ActionToolsGraphicalPanelOptionsExecute(Sender: TObject);
Begin
   OptionsForm(COMPUTATION_OPTIONS, GRAPHICAL_PANEL_SUB_OPTIONS);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

