//==============================================================================
// Editor frame
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

Unit Editor;

//==============================================================================

Interface

//==============================================================================

Uses
   Messages, Classes, Controls, Forms, Contents, Menus, ActnList, ImgList,
   ComCtrls, ExtCtrls, ToolWin, Msgs, CommandViewer, StdActns, SyntaxEdit,
   CmdGraph, SynEdit, Dialogs, CORCommon, DeCAL, Dockable, Tabs;

//==============================================================================

Type
   TEditorFrame = Class(TContentsFrame)
    MRUListPopupMenu: TPopupMenu;
    FindDialog: TFindDialog;
    ReplaceDialog: TReplaceDialog;
    ActionEditCut: TAction;
    ActionEditCopy: TAction;
    ActionEditPaste: TEditPaste;
    ActionEditFind: TAction;
    ActionEditReplace: TAction;
    ActionFileOpen: TAction;
    ActionFileReopenMRUFile: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TAction;
    ActionFileSaveAll: TAction;
    ActionFileClose: TAction;
    ActionFileCloseAll: TAction;
    ActionViewCommandViewer: TAction;
    ActionViewMsgsViewer: TAction;
    ActionViewEditToolBar: TAction;
    MenuEdit: TMenuItem;
    MenuEditReplace: TMenuItem;
    MenuEditFind: TMenuItem;
    MenuEditSep2: TMenuItem;
    MenuEditPaste: TMenuItem;
    MenuEditCopy: TMenuItem;
    MenuEditCut: TMenuItem;
    MenuViewMsgsViewer: TMenuItem;
    MenuFileSep1: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileReopen: TMenuItem;
    MenuFileSep4: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    MenuFileSaveAll: TMenuItem;
    MenuFileSep5: TMenuItem;
    MenuFileClose: TMenuItem;
    MenuFileCloseAll: TMenuItem;
    MenuFileSep6: TMenuItem;
    MenuViewToolBarsEdit: TMenuItem;
    ControlBarPopupMenuEditMenuItem: TMenuItem;
    PopupMenu: TPopupMenu;
    PopupMenuClose: TMenuItem;
    PopupMenuSep2: TMenuItem;
    PopupMenuCut: TMenuItem;
    PopupMenuCopy: TMenuItem;
    PopupMenuPaste: TMenuItem;
    PopupMenuSep3: TMenuItem;
    PopupMenuFind: TMenuItem;
    PopupMenuReplace: TMenuItem;
    PopupMenuSep4: TMenuItem;
    PopupMenuMsgs: TMenuItem;
    PopupMenuCommandViewer: TMenuItem;
    ControlBarEditMenuToolBar: TToolBar;
    ControlBarEditMenuToolCutBtn: TToolButton;
    ControlBarEditMenuToolCopyBtn: TToolButton;
    ControlBarEditMenuToolPasteBtn: TToolButton;
    ControlBarFileMenuToolBarSep1: TToolButton;
    ControlBarFileMenuToolBarNewBtn: TToolButton;
    ControlBarFileMenuToolBarOpenBtn: TToolButton;
    ControlBarFileMenuToolBarSaveBtn: TToolButton;
    ControlBarMenuToolBarEditMenu: TToolButton;
    ControlBarMenuToolBarFileMenu: TToolButton;
    ControlBarMenuToolBarViewMenu: TToolButton;
    ControlBarMenuToolBarRunMenu: TToolButton;
    ControlBarMenuToolBarHelpMenu: TToolButton;
    ControlBarRunMenuToolBarRunBtn: TToolButton;
    ControlBarFileMenuToolBarExitBtn: TToolButton;
    CommandViewerFrame: TCommandViewerFrame;
    MsgsFrame: TMsgsFrame;
    MenuFileNew: TMenuItem;
    ActionFileNew: TAction;
    ControlBarFileMenuToolBarSaveAllBtn: TToolButton;
    ControlBarMenuToolBarToolsMenu: TToolButton;
    ActionUncategorisedClearMsgs: TAction;
    MsgsPopupMenu: TPopupMenu;
    MsgsPopupMenuClearMessages: TMenuItem;
    PopupMenuSep8: TMenuItem;
    ActionEditUndo: TAction;
    ActionEditRedo: TAction;
    MenuEditSep1: TMenuItem;
    MenuEditUndo: TMenuItem;
    MenuEditRedo: TMenuItem;
    PopupMenuUndo: TMenuItem;
    PopupMenuRedo: TMenuItem;
    ActionEditDelete: TEditPaste;
    ActionEditSelectAll: TEditPaste;
    PopupMenuDelete: TMenuItem;
    PopupMenuSelectAll: TMenuItem;
    MenuEditDelete: TMenuItem;
    MenuEditSelectAll: TMenuItem;
    PopupMenuSep1: TMenuItem;
    ActionFileReopen: TAction;
    ActionEditSearchAgain: TAction;
    MenuEditSearchAgain: TMenuItem;
    PopupMenuSearchAgain: TMenuItem;
    PopupMenuSep9: TMenuItem;
    MenuViewCommandViewer: TMenuItem;
    MenuToolsSep4: TMenuItem;
    ActionToolsEditorOptions: TAction;
    PopupMenuEditorOptions: TMenuItem;
    ActionToolsCommandViewerOptions: TAction;
    CmdGraphPopupMenu: TPopupMenu;
    CmdGraphPopupMenuCommandViewerOptions: TMenuItem;
    CmdGraphPopupMenuSep2: TMenuItem;
    ActionUncategorisedCopyCmdGraphToClipboard: TAction;
    CmdGraphPopupMenuCopyToClipboard: TMenuItem;
    CmdGraphPopupMenuSep1: TMenuItem;
    MenuToolsSep3: TMenuItem;
    ActionToolsCheckValidity: TAction;
    MenuToolsCheckValidity: TMenuItem;
    PopupMenuSep5: TMenuItem;
    PopupMenuCheckValidity: TMenuItem;
    ActionViewCommandViewerChangeFormattingForward: TAction;
    ActionViewCommandViewerChangeFormattingBackward: TAction;
    PopupMenuCloseAll: TMenuItem;
    ActionToolsExportToC: TAction;
    MenuToolsSep2: TMenuItem;
    ActionToolsExportTo: TAction;
    MenuToolsExport: TMenuItem;
    MenuToolsExportToC: TMenuItem;
    PopupMenuSep7: TMenuItem;
    PopupMenuExport: TMenuItem;
    PopupMenuExportToC: TMenuItem;
    ActionToolsExportToFortran77: TAction;
    ActionToolsExportToJava: TAction;
    ActionToolsExportToMATLAB: TAction;
    MenuToolsExportToFortran77: TMenuItem;
    MenuToolsExportToJava: TMenuItem;
    MenuToolsExportToMATLAB: TMenuItem;
    PopupMenuExportToFortran77: TMenuItem;
    PopupMenuExportToJava: TMenuItem;
    PopupMenuExportToMATLAB: TMenuItem;
    ActionToolsExportToPascal: TAction;
    MenuToolsExportToPascal: TMenuItem;
    PopupMenuExportToPascal: TMenuItem;
    ActionToolsExportToCPP: TAction;
    MenuToolsExportToCPP: TMenuItem;
    PopupMenuExportToCPP: TMenuItem;
    ActionToolsExportToDelphiForWin32: TAction;
    MenuToolsExportToCSharp: TMenuItem;
    MenuToolsExportToDelphiForDotNET: TMenuItem;
    MenuToolsExportToDelphiForWin32: TMenuItem;
    PopupMenuExportToCSharp: TMenuItem;
    PopupMenuExportToDelphiForDotNET: TMenuItem;
    PopupMenuExportToDelphiForWin32: TMenuItem;
    ActionToolsReformat: TAction;
    MenuToolsReformat: TMenuItem;
    PopupMenuReformat: TMenuItem;
    PageCtrl: TPageControl;
    ActionToolsExportToTeX: TAction;
    MenuToolsExportToTeX: TMenuItem;
    PopupMenuExportToTeX: TMenuItem;
    ActionUncategorisedCopyMsgsToClipboard: TAction;
    MsgsPopupMenuSep: TMenuItem;
    MsgsPopupMenuCopyMessagesToClipboard: TMenuItem;
    CmdGraphPopupMenuChangeFormattingForward: TMenuItem;
    CmdGraphPopupMenuChangeFormattingBackward: TMenuItem;
    ActionFileLocked: TAction;
    MenuFileSep3: TMenuItem;
    MenuFileLocked: TMenuItem;
    ActionFileDuplicate: TAction;
    MenuFileSep2: TMenuItem;
    MenuFileDuplicate: TMenuItem;
    CheckFilesTimer: TTimer;
    ControlBarFileMenuToolBarSep2: TToolButton;
    ControlBarFileMenuToolBarSep3: TToolButton;
    ControlBarFileMenuToolBarSaveAsBtn: TToolButton;
    ActionToolsExportToMSWord: TAction;
    MenuToolsExportToMSWord: TMenuItem;
    PopupMenuExportToMSWord: TMenuItem;
    ActionEditReverseSearch: TAction;
    TabSet: TTabSet;
    EditorPanel: TPanel;
    ActionViewToggleView: TAction;
    MenuViewSep2: TMenuItem;
    MenuViewToggleView: TMenuItem;
    PopupMenuSep6: TMenuItem;
    PopupMenuToggleView: TMenuItem;
    procedure ActionUncategorisedCopyMsgsToClipboardExecute(Sender: TObject);
    procedure ActionUncategorisedMsgsUpdate(Sender: TObject);
    procedure ActionFileNewExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionFileReopenMRUFileExecute(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionFileSaveAsExecute(Sender: TObject);
    procedure ActionFileSaveAllExecute(Sender: TObject);
    procedure ActionFileCloseExecute(Sender: TObject);
    procedure ActionFileCloseAllExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionEditFindExecute(Sender: TObject);
    procedure ActionEditReplaceExecute(Sender: TObject);
    procedure ActionViewExecute(Sender: TObject);
    procedure ActionViewUpdate(Sender: TObject);
    procedure ActionViewToolBarsExecute(Sender: TObject);
    procedure ActionRunRunExecute(Sender: TObject);
    procedure PageCtrlChange(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure MsgsFrameMsgsDblClick(Sender: TObject);
    procedure ActionDummyExecute(Sender: TObject);
    procedure ActionToolsEditorOptionsExecute(Sender: TObject);
    procedure ActionUncategorisedClearMsgsExecute(Sender: TObject);
    procedure ActionEditUndoExecute(Sender: TObject);
    procedure ActionEditRedoExecute(Sender: TObject);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ActionEditSelectAllExecute(Sender: TObject);
    procedure ActionFileSaveUpdate(Sender: TObject);
    procedure ActionFileSaveAsUpdate(Sender: TObject);
    procedure ActionFileSaveAllUpdate(Sender: TObject);
    procedure ActionFileCloseOrCloseAllUpdate(Sender: TObject);
    procedure ActionEditUndoUpdate(Sender: TObject);
    procedure ActionEditRedoUpdate(Sender: TObject);
    procedure ActionRunRunUpdate(Sender: TObject);
    procedure ActionFileReopenUpdate(Sender: TObject);
    procedure ActionEditSearchExecute(Sender: TObject);
    procedure ActionEditSearchUpdate(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure ActionEditCopyUpdate(Sender: TObject);
    procedure ActionEditPasteUpdate(Sender: TObject);
    procedure ActionValidCellMLFile(Sender: TObject);
    procedure ActionToolsCommandViewerOptionsExecute(Sender: TObject);
    procedure ActionUncategorisedCopyCmdGraphToClipboardExecute(Sender: TObject);
    procedure ActionToolsCheckValidityExecute(Sender: TObject);
    procedure ActionToolsReformatExecute(Sender: TObject);
    procedure ActionUncategorisedCommandViewerFormattingExecute(
      Sender: TObject);
    procedure ActionToolsExportToCExecute(Sender: TObject);
    procedure ActionToolsExportToFortran77Execute(Sender: TObject);
    procedure ActionToolsExportToJavaExecute(Sender: TObject);
    procedure ActionToolsExportToMATLABExecute(Sender: TObject);
    procedure ActionToolsExportToPascalExecute(Sender: TObject);
    procedure ActionToolsExportToCPPExecute(Sender: TObject);
    procedure ActionToolsExportToDelphiForWin32Execute(Sender: TObject);
    procedure ActionToolsExportToTeXExecute(Sender: TObject);
    procedure PageCtrlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageCtrlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PageCtrlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ActionUpdate(Sender: TObject);
    procedure ActionEditSomeTextUpdate(Sender: TObject);
    procedure ActionCmdGraphUpdate(Sender: TObject);
    procedure ActionEditCutOrDeleteUpdate(Sender: TObject);
    procedure ActionViewToolBarsUpdate(Sender: TObject);
    procedure ActionFileLockedExecute(Sender: TObject);
    procedure ActionFileLockedUpdate(Sender: TObject);
    procedure ActionFileDuplicateExecute(Sender: TObject);
    procedure ActionFileDuplicateUpdate(Sender: TObject);
    procedure CheckFilesTimerTimer(Sender: TObject);
    procedure ActionToolsExportToMSWordExecute(Sender: TObject);
    procedure ActionViewToggleViewUpdate(Sender: TObject);
    procedure ActionViewToggleViewExecute(Sender: TObject);
    procedure TabSetChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
      Private
         CheckingFiles: Boolean;

         CrtCmd: String;   // Current command
         CmdBinTree: TCmdBinTree;   // Current command being shown

         Procedure SyntaxEditChangeOrClickOrDblClick(aSender: TObject); Inline;
         Procedure SyntaxEditKeyDown(aSender: TObject; Var aKey: Word; aShift: TShiftState); Inline;
         Procedure SyntaxEditKeyUp(aSender: TObject; Var aKey: Word; aShift: TShiftState); Inline;

         Procedure UpdateMRUList;

         Procedure UpdateTabSet(Const aSyntaxEdit: TSyntaxEdit; Const aUpdateTabIndex: Boolean = True; Const aForceUpdateTabIndex: Boolean = False);

      Protected
         Function GetDockSiteSize(aDockSite: TPanel): Pointer; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;

         Procedure FrameCreate; Override;
         Procedure FrameCloseQuery(Var aCanClose: Boolean);

         Procedure BackupToolBarsAndViews; Override;
         Procedure RestoreToolBarsAndViews; Override;

         Procedure WMActivate(Var aMsg: TWMActivate);
         Procedure WMDropFiles(Var aMsg: TWMDropFiles);

         Function EnterMode: Boolean; Override;
         Function ExitMode: Boolean; Override;

         Function SetFocusTo(Const aSyntaxEdit: TSyntaxEdit): Boolean; Inline;

         Procedure AddMRUFile(Const aFileName: String);
         Procedure RemoveMRUFile(Const aFileName: String);

         Procedure ClosePage(Const aTabSheet: TTabSheet);

         Function OpenFile(Const aFileName: String; Const aView: TEditorView; Const aFileExt: String = CELLML_FILE_EXT; Const aSyntaxEdit: TSyntaxEdit = Nil): Boolean;
         Function SaveFile(Const aSyntaxEdit: TSyntaxEdit; Const aSaveAs: Boolean = False): Boolean;
         Function RunFile(Const aFileName: String): Boolean;

         Procedure SyntaxEditStatusChange(aSender: TObject; aChanges: TSynStatusChanges);

         Procedure ClearSpecMsgs(Const aFileName: String);
         Procedure UpdateSpecMsgs(Const aOldFileName, aNewFileName: String);
         Function SelFirstMsg(Const aFileName: String): Boolean;
         Function AtLeastOneMsg(Const aFileName: String): Boolean;

         Procedure AddMsg(Const aMsgType: TMsgDlgType; Const aFileName: String; Const aLineNb, aColNb: Integer; Const aMsg: String; Const aGoToMsg: Boolean);
         Procedure AddEngineMsgs(Const aEngineMsgs: DArray);
         Procedure ClearMsgs;
         Procedure CopyMsgsToClipboard; Inline;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   Windows, SysUtils, ShellAPI, CommCtrl, Main, DockSites, CellMLAPI, Options,
   Clipbrd, SynEditTypes, Common, Engine, StrUtils,VirtualTrees, ActiveX,
   ComObj, Registry, Graphics;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TEditorFrame.Create(aOwner: TComponent);
Var
   Iter: Integer;
Begin
   Inherited;

   CrtCmd := '';
   CmdBinTree := Nil;

   // Make the page control double-buffered (avoids some awful flickering)

   PageCtrl.DoubleBuffered := True;

   // Assign the right dock class for the toolbars and views

   ControlBarEditMenuToolBar.FloatingDockSiteClass := TToolBarDockForm;

   CommandViewerFrame.FloatingDockSiteClass := TDockForm;
   MsgsFrame.FloatingDockSiteClass          := TDockForm;

   // Set the different views

   TabSet.Tabs.Clear;   // Just in case...

   For Iter := 0 To Views.Size-1 Do
      TabSet.Tabs.Add(String(Views.At(Iter).VString));

   TabSet.Visible := False;   // To be on the safe side, since it should only be
                              // visible in the case of a CellML file
End;

//==============================================================================

Destructor TEditorFrame.Destroy;
Begin
   CmdBinTree.Free;

   Inherited;
End;

//==============================================================================

Procedure TEditorFrame.FrameCreate;
Var                               
   Iter: Integer;
Begin
   Inherited;

   // Toolbars and views

   SetToolBarWidth(ControlBarMenuToolBar);

   SetToolBarWidth(ControlBarFileMenuToolBar);
   SetToolBarWidth(ControlBarEditMenuToolBar);
   SetToolBarWidth(ControlBarRunMenuToolBar);

   // Command viewer

   CommandViewerFrame.CmdGraph.SetOptions(EditorOptions.CommandViewerOptions);

   // Opened files

   CheckingFiles := False;

   If (EditorFrameOpenedFiles.Size <> 0) Then Begin
      // Open the files that were open the last time COR was used

      For Iter := 0 To EditorFrameOpenedFiles.Size-1 Do
         With TEditorFrameOpenedFile(EditorFrameOpenedFiles.At(Iter).VObject) Do
            If (FileExists(FileName)) Then
               OpenFile(FileName, View, CELLML_FILE_EXT, Nil)
            Else If (EditorFrameSelectedFile >= Iter) Then
               Dec(EditorFrameSelectedFile);

      // Select the tab of the last selected opened file

      PageCtrl.ActivePageIndex := EditorFrameSelectedFile;

      If (PageCtrl.ActivePageIndex < 0) Then
         PageCtrl.ActivePageIndex := 0;

      If (PageCtrl.ActivePage <> Nil) Then Begin
         // Ensure that everything related to the editor is up to date

         PageCtrlChange(Self);

         // Select the first message related to the selected file

         SelFirstMsg((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName);
      End;
   End;

   UpdateMRUList;
End;

//==============================================================================

Procedure TEditorFrame.FrameCloseQuery(Var aCanClose: Boolean);
Var
   EditorFrameOpenedFile: TEditorFrameOpenedFile;
Begin
   ObjFree(EditorFrameOpenedFiles);

   EditorFrameOpenedFiles.Clear;

   ActionFileCloseOrCloseAllUpdate(ActionFileClose);
   ActionFileCloseOrCloseAllUpdate(ActionFileCloseAll);
   // Note: to be sure that they are up to date... Indeed, it's not necessarily
   //       the case normally...

   If (ActionFileCloseAll.Enabled) Then
      ActionFileCloseAllExecute(Self)
   Else If (ActionFileClose.Enabled) Then
      ActionFileCloseExecute(Self);

   aCanClose := PageCtrl.PageCount = 0;

   If (aCanClose) Then Begin
      // Clean the MRU list popup menus

      MenuFileReopen.Clear;
      MRUListPopupMenu.Items.Clear;

      // Back up the dimensions for the last time

      MainForm.SaveDimensions;

      // Backup the the tool bars and views for the last time

      BackupToolBarsAndViews;
   End Else
      // Cannot close, so add all the files in the // "EditorFrameOpenedFiles"
      // list to the MRU list

      While (EditorFrameOpenedFiles.Size <> 0) Do Begin
         EditorFrameOpenedFile := TEditorFrameOpenedFile(EditorFrameOpenedFiles.At(0).VObject);

         AddMRUFile(EditorFrameOpenedFile.FileName);

         EditorFrameOpenedFiles.Remove([EditorFrameOpenedFile]);

         EditorFrameOpenedFile.Free;
      End;
End;

//==============================================================================

Procedure TEditorFrame.BackupToolBarsAndViews;
Begin
   // Backup the settings of the computational frame's toolbars and views

   If (Not MainForm.FormCreating) Then Begin
      EditorFrameControlBarFileMenuToolBarVisible := ControlBarFileMenuToolBar.Visible;
      EditorFrameControlBarEditMenuToolBarVisible := ControlBarEditMenuToolBar.Visible;
      EditorFrameControlBarRunMenuToolBarVisible  := ControlBarRunMenuToolBar.Visible;

      EditorFrameCommandViewerFrameVisible := CommandViewerFrame.Visible;
      EditorFrameMsgsFrameVisible          := MsgsFrame.Visible;
   End;

   // Make the computational frame's toolbars and views invisible (VERY
   // important to do if they are floating!)

   If (ControlBarFileMenuToolBar.Floating) Then
      ControlBarFileMenuToolBar.Visible := False;

   If (ControlBarEditMenuToolBar.Floating) Then
      ControlBarEditMenuToolBar.Visible := False;

   If (ControlBarRunMenuToolBar.Floating) Then
      ControlBarRunMenuToolBar.Visible := False;

   If (CommandViewerFrame.Floating) Then
      CommandViewerFrame.Visible := False;

   If (MsgsFrame.Floating) Then
      MsgsFrame.Visible := False;
End;

//==============================================================================

Procedure TEditorFrame.RestoreToolBarsAndViews;
Begin
   // Restore the settings of the editor frame's toolbars and views

   RestoreControl(ControlBarFileMenuToolBar, EditorFrameControlBarFileMenuToolBarVisible);
   RestoreControl(ControlBarEditMenuToolBar, EditorFrameControlBarEditMenuToolBarVisible);
   RestoreControl(ControlBarRunMenuToolBar, EditorFrameControlBarRunMenuToolBarVisible);

   RestoreControl(CommandViewerFrame, EditorFrameCommandViewerFrameVisible);
   RestoreControl(MsgsFrame, EditorFrameMsgsFrameVisible);
End;

//==============================================================================

Procedure TEditorFrame.WMActivate(Var aMsg: TWMActivate);
Begin
   If ((aMsg.Active = WA_ACTIVE) Or (aMsg.Active = WA_CLICKACTIVE)) Then
      CheckFilesTimer.Interval := 1;
      // Note: ideally we would make a call to "CheckFilesTimerTimer", but then
      //       the mouse will be 'locked' and one will have to click once over
      //       COR to get it active again, while here we more or less achieve
      //       the same result by setting the interval to 1 ms, thus ensuring
      //       that "CheckFilesTimerTimer" is called as quickly as possible
      //       without causing the aforementioned problem... This also means
      //       that the interval has to be reset in "CheckFilesTimerTimer"...
End;

//==============================================================================

Procedure TEditorFrame.WMDropFiles(Var aMsg: TWMDropFiles);
Var
   NbOfFiles: UINT;
   I: UINT;
   FileName: Array[0..MAX_PATH] Of Char;
Begin
   NbOfFiles := DragQueryFile(aMsg.Drop, $FFFFFFFF, Nil, 0);

   For I := 0 To NbOfFiles-1 Do Begin
      DragQueryFile(aMsg.Drop, I, FileName, Length(FileName));

      OpenFile(FileName, EditorOptions.GeneralOptions.View);

      Application.ProcessMessages;
   End;
End;

//==============================================================================

Procedure TEditorFrame.ActionFileReopenUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (MenuFileReopen.Count <> 0);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileSaveUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).Modified;
End;

//==============================================================================

Procedure TEditorFrame.ActionFileSaveAsUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileSaveAllUpdate(Sender: TObject);
Var
   I: Integer;
Begin
   If (MainForm.Mode = mEditorial) Then Begin
      For I := 0 To PageCtrl.PageCount-1 Do
         If ((PageCtrl.Pages[I].Controls[0] As TSyntaxEdit).Modified) Then Begin
            (Sender As TAction).Enabled := True;

            Exit;
         End;

      (Sender As TAction).Enabled := False;
   End Else
      (Sender As TAction).Enabled := False;
End;

//==============================================================================

Procedure TEditorFrame.ActionFileCloseOrCloseAllUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.PageCount <> 0);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileDuplicateExecute(Sender: TObject);
Var
   SyntaxEdit: TSyntaxEdit;
   FileExt: String;
Begin
   // Duplicate the current file, i.e. create a new file which contents is that
   // of the current one

   SyntaxEdit := PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

   If (CellMLFile(SyntaxEdit.FileName)) Then
      FileExt := CELLML_FILE_EXT
   Else
      FileExt := LowerCase(ExtractFileExt(SyntaxEdit.FileName));

   OpenFile('', SyntaxEdit.View, FileExt, SyntaxEdit);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileDuplicateUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil);
End;

//==============================================================================

Procedure TEditorFrame.ActionEditUndoUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).CanUndo;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditRedoUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).CanRedo;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditCutOrDeleteUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).SelAvail And
                                  Not (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ReadOnly;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditCopyUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).SelAvail;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditPasteUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).CanPaste;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditSomeTextUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) and
                                  ((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).Lines.Count <> 0);
End;

//==============================================================================

Procedure TEditorFrame.ActionEditSearchUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  ((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).Lines.Count <> 0) And
                                  (CompareStr((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).SearchText, '') <> 0);
End;

//==============================================================================

Procedure TEditorFrame.ActionRunRunUpdate(Sender: TObject);
Begin
   If (PageCtrl.ActivePage <> Nil) Then
      (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                     CellMLFile((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName)
   Else
      (Sender As TAction).Enabled := False;
End;

//==============================================================================

Procedure TEditorFrame.ActionUncategorisedMsgsUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (MsgsFrame.Msgs.Count <> 0);
End;

//==============================================================================

Procedure TEditorFrame.ActionUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileNewExecute(Sender: TObject);
Begin
   OpenFile('', EditorOptions.GeneralOptions.View, CELLML_FILE_EXT);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileOpenExecute(Sender: TObject);
Var
   I: Integer;
Begin
   If (OpenDialog.Execute) Then
      For I := 0 To OpenDialog.Files.Count-1 Do
         OpenFile(OpenDialog.Files.Strings[I], EditorOptions.GeneralOptions.View);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileReopenMRUFileExecute(Sender: TObject);
   Function GetFileNameFromMenuCaption(aSender: TObject): String;
   Var
      PosVal: Integer;
   Begin
      Result := (aSender As TMenuItem).Caption;

      PosVal := Pos('&', Result);

      If (PosVal <> 0) Then
         // Delphi absolutely wants to create a shortcut by putting an '&'
         // somewhere, but we don't want it, so...
         // Note: it has to be removed here, since when creating the menu item
         //       everything's ok...

         Result := Copy(Result, 1, PosVal-1)+Copy(Result, PosVal+1, Length(Result)-PosVal);
   End;
Begin
   OpenFile(GetFileNameFromMenuCaption(Sender), EditorOptions.GeneralOptions.View);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileSaveExecute(Sender: TObject);
Begin
   SaveFile(PageCtrl.ActivePage.Controls[0] As TSyntaxEdit);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileSaveAsExecute(Sender: TObject);
Begin
   SaveFile(PageCtrl.ActivePage.Controls[0] As TSyntaxEdit, True);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileSaveAllExecute(Sender: TObject);
Var
   I: Integer;
Begin
   For I := 0 To PageCtrl.PageCount-1 Do
      SaveFile(PageCtrl.Pages[I].Controls[0] As TSyntaxEdit);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileCloseExecute(Sender: TObject);
Begin
   With (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit) Do
      If (CloseFileQuery <> mrCancel) Then
         // Close the file currently open

         CloseFile;
End;

//==============================================================================

Procedure TEditorFrame.ActionFileCloseAllExecute(Sender: TObject);
Var
   I: Integer;
   CrtPageIndex: Integer;
   Shift: Integer;
Begin
   For I := 0 To PageCtrl.PageCount-1 Do
      If ((PageCtrl.Pages[I].Controls[0] As TSyntaxEdit).CloseFileQuery = mrCancel) Then
         // Cancel the whole thing...

         Exit;

   // Close all the files (and save them first, if necessary)

   EditorFrameSelectedFile := PageCtrl.ActivePageIndex;
   // Default index of the selected file

   CrtPageIndex := 0;
   Shift        := 0;

   While PageCtrl.PageCount <> 0 Do Begin
      PageCtrl.ActivePageIndex := 0;
      // So we are sure to keep the order in which the files were opened
      // (important to know when closing COR)

      If (Not (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).CloseFile And
          (CrtPageIndex <= EditorFrameSelectedFile)) Then
         // File not "properly" closed (i.e. it is NOT going to be reopened the
         // next time we run COR OR it's not going to be put in the MRU list)

         Inc(Shift);

      Inc(CrtPageIndex);
   End;

   // Update the index of the selected file, if necessary (i.e. "Shift" is
   // different from 0), and correct it if necessary too (i.e. if < 0)

   If ((EditorFrameSelectedFile-Shift) < 0) Then
      EditorFrameSelectedFile := 0
   Else
      Dec(EditorFrameSelectedFile, Shift);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileExitExecute(Sender: TObject);
Begin
   // Simply exit COR...

   MainForm.Close;
End;

//==============================================================================

Procedure TEditorFrame.ActionFileLockedExecute(Sender: TObject);
Var
   SyntaxEdit: TSyntaxEdit;
Begin
   SyntaxEdit := PageCtrl.ActivePage.Controls[0] as TSyntaxEdit;

   // Update the 'read only' attribute of the editor

   SyntaxEdit.ReadOnly := Not SyntaxEdit.ReadOnly;

   // And now of the file itself...

{$WARN SYMBOL_PLATFORM OFF}
   If (SyntaxEdit.ReadOnly) Then
      FileSetAttr(SyntaxEdit.FileName, FileGetAttr(SyntaxEdit.FileName)+faReadOnly)
   Else
      FileSetAttr(SyntaxEdit.FileName, FileGetAttr(SyntaxEdit.FileName)-faReadOnly);
{$WARN SYMBOL_PLATFORM ON}

   // Ensure that everything related to the editor is up to date

   PageCtrlChange(Self);
End;

//==============================================================================

Procedure TEditorFrame.ActionFileLockedUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil);

   If ((Sender As TAction).Enabled) Then
      ActionFileLocked.Checked := (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ReadOnly
   Else
      ActionFileLocked.Checked := False;

   (Sender As TAction).Enabled := (Sender As TAction).Enabled And
                                  DirectoryIsWritable(ExtractFileDir((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName));
   // Note: we only "add" the check for directory access at this point, because
   //       we want to be able to get the locking status right, in case the
   //       model is located in a non-writable directory (as would be the case
   //       under Vista for the models that are shipped with COR) 
End;

//==============================================================================

Procedure TEditorFrame.ActionEditUndoExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).Undo;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditRedoExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).Redo;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditCutExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).CutToClipboard;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditCopyExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).CopyToClipboard;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditPasteExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).PasteFromClipboard;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditDeleteExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).SelText := '';
End;

//==============================================================================

Procedure TEditorFrame.ActionEditSelectAllExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).SelectAll;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditFindExecute(Sender: TObject);
Begin
   With PageCtrl.ActivePage.Controls[0] As TSyntaxEdit Do
      If (SelAvail And (BlockBegin.Line = BlockEnd.Line)) Then
         FindDialog.FindText := SelText
      Else
         FindDialog.FindText := GetWordAtRowCol(CaretXY);

   FindDialog.Execute;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditReplaceExecute(Sender: TObject);
Begin
   With PageCtrl.ActivePage.Controls[0] As TSyntaxEdit Do
      If (SelAvail And (BlockBegin.Line = BlockEnd.Line)) Then
         ReplaceDialog.FindText := SelText
      Else
         ReplaceDialog.FindText := GetWordAtRowCol(CaretXY);

   ReplaceDialog.Execute;
End;

//==============================================================================

Procedure TEditorFrame.ActionEditSearchExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).SearchReplace(Sender = ActionEditSearchAgain);
End;

//==============================================================================

Procedure TEditorFrame.ActionViewExecute(Sender: TObject);
Begin
   If (Sender = ActionViewCommandViewer) Then
      ShowHideControl(CommandViewerFrame)
   Else
      ShowHideControl(MsgsFrame);
End;

//==============================================================================

Procedure TEditorFrame.ActionViewUpdate(Sender: TObject);
Begin
   If (Sender = ActionViewCommandViewer) Then
      ActionViewCommandViewer.Checked := CommandViewerFrame.Visible
   Else
      ActionViewMsgsViewer.Checked := MsgsFrame.Visible;
End;

//==============================================================================

Procedure TEditorFrame.ActionViewToggleViewExecute(Sender: TObject);
Begin
   If (TabSet.TabIndex = Integer(evCOR)) Then Begin
      TabSet.TabIndex := Integer(evRaw);
   End Else
      TabSet.TabIndex := Integer(evCOR);
End;

//==============================================================================

Procedure TEditorFrame.ActionViewToggleViewUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  CellMLFile((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName);
End;

//==============================================================================

Procedure TEditorFrame.ActionViewToolBarsExecute(Sender: TObject);
Begin
   Inherited;

   // Show/hide a particular toolbar

   If ((Sender As TAction) = ActionViewEditToolBar) Then
      ShowHideControl(ControlBarEditMenuToolBar);
End;

//==============================================================================

Procedure TEditorFrame.ActionViewToolBarsUpdate(Sender: TObject);
Begin
   Inherited;

   // Update the menu of a particular toolbar

   If ((Sender As TAction) = ActionViewEditToolBar) Then
      ActionViewEditToolBar.Checked := ControlBarEditMenuToolBar.Visible;
End;

//==============================================================================

Procedure TEditorFrame.ActionRunRunExecute(Sender: TObject);
Begin
   // Execute the CellML file

   RunFile((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName);
End;

//==============================================================================

Function TEditorFrame.SetFocusTo(Const aSyntaxEdit: TSyntaxEdit): Boolean;
Begin
   Try
      If (Not MainForm.FormCreating And MainForm.Visible And
          (MainForm.Mode = mEditorial) And aSyntaxEdit.Visible) Then Begin
         SetFocus;

         aSyntaxEdit.SetFocus;

         Result := True;
      End Else
         Result := False;
   Except
      Result := False;
   End;
End;

//==============================================================================

Procedure TEditorFrame.UpdateTabSet(Const aSyntaxEdit: TSyntaxEdit;
                                    Const aUpdateTabIndex, aForceUpdateTabIndex: Boolean);
Var
   TabSetOnChange: TTabChangeEvent;
Begin
   TabSet.Visible := CellMLFile(aSyntaxEdit.FileName);

   If (aUpdateTabIndex And TabSet.Visible) Then Begin
      If (aForceUpdateTabIndex) Then Begin
         TabSetOnChange := TabSet.OnChange;

         TabSet.OnChange := Nil;
      End Else
         TabSetOnChange := Nil;   // Just to avoid the warning...

      // Set the tab set index

      // Note: by setting it, we actually try to switch from one view to the
      //       other (via "TabSet.OnChange"). However, it may happen that we
      //       just want to switch the tab set (e.g. when switching from one
      //       CellML model to another and therefore want the tab set to reflect
      //       the state of the file). In such a case, we must therefore force
      //       the setting of the tab set index, no matter what hence the
      //       "aForceUpdateTabIndex" parameter

      TabSet.TabIndex := Integer(aSyntaxEdit.View);

      If (aForceUpdateTabIndex) Then
         TabSet.OnChange := TabSetOnChange;
   End;
End;

//==============================================================================

Procedure TEditorFrame.PageCtrlChange(Sender: TObject);
Var
   SyntaxEdit: TSyntaxEdit;
Begin
   // Give the focus to the new active page's editor after making sure that the
   // colours and fonts are up to date...

   SyntaxEdit := PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

   UpdateEditorBackgroundColorAndCORAndCellMLSynHighlighters(SyntaxEdit);

   SetFocusTo(SyntaxEdit);

   // Show/hide the tab set depending on the CellML validity of the file

   UpdateTabSet(SyntaxEdit, True, True);

   SyntaxEditStatusChange(SyntaxEdit, []);
End;

//==============================================================================

Procedure TEditorFrame.FindDialogFind(Sender: TObject);
Var
   SyntaxEdit: TSyntaxEdit;
   FindDialog: TFindDialog;
Begin
   SyntaxEdit := PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

   FindDialog := Sender As TFindDialog;

   SyntaxEdit.SearchReplaceOptions := [];

   If (Not (frDown In FindDialog.Options)) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoBackwards];

   If (frMatchCase In FindDialog.Options) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoMatchCase];

   If (frWholeWord In FindDialog.Options) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoWholeWord];

   SyntaxEdit.SearchText  := FindDialog.FindText;
   SyntaxEdit.ReplaceText := '';

   FindDialog.CloseDialog;

   SetFocusTo(SyntaxEdit);   // Just in case...

   SyntaxEdit.SearchReplace;
End;

//==============================================================================

Procedure TEditorFrame.ReplaceDialogReplace(Sender: TObject);
Var
   SyntaxEdit: TSyntaxEdit;
   ReplaceDialog: TReplaceDialog;
   OldOptions: TSynEditorOptions;
Begin
   SyntaxEdit := PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

   OldOptions := SyntaxEdit.Options;

   SyntaxEdit.Options := SyntaxEdit.Options+[eoNoCaret];   // Make sure the
                                                           // caret is hidden

   ReplaceDialog := Sender As TReplaceDialog;

   SyntaxEdit.SearchReplaceOptions := [];

   If (Not (frDown In ReplaceDialog.Options)) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoBackwards];

   If (frMatchCase In ReplaceDialog.Options) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoMatchCase];

   If (frWholeWord In ReplaceDialog.Options) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoWholeWord];

   If (frReplace In ReplaceDialog.Options) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoReplace];

   If (frReplaceAll In ReplaceDialog.Options) Then
      SyntaxEdit.SearchReplaceOptions := SyntaxEdit.SearchReplaceOptions+[ssoReplaceAll, ssoEntireScope];

   SyntaxEdit.SearchText  := ReplaceDialog.FindText;
   SyntaxEdit.ReplaceText := ReplaceDialog.ReplaceText;

   ReplaceDialog.CloseDialog;

   SetFocusTo(SyntaxEdit);   // Just in case...

   SyntaxEdit.SearchReplace;

   SyntaxEdit.Options := OldOptions;
End;

//==============================================================================

Procedure TEditorFrame.ClearSpecMsgs(Const aFileName: String);
Var
   I: Integer;
   UCFileName: String;
Begin
   // Delete all the messages that are related to the file

   I := 0;

   UCFileName := UpperCase(aFileName);

   While (I <> MsgsFrame.Msgs.Count) Do
      If (CompareStr(UCFileName, String(MsgsFrame.FileNames.At(I).VString)) = 0) Then Begin
         MsgsFrame.Msgs.Delete(I);

         MsgsFrame.FileNames.RemoveAt(I);
         MsgsFrame.LineNbs.RemoveAt(I);
         MsgsFrame.ColNbs.RemoveAt(I);
      End Else
         Inc(I);
End;

//==============================================================================

Procedure TEditorFrame.UpdateSpecMsgs(Const aOldFileName, aNewFileName: String);
Var
   I: Integer;
   UCOldFileName, UCNewFileName: String;
   RelOldFileName, RelNewFileName: String;
Begin
   // Delete all the messages that are related to the file

   UCOldFileName := UpperCase(aOldFileName);
   UCNewFileName := UpperCase(aNewFileName);

   RelOldFileName := RelativeFileName(aOldFileName);
   RelNewFileName := RelativeFileName(aNewFileName);

   For I := 0 To MsgsFrame.Msgs.Count-1 Do
      If (CompareStr(UCOldFileName, String(MsgsFrame.FileNames.At(I).VString)) = 0) Then Begin
         MsgsFrame.Msgs.Replace(I, ReplaceStr(MsgsFrame.Msgs.Get(I), RelativeFileName(aOldFileName), RelativeFileName(aNewFileName)));

         MsgsFrame.FileNames.ReplaceWithin(I, I, [String(MsgsFrame.FileNames.At(I).VString)], [UCNewFileName]);
      End;
End;

//==============================================================================

Function TEditorFrame.SelFirstMsg(Const aFileName: String): Boolean;
Var
   I: Integer;
   UCFileName: String;
Begin
   MsgsFrame.Msgs.NodeIndex := UNDEFINED;

   // Look for the first message related to the given file, if any

   UCFileName := UpperCase(aFileName);

   For I := 0 To MsgsFrame.Msgs.Count-1 Do
      If (CompareStr(UCFileName, String(MsgsFrame.FileNames.At(I).VString)) = 0) Then Begin
         MsgsFrame.Msgs.NodeIndex := I;

         Break;
      End;

   If (MsgsFrame.Msgs.NodeIndex <> UNDEFINED) Then Begin
      MsgsFrameMsgsDblClick(MsgsFrame.Msgs);

      Result := True;
   End Else
      Result := False;
End;

//==============================================================================

Function TEditorFrame.AtLeastOneMsg(Const aFileName: String): Boolean;
Var
   I: Integer;
   UCFileName: String;
Begin
   Result := False;

   // Look for the first error related to the file, if any

   UCFileName := UpperCase(aFileName);

   For I := 0 To MsgsFrame.Msgs.Count-1 Do
      If (CompareStr(UCFileName, String(MsgsFrame.FileNames.At(I).VString)) = 0) Then Begin
         Result := True;

         Break;
      End;
End;

//==============================================================================

Procedure TEditorFrame.AddMsg(Const aMsgType: TMsgDlgType;
                              Const aFileName: String;
                              Const aLineNb, aColNb: Integer;
                              Const aMsg: String;
                              Const aGoToMsg: Boolean);
Var
   TypeStr, LeftPar, LineStr, CommaStr, ColStr, RightPar: String;
Begin
   Case aMsgType Of
      mtError:
         TypeStr := '[Error] ';
      mtWarning:
         TypeStr := '[Warning] ';
   End;

   If (aLineNb <> UNDEFINED) Then Begin
      LeftPar := ' (';

      LineStr := IntToStr(aLineNb);

      If (aColNb <> UNDEFINED) Then Begin
         CommaStr := ', ';
         ColStr   := IntToStr(aColNb);
      End Else Begin
         CommaStr := '';
         ColStr   := '';
      End;

      RightPar := ')';
   End Else Begin
      LeftPar  := '';
      LineStr  := '';
      CommaStr := '';
      ColStr   := '';
      RightPar := '';
   End;

   // Add the message

   MsgsFrame.Msgs.Add(TypeStr+RelativeFileName(aFileName)+LeftPar+LineStr+CommaStr+ColStr+RightPar+': '+aMsg);

   // Go to the message, if required

   If (aGoToMsg) Then
      MsgsFrame.Msgs.NodeIndex := MsgsFrame.Msgs.Count-1;

   // Add the message's data

   MsgsFrame.FileNames.Add([UpperCase(aFileName)]);
   MsgsFrame.LineNbs.Add([aLineNb]);
   MsgsFrame.ColNbs.Add([aColNb]);
End;

//==============================================================================

Procedure TEditorFrame.AddEngineMsgs(Const aEngineMsgs: DArray);
Var
   Iter: Integer;
Begin
   // Get ready to add messages

   MsgsFrame.Msgs.BeginUpdate;

   // Add the messages

   For Iter := 0 To aEngineMsgs.Size-1 Do
      With TEngineMsg(aEngineMsgs.At(Iter).VObject) Do
         AddMsg(MsgType, FileName, LineNb, ColNb, Msg, False);

   // Finalise things

   MsgsFrame.Msgs.EndUpdate;
End;

//==============================================================================

Procedure TEditorFrame.ClearMsgs;
Begin
   MsgsFrame.Msgs.Clear;

   MsgsFrame.FileNames.Clear;
   MsgsFrame.LineNbs.Clear;
   MsgsFrame.ColNbs.Clear;
End;

//==============================================================================

Procedure TEditorFrame.CopyMsgsToClipboard;
Begin
   Clipboard.SetTextBuf(PChar(MsgsFrame.Msgs.Strings));
End;

//==============================================================================

Procedure TEditorFrame.SyntaxEditChangeOrClickOrDblClick(aSender: TObject);
Begin
   (aSender As TSyntaxEdit).UnhighlightMsg;   // Just in case...
End;

//==============================================================================

Procedure TEditorFrame.SyntaxEditKeyDown(aSender: TObject; Var aKey: Word;
                                         aShift: TShiftState);
Begin
   SyntaxEditChangeOrClickOrDblClick(aSender);

   SyntaxEditKeyUp(aSender, aKey, aShift);
End;

//==============================================================================

Procedure TEditorFrame.SyntaxEditKeyUp(aSender: TObject; Var aKey: Word;
                                       aShift: TShiftState);
Begin
   // Some key combinations cannot be caught, yet we want to know about them
   // since it may be important (e.g. when modifying an equation the equation
   // viewer must be updated)

   If (aKey = VK_DELETE) Then
      SyntaxEditStatusChange(aSender, [])
   Else If ((UpperCase(Char(aKey))[1] = 'Y') And (ssCtrl In aShift)) Then
      SyntaxEditStatusChange(aSender, []);
End;

//==============================================================================

Procedure TEditorFrame.SyntaxEditStatusChange(aSender: TObject;
                                              aChanges: TSynStatusChanges);
   Function CellMLCmdASCIIToCellMLCmdBinTree(Const aCmd: String;
                                             Var aCmdBinTree: TCmdBinTree): Boolean;
   Var
      MathMLCommandBinTree: TMathMLCommandBinTree;
   Begin
      Result := True;

      FreeAndNil(aCmdBinTree);

      If (CompareStr(aCmd, '') <> 0) Then Begin
         If (CellMLASCIIToCellMLAPI(aCmd, MathMLCommandBinTree) And
             (MathMLCommandBinTree <> Nil)) Then Begin
            // Convert the MathML equation binary tree into an equation binary
            // tree for the command viewer

            MathMLCmdToCmdBinTreeConv(MathMLCommandBinTree, aCmdBinTree);

            // Get rid of the MathML equation binary tree

            MathMLCommandBinTree.Free;
         End Else
            Result := False;
      End;
   End;
Var
   SyntaxEdit: TSyntaxEdit;
   Cmd: String;
Begin
   If (MainForm.Mode <> mEditorial) Then
      Exit;

   SyntaxEdit := aSender As TSyntaxEdit;

   // Update the caption

   UpdateCaption;

   // Update the status bar

   StatusBarInfoEdit(SyntaxEdit);

   // Look for a valid equation, if any and if the current file is a CellML file

   If (CellMLFile(SyntaxEdit.FileName)) Then Begin
      If (SyntaxEdit.View = evCOR) Then
         Cmd := SyntaxEdit.CrtCmd
      Else
         Cmd := '';

      If (CompareStr(Cmd, CrtCmd) <> 0) Then Begin
         // The current equation is different from the previous one, so...

         CrtCmd := Cmd;

         // Generate a binary tree for the new current equation...

         If (CellMLCmdASCIIToCellMLCmdBinTree(CrtCmd, CmdBinTree)) Then
            CommandViewerFrame.CmdGraph.CmdBinTree := CmdBinTree
         Else
            CommandViewerFrame.CmdGraph.Invalid := True;
      End;
   End;
End;

//==============================================================================

Procedure TEditorFrame.TabSetChange(Sender: TObject; NewTab: Integer;
                                    Var AllowChange: Boolean);
Var
   SyntaxEdit: TSyntaxEdit;
   NewView: String;
   OldView: TEditorView;
Begin
   SyntaxEdit := PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

   If (SyntaxEdit.NewFile) Then Begin
      If (SyntaxEdit.View = evCOR) Then
         NewView := String(Views.At(Integer(evRaw)).VString)
      Else
         NewView := String(Views.At(Integer(evCOR)).VString);

      If (MessageDlg(SyntaxEdit.FileName+' has never been saved, but it needs to be before switching to the '+NewView+' view. Do you want to save it?', mtConfirmation, [mbYes, mbNo], 0) = IDNO) Then Begin
         AllowChange := False;

         Exit;
      End;
   End;

   If ((SyntaxEdit.NewFile Or (NewTab = 1)) And
       Not SaveFile(SyntaxEdit)) Then Begin
      // This is a new file, so it needs to be saved, but it can't be, so...
      // Alternatively, we want to go from the COR view to the Raw view, but the
      // CellML file cannot be saved, so...

      AllowChange := False;

      SelFirstMsg(SyntaxEdit.FileName);

      Exit;
   End;

   // Try to toggle the view

   OldView := SyntaxEdit.View;

   If (NewTab = 0) Then
      SyntaxEdit.View := evCOR
   Else
      SyntaxEdit.View := evRaw;

   AllowChange := OldView <> SyntaxEdit.View;

   If (AllowChange) Then Begin
      // Unhighlight things, if necessary

      SyntaxEdit.UnhighlightMsg;

      // Remove any message associated with the file, since we just changed views

      ClearSpecMsgs(SyntaxEdit.FileName);

      // Update the tab set

      UpdateTabSet(SyntaxEdit, False);
   End;
End;

//==============================================================================

Procedure TEditorFrame.UpdateMRUList;
Var
   I: Integer;
   MenuItem1: TMenuItem;
   MenuItem2: TMenuItem;
Begin
   // Update/create the MRU list for the main and popup menus

   MenuFileReopen.Clear;
   MRUListPopupMenu.Items.Clear;

   For I := 0 To MAX_NB_OF_MRU_FILES Do
      If (CompareStr(EditorFrameMRUFiles[I], '') <> 0) Then Begin
         MenuItem1 := TMenuItem.Create(MenuFileReopen);

         MenuItem1.Caption := EditorFrameMRUFiles[I];
         MenuItem1.Hint    := 'Open|Open '+MenuItem1.Caption;
         MenuItem1.OnClick := ActionFileReopenMRUFileExecute;

         MenuFileReopen.Add(MenuItem1);

         MenuItem2 := TMenuItem.Create(MRUListPopupMenu);

         MenuItem2.Caption := MenuItem1.Caption;
         MenuItem2.Hint    := MenuItem1.Hint;
         MenuItem2.OnClick := MenuItem1.OnClick;

         MRUListPopupMenu.Items.Add(MenuItem2);
      End Else
         Break;
End;

//==============================================================================

Function TEditorFrame.EnterMode: Boolean;
Begin
   CheckFilesTimer.Enabled := True;

   Result := True;
End;

//==============================================================================

Function TEditorFrame.ExitMode: Boolean;
Begin
   CheckFilesTimer.Enabled := False;

   Result := True;
End;

//==============================================================================

Procedure TEditorFrame.AddMRUFile(Const aFileName: String);
Var
   I: Integer;
Begin
   // First, check that the file/exe name is not already in the list

   For I := 0 To MAX_NB_OF_MRU_FILES Do
      If (CompareStr(EditorFrameMRUFiles[I], aFileName) = 0) Then Begin
         RemoveMRUFile(aFileName);

         Break;
      End;

   // Shift everything and put the file/exe name in top position

   For I := MAX_NB_OF_MRU_FILES DownTo 1 Do
      EditorFrameMRUFiles[I] := EditorFrameMRUFiles[I-1];

   EditorFrameMRUFiles[0] := aFileName;

   UpdateMRUList;
End;

//==============================================================================

Procedure TEditorFrame.RemoveMRUFile(Const aFileName: String);
Var
   CanShift: Boolean;
   I: Integer;
Begin
   CanShift := False;

   For I := 0 To MAX_NB_OF_MRU_FILES Do Begin
      If (Not CanShift And (CompareStr(EditorFrameMRUFiles[I], aFileName) = 0)) Then
         CanShift := True;

      If (CanShift) Then Begin
         If (I <> MAX_NB_OF_MRU_FILES) Then
            EditorFrameMRUFiles[I] := EditorFrameMRUFiles[I+1]
         Else
            EditorFrameMRUFiles[I] := '';
      End;
   End;

   UpdateMRUList;
End;

//==============================================================================

Procedure TEditorFrame.ClosePage(Const aTabSheet: TTabSheet);
Begin
   // Remove the page

   aTabSheet.Visible := False;

   aTabSheet.Free;
   // Note: this also frees the editor...

   // Make sure that the "new" syntax editor, if any, has the focus. If no
   // syntax editor is left, then reset the equation graph and the status line

   If (PageCtrl.PageCount <> 0) Then
      PageCtrlChange(Self)
   Else Begin
      CommandViewerFrame.CmdGraph.CmdBinTree := Nil;

      UpdateCaption;

      TabSet.Visible := False;

      StatusBarInfoHint;
   End;
End;

//==============================================================================

Function TEditorFrame.OpenFile(Const aFileName: String;
                               Const aView: TEditorView;
                               Const aFileExt: String;
                               Const aSyntaxEdit: TSyntaxEdit): Boolean;
   Procedure CleanUp(Const aFileName: String);
   Begin
      RemoveMRUFile(aFileName);

      ClearSpecMsgs(aFileName);
   End;
Var
   NewFile: Boolean;
   FileName, UCFileName: String;
   I: Integer;
   TabSheet: TTabSheet;
   SyntaxEdit: TSyntaxEdit;
   ErrorMsg: String;
   FileHandle: Integer;
Begin
   // Check whether the file exists

   FileName := ExpandFileName(aFileName);

   NewFile := CompareStr(FileName, '') = 0;

   If (Not NewFile) Then Begin
      ErrorMsg := '';

      If (Not FileExists(FileName)) Then
         // The file doesn't exist (anymore, maybe), so clean up anything that is
         // related to the file and let the user know about it

         ErrorMsg := 'does not exist and therefore cannot be opened'
      Else Begin
         FileHandle := FileOpen(aFileName, fmOpenRead);

         If (FileHandle < 0) Then
            ErrorMsg := 'cannot be opened'
         Else
            FileClose(FileHandle);
      End;

      If (CompareStr(ErrorMsg, '') <> 0) Then Begin
         CleanUp(FileName);

         AddMsg(mtError, FileName, UNDEFINED, UNDEFINED, ErrorMsg, True);

         Result := False;

         Exit;
      End;
   End;

   // Assign a default name to the file, If necessary and check that the file is
   // not already present (in case the file is not new)

   SyntaxEdit := Nil;

   If (NewFile) Then Begin
      If (aFileExt = CELLML_FILE_EXT) Then Begin
         FileName := COR_MODELS_PATH+'CellML File #'+IntToStr(NewCellMLFileID)+CELLML_FILE_EXT;

         Inc(NewCellMLFileID);
      End Else Begin
         FileName := 'File #'+IntToStr(NewFileID)+aFileExt;

         Inc(NewFileID);
      End;
   End Else Begin
      // Check whether the file isn't already opened         

      UCFileName := UpperCase(FileName);

      For I := 0 To PageCtrl.PageCount-1 Do
         If (CompareStr(UpperCase((PageCtrl.Pages[I].Controls[0] As TSyntaxEdit).FileName), UCFileName) = 0) Then Begin
            SyntaxEdit := PageCtrl.Pages[I].Controls[0] As TSyntaxEdit;

            RemoveMRUFile(FileName);   // Just in case we try to open a file
                                       // with the same name, but with different
                                       // sets of lower/upper cases...

            Break;
         End;
   End;

   If (SyntaxEdit = Nil) Then Begin
      If (Not NewFile) Then
         CleanUp(FileName);

      // Create a sheet for the new file

      TabSheet := TTabSheet.Create(PageCtrl);

      TabSheet.Caption := ExtractCellMLFileName(FileName);

      TabSheet.PageControl := PageCtrl;

      If (PageCtrl.PageCount > 1) Then
         TabSheet.PageIndex := PageCtrl.ActivePageIndex+1;

      // Create an editor within the sheet (editor, which syntax is highlighted
      // or not, depending on the file type)

      SyntaxEdit := TSyntaxEdit.Create(TabSheet, FileName, NewFile);

      SyntaxEdit.SetOptions(EditorOptions);

      SyntaxEdit.View := aView;

      SyntaxEdit.PopupMenu := PopupMenu;

      SyntaxEdit.OnChange       := SyntaxEditChangeOrClickOrDblClick;
      SyntaxEdit.OnClick        := SyntaxEditChangeOrClickOrDblClick;
      SyntaxEdit.OnDblClick     := SyntaxEditChangeOrClickOrDblClick;
      SyntaxEdit.OnKeyDown      := SyntaxEditKeyDown;
      SyntaxEdit.OnKeyUp        := SyntaxEditKeyUp;
      SyntaxEdit.OnStatusChange := SyntaxEditStatusChange;

      // Load the file, if it exists, or create a skeleton for it, if it is a
      // new one

      If (Not NewFile) Then
         SyntaxEdit.LoadFile(SyntaxEdit.FileName)
      Else If (aSyntaxEdit <> Nil) Then
         // Want to duplicate a file, so...

         SyntaxEdit.Duplicate(aSyntaxEdit)
      Else If (CellMLFile(SyntaxEdit.FileName)) Then
         SyntaxEdit.NewCellMLFile;
   End;

   // Show the editor form and set the focus on the file that we just added or
   // created (but only if we are in editorial mode!)
   // Note: would we always create new files, we wouldn't need to go through all
   //       the tab sheets, but since we don't...

   If (MainForm.Mode = mEditorial) Then Begin
      For I := 0 To PageCtrl.PageCount-1 Do
         If (PageCtrl.Pages[I].Controls[0] As TSyntaxEdit = SyntaxEdit) Then Begin
            // Note: we cannot just compare the file name, since a new file will
            //       have a name, which is located in the "root" directory, and
            //       that there might be a file with the exact same name, so...

            PageCtrl.ActivePage := PageCtrl.Pages[I];

            SetFocusTo(SyntaxEdit);

            Break;
         End;

      // Ensure that everything related to the editor is up to date (e.g. the
      // command viewer)

      PageCtrlChange(Self);
   End;

   Result := True;
End;

//==============================================================================

Function TEditorFrame.SaveFile(Const aSyntaxEdit: TSyntaxEdit;
                               Const aSaveAs: Boolean): Boolean;
Begin
   Result := aSyntaxEdit.SaveFile(aSaveAs, Nil, Nil, False);

   // Update the tab set

   UpdateTabSet(aSyntaxEdit);
End;

//==============================================================================

Function TEditorFrame.RunFile(Const aFileName: String): Boolean;
Var
   FileName: String;
   SyntaxEdit: TSyntaxEdit;
Begin
   Result := False;   // By default the file cannot be run properly

   // Remove any message related to the current file, should there be any, and
   // check that the file actually exists

   SyntaxEdit := PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

   FileName := ExpandFileName(aFileName);

   ClearSpecMsgs(FileName);

   If (Not FileExists(FileName)) Then Begin
      // The file doesn't exist, so save it first...

      SaveFile(SyntaxEdit);

      If (Not FileExists(SyntaxEdit.FileName)) Then Begin
         // The file still doesn't exist, so either the user didn't save it or
         // it's not a valid file

         If (Not AtLeastOneMsg(FileName)) Then
            // The file has definitely not been saved, so...

            MessageDlg('The CellML file has not been saved and can, therefore, not be run.', mtWarning, [mbOK], 0);

         Exit;
      End;

      // Remove any message related to the current file, should there be any,
      // and update the file name

      ClearSpecMsgs(FileName);

      FileName := SyntaxEdit.FileName;
   End;

   // Run the file

   Result := SyntaxEdit.RunFile;
End;

//==============================================================================

Procedure TEditorFrame.CheckFilesTimerTimer(Sender: TObject);
Var
   I: Integer;
   SyntaxEdit: TSyntaxEdit;
   NeedUpdateActivePage: Boolean;
Begin
   CheckFilesTimer.Interval := 1000;   // In case it has been reset (see
                                       // "WMActivate")...

   If (Not CheckingFiles And Application.Active And
       (PageCtrl.ActivePage <> Nil) And (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).Focused) Then Begin
      // We are directly dealing with the editor, so...

      // Check that all the files in the editor are up to date

      CheckingFiles := True;

      NeedUpdateActivePage := False;

      For I := 0 To PageCtrl.PageCount-1 Do Begin
         SyntaxEdit := PageCtrl.Pages[I].Controls[0] As TSyntaxEdit;

         If (Not SyntaxEdit.NewFile) Then Begin
            If (Not FileExists(SyntaxEdit.FileName)) Then Begin
               // The file doesn't exist anymore, so check with the user whether
               // to keep it or not in the editor

               If (MessageDlg(SyntaxEdit.FileName+' does not exist anymore. Do you want to close it?', mtConfirmation, [mbYes, mbNo], 0) = IDYES) Then Begin
                  // Force the closing of the file, as well as clear all
                  // messages related to it

                  ClearSpecMsgs(SyntaxEdit.FileName);

                  SyntaxEdit.CloseFile(True);

                  Break;
                  // Note: we have just closed a file, so we cannot carry on
                  //       going through the remaining files, as this may cause
                  //       problems (e.g. say we have two open files and we
                  //       close the first one, then the second one's index will
                  //       become 0 while we would here try to access one with
                  //       an index of 1... hence exception!), so...
               End Else
                  // The user wants to keep the file in the editor, so make it a
                  // new file, so that it can be properly saved if needed

                  With SyntaxEdit Do Begin
                     NewFile  := True;
                     Modified := True;
                     MD5      := '';
                  End;
            End Else If (((FileMD5(SyntaxEdit.FileName) <> SyntaxEdit.MD5) Or
                          (FileIsReadOnly(SyntaxEdit.FileName) <> SyntaxEdit.ReadOnly)) And
                         (MessageDlg(SyntaxEdit.FileName+' has changed. Do you want to reload it?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)) Then Begin
               // Reload the file
               // Note: we don't care about where the caret is after reloading
               //       the file. Should we?

               ClearSpecMsgs(SyntaxEdit.FileName);

               SyntaxEdit.LoadFile(SyntaxEdit.FileName);

               NeedUpdateActivePage := NeedUpdateActivePage Or
                                       (I = PageCtrl.ActivePageIndex);
            End Else
               SyntaxEdit.MD5 := FileMD5(SyntaxEdit.FileName);
               // Note: just to be on the safe side, in case the file's MD5 has
               //       changed, but the user doesn't want to reload the file,
               //       so updating the MD5 will ensure that the user doesn't
               //       get asked again in a second or so...
         End;
      End;

      // Ensure that everything related to the editor is up to date

      If (NeedUpdateActivePage) Then
         PageCtrlChange(Self);

      CheckingFiles := False;
   End;
End;

//==============================================================================

Function TEditorFrame.GetDockSiteSize(aDockSite: TPanel): Pointer;
Begin
   If (aDockSite = TopDockSite) Then
      Result := @EditorFrameTopDockSiteSize
   Else If (aDockSite = LeftDockSite) Then
      Result := @EditorFrameLeftDockSiteSize
   Else If (aDockSite = BottomDockSite) Then
      Result := @EditorFrameBottomDockSiteSize
   Else
      Result := @EditorFrameRightDockSiteSize;
End;

//==============================================================================

Procedure TEditorFrame.MsgsFrameMsgsDblClick(Sender: TObject);
Var
   I: Integer;
   SyntaxEdit: TSyntaxEdit;
   FileName: String;
Begin
   If (MsgsFrame.Msgs.NodeIndex = UNDEFINED) Then
      Exit;

   FileName := String(MsgsFrame.FileNames.At(MsgsFrame.Msgs.NodeIndex).VString);

   If (CompareStr(FileName, '') = 0) Then
      Exit;

   // Set the focus to the file that contains the error

   SyntaxEdit := Nil;

   For I := 0 To PageCtrl.PageCount-1 Do
      If (CompareStr(UpperCase((PageCtrl.Pages[I].Controls[0] As TSyntaxEdit).FileName), FileName) = 0) Then Begin
         PageCtrl.ActivePage := PageCtrl.Pages[I];

         SyntaxEdit := PageCtrl.ActivePage.Controls[0] As TSyntaxEdit;

         SetFocusTo(SyntaxEdit);

         Break;
      End;

   // Ensure that everything related to the editor is up to date (e.g. the
   // command viewer)

   PageCtrlChange(Self);

   // Highlight the line that contains the error...

   If (SetFocusTo(SyntaxEdit)) Then
      SyntaxEdit.HighlightMsg(MsgsFrame.LineNbs.At(MsgsFrame.Msgs.NodeIndex).VInteger,
                              MsgsFrame.ColNbs.At(MsgsFrame.Msgs.NodeIndex).VInteger);
End;

//==============================================================================

Procedure TEditorFrame.ActionCmdGraphUpdate(Sender: TObject);
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (CmdBinTree <> Nil) And
                                  Not CommandViewerFrame.CmdGraph.Invalid;
End;

//==============================================================================

Procedure TEditorFrame.ActionDummyExecute(Sender: TObject);
Begin
   // Do nothing, and that's exactly what we want! This allows File|New to work,
   // since there must a handler for "OnExecute" as otherwise the action will
   // simply be disabled...
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsCommandViewerOptionsExecute(Sender: TObject);
Begin
   OptionsForm(EDITOR_OPTIONS, COMMAND_VIEWER_SUB_OPTIONS);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsEditorOptionsExecute(Sender: TObject);
Begin
   OptionsForm(EDITOR_OPTIONS);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToCExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efC);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToCPPExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efCPP);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToDelphiForWin32Execute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efDelphiForWin32);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToFortran77Execute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efFortran77);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToJavaExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efJava);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToMATLABExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efMATLAB);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToMSWordExecute(Sender: TObject);
Var
   WordAlreadyRunning: Boolean;
   WordClassID: TCLSID;
   Unknown: IUnknown;
   CanExit: Boolean;
Begin
   Repeat
      // Check whether MS Word is already running or not

      Try
         WordClassID := ProgIDToClassID('Word.Application');

         WordAlreadyRunning  := GetActiveObject(WordClassID, Nil, Unknown) = S_OK;
      Except
         WordAlreadyRunning := False;
      End;

      If (WordAlreadyRunning) Then
         CanExit := MessageDlg('Please close Microsoft Word 2007/2010 before proceeding.', mtInformation, [mbRetry, mbCancel], 0) = mrCancel
      Else Begin
         (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efMSWord);

         CanExit := True;
      End;
   Until CanExit;
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToPascalExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efPascal);
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsExportToTeXExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ExportCellMLFile(efTeX);
End;

//==============================================================================

Procedure TEditorFrame.ActionValidCellMLFile(Sender: TObject);
Var
   ClassID: TCLSID;
   Registry: TRegistry;
Begin
   (Sender As TAction).Enabled := (MainForm.Mode = mEditorial) And
                                  (PageCtrl.ActivePage <> Nil) And
                                  CellMLFile((PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName);

   If ((Sender As TAction).Enabled) Then Begin
      // The action should be active, but some actions may require an additional
      // test, so...

      If (Sender = ActionToolsExportToMSWord) Then Begin
         (Sender As TAction).Enabled := False;

         If (CLSIDFromProgID('Word.Application', ClassID) = S_OK) Then Begin
            // The "Word.Application" class has been registered, so MS Word is
            // installed, but is it the right version?!

            Registry := TRegistry.Create;

            Try
               Registry.RootKey := HKEY_CLASSES_ROOT;

               If (Registry.OpenKeyReadOnly('\Word.Application\CurVer')) Then
                  (Sender As TAction).Enabled := (CompareStr(Registry.ReadString(''), 'Word.Application.12') = 0) Or
                                                 (CompareStr(Registry.ReadString(''), 'Word.Application.14') = 0);
            Finally
               Registry.CloseKey;

               Registry.Free;
            End;
         End;
      End Else If (Sender = ActionToolsReformat) Then
         (Sender As TAction).Enabled := Not (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).ReadOnly;
   End;
End;

//==============================================================================

Procedure TEditorFrame.ActionUncategorisedClearMsgsExecute(Sender: TObject);
Begin
   ClearMsgs;
End;

//==============================================================================

Procedure TEditorFrame.ActionUncategorisedCopyMsgsToClipboardExecute(Sender: TObject);
Begin
   CopyMsgsToClipboard;
End;

//==============================================================================

Procedure TEditorFrame.ActionUncategorisedCopyCmdGraphToClipboardExecute(
  Sender: TObject);
Begin
   CommandViewerFrame.CmdGraph.CopyToClipboard;
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsCheckValidityExecute(Sender: TObject);
Begin
   (PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).IsValid;
End;

//==============================================================================

Procedure TEditorFrame.ActionToolsReformatExecute(Sender: TObject);
Var
   TL: Integer;
   CXY: TBufferCoord;
Begin
   With PageCtrl.ActivePage.Controls[0] As TSyntaxEdit Do Begin
      TL  := TopLine;
      CXY := CaretXY;

      ClearSpecMsgs(FileName);

      If (Reformat) Then Begin
         CaretXY := CXY;
         TopLine := TL;
         // Note: we must set the top line after setting the caret's
         //       coordinates, as otherwise the top line will be overridden...
      End;
   End;
End;

//==============================================================================

Procedure TEditorFrame.ActionUncategorisedCommandViewerFormattingExecute(Sender: TObject);
Var
   EVOptions: Integer;
Begin
   With EditorOptions.CommandViewerOptions Do Begin
      EVOptions := 0;

      If (UnderscoreForSub) Then
         Inc(EVoptions);

      If (GreekSymbols) Then
         Inc(EVoptions, 2);

      If (DigitGrouping) Then
         Inc(EVoptions, 4);

      If (Sender = ActionViewCommandViewerChangeFormattingForward) Then
         EVOptions := (EVOptions+1) Mod 8
      Else Begin
         EVoptions := (EVOptions-1) Mod 8;

         If (EVOptions = -1) Then
            EVOptions := 7;
      End;

      UnderScoreForSub := (EVoptions Mod 2) = 1;
      GreekSymbols     := ((EVoptions ShR 1) Mod 2) = 1;
      DigitGrouping    := ((EVoptions ShR 2) Mod 2) = 1;

      CommandViewerFrame.CmdGraph.UnderscoreForSub := UnderscoreForSub;
      CommandViewerFrame.CmdGraph.GreekSymbols     := GreekSymbols;
      CommandViewerFrame.CmdGraph.DigitGrouping    := DigitGrouping;
   End;
End;

//==============================================================================

Procedure TEditorFrame.PageCtrlMouseDown(Sender: TObject; Button: TMouseButton;
                                         Shift: TShiftState; X, Y: Integer);
Begin
   PageCtrl.BeginDrag(False) ;
End;

//==============================================================================

Procedure TEditorFrame.PageCtrlDragOver(Sender, Source: TObject; X, Y: Integer;
                                        State: TDragState; var Accept: Boolean);
Begin
   Accept := (Sender Is TPageControl) And (TPageControl(Sender) = PageCtrl);
   // Just to be 100% sure that we are not trying to drop anything on the page
   // control, besides one of its pages...
End;

//==============================================================================

Procedure TEditorFrame.PageCtrlDragDrop(Sender, Source: TObject; X, Y: Integer);
Var
   TabRect: TRect;
   I: Integer;
Begin
   If (Sender Is TPageControl) Then
      For I := 0 to PageCtrl.PageCount-1 Do Begin
         PageCtrl.Perform(TCM_GETITEMRECT, I, LParam(@TabRect));

         If (PtInRect(TabRect, Point(X, Y))) Then Begin
            If (PageCtrl.ActivePage.PageIndex <> I) Then
               PageCtrl.ActivePage.PageIndex := I;

            Exit;
         End;
      End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

