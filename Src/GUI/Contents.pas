//==============================================================================
// Contents frame
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
// Notes:
//  - "ControlBarMenuToolBar" must not be made dockable, as otherwise, when
//    going through the sub-menus, and then deciding not to select one may
//    result in the toolbar starting a docking process (!!)... Probably the
//    reason other applications don't allow the menu to be dockable...
//  - "DummyPanel" is very important to get the right dock site working
//    properly. Without it, a view docked to the right dock site cannot be
//    resized (!!)... So, "DummyPanel" is an ugly trick that allows the
//    resizing...  
//==============================================================================

Unit Contents;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, Messages, Classes, Controls, Forms, Menus, ActnList, ImgList,
   ComCtrls, ExtCtrls, ToolWin, SyntaxEdit;

//==============================================================================

Const
   SPLITTER_SIZE = 3;

//==============================================================================

Type
   TContentsFrameMode = (mEditorial, mComputational);
   TContentsStatusBarState = (sbsHint, sbsEditor, sbsProgress);
   TContentsFrame = Class(TFrame)
    ControlBar: TControlBar;
    ControlBarMenuToolBar: TToolBar;
    ControlBarRunMenuToolBar: TToolBar;
    StatusBar: TStatusBar;
    RootPanel: TPanel;
    BottomSplitter: TSplitter;
    TopSplitter: TSplitter;
    LeftSplitter: TSplitter;
    RightSplitter: TSplitter;
    BottomDockSite: TPanel;
    TopDockSite: TPanel;
    LeftDockSite: TPanel;
    RightDockSite: TPanel;
    Menu: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuView: TMenuItem;
    MenuViewSep1: TMenuItem;
    MenuViewToolBars: TMenuItem;
    MenuViewToolBarsRun: TMenuItem;
    MenuRun: TMenuItem;
    MenuRunRun: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpAbout: TMenuItem;
    ImageList: TImageList;
    ActionList: TActionList;
    ActionViewRunToolBar: TAction;
    ActionFileExit: TAction;
    ActionRunRun: TAction;
    ActionHelpAbout: TAction;
    ControlBarPopupMenu: TPopupMenu;
    ControlBarPopupMenuRunMenuItem: TMenuItem;
    ControlBarFileMenuToolBar: TToolBar;
    DummyPanel: TPanel;
    ActionViewFileToolBar: TAction;
    MenuViewToolbarsFile: TMenuItem;
    ControlBarPopupMenuFileMenuItem: TMenuItem;
    MenuHelpSep1: TMenuItem;
    ActionHelpHelp: TAction;
    MenuHelpHelp: TMenuItem;
    MenuHelpSep3: TMenuItem;
    ActionHelpHomePage: TAction;
    MenuHelpHomePage: TMenuItem;
    MenuTools: TMenuItem;
    MenuToolsOptions: TMenuItem;
    ActionToolsOptions: TAction;
    ActionHelpUpdate: TAction;
    MenuHelpUpdate: TMenuItem;
    ActionHelpCellMLHomePage: TAction;
    MenuHelpSep4: TMenuItem;
    MenuHelpCellMLRepository: TMenuItem;
    MenuHelpCellMLHomePage: TMenuItem;
    ActionHelpCellMLRepository: TAction;
    ActionHelpFAQ: TAction;
    MenuHelpFAQ: TMenuItem;
    MenuHelpSep2: TMenuItem;
    TopPanel: TPanel;
    ProgressBar: TProgressBar;
    ActionHelpHowTo: TAction;
    MenuHelpSep5: TMenuItem;
    MenuHelpHowTo: TMenuItem;
    ActionToolsResetAll: TAction;
    MenuToolsResetAll: TMenuItem;
    MenuToolsSep1: TMenuItem;
    procedure ActionHelpFAQExecute(Sender: TObject);
    procedure ControlBarGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure DockSiteGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure DockSiteDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TopDockSiteDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure LeftDockSiteDockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure BottomDockSiteDockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure RightDockSiteDockDrop(Sender: TObject;
      Source: TDragDockObject; X, Y: Integer);
    procedure TopDockSiteUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure LeftDockSiteUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure BottomDockSiteUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure RightDockSiteUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionViewToolBarsExecute(Sender: TObject);
    procedure ActionViewToolBarsUpdate(Sender: TObject);
    procedure ActionHelpHelpExecute(Sender: TObject);
    procedure ActionHelpHomePageExecute(Sender: TObject);
    procedure ActionToolsOptionsExecute(Sender: TObject);
    procedure ActionHelpUpdateExecute(Sender: TObject);
    procedure ActionHelpCellMLHomePageExecute(Sender: TObject);
    procedure ActionHelpCellMLRepositoryExecute(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure ActionHelpHowToExecute(Sender: TObject);
    procedure ActionHelpUpdateUpdate(Sender: TObject);
    procedure ActionToolsResetAllExecute(Sender: TObject);
      Private
         OldTopDockWndProc: TWndMethod;
         OldLeftDockWndProc: TWndMethod;
         OldBottomDockWndProc: TWndMethod;
         OldRightDockWndProc: TWndMethod;

         FActiveControl: TWinControl;

         StatusBarState: TContentsStatusBarState;

         Procedure TopDockWndProc(Var aMsg: TMessage);
         Procedure LeftDockWndProc(Var aMsg: TMessage);
         Procedure BottomDockWndProc(Var aMsg: TMessage);
         Procedure RightDockWndProc(Var aMsg: TMessage);

      Protected
         Function GetDockSiteSize(aDockSite: TPanel): Pointer; Virtual; Abstract;

         Procedure DockSiteVisibilityChanged(aDockSite: TPanel; aFrame: TFrame; Const aVisible: Boolean); Virtual;

         Procedure ShowHideControl(aControl: TControl);

      Public
         Constructor Create(aOwner: TComponent); Override;

         Procedure FrameCreate; Virtual;
         Procedure FrameDestroy;

         Procedure BackupToolBarsAndViews; Virtual; Abstract;
         Procedure RestoreToolBarsAndViews; Virtual; Abstract;

         Function EnterMode: Boolean; Virtual;
         Function ExitMode: Boolean; Virtual;

         Procedure StatusBarInfoHint;
         Procedure StatusBarInfoEdit(aSyntaxEdit: TSyntaxEdit); Inline;
         Procedure StatusBarInfoProgBar(Const aPosition: Integer = 0);

         Procedure UpdateCaption;

      Published
         Property ActiveControl: TWinControl Read FActiveControl Write FActiveControl;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   SysUtils, Main, DockSites, Consts, Common, CORCommon, About, CellMLAPI,
   CORHelp, JclSecurity, Dialogs, Registry;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TContentsFrame.Create(aOwner: TComponent);
Begin
   Inherited;

   // Assign the right dock class for the toolbars and views

   ControlBarFileMenuToolBar.FloatingDockSiteClass := TToolBarDockForm;
   ControlBarRunMenuToolBar.FloatingDockSiteClass  := TToolBarDockForm;

   // Status bar

   StatusBarState := sbsHint;

   // Connect the progress bar to the status bar and remove its border

   With ProgressBar Do Begin
      Parent := StatusBar;

      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE)-WS_EX_STATICEDGE);
   End;

   // Initialise the caption (useful if no file is to be opened)

   UpdateCaption;
End;

//==============================================================================

Procedure TContentsFrame.FrameCreate;
Begin
   Align := alClient;

   // Change the window procedure of the different dock sites

   OldTopDockWndProc    := TopDockSite.WindowProc;
   OldLeftDockWndProc   := LeftDockSite.WindowProc;
   OldBottomDockWndProc := BottomDockSite.WindowProc;
   OldRightDockWndProc  := RightDockSite.WindowProc;

   TopDockSite.WindowProc    := TopDockWndProc;
   LeftDockSite.WindowProc   := LeftDockWndProc;
   BottomDockSite.WindowProc := BottomDockWndProc;
   RightDockSite.WindowProc  := RightDockWndProc;
End;

//==============================================================================

Procedure TContentsFrame.FrameDestroy;
Begin
   // Reset the window procedure of the different dock sites

   If (@OldTopDockWndProc <> Nil) Then
      TopDockSite.WindowProc := OldTopDockWndProc;

   If (@OldLeftDockWndProc <> Nil) Then
      LeftDockSite.WindowProc := OldLeftDockWndProc;

   If (@OldBottomDockWndProc <> Nil) Then
      BottomDockSite.WindowProc := OldBottomDockWndProc;

   If (@OldRightDockWndProc <> Nil) Then
      RightDockSite.WindowProc := OldRightDockWndProc;
End;

//==============================================================================

Function TContentsFrame.EnterMode: Boolean;
Begin
   Result := True;
End;

//==============================================================================

Function TContentsFrame.ExitMode: Boolean;
Begin
   Result := True;
End;

//==============================================================================

Procedure TContentsFrame.StatusBarInfoHint;
Begin
   StatusBar.SimpleText := GetLongHint(Application.Hint);

   If (StatusBarState <> sbsHint) Then Begin
      StatusBar.SimplePanel := True;

      ProgressBar.Visible := False;

      StatusBarState := sbsHint;
   End;
End;

//==============================================================================

Procedure TContentsFrame.StatusBarInfoEdit(aSyntaxEdit: TSyntaxEdit);
Begin
   If (StatusBarState <> sbsEditor) Then Begin
      StatusBar.Panels.Clear;

      ProgressBar.Visible := False;

      StatusBar.Panels.Add;
      StatusBar.Panels.Add;
      StatusBar.Panels.Add;

      StatusBar.Panels.Items[0].Alignment := taCenter;
      StatusBar.Panels.Items[0].Width     := 70;

      StatusBar.Panels.Items[1].Width := 70;
   End;

   With aSyntaxEdit Do Begin
      StatusBar.Panels.Items[0].Text := SysUtils.Format('%5D:%5D', [CaretY, CaretX]);

      If (Modified) Then
         StatusBar.Panels.Items[1].Text := 'Modified'
      Else
         StatusBar.Panels.Items[1].Text := '';

      If (ReadOnly) Then
         StatusBar.Panels.Items[2].Text := 'Locked'
      Else If (InsertMode) Then
         StatusBar.Panels.Items[2].Text := 'Insert'
      Else
         StatusBar.Panels.Items[2].Text := 'Overwrite';
   End;

   If (StatusBarState <> sbsEditor) Then Begin
      StatusBar.SimplePanel := False;

      StatusBarState := sbsEditor;
   End;
End;

//==============================================================================

Procedure TContentsFrame.StatusBarInfoProgBar(Const aPosition: Integer);
Begin
   ProgressBar.Position := aPosition;

   If (StatusBarState <> sbsProgress) Then Begin
      StatusBar.Panels.Clear;

      StatusBar.Panels.Add;

      StatusBar.Panels[0].Style := psOwnerDraw;

      ProgressBar.Visible := True;

      StatusBar.SimplePanel := False;

      StatusBarState := sbsProgress;

      StatusBar.Refresh;   // To force the "OnDrawPanel" event to be raised, as
                           // otherwise the progress bar may not show up
                           // properly
   End;
End;

//==============================================================================

Procedure TContentsFrame.UpdateCaption;
Var
   NewCaption: String;
Begin
   // Update the caption

   NewCaption := Application.Title;

   Case MainForm.Mode Of
      mEditorial:
         NewCaption := NewCaption+' [Editorial Mode]';
      mComputational:
         NewCaption := NewCaption+' [Computational Mode]';
   End;

   If (MainForm.EditorFrame.PageCtrl.ActivePage <> Nil) Then Begin
      NewCaption := NewCaption+' - '+RelativeFileName((MainForm.EditorFrame.PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).FileName);

      If ((MainForm.EditorFrame.PageCtrl.ActivePage.Controls[0] As TSyntaxEdit).Modified) Then
         NewCaption := NewCaption+' (Modified)';
   End;

   If (CompareStr(NewCaption, MainForm.Caption) <> 0) Then
      MainForm.Caption := NewCaption;   // Avoid updating for nothing, thus
                                        // avoiding any flickering
End;

//==============================================================================

Procedure TContentsFrame.ControlBarGetSiteInfo(Sender: TObject;
                                               DockClient: TControl;
                                               Var InfluenceRect: TRect;
                                               MousePos: TPoint;
                                               Var CanDock: Boolean);
Begin
   CanDock := DockClient.Tag = 1;   // Only accept toolbars and the like...
End;

//==============================================================================

Procedure TContentsFrame.DockSiteGetSiteInfo(Sender: TObject;
                                             DockClient: TControl;
                                             Var InfluenceRect: TRect;
                                             MousePos: TPoint;
                                             Var CanDock: Boolean);
Begin
   CanDock := DockClient.Tag = 2;   // Only accept messages, etc. frame
End;

//==============================================================================

Procedure TContentsFrame.DockSiteDockOver(Sender: TObject;
                                          Source: TDragDockObject;
                                          X, Y: Integer;
                                          State: TDragState;
                                          Var Accept: Boolean);
Var
   DockRect: TRect;
Begin
   DockRect := Source.DockRect;

   If ((Sender As TPanel).Width = 0) Then Begin
      If (Sender = LeftDockSite) Then
         DockRect.Right := Source.DockRect.Left+Integer(GetDockSiteSize(Sender As TPanel)^)
      Else
         DockRect.Left := Source.DockRect.Left-Integer(GetDockSiteSize(Sender As TPanel)^);
   End Else If ((Sender As TPanel).Height = 0) Then Begin
      If (Sender = TopDockSite) Then
         DockRect.Bottom := Source.DockRect.Top+Integer(GetDockSiteSize(Sender As TPanel)^)
      Else
         DockRect.Top := Source.DockRect.Top-Integer(GetDockSiteSize(Sender As TPanel)^);
   End;

   Source.DockRect := DockRect;
End;

//==============================================================================

Procedure TContentsFrame.TopDockSiteDockDrop(Sender: TObject;
                                             Source: TDragDockObject;
                                             X, Y: Integer);
Begin
   If (TopDockSite.Height = 0) Then
      TopDockSite.Height := Integer(GetDockSiteSize(Sender As TPanel)^);

   TopSplitter.Height := SPLITTER_SIZE;
End;

//==============================================================================

Procedure TContentsFrame.LeftDockSiteDockDrop(Sender: TObject;
                                              Source: TDragDockObject;
                                              X, Y: Integer);
Begin
   If (LeftDockSite.Width = 0) Then
      LeftDockSite.Width := Integer(GetDockSiteSize(Sender As TPanel)^);

   LeftSplitter.Width := SPLITTER_SIZE;
End;

//==============================================================================

Procedure TContentsFrame.BottomDockSiteDockDrop(Sender: TObject;
                                                Source: TDragDockObject;
                                                X, Y: Integer);
Begin
   If (BottomDockSite.Height = 0) Then
      BottomDockSite.Height := Integer(GetDockSiteSize(Sender As TPanel)^);

   BottomSplitter.Height := SPLITTER_SIZE;
End;

//==============================================================================

Procedure TContentsFrame.RightDockSiteDockDrop(Sender: TObject;
                                               Source: TDragDockObject;
                                               X, Y: Integer);
Begin
   If (RightDockSite.Width = 0) Then
      RightDockSite.Width := Integer(GetDockSiteSize(Sender As TPanel)^);

   RightSplitter.Width := SPLITTER_SIZE;
End;

//==============================================================================

Procedure TContentsFrame.TopDockSiteUnDock(Sender: TObject; Client: TControl;
                                           NewTarget: TWinControl;
                                           Var Allow: Boolean);
Begin
   If (TopDockSite.VisibleDockClientCount = 1) Then Begin
      Integer(GetDockSiteSize(Sender As TPanel)^) := TopDockSite.Height;

      TopDockSite.Height := 0;

      TopSplitter.Height := 0;
   End;
End;

//==============================================================================

Procedure TContentsFrame.LeftDockSiteUnDock(Sender: TObject; Client: TControl;
                                            NewTarget: TWinControl;
                                            Var Allow: Boolean);
Begin
   If (LeftDockSite.VisibleDockClientCount = 1) Then Begin
      Integer(GetDockSiteSize(Sender As TPanel)^) := LeftDockSite.Width;

      LeftDockSite.Width := 0;

      LeftSplitter.Width := 0;
   End;
End;

//==============================================================================

Procedure TContentsFrame.BottomDockSiteUnDock(Sender: TObject; Client: TControl;
                                              NewTarget: TWinControl;
                                              Var Allow: Boolean);
Begin
   If (BottomDockSite.VisibleDockClientCount = 1) Then Begin
      Integer(GetDockSiteSize(Sender As TPanel)^) := BottomDockSite.Height;

      BottomDockSite.Height := 0;

      BottomSplitter.Height := 0;
   End;
End;

//==============================================================================

Procedure TContentsFrame.RightDockSiteUnDock(Sender: TObject; Client: TControl;
                                             NewTarget: TWinControl;
                                             Var Allow: Boolean);
Begin
   If (RightDockSite.VisibleDockClientCount = 1) Then Begin
      Integer(GetDockSiteSize(Sender As TPanel)^) := RightDockSite.Width;
      
      RightDockSite.Width := 0;

      RightSplitter.Width := 0;
   End;
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpHelpExecute(Sender: TObject);
Begin
   Application.HelpContext(HELP_WelcomeToCOR);
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpHomePageExecute(Sender: TObject);
Begin
   CORShellExecute(COR_URL);
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpHowToExecute(Sender: TObject);
Begin
   Application.HelpContext(HELP_HowTo);
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpFAQExecute(Sender: TObject);
Begin
   Application.HelpContext(HELP_FAQ);
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpUpdateExecute(Sender: TObject);
Begin
   CORUpdate;
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpUpdateUpdate(Sender: TObject);
Begin
   ActionHelpUpdate.Enabled := IsWinVistaOrAboveOrAdministrator;
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpCellMLHomePageExecute(Sender: TObject);
Begin
   CORShellExecute(CELLML_URL);
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpCellMLRepositoryExecute(Sender: TObject);
Begin
   CORShellExecute('http://models.cellml.org/');
End;

//==============================================================================

Procedure TContentsFrame.ActionHelpAboutExecute(Sender: TObject);
Begin
   With TAboutForm.Create(MainForm) Do Begin
      ShowModal;

      Free;
   End;
End;

//==============================================================================

Procedure TContentsFrame.ActionViewToolBarsExecute(Sender: TObject);
Begin
   // Show/hide a particular toolbar

   If ((Sender As TAction) = ActionViewFileToolBar) Then
      Case MainForm.Mode Of
         mEditorial:
            ShowHideControl(ControlBarFileMenuToolBar);
         mComputational:
            ShowHideControl(ControlBarFileMenuToolBar);
      End
   Else If ((Sender As TAction) = ActionViewRunToolBar) Then
      Case MainForm.Mode Of
         mEditorial:
            ShowHideControl(ControlBarRunMenuToolBar);
         mComputational:
            ShowHideControl(ControlBarRunMenuToolBar);
      End;
End;

//==============================================================================

Procedure TContentsFrame.ActionViewToolBarsUpdate(Sender: TObject);
Begin
   // Update the menu of a particular toolbar

   If ((Sender As TAction) = ActionViewFileToolBar) Then
      ActionViewFileToolBar.Checked := ControlBarFileMenuToolBar.Visible
   Else If ((Sender As TAction) = ActionViewRunToolBar) Then
      ActionViewRunToolBar.Checked := ControlBarRunMenuToolBar.Visible;
End;

//==============================================================================

Procedure TContentsFrame.TopDockWndProc(Var aMsg: TMessage);
Begin
   OldTopDockWndProc(aMsg);

   If (aMsg.Msg = CM_DOCKNOTIFICATION) Then
      With (TCMDockNotification(aMsg)) Do
         If (NotifyRec.ClientMsg = CM_VISIBLECHANGED) Then
            DockSiteVisibilityChanged(TopDockSite, Client As TFrame, Boolean(NotifyRec.MsgWParam));
End;

//==============================================================================

Procedure TContentsFrame.LeftDockWndProc(Var aMsg: TMessage);
Begin
   OldLeftDockWndProc(aMsg);

   If (aMsg.Msg = CM_DOCKNOTIFICATION) Then
      With (TCMDockNotification(aMsg)) Do
         If (NotifyRec.ClientMsg = CM_VISIBLECHANGED) Then
            DockSiteVisibilityChanged(LeftDockSite, Client As TFrame, Boolean(NotifyRec.MsgWParam));
End;

//==============================================================================

Procedure TContentsFrame.BottomDockWndProc(Var aMsg: TMessage);
Begin
   OldBottomDockWndProc(aMsg);

   If (aMsg.Msg = CM_DOCKNOTIFICATION) Then
      With (TCMDockNotification(aMsg)) Do
         If (NotifyRec.ClientMsg = CM_VISIBLECHANGED) Then
            DockSiteVisibilityChanged(BottomDockSite, Client As TFrame, Boolean(NotifyRec.MsgWParam));
End;

//==============================================================================

Procedure TContentsFrame.RightDockWndProc(Var aMsg: TMessage);
Begin
   OldRightDockWndProc(aMsg);

   If (aMsg.Msg = CM_DOCKNOTIFICATION) Then
      With (TCMDockNotification(aMsg)) Do
         If (NotifyRec.ClientMsg = CM_VISIBLECHANGED) Then
            DockSiteVisibilityChanged(RightDockSite, Client As TFrame, Boolean(NotifyRec.MsgWParam));
End;

//==============================================================================

Procedure TContentsFrame.DockSiteVisibilityChanged(aDockSite: TPanel;
                                                   aFrame: TFrame;
                                                   Const aVisible: Boolean);
Begin
   If (aDockSite.VisibleDockClientCount = 0) Then Begin
      // No view docked to the dock site, so hide it...

      If ((aDockSite = TopDockSite) Or (aDockSite = BottomDockSite)) Then Begin
         Integer(GetDockSiteSize(aDockSite)^) := aDockSite.Height;

         aDockSite.Height := 0;
      End Else Begin
         Integer(GetDockSiteSize(aDockSite)^) := aDockSite.Width;

         aDockSite.Width := 0;
      End;

      If (aDockSite = TopDockSite) Then
         TopSplitter.Height := 0
      Else If (aDockSite = LeftDockSite) Then
         LeftSplitter.Width := 0
      Else If (aDockSite = BottomDockSite) Then
         BottomSplitter.Height := 0
      Else
         RightSplitter.Width := 0;
   End Else If (aVisible And (aDockSite.VisibleDockClientCount = 1)) Then Begin
      // A view is docked to an empty dock site, so show it...

      If ((aDockSite = TopDockSite) Or (aDockSite = BottomDockSite)) Then
         aDockSite.Height := Integer(GetDockSiteSize(aDockSite)^)
      Else
         aDockSite.Width := Integer(GetDockSiteSize(aDockSite)^);

      If (aDockSite = TopDockSite) Then
         TopSplitter.Height := SPLITTER_SIZE
      Else If (aDockSite = LeftDockSite) Then
         LeftSplitter.Width := SPLITTER_SIZE
      Else If (aDockSite = BottomDockSite) Then
         BottomSplitter.Height := SPLITTER_SIZE
      Else
         RightSplitter.Width := SPLITTER_SIZE;
   End;
End;

//==============================================================================

Procedure TContentsFrame.ShowHideControl(aControl: TControl);
Begin
   If (aControl.Visible) Then
      aControl.Hide
   Else
      RestoreControl(aControl, True);
End;

//==============================================================================

Procedure TContentsFrame.ActionToolsOptionsExecute(Sender: TObject);
Begin
   OptionsForm;
End;

//==============================================================================

Procedure TContentsFrame.ActionToolsResetAllExecute(Sender: TObject);
Begin
   If (MessageDlg('All your settings and options are irrevocably going to be reset. Do you still want to go ahead?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) Then Begin
      With TRegistryIniFile.Create(SOFTWARE_REGISTRY) Do Begin
         EraseSection(COR_NAME);

         Free;
      End;

      MainForm.ResetAll := True;

      MessageDlg('Your settings and options have been reset, though this will only take effect the next time you start '+COR_NAME+'.', mtInformation, [mbOK], 0);
   End;
End;

//==============================================================================

Procedure TContentsFrame.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
Begin
   If (StatusBarState = sbsProgress) Then
      With ProgressBar Do Begin
         Top    := Rect.Top;
         Left   := Rect.Left;
         Width  := Rect.Right-Rect.Left-StatusBar.Height;
         Height := Rect.Bottom-Rect.Top;
      End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

