//==============================================================================
// Properties frame
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 21/12/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// Note: heavily inspired by Mike Lischke's advanced example of
//       TVirtualStringTree (see http://www.delphi-gems.com/VirtualTreeview/)
//==============================================================================

Unit Properties;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, Classes, Controls, Types, Messages, Dockable, VirtualTrees,
   ExtCtrls, StdCtrls, Graphics, OGLGraphPanel, ComCtrls, ToolWin;

//==============================================================================

Const
   WM_STARTEDITING = WM_USER+1789;

//==============================================================================

Type
   TPropertyType = (ptNone, ptPickStr, ptStrictPosFloat, ptPosFloat,
                    ptRangeInteger, ptStrictPosInteger, ptPosInteger,
                    ptStateVar, ptCst, ptCompVar);
   TPropertyDropDownList = Class
      Items: Array Of String;
      Index: Integer;
   End;
   TPropertyRangeInteger = Class
      Min: Integer;
      Max: Integer;
   End;
   TPropertyParameter = Class
      GraphPanel: TOGLGraphPanel;
      Graphs: Array Of TOGLGPGraph;
      GraphsSize: Integer;
      Value: PDouble;

      // Constructor & Destructor

      Constructor Create(Const aValue: PDouble); 
   End;
   TPropertyData = Packed Record
      PropType: TPropertyType;
      CheckType: TCheckType;
      CheckState: TCheckState;
      Index: Integer;
      Owner: WideString;
      Name: WideString;
      Value: WideString;
      Units: WideString;
      Data: Pointer;
   End;
   PPropertyData = ^TPropertyData;
   TSpecialLabel = Class(TCustomLabel)
      Protected
         // Methods handling different messages

         Procedure WMLButtonDblClk(Var aMessage: TWMLButtonDblClk); Message WM_LBUTTONDBLCLK;
   End;
   TSpecialListBox = Class(TCustomListBox)
      Protected
         // Methods handling different messages

         Procedure CNCommand(Var aMessage: TWMCommand); Message CN_COMMAND;
         Procedure CNKeyDown(Var aMessage: TWMKeyDown); Message CN_KEYDOWN;
   End;
   TSpecialComboBox = Class(TCustomControl)
      Private
         // Methods to modify the different published properties

         Function GetDroppedDown: Boolean; Inline;

         Procedure SetText(Const aValue: String); Inline;
         Function GetText: String; Inline;

         Procedure SetIndex(Const aValue: Integer); Inline;
         Function GetIndex: Integer; Inline;

         // Methods used for internal purposes

         Procedure ShowHideListBox(Const aShow: Boolean = True);

         Procedure ListBoxKeyDown(aSender: TObject; Var aKey: Word; aShift: TShiftState);
         Procedure ListBoxMouseUp(aSender: TObject; aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer);
         Procedure ListBoxMouseMove(aSender: TObject; aShift: TShiftState; aX, aY: Integer);

      Protected
         // Properties used for internal purposes

         Lab: TSpecialLabel;
         BtnRect: TRect;
         ListBox: TSpecialListBox;

         // Inherited methods

         Procedure Paint; Override;
         Procedure MouseDown(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer); Override;

         // Methods handling different messages
         
         Procedure CNKeyDown(Var aMessage: TWMKeyDown); Message CN_KEYDOWN;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;

      Published
         // Published properties

         Property DroppedDown: Boolean Read GetDroppedDown;
         Property Text: String Read GetText Write SetText;
         Property Index: Integer Read GetIndex Write SetIndex;

         // Available events

         Property OnKeyDown;
   End;
   TPropertyEditLink = Class(TInterfacedObject, IVTEditLink)
      Private
         // Properties used for internal purposes

         Owner: TVirtualStringTree;
         Node: PVirtualNode;
         Column: Integer;
         Edit: TWinControl;

         PreventBeep: Boolean;

         // Methods used for internal purposes

         Procedure EditKeyDown(aSender: TObject; Var aKey: Word; aShift: TShiftState);
         Procedure EditKeyPress(aSender: TObject; Var aKey: Char);

      Public
         // Constructor & Destructor

         Destructor Destroy; Override;

         // User's methods

         Function BeginEdit: Boolean; StdCall;
         Function CancelEdit: Boolean; StdCall;
         Function EndEdit: Boolean; StdCall;

         Function GetBounds: TRect; StdCall; Inline;
         Procedure SetBounds(aRect: TRect); StdCall;

         Function PrepareEdit(aOwner: TBaseVirtualTree; aNode: PVirtualNode; aColumn: TColumnIndex): Boolean; StdCall;

         Procedure ProcessMessage(Var aMessage: TMessage); StdCall; Inline;
   End;
   TPropertiesFrame = Class(TDockableFrame)
    RootPanel: TPanel;
    VirtualStringTree: TVirtualStringTree;
    procedure VirtualStringTreeChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTreeCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure VirtualStringTreeEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure VirtualStringTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VirtualStringTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VirtualStringTreePaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VirtualStringTreeBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure VirtualStringTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VirtualStringTreeMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualStringTreeMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure VirtualStringTreeMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualStringTreeEdited(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VirtualStringTreeCollapsedOrExpanded(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTreeResize(Sender: TObject);
      Private
         // Properties used for internal purposes

         CrtEditableNode: PVirtualNode;
         MouseDown: Boolean;

         CrtEditLink: TPropertyEditLink;

         SkipEndEdit: Boolean;

         OldY, MaxY: Integer;

         // Methods used for internal purposes

         Procedure WMStartEditing(Var aMessage: TMessage); Message WM_STARTEDITING;

         Procedure UpdateMaxY(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);

      Public
         ActiveNode: PVirtualNode;

         // User's methods

         Function ValidProperty(Const aDisplayErrorMsg: Boolean = True): Boolean;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   Forms, SysUtils, Math, JvValidateEdit, Common, CORCommon;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TPropertyParameter.Create(Const aValue: PDouble);
Begin
   Value := aValue;

   SetLength(Graphs, 1);

   GraphsSize := 0;
End;

//==============================================================================

Procedure TSpecialLabel.WMLButtonDblClk(Var aMessage: TWMLButtonDblClk);
Begin
   // Note: one would normally have used "TLabel" and handle the "OnDblClick"
   //       event, but to do so with the code below will generate an exception.
   //       The culprit seems to be "EndEditNode", yet it works fine in this
   //       configuration! Weird...

   Inherited;

   // Select the next item in the list

   With Owner As TSpecialComboBox Do
      With ListBox Do Begin
         If (ItemIndex = Items.Count-1) Then
            ItemIndex := 0
         Else
            ItemIndex := ItemIndex+1;

         Lab.Caption := Items[ItemIndex];

         // Having selected the next item, we simulate the end of the editing
         // and start the editing again... so that anything that depends on the
         // value of the new item in the list (e.g. see the integrator in the
         // computation frame) will be taken into account...

         With Parent As TVirtualStringTree Do Begin
            EndEditNode;

            With (Parent As TPanel).Parent As TPropertiesFrame Do
               PostMessage(Handle, WM_STARTEDITING, Integer(CrtEditableNode), 0);
         End;
      End;
End;

//==============================================================================

Procedure TSpecialListBox.CNCommand(Var aMessage: TWMCommand);
Begin
   Inherited;

   Case aMessage.NotifyCode Of
      CBN_SELCHANGE:
         If (ItemIndex <> -1) Then
            (Owner As TSpecialComboBox).Lab.Caption := Items[ItemIndex];
   End;
End;

//==============================================================================

Procedure TSpecialListBox.CNKeyDown(Var aMessage: TWMKeyDown);
Begin
   Inherited;

   Case aMessage.CharCode Of
      VK_SPACE:
         With Parent As TVirtualStringTree Do Begin
            EndEditNode;

            With (Parent As TPanel).Parent As TPropertiesFrame Do
               PostMessage(Handle, WM_STARTEDITING, Integer(CrtEditableNode), 0);
         End;
   Else
      Try
         DoKeyDown(aMessage);

         UpdateUIState(aMessage.CharCode);
      Except
      End;
   End;
End;

//==============================================================================

Constructor TSpecialComboBox.Create(aOwner: TComponent);
Begin
   Inherited;

   Lab := TSpecialLabel.Create(Self);

   With Lab Do Begin
      Parent := aOwner As TWinControl;

      AutoSize := False;

      Color := clHighlight;

      Font.Color := clHighlightText;

      Transparent := False;
   End;

   ListBox := TSpecialListBox.Create(Self);

   With ListBox Do Begin
      Visible := False;

      Parent := aOwner As TWinControl;

      Ctl3D := False;

      OnKeyDown   := ListBoxKeyDown;
      OnMouseUp   := ListBoxMouseUp;
      OnMouseMove := ListBoxMouseMove;
   End;
End;

//==============================================================================

Destructor TSpecialComboBox.Destroy;
Begin
   Lab.Free;
   ListBox.Free;

   Inherited;
End;

//==============================================================================

Procedure TSpecialComboBox.Paint;
Begin
   DrawFrameControl(Canvas.Handle, BtnRect, DFC_SCROLL, DFCS_FLAT Or DFCS_SCROLLCOMBOBOX);
End;

//==============================================================================

Procedure TSpecialComboBox.ShowHideListBox(Const aShow: Boolean);
   Function ListBoxMaxWidth: Integer;
   Var
      I: Integer;
   Begin
      Result := 0;

      For I := 0 To ListBox.Items.Count-1 Do
         Result := Max(Result, 4+ListBox.Canvas.TextWidth(ListBox.Items[I])+4);
         // Note: the two "4"s are because of the list-box needing some space on
         //       both ends of an item. The question is to know how much space
         //       is required (4 pixels on each side apparently) and whether
         //       that space can vary depending on one's display settings (that
         //       is the real question!)... We will assume that the latter
         //       question is in fact not that critical... Argh, if it is!! :)
   End;
Const
   MaxNbOfItems = 8;
Begin
   If (Not ListBox.Visible And aShow) Then Begin
      With Lab Do Begin
         Color := clWindow;

         Font.Color := clWindowText;
      End;

      With ListBox Do Begin
         Top    := Lab.Top+Lab.Height+1;
         Left   := Lab.Left;
         Width  := Max(Lab.Width+1+Self.Width, ListBoxMaxWidth);
         Height := Min(Max(Items.Count, 1), MaxNbOfItems)*ItemHeight+2;

         Show;

         SetFocus;

         Try
            Lab.Caption := ListBox.Items[ListBox.ItemIndex];
            // Note: we should never get an exception, since the listb-box
            //       should contain items and that the item index should be
            //       valid, but who knows? So...
         Except
         End;
      End;
   End Else Begin
      ListBox.Hide;

      With Lab Do Begin
         Color := clHighlight;

         Font.Color := clHighlightText;

         SetFocus;
      End;
   End;
End;

//==============================================================================

Procedure TSpecialComboBox.MouseDown(aButton: TMouseButton; aShift: TShiftState;
                                     aX, aY: Integer);
Begin
   Inherited;

   ShowHideListBox(PtInRect(BtnRect, Point(aX, aY)));
End;

//==============================================================================

Procedure TSpecialComboBox.CNKeyDown(Var aMessage: TWMKeyDown);
Begin
   Inherited;

   Case aMessage.CharCode Of
      VK_ESCAPE:
         (Owner As TVirtualStringTree).CancelEditNode;
      VK_RETURN:
         (Owner As TVirtualStringTree).EndEditNode;
      VK_SPACE:
         ShowHideListBox;
   Else
      Try
         DoKeyDown(aMessage);

         UpdateUIState(aMessage.CharCode);
      Except
      End;
   End;
End;

//==============================================================================

Function TSpecialComboBox.GetDroppedDown: Boolean;
Begin
   Result := ListBox.Visible;
End;

//==============================================================================

Procedure TSpecialComboBox.SetText(Const aValue: String);
Begin
   If (aValue <> Lab.Caption) Then
      Lab.Caption := aValue;
End;

//==============================================================================

Function TSpecialComboBox.GetText: String;
Begin
   If (DroppedDown And (ListBox.ItemIndex <> -1)) Then
      Result := ListBox.Items[ListBox.ItemIndex]
   Else
      Result := Lab.Caption;
End;

//==============================================================================

Procedure TSpecialComboBox.SetIndex(Const aValue: Integer);
Begin
   ListBox.ItemIndex := aValue;
End;

//==============================================================================

Function TSpecialComboBox.GetIndex: Integer;
Begin
   Result := ListBox.ItemIndex;
End;

//==============================================================================

Procedure TSpecialComboBox.ListBoxKeyDown(aSender: TObject; Var aKey: Word;
                                          aShift: TShiftState);
Begin
   Case aKey Of
      VK_ESCAPE: 
         ShowHideListBox(False);
      VK_RETURN:
         // Forward the keypress to the owner to allow for the editing to be
         // ended...

         PostMessage((Owner As TVirtualStringTree).Handle, WM_KEYDOWN, aKey, 0);
   End;
End;

//==============================================================================

Procedure TSpecialComboBox.ListBoxMouseUp(aSender: TObject;
                                          aButton: TMouseButton;
                                          aShift: TShiftState;
                                          aX, aY: Integer);
Begin
   (Owner As TVirtualStringTree).EndEditNode;
End;

//==============================================================================

Procedure TSpecialComboBox.ListBoxMouseMove(aSender: TObject;
                                            aShift: TShiftState;
                                            aX, aY: Integer);
Begin
   If (ListBox.ItemIndex <> -1) Then
      Lab.Caption := ListBox.Items[ListBox.ItemIndex];
End;

//==============================================================================

Destructor TPropertyEditLink.Destroy;
Begin
   (Owner.Owner As TPropertiesFrame).CrtEditLink := Nil;

   Edit.Free;

   Inherited;
End;

//==============================================================================

Procedure TPropertyEditLink.EditKeyDown(aSender: TObject;
                                        Var aKey: Word; aShift: TShiftState);
Begin
   Case aKey Of
      0: Begin
         // Forward the keypress to the owner to allow for the editing to be
         // cancelled...
         // Note: one would normally use "VK_ESCAPE", but
         //       "TJvCustomValidateEdit.KeyDown" resets "aKey" to zero when
         //       pressing the escape key, so...

         PostMessage(Owner.Handle, WM_KEYDOWN, VK_ESCAPE, 0);

         PreventBeep := True;
      End;
      VK_RETURN: Begin
         // Forward the keypress to the owner to allow for the editing to be
         // ended...

         PostMessage(Owner.Handle, WM_KEYDOWN, aKey, 0);

         PreventBeep := True;
      End;
      VK_UP, VK_DOWN:
         If (aShift = []) Then 
            // Forward the keypress to the owner to asynchronously change the
            // focused node...

            PostMessage(Owner.Handle, WM_KEYDOWN, aKey, 0);
   End;
End;

//==============================================================================

Procedure TPropertyEditLink.EditKeyPress(aSender: TObject; Var aKey: Char);
Begin
   If (PreventBeep) Then Begin
      aKey := Chr(0);   // To prevent the beep...
      // Note: we actually deal with the pressing of the key in "EditKeyDown"...

      PreventBeep := False;
   End;
End;

//==============================================================================

Function TPropertyEditLink.BeginEdit: Boolean;
Begin
   Result := True;

   With PPropertyData(Owner.GetNodeData(Node))^ Do 
      If ((PropType = ptStateVar) Or (PropType = ptCst)) Then
         (Edit As TJvValidateEdit).Text := SimplifyNb(TPropertyParameter(Data).Value^);

   With Edit Do Begin
      Show;

      SetFocus;
   End;
End;

//==============================================================================

Function TPropertyEditLink.CancelEdit: Boolean;
Begin
   Result := True;

   Edit.Hide;

   Owner.SetFocus;
End;

//==============================================================================

Function TPropertiesFrame.ValidProperty(Const aDisplayErrorMsg: Boolean): Boolean;
Begin
   Result := True;

   If ((CrtEditLink <> Nil) And (CrtEditLink.Edit Is TJvValidateEdit)) Then
      With PPropertyData(VirtualStringTree.GetNodeData(VirtualStringTree.FocusedNode))^ Do
         Case PropType Of
            ptStrictPosFloat:
               Result := CheckEditFieldForStrictlyPositiveFloatValidity(CrtEditLink.Edit As TJvValidateEdit, aDisplayErrorMsg);
            ptPosFloat:
               Result := CheckEditFieldForPositiveFloatValidity(CrtEditLink.Edit As TJvValidateEdit, aDisplayErrorMsg);
            ptRangeInteger:
               Result := CheckEditFieldForRangeIntegerValidity(CrtEditLink.Edit As TJvValidateEdit, TPropertyRangeInteger(Data).Min, TPropertyRangeInteger(Data).Max, aDisplayErrorMsg);
            ptStrictPosInteger:
               Result := CheckEditFieldForStrictlyPositiveIntegerValidity(CrtEditLink.Edit As TJvValidateEdit, aDisplayErrorMsg);
            ptPosInteger:
               Result := CheckEditFieldForPositiveIntegerValidity(CrtEditLink.Edit As TJvValidateEdit, aDisplayErrorMsg);
         Else
            Result := CheckEditFieldForFloatValidity(CrtEditLink.Edit As TJvValidateEdit, aDisplayErrorMsg);
         End;
End;

//==============================================================================

Function TPropertyEditLink.EndEdit: Boolean;
Var
   Data: PPropertyData;
   Val: String;
   PropertiesFrame: TPropertiesFrame;
Begin
   PropertiesFrame := Owner.Owner As TPropertiesFrame;

   If (PropertiesFrame.SkipEndEdit) Then Begin
      Result := False;

      PropertiesFrame.SkipEndEdit := False;
   End Else Begin
      Result := True;

      Data := Owner.GetNodeData(Node);

      If (Edit Is TSpecialComboBox) Then
         With Edit As TSpecialComboBox Do Begin
            Val := Text;

            TPropertyDropDownList(Data^.Data).Index := Index;
         End
      Else If (Edit Is TJvValidateEdit) Then Begin
         Result := PropertiesFrame.ValidProperty;

         If (Result) Then
            Val := SimplifyNbStr((Edit As TJvValidateEdit).Text);
      End Else
         // Should never reach that point, but who knows?!

         Val := Data^.Value;

      If (Result) Then Begin
         If (CompareStr(Val, Data^.Value) <> 0) Then Begin
            Data^.Value := Val;

            Owner.InvalidateNode(Node);
         End;

         Edit.Hide;

         Owner.SetFocus;
      End Else
         Edit.SetFocus;
   End;
End;

//==============================================================================

Function TPropertyEditLink.GetBounds: TRect;
Begin
   Result := Edit.BoundsRect;
End;

//==============================================================================

Procedure TPropertyEditLink.SetBounds(aRect: TRect);
Var
   CellTop, CellBottom: Integer;
   OldFont: TFont;
   TxtHeight: Integer;
Begin
   Owner.Header.Columns.GetColumnBounds(Column, aRect.Left, aRect.Right);

   CellTop    := aRect.Top;
   CellBottom := aRect.Bottom;

   With Owner.Canvas Do Begin
      OldFont := Owner.Font;

      Font := Owner.Header.Font;

      TxtHeight := TextHeight('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890.');

      Font := OldFont;
   End;

   Inc(aRect.Top, (CellBottom-CellTop-TxtHeight) Div 2);
   Inc(aRect.Left, Owner.Margin+Owner.TextMargin);
   Dec(aRect.Bottom, CellBottom-CellTop-(CellBottom-CellTop-TxtHeight) Div 2-TxtHeight);
   Dec(aRect.Right, Owner.TextMargin+1);

   Edit.BoundsRect := aRect;

   If (Edit Is TSpecialComboBox) Then
      With Edit As TSpecialComboBox Do Begin
         With Lab Do Begin
            Top    := aRect.Top;
            Left   := aRect.Left;
            Height := aRect.Bottom-aRect.Top;
            Width  := aRect.Right-aRect.Left-Min(GetSystemMetrics(SM_CXHTHUMB), Height)-1;
         End;

         BtnRect := Rect(0, 0, aRect.Right-Lab.Left-Lab.Width-1, Height);

         Edit.Left  := Edit.Left+Lab.Width+1;
         Edit.Width := Edit.Width-Lab.Width-1;
      End;
End;

//==============================================================================

Function TPropertyEditLink.PrepareEdit(aOwner: TBaseVirtualTree;
                                       aNode: PVirtualNode;
                                       aColumn: TColumnIndex): Boolean;
Var
   Data: PPropertyData;
   I: Integer;
   JvValidateEdit: TJvValidateEdit;
   PropertyDropDownList: TPropertyDropDownList;
Begin
   Result := True;

   Owner  := aOwner As TVirtualStringTree;
   Node   := aNode;
   Column := aColumn;

   // Determine the type of edit that is actually needed

   FreeAndNil(Edit);

   Data := Owner.GetNodeData(aNode);

   Case Data^.PropType Of
      ptPickStr: Begin
         Edit := TSpecialComboBox.Create(aOwner);

         With Edit As TSpecialComboBox Do Begin
            Visible := False;

            Parent := aOwner;

            DoubleBuffered := True;

            PropertyDropDownList := TPropertyDropDownList(Data^.Data); 

            For I := 0 To High(PropertyDropDownList.Items) Do
               ListBox.Items.Add(PropertyDropDownList.Items[I]);

            Text  := Data^.Value;
            Index := PropertyDropDownList.Index;

            OnKeyDown  := EditKeyDown;
            OnKeyPress := EditKeyPress;
         End;
      End;
      ptStrictPosFloat, ptPosFloat,
      ptRangeInteger, ptStrictPosInteger, ptPosInteger,
      ptStateVar, ptCst: Begin
         JvValidateEdit := TJvValidateEdit.Create(aOwner);

         Edit := JvValidateEdit;

         With JvValidateEdit Do Begin
            Visible := False;

            Parent := aOwner;

            DoubleBuffered := True;

            Case Data^.PropType Of
               ptStrictPosFloat:
                  EditFieldForStrictlyPositiveFloat(JvValidateEdit);
               ptPosFloat:
                  EditFieldForPositiveFloat(JvValidateEdit);
               ptRangeInteger:
                  EditFieldForRangeInteger(JvValidateEdit);
               ptStrictPosInteger:
                  EditFieldForStrictlyPositiveInteger(JvValidateEdit);
               ptPosInteger:
                  EditFieldForPositiveInteger(JvValidateEdit);
            Else
               EditFieldForFloat(JvValidateEdit);
            End;

            Alignment := taLeftJustify;

            BorderStyle := bsNone;

            Text := Data.Value;

            OnKeyDown  := EditKeyDown;
            OnKeyPress := EditKeyPress;
         End;
      End;
   Else
      Result := False;
   End;

   If (Result) Then
      If (Not (Edit Is TSpecialComboBox)) Then
         Edit.Height := Owner.DefaultNodeHeight;
End;

//==============================================================================

Procedure TPropertyEditLink.ProcessMessage(Var aMessage: TMessage);
Begin
   Edit.WindowProc(aMessage);
End;

//==============================================================================

Procedure TPropertiesFrame.WMStartEditing(Var aMessage: TMessage);
Begin
   If (Pointer(aMessage.WParam) <> Nil) Then
      VirtualStringTree.EditNode(Pointer(aMessage.WParam), 1);
End;

//==============================================================================

Procedure TPropertiesFrame.UpdateMaxY(aSender: TBaseVirtualTree;
                                      aNode: PVirtualNode; aData: Pointer;
                                      Var aAbort: Boolean);
Begin
   If (VirtualStringTree.FullyVisible[aNode]) Then
      Inc(MaxY, VirtualStringTree.NodeHeight[aNode]);
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
Begin
   With Sender Do
      // Start editing as soon as a node gets the focus

      If (Assigned(Node) And (Node.Parent <> RootNode) And
          Not (tsIncrementalSearching In TreeStates)) Then Begin
         // It may happen that we come here because of a node being currently
         // edited, which can be a problem in case the last change event has
         // not been completely handled. To circumvent this problem, we post
         // to ourselves a special message to ask to start editing the new
         // node. This works, because the posted message is the first one to
         // be executed *after* this event and the message, which triggered
         // it, is terminated...

         PostMessage(Self.Handle, WM_STARTEDITING, Integer(Node), 0);

         CrtEditableNode := Node;
      End Else
         CrtEditableNode := Nil;
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeCollapsedOrExpanded(Sender: TBaseVirtualTree;
                                                                Node: PVirtualNode);
Begin
   // Just collapsed or expanded the virtual string tree, so need to recompute
   // MaxY

   VirtualStringTreeResize(Sender);
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeCreateEditor(Sender: TBaseVirtualTree;
                                                         Node: PVirtualNode; Column: TColumnIndex;
                                                         Out EditLink: IVTEditLink);
Begin
   CrtEditLink := TPropertyEditLink.Create;   // Useful for "ValidProperty"

   EditLink := CrtEditLink;
   // Note: "EditLink" is destroyed automatically once editing is finished
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeEditing(Sender: TBaseVirtualTree;
                                                    Node: PVirtualNode;
                                                    Column: TColumnIndex;
                                                    Var Allowed: Boolean);
Var
   Data: PPropertyData;
Begin
   With Sender Do Begin
      Data := GetNodeData(Node);

      Allowed := (Node.Parent <> RootNode) And (Column = 1) And (Data.PropType <> ptNone);
   End;
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeGetNodeDataSize(Sender: TBaseVirtualTree;
                                                            Var NodeDataSize: Integer);
Begin
   NodeDataSize := SizeOf(TPropertyData);
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeGetText(Sender: TBaseVirtualTree;
                                                    Node: PVirtualNode;
                                                    Column: TColumnIndex;
                                                    TextType: TVSTTextType;
                                                    Var CellText: WideString);
Begin
   If (TextType = ttNormal) Then
      Case Column Of
         0:   // Name
            CellText := PPropertyData(Sender.GetNodeData(Node))^.Name;
         1:   // Value
            CellText := PPropertyData(Sender.GetNodeData(Node))^.Value;
         2:   // Unit
            CellText := PPropertyData(Sender.GetNodeData(Node))^.Units;
      End;
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreePaintText(Sender: TBaseVirtualTree;
                                                      Const TargetCanvas: TCanvas;
                                                      Node: PVirtualNode;
                                                      Column: TColumnIndex;
                                                      TextType: TVSTTextType);
Begin
   // Draw root nodes and components in bold style

   If (PPropertyData(Sender.GetNodeData(Node))^.PropType = ptNone) Then
      TargetCanvas.Font.Style := [fsBold]
   Else
      TargetCanvas.Font.Style := [];
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeResize(Sender: TObject);
Begin
   // Just resized, collapsed or expanded the virtual string tree, so need to
   // recompute MaxY

   MaxY := 0;

   With VirtualStringTree Do Begin
      BeginUpdate;
         IterateSubtree(Nil, UpdateMaxY, Nil);
      EndUpdate;
   End;

   MaxY := Min(MaxY, VirtualStringTree.Height-Integer(VirtualStringTree.Header.Height));
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeBeforeCellPaint(Sender: TBaseVirtualTree;
                                                            TargetCanvas: TCanvas;
                                                            Node: PVirtualNode;
                                                            Column: TColumnIndex;
                                                            CellPaintMode: TVTCellPaintMode;
                                                            CellRect: TRect;
                                                            Var ContentRect: TRect);
Begin
   // Paint the right background for the cell

   With TargetCanvas Do Begin
      Brush.Color := (Sender As TVirtualStringTree).Header.Columns[Column].Color;

      FillRect(CellRect);
   End;
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeKeyDown(Sender: TObject;
                                                    Var Key: Word;
                                                    Shift: TShiftState);
Begin
   Case Key Of
      VK_ESCAPE:
         // Cancel the editiing (it will do nothing, if nothing is being
         // currently edited)

         If (VirtualStringTree.EditLink <> Nil) Then
            VirtualStringTree.CancelEditNode;
      VK_RETURN:
         If (VirtualStringTree.EditLink <> Nil) Then
            VirtualStringTree.EndEditNode
         Else If (CrtEditableNode <> Nil) Then
            // Re-edit the current node

            PostMessage(Self.Handle, WM_STARTEDITING, Integer(CrtEditableNode), 0);
      VK_UP, VK_DOWN:
         If (Not ValidProperty(False)) Then Begin
            // Trying to change fields, but the current one is not valid, so...
            // cancel the change...

            Beep;

            ValidProperty;   // Just to get the error message, since we want the
                             // beep first...

            Key := 0;
         End;
   End;
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeMouseDown(Sender: TObject;
                                                      Button: TMouseButton;
                                                      Shift: TShiftState;
                                                      X, Y: Integer);
Var
   HitInfo: THitInfo;
Begin
   VirtualStringTree.GetHitTestInfoAt(X, Y, True, HitInfo);

   If (Not (hiOnItemButton In HitInfo.HitPositions) And
       Not (hiOnItemCheckBox In HitInfo.HitPositions)) Then Begin
      If ((Button = mbLeft) And Not (ssDouble In Shift) And
          (VirtualStringTree.GetNodeAt(X, Y) = CrtEditableNode) And (VirtualStringTree.EditLink = Nil)) Then
         // Re-edit the current node

         PostMessage(Self.Handle, WM_STARTEDITING, Integer(CrtEditableNode), 0)
      Else If (Not ValidProperty(False)) Then Begin
         If (VirtualStringTree.GetNodeAt(X, Y) = CrtEditableNode) Then
            SkipEndEdit := True
            // Note: very useful when badly editing a node and clicking on it.
            //       By default it would exit the editing and re-activate it.
            //       While exiting it would check the validity of the property,
            //       which we don't want to do. The same holds for exiting
            //       itself, so...
         Else
            Beep;   // The error message comes later...
      End;

      MouseDown := True;

      OldY := Y;
   End Else If (hiOnItemCheckBox In HitInfo.HitPositions) Then
      // We are clicking on a check box, so update the active node

      With VirtualStringTree.ScreenToClient(Mouse.CursorPos) Do
         ActiveNode := VirtualStringTree.GetNodeAt(X, Y);
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeMouseMove(Sender: TObject;
                                                      Shift: TShiftState;
                                                      X, Y: Integer);
Var
   Default: Boolean;
   CrtNode: PVirtualNode;
Begin
   If (MouseDown) Then Begin
      Default := False;

      CrtNode := VirtualStringTree.GetNodeAt(X, Y);

      If ((CrtNode <> VirtualStringTree.FocusedNode) Or (Y < 0)) Then Begin
         If ((Y < Integer(VirtualStringTree.Header.Height)) And (Y < OldY) And
             (VirtualStringTree.FocusedNode <> Nil)) Then Begin
            If (ValidProperty(False) And (CrtEditableNode <> Nil) And (VirtualStringTree.EditLink <> Nil)) Then
               // A node is currently being edited, so cancel it...

               VirtualStringTree.CancelEditNode;

            // Simulate pressing the up key

            keybd_event(VK_UP, 1, 0, 0);
            keybd_event(VK_UP, 1, KEYEVENTF_KEYUP, 0);
         End Else If ((Y >= MaxY) And (Y > OldY)) Then Begin
            If (ValidProperty(False) And (CrtEditableNode <> Nil) And (VirtualStringTree.EditLink <> Nil)) Then
               // A node is currently being edited, so cancel it...

               VirtualStringTree.CancelEditNode;

            // Simulate pressing the down key

            keybd_event(VK_DOWN, 1, 0, 0);
            keybd_event(VK_DOWN, 1, KEYEVENTF_KEYUP, 0);
         End Else If (Y < MaxY) Then
            Default := True;

         OldY := Y;
      End Else
         Default := True;

      If (Default And (CrtNode <> Nil) And (CrtNode <> CrtEditableNode) And
          ValidProperty(False)) Then Begin
         If ((CrtEditableNode <> Nil) And (VirtualStringTree.EditLink <> Nil)) Then
            // A node is currently being edited, so cancel it...

            VirtualStringTree.CancelEditNode;

         // Select and give the focus to the node

         VirtualStringTree.Selected[CrtNode] := True;

         VirtualStringTree.FocusedNode := CrtNode;

         If (PPropertyData(VirtualStringTree.GetNodeData(CrtNode)).PropType = ptNone) Then
            // The node over which we are doesn't contain any editable
            // information, so just reset CrtEditableNode and nothing more...

            CrtEditableNode := Nil
         Else Begin
            // The node over which we are contains editable information,
            // so... edit it!

            CrtEditableNode := CrtNode;

            PostMessage(Self.Handle, WM_STARTEDITING, Integer(CrtEditableNode), 0);
         End;

         OldY := Y;
      End;
   End;
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeMouseUp(Sender: TObject;
                                                    Button: TMouseButton;
                                                    Shift: TShiftState;
                                                    X, Y: Integer);
Begin
   MouseDown := False;
End;

//==============================================================================

Procedure TPropertiesFrame.VirtualStringTreeEdited(Sender: TBaseVirtualTree;
                                                   Node: PVirtualNode;
                                                   Column: TColumnIndex);
Var
   NewValue: Double;
Begin
   With PPropertyData(Sender.GetNodeData(Node))^ Do
      If ((PropType = ptStateVar) Or (PropType = ptCst)) Then Begin
         NewValue := StrToFloat(PPropertyData(Sender.GetNodeData(Node))^.Value);

         If (TPropertyParameter(Data).Value^ <> NewValue) Then 
            // The edited node is a state variable or constant, so update its
            // real value...

            TPropertyParameter(Data).Value^ := NewValue;
      End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

