//==============================================================================
// Virtual String Tree list box component
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 04/07/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// Note: uses the Virtual String Tree component to create a much more efficient
//       list box component
//==============================================================================

Unit VSTListBox;

//==============================================================================

Interface

//==============================================================================

Uses
   Classes, Controls, VirtualTrees, DeCAL;

//==============================================================================

Type
   TVSTListBox = Class(TVirtualStringTree)
      Private
         // Properties used for internal purposes

         Data: DArray;

         MouseDown: Boolean;

         OldY, MaxY: Integer;

         // Methods to modify the different published properties

         Procedure SetNodeIndex(Const aValue: Integer);

         // Methods used for internal purposes

         Procedure VSTListBoxGetText(aSender: TBaseVirtualTree; aNode: PVirtualNode; aColumn: TColumnIndex; aTextType: TVSTTextType; Var aCellText: WideString);

         Procedure VSTListBoxMouseDown(aSender: TObject; aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer);
         Procedure VSTListBoxMouseMove(aSender: TObject; aShift: TShiftState; aX, aY: Integer);
         Procedure VSTListBoxMouseUp(aSender: TObject; aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer);

         Procedure VSTListBoxResize(aSender: TObject);

         Procedure UpdateMaxY(aSender: TBaseVirtualTree; aNode: PVirtualNode; aData: Pointer; Var aAbort: Boolean);

      Protected
         // Private representation of published properties

         FNodeIndex: Integer;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;

         // User's methods

         Procedure Add(Const aItem: String);
         Procedure Delete(Const aItemIndex: Integer);

         Function Get(Const aItemIndex: Integer): String;
         Procedure Replace(Const aItemIndex: Integer; Const aString: String);

         Procedure Clear; Reintroduce;

         Function Count: Integer;

         Function Strings: String;

      Published
         // Published properties

         Property NodeIndex: Integer Read FNodeIndex Write SetNodeIndex;
   End;

//==============================================================================

Procedure Register;

//==============================================================================

Implementation

//==============================================================================

Uses
   Windows, Math, Common;

//==============================================================================

Constructor TVSTListBox.Create(aOwner: TComponent);
Begin
   Inherited;

   FNodeIndex := -1;

   Data := DArray.Create;

   DefaultNodeHeight := 13;

   Margin := 0;

   ScrollBarOptions.HorizontalIncrement := DefaultNodeHeight;
   ScrollBarOptions.VerticalIncrement   := DefaultNodeHeight;

   TextMargin := 2;

   TreeOptions.PaintOptions     := [toHideFocusRect,toShowButtons,toShowDropmark,toThemeAware,toUseBlendedImages];
   TreeOptions.SelectionOptions := [toExtendedFocus,toFullRowSelect];

   OnGetText := VSTListBoxGetText;

   OnMouseDown := VSTListBoxMouseDown;
   OnMouseMove := VSTListBoxMouseMove;
   OnMouseUp   := VSTListBoxMouseUp;

   OnResize := VSTListBoxResize;
End;

//==============================================================================

Destructor TVSTListBox.Destroy;
Begin
   Inherited;

   Data.Free;
   // Note: we must NOT free the objects held by "Data", for they are strings!
End;

//==============================================================================

Procedure TVSTListBox.SetNodeIndex(Const aValue: Integer);
Var
   CrtNode: PVirtualNode;
Begin
   If ((aValue <> FNodeIndex) And
       (aValue >= -1) And (aValue < Count)) Then Begin
      FNodeIndex := aValue;

      If (aValue = -1) Then
         FocusedNode := Nil
      Else Begin
         CrtNode := RootNode.FirstChild;

         While Integer(CrtNode.Index) <> aValue Do
            CrtNode := CrtNode.NextSibling;

         Selected[CrtNode] := True;

         FocusedNode := CrtNode;

//---GRY--- IS THE CALL BELOW REALLY NECESSARY? IF SO, BE CAREFUL ABOUT
//          "TOptionsForm.PageCtrlEditorPageCtrlColElementValChange", SINCE
//          SELECTING A "TOP" ITEM WILL AUTOMATICALLY SCROLL THE LIST (!!)
//         ScrollIntoView(RootNode.LastChild, False);
         // Note: a simple trick to ensure that the the next call to
         //       "ScrollIntoView" shows the node at the "top" of the list,
         //       rather than at the bottom of it 

         ScrollIntoView(CrtNode, False);
      End;
   End;
End;

//==============================================================================

Procedure TVSTListBox.VSTListBoxGetText(aSender: TBaseVirtualTree;
                                        aNode: PVirtualNode;
                                        aColumn: TColumnIndex;
                                        aTextType: TVSTTextType;
                                        Var aCellText: WideString);
Begin
   Inherited;

   aCellText := String(Data.At(aNode^.Index).VString);
End;

//==============================================================================

Procedure TVSTListBox.VSTListBoxMouseDown(aSender: TObject;
                                          aButton: TMouseButton;
                                          aShift: TShiftState; aX, aY: Integer);
Var
   HitInfo: THitInfo;
Begin
   Inherited;

   GetHitTestInfoAt(aX, aY, True, HitInfo);

   MouseDown := True;

   OldY := aY;
End;

//==============================================================================

Procedure TVSTListBox.VSTListBoxMouseMove(aSender: TObject; aShift: TShiftState;
                                          aX, aY: Integer);
Var
   Default: Boolean;
   CrtNode: PVirtualNode;
Begin
   Inherited;

   If (MouseDown) Then Begin
      Default := False;

      CrtNode := GetNodeAt(aX, aY);

      If ((CrtNode <> FocusedNode) Or (aY < 0)) Then Begin
         If ((aY < 0) And (aY < OldY) And (FocusedNode <> Nil)) Then Begin
            // Simulate pressing the up key

            keybd_event(VK_UP, 1, 0, 0);
            keybd_event(VK_UP, 1, KEYEVENTF_KEYUP, 0);

            OldY := aY;
         End Else If ((aY >= MaxY) And (aY > OldY)) Then Begin
            // Simulate pressing the down key

            keybd_event(VK_DOWN, 1, 0, 0);
            keybd_event(VK_DOWN, 1, KEYEVENTF_KEYUP, 0);

            OldY := aY;
         End Else If (aY < MaxY) Then
            Default := True;
      End;

      If (Default And (CrtNode <> Nil)) Then Begin
         // Select and give the focus to the node

         Selected[CrtNode] := True;

         FocusedNode := CrtNode;

         OldY := aY;
      End;

      // Update the node index, independent of the outcome
      // Note: we must directly assign the node index to "FNodeIndex" and
      //       therefore avoid "SetNodeIndex", since we have already done what
      //       that method does...

      If (FocusedNode <> Nil) Then Begin
         FNodeIndex := FocusedNode.Index;

         If (Assigned(OnChange)) Then
            OnChange(Self, FocusedNode);
      End;
   End;
End;

//==============================================================================

Procedure TVSTListBox.VSTListBoxMouseUp(aSender: TObject; aButton: TMouseButton;
                                        aShift: TShiftState; aX, aY: Integer);
Begin
   Inherited;

   MouseDown := False;

   // Update the node index, independent of the outcome
   // Note: we must directly assign the node index to "FNodeIndex" and therefore
   //       avoid "SetNodeIndex", since we have already done what that method
   //       does, so...

   If (FocusedNode <> Nil) Then Begin
      FNodeIndex := FocusedNode.Index;

      If (Assigned(OnChange)) Then
         OnChange(Self, FocusedNode);
   End;
End;

//==============================================================================

Procedure TVSTListBox.VSTListBoxResize(aSender: TObject);
Begin
   Inherited;

   // Just resized, collapsed or expanded the virtual string tree, so need to
   // recompute MaxY

   MaxY := 0;

   BeginUpdate;
      IterateSubtree(Nil, UpdateMaxY, Nil);
   EndUpdate;

   MaxY := Min(MaxY, Height-Integer(Header.Height));
End;

//==============================================================================

Procedure TVSTListBox.UpdateMaxY(aSender: TBaseVirtualTree;
                                 aNode: PVirtualNode; aData: Pointer;
                                 Var aAbort: Boolean);
Begin
   If (FullyVisible[aNode]) Then
      Inc(MaxY, NodeHeight[aNode]);
End;

//==============================================================================

Procedure TVSTListBox.Add(Const aItem: String);
Begin
   Data.Add([aItem]);

   RootNodeCount := Data.Size;
End;

//==============================================================================

Procedure TVSTListBox.Delete(Const aItemIndex: Integer);
Begin
   If (FNodeIndex = Count-1) Then
      SetNodeIndex(Count-2);
      // Note: this is ok, since in the "worst" of cases we will end up with
      //       "-1", i.e. no focused node, which is exactly what we want!

   Data.RemoveAt(aItemIndex);

   RootNodeCount := Data.Size;
End;

//==============================================================================

Function TVSTListBox.Get(Const aItemIndex: Integer): String;
Begin
   If ((aItemIndex >= 0) And (aItemIndex < Count)) Then
      Result := String(Data.At(aItemIndex).VString)
   Else
      Result := '';
End;

//==============================================================================

Procedure TVSTListBox.Replace(Const aItemIndex: Integer; Const aString: String);
Begin
   If ((aItemIndex >= 0) And (aItemIndex < Count)) Then
      Data.ReplaceWithin(aItemIndex, aItemIndex, [String(Data.At(aItemIndex).VString)], [aString]);
End;

//==============================================================================

Procedure TVSTListBox.Clear;
Begin
   Data.Clear;

   RootNodeCount := Data.Size;
End;

//==============================================================================

Function TVSTListBox.Count: Integer;
Begin
   Result := Data.Size;
End;

//==============================================================================

Function TVSTListBox.Strings: String;
Var
   Iter, StrLen, Size: Integer;
   Ptr: PChar;
   Str: String;
Begin
   Size := 0;

   For Iter := 0 To Count-1 Do
      Inc(Size, Length(String(Data.At(Iter).VString))+Length(CRLF));

   System.SetString(Result, Nil, Size);

   Ptr := Pointer(Result);

   For Iter := 0 To Count-1 Do Begin
      Str := String(Data.At(Iter).VString);

      StrLen := Length(Str);

      If (StrLen <> 0) Then Begin
         Move(Pointer(Str)^, Ptr^, StrLen);

         Inc(Ptr, StrLen);
      End;

      StrLen := Length(CRLF);

      Move(Pointer(CRLF)^, Ptr^, StrLen);

      Inc(Ptr, StrLen);
   End;
End;

//==============================================================================

Procedure Register;
Begin
   RegisterComponents('COR', [TVSTListBox]);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

