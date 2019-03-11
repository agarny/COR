//==============================================================================
// Syntax editor component
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 19/04/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit SyntaxEdit;

//==============================================================================

Interface

//==============================================================================

Uses
{$IFDEF COR_SPECIFIC}
   CellMLAPI, CORCommon, VirtualTrees, CellMLAPIToMCEngine,
{$ENDIF}
   Classes, Graphics, SynEdit, SynEditKeyCmds, SynEditTypes;

//==============================================================================

Type
   TSyntaxEditBFColors = Packed Record
      BackgroundColor: TColor;
      ForegroundColor: TColor;
   End;
   TSyntaxEdit = Class(TSynEdit)
      Private
         // Properties used for internal purposes

         HighlightedLineNb: Integer;
         HighlightedLine: TSyntaxEditBFColors;

{$IFDEF COR_SPECIFIC}
         OtherView: String;

         OtherTopLine: Integer;
         OtherCaretXY: TBufferCoord;

         Saving: Boolean;
{$ENDIF}

         // Methods to modify the different published properties

{$IFDEF COR_SPECIFIC}
         Procedure SetMD5(Const aValue: String); Inline;
{$ENDIF}
         Procedure SetFileName(Const aValue: String); Inline;
         Procedure SetNewFile(Const aValue: Boolean); Inline;
         Procedure SetCanSave(Const aValue: Boolean); Inline;
         Procedure SetSearchText(Const aValue: String); Inline;
         Procedure SetReplaceText(Const aValue: String); Inline;
         Procedure SetSearchReplaceOptions(Const aValue: TSynSearchOptions); Inline;
{$IFDEF COR_SPECIFIC}
         Procedure SetView(Const aValue: TEditorView); Inline;
{$ENDIF}

         // Methods used for internal purposes

         Procedure CommandHandler(aSender: TObject; aAfterProcessing: Boolean; Var aHandled: Boolean; Var aCommand: TSynEditorCommand; Var aChar: Char; aData, aHandlerData: Pointer);

         Procedure SyntaxEditSpecialLineColors(aSender: TObject; aLine: Integer; Var aSpecial: Boolean; Var aFG, aBG: TColor); Inline;

         Function IsNormSep(Const aChar: Char): Boolean; Inline;
         Function IsSpecSep(Const aChar: Char): Boolean; Inline;
         Function IsAnySep(Const aChar: Char): Boolean; Inline;

      Protected
         // Private representation of published properties

         FFileName: String;      // Name of the file which is displayed
{$IFDEF COR_SPECIFIC}
         FMD5: String;           // File's MD5 value
{$ENDIF}
         FNewFile: Boolean;      // Whether this is a new file or not
         FCanSave: Boolean;      // Whether we can to save the file or not
                                 // (when asked to close it)
         FSearchText: String;    // Text to be searched
         FReplaceText: String;   // Text to be searched
         FSearchReplaceOptions: TSynSearchOptions;   // Options for searching or
                                                     // replacing
{$IFDEF COR_SPECIFIC}
         FView: TEditorView;   // View in which the CellML file is to be
                               // displayed
         FExtra: TCellMLModelExtra;
{$ENDIF}

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent; Const aFileName: String = ''; Const aNewFile: Boolean = False); Reintroduce;
         Destructor Destroy; Override;

         // User's methods

{$IFDEF COR_SPECIFIC}
         Procedure SetOptions(Const aValue: TEditorOptions);

         Procedure GoToPos(Const aLineNb: Integer; Const aColNb: Integer = -1);

         Procedure UnhighlightMsg(Const aUpdateOptions: Boolean = True);
         Procedure HighlightMsg(Const aLineNb: Integer; Const aColNb: Integer);
{$ENDIF}

         Function CrtCmd: String;
{$IFDEF COR_SPECIFIC}
         Procedure SearchReplace(Const aForward: Boolean = True); Reintroduce;

         Procedure Duplicate(Const aSyntaxEdit: TSyntaxEdit);

         Procedure NewCellMLFile;

         Procedure LoadFileAsRaw(Const aFileName: String);
         Function LoadFile(Const aFileName: String; Const aSelFirstMsg: Boolean = True): Boolean;

         Function SaveFile(Const aSaveAs: Boolean = False; Const aVirtualStringTree: TVirtualStringTree = Nil; Const aCellMLModelRuntime: TCellMLModelRuntime = Nil; Const aSelFirstMsg: Boolean = True): Boolean;

         Function CloseFileQuery: Integer;
         Function CloseFile(Const aForce: Boolean = False): Boolean;

         Function IsValid(Const aAcknowledgment: Boolean = True; Const aUseTempFile: Boolean = False): Boolean;

         Function Reformat: Boolean;

         Function RunFile: Boolean;

         Procedure ExportCellMLFile(Const aTargetLanguage: TCellMLExportFormat);
{$ENDIF}

      Published
         // Published properties

{$IFDEF COR_SPECIFIC}
         Property MD5: String Read FMD5 Write SetMD5 NoDefault;
{$ENDIF}
         Property FileName: String Read FFileName Write SetFileName NoDefault;
         Property NewFile: Boolean Read FNewFile Write SetNewFile NoDefault;
         Property CanSave: Boolean Read FCanSave Write SetCanSave Default False;
         Property SearchText: String Read FSearchText Write SetSearchText NoDefault;
         Property ReplaceText: String Read FReplaceText Write SetReplaceText NoDefault;
         Property SearchReplaceOptions: TSynSearchOptions Read FSearchReplaceOptions Write SetSearchReplaceOptions Default [];
{$IFDEF COR_SPECIFIC}
         Property View: TEditorView Read FView Write SetView Default evCOR;
         Property Extra: TCellMLModelExtra Read FExtra;
{$ENDIF}
   End;

//==============================================================================

Procedure Register;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF COR_SPECIFIC}
   Forms, ComCtrls, Consts, Main, Contents, Msg, CellMLAPIToCFileEngine,
   CellMLAPIToCPPFileEngine, CellMLAPIToDelphiForWin32FileEngine,
   CellMLAPIToFortran77FileEngine, CellMLAPIToJavaFileEngine,
   CellMLAPIToMATLABFileEngine, CellMLAPIToMSWordFileEngine,
   CellMLAPIToPascalFileEngine, CellMLAPIToTeXFileEngine, SynEditSearch,
{$ENDIF}
   Windows, Messages, SysUtils, StrUtils, Controls, Dialogs, SynEditKeyConst,
   Common;

//==============================================================================

Const
   ecNextPage = ecUserFirst;
   ecPrevPage = ecUserFirst+1;

//==============================================================================

{$IFDEF COR_SPECIFIC}
Var
   SyntaxEditSearch: TSynEditSearch;
{$ENDIF}

//==============================================================================

Constructor TSyntaxEdit.Create(aOwner: TComponent; Const aFileName: String;
                               Const aNewFile: Boolean);
Begin
   Inherited Create(aOwner);

{$IFDEF COR_SPECIFIC}
   Width  := 0;    // This prevents the control from having a default size and
   Height := 0;    // look somewhat odd, while in the end we want it to fill the
                   // client area of its owner, so...

   Parent := TWinControl(aOwner);

   Align := alClient;
{$ENDIF}

   DoubleBuffered := True;     

   HighlightedLineNb := -1;

   Gutter.Width := 17;
   WantTabs := True;

   Options := Options+[eoRightMouseMovesCursor, eoScrollHintFollows, eoTabIndent];

   AddKey(ecNextPage, SYNEDIT_TAB, [ssCtrl]);
   AddKey(ecPrevPage, SYNEDIT_TAB, [ssCtrl, ssShift]);

   RegisterCommandHandler(CommandHandler, Nil);

{$IFDEF COR_SPECIFIC}
   SearchEngine := SyntaxEditSearch;
{$ENDIF}

   TabWidth := 3;

   FFileName := aFileName;
   FNewFile  := aNewFile;

   If (Not aNewFile) Then Begin
{$IFDEF COR_SPECIFIC}
      FMD5     := FileMD5(aFileName);
{$ENDIF}
      ReadOnly := FileIsReadOnly(aFileName);
      // Note: for some reason, "ReadOnly" doesn't always seem to be properly
      //       initialised, so we do it ourselves...
   End Else Begin
{$IFDEF COR_SPECIFIC}
      FMD5     := '';
{$ENDIF}
      ReadOnly := False;
   End;

   FCanSave              := False;
   FSearchText           := '';
   FReplaceText          := '';
   FSearchReplaceOptions := [];

   OnSpecialLineColors := SyntaxEditSpecialLineColors;

{$IFDEF COR_SPECIFIC}
   FView := EditorOptions.GeneralOptions.View;

   FExtra := TCellMLModelExtra.Create(aOwner);
{$ENDIF}
End;

//==============================================================================

Destructor TSyntaxEdit.Destroy;
Begin
{$IFDEF COR_SPECIFIC}
   FExtra.Free;
{$ENDIF}

   Inherited;
End;

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.SetMD5(Const aValue: String);
Begin
   If (aValue <> FMD5) Then
      FMD5 := aValue;
End;
{$ENDIF}

//==============================================================================

Procedure TSyntaxEdit.SetFileName(Const aValue: String);
Begin
   If (CompareStr(aValue, FFileName) <> 0) Then
      FFileName := aValue;
End;

//==============================================================================

Procedure TSyntaxEdit.SetNewFile(Const aValue: Boolean);
Begin
   If (aValue <> FNewFile) Then
      FNewFile := aValue;
End;

//==============================================================================

Procedure TSyntaxEdit.SetCanSave(Const aValue: Boolean);
Begin
   If (aValue <> FCanSave) Then
      FCanSave := aValue;
End;

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.SetView(Const aValue: TEditorView);
Var
   TempText: String;
   TempTopLine: Integer;
   TempCaretXY: TBufferCoord;
Begin
   If (aValue <> FView) Then Begin
      If (Modified And Not Saving) Then
         If (SaveFile And (FView = evCOR)) Then
            // The file was modified, so saved it and because we are not in the
            // Raw view, we must reload it
            // Note: in the case of the Raw view, there is no need to reload the
            //       file, since it is done automatically

            LoadFile(FFileName);

      // Switch to the new view and load the right contents

      If (CellMLFile(FFileName) And
          (MainForm.FormCreating Or IsValid(False))) Then Begin
         FView := aValue;

         TempText := Text;

         TempTopLine := TopLine;
         TempCaretXY := CaretXY;

         If (FView = evCOR) Then
            // We want the COR view, so...

            Highlighter := SynCellMLSyn
         Else
            // We want the Raw view, so...

            Highlighter := SynXMLSyn;

         Text := OtherView;

         CaretXY := OtherCaretXY;
         TopLine := OtherTopLine;
         // Note: we must set the top line after setting the caret's
         //       coordinates, as otherwise the top line will be overridden...

         OtherView := TempText;

         OtherTopLine := TempTopLine;
         OtherCaretXY := TempCaretXY;
      End;
   End;
End;
{$ENDIF}

//==============================================================================

Procedure TSyntaxEdit.SetSearchText(Const aValue: String);
Begin
   If (CompareStr(aValue, FSearchText) <> 0) Then
      FSearchText := aValue;
End;

//==============================================================================

Procedure TSyntaxEdit.SetReplaceText(Const aValue: String);
Begin
   If (CompareStr(aValue, FReplaceText) <> 0) Then
      FReplaceText := aValue;
End;

//==============================================================================

Procedure TSyntaxEdit.SetSearchReplaceOptions(Const aValue: TSynSearchOptions);
Begin
   If (aValue <> FSearchReplaceOptions) Then
      FSearchReplaceOptions := aValue;
End;

//==============================================================================

Procedure TSyntaxEdit.CommandHandler(aSender: TObject;
                                     aAfterProcessing: Boolean;
                                     Var aHandled: Boolean;
                                     Var aCommand: TSynEditorCommand;
                                     Var aChar: Char;
                                     aData, aHandlerData: Pointer);
Begin
   aHandled := True;

   Try
      Case aCommand Of
         ecNextPage,
         ecPrevPage:
            PostMessage((Owner As TWinControl).Handle, WM_KEYDOWN, VK_TAB, 0);
      Else
         aHandled := False;   // Not a command we can recognise, so...
      End;
   Except
      aHandled := False;   // It should never happen, since the owner of a
                           // syntax editor should be some sort of a windo
                           // control, but one never knows, so...
   End;
End;

//==============================================================================

Procedure TSyntaxEdit.SyntaxEditSpecialLineColors(aSender: TObject;
                                                  aLine: Integer;
                                                  Var aSpecial: Boolean;
                                                  Var aFG, aBG: TColor);
Begin
   If (aLine = HighlightedLineNb) Then Begin
      aBG := HighlightedLine.BackgroundColor;
      aFG := HighlightedLine.ForegroundColor;

      aSpecial := True;
   End;
End;

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.SetOptions(Const aValue: TEditorOptions);
Begin
   With aValue Do Begin
      With GeneralOptions Do Begin
         InsertMode := aValue.GeneralOptions.InsertMode;

         If (GroupUndo) Then
            Options := Options+[eoGroupUndo]
         Else
            Options := Options-[eoGroupUndo];

         If (ScrollPastEOF) Then
            Options := Options+[eoScrollPastEof]
         Else
            Options := Options-[eoScrollPastEof];

         If (AutoIndentMode) Then
            Options := Options+[eoAutoIndent]
         Else
            Options := Options-[eoAutoIndent];

         If (SmartTabs) Then
            Options := Options+[eoSmartTabs]
         Else
            Options := Options-[eoSmartTabs];

         If (BackspaceUnindents) Then
            Options := Options+[eoSmartTabDelete]
         Else
            Options := Options-[eoSmartTabDelete];

         If (ShowScrollHint) Then
            Options := Options+[eoShowScrollHint]
         Else
            Options := Options-[eoShowScrollHint];

         Self.WordWrap := WordWrap;

         Self.InsertCaret    := aValue.GeneralOptions.InsertCaret;
         Self.OverwriteCaret := aValue.GeneralOptions.OverwriteCaret;

         TabWidth := aValue.GeneralOptions.TabIndent;
      End;

      With DisplayOptions Do Begin
         If (ShowRightMargin) Then
            RightEdge := RightMargin
         Else
            RightEdge := 0;

         Gutter.Visible := ShowGutter;
         Gutter.Width   := GutterWidth;

         Font.Name := FontName;
         Font.Size := FontSize;
      End;

      // Elements

      With ColourOptions Do Begin
         With Elements.Get(DEFAULT) Do Begin
            Color      := BackgroundColor;
            Font.Color := ForegroundColor;
         End;

         With Elements.Get(ERROR_LINE) Do Begin
            HighlightedLine.BackgroundColor := BackgroundColor;
            HighlightedLine.ForegroundColor := ForegroundColor;
         End;

         With Elements.Get(SELECTED_TEXT) Do Begin
            SelectedColor.Background := BackgroundColor;
            SelectedColor.Foreground := ForegroundColor;
         End;

         RightEdgeColor := Elements.Get(RIGHT_MARGIN).ForegroundColor;

         Gutter.Color := Elements.Get(GUTTER_AREA).BackgroundColor;

         ScrollHintColor := Elements.Get(SCROLL_HINT).BackgroundColor;
      End;
   End;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.GoToPos(Const aLineNb: Integer; Const aColNb: Integer);
Var
   RealLinesInWindow: Integer;
Begin
   // Go to the requested line/column

   If (aColNb <> -1) Then
      SetCaretXYEx(False, BufferCoord(aColNb, aLineNb))
   Else
      SetCaretXYEx(False, BufferCoord(1, aLineNb));

   // Make sure that the caret is visible
   // Note: we would normally use "EnsureCursorPosVisibleEx", but the value of
   //       "LinesInWindow" is not yet set at this stage, so...

   If (Not MainForm.FormCreating) Then
      RealLinesInWindow := LinesInWindow
   Else
      // When starting COR, "LinesInWindow" has not yet been set, while both
      // "ClientHeight" and "LineHeight" have already been, so...

      RealLinesInWindow := ClientHeight Div LineHeight;

   If ((aLineNb < TopLine) Or (aLineNb >= TopLine+RealLinesInWindow)) Then
      TopLine := aLineNb-(RealLinesInWindow-1) Div 2;
      // Note: the "-1" is just for the case where there is an even number of
      //       lines and therefore make sure that we end up with an even number
      //       of lines above the current position, while an odd number below it
      //       (yes, I prefer it that way, so... :))...

   // Unselect the current block, if any

   If (SelAvail) Then Begin
      BlockBegin := BufferCoord(CaretX, CaretY);
      BlockEnd   := BlockBegin;
   End;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.UnhighlightMsg(Const aUpdateOptions: Boolean);
Var
   OldHighlightedLineNb: Integer;
Begin
   If (HighlightedLineNb <> -1) Then Begin
      OldHighlightedLineNb := HighlightedLineNb;

      HighlightedLineNb := -1;

      InvalidateLine(OldHighlightedLineNb);

      If (aUpdateOptions) Then
         Options := Options-[eoNoCaret];
   End;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.HighlightMsg(Const aLineNb: Integer;
                                   Const aColNb: Integer);
Begin
   If (aLineNb <> -1) Then Begin
      If (aColNb = -1) Then Begin
         Options := Options+[eoNoCaret];

         HideCaret;
      End;

      UnhighlightMsg(False);

      HighlightedLineNb := aLineNb;

      // Go to the line/column

      GoToPos(aLineNb, aColNb);

      // Highlight it

      InvalidateLine(aLineNb);
   End;
End;
{$ENDIF}

//==============================================================================

Function TSyntaxEdit.IsNormSep(Const aChar: Char): Boolean;
Begin
   Result := (aChar = CR)  Or (aChar = LF) Or
             (aChar = TAB) Or (aChar = SPACE);
End;

//==============================================================================

Function TSyntaxEdit.IsSpecSep(Const aChar: Char): Boolean;
Begin
   Result := (aChar = '(') Or (aChar = ')') Or (aChar = '=') Or
             (aChar = ',') Or (aChar = ';') Or (aChar = ':');
End;

//==============================================================================

Function TSyntaxEdit.IsAnySep(Const aChar: Char): Boolean;
Begin
   Result := IsNormSep(aChar) Or IsSpecSep(aChar);
End;

//==============================================================================

Function TSyntaxEdit.CrtCmd: String;
   Function GetPos(Const aCaretX, aCaretY: Integer): Integer;
   Var
      Loop: Integer;
      Len: Integer;
   Begin
      Result := 0;

      Loop := 0;

      While (Loop < (aCaretY-1)) Do Begin
         Inc(Result, Length(Lines.Strings[Loop])+2);

         Inc(Loop);
      End;

      Len := Length(Lines.Strings[Loop])+1;

      If (aCaretX < Len) Then
         Inc(Result, aCaretX)
      Else
         Inc(Result, Len);
   End;
   Function GetCrtWord(Const aText: String; Var aCrtWordStartPos, aCrtWordEndPos: Integer;
                       Const aNext: Boolean = True): String;
   Var
      TextLen: Integer;
   Begin
      TextLen := Length(aText);

      If (aNext) Then Begin
         aCrtWordStartPos := aCrtWordEndPos+1;

         If (aCrtWordStartPos > TextLen) Then Begin
            Result := '';

            Exit;
         End;

         // Go to the beginning of the current word, if necessary...

         While (aCrtWordStartPos < TextLen) And IsNormSep(aText[aCrtWordStartPos]) Do
            Inc(aCrtWordStartPos);

         // Determine the end of the current word

         aCrtWordEndPos := aCrtWordStartPos;

         While (aCrtWordEndPos < TextLen) And Not IsAnySep(aText[aCrtWordEndPos]) Do
            Inc(aCrtWordEndPos);

         If ((aCrtWordStartPos <> aCrtWordEndPos) And
             (aCrtWordEndPos <= TextLen) And IsAnySep(aText[aCrtWordEndPos])) Then
            Dec(aCrtWordEndPos);   // Since we went one too far...

         // Determine the beginning of the current word
         // Note: useful if we are not positioned at the end of a word when
         //       coming here...

         aCrtWordStartPos := aCrtWordEndPos;

         While (aCrtWordStartPos > 1) And Not IsAnySep(aText[aCrtWordStartPos]) Do
            Dec(aCrtWordStartPos);

         If ((aCrtWordStartPos <> aCrtWordEndPos) And
             (aCrtWordStartPos > 1) And IsAnySep(aText[aCrtWordStartPos])) Then
            Inc(aCrtWordStartPos);   // Since we went one too far...
      End Else Begin
         aCrtWordEndPos := aCrtWordStartPos-1;

         If (aCrtWordEndPos < 1) Then Begin
            Result := '';

            Exit;
         End;

         // Go to the end of the current word, if necessary...

         While (aCrtWordEndPos > 1) And IsNormSep(aText[aCrtWordEndPos]) Do
            Dec(aCrtWordEndPos);

         // Determine the beginning of the current word

         aCrtWordStartPos := aCrtWordEndPos;

         While (aCrtWordStartPos > 1) And Not IsAnySep(aText[aCrtWordStartPos]) Do
            Dec(aCrtWordStartPos);

         If ((aCrtWordStartPos <> aCrtWordEndPos) And
             (aCrtWordStartPos > 1) And IsAnySep(aText[aCrtWordStartPos])) Then
            Inc(aCrtWordStartPos);   // Since we went one too far...

         // Determine the end of the current word
         // Note: useful if we are not positioned at the beginning of a word
         //       when coming here...

         aCrtWordEndPos := aCrtWordStartPos;

         While (aCrtWordEndPos < TextLen) And Not IsAnySep(aText[aCrtWordEndPos]) Do
            Inc(aCrtWordEndPos);

         If ((aCrtWordStartPos <> aCrtWordEndPos) And
             (aCrtWordEndPos <= TextLen) And IsAnySep(aText[aCrtWordEndPos])) Then
            Dec(aCrtWordEndPos);   // Since we went one too far...
      End;

      // Extract the current word

      Result := Copy(aText, aCrtWordStartPos, aCrtWordEndPos-aCrtWordStartPos+1);
   End;
   Function GetCrtCmd(Const aText: String; Const aPos, aMaxPos: Integer;
                      Var aCrtCmdStartPos, aCrtCmdEndPos: Integer;
                      Const aBeginningOfUnitDefinition: Boolean = False): String;
   Var
      CrtWord: String;
      CrtWordStartPos, CrtWordEndPos: Integer;
   Begin
      If ((aPos < 1) Or (aPos > aMaxPos)) Then Begin
         Result := '';

         Exit;
      End;

      // Go to the beginning of the current command

      CrtWordStartPos := aPos+1;
      // Note: the "+1" is because of the loop and the way "GetCrtWord" works...

      Repeat
         CrtWord := GetCrtWord(aText, CrtWordStartPos, CrtWordEndPos, False);
      Until (CrtWordStartPos = 1) Or
            (((CompareStr(CrtWord, ';') = 0) Or (CompareStr(CrtWord, 'as') = 0)) And
             ((aPos < CrtWordStartPos) Or (aPos > CrtWordEndPos)));

      If (CrtWordStartPos = 1) Then
         aCrtCmdStartPos := CrtWordStartPos
      Else
         aCrtCmdStartPos := CrtWordEndPos+1;

      While (aCrtCmdStartPos < aMaxPos) And IsNormSep(aText[aCrtCmdStartPos]) Do
         Inc(aCrtCmdStartPos);

      // Go to the end of the current command

      Repeat
         CrtWord := GetCrtWord(aText, CrtWordStartPos, CrtWordEndPos);
      Until (CrtWordEndPos = aMaxPos) Or (CompareStr(CrtWord, ';') = 0) Or
            (aBeginningOfUnitDefinition And (CompareStr(CrtWord, 'as') = 0));

      aCrtCmdEndPos := CrtWordEndPos;

      Result := Copy(aText, aCrtCmdStartPos, aCrtCmdEndPos-aCrtCmdStartPos+1);
   End;
   Function GetBeginningOfUnitDefinition(Const aText: String;
                                         Const aMaxPos: Integer;
                                         Var aCrtCmdStartPos, aCrtCmdEndPos: Integer;
                                         Const aResetIfNecessary: Boolean): String;
   Var
      CrtCmd: String;
      OldCrtCmdStartPos: Integer;
      CrtWordStartPos, CrtWordEndPos: Integer;
      CrtWord1, CrtWord2: String;
   Begin
      Result := '';

      Repeat
         OldCrtCmdStartPos := aCrtCmdStartPos;   // To check for in case we are
                                                 // going in circles...

         CrtWordEndPos := 0;

         // Go to the end of the previous word

         Dec(aCrtCmdStartPos);

         While (aCrtCmdStartPos > 1) And IsNormSep(aText[aCrtCmdStartPos]) Do
            Dec(aCrtCmdStartPos);

         CrtCmd := GetCrtCmd(aText, aCrtCmdStartPos, aMaxPos, aCrtCmdStartPos, aCrtCmdEndPos, True);

         Result := CrtCmd+' '+Result;
         // Note: the space is just in case "CrtCmd" finishes with "as", i.e.
         //       not a 'proper' separator. Indeed, "Result" could be equal to
         //       "unit..." in which case we would end up with "... asunit...",
         //       so...

         CrtWord1 := GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
         CrtWord2 := GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
      Until (aCrtCmdStartPos = 0) Or (aCrtCmdStartPos = OldCrtCmdStartPos) Or
            ((CompareStr(CrtWord1, 'unit') <> 0) And (CompareStr(CrtWord1, 'def') <> 0) And (CompareStr(CrtWord2, 'unit') <> 0)) Or
            ((CompareStr(CrtWord1, 'def') = 0) And (CompareStr(CrtWord2, 'unit') = 0));

      If (aResetIfNecessary And
          (CompareStr(CrtWord1, 'def') <> 0) And (CompareStr(CrtWord2, 'unit') <> 0)) Then
         // Cannot find the corresponding "def unit", so it may not have been a
         // unit declaration after all, so...

         Result := '';
   End;
   Function GetEndOfUnitDefinition(Const aText: String;
                                   Const aMaxPos: Integer;
                                   Var aCrtCmdStartPos, aCrtCmdEndPos: Integer): String;
   Var
      CrtCmd: String;
      CrtWordStartPos, CrtWordEndPos: Integer;
   Begin
      Result := '';

      Repeat
         CrtWordEndPos := 0;

         CrtCmd := GetCrtCmd(aText, aCrtCmdEndPos+1, aMaxPos, aCrtCmdStartPos, aCrtCmdEndPos);

         Result := Result+CrtCmd;
      Until (aCrtCmdEndPos = aMaxPos) Or
            (CompareStr(GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos), 'enddef') = 0);
   End;
   Function GetBeginningOfPiecewiseEqn(Const aText: String;
                                       Const aMaxPos: Integer;
                                       Var aCrtCmdStartPos, aCrtCmdEndPos: Integer): String;
   Var
      CrtCmd: String;
      OldCrtCmdStartPos: Integer;
      CrtWordStartPos, CrtWordEndPos: Integer;
      CrtWord1, CrtWord2, CrtWord3, CrtWord8: String;
   Begin
      Result := '';

      Repeat
         OldCrtCmdStartPos := aCrtCmdStartPos;   // To check for in case we are
                                                 // going in circles...

         CrtWordEndPos := 0;

         // Go to the end of the previous word

         Dec(aCrtCmdStartPos);

         While (aCrtCmdStartPos > 1) And IsNormSep(aText[aCrtCmdStartPos]) Do
            Dec(aCrtCmdStartPos);

         CrtCmd := GetCrtCmd(aText, aCrtCmdStartPos, aMaxPos, aCrtCmdStartPos, aCrtCmdEndPos);

         Result := CrtCmd+Result;

         CrtWord1 := GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
         CrtWord2 := GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
         CrtWord3 := GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);

         GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
         GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
         GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
         GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);

         CrtWord8 := GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);
      Until (aCrtCmdStartPos = 0) Or (aCrtCmdStartPos = OldCrtCmdStartPos) Or
            ((CompareStr(CrtWord1, 'case') <> 0) Or (CompareStr(CrtWord1, 'otherwise') <> 0)) And
             (((CompareStr(CrtWord2, '=') = 0) And (CompareStr(CrtWord3, 'sel') = 0)) Or
              ((CompareStr(CrtWord1, 'ode') = 0) And (CompareStr(CrtWord8, 'sel') = 0))) Or
            (CompareStr(CrtWord2, '=') = 0) And (CompareStr(CrtWord3, 'sel') = 0) Or
            (CompareStr(CrtWord1, 'ode') = 0) And (CompareStr(CrtWord8, 'sel') = 0);
   End;
   Function GetEndOfPiecewiseEqn(Const aText: String;
                                 Const aMaxPos: Integer;
                                 Var aCrtCmdStartPos, aCrtCmdEndPos: Integer): String;
   Var
      CrtCmd: String;
      CrtWordStartPos, CrtWordEndPos: Integer;
   Begin
      Result := '';

      Repeat
         CrtWordEndPos := 0;

         CrtCmd := GetCrtCmd(aText, aCrtCmdEndPos+1, aMaxPos, aCrtCmdStartPos, aCrtCmdEndPos);

         Result := Result+CrtCmd;
      Until (aCrtCmdEndPos = aMaxPos) Or
            (CompareStr(GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos), 'endsel') = 0);
   End;
Var
   CrtText: String;
   Pos: Integer;
   CrtTextLen: Integer;
   CrtCmdStartPos, CrtCmdEndPos: Integer;
   CrtCmd, CrtCmdAddOn: String;
   CrtWordStartPos, CrtWordEndPos: Integer;
   CrtWord: String;
   OrigCrtCmdStartPos, OrigCrtCmdEndPos: Integer;
Begin
   CrtText := Text;   // We get it once, since it is VERY time consuming!

   Pos := GetPos(CaretX, CaretY);

   CrtTextLen := Length(CrtText);

   If ((CrtTextLen = 0) Or (Pos > CrtTextLen)) Then Begin
      Result := '';

      Exit;
   End;

   // Get the current command

   CrtCmd := GetCrtCmd(CrtText, Pos, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos);

   // Is the current command the beginning of a valid one?

   CrtWordEndPos := 0;

   CrtWord := GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);

   If (CompareStr(CrtWord, 'def') = 0) Then Begin
      // Maybe the beginning of a unit definition, so...

      If (CompareStr(GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos), 'unit') = 0) Then
         // Should be a unit definition, so...

         Result := CrtCmd+GetEndOfUnitDefinition(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos)
      Else
         // Not a unit definition, so...

         Result := '';
   End Else If (CompareStr(CrtWord, 'base') = 0) Then 
      // Middle of a unit definition, so... look for the corresponding "def
      // unit", if any...

      Result := GetBeginningOfUnitDefinition(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos, False)+' '+CrtCmd
      // Note: the space before "CrtCmd" is because
      //       "GetBeginningOfUnitDefinition" returns something like
      //       "def unit MyUnit as" and "CrtCmd" will be "base unit;", which
      //       means we would up with "... asbase...", so...
   Else If (CompareStr(CrtWord, 'unit') = 0) Then Begin
      // Middle of a unit definition, so... look for the corresponding "def
      // unit" and then "enddef", if any...

      OrigCrtCmdStartPos := CrtCmdStartPos;
      OrigCrtCmdEndPos   := CrtCmdEndPos;

      // Look for the corresponding "def unit", if any...

      Result := GetBeginningOfUnitDefinition(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos, False)+' '+CrtCmd;
      // Note: the space before "CrtCmd" is just in case
      //       "GetBeginningOfUnitDefinition" returns something like
      //       "def unit MyUnit as", "CrtCmd" will be with "unit..." and if we
      //       didn't have the space, we would end up with "... asunit...",
      //       so...

      // Look for the corresponding "enddef", if any...

      Result := Result+GetEndOfUnitDefinition(CrtText, CrtTextLen, OrigCrtCmdStartPos, OrigCrtCmdEndPos);
   End Else If (CompareStr(CrtWord, 'enddef') = 0) Then Begin
      // Maybe the end of a unit definition, so... look for the corresponding
      // "def unit", if any...

      CrtCmdAddOn := GetBeginningOfUnitDefinition(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos, True);

      If (CompareStr(CrtCmdAddOn, '') <> 0) Then
         Result := CrtCmdAddOn+CrtCmd
      Else
         Result := '';
   End Else If (CompareStr(CrtWord, 'var') = 0) Then
      // Variable declaration, so...

      Result := CrtCmd
   Else If (CompareStr(CrtWord, 'ode') = 0) Then Begin
      // Should be an ODE, so...

      GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);   // "("
      GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);   // State variable
      GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);   // ","
      GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);   // Time
      GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);   // ")"
      GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos);   // "="

      If (CompareStr(GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos), 'sel') = 0) Then
         // Piecewise statement, so...

         Result := CrtCmd+GetEndOfPiecewiseEqn(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos)
      Else
         // Simple ODE, so...

         Result := CrtCmd;
   End Else If ((CompareStr(CrtWord, 'case') = 0) Or (CompareStr(CrtWord, 'otherwise') = 0)) Then Begin
      // Middle of a piecewise equation, whether algebraic or ODE, so...

      OrigCrtCmdStartPos := CrtCmdStartPos;
      OrigCrtCmdEndPos   := CrtCmdEndPos;

      // Look for the corresponding "def unit", if any...

      Result := GetBeginningOfPiecewiseEqn(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos)+CrtCmd;

      // Look for the corresponding "enddef", if any...

      Result := Result+GetEndOfPiecewiseEqn(CrtText, CrtTextLen, OrigCrtCmdStartPos, OrigCrtCmdEndPos);
   End Else If (CompareStr(CrtWord, 'endsel') = 0) Then
      // End of a piecewise equation, whether algebraic or ODE, so...

      Result := GetBeginningOfPiecewiseEqn(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos)+CrtCmd
   Else Begin
      // Nothing that we can recognise so far, so check the second word to see
      // whether it isn't an algebraic equation...

      CrtWordStartPos := CrtWordEndPos+1;

      If (CompareStr(GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos), '=') = 0) Then Begin
         // Should be an algebraic equation, so...

         If (CompareStr(GetCrtWord(CrtCmd, CrtWordStartPos, CrtWordEndPos), 'sel') = 0) Then
            // Piecewise statement, so...

            Result := CrtCmd+GetEndOfPiecewiseEqn(CrtText, CrtTextLen, CrtCmdStartPos, CrtCmdEndPos)
         Else
            // Simple algebraic equatio, so...

            Result := CrtCmd;
      End Else
         // Definitely not something we can recognise, so...

         Result := '';
   End;
End;

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.SearchReplace(Const aForward: Boolean);
Var
   LocalSearchReplaceOptions: TSynSearchOptions;
Begin
   If (aForward) Then Begin
      LocalSearchReplaceOptions := FSearchReplaceOptions;

      If (ssoBackwards In FSearchReplaceOptions) Then
         CaretXY := BlockBegin
      Else
         CaretXY := BlockEnd;
   End Else Begin
      If (ssoBackwards In FSearchReplaceOptions) Then Begin
         LocalSearchReplaceOptions := FSearchReplaceOptions-[ssoBackwards];

         CaretXY := BlockEnd;
      End Else Begin
         LocalSearchReplaceOptions := FSearchReplaceOptions+[ssoBackwards];

         CaretXY := BlockBegin;
      End;
   End;

   If (Inherited SearchReplace(SearchText, FReplaceText, LocalSearchReplaceOptions) = 0) Then Begin
      MessageDlg('Search string '''+SearchText+''' not found', mtInformation, [mbOK], 0);

      If (ssoBackwards In LocalSearchReplaceOptions) Then
         BlockEnd := BlockBegin
      Else
         BlockBegin := BlockEnd;

      CaretXY := BlockBegin;
   End;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.Duplicate(Const aSyntaxEdit: TSyntaxEdit);
Begin
   If (CellMLFile(aSyntaxEdit.FileName)) Then
      Highlighter := aSyntaxEdit.Highlighter;

   Text := aSyntaxEdit.Text;

   FView := aSyntaxEdit.View;

   Modified := True;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.NewCellMLFile;
Begin
   FView := EditorOptions.GeneralOptions.View;

   If (FView = evCOR) Then Begin
      Highlighter := SynCellMLSyn;

      Lines.Add('def model MyModel'+IntToStr(NewCellMLFileID-1)+' as');
      Lines.Add('enddef;');
   End Else Begin
      Highlighter := SynXMLSyn;

      Lines.Add('<?xml version="1.0" encoding="utf-8"?>');
      Lines.Add('<!--');
      Lines.Add('This CellML file was generated on '+ReplaceStr(DateTimeToStr(Now), ' ', ' at ')+' using:');
      Lines.Add('');
      Lines.Add(COR_NAME+' ('+COR_VERSION+')');
      Lines.Add(COR_FULL_COPYRIGHT);
      Lines.Add(COR_URL+' - '+COR_EMAIL);
      Lines.Add('');
      Lines.Add(CELLML_VERSION+' was used to generate this model');
      Lines.Add(CELLML_URL);
      Lines.Add('-->');
      Lines.Add('<model name="MyModel'+IntToStr(NewCellMLFileID-1)+'" cmeta:id="MyModel'+IntToStr(NewCellMLFileID-1)+'" xmlns="http://www.cellml.org/cellml/1.0#" xmlns:cellml="http://www.cellml.org/cellml/1.0#" xmlns:cmeta="http://www.cellml.org/metadata/1.0#">');
      Lines.Add('</model>');
   End;

   Modified := True;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.LoadFileAsRaw(Const aFileName: String);
Var
   TempText: String;
   TempTopLine: Integer;
   TempCaretXY: TBufferCoord;
Begin
   OtherView := Text;

   OtherTopLine := TopLine;
   OtherCaretXY := CaretXY;

   Lines.LoadFromFile(aFileName);

   If (FView = evCOR) Then Begin
      TempText := Text;

      TempTopLine := TopLine;
      TempCaretXY := CaretXY;

      Text := OtherView;

      CaretXY := OtherCaretXY;
      TopLine := OtherTopLine;
      // Note: we must set the top line after setting the caret's
      //       coordinates, as otherwise the top line will be overridden...

      OtherView := TempText;

      OtherTopLine := TempTopLine;
      OtherCaretXY := TempCaretXY;
   End;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Function TSyntaxEdit.LoadFile(Const aFileName: String;
                              Const aSelFirstMsg: Boolean): Boolean;
Var
   ShouldHaveFocus: Boolean;
   CellMLModel: TCellMLModel;
   TL: Integer;
   CXY: TBufferCoord;
Begin
   ShouldHaveFocus := Focused;

   Enabled := False;
   // Note: this ensures that there is no risk of flickering due to "TopLine"
   //       and "CaretXY" being potentially reset at some point or another. E.g.
   //       say you are in Raw view and that you save a CellML file. This will
   //       involve reloading it (in case the CellML file was malformed and now
   //       is properly formed), which means loading the COR view and then Raw
   //       view. The COR view is always shorter than the Raw view, so that
   //       means that if we were toward the end of the Raw view, then "TopLine"
   //       and "CaretXY" will be reset (done automatically upon setting the
   //       "Text"/"Lines" property)and set back to their original values (done
   //       by ourselves), hence the flickering indeed...

   TL  := TopLine;
   CXY := CaretXY;

   Result := True;

   If (CellMLFile(aFileName)) Then Begin
      // CellML file

      If (FView = evCOR) Then
         Highlighter := SynCellMLSyn
      Else
         Highlighter := SynXMLSyn;

      // Load the CellML file as a COR version, and check for validity if in COR
      // view

      CellMLModel := TCellMLModel.Create(aFileName);

      If (CellMLFileToCellMLAPI(aFileName, CellMLModel)) Then Begin
         If (CellMLAPIToCellMLASCII(CellMLModel, Self)) Then Begin
            If (FView = evCOR) Then
               CellMLModelIsValid(CellMLModel);
         End Else
            Result := False;
      End Else
         Result := False;

      CellMLModel.Free;

      // Load the CellML file as a Raw version, and check for validity if in Raw
      // view

      LoadFileAsRaw(aFileName);

      If (Result And (FView = evRaw)) Then Begin
         CellMLModel := TCellMLModel.Create(aFileName);

         CellMLFileToCellMLAPI(aFileName, CellMLModel);

         CellMLModelIsValid(CellMLModel);

         CellMLModel.Free;
      End;

      // Check whether something went wrong when converting the CellML file

      If (Not Result And Not MainForm.EditorFrame.AtLeastOneMsg(aFileName)) Then
         // Something wrong didn't get picked up while converting the CellML
         // file, so...

         MainForm.EditorFrame.AddMsg(mtError, aFileName, UNDEFINED, UNDEFINED, 'an unknown problem was found when trying to open this CellML file', False);
   End Else
      // Unrecognisable file, so...

      Result := False;

   If (Not Result) Then Begin
      // Text file of some sort or problem with the CellML file, so...

      Lines.LoadFromFile(aFileName);

      // If the file was supposed to be a CellML file, then...

      If (CellMLFile(aFileName)) Then Begin
         // It should be a CellML file, so highlight the file using an XML
         // highlighter

         FView := evRaw;

         Highlighter := SynXMLSyn;
      End;
   End;

   // A few other settings

   Modified := False;
   FNewFile := False;   // Not always useful, but...
{$IFDEF COR_SPECIFIC}
   FMD5     := FileMD5(aFileName);
{$ENDIF}
   ReadOnly := FileIsReadOnly(aFileName);

   CaretXY := CXY;
   TopLine := TL;
   // Note: we must set the top line after setting the caret's coordinates, as
   //       otherwise the top line will be overridden...

   Enabled := True;

   // The focus will have been lost (probably because of the disabling the
   // component), so reset the focus to the syntax editor, in case it had it...

   If (ShouldHaveFocus) Then
      MainForm.EditorFrame.SetFocusTo(Self);

   // Select the first error message related to the current file, if any

   If (aSelFirstMsg) Then
      MainForm.EditorFrame.SelFirstMsg(aFileName);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Function TSyntaxEdit.SaveFile(Const aSaveAs: Boolean;
                              Const aVirtualStringTree: TVirtualStringTree;
                              Const aCellMLModelRuntime: TCellMLModelRuntime;
                              Const aSelFirstMsg: Boolean): Boolean;
   Procedure UpdateProgressBar(Var aProgBarVal: Integer;
                               Const aNbOfSteps: Integer);
   Begin
      Inc(aProgBarVal);

      MainForm.ComputationFrame.ProgressBar.Position := Round(100*aProgBarVal/aNbOfSteps);

      Application.ProcessMessages;
   End;
Const
   _NB_OF_CELLML_STEPS_ = 5;
Var
   ShouldHaveFocus: Boolean;
   AsFileName: String;
   CellMLModel: TCellMLModel;
   SaveAndUpdate: Boolean;
   CellMLStepNb: Integer;
   RealNewFile: Boolean;
Begin
   ShouldHaveFocus := Focused;

   Saving := True;

   Result := True;   // File saved by default

   RealNewFile := FNewFile;   // Save it, since it gets overridden when dealing
                              // with a Raw file...

   If (RealNewFile Or aSaveAs) Then Begin
      If (ExecSaveDialog(FFileName)) Then Begin
         If (CanOverwriteFile(SaveDialog.FileName)) Then
            AsFileName := SaveDialog.FileName
         Else
            Result := False;   // File not saved
      End Else
         Result := False;   // File not saved
   End Else If (Modified Or (aVirtualStringTree <> Nil)) Then
      AsFileName := FFileName
   Else Begin
      Saving := False;

      Exit;   // Nothing to save, so... (note: "Result" must be "True"!)
   End;

   If (Result) Then Begin
      If (CellMLFile(AsFileName) And (FView = evCOR)) Then Begin
         // CellML file

         CellMLModel := TCellMLModel.Create(AsFileName);

         If ((aCellMLModelRuntime <> Nil) And (aVirtualStringTree <> Nil)) Then Begin
            // The initial values of the model's parameters are available and we
            // want to update the runtime version of the model, so...

            SaveAndUpdate := True;

            CellMLStepNb := 0;

            MainForm.ComputationFrame.StatusBarInfoProgBar;
         End Else
            SaveAndUpdate := False;

         If (CellMLASCIIToCellMLAPI(Self, CellMLModel)) Then Begin
            If (SaveAndUpdate) Then Begin
               UpdateProgressBar(CellMLStepNb, _NB_OF_CELLML_STEPS_);

               UpdateCellMLAPI(CellMLModel, aVirtualStringTree);
               // Note: it's not time consuming enough to update the progress
               //       bar, so...
            End;

            If (CellMLAPIToCellMLFile(CellMLModel, AsFileName)) Then Begin
               If (SaveAndUpdate) Then
                  UpdateProgressBar(CellMLStepNb, _NB_OF_CELLML_STEPS_);

               // Reload the Raw view of the CellML file since it has just been
               // successfully saved

               LoadFileAsRaw(AsFileName);

               CellMLModelIsValid(CellMLModel, SaveAndUpdate);
               // Just to keep the user updated
               // Note: we do NOT select the first message!

               If (SaveAndUpdate) Then Begin
                  UpdateProgressBar(CellMLStepNb, _NB_OF_CELLML_STEPS_);

                  // While saving the file, we are also asked to provide a
                  // runtime version of the model, so...

                  With TCellMLAPIToMCEngine.Create(CellMLModel, aCellMLModelRuntime) Do Begin
                     Execute;

                     Free;
                  End;

                  UpdateProgressBar(CellMLStepNb, _NB_OF_CELLML_STEPS_);

                  // The initial value of some model parameters have been
                  // updated, so we must 'reload' the file
                  // Note: we would normally use "LoadFile", but that would
                  //       involve generating a new "CellMLModel" while it would
                  //       be more efficient to use the one we have just
                  //       created...

                  CellMLAPIToCellMLASCII(CellMLModel, Self);

                  UpdateProgressBar(CellMLStepNb, _NB_OF_CELLML_STEPS_);
               End;
            End Else Begin
               // We should never reach this point, since we know that the
               // CellML file is valid, so it has to be something wrong with
               // saving the CellML file itself, so...

               MainForm.EditorFrame.ClearSpecMsgs(AsFileName);

               MainForm.ComputationFrame.StatusBarInfoEdit(Self);

               Application.ProcessMessages;

               MessageDlg(''''+AsFileName+'''cannot be saved', mtInformation, [mbOK], 0);

               Result := False;
            End;
         End Else
            Result := False;

         CellMLModel.Free;
      End Else Begin
         // Text file of some sort or Raw view of the CellML file, so...

         Lines.SaveToFile(AsFileName);

         // Reload the file, if its extension is that of a CellML file
         // Note: useful if there was a problem with the XML code and that the
         //       user has fixed it and saved the file...

         If (CellMLFile(AsFileName)) Then
            LoadFile(AsFileName, aSelFirstMsg);
      End;

      // Update a few things regarding the file

      If (Result) Then Begin
         // Update the tab with the new name of the file

         If (aSaveAs) Then
            // The current file was saved under a new name, so save it in the
            // MRU list

            MainForm.EditorFrame.AddMRUFile(FFileName);

         If (RealNewFile Or aSaveAs) Then Begin
            // The current file is to be replaced with a 'new' one, so update
            // its messages

            MainForm.EditorFrame.UpdateSpecMsgs(FFileName, AsFileName);

            // Update the page's caption with the new file name

            MainForm.EditorFrame.PageCtrl.ActivePage.Caption := ExtractCellMLFileName(AsFileName);
         End;

         Modified   := False;
         FNewFile   := False;   // Not always useful, but...
{$IFDEF COR_SPECIFIC}
         FMD5       := FileMD5(AsFileName);
{$ENDIF}
         ReadOnly   := FileIsReadOnly(AsFileName);
         FFileName  := AsFileName;

         MainForm.ComputationFrame.StatusBarInfoEdit(Self);

         Application.ProcessMessages;
      End Else If (aSelFirstMsg) Then
         // Select the first error message related to the current file, if any

         MainForm.EditorFrame.SelFirstMsg(AsFileName);
   End;

   Saving := False;

   // The focus may have been lost (if the file had to be reloaded for some
   // reason or another), so...

   If (ShouldHaveFocus) Then
      MainForm.EditorFrame.SetFocusTo(Self);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Function TSyntaxEdit.CloseFileQuery: Integer;
Var
   FileState: String;
Begin
   FCanSave := False;

   If (Modified) Then Begin
      If (FNewFile) Then
         FileState := ' has never been saved'
      Else
         FileState := ' has been modfied';

      If (CellMLFile(FFileName) And IsValid(False, True)) Then Begin
         Result := MessageDlg(FFileName+FileState+'. Do you want to save it before closing it?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);

         FCanSave := Result = IDYES;
      End Else Begin
         Result := MessageDlg(FFileName+FileState+', but is not valid and therefore cannot be saved. Do you still want to close it?', mtConfirmation, [mbYes, mbNo], 0);

         If (Result = IDNO) Then
            Result := IDCANCEL;   // So that the file is not closed...
      End;
   End Else
      Result := IDNO;   // So that the file is closed...
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Function TSyntaxEdit.CloseFile(Const aForce: Boolean): Boolean;
Var
   ValidFile: Boolean;
Begin
   Result := False;   // No file closed by default

   If (Not aForce And (FileExists(FFileName) Or Modified)) Then Begin
      // The file is valid in the sense that it can potentially be put in the
      // MRU list...

      ValidFile := True;

      If (Modified) Then Begin
         If (FCanSave) Then
            SaveFile
         Else If (FNewFile) Then
            // The file is new and not empty, but it has not been saved, so...

            ValidFile := False;
      End;

      If (ValidFile) Then Begin
         If (MainForm.FormCloseQuerying) Then Begin
            // The current file is to be added to the list of files to be
            // automatically opened in the next session

            If (FView = evCOR) Then
               EditorFrameOpenedFiles.PushBack([TEditorFrameOpenedFile.Create(FFileName, evCOR)])
            Else
               EditorFrameOpenedFiles.PushBack([TEditorFrameOpenedFile.Create(FFileName, evRaw)]);
         End Else
            // The current file is closed normally, so just add it to the MRU
            // file list

            MainForm.EditorFrame.AddMRUFile(FFileName);

         // File about to be "properly" closed

         Result := True;
      End;

      // Remove all the messages related to the file

      MainForm.EditorFrame.ClearSpecMsgs(FFileName);
   End;

   MainForm.EditorFrame.ClosePage(Owner As TTabSheet);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Function TSyntaxEdit.IsValid(Const aAcknowledgment, aUseTempFile: Boolean): Boolean;
Var
   CellMLModel: TCellMLModel;
   ValidMsg: String;
   RealFileName: String;
Begin
   // Check the CellML file

   ValidMsg := 'The CellML file is valid';

   RealFileName := FFileName;

   CellMLModel := TCellMLModel.Create(RealFileName);

   If (FView = evCOR) Then Begin
      // We are in a COR view, so...

      If (CellMLASCIIToCellMLAPI(Self, CellMLModel)) Then
         Result := CellMLModelIsValid(CellMLModel)
      Else
         Result := False;
   End Else Begin
      // Raw XML view, so first save the file, in case it needs saving

      Result := True;

      If (aUseTempFile) Then Begin
         RealFileName := TempDir+'Temp.cellml';

         Lines.SaveToFile(RealFileName);
      End Else If (Not SaveFile) Then
         Result := False;

      If (Result) Then Begin
         If (CellMLFileToCellMLAPI(RealFileName, CellMLModel)) Then
            Result := CellMLModelIsValid(CellMLModel)
         Else
            Result := False;
      End;
   End;

   CellMLModel.Free;

   If (Result) Then Begin
      If (CellMLFile(RealFileName) And aAcknowledgment) Then
         MessageDlg(ValidMsg, mtInformation, [mbOK], 0);
   End Else Begin
      // There are problems with the file, so show the first message, but only
      // if not dealing with a temporary file

      If (aUseTempFile) Then
         MainForm.EditorFrame.ClearSpecMsgs(RealFileName)
      Else
         MainForm.EditorFrame.SelFirstMsg(RealFileName);
   End;

   If (aUseTempFile And (CompareStr(RealFileName, FFileName) <> 0)) Then
      DeleteFile(RealFileName);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Function TSyntaxEdit.Reformat: Boolean;
Var
   CellMLModel: TCellMLModel;
Begin
   CellMLModel := TCellMLModel.Create(FFileName);

   If (FView = evCOR) Then Begin
      // We are in a COR view, so...

      If (CellMLASCIIToCellMLAPI(Self, CellMLModel)) Then
         Result := CellMLAPIToCellMLASCII(CellMLModel, Self)
      Else
         Result := False;
   End Else
      // Raw XML view, so first save the file, in case it needs saving

      If (SaveFile) Then Begin
         If (CellMLFileToCellMLAPI(FFileName, CellMLModel)) Then Begin
            Result := CellMLAPIToCellMLFile(CellMLModel, FFileName);

            LoadFile(FFileName);
         End Else
            Result := False;
      End Else
         Result := False;

   CellMLModel.Free;

   If (Not Result) Then
      MainForm.EditorFrame.SelFirstMsg(FFileName);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Function TSyntaxEdit.RunFile: Boolean;
Begin
   If (SaveFile) Then Begin
      MainForm.ComputationFrame.CellMLModel := TCellMLModel.Create(FFileName);
      // Note: if everything goes well, then it will be released when destroying
      //       the cell when in computational mode

      If (FView = evCOR) Then
         Result := CellMLASCIIToCellMLAPI(Self, MainForm.ComputationFrame.CellMLModel)
      Else
         Result := CellMLFileToCellMLAPI(FFileName, MainForm.ComputationFrame.CellMLModel);

      Result := Result And CellMLModelIsValid(MainForm.ComputationFrame.CellMLModel);

      If (Result) Then
         // At this stage, we are ready to go into computational mode

         MainForm.GoIntoMode(mComputational)
      Else
         // Things didn't go according to plan, so...

         MainForm.ComputationFrame.CellMLModel.Free;

      // Select the first error message related to the current file, if any

      If (Not Result) Then
         MainForm.EditorFrame.SelFirstMsg(FFileName);
   End Else
      Result := False;
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TSyntaxEdit.ExportCellMLFile(Const aTargetLanguage: TCellMLExportFormat);
Const
   C_FORMAT_INFO = 'C';
   C_FORMAT_DEFEXT = 'c';
   C_FORMAT_DEFEXT2 = 'h';
   C_FORMAT_FILTER = 'C file (*.c; *.h)|*.c;*.h';

   CPP_FORMAT_INFO = 'C++';
   CPP_FORMAT_DEFEXT = 'cpp';
   CPP_FORMAT_DEFEXT2 = 'hpp';
   CPP_FORMAT_FILTER = 'C++ file (*.cpp; *.hpp)|*.cpp;*.hpp';

   DELPHI_FOR_WIN32_FORMAT_INFO = 'Delphi for Win32';
   DELPHI_FOR_WIN32_FORMAT_DEFEXT = 'pas';
   DELPHI_FOR_WIN32_FORMAT_FILTER = 'Delphi for Win32 file (*.pas)|*.pas';

   FORTRAN77_FORMAT_INFO = 'Fortran 77';
   FORTRAN77_FORMAT_DEFEXT = 'f';
   FORTRAN77_FORMAT_DEFEXT2 = 'inc';
   FORTRAN77_FORMAT_FILTER = 'Fortran 77 file (*.f; *.inc)|*.f;*.inc';

   JAVA_FORMAT_INFO = 'Java';
   JAVA_FORMAT_DEFEXT = 'java';
   JAVA_FORMAT_FILTER = 'Java file (*.java)|*.java';

   MATLAB_FORMAT_INFO = 'MATLAB';
   MATLAB_FORMAT_DEFEXT = 'm';
   MATLAB_FORMAT_FILTER = 'MATLAB file (*.m)|*.m';

   MSWORD_FORMAT_INFO = 'Microsoft Word 2007/2010';
   MSWORD_FORMAT_DEFEXT = 'docx';
   MSWORD_FORMAT_FILTER = 'Microsoft Word 2007/2010 file (*.docx)|*.docx';

   PASCAL_FORMAT_INFO = 'Pascal';
   PASCAL_FORMAT_DEFEXT = 'pas';
   PASCAL_FORMAT_FILTER = 'Pascal file (*.pas)|*.pas';

   TEX_FORMAT_INFO = 'TeX';
   TEX_FORMAT_DEFEXT = 'tex';
   TEX_FORMAT_FILTER = 'TeX file (*.tex)|*.tex';
Var
   DoExportCellMLFile: Boolean;
   CellMLModel: TCellMLModel;
   ConversionToCellMLAPIOK: Boolean;
   Info, DefaultExt, Filter: String;
   ExpFileName: Array Of String;
   ExpBaseFileName: String;
   MsgForm: TMsgForm;
Begin
   If (SaveFile) Then Begin
      DoExportCellMLFile := True;   // File to be exported by default

      CellMLModel := TCellMLModel.Create(FFileName);

      If (FView = evCOR) Then
         ConversionToCellMLAPIOK := CellMLASCIIToCellMLAPI(Self, CellMLModel)
      Else
         ConversionToCellMLAPIOK := CellMLFileToCellMLAPI(FFileName, CellMLModel);

      If (ConversionToCellMLAPIOK And CellMLModelIsValid(CellMLModel)) Then Begin
         Case aTargetLanguage Of
            efC: Begin
               Info       := C_FORMAT_INFO;
               DefaultExt := C_FORMAT_DEFEXT;
               Filter     := C_FORMAT_FILTER;
            End;
            efCPP: Begin
               Info       := CPP_FORMAT_INFO;
               DefaultExt := CPP_FORMAT_DEFEXT;
               Filter     := CPP_FORMAT_FILTER;
            End;
            efDelphiForWin32: Begin
               Info       := DELPHI_FOR_WIN32_FORMAT_INFO;
               DefaultExt := DELPHI_FOR_WIN32_FORMAT_DEFEXT;
               Filter     := DELPHI_FOR_WIN32_FORMAT_FILTER;
            End;
            efFortran77: Begin
               Info       := FORTRAN77_FORMAT_INFO;
               DefaultExt := FORTRAN77_FORMAT_DEFEXT;
               Filter     := FORTRAN77_FORMAT_FILTER;
            End;
            efJava: Begin
               Info       := JAVA_FORMAT_INFO;
               DefaultExt := JAVA_FORMAT_DEFEXT;
               Filter     := JAVA_FORMAT_FILTER;
            End;
            efMATLAB: Begin
               Info       := MATLAB_FORMAT_INFO;
               DefaultExt := MATLAB_FORMAT_DEFEXT;
               Filter     := MATLAB_FORMAT_FILTER;
            End;
            efMSWord: Begin
               Info       := MSWORD_FORMAT_INFO;
               DefaultExt := MSWORD_FORMAT_DEFEXT;
               Filter     := MSWORD_FORMAT_FILTER;
            End;
            efPascal: Begin
               Info       := PASCAL_FORMAT_INFO;
               DefaultExt := PASCAL_FORMAT_DEFEXT;
               Filter     := PASCAL_FORMAT_FILTER;
            End;
            efTeX: Begin
               Info       := TEX_FORMAT_INFO;
               DefaultExt := TEX_FORMAT_DEFEXT;
               Filter     := TEX_FORMAT_FILTER;
            End;
         End;

         ExpBaseFileName := ExtractFilePath(FFileName)+ChangeFileExt(ReplaceStr(ExtractFileName(FFileName), ' ', '_'), '.'+DefaultExt);
         // Note: we replace any space in the file name with an underscore...

         If (ExecSaveDialog(ExpBaseFileName, DefaultExt, Filter)) Then Begin
            ExpBaseFileName := ExtractFilePath(SaveDialog.FileName)+ReplaceStr(ExtractFileName(SaveDialog.FileName), ' ', '_');
            // Note: we replace any space in the file name with an underscore
            //       in case the user changed the file name and put one or
            //       several spaces in it...

            If (CompareStr(DefaultExt, C_FORMAT_DEFEXT) = 0) Then Begin
               SetLength(ExpFileName, 2);

               ExpFileName[0] := ExpBaseFileName;
               ExpFileName[1] := ChangeFileExt(ExpBaseFileName, '.'+C_FORMAT_DEFEXT2);
            End Else If (CompareStr(DefaultExt, CPP_FORMAT_DEFEXT) = 0) Then Begin
               SetLength(ExpFileName, 2);

               ExpFileName[0] := ExpBaseFileName;
               ExpFileName[1] := ChangeFileExt(ExpBaseFileName, '.'+CPP_FORMAT_DEFEXT2);
            End Else If (CompareStr(DefaultExt, FORTRAN77_FORMAT_DEFEXT) = 0) Then Begin
               SetLength(ExpFileName, 2);

               ExpFileName[0] := ExpBaseFileName;
               ExpFileName[1] := ChangeFileExt(ExpBaseFileName, '.'+FORTRAN77_FORMAT_DEFEXT2);
            End Else Begin
               SetLength(ExpFileName, 1);

               ExpFileName[0] := ExpBaseFileName;
            End;

            If (Not CanOverwriteFile(ExpFileName)) Then
               DoExportCellMLFile := False;   // File not to be exported
         End Else
            DoExportCellMLFile := False;   // File not to be exported

         If (DoExportCellMLFile) Then Begin
            // Export of the CellML file

            MsgForm := TMsgForm.Create('Exporting to '+Info+'...', MAX_FONT_SIZE);

            Application.ProcessMessages;
            // Note: it ensures that the message window appears straight away...

            Case aTargetLanguage Of
               efC:
                  With TCellMLAPIToCFileEngine.Create(CellMLModel, ExpFileName) Do Begin
                     Execute;

                     Free;
                  End;
               efCPP:
                  With TCellMLAPIToCPPFileEngine.Create(CellMLModel, ExpFileName) Do Begin
                     Execute;

                     Free;
                  End;
               efDelphiForWin32:
                  With TCellMLAPIToDelphiForWin32FileEngine.Create(CellMLModel, ExpFileName) Do Begin
                     Execute;

                     Free;
                  End;
               efFortran77:
                  With TCellMLAPIToFortran77FileEngine.Create(CellMLModel, ExpFileName) Do Begin
                     Execute;

                     Free;
                  End;
               efJava:
                  With TCellMLAPIToJavaFileEngine.Create(CellMLModel, ExpFileName) Do Begin
                     Execute;

                     Free;
                  End;
               efMATLAB:
                  With TCellMLAPIToMATLABFileEngine.Create(CellMLModel, ExpFileName) Do Begin
                     Execute;

                     Free;
                  End;
               efMSWord:
                  With TCellMLAPIToMSWordFileEngine.Create(CellMLModel, ExpFileName, EditorOptions.CommandViewerOptions.UnderscoreForSub, EditorOptions.CommandViewerOptions.GreekSymbols, EditorOptions.CommandViewerOptions.DigitGrouping) Do Begin
                     Execute;

                     Free;
                  End;
               efPascal:
                  With TCellMLAPIToPascalFileEngine.Create(CellMLModel, ExpFileName) Do Begin
                     Execute;

                     Free;
                  End;
               efTeX:
                  With TCellMLAPIToTeXFileEngine.Create(CellMLModel, ExpFileName, EditorOptions.CommandViewerOptions.UnderscoreForSub, EditorOptions.CommandViewerOptions.GreekSymbols, EditorOptions.CommandViewerOptions.DigitGrouping) Do Begin
                     Execute;

                     Free;
                  End;
            End;

            MsgForm.Free;
         End;
      End Else
         // Select the first error message related to the current file, if any

         MainForm.EditorFrame.SelFirstMsg(FFileName);

      CellMLModel.Free;
   End;
End;
{$ENDIF}

//==============================================================================

Procedure Register;
Begin
   RegisterComponents('COR', [TSyntaxEdit]);
End;

//==============================================================================

Initialization

//==============================================================================

{$IFDEF COR_SPECIFIC}
SyntaxEditSearch := TSynEditSearch.Create(Nil);
{$ENDIF}

//==============================================================================

Finalization

//==============================================================================

{$IFDEF COR_SPECIFIC}
SyntaxEditSearch.Free;
{$ENDIF}

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

