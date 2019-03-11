//==============================================================================
// Engine class for exporting a CellML API object
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 07/11/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIExportEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   Engine, CellMLAPI, DeCAL, Common;

//==============================================================================

Type
   TCellMLAPIExportEngineTypeOfEquations = (toeAll, toeComputeOnce, toeCompute);
   TCellMLAPIExportEngine = Class
      Private
         // Properties used for internal purposes

         PrevStatementWasEqn: Boolean;
         PrevEqnWasPiecewiseEqn: Boolean;

      Protected
         // Properties used for internal purposes

         FileName: Array Of String;

         TextFileID: Array Of TextFile;

         PrevStr: Array Of String;

         CellMLModel: TCellMLModel;

         EqnsOutputID: Integer;   // ID of the file where the equations are to
                                  // be output

         CaseSensitive: Boolean;

         StatementSep: Char;

         StatementLen: Integer;
         StatementCommentTag: String;
         StatementContTag: String;
         StatementContTagLoc: Integer;
         StatementContMaxLines: Integer;

         VariableLen: Integer;

         OutputNbExpTag: String;
         OutputNbTail: String;

         PieceOrOtherwiseStatement: String;

         AlreadyACase: Boolean;

         IfThenElseStatementsWithStatementSep: Boolean;

         IfStatement: String;
         ThenStatement: String;
         ElseIfStatement: String;
         ElseStatement: String;
         EndIfStatement: String;

         LeftBracket: String;
         RightBracket: String;

         EqStr: String;
         EqEqStr: String;
         NEqStr: String;
         GTStr: String;
         LTStr: String;
         GEqStr: String;
         LEqStr: String;
         PlusStr: String;
         MinusStr: String;
         TimesStr: String;
         AndStr: String;
         OrStr: String;

         XOrStr: String;
         XOrAsFunc: Boolean;

         PowerStr: String;
         PowerAsOperand: Boolean;
         RootAsOperand: Boolean;

         NotStr: String;
         NotSpaceStr: String;

         tStr: String;

         NbOfStateVariablesStr: String;

         YStart, YEnd: String;
         dYStart, dYEnd: String;

         LogStr: String;

         SqrStr: String;
         SqrtStr: String;

         DotZeroStr: String;

         NoEmptyString: Boolean;

         TrueStr, FalseStr: String;

         PIStr: String;

         ExponentialeStr: String;

         // Methods used for internal purposes

         Function RemoveDuplicateUnderscores(Const aString: String): String;

         Function ShortStringOrFinishWithComment(Const aFullString: String; Const aMaxLen: Integer): Boolean;

         Procedure OutputString(Const aIndex: Integer; Const aString: String = ''); Virtual;
         Procedure Output(Const aIndex: Integer; Const aString: String = '');

         Function OutputNb(Const aNbString: String): String; Overload; Virtual;
         Function OutputNb(Const aNb: Double): String; Overload; 
         Procedure OutputPieceOrOtherwiseStatement(Const aNeedStatementSep: Boolean);
         Procedure AddPieceOrOtherwise(Const aCellMLComponent: String; Const aLHS: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Virtual;
         Procedure AddPiecewiseEqn(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Virtual;
         Procedure AddParentheses(Var aString: String); Inline;
         Function AddOperand(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAssignmentEq: Boolean): String; Virtual;
         Function AddFunc(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Virtual; Abstract;
         Function AddPlus(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
         Function AddMinus(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
         Function AddPower(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Virtual;
         Function AddRoot(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Virtual;
         Function AddNot(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;

         Function GenerateEquation(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAssignmentEqn: Boolean = False): String; Virtual;

         Procedure MathMLEquation(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Virtual;

         Procedure GenerateMathMLEquations(Const aTypeOfEquations: TCellMLAPIExportEngineTypeOfEquations);

      Public
         // Constructor & Destructor

         Constructor Create(Const aCellMLModel: TCellMLModel; Const aFileName: Array Of String; Const aTextFileOutput: Boolean = True);
         Destructor Destroy; Override;

         // User's methods

         Procedure Execute; Virtual;
   End;

//==============================================================================

Const
   UNDEFINED_STATEMENT_SEP = #0;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
   SysUtils, StrUtils;

//==============================================================================

Constructor TCellMLAPIExportEngine.Create(Const aCellMLModel: TCellMLModel;
                                          Const aFileName: Array Of String;
                                          Const aTextFileOutput: Boolean);
Var
   NbOfFileNames: Integer;
   I: Integer;
Begin
   CellMLModel := aCellMLModel;

   CellMLModel.GlobalVariableList.Comparator := CellMLModel.CompareByVarState;

   Sort(CellMLModel.GlobalVariableList);

   NbOfFileNames := Length(aFileName);

   If (aTextFileOutput) Then Begin
      SetLength(FileName, NbOfFileNames);
      SetLength(TextFileID, NbOfFileNames);

      For I := 0 To High(aFileName) Do Begin
         FileName[I] := aFileName[I];

         AssignFile(TextFileID[I], aFileName[I]);

         Rewrite(TextFileID[I]);
      End;
   End;

   SetLength(PrevStr, NbOfFileNames);
   SetLength(CellMLIndent, NbOfFileNames);

   For I := 0 To High(aFileName) Do
      CellMLIndent[I] := '';

   // Default settings are for C/C++

   CaseSensitive := True;

   StatementSep := ';';

   StatementLen := 0;   // No limit
   StatementCommentTag := '//';

   VariableLen := 0;   // No limit

   OutputNbExpTag := 'e';
   OutputNbTail := '';

   IfThenElseStatementsWithStatementSep := True;

   IfStatement     := 'if ';
   ThenStatement   := '';
   ElseIfStatement := 'else if ';
   ElseStatement   := 'else';
   EndIfStatement  := '';

   LeftBracket  := '(';
   RightBracket := ')';

   EqStr     := ' = ';
   EqEqStr   := ' == ';
   NEqStr    := ' != ';
   GTStr     := ' > ';
   LTStr     := ' < ';
   GEqStr    := ' >= ';
   LEqStr    := ' <= ';
   PlusStr   := '+';
   MinusStr  := '-';
   TimesStr  := '*';
   AndStr    := ' && ';
   OrStr     := ' || ';

   XOrStr  := ' ^ ';

   PowerStr := 'pow';

   NotStr      := '!';
   NotSpaceStr := '';

   tStr := CellMLModel.FreeVariable;

   NbOfStateVariablesStr := '_NB_OF_STATE_VARIABLES_';

   YStart := 'Y[';
   YEnd   := ']';

   dYStart := 'dY[';
   dYEnd   := ']';

   LogStr := 'log';

   SqrStr  := '';
   SqrtStr := 'sqrt';

   DotZeroStr := '.0';

   TrueStr  := OutputNb(1);
   FalseStr := OutputNb(0);

   PIStr := OutputNb(PI);

{$IFDEF OPT_MATH}
   ExponentialeStr := OutputNb(OptExp(1));
{$ELSE}
   ExponentialeStr := OutputNb(Exp(1));
{$ENDIF}
End;

//==============================================================================

Destructor TCellMLAPIExportEngine.Destroy;
Var
   I: Integer;
Begin
   For I := 0 To High(TextFileID) Do
      CloseFile(TextFileID[I]);
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.Execute;
   Function UniqueVarName(Const aComponent, aVariable: String;
                          Var aGenName: Boolean): String;
   Var
      Iter: Integer;
      VarCtr, VarNdx: Integer;
      VarSep: String;
   Begin
      VarCtr := 0;
      VarNdx := 0;

      Result := aVariable;

      // Check whether the variable name is unique within the whole model

      For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
         With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
            If ((State In [vsState, vsConstant, vsComputed]) And
                ((CaseSensitive And (CompareStr(Variable, aVariable) = 0)) Or
                 (Not CaseSensitive And (CompareText(Variable, aVariable) = 0)))) Then Begin
               Inc(VarCtr);

               If ((CompareStr(Component, aComponent) = 0) And
                   (CompareStr(Variable, aVariable) = 0)) Then
                  VarNdx := VarCtr;
            End;

      // Generate a unique name in case the variable name is not unique within
      // the whole model

      If (VarCtr > 1) Then Begin
         If (Pos('_', Result) <> 0) Then
            VarSep := '_'
         Else
            VarSep := '';

{$IFDEF OPT_MATH}
         Result := Result+VarSep+StringOfChar('0', OptFloor(OptLog10(VarCtr))-OptFloor(OptLog10(VarNdx)))+IntToStr(VarNdx);
{$ELSE}
         Result := Result+VarSep+StringOfChar('0', Floor(Log10(VarCtr))-Floor(Log10(VarNdx)))+IntToStr(VarNdx);
{$ENDIF}

         aGenName := True;
      End;
   End;
Const
   CST_NAME = 'Cst';
   COMP_VAR_NAME = 'CompVar';
   // Note: these names have to be shorter than VariableLen, but unless we are
   //       dealing with a language that requires insanely short names we should
   //       be fine...
Var
   Iter: Integer;
   GenCtr, GenNdx: Integer;
   GenCstCtr, GenCstNdx: Integer;
   GenCompVarCtr, GenCompVarNdx: Integer;
   GenNameStr: String;
Begin
{$IFDEF OPT_MATH}
   Assert((VariableLen = 0) Or ((VariableLen <> 0) And (VariableLen >= OptMaxI(Length(CST_NAME), Length(COMP_VAR_NAME))+OptFloor(OptLog10(MaxInt)))),
{$ELSE}
   Assert((VariableLen = 0) Or ((VariableLen <> 0) And (VariableLen >= Max(Length(CST_NAME), Length(COMP_VAR_NAME))+Floor(Log10(MaxInt)))),
{$ENDIF}
          'TCellMLAPIExportEngine.Execute: VariableLen is too small.');

   // Generate more meaningful variable names

   GenCstCtr := 0;
   GenCstNdx := 0;

   GenCompVarCtr := 0;
   GenCompVarNdx := 0;

   // The default name of the variable to be used for an export is the variable
   // name itself, i.e. <VariableName>. There is, however, the issue of some
   // languages (e.g. MATLAB) that have a length limit for variable names. So,
   // in the end, the name of a variable is defined as follows:
   //  - Use only the variable name in case it is unique throughout the whole
   //    model;
   //  - Use <VariableName>001, <VariableName>002, etc. for variables with the
   //    same name; and
   //  - Use GenName001, GenName002, etc. for variables with a name that is
   //    more than the allowed size for variable names.

   // Address the first two points

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
         If (State In [vsState, vsConstant, vsComputed]) Then Begin
            VarName := UniqueVarName(Component, Variable, GenName);

            If ((VariableLen <> 0) And (State <> vsState) And (Length(VarName) > VariableLen)) Then Begin
               If (State = vsConstant) Then
                  Inc(GenCstCtr)
               Else
                  Inc(GenCompVarCtr);
            End;
         End;

   // Address the last point
   // Note: we only need to do that for constants and computed variables, since
   //       state variable names are only used in comments

   If ((VariableLen <> 0) And ((GenCstCtr <> 0) Or (GenCompVarCtr <> 0))) Then
      For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
         With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
            If ((State In [vsConstant, vsComputed]) And (Length(VarName) > VariableLen)) Then Begin
               // The name is too long, so use a generic one instead

               If (State = vsConstant) Then Begin
                  GenNameStr := CST_NAME;

                  Inc(GenCstNdx);

                  GenCtr := GenCstCtr;
                  GenNdx := GenCstNdx;
               End Else Begin
                  GenNameStr := COMP_VAR_NAME;

                  Inc(GenCompVarNdx);

                  GenCtr := GenCompVarCtr;
                  GenNdx := GenCompVarNdx;
               End;

{$IFDEF OPT_MATH}
               VarName := GenNameStr+StringOfChar('0', OptFloor(OptLog10(GenCtr))-OptFloor(OptLog10(GenNdx)))+IntToStr(GenNdx);
{$ELSE}
               VarName := GenNameStr+StringOfChar('0', Floor(Log10(GenCtr))-Floor(Log10(GenNdx)))+IntToStr(GenNdx);
{$ENDIF}

               GenName := True;
            End;
End;

//==============================================================================

Function TCellMLAPIExportEngine.RemoveDuplicateUnderscores(Const aString: String): String;
Var
   TempString: String;
Begin
   TempString := aString;

   Result := ReplaceStr(TempString, '__', '_');

   While CompareStr(Result, TempString) <> 0 Do Begin
      TempString := Result;

      Result := ReplaceStr(TempString, '__', '_');
   End;
End;

//==============================================================================

Function TCellMLAPIExportEngine.ShortStringOrFinishWithComment(Const aFullString: String;
                                                               Const aMaxLen: Integer): Boolean;
Var
   CommentPos: Integer;
Begin
   If ((aMaxLen = 0) Or (Length(aFullString) <= aMaxLen)) Then
      // Within the limits, so...

      Result := True
   Else Begin
      // Only ok if it finishes with a comment...

      CommentPos := Pos(StringOfChar(' ', INDENT)+StatementCommentTag, aFullString);

      Result := (CommentPos <> 0) And (CommentPos <= aMaxLen+1);

      If (Not Result And (Pos(StatementCommentTag, aFullString) = 1)) Then
         // The string starts with a comment, so...

         Result := True;
   End;
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.OutputString(Const aIndex: Integer;
                                              Const aString: String);
Begin
   Write(TextFileID[aIndex], aString+CRLF);
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.Output(Const aIndex: Integer;
                                        Const aString: String);
Var
   Str, TempStr: String;
   IndentLen: Integer;
   StatementContTagLen: Integer;
Begin
   If (CompareStr(aString, '') = 0) Then Begin
      If (NoEmptyString) Then
         Exit;
         
      // Check that the previous line is not empty. If it is, then exit...

      If (CompareStr(PrevStr[aIndex], '') = 0) Then
         Exit;

      Str := '';
   End Else If (ShortStringOrFinishWithComment(CellMLIndent[aIndex]+aString, StatementLen)) Then
      Str := CellMLIndent[aIndex]+aString
   Else Begin
      // The string cannot be longer than a given length, but it happens to be,
      // so we have to split it in several segments...

      TempStr := aString;

      IndentLen := Length(CellMLIndent[aIndex]);

      // First segment

      Str     := CellMLIndent[aIndex]+Copy(TempStr, 1, StatementLen-IndentLen);
      TempStr := Copy(TempStr, StatementLen-IndentLen+1, Length(TempStr)-StatementLen+IndentLen);

      // Remaining segments

      StatementContTagLen := Length(StatementContTag);

      If ((StatementContTagLen = 0) Or (StatementContTagLoc = 0)) Then
         Repeat
            OutputString(aIndex, Str);

            Str     := CellMLIndent[aIndex]+Copy(TempStr, 1, StatementLen-IndentLen);
            TempStr := Copy(TempStr, StatementLen-IndentLen+1, Length(TempStr)-StatementLen+IndentLen);
         Until Length(TempStr) = 0
      Else
         Repeat
            OutputString(aIndex, Str);

            Str     := StringOfChar(' ', StatementContTagLoc-1)+StatementContTag+Copy(TempStr, 1, StatementLen-StatementContTagLoc);
            TempStr := Copy(TempStr, StatementLen-StatementContTagLoc+1, Length(TempStr)-StatementLen+StatementContTagLoc);
         Until Length(TempStr) = 0;
   End;

   OutputString(aIndex, Str);

   PrevStr[aIndex] := Str;
End;

//==============================================================================

Function TCellMLAPIExportEngine.OutputNb(Const aNbString: String): String;
Var
   Val: Double;
   ExpTagPos: Integer;
Begin
   // Note #1: when outputing the number, we want to make sure that it ends in
   //          ".0" in case of "integer" numbers, as that may be critical when
   //          trying, for instance, to divide something by such a number...
   //          Indeed, "5/4" could, depending on the language, yield "1" instead
   //          of "1.25", so...
   // Note #2: this obviously doesn't apply to all languages (Delphi doesn't
   //          care about that for instance), but we may as well do it for all
   //          of them, since it doesn't harm...

   Result := ReplaceStr(UpperCase(aNbString), 'E', OutputNbExpTag);

   ExpTagPos := Pos(OutputNbExpTag, Result);

   If (ExpTagPos <> 0) Then Begin
      // Number in scientific notation (e.g. 123e45)

      Val := StrToFloat(Copy(Result, 1, ExpTagPos-1));

      If (Val = Round(Val)) Then
         // Need to convert to 123.0e45

         Result := FloatToStr(Val)+DotZeroStr+Copy(Result, ExpTagPos, Length(Result)-ExpTagPos+1);
   End Else Begin
      // Number in classical notation (e.g. 123)

      Val := StrToFloat(aNbString);

      If (Val = Round(Val)) Then
         // Need to convert to 123.0

         Result := Result+DotZeroStr;

      If (CompareStr(OutputNbTail, '') <> 0) Then
         // Need to convert to 123.0D0 (for instance)

         Result := Result+OutputNbTail;
   End;
End;

//==============================================================================

Function TCellMLAPIExportEngine.OutputNb(Const aNb: Double): String;
Begin
   Result := OutputNb(SimplifyNb(aNb));
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.OutputPieceOrOtherwiseStatement(Const aNeedStatementSep: Boolean);
Begin
   If (CompareStr(PieceOrOtherwiseStatement, '') <> 0) Then Begin
      IncIndent(EqnsOutputID);

      If (aNeedStatementSep And (StatementSep <> UNDEFINED_STATEMENT_SEP)) Then
         Output(EqnsOutputID, PieceOrOtherwiseStatement+StatementSep)
      Else
         Output(EqnsOutputID, PieceOrOtherwiseStatement);

      DecIndent(EqnsOutputID);

      PieceOrOtherwiseStatement := '';
   End;
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.AddPieceOrOtherwise(Const aCellMLComponent: String;
                                                     Const aLHS: String;
                                                     Const aMathMLEquationBinTree: TMathMLCommandBinTree);
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitPiecewise: Begin
         AddPieceOrOtherwise(aCellMLComponent, aLHS, aMathMLEquationBinTree.Left);

         If (aMathMLEquationBinTree.Right <> Nil) Then
            AddPieceOrOtherwise(aCellMLComponent, aLHS, aMathMLEquationBinTree.Right);
      End;
      mitPiece: Begin
         If (Not AlreadyACase) Then Begin
            Output(EqnsOutputID, IfStatement+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+RightBracket+ThenStatement);

            AlreadyACase := True;
         End Else Begin
            OutputPieceOrOtherwiseStatement(IfThenElseStatementsWithStatementSep);

            Output(EqnsOutputID, ElseIfStatement+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+RightBracket+ThenStatement);
         End;

         PieceOrOtherwiseStatement := aLHS+EqStr+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);
      End;
      mitOtherwise: Begin
         OutputPieceOrOtherwiseStatement(IfThenElseStatementsWithStatementSep);

         Output(EqnsOutputID, ElseStatement);

         PieceOrOtherwiseStatement := aLHS+EqStr+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);
      End;
   End;
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.AddPiecewiseEqn(Const aCellMLComponent: String;
                                                 Const aMathMLEquationBinTree: TMathMLCommandBinTree);
Begin
   If (PrevStatementWasEqn) Then
      Output(EqnsOutputID);

   PieceOrOtherwiseStatement := '';

   AlreadyACase := False;

   AddPieceOrOtherwise(aCellMLComponent, GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left), aMathMLEquationBinTree.Right);

   OutputPieceOrOtherwiseStatement(True);

   If (CompareStr(EndIfStatement, '') <> 0) Then
      Output(EqnsOutputID, EndIfStatement);

   PrevStatementWasEqn    := True;
   PrevEqnWasPiecewiseEqn := True;
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.AddParentheses(Var aString: String);
Begin
   aString := LeftBracket+aString+RightBracket;
End;

//==============================================================================

Function TCellMLAPIExportEngine.AddOperand(Const aCellMLComponent: String;
                                           Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                           Const aAssignmentEq: Boolean): String;
Var
   Left, Right: String;
   Op: String;
Begin
   Left  := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);
   Right := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right);

   Op := aMathMLEquationBinTree.Str;

   // Determine whether "Left" and/or "Right" need parentheses around them

   Case aMathMLEquationBinTree.ItemType Of
      mitEq:
         Op := EqStr;
      mitEqEq:
         Op := EqEqStr;
      mitNEq:
         Op := NEqStr;
      mitGT:
         Op := GTStr;
      mitLT:
         Op := LTStr;
      mitGEq:
         Op := GEqStr;
      mitLEq:
         Op := LEqStr;
      mitPlus: Begin
         Op := PlusStr;

         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitMinus,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Right);
         End;
      End;
      mitMinus: Begin
         Op := MinusStr;

         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitMinus,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Right);
            mitPlus:
               If (aMathMLEquationBinTree.Right.Right <> Nil) Then
                  AddParentheses(Right);
         End;
      End;
      mitTimes: Begin
         Op := TimesStr;

         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
            mitPlus, mitMinus:
               If (aMathMLEquationBinTree.Left.Right <> Nil) Then
                  AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Right);
            mitPlus, mitMinus:
               If (aMathMLEquationBinTree.Right.Right <> Nil) Then
                  AddParentheses(Right);
         End;
      End;
      mitDivide: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
            mitPlus, mitMinus:
               If (aMathMLEquationBinTree.Left.Right <> Nil) Then
                  AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitTimes, mitDivide,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Right);
            mitPlus, mitMinus:
               If (aMathMLEquationBinTree.Right.Right <> Nil) Then
                  AddParentheses(Right);
         End;
      End;
      mitAnd: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus, mitTimes, mitDivide,
            mitOr, mitXOr:
               AddParentheses(Left);
            mitPow:
               If (PowerAsOperand) Then
                  AddParentheses(Left);
            mitRoot:
               If (RootAsOperand) Then
                  AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus, mitTimes, mitDivide,
            mitOr, mitXOr:
               AddParentheses(Right);
            mitPow:
               If (PowerAsOperand) Then
                  AddParentheses(Right);
            mitRoot:
               If (RootAsOperand) Then
                  AddParentheses(Right);
         End;

         Op := AndStr;
      End;
      mitOr: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus, mitTimes, mitDivide,
            mitAnd, mitXOr:
               AddParentheses(Left);
            mitPow:
               If (PowerAsOperand) Then
                  AddParentheses(Left);
            mitRoot:
               If (RootAsOperand) Then
                  AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus, mitTimes, mitDivide,
            mitAnd, mitXOr:
               AddParentheses(Right);
            mitPow:
               If (PowerAsOperand) Then
                  AddParentheses(Right);
            mitRoot:
               If (RootAsOperand) Then
                  AddParentheses(Right);
         End;

         Op := OrStr;
      End;
      mitXOr:
         If (XOrAsFunc) Then Begin
            Result := XOrStr+LeftBracket+Left+', '+Right+RightBracket;

            Exit;
         End Else Begin
            Case aMathMLEquationBinTree.Left.ItemType Of
               mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitPlus, mitMinus, mitTimes, mitDivide,
               mitAnd, mitOr:
                  AddParentheses(Left);
               mitPow:
                  If (PowerAsOperand) Then
                     AddParentheses(Left);
               mitRoot:
                  If (RootAsOperand) Then
                     AddParentheses(Left);
            End;

            Case aMathMLEquationBinTree.Right.ItemType Of
               mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitPlus, mitMinus, mitTimes, mitDivide,
               mitAnd, mitOr:
                  AddParentheses(Right);
               mitPow:
                  If (PowerAsOperand) Then
                     AddParentheses(Right);
               mitRoot:
                  If (RootAsOperand) Then
                     AddParentheses(Right);
            End;

            Op := XOrStr;
         End;
   End;

   Result := Left+Op+Right;
End;

//==============================================================================

Function TCellMLAPIExportEngine.AddPlus(Const aCellMLComponent: String;
                                        Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Result := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);
End;

//==============================================================================

Function TCellMLAPIExportEngine.AddMinus(Const aCellMLComponent: String;
                                         Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Var
   Left: String;
Begin
   Left := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);

   Case aMathMLEquationBinTree.Left.ItemType Of
      mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
      mitPlus, mitMinus,
      mitAnd, mitOr, mitXOr:
         AddParentheses(Left);
   End;

   Result := MinusStr+Left;
End;

//==============================================================================

Function TCellMLAPIExportEngine.AddPower(Const aCellMLComponent: String;
                                         Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Var
   Left, Right: String;
Begin
   Left  := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);
   Right := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right);

   If (PowerAsOperand) Then Begin
      // Determine whether "Left" and/or "Right" need parentheses around them

      Case aMathMLEquationBinTree.Left.ItemType Of
         mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
         mitTimes, mitDivide,
         mitAnd, mitOr, mitXOr:
            AddParentheses(Left);
         mitPlus, mitMinus:
            If (aMathMLEquationBinTree.Left.Right <> Nil) Then
               AddParentheses(Left);
      End;

      Case aMathMLEquationBinTree.Right.ItemType Of
         mitEqEq, mitNEq, mitLT, mitGT, mitLEq, mitGEq,
         mitTimes, mitDivide, mitPow, mitRoot,
         mitAnd, mitOr, mitXOr:
            AddParentheses(Right);
         mitPlus, mitMinus:
            If (aMathMLEquationBinTree.Right.Right <> Nil) Then
               AddParentheses(Right);
      End;

      Result := Left+PowerStr+Right;
   End Else
      Result := PowerStr+LeftBracket+Left+', '+Right+RightBracket;
End;

//==============================================================================

Function TCellMLAPIExportEngine.AddRoot(Const aCellMLComponent: String;
                                        Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Var
   Left, Right: String;
Begin
   Left  := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left.Left);
   Right := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right);

   If (RootAsOperand) Then Begin
      // Determine whether "Left" and/or "Right" need parentheses around them

      Case aMathMLEquationBinTree.Left.Left.ItemType Of
         mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
         mitTimes, mitPow, mitRoot,
         mitAnd, mitOr, mitXOr:
            AddParentheses(Left);
         mitPlus, mitMinus:
            If (aMathMLEquationBinTree.Left.Left.Right <> Nil) Then
               AddParentheses(Left);
      End;

      Case aMathMLEquationBinTree.Right.ItemType Of
         mitEqEq, mitNEq, mitLT, mitGT, mitLEq, mitGEq,
         mitTimes, mitDivide,
         mitAnd, mitOr, mitXOr:
            AddParentheses(Right);
         mitPlus, mitMinus:
            If (aMathMLEquationBinTree.Right.Right <> Nil) Then
               AddParentheses(Right);
      End;

      Result := Right+PowerStr+LeftBracket+OutputNb(1)+'/'+Left+RightBracket;
   End Else Begin
      // Determine whether "Left" needs parentheses around them

      Case aMathMLEquationBinTree.Left.Left.ItemType Of
         mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
         mitTimes, mitDivide,
         mitAnd, mitOr, mitXOr:
            AddParentheses(Left);
         mitPlus, mitMinus:
            If (aMathMLEquationBinTree.Left.Left.Right <> Nil) Then
               AddParentheses(Left);
      End;

      Result := PowerStr+LeftBracket+Right+', '+OutputNb(1)+'/'+Left+RightBracket;
   End;
End;

//==============================================================================

Function TCellMLAPIExportEngine.AddNot(Const aCellMLComponent: String;
                                       Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Var
   Left: String;
   AddedParentheses: Boolean;
Begin
   Left := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);

   AddedParentheses := False;

   Case aMathMLEquationBinTree.Left.ItemType Of
      mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
      mitPlus, mitMinus, mitTimes, mitDivide,
      mitAnd, mitOr, mitXOr: Begin
         AddParentheses(Left);

         AddedParentheses := True;
      End;
      mitPow:
         If (PowerAsOperand) Then Begin
            AddParentheses(Left);

            AddedParentheses := True;
         End;
      mitRoot:
         If (RootAsOperand) Then Begin
            AddParentheses(Left);

            AddedParentheses := True;
         End;
   End;

   If (AddedParentheses) Then
      Result := NotStr+Left
   Else
      Result := NotStr+NotSpaceStr+Left;
End;

//==============================================================================

Function TCellMLAPIExportEngine.GenerateEquation(Const aCellMLComponent: String;
                                                 Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                 Const aAssignmentEqn: Boolean): String;
Var
   OpType: TMathMLOpType;
   Val: Double;
Begin
   Result := '';   // Since we don't always generate a result (the case with the
                   // "Piecewise" element, for instance)

   // Determine the type of operator we are dealing with

   Case aMathMLEquationBinTree.ItemType Of
      mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
      mitTimes, mitDivide,
      mitAnd, mitOr, mitXOr:
         OpType := motOperand;
      mitEq:
         If (aMathMLEquationBinTree.Right.ItemType = mitPiecewise) Then
            OpType := motOwn
         Else
            OpType := motOperand;
      mitPlus, mitMinus:
         If (aMathMLEquationBinTree.Right <> Nil) Then
            OpType := motOperand
         Else
            OpType := motOwn;
      mitLog:
         If (aMathMLEquationBinTree.Left.ItemType = mitLogBase) Then
            OpType := motOwn
         Else
            OpType := motFunc;
      mitCN, mitCI,
      mitPow, mitRoot,
      mitNot,
      mitDiff,
      mitTrue, mitFalse, mitPI, mitExponentiale:
         OpType := motOwn;
   Else
      OpType := motFunc;
   End;

   // Combine the text with the sub-texts, if any, and this based on the type of
   // the operator

   Case OpType Of
      motOperand:
         Result := AddOperand(aCellMLComponent, aMathMLEquationBinTree, aAssignmentEqn);
      motFunc:
         Result := AddFunc(aCellMLComponent, aMathMLEquationBinTree);
      motOwn:
         Case aMathMLEquationBinTree.ItemType Of
            mitCN:
               Result := OutputNb(aMathMLEquationBinTree.Str);
            mitCI:
               With CellMLModel.MapVariable(aCellMLComponent, aMathMLEquationBinTree.Str, aCellMLComponent, aMathMLEquationBinTree.Str) Do
                  Case State Of
                     vsFree:
                        Result := tStr;
                     vsState:
                        Result := YStart+IntToStr(VarIndex)+YEnd;
                  Else
                     Result := RealName;
                  End;
            mitEq:
               AddPiecewiseEqn(aCellMLComponent, aMathMLEquationBinTree);
            mitDiff:
               Result := dYStart+IntToStr(CellMLModel.MapVariable(aCellMLComponent, aMathMLEquationBinTree.Right.Str, aCellMLComponent, aMathMLEquationBinTree.Right.Str).VarIndex)+dYEnd;
            mitPlus:
               Result := AddPlus(aCellMLComponent, aMathMLEquationBinTree);
            mitMinus:
               Result := AddMinus(aCellMLComponent, aMathMLEquationBinTree);
            mitLog:
               Result := LogStr+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+RightBracket+'/'+LogStr+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left.Left)+RightBracket;
            mitPow: Begin
               If (Not TryStrToFloat(aMathMLEquationBinTree.Right.Str, Val)) Then
                  Val := 0;   // Simply to avoid the "sqr" case...

               If ((Val = 2) And (CompareStr(SqrStr, '') <> 0)) Then
                  Result := SqrStr+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+RightBracket
               Else If (Val = 0.5) Then
                  Result := SqrtStr+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+RightBracket
               Else
                  Result := AddPower(aCellMLComponent, aMathMLEquationBinTree);
            End;
            mitRoot: Begin
               If (aMathMLEquationBinTree.Right = Nil) Then
                  Val := 2
               Else If (Not TryStrToFloat(aMathMLEquationBinTree.Left.Left.Str, Val)) Then
                  Val := 0;   // Simply to avoid the "sqrt" case...

               If (Val = 2) Then Begin
                  If (aMathMLEquationBinTree.Right = Nil) Then
                     Result := SqrtStr+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+RightBracket
                  Else
                     Result := SqrtStr+LeftBracket+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+RightBracket;
               End Else
                  Result := AddRoot(aCellMLComponent, aMathMLEquationBinTree);
            End;
            mitNot:
               Result := AddNot(aCellMLComponent, aMathMLEquationBinTree);
            mitTrue:
               Result := TrueStr;
            mitFalse:
               Result := FalseStr;
            mitPI:
               Result := PIStr;
            mitExponentiale:
               Result := ExponentialeStr;
         End;
   End;
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.MathMLEquation(Const aCellMLComponent: String;
                                                Const aMathMLEquationBinTree: TMathMLCommandBinTree);
Var
   Eqn: String;
Begin
   Eqn := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree, True);

   If (CompareStr(Eqn, '') <> 0) Then Begin
      // Note: the case where "Eqn" is empty is when we have a piecewise
      //       equation

      If ((StatementSep <> UNDEFINED_STATEMENT_SEP) And (Eqn[Length(Eqn)] <> StatementSep)) Then
         // Note: this occurs when we have a condition

         Eqn := Eqn+StatementSep;

      If (PrevEqnWasPiecewiseEqn) Then
         Output(EqnsOutputID);

      Output(EqnsOutputID, Eqn);

      PrevStatementWasEqn    := True;
      PrevEqnWasPiecewiseEqn := False;
   End;
End;

//==============================================================================

Procedure TCellMLAPIExportEngine.GenerateMathMLEquations(Const aTypeOfEquations: TCellMLAPIExportEngineTypeOfEquations);
Var
   Iter: Integer;
Begin
   PrevStatementWasEqn    := False;
   PrevEqnWasPiecewiseEqn := False;

   CellMLModel.GlobalVariableList.Comparator := CellMLModel.GlobalVariableList.Compare;

   Sort(CellMLModel.GlobalVariableList);
   // Note: this is very important, since we need to be able to search for
   //       variables using an alphabetical order, as opposed to a state one...

   For Iter := 0 To CellMLModel.EquationList.Size-1 Do
      With TCellMLModelEquation(CellMLModel.EquationList.At(Iter).VObject) Do
         Case aTypeOfEquations Of
            toeComputeOnce:
               If (ComputeOnce) Then
                  MathMLEquation(Component, EqnBinTree);
            toeCompute:
               If (Not ComputeOnce) Then
                  MathMLEquation(Component, EqnBinTree);
         Else
            // toeAll

            MathMLEquation(Component, EqnBinTree);
         End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

