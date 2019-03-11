//==============================================================================
// Engine class for exporting a CellML API object to a TeX file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 25/10/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToTeXFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   StrUtils, CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToTeXFileEngine = Class(TCellMLAPIExportEngine)
      Private
         // Properties used for internal purposes

         UnderscoreForSub: Boolean;
         GreekSymbols: Boolean;
         DigitGrouping: Boolean;

         FontSizeLevel: Integer;

      Protected
         // Methods used for internal purposes

         Function FontSizeLevelStr: String; Inline;

         Function TeXify(Const aString: String): String; Inline;
         Function FormatParamName(Const aParamName: String): String;

         Function OutputNb(Const aNbString: String): String; Override;
         Procedure AddPieceOrOtherwise(Const aCellMLComponent: String; Const aLHS: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Override;
         Procedure AddPiecewiseEqn(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Override;
         Function AddOperand(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAssignmentEq: Boolean): String; Override;
         Function AddFunc(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;
         Function AddPower(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;
         Function AddRoot(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;

         Function GenerateEquation(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAssignmentEqn: Boolean = False): String; Override;

         Procedure MathMLEquation(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Override;

         Procedure GenerateMathMLEquations;

      Public
         // Constructor & Destructor

         Constructor Create(Const aCellMLModel: TCellMLModel; Const aFileName: Array Of String; Const aUnderscoreForSub, aGreekSymbols, aDigitGrouping: Boolean);

         // User's methods

         Procedure Execute; Override;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF COR_SPECIFIC}
   CORCommon,
{$ENDIF}
   SysUtils, DeCAL, Common;

//==============================================================================

Constructor TCellMLAPIToTeXFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                             Const aFileName: Array Of String;
                                             Const aUnderscoreForSub, aGreekSymbols, aDigitGrouping: Boolean);
Begin
   Inherited Create(aCellMLModel, aFileName);

   UnderscoreForSub := aUnderscoreForSub;
   GreekSymbols     := aGreekSymbols;
   DigitGrouping    := aDigitGrouping;

   StatementSep := UNDEFINED_STATEMENT_SEP;

   LeftBracket  := '\left( ';
   RightBracket := ' \right) ';

   EqEqStr  := EqStr;
   NEqStr   := '\ne ';
   GEqStr   := '\ge ';
   LEqStr   := '\le ';
   TimesStr := '\cdot ';
   AndStr   := ' \textrm{and }';
   OrStr    := ' \textrm{or }' ;
   XOrStr   := ' \textrm{xor }';

   NotStr := '\textrm{not }';

   DotZeroStr := '';

   NoEmptyString := True;

   TrueStr  := 'true ';
   FalseStr := 'false ';

   PIStr := '\pi ';

   ExponentialeStr := 'e ';
End;

//==============================================================================

Procedure TCellMLAPIToTeXFileEngine.Execute;
Var
   Iter: Integer;
   PrevState: TCellMLModelVariableState;
   NeedFinishTable: Boolean;
Begin
   Inherited;

   Output(0, '\documentclass{article}');
   Output(0, '\usepackage{longtable}');
   Output(0, '\setlength{\parindent}{0pt}');
   Output(0, '\begin{document}');

   IncIndent(0);

   Output(0, '\begin{center}');

   IncIndent(0);

   Output(0, '\begin{tabular}{@{}l@{}}');

   IncIndent(0);

   Output(0, '\hline\hline');
   Output(0, '\textbf{CellML model:} '+TeXify(CellMLModel.Name)+'\\');
   Output(0, '\textbf{Date and time:} '+ReplaceStr(DateTimeToStr(Now), ' ', ' at ')+'\\');
   Output(0, '\hline');
{$IFDEF COR_SPECIFIC}
   Output(0, 'Conversion from '+CELLML_VERSION+' to \TeX{} was done using '+COR_NAME+' ('+COR_VERSION+')'+'\\');
   Output(0, COR_FULL_COPYRIGHT+'\\');
   Output(0, '\texttt{'+COR_URL+'} - \texttt{'+COR_EMAIL+'}\\');
{$ENDIF}
   Output(0, '\texttt{'+CELLML_URL+'}\\');
   Output(0, '\hline\hline');

   DecIndent(0);

   Output(0, '\end{tabular}');

   PrevState := vsUnknown;

   NeedFinishTable := False;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               If (PrevState <= vsFree) Then Begin
                  Output(0, '\begin{longtable}{lcc}');

                  IncIndent(0);

                  Output(0, '\multicolumn{3}{c}{\emph{State variables}}\\');
                  Output(0, '\hline\hline');
                  Output(0, '\multicolumn{1}{c}{} & \multicolumn{1}{c}{Initial value} & \multicolumn{1}{c}{Unit}\\');
                  Output(0, '\hline');
                  Output(0, '\endfirsthead');
                  Output(0, '\multicolumn{3}{c}{\emph{State variables (continued)}}\\');
                  Output(0, '\hline\hline');
                  Output(0, '\multicolumn{1}{c}{} & \multicolumn{1}{c}{Initial value} & \multicolumn{1}{c}{Unit}\\');
                  Output(0, '\hline');
                  Output(0, '\endhead');
                  Output(0, '\hline');
                  Output(0, '\endfoot');
                  Output(0, '\hline');
                  Output(0, '\endlastfoot');

                  NeedFinishTable := True;
               End;

               Output(0, '$'+FormatParamName(Variable)+'$ & $'+OutputNb(InitialValue)+'$ & '+TeXify(Units)+'\\');
            End;
            vsConstant: Begin
               If (PrevState <= vsState) Then Begin
                  If (NeedFinishTable) Then Begin
                     DecIndent(0);

                     Output(0, '\end{longtable}');
                  End;

                  Output(0, '\begin{longtable}{lcc}');

                  IncIndent(0);

                  Output(0, '\multicolumn{3}{c}{\emph{Constants}}\\');
                  Output(0, '\hline\hline');
                  Output(0, '\multicolumn{1}{c}{} & \multicolumn{1}{c}{Value} & \multicolumn{1}{c}{Unit}\\');
                  Output(0, '\hline');
                  Output(0, '\endfirsthead');
                  Output(0, '\multicolumn{3}{c}{\emph{Constants (continued)}}\\');
                  Output(0, '\hline\hline');
                  Output(0, '\multicolumn{1}{c}{} & \multicolumn{1}{c}{Value} & \multicolumn{1}{c}{Unit}\\');
                  Output(0, '\hline');
                  Output(0, '\endhead');
                  Output(0, '\hline');
                  Output(0, '\endfoot');
                  Output(0, '\hline');
                  Output(0, '\endlastfoot');

                  NeedFinishTable := True;
               End;

               Output(0, '$'+FormatParamName(Variable)+'$ & $'+OutputNb(InitialValue)+'$ & '+TeXify(Units)+'\\');
            End;
         End;

         PrevState := State;
      End;

   If (NeedFinishTable) Then Begin
      DecIndent(0);

      Output(0, '\end{longtable}');
   End;

   // Equations

   GenerateMathMLEquations;

   DecIndent(0);

   Output(0, '\end{center}');

   DecIndent(0);

   Output(0, '\end{document}');
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.FontSizeLevelStr: String;
Begin
   Case FontSizeLevel Of
      0: Result := '\displaystyle ';
      1: Result := '\scriptstyle ';
   Else
      Result := '\scriptscriptstyle ';
   End;
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.TeXify(Const aString: String): String;
Begin
   // Put a backslash in front of the following characters, should they be
   // present...
   // # $ % ^ & _ { } ~ \

   Result := ReplaceStr(
                ReplaceStr(
                   ReplaceStr(
                      ReplaceStr(
                         ReplaceStr(
                            ReplaceStr(
                               ReplaceStr(
                                  ReplaceStr(
                                     ReplaceStr(
                                        ReplaceStr(aString, '\', '\\'),
                                                            '#', '\#'),
                                                            '$', '\$'),
                                                            '%', '\%'),
                                                            '^', '\^'),
                                                            '&', '\&'),
                                                            '_', '\_'),
                                                            '{', '\{'),
                                                            '}', '\}'),
                                                            '~', '\~');
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.FormatParamName(Const aParamName: String): String;
   Procedure UpdateParamName(Var aParamName: String;
                             Const aSep1, aSep2: String);
   Const
      GREEK_ALPHABET: Array[0..22] Of String = ('alpha', 'beta', 'gamma',
                                                'delta', 'epsilon', 'zeta',
                                                'eta', 'theta', 'iota', 'kappa',
                                                'lambda', 'mu', 'nu', 'xi',
                                                'pi', 'rho', 'sigma', 'tau',
                                                'upsilon', 'phi', 'chi', 'psi',
                                                'omega');
      // Note: there is no 'omicron' in TeX, so...
   Var
      I: Integer;
   Begin
      For I := 0 To High(GREEK_ALPHABET) Do
         aParamName := ReplaceText(aParamName, aSep1+GREEK_ALPHABET[I]+aSep2, aSep1+'\'+GREEK_ALPHABET[I]+aSep2);

      aParamName := ReplaceText(aParamName, aSep1+'omicron'+aSep2, aSep1+'o'+aSep2);
      aParamName := ReplaceText(aParamName, aSep1+'infinity'+aSep2, aSep1+'\infty'+aSep2);
   End;
Var
   ParamName: String;
   UnderscorePos: Integer;
   AddedLeadingUnderscore, AddedTrailingUnderscore: Boolean;
   Underscore: String;
   ResStart, ResEnd: String;
Begin
   // Take advantage of underscores?

   If (UnderscoreForSub) Then Begin
      ParamName := RemoveDuplicateUnderscores(aParamName);

      ResStart := '';
      ResEnd   := '';

      Repeat
         UnderscorePos := Pos('_', ParamName);

         If (UnderscorePos <> 0) Then Begin
            ResStart := ResStart+Copy(ParamName, 1, UnderscorePos)+'{';
            ResEnd   := ResEnd+'}';

            ParamName := Copy(ParamName, UnderscorePos+1, Length(ParamName)-UnderscorePos);
         End;
      Until UnderscorePos = 0;

      Result := ResStart+ParamName+ResEnd;
   End Else
      Result := ReplaceStr(aParamName, '_', '\_');

   // Take advantage of Greek characters?

   If (GreekSymbols) Then Begin
      If (UnderscoreForSub) Then
         Underscore := '_{'
      Else
         Underscore := '\_';

      // Add a leading/trailing underscore, if necessary (useful for the
      // algorithm below)

      If (CompareStr(Copy(Result, 1, Length(Underscore)), Underscore) <> 0) Then Begin
         Result := Underscore+Result;

         AddedLeadingUnderscore := True;
      End Else
         AddedLeadingUnderscore := False;

      AddedTrailingUnderscore := False;

      If (Not UnderscoreForSub) Then
         If (CompareStr(Copy(Result, Length(Result)-Length(Underscore)+1, Length(Underscore)), Underscore) <> 0) Then Begin
            Result := Result+Underscore;

            AddedTrailingUnderscore := True;
         End;

      UpdateParamName(Result, Underscore, Underscore);

      // Special case of when we use underscores for subscripts. We can have
      // either "_{xxx}" or "_{xxx_{yyy...}}". The latter case has just been
      // dealt with above, but not the former, so...

      If (UnderscoreForSub) Then
         UpdateParamName(Result, Underscore, '}');

      If (AddedLeadingUnderscore) Then
         Result := Copy(Result, Length(Underscore)+1, Length(Result)-Length(Underscore));

      If (AddedTrailingUnderscore) Then
         Result := Copy(Result, 1, Length(Result)-Length(Underscore));
   End;
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.OutputNb(Const aNbString: String): String;
Var
   ExpTagPos: Integer;
Begin
   Result := Inherited OutputNb(aNbString);

   ExpTagPos := Pos(OutputNbExpTag, Result);

   If (ExpTagPos <> 0) Then Begin
      If (DigitGrouping) Then
         Result := GroupDigits(Copy(Result, 1, ExpTagPos-1))+TimesStr+'10^{'+GroupDigits(Copy(Result, ExpTagPos+Length(OutputNbExpTag), Length(Result)-(ExpTagPos+Length(OutputNbExpTag))+1))+'}'
      Else
         Result := Copy(Result, 1, ExpTagPos-1)+TimesStr+'10^{'+Copy(Result, ExpTagPos+Length(OutputNbExpTag), Length(Result)-(ExpTagPos+Length(OutputNbExpTag))+1)+'}';
   End Else If (DigitGrouping) Then
      Result := GroupDigits(Result);
End;

//==============================================================================

Procedure TCellMLAPIToTeXFileEngine.AddPieceOrOtherwise(Const aCellMLComponent: String;
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
            Output(0, aLHS+EqStr+'\left\{');

            IncIndent(0);

            Output(0, '\begin{array}{ll}');

            IncIndent(0);

            AlreadyACase := True;
         End Else
            Output(0, '\\');

         Output(0, GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+' & \textrm{if }'+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right));
      End;
      mitOtherwise: Begin
         Output(0, '\\');

         Output(0, GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+' & \textrm{otherwise}');
      End;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToTeXFileEngine.AddPiecewiseEqn(Const aCellMLComponent: String;
                                                    Const aMathMLEquationBinTree: TMathMLCommandBinTree);
Begin
   AlreadyACase := False;

   AddPieceOrOtherwise(aCellMLComponent, GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left), aMathMLEquationBinTree.Right);

   Output(0, '\\');

   DecIndent(0);

   Output(0, '\end{array}');

   DecIndent(0);

   Output(0, '\right.');
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.AddOperand(Const aCellMLComponent: String;
                                              Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                              Const aAssignmentEq: Boolean): String;
Begin
   Result := Inherited AddOperand(aCellMLComponent, aMathMLEquationBinTree, aAssignmentEq);

   Case aMathMLEquationBinTree.ItemType Of
      mitDivide: 
         Result := '\frac{'+FontSizeLevelStr+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+'}{'+FontSizeLevelStr+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+'}';
   End;
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.AddFunc(Const aCellMLComponent: String;
                                           Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Var
   Left: String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitAbs:
         Result := '|'+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+'|';
      mitExp: Begin
         Result := ' e^';

         Inc(FontSizeLevel);

         Result := Result+'{'+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+'}';

         Dec(FontSizeLevel);
      End;
      mitCeil:
         Result := '\lceil '+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+'\rceil ';
      mitFloor:
         Result := '\lfloor '+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+'\rfloor ';
      mitFact: Begin
         Left := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);

         // Determine whether "Left" needs parentheses around it

         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus, mitTimes, mitPow, mitRoot,
            mitExp, mitLN, mitLog,
            mitAnd, mitOr, mitXOr, mitNot,
            mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot,
            mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH,
            mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
            mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH:
               AddParentheses(Left);
         End;

         Result := Left+'!';
      End;
   Else
      Left := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);

      // Determine whether "Left" needs parentheses around it

      Case aMathMLEquationBinTree.ItemType Of
         mitPlus:
            Case aMathMLEquationBinTree.Left.ItemType Of
               mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitAnd, mitOr, mitXOr, mitNot:
                  AddParentheses(Left);
            End;
         mitMinus:
            Case aMathMLEquationBinTree.Left.ItemType Of
               mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitPlus, mitMinus,
               mitAnd, mitOr, mitXOr, mitNot:
                  AddParentheses(Left);
            End;
      Else
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus,
            mitExp, mitLN, mitLog, mitFact,
            mitAnd, mitOr, mitXOr, mitNot,
            mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot,
            mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH,
            mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
            mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH:
               AddParentheses(Left);
         End;
      End;

      Case aMathMLEquationBinTree.ItemType Of
         mitSecH, mitCscH:
            Result := '\textrm{'+aMathMLEquationBinTree.Str+'} '+Left;
         mitASin, mitACos, mitATan:
            Result := '\'+Copy(aMathMLEquationBinTree.Str, 1, 1)+'rc'+Copy(aMathMLEquationBinTree.Str, 2, Length(aMathMLEquationBinTree.Str)-1)+' '+Left;
         mitASec, mitACsc, mitACot,
         mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH:
            Result := '\textrm{'+Copy(aMathMLEquationBinTree.Str, 1, 1)+'rc'+Copy(aMathMLEquationBinTree.Str, 2, Length(aMathMLEquationBinTree.Str)-1)+'} '+Left;
      Else
         Result := '\'+aMathMLEquationBinTree.Str+' '+Left;
      End;
   End;
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.AddPower(Const aCellMLComponent: String;
                                            Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Var
   Left: String;
Begin
   Left := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);

   // Determine whether "Left" and/or "Right" need parentheses around them

   Case aMathMLEquationBinTree.Left.ItemType Of
      mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
      mitTimes, mitDivide, mitPow, mitRoot,
      mitExp, mitLN, mitLog, mitFact,
      mitAnd, mitOr, mitXOr, mitNot,
      mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot,
      mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH,
      mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
      mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH:
         AddParentheses(Left);
      mitPlus, mitMinus:
         If (aMathMLEquationBinTree.Left.Right <> Nil) Then
            AddParentheses(Left);
   End;

   Result := Left+'^{';

   Inc(FontSizeLevel);

   Result := Result+FontSizeLevelStr+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+'}';

   Dec(FontSizeLevel);
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.AddRoot(Const aCellMLComponent: String;
                                           Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Inc(FontSizeLevel);

   Result := '\sqrt['+FontSizeLevelStr+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left.Left);

   Dec(FontSizeLevel);

   Result := Result+']{'+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+'}';
End;

//==============================================================================

Function TCellMLAPIToTeXFileEngine.GenerateEquation(Const aCellMLComponent: String;
                                                    Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                    Const aAssignmentEqn: Boolean): String;
Var
   Right: String;
   Val: Double;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitCI:
         Result := FormatParamName(aMathMLEquationBinTree.Str);
      mitLog:
         If (aMathMLEquationBinTree.Left.ItemType = mitLogBase) Then Begin
            Inc(FontSizeLevel);

            Result := '\log_{'+FontSizeLevelStr+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left.Left);

            Dec(FontSizeLevel);

            Right := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right);

            // Determine whether "Right" needs parentheses around it

            Case aMathMLEquationBinTree.Right.ItemType Of
               mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitPlus, mitMinus,
               mitExp, mitLN, mitLog, mitFact,
               mitAnd, mitOr, mitXOr, mitNot,
               mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot,
               mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH,
               mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
               mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH:
                  AddParentheses(Right);
            End;

            Result := Result+'}{'+Right+'}'
         End Else
            Result := Inherited GenerateEquation(aCellMLComponent, aMathMLEquationBinTree, aAssignmentEqn);
      mitRoot: Begin
         If (aMathMLEquationBinTree.Right = Nil) Then
            Val := 2
         Else If (Not TryStrToFloat(aMathMLEquationBinTree.Left.Left.Str, Val)) Then
            Val := 0;   // Simply to avoid the "sqrt" case...

         If (Val = 2) Then Begin
            If (aMathMLEquationBinTree.Right = Nil) Then
               Result := '\sqrt{'+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+'}'
            Else
               Result := '\sqrt{'+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+'}';
         End Else
            Result := AddRoot(aCellMLComponent, aMathMLEquationBinTree);
      End;
      mitDiff:
         Result := '\frac{'+FontSizeLevelStr+'\textrm{d}'+FormatParamName(aMathMLEquationBinTree.Right.Str)+'}{'+FontSizeLevelStr+'\textrm{d}'+FormatParamName(aMathMLEquationBinTree.Left.Str)+'}';
   Else
      Result := Inherited GenerateEquation(aCellMLComponent, aMathMLEquationBinTree, aAssignmentEqn);
   End;
End;

//==============================================================================

Procedure TCellMLAPIToTeXFileEngine.MathMLEquation(Const aCellMLComponent: String;
                                                   Const aMathMLEquationBinTree: TMathMLCommandBinTree);
Begin
   Output(0, '$');

   IncIndent(0);

   Inherited;

   DecIndent(0);

   Output(0, '$');
End;

//==============================================================================

Procedure TCellMLAPIToTeXFileEngine.GenerateMathMLEquations;
Var
   Iter, Iter2: Integer;
   Comp: TCellMLComponent;
   FirstEqn: Boolean;
Begin
   CellMLModel.GlobalVariableList.Comparator := CellMLModel.GlobalVariableList.Compare;

   Sort(CellMLModel.GlobalVariableList);
   // Note: this is very important, since we need to be able to search for
   //       variables using an alphabetical order, as opposed to a state one...

   For Iter := 0 To CellMLModel.ComponentList.Size-1 Do Begin
      Comp := TCellMLComponent(CellMLModel.ComponentList.At(Iter).VObject);

      If (Comp.EquationList.Size <> 0) Then Begin
         Output(0, '\begin{longtable}{c}');

         IncIndent(0);

         Output(0, '\multicolumn{1}{c}{\emph{'+TeXify(Comp.Name)+'}}\\');
         Output(0, '\hline\hline');
         Output(0, '\endfirsthead');
         Output(0, '\multicolumn{1}{c}{\emph{'+TeXify(Comp.Name)+' (continued)}}\\');
         Output(0, '\hline');
         Output(0, '\endhead');
         Output(0, '\hline');
         Output(0, '\endfoot');
         Output(0, '\hline');
         Output(0, '\endlastfoot');

         FirstEqn := True;

         For Iter2 := 0 To Comp.EquationList.Size-1 Do Begin
            If (FirstEqn) Then
               FirstEqn := False
            Else
               Output(0, '\\');

            With TMathMLEquation(Comp.EquationList.At(Iter2).VObject) Do
               MathMLEquation(Owner.Name, MathMLEquationBinTree);
         End;

         DecIndent(0);

         Output(0, '\end{longtable}');
      End;
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

