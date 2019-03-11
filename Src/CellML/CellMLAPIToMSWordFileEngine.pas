//==============================================================================
// Engine class for exporting a CellML API object to a MS Word 2007/2010 file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 13/09/2008
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToMSWordFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   WordXP, OleServer, Variants, CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToMSWordFileEngine = Class(TCellMLAPIExportEngine)
      Private
         // Properties used for internal purposes

         UnderscoreForSub: Boolean;
         GreekSymbols: Boolean;
         DigitGrouping: Boolean;

         FileName: OleVariant;

         WASelection: WordSelection;

         TrueOleVariant, FalseOleVariant: OleVariant;

         FirstEquation: Boolean;

         PiecewiseEqn: String;

         // Methods used for internal purposes

         Procedure InsertHeader(Const aHeader: String);

      Protected
         // Methods used for internal purposes

         Function FormatParamName(Const aParamName: String): String;

         Procedure OutputString(Const aIndex: Integer; Const aString: String = ''); Override;

         Function OutputNb(Const aNbString: String): String; Override;
         Procedure AddPieceOrOtherwise(Const aCellMLComponent: String; Const aLHS: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Override;
         Procedure AddPiecewiseEqn(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree); Override;
         Function AddOperand(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAssignmentEq: Boolean): String; Override;
         Function AddFunc(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;
         Function AddPower(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;
         Function AddRoot(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;

         Function GenerateEquation(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAssignmentEqn: Boolean = False): String; Override;

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
{$IFNDEF COR_RELEASE}
   FastMM4,
{$ENDIF}
   Dialogs, SysUtils, StrUtils, DeCAL, Common, Clipbrd;

//==============================================================================

Const
   MML_START        = '<mml:math xmlns:mml="http://www.w3.org/1998/Math/MathML" xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math">';
   MML_END          = '</mml:math>';
   MROW_START       = '<mml:mrow>';
   MROW_END         = '</mml:mrow>';
   MFRAC_START      = '<mml:mfrac>';
   MFRAC_END        = '</mml:mfrac>';
   MSUP_START       = '<mml:msup>';
   MSUP_END         = '</mml:msup>';
   MSUB_START       = '<mml:msub>';
   MSUB_END         = '</mml:msub>';
   MI_START         = '<mml:mi mathvariant="italic">';
   MI_NORMAL_START  = '<mml:mi mathvariant="normal">';
   MI_END           = '</mml:mi>';
   MN_START         = '<mml:mn>';
   MN_END           = '</mml:mn>';
   MO_START         = '<mml:mo>';
   MO_END           = '</mml:mo>';
   MTEXT_START      = '<mml:mtext>';
   MTEXT_END        = '</mml:mtext>';
   MSQRT_START      = '<mml:msqrt>';
   MSQRT_END        = '</mml:msqrt>';
   MROOT_START      = '<mml:mroot>';
   MROOT_END        = '</mml:mroot>';
   MABS_START       = '<mml:mfenced open="|" close="|" separators="|">';
   MABS_END         = '</mml:mfenced>';
   MFLOOR_START     = '<mml:mfenced open="&#x0230A;" close="&#x0230B;" separators="|">';
   MFLOOR_END       = '</mml:mfenced>';
   MCEIL_START      = '<mml:mfenced open="&#x02308;" close="&#x02309;" separators="|">';
   MCEIL_END        = '</mml:mfenced>';
   MPIECEWISE_START = '<mml:mfenced open="{" close="" separators="|">';
   MPIECEWISE_END   = '</mml:mfenced>';
   MTABLE_START     = '<mml:mtable frame="none" columnlines="none" rowlines="none">';
   MTABLE_END       = '</mml:mtable>';
   MTR_START        = '<mml:mtr>';
   MTR_END          = '</mml:mtr>';
   MTD_START        = '<mml:mtd>';
   MTD_END          = '</mml:mtd>';
   MSPACE           = MO_START+'&#x02061;'+MO_END;

//==============================================================================

Constructor TCellMLAPIToMSWordFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                                Const aFileName: Array Of String;
                                                Const aUnderscoreForSub, aGreekSymbols, aDigitGrouping: Boolean);
Begin
   Inherited Create(aCellMLModel, aFileName, False);

   FileName := aFileName[0];

   UnderscoreForSub := aUnderscoreForSub;
   GreekSymbols     := aGreekSymbols;
   DigitGrouping    := aDigitGrouping;

   TrueOleVariant  := True;
   FalseOleVariant := False;

   StatementSep := UNDEFINED_STATEMENT_SEP;

   LeftBracket  := '<mml:mfenced separators="|">'+MROW_START;
   RightBracket := MROW_END+'</mml:mfenced>';

   EqStr    := MO_START+'='+MO_END;
   EqEqStr  := EqStr;
   NEqStr   := MO_START+'&#x02260;'+MO_END;
   GTStr    := MO_START+'&gt;'+MO_END;
   LTStr    := MO_START+'&lt;'+MO_END;
   GEqStr   := MO_START+'&#x02265;'+MO_END;
   LEqStr   := MO_START+'&#x02264;'+MO_END;
   PlusStr  := MO_START+'+'+MO_END;
   MinusStr := MO_START+'-'+MO_END;
   TimesStr := MO_START+'·'+MO_END;
   AndStr   := MSPACE+MI_NORMAL_START+'and'+MI_END+MSPACE;
   OrStr    := MSPACE+MI_NORMAL_START+'or'+MI_END+MSPACE;

   XOrStr := MSPACE+MI_NORMAL_START+'xor'+MI_END+MSPACE;

   NotStr      := MI_NORMAL_START+'not'+MI_END;
   NotSpaceStr := MSPACE;

   DotZeroStr := '';

   NoEmptyString := True;

   TrueStr  := MI_START+'true'+MI_END;
   FalseStr := MI_START+'false'+MI_END;

   PIStr := MO_START+'&#x003C0;'+MO_END;

   ExponentialeStr := MI_START+'e'+MI_END;
End;

//==============================================================================

Procedure TCellMLAPIToMSWordFileEngine.InsertHeader(Const aHeader: String);
Begin
   WASelection.Font.Italic := FalseOleVariant;   // Just in case!

   WASelection.TypeText(CR+CR);

   WASelection.Paragraphs.Alignment := wdAlignParagraphCenter;

   WASelection.Font.Bold := TrueOleVariant;

   WASelection.TypeText(aHeader+CR);
   // Note: to get the header centered, we need a "CR"!

   WASelection.Font.Bold := FalseOleVariant;

   WASelection.Paragraphs.Alignment := wdAlignParagraphJustify;
End;

//==============================================================================

Procedure TCellMLAPIToMSWordFileEngine.Execute;
Var
   WordDocument: TWordDocument;
   Procedure OutputURL(Const aURL: String; Const aMailAddress: Boolean = False);
   Var
      Address, TextToDisplay: OleVariant;
   Begin
      If (aMailAddress) Then
         Address := 'mailto:'+aURL
      Else
         Address := aURL;

      TextToDisplay := aURL;

      WordDocument.Hyperlinks.Add(WASelection.Range, Address, EmptyParam, EmptyParam, TextToDisplay, EmptyParam);
   End;
   Procedure GenerateContents;
      Procedure OutputMMLStatement(Const aVariable, aInitialValue, aUnits: String;
                                   Const aCR: Boolean);
      Var
         MMLStatement: String;
      Begin
         If (aCR) Then Begin
            WASelection.Font.Italic := FalseOleVariant;   // Just in case!

            WASelection.TypeText(CR);
         End;

         MMLStatement := MML_START+

                         // Insert the variable name

                         FormatParamName(aVariable)+

                         // Insert the initial value

                         EqStr+
                         OutputNb(aInitialValue)+

                         MML_END;

         Clipboard.AsText := MMLStatement;

         WASelection.Paste;

         // Insert the units

         WASelection.TypeText(' ');

         WASelection.Font.Italic := TrueOleVariant;

         WASelection.TypeText('('+aUnits+')');
      End;
   Var
      Iter: Integer;
      PrevState: TCellMLModelVariableState;
   Begin
      WASelection.Paragraphs.Alignment := wdAlignParagraphJustify;

      WASelection.Font.Bold := TrueOleVariant;

      WASelection.TypeText('CellML model: ');

      WASelection.Font.Bold := FalseOleVariant;

      WASelection.TypeText(CellMLModel.Name+CR);

      WASelection.Font.Bold := TrueOleVariant;

      WASelection.TypeText('Date and time: ');

      WASelection.Font.Bold := FalseOleVariant;

      WASelection.TypeText(ReplaceStr(DateTimeToStr(Now), ' ', ' at ')+CR);
      WASelection.TypeText(CR);
{$IFDEF COR_SPECIFIC}
      WASelection.TypeText('Conversion from '+CELLML_VERSION+' to Microsoft Word 2007/2010 was done using '+COR_NAME+' ('+COR_VERSION+')'+CR);
      WASelection.TypeText(COR_FULL_COPYRIGHT+CR);
      OutputURL(COR_URL);
      WASelection.TypeText(' - ');
      OutputURL(COR_EMAIL, True);
      WASelection.TypeText(CR);
{$ENDIF}
      OutputURL(CELLML_URL);

      PrevState := vsUnknown;

      For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
         With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
            Case State Of
               vsState: Begin
                  If (PrevState <= vsFree) Then
                     InsertHeader('State variables');

                  OutputMMLStatement(Variable, InitialValue, Units, PrevState > vsFree);
               End;
               vsConstant: Begin
                  If (PrevState <= vsState) Then
                     InsertHeader('Constants');

                  OutputMMLStatement(Variable, InitialValue, Units, PrevState > vsState);
               End;
            End;

            PrevState := State;
         End;

      // Equations

      GenerateMathMLEquations;
   End;
Var
   WordApplication: TWordApplication;
Begin
   Inherited;

   WordApplication := TWordApplication.Create(Nil);

{$IFNDEF COR_RELEASE}
   RegisterExpectedMemoryLeak(TServerEventDispatch, 1);
   // Note: TWordApplication creates a TServerEventDispatch object which FastMM4
   //       may report as never being released, so...
{$ENDIF}

   Try
      Try
         WordApplication.ConnectKind := ckNewInstance;

         WordApplication.Connect;

         WordApplication.DisplayAlerts := wdAlertsNone;

         WordApplication.Options.CheckSpellingAsYouType := False;   // To speed
         WordApplication.Options.CheckGrammarAsYouType  := False;   // things up
                                                                    // a bit...

         WordApplication.Documents.Add(EmptyParam, EmptyParam, EmptyParam, EmptyParam);

         WordDocument := TWordDocument.Create(Nil);

{$IFNDEF COR_RELEASE}
         RegisterExpectedMemoryLeak(TServerEventDispatch, 1);
         // Note: TWordDocument creates a TServerEventDispatch object which
         //       FastMM4 may report as never being released, so...
{$ENDIF}

         Try
            WordDocument.ConnectKind := ckAttachToInterface;

            WordDocument.ConnectTo(WordApplication.ActiveDocument);

            WASelection := WordApplication.Selection;

            GenerateContents;

            WordDocument.SaveAs(FileName);

            WordDocument.Close;

            WordDocument.Disconnect;
         Finally
            WordDocument.Free;
         End;

         WordApplication.Quit;

         WordApplication.Disconnect;
      Except
         MessageDlg('A problem occurred with Microsoft Word 2007/2010.', mtError, [mbOK], 0);
      End;
   Finally
      WordApplication.Free;
   End;
End;

//==============================================================================

Function TCellMLAPIToMSWordFileEngine.FormatParamName(Const aParamName: String): String;
Const
   CONV_TABLE: Array[0..23, 0..1] Of String = (('alpha',   '&#x003B1;'),
                                               ('beta',    '&#x003B2;'),
                                               ('gamma',   '&#x003B3;'),
                                               ('delta',   '&#x003B4;'),
                                               ('epsilon', '&#x003B5;'),
                                               ('zeta',    '&#x003B6;'),
                                               ('eta',     '&#x003B7;'),
                                               ('theta',   '&#x003B8;'),
                                               ('iota',    '&#x003B9;'),
                                               ('kappa',   '&#x003BA;'),
                                               ('lambda',  '&#x003BB;'),
                                               ('mu',      '&#x003BC;'),
                                               ('nu',      '&#x003BD;'),
                                               ('xi',      '&#x003BE;'),
                                               ('omicron', '&#x003BF;'),
                                               ('pi',      '&#x003C0;'),
                                               ('rho',     '&#x003C1;'),
                                               // &#x003C2; is the final sigma,
                                               // which we don't process...
                                               ('sigma',   '&#x003C3;'),
                                               ('tau',     '&#x003C4;'),
                                               ('upsilon', '&#x003C5;'),
                                               ('phi',     '&#x003C6;'),
                                               ('chi',     '&#x003C7;'),
                                               ('psi',     '&#x003C8;'),
                                               ('omega',   '&#x003C9;'));
   GT: String = '>';
   LT: String = '<';
Var
   ParamName: String;
   UnderscorePos: Integer;
   ResStart, ResEnd: String;
   I: Integer;
Begin
   // Take advantage of underscores?

   If (UnderscoreForSub) Then Begin
      If (Pos('_', aParamName) = 0) Then
         Result := ''
      Else Begin
         ParamName := RemoveDuplicateUnderscores(aParamName);

         ResStart := '';
         ResEnd   := '';

         Repeat
            UnderscorePos := Pos('_', ParamName);

            If (UnderscorePos <> 0) Then Begin
               ResStart := ResStart+MSUB_START+MROW_START+MI_START+Copy(ParamName, 1, UnderscorePos-1)+MI_END+MROW_END+MROW_START;
               ResEnd   := ResEnd+MROW_END+MSUB_END;

               ParamName := Copy(ParamName, UnderscorePos+1, Length(ParamName)-UnderscorePos);
            End;
         Until UnderscorePos = 0;

         Result := ResStart+MI_START+ParamName+MI_END+ResEnd;
      End;
   End Else
      Result := '';

   If (CompareStr(Result, '') = 0) Then
      // No underscore in the parameter name or we don't want to replace them,
      // so...

      Result := MI_START+aParamName+MI_END;

   // Take advantage of Greek characters?

   If (GreekSymbols) Then Begin
      For I := 0 To High(CONV_TABLE) Do
         Result := ReplaceText(Result, GT+CONV_TABLE[I, 0]+LT, GT+CONV_TABLE[I, 1]+LT);

      Result := ReplaceText(Result, GT+'infinity'+LT, GT+'&#x0221E;'+LT);
   End;
End;

//==============================================================================

Procedure TCellMLAPIToMSWordFileEngine.OutputString(Const aIndex: Integer;
                                                    Const aString: String);
Begin
   If (Not FirstEquation) Then Begin
      WASelection.Font.Italic := FalseOleVariant;   // Just in case!

      WASelection.TypeText(CR);
   End;

   Clipboard.AsText := MML_START+aString+MML_END;

   WASelection.Paste;
End;

//==============================================================================

Function TCellMLAPIToMSWordFileEngine.OutputNb(Const aNbString: String): String;
Var
   ExpTagPos: Integer;
Begin
   Result := Inherited OutputNb(aNbString);

   ExpTagPos := Pos(OutputNbExpTag, Result);

   If (ExpTagPos <> 0) Then Begin
      If (DigitGrouping) Then
         Result := MN_START+GroupDigits(Copy(Result, 1, ExpTagPos-1))+MN_END+TimesStr+MSUP_START+MROW_START+MN_START+'10'+MN_END+MROW_END+MROW_START+MN_START+GroupDigits(Copy(Result, ExpTagPos+Length(OutputNbExpTag), Length(Result)-(ExpTagPos+Length(OutputNbExpTag))+1))+MN_END+MROW_END+MSUP_END
      Else
         Result := MN_START+Copy(Result, 1, ExpTagPos-1)+MN_END+TimesStr+MSUP_START+MROW_START+MN_START+'10'+MN_END+MROW_END+MROW_START+MN_START+Copy(Result, ExpTagPos+Length(OutputNbExpTag), Length(Result)-(ExpTagPos+Length(OutputNbExpTag))+1)+MN_END+MROW_END+MSUP_END;
   End Else Begin
      If (DigitGrouping) Then
         Result := GroupDigits(Result);

      Result := MN_START+Result+MN_END;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToMSWordFileEngine.AddPieceOrOtherwise(Const aCellMLComponent: String;
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
            PiecewiseEqn := aLHS+EqStr+MPIECEWISE_START+MROW_START+MTABLE_START;

            AlreadyACase := True;
         End;

         PiecewiseEqn := PiecewiseEqn+MTR_START+MTD_START+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MO_START+',&#x02061;'+MO_END+MI_NORMAL_START+'if'+MI_END+MSPACE+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+MROW_END+MTD_END+MTR_END;
      End;
      mitOtherwise: Begin
         PiecewiseEqn := PiecewiseEqn+MTR_START+MTD_START+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MO_START+',&#x02061;'+MO_END+MI_NORMAL_START+'otherwise'+MI_END+MROW_END+MTD_END+MTR_END;
      End;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToMSWordFileEngine.AddPiecewiseEqn(Const aCellMLComponent: String;
                                                       Const aMathMLEquationBinTree: TMathMLCommandBinTree);
Begin
   AlreadyACase := False;

   AddPieceOrOtherwise(aCellMLComponent, GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left), aMathMLEquationBinTree.Right);

   PiecewiseEqn := PiecewiseEqn+MTABLE_END+MROW_END+MPIECEWISE_END;

   Output(0, PiecewiseEqn);
End;

//==============================================================================

Function TCellMLAPIToMSWordFileEngine.AddOperand(Const aCellMLComponent: String;
                                                 Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                 Const aAssignmentEq: Boolean): String;
Begin
   Result := Inherited AddOperand(aCellMLComponent, aMathMLEquationBinTree, aAssignmentEq);

   Case aMathMLEquationBinTree.ItemType Of
      mitDivide:
         Result := MFRAC_START+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MROW_END+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+MROW_END+MFRAC_END;
   End;
End;

//==============================================================================

Function TCellMLAPIToMSWordFileEngine.AddFunc(Const aCellMLComponent: String;
                                              Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Var
   Left, FuncName: String;
   AddedParentheses: Boolean;
   Space: String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitAbs:
         Result := MABS_START+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MROW_END+MABS_END;
      mitExp:
         Result := MSUP_START+MROW_START+MI_START+ExponentialeStr+MI_END+MROW_END+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MROW_END+MSUP_END;
      mitCeil:
         Result := MCEIL_START+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MROW_END+MCEIL_END;
      mitFloor:
         Result := MFLOOR_START+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MROW_END+MFLOOR_END;
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

         Result := Left+MO_START+'!'+MO_END;
      End;
   Else
      Left := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left);

      AddedParentheses := False;

      // Determine whether "Left" needs parentheses around it

      Case aMathMLEquationBinTree.ItemType Of
         mitPlus:
            Case aMathMLEquationBinTree.Left.ItemType Of
               mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitAnd, mitOr, mitXOr, mitNot: Begin
                  AddParentheses(Left);

                  AddedParentheses := True;
               End;
            End;
         mitMinus:
            Case aMathMLEquationBinTree.Left.ItemType Of
               mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitPlus, mitMinus,
               mitAnd, mitOr, mitXOr, mitNot: Begin
                  AddParentheses(Left);

                  AddedParentheses := True;
               End;
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
            mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH: Begin
               AddParentheses(Left);

               AddedParentheses := True;
            End;
         End;
      End;

      Case aMathMLEquationBinTree.ItemType Of
         mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
         mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH:
            FuncName := Copy(aMathMLEquationBinTree.Str, 1, 1)+'rc'+Copy(aMathMLEquationBinTree.Str, 2, Length(aMathMLEquationBinTree.Str)-1);
      Else
         FuncName := aMathMLEquationBinTree.Str;
      End;

      If (AddedParentheses) Then
         Space := ''
      Else
         Space := MSPACE;

      Result := MROW_START+MROW_START+MI_NORMAL_START+FuncName+MI_END+MROW_END+Space+Left+MROW_END;
   End;
End;

//==============================================================================

Function TCellMLAPIToMSWordFileEngine.AddPower(Const aCellMLComponent: String;
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

   Result := MSUP_START+MROW_START+Left+MROW_END+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+MROW_END+MSUP_END;
End;

//==============================================================================

Function TCellMLAPIToMSWordFileEngine.AddRoot(Const aCellMLComponent: String;
                                              Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Result := MROOT_START+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+MROW_END+MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left.Left)+MROW_END+MROOT_END;
End;

//==============================================================================

Function TCellMLAPIToMSWordFileEngine.GenerateEquation(Const aCellMLComponent: String;
                                                       Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                       Const aAssignmentEqn: Boolean): String;
Var
   Right: String;
   Val: Double;
   AddedParentheses: Boolean;
   Space: String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitCI:
         Result := FormatParamName(aMathMLEquationBinTree.Str);
      mitLog:
         If (aMathMLEquationBinTree.Left.ItemType = mitLogBase) Then Begin
            Result := MSUB_START+MROW_START+MI_NORMAL_START+LogStr+MI_END+MROW_END+
                                 MROW_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left.Left)+MROW_END+MSUB_END;

            Right := GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right);

            // Determine whether "Right" needs parentheses around it

            AddedParentheses := False;

            Case aMathMLEquationBinTree.Right.ItemType Of
               mitEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
               mitPlus, mitMinus,
               mitExp, mitLN, mitLog, mitFact,
               mitAnd, mitOr, mitXOr, mitNot,
               mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot,
               mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH,
               mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
               mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH: Begin
                  AddParentheses(Right);

                  AddedParentheses := True;
               End;
            End;

            If (AddedParentheses) Then
               Space := ''
            Else
               Space := MSPACE;

            Result := MROW_START+Result+Space+Right+MROW_END;
         End Else
            Result := Inherited GenerateEquation(aCellMLComponent, aMathMLEquationBinTree, aAssignmentEqn);
      mitRoot: Begin
         If (aMathMLEquationBinTree.Right = Nil) Then
            Val := 2
         Else If (Not TryStrToFloat(aMathMLEquationBinTree.Left.Left.Str, Val)) Then
            Val := 0;   // Simply to avoid the "sqrt" case...

         If (Val = 2) Then Begin
            If (aMathMLEquationBinTree.Right = Nil) Then
               Result := MSQRT_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+MSQRT_END
            Else
               Result := MSQRT_START+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Right)+MSQRT_END;
         End Else
            Result := AddRoot(aCellMLComponent, aMathMLEquationBinTree);
      End;
      mitDiff:
         Result := MFRAC_START+MROW_START+MI_NORMAL_START+'d'+MI_END+FormatParamName(aMathMLEquationBinTree.Right.Str)+MROW_END+MROW_START+MI_NORMAL_START+'d'+MI_END+FormatParamName(aMathMLEquationBinTree.Left.Str)+MROW_END+MFRAC_END;
   Else
      Result := Inherited GenerateEquation(aCellMLComponent, aMathMLEquationBinTree, aAssignmentEqn);
   End;
End;

//==============================================================================

Procedure TCellMLAPIToMSWordFileEngine.GenerateMathMLEquations;
Var
   Iter, Iter2: Integer;
   Comp: TCellMLComponent;
Begin
   CellMLModel.GlobalVariableList.Comparator := CellMLModel.GlobalVariableList.Compare;

   Sort(CellMLModel.GlobalVariableList);
   // Note: this is very important, since we need to be able to search for
   //       variables using an alphabetical order, as opposed to a state one...

   For Iter := 0 To CellMLModel.ComponentList.Size-1 Do Begin
      Comp := TCellMLComponent(CellMLModel.ComponentList.At(Iter).VObject);

      If (Comp.EquationList.Size <> 0) Then Begin
         InsertHeader(Comp.Name);

         For Iter2 := 0 To Comp.EquationList.Size-1 Do Begin
            FirstEquation := Iter2 = 0;

            With TMathMLEquation(Comp.EquationList.At(Iter2).VObject) Do
               MathMLEquation(Owner.Name, MathMLEquationBinTree);
         End;
      End;
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

