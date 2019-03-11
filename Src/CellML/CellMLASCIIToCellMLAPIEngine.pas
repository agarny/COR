//==============================================================================
// Engine class for converting a CellML ASCII form to a CellML API object
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 04/08/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLASCIIToCellMLAPIEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLScannerEngine, Common, DeCAL;

//==============================================================================

Type
   TParseASCIIEquationExprFunc = Function(Var aNode: TMathMLCommandBinTree): Boolean Of Object;
   TCellMLASCIIToCellMLAPIEngine = Class
      Private
         // Properties used for internal purposes

         FileName: String;

         TextOrCmd: String;

         CellMLModel: TCellMLModel;

         MathMLCommandBinTree: PMathMLCommandBinTree;

         CellMLScannerEngine: TCellMLScannerEngine;

         Messages: DArray;

         // Methods used for internal purposes

         Procedure AddMsg(Const aToken: TCellMLScannerEngineToken; Const aMsg: String; Const aTypicalMsg: Boolean = True); 

         Function DefToken: Boolean;
         Function IdentifierToken: Boolean;
         Function ColonToken: Boolean;
         Function NumberToken(Const aAddMsg: Boolean = True): Boolean;
         Function NumberValueToken(Const aAddMsg: Boolean = True): Boolean;
         Function PrefValueToken: Boolean;
         Function PubPrivInterfaceValueToken: Boolean;
         Function AsToken: Boolean;
         Function ForToken: Boolean;
         Function AndToken: Boolean;
         Function CommaToken: Boolean;
         Function OpeningBracketToken: Boolean;
         Function ClosingBracketToken: Boolean;
         Function OpeningCurlyBracketToken: Boolean;
         Function ClosingCurlyBracketToken: Boolean;
         Function EndDefPlusSemiColonToken: Boolean;
         Function SemiColonToken: Boolean;

         Function ParseASCIIModel: Boolean;
         Function ParseASCIIUnits(aCellMLComponent: TCellMLComponent; Var aNode: TMathMLCommandBinTree; Const aConvertToEqn: Boolean; Var aBaseUnit: Boolean): Boolean;
         Function ParseASCIIUnit(aCellMLUnits: TCellMLUnits; Var aNode: TMathMLCommandBinTree): Boolean;
         Function ParseASCIIVarDecl(aCellMLComponent: TCellMLComponent; Var aNode: TMathMLCommandBinTree): Boolean;
         Function ParseASCIIComponent(aCellMLComponent: TCellMLComponent): Boolean;
         Procedure UpdateMathMLCommandBinTree(Var aNode: TMathMLCommandBinTree; Const aItemType: TMathMLCommandBinTreeItemType; aLeft, aRight: TMathMLCommandBinTree; Const aLine: Integer = UNDEFINED);
         Procedure IdentifierNode(Var aNode: TMathMLCommandBinTree); Inline;
         Function DerivativeNode(Var aNode: TMathMLCommandBinTree): Boolean;
         Function ParseASCIIEquation(Var aNode: TMathMLCommandBinTree): Boolean;
         Function GenericParseASCIIEquationExpr(Var aNode: TMathMLCommandBinTree; aSymbolSet: TCellMLScannerEngineSymbolSet; aParseASCIIEquationExprFunc: TParseASCIIEquationExprFunc): Boolean;
         Function ParseASCIIEquationExpr01(Var aNode: TMathMLCommandBinTree): Boolean; Inline;
         Function ParseASCIIEquationExpr02(Var aNode: TMathMLCommandBinTree): Boolean; Inline;
         Function ParseASCIIEquationExpr03(Var aNode: TMathMLCommandBinTree): Boolean; Inline;
         Function ParseASCIIEquationExpr04(Var aNode: TMathMLCommandBinTree): Boolean; Inline;
         Function ParseASCIIEquationExpr05(Var aNode: TMathMLCommandBinTree): Boolean; Inline;
         Function ParseASCIIEquationExpr06(Var aNode: TMathMLCommandBinTree): Boolean; Inline;
         Function ParseASCIIEquationExpr07(Var aNode: TMathMLCommandBinTree): Boolean; Inline;
         Function ParseASCIIEquationExpr08(Var aNode: TMathMLCommandBinTree): Boolean;
         Function ParseASCIIEquationExpr09(Var aNode: TMathMLCommandBinTree): Boolean;
         Function ParseASCIIGroup(aCellMLGroup: TCellMLGroup): Boolean;
         Function ParseASCIIComponentRef(aCellMLComponentRef: TCellMLComponentRef): Boolean;
         Function ParseASCIIConnection(aCellMLConnection: TCellMLConnection): Boolean;

      Public
         // Constructor & Destructor

         Constructor Create(Const aFileName, aText: String; Const aCellMLModel: TCellMLModel; Const aMessages: DArray); Overload;
         Constructor Create(Const aCmd: String; Var aMathMLCommandBinTree: TMathMLCommandBinTree); Overload;

         // User's methods

         Function Execute: Boolean;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   SysUtils, Dialogs, Engine;

//==============================================================================

Constructor TCellMLASCIIToCellMLAPIEngine.Create(Const aFileName, aText: String;
                                                 Const aCellMLModel: TCellMLModel;
                                                 Const aMessages: DArray);
Begin
   FileName := aFileName;

   TextOrCmd := aText;                                         

   CellMLModel := aCellMLModel;

   Messages := aMessages;
End;

//==============================================================================

Constructor TCellMLASCIIToCellMLAPIEngine.Create(Const aCmd: String;
                                                 Var aMathMLCommandBinTree: TMathMLCommandBinTree);
Begin
   Inherited Create;

   FileName := '';

   TextOrCmd := aCmd;

   MathMLCommandBinTree := @aMathMLCommandBinTree;

   Messages := Nil;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.Execute: Boolean;
Var
   BaseUnit: Boolean;
Begin
   Result := True;

   CellMLScannerEngine := TCellMLScannerEngine.Create(TextOrCmd);

   If (CompareStr(FileName, '') <> 0) Then Begin
      // Want to convert a whole file

      // "def"

      If (DefToken) Then Begin
         CellMLScannerEngine.GetNextToken;

         // "model"

         If (CellMLScannerEngine.Token.Sym <> sModel) Then Begin
            AddMsg(CellMLScannerEngine.Token, '''model''');

            Result := False;
         End Else Begin
            CellMLScannerEngine.GetNextToken;

            // Name of the model

            If (IdentifierToken) Then Begin
               CellMLModel.Name := CellMLScannerEngine.Token.Str;

               CellMLScannerEngine.GetNextToken;

               // "as"

               If (AsToken) Then Begin
                  CellMLScannerEngine.GetNextToken;

                  // Model definition itself

                  If (CellMLScannerEngine.Token.Sym <> sEndDef) Then
                     Repeat
                        Result := ParseASCIIModel;
                     Until Not Result Or (CellMLScannerEngine.Token.Sym <> sDef);

                  // "enddef;"

                  If (Result And EndDefPlusSemiColonToken) Then Begin
                     CellMLScannerEngine.GetNextToken;

                     // Look for what should be the end of the file...

                     If (CellMLScannerEngine.Token.Sym <> sEOF) Then Begin
                        AddMsg(CellMLScannerEngine.Token, 'end of file');

                        Result := False;
                     End;
                  End Else
                     Result := False;   // In case Result was true, but didn't
                                        // find "enddef;"
               End Else
                  Result := False;
            End Else
               Result := False;
         End;
      End Else
         Result := False;
   End Else Begin
      // Want to convert a unit definition, a variable declaration or an
      // equation

      Case CellMLScannerEngine.Token.Sym Of
         sDef: Begin
            CellMLScannerEngine.GetNextToken;

            If (CellMLScannerEngine.Token.Sym = sUnit) Then Begin
               If (ParseASCIIUnits(Nil, MathMLCommandBinTree^, True, BaseUnit)) Then Begin
                  // Look for "enddef;", since "ParseASCIIUnits" doesn't, due to
                  // the fact that it's embedded in a function that does it for
                  // it, as well as for other functions that deal with other
                  // types of definitions (see "ParseASCIIModel")...

                  If ((BaseUnit And Not SemiColonToken) Or
                      (Not BaseUnit And Not EndDefPlusSemiColonToken)) Then Begin
                     Result := False;

                     MathMLCommandBinTree.Free;
                  End;
               End Else
                  Result := False;
            End Else
               Result := False;
         End;
         sVar:
            If (Not ParseASCIIVarDecl(Nil, MathMLCommandBinTree^)) Then
               Result := False;
      Else
         // Not a unit/variable declaration, so it must be an equation...

         If (Not ParseASCIIEquation(MathMLCommandBinTree^)) Then
            Result := False;
      End;
   End;

   CellMLScannerEngine.Free;
End;

//==============================================================================

Procedure TCellMLASCIIToCellMLAPIEngine.AddMsg(Const aToken: TCellMLScannerEngineToken;
                                               Const aMsg: String;
                                               Const aTypicalMsg: Boolean);
Begin
   If (Messages <> Nil) Then Begin
      If (aTypicalMsg) Then
         Messages.Add([TEngineMsg.Create(mtError, FileName, aToken.Line, aToken.Col, aMsg+' is expected, but '''+CellMLScannerEngine.Token.Str+''' was found instead')])
      Else
         Messages.Add([TEngineMsg.Create(mtError, FileName, aToken.Line, aToken.Col, aMsg)]);
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.DefToken: Boolean;
Begin
   // "def"

   If (CellMLScannerEngine.Token.Sym <> sDef) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''def''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.IdentifierToken: Boolean;
Begin
   // Identifier

   If (CellMLScannerEngine.Token.Sym <> sIdentifier) Then Begin
      AddMsg(CellMLScannerEngine.Token, 'an identifier');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ColonToken: Boolean;
Begin
   // ";"

   If (CellMLScannerEngine.Token.Sym <> sColon) Then Begin
      AddMsg(CellMLScannerEngine.Token, ''':''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.NumberToken(Const aAddMsg: Boolean): Boolean;
Begin
   // Number

   If (Not (CellMLScannerEngine.Token.Sym In [sNumber..sTooBigANumber])) Then Begin
      If (aAddMsg) Then
         AddMsg(CellMLScannerEngine.Token, 'a number');

      Result := False;
   End Else If (CellMLScannerEngine.Token.Sym = sTooBigANumber) Then Begin
      If (aAddMsg) Then
         AddMsg(CellMLScannerEngine.Token, 'the number is too big', False);

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.NumberValueToken(Const aAddMsg: Boolean): Boolean;
Var
   Sign: ShortInt;
Begin
   // Negative number?

   Sign := 0;

   If (CellMLScannerEngine.Token.Sym = sPlus) Then Begin
      Sign := 1;

      CellMLScannerEngine.GetNextToken;
   End Else If (CellMLScannerEngine.Token.Sym = sMinus) Then Begin
      Sign := -1;

      CellMLScannerEngine.GetNextToken;
   End;

   Result := NumberToken(aAddMsg);

   If (Result) Then Begin
      If (Sign = 1) Then
         CellMLScannerEngine.Token.Str := '+'+CellMLScannerEngine.Token.Str
      Else If (Sign = -1) Then
         CellMLScannerEngine.Token.Str := '-'+CellMLScannerEngine.Token.Str;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.PrefValueToken: Boolean;
Begin
   // "yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo", "hecto",
   // "deka", "deci", "centi", "milli", "micro", "nano", "pico", "femto",
   // "atto", "zepto", "yocto"

   If (Not (CellMLScannerEngine.Token.Sym In [sYotta..sYocto])) Then Begin
      AddMsg(CellMLScannerEngine.Token, 'either a number, ''yotta'', ' +
                                        '''zetta'', ''exa'', ''peta'', ' +
                                        '''tera'', ''giga'', ''mega'', ' +
                                        '''kilo'', ''hecto'', ''deka'', ' +
                                        '''deci'', ''centi'', ''milli'', ' +
                                        '''micro'', ''nano'', ''pico'', ' +
                                        '''femto'', ''atto'', ''zepto'' or ' +
                                        '''yocto''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.PubPrivInterfaceValueToken: Boolean;
Begin
   // "none", "in" or "out"

   If (Not (CellMLScannerEngine.Token.Sym In [sNone..sOut])) Then Begin
      AddMsg(CellMLScannerEngine.Token, 'either ''none'', ''in'' or ''out''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.AsToken: Boolean;
Begin
   // "as"

   If (CellMLScannerEngine.Token.Sym <> sAs) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''as''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ForToken: Boolean;
Begin
   // "for"

   If (CellMLScannerEngine.Token.Sym <> sFor) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''for''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.AndToken: Boolean;
Begin
   // "and"

   If (CellMLScannerEngine.Token.Sym <> sAnd) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''and''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.CommaToken: Boolean;
Begin
   // ","

   If (CellMLScannerEngine.Token.Sym <> sComma) Then Begin
      AddMsg(CellMLScannerEngine.Token, ''',''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.OpeningBracketToken: Boolean;
Begin
   // "("

   If (CellMLScannerEngine.Token.Sym <> sOpeningBracket) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''(''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ClosingBracketToken: Boolean;
Begin
   // ")"

   If (CellMLScannerEngine.Token.Sym <> sClosingBracket) Then Begin
      AddMsg(CellMLScannerEngine.Token, ''')''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.OpeningCurlyBracketToken: Boolean;
Begin
   // "{"

   If (CellMLScannerEngine.Token.Sym <> sOpeningCurlyBracket) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''{''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ClosingCurlyBracketToken: Boolean;
Begin
   // "}"

   If (CellMLScannerEngine.Token.Sym <> sClosingCurlyBracket) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''}''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.EndDefPlusSemiColonToken: Boolean;
Begin
   // "enddef"

   If (CellMLScannerEngine.Token.Sym <> sEndDef) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''enddef''');

      Result := False;
   End Else Begin
      CellMLScannerEngine.GetNextToken;

      // ";"

      Result := SemiColonToken;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.SemiColonToken: Boolean;
Begin
   // ";"

   If (CellMLScannerEngine.Token.Sym <> sSemiColon) Then Begin
      AddMsg(CellMLScannerEngine.Token, ''';''');

      Result := False;
   End Else
      Result := True;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIModel: Boolean;
Var
   DummyMathMLCommandBinTree: TMathMLCommandBinTree;
   CellMLComponent: TCellMLComponent;
   CellMLGroup: TCellMLGroup;
   CellMLConnection: TCellMLConnection;
   BaseUnit: Boolean;
Begin
   If (CellMLScannerEngine.Token.Sym = sDef) Then Begin
      CellMLScannerEngine.GetNextToken;

      // Either a "units", "component", "group" or "connection" definition

      Case CellMLScannerEngine.Token.Sym Of
         sUnit:
            Result := ParseASCIIUnits(Nil, DummyMathMLCommandBinTree, False, BaseUnit);
         sComp: Begin
            CellMLComponent := TCellMLComponent.Create(CellMLModel);

            CellMLComponent.Name := CellMLScannerEngine.Token.Str;
            CellMLComponent.Line := CellMLScannerEngine.Token.Line;

            CellMLScannerEngine.GetNextToken;

            Result := ParseASCIIComponent(CellMLComponent);

            If (Result) Then
               CellMLModel.ComponentList.Add([CellMLComponent])
            Else
               CellMLComponent.Free;
         End;
         sGroup: Begin
            CellMLGroup := TCellMLGroup.Create(CellMLModel);

            CellMLGroup.Line := CellMLScannerEngine.Token.Line;

            CellMLScannerEngine.GetNextToken;

            Result := ParseASCIIGroup(CellMLGroup);

            If (Result) Then
               CellMLModel.GroupList.Add([CellMLGroup])
            Else
               CellMLGroup.Free;
         End;
         sMap: Begin
            CellMLConnection := TCellMLConnection.Create(CellMLModel);

            CellMLConnection.Line := CellMLScannerEngine.Token.Line;

            CellMLScannerEngine.GetNextToken;

            Result := ParseASCIIConnection(CellMLConnection);

            If (Result) Then
               CellMLModel.ConnectionList.Add([CellMLConnection])
            Else
               CellMLConnection.Free;
         End;
      Else
         AddMsg(CellMLScannerEngine.Token, 'either a ''unit'', ''component'', ''group'' or ''connection'' definiton');

         Result := False;
      End;

      If (Result) Then Begin
         // "enddef;"

         Result := (BaseUnit And SemiColonToken) Or (Not BaseUnit And EndDefPlusSemiColonToken);

         If (Result) Then
            CellMLScannerEngine.GetNextToken;
      End;
   End Else Begin
      // We were expecting "def", but got something else, so...

      AddMsg(CellMLScannerEngine.Token, '''def''');

      Result := False;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIUnits(aCellMLComponent: TCellMLComponent;
                                                       Var aNode: TMathMLCommandBinTree;
                                                       Const aConvertToEqn: Boolean;
                                                       Var aBaseUnit: Boolean): Boolean;
Var
   CellMLUnits: TCellMLUnits;
   Left, Right, SubRight: TMathMLCommandBinTree;
Begin
   // One of the top parsing functions, so need to initialise "aNode", just in
   // case something goes wrong in the parsing...

   aNode := Nil;

   aBaseUnit := False;   // Just in case...

   // Unit declaration

   CellMLScannerEngine.GetNextToken;

   // Name of the units

   Result := IdentifierToken;

   If (Result) Then Begin
      Left  := Nil;
      Right := Nil;

      If (aConvertToEqn) Then Begin
         CellMLUnits := Nil;

         Left := TMathMLCommandBinTree.Create(FileName, mitUnit, CellMLScannerEngine.Token.Str);
      End Else Begin
         If (aCellMLComponent <> Nil) Then
            CellMLUnits := TCellMLUnits.Create(Nil, aCellMLComponent, CellMLScannerEngine.Token.Str)
         Else
            CellMLUnits := TCellMLUnits.Create(CellMLModel, Nil, CellMLScannerEngine.Token.Str);

         CellMLUnits.Line := CellMLScannerEngine.Token.Line;
      End;

      CellMLScannerEngine.GetNextToken;

      // "as"

      If (CellMLScannerEngine.Token.Sym = sAs) Then Begin
         CellMLScannerEngine.GetNextToken;

         // "base"

         If (CellMLScannerEngine.Token.Sym = sBase) Then Begin
            CellMLScannerEngine.GetNextToken;

            // "unit"

            If (CellMLScannerEngine.Token.Sym <> sUnit) Then Begin
               AddMsg(CellMLScannerEngine.Token, '''unit''');

               Result := False;
            End Else Begin
               aBaseUnit := True;

               If (aConvertToEqn) Then
                  Right := TMathMLCommandBinTree.Create(FileName, 'base unit')
               Else Begin
                  CellMLUnits.BaseUnits := True;

                  CellMLUnits.AddUnitElement([TCellMLUnit.Create(Nil, CellMLUnits.Name, '0', '1', '1', '0')]);
                  // Note: a user-defined base unit requires one unit element to
                  //       make units checking work, so... (see
                  //       "AddStandardUnit" in the "CellMLAPI" unit)
               End;

               CellMLScannerEngine.GetNextToken;
            End;
         End Else Begin
            // List of unit(s) used for the definition of the new unit

            If (aConvertToEqn) Then Begin
               While (Result And (CellMLScannerEngine.Token.Sym = sUnit)) Do
                  If (Right <> Nil) Then Begin
                     Result := ParseASCIIUnit(CellMLUnits, SubRight);

                     If (Result) Then
                        UpdateMathMLCommandBinTree(Right, mitTimes, Right, SubRight);
                  End Else
                     Result := ParseASCIIUnit(CellMLUnits, Right);
            End Else
               While (Result And (CellMLScannerEngine.Token.Sym = sUnit)) Do
                  Result := ParseASCIIUnit(CellMLUnits, Right);
         End;
      End Else Begin
         AddMsg(CellMLScannerEngine.Token, '''as''');

         Result := False;
      End;

      If (aConvertToEqn) Then Begin
         If (Right <> Nil) Then
            UpdateMathMLCommandBinTree(aNode, mitEq, Left, Right)
         Else Begin
            Result := False;

            Left.Free;
         End;
      End Else If (Result) Then Begin
         If (aCellMLComponent <> Nil) Then
            aCellMLComponent.UnitsList.Add([CellMLUnits])
         Else
            CellMLModel.UnitsList.Add([CellMLUnits]);
      End Else Begin
         Left.Free;
         Right.Free;

         CellMLUnits.Free;
      End;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIUnit(aCellMLUnits: TCellMLUnits;
                                                      Var aNode: TMathMLCommandBinTree): Boolean;
Var
   CellMLUnit: TCellMLUnit;
   Sym: TCellMLScannerEngineSymbol;
   DummyDblNbVal: Double;
   Pref, Expo, Mult, Off, ExpoBis, ExpoPref, TenExpo, Left, Right: TMathMLCommandBinTree;
Begin
   // "unit" has been scanned, otherwise we wouldn't be here...

   CellMLScannerEngine.GetNextToken;

   // Name of the unit

   Result := IdentifierToken;

   If (Result) Then Begin
      If (aCellMLUnits <> Nil) Then Begin
         CellMLUnit := TCellMLUnit.Create(aCellMLUnits, CellMLScannerEngine.Token.Str);

         CellMLUnit.Line := CellMLScannerEngine.Token.Line;
      End Else Begin
         CellMLUnit := Nil;

         aNode := TMathMLCommandBinTree.Create(FileName, mitUnit, CellMLScannerEngine.Token.Str);
      End;

      Pref := Nil;
      Expo := Nil;
      Mult := Nil;
      Off  := Nil;

      ExpoBis  := Nil;
      ExpoPref := Nil;
      TenExpo  := Nil;

      CellMLScannerEngine.GetNextToken;

      // "{"?

      If (CellMLScannerEngine.Token.Sym = sOpeningCurlyBracket) Then Begin
         // Start the definition of some options for the unit

         CellMLScannerEngine.GetNextToken;

         While (Result And (CellMLScannerEngine.Token.Sym In [sPref..sOff])) Do Begin
            Sym := CellMLScannerEngine.Token.Sym;

            CellMLScannerEngine.GetNextToken;

            // ":"

            Result := ColonToken;

            If (Result) Then Begin
               CellMLScannerEngine.GetNextToken;

               // Floating value or predefined prefix

               Result := NumberValueToken(Sym <> sPref);

               If (Not Result And (Sym = sPref)) Then
                  // Not a number, so it has to be a predefined prefix

                  Result := PrefValueToken;

               If (Result) Then Begin
                  If (aCellMLUnits <> Nil) Then
                     Case Sym Of
                        sPref: Begin
                           If (TryStrToFloat(CellMLScannerEngine.Token.Str, DummyDblNbVal)) Then
                              CellMLUnit.Prefix := SimplifyNbStr(CellMLScannerEngine.Token.Str)
                           Else
                              CellMLUnit.Prefix := CellMLScannerEngine.Token.Str;
                        End;
                        sExpo:
                           CellMLUnit.Exponent := SimplifyNbStr(CellMLScannerEngine.Token.Str);
                        sMult:
                           CellMLUnit.Multiplier := SimplifyNbStr(CellMLScannerEngine.Token.Str);
                        sOff:
                           CellMLUnit.Offset := SimplifyNbStr(CellMLScannerEngine.Token.Str);
                     End
                  Else Begin
                     Case Sym Of
                        sPref: Begin
                           Left := TMathMLCommandBinTree.Create(FileName, '10');

                           If (TryStrToFloat(CellMLScannerEngine.Token.Str, DummyDblNbVal)) Then
                              Right := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(CellMLScannerEngine.Token.Str))
                           Else
                              Right := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(IntToStr(PrefixVal(CellMLScannerEngine.Token.Str))));

                           UpdateMathMLCommandBinTree(Pref, mitPow, Left, Right);
                        End;
                        sExpo: Begin
                           Expo    := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(CellMLScannerEngine.Token.Str));
                           ExpoBis := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(CellMLScannerEngine.Token.Str));
                        End;
                        sMult:
                           Mult := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(CellMLScannerEngine.Token.Str));
                        sOff:
                           Off := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(CellMLScannerEngine.Token.Str));
                     End;
                  End;

                  CellMLScannerEngine.GetNextToken;

                  // ","?

                  If (CellMLScannerEngine.Token.Sym = sComma) Then Begin
                     CellMLScannerEngine.GetNextToken;

                     // We MUST have a new parameter

                     If (Not (CellMLScannerEngine.Token.Sym In [sPref..sOff])) Then Begin
                        AddMsg(CellMLScannerEngine.Token, 'either a ''pref'', ''expo'', ''mult'' or ''off'' parameter');

                        Result := False;
                     End;
                  End;
               End;
            End;
         End;

         If (Result) Then Begin
            // "}"

            Result := ClosingCurlyBracketToken;

            If (Result) Then
               CellMLScannerEngine.GetNextToken;
         End;
      End;

      If (Result) Then Begin
         // ";"

         Result := SemiColonToken;

         If (Result) Then
            CellMLScannerEngine.GetNextToken;
      End;

      If (aCellMLUnits <> Nil) Then Begin
         If (Result) Then
            aCellMLUnits.AddUnitElement([CellMLUnit])
         Else
            CellMLUnit.Free;
      End Else If (Result) Then Begin
         // Add the prefix, exponent, multiplier and offset information, should
         // they be present. Note the importance of the order in which this is
         // being done!

         If (Expo <> Nil) Then
            UpdateMathMLCommandBinTree(aNode, mitPow, aNode, Expo);

         If (Pref <> Nil) Then Begin
            If (ExpoBis <> Nil) Then Begin
               UpdateMathMLCommandBinTree(ExpoPref, mitPow, Pref, ExpoBis);

               UpdateMathMLCommandBinTree(aNode, mitTimes, aNode, ExpoPref);
            End Else
               UpdateMathMLCommandBinTree(aNode, mitTimes, aNode, Pref);
         End Else If (ExpoBis <> Nil) Then
            // There is no prefix, so no need for the exponent

            ExpoBis.Free;

         If (Mult <> Nil) Then
            UpdateMathMLCommandBinTree(aNode, mitTimes, Mult, aNode);

         If (Off <> Nil) Then Begin
            If (Off.Str[1] = '-') Then Begin
               Off.Str := Copy(Off.Str, 2, Length(Off.Str)-1);

               UpdateMathMLCommandBinTree(aNode, mitMinus, aNode, Off);
               // Note: this avoids having something like "<unit>+-3" and
               //       instead have "<unit>-3"
            End Else
               UpdateMathMLCommandBinTree(aNode, mitPlus, aNode, Off);
         End;
      End Else Begin
         FreeAndNil(aNode);

         Pref.Free;
         Mult.Free;
         Expo.Free;
         ExpoBis.Free;
         Off.Free;
         ExpoPref.Free;
         TenExpo.Free;
      End;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIVarDecl(aCellMLComponent: TCellMLComponent;
                                                         Var aNode: TMathMLCommandBinTree): Boolean;
Var
   CellMLVariable: TCellMLVariable;
   Sym: TCellMLScannerEngineSymbol;
   Properties: TMathMLCommandBinTree;
   Init, Pub, Priv: TMathMLCommandBinTree;
Begin
   // One of the top parsing functions, so need to initialise "aNode", just in
   // case something goes wrong in the parsing...

   aNode := Nil;

   // Variable declaration

   CellMLScannerEngine.GetNextToken;

   // Name of the variable

   Result := IdentifierToken;

   If (Result) Then Begin
      If (aCellMLComponent <> Nil) Then Begin
         CellMLVariable := TCellMLVariable.Create(aCellMLComponent);

         CellMLVariable.Name := CellMLScannerEngine.Token.Str;
         CellMLVariable.Line := CellMLScannerEngine.Token.Line;
      End Else Begin
         CellMLVariable := Nil;

         aNode := TMathMLCommandBinTree.Create(FileName, CellMLScannerEngine.Token.Str);
      End;

      Init := Nil;
      Pub  := Nil;
      Priv := Nil;

      CellMLScannerEngine.GetNextToken;

      // ":"

      Result := ColonToken;

      If (Result) Then Begin
         CellMLScannerEngine.GetNextToken;

         // Type of the variable

         Result := IdentifierToken;

         If (Result) Then Begin
            If (aCellMLComponent <> Nil) Then
               CellMLVariable.Units := CellMLScannerEngine.Token.Str
            Else
               UpdateMathMLCommandBinTree(aNode, mitConcatenate, aNode, TMathMLCommandBinTree.Create(FileName, mitUnit, ' ('+CellMLScannerEngine.Token.Str+') '));

            CellMLScannerEngine.GetNextToken;

            // "{"?

            If (CellMLScannerEngine.Token.Sym = sOpeningCurlyBracket) Then Begin
               // Start the definition of some options for the variable

               CellMLScannerEngine.GetNextToken;

               While (Result And (CellMLScannerEngine.Token.Sym In [sInit..sPriv])) Do Begin
                  Sym := CellMLScannerEngine.Token.Sym;

                  CellMLScannerEngine.GetNextToken;

                  // ":"

                  Result := ColonToken;

                  If (Result) Then Begin
                     CellMLScannerEngine.GetNextToken;

                     // Floating value or predefined prefix

                     If (Sym = sInit) Then
                        Result := NumberValueToken
                     Else
                        Result := PubPrivInterfaceValueToken;

                     If (Result) Then Begin
                        If (aCellMLComponent <> Nil) Then
                           Case Sym Of
                              sInit:
                                 CellMLVariable.InitialValue := SimplifyNbStr(CellMLScannerEngine.Token.Str);
                              sPub:
                                 CellMLVariable.PublicInterface := CellMLScannerEngine.Token.Str;
                              sPriv:
                                 CellMLVariable.PrivateInterface := CellMLScannerEngine.Token.Str;
                           End
                        Else
                           Case Sym Of
                              sInit:
                                 UpdateMathMLCommandBinTree(Init, mitConcatenate,
                                                            TMathMLCommandBinTree.Create(FileName, 'init: '),
                                                            TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(CellMLScannerEngine.Token.Str)));
                              sPub:
                                 UpdateMathMLCommandBinTree(Pub, mitConcatenate,
                                                            TMathMLCommandBinTree.Create(FileName, 'pub: '),
                                                            TMathMLCommandBinTree.Create(FileName, CellMLScannerEngine.Token.Str));
                              sPriv:
                                 UpdateMathMLCommandBinTree(Priv, mitConcatenate,
                                                            TMathMLCommandBinTree.Create(FileName, 'priv: '),
                                                            TMathMLCommandBinTree.Create(FileName, CellMLScannerEngine.Token.Str));
                           End;

                        CellMLScannerEngine.GetNextToken;

                        // ","?

                        If (CellMLScannerEngine.Token.Sym = sComma) Then Begin
                           CellMLScannerEngine.GetNextToken;

                           // We MUST have a new parameter

                           If (Not (CellMLScannerEngine.Token.Sym In [sInit..sPriv])) Then Begin
                              AddMsg(CellMLScannerEngine.Token, 'either a ''init'', ''pub'' or ''priv'' parameter');

                              Result := False;
                           End;
                        End;
                     End;
                  End;
               End;

               If (Result) Then Begin
                  // "}"

                  Result := ClosingCurlyBracketToken;

                  If (Result) Then
                     CellMLScannerEngine.GetNextToken;
               End;
            End;
         End;
      End;

      If (Result) Then Begin
         // ";"

         Result := SemiColonToken;

         If (Result) Then
            CellMLScannerEngine.GetNextToken;
      End;

      If (aCellMLComponent <> Nil) Then Begin
         If (Result) Then
            aCellMLComponent.VariableList.Add([CellMLVariable])
         Else
            CellMLVariable.Free;
      End Else If (Result) Then Begin
         // Add the init, pub and priv information, should it be present. Note
         // the importance of the order in which this is being done!

         If ((Init <> Nil) Or (Pub <> Nil) Or (Priv <> Nil)) Then Begin
            Properties := Nil;

            If (Init <> Nil) Then
               // This is the first property that needs to be put into the
               // structure, so we must initialise things...

               Properties := Init;

            If (Pub <> Nil) Then Begin
               If (Properties <> Nil) Then 
                  UpdateMathMLCommandBinTree(Properties, mitProperty, Properties, Pub)
               Else
                  // This is the first property that needs to be put into the
                  // structure, so we must initialise things...

                  Properties := Pub;
            End;

            If (Priv <> Nil) Then Begin
               If (Properties <> Nil) Then 
                  UpdateMathMLCommandBinTree(Properties, mitProperty, Properties, Priv)
               Else
                  // This is the first property that needs to be put into the
                  // structure, so we must initialise things...

                  Properties := Priv;
            End;

            UpdateMathMLCommandBinTree(aNode, mitProperties, aNode, Properties);
         End;
      End Else Begin
         FreeAndNil(aNode);

         Init.Free;
         Pub.Free;
         Priv.Free;
      End;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIComponent(aCellMLComponent: TCellMLComponent): Boolean;
Var
   MathMLEquation: TMathMLEquation;
   MathMLCommandBinTree: TMathMLCommandBinTree;
   BaseUnit: Boolean;
Begin
   // Name of the component

   Result := IdentifierToken;

   If (Result) Then Begin
      aCellMLComponent.Name := CellMLScannerEngine.Token.Str;

      CellMLScannerEngine.GetNextToken;

      // "as"

      Result := AsToken;

      If (Result) Then Begin
         CellMLScannerEngine.GetNextToken;

         // List of unit(s)/variable(s)/equation(s) that make up the component

         While (Result And (CellMLScannerEngine.Token.Sym In [sDef..sIdentifier, sODE])) Do
            Case CellMLScannerEngine.Token.Sym Of
               sDef: Begin
                  // Unit definition

                  CellMLScannerEngine.GetNextToken;

                  If (CellMLScannerEngine.Token.Sym <> sUnit) Then Begin
                     AddMsg(CellMLScannerEngine.Token, '''units''');

                     Result := False;

                     Break;
                  End Else Begin
                     Result := ParseASCIIUnits(aCellMLComponent, MathMLCommandBinTree, False, BaseUnit);

                     If (Result) Then Begin
                        // "enddef;"

                        Result := (BaseUnit And SemiColonToken) Or (Not BaseUnit And EndDefPlusSemiColonToken);

                        If (Result) Then
                           CellMLScannerEngine.GetNextToken;
                     End;
                  End;
               End;
               sVar:
                  // variable declaration

                  Result := ParseASCIIVarDecl(aCellMLComponent, MathMLCommandBinTree);
               sIdentifier, sODE: Begin
                  // Equation 

                  MathMLEquation := TMathMLEquation.Create(aCellMLComponent);

                  Result := ParseASCIIEquation(MathMLCommandBinTree);

                  If (Result) Then Begin
                     aCellMLComponent.EquationList.Add([MathMLEquation]);

                     MathMLEquation.MathMLEquationBinTree := MathMLCommandBinTree;
                     MathMLEquation.Line                  := MathMLCommandBinTree.Line;
                  End Else
                     MathMLEquation.Free;
               End;
            End;
      End;
   End;
End;

//==============================================================================

Procedure TCellMLASCIIToCellMLAPIEngine.UpdateMathMLCommandBinTree(Var aNode: TMathMLCommandBinTree;
                                                                   Const aItemType: TMathMLCommandBinTreeItemType;
                                                                   aLeft: TMathMLCommandBinTree;
                                                                   aRight: TMathMLCommandBinTree;
                                                                   Const aLine: Integer);
Begin
   aNode := TMathMLCommandBinTree.Create(FileName, aItemType);

   aNode.Left  := aLeft;
   aNode.Right := aRight;

   If (aLine = UNDEFINED) Then
      aNode.Line := CellMLScannerEngine.Token.Line
   Else
      aNode.Line := aLine;
End;

//==============================================================================

Procedure TCellMLASCIIToCellMLAPIEngine.IdentifierNode(Var aNode: TMathMLCommandBinTree);
Begin
   aNode := TMathMLCommandBinTree.Create(FileName, CellMLScannerEngine.Token.Str);

   aNode.Line := CellMLScannerEngine.Token.Line;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.DerivativeNode(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   aNode := TMathMLCommandBinTree.Create(FileName, mitDiff);

   aNode.Line := CellMLScannerEngine.Token.Line;

   CellMLScannerEngine.GetNextToken;

   // "("

   Result := OpeningBracketToken;

   If (Result) Then Begin
      CellMLScannerEngine.GetNextToken;

      // Identifier

      Result := IdentifierToken;

      If (Result) Then Begin
         aNode.Right := TMathMLCommandBinTree.Create(FileName, CellMLScannerEngine.Token.Str);

         aNode.Right.Line := CellMLScannerEngine.Token.Line;

         CellMLScannerEngine.GetNextToken;

         // ","

         Result := CommaToken;

         If (Result) Then Begin
            CellMLScannerEngine.GetNextToken;

            // Identifier

            Result := IdentifierToken;

            If (Result) Then Begin
               aNode.Left := TMathMLCommandBinTree.Create(FileName, CellMLScannerEngine.Token.Str);

               aNode.Left.Line := CellMLScannerEngine.Token.Line;

               CellMLScannerEngine.GetNextToken;

               // ")"

               Result := ClosingBracketToken;

               If (Not Result) Then
                  FreeAndNil(aNode.Left);
            End;
         End;

         If (Not Result) Then
            FreeAndNil(aNode.Right);
      End;
   End;

   If (Not Result) Then 
      FreeAndNil(aNode);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquation(Var aNode: TMathMLCommandBinTree): Boolean;
Var
   Left, Right: TMathMLCommandBinTree;
   CrtNode, TempLeft, TempRight: TMathMLCommandBinTree;
   EqualLine, CaseLine, OtherwiseLine: Integer;
   NeedAtLeastOnePiecewise: Boolean;
Begin
   // One of the top parsing functions, so need to initialise "aNode", just in
   // case something goes wrong in the parsing...

   aNode := Nil;

   // Either an identifier or a derivative has been scanned, otherwise we
   // wouldn't be here...

   If (CellMLScannerEngine.Token.Sym = sIdentifier) Then
      // Identifier

      IdentifierNode(Left)
   Else Begin
      // Should be a derivative

      Result := DerivativeNode(Left);

      If (Not Result) Then
         Exit;
   End;

   CellMLScannerEngine.GetNextToken;

   // "="

   If (CellMLScannerEngine.Token.Sym <> sEq) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''=''');

      Result := False;
   End Else Begin
      EqualLine := CellMLScannerEngine.Token.Line;

      CellMLScannerEngine.GetNextToken;

      // "sel"?

      If (CellMLScannerEngine.Token.Sym = sSel) Then Begin
         NeedAtLeastOnePiecewise := True;

         CellMLScannerEngine.GetNextToken;

         If (CellMLScannerEngine.Token.Sym <> sCase) Then Begin
            AddMsg(CellMLScannerEngine.Token, '''case''');

            Result := False;
         End Else Begin
            // One or several "case"s

            Result := True;

            Right := Nil;

            While (Result And (CellMLScannerEngine.Token.Sym = sCase)) Do Begin
               CaseLine := CellMLScannerEngine.Token.Line;

               CellMLScannerEngine.GetNextToken;

               // Condition

               Result := ParseASCIIEquationExpr01(TempRight);

               If (Result) Then Begin
                  // ":"

                  Result := ColonToken;

                  If (Result) Then Begin
                     CellMLScannerEngine.GetNextToken;

                     // Expression

                     Result := ParseASCIIEquationExpr01(TempLeft);

                     If (Result) Then Begin
                        // ";"

                        Result := SemiColonToken;

                        If (Result) Then Begin
                           CellMLScannerEngine.GetNextToken;

                           UpdateMathMLCommandBinTree(CrtNode, mitPiece, TempLeft, TempRight, CaseLine);

                           If (Right <> Nil) Then Begin
                              UpdateMathMLCommandBinTree(Right, mitPiecewise, Right, CrtNode);

                              NeedAtLeastOnePiecewise := False;
                           End Else
                              // This is the first piecewise element that needs
                              // to be put into the structure, so we must
                              // initialise things...

                              Right := CrtNode;
                        End;
                     End;

                     If (Not Result) Then
                        TempLeft.Free;
                  End;
               End;

               If (Not Result) Then
                  TempRight.Free;
            End;

            If (Result) Then Begin
               // "otherwise"?

               If (CellMLScannerEngine.Token.Sym = sOtherwise) Then Begin
                  OtherwiseLine := CellMLScannerEngine.Token.Line;

                  CellMLScannerEngine.GetNextToken;

                  // ":"

                  Result := ColonToken;

                  If (Result) Then Begin
                     CellMLScannerEngine.GetNextToken;

                     // Expression

                     Result := ParseASCIIEquationExpr01(TempLeft);

                     If (Result) Then Begin
                        // ";"

                        Result := SemiColonToken;

                        If (Result) Then Begin
                           CellMLScannerEngine.GetNextToken;

                           UpdateMathMLCommandBinTree(CrtNode, mitOtherwise, TempLeft, Nil, OtherwiseLine);

                           // Add the otherwise condition to the rest of the
                           // select statement

                           UpdateMathMLCommandBinTree(Right, mitPiecewise, Right, CrtNode);

                           NeedAtLeastOnePiecewise := False;
                        End;
                     End;

                     If (Not Result) Then
                        TempLeft.Free;
                  End;
               End;

               If (Result) Then Begin
                  // "endsel"

                  If (CellMLScannerEngine.Token.Sym <> sEndSel) Then Begin
                     AddMsg(CellMLScannerEngine.Token, '''endsel''');

                     Result := False;
                  End Else Begin
                     CellMLScannerEngine.GetNextToken;

                     // ";"

                     Result := SemiColonToken;

                     If (Result) Then
                        CellMLScannerEngine.GetNextToken;
                  End;
               End;
            End;

            If (Not Result) Then
               Right.Free
            Else If (NeedAtLeastOnePiecewise) Then 
               UpdateMathMLCommandBinTree(Right, mitPiecewise, Right, Nil);
         End;
      End Else Begin
         // Not a selection expression, so it has to be a "normal" one...

         Result := ParseASCIIEquationExpr01(Right);

         If (Result) Then Begin
            // ";"

            Result := SemiColonToken;

            If (Result) Then
               CellMLScannerEngine.GetNextToken
         End;

         If (Not Result) Then
            Right.Free;
      End;

      If (Result) Then
         UpdateMathMLCommandBinTree(aNode, mitEq, Left, Right, EqualLine);
   End;

   If (Not Result) Then
      Left.Free;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.GenericParseASCIIEquationExpr(Var aNode: TMathMLCommandBinTree;
                                                                     aSymbolSet: TCellMLScannerEngineSymbolSet;
                                                                     aParseASCIIEquationExprFunc: TParseASCIIEquationExprFunc): Boolean;
Var
   CellMLSym: TMathMLCommandBinTreeItemType;
   Right: TMathMLCommandBinTree;
Begin
   Result := aParseASCIIEquationExprFunc(aNode);

   While (Result And (CellMLScannerEngine.Token.Sym In aSymbolSet)) Do Begin
      CellMLSym := CellMLScannerEngine.Token.CellMLSym;

      CellMLScannerEngine.GetNextToken;

      Right := Nil;

      Result := aParseASCIIEquationExprFunc(Right);

      If (Result) Then 
         UpdateMathMLCommandBinTree(aNode, CellMLSym, aNode, Right)
      Else
         Right.Free;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr01(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   // One of the top parsing functions, so need to initialise "aNode", just in
   // case something goes wrong in the parsing...

   aNode := Nil;

   // "or"

   Result := GenericParseASCIIEquationExpr(aNode, [sOr], ParseASCIIEquationExpr02);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr02(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   // "and"

   Result := GenericParseASCIIEquationExpr(aNode, [sAnd], ParseASCIIEquationExpr03);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr03(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   // "xor"

   Result := GenericParseASCIIEquationExpr(aNode, [sXOr], ParseASCIIEquationExpr04);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr04(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   // "==" or "<>"

   Result := GenericParseASCIIEquationExpr(aNode, [sEqEq, sNEq], ParseASCIIEquationExpr05);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr05(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   // "<", ">", "<=" or ">="

   Result := GenericParseASCIIEquationExpr(aNode, [sLT, sGT, sLEq, sGEq], ParseASCIIEquationExpr06);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr06(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   // "+" or "-"

   Result := GenericParseASCIIEquationExpr(aNode, [sPlus, sMinus], ParseASCIIEquationExpr07);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr07(Var aNode: TMathMLCommandBinTree): Boolean;
Begin
   // "*" or "/"

   Result := GenericParseASCIIEquationExpr(aNode, [sTimes, sDivide], ParseASCIIEquationExpr08);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr08(Var aNode: TMathMLCommandBinTree): Boolean;
Var
   Left: TMathMLCommandBinTree;
   CellMLSym: TMathMLCommandBinTreeItemType;
Begin
   // "not", "+" or "-"

   If (CellMLScannerEngine.Token.Sym In [sNot, sPlus, sMinus]) Then Begin
      CellMLSym := CellMLScannerEngine.Token.CellMLSym;

      CellMLScannerEngine.GetNextToken;

      If (CellMLSym = mitNot) Then Begin
         Result := ParseASCIIEquationExpr01(Left);

         If (Result) Then 
            UpdateMathMLCommandBinTree(aNode, CellMLSym, Left, Nil)
         Else
            Left.Free;
      End Else Begin
         Left := Nil;

         Result := ParseASCIIEquationExpr09(Left);

         If (Result) Then
            UpdateMathMLCommandBinTree(aNode, CellMLSym, Left, Nil)
         Else
            Left.Free;
      End;
   End Else
      Result := ParseASCIIEquationExpr09(aNode);
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIEquationExpr09(Var aNode: TMathMLCommandBinTree): Boolean;
Var
   DblNbStr: String;
   Left, Right, LogBase, Degree: TMathMLCommandBinTree;
   CellMLSym: TMathMLCommandBinTreeItemType;
Begin
   // Identifier, derivative, number, mathematical function, opening bracket or
   // a mathematical constant

   Case CellMLScannerEngine.Token.Sym Of
      sIdentifier: Begin
         IdentifierNode(aNode);

         Result := True;

         CellMLScannerEngine.GetNextToken;
      End;
      sODE: Begin
         Result := DerivativeNode(aNode);

         CellMLScannerEngine.GetNextToken;
      End;
      sNumber: Begin
         DblNbStr := SimplifyNbStr(CellMLScannerEngine.Token.Str);

         CellMLScannerEngine.GetNextToken;

         // "{"

         Result := OpeningCurlyBracketToken;

         If (Result) Then Begin
            CellMLScannerEngine.GetNextToken;

            // Unit for the number

            Result := IdentifierToken;

            If (Result) Then Begin
               aNode := TMathMLCommandBinTree.Create(FileName, DblNbStr, CellMLScannerEngine.Token.Str);

               aNode.Line := CellMLScannerEngine.Token.Line;

               CellMLScannerEngine.GetNextToken;

               // "}"

               Result := ClosingCurlyBracketToken;

               If (Result) Then
                  CellMLScannerEngine.GetNextToken
               Else
                  FreeAndNil(aNode);
            End;
         End;
      End;
      sTooBigANumber: 
         Result := NumberToken;   // Will generate an error message for us
                                  // Note: could do it here, but this is in case
                                  //       we want/need to update the handling
                                  //       of too big numbers
      sSqr..sACotH: Begin   // Mathematical functions with 1 argument
         CellMLSym := CellMLScannerEngine.Token.CellMLSym;

         CellMLScannerEngine.GetNextToken;

         // "("

         Result := OpeningBracketToken;

         If (Result) Then Begin
            CellMLScannerEngine.GetNextToken;

            Result := ParseASCIIEquationExpr01(Left);

            If (Result) Then Begin
               // ")"

               Result := ClosingBracketToken;

               If (Result) Then Begin
                  Case CellMLSym Of
                     mitPow: Begin
                        Right := TMathMLCommandBinTree.Create(FileName, '2', 'dimensionless');

                        Right.Line := CellMLScannerEngine.Token.Line;

                        UpdateMathMLCommandBinTree(aNode, mitPow, Left, Right);
                     End;
                     mitRoot:
                        UpdateMathMLCommandBinTree(aNode, mitRoot, Left, Nil);
                  Else
                     UpdateMathMLCommandBinTree(aNode, CellMLSym, Left, Nil);
                  End;

                  CellMLScannerEngine.GetNextToken;
               End;
            End;

            If (Not Result) Then
               Left.Free;
         End;
      End;
      sLog: Begin   // Mathematical functions with 1 or 2 arguments
         CellMLSym := CellMLScannerEngine.Token.CellMLSym;

         CellMLScannerEngine.GetNextToken;

         // "("

         Result := OpeningBracketToken;

         If (Result) Then Begin
            CellMLScannerEngine.GetNextToken;

            Result := ParseASCIIEquationExpr01(Left);

            If (Result) Then Begin
               // ","?

               If (CellMLScannerEngine.Token.Sym = sComma) Then Begin
                  CellMLScannerEngine.GetNextToken;

                  Result := ParseASCIIEquationExpr01(Right);
               End Else
                  Right := Nil;

               If (Result) Then Begin
                  If (Right <> Nil) Then Begin
                     LogBase := TMathMLCommandBinTree.Create(FileName, mitLogBase);

                     LogBase.Line := CellMLScannerEngine.Token.Line;

                     LogBase.Left := Right;

                     UpdateMathMLCommandBinTree(aNode, CellMLSym, LogBase, Left);
                  End Else
                     UpdateMathMLCommandBinTree(aNode, CellMLSym, Left, Right);

                  // ")"

                  Result := ClosingBracketToken;

                  If (Result) Then
                     CellMLScannerEngine.GetNextToken
                  Else Begin
                     FreeAndNil(aNode);

                     Left  := Nil;
                     Right := Nil;
                  End;
               End;

               If (Not Result) Then
                  Right.Free;
            End;

            If (Not Result) Then
               Left.Free;
         End;
      End;
      sPow..sRoot: Begin   // Mathematical functions with 2 arguments
         CellMLSym := CellMLScannerEngine.Token.CellMLSym;

         CellMLScannerEngine.GetNextToken;

         // "("

         Result := OpeningBracketToken;

         If (Result) Then Begin
            CellMLScannerEngine.GetNextToken;

            Result := ParseASCIIEquationExpr01(Left);

            If (Result) Then Begin
               // ","

               Result := CommaToken;

               If (Result) Then Begin
                  CellMLScannerEngine.GetNextToken;

                  Result := ParseASCIIEquationExpr01(Right);

                  If (Result) Then Begin
                     If (CellMLSym = mitRoot) Then Begin
                        // Special case of the root function...

                        Degree := TMathMLCommandBinTree.Create(FileName, mitDegree);

                        Degree.Line := CellMLScannerEngine.Token.Line;

                        Degree.Left := Right;

                        UpdateMathMLCommandBinTree(aNode, CellMLSym, Degree, Left);
                     End Else
                        UpdateMathMLCommandBinTree(aNode, CellMLSym, Left, Right);

                     // ")"

                     Result := ClosingBracketToken;

                     If (Result) Then
                        CellMLScannerEngine.GetNextToken
                     Else Begin
                        FreeAndNil(aNode);

                        Left  := Nil;
                        Right := Nil;
                     End;
                  End;

                  If (Not Result) Then
                     Right.Free;
               End;
            End;

            If (Not Result) Then
               Left.Free;
         End;
      End;
      sOpeningBracket: Begin
         CellMLScannerEngine.GetNextToken;

         Result := ParseASCIIEquationExpr01(aNode);

         If (Result) Then Begin
            // ")"

            Result := ClosingBracketToken;

            If (Result) Then
               CellMLScannerEngine.GetNextToken;
         End Else
            FreeAndNil(aNode);
      End;
      sTrue, sFalse,
      sPI, sExponentiale: Begin
         UpdateMathMLCommandBinTree(aNode, CellMLScannerEngine.Token.CellMLSym, Nil, Nil);

         Result := True;

         CellMLScannerEngine.GetNextToken;
      End;
   Else
      AddMsg(CellMLScannerEngine.Token, 'either an identifier, derivative, number, mathematical function, opening bracket or mathematical constant');

      Result := False;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIGroup(aCellMLGroup: TCellMLGroup): Boolean;
Var
   AtLeastOneGroupType: Boolean;
   CellMLComponentRef: TCellMLComponentRef;
Begin
   // "as"

   Result := AsToken;

   If (Result) Then Begin
      CellMLScannerEngine.GetNextToken;

      AtLeastOneGroupType := False;

      // "encapsulation"/"containment"?

      While (Result And (CellMLScannerEngine.Token.Sym In [sEncapsulation, sContainment])) Do Begin
         If (CellMLScannerEngine.Token.Sym = sEncapsulation) Then Begin
            aCellMLGroup.Encapsulation := True;

            AtLeastOneGroupType := True;

            CellMLScannerEngine.GetNextToken;
         End Else Begin
            aCellMLGroup.Containment := True;

            AtLeastOneGroupType := True;

            CellMLScannerEngine.GetNextToken;

            // "name"?

            If (CellMLScannerEngine.Token.Sym = sIdentifier) Then Begin
               aCellMLGroup.Name := CellMLScannerEngine.Token.Str;

               CellMLScannerEngine.GetNextToken;
            End;
         End;
      End;

      If (Not AtLeastOneGroupType) Then Begin
         AddMsg(CellMLScannerEngine.Token, 'at least ''encapsulation'' or ''containment''');

         Result := False;
      End Else Begin
         // "for"

         Result := ForToken;

         If (Result) Then Begin
            CellMLScannerEngine.GetNextToken;

            // List of component reference(s) used for the definition of the
            // group

            While (Result And (CellMLScannerEngine.Token.Sym = sComp)) Do Begin
               CellMLScannerEngine.GetNextToken;

               CellMLComponentRef := TCellMLComponentRef.Create(aCellMLGroup);

               CellMLComponentRef.Line := CellMLScannerEngine.Token.Line;

               Result := ParseASCIIComponentRef(CellMLComponentRef);

               If (Result) Then
                  aCellMLGroup.ComponentRefList.Add([CellMLComponentRef])
               Else
                  CellMLComponentRef.Free;
            End;
         End;
      End;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIComponentRef(aCellMLComponentRef: TCellMLComponentRef): Boolean;
Var
   CellMLComponentRef: TCellMLComponentRef;
Begin
   // Name of the component reference

   Result := IdentifierToken;

   If (Result) Then Begin
      aCellMLComponentRef.Name := CellMLScannerEngine.Token.Str;

      CellMLScannerEngine.GetNextToken;

      // "incl"?

      If (CellMLScannerEngine.Token.Sym = sIncl) Then Begin
         CellMLScannerEngine.GetNextToken;

         // List of component reference(s) used for the definition of the
         // component

         While (Result And (CellMLScannerEngine.Token.Sym = sComp)) Do Begin
            CellMLScannerEngine.GetNextToken;

            CellMLComponentRef := TCellMLComponentRef.Create(aCellMLComponentRef);

            CellMLComponentRef.Line := CellMLScannerEngine.Token.Line;

            Result := ParseASCIIComponentRef(CellMLComponentRef);

            If (Result) Then
               aCellMLComponentRef.ComponentRefList.Add([CellMLComponentRef])
            Else
               CellMLComponentRef.Free;
         End;

         If (Result) Then Begin
            // "endcomp"

            If (CellMLScannerEngine.Token.Sym <> sEndComp) Then Begin
               AddMsg(CellMLScannerEngine.Token, '''endcomp''');

               Result := False;
            End Else Begin
               CellMLScannerEngine.GetNextToken;

               // ";"

               Result := SemiColonToken;

               If (Result) Then
                  CellMLScannerEngine.GetNextToken;
            End;
         End;
      End Else Begin
         // ";"

         Result := SemiColonToken;

         If (Result) Then
            CellMLScannerEngine.GetNextToken;
      End;
   End;
End;

//==============================================================================

Function TCellMLASCIIToCellMLAPIEngine.ParseASCIIConnection(aCellMLConnection: TCellMLConnection): Boolean;
Var
   CellMLMapVariables: TCellMLMapVariables;
   Variable1, Variable2: String;
Begin
   // "between"

   If (CellMLScannerEngine.Token.Sym <> sBetween) Then Begin
      AddMsg(CellMLScannerEngine.Token, '''between''');

      Result := False;
   End Else Begin
      CellMLScannerEngine.GetNextToken;

      // First component

      Result := IdentifierToken;

      If (Result) Then Begin
         aCellMLConnection.Component1 := CellMLScannerEngine.Token.Str;

         CellMLScannerEngine.GetNextToken;

         // "and"

         Result := AndToken;

         If (Result) Then Begin
            CellMLScannerEngine.GetNextToken;

            // Second component

            Result := IdentifierToken;

            If (Result) Then Begin
               aCellMLConnection.Component2 := CellMLScannerEngine.Token.Str;

               CellMLScannerEngine.GetNextToken;

               // "for"

               Result := ForToken;

               If (Result) Then Begin
                  CellMLScannerEngine.GetNextToken;

                  // List of variables to connect

                  While (Result And (CellMLScannerEngine.Token.Sym = sVars)) Do Begin
                     CellMLScannerEngine.GetNextToken;

                     // First variable

                     Result := IdentifierToken;

                     If (Result) Then Begin
                        Variable1 := CellMLScannerEngine.Token.Str;

                        CellMLScannerEngine.GetNextToken;

                        // "and"

                        Result := AndToken;

                        If (Result) Then Begin
                           CellMLScannerEngine.GetNextToken;

                           // Second variable

                           Result := IdentifierToken;

                           If (Result) Then Begin
                              Variable2 := CellMLScannerEngine.Token.Str;

                              CellMLScannerEngine.GetNextToken;

                              // ";"

                              Result := SemiColonToken;
                           End;
                        End;
                     End;

                     If (Result) Then Begin
                        CellMLMapVariables := TCellMLMapVariables.Create(aCellMLConnection, Variable1, Variable2);

                        CellMLMapVariables.Line := CellMLScannerEngine.Token.Line;

                        aCellMLConnection.MapVariablesList.Add([CellMLMapVariables]);

                        CellMLScannerEngine.GetNextToken;
                     End;
                  End;
               End;
            End;
         End;
      End;
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
