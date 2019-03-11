//==============================================================================
// Engine class for converting a CellML API object to a CellML ASCII form
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
//------------------------------------------------------------------------------
// Note: all the MathML stuff has to be consistent with what is done in
//       "TCmdGraph"
//==============================================================================

Unit CellMLAPIToCellMLASCIIEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   Classes, XMLIntf, CellMLAPIEngine, CellMLAPI, Engine;

//==============================================================================

Type
   TCellMLAPIToCellMLASCIIEngine = Class(TCellMLAPIEngine)
      Private
         // Properties used for internal purposes

         Stream: String;
         StreamPos: Integer;

         Line: Integer;

         ASCII: PString;

         PrevUnitDefIsBaseUnitDef: Boolean;

         // Methods used for internal purposes

         Procedure Output(Const aString: String = ''; Const aCRLF: Boolean = True);

      Protected
         // Methods used for internal purposes

         Procedure StartCellMLModel(aCellMLModel: TCellMLModel; Var aNode: IXMLNode); Override;
         Procedure EndCellMLModel; Override;

         Procedure StartCellMLUnits(aCellMLUnits: TCellMLUnits; Var aNode: IXMLNode); Override;
         Procedure CellMLUnit(aCellMLUnit: TCellMLUnit; aNode: IXMLNode); Override;
         Procedure EndCellMLUnits(Const aNextItemType: TXMLNextItemType); Override;

         Procedure StartCellMLComponent(aCellMLComponent: TCellMLComponent; Var aNode: IXMLNode); Override;
         Procedure EndCellMLComponent(Const aNextItemType: TXMLNextItemType); Override;

         Procedure CellMLVariable(aCellMLVariable: TCellMLVariable; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode); Override;

         Procedure AddPieceOrOtherwise(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType);
         Procedure AddEqPiecewise(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType);
         Procedure AddParentheses(Var aString: String); Inline;
         Function AddOperand(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType; Const aAssignmentEq: Boolean): String;
         Function AddFunc(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType): String; Inline;
         Function AddPlus(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType): String;
         Function AddMinus(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType): String;
         Function AddPower(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType): String; Inline;
         Function AddRoot(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType): String; Inline;
         Function AddNot(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType): String;

         Function GenerateEquation(aMathMLEquationBinTree: TMathMLCommandBinTree; Const aNextItemType: TXMLNextItemType; Const aAssignmentEqn: Boolean = False): String;

         Procedure StartMathMLEquation(Var aNode: IXMLNode); Override;
         Procedure MathMLEquation(aMathMLEquation: TMathMLEquation; Const aNextItemType: TXMLNextItemType; Var aNode: IXMLNode); Override;

         Procedure StartCellMLGroup(aCellMLGroup: TCellMLGroup; Var aNode: IXMLNode); Override;
         Procedure EndCellMLGroup(Const aNextItemType: TXMLNextItemType); Override;

         Procedure CellMLComponentRef(aCellMLComponentRef: TCellMLComponentRef; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode); Override;

         Procedure StartCellMLConnection(aCellMLConnection: TCellMLConnection; Var aNode: IXMLNode); Override;
         Procedure EndCellMLConnection(Const aNextItemType: TXMLNextItemType); Override;

         Procedure CellMLMapVariables(aCellMLMapVariables: TCellMLMapVariables; aNode: IXMLNode); Override;

      Public
         // Constructor & Destructor

         Constructor Create(Const aCellMLModel: TCellMLModel; Var aASCII: String);
         Destructor Destroy; Override;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   SysUtils, Common;

//==============================================================================

Constructor TCellMLAPIToCellMLASCIIEngine.Create(Const aCellMLModel: TCellMLModel;
                                                 Var aASCII: String);
Begin
   Inherited Create(aCellMLModel);

   SetLength(CellMLIndent, 1);

   CellMLIndent[0] := '';

   StreamPos := 1;

   Line := 1;

   ASCII := @aASCII;
End;

//==============================================================================

Destructor TCellMLAPIToCellMLASCIIEngine.Destroy;
Begin
   ASCII^ := Stream;

   Inherited;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.Output(Const aString: String;
                                               Const aCRLF: Boolean);
Var
   Str: String;
Begin
   // Note: to write directly to the CellML editor (i.e. do something like:
   //       "CellMLEdit.Lines.Add(CellMLIndent[0]+aString);" doesn't
   //       look nice in some instances. Indeed, the editor will scroll to the
   //       newly added string, which is not at all what we want. We want all
   //       the lines to be added and still be at the top of the editor. This
   //       can be achieved using a memory stream, and then loading its contents
   //       into the editor. This incidently makes the whole process slightly
   //       faster (since the editor doesn't scroll)...

   If (CompareStr(aString, '') = 0) Then Begin
      // Check that the previous line is not empty. If it is, then exit...

      If (StreamPos > 4) Then Begin
         If (CompareStr(Copy(Stream, StreamPos-4, 4), CRLF+CRLF) = 0) Then
            Exit;
      End Else If (StreamPos > 2) Then
         If (CompareStr(Copy(Stream, StreamPos-2, 2), CRLF) = 0) Then
            Exit;

      Str := CRLF;
   End Else Begin
      Str := CellMLIndent[0]+aString;

      If (aCRLF) Then
         Str := Str+CRLF;
   End;

   Stream := Stream+Str;

   Inc(StreamPos, Length(Str));

   Inc(Line);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.StartCellMLModel(aCellMLModel: TCellMLModel;
                                                         Var aNode: IXMLNode);
Begin
   aCellMLModel.Line := Line;

   Output('def model '+aCellMLModel.Name+' as');

   IncIndent(0);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.EndCellMLModel;
Begin
   DecIndent(0);

   Output('enddef;', False);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.StartCellMLUnits(aCellMLUnits: TCellMLUnits;
                                                         Var aNode: IXMLNode);
Begin
   aCellMLUnits.Line := Line;

   If (aCellMLUnits.BaseUnits) Then Begin
      Output('def unit '+aCellMLUnits.Name+' as base unit;');

      PrevUnitDefIsBaseUnitDef := True;
   End Else Begin
      If (PrevUnitDefIsBaseUnitDef) Then Begin
         PrevUnitDefIsBaseUnitDef := False;

         Output;
      End;

      Output('def unit '+aCellMLUnits.Name+' as');

      IncIndent(0);
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.CellMLUnit(aCellMLUnit: TCellMLUnit;
                                                   aNode: IXMLNode);
Var
   Cmd: String;
   Params: String;
Begin
   Cmd := 'unit '+aCellMLUnit.Name;

   Params := '';

   If (CompareStr(aCellMLUnit.Prefix, '') <> 0) Then
      Params := 'pref: '+aCellMLUnit.Prefix;

   If (CompareStr(aCellMLUnit.Exponent, '') <> 0) Then Begin
      If (CompareStr(Params, '') <> 0) Then
         Params := Params+', ';

      Params := Params+'expo: '+aCellMLUnit.Exponent;
   End;

   If (CompareStr(aCellMLUnit.Multiplier, '') <> 0) Then Begin
      If (CompareStr(Params, '') <> 0) Then
         Params := Params+', ';

      Params := Params+'mult: '+aCellMLUnit.Multiplier;
   End;

   If (CompareStr(aCellMLUnit.Offset, '') <> 0) Then Begin
      If (CompareStr(Params, '') <> 0) Then
         Params := Params+', ';

      Params := Params+'off: '+aCellMLUnit.Offset;
   End;

   If (CompareStr(Params, '') <> 0) Then
      Cmd := Cmd+' {'+Params+'}';

   aCellMLUnit.Line := Line;

   Output(Cmd+';');
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.EndCellMLUnits(Const aNextItemType: TXMLNextItemType);
Begin
   DecIndent(0);

   Output('enddef;');

   If (aNextItemType <> xitNone) Then
      Output;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.StartCellMLComponent(aCellMLComponent: TCellMLComponent;
                                                             Var aNode: IXMLNode);
Begin
   aCellMLComponent.Line := Line;

   Output('def comp '+aCellMLComponent.Name+' as');

   IncIndent(0);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.EndCellMLComponent(Const aNextItemType: TXMLNextItemType);
Begin
   DecIndent(0);

   Output('enddef;');

   If (aNextItemType <> xitNone) Then
      Output;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.CellMLVariable(aCellMLVariable: TCellMLVariable;
                                                       Const aNextItemType: TXMLNextItemType;
                                                       aNode: IXMLNode);
Var
   Cmd: String;
   Params: String;
Begin
   Cmd := 'var '+aCellMLVariable.Name+': '+aCellMLVariable.Units;

   Params := '';

   If (CompareStr(aCellMLVariable.InitialValue, '') <> 0) Then
      Params := 'init: '+SimplifyNbStr(aCellMLVariable.InitialValue);

   If (CompareStr(aCellMLVariable.PublicInterface, '') <> 0) Then Begin
      If (CompareStr(Params, '') <> 0) Then
         Params := Params+', ';

      Params := Params+'pub: '+aCellMLVariable.PublicInterface;
   End;

   If (CompareStr(aCellMLVariable.PrivateInterface, '') <> 0) Then Begin
      If (CompareStr(Params, '') <> 0) Then
         Params := Params+', ';

      Params := Params+'priv: '+aCellMLVariable.PrivateInterface;
   End;

   If (CompareStr(Params, '') <> 0) Then
      Cmd := Cmd+' {'+Params+'}';

   aCellMLVariable.Line := Line;

   Output(Cmd+';');

   If (aNextItemType = xitOther) Then
      Output;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.AddPieceOrOtherwise(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                            Const aNextItemType: TXMLNextItemType);
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitPiecewise: Begin
         AddPieceOrOtherwise(aMathMLEquationBinTree.Left, aNextItemType);

         If (aMathMLEquationBinTree.Right <> Nil) Then
            AddPieceOrOtherwise(aMathMLEquationBinTree.Right, aNextItemType);
      End;
      mitPiece: Begin
         Output('case '+GenerateEquation(aMathMLEquationBinTree.Right, aNextItemType)+':');

         IncIndent(0);

         Output(GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+';');

         DecIndent(0);
      End;
      mitOtherwise: Begin
         Output('otherwise:');

         IncIndent(0);

         Output(GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+';');

         DecIndent(0);
      End;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.AddEqPiecewise(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                       Const aNextItemType: TXMLNextItemType);
Begin
   Output;

   Output(GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+aMathMLEquationBinTree.Str+'sel');

   IncIndent(0);

   AddPieceOrOtherwise(aMathMLEquationBinTree.Right, aNextItemType);

   DecIndent(0);

   Output('endsel;');

   If (aNextItemType = xitSame) Then
      Output;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.AddParentheses(Var aString: String);
Begin
   aString := '('+aString+')';
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.AddOperand(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                  Const aNextItemType: TXMLNextItemType;
                                                  Const aAssignmentEq: Boolean): String;
Var
   Left, Right: String;
Begin
   Left  := GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType);
   Right := GenerateEquation(aMathMLEquationBinTree.Right, aNextItemType);

   // Determine whether "Left" and/or "Right" need parentheses around them

   Case aMathMLEquationBinTree.ItemType Of
      mitPlus: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitMinus,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Right);
         End;
      End;
      mitMinus: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitMinus,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Right);
            mitPlus:
               If (aMathMLEquationBinTree.Right.Right <> Nil) Then
                  AddParentheses(Right);
         End;
      End;
      mitTimes: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
            mitPlus, mitMinus:
               If (aMathMLEquationBinTree.Left.Right <> Nil) Then
                  AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Right);
            mitPlus, mitMinus:
               If (aMathMLEquationBinTree.Right.Right <> Nil) Then
                  AddParentheses(Right);
         End;
      End;
      mitDivide: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitAnd, mitOr, mitXOr:
               AddParentheses(Left);
            mitPlus, mitMinus:
               If (aMathMLEquationBinTree.Left.Right <> Nil) Then
                  AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
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
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus,
            mitOr, mitXOr:
               AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus,
            mitOr, mitXOr:
               AddParentheses(Right);
         End;
      End;
      mitOr: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus,
            mitAnd, mitXOr:
               AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus,
            mitAnd, mitXOr:
               AddParentheses(Right);
         End;
      End;
      mitXOr: Begin
         Case aMathMLEquationBinTree.Left.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus,
            mitAnd, mitOr:
               AddParentheses(Left);
         End;

         Case aMathMLEquationBinTree.Right.ItemType Of
            mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
            mitPlus, mitMinus,
            mitAnd, mitOr:
               AddParentheses(Right);
         End;
      End;
   End;

   Result := Left+aMathMLEquationBinTree.Str+Right;
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.AddFunc(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                               Const aNextItemType: TXMLNextItemType): String;
Begin
   Result := aMathMLEquationBinTree.Str+'('+GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+')';
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.AddPlus(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                               Const aNextItemType: TXMLNextItemType): String;
Begin
   Result := GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType);
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.AddMinus(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                Const aNextItemType: TXMLNextItemType): String;
Var
   Left: String;
Begin
   Left := GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType);

   Case aMathMLEquationBinTree.Left.ItemType Of
      mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
      mitPlus, mitMinus,
      mitAnd, mitOr, mitXOr:
         AddParentheses(Left);
   End;

   Result := aMathMLEquationBinTree.Str+Left;
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.AddPower(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                Const aNextItemType: TXMLNextItemType): String;
Begin
   Result := aMathMLEquationBinTree.Str+'('+
                GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+', '+
                GenerateEquation(aMathMLEquationBinTree.Right, aNextItemType)+')';
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.AddRoot(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                               Const aNextItemType: TXMLNextItemType): String;
Begin
   Result := aMathMLEquationBinTree.Str+'('+
                GenerateEquation(aMathMLEquationBinTree.Right, aNextItemType)+', '+
                GenerateEquation(aMathMLEquationBinTree.Left.Left, aNextItemType)+')';
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.AddNot(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                              Const aNextItemType: TXMLNextItemType): String;
Var
   Left: String;
Begin
   Left := GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType);

   Case aMathMLEquationBinTree.Left.ItemType Of
      mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq,
      mitPlus, mitMinus, mitTimes, mitDivide,
      mitAnd, mitOr, mitXOr: Begin
         AddParentheses(Left);

         Result := aMathMLEquationBinTree.Str+Left;
      End;
   Else
      Result := aMathMLEquationBinTree.Str+' '+Left;
   End;
End;

//==============================================================================

Function TCellMLAPIToCellMLASCIIEngine.GenerateEquation(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                        Const aNextItemType: TXMLNextItemType;
                                                        Const aAssignmentEqn: Boolean): String;
Var
   OpType: TMathMLOpType;
   Val: Double;
Begin
   Result := '';   // Since we don't always generate a result (the case with the
                   // "Piecewise" element, for instance)

   aMathMLEquationBinTree.Line := Line;

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
      mitDegree, mitLogBase,
      mitTrue, mitFalse, mitPI, mitExponentiale:
         OpType := motOwn;
   Else
      OpType := motFunc;
   End;

   // Combine the text with the sub-texts, if any, and this based on the type of
   // the operator

   Case OpType Of
      motOperand:
         Result := AddOperand(aMathMLEquationBinTree, aNextItemType, aAssignmentEqn);
      motFunc:
         Result := AddFunc(aMathMLEquationBinTree, aNextItemType);
      motOwn:
         Case aMathMLEquationBinTree.ItemType Of
            mitCN:
               Result := SimplifyNbStr(aMathMLEquationBinTree.Str)+'{'+aMathMLEquationBinTree.Units+'}';
            mitCI:
               Result := aMathMLEquationBinTree.Str;
            mitEq:
               AddEqPiecewise(aMathMLEquationBinTree, aNextItemType);
            mitDiff:
               Result := 'ode('+GenerateEquation(aMathMLEquationBinTree.Right, aNextItemType)+', '+GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+')';
            mitPlus:
               Result := AddPlus(aMathMLEquationBinTree, aNextItemType);
            mitMinus:
               Result := AddMinus(aMathMLEquationBinTree, aNextItemType);
            mitLog:
               Result := aMathMLEquationBinTree.Str+'('+
                         GenerateEquation(aMathMLEquationBinTree.Right, aNextItemType)+', '+
                         GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+')';
            mitDegree, mitLogBase:
               Result := GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType);
            mitPow: Begin
               If (Not TryStrToFloat(aMathMLEquationBinTree.Right.Str, Val)) Then
                  Val := 0;   // Simply to avoid the "sqr" case...

               If (Val = 2) Then
                  Result := 'sqr'+'('+GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+')'
               Else If (Val = 0.5) Then
                  Result := 'sqrt'+'('+GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+')'
               Else
                  Result := AddPower(aMathMLEquationBinTree, aNextItemType);
            End;
            mitRoot: Begin
               If (aMathMLEquationBinTree.Right = Nil) Then
                  Val := 2
               Else If (Not TryStrToFloat(aMathMLEquationBinTree.Left.Left.Str, Val)) Then
                  Val := 0;   // Simply to avoid the "sqrt" case...

               If (Val = 2) Then Begin
                  If (aMathMLEquationBinTree.Right = Nil) Then
                     Result := 'sqrt'+'('+GenerateEquation(aMathMLEquationBinTree.Left, aNextItemType)+')'
                  Else
                     Result := 'sqrt'+'('+GenerateEquation(aMathMLEquationBinTree.Right, aNextItemType)+')';
               End Else
                  Result := AddRoot(aMathMLEquationBinTree, aNextItemType);
            End;
            mitNot:
               Result := AddNot(aMathMLEquationBinTree, aNextItemType);
            mitTrue, mitFalse, mitPI, mitExponentiale:
               Result := aMathMLEquationBinTree.Str;
         End;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.StartMathMLEquation(Var aNode: IXMLNode);
Begin
   // Nothing to do!
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.MathMLEquation(aMathMLEquation: TMathMLEquation;
                                                       Const aNextItemType: TXMLNextItemType;
                                                       Var aNode: IXMLNode);
Var
   Eqn: String;
Begin
   Eqn := GenerateEquation(aMathMLEquation.MathMLEquationBinTree, aNextItemType, True);

   If (CompareStr(Eqn, '') <> 0) Then Begin
      // Note: the case where "Eqn" is empty is when we have a piecewise
      //       equation

      If (Copy(Eqn, Length(Eqn), 1)[1] <> ';') Then
         // Note: this occurs when we have a condition

         Eqn := Eqn+';';

      Output(Eqn);
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.StartCellMLGroup(aCellMLGroup: TCellMLGroup;
                                                         Var aNode: IXMLNode);
Var
   Cmd: String;
Begin
   Cmd := 'def group as ';

   If (aCellMLGroup.Encapsulation) Then
      Cmd := Cmd+'encapsulation';

   If (aCellMLGroup.Containment) Then Begin
      If (aCellMLGroup.Encapsulation) Then
         Cmd := Cmd+' and ';

      Cmd := Cmd+'containment';

      If (CompareStr(aCellMLGroup.Name, '') <> 0) Then
         Cmd := Cmd+' '+aCellMLGroup.Name;
   End;

   Cmd := Cmd+' for';

   aCellMLGroup.Line := Line;

   Output(Cmd);

   IncIndent(0);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.EndCellMLGroup(Const aNextItemType: TXMLNextItemType);
Begin
   DecIndent(0);

   Output('enddef;');

   If (aNextItemType <> xitNone) Then
      Output;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.CellMLComponentRef(aCellMLComponentRef: TCellMLComponentRef;
                                                           Const aNextItemType: TXMLNextItemType;
                                                           aNode: IXMLNode);
Begin
   If (aCellMLComponentRef.ComponentRefList.Size = 0) Then Begin
      aCellMLComponentRef.Line := Line;

      Output('comp '+aCellMLComponentRef.Name+';');

      If (aNextItemType = xitOther) Then
         Output;
   End Else Begin
      aCellMLComponentRef.Line := Line;

      Output('comp '+aCellMLComponentRef.Name+' incl');

      IncIndent(0);

      Inherited;

      DecIndent(0);

      Output('endcomp;');

      If (aNextItemType <> xitNone) Then
         Output;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.StartCellMLConnection(aCellMLConnection: TCellMLConnection;
                                                              Var aNode: IXMLNode);
Begin
   aCellMLConnection.Line := Line;

   Output('def map between '+aCellMLConnection.Component1+' and '+aCellMLConnection.Component2+' for');

   IncIndent(0);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.EndCellMLConnection(Const aNextItemType: TXMLNextItemType);
Begin
   DecIndent(0);

   Output('enddef;');

   If (aNextItemType <> xitNone) Then
      Output;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLASCIIEngine.CellMLMapVariables(aCellMLMapVariables: TCellMLMapVariables;
                                                           aNode: IXMLNode);
Begin          
   aCellMLMapVariables.Line := Line;

   Output('vars '+aCellMLMapVariables.Variable1+' and '+aCellMLMapVariables.Variable2+';');
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

