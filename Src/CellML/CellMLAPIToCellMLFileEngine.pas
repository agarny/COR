//==============================================================================
// Engine class for converting a CellML API object to a CellML file
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

Unit CellMLAPIToCellMLFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   XMLIntf, CellMLAPIEngine, CellMLAPI, Engine;

//==============================================================================

Type
   TCellMLAPIToCellMLFileEngine = Class(TCellMLAPIEngine)
      Private
         // Properties used for internal purposes

         FileName: String;

         PMessage: PString;

         PXMLDocument: ^IXMLDocument;

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

         Procedure TryAddSameElement(aMathMLEquationBinTree: TMathMLCommandBinTree; aNode: IXMLNode);

         Procedure AddApply(aMathMLEquationBinTree: TMathMLCommandBinTree; aNode: IXMLNode);
         Procedure AddDiff(aMathMLEquationBinTree: TMathMLCommandBinTree; aNode: IXMLNode); Inline;

         Procedure GenerateMathMLEquation(aMathMLEquationBinTree: TMathMLCommandBinTree; aNode: IXMLNode);

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

         Constructor Create(Const aCellMLModel: TCellMLModel; Const aFileName: String; Var aMessage: String);

         // User's methods

         Function Execute: Boolean; Override;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF COR_SPECIFIC}
   CORCommon,
{$ENDIF}
   XMLDoc, Windows, SysUtils, Common, StrUtils;

//==============================================================================

Const
   CELLML_NAMESPACE = 'http://www.cellml.org/cellml/1.0#'; 

//==============================================================================

Constructor TCellMLAPIToCellMLFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                                Const aFileName: String;
                                                Var aMessage: String);
Begin
   Inherited Create(aCellMLModel);

   FileName := aFileName;

   PMessage := @aMessage;
End;

//==============================================================================

Function TCellMLAPIToCellMLFileEngine.Execute: Boolean;
Var
   XMLDocument: IXMLDocument;
Begin
   Result := True;

   XMLDocument := TXMLDocument.Create(Nil);

   Try
      XMLDocument.Active := True;

      Try
         // Initialise the CellML document

         InitXMLDocument(XMLDocument, False);

         // Use the XML document that contains the extra information as a
         // starting point
         // Note: it doesn't matter if there is none...

         CloneXMLDocument(XMLDocument, CellMLModel.Extra.Metadata);

         // Some information about the current CellML file

         XMLDocument.ChildNodes.Insert(1, XMLDocument.CreateNode(CRLF+
                                                                 'This CellML file was generated on '+ReplaceStr(DateTimeToStr(Now), ' ', ' at ')+' using:'+CRLF+
{$IFDEF COR_SPECIFIC}
                                                                 CRLF+
                                                                 COR_NAME+' ('+COR_VERSION+')'+CRLF+
                                                                 COR_FULL_COPYRIGHT+CRLF+
                                                                 COR_URL+' - '+COR_EMAIL+CRLF+
                                                                 CRLF+
                                                                 CELLML_VERSION+' was used to generate this model'+CRLF+
{$ELSE}
                                                                 CRLF+
                                                                 CELLML_VERSION+CRLF+
{$ENDIF}
                                                                 CELLML_URL+CRLF, ntComment));

         // Now, call the parent which will populate the CellML file for us

         PXMLDocument := @XMLDocument;

         Inherited Execute;

         DeleteFile(FileName);

         XMLDocument.SaveToFile(FileName);
      Except
         On E: Exception Do Begin
            PMessage^ := E.Message;

            Result := False;   // Not a valid CellML API
         End;
      End;

      XMLDocument.Active := False;
   Finally
      XMLDocument := Nil;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.StartCellMLModel(aCellMLModel: TCellMLModel;
                                                        Var aNode: IXMLNode);
Begin
   aNode := PXMLDocument^.DocumentElement;

   aNode.Attributes['name']     := aCellMLModel.Name;
   aNode.Attributes['cmeta:id'] := aCellMLModel.Name;

   // Namespaces

   aNode.Attributes['xmlns']        := CELLML_NAMESPACE;
   aNode.Attributes['xmlns:cellml'] := CELLML_NAMESPACE;
   aNode.Attributes['xmlns:cmeta']  := 'http://www.cellml.org/metadata/1.0#';
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.EndCellMLModel;
Begin
   // Nothing to do!
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.StartCellMLUnits(aCellMLUnits: TCellMLUnits;
                                                        Var aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('units', CELLML_NAMESPACE);

   aNode.Attributes['name'] := aCellMLUnits.Name;

   If (aCellMLUnits.BaseUnits) Then
      aNode.Attributes['base_units'] := 'yes';
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.CellMLUnit(aCellMLUnit: TCellMLUnit;
                                                  aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('unit');

   aNode.Attributes['units'] := aCellMLUnit.Name;

   If ((CompareStr(aCellMLUnit.Prefix, '') <> 0) And (CompareStr(aCellMLUnit.Prefix, '0') <> 0)) Then
      aNode.Attributes['prefix'] := aCellMLUnit.Prefix;

   If ((CompareStr(aCellMLUnit.Exponent, '') <> 0) And (StrToFloat(aCellMLUnit.Exponent) <> 1)) Then
      aNode.Attributes['exponent'] := aCellMLUnit.Exponent;

   If ((CompareStr(aCellMLUnit.Multiplier, '') <> 0) And (StrToFloat(aCellMLUnit.Multiplier) <> 1)) Then
      aNode.Attributes['multiplier'] := aCellMLUnit.Multiplier;

   If ((CompareStr(aCellMLUnit.Offset, '') <> 0) And (StrToFloat(aCellMLUnit.Offset) <> 0)) Then
      aNode.Attributes['offset'] := aCellMLUnit.Offset;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.EndCellMLUnits(Const aNextItemType: TXMLNextItemType);
Begin
   // Nothing to do!
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.StartCellMLComponent(aCellMLComponent: TCellMLComponent;
                                                            Var aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('component', CELLML_NAMESPACE);

   aNode.Attributes['name'] := aCellMLComponent.Name;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.EndCellMLComponent(Const aNextItemType: TXMLNextItemType);
Begin
   // Nothing to do!
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.CellMLVariable(aCellMLVariable: TCellMLVariable;
                                                      Const aNextItemType: TXMLNextItemType;
                                                      aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('variable');

   aNode.Attributes['name'] := aCellMLVariable.Name;
   aNode.Attributes['units'] := aCellMLVariable.Units;

   If (CompareStr(aCellMLVariable.InitialValue, '') <> 0) Then
      aNode.Attributes['initial_value'] := aCellMLVariable.InitialValue;

   If ((CompareStr(aCellMLVariable.PublicInterface, '') <> 0) And
       (CompareStr(aCellMLVariable.PublicInterface, 'none') <> 0)) Then
      aNode.Attributes['public_interface'] := aCellMLVariable.PublicInterface;

   If ((CompareStr(aCellMLVariable.PrivateInterface, '') <> 0) And
       (CompareStr(aCellMLVariable.PrivateInterface, 'none') <> 0)) Then
      aNode.Attributes['private_interface'] := aCellMLVariable.PrivateInterface;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.TryAddSameElement(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                         aNode: IXMLNode);
Begin
   If ((aMathMLEquationBinTree.Left.ItemType = aMathMLEquationBinTree.ItemType) And
       ((aMathMLEquationBinTree.ItemType = mitPlus) Or 
        (aMathMLEquationBinTree.ItemType = mitTimes) Or
        (aMathMLEquationBinTree.ItemType = mitAnd) Or
        (aMathMLEquationBinTree.ItemType = mitOr) Or
        (aMathMLEquationBinTree.ItemType = mitXOr))) Then
      // The type of the left element is the same as the current one and is an
      // n-ary element, so...
      // Note: from the MathML specifications (section 4.2.3 of February 21,
      //       2001), n-ary elements are of different types:
      //          arithmetic: plus, times, max, min, gcd, lcm
      //          statistical: mean, sdev, variance, median, mode
      //          logical: and, or, xor
      //          linear: algebra selector
      //          set operator: union, intersect, cartesianproduct
      //          functional: fn, compose

      TryAddSameElement(aMathMLEquationBinTree.Left, aNode)
   Else
      // The type of the left element is not the same as the current one, so...

      GenerateMathMLEquation(aMathMLEquationBinTree.Left, aNode);

   // Deal with the right element, if any...

   If (aMathMLEquationBinTree.Right <> Nil) Then
      // There is a right element, so...

      GenerateMathMLEquation(aMathMLEquationBinTree.Right, aNode);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.AddApply(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('apply');

   Case aMathMLEquationBinTree.ItemType Of
      // Relational operators

      mitEq,
      mitEqEq:
         aNode.AddChild('eq');
      mitNEq:
         aNode.AddChild('neq');
      mitGT:
         aNode.AddChild('gt');
      mitLT:
         aNode.AddChild('lt');
      mitGEq:
         aNode.AddChild('geq');
      mitLEq:
         aNode.AddChild('leq');

      // Arithmetic operators

      mitPlus:
         aNode.AddChild('plus');
      mitMinus:
         aNode.AddChild('minus');
      mitTimes:
         aNode.AddChild('times');
      mitDivide:
         aNode.AddChild('divide');
      mitPow:
         aNode.AddChild('power');
      mitRoot:
         aNode.AddChild('root');
      mitAbs:
         aNode.AddChild('abs');
      mitExp:
         aNode.AddChild('exp');
      mitLN:
         aNode.AddChild('ln');
      mitLog:
         aNode.AddChild('log');
      mitCeil:
         aNode.AddChild('ceiling');
      mitFloor:
         aNode.AddChild('floor');
      mitFact:
         aNode.AddChild('factorial');

      // Logical operators

      mitAnd:
         aNode.AddChild('and');
      mitOr:
         aNode.AddChild('or');
      mitXOr:
         aNode.AddChild('xor');
      mitNot: 
         aNode.AddChild('not');

      // Trigonometric operators

      mitSin:
         aNode.AddChild('sin');
      mitCos:
         aNode.AddChild('cos');
      mitTan:
         aNode.AddChild('tan');
      mitSec:
         aNode.AddChild('sec');
      mitCsc:
         aNode.AddChild('csc');
      mitCot:
         aNode.AddChild('cot');
      mitSinH:
         aNode.AddChild('sinh');
      mitCosH:
         aNode.AddChild('cosh');
      mitTanH:
         aNode.AddChild('tanh');
      mitSecH:
         aNode.AddChild('sech');
      mitCscH:
         aNode.AddChild('csch');
      mitCotH:
         aNode.AddChild('coth');
      mitASin:
         aNode.AddChild('arcsin');
      mitACos:
         aNode.AddChild('arccos');
      mitATan:
         aNode.AddChild('arctan');
      mitASec:
         aNode.AddChild('arcsec');
      mitACsc:
         aNode.AddChild('arccsc');
      mitACot:
         aNode.AddChild('arccot');
      mitASinH:
         aNode.AddChild('arcsinh');
      mitACosH:
         aNode.AddChild('arccosh');
      mitATanH:
         aNode.AddChild('arctanh');
      mitASecH:
         aNode.AddChild('arcsech');
      mitACscH:
         aNode.AddChild('arccsch');
      mitACotH:
         aNode.AddChild('arccoth');
   End;

   // Deal with the operator itself...

   TryAddSameElement(aMathMLEquationBinTree, aNode);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.AddDiff(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                               aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('apply');

   aNode.AddChild('diff');

   GenerateMathMLEquation(aMathMLEquationBinTree.Left, aNode.AddChild('bvar'));
   GenerateMathMLEquation(aMathMLEquationBinTree.Right, aNode);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.GenerateMathMLEquation(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                              aNode: IXMLNode);
   Procedure GenerateMathMLEquationPiecewiseStatement(Var aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                      aNode: IXMLNode);
   Begin
      If (aMathMLEquationBinTree.ItemType = mitPiecewise) Then Begin
         GenerateMathMLEquationPiecewiseStatement(aMathMLEquationBinTree.Left, aNode);

         GenerateMathMLEquation(aMathMLEquationBinTree.Right, aNode);
      End Else
         GenerateMathMLEquation(aMathMLEquationBinTree, aNode);
   End;
Var
   Element: IXMLNode;
   ePos: Integer;
Begin
   // Determine the type of operator we are dealing with

   Case aMathMLEquationBinTree.ItemType Of
      // Token elements

      mitCN: Begin
         aNode := aNode.AddChild('cn');

         ePos := Pos('E', UpperCase(aMathMLEquationBinTree.Str));

         If (ePos <> 0) Then Begin
            aNode.Attributes['type'] := 'e-notation';

            // The node's 'value' must be of the form: Number<sep/>Exponent
            // E.g. 1.23e45 will become 1.23<sep/>45
            // This cannot, however, be done by setting the "NodeValue"
            // property, hence going through the DOM...

            aNode.DOMNode.AppendChild(CreateDOMNode(aNode.DOMNode.OwnerDocument, Copy(aMathMLEquationBinTree.Str, 1, ePos-1), ntText));
            aNode.AddChild('sep');
            aNode.DOMNode.AppendChild(CreateDOMNode(aNode.DOMNode.OwnerDocument, Copy(aMathMLEquationBinTree.Str, ePos+1, Length(aMathMLEquationBinTree.Str)-ePos), ntText));
         End Else
            aNode.NodeValue := aMathMLEquationBinTree.Str;

         aNode.Attributes['cellml:units'] := aMathMLEquationBinTree.Units;
      End;
      mitCI: Begin
         aNode := aNode.AddChild('ci');

         aNode.NodeValue := aMathMLEquationBinTree.Str;
      End;

      // Basic content elements

      mitPiecewise: Begin
         Element := aNode.AddChild('piecewise');

         If (aMathMLEquationBinTree.Left.ItemType = aMathMLEquationBinTree.ItemType) Then
            // The type of the right element is also piecewise, so deal with
            // it in a recursive manner...

            GenerateMathMLEquationPiecewiseStatement(aMathMLEquationBinTree.Left, Element)
         Else
            GenerateMathMLEquation(aMathMLEquationBinTree.Left, Element);

         If (aMathMLEquationBinTree.Right <> Nil) Then
            GenerateMathMLEquation(aMathMLEquationBinTree.Right, Element);
      End;
      mitPiece: Begin
         Element := aNode.AddChild('piece');

         GenerateMathMLEquation(aMathMLEquationBinTree.Left, Element);
         GenerateMathMLEquation(aMathMLEquationBinTree.Right, Element);
      End;
      mitOtherwise:
         GenerateMathMLEquation(aMathMLEquationBinTree.Left, aNode.AddChild('otherwise'));

      // Relational operators

      mitEq, mitEqEq, mitNEq, mitGT, mitLT, mitGEq, mitLEq:
         AddApply(aMathMLEquationBinTree, aNode);

      // Arithmetic operators

      mitPlus:
         // Unirary pluses ought to be ignored. We can allow them within the
         // editor, but not within a CellML file, since it's not in the specs,
         // so...

         If (aMathMLEquationBinTree.Right <> Nil) Then
            AddApply(aMathMLEquationBinTree, aNode)
         Else
            GenerateMathMLEquation(aMathMLEquationBinTree.Left, aNode);

      mitMinus, mitTimes, mitDivide,
      mitPow, mitRoot, mitAbs,
      mitExp, mitLN, mitLog,
      mitCeil, mitFloor, mitFact:
         AddApply(aMathMLEquationBinTree, aNode);

      // Logical operators

      mitAnd, mitOr, mitXOr, mitNot:
         AddApply(aMathMLEquationBinTree, aNode);

      // Calculus elements

      mitDiff:
         AddDiff(aMathMLEquationBinTree, aNode);

      // Qualifier elements

      mitDegree:
         GenerateMathMLEquation(aMathMLEquationBinTree.Left, aNode.AddChild('degree'));
      mitLogBase:
         GenerateMathMLEquation(aMathMLEquationBinTree.Left, aNode.AddChild('logbase'));

      // Trigonometric operators

      mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot,
      mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH,
      mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
      mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH:
         AddApply(aMathMLEquationBinTree, aNode);

      // Constants

      mitTrue:
         aNode := aNode.AddChild('true');
      mitFalse:
         aNode := aNode.AddChild('false');
      mitPI:
         aNode := aNode.AddChild('pi');
      mitExponentiale:
         aNode := aNode.AddChild('exponentiale');
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.StartMathMLEquation(Var aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('math', 'http://www.w3.org/1998/Math/MathML');
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.MathMLEquation(aMathMLEquation: TMathMLEquation;
                                                      Const aNextItemType: TXMLNextItemType;
                                                      Var aNode: IXMLNode);
Begin
   GenerateMathMLEquation(aMathMLEquation.MathMLEquationBinTree, aNode);
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.StartCellMLGroup(aCellMLGroup: TCellMLGroup;
                                                        Var aNode: IXMLNode);
Var
   RelationshipRef: IXMLNode;
Begin
   aNode := aNode.AddChild('group', CELLML_NAMESPACE);

   If (aCellMLGroup.Encapsulation) Then Begin
      RelationshipRef := aNode.AddChild('relationship_ref');

      RelationshipRef.Attributes['relationship'] := 'encapsulation';
   End;

   If (aCellMLGroup.Containment) Then Begin
      RelationshipRef := aNode.AddChild('relationship_ref');

      RelationshipRef.Attributes['relationship'] := 'containment';

      If (CompareStr(aCellMLGroup.Name, '') <> 0) Then
         RelationshipRef.Attributes['name'] := aCellMLGroup.Name;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.EndCellMLGroup(Const aNextItemType: TXMLNextItemType);
Begin
   // Nothing to do!
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.CellMLComponentRef(aCellMLComponentRef: TCellMLComponentRef;
                                                          Const aNextItemType: TXMLNextItemType;
                                                          aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('component_ref');

   aNode.Attributes['component'] := aCellMLComponentRef.Name;

   Inherited;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.StartCellMLConnection(aCellMLConnection: TCellMLConnection;
                                                             Var aNode: IXMLNode);
Var
   Components: IXMLNode;
Begin
   aNode := aNode.AddChild('connection', CELLML_NAMESPACE);

   Components := aNode.AddChild('map_components');

   Components.Attributes['component_1'] := aCellMLConnection.Component1;
   Components.Attributes['component_2'] := aCellMLConnection.Component2;
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.EndCellMLConnection(Const aNextItemType: TXMLNextItemType);
Begin
   // Nothing to do!
End;

//==============================================================================

Procedure TCellMLAPIToCellMLFileEngine.CellMLMapVariables(aCellMLMapVariables: TCellMLMapVariables;
                                                          aNode: IXMLNode);
Begin
   aNode := aNode.AddChild('map_variables');

   aNode.Attributes['variable_1'] := aCellMLMapVariables.Variable1;
   aNode.Attributes['variable_2'] := aCellMLMapVariables.Variable2;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
