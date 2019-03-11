//==============================================================================
// Engine class for converting a CellML file to a CellML API object
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

Unit CellMLFileToCellMLAPIEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, XMLIntf;

//==============================================================================

Type
   TCellMLFileToCellMLAPIEngine = Class
      Private
         // Properties used for internal purposes

         FileName: String;
         
         CellMLModel: TCellMLModel;

         AssignmentDone: Boolean;

         PMessage: PString;

         // Methods used for internal purposes

         Procedure ExtractUnknownElement(aXMLNode: IXMLNode); Inline;

         Procedure ParseCellMLDocument(aXMLNode: IXMLNode);
         Procedure ParseCellMLModel(aXMLNode: IXMLNode);
         Procedure ParseCellMLUnits(aXMLNode: IXMLNode; aCellMLComponent: TCellMLComponent);
         Procedure ParseCellMLUnit(aXMLNode: IXMLNode; aCellMLUnits: TCellMLUnits);
         Procedure ParseCellMLComponent(aXMLNode: IXMLNode; aCellMLComponent: TCellMLComponent);
         Procedure ParseMathMLEquation(aXMLNode: IXMLNode; Var aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAlreadyPiecewiseStatement: Boolean; Var aErrorMsg: String);
         Procedure ParseCellMLGroup(aXMLNode: IXMLNode; aCellMLGroup: TCellMLGroup);
         Procedure ParseCellMLComponentRef(aXMLNode: IXMLNode; aCellMLComponentRef: TCellMLComponentRef);
         Procedure ParseCellMLConnection(aXMLNode: IXMLNode; aCellMLConnection: TCellMLConnection);

      Public
         // Constructor & Destructor

         Constructor Create(Const aFileName: String; Const aCellMLModel: TCellMLModel; Var aMessage: String);

         // User's methods

         Function Execute: Boolean;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   XMLDoc, SysUtils, Common;

//==============================================================================

Constructor TCellMLFileToCellMLAPIEngine.Create(Const aFileName: String;
                                                Const aCellMLModel: TCellMLModel;
                                                Var aMessage: String);
Begin
   FileName := aFileName;

   CellMLModel := aCellMLModel;

   PMessage := @aMessage;
End;

//==============================================================================

Function TCellMLFileToCellMLAPIEngine.Execute: Boolean;
   Procedure StripXMLAndComments(aXMLNode: IXMLNode);
   Var
      I: Integer;
   Begin
      For I := 0 To aXMLNode.ChildNodes.Count-1 Do
         StripXMLAndComments(aXMLNode.ChildNodes[I]);

      If ((CompareStr(aXMLNode.NodeName, 'xml') = 0) Or
          (CompareStr(aXMLNode.NodeName, '#comment') = 0)) Then
         aXMLNode.ParentNode.DOMNode.removeChild(aXMLNode.DOMNode);
   End;
Var
   XMLDocument: IXMLDocument;
   I: Integer;
Begin
   Result := True;

   XMLDocument := TXMLDocument.Create(Nil);

   Try
      Try
         XMLDocument.FileName := FileName;

         XMLDocument.Active := True;

         // Strip the CellML file of its initial "xml" node and of all its
         // "#comment" nodes (at ANY level)

         StripXMLAndComments(XMLDocument.Node);

         XMLDocument.Resync;

         // Try to convert the CellML file into CellML objects

         For I := 0 To XMLDocument.Node.ChildNodes.Count-1 Do
            ParseCellMLDocument(XMLDocument.Node.ChildNodes[I]);

         XMLDocument.Active := False;
      Except
         On E: Exception Do Begin
            PMessage^ := E.Message;

            Result := False;   // Not a valid CellML file
         End;
      End;
   Finally
      XMLDocument := Nil;
   End;
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ExtractUnknownElement(aXMLNode: IXMLNode);
Begin
   CellMLModel.Extra.Metadata.DocumentElement.ChildNodes.Add(aXMLNode.CloneNode(True));
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLDocument(aXMLNode: IXMLNode);
Var
   I: Integer;
Begin
   If (CompareStr(aXMLNode.NodeName, 'model') = 0) Then Begin
      CellMLModel.Name := aXMLNode.AttributeNodes.Nodes['name'].Text;

      For I := 0 To aXMLNode.ChildNodes.Count-1 Do
         ParseCellMLModel(aXMLNode.ChildNodes[I]);

      // Model attributes not known to COR

      For I := 0 To aXMLNode.AttributeNodes.Count-1 Do
         If ((CompareStr(aXMLNode.AttributeNodes[I].NodeName, 'name')         <> 0) And
             (CompareStr(aXMLNode.AttributeNodes[I].NodeName, 'cmeta:id')     <> 0) And
             (CompareStr(aXMLNode.AttributeNodes[I].NodeName, 'xmlns')        <> 0) And
             (CompareStr(aXMLNode.AttributeNodes[I].NodeName, 'xmlns:cellml') <> 0) And
             (CompareStr(aXMLNode.AttributeNodes[I].NodeName, 'xmlns:cmeta')  <> 0)) Then
            // The attribute is unknown, so add it to the extra XML document...

            CellMLModel.Extra.Metadata.DocumentElement.Attributes[aXMLNode.AttributeNodes[I].NodeName] := aXMLNode.AttributeNodes[I].NodeValue;
   End Else
      // Uknown element, so...

      ExtractUnknownElement(aXMLNode);
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLModel(aXMLNode: IXMLNode);
Var
   I: Integer;
   CellMLComponent: TCellMLComponent;
   CellMLGroup: TCellMLGroup;
   CellMLConnection: TCellMLConnection;
Begin
   If (CompareStr(aXMLNode.NodeName, 'units') = 0) Then
      ParseCellMLUnits(aXMLNode, Nil)
   Else If (CompareStr(aXMLNode.NodeName, 'component') = 0) Then Begin
      CellMLComponent := TCellMLComponent.Create(CellMLModel);

      CellMLModel.ComponentList.Add([CellMLComponent]);

      CellMLComponent.Name := aXMLNode.AttributeNodes.Nodes['name'].Text;

      For I := 0 To aXMLNode.ChildNodes.Count-1 Do
         ParseCellMLComponent(aXMLNode.ChildNodes[I], CellMLComponent);
   End Else If (CompareStr(aXMLNode.NodeName, 'group') = 0) Then Begin
      CellMLGroup := TCellMLGroup.Create(CellMLModel);

      CellMLModel.GroupList.Add([CellMLGroup]);

      For I := 0 To aXMLNode.ChildNodes.Count-1 Do
         ParseCellMLGroup(aXMLNode.ChildNodes[I], CellMLGroup);
   End Else If (CompareStr(aXMLNode.NodeName, 'connection') = 0) Then Begin
      CellMLConnection := TCellMLConnection.Create(CellMLModel);

      CellMLModel.ConnectionList.Add([CellMLConnection]);

      For I := 0 To aXMLNode.ChildNodes.Count-1 Do
         ParseCellMLConnection(aXMLNode.ChildNodes[I], CellMLConnection);
   End Else
      // Uknown element, so...

      ExtractUnknownElement(aXMLNode);
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLUnits(aXMLNode: IXMLNode;
                                                        aCellMLComponent: TCellMLComponent);
Var
   I: Integer;
   CellMLUnits: TCellMLUnits;
Begin
   If (aCellMLComponent <> Nil) Then Begin
      CellMLUnits := TCellMLUnits.Create(Nil, aCellMLComponent, aXMLNode.AttributeNodes.Nodes['name'].Text);

      aCellMLComponent.UnitsList.Add([CellMLUnits]);
   End Else Begin
      CellMLUnits := TCellMLUnits.Create(CellMLModel, Nil, aXMLNode.AttributeNodes.Nodes['name'].Text);

      CellMLModel.UnitsList.Add([CellMLUnits]);
   End;

   If ((CompareStr(aXMLNode.AttributeNodes.Nodes['base_units'].Text, '') <> 0) And
       (CompareStr(aXMLNode.AttributeNodes.Nodes['base_units'].Text, 'yes') <> 0) And
       (CompareStr(aXMLNode.AttributeNodes.Nodes['base_units'].Text, 'no') <> 0)) Then
      Raise Exception.Create('the ''base_units'' attribute must have a value equal to ''yes'' or ''no''.')
   Else Begin
      CellMLUnits.BaseUnits := CompareStr(aXMLNode.AttributeNodes.Nodes['base_units'].Text, 'yes') = 0;

      If (CellMLUnits.BaseUnits) Then
         CellMLUnits.AddUnitElement([TCellMLUnit.Create(Nil, CellMLUnits.Name, '0', '1', '1', '0')]);
         // Note: a user-defined base unit requires one unit element to make
         //       units checking work, so... (see "AddStandardUnit" in the
         //       "CellMLAPI" unit)
   End;

   For I := 0 To aXMLNode.ChildNodes.Count-1 Do
      ParseCellMLUnit(aXMLNode.ChildNodes[I], CellMLUnits);
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLUnit(aXMLNode: IXMLNode;
                                                       aCellMLUnits: TCellMLUnits);
Var
   CellMLUnit: TCellMLUnit;
   DummyDblNbVal: Double;
Begin
   If (CompareStr(aXMLNode.NodeName, 'unit') = 0) Then Begin
      // Definition of a unit, so initialise it and add it to the units

      CellMLUnit := TCellMLUnit.Create(aCellMLUnits, aXMLNode.AttributeNodes.Nodes['units'].Text);

      aCellMLUnits.AddUnitElement([CellMLUnit]);

      If (CompareStr(aXMLNode.AttributeNodes.Nodes['prefix'].Text, '') <> 0) Then Begin
         If (TryStrToFloat(aXMLNode.AttributeNodes.Nodes['prefix'].Text, DummyDblNbVal)) Then
            CellMLUnit.Prefix := SimplifyNbStr(aXMLNode.AttributeNodes.Nodes['prefix'].Text)
         Else
            CellMLUnit.Prefix := aXMLNode.AttributeNodes.Nodes['prefix'].Text;
      End;

      If (CompareStr(aXMLNode.AttributeNodes.Nodes['exponent'].Text, '') <> 0) Then
         CellMLUnit.Exponent := SimplifyNbStr(aXMLNode.AttributeNodes.Nodes['exponent'].Text);

      If (CompareStr(aXMLNode.AttributeNodes.Nodes['multiplier'].Text, '') <> 0) Then
         CellMLUnit.Multiplier := SimplifyNbStr(aXMLNode.AttributeNodes.Nodes['multiplier'].Text);

      If (CompareStr(aXMLNode.AttributeNodes.Nodes['offset'].Text, '') <> 0) Then
         CellMLUnit.Offset := SimplifyNbStr(aXMLNode.AttributeNodes.Nodes['offset'].Text);
   End Else
      // Uknown element, so...

      ExtractUnknownElement(aXMLNode);
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLComponent(aXMLNode: IXMLNode;
                                                            aCellMLComponent: TCellMLComponent);
Var
   I: Integer;
   CellMLVariable: TCellMLVariable;
   MathMLEquation: TMathMLEquation;
   ErrorMsg, ErrorMsgExtra: String;
Begin
   If (CompareStr(aXMLNode.NodeName, 'units') = 0) Then
      ParseCellMLUnits(aXMLNode, aCellMLComponent)
   Else If (CompareStr(aXMLNode.NodeName, 'variable') = 0) Then Begin
      CellMLVariable := TCellMLVariable.Create(aCellMLComponent);

      aCellMLComponent.VariableList.Add([CellMLVariable]);

      CellMLVariable.Name  := aXMLNode.AttributeNodes.Nodes['name'].Text;
      CellMLVariable.Units := aXMLNode.AttributeNodes.Nodes['units'].Text;

      If (CompareStr(aXMLNode.AttributeNodes.Nodes['initial_value'].Text, '') <> 0) Then
         CellMLVariable.InitialValue := SimplifyNbStr(aXMLNode.AttributeNodes.Nodes['initial_value'].Text);

      If (CompareStr(aXMLNode.AttributeNodes.Nodes['public_interface'].Text, '') <> 0) Then
         CellMLVariable.PublicInterface := aXMLNode.AttributeNodes.Nodes['public_interface'].Text;

      If (CompareStr(aXMLNode.AttributeNodes.Nodes['private_interface'].Text, '') <> 0) Then
         CellMLVariable.PrivateInterface := aXMLNode.AttributeNodes.Nodes['private_interface'].Text;
   End Else If (CompareStr(aXMLNode.NodeName, 'math') = 0) Then
      For I := 0 To aXMLNode.ChildNodes.Count-1 Do Begin
         MathMLEquation := TMathMLEquation.Create(aCellMLComponent);

         AssignmentDone := False;

         ParseMathMLEquation(aXMLNode.ChildNodes[I], MathMLEquation.MathMLEquationBinTree, False, ErrorMsg);

         If (CompareStr(ErrorMsg, '') <> 0) Then Begin
            ErrorMsgExtra := '';

            if ((MathMLEquation.MathMLEquationBinTree <> Nil) And
                (MathMLEquation.MathMLEquationBinTree.Left <> Nil)) Then Begin
               If (MathMLEquation.MathMLEquationBinTree.Left.ItemType = mitDiff) Then
                  ErrorMsgExtra := ErrorMsgExtra+'the '''+MathMLEquation.MathMLEquationBinTree.Left.Right.Str+''' ordinary differential equation of '
               Else
                  ErrorMsgExtra := ErrorMsgExtra+'the '''+MathMLEquation.MathMLEquationBinTree.Left.Str+''' algebraic equation of ';
            End;

            ErrorMsgExtra := ErrorMsgExtra+'the '''+aCellMLComponent.Name+''' component';

            MathMLEquation.Free;

            Raise Exception.Create(Format(ErrorMsg, [ErrorMsgExtra]));
         End Else If (MathMLEquation.MathMLEquationBinTree = Nil) Then
            MathMLEquation.Free
         Else
            aCellMLComponent.EquationList.Add([MathMLEquation]);
      End
   Else If (CompareStr(aXMLNode.NodeName, 'reaction') = 0) Then
      Raise Exception.Create('this model appears to contain reactions, a feature that is not supported by COR.')
   Else
      // Uknown element, so...

      ExtractUnknownElement(aXMLNode);
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseMathMLEquation(aXMLNode: IXMLNode;
                                                           Var aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                           Const aAlreadyPiecewiseStatement: Boolean;
                                                           Var aErrorMsg: String);
Var
   SubMathMLEquationBinTree: TMathMLCommandBinTree;
   I: Integer;
   CNType: String;
   eNumber: String;
Begin
   If (CompareStr(aErrorMsg, '') <> 0) Then
      Exit;

   //---GRY--- THE FOLLOWING ELEMENTS ARE NOT CURRENTLY TREATED: "degree" (for
   //          "diff", not "root"), "notanumber", "infinity", "semantics",
   //          "annotation" and "annotation-xml". I, INDEED, CANNOT SEE ANY GOOD
   //          REASON FOR HAVING THEM (FOR CARDIAC ELECTROPHYSIOLOGICAL
   //          MODELLING AT LEAST), SO...

   // Token elements

   If (CompareStr(aXMLNode.NodeName, 'cn') = 0) Then Begin
      CNType := aXMLNode.AttributeNodes.Nodes['type'].Text;

      If ((CompareStr(CNType, '') = 0) Or (CompareStr(CNType, 'real') = 0)) Then
         // Either no type (i.e. real type by default) or real type

         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(Trim(aXMLNode.NodeValue)), aXMLNode.AttributeNodes.Nodes['cellml:units'].Text)
      Else If (CompareStr(CNType, 'e-notation') = 0) Then Begin
         // E-notation type

         If (aXMLNode.ChildNodes.Count <> 3) Then
            Raise Exception.Create('a '''+aXMLNode.NodeName+''' element with a '''+CNType+''' type must have three children.')
         Else Begin
            If (aXMLNode.ChildNodes.Get(0).NodeType <> ntText) Then
               Raise Exception.Create('the first child of a '''+aXMLNode.NodeName+''' element with a '''+CNType+''' type must be of type ''ntText''.')
            Else If (aXMLNode.ChildNodes.Get(1).NodeType <> ntElement) Then
               Raise Exception.Create('the second child of a '''+aXMLNode.NodeName+''' element with a '''+CNType+''' type must be of type ''ntElement''.')
            Else If (aXMLNode.ChildNodes.Get(1).NodeName <> 'sep') Then
               Raise Exception.Create('the name of the second child of a '''+aXMLNode.NodeName+''' element with a '''+CNType+''' type must be ''sep''.')
            Else If (aXMLNode.ChildNodes.Get(2).NodeType <> ntText) Then
               Raise Exception.Create('the third child of a '''+aXMLNode.NodeName+''' element with a '''+CNType+''' type must be of type ''ntText''.')
            Else Begin
               eNumber := Trim(aXMLNode.ChildNodes.Get(0).NodeValue)+'e'+Trim(aXMLNode.ChildNodes.Get(2).NodeValue);

               aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, SimplifyNbStr(Trim(eNumber)), aXMLNode.AttributeNodes.Nodes['cellml:units'].Text)
            End;
         End;
      End Else If ((CompareStr(CNType, 'integer') = 0)           Or
                   (CompareStr(CNType, 'rational') = 0)          Or
                   (CompareStr(CNType, 'complex-polar') = 0)     Or
                   (CompareStr(CNType, 'complex-cartesian') = 0) Or
                   (CompareStr(CNType, 'constant') = 0)) Then
         // A known, but unsupported type

         Raise Exception.Create('the '''+CNType+''' type of the '''+aXMLNode.NodeName+''' element is not supported.')
      Else
         // Unknown type

         Raise Exception.Create('an unknown type ('''+CNType+''') is used by a '''+aXMLNode.NodeName+''' element.')
   End Else If (CompareStr(aXMLNode.NodeName, 'ci') = 0) Then
      aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, Trim(aXMLNode.NodeValue))

   // Basic content elements

   Else If (CompareStr(aXMLNode.NodeName, 'apply') = 0) Then Begin
      If (aXMLNode.ChildNodes.Count <= 3) Then Begin
         // Case where there are either 1, 2 or 3 child nodes, e.g. "pi",
         // "sin(a)" or "a+b", respectively. These will translate into:
         //
         // pi,  sin   or   +
         //      / \       / \
         //     a  Nil    a   b

         If (aXMLNode.ChildNodes.Count = 0) Then
            aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) must have at least one child.'
         Else Begin
            ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree, aAlreadyPiecewiseStatement, aErrorMsg);

            If (aXMLNode.ChildNodes.Count >= 2) Then Begin
               ParseMathMLEquation(aXMLNode.ChildNodes[1], aMathMLEquationBinTree.Left, aAlreadyPiecewiseStatement, aErrorMsg);

               If (aXMLNode.ChildNodes.Count = 3) Then
                  ParseMathMLEquation(aXMLNode.ChildNodes[2], aMathMLEquationBinTree.Right, aAlreadyPiecewiseStatement, aErrorMsg);
            End;
         End;
      End Else Begin
         // Case where there are more than 3 child nodes, e.g. "a+b+c+d+e". This
         // will translate into:
         //
         //   +
         //  / \
         // a   +
         //    / \
         //   b   +
         //      / \
         //     c   +
         //        / \
         //       d   e

         ParseMathMLEquation(aXMLNode.ChildNodes[0], SubMathMLEquationBinTree, aAlreadyPiecewiseStatement, aErrorMsg);
         ParseMathMLEquation(aXMLNode.ChildNodes[aXMLNode.ChildNodes.Count-2], SubMathMLEquationBinTree.Left, aAlreadyPiecewiseStatement, aErrorMsg);
         ParseMathMLEquation(aXMLNode.ChildNodes[aXMLNode.ChildNodes.Count-1], SubMathMLEquationBinTree.Right, aAlreadyPiecewiseStatement, aErrorMsg);

         For I := aXMLNode.ChildNodes.Count-3 DownTo 1 Do Begin
            ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree, aAlreadyPiecewiseStatement, aErrorMsg);
            ParseMathMLEquation(aXMLNode.ChildNodes[I], aMathMLEquationBinTree.Left, aAlreadyPiecewiseStatement, aErrorMsg);

            If (CompareStr(aErrorMsg, '') = 0) Then Begin
               aMathMLEquationBinTree.Right := SubMathMLEquationBinTree;

               SubMathMLEquationBinTree := aMathMLEquationBinTree;
            End Else Begin
               // There has been a problem, so release the memory and exit the
               // loop

               SubMathMLEquationBinTree.Free;

               Break;
            End;
         End;
      End;
   End Else If (CompareStr(aXMLNode.NodeName, 'piecewise') = 0) Then Begin
      If (aAlreadyPiecewiseStatement) Then
         // Cannot accept nested piecewise statements!!

         aErrorMsg := 'nested piecewise statements (in %S) are not allowed.'

      Else If ((aXMLNode.ParentNode.ChildNodes.Count <> 3) Or
          ((aXMLNode.ParentNode.ChildNodes.Count = 3) And (CompareStr(aXMLNode.ParentNode.ChildNodes[0].NodeName, 'eq') <> 0))) Then
         // Only accept piecewise statements at the top level of the RHS of an
         // equation

         aErrorMsg := 'non top-level piecewise statements (in %S) are not allowed.'

      // See "apply" for an explanation of the (similar) algorithm used here

      Else If (aXMLNode.ChildNodes.Count <= 2) Then Begin
         If (aXMLNode.ChildNodes.Count = 0) Then
            aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) must have at least one child.'
         Else Begin
            aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitPiecewise);

            ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree.Left, True, aErrorMsg);

            If (aXMLNode.ChildNodes.Count = 2) Then
               ParseMathMLEquation(aXMLNode.ChildNodes[1], aMathMLEquationBinTree.Right, True, aErrorMsg);
         End;
      End Else Begin
         SubMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitPiecewise);

         ParseMathMLEquation(aXMLNode.ChildNodes[0], SubMathMLEquationBinTree.Left, True, aErrorMsg);
         ParseMathMLEquation(aXMLNode.ChildNodes[1], SubMathMLEquationBinTree.Right, True, aErrorMsg);

         For I := 2 To aXMLNode.ChildNodes.Count-1 Do Begin
            aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitPiecewise);

            ParseMathMLEquation(aXMLNode.ChildNodes[I], aMathMLEquationBinTree.Right, True, aErrorMsg);

            If (CompareStr(aErrorMsg, '') = 0) Then Begin
               aMathMLEquationBinTree.Left := SubMathMLEquationBinTree;

               SubMathMLEquationBinTree := aMathMLEquationBinTree;
            End Else Begin
               // There has been a problem, so release the memory and exit the
               // loop

               SubMathMLEquationBinTree.Free;

               Break;
            End;
         End;
      End;
   End Else If (CompareStr(aXMLNode.NodeName, 'piece') = 0) Then Begin
      If (aXMLNode.ChildNodes.Count <> 2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) must have two children.'
      Else Begin
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitPiece);

         ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree.Left, aAlreadyPiecewiseStatement, aErrorMsg);
         ParseMathMLEquation(aXMLNode.ChildNodes[1], aMathMLEquationBinTree.Right, aAlreadyPiecewiseStatement, aErrorMsg);
      End;
   End Else If (CompareStr(aXMLNode.NodeName, 'otherwise') = 0) Then Begin
      If (aXMLNode.ChildNodes.Count <> 1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) must have one child.'
      Else Begin
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitOtherwise);

         ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree.Left, aAlreadyPiecewiseStatement, aErrorMsg);
      End

   // Relational operators

   End Else If (CompareStr(aXMLNode.NodeName, 'eq') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires two operands.'
      Else If (Not AssignmentDone) Then Begin
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitEq);

         AssignmentDone := True;
      End Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitEqEq);
   End Else If (CompareStr(aXMLNode.NodeName, 'neq') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitNEq);
   End Else If (CompareStr(aXMLNode.NodeName, 'gt') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitGT);
   End Else If (CompareStr(aXMLNode.NodeName, 'lt') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitLT);
   End Else If (CompareStr(aXMLNode.NodeName, 'geq') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitGEq);
   End Else If (CompareStr(aXMLNode.NodeName, 'leq') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitLEq);

   // Arithmetic operators

   End Else If (CompareStr(aXMLNode.NodeName, 'plus') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count < 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires at least one operand.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitPlus);
   End Else If (CompareStr(aXMLNode.NodeName, 'minus') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count < 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires at least one operand.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitMinus);
   End Else If (CompareStr(aXMLNode.NodeName, 'times') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count < 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires at least two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitTimes);
   End Else If (CompareStr(aXMLNode.NodeName, 'divide') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count < 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires at least two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitDivide);
   End Else If (CompareStr(aXMLNode.NodeName, 'power') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires two arguments.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitPow);
   End Else If (CompareStr(aXMLNode.NodeName, 'root') = 0) Then Begin
      If ((aXMLNode.ParentNode.ChildNodes.Count <> 1+1) And
          (aXMLNode.ParentNode.ChildNodes.Count <> 1+2)) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one or two arguments.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitRoot);
   End Else If (CompareStr(aXMLNode.NodeName, 'abs') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitAbs);
   End Else If (CompareStr(aXMLNode.NodeName, 'exp') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitExp);
   End Else If (CompareStr(aXMLNode.NodeName, 'ln') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitLN);
   End Else If (CompareStr(aXMLNode.NodeName, 'log') = 0) Then Begin
      If ((aXMLNode.ParentNode.ChildNodes.Count <> 1+1) And
          (aXMLNode.ParentNode.ChildNodes.Count <> 1+2)) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one or two arguments.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitLog);
   End Else If (CompareStr(aXMLNode.NodeName, 'ceiling') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitCeil);
   End Else If (CompareStr(aXMLNode.NodeName, 'floor') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitFloor);
   End Else If (CompareStr(aXMLNode.NodeName, 'factorial') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitFact);

   // Logical operators

   End Else If (CompareStr(aXMLNode.NodeName, 'and') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count < 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires at least two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitAnd);
   End Else If (CompareStr(aXMLNode.NodeName, 'or') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count < 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires at least two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitOr);
   End Else If (CompareStr(aXMLNode.NodeName, 'xor') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count < 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires at least two operands.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitXOr);
   End Else If (CompareStr(aXMLNode.NodeName, 'not') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' operator (in %S) requires one operand.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitNot);

   // Calculus elements

   End Else If (CompareStr(aXMLNode.NodeName, 'diff') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+2) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) requires two arguments.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitDiff);

   // Qualifier elements

   End Else If (CompareStr(aXMLNode.NodeName, 'degree') = 0) Then Begin
      If (aXMLNode.ChildNodes.Count <> 1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) must have one child.'
      Else Begin
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitDegree);

         ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree.Left, aAlreadyPiecewiseStatement, aErrorMsg);
      End;
   End Else If (CompareStr(aXMLNode.NodeName, 'bvar') = 0) Then Begin
      If (aXMLNode.ChildNodes.Count <> 1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) must have one child.'
      Else
         ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree, aAlreadyPiecewiseStatement, aErrorMsg);
   End Else If (CompareStr(aXMLNode.NodeName, 'logbase') = 0) Then Begin
      If (aXMLNode.ChildNodes.Count <> 1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S) must have one child.'
      Else Begin
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitLogBase);

         ParseMathMLEquation(aXMLNode.ChildNodes[0], aMathMLEquationBinTree.Left, aAlreadyPiecewiseStatement, aErrorMsg);
      End;

   // Trigonometric operators

   End Else If (CompareStr(aXMLNode.NodeName, 'sin') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitSin);
   End Else If (CompareStr(aXMLNode.NodeName, 'cos') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitCos);
   End Else If (CompareStr(aXMLNode.NodeName, 'tan') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitTan);
   End Else If (CompareStr(aXMLNode.NodeName, 'sec') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitSec);
   End Else If (CompareStr(aXMLNode.NodeName, 'csc') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitCsc);
   End Else If (CompareStr(aXMLNode.NodeName, 'cot') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitCot);
   End Else If (CompareStr(aXMLNode.NodeName, 'sinh') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitSinH);
   End Else If (CompareStr(aXMLNode.NodeName, 'cosh') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitCosH);
   End Else If (CompareStr(aXMLNode.NodeName, 'tanh') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitTanH);
   End Else If (CompareStr(aXMLNode.NodeName, 'sech') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitSecH);
   End Else If (CompareStr(aXMLNode.NodeName, 'csch') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitCscH);
   End Else If (CompareStr(aXMLNode.NodeName, 'coth') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitCotH);
   End Else If (CompareStr(aXMLNode.NodeName, 'arcsin') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitASin);
   End Else If (CompareStr(aXMLNode.NodeName, 'arccos') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitACos);
   End Else If (CompareStr(aXMLNode.NodeName, 'arctan') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitATan);
   End Else If (CompareStr(aXMLNode.NodeName, 'arcsec') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitASec);
   End Else If (CompareStr(aXMLNode.NodeName, 'arccsc') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitACsc);
   End Else If (CompareStr(aXMLNode.NodeName, 'arccot') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitACot);
   End Else If (CompareStr(aXMLNode.NodeName, 'arcsinh') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitASinH);
   End Else If (CompareStr(aXMLNode.NodeName, 'arccosh') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitACosH);
   End Else If (CompareStr(aXMLNode.NodeName, 'arctanh') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitATanH);
   End Else If (CompareStr(aXMLNode.NodeName, 'arcsech') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitASecH);
   End Else If (CompareStr(aXMLNode.NodeName, 'arccsch') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitACscH);
   End Else If (CompareStr(aXMLNode.NodeName, 'arccoth') = 0) Then Begin
      If (aXMLNode.ParentNode.ChildNodes.Count <> 1+1) Then
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' function (in %S) requires one argument.'
      Else
         aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitACotH);

   // Constants

   End Else If (CompareStr(aXMLNode.NodeName, 'true') = 0) Then
      aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitTrue)
   Else If (CompareStr(aXMLNode.NodeName, 'false') = 0) Then
      aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitFalse)
   Else If (CompareStr(aXMLNode.NodeName, 'pi') = 0) Then
      aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitPI)
   Else If (CompareStr(aXMLNode.NodeName, 'exponentiale') = 0) Then
      aMathMLEquationBinTree := TMathMLCommandBinTree.Create(FileName, mitExponentiale)

   // Unknown element

   Else
      Try
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' element (in %S and which value is '''+Trim(aXMLNode.NodeValue)+''') is not recognised.';
      Except
         aErrorMsg := 'the '''+aXMLNode.NodeName+''' '+CRLF+'element (in %S) is not recognised.';
      End;
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLGroup(aXMLNode: IXMLNode;
                                                        aCellMLGroup: TCellMLGroup);
Var
   I: Integer;
   CellMLComponentRef: TCellMLComponentRef;
Begin
   If (CompareStr(aXMLNode.NodeName, 'relationship_ref') = 0) Then Begin
      If (CompareStr(aXMLNode.AttributeNodes.Nodes['relationship'].Text, 'encapsulation') = 0) Then Begin
         aCellMLGroup.Encapsulation := True;

         If (CompareStr(aXMLNode.AttributeNodes.Nodes['name'].Text, '') <> 0) Then
            Raise Exception.Create('a group of ''encapsulation'' type cannot have a name.');
      End Else If (CompareStr(aXMLNode.AttributeNodes.Nodes['relationship'].Text, 'containment') = 0) Then Begin
         aCellMLGroup.Containment := True;
         aCellMLGroup.Name        := aXMLNode.AttributeNodes.Nodes['name'].Text;
      End Else
         Raise Exception.Create('the ''relationship'' attribute must have a value equal to ''encapsulation'' or ''containment''.');
   End Else If (CompareStr(aXMLNode.NodeName, 'component_ref') = 0) Then Begin
      CellMLComponentRef := TCellMLComponentRef.Create(aCellMLGroup);

      aCellMLGroup.ComponentRefList.Add([CellMLComponentRef]);

      CellMLComponentRef.Name := aXMLNode.AttributeNodes.Nodes['component'].Text;

      For I := 0 To aXMLNode.ChildNodes.Count-1 Do
         ParseCellMLComponentRef(aXMLNode.ChildNodes[I], CellMLComponentRef);
   End Else
      // Uknown element, so...

      ExtractUnknownElement(aXMLNode);
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLComponentRef(aXMLNode: IXMLNode;
                                                               aCellMLComponentRef: TCellMLComponentRef);
Var
   I: Integer;
   CellMLComponentRef: TCellMLComponentRef;
Begin
   If (CompareStr(aXMLNode.NodeName, 'component_ref') = 0) Then Begin
      CellMLComponentRef := TCellMLComponentRef.Create(aCellMLComponentRef);

      aCellMLComponentRef.ComponentRefList.Add([CellMLComponentRef]);

      CellMLComponentRef.Name := aXMLNode.AttributeNodes.Nodes['component'].Text;

      For I := 0 To aXMLNode.ChildNodes.Count-1 Do
         ParseCellMLComponentRef(aXMLNode.ChildNodes[I], CellMLComponentRef);
   End Else
      // Uknown element, so...

      ExtractUnknownElement(aXMLNode);
End;

//==============================================================================

Procedure TCellMLFileToCellMLAPIEngine.ParseCellMLConnection(aXMLNode: IXMLNode;
                                                             aCellMLConnection: TCellMLConnection);
Var
   CellMLMapVariables: TCellMLMapVariables;
Begin
   If (CompareStr(aXMLNode.NodeName, 'map_components') = 0) Then Begin
      aCellMLConnection.Component1 := aXMLNode.AttributeNodes.Nodes['component_1'].Text;
      aCellMLConnection.Component2 := aXMLNode.AttributeNodes.Nodes['component_2'].Text;
   End Else If (CompareStr(aXMLNode.NodeName, 'map_variables') = 0) Then Begin
      CellMLMapVariables := TCellMLMapVariables.Create(aCellMLConnection,
                                                       aXMLNode.AttributeNodes.Nodes['variable_1'].Text,
                                                       aXMLNode.AttributeNodes.Nodes['variable_2'].Text);

      aCellMLConnection.MapVariablesList.Add([CellMLMapVariables]);
   End Else
      // Uknown element, so...

      ExtractUnknownElement(aXMLNode);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
