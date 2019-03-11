//==============================================================================
// Engine class for dealing with CellML API objects
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

Unit CellMLAPIEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   Engine, XMLIntf, CellMLAPI;

//==============================================================================

Type
   TCellMLAPIEngine = Class
      Protected
         // Properties used for internal purposes

         CellMLModel: TCellMLModel;

      Protected
         // Methods used for internal purposes

         Procedure StartCellMLModel(aCellMModel: TCellMLModel; Var aNode: IXMLNode); Virtual; Abstract;
         Procedure EndCellMLModel; Virtual; Abstract;

         Procedure CellMLUnits(aCellMLUnits: TCellMLUnits; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode);

         Procedure StartCellMLUnits(aCellMLUnits: TCellMLUnits; Var aNode: IXMLNode); Virtual; Abstract;
         Procedure CellMLUnit(aCellMLUnit: TCellMLUnit; aNode: IXMLNode); Virtual; Abstract;
         Procedure EndCellMLUnits(Const aNextItemType: TXMLNextItemType); Virtual; Abstract;

         Procedure CellMLComponent(aCellMLComponent: TCellMLComponent; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode);

         Procedure StartCellMLComponent(aCellMLComponent: TCellMLComponent; Var aNode: IXMLNode); Virtual; Abstract;
         Procedure EndCellMLComponent(Const aNextItemType: TXMLNextItemType); Virtual; Abstract;

         Procedure CellMLVariable(aCellMLVariable: TCellMLVariable; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode); Virtual; Abstract;

         Procedure StartMathMLEquation(Var aNode: IXMLNode); Virtual; Abstract;
         Procedure MathMLEquation(aMathMLEquation: TMathMLEquation; Const aNextItemType: TXMLNextItemType; Var aNode: IXMLNode); Virtual; Abstract;

         Procedure CellMLGroup(aCellMLGroup: TCellMLGroup; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode);

         Procedure StartCellMLGroup(aCellMLGroup: TCellMLGroup; Var aNode: IXMLNode); Virtual; Abstract;
         Procedure EndCellMLGroup(Const aNextItemType: TXMLNextItemType); Virtual; Abstract;

         Procedure CellMLComponentRef(aCellMLComponentRef: TCellMLComponentRef; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode); Virtual;

         Procedure CellMLConnection(aCellMLConnection: TCellMLConnection; Const aNextItemType: TXMLNextItemType; aNode: IXMLNode);

         Procedure StartCellMLConnection(aCellMLConnection: TCellMLConnection; Var aNode: IXMLNode); Virtual; Abstract;
         Procedure EndCellMLConnection(Const aNextItemType: TXMLNextItemType); Virtual; Abstract;

         Procedure CellMLMapVariables(aCellMLMapVariables: TCellMLMapVariables; aNode: IXMLNode); Virtual; Abstract;

      Public
         // Constructor & Destructor

         Constructor Create(Const aCellMLModel: TCellMLModel);

         // User's methods

         Function Execute: Boolean; Virtual;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   DeCAL;

//==============================================================================

Constructor TCellMLAPIEngine.Create(Const aCellMLModel: TCellMLModel);
Begin
   CellMLModel := aCellMLModel;
End;

//==============================================================================

Function TCellMLAPIEngine.Execute: Boolean;
Var
   Iter: Integer;
   Node: IXMLNode;
Begin
   Result := True;

   Try
      StartCellMLModel(CellMLModel, Node);

      // Units

      For Iter := 0 To CellMLModel.UnitsList.Size-1 Do
         If (Iter <> CellMLModel.UnitsList.Size-1) Then
            CellMLUnits(TCellMLUnits(CellMLModel.UnitsList.At(Iter).VObject), xitSame, Node)
         Else If ((CellMLModel.ComponentList.Size <> 0) Or
                  (CellMLModel.GroupList.Size <> 0)     Or
                  (CellMLModel.ConnectionList.Size <> 0)) Then
            CellMLUnits(TCellMLUnits(CellMLModel.UnitsList.At(Iter).VObject), xitOther, Node)
         Else
            CellMLUnits(TCellMLUnits(CellMLModel.UnitsList.At(Iter).VObject), xitNone, Node);

      // Components

      For Iter := 0 To CellMLModel.ComponentList.Size-1 Do
         If (Iter <> CellMLModel.ComponentList.Size-1) Then
            CellMLComponent(TCellMLComponent(CellMLModel.ComponentList.At(Iter).VObject), xitSame, Node)
         Else If ((CellMLModel.GroupList.Size <> 0) Or
                  (CellMLModel.ConnectionList.Size <> 0)) Then
            CellMLComponent(TCellMLComponent(CellMLModel.ComponentList.At(Iter).VObject), xitOther, Node)
         Else
            CellMLComponent(TCellMLComponent(CellMLModel.ComponentList.At(Iter).VObject), xitNone, Node);

      // Groups

      For Iter := 0 To CellMLModel.GroupList.Size-1 Do
         If (Iter <> CellMLModel.GroupList.Size-1) Then
            CellMLGroup(TCellMLGroup(CellMLModel.GroupList.At(Iter).VObject), xitSame, Node)
         Else If (CellMLModel.ConnectionList.Size <> 0) Then
            CellMLGroup(TCellMLGroup(CellMLModel.GroupList.At(Iter).VObject), xitOther, Node)
         Else
            CellMLGroup(TCellMLGroup(CellMLModel.GroupList.At(Iter).VObject), xitNone, Node);

      // Connections

      For Iter := 0 To CellMLModel.ConnectionList.Size-1 Do
         If (Iter <> CellMLModel.ConnectionList.Size-1) Then
            CellMLConnection(TCellMLConnection(CellMLModel.ConnectionList.At(Iter).VObject), xitSame, Node)
         Else
            CellMLConnection(TCellMLConnection(CellMLModel.ConnectionList.At(Iter).VObject), xitNone, Node);

      EndCellMLModel;
   Except
      Result := False;   // Something went wrong, so...
   End;
End;

//==============================================================================

Procedure TCellMLAPIEngine.CellMLUnits(aCellMLUnits: TCellMLUnits;
                                       Const aNextItemType: TXMLNextItemType;
                                       aNode: IXMLNode);
Var
   Iter: Integer;
Begin
   StartCellMLUnits(aCellMLUnits, aNode);

   If (Not aCellMLUnits.BaseUnits) Then Begin
      // Note: a user-defined base unit requires one unit element to make units
      //       checking work, but we don't want to display that information,
      //       so... (see "AddStandardUnit" in the "CellMLAPI" unit)

      For Iter := 0 To aCellMLUnits.NbOfUnitElements-1 Do
         CellMLUnit(aCellMLUnits.GetUnitElement(Iter), aNode);

      EndCellMLUnits(aNextItemType);
   End;
End;

//==============================================================================

Procedure TCellMLAPIEngine.CellMLComponent(aCellMLComponent: TCellMLComponent;
                                           Const aNextItemType: TXMLNextItemType;
                                           aNode: IXMLNode);
Var
   Iter: Integer;
Begin
   StartCellMLComponent(aCellMLComponent, aNode);

   // Units

   For Iter := 0 To aCellMLComponent.UnitsList.Size-1 Do
      If (Iter <> aCellMLComponent.UnitsList.Size-1) Then
         CellMLUnits(TCellMLUnits(aCellMLComponent.UnitsList.At(Iter).VObject), xitSame, aNode)
      Else If ((aCellMLComponent.VariableList.Size <> 0) Or
               (aCellMLComponent.EquationList.Size <> 0)) Then
         CellMLUnits(TCellMLUnits(aCellMLComponent.UnitsList.At(Iter).VObject), xitOther, aNode)
      Else
         CellMLUnits(TCellMLUnits(aCellMLComponent.UnitsList.At(Iter).VObject), xitNone, aNode);

   // Variables

   For Iter := 0 To aCellMLComponent.VariableList.Size-1 Do
      If (Iter <> aCellMLComponent.VariableList.Size-1) Then
         CellMLVariable(TCellMLVariable(aCellMLComponent.VariableList.At(Iter).VObject), xitSame, aNode)
      Else If (aCellMLComponent.EquationList.Size <> 0) Then
         CellMLVariable(TCellMLVariable(aCellMLComponent.VariableList.At(Iter).VObject), xitOther, aNode)
      Else
         CellMLVariable(TCellMLVariable(aCellMLComponent.VariableList.At(Iter).VObject), xitNone, aNode);

   // Equations

   If (aCellMLComponent.EquationList.Size <> 0) Then
      StartMathMLEquation(aNode);

   For Iter := 0 To aCellMLComponent.EquationList.Size-1 Do
      If (Iter <> aCellMLComponent.EquationList.Size-1) Then
         MathMLEquation(TMathMLEquation(aCellMLComponent.EquationList.At(Iter).VObject), xitSame, aNode)
      Else
         MathMLEquation(TMathMLEquation(aCellMLComponent.EquationList.At(Iter).VObject), xitNone, aNode);

   EndCellMLComponent(aNextItemType);
End;

//==============================================================================

Procedure TCellMLAPIEngine.CellMLGroup(aCellMLGroup: TCellMLGroup;
                                       Const aNextItemType: TXMLNextItemType;
                                       aNode: IXMLNode);
Var
   Iter: Integer;
Begin
   StartCellMLGroup(aCellMLGroup, aNode);

   For Iter := 0 To aCellMLGroup.ComponentRefList.Size-1 Do
      If (Iter <> aCellMLGroup.ComponentRefList.Size-1) Then
         CellMLComponentRef(TCellMLComponentRef(aCellMLGroup.ComponentRefList.At(Iter).VObject), xitSame, aNode)
      Else
         CellMLComponentRef(TCellMLComponentRef(aCellMLGroup.ComponentRefList.At(Iter).VObject), xitNone, aNode);

   EndCellMLGroup(aNextItemType);
End;

//==============================================================================

Procedure TCellMLAPIEngine.CellMLComponentRef(aCellMLComponentRef: TCellMLComponentRef;
                                              Const aNextItemType: TXMLNextItemType;
                                              aNode: IXMLNode);
Var
   Iter: Integer;
Begin
   For Iter := 0 To aCellMLComponentRef.ComponentRefList.Size-1 Do
      If (Iter <> aCellMLComponentRef.ComponentRefList.Size-1) Then Begin
         If (TCellMLComponentRef(aCellMLComponentRef.ComponentRefList.At(Iter+1).VObject).ComponentRefList.Size = 0) Then
            CellMLComponentRef(TCellMLComponentRef(aCellMLComponentRef.ComponentRefList.At(Iter).VObject), xitSame, aNode)
         Else
            CellMLComponentRef(TCellMLComponentRef(aCellMLComponentRef.ComponentRefList.At(Iter).VObject), xitOther, aNode);
      End Else
         CellMLComponentRef(TCellMLComponentRef(aCellMLComponentRef.ComponentRefList.At(Iter).VObject), xitNone, aNode);
End;

//==============================================================================

Procedure TCellMLAPIEngine.CellMLConnection(aCellMLConnection: TCellMLConnection;
                                            Const aNextItemType: TXMLNextItemType;
                                            aNode: IXMLNode);
Var
   Iter: Integer;
Begin
   StartCellMLConnection(aCellMLConnection, aNode);

   For Iter := 0 To aCellMLConnection.MapVariablesList.Size-1 Do
      CellMLMapVariables(TCellMLMapVariables(aCellMLConnection.MapVariablesList.At(Iter).VObject), aNode);

   EndCellMLConnection(aNextItemType);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

