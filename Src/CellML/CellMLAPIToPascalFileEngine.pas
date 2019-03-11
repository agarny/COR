//==============================================================================
// Engine class for exporting a CellML API object to a Pascal file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 20/12/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToPascalFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToPascalFileEngine = Class(TCellMLAPIExportEngine)
      Protected
         // Methods used for internal purposes

         Function AddFunc(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;

      Public
         // Constructor & Destructor

         Constructor Create(Const aCellMLModel: TCellMLModel; Const aFileName: Array Of String);

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
   SysUtils, StrUtils, DeCAL;

//==============================================================================

Constructor TCellMLAPIToPascalFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                                Const aFileName: Array Of String);
Begin
   Inherited Create(aCellMLModel, aFileName);

   CaseSensitive := False;

   IfThenElseStatementsWithStatementSep := False;

   IfStatement     := 'If ';
   ThenStatement   := ' Then';
   ElseIfStatement := 'Else If ';
   ElseStatement   := 'Else';

   EqStr   := ' := ';
   EqEqStr := ' = ';
   NEqStr  := ' <> ';
   AndStr  := ' And ';
   OrStr   := ' Or ';
   XOrStr  := ' XOr ';

   PowerStr := 'Power';

   NotStr      := 'Not';
   NotSpaceStr := ' ';

   LogStr := 'LN';

   SqrStr  := 'Sqr';
   SqrtStr := 'Sqrt';

   TrueStr  := 'True';
   FalseStr := 'False';
End;

//==============================================================================

Procedure TCellMLAPIToPascalFileEngine.Execute;
Var
   NeedYExtra1, NeedYExtra2: Boolean;
   YNames, YUnits, YComponents: Array Of String;
   Procedure YExtra1;
   Begin
      Output(0);
      Output(0, 'YNames: Array[0..'+NbOfStateVariablesStr+'-1] Of String;');
      Output(0, 'YUnits: Array[0..'+NbOfStateVariablesStr+'-1] Of String;');
      Output(0, 'YComponents: Array[0..'+NbOfStateVariablesStr+'-1] Of String;');

      NeedYExtra1 := False;
   End;
   Procedure YExtra2;
   Var
      Iter: Integer;
   Begin
      Output(0);

      For Iter := 0 To High(YNames) Do
         Output(0, YNames[Iter]);

      Output(0);

      For Iter := 0 To High(YUnits) Do
         Output(0, YUnits[Iter]);

      Output(0);

      For Iter := 0 To High(YComponents) Do
         Output(0, YComponents[Iter]);

      NeedYExtra2 := False;
   End;
Var
   Iter: Integer;
   PrevState: TCellMLModelVariableState;
   VarIndexVal: Integer;
   NeedEmptyLine, NeedDecIndent: Boolean;
Begin
   Inherited;

   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' CellML file:   '+CellMLModel.FileName);
   Output(0, StatementCommentTag+' CellML model:  '+CellMLModel.Name);
   Output(0, StatementCommentTag+' Date and time: '+ReplaceStr(DateTimeToStr(Now), ' ', ' at '));
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$IFDEF COR_SPECIFIC}
   Output(0, StatementCommentTag+' Conversion from '+CELLML_VERSION+' to Pascal was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(0, StatementCommentTag+'    '+COR_FULL_COPYRIGHT);
   Output(0, StatementCommentTag+'    '+COR_URL+' - '+COR_EMAIL);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$ENDIF}
   Output(0, StatementCommentTag+' '+CELLML_URL);
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'Unit '+ChangeFileExt(ExtractFileName(FileName[0]), '')+';');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'Interface');
   Output(0);

   PrevState := vsUnknown;

   VarIndexVal := 0;

   NeedEmptyLine := False;
   NeedDecIndent := False;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               If (PrevState <= vsFree) Then Begin
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' State variables');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
                  Output(0, 'Const '+NbOfStateVariablesStr+' = '+IntToStr(CellMLModel.NbOfStateVariables)+';');
                  Output(0);
                  Output(0, 'Var');

                  IncIndent(0);

                  NeedDecIndent := True;

                  Output(0, 'Y: Array[0..'+NbOfStateVariablesStr+'-1] Of Double;');
                  Output(0, 'dY: Array[0..'+NbOfStateVariablesStr+'-1] Of Double;');
               End;

               VarIndex := VarIndexVal;

               Output(0, StatementCommentTag+' '+IntToStr(VarIndex)+': '+RealName+' ('+Units+')'+GenInfo);

               Inc(VarIndexVal);

               NeedEmptyLine := True;

               NeedYExtra1 := True;
            End;
            vsConstant: Begin
               If (PrevState <= vsState) Then Begin
                  If (NeedYExtra1) Then
                     YExtra1;

                  If (NeedDecIndent) Then
                     DecIndent(0);

                  If (NeedEmptyLine) Then
                     Output(0);

                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' Constants');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
                  Output(0, 'Var');

                  IncIndent(0);

                  NeedDecIndent := True;
               End;

               Output(0, RealName+': Double;'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);

               NeedEmptyLine := True;
            End;
            vsComputed: Begin
               If (PrevState <= vsConstant) Then Begin
                  If (NeedYExtra1) Then
                     YExtra1;

                  If (NeedDecIndent) Then
                     DecIndent(0);

                  If (NeedEmptyLine) Then
                     Output(0);

                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' Computed variables');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
                  Output(0, 'Var');

                  IncIndent(0);

                  NeedDecIndent := True;
               End;

               Output(0, RealName+': Double;'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
            End;
         End;

         PrevState := State;
      End;

   If (NeedYExtra1) Then
      YExtra1;

   If (NeedDecIndent) Then
      DecIndent(0);

   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' Procedures');
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then Begin
      Output(0, 'Function Fact(N: Double): Double;');
      Output(0);
   End;

   Output(0, 'Procedure Init;');
   Output(0, 'Procedure Compute('+tStr+': Double);');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'Implementation');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'Uses');

   IncIndent(0);

   Output(0, 'Math;');

   DecIndent(0);

   Output(0);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Factorial');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'Function Fact(N: Double): Double;');
      Output(0, 'Begin');

      IncIndent(0);

      Output(0, 'Result := 1.0;');
      Output(0);
      Output(0, 'While (N > 1.0) Do Begin');

      IncIndent(0);

      Output(0, 'Result := N*Result;');
      Output(0);
      Output(0, 'N := N-1.0;');

      DecIndent(0);

      Output(0, 'End;');

      DecIndent(0);

      Output(0, 'End;');
      Output(0);
   End;

   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' Initialisation');
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'Procedure Init;');
   Output(0, 'Begin');

   IncIndent(0);

   PrevState := vsUnknown;

   NeedEmptyLine := False;

   SetLength(YNames, CellMLModel.NbOfStateVariables);
   SetLength(YUnits, CellMLModel.NbOfStateVariables);
   SetLength(YComponents, CellMLModel.NbOfStateVariables);

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               If (PrevState <= vsFree) Then Begin
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' State variables');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, 'Y['+IntToStr(VarIndex)+'] := '+OutputNb(InitialValue)+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+RealName+' ('+Units+')'+GenInfo);

               YNames[VarIndex]      := 'YNames['+IntToStr(VarIndex)+'] := '''+RealName+''';';
               YUnits[VarIndex]      := 'YUnits['+IntToStr(VarIndex)+'] := '''+Units+''';';
               YComponents[VarIndex] := 'YComponents['+IntToStr(VarIndex)+'] := '''+Component+''';';

               NeedEmptyLine := True;

               NeedYExtra2 := True;
            End;
            vsConstant: Begin
               If (PrevState <= vsState) Then Begin
                  If (NeedYExtra2) Then
                     YExtra2;

                  If (NeedEmptyLine) Then
                     Output(0);

                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' Constants');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, RealName+' := '+OutputNb(InitialValue)+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
            End;
         End;

         PrevState := State;
      End;

   If (NeedYExtra2) Then
      YExtra2;

   DecIndent(0);

   If (CellMLModel.HasAtLeastOneComputeOnceEquation) Then Begin
      Output(0);

      IncIndent(0);

      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Computed variables');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);

      GenerateMathMLEquations(toeComputeOnce);

      DecIndent(0);
   End;

   Output(0, 'End;');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' Computation');
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'Procedure Compute('+tStr+': Double);');
   Output(0, 'Begin');

   IncIndent(0);

   Output(0, StatementCommentTag+' '+tStr+': time ('+CellMLModel.FreeVariableUnits.Name+')');
   Output(0);

   GenerateMathMLEquations(toeCompute);

   DecIndent(0);

   Output(0, 'End;');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'End.');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' End of file');
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
End;

//==============================================================================

Function TCellMLAPIToPascalFileEngine.AddFunc(Const aCellMLComponent: String;
                                              Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitAbs:   Result := 'Abs';
      mitExp:   Result := 'Exp';
      mitLN:    Result := 'LN';
      mitLog:   Result := 'Log10';
      mitCeil:  Result := 'Ceil';
      mitFloor: Result := 'Floor';
      mitFact:  Result := 'Fact';
      mitSin:   Result := 'Sin';
      mitCos:   Result := 'Cos';
      mitTan:   Result := 'Tan';
      mitSec:   Result := 'Sec';
      mitCsc:   Result := 'Csc';
      mitCot:   Result := 'Cot';
      mitSinH:  Result := 'SinH';
      mitCosH:  Result := 'CosH';
      mitTanH:  Result := 'TanH';
      mitSecH:  Result := 'SecH';
      mitCscH:  Result := 'CscH';
      mitCotH:  Result := 'CotH';
      mitASin:  Result := 'ArcSin';
      mitACos:  Result := 'ArcCos';
      mitATan:  Result := 'ArcTan';
      mitASec:  Result := 'ArcSec';
      mitACsc:  Result := 'ArcCsc';
      mitACot:  Result := 'ArcCot';
      mitASinH: Result := 'ArcSinH';
      mitACosH: Result := 'ArcCosH';
      mitATanH: Result := 'ArcTanH';
      mitASecH: Result := 'ArcSecH';
      mitACscH: Result := 'ArcCscH';
      mitACotH: Result := 'ArcCotH';
   Else
      Result := aMathMLEquationBinTree.Str;
   End;

   Result := Result+'('+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+')';
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

