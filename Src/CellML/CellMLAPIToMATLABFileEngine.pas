//==============================================================================
// Engine class for exporting a CellML API object to a MATLAB file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 17/11/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToMATLABFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToMATLABFileEngine = Class(TCellMLAPIExportEngine)
      Protected
         // Properties used for internal purposes

         FuncNameStr: String;

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
   SysUtils, StrUtils, DeCAL, Common;

//==============================================================================

Constructor TCellMLAPIToMATLABFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                                Const aFileName: Array Of String);
Begin
   Inherited Create(aCellMLModel, aFileName);

   StatementCommentTag := '%';

   VariableLen := 63;   // Variable names cannot be more than 63 characters long
                        // in MATLAB, so...

   ElseIfStatement := 'elseif ';
   EndIfStatement := 'end;';

   NEqStr := ' ~= ';

   AndStr := ' && ';
   OrStr  := ' || ';

   XOrStr    := 'xor';
   XOrAsFunc := True;

   PowerStr       := '^';
   PowerAsOperand := True;
   RootAsOperand  := True;

   NotStr := '~';

   YStart := 'Y(';
   YEnd   := ')';

   dYStart := 'dY(';
   dYEnd   := ', 1)';

   PIStr := 'pi';

   FuncNameStr := ChangeFileExt(ExtractFileName(aFileName[0]), '');
End;

//==============================================================================

Procedure TCellMLAPIToMATLABFileEngine.Execute;
Var
   NeedYExtra: Boolean;
   StateVarsInit, YNames, YUnits, YComponents, StateVarsComment: String;
   Procedure YExtra;
   Begin
      Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Initial conditions');
      Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
      Output(0);
      Output(0, Copy(StateVarsInit, 1, Length(StateVarsInit)-2)+'];');
      Output(0);
      Output(0, Copy(YNames, 1, Length(YNames)-2)+'};');
      Output(0, Copy(YUnits, 1, Length(YUnits)-2)+'};');
      Output(0, Copy(YComponents, 1, Length(YComponents)-2)+'};');
      Output(0);
      Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' State variables');
      Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
      Output(0);
      Output(0, Copy(StateVarsComment, 1, Length(StateVarsComment)-Length(CRLF)));

      NeedYExtra := False;
   End;
Var
   Iter: Integer;
   PrevState: TCellMLModelVariableState;
   VarIndexVal: Integer;
   NeedEmptyLine: Boolean;
Begin
   Inherited;

   Output(0, StatementCommentTag+StringOfChar('=', 79-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' CellML file:   '+CellMLModel.FileName);
   Output(0, StatementCommentTag+' CellML model:  '+CellMLModel.Name);
   Output(0, StatementCommentTag+' Date and time: '+ReplaceStr(DateTimeToStr(Now), ' ', ' at '));
   Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
{$IFDEF COR_SPECIFIC}
   Output(0, StatementCommentTag+' Conversion from '+CELLML_VERSION+' to MATLAB (init) was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(0, StatementCommentTag+'    '+COR_FULL_COPYRIGHT);
   Output(0, StatementCommentTag+'    '+COR_URL+' - '+COR_EMAIL);
   Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
{$ENDIF}
   Output(0, StatementCommentTag+' '+CELLML_URL);
   Output(0, StatementCommentTag+StringOfChar('=', 79-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'function dY = '+FuncNameStr+'('+tStr+', Y)');
   Output(0);

   PrevState := vsUnknown;

   VarIndexVal := 1;

   NeedEmptyLine := False;

   StateVarsInit := StatementCommentTag+' Y = [';

   YNames      := StatementCommentTag+' YNames = {';
   YUnits      := StatementCommentTag+' YUnits = {';
   YComponents := StatementCommentTag+' YComponents = {';

   StateVarsComment := '';

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               VarIndex := VarIndexVal;

               StateVarsInit := StateVarsInit+OutputNb(InitialValue)+', ';

               YNames      := YNames+''''+RealName+''', ';
               YUnits      := YUnits+''''+Units+''', ';
               YComponents := YComponents+''''+Component+''', ';

               StateVarsComment := StateVarsComment+
                                   StatementCommentTag+' '+IntToStr(VarIndex)+': '+RealName+' ('+Units+')'+GenInfo+CRLF;

               Inc(VarIndexVal);

               NeedEmptyLine := True;

               NeedYExtra := True;
            End;
            vsConstant: Begin
               If (PrevState <= vsState) Then Begin
                  If (NeedYExtra) Then
                     YExtra;

                  If (NeedEmptyLine) Then
                     Output(0);

                  Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' Constants');
                  Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, RealName+' = '+OutputNb(InitialValue)+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
            End;
         End;

         PrevState := State;
      End;

   If (NeedYExtra) Then
      YExtra;

   Output(0);

   PrevState := vsUnknown;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsComputed: Begin
               If (PrevState <= vsConstant) Then Begin
                  Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' Computed variables');
                  Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, StatementCommentTag+' '+RealName+' ('+Units+')'+GenInfo);
            End;
         End;

         PrevState := State;
      End;

   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' Computation');
   Output(0, StatementCommentTag+StringOfChar('-', 79-Length(CellMLIndent[0])));
   Output(0);
   Output(0, StatementCommentTag+' '+tStr+' ('+CellMLModel.FreeVariableUnits.Name+')');
   Output(0);

   GenerateMathMLEquations(toeAll);

   Output(0);
   Output(0, StatementCommentTag+StringOfChar('=', 79-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' End of file');
   Output(0, StatementCommentTag+StringOfChar('=', 79-Length(CellMLIndent[0])));
End;

//==============================================================================

Function TCellMLAPIToMATLABFileEngine.AddFunc(Const aCellMLComponent: String;
                                              Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitLN:
         Result := 'log('+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+')';
      mitLog:
         Result := 'log10('+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+')';
      mitFact:
         Result := 'factorial('+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+')';
   Else
      Result := aMathMLEquationBinTree.Str+'('+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+')';
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

