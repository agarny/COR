//==============================================================================
// Engine class for exporting a CellML API object to a C file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 04/11/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToCMCFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToCMCFileEngine = Class(TCellMLAPIExportEngine)
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
   SysUtils, DeCAL;

//==============================================================================

Constructor TCellMLAPIToCMCFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                             Const aFileName: Array Of String);
Begin
   Inherited Create(aCellMLModel, aFileName);

   tStr := 'at';

   YStart := 'aY[';

   dYStart := 'adY[';
End;

//==============================================================================

Procedure TCellMLAPIToCMCFileEngine.Execute;
Var
   Iter: Integer;
   StateVarIndexVal, CstIndexVal, CompVarIndexVal: Integer;
Begin
   Inherited;

   StateVarIndexVal := 0;
   CstIndexVal      := 0;
   CompVarIndexVal  := 0;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               VarIndex := StateVarIndexVal;

               Inc(StateVarIndexVal);
            End;
            vsConstant: Begin
               VarName := 'aCsts['+IntToStr(CstIndexVal)+']';

               VarIndex := CstIndexVal;

               Inc(CstIndexVal);
            End;
            vsComputed: Begin
               VarName := 'aCompVars['+IntToStr(CompVarIndexVal)+']';

               VarIndex := CompVarIndexVal;

               Inc(CompVarIndexVal);
            End;
         End;
      End;

   Output(0, '#include <math.h>');
   Output(0);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then Begin
      Output(0, 'double fact(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'double res = 1.0;');
      Output(0);
      Output(0, 'while (n > 1.0)');

      IncIndent(0);

      Output(0, 'res *= n--;');

      DecIndent(0);

      Output(0);
      Output(0, 'return res;');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sec']))) Then Begin
      Output(0, 'double sec(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/cos(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csc']))) Then Begin
      Output(0, 'double csc(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/sin(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cot']))) Then Begin
      Output(0, 'double cot(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/tan(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then Begin
      Output(0, 'double sech(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/cosh(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csch']))) Then Begin
      Output(0, 'double csch(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/sinh(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['coth']))) Then Begin
      Output(0, 'double coth(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/tanh(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asec']))) Then Begin
      Output(0, 'double asec(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return acos(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsc']))) Then Begin
      Output(0, 'double acsc(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return asin(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acot']))) Then Begin
      Output(0, 'double acot(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return atan(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asinh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then Begin
      Output(0, 'double asinh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return log(n+sqrt(n*n+1.0));');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acosh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then Begin
      Output(0, 'double acosh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return log(n+sqrt(n*n-1.0));');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['atanh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then Begin
      Output(0, 'double atanh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 0.5*log((1.0+n)/(1.0-n));');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then Begin
      Output(0, 'double asech(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return acosh(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then Begin
      Output(0, 'double acsch(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return asinh(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then Begin
      Output(0, 'double acoth(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return atanh(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   Output(0, 'void __declspec(dllexport) __cdecl initStateVars(double* aData)');
   Output(0, '{');

   IncIndent(0);

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
         If (State = vsState) Then
            Output(0, 'aData['+IntToStr(VarIndex)+'] = '+OutputNb(InitialValue)+';');

   DecIndent(0);

   Output(0, '}');
   Output(0);
   Output(0, 'void __declspec(dllexport) __cdecl initCsts(double* aData)');
   Output(0, '{');

   IncIndent(0);

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
         If (State = vsConstant) Then
            Output(0, 'aData['+IntToStr(VarIndex)+'] = '+OutputNb(InitialValue)+';');

   DecIndent(0);

   Output(0, '}');
   Output(0);
   Output(0, 'void __declspec(dllexport) __cdecl computeOnce(double* aY, double* adY, double* aCsts, double* aCompVars)');
   Output(0, '{');

   IncIndent(0);

   GenerateMathMLEquations(toeComputeOnce);

   DecIndent(0);

   Output(0, '}');
   Output(0);
   Output(0, 'void __declspec(dllexport) __cdecl compute(double at, double* aY, double* adY, double* aCsts, double* aCompVars)');
   Output(0, '{');

   IncIndent(0);

   GenerateMathMLEquations(toeCompute);

   DecIndent(0);

   Output(0, '}');
End;

//==============================================================================

Function TCellMLAPIToCMCFileEngine.AddFunc(Const aCellMLComponent: String;
                                           Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitAbs:   Result := 'fabs';
      mitLN:    Result := 'log';
      mitLog:   Result := 'log10';
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

