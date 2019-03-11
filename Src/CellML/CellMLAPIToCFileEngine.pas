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

Unit CellMLAPIToCFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToCFileEngine = Class(TCellMLAPIExportEngine)
      Private
         // Properties used for internal purposes

         YNamesLen: Integer;
         YUnitsLen: Integer;
         YComponentsLen: Integer;

      Protected
         // Methods used for internal purposes

         Procedure GenerateCFile;
         Procedure GenerateHFile;

         Function AddFunc(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree): String; Override;

      Public
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
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
   SysUtils, StrUtils, DeCAL;

//==============================================================================

Procedure TCellMLAPIToCFileEngine.Execute;
Var
   Iter: Integer;
Begin
   Inherited;

   // Compute the length of YNames, YUnits and YComponents strings

   YNamesLen      := 0;
   YUnitsLen      := 0;
   YComponentsLen := 0;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
         If (State = vsState) Then Begin
{$IFDEF OPT_MATH}
            YNamesLen      := OptMaxI(YNamesLen, Length(RealName));
            YUnitsLen      := OptMaxI(YUnitsLen, Length(Units));
            YComponentsLen := OptMaxI(YComponentsLen, Length(Component));
{$ELSE}
            YNamesLen      := Max(YNamesLen, Length(RealName));
            YUnitsLen      := Max(YUnitsLen, Length(Units));
            YComponentsLen := Max(YComponentsLen, Length(Component));
{$ENDIF}
         End;

   Inc(YNamesLen);        // This is to
   Inc(YUnitsLen);        // account for
   Inc(YComponentsLen);   // '\0'

   // Generate the relevant code

   GenerateCFile;
   GenerateHFile;
End;

//==============================================================================

Procedure TCellMLAPIToCFileEngine.GenerateCFile;
Var
   NeedYExtra1, NeedYExtra2: Boolean;
   YNames, YUnits, YComponents: Array Of String;
   Procedure YExtra1;
   Begin
      Output(0);
      Output(0, 'char YNames['+NbOfStateVariablesStr+']['+IntToStr(YNamesLen)+'];');
      Output(0, 'char YUnits['+NbOfStateVariablesStr+']['+IntToStr(YUnitsLen)+'];');
      Output(0, 'char YComponents['+NbOfStateVariablesStr+']['+IntToStr(YComponentsLen)+'];');

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
   NeedEmptyLine: Boolean;
Begin
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' CellML file:   '+CellMLModel.FileName);
   Output(0, StatementCommentTag+' CellML model:  '+CellMLModel.Name);
   Output(0, StatementCommentTag+' Date and time: '+ReplaceStr(DateTimeToStr(Now), ' ', ' at '));
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$IFDEF COR_SPECIFIC}
   Output(0, StatementCommentTag+' Conversion from '+CELLML_VERSION+' to C was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(0, StatementCommentTag+'    '+COR_FULL_COPYRIGHT);
   Output(0, StatementCommentTag+'    '+COR_URL+' - '+COR_EMAIL);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$ENDIF}
   Output(0, StatementCommentTag+' '+CELLML_URL);
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, '#include "'+ExtractFileName(FileName[1])+'"');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, '#include <math.h>');
   Output(0, '#include <string.h>');
   Output(0);

   PrevState := vsUnknown;

   VarIndexVal := 0;

   NeedEmptyLine := False;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               If (PrevState <= vsFree) Then Begin
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' State variables');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
                  Output(0, 'double Y['+NbOfStateVariablesStr+'];');
                  Output(0, 'double dY['+NbOfStateVariablesStr+'];');
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

                  If (NeedEmptyLine) Then
                     Output(0);

                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' Constants');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, 'double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);

               NeedEmptyLine := True;
            End;
            vsComputed: Begin
               If (PrevState <= vsConstant) Then Begin
                  If (NeedYExtra1) Then
                     YExtra1;

                  If (NeedEmptyLine) Then
                     Output(0);

                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' Computed variables');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, 'double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
            End;
         End;

         PrevState := State;
      End;

   If (NeedYExtra1) Then
      YExtra1;

   Output(0);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Factorial');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
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
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Secant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double sec(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/cos(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csc']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Cosecant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double csc(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/sin(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cot']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Cotangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double cot(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/tan(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Hyperbolic secant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double sech(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/cosh(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csch']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Hyperbolic cosecant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double csch(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/sinh(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['coth']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Hyperbolic cotangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double coth(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/tanh(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asec']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse secant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double asec(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return acos(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsc']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse cosecant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double acsc(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return asin(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acot']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse cotangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
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
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse hyperbolic sinus');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
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
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse hyperbolic cosinus');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
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
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse hyperbolic tangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double atanh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 0.5*log((1.0+n)/(1.0-n));');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse hyperbolic secant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double asech(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return acosh(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse hyperbolic cosecant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double acsch(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return asinh(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse hyperbolic cotangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double acoth(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return atanh(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' Initialisation');
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'void init()');
   Output(0, '{');

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

               Output(0, 'Y['+IntToStr(VarIndex)+'] = '+OutputNb(InitialValue)+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+RealName+' ('+Units+')'+GenInfo);

               YNames[VarIndex]      := 'strcpy(YNames['+IntToStr(VarIndex)+'], "'+RealName+'");';
               YUnits[VarIndex]      := 'strcpy(YUnits['+IntToStr(VarIndex)+'], "'+Units+'");';
               YComponents[VarIndex] := 'strcpy(YComponents['+IntToStr(VarIndex)+'], "'+Component+'");';

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

               Output(0, RealName+' = '+OutputNb(InitialValue)+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
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

   Output(0, '}');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' Computation');
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'void compute(double '+tStr+')');
   Output(0, '{');

   IncIndent(0);

   Output(0, StatementCommentTag+' '+tStr+': time ('+CellMLModel.FreeVariableUnits.Name+')');
   Output(0);

   GenerateMathMLEquations(toeCompute);

   DecIndent(0);

   Output(0, '}');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' End of file');
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
End;

//==============================================================================

Procedure TCellMLAPIToCFileEngine.GenerateHFile;
Var
   NeedYExtra: Boolean;
   Procedure YExtra;
   Begin
      Output(1);
      Output(1, 'extern char YNames['+NbOfStateVariablesStr+']['+IntToStr(YNamesLen)+'];');
      Output(1, 'extern char YUnits['+NbOfStateVariablesStr+']['+IntToStr(YUnitsLen)+'];');
      Output(1, 'extern char YComponents['+NbOfStateVariablesStr+']['+IntToStr(YComponentsLen)+'];');

      NeedYExtra := False;
   End;
Var
   Iter: Integer;
   PrevState: TCellMLModelVariableState;
   NeedEmptyLine: Boolean;
Begin
   Output(1, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[1])));
   Output(1, StatementCommentTag+' CellML file:   '+CellMLModel.FileName);
   Output(1, StatementCommentTag+' CellML model:  '+CellMLModel.Name);
   Output(1, StatementCommentTag+' Date and time: '+ReplaceStr(DateTimeToStr(Now), ' ', ' at '));
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
{$IFDEF COR_SPECIFIC}
   Output(1, StatementCommentTag+' Conversion from '+CELLML_VERSION+' to C (header) was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(1, StatementCommentTag+'    '+COR_FULL_COPYRIGHT);
   Output(1, StatementCommentTag+'    '+COR_URL+' - '+COR_EMAIL);
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$ENDIF}
   Output(1, StatementCommentTag+' '+CELLML_URL);
   Output(1, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[1])));
   Output(1);
   Output(1, '#ifndef __'+UpperCase(ChangeFileExt(ExtractFileName(FileName[1]), ''))+'_H__');
   Output(1, '#define __'+UpperCase(ChangeFileExt(ExtractFileName(FileName[1]), ''))+'_H__');
   Output(1);

   CellMLModel.GlobalVariableList.Comparator := CellMLModel.CompareByVarState;

   Sort(CellMLModel.GlobalVariableList);

   PrevState := vsUnknown;

   NeedEmptyLine := False;
      
   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               If (PrevState <= vsFree) Then Begin
                  Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
                  Output(1, StatementCommentTag+' State variables');
                  Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
                  Output(1);
                  Output(1, '#define '+NbOfStateVariablesStr+' '+IntToStr(CellMLModel.NbOfStateVariables));
                  Output(1);
                  Output(1, 'extern double Y['+NbOfStateVariablesStr+'];');
                  Output(1, 'extern double dY['+NbOfStateVariablesStr+'];');
               End;

               Output(1, StatementCommentTag+' '+IntToStr(VarIndex)+': '+RealName+' ('+Units+')'+GenInfo);

               NeedEmptyLine := True;

               NeedYExtra := True;
            End;
            vsConstant: Begin
               If (PrevState <= vsState) Then Begin
                  If (NeedYExtra) Then
                     YExtra;

                  If (NeedEmptyLine) Then
                     Output(1);

                  Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
                  Output(1, StatementCommentTag+' Constants');
                  Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
                  Output(1);
               End;

               Output(1, 'extern double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);

               NeedEmptyLine := True;
            End;
            vsComputed: Begin
               If (PrevState <= vsConstant) Then Begin
                  If (NeedYExtra) Then
                     YExtra;

                  If (NeedEmptyLine) Then
                     Output(1);

                  Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
                  Output(1, StatementCommentTag+' Computed variables');
                  Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
                  Output(1);
               End;

               Output(1, 'extern double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
            End;
         End;

         PrevState := State;
      End;

   If (NeedYExtra) Then
      YExtra;

   Output(1);
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
   Output(1, StatementCommentTag+' Procedures');
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
   Output(1);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then
      Output(1, 'extern double fact(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sec']))) Then
      Output(1, 'extern double sec(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csc']))) Then
      Output(1, 'extern double csc(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cot']))) Then
      Output(1, 'extern double cot(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then
      Output(1, 'extern double sech(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csch']))) Then
      Output(1, 'extern double csch(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['coth']))) Then
      Output(1, 'extern double coth(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asec']))) Then
      Output(1, 'extern double asec(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsc']))) Then
      Output(1, 'extern double acsc(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acot']))) Then
      Output(1, 'extern double acot(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asinh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then
      Output(1, 'extern double asinh(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acosh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then
      Output(1, 'extern double acosh(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['atanh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then
      Output(1, 'extern double atanh(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then
      Output(1, 'extern double asech(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then
      Output(1, 'extern double acsch(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then
      Output(1, 'extern double acoth(double n);');

   Output(1);   // Note: may not always be relevant, depending on whether
                //       there is/are required function/s

   Output(1, 'extern void init();');
   Output(1, 'extern void compute(double '+tStr+');');
   Output(1);
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
   Output(1);
   Output(1, '#endif');
   Output(1);
   Output(1, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[1])));
   Output(1, StatementCommentTag+' End of file');
   Output(1, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[1])));
End;

//==============================================================================

Function TCellMLAPIToCFileEngine.AddFunc(Const aCellMLComponent: String;
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

