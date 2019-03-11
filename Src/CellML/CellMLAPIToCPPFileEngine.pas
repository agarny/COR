//==============================================================================
// Engine class for exporting a CellML API object to a C++ file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 07/11/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToCPPFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToCPPFileEngine = Class(TCellMLAPIExportEngine)
      Protected
         // Properties used for internal purposes

         ClassNameStr: String;

         // Methods used for internal purposes

         Procedure GenerateCPPFile;
         Procedure GenerateHPPFile;

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

Constructor TCellMLAPIToCPPFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                             Const aFileName: Array Of String);
Begin
   Inherited Create(aCellMLModel, aFileName);

   ClassNameStr := ChangeFileExt(ExtractFileName(aFileName[0]), '');
End;

//==============================================================================

Procedure TCellMLAPIToCPPFileEngine.Execute;
Begin
   Inherited;

   GenerateCPPFile;
   GenerateHPPFile;
End;

//==============================================================================

Procedure TCellMLAPIToCPPFileEngine.GenerateCPPFile;
Var
   NeedYExtra: Boolean;
   YNames, YUnits, YComponents: Array Of String;
   Procedure YExtra;
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

      NeedYExtra := False;
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
   Output(0, StatementCommentTag+' Conversion from '+CELLML_VERSION+' to C++ was done using '+COR_NAME+' ('+COR_VERSION+')');
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
   Output(0);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Factorial');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'double '+ClassNameStr+'::fact(double n)');
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
      Output(0, 'double '+ClassNameStr+'::sec(double n)');
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
      Output(0, 'double '+ClassNameStr+'::csc(double n)');
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
      Output(0, 'double '+ClassNameStr+'::cot(double n)');
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
      Output(0, 'double '+ClassNameStr+'::sech(double n)');
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
      Output(0, 'double '+ClassNameStr+'::csch(double n)');
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
      Output(0, 'double '+ClassNameStr+'::coth(double n)');
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
      Output(0, 'double '+ClassNameStr+'::asec(double n)');
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
      Output(0, 'double '+ClassNameStr+'::acsc(double n)');
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
      Output(0, 'double '+ClassNameStr+'::acot(double n)');
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
      Output(0, 'double '+ClassNameStr+'::asinh(double n)');
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
      Output(0, 'double '+ClassNameStr+'::acosh(double n)');
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
      Output(0, 'double '+ClassNameStr+'::atanh(double n)');
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
      Output(0, 'double '+ClassNameStr+'::asech(double n)');
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
      Output(0, 'double '+ClassNameStr+'::acsch(double n)');
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
      Output(0, 'double '+ClassNameStr+'::acoth(double n)');
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
   Output(0, 'void '+ClassNameStr+'::init()');
   Output(0, '{');

   IncIndent(0);

   PrevState := vsUnknown;

   VarIndexVal := 0;

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

               VarIndex := VarIndexVal;

               Output(0, 'Y['+IntToStr(VarIndex)+'] = '+OutputNb(InitialValue)+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+RealName+' ('+Units+')'+GenInfo);

               YNames[VarIndex]      := 'YNames['+IntToStr(VarIndex)+'].assign("'+RealName+'");';
               YUnits[VarIndex]      := 'YUnits['+IntToStr(VarIndex)+'].assign("'+Units+'");';
               YComponents[VarIndex] := 'YComponents['+IntToStr(VarIndex)+'].assign("'+Component+'");';

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

   If (NeedYExtra) Then
      YExtra;

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
   Output(0, 'void '+ClassNameStr+'::compute(double '+tStr+')');
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

Procedure TCellMLAPIToCPPFileEngine.GenerateHPPFile;
Var
   NeedYExtra: Boolean;
   Procedure YExtra;
   Begin
      Output(1);
      Output(1, 'std::string YNames['+NbOfStateVariablesStr+'];');
      Output(1, 'std::string YUnits['+NbOfStateVariablesStr+'];');
      Output(1, 'std::string YComponents['+NbOfStateVariablesStr+'];');

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
   Output(1, StatementCommentTag+' Conversion from '+CELLML_VERSION+' to C++ (header) was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(1, StatementCommentTag+'    '+COR_FULL_COPYRIGHT);
   Output(1, StatementCommentTag+'    '+COR_URL+' - '+COR_EMAIL);
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$ENDIF}
   Output(1, StatementCommentTag+' '+CELLML_URL);
   Output(1, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[1])));
   Output(1);
   Output(1, '#ifndef __'+UpperCase(ChangeFileExt(ExtractFileName(FileName[1]), ''))+'_HPP__');
   Output(1, '#define __'+UpperCase(ChangeFileExt(ExtractFileName(FileName[1]), ''))+'_HPP__');
   Output(1);
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
   Output(1);
   Output(1, '#define '+NbOfStateVariablesStr+' '+IntToStr(CellMLModel.NbOfStateVariables));
   Output(1);
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
   Output(1);
   Output(1, '#include <string>');
   Output(1);
   Output(1, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[1])));
   Output(1);
   Output(1, 'class '+ClassNameStr);
   Output(1, '{');

   IncIndent(1);

   Output(1, 'public:');

   IncIndent(1);

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
                  Output(1, 'double Y['+NbOfStateVariablesStr+'];');
                  Output(1, 'double dY['+NbOfStateVariablesStr+'];');
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

               Output(1, 'double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);

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

               Output(1, 'double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
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
      Output(1, 'double fact(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sec']))) Then
      Output(1, 'double sec(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csc']))) Then
      Output(1, 'double csc(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cot']))) Then
      Output(1, 'double cot(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then
      Output(1, 'double sech(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csch']))) Then
      Output(1, 'double csch(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['coth']))) Then
      Output(1, 'double coth(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asec']))) Then
      Output(1, 'double asec(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsc']))) Then
      Output(1, 'double acsc(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acot']))) Then
      Output(1, 'double acot(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asinh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then
      Output(1, 'double asinh(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acosh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then
      Output(1, 'double acosh(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['atanh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then
      Output(1, 'double atanh(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then
      Output(1, 'double asech(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then
      Output(1, 'double acsch(double n);');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then
      Output(1, 'double acoth(double n);');

   Output(1);   // Note: may not always be relevant, depending on whether
                //       there is/are required function/s

   Output(1, 'void init();');
   Output(1, 'void compute(double '+tStr+');');

   DecIndent(1);
   DecIndent(1);

   Output(1, '};');
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

Function TCellMLAPIToCPPFileEngine.AddFunc(Const aCellMLComponent: String;
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

