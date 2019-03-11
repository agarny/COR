//==============================================================================
// Engine class for exporting a CellML API object to a Java file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 19/11/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToJavaFileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToJavaFileEngine = Class(TCellMLAPIExportEngine)
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

Constructor TCellMLAPIToJavaFileEngine.Create(Const aCellMLModel: TCellMLModel;
                                              Const aFileName: Array Of String);
Begin
   Inherited Create(aCellMLModel, aFileName);

   PowerStr := 'Math.pow';

   LogStr := 'Math.log';

   SqrtStr := 'Math.sqrt';

   TrueStr  := 'true';
   FalseStr := 'false';

   PIStr := 'Math.PI';

   ExponentialeStr := 'Math.E';
End;

//==============================================================================

Procedure TCellMLAPIToJavaFileEngine.Execute;
Var
   NeedYExtra1, NeedYExtra2: Boolean;
   YNames, YUnits, YComponents: Array Of String;
   Procedure YExtra1;
   Begin
      Output(0);
      Output(0, 'public String[] YNames = new String['+NbOfStateVariablesStr+'];');
      Output(0, 'public String[] YUnits = new String['+NbOfStateVariablesStr+'];');
      Output(0, 'public String[] YComponents = new String['+NbOfStateVariablesStr+'];');

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
   Inherited;

   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' CellML file:   '+CellMLModel.FileName);
   Output(0, StatementCommentTag+' CellML model:  '+CellMLModel.Name);
   Output(0, StatementCommentTag+' Date and time: '+ReplaceStr(DateTimeToStr(Now), ' ', ' at '));
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$IFDEF COR_SPECIFIC}
   Output(0, StatementCommentTag+' Conversion from '+CELLML_VERSION+' to Java was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(0, StatementCommentTag+'    '+COR_FULL_COPYRIGHT);
   Output(0, StatementCommentTag+'    '+COR_URL+' - '+COR_EMAIL);
   Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
{$ENDIF}
   Output(0, StatementCommentTag+' '+CELLML_URL);
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'public class '+ChangeFileExt(ExtractFileName(FileName[0]), ''));
   Output(0, '{');

   IncIndent(0);

   PrevState := vsUnknown;

   VarIndexVal := 0;

   NeedEmptyLine := False;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               If (PrevState <= vsFree) Then Begin
                  Output(0, 'public int '+NbOfStateVariablesStr+' = '+IntToStr(CellMLModel.NbOfStateVariables)+';');
                  Output(0);
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0, StatementCommentTag+' State variables');
                  Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
                  Output(0);
                  Output(0, 'public double[] Y = new double['+NbOfStateVariablesStr+'];');
                  Output(0, 'public double[] dY = new double['+NbOfStateVariablesStr+'];');
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

               Output(0, 'public double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);

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

               Output(0, 'public double '+RealName+';'+StringOfChar(' ', INDENT)+StatementCommentTag+' '+Units+GenInfo);
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
      Output(0, 'public double fact(double n)');
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

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['log']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Decimal logarithm');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double log10(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return Math.log(n)/Math.log(10.0);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sec']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Secant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double sec(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/Math.cos(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csc']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Cosecant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double csc(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/Math.sin(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cot']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Cotangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double cot(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 1.0/Math.tan(n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sinh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csch']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Hyperbolic sinus');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double sinh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return (Math.exp(n)-Math.exp(-(n)))/2.0;');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cosh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Hyperbolic cosinus');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double cosh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return (Math.exp(n)+Math.exp(-(n)))/2.0;');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['tanh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['coth']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Hyperbolic tangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double tanh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'double exp_pn = Math.exp(n);');
      Output(0, 'double exp_mn = Math.exp(-n);');
      Output(0);
      Output(0, 'return (exp_pn-exp_mn)/(exp_pn+exp_mn);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Hyperbolic secant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double sech(double n)');
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
      Output(0, 'public double csch(double n)');
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
      Output(0, 'public double coth(double n)');
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
      Output(0, 'public double asec(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return Math.acos(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsc']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse cosecant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double acsc(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return Math.asin(1.0/n);');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acot']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse cotangent');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double acot(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return Math.atan(1.0/n);');

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
      Output(0, 'public double asinh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return Math.log(n+Math.pow(Math.pow(n, 2.0)+1.0, 0.5));');

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
      Output(0, 'public double acosh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return Math.log(n+Math.pow(Math.pow(n, 2.0)-1.0, 0.5));');

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
      Output(0, 'public double atanh(double n)');
      Output(0, '{');

      IncIndent(0);

      Output(0, 'return 0.5*Math.log((1.0+n)/(1.0-(n)));');

      DecIndent(0);

      Output(0, '}');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then Begin
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, StatementCommentTag+' Inverse hyperbolic secant');
      Output(0, StatementCommentTag+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'public double asech(double n)');
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
      Output(0, 'public double acsch(double n)');
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
      Output(0, 'public double acoth(double n)');
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
   Output(0, 'public void init()');
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

               YNames[VarIndex]      := 'YNames['+IntToStr(VarIndex)+'] = "'+RealName+'";';
               YUnits[VarIndex]      := 'YUnits['+IntToStr(VarIndex)+'] = "'+Units+'";';
               YComponents[VarIndex] := 'YComponents['+IntToStr(VarIndex)+'] = "'+Component+'";';

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
   Output(0, 'public void compute(double '+tStr+')');
   Output(0, '{');

   IncIndent(0);

   Output(0, StatementCommentTag+' '+tStr+': time ('+CellMLModel.FreeVariableUnits.Name+')');
   Output(0);

   GenerateMathMLEquations(toeCompute);

   DecIndent(0);

   Output(0, '}');

   DecIndent(0);

   Output(0, '}');
   Output(0);
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
   Output(0, StatementCommentTag+' End of file');
   Output(0, StatementCommentTag+StringOfChar('=', 78-Length(CellMLIndent[0])));
End;

//==============================================================================

Function TCellMLAPIToJavaFileEngine.AddFunc(Const aCellMLComponent: String;
                                            Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitLN:    Result := 'Math.log';
      mitLog:   Result := 'log10';
      mitFact:  Result := 'fact';
      mitSec:   Result := 'sec';
      mitCsc:   Result := 'csc';
      mitCot:   Result := 'cot';
      mitSinH:  Result := 'sinh';
      mitCosH:  Result := 'cosh';
      mitTanH:  Result := 'tanh';
      mitSecH:  Result := 'sech';
      mitCscH:  Result := 'csch';
      mitCotH:  Result := 'coth';
      mitASec:  Result := 'asec';
      mitACsc:  Result := 'acsc';
      mitACot:  Result := 'acot';
      mitASinH: Result := 'asinh';
      mitACosH: Result := 'acosh';
      mitATanH: Result := 'atanh';
      mitASecH: Result := 'asech';
      mitACscH: Result := 'acsch';
      mitACotH: Result := 'acoth';
   Else
      If (CompareStr(aMathMLEquationBinTree.Str, '') <> 0) Then
         Result := 'Math.'+aMathMLEquationBinTree.Str
      Else
         Result := '';
   End;

   Result := Result+'('+GenerateEquation(aCellMLComponent, aMathMLEquationBinTree.Left)+')';
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

