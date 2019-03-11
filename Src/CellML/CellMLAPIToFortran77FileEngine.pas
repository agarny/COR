//==============================================================================
// Engine class for exporting a CellML API object to a Fortran 77 file
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 26/05/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToFortran77FileEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPI, CellMLAPIExportEngine;

//==============================================================================

Type
   TCellMLAPIToFortran77FileEngine = Class(TCellMLAPIExportEngine)
      Private
         // Properties used for internal purposes

         HasConstants: Boolean;
         HasCompVars: Boolean;

         YNamesLength: Integer;
         YUnitsLength: Integer;
         YComponentsLength: Integer;

      Protected
         // Methods used for internal purposes

         Procedure Common(Const aIndex: Integer; Const aCommonLab: String; aCommonVal: String);
         Procedure StateVars(Const aIndex: Integer); Inline;
         Procedure StateVarsDeriv(Const aIndex: Integer); Inline;
         Procedure YNamesDecl(Const aIndex: Integer); Inline;
         Procedure YUnitsDecl(Const aIndex: Integer); Inline;
         Procedure YComponentsDecl(Const aIndex: Integer); Inline;
         Procedure Constants(Const aIndex: Integer);
         Procedure CompVars(Const aIndex: Integer);

         Procedure GenerateFFile;
         Procedure GenerateIncFile;

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
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
   SysUtils, StrUtils, DeCAL, Common;

//==============================================================================

Constructor TCellMLAPIToFortran77FileEngine.Create(Const aCellMLModel: TCellMLModel;
                                                   Const aFileName: Array Of String);
Begin
   Inherited Create(aCellMLModel, aFileName);

   CaseSensitive := False;

   StatementSep := UNDEFINED_STATEMENT_SEP;

   StatementLen := 72;
   StatementCommentTag := '!';
   StatementContTag := '''';
   StatementContTagLoc := 6;

   OutputNbExpTag := 'D';
   OutputNbTail := 'D0';

   IfThenElseStatementsWithStatementSep := False;

   IfStatement     := 'IF ';
   ThenStatement   := ' THEN';
   ElseIfStatement := 'ELSE IF ';
   ElseStatement   := 'ELSE';
   EndIfStatement  := 'END IF';

   EqStr   := ' = ';
   EqEqStr := ' .EQ. ';
   NEqStr  := ' .NE. ';
   GTStr   := ' .GT. ';
   LTStr   := ' .LT. ';
   GEqStr  := ' .GE. ';
   LEqStr  := ' .LE. ';
   AndStr  := ' .AND. ';
   OrStr   := ' .OR. ';
   XOrStr  := ' .XOR. ';

   PowerStr := '**';
   PowerAsOperand := True;
   RootAsOperand  := True;

   NotStr      := '.NOT.';
   NotSpaceStr := ' ';

   YStart := 'Y(';
   YEnd   := ')';

   dYStart := 'DY(';
   dYEnd   := ')';

   LogStr := 'LOG';

   SqrStr  := '';
   SqrtStr := 'SQRT';

   TrueStr  := '.TRUE.';
   FalseStr := '.FALSE.';

   PIStr := OutputNb(PI);
   // Note: it's a repeat from the default constructor, but it's necessary
   //       because it requires a valid value for "OutputNbExpTag" and
   //       "OutputNbTail"...

   ExponentialeStr := OutputNb(Exp(1));
   // Note: it's a repeat from the default constructor, but it's necessary
   //       because it requires a valid value for "OutputNbExpTag" and
   //       "OutputNbTail"...
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.Execute;
Begin
   Inherited;

   GenerateIncFile;
   GenerateFFile;
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.Common(Const aIndex: Integer;
                                                 Const aCommonLab: String;
                                                 aCommonVal: String);
Const
   CommonLabLen = 12;
   // COMMON /<aCommonLab><CommonLabIndex>/
Var
   CommonMaxLen: Integer;
   CommonLabIndex: Integer;
   CommonValLim: Integer;
Begin
   CommonMaxLen := 100*(StatementLen-FORTRAN77_INDENT-INDENT);
   // In Fortran 77, a statement can have 1 initial line and 99 continuation
   // lines, so...

   CommonLabIndex := 1;

   While CommonLabLen+Length(aCommonVal) > CommonMaxLen Do Begin
      CommonValLim := CommonMaxLen-CommonLabLen;

      // We can only split a string before a ",", so...

      While (aCommonVal[CommonValLim] <> ',') And (aCommonVal[CommonValLim] <> ' ') Do
         Dec(CommonValLim);

      While aCommonVal[CommonValLim] <> ',' Do
         Dec(CommonValLim);

      Output(aIndex, 'COMMON /'+aCommonLab+Format('%.3d', [CommonLabIndex])+'/ '+TrimLeft(Copy(aCommonVal, 1, CommonValLim-1)));

      aCommonVal := Copy(aCommonVal, CommonValLim+1, Length(aCommonVal)-CommonValLim);

      Inc(CommonLabIndex);
   End;

   // Last bit...

   Output(aIndex, 'COMMON /'+aCommonLab+Format('%.3d', [CommonLabIndex])+'/ '+TrimLeft(aCommonVal));
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.StateVars(Const aIndex: Integer);
Begin
   Output(aIndex, 'DOUBLE PRECISION Y('+IntToStr(CellMLModel.NbOfStateVariables)+')');
   Output(aIndex, 'COMMON /Y/ Y');
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.StateVarsDeriv(Const aIndex: Integer);
Begin
   Output(aIndex, 'DOUBLE PRECISION DY('+IntToStr(CellMLModel.NbOfStateVariables)+')');
   Output(aIndex, 'COMMON /DY/ DY');
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.YNamesDecl(Const aIndex: Integer);
Begin
   Output(aIndex, 'CHARACTER YNames('+IntToStr(CellMLModel.NbOfStateVariables)+')*'+IntToStr(YNamesLength));
   Output(aIndex, 'COMMON /YNames/ YNames');
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.YUnitsDecl(Const aIndex: Integer);
Begin
   Output(aIndex, 'CHARACTER YUnits('+IntToStr(CellMLModel.NbOfStateVariables)+')*'+IntToStr(YUnitsLength));
   Output(aIndex, 'COMMON /YUnits/ YUnits');
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.YComponentsDecl(Const aIndex: Integer);
Begin
   Output(aIndex, 'CHARACTER YComponents('+IntToStr(CellMLModel.NbOfStateVariables)+')*'+IntToStr(YComponentsLength));
   Output(aIndex, 'COMMON /YComponents/ YComponents');
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.Constants(Const aIndex: Integer);
Var
   Iter: Integer;
   ConstantsStr: String;
Begin
   If (Not HasConstants) Then
      Exit;

   ConstantsStr := '';

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
         If (State = vsConstant) Then Begin
            Output(aIndex, 'DOUBLE PRECISION '+RealName+StringOfChar(' ', INDENT)+'! '+Units+GenInfo);

            ConstantsStr := ConstantsStr+RealName+', ';
         End;

   ConstantsStr := Copy(ConstantsStr, 1, Length(ConstantsStr)-2);
   // Note: to get rid of the tailing ", "...

   Output(aIndex);

   Common(aIndex, 'CONSTANTS', ConstantsStr);
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.CompVars(Const aIndex: Integer);
Var
   Iter: Integer;
   CompVarsStr: String;
Begin
   If (Not HasCompVars) Then
      Exit;

   CompVarsStr := '';

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
         If (State = vsComputed) Then Begin
            Output(aIndex, 'DOUBLE PRECISION '+RealName+StringOfChar(' ', INDENT)+'! '+Units+GenInfo);

            CompVarsStr := CompVarsStr+RealName+', ';
         End;

   CompVarsStr := Copy(CompVarsStr, 1, Length(CompVarsStr)-2);
   // Note: to get rid of the tailing ", "...

   Output(aIndex);

   Common(aIndex, 'COMPVARS', CompVarsStr);
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.GenerateFFile;
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
   NeedEmptyLine: Boolean;
Begin
   Output(0, '!'+StringOfChar('=', 71-Length(CellMLIndent[0])));
   Output(0, '! CellML file:   '+CellMLModel.FileName);
   Output(0, '! CellML model:  '+CellMLModel.Name);
   Output(0, '! Date and time: '+ReplaceStr(DateTimeToStr(Now), ' ', ' at '));
   Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
{$IFDEF COR_SPECIFIC}
   Output(0, '! Conversion from '+CELLML_VERSION+' to Fortran 77 was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(0, '!    '+COR_FULL_COPYRIGHT);
   Output(0, '!    '+COR_URL+' - '+COR_EMAIL);
   Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
{$ENDIF}
   Output(0, '! '+CELLML_URL);
   Output(0, '!'+StringOfChar('=', 71-Length(CellMLIndent[0])));
   Output(0);

   IncIndent(0, efFortran77);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['floor']))) Then Begin
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0, '! Floor');
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0);

      Output(0, 'DOUBLE PRECISION FUNCTION DFLOOR(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'IF (N .GE. 0.0D0) THEN');

      IncIndent(0, efFortran77);

      Output(0, 'DFLOOR = INT(N)');

      DecIndent(0, efFortran77);

      Output(0, 'ELSE IF (N .EQ. INT(N)) THEN');

      IncIndent(0, efFortran77);

      Output(0, 'DFLOOR = N');

      DecIndent(0, efFortran77);

      Output(0, 'ELSE');

      IncIndent(0, efFortran77);

      Output(0, 'DFLOOR = INT(N)-1.0D0');

      DecIndent(0, efFortran77);

      Output(0, 'END IF');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['ceil']))) Then Begin
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0, '! Ceil');
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0);

      Output(0, 'DOUBLE PRECISION FUNCTION DCEIL(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'IF (N .LE. 0.0D0) THEN');

      IncIndent(0, efFortran77);

      Output(0, 'DCEIL = INT(N)');

      DecIndent(0, efFortran77);

      Output(0, 'ELSE IF (N .EQ. INT(N)) THEN');

      IncIndent(0, efFortran77);

      Output(0, 'DCEIL = N');

      DecIndent(0, efFortran77);

      Output(0, 'ELSE');

      IncIndent(0, efFortran77);

      Output(0, 'DCEIL = INT(N)+1.0D0');

      DecIndent(0, efFortran77);

      Output(0, 'END IF');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);
         
      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then Begin
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0, '! Factorial');
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0);

      Output(0, 'DOUBLE PRECISION FUNCTION DFACT(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DFACT = 1.0D0');
      Output(0);
      Output(0, 'DO WHILE (N .GT. 1.0D0)');

      IncIndent(0, efFortran77);

      Output(0, 'DFACT = N*DFACT');
      Output(0);
      Output(0, 'N = N-1.0D0');

      DecIndent(0, efFortran77);

      Output(0, 'ENDDO');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sec']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Secant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DSEC(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DSEC = 1.0D0/DCOS(N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csc']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Cosecant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DCSC(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DCSC = 1.0D0/DSIN(N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cot']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Cotangent');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DCOT(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DCOT = 1.0D0/DTAN(N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Hyperbolic secant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DSECH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DSECH = 1.0D0/DCOSH(N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csch']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Hyperbolic cosecant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DCSCH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DCSCH = 1.0D0/DSINH(N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['coth']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Hyperbolic cotangent');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DCOTH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DCOTH = 1.0D0/DTANH(N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asec']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse secant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DASEC(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DASEC = DACOS(1.0D0/N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsc']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse cosecant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DACSC(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DACSC = DASIN(1.0/N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acot']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse cotangent');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DACOT(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DACOT = DATAN(1.0D0/N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asinh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse hyperbolic sinus');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DASINH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DASINH = DLOG10(N+(N**2.0D0+1.0D0)**0.5D0)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acosh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse hyperbolic cosinus');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DACOSH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DACOSH = DLOG10(N+(N**2.0D0-1.0D0)**0.5D0)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['atanh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse hyperbolic tangent');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DATANH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DATANH = 0.5D0*DLOG10((1.0D0+N)/(1.0D0-(N)))');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse hyperbolic secant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DASECH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DOUBLE PRECISION DACOSH');
      Output(0);
      Output(0, 'DASECH = DACOSH(1.0D0/N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse hyperbolic cosecant');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DACSCH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DOUBLE PRECISION DASINH');
      Output(0);
      Output(0, 'DACSCH = DASINH(1.0D0/N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then Begin
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0, '! Inverse hyperbolic cotangent');
      Output(0, '!'+StringOfChar('-', 78-Length(CellMLIndent[0])));
      Output(0);
      Output(0, 'DOUBLE PRECISION FUNCTION DACOTH(N)');

      IncIndent(0, efFortran77);

      Output(0, 'IMPLICIT NONE');
      Output(0);
      Output(0, 'DOUBLE PRECISION N');
      Output(0);
      Output(0, 'DOUBLE PRECISION DATANH');
      Output(0);
      Output(0, 'DACOTH = DATANH(1.0D0/N)');
      Output(0);
      Output(0, 'RETURN');

      DecIndent(0, efFortran77);

      Output(0, 'END');
      Output(0);
   End;

   Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
   Output(0, '! Initialisation');
   Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
   Output(0);

   Output(0, 'SUBROUTINE INIT');

   IncIndent(0, efFortran77);

   Output(0, 'IMPLICIT NONE');
   Output(0);

   StateVars(0);

   Output(0);

   YNamesDecl(0);
   YUnitsDecl(0);
   YComponentsDecl(0);

   Output(0);

   Constants(0);

   Output(0);

   CompVars(0);

   Output(0);

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
                  Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
                  Output(0, '! State variables');
                  Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, 'Y('+IntToStr(VarIndex)+') = '+OutputNb(InitialValue)+StringOfChar(' ', INDENT)+'! '+RealName+' ('+Units+')'+GenInfo);

               YNames[VarIndex-1]      := 'YNames('+IntToStr(VarIndex)+') = '''+RealName+'''';
               YUnits[VarIndex-1]      := 'YUnits('+IntToStr(VarIndex)+') = '''+Units+'''';
               YComponents[VarIndex-1] := 'YComponents('+IntToStr(VarIndex)+') = '''+Component+'''';

               NeedEmptyLine := True;

               NeedYExtra := True;
            End;
            vsConstant: Begin
               If (PrevState <= vsState) Then Begin
                  If (NeedYExtra) Then
                     YExtra;

                  If (NeedEmptyLine) Then
                     Output(0);

                  Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
                  Output(0, '! Constants');
                  Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
                  Output(0);
               End;

               Output(0, RealName+' = '+OutputNb(InitialValue)+StringOfChar(' ', INDENT)+'! '+Units+GenInfo);
            End;
         End;

         PrevState := State;
      End;

   If (NeedYExtra) Then
      YExtra;

   Output(0);

   If (CellMLModel.HasAtLeastOneComputeOnceEquation) Then Begin
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0, '! Computed variables');
      Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
      Output(0);

      GenerateMathMLEquations(toeComputeOnce);

      Output(0);
   End;

   Output(0, 'RETURN');

   DecIndent(0, efFortran77);

   Output(0, 'END');
   Output(0);
   Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
   Output(0, '! Computation');
   Output(0, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
   Output(0);
   Output(0, 'SUBROUTINE COMPUTE('+tStr+')');

   IncIndent(0, efFortran77);

   Output(0, 'IMPLICIT NONE');
   Output(0);
   Output(0, 'DOUBLE PRECISION '+tStr+'');
   Output(0, '! '+tStr+': time ('+CellMLModel.FreeVariableUnits.Name+')');
   Output(0);

   StateVars(0);
   StateVarsDeriv(0);

   Output(0);

   Constants(0);

   Output(0);

   CompVars(0);

   Output(0);

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['floor']))) Then
      Output(0, 'DOUBLE PRECISION DFLOOR');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['ceil']))) Then
      Output(0, 'DOUBLE PRECISION DCEIL');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['fact']))) Then
      Output(0, 'DOUBLE PRECISION DFACT');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sec']))) Then
      Output(0, 'DOUBLE PRECISION DSEC');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csc']))) Then
      Output(0, 'DOUBLE PRECISION DCSC');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['cot']))) Then
      Output(0, 'DOUBLE PRECISION DCOT');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['sech']))) Then
      Output(0, 'DOUBLE PRECISION DSECH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['csch']))) Then
      Output(0, 'DOUBLE PRECISION DCSCH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['coth']))) Then
      Output(0, 'DOUBLE PRECISION DCOTH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asec']))) Then
      Output(0, 'DOUBLE PRECISION DASEC');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsc']))) Then
      Output(0, 'DOUBLE PRECISION DACSC');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acot']))) Then
      Output(0, 'DOUBLE PRECISION DACOT');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asinh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then
      Output(0, 'DOUBLE PRECISION DASINH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acosh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then
      Output(0, 'DOUBLE PRECISION DACOSH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['atanh'])) Or
       Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then
      Output(0, 'DOUBLE PRECISION DATANH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['asech']))) Then
      Output(0, 'DOUBLE PRECISION DASECH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acsch']))) Then
      Output(0, 'DOUBLE PRECISION DACSCH');

   If (Not AtEnd(BinarySearch(CellMLModel.RequiredFunctions, ['acoth']))) Then
      Output(0, 'DOUBLE PRECISION DACOTH');

   Output(0);   // Note: may not always be relevant, depending on whether
                //       there is/are required function/s

   GenerateMathMLEquations(toeCompute);

   Output(0);

   Output(0, 'RETURN');

   DecIndent(0, efFortran77);

   Output(0, 'END');

   DecIndent(0, efFortran77);

   Output(0);
   Output(0, '!'+StringOfChar('=', 71-Length(CellMLIndent[0])));
   Output(0, '! End of file');
   Output(0, '!'+StringOfChar('=', 71-Length(CellMLIndent[0])));
End;

//==============================================================================

Procedure TCellMLAPIToFortran77FileEngine.GenerateIncFile;
Var
   NeedYExtra: Boolean;
   Procedure YExtra;
   Begin
      Output(1);

      YNamesDecl(1);
      YUnitsDecl(1);
      YComponentsDecl(1);

      NeedYExtra := False;
   End;
Var
   Iter: Integer;
   PrevState: TCellMLModelVariableState;
   VarIndexVal: Integer;
   NeedEmptyLine: Boolean;
   NeedConstants, NeedCompVars: Boolean;
Begin
   Output(1, '!'+StringOfChar('=', 71-Length(CellMLIndent[1])));
   Output(1, '! CellML file:   '+CellMLModel.FileName);
   Output(1, '! CellML model:  '+CellMLModel.Name);
   Output(1, '! Date and time: '+ReplaceStr(DateTimeToStr(Now), ' ', ' at '));
   Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
{$IFDEF COR_SPECIFIC}
   Output(1, '! Conversion from '+CELLML_VERSION+' to Fortran 77 (include) was done using '+COR_NAME+' ('+COR_VERSION+')');
   Output(1, '!    '+COR_FULL_COPYRIGHT);
   Output(1, '!    '+COR_URL+' - '+COR_EMAIL);
   Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[0])));
{$ENDIF}
   Output(1, '! '+CELLML_URL);
   Output(1, '!'+StringOfChar('=', 71-Length(CellMLIndent[1])));
   Output(1);

   PrevState := vsUnknown;

   VarIndexVal := 1;

   NeedEmptyLine := False;

   NeedConstants := False;
   NeedCompVars  := False;

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do Begin
         Case State Of
            vsState: Begin
               If (PrevState <= vsFree) Then Begin
                  IncIndent(1, efFortran77);

                  Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
                  Output(1, '! State variables');
                  Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
                  Output(1);
                  Output(1, 'INTEGER '+NbOfStateVariablesStr+'');
                  Output(1, 'PARAMETER ('+NbOfStateVariablesStr+' = '+IntToStr(CellMLModel.NbOfStateVariables)+')');
                  Output(1);

                  StateVars(1);
                  StateVarsDeriv(1);
               End;

               VarIndex := VarIndexVal;

               Output(1, '! '+IntToStr(VarIndex)+': '+RealName+' ('+Units+')'+GenInfo);

{$IFDEF OPT_MATH}
               YNamesLength      := OptMaxI(YNamesLength, Length(RealName));
               YUnitsLength      := OptMaxI(YUnitsLength, Length(Units));
               YComponentsLength := OptMaxI(YComponentsLength, Length(Component));
{$ELSE}
               YNamesLength      := Max(YNamesLength, Length(RealName));
               YUnitsLength      := Max(YUnitsLength, Length(Units));
               YComponentsLength := Max(YComponentsLength, Length(Component));
{$ENDIF}

               Inc(VarIndexVal);

               NeedYExtra := True;

               NeedEmptyLine := True;
            End;
            vsConstant: Begin
               If (PrevState <= vsState) Then Begin
                  If (NeedYExtra) Then
                     YExtra;

                  If (NeedEmptyLine) Then
                     Output(1);

                  Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
                  Output(1, '! Constants');
                  Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
                  Output(1);

                  HasConstants  := True;
                  NeedConstants := True;
               End;

               NeedEmptyLine := True;
            End;
            vsComputed: Begin
               If (PrevState <= vsConstant) Then Begin
                  If (NeedYExtra) Then
                     YExtra;

                  If (NeedConstants) Then Begin
                     Constants(1);

                     NeedConstants := False;
                  End;

                  If (NeedEmptyLine) Then
                     Output(1);

                  Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
                  Output(1, '! Computed variables');
                  Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
                  Output(1);

                  HasCompVars  := True;
                  NeedCompVars := True;
               End;
            End;
         End;

         PrevState := State;
      End;

   If (NeedYExtra) Then
      YExtra;

   If (NeedConstants) Then
      Constants(1);

   If (NeedCompVars) Then
      CompVars(1);

   Output(1);
   Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
   Output(1, '! User''s methods');
   Output(1, '!'+StringOfChar('-', 71-Length(CellMLIndent[1])));
   Output(1);
   Output(1, 'EXTERNAL INIT');
   Output(1, 'EXTERNAL COMPUTE');

   DecIndent(1, efFortran77);

   Output(1);
   Output(1, '!'+StringOfChar('=', 71-Length(CellMLIndent[1])));
   Output(1, '! End of file');
   Output(1, '!'+StringOfChar('=', 71-Length(CellMLIndent[1])));
End;

//==============================================================================

Function TCellMLAPIToFortran77FileEngine.AddFunc(Const aCellMLComponent: String;
                                                 Const aMathMLEquationBinTree: TMathMLCommandBinTree): String;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitAbs:   Result := 'DABS';
      mitExp:   Result := 'DEXP';
      mitLN:    Result := 'DLOG';
      mitLog:   Result := 'DLOG10';
      mitCeil:  Result := 'DCEIL';
      mitFloor: Result := 'DFLOOR';
      mitFact:  Result := 'DFACT';
      mitSin:   Result := 'DSIN';
      mitCos:   Result := 'DCOS';
      mitTan:   Result := 'DTAN';
      mitSec:   Result := 'DSEC';
      mitCsc:   Result := 'DCSC';
      mitCot:   Result := 'DCOT';
      mitSinH:  Result := 'DSINH';
      mitCosH:  Result := 'DCOSH';
      mitTanH:  Result := 'DTANH';
      mitSecH:  Result := 'DSECH';
      mitCscH:  Result := 'DCSCH';
      mitCotH:  Result := 'DCOTH';
      mitASin:  Result := 'DASIN';
      mitACos:  Result := 'DACOS';
      mitATan:  Result := 'DATAN';
      mitASec:  Result := 'DASEC';
      mitACsc:  Result := 'DACSC';
      mitACot:  Result := 'DACOT';
      mitASinH: Result := 'DASINH';
      mitACosH: Result := 'DACOSH';
      mitATanH: Result := 'DATANH';
      mitASecH: Result := 'DASECH';
      mitACscH: Result := 'DACSCH';
      mitACotH: Result := 'DACOTH';
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

