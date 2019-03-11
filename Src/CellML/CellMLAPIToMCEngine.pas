//==============================================================================
// Engine class for converting a CellML API object to machine code
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 22/12/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPIToMCEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   SysUtils, CellMLAPI, DeCAL;

//==============================================================================

{$M+}

//==============================================================================

Type
   ADouble = Array [0..MaxInt Div SizeOf(Double)-1] Of Double;
   PADouble = ^ADouble;
   TCellMLModelRuntimeInitProc = Procedure(Const aData: PADouble) Of Object;
   TCellMLModelRuntimeComputeOnceProc = Procedure(Const aY, adY, aCsts, aCompVars: PADouble) Of Object;
   TCellMLModelRuntimeComputeProc = Procedure(Const aTime: Double; Const aY, adY, aCsts, aCompVars: PADouble) Of Object;
   TCellMLModelRuntimeInitProcDDecl = Procedure(Const aData: PADouble);
   TCellMLModelRuntimeComputeOnceProcDDecl = Procedure(Const aY, adY, aCsts, aCompVars: PADouble);
   TCellMLModelRuntimeComputeProcDDecl = Procedure(Const aTime: Double; Const aY, adY, aCsts, aCompVars: PADouble);
   TCellMLModelRuntimeInitProcCDecl = Procedure(Const aData: PADouble); CDecl;
   TCellMLModelRuntimeComputeOnceProcCDecl = Procedure(Const aY, adY, aCsts, aCompVars: PADouble); CDecl;
   TCellMLModelRuntimeComputeProcCDecl = Procedure(Const aTime: Double; Const aY, adY, aCsts, aCompVars: PADouble); CDecl;
   TCellMLModelRuntimeComponent = Class;
   TCellMLModelRuntimeBase = Class
      Protected
         // Private representation of published properties

         FOwner: TCellMLModelRuntimeComponent;

         FName: String;

         // Methods to modify the different published properties

         Procedure SetOwner(Const aValue: TCellMLModelRuntimeComponent); Inline;
         Procedure SetName(Const aValue: String); Inline;

      Public
         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLModelRuntimeComponent; Const aName: String);

      Published
         // Published properties

         Property Owner: TCellMLModelRuntimeComponent Read FOwner Write SetOwner;
         Property Name: String Read FName Write SetName;
   End;
   TCellMLModelRuntimeComponent = Class(TCellMLModelRuntimeBase)
      Protected
         // Private representation of published properties

         FComponentsList: DArray;
         FVariablesList: DArray;

      Public
         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLModelRuntimeComponent = Nil; Const aName: String = '');
         Destructor Destroy; Override;

         // User's methods

         Class Function Compare(Const aObj1, aObj2: DObject): Integer; Inline;

      Published
         // Published properties

         Property ComponentsList: DArray Read FComponentsList;
         Property VariablesList: DArray Read FVariablesList;
   End;
   TCellMLModelRuntimeVariable = Class(TCellMLModelRuntimeBase)
      Private
         // Methods to modify the different published properties

         Procedure SetIndex(Const aValue: Integer); Inline;

      Protected
         // Private representation of published properties

         FUnits: String;
         FIndex: Integer;
         FState: TCellMLModelVariableState;

      Public
         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLModelRuntimeComponent = Nil; Const aName: String = ''; Const aUnits: String = ''; Const aState: TCellMLModelVariableState = vsUnknown);

         // User's methods

         Class Function Compare(Const aObj1, aObj2: DObject): Integer; Inline;

      Published
         // Published properties

         Property Units: String Read FUnits;
         Property Index: Integer Read FIndex Write SetIndex;
         Property State: TCellMLModelVariableState Read FState;
   End;
   TCellMLModelRuntimeString = Class
      Private        
         // Methods used for internal purposes

         Procedure CheckCapacity(Const aNeededSpace: Integer); Inline;

      Protected
         // Private representation of published properties

         FSize: Integer;
         FValue: TBytes;

      Public
         // Constructor & Destructor

         Constructor Create;

         // User's methods

         Procedure AddOneByte(Const aByte: Byte);
         Procedure AddTwoBytes(Const aByte1, aByte2: Byte);
         Procedure AddThreeBytes(Const aByte1, aByte2, aByte3: Byte);
         Procedure AddFourBytes(Const aByte1, aByte2, aByte3, aByte4: Byte);

         Procedure AddAddr(Const aAddr: Cardinal);

         Procedure AddString(Const aString: TCellMLModelRuntimeString);

         Procedure Compact; Inline;

         Function Value: TBytes; Inline;

      Published
         // Published properties

         Property Size: Integer Read FSize;
   End;
   TCellMLModelRuntimeRPNStackItemAction = (siaFLD, siaFSTP, siaFCOMP, siaFSTSWAXAndSAHF, siaJumpOrTestJump, siaTest,
                                            siaEqEq, siaNEq, siaLT, siaGT, siaLEq, siaGEq,
                                            siaPlus, siaMinus, siaMinusUnary, siaTimes, siaDivide, siaPow, siaSqr, siaSqrt, siaAbs, siaExp, siaLN, siaLog, siaFloor, siaCeil, siaFact,
                                            siaAnd, siaOr, siaXOr, siaNot,
                                            siaSin, siaCos, siaTan, siaSec, siaCsc, siaCot, siaSinH, siaCosH, siaTanH, siaSecH, siaCscH, siaCotH, siaASin, siaACos, siaATan, siaASec, siaACsc, siaACot, siaASinH, siaACosH, siaATanH, siaASecH, siaACscH, siaACotH);
   TCellMLModelRuntimeRPNStackItemType = (sitTime, sitY, sitdY, sitCstNb, sitCstVar, sitCompVar);
   TCellMLModelRuntimeRPNStackItem = Class
      Private
         // Methods used for internal purposes

         Procedure AddOperator(Const aOperand: TCellMLModelRuntimeRPNStackItemAction; Const aDirect: Boolean = True; Const aInverse: Boolean = False);
         Procedure AddComparison;

         Procedure AddFunc(Var aFuncAddr: Cardinal; Const aTwoParams: Boolean = False; Const aReturnInt: Boolean = False);

         Procedure AddExp;
         Procedure AddLN; Inline;

         Procedure AddUpdateFlags; Inline;

      Public
         // Published properties

         ItemType: TCellMLModelRuntimeRPNStackItemType;

         Action: TCellMLModelRuntimeRPNStackItemAction;
         Indice: Integer;
         CstNb: Double;

         MC: TCellMLModelRuntimeString;

         // Constructor & Destructor

         Constructor Create; Overload;
         Constructor Create(Const aCstNb: Double); Overload;
         Constructor Create(Const aAction: TCellMLModelRuntimeRPNStackItemAction); Overload;
         Constructor Create(Const aAction: TCellMLModelRuntimeRPNStackItemAction; Const aItemType: TCellMLModelRuntimeRPNStackItemType; Const aIndice: Integer); Overload;
         Constructor Create(Const aTestJump: Byte; Const aFullTest: Boolean); Overload;

         Destructor Destroy; Override;

         // User's methods

         Procedure GenerateMC;
   End;
   TCellMLModelRuntimeCompiler = (cInternal, cIntelCPP, cMicrosoftCPP);
   TCellMLModelRuntime = Class
      Private
         // Properties used for internal purposes

         DLLHandle: HMODULE;

         InitStateVarsStr: TCellMLModelRuntimeString;
         InitCstsStr: TCellMLModelRuntimeString;
         ComputeOnceStr: TCellMLModelRuntimeString;
         ComputeStr: TCellMLModelRuntimeString;

         InitStateVarsDDecl: TCellMLModelRuntimeInitProcDDecl;
         InitCstsDDecl: TCellMLModelRuntimeInitProcDDecl;
         ComputeOnceDDecl: TCellMLModelRuntimeComputeOnceProcDDecl;
         ComputeDDecl: TCellMLModelRuntimeComputeProcDDecl;

{$IFDEF COR_SPECIFIC}
         InitStateVarsCDecl: TCellMLModelRuntimeInitProcCDecl;
         InitCstsCDecl: TCellMLModelRuntimeInitProcCDecl;
         ComputeOnceCDecl: TCellMLModelRuntimeComputeOnceProcCDecl;
         ComputeCDecl: TCellMLModelRuntimeComputeProcCDecl;
{$ENDIF}

         // Methods used for internal purposes

         Procedure IntInitStateVars(Const aData: PADouble); Inline;
         Procedure IntInitCsts(Const aData: PADouble); Inline;
         Procedure IntComputeOnce(Const aY, adY, aCsts, aCompVars: PADouble); Inline;
         Procedure IntCompute(Const aTime: Double; Const aY, adY, aCsts, aCompVars: PADouble); Inline;

{$IFDEF COR_SPECIFIC}
         Procedure ExtInitStateVars(Const aData: PADouble); Inline;
         Procedure ExtInitCsts(Const aData: PADouble); Inline;
         Procedure ExtComputeOnce(Const aY, adY, aCsts, aCompVars: PADouble); Inline;
         Procedure ExtCompute(Const aTime: Double; Const aY, adY, aCsts, aCompVars: PADouble); Inline;
{$ENDIF}

      Protected
         // Private representation of published properties

         FName: String;

         FComponentsList: DArray;
         FVariablesList: DArray;

         FNbOfStateVars: Integer;
         FNbOfCsts: Integer;
         FNbOfCompVars: Integer;

         FUseMCMethods: Boolean;

      Public
         // Public properties

         InitStateVars: TCellMLModelRuntimeInitProc;
         InitCsts: TCellMLModelRuntimeInitProc;
         ComputeOnce: TCellMLModelRuntimeComputeOnceProc;
         Compute: TCellMLModelRuntimeComputeProc;

         // Constructor & Destructor

         Constructor Create;
         Destructor Destroy; Override;

         // User's methods

         Procedure Init(Const aY, aCsts: PADouble); Inline;
{$IFDEF COR_SPECIFIC}
         Function GenerateExtProcs(Const aCellMLModel: TCellMLModel; Const aCompiler: TCellMLModelRuntimeCompiler; Const aCompilerLocation: String): Boolean;
{$ENDIF}
      Published
         // Published properties

         Property Name: String Read FName;
         Property ComponentsList: DArray Read FComponentsList;
         Property VariablesList: DArray Read FVariablesList;
         Property NbOfStateVars: Integer Read FNbOfStateVars;
         Property NbOfCsts: Integer Read FNbOfCsts;
         Property NbOfCompVars: Integer Read FNbOfCompVars;
         Property UseMCMethods: Boolean Read FUseMCMethods;
   End;
   TNotifyCellMLAPIToMBeginEvent = Procedure(Const aNbOfSteps: Integer) Of Object;
   TNotifyCellMLAPIToMProgressEvent = Procedure(Const aStepNb, aNbOfSteps: Integer) Of Object;
   TNotifyCellMLAPIToMEndEvent = Procedure Of Object;
   TCellMLAPIToMCEngine = Class
      Private
         // Properties used for internal purposes

         CellMLModel: TCellMLModel;

         CellMLModelRuntime: TCellMLModelRuntime;

         RPNStack: DList;

         DummyComp: TCellMLModelRuntimeComponent;
         DummyVar: TCellMLModelRuntimeVariable;

         NbOfPieceParts: Integer;
         OtherwisePart: Boolean;

         CrtPiecePart: Integer;

         RPNStackJZItemIter: DIterator;
         RPNStackJMPItemIterList: Array Of DIterator;
         RPNStackJMPItemIterListIndex: Integer;

         Compiler: TCellMLModelRuntimeCompiler;
         CompilerLocation: String; 

         // Methods used for internal purposes

         Function FindComponent(Const aComponent: String): TCellMLModelRuntimeComponent;
         Function FindVariable(Const aComponent, aVariable: String): TCellMLModelRuntimeVariable;

         Procedure CreateOperand(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAction: TCellMLModelRuntimeRPNStackItemAction);
         Procedure CreateOneArgFunction(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAction: TCellMLModelRuntimeRPNStackItemAction; Const aLeftArg: Boolean = True); Inline;
         Procedure CreateComparison(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAction: TCellMLModelRuntimeRPNStackItemAction);
         Procedure CreateLogicalOperator(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree; Const aAction: TCellMLModelRuntimeRPNStackItemAction); Inline;
         Procedure CreateJZ;
         Procedure CreateJMPs;

         Procedure ScanPiecewiseStatement(Const aMathMLEquationBinTree: TMathMLCommandBinTree);

         Procedure MathMLEquationToRPNStack(Const aCellMLComponent: String; Const aMathMLEquationBinTree: TMathMLCommandBinTree);

      Protected
         // Private representation of published properties

         FOnBegin: TNotifyCellMLAPIToMBeginEvent;
         FOnProgress: TNotifyCellMLAPIToMProgressEvent;
         FOnEnd: TNotifyCellMLAPIToMEndEvent;

      Public
         // Constructor & Destructor

         Constructor Create(Const aCellMLModel: TCellMLModel; Const aCellMLModelRuntime: TCellMLModelRuntime; Const aCompiler: TCellMLModelRuntimeCompiler = cInternal; Const aCompilerLocation: String = ''; Const aBeginEventHandler: TNotifyCellMLAPIToMBeginEvent = Nil; Const aProgressEventHandler: TNotifyCellMLAPIToMProgressEvent = Nil; Const aEndEventHandler: TNotifyCellMLAPIToMEndEvent = Nil);
         Destructor Destroy; Override;

         // User's methods

         Procedure Execute;

      Published
         Property OnBegin: TNotifyCellMLAPIToMBeginEvent Read FOnBegin Write FOnBegin Default Nil;
         Property OnProgress: TNotifyCellMLAPIToMProgressEvent Read FOnProgress Write FOnProgress Default Nil;
         Property OnEnd: TNotifyCellMLAPIToMEndEvent Read FOnEnd Write FOnEnd Default Nil;
   End;

//==============================================================================

Var
   Compilers: DArray;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
{$IFDEF COR_SPECIFIC}
   CORCommon, CellMLAPIToCMCFileEngine,
{$ENDIF}
   Windows, Common;

//==============================================================================

Const
   JMP  = $01;
   JB   = $02;
   JNB  = $03;
   JZ   = $04;
   JNZ  = $05;
   JBE  = $06;
   JNBE = $07;

//==============================================================================

Var
   CellMLModelRuntimeCounter: Integer;

   Constants: DArray;          // List of constants

{$IFDEF OPT_MATH}
   ExpAddr: Cardinal;          // Address of the exponential function
{$ENDIF}

   CeilAddr: Cardinal;         // Address of the ceil function
   FloorAddr: Cardinal;        // Address of the floor function

   PowerAddr: Cardinal;        // Address of the power function

   DblVal1, DblVal2: Double;   // Properties used for internal purposes

//==============================================================================

Function ConstantsCompare(aPtr: Pointer;
                          Const aObj1, aObj2: DObject): Integer; Inline;
Begin
   If (Double(aObj1.VPointer^) < Double(aObj2.VPointer^)) Then
      Result := -1
   Else If (Double(aObj1.VPointer^) > Double(aObj2.VPointer^)) Then
      Result := 1
   Else
      Result := 0;
End;

//==============================================================================

Function GetAddrOfCstNb(Const aCstNb: Double): Cardinal;
Var
   ConstantIter: DIterator;
   CstValPtr: ^Double;
Begin
   New(CstValPtr);

   CstValPtr^ := aCstNb;

   ConstantIter := BinarySearch(Constants, [CstValPtr]);

   If (Not AtEnd(ConstantIter)) Then Begin
      Result := Cardinal(GetRef(ConstantIter).VPointer);

      Dispose(CstValPtr);
   End Else Begin
      Constants.Add([CstValPtr]);

      If ((Constants.Size >= 2) And
          ((Double(GetRef(AdvanceByF(Constants.Finish, -2))^) > Double(GetRef(AdvanceByF(Constants.Finish, -1))^)))) Then
         // Need to sort, so...

         Sort(Constants);

      Result := Cardinal(CstValPtr);
   End;
End;

//==============================================================================

Constructor TCellMLModelRuntimeBase.Create(Const aOwner: TCellMLModelRuntimeComponent;
                                           Const aName: String);
Begin
   FOwner := aOwner;

   FName := aName;
End;

//==============================================================================

Procedure TCellMLModelRuntimeBase.SetOwner(Const aValue: TCellMLModelRuntimeComponent);
Begin
   If (aValue <> FOwner) Then
      FOwner := aValue;
End;

//==============================================================================

Procedure TCellMLModelRuntimeBase.SetName(Const aValue: String);
Begin
   If (CompareStr(aValue, FName) <> 0) Then
      FName := aValue;
End;

//==============================================================================

Constructor TCellMLModelRuntimeComponent.Create(Const aOwner: TCellMLModelRuntimeComponent;
                                                Const aName: String);
Begin
   Inherited;

   FComponentsList := DArray.CreateWith(TCellMLModelRuntimeComponent.Compare);
   FVariablesList  := DArray.CreateWith(TCellMLModelRuntimeVariable.Compare);
End;

//==============================================================================

Destructor TCellMLModelRuntimeComponent.Destroy;
Begin
   FComponentsList.Free;
   FVariablesList.Free;
End;

//==============================================================================

Class Function TCellMLModelRuntimeComponent.Compare(Const aObj1, aObj2: DObject): Integer;
Begin
   Result := CompareStr(TCellMLModelRuntimeComponent(aObj1.VObject).Name,
                        TCellMLModelRuntimeComponent(aObj2.VObject).Name);
End;

//==============================================================================

Constructor TCellMLModelRuntimeVariable.Create(Const aOwner: TCellMLModelRuntimeComponent;
                                               Const aName, aUnits: String;
                                               Const aState: TCellMLModelVariableState);
Begin
   Inherited Create(aOwner, aName);

   FUnits := aUnits;
   FState := aState;
End;

//==============================================================================

Class Function TCellMLModelRuntimeVariable.Compare(Const aObj1, aObj2: DObject): Integer;
Var
   Var1, Var2: TCellMLModelRuntimeVariable;
Begin
   Var1 := TCellMLModelRuntimeVariable(aObj1.VObject);
   Var2 := TCellMLModelRuntimeVariable(aObj2.VObject);

   Result := CompareStr(Var1.Owner.Name+Var1.Name, Var2.Owner.Name+Var2.Name);
End;

//==============================================================================

Procedure TCellMLModelRuntimeVariable.SetIndex(Const aValue: Integer);
Begin
   If (aValue <> FIndex) Then
      FIndex := aValue;
End;

//==============================================================================

Constructor TCellMLModelRuntimeString.Create;
Begin
   FSize := 0;

   SetLength(FValue, 32768);   // Kind of an optimal value (i.e. it should
                               // prevent the runtime string from being resized
                               // and therefore unnecessarily waste of time)
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.CheckCapacity(Const aNeededSpace: Integer);
Begin
   If (Length(FValue) < FSize+aNeededSpace) Then
{$IFDEF OPT_MATH}
      SetLength(FValue, Round(OptPower(2, OptCeil(OptLog10(FSize+aNeededSpace)/OptLog10(2)))));
{$ELSE}
      SetLength(FValue, Round(Power(2, Ceil(Log10(FSize+aNeededSpace)/Log10(2)))));
{$ENDIF}
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.AddOneByte(Const aByte: Byte);
Begin
   CheckCapacity(1);

   FValue[FSize] := Byte(aByte);

   Inc(FSize);
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.AddTwoBytes(Const aByte1, aByte2: Byte);
Begin
   CheckCapacity(2);

   FValue[FSize] := Byte(aByte1);

   Inc(FSize);

   FValue[FSize] := Byte(aByte2);

   Inc(FSize);
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.AddThreeBytes(Const aByte1, aByte2, aByte3: Byte);
Begin
   CheckCapacity(3);

   FValue[FSize] := Byte(aByte1);

   Inc(FSize);

   FValue[FSize] := Byte(aByte2);

   Inc(FSize);

   FValue[FSize] := Byte(aByte3);

   Inc(FSize);
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.AddFourBytes(Const aByte1, aByte2, aByte3, aByte4: Byte);
Begin
   CheckCapacity(4);

   FValue[FSize] := Byte(aByte1);

   Inc(FSize);

   FValue[FSize] := Byte(aByte2);

   Inc(FSize);

   FValue[FSize] := Byte(aByte3);

   Inc(FSize);

   FValue[FSize] := Byte(aByte4);

   Inc(FSize);
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.AddAddr(Const aAddr: Cardinal);
Begin
   CheckCapacity(4);

   FValue[FSize] := Byte((aAddr ShL 24) ShR 24);

   Inc(FSize);

   FValue[FSize] := Byte((aAddr ShL 16) ShR 24);

   Inc(FSize);

   FValue[FSize] := Byte((aAddr ShL 8) ShR 24);

   Inc(FSize);

   FValue[FSize] := Byte(aAddr ShR 24);

   Inc(FSize);
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.AddString(Const aString: TCellMLModelRuntimeString);
Begin
   CheckCapacity(aString.Size);

   Move(aString.Value[0], FValue[FSize], aString.Size);

   Inc(FSize, aString.Size);
End;

//==============================================================================

Procedure TCellMLModelRuntimeString.Compact;
Begin
   SetLength(FValue, FSize);
End;

//==============================================================================

Function TCellMLModelRuntimeString.Value: TBytes;
Begin
   Result := FValue;
End;

//==============================================================================

Constructor TCellMLModelRuntimeRPNStackItem.Create;
Begin
   // Constructor for loading time 

   ItemType := sitTime;

   Action := siaFLD;

   MC := TCellMLModelRuntimeString.Create;
End;

//==============================================================================

Constructor TCellMLModelRuntimeRPNStackItem.Create(Const aCstNb: Double);
Begin
   // Constructor for loading a constant number

   ItemType := sitCstNb;

   Action := siaFLD;
   CstNb  := aCstNb;

   MC := TCellMLModelRuntimeString.Create;
End;

//==============================================================================

Constructor TCellMLModelRuntimeRPNStackItem.Create(Const aAction: TCellMLModelRuntimeRPNStackItemAction);
   Function MCPower(Const Base, Exponent: Double): Double;
      Function MCIntPower(Const Base, Exponent: Double): Double;
      Asm
         MOV    ECX, EAX
         CDQ
         FLD1
         XOR    EAX, EDX
         SUB    EAX, EDX
         JZ     @@3
         FLD    Base
         JMP    @@2
      @@1:
         FMUL   ST, ST
      @@2:
         SHR    EAX, 1
         JNC    @@1
         FMUL   ST(1), ST
         JNZ    @@1
         FSTP   ST
         CMP    ECX, 0
         JGE    @@3
         FLD1
         FDIVRP
      @@3:
      End;
      Function MCTrunc(Const aNb: Double): Integer;
      Var
         OldCW, NewCW : Word;
      Asm
         // Note: adapted from Trunc32DoubleJOH
         //       (http://fastcode.sourceforge.net/challenge_content/Trunc32.html)

         FNSTCW OldCW        // Save current control word
         MOVZX  EAX, OldCW
         OR     AX, $0F00    // Set bits 8, 9, 10 and 11
         MOV    NewCW, AX    // Bits 8/9 = precision, bits 10/11 = rounding mode
         FLDCW  NewCW        // Set round towards 0 mode with 64-bit precision
         FLD    aNb
         FISTP  Result
         FLDCW  OldCW        // Restore original control word
      End;
   Begin
      If ((Frac(Exponent) = 0) And (Abs(Exponent) <= MaxInt)) Then
         Result := MCIntPower(Base, Integer(MCTrunc(Exponent)))
      Else
         Result := Exp(Exponent*Ln(Base));
   End;
   Function MCFloor(Const aNb: Double): Integer;
   Asm
      // Note: from Floor32_PLR_IA32_2
      //       (http://fastcode.sourceforge.net/challenge_content/Floor32.html)

      // On entry: aNb = [ESP+8]

      // Double variable layout: S1E11M52

      // Get the upper 32 bits of the mantissa into EAX and the lower 32 bits
      // into EBP

      MOV EBP, [ESP+8]
      MOV EAX, [ESP+12]

      // Save the sign in EDX

      CDQ

      // Is the number zero?

      AND  EAX, $7FFFFFFF
      JNZ  @NotZero
      TEST EBP, EBP
      JZ   @Done

   @NotZero:
      // Get the exponent in ECX

      MOV ECX, EAX
      SHR ECX, 20

      // Get the number of positions to shift the mantissa

      NEG ECX
      ADD ECX, 1023+31
      CMP ECX, 31
      JA  @FractionOrTooLarge

      // Shift the mantissa all the way to the left

      SHLD EAX, EBP, 11
      SHL  EBP, 11

      // Add the implied 1 bit to the mantissa

      OR EAX, $80000000

      // Check the sign of the number

      TEST EDX, EDX
      JS   @NegativeNumber

      // Shift the bits for the positive number

      SHR EAX, CL

      // Sign bit may not be set

      TEST EAX, EAX
      JNS  @Done

   @RaiseInvalidOperationException:
      // Raise an invalid operation exception

      MOV AL, reInvalidOp
      JMP System.Error

   @NegativeNumber:
      TEST EBP, EBP
      JNZ  @NegativeWithFraction

      // Is there a fractional part?

      SHL EDX, CL
      NOT EDX
      AND EDX, EAX
      JNZ @NegativeWithFraction

      // Negative number, no fraction

      SHR EAX, CL
      NEG EAX
      JS  @Done
      JMP @RaiseInvalidOperationException

   @NegativeWithFraction:
      // Negative number with fractional part

      SHR EAX, CL
      NEG EAX
      SUB EAX, 1
      JS  @Done
      JMP @RaiseInvalidOperationException

   @FractionOrTooLarge:
      // Absolute value of number is either < 1 or it is too great to represent

      // Is it too large?

      TEST ECX, ECX
      JS   @RaiseInvalidOperationException

      // Number is -1 < x < 1: number is known not to be zero, so return the
      // sign

      MOV EAX, EDX
   @Done:
   End;
   Function MCCeil(Const aNb: Double): Integer;
   Asm
      // Note: from Ceil32_PLR_IA32_2
      //       (http://fastcode.sourceforge.net/challenge_content/Ceil32.html)

      // On entry: aNb = [ESP+8]

      // Double variable layout: S1E11M52

      // Get the upper 32 bits of the mantissa into EAX and the lower 32 bits
      // into EBP

      MOV EBP, [ESP+8]
      MOV EAX, [ESP+12]

      // Save the sign in EDX

      CDQ

      // Is the number zero?

      AND  EAX, $7FFFFFFF
      JNZ  @NotZero
      TEST EBP, EBP
      JZ   @Done

   @NotZero:
      // Get the exponent in ECX

      MOV ECX, EAX
      SHR ECX, 20

      // Get the number of positions to shift the mantissa

      NEG ECX
      ADD ECX, 1023+31
      CMP ECX, 31
      JA  @FractionOrTooLarge

      // Shift the mantissa all the way to the left

      SHLD EAX, EBP, 11
      SHL  EBP, 11

      // Add the implied 1 bit to the mantissa

      OR EAX, $80000000

      // Check the sign of the number

      TEST EDX, EDX
      JNS  @PositiveNumber

      // Shift the bits for the negative number

      SHR EAX, CL

      // Negate the number

      NEG EAX
      JS  @Done

   @RaiseInvalidOperationException:
      // Raise an invalid operation exception

      MOV AL, reInvalidOp
      JMP System.Error

   @PositiveNumber:
      TEST EBP, EBP
      JNZ  @PositiveWithFraction

      // Is there a fractional part?

      MOV EDX, -1
      SHL EDX, CL
      NOT EDX
      AND EDX, EAX
      JNZ @PositiveWithFraction

      // Positive number, no fraction

      SHR  EAX, CL
      TEST EAX, EAX
      JNS  @Done
      JMP  @RaiseInvalidOperationException

   @PositiveWithFraction:
      // Positive number with fractional part

      SHR EAX, CL
      ADD EAX, 1
      JNS @Done
      JMP @RaiseInvalidOperationException

   @FractionOrTooLarge:
      // Absolute value of number is either < 1 or it is too great to represent
   
      // Is it too large?

      TEST ECX, ECX
      JS   @RaiseInvalidOperationException

      // Number is -1 < x < 1: number is known not to be zero, so return 1 +
      // sign

      LEA EAX, [EDX+1]
   @Done:
   End;
Begin
   // Check that we know about the address to some key functions
   // Note: ideally, we would initialise those addresses as part of the
   //       initialisation of the unit, but at that stage we may or may not know
   //       about the Intel functions depending on the order of the unit usage,
   //       so rather than always ensuring that the order is right we may as
   //       well do it the way below... 

   If (CeilAddr = 0) Then Begin
{$IFDEF OPT_MATH}
      ExpAddr   := Cardinal(@OptExp);

      CeilAddr  := Cardinal(@OptCeil);
      FloorAddr := Cardinal(@OptFloor);

      PowerAddr := Cardinal(@OptPower);
{$ELSE}
      CeilAddr  := Cardinal(@MCCeil);
      FloorAddr := Cardinal(@MCFloor);

      PowerAddr := Cardinal(@MCPower);
{$ENDIF}
   End;

   // Constructor for executing a particular action

   Action := aAction;

   MC := TCellMLModelRuntimeString.Create;

   // Note #1: one would probably call some of the predefined functions, but the
   //          fact is that we want things to be as fast as possible, while a
   //          proper call is too time consuming, hence bypassing those calls as
   //          much as possible, even if it means that the generated code may be
   //          longer in some instances...
   // Note #2: we don't check for domains of definition. Indeed, any model ought
   //          to be valid in that respect, so no need to waste time checking
   //          for them...

   With MC Do Begin
      Case aAction Of
         siaFCOMP: Begin
            AddTwoBytes($D8, $D9);
            // FCOMP ST(1)

            AddTwoBytes($DD, $D8);
            // FSTP ST(0)
         End;
         siaFSTSWAXAndSAHF:
            AddUpdateFlags;
         siaTest: Begin
            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddUpdateFlags;
         End;
         siaPlus:
            AddTwoBytes($DE, $C1);
            // FADDP ST(1)
         siaMinus:
            AddTwoBytes($DE, $E9);
            // FSUBP ST(1)
         siaMinusUnary:
            AddTwoBytes($D9, $E0);
            // FCHS
         siaTimes:
            AddTwoBytes($DE, $C9);
            // FMULP ST(1)
         siaDivide:
            AddTwoBytes($DE, $F9);
            // FDIVP ST(1)
         siaPow:
            AddFunc(PowerAddr, True);
         siaSqr:
            AddTwoBytes($D8, $C8);
            // FMUL ST(0)
         siaSqrt:
            AddTwoBytes($D9, $FA);
            // FSQRT
         siaAbs:
            AddTwoBytes($D9, $E1);
            // FABS
         siaExp:
            AddExp;
         siaLN:
            AddLN;
         siaLog: Begin
            AddTwoBytes($D9, $EC);
            // FLDLG2

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D9, $F1);
            // FYL2X
         End;
         siaFloor:
            AddFunc(FloorAddr, False, True);
         siaCeil:
            AddFunc(CeilAddr, False, True);
         siaFact: Begin
            AddTwoBytes($DD, $15); AddAddr(Cardinal(@DblVal1));
            // FST DblVal1

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($EB, $18);
            // JMP $xx

            AddTwoBytes($DC, $0D); AddAddr(Cardinal(@DblVal1));
            // FMUL DblVal1

            AddTwoBytes($DD, $05); AddAddr(Cardinal(@DblVal1));
            // FLD DblVal1

            AddTwoBytes($DC, $25); AddAddr(GetAddrOfCstNb(1));
            // FSUB QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DD, $15); AddAddr(Cardinal(@DblVal1));
            // FST DblVal1

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(1));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddUpdateFlags;

            AddTwoBytes($77, $D9);
            // JNBE $xx
         End;
         siaAnd: Begin
            AddTwoBytes($66, $50);
            // PUSH AX

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DF, $E0);
            // FSTSW AX

            AddOneByte($9E);
            // SAHF

            AddTwoBytes($74, $0F);
            // JZ $0F

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DF, $E0);
            // FSTSW AX

            AddOneByte($9E);
            // SAHF

            AddTwoBytes($74, $06);
            // JZ $06

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($EB, $04);
            // JMP +$04

            AddTwoBytes($DD, $D8);
            // FSTP ST(0)

            AddTwoBytes($D9, $EE);
            // FLDZ

            AddTwoBytes($66, $58);
            // POP AX
         End;
         siaOr: Begin
            AddTwoBytes($66, $50);
            // PUSH AX

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DF, $E0);
            // FSTSW AX

            AddOneByte($9E);
            // SAHF

            AddTwoBytes($75, $0D);
            // JNZ $0D

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DF, $E0);
            // FSTSW AX

            AddOneByte($9E);
            // SAHF

            AddTwoBytes($74, $08);
            // JZ $08

            AddTwoBytes($EB, $02);
            // JMP +$02

            AddTwoBytes($DD, $D8);
            // FSTP ST(0)

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($EB, $02);
            // JMP +$02

            AddTwoBytes($D9, $EE);
            // FLDZ

            AddTwoBytes($66, $58);
            // POP AX
         End;
         siaXOr: Begin
            AddTwoBytes($66, $50);
            // PUSH AX

            AddTwoBytes($66, $52);
            // PUSH DX

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DF, $E0);
            // FSTSW AX

            AddOneByte($9E);
            // SAHF

            AddThreeBytes($0F, $95, $C2);
            // SETNZ DL

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DF, $E0);
            // FSTSW AX

            AddOneByte($9E);
            // SAHF

            AddThreeBytes($0F, $95, $C0);
            // SETNZ AL

            AddTwoBytes($32, $D0);
            // XOR DL, AL

            AddTwoBytes($74, $04);
            // JZ $04

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($EB, $02);
            // JMP +$02

            AddTwoBytes($D9, $EE);
            // FLDZ

            AddTwoBytes($66, $5A);
            // POP DX

            AddTwoBytes($66, $58);
            // POP AX
         End;
         siaNot: Begin
            AddTwoBytes($66, $50);
            // PUSH AX

            AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(0));
            // FCOMP QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DF, $E0);
            // FSTSW AX

            AddOneByte($9E);
            // SAHF

            AddTwoBytes($75, $04);
            // JNZ $04

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($EB, $02);
            // JMP +$02

            AddTwoBytes($D9, $EE);
            // FLDZ

            AddTwoBytes($66, $58);
            // POP AX
         End;
         siaSin:
            AddTwoBytes($D9, $FE);
            // FSIN
         siaCos:
            AddTwoBytes($D9, $FF);
            // FCOS
         siaTan: Begin
            AddTwoBytes($D9, $F2);
            // FPTAN

            AddTwoBytes($DD, $D8);
            // FSTP ST(0)
         End;
         siaSec: Begin
            AddTwoBytes($D9, $FF);
            // FCOS

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($DE, $F1);
            // FDIVRP ST(1)
         End;
         siaCsc: Begin
            AddTwoBytes($D9, $FE);
            // FSIN

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($DE, $F1);
            // FDIVRP ST(1)
         End;
         siaCot: Begin
            AddTwoBytes($D9, $F2);
            // FPTAN

            AddTwoBytes($DE, $F1);
            // FDIVRP ST(1)
         End;
         siaSinH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddExp;

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D9, $E0);
            // FCHS

            AddExp;

            AddTwoBytes($DE, $E9);
            // FSUBP ST(1)

            AddTwoBytes($DC, $0D); AddAddr(GetAddrOfCstNb(0.5));
            // FMUL QWORD PTR[$xxxxxxxx]
         End;
         siaCosH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddExp;

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D9, $E0);
            // FCHS

            AddExp;

            AddTwoBytes($DE, $C1);
            // FADDP ST(1)

            AddTwoBytes($DC, $0D); AddAddr(GetAddrOfCstNb(0.5));
            // FMUL QWORD PTR[$xxxxxxxx]
         End;
         siaTanH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddExp;

            AddTwoBytes($DD, $1D); AddAddr(Cardinal(@DblVal1));
            // FSTP DblVal1

            AddTwoBytes($D9, $E0);
            // FCHS

            AddExp;

            AddTwoBytes($DD, $15); AddAddr(Cardinal(@DblVal2));
            // FST DblVal2

            AddTwoBytes($DC, $2D); AddAddr(Cardinal(@DblVal1));
            // FSUBR DblVal1

            AddTwoBytes($DD, $05); AddAddr(Cardinal(@DblVal1));
            // FLD DblVal1

            AddTwoBytes($DC, $05); AddAddr(Cardinal(@DblVal2));
            // FADD DblVal2

            AddTwoBytes($DE, $F9);
            // FDIVP ST(1)
         End;
         siaSecH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddExp;

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D9, $E0);
            // FCHS

            AddExp;

            AddTwoBytes($DE, $C1);
            // FADDP ST(1)

            AddTwoBytes($DC, $3D); AddAddr(GetAddrOfCstNb(2));
            // FDIVR QWORD PTR[$xxxxxxxx]
         End;
         siaCscH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddExp;

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D9, $E0);
            // FCHS

            AddExp;

            AddTwoBytes($DE, $E9);
            // FSUBP ST(1)

            AddTwoBytes($DC, $3D); AddAddr(GetAddrOfCstNb(2));
            // FDIVR QWORD PTR[$xxxxxxxx]
         End;
         siaCotH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddExp;

            AddTwoBytes($DD, $1D); AddAddr(Cardinal(@DblVal1));
            // FSTP DblVal1

            AddTwoBytes($D9, $E0);
            // FCHS

            AddExp;

            AddTwoBytes($DD, $15); AddAddr(Cardinal(@DblVal2));
            // FST DblVal2

            AddTwoBytes($DC, $05); AddAddr(Cardinal(@DblVal1));
            // FADD DblVal2

            AddTwoBytes($DD, $05); AddAddr(Cardinal(@DblVal1));
            // FLD DblVal1

            AddTwoBytes($DC, $25); AddAddr(Cardinal(@DblVal2));
            // FSUB DblVal1

            AddTwoBytes($DE, $F9);
            // FDIVP ST(1)
         End;
         siaASin: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D8, $C8);
            // FMUL ST(0)

            AddTwoBytes($DE, $E9);
            // FSUBP

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($D9, $F3);
            // FPATAN
         End;
         siaACos: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D8, $C8);
            // FMUL ST(0)

            AddTwoBytes($DE, $E9);
            // FSUBP

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D9, $F3);
            // FPATAN
         End;
         siaATan: Begin
            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($D9, $F3);
            // FPATAN
         End;
         siaASec: Begin
            AddTwoBytes($DC, $3D); AddAddr(GetAddrOfCstNb(1));
            // FDIVR QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D8, $C8);
            // FMUL ST(0)

            AddTwoBytes($DE, $E9);
            // FSUBP

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D9, $F3);
            // FPATAN
         End;
         siaACsc: Begin
            AddTwoBytes($DC, $3D); AddAddr(GetAddrOfCstNb(1));
            // FDIVR QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($D8, $C8);
            // FMUL ST(0)

            AddTwoBytes($DE, $E9);
            // FSUBP

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($D9, $F3);
            // FPATAN
         End;
         siaACot: Begin
            AddTwoBytes($DC, $3D); AddAddr(GetAddrOfCstNb(1));
            // FDIVR QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $E8);
            // FLD1

            AddTwoBytes($D9, $F3);
            // FPATAN
         End;
         siaASinH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($D8, $C8);
            // FMUL ST(0)

            AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(1));
            // FADD QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($DE, $C1);
            // FADDP ST(1)

            AddLN;
        End;
         siaACosH: Begin
            AddTwoBytes($DD, $15); AddAddr(Cardinal(@DblVal1));
            // FST DblVal1

            AddTwoBytes($DC, $25); AddAddr(GetAddrOfCstNb(1));
            // FSUB QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DD, $05); AddAddr(Cardinal(@DblVal1));
            // FLD DblVal1

            AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(1));
            // FADD QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DE, $F9);
            // FDIVP ST(1)

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($DD, $05); AddAddr(Cardinal(@DblVal1));
            // FLD DblVal1

            AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(1));
            // FADD QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DE, $C9);
            // FMULP ST(1)

            AddTwoBytes($DD, $05); AddAddr(Cardinal(@DblVal1));
            // FLD DblVal1

            AddTwoBytes($DE, $C1);
            // FADDP ST(1)

            AddLN;
         End;
         siaATanH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(1));
            // FADD QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($DC, $2D); AddAddr(GetAddrOfCstNb(1));
            // FSUBR QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DE, $F9);
            // FDIVP ST(1)

            AddLN;

            AddTwoBytes($DC, $0D); AddAddr(GetAddrOfCstNb(0.5));
            // FMUL QWORD PTR[$xxxxxxxx]
         End;
         siaASecH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($D8, $C8);
            // FMUL ST(0)

            AddTwoBytes($DC, $2D); AddAddr(GetAddrOfCstNb(1));
            // FSUBR QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(1));
            // FADD QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DE, $F1);
            // FDIVRP ST(1)

            AddLN;
         End;
         siaACscH: Begin
            AddTwoBytes($DC, $3D); AddAddr(GetAddrOfCstNb(1));
            // FDIVR QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($D8, $C8);
            // FMUL ST(0)

            AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(1));
            // FADD QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $FA);
            // FSQRT

            AddTwoBytes($DE, $C1);
            // FADDP ST(1)

            AddLN;
         End;
         siaACotH: Begin
            AddTwoBytes($D9, $C0);
            // FLD ST(0)

            AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(1));
            // FADD QWORD PTR[$xxxxxxxx]

            AddTwoBytes($D9, $C9);
            // FXCH ST(1)

            AddTwoBytes($DC, $25); AddAddr(GetAddrOfCstNb(1));
            // FSUB QWORD PTR[$xxxxxxxx]

            AddTwoBytes($DE, $F9);
            // FDIVP ST(1)

            AddLN;

            AddTwoBytes($DC, $0D); AddAddr(GetAddrOfCstNb(0.5));
            // FMUL QWORD PTR[$xxxxxxxx]
         End;
      End;
   End;
End;

//==============================================================================

Constructor TCellMLModelRuntimeRPNStackItem.Create(Const aAction: TCellMLModelRuntimeRPNStackItemAction;
                                                   Const aItemType: TCellMLModelRuntimeRPNStackItemType;
                                                   Const aIndice: Integer);
Begin
   // Constructor for either loading or storing a particular item

   ItemType := aItemType;

   Action := aAction;
   Indice := aIndice;

   MC := TCellMLModelRuntimeString.Create;

   // The loading of a particular item cannot be converted into machine code
   // straight away, as that depends on the action that will follow (e.g. "A+B"
   // ought to be converted into "A +B" and not "A B +"). Only storing a
   // particular item can be done now, so...

   If (aAction = siaFSTP) Then
      With MC Do Begin
         Case aItemType Of
            sitdY:
               If (aIndice = 0) Then
                  AddTwoBytes($DD, $1A)
                  // FSTP QWORD PTR[EDX]
               Else If (aIndice < 16) Then
                  AddThreeBytes($DD, $5A, 8*aIndice)
                  // FSTP QWORD PTR[EDX+xx]
               Else Begin
                  AddTwoBytes($DD, $9A); AddAddr(8*aIndice);
                  // FSTP QWORD PTR[EDX+xxxxxxxx]
               End;
            sitCompVar:
               If (aIndice = 0) Then
                  AddTwoBytes($DD, $1B)
                  // FSTP QWORD PTR[EBX]
               Else If (aIndice < 16) Then
                  AddThreeBytes($DD, $5B, 8*aIndice)
                  // FSTP QWORD PTR[EBX+xx]
               Else Begin
                  AddTwoBytes($DD, $9B); AddAddr(8*aIndice);
                  // FSTP QWORD PTR[EBX+xxxxxxxx]
               End;
         End;
      End;
End;

//==============================================================================

Constructor TCellMLModelRuntimeRPNStackItem.Create(Const aTestJump: Byte;
                                                   Const aFullTest: Boolean);
Begin
   // Constructor for executing a particular action

   Action := siaJumpOrTestJump;

   MC := TCellMLModelRuntimeString.Create;

   If (aFullTest) Then
      With MC Do Begin
         AddTwoBytes($70+aTestJump, $04);
         // JB|JNB|JZ|JNZ|JBE|JNBE +$04

         AddTwoBytes($D9, $E8);
         // FLD1

         AddTwoBytes($EB, $02);
         // JMP +$02

         AddTwoBytes($D9, $EE);
         // FLDZ
      End
   Else If (aTestJump = JMP) Then
      // Just create the space required for a jump. Normally, a jump can be
      // machine coded in two different ways, depending on its length. If it is
      // less than 128 bytes long then we would have something like "$EB aAddr",
      // which requires 2 bytes, otherwise "$E9 aAddr", which requires 5 bytes.
      // Now, to distinguish between the two cases (and, in essence, try to
      // optimise things) is not worthwhile, as this will make the algorithm
      // much more complex for machine coding piecewise statements (in
      // particular the JZ statements), so we just use the latter...

      With MC Do Begin
         AddOneByte($E9); AddAddr($00000000);
         // JMP $xxxxxxxx
      End;
End;

//==============================================================================

Destructor TCellMLModelRuntimeRPNStackItem.Destroy;
Begin
   MC.Free;
End;

//==============================================================================

Procedure TCellMLModelRuntimeRPNStackItem.AddOperator(Const aOperand: TCellMLModelRuntimeRPNStackItemAction;
                                                      Const aDirect, aInverse: Boolean);
Var
   Offset: Cardinal;
Begin
   Action := aOperand;

   If (ItemType = sitTime) Then
      // Special case of the time...

      With MC Do Begin
         Case aOperand Of
            siaPlus:
               AddThreeBytes($DC, $45, $0C);
               // FADD QWORD PTR[EBP+$0C]
            siaMinus:
               If (aDirect) Then
                  AddThreeBytes($DC, $65, $0C)
                  // FSUB QWORD PTR[EBP+$0C]
               Else
                  AddThreeBytes($DC, $6D, $0C);
                  // FSUBR QWORD PTR[EBP+$0C]
            siaTimes:
               AddThreeBytes($DC, $4D, $0C);
               // FMUL QWORD PTR[EBP+$0C]
            siaDivide:
               If (aDirect) Then
                  AddThreeBytes($DC, $75, $0C)
                  // FDIV QWORD PTR[EBP+$0C]
               Else
                  AddThreeBytes($DC, $7D, $0C);
                  // FDIVR QWORD PTR[EBP+$0C]
         End;
      End
   Else If (ItemType = sitCstNb) Then
      // Special case of a constant number...

      With MC Do Begin
         Case aOperand Of
            siaPlus: Begin
               AddTwoBytes($DC, $05); AddAddr(GetAddrOfCstNb(CstNb));
               // FADD QWORD PTR[$xxxxxxxx]
            End;
            siaMinus:
               If (aDirect) Then Begin
                  AddTwoBytes($DC, $25); AddAddr(GetAddrOfCstNb(CstNb));
                  // FSUB QWORD PTR[$xxxxxxxx]
               End Else Begin
                  AddTwoBytes($DC, $2D); AddAddr(GetAddrOfCstNb(CstNb));
                  // FSUBR QWORD PTR[$xxxxxxxx]
               End;
            siaTimes: Begin
               AddTwoBytes($DC, $0D);
               // FMUL QWORD PTR[$xxxxxxxx]

               If (aInverse) Then
                  AddAddr(GetAddrOfCstNb(1/CstNb))
               Else
                  AddAddr(GetAddrOfCstNb(CstNb));
            End;
            siaDivide:
               If (aDirect) Then Begin
                  AddTwoBytes($DC, $35); AddAddr(GetAddrOfCstNb(CstNb));
                  // FDIV QWORD PTR[$xxxxxxxx]
               End Else Begin
                  AddTwoBytes($DC, $3D); AddAddr(GetAddrOfCstNb(CstNb));
                  // FDIVR QWORD PTR[$xxxxxxxx]
               End;
         End;
      End
   Else Begin
      // General case of either a "Y", "dY", "CstVar" or "CompVar"...

      Case ItemType Of
         sitdY:      Offset := $02;   // EDX
         sitCstVar:  Offset := $01;   // ECX
         sitCompVar: Offset := $03;   // EBX
      Else
         Offset := $00;   // sitY (EAX)
      End;

      With MC Do Begin
         Case aOperand Of
            siaPlus:
               If (Indice = 0) Then
                  AddTwoBytes($DC, $00+Offset)
                  // FADD QWORD PTR[XXX]
               Else If (Indice < 16) Then
                  AddThreeBytes($DC, $40+Offset, 8*Indice)
                  // FADD QWORD PTR[XXX+xx]
               Else Begin
                  AddTwoBytes($DC, $80+Offset); AddAddr(8*Indice);
                  // FADD QWORD PTR[XXX+xxxxxxxx]
               End;
            siaMinus:
               If (Indice = 0) Then Begin
                  If (aDirect) Then
                     AddTwoBytes($DC, $20+Offset)
                     // FSUB QWORD PTR[XXX]
                  Else
                     AddTwoBytes($DC, $28+Offset);
                     // FSUBR QWORD PTR[XXX]
               End Else If (Indice < 16) Then Begin
                  If (aDirect) Then
                     AddThreeBytes($DC, $60+Offset, 8*Indice)
                     // FSUB QWORD PTR[XXX+xx]
                  Else
                     AddThreeBytes($DC, $68+Offset, 8*Indice);
                     // FSUBR QWORD PTR[XXX+xx]
               End Else Begin
                  If (aDirect) Then Begin
                     AddTwoBytes($DC, $A0+Offset); AddAddr(8*Indice);
                     // FSUB QWORD PTR[XXX+xxxxxxxx]
                  End Else Begin
                     AddTwoBytes($DC, $A8+Offset); AddAddr(8*Indice);
                     // FSUBR QWORD PTR[XXX+xxxxxxxx]
                  End;
               End;
            siaTimes:
               If (Indice = 0) Then
                  AddTwoBytes($DC, $08+Offset)
                  // FMUL QWORD PTR[XXX]
               Else If (Indice < 16) Then
                  AddThreeBytes($DC, $48+Offset, 8*Indice)
                  // FMUL QWORD PTR[XXX+xx]
               Else Begin
                  AddTwoBytes($DC, $88+Offset); AddAddr(8*Indice);
                  // FMUL QWORD PTR[XXX+xxxxxxxx]
               End;
            siaDivide:
               If (Indice = 0) Then Begin
                  If (aDirect) Then
                     AddTwoBytes($DC, $30+Offset)
                     // FDIV QWORD PTR[XXX]
                  Else
                     AddTwoBytes($DC, $38+Offset);
                     // FDIVR QWORD PTR[XXX]
               End Else If (Indice < 16) Then Begin
                  If (aDirect) Then
                     AddThreeBytes($DC, $70+Offset, 8*Indice)
                     // FDIV QWORD PTR[XXX+xx]
                  Else
                     AddThreeBytes($DC, $78+Offset, 8*Indice);
                     // FDIVR QWORD PTR[XXX+xx]
               End Else Begin
                  If (aDirect) Then Begin
                     AddTwoBytes($DC, $B0+Offset); AddAddr(8*Indice);
                     // FDIV QWORD PTR[XXX+xxxxxxxx]
                  End Else Begin
                     AddTwoBytes($DC, $B8+Offset); AddAddr(8*Indice);
                     // FDIVR QWORD PTR[XXX+xxxxxxxx]
                  End;
               End;
         End;
      End;
   End;
End;

//==============================================================================

Procedure TCellMLModelRuntimeRPNStackItem.AddComparison;
Var
   Offset: Cardinal;
Begin
   Action := siaFCOMP;

   If (ItemType = sitTime) Then
      // Special case of the time...

      MC.AddThreeBytes($DC, $5D, $0C)
      // FCOMP QWORD PTR[EBP+$0C]
   Else If (ItemType = sitCstNb) Then
      // Special case of a constant number...

      With MC Do Begin
         AddTwoBytes($DC, $1D); AddAddr(GetAddrOfCstNb(CstNb));
         // FCOMP QWORD PTR[$xxxxxxxx]
      End
   Else Begin
      // General case of either a "Y", "dY", "CstVar" or "CompVar"...

      Case ItemType Of
         sitdY:      Offset := $1A;   // EDX
         sitCstVar:  Offset := $19;   // ECX
         sitCompVar: Offset := $1B;   // EBX
      Else
         Offset := $18;   // sitY (EAX)
      End;

      With MC Do
         If (Indice = 0) Then
            AddTwoBytes($DC, $00+Offset)
            // FCOMP QWORD PTR[XXX]
         Else If (Indice < 16) Then
            AddThreeBytes($DC, $40+Offset, 8*Indice)
            // FCOMP QWORD PTR[XXX+xx]
         Else Begin
            AddTwoBytes($DC, $80+Offset); AddAddr(8*Indice);
            // FCOMP QWORD PTR[XXX+xxxxxxxx]
         End;
   End;
End;

//==============================================================================

Procedure TCellMLModelRuntimeRPNStackItem.AddFunc(Var aFuncAddr: Cardinal;
                                                  Const aTwoParams: Boolean;
                                                  Const aReturnInt: Boolean);
Begin
   With MC Do Begin
      AddOneByte($50);
      // PUSH EAX

      AddOneByte($51);
      // PUSH ECX

      AddOneByte($52);
      // PUSH EDX

{$IFNDEF OPT_MATH}
      If (aTwoParams) Then
         AddTwoBytes($D9, $C9);
         // FXCH ST(1)
{$ENDIF}

      AddThreeBytes($83, $C4, $F8);
      // ADD ESP, -$08

      AddThreeBytes($DD, $1C, $24);
      // FSTP QWORD PTR[ESP]

      If (aTwoParams) Then Begin
         AddThreeBytes($83, $C4, $F8);
         // ADD ESP, -$08

         AddThreeBytes($DD, $1C, $24);
         // FSTP QWORD PTR[ESP]
      End;

      AddTwoBytes($FF, $15); AddAddr(Cardinal(@aFuncAddr));
      // CALL DWORD PTR[aFuncAddr]

{$IFDEF OPT_MATH}
      If (aTwoParams) Then
         AddThreeBytes($83, $C4, $10)
         // ADD ESP, $10
      Else
         AddThreeBytes($83, $C4, $08);
         // ADD ESP, $08
{$ENDIF}

      If (aReturnInt) Then Begin
{$IFDEF OPT_MATH}
         AddThreeBytes($89, $45, $EC);
         // MOV [EBP-$14], EAX

         AddThreeBytes($DB, $45, $EC);
         // FILD DWORD PTR[EBP-$14]
{$ELSE}
         AddThreeBytes($83, $C4, $FC);
         // ADD ESP, -$04

         AddThreeBytes($89, $04, $24);
         // MOV [ESP], EAX

         AddThreeBytes($DB, $04, $24);
         // FILD DWORD PTR[ESP]

         AddThreeBytes($83, $C4, $04);
         // ADD ESP, $04
{$ENDIF}
      End;

      AddOneByte($5A);
      // POP EDX

      AddOneByte($59);
      // POP ECX

      AddOneByte($58);
      // POP EAX
   End;
End;

//==============================================================================

Procedure TCellMLModelRuntimeRPNStackItem.AddExp;
Begin
{$IFDEF OPT_MATH}
   AddFunc(ExpAddr);
{$ELSE}
   With MC Do Begin
      AddTwoBytes($D9, $EA);
      // FLDL2E

      AddTwoBytes($DE, $C9);
      // FMULP ST(1)

      AddTwoBytes($D9, $C0);
      // FLD ST(0)

      AddTwoBytes($D9, $FC);
      // FRNDINT

      AddTwoBytes($DC, $E9);
      // FSUB ST(1)

      AddTwoBytes($D9, $C9);
      // FXCH ST(1)

      AddTwoBytes($D9, $F0);
      // F2XM1

      AddTwoBytes($D9, $E8);
      // FLD1

      AddTwoBytes($DE, $C1);
      // FADDP ST(1)

      AddTwoBytes($D9, $FD);
      // FSCALE

      AddTwoBytes($DD, $D9);
      // FSTP ST(1)
   End;
{$ENDIF}
End;

//==============================================================================

Procedure TCellMLModelRuntimeRPNStackItem.AddLN;
Begin
   With MC Do Begin
      AddTwoBytes($D9, $ED);
      // FLDLN2

      AddTwoBytes($D9, $C9);
      // FXCH ST(1)

      AddTwoBytes($D9, $F1);
      // FYL2X
   End;
End;

//==============================================================================

Procedure TCellMLModelRuntimeRPNStackItem.AddUpdateFlags;
Begin
   With MC Do Begin
      AddTwoBytes($66, $50);
      // PUSH AX

      AddTwoBytes($DF, $E0);
      // FSTSW AX

      AddOneByte($9E);
      // SAHF

      AddTwoBytes($66, $58);
      // POP AX
   End;
End;
//==============================================================================

Procedure TCellMLModelRuntimeRPNStackItem.GenerateMC;
Const
   DblPI: Double = PI;
Var
   Offset: Cardinal;
Begin
   With MC Do
      If ((Size = 0) And (Action = siaFLD)) Then Begin
         // No machine code has been generated for the loading of a particular
         // item, so...
         // Note: when no machine code has been generated, it can only be for
         //       the loading of a particular item. If it is for a different
         //       action, then there is something wrong with COR!

         If (ItemType = sitTime) Then
            AddThreeBytes($DD, $45, $0C)
            // FLD QWORD PTR[EBP+$0C]
         Else If (ItemType = sitCstNb) Then Begin
            // Load predefined constants if possible
            // Note #1: we don't do that all the time, since we may have cases
            //          such as "3+5", which can be converted into "3 +5"
            //          instead of "3 5 +"...
            // Note #2: there are other predefined constants, but they are
            //          rather untypical (e.g. Log2(e)), so we just skip them...

            If (CstNb = 0) Then
               AddTwoBytes($D9, $EE)
               // FLDZ
            Else If (CstNb = 1) Then
               AddTwoBytes($D9, $E8)
               // FLD1
            Else If (CstNb = DblPI) Then
               AddTwoBytes($D9, $EB)
               // FLDPI
            Else Begin
               // Not one of the predefined constants, so...

               AddTwoBytes($DD, $05); AddAddr(GetAddrOfCstNb(CstNb));
               // FLD QWORD PTR[$xxxxxxxx]
            End;
         End Else Begin
            Case ItemType Of
               sitdY:      Offset := $02;   // EDX
               sitCstVar:  Offset := $01;   // ECX
               sitCompVar: Offset := $03;   // EBX
            Else
               Offset := $00;   // sitY (EAX)
            End;

            If (Indice = 0) Then
               AddTwoBytes($DD, $00+Offset)
               // FLD QWORD PTR[XXX]
            Else If (Indice < 16) Then
               AddThreeBytes($DD, $40+Offset, 8*Indice)
               // FLD QWORD PTR[XXX+xx]
            Else Begin
               AddTwoBytes($DD, $80+Offset); AddAddr(8*Indice);
               // FLD QWORD PTR[XXX+xxxxxxxx]
            End;
         End;
      End;
End;

//==============================================================================

Constructor TCellMLModelRuntime.Create;
Begin
   Inc(CellMLModelRuntimeCounter);

{$IFDEF OPT_MATH}
   FName := 'COR - CellML Model #'+StringOfChar('0', OptFloor(OptLog10(MaxInt))-OptFloor(OptLog10(CellMLModelRuntimeCounter)))+IntToStr(CellMLModelRuntimeCounter);
{$ELSE}
   FName := 'COR - CellML Model #'+StringOfChar('0', Floor(Log10(MaxInt))-Floor(Log10(CellMLModelRuntimeCounter)))+IntToStr(CellMLModelRuntimeCounter);
{$ENDIF}

   FComponentsList := DArray.CreateWith(TCellMLModelRuntimeComponent.Compare);
   FVariablesList  := DArray.CreateWith(TCellMLModelRuntimeVariable.Compare);

   InitStateVarsStr := TCellMLModelRuntimeString.Create;
   InitCstsStr      := TCellMLModelRuntimeString.Create;
   ComputeOnceStr   := TCellMLModelRuntimeString.Create;
   ComputeStr       := TCellMLModelRuntimeString.Create;
End;

//==============================================================================

Destructor TCellMLModelRuntime.Destroy;
Begin
   If (DLLHandle <> 0) Then Begin
      FreeLibrary(DLLHandle);

      SysUtils.DeleteFile(TempDir+FName+'.dll');
   End;

   FreeAndClear(FComponentsList);
   FreeAndClear(FVariablesList);

   InitStateVarsStr.Free;
   InitCstsStr.Free;
   ComputeOnceStr.Free;
   ComputeStr.Free;
End;

//==============================================================================

Procedure TCellMLModelRuntime.IntInitStateVars(Const aData: PADouble);
Begin
   InitStateVarsDDecl(aData);
End;

//==============================================================================

Procedure TCellMLModelRuntime.IntInitCsts(Const aData: PADouble);
Begin
   InitCstsDDecl(aData);
End;

//==============================================================================

Procedure TCellMLModelRuntime.IntComputeOnce(Const aY, adY, aCsts, aCompVars: PADouble);
Begin
   ComputeOnceDDecl(aY, adY, aCsts, aCompVars);
End;

//==============================================================================

Procedure TCellMLModelRuntime.IntCompute(Const aTime: Double;
                                         Const aY, adY, aCsts, aCompVars: PADouble);
Begin
   ComputeDDecl(aTime, aY, adY, aCsts, aCompVars);
End;

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TCellMLModelRuntime.ExtInitStateVars(Const aData: PADouble);
Begin
   InitStateVarsCDecl(aData);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TCellMLModelRuntime.ExtInitCsts(Const aData: PADouble);
Begin
   InitCstsCDecl(aData);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TCellMLModelRuntime.ExtComputeOnce(Const aY, adY, aCsts, aCompVars: PADouble);
Begin
   ComputeOnceCDecl(aY, adY, aCsts, aCompVars);
End;
{$ENDIF}

//==============================================================================

{$IFDEF COR_SPECIFIC}
Procedure TCellMLModelRuntime.ExtCompute(Const aTime: Double;
                                         Const aY, adY, aCsts, aCompVars: PADouble);
Begin
   ComputeCDecl(aTime, aY, adY, aCsts, aCompVars);
End;
{$ENDIF}

//==============================================================================

Procedure TCellMLModelRuntime.Init(Const aY, aCsts: PADouble);
Begin
   InitStateVars(aY);
   InitCsts(aCsts);
End;

//==============================================================================
{$IFDEF COR_SPECIFIC}
Function TCellMLModelRuntime.GenerateExtProcs(Const aCellMLModel: TCellMLModel;
                                              Const aCompiler: TCellMLModelRuntimeCompiler;
                                              Const aCompilerLocation: String): Boolean;
Var
   TextFileID: TextFile;
   BatchFileName, Compiler: String;
   ExtraCompFlags: String;
Begin
   // Retrieve compiler specific information

   If (aCompiler = cIntelCPP) Then Begin
      BatchFileName := 'ICLVars.bat';

      Compiler := 'ICL';

      ExtraCompFlags := '/QaxKWNPB /Qipo-c';
   End Else Begin
      BatchFileName := 'Vcvars32.bat';

      Compiler := 'CL';
   End;

   // Generate the C MC file

   With TCellMLAPIToCMCFileEngine.Create(aCellMLModel, [TempDir+FName+'.c']) Do Begin
      Execute;

      Free;
   End;

   // Generate the batch file that will generate the MC DLL file

   AssignFile(TextFileID, TempDir+FName+'.bat');

   Rewrite(TextFileID);

   WriteLn(TextFileID, '@ECHO OFF');
   WriteLn(TextFileID);
   WriteLn(TextFileID, '@CALL "'+aCompilerLocation+'Bin\'+BatchFileName+'"');
   WriteLn(TextFileID);
   WriteLn(TextFileID, 'CD "'+TempDir+'"');
   WriteLn(TextFileID, ExtractFileDrive(TempDir));
   WriteLn(TextFileID);
   WriteLn(TextFileID, Compiler+' /w /c /Ox '+ExtraCompFlags+' "'+FName+'.c"');
   WriteLn(TextFileID, Compiler+' /LD /Fe"'+FName+'.dll" "'+FName+'.obj"');
   WriteLn(TextFileID);
   WriteLn(TextFileID, 'DEL *.exp *.lib *.obj');

   CloseFile(TextFileID);

   // Execute the batch file to generate the MC DLL file

   ExecProgAndWait(TempDir+FName+'.bat');

   // Import the MC DLL functions

   DLLHandle := LoadLibrary(PChar(TempDir+FName+'.dll'));

   If (DLLHandle <> 0) Then Begin
      InitStateVarsCDecl := GetProcAddress(DLLHandle, 'initStateVars');
      InitCstsCDecl      := GetProcAddress(DLLHandle, 'initCsts');

      ComputeOnceCDecl := GetProcAddress(DLLHandle, 'computeOnce');
      ComputeCDecl     := GetProcAddress(DLLHandle, 'compute');

      InitStateVars := ExtInitStateVars;
      InitCsts      := ExtInitCsts;

      ComputeOnce := ExtComputeOnce;
      Compute     := ExtCompute;

      Result := True;
   End Else
      Result := False;

   // Delete the C MC and batch files

   DeleteFile(PChar(TempDir+FName+'.c'));
   DeleteFile(PChar(TempDir+FName+'.bat'));
End;
{$ENDIF}
//==============================================================================

Constructor TCellMLAPIToMCEngine.Create(Const aCellMLModel: TCellMLModel;
                                        Const aCellMLModelRuntime: TCellMLModelRuntime;
                                        Const aCompiler: TCellMLModelRuntimeCompiler;
                                        Const aCompilerLocation: String;
                                        Const aBeginEventHandler: TNotifyCellMLAPIToMBeginEvent;
                                        Const aProgressEventHandler: TNotifyCellMLAPIToMProgressEvent;
                                        Const aEndEventHandler: TNotifyCellMLAPIToMEndEvent);
Begin
   CellMLModel := aCellMLModel;

   CellMLModelRuntime := aCellMLModelRuntime;

   RPNStack := DList.Create;

   DummyComp := TCellMLModelRuntimeComponent.Create;
   DummyVar  := TCellMLModelRuntimeVariable.Create;

   Compiler         := aCompiler;
   CompilerLocation := aCompilerLocation;

   FOnBegin    := aBeginEventHandler;
   FOnProgress := aProgressEventHandler;
   FOnEnd      := aEndEventHandler;
End;

//==============================================================================

Destructor TCellMLAPIToMCEngine.Destroy;
Begin
   FreeAndClear(RPNStack);

   DummyComp.Free;
   DummyVar.Free;
End;

//==============================================================================

Function TCellMLAPIToMCEngine.FindComponent(Const aComponent: String): TCellMLModelRuntimeComponent;
Var
   ComponentIter: DIterator;
Begin
   DummyComp.Name := aComponent;

   ComponentIter := BinarySearch(CellMLModelRuntime.ComponentsList, [DummyComp]);

   If (Not AtEnd(ComponentIter)) Then
      Result := TCellMLModelRuntimeComponent(DeCAL.GetObject(ComponentIter))
   Else
      Result := Nil;
End;

//==============================================================================

Function TCellMLAPIToMCEngine.FindVariable(Const aComponent, aVariable: String): TCellMLModelRuntimeVariable;
Var
   VariableIter: DIterator;
Begin
   DummyVar.Owner := FindComponent(aComponent);

   If (DummyVar.Owner <> Nil) Then Begin
      DummyVar.Name  := aVariable;

      VariableIter := BinarySearch(CellMLModelRuntime.VariablesList, [DummyVar]);

      If (Not AtEnd(VariableIter)) Then
         Result := TCellMLModelRuntimeVariable(DeCAL.GetObject(VariableIter))
      Else
         Result := Nil;
   End Else
      Result := Nil;
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.Execute;
   Procedure DoProgress(Const aOnProgress: TNotifyCellMLAPIToMProgressEvent;
                        Var aStepNb: Integer; Const aNbOfSteps: Integer); Inline;
   Begin
      Inc(aStepNb);

      If (Assigned(aOnProgress)) Then
         aOnProgress(aStepNb, aNbOfSteps);
   End;
   Function CreateVariable(Const aComponent, aVariable, aUnit: String;
                           Const aState: TCellMLModelVariableState): TCellMLModelRuntimeVariable;
      Function CreateComponent(Const aComponent: String): TCellMLModelRuntimeComponent;
      Var
         Iter, Iter2: Integer;
         CompRef: TCellMLComponentRef;
         CompOwner: TCellMLModelRuntimeComponent;
      Begin
         // Look for the soon to be created component in the containment group

         CompRef := Nil;

         With CellMLModel Do Begin
            For Iter := 0 To GroupList.Size-1 Do Begin
               With TCellMLGroup(GroupList.At(Iter).VObject) Do 
                  If (Containment) Then
                     For Iter2 := 0 To ComponentRefList.Size-1 Do Begin
                        CompRef := FindComponentRef(aComponent, TCellMLComponentRef(ComponentRefList.At(Iter2).VObject));

                        If (CompRef <> Nil) Then
                           Break;
                     End;

               If (CompRef <> Nil) Then
                  Break;
            End;
         End;

         // Is the soon to be created component 'owned' by another?

         If ((CompRef <> Nil) And (CompRef.OwnerComponentRef <> Nil)) Then Begin
            CompOwner := FindComponent(CompRef.OwnerComponentRef.Name);

            If (CompOwner = Nil) Then
               CompOwner := CreateComponent(CompRef.OwnerComponentRef.Name);
         End Else
            CompOwner := Nil;

         // Create the component

         Result := TCellMLModelRuntimeComponent.Create(CompOwner, aComponent);

         // Add the newly created component to the list

         CellMLModelRuntime.ComponentsList.Add([Result]);

         If ((CellMLModelRuntime.ComponentsList.Size >= 2) And
             (TCellMLModelRuntimeComponent.Compare(GetRef(AdvanceByF(CellMLModelRuntime.ComponentsList.Finish, -2))^, GetRef(AdvanceByF(CellMLModelRuntime.ComponentsList.Finish, -1))^) > 0)) Then
            // Need to sort, so...

            Sort(CellMLModelRuntime.ComponentsList);

         If (CompOwner <> Nil) Then
            // The newly created component is 'owned', so we need to update its
            // 'owner'...

            CompOwner.ComponentsList.Add([Result]);
            // Note: no need to sort the list, since we don't need to search
            //       components
      End;
   Var
      Comp: TCellMLModelRuntimeComponent;
   Begin
      Comp := FindComponent(aComponent);

      If (Comp = Nil) Then
         Comp := CreateComponent(aComponent);

      Result := TCellMLModelRuntimeVariable.Create(Comp, aVariable, aUnit, aState);

      Comp.VariablesList.Add([Result]);
      // Note: no need to sort the list, since we don't need to search variables
   End;
   Procedure GenerateInitProcInit(Var aInitStr: TCellMLModelRuntimeString;
                                  Const aIndice: Integer;
                                  Const aInitialValue: Double); Inline;
   Begin
      With aInitStr Do Begin
         AddTwoBytes($DD, $05); AddAddr(GetAddrOfCstNb(aInitialValue));
         // FLD QWORD PTR[$xxxxxxxx]

         If (aIndice = 0) Then
            AddTwoBytes($DD, $18)
            // FSTP QWORD PTR[EAX]
         Else If (aIndice < 16) Then
            AddThreeBytes($DD, $58, 8*aIndice)
            // FSTP QWORD PTR[EAX+xx]
         Else Begin
            AddTwoBytes($DD, $98); AddAddr(8*aIndice);
            // FSTP QWORD PTR[EAX+xxxxxxxx]
         End;
      End;
   End;
   Procedure EndInitProc(Var aInitStr: TCellMLModelRuntimeString); Inline;
   Begin
      With aInitStr Do Begin
         AddOneByte($C3);
         // RET

         Compact;
      End;
   End;
   Procedure BeginComputeProc(Var aComputeStr: TCellMLModelRuntimeString); Inline;
   Begin
      With aComputeStr Do Begin
         AddOneByte($55);
         // PUSH EBP

         AddTwoBytes($8B, $EC);
         // MOV EBP, ESP

         AddOneByte($53);
         // PUSH EBX

         AddThreeBytes($8B, $5D, $08);
         // MOV EBX, [EBP+$08]
      End;
   End;
   Procedure EndComputeProc(Var aComputeStr: TCellMLModelRuntimeString); Inline;
   Begin
      With aComputeStr Do Begin
         AddOneByte($5B);
         // POP EBX

         AddOneByte($5D);
         // POP EBP

         AddThreeBytes($C2, $0C, $00);
         // RET $000C

         Compact;
      End;
   End;
Var
   StepNb, NbOfSteps: Integer;
   Iter, Iter2: Integer;
   NewVar: TCellMLModelRuntimeVariable;
   TotalNbOfStateVars, TotalNbOfCsts, TotalNbOfCompVars: Integer;
   UseMCMethods: Boolean;
Begin
   StepNb    := 0;
   NbOfsteps := CellMLModel.GlobalVariableList.Size+CellMLModel.EquationList.Size;

   // Inform of the number of steps the process is going to take

   If (Assigned(FOnBegin)) Then
      FOnBegin(NbOfSteps);

   // Generate the code that initialises the state variables and constants...

   // Initialise the number of state variables, constants and computed variables

   TotalNbOfStateVars := 0;
   TotalNbOfCsts      := 0;
   TotalNbOfCompVars  := 0;

   // Beginning of the initialisation procedures

   For Iter := 0 To CellMLModel.GlobalVariableList.Size-1 Do Begin
      With TCellMLModelVariable(CellMLModel.GlobalVariableList.At(Iter).VObject) Do
         If ((State = vsState) Or (State = vsConstant) Or (State = vsComputed)) Then Begin
            NewVar := CreateVariable(Component, Variable, Units, State);

            CellMLModelRuntime.VariablesList.Add([NewVar]);

            Case State Of
               vsConstant: Begin
                  GenerateInitProcInit(CellMLModelRuntime.InitCstsStr, TotalNbOfCsts, StrToFloat(InitialValue));

                  NewVar.Index := TotalNbOfCsts;

                  Inc(TotalNbOfCsts);
               End;
               vsComputed: Begin
                  NewVar.Index := TotalNbOfCompVars;

                  Inc(TotalNbOfCompVars);
               End;
            Else   // vsState
               GenerateInitProcInit(CellMLModelRuntime.InitStateVarsStr, TotalNbOfStateVars, StrToFloat(InitialValue));

               NewVar.Index := TotalNbOfStateVars;

               Inc(TotalNbOfStateVars);
            End;
         End;

      // Inform of the progress of the process

      DoProgress(FOnProgress, StepNb, NbOfSteps);
   End;

   // Set the number of state variables, constants and computed variables
   // Note: we could have incremented the number of state variables, constants
   //       and computed variables as we were scanning through the list of
   //       variables, but that wouldn't be efficient when it comes to the
   //       number of computed variables, since that would mean increasing the
   //       size of the array that holds all the computed variables, which
   //       would be a waste of time, so...

   With CellMLModelRuntime Do Begin
      FNbOfStateVars := TotalNbOfStateVars;
      FNbOfCsts      := TotalNbOfCsts;
      FNbOfCompVars  := TotalNbOfCompVars;

      // End of the initialisation procedures

      EndInitProc(InitStateVarsStr);
      EndInitProc(InitCstsStr);

      // Generate the various equations that make up the CellML model

      // Trim and sort a few lists

      ComponentsList.TrimToSize;
      VariablesList.TrimToSize;

      Sort(VariablesList);
      // Note: unlike the components list, the variables list is not sorted
      //       whenever needed after adding a new variable to it, so we need to
      //       sort it now...

      For Iter := 0 To ComponentsList.Size-1 Do
         With TCellMLModelRuntimeComponent(ComponentsList.At(Iter).VObject) Do Begin
            ComponentsList.TrimToSize;
            VariablesList.TrimToSize;

            Sort(ComponentsList);
            Sort(VariablesList);
         End;

      // Convert the initialisation strings into procedures

      InitStateVarsDDecl := TCellMLModelRuntimeInitProcDDecl(PChar(InitStateVarsStr.Value));
      InitCstsDDecl      := TCellMLModelRuntimeInitProcDDecl(PChar(InitCstsStr.Value));

      // Beginning of the compute once and compute procedures

      BeginComputeProc(ComputeOnceStr);
      BeginComputeProc(ComputeStr);

      CellMLModel.GlobalVariableList.Comparator := CellMLModel.GlobalVariableList.Compare;

      Sort(CellMLModel.GlobalVariableList);
      // Note: this is very important, since we need to be able to search for
      //       variables using an alphabetical order, as opposed to a state
      //       one...

      For Iter := 0 To CellMLModel.EquationList.Size-1 Do Begin
         With TCellMLModelEquation(CellMLModel.EquationList.At(Iter).VObject) Do Begin
            // Convert the binary tree into a RPN stack

            MathMLEquationToRPNStack(Component, EqnBinTree);

            // Add the machine code associated to the various stack items to
            // the compute once or compute string

            For Iter2 := 0 To RPNStack.Size-1 Do
               With TCellMLModelRuntimeRPNStackItem(RPNStack.At(Iter2).VObject) Do Begin
                  GenerateMC;
                  // Note: required when the right hand side only consists of
                  //       one variable, constant, etc.

                  If (ComputeOnce) Then
                     CellMLModelRuntime.ComputeOnceStr.AddString(MC)
                  Else
                     CellMLModelRuntime.ComputeStr.AddString(MC);
               End;
         End;

         // Free the contents of the stack, since we don't need it anymore

         ObjFree(RPNStack);

         RPNStack.Clear;

         // Inform of the progress of the process

         DoProgress(FOnProgress, StepNb, NbOfSteps);
      End;

      // End of the compute once and compute procedures

      EndComputeProc(ComputeOnceStr);
      EndComputeProc(ComputeStr);

      // Convert the computation string into a procedure

      ComputeOnceDDecl := TCellMLModelRuntimeComputeOnceProcDDecl(PChar(ComputeOnceStr.Value));
      ComputeDDecl     := TCellMLModelRuntimeComputeProcDDecl(PChar(ComputeStr.Value));
   End;

   // Trim the constants array, since we are done with it...

   Constants.TrimToSize;

   // Generate the MC DLL, if required

{$IFDEF COR_SPECIFIC}
   If (Compiler = cInternal) Then
      UseMCMethods := True
   Else
      UseMCMethods := Not CellMLModelRuntime.GenerateExtProcs(CellMLModel, Compiler, CompilerLocation);
{$ELSE}
   UseMCMethods := True;
{$ENDIF}

   // Use the MC methods, if required

   If (UseMCMethods) Then
      With CellMLModelRuntime Do Begin
         InitStateVars := IntInitStateVars;
         InitCsts      := IntInitCsts;

         ComputeOnce := IntComputeOnce;
         Compute     := IntCompute;
      End;

   CellMLModelRuntime.FUseMCMethods := UseMCMethods;

   // Inform of the end of the process

   If (Assigned(FOnEnd)) Then
      FOnEnd;
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.CreateOperand(Const aCellMLComponent: String;
                                             Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                             Const aAction: TCellMLModelRuntimeRPNStackItemAction);
Var
   RPNStackItem1, RPNStackItem2: TCellMLModelRuntimeRPNStackItem;
Begin
   MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);

   If (aMathMLEquationBinTree.Right <> Nil) Then Begin
      // Not the unary operator, so...

      RPNStackItem1 := TCellMLModelRuntimeRPNStackItem(RPNStack.Back.VObject);

      MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

      RPNStackItem2 := TCellMLModelRuntimeRPNStackItem(RPNStack.Back.VObject);

      // Try to optimise the operator

      If (RPNStackItem2.Action = siaFLD) Then Begin
         If ((RPNStackItem1.Action = siaFLD) And
             (RPNStackItem1.ItemType = sitCstNb) And (RPNStackItem2.ItemType = sitCstNb)) Then Begin
            // Both operands are pure "FLD" actions on a constant number, so we
            // can replace them with "CstNb1+CstNb2", "CstNb1-CstNb2",
            // "CstNb1*CstNb2" or "CstNb1/CstNb2". This means removing the
            // second operand and replacing the first one with the equivalent
            // operation

            Case aAction Of
               siaPlus:
                  RPNStackItem1.CstNb := RPNStackItem1.CstNb+RPNStackItem2.CstNb;
               siaMinus:
                  RPNStackItem1.CstNb := RPNStackItem1.CstNb-RPNStackItem2.CstNb;
               siaTimes:
                  RPNStackItem1.CstNb := RPNStackItem1.CstNb*RPNStackItem2.CstNb;
               siaDivide:
                  RPNStackItem1.CstNb := RPNStackItem1.CstNb/RPNStackItem2.CstNb;
            End;

            RPNStack.Remove([RPNStackItem2]);

            RPNStackItem2.Free;
         End Else Begin
            // The second operand is a pure "FLD" action, so we can replace it
            // with a "FADD|FSUB|FMUL Addr" one or, in the case of "FDIV",
            // replace it with a "FMUL Addr(1/CstNb)" one, if possible. We also
            // replace "A+0", "A-0", "A*1", "A/1" with "A", and "A*0" with "0"

            If ((RPNStackItem2.ItemType = sitCstNb) And
                ((((aAction = siaPlus) Or (aAction = siaMinus)) And (RPNStackItem2.CstNb = 0)) Or
                 (((aAction = siaTimes) Or (aAction = siaDivide)) And (RPNStackItem2.CstNb = 1)))) Then Begin
               // The current operation is unnecessary, so just skip it by
               // removing the second operand

               RPNStack.Remove([RPNStackItem2]);

               RPNStackItem2.Free;
            End Else If ((RPNStackItem2.ItemType = sitCstNb) And
                     (aAction = siaTimes) And (RPNStackItem2.CstNb = 0)) Then Begin
               // The current operation is "A*0", so reduce it to "0"

               RPNStack.Remove([RPNStackItem2]);

               RPNStackItem2.Free;

               RPNStackItem1.ItemType := sitCstNb;
               RPNStackItem1.CstNb    := 0;
            End Else If ((aAction = siaDivide) And (RPNStackItem2.ItemType = sitCstNb)) Then
               // Do a bit of partial evaluation by replacing "A/CstNb" with
               // "A*(1/CstNb)"

               RPNStackItem2.AddOperator(siaTimes, True, True)
            Else
               RPNStackItem2.AddOperator(aAction);
         End;
      End Else If (RPNStackItem1.Action = siaFLD) Then Begin
         // The first operand is a pure "FLD" action, so we can replace it with
         // a "FADD|FSUBR|FMUL|FDIVR Addr" one. This means updating it, removing
         // it and putting it after the second operand...

         RPNStackItem1.AddOperator(aAction, False);

         RPNStack.Remove([RPNStackItem1]);
         RPNStack.PushBack([RPNStackItem1]);
      End Else
         // None of the operands is of pure "FLD" type, so...

         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(aAction)]);
   End Else
      RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaMinusUnary)]);
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.CreateOneArgFunction(Const aCellMLComponent: String;
                                                    Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                    Const aAction: TCellMLModelRuntimeRPNStackItemAction;
                                                    Const aLeftArg: Boolean);
Begin
   If (aLeftArg) Then
      MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left)
   Else
      MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

   RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(aAction)]);
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.CreateComparison(Const aCellMLComponent: String;
                                                Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                Const aAction: TCellMLModelRuntimeRPNStackItemAction);
Var
   RPNStackItem1, RPNStackItem2: TCellMLModelRuntimeRPNStackItem;
   Direct: Boolean;
   Test: Byte;
Begin
   MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);

   RPNStackItem1 := TCellMLModelRuntimeRPNStackItem(RPNStack.Back.VObject);

   MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

   RPNStackItem2 := TCellMLModelRuntimeRPNStackItem(RPNStack.Back.VObject);

   // Try to optimise the operator

   Direct := False;

   If (RPNStackItem2.Action = siaFLD) Then Begin
      // The second operand is a pure "FLD" action, so can replace it by a
      // "FCOMP Addr" one...

      Direct := True;

      RPNStackItem2.AddComparison;
   End Else If (RPNStackItem1.Action = siaFLD) Then Begin
      // The first operand is a pure "FLD" action, so can replace it by a
      // "FCOMP Addr" one. This means updating it, removing it and putting it
      // after the second operand...

      RPNStackItem1.AddComparison;

      RPNStack.Remove([RPNStackItem1]);
      RPNStack.PushBack([RPNStackItem1]);
   End Else
      // None of the operands is of pure "FLD" type, so...

      RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFCOMP)]);

   RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFSTSWAXAndSAHF)]);

   // Determine the type of test that should be carried out

   Case aAction Of
      siaNEq:
         Test := JZ;
      siaLT:
         If (Direct) Then
            Test := JNB
         Else
            Test := JB;
      siaGT:
         If (Direct) Then
            Test := JBE
         Else
            Test := JNBE;
      siaLEq:
         If (Direct) Then
            Test := JNBE
         Else
            Test := JBE;
      siaGEq:
         If (Direct) Then
            Test := JB
         Else
            Test := JNB;
   Else
      // siaEqEq

      Test := JNZ;
   End;

   // Do the test itself

   RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(Test, True)]);
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.CreateLogicalOperator(Const aCellMLComponent: String;
                                                     Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                                     Const aAction: TCellMLModelRuntimeRPNStackItemAction);
Begin
   MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);
   MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

   RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(aAction)]);
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.CreateJZ;
Var
   Addr: Cardinal;
   RPNStackItemIter: DIterator;
Begin
   Addr := 0;

   RPNStackItemIter := RPNStack.Finish;

   Retreat(RPNStackItemIter);

   While (Not DeCAL.Equals(RPNStackItemIter, RPNStackJZItemIter)) Do Begin
      With TCellMLModelRuntimeRPNStackItem(DeCAL.GetObject(RPNStackItemIter)) Do Begin
         GenerateMC;   // Just to be on the safe side, since we need it to be
                       // the size of the machine code...

         Inc(Addr, MC.Size);
      End;

      Retreat(RPNStackItemIter);
   End;

   With TCellMLModelRuntimeRPNStackItem(DeCAL.GetObject(RPNStackJZItemIter)).MC Do
      If (Addr < $80) Then
         AddTwoBytes($70+JZ, Addr)
         // JZ $xx
      Else Begin
         AddTwoBytes($0F, $80+JZ); AddAddr(Addr);
         // JZ $xxxxxxxx
      End;
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.CreateJMPs;
Var
   Addr: Cardinal;
   RPNStackJMPItemIter, RPNStackItemIter: DIterator;
   I: Integer;
Begin
   RPNStackItemIter := RPNStack.Finish;

   Retreat(RPNStackItemIter);
   Retreat(RPNStackItemIter);
   // Note: two times, because we want to skip the left hand side of the
   //       equation...

   Addr := 0;

   For I := High(RPNStackJMPItemIterList) DownTo 0 Do Begin
      // Note: we scan the JMPs backward, so as to increase the speed of the
      //       algorithm by not recomputing the full value of "Addr", but by
      //       starting from its previous value...

      RPNStackJMPItemIter := RPNStackJMPItemIterList[I];

      While (Not DeCAL.Equals(RPNStackItemIter, RPNStackJMPItemIter)) Do Begin
         With TCellMLModelRuntimeRPNStackItem(DeCAL.GetObject(RPNStackItemIter)) Do Begin
            GenerateMC;   // Just to be on the safe side, since we need it the
                          // size of the machine code...

            Inc(Addr, MC.Size);
         End;

         Retreat(RPNStackItemIter);
      End;

      With TCellMLModelRuntimeRPNStackItem(DeCAL.GetObject(RPNStackJMPItemIter)).MC Do Begin
         FSize := 1;

         AddAddr(Addr);
      End;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.ScanPiecewiseStatement(Const aMathMLEquationBinTree: TMathMLCommandBinTree);
   Procedure ScanWithinPiecewiseStatement(Const aMathMLEquationBinTree: TMathMLCommandBinTree);
   Begin
      Case aMathMLEquationBinTree.ItemType Of
         mitPiece:
            Inc(NbOfPieceParts);
         mitOtherwise:
            OtherwisePart := True;
      End;

      If (aMathMLEquationBinTree.Left <> Nil) Then
         ScanWithinPiecewiseStatement(aMathMLEquationBinTree.Left);

      If (aMathMLEquationBinTree.Right <> Nil) Then
         ScanWithinPiecewiseStatement(aMathMLEquationBinTree.Right);
   End;
Begin
   // Reset everything

   NbOfPieceParts := 0;
   OtherwisePart  := False;

   CrtPiecePart := 0;

   ScanWithinPiecewiseStatement(aMathMLEquationBinTree);

   If (NbOfPieceParts <> 0) Then Begin
      If (OtherwisePart) Then
         SetLength(RPNStackJMPItemIterList, NbOfPieceParts)
      Else
         SetLength(RPNStackJMPItemIterList, NbOfPieceParts-1);

      RPNStackJMPItemIterListIndex := 0;
   End;
End;

//==============================================================================

Procedure TCellMLAPIToMCEngine.MathMLEquationToRPNStack(Const aCellMLComponent: String;
                                                        Const aMathMLEquationBinTree: TMathMLCommandBinTree);
Var
   RPNStackJMPItemIter: DIterator;
   Val: Double;
Begin
   Case aMathMLEquationBinTree.ItemType Of
      mitCN:
         // Load the constant onto the stack

         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(StrToFloat(aMathMLEquationBinTree.Str))]);
      mitCI:
         // Load the variable onto the stack

         With CellMLModel.MapVariable(aCellMLComponent, aMathMLEquationBinTree.Str, aCellMLComponent, aMathMLEquationBinTree.Str) Do
            Case State Of
               vsFree:
                  // Time

                  RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create]);
               vsState:
                  // State variable

                  RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFLD, sitY, FindVariable(Component, Variable).Index)]);
               vsConstant:
                  // Constant

                  RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFLD, sitCstVar, FindVariable(Component, Variable).Index)]);
               vsComputed:
                  // Computed variable

                  RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFLD, sitCompVar, FindVariable(Component, Variable).Index)]);
            End;
      mitPiecewise: Begin
         MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);

         If (aMathMLEquationBinTree.Right <> Nil) Then
            MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);
      End;
      mitPiece: Begin
         Inc(CrtPiecePart);

         // Generate the test

         MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaTest)]);
         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(JZ, False)]);

         // Keep track of the conditional jump, since we will need to specify
         // the length of the jump later on, so...

         RPNStackJZItemIter := RPNStack.Finish;
         // Note: it's a zero-based index, so...

         Retreat(RPNStackJZItemIter);

         // Generate the right hand side of the equation associated to the test

         MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);

         // Add a jump code in case there is more than one piece part or an
         // otherwise part

         If ((CrtPiecePart <> NbOfPieceParts) Or OtherwisePart) Then Begin
            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(JMP, False)]);

            // Keep track of the jump's location, since we need to properly
            // create it once we know where the assignment finishes...

            RPNStackJMPItemIter := RPNStack.Finish;

            Retreat(RPNStackJMPItemIter);

            RPNStackJMPItemIterList[RPNStackJMPItemIterListIndex] := RPNStackJMPItemIter;

            Inc(RPNStackJMPItemIterListIndex);

            // We can now create the JZ, so...

            CreateJZ;
         End;
      End;
      mitOtherwise:
         MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);
      mitEq: Begin
         // Determine whether the right hand side is a piecewise statement and
         // if so, then determine the number of piece parts there are and
         // whether there is an otherwise part

         ScanPiecewiseStatement(aMathMLEquationBinTree.Right);

         // Stack up the right hand side of the assignment

         MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

         // The left hand side of the equation is either a derivative or a
         // computed variable, which is where the result of the right hand side
         // of the equation is to be stored

         If (aMathMLEquationBinTree.Left.ItemType = mitDiff) Then
            // Derivative

            With CellMLModel.MapVariable(aCellMLComponent, aMathMLEquationBinTree.Left.Right.Str, aCellMLComponent, aMathMLEquationBinTree.Left.Right.Str) Do
               RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFSTP, sitdY, FindVariable(Component, Variable).Index)])
         Else
            // Computed variable

            With CellMLModel.MapVariable(aCellMLComponent, aMathMLEquationBinTree.Left.Str, aCellMLComponent, aMathMLEquationBinTree.Left.Str) Do
               RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFSTP, sitCompVar, FindVariable(Component, Variable).Index)]);

         // Create the last JZ, if required and in case we are dealing with a
         // piecewise statement

         If ((NbOfPieceParts <> 0) And Not OtherwisePart) Then
            CreateJZ;

         // Update the jumps in case we are dealing with a piecewise statement

         If (NbOfPieceParts <> 0) Then
            CreateJMPs;
      End;
      mitEqEq:
         CreateComparison(aCellMLComponent, aMathMLEquationBinTree, siaEqEq);
      mitNEq:
         CreateComparison(aCellMLComponent, aMathMLEquationBinTree, siaNEq);
      mitLT:
         CreateComparison(aCellMLComponent, aMathMLEquationBinTree, siaLT);
      mitGT:
         CreateComparison(aCellMLComponent, aMathMLEquationBinTree, siaGT);
      mitLEq:
         CreateComparison(aCellMLComponent, aMathMLEquationBinTree, siaLEq);
      mitGEq:
         CreateComparison(aCellMLComponent, aMathMLEquationBinTree, siaGEq);
      mitPlus:
         CreateOperand(aCellMLComponent, aMathMLEquationBinTree, siaPlus);
      mitMinus:
         CreateOperand(aCellMLComponent, aMathMLEquationBinTree, siaMinus);
      mitTimes:
         CreateOperand(aCellMLComponent, aMathMLEquationBinTree, siaTimes);
      mitDivide:
         CreateOperand(aCellMLComponent, aMathMLEquationBinTree, siaDivide);
      mitPow: Begin
         If (Not TryStrToFloat(aMathMLEquationBinTree.Right.Str, Val)) Then
            Val := 0;   // Simply to avoid the "sqr" case...

         If (Val = 2) Then
            CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaSqr)
         Else If (Val = 0.5) Then
            CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaSqrt)
         Else Begin
            MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);
            MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaPow)]);
         End;
      End;
      mitRoot: Begin
         If (aMathMLEquationBinTree.Right = Nil) Then
            Val := 2
         Else If (Not TryStrToFloat(aMathMLEquationBinTree.Left.Left.Str, Val)) Then
            Val := 0;   // Simply to avoid the "sqrt" case...

         If (Val = 2) Then
            CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaSqrt, aMathMLEquationBinTree.Right = Nil)
         Else Begin
            MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(1)]);

            MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);

            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaDivide)]);

            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaPow)]);
         End;
      End;
      mitAbs:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaAbs);
      mitExp:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaExp);
      mitLN:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaLN);
      mitLog: Begin
         If (aMathMLEquationBinTree.Left.ItemType = mitLogBase) Then Begin
            MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Right);

            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaLog)]);

            MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left.Left);

            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaLog)]);

            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaDivide)]);
         End Else
            CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaLog);
      End;
      mitCeil:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaCeil);
      mitFloor:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaFloor);
      mitFact:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaFact);
      mitAnd:
         CreateLogicalOperator(aCellMLComponent, aMathMLEquationBinTree, siaAnd);
      mitOr:
         CreateLogicalOperator(aCellMLComponent, aMathMLEquationBinTree, siaOr);
      mitXOr:
         CreateLogicalOperator(aCellMLComponent, aMathMLEquationBinTree, siaXOr);
      mitNot: Begin
         MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);

         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaNot)]);
      End;
      mitDiff: Begin
         // Load the derivative

         With CellMLModel.MapVariable(aCellMLComponent, aMathMLEquationBinTree.Right.Str, aCellMLComponent, aMathMLEquationBinTree.Right.Str) Do
            RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(siaFLD, sitdY, FindVariable(Component, Variable).Index)]);
      End;
      mitDegree:
         // Just skip the current node and go straight to the relevant one...

         MathMLEquationToRPNStack(aCellMLComponent, aMathMLEquationBinTree.Left);
      mitSin:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaSin);
      mitCos:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaCos);
      mitTan:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaTan);
      mitSec:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaSec);
      mitCsc:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaCsc);
      mitCot:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaCot);
      mitSinH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaSinH);
      mitCosH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaCosH);
      mitTanH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaTanH);
      mitSecH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaSecH);
      mitCscH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaCscH);
      mitCotH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaCotH);
      mitASin:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaASin);
      mitACos:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaACos);
      mitATan:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaATan);
      mitASec:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaASec);
      mitACsc:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaACsc);
      mitACot:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaACot);
      mitASinH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaASinH);
      mitACosH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaACosH);
      mitATanH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaATanH);
      mitASecH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaASecH);
      mitACscH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaACscH);
      mitACotH:
         CreateOneArgFunction(aCellMLComponent, aMathMLEquationBinTree, siaACotH);
      mitTrue:
         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(1)]);
      mitFalse:
         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(0)]);
      mitPI:
         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(PI)]);
      mitExponentiale:
         RPNStack.PushBack([TCellMLModelRuntimeRPNStackItem.Create(Exp(1))]);
   End;
End;

//==============================================================================

Initialization

//==============================================================================

Compilers := DArray.Create;

With Compilers Do Begin
   Add(['Internal']);
   Add(['Intel C++']);
   Add(['Microsoft Visual C++']);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

Constants := DArray.CreateWith(MakeComparator(ConstantsCompare));

//==============================================================================

Finalization

//==============================================================================

// Note: we must NOT free the objects held by "Compilers", for they are strings!

Compilers.Free;

ObjDispose(Constants);

Constants.Free;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

