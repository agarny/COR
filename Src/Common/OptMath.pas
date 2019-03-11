//==============================================================================
// Optimised mathematical functions
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 26/04/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit OptMath;

//==============================================================================

Interface

//==============================================================================

Const
   OptMaxDouble = 1.7e+308;

//==============================================================================

Var
   OptSin: Function(Const aNb: Double): Double; CDecl;
   OptCos: Function(Const aNb: Double): Double; CDecl;
   OptTan: Function(Const aNb: Double): Double; CDecl;
   OptSec: Function(Const aNb: Double): Double; CDecl;
   OptCsc: Function(Const aNb: Double): Double; CDecl;
   OptCot: Function(Const aNb: Double): Double; CDecl;
   OptSinH: Function(Const aNb: Double): Double; CDecl;
   OptCosH: Function(Const aNb: Double): Double; CDecl;
   OptTanH: Function(Const aNb: Double): Double; CDecl;
   OptSecH: Function(Const aNb: Double): Double; CDecl;
   OptCscH: Function(Const aNb: Double): Double; CDecl;
   OptCotH: Function(Const aNb: Double): Double; CDecl;
   OptArcSin: Function(Const aNb: Double): Double; CDecl;
   OptArcCos: Function(Const aNb: Double): Double; CDecl;
   OptArcTan: Function(Const aNb: Double): Double; CDecl;
   OptArcSec: Function(Const aNb: Double): Double; CDecl;
   OptArcCsc: Function(Const aNb: Double): Double; CDecl;
   OptArcCot: Function(Const aNb: Double): Double; CDecl;
   OptArcSinH: Function(Const aNb: Double): Double; CDecl;
   OptArcCosH: Function(Const aNb: Double): Double; CDecl;
   OptArcTanH: Function(Const aNb: Double): Double; CDecl;
   OptArcSecH: Function(Const aNb: Double): Double; CDecl;
   OptArcCscH: Function(Const aNb: Double): Double; CDecl;
   OptArcCotH: Function(Const aNb: Double): Double; CDecl;

   OptCeil:   Function(Const aNb: Double): Integer; CDecl;
   OptFloor:  Function(Const aNb: Double): Integer; CDecl;

   OptExp:    Function(Const aNb: Double): Double; CDecl;
   OptLN:     Function(Const aNb: Double): Double; CDecl;
   OptLog10:  Function(Const aNb: Double): Double; CDecl;

   OptMaxI:   Function(Const aNb1, aNb2: Integer): Integer; CDecl;
   OptMaxD:   Function(Const aNb1, aNb2: Double): Double; CDecl;
   OptMinI:   Function(Const aNb1, aNb2: Integer): Integer; CDecl;
   OptMinD:   Function(Const aNb1, aNb2: Double): Double; CDecl;

   OptPower:  Function(Const aNb, aExponent: Double): Double; CDecl;

   OptFact:   Function(Const aNb: Double): Double; CDecl;

//==============================================================================

Procedure LoadOptMathFunctions;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF COR_SPECIFIC}
   CORCommon,
{$ELSE}
   Forms, Common, SysUtils,
{$ENDIF}
   Windows;

//==============================================================================

{$IFNDEF COR_SPECIFIC}
Var
   OptMathDLLFileName: String;
   OptMathDLLHandle: HMODULE;
{$ENDIF}

//==============================================================================

Procedure LoadOptMathFunctions;
Begin
   OptSin := GetProcAddress(OptMathDLLHandle, 'OptSin');
   OptCos := GetProcAddress(OptMathDLLHandle, 'OptCos');
   OptTan := GetProcAddress(OptMathDLLHandle, 'OptTan');
   OptSec := GetProcAddress(OptMathDLLHandle, 'OptSec');
   OptCsc := GetProcAddress(OptMathDLLHandle, 'OptCsc');
   OptCot := GetProcAddress(OptMathDLLHandle, 'OptCot');
   OptSinH := GetProcAddress(OptMathDLLHandle, 'OptSinH');
   OptCosH := GetProcAddress(OptMathDLLHandle, 'OptCosH');
   OptTanH := GetProcAddress(OptMathDLLHandle, 'OptTanH');
   OptSecH := GetProcAddress(OptMathDLLHandle, 'OptSecH');
   OptCscH := GetProcAddress(OptMathDLLHandle, 'OptCscH');
   OptCotH := GetProcAddress(OptMathDLLHandle, 'OptCotH');
   OptArcSin := GetProcAddress(OptMathDLLHandle, 'OptArcSin');
   OptArcCos := GetProcAddress(OptMathDLLHandle, 'OptArcCos');
   OptArcTan := GetProcAddress(OptMathDLLHandle, 'OptArcTan');
   OptArcSec := GetProcAddress(OptMathDLLHandle, 'OptArcSec');
   OptArcCsc := GetProcAddress(OptMathDLLHandle, 'OptArcCsc');
   OptArcCot := GetProcAddress(OptMathDLLHandle, 'OptArcCot');
   OptArcSinH := GetProcAddress(OptMathDLLHandle, 'OptArcSinH');
   OptArcCosH := GetProcAddress(OptMathDLLHandle, 'OptArcCosH');
   OptArcTanH := GetProcAddress(OptMathDLLHandle, 'OptArcTanH');
   OptArcSecH := GetProcAddress(OptMathDLLHandle, 'OptArcSecH');
   OptArcCscH := GetProcAddress(OptMathDLLHandle, 'OptArcCscH');
   OptArcCotH := GetProcAddress(OptMathDLLHandle, 'OptArcCotH');

   OptCeil   := GetProcAddress(OptMathDLLHandle, 'OptCeil');
   OptFloor  := GetProcAddress(OptMathDLLHandle, 'OptFloor');

   OptExp    := GetProcAddress(OptMathDLLHandle, 'OptExp');
   OptLN     := GetProcAddress(OptMathDLLHandle, 'OptLN');
   OptLog10  := GetProcAddress(OptMathDLLHandle, 'OptLog10');

   OptMaxI   := GetProcAddress(OptMathDLLHandle, 'OptMaxI');
   OptMaxD   := GetProcAddress(OptMathDLLHandle, 'OptMaxD');
   OptMinI   := GetProcAddress(OptMathDLLHandle, 'OptMinI');
   OptMinD   := GetProcAddress(OptMathDLLHandle, 'OptMinD');

   OptPower  := GetProcAddress(OptMathDLLHandle, 'OptPower');

   OptFact   := GetProcAddress(OptMathDLLHandle, 'OptFact');
End;

//==============================================================================

Initialization

//==============================================================================

{$IFNDEF COR_SPECIFIC}
If (CPUVendor = cvIntel) Then Begin
   // Optimised Math DLL

   OptMathDLLFileName := TempDir+'IntelMath'+IntToStr(Integer(Application.Handle))+'.dll';

   // Extract the Optimised Math DLL

   ExtractResource('INTELMATHDLL', OptMathDLLFileName);
End Else Begin
   // Optimised Math DLL

   OptMathDLLFileName := TempDir+'MicrosoftMath'+IntToStr(Integer(Application.Handle))+'.dll';

   // Extract the Optimised Math DLL

   ExtractResource('MICROSOFTMATHDLL', OptMathDLLFileName);
End;

// Load the Optimised Math DLL and its different functions

OptMathDLLHandle := LoadLibrary(PChar(OptMathDLLFileName));

LoadOptMathFunctions;
{$ENDIF}

//==============================================================================

Finalization

//==============================================================================

{$IFNDEF COR_SPECIFIC}
FreeLibrary(OptMathDLLHandle);

SysUtils.DeleteFile(OptMathDLLFileName);
{$ENDIF}

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

