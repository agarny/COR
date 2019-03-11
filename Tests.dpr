//==============================================================================
// Tests
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://COR.physiol.ox.ac.uk/
//
// © Copyright 2002-2009
//------------------------------------------------------------------------------
// Date of Creation: 20/06/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
//==============================================================================

Program Tests;

//==============================================================================

{$IFDEF CONSOLE_TESTRUNNER}
   {$APPTYPE CONSOLE}
{$ENDIF}

//==============================================================================

{$R 'Res\Resources.res' 'Res\Resources.rc'}

//==============================================================================

Uses
   Forms, SysUtils, TestFramework,

{$IFDEF CONSOLE_TESTRUNNER}
   TextTestRunner,
{$ELSE}
   GUITestRunner,
{$ENDIF}

   CellMLAPITests in 'Src\CellML\Tests\CellMLAPITests.pas';

//==============================================================================

{$R *.RES}

//==============================================================================

Function CORTests: ITestSuite;
Begin
   Result := TTestSuite.Create('COR Tests');

   Result.AddTest(CellMLAPITests.Suite);
End;

//==============================================================================

Begin
   // Set the decimal and thousand separators and prevent Windows from resetting
   // them
   // Note: this is very important, since some countries use different
   //       conventions, so...

   DecimalSeparator  := '.';
   ThousandSeparator := ',';

   Application.UpdateFormatSettings := False;

   // Initialise and start the application...

   Application.Initialize;

{$IFDEF CONSOLE_TESTRUNNER}
   TextTestRunner.RunTest(CORTests, rxbHaltOnFailures);
{$ELSE}
   GUITestRunner.RunTest(CORTests);
{$ENDIF}
End.

//==============================================================================
// End of file
//==============================================================================

