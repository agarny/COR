//==============================================================================
// ODE integrator class
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 22/05/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit ODEIntegrator;

//==============================================================================

Interface

//==============================================================================

Uses
   CellMLAPIToMCEngine, DeCAL, Cell;

//==============================================================================

{$M+}

//==============================================================================

Type
   TODECallBackProc = Procedure(Const aTime: Double; Const aY, adY: PADouble; Const aData: Pointer); CDecl;
   TODEIntegratorType = (itForwardEuler, it2ndOrderRungeKutta, it4thOrderRungeKutta, itCVODE);
   TODEIntegrator = Class
      Public
         // User's methods

         Procedure Execute(Const aCell: PCell; Var at: Double; Const atEnd: Double); Virtual; Abstract;
   End;

//==============================================================================

Var
   ODEIntegrators: DArray;
{$IFNDEF COR_SPECIFIC}
   ODEDLLHandle: HMODULE;
{$ENDIF}

//==============================================================================

Procedure ODECallBack(Const aTime: Double; Const aY, adY: PADouble; Const aData: Pointer); CDecl;

//==============================================================================

Implementation

//==============================================================================

{$IFNDEF COR_SPECIFIC}
Uses
   Windows, SysUtils, Forms, Common, ODEForwardEulerIntegrator,
   ODE2ndOrderRungeKuttaIntegrator, ODE4thOrderRungeKuttaIntegrator,
   ODECVODEIntegrator;
{$ENDIF}

//==============================================================================

{$IFNDEF COR_SPECIFIC}
Var
   ODEDLLFileName: String;
{$ENDIF}

//==============================================================================

Procedure ODECallBack(Const aTime: Double; Const aY, adY: PADouble;
                      Const aData: Pointer);
Begin
   With TCell(aData^) Do
      Runtime.Compute(aTime, aY, adY, Csts, CompVars);
End;

//==============================================================================

Initialization

//==============================================================================

{$IFNDEF COR_SPECIFIC}
If (CPUVendor = cvIntel) Then Begin
   // ODE DLL

   ODEDLLFileName := TempDir+'IntelODE'+IntToStr(Integer(Application.Handle))+'.dll';

   // Extract the ODE DLL

   ExtractResource('INTELODEDLL', ODEDLLFileName);
End Else Begin
   // ODE DLL

   ODEDLLFileName := TempDir+'MicrosoftODE'+IntToStr(Integer(Application.Handle))+'.dll';

   // Extract the ODE DLL

   ExtractResource('MICROSOFTODEDLL', ODEDLLFileName);
End;

// Load the ODE DLL and its different sets of functions

ODEDLLHandle := LoadLibrary(PChar(ODEDLLFileName));

If (ODEDLLHandle = 0) Then
   Raise Exception.Create('Cannot load the ODE library ('+ODEDLLFileName+').');
{$ENDIF}

// Create the list of ODE integrators
// Note: the order in which we list the various integrators must be the same as
//       for their corresponding constant declaration. This is very important!
//       (see above)

ODEIntegrators := DArray.Create;

With ODEIntegrators Do Begin
{$IFNDEF COR_SPECIFIC}
   LoadODEForwardEulerFunction;
{$ENDIF}
   Add(['Forward Euler']);

{$IFNDEF COR_SPECIFIC}
   LoadODE2ndOrderRungeKuttaFunction;
{$ENDIF}
   Add(['2nd order Runge-Kutta']);

{$IFNDEF COR_SPECIFIC}
   LoadODE4thOrderRungeKuttaFunction;
{$ENDIF}
   Add(['4th order Runge-Kutta']);

{$IFNDEF COR_SPECIFIC}
   LoadODECVODEFunctions;
{$ENDIF}
   Add(['CVODE']);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

//==============================================================================

Finalization

//==============================================================================

// Note: we must NOT free the objects held by "ODEIntegrators", for they are
//       strings!

ODEIntegrators.Free;

{$IFNDEF COR_SPECIFIC}
FreeLibrary(ODEDLLHandle);

SysUtils.DeleteFile(ODEDLLFileName);
{$ENDIF}

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

