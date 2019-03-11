//==============================================================================
// ODE forward Euler integrator class
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

Unit ODEForwardEulerIntegrator;

//==============================================================================

Interface

//==============================================================================

Uses
   ODEFixedTimeStepIntegrator, CellMLAPIToMCEngine, Cell;

//==============================================================================

Type
   TODEForwardEulerIntegrator = Class(TODEFixedTimeStepIntegrator)
      Public
         // Constructor & Destructor

         Constructor Create(Const ah: Double = 1E-5 {s});

         // User's methods

         Procedure Execute(Const aCell: PCell; Var at: Double; Const atEnd: Double); Override;
   End;

//==============================================================================

Procedure LoadODEForwardEulerFunction;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF COR_SPECIFIC}
   CORCommon,
{$ENDIF}
   ODEIntegrator, Windows;

//==============================================================================

Var
   ODEForwardEuler: Procedure(Const aODECallBack: TODECallBackProc; Const anY: Integer; Const aY, adY: PADouble; Const at: PDouble; Const atEnd, ah: Double; Const aData: Pointer); CDecl;

//==============================================================================

Procedure LoadODEForwardEulerFunction;
Begin
   ODEForwardEuler := GetProcAddress(ODEDLLHandle, 'ODEForwardEuler');
End;

//==============================================================================

Constructor TODEForwardEulerIntegrator.Create(Const ah: Double);
Begin
   Inherited Create(ah);
End;

//==============================================================================

Procedure TODEForwardEulerIntegrator.Execute(Const aCell: PCell; Var at: Double;
                                             Const atEnd: Double);
Begin
   // Compute the model itself

   ODEForwardEuler(ODECallBack, aCell^.Runtime.NbOfStateVars, aCell^.Y, aCell^.dY, @at, atEnd, Fh, aCell);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

