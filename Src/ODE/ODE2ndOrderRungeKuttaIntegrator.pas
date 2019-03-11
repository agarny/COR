//==============================================================================
// ODE 2nd-order Runge-Kutta integrator class
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 23/05/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit ODE2ndOrderRungeKuttaIntegrator;

//==============================================================================

Interface

//==============================================================================

Uses
   ODEFixedTimeStepIntegrator, CellMLAPIToMCEngine, Cell;

//==============================================================================

Type
   TODE2ndOrderRungeKuttaIntegrator = Class(TODEFixedTimeStepIntegrator)
      Private
         // Properties used for internal purposes

         Yk2: PADouble;

      Public
         // Constructor & Destructor

         Constructor Create(Const aNbOfStateVars: Integer; Const ah: Double = 1E-5 {s});
         Destructor Destroy; Override;

         // User's methods

         Procedure Execute(Const aCell: PCell; Var at: Double; Const atEnd: Double); Override;
   End;

//==============================================================================

Procedure LoadODE2ndOrderRungeKuttaFunction;

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
   ODE2ndOrderRungeKutta: Procedure(Const aODECallBack: TODECallBackProc; Const anY: Integer; Const aY, adY: PADouble; Const at: PDouble; Const atEnd, ah: Double; Const aData: Pointer; Const aYk2: PADouble); CDecl;

//==============================================================================

Procedure LoadODE2ndOrderRungeKuttaFunction;
Begin
   ODE2ndOrderRungeKutta := GetProcAddress(ODEDLLHandle, 'ODE2ndOrderRungeKutta');
End;

//==============================================================================

Constructor TODE2ndOrderRungeKuttaIntegrator.Create(Const aNbOfStateVars: Integer;
                                                    Const ah: Double);
Begin
   Inherited Create(ah);

   Yk2 := AllocMem(aNbOfStateVars*SizeOf(Double));
End;

//==============================================================================

Destructor TODE2ndOrderRungeKuttaIntegrator.Destroy;
Begin
   FreeMem(Yk2);

   Inherited;
End;

//==============================================================================

Procedure TODE2ndOrderRungeKuttaIntegrator.Execute(Const aCell: PCell;
                                                   Var at: Double;
                                                   Const atEnd: Double);
Begin
   // Compute the model itself

   ODE2ndOrderRungeKutta(ODECallBack, aCell^.Runtime.NbOfStateVars, aCell^.Y, aCell^.dY, @at, atEnd, Fh, aCell, Yk2);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

