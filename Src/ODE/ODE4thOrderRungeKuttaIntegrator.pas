//==============================================================================
// ODE 4th-order Runge-Kutta integrator class
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

Unit ODE4thOrderRungeKuttaIntegrator;

//==============================================================================

Interface

//==============================================================================

Uses
   ODEFixedTimeStepIntegrator, CellMLAPIToMCEngine, Cell;

//==============================================================================

Type
   TODE4thOrderRungeKuttaIntegrator = Class(TODEFixedTimeStepIntegrator)
      Private
         // Properties used for internal purposes

         k1, k2And3, Yk2Or3Or4: PADouble;
         
      Public
         // Constructor & Destructor

         Constructor Create(Const aNbOfStateVars: Integer; Const ah: Double = 1E-5 {s});
         Destructor Destroy; Override;

         // User's methods

         Procedure Execute(Const aCell: PCell; Var at: Double; Const atEnd: Double); Override;
   End;

//==============================================================================

Procedure LoadODE4thOrderRungeKuttaFunction;

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
   ODE4thOrderRungeKutta: Procedure(Const aODECallBack: TODECallBackProc; Const anY: Integer; Const aY, adY: PADouble; Const at: PDouble; Const atEnd, ah: Double; Const aData: Pointer; Const ak1, ak2And3, aYk2Or3Or4: PADouble); CDecl;

//==============================================================================

Procedure LoadODE4thOrderRungeKuttaFunction;
Begin
   ODE4thOrderRungeKutta := GetProcAddress(ODEDLLHandle, 'ODE4thOrderRungeKutta');
End;

//==============================================================================

Constructor TODE4thOrderRungeKuttaIntegrator.Create(Const aNbOfStateVars: Integer;
                                                    Const ah: Double);
Begin
   Inherited Create(ah);

   k1     := AllocMem(aNbOfStateVars*SizeOf(Double));
   k2And3 := AllocMem(aNbOfStateVars*SizeOf(Double));

   Yk2Or3Or4 := AllocMem(aNbOfStateVars*SizeOf(Double));
End;

//==============================================================================

Destructor TODE4thOrderRungeKuttaIntegrator.Destroy;
Begin
   FreeMem(k1);
   FreeMem(k2And3);

   FreeMem(Yk2Or3Or4);

   Inherited;
End;

//==============================================================================

Procedure TODE4thOrderRungeKuttaIntegrator.Execute(Const aCell: PCell;
                                                   Var at: Double;
                                                   Const atEnd: Double);
Begin
   // Compute the model itself

   ODE4thOrderRungeKutta(ODECallBack, aCell^.Runtime.NbOfStateVars, aCell^.Y, aCell^.dY, @at, atEnd, Fh, aCell, k1, k2And3, Yk2Or3Or4);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

