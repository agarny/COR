//==============================================================================
// Cell class
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 17/03/2006
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Cell;

//==============================================================================

Interface

//==============================================================================

Uses
   SysUtils, CellMLAPIToMCEngine;

//==============================================================================

{$M+}

//==============================================================================

Type
   CellException = Class(Exception);
   TCell = Class
      Protected
         // Private representation of published properties

         FRuntime: TCellMLModelRuntime;

         FIntegrator: TObject;

      Public
         Y: PADouble;
         dY: PADouble;
         Csts: PADouble;
         CompVars: PADouble;

         // Constructor & Destructor

         Constructor Create(Const aRuntime: TCellMLModelRuntime);
         Destructor Destroy; Override;

         // User's methods

         Procedure Init; Inline;
         Procedure InitStateVars; Inline;
         Procedure InitCsts; Inline;
         Procedure InitCompVars; Inline;

         Procedure ComputeOnce; Inline;
         Procedure Compute(Var at: Double; Const atEnd: Double); Inline;

      Published
         // Published properties

         Property Runtime: TCellMLModelRuntime Read FRuntime;

         Property Integrator: TObject Read FIntegrator Write FIntegrator;
   End;
   PCell = ^TCell;

//==============================================================================

Implementation

//==============================================================================

Uses
   ODEIntegrator;

//==============================================================================

Constructor TCell.Create(Const aRuntime: TCellMLModelRuntime);
Begin
   FRuntime := aRuntime;

   GetMem(Y, FRunTime.NbOfStateVars*SizeOf(Double));
   GetMem(dY, FRunTime.NbOfStateVars*SizeOf(Double));
   GetMem(Csts, FRunTime.NbOfCsts*SizeOf(Double));
   GetMem(CompVars, FRunTime.NbOfCompVars*SizeOf(Double));
End;

//==============================================================================

Destructor TCell.Destroy;
Begin
   FreeMem(Y);
   FreeMem(dY);
   FreeMem(Csts);
   FreeMem(CompVars);
End;

//==============================================================================

Procedure TCell.Init;
Begin
   FRuntime.Init(Y, Csts);
End;

//==============================================================================

Procedure TCell.InitStateVars;
Begin
   FRuntime.InitStateVars(Y);
End;

//==============================================================================

Procedure TCell.InitCsts;
Begin
   FRuntime.InitCsts(Csts);
End;

//==============================================================================

Procedure TCell.InitCompVars;
Var
   I: Integer;
Begin
   For I := 0 To Runtime.NbOfCompVars-1 Do
      CompVars[I] := 0;
End;

//==============================================================================

Procedure TCell.ComputeOnce;
Begin
   FRuntime.ComputeOnce(Y, dY, Csts, CompVars);
End;

//==============================================================================

Procedure TCell.Compute(Var at: Double; Const atEnd: Double);
Begin
   Assert(FIntegrator <> Nil, 'TCell.Compute: an integrator must be associated to the cell.');

   (FIntegrator As TODEIntegrator).Execute(@Self, at, atEnd);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

