//==============================================================================
// ODE fixed time step integrator class
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 07/07/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit ODEFixedTimeStepIntegrator;

//==============================================================================

Interface

//==============================================================================

Uses
   ODEIntegrator, Cell;

//==============================================================================

Type
   TODEFixedTimeStepIntegrator = Class(TODEIntegrator)
      Protected
         // Private representation of published properties

         Fh: Double;

      Public
         // Constructor & Destructor

         Constructor Create(Const ah: Double);

      Published
         // Published properties

         Property h: Double Read Fh Write Fh;
   End;

//==============================================================================

Implementation

//==============================================================================

Constructor TODEFixedTimeStepIntegrator.Create(Const ah: Double);
Begin
   Inherited Create;

   Fh := ah;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

