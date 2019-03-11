//==============================================================================
// ODE CVODE integrator class
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 05/07/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// Note: makes use of version 2.5.0 of CVODE
//          http://www.llnl.gov/CASC/sundials/download/download.html
//==============================================================================

Unit ODECVODEIntegrator;

//==============================================================================

Interface

//==============================================================================

Uses
   SysUtils, ODEIntegrator, CellMLAPIToMCEngine, DeCAL, Cell;

//==============================================================================

{$M+}

//==============================================================================

Type
   N_Vector = ^_generic_N_Vector;
   _generic_N_Vector_Ops = Packed Record
      nvclone: Function(Const w: N_Vector): N_Vector;
      nvcloneempty: Function(Const w: N_Vector): N_Vector;
      nvdestroy: Procedure(v: N_Vector);
      nvspace: Procedure(Const v: N_Vector; lrw, liw: PLongInt);
      nvgetarraypointer: Function(v: N_Vector): PDouble;
      nvsetarraypointer: Procedure(v_data: PDouble; v: N_Vector);
      nvlinearsum: Procedure(Const a: Double; x: N_Vector; Const b: Double; y, z: N_Vector);
      nvconst: Procedure(Const c: Double; z: N_Vector);
      nvprod: Procedure(Const x, y: N_Vector; z: N_Vector);
      nvdiv: Procedure(Const x, y: N_Vector; z: N_Vector);
      nvscale: Procedure(Const c: Double; Const x: N_Vector; z: N_Vector);
      nvabs: Procedure(Const x: N_Vector; z: N_Vector);
      nvinv: Procedure(Const x: N_Vector; z: N_Vector);
      nvaddconst: Procedure(Const x: N_Vector; Const b: Double; z: N_Vector);
      nvdotprod: Function(Const x, y: N_Vector): Double;
      nvmaxnorm: Function(Const x: N_Vector): Double;
      nvwrmsnorm: Function(Const x, w: N_Vector): Double;
      nvwrmsnormmask: Function(Const x, w, id: N_Vector): Double;
      nvmin: Function(Const x: N_Vector): Double;
      nvwl2norm: Function(Const x, w: N_Vector): Double;
      nvl1norm: Function(Const x: N_Vector): Double;
      nvcompare: Procedure(Const c: Double; Const x: N_Vector; z: N_Vector);
      nvinvtest: Function(Const x: N_Vector; z: N_Vector): Integer;
      nvconstrmask: Function(Const c, x: N_Vector; m: N_Vector): Integer;
      nvminquotient: Function(Const num, denom: N_Vector): Double;
   End;
   _generic_N_Vector = Packed Record
      content: Pointer;
      ops: ^_generic_N_Vector_Ops;
   End;
   N_VectorContent_Serial = ^_N_VectorContent_Serial;
   _N_VectorContent_Serial = Packed Record
      length: LongInt;
      own_data: Integer;
      data: PDouble;
   End;
   CVODEFunc = Function (t: Double; y: N_Vector; ydot: N_Vector; f_data: Pointer): Integer; CDecl;
   CVErrHandlerFunc = Procedure(Const error_code: Integer; Const module, _function, msg: PChar; Const eh_data: Pointer); CDecl;

Type
   CVODEException = Class(Exception);
   TODECVODEIntegratorMethod = (mAdamsMoulton = 1, mBDF = 2);
   TODECVODEIntegratorIterator = (iFunctional = 1, iNewton = 2);
   TODECVODEIntegratorLinearSolver = (lsDense, lsBanded, lsDiagonal, lsGMRES, lsBiCGStab, lsTFQMR);
   TODECVODEIntegratorPreconditioner = (pNone, pBanded);
   TODECVODEIntegrator = Class(TODEIntegrator)
      Private
         // Properties used for internal purposes

         CVODEMem: Pointer;

         Initialised: Boolean;

         YVector: N_Vector;

         FMaxTimeStep: Double;

         FMaxNbOfSteps: Integer;

         FMethod: TODECVODEIntegratorMethod;

         FIterator: TODECVODEIntegratorIterator;
         FLinearSolver: TODECVODEIntegratorLinearSolver;
         FPreconditioner: TODECVODEIntegratorPreconditioner;
         FUpperHalfBandwidth, FLowerHalfBandwidth: Integer;

         FRelTol: Double;
         FAbsTol: Double;

      Public
         // Constructor & Destructor

         Constructor Create(Const aCell: PCell; Const aMaxTimeStep: Double = 0; Const aMaxNbOfSteps: Integer = 500; Const aMethod: TODECVODEIntegratorMethod = mBDF; Const aIterator: TODECVODEIntegratorIterator = iNewton; Const aLinearSolver: TODECVODEIntegratorLinearSolver = lsDense; Const aPreconditioner: TODECVODEIntegratorPreconditioner = pBanded; Const aUpperHalfBandwidth: Integer = 1; Const aLowerHalfBandwidth: Integer = 1; Const aRelTol: Double = 1e-7; Const aAbsTol: Double = 1e-9);
         Destructor Destroy; Override;

         // User's methods

         Procedure Init(Const at: Double = 0);
         Procedure Execute(Const aCell: PCell; Var at: Double; Const atEnd: Double); Override;

      Published
         Property MaxTimeStep: Double Read FMaxTimeStep;

         Property MaxNbOfSteps: Integer Read FMaxNbOfSteps;

         Property Method: TODECVODEIntegratorMethod Read FMethod;
         
         Property Iterator: TODECVODEIntegratorIterator Read FIterator;
         Property LinearSolver: TODECVODEIntegratorLinearSolver Read FLinearSolver;
         Property Preconditioner: TODECVODEIntegratorPreconditioner Read FPreconditioner;
         Property UpperHalfBandwidth: Integer Read FUpperHalfBandwidth;
         Property LowerHalfBandwidth: Integer Read FLowerHalfBandwidth;

         Property RelTol: Double Read FRelTol;
         Property AbsTol: Double Read FAbsTol;
   End;

//==============================================================================

Var
   ODECVODEIntegratorMethods: DArray;
   ODECVODEIntegratorIterators: DArray;
   ODECVODEIntegratorLinearSolvers: DArray;
   ODECVODEIntegratorPreconditioners: DArray;

//==============================================================================

Procedure LoadODECVODEFunctions;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF COR_SPECIFIC}
   CORCommon,
{$ENDIF}
   Windows;

//==============================================================================

Var
   CVBand: Function(Const aCVODEMem: Pointer; Const aNbOfStateVariables, aUpperHalfBandwidth, aLowerHalfBandwidth: LongInt): Integer; CDecl;
   CVBandPrecInit: Function(Const aCVODEMem: Pointer; Const aNbOfStateVariables, aUpperHalfBandwidth, aLowerHalfBandwidth: LongInt): Pointer; CDecl;
   CVDense: Function(Const aCVODEMem: Pointer; Const aNbOfStateVariables: LongInt): Integer; CDecl;
   CVDiag: Function(Const aCVODEMem: Pointer): Integer; CDecl;
   CVODE: Function(Const aCVODEMem: Pointer; Const atEnd: Double; Const aRealY: N_Vector; Const at: PDouble; Const aTask: Integer): Integer; CDecl;
   CVODECreate: Function(Const aIntegrator, aIterator: Integer): Pointer; CDecl;
   CVODEFree: Procedure(Const aCVODEMem: PPointer); CDecl;
   CVODEInit: Function(Const aCVODEMem: Pointer; Const aCVODEFunc: CVODEFunc; Const at0: Double; Const aRealY0: N_Vector): Integer; CDecl;
   CVODEReInit: Function(Const aCVODEMem: Pointer; Const at0: Double; Const aRealY0: N_Vector): Integer; CDecl;
   CVODESetErrHandlerFn: Function(Const aCVODEMem: Pointer; Const aErrHandlerFunc: CVErrHandlerFunc; Const aErrHandlerData: Pointer): Integer; CDecl;
   CVODESetMaxNumSteps: Function(Const aCVODEMem: Pointer; Const aMaxNbOfSteps: LongInt): Integer; CDecl;
   CVODESetMaxStep: Function(Const aCVODEMem: Pointer; Const ahMax: Double): Integer; CDecl;
   CVODESetUserData: Function(Const aCVODEMem: Pointer; Const aFData: Pointer): Integer; CDecl;
   CVODESSTolerances: Function(Const aCVODEMem: Pointer; Const aRelTol: Double; Const aAbsTol: Double): Integer; CDecl;
   CVSpbcg: Function(Const aCVODEMem: Pointer; Const aPreType, aMaxKrylovDim: Integer): Integer; CDecl;
   CVSpgmr: Function(Const aCVODEMem: Pointer; Const aPreType, aMaxKrylovDim: Integer): Integer; CDecl;
   CVSptfqmr: Function(Const aCVODEMem: Pointer; Const aPreType, aMaxKrylovDim: Integer): Integer; CDecl;
   N_VDestroy_Serial: Procedure(Const v: N_Vector); CDecl;
   N_VMake_Serial: Function(Const aNbOfStateVariables: LongInt; Const aStateVariables: PPointer): N_Vector; CDecl;

//==============================================================================

Procedure LoadODECVODEFunctions;
Begin
   CVBand               := GetProcAddress(ODEDLLHandle, 'CVBand');
   CVBandPrecInit       := GetProcAddress(ODEDLLHandle, 'CVBandPrecInit');
   CVDense              := GetProcAddress(ODEDLLHandle, 'CVDense');
   CVDiag               := GetProcAddress(ODEDLLHandle, 'CVDiag');
   CVODE                := GetProcAddress(ODEDLLHandle, 'CVode');
   CVODECreate          := GetProcAddress(ODEDLLHandle, 'CVodeCreate');
   CVODEFree            := GetProcAddress(ODEDLLHandle, 'CVodeFree');
   CVODEInit            := GetProcAddress(ODEDLLHandle, 'CVodeInit');
   CVODEReInit          := GetProcAddress(ODEDLLHandle, 'CVodeReInit');
   CVODESetErrHandlerFn := GetProcAddress(ODEDLLHandle, 'CVodeSetErrHandlerFn');
   CVODESetMaxNumSteps  := GetProcAddress(ODEDLLHandle, 'CVodeSetMaxNumSteps');
   CVODESetMaxStep      := GetProcAddress(ODEDLLHandle, 'CVodeSetMaxStep');
   CVODESetUserData     := GetProcAddress(ODEDLLHandle, 'CVodeSetUserData');
   CVODESSTolerances    := GetProcAddress(ODEDLLHandle, 'CVodeSStolerances');
   CVSpbcg              := GetProcAddress(ODEDLLHandle, 'CVSpbcg');
   CVSpgmr              := GetProcAddress(ODEDLLHandle, 'CVSpgmr');
   CVSptfqmr            := GetProcAddress(ODEDLLHandle, 'CVSptfqmr');
   N_VDestroy_Serial    := GetProcAddress(ODEDLLHandle, 'N_VDestroy_Serial');
   N_VMake_Serial       := GetProcAddress(ODEDLLHandle, 'N_VMake_Serial');
End;

//==============================================================================

Procedure CVODEErrHandler(Const aErrorCode: Integer;
                          Const aModule, aFunction, aErrorMsg: PChar;
                          Const aErrorHandlerData: Pointer); CDecl; Inline;
Begin
   If (aErrorCode <> 99 {CV_WARNING}) Then
      Raise CVODEException.Create(LowerCase(aErrorMsg[0])+Copy(aErrorMsg, 2, Length(aErrorMsg)-1));
End;

//==============================================================================

Function CVODECompute(at: Double; aRealY: N_Vector; aRealdY: N_Vector;
                      aData: Pointer): Integer; CDecl;
Begin
   With TCell(aData^) Do
      Runtime.Compute(at, PADouble(N_VectorContent_Serial(aRealY.content).data),
                          PADouble(N_VectorContent_Serial(aRealdY.content).data),
                          Csts, CompVars);

   Result := 0;
End;

//==============================================================================

Constructor TODECVODEIntegrator.Create(Const aCell: PCell;
                                       Const aMaxTimeStep: Double;
                                       Const aMaxNbOfSteps: Integer;
                                       Const aMethod: TODECVODEIntegratorMethod;
                                       Const aIterator: TODECVODEIntegratorIterator;
                                       Const aLinearSolver: TODECVODEIntegratorLinearSolver;
                                       Const aPreconditioner: TODECVODEIntegratorPreconditioner;
                                       Const aUpperHalfBandwidth, aLowerHalfBandwidth: Integer;
                                       Const aRelTol, aAbsTol: Double);
Var
   LinearSolver: String;
   Preconditioner: String;
   HalfBandwidths: String;
   NeedHalfBandwidths: Boolean;
Begin
   Inherited Create;

   LinearSolver   := '';
   Preconditioner := '';
   HalfBandwidths := '';

   If (aIterator = iNewton) Then Begin
      LinearSolver := ', '+ODECVODEIntegratorLinearSolvers.AtAsString(Integer(aLinearSolver));

      Case aLinearSolver Of
         lsBanded:
            NeedHalfBandwidths := True;
         lsGMRES, lsBiCGStab, lsTFQMR: Begin
            Preconditioner := ', '+ODECVODEIntegratorPreconditioners.AtAsString(Integer(aPreconditioner));

            NeedHalfBandwidths := aPreconditioner = pBanded;
         End;
      Else   //   lsDense or lsDiagonal

         NeedHalfBandwidths := False;
      End;

      If (NeedHalfBandwidths) Then
         HalfBandwidths := ', '+IntToStr(aUpperHalfBandwidth)+', '+IntToStr(aLowerHalfBandwidth);
   End;

   // Retrieve the maximum time step

   FMaxTimeStep := aMaxTimeStep;

   // Retrieve the maximum number of steps

   FMaxNbOfSteps := aMaxNbOfSteps;

   // Method of integration

   FMethod := aMethod;

   // Iterator

   FIterator           := aIterator;
   FLinearSolver       := aLinearSolver;
   FPreconditioner     := aPreconditioner;
   FUpperHalfBandwidth := aUpperHalfBandwidth;
   FLowerHalfBandwidth := aLowerHalfBandwidth;

   // Retrieve the absolute and relative tolerances

   FRelTol := aRelTol;
   FAbsTol := aAbsTol;

   // Create the Y vector

   YVector := N_VMake_Serial(aCell^.Runtime.NbOfStateVars, Pointer(aCell^.Y));

   // Create the CVODE object

   CVODEMem := CVODECreate(Integer(FMethod), Integer(FIterator));

   // Specify the address of the user data to be passed to CVODE

   CVODESetUserData(CVODEMem, aCell);

   // Specify the maximum time step to be used

   CVODESetMaxStep(CVODEMem, FMaxTimeStep);

   // Specify the maximum number of steps allowed in one operation

   CVODESetMaxNumSteps(CVODEMem, FMaxNbOfSteps);

   // Use our own error handler

   CVODESetErrHandlerFn(CVODEMem, CVODEErrHandler, Nil);
End;

//==============================================================================

Destructor TODECVODEIntegrator.Destroy;
Begin
   // Destroy the Y vector

   N_VDestroy_Serial(YVector);

   // Free the CVODE object

   CVODEFree(@CVODEMem);
End;

//==============================================================================

Procedure TODECVODEIntegrator.Init(Const at: Double);
Type
   TODECVODEIntegratorPreconditioningType = (pNone, pLeft, pRight, pBoth);
Begin
   If (Not Initialised) Then Begin
      // Initialise the CVODE object

      CVODEInit(CVODEMem, CVODECompute, at, YVector);

      // Set the relative and absolute tolerances

      CVODESSTolerances(CVODEMem, FRelTol, FAbsTol);

      // Attach the linear solver, in case we use a Newton type of iterator

      If (FIterator = iNewton) Then
         Case FLinearSolver Of
            lsDense:
               CVDense(CVODEMem, N_VectorContent_Serial(YVector.content).length);
            lsBanded:
               CVBand(CVODEMem, N_VectorContent_Serial(YVector.content).length, FUpperHalfBandwidth, FLowerHalfBandwidth);
            lsDiagonal:
               CVDiag(CVODEMem);
         Else
            // We are using one of the Krylov linear solvers, so initialise the
            // preconditioner module, if needed

            Case FPreconditioner Of
               pBanded: Begin
                  Case FLinearSolver Of
                     lsGMRES:
                        CVSpgmr(CVODEMem, Integer(pLeft), 0);
                     lsBiCGStab:
                        CVSpbcg(CVODEMem, Integer(pLeft), 0);
                     lsTFQMR:
                        CVSptfqmr(CVODEMem, Integer(pLeft), 0);
                  End;

                  CVBandPrecInit(CVODEMem, N_VectorContent_Serial(YVector.content).length, FUpperHalfBandwidth, FLowerHalfBandwidth);
               End;
            Else
               // No preconditioner

               Case FLinearSolver Of
                  lsGMRES:
                     CVSpgmr(CVODEMem, Integer(pNone), 0);
                  lsBiCGStab:
                     CVSpbcg(CVODEMem, Integer(pNone), 0);
                  lsTFQMR:
                     CVSptfqmr(CVODEMem, Integer(pNone), 0);
               End;
            End;
         End;

      Initialised := True;
   End Else
      // Reinitialise the CVODE object

      CVODEReInit(CVODEMem, at, YVector);
End;

//==============================================================================

Procedure TODECVODEIntegrator.Execute(Const aCell: PCell; Var at: Double;
                                      Const atEnd: Double);
Type
   TODECVODEIntegratorTask = (tNormal = 1, tOneStep = 2);
Begin
   // Compute the model itself

   CVODE(CVODEMem, atEnd, YVector, @at, Integer(tNormal));
End;

//==============================================================================

Initialization

//==============================================================================

// Note: the order in which we list the various integrators must be the same as
//       for their corresponding constant declaration. This is very important!
//       (see above)

ODECVODEIntegratorMethods := DArray.Create;

With ODECVODEIntegratorMethods Do Begin
   Add(['Adams-Moulton']);
   Add(['BDF']);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

ODECVODEIntegratorIterators := DArray.Create;

With ODECVODEIntegratorIterators Do Begin
   Add(['Functional']);
   Add(['Newton']);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

ODECVODEIntegratorLinearSolvers := DArray.Create;

With ODECVODEIntegratorLinearSolvers Do Begin
   Add(['Dense']);
   Add(['Banded']);
   Add(['Diagonal']);
   Add(['GMRES']);
   Add(['BiCGStab']);
   Add(['TFQMR']);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

ODECVODEIntegratorPreconditioners := DArray.Create;

With ODECVODEIntegratorPreconditioners Do Begin
   Add(['None']);
   Add(['Banded']);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

//==============================================================================

Finalization

//==============================================================================

// Note: we must NOT free the objects held by "ODECVODEIntegratorMethods",
//       "ODECVODEIntegratorIterators", "ODECVODEIntegratorLinearSolvers" and
//       "ODECVODEIntegratorPreconditioners", for they are strings!

ODECVODEIntegratorMethods.Free;
ODECVODEIntegratorIterators.Free;
ODECVODEIntegratorLinearSolvers.Free;
ODECVODEIntegratorPreconditioners.Free;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

