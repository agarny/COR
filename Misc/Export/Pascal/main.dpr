Program main;

{$APPTYPE CONSOLE}

Uses
   test;

Const
   tMax = 30.0;   // Simulation duration (s)
   h = 0.001;     // Time step (s)
   hh = 0.1;      // Output time step (s)

Var
   t, tt: Double;
   I: Integer;

Begin
   t := 0.0;    // Time (s)
   tt := 0.0;   // Output time (s)

   Init;   // Initialise the model

   While (t <= tMax+h/2.0) Do Begin
      // Output the data, if necessary

      If (t >= tt-h/2.0) Then Begin
         Write(t);

         For I := 0 To _NB_OF_STATE_VARIABLES_-1 Do Begin
            Write(Chr(9));

            Write(Y[I]);
         End;

         WriteLn;

         tt := tt+hh;
      End;

      Compute(t);   // Compute the model

      // Integrate the model

      For I := 0 To _NB_OF_STATE_VARIABLES_-1 Do
         Y[I] := Y[I]+h*dY[I];

      t := t+h;   // Advance in time
   End;
End.
