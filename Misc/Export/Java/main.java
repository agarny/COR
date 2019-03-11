class main
{  
   public static void main(String args[])
   {
      double tMax = 30.0;   // Simulation duration (s)

      double h = 0.001;   // Time step (s)
      double hh = 0.1;    // Output time step (s)

      double t = 0.0;    // Time (s)
      double tt = 0.0;   // Output time (s)

      test myTest = new test();   // Model

      myTest.init();   // Initialise the model

      while (t <= tMax+h/2.0) {
         // Output the data, if necessary

         if (t >= tt-h/2.0) {
            System.out.print(t);

            for (int i = 0; i < myTest._NB_OF_STATE_VARIABLES_; i++)
               System.out.print("\t"+myTest.Y[i]);

            System.out.println();

            tt += hh;
         }

         myTest.compute(t);   // Compute the model

         // Integrate the model

         for (int i = 0; i < myTest._NB_OF_STATE_VARIABLES_; i++)
            myTest.Y[i] += h*myTest.dY[i];

         t += h;   // Advance in time
      }
   }
}
