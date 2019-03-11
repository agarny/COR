#include <stdio.h>
#include "test.hpp"

main()
{
   const double tMax = 30.0;   // Simulation duration (s)

   const double h = 0.001;   // Time step (s)
   const double hh = 0.1;    // Output time step (s)

   double t = 0.0;    // Time (s)
   double tt = 0.0;   // Output time (s)

   test *myTest = new test;   // Model

   myTest->init();   // Initialise the model

   while (t <= tMax+h/2.0) {
      // Output the data, if necessary

      if (t >= tt-h/2.0) {
         printf("%f", t);

         for (int i = 0; i < _NB_OF_STATE_VARIABLES_; i++)
            printf("\t%f", myTest->Y[i]);

         printf("\n");

         tt += hh;
      }

      myTest->compute(t);   // Compute the model

      // Integrate the model

      for (int i = 0; i < _NB_OF_STATE_VARIABLES_; i++)
         myTest->Y[i] += h*myTest->dY[i];

      t += h;   // Advance in time
   }

   delete myTest;
}
