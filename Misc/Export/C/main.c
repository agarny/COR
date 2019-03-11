#include <stdio.h>
#include "test.h"

main()
{
   const double tMax = 30.0;   // Simulation duration (s)

   const double h = 0.001;   // Time step (s)
   const double hh = 0.1;    // Output time step (s)

   double t = 0.0;    // Time (s)
   double tt = 0.0;   // Output time (s)

   int i;

   init();   // Initialise the model

   while (t <= tMax+h/2.0) {
      // Output the data, if necessary

      if (t >= tt-h/2.0) {
         printf("%f", t);

         for (i = 0; i < _NB_OF_STATE_VARIABLES_; i++)
            printf("\t%f", Y[i]);

         printf("\n");

         tt += hh;
      }

      compute(t);   // Compute the model

      // Integrate the model

      for (i = 0; i < _NB_OF_STATE_VARIABLES_; i++)
         Y[i] += h*dY[i];

      t += h;   // Advance in time
   }
}
