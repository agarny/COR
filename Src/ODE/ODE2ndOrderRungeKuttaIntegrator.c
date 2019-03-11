//==============================================================================
// ODE 2nd-order Runge-Kutta integrator method
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 21/04/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

void __declspec(dllexport) __cdecl ODE2ndOrderRungeKutta(void (*aCallBack)(double, double*, double*, void*), int anY, double* aY, double* adY, double* at, double atEnd, double ah, void* aData, double* aYk2)
{
   double tStart, Realh, Realh_2;
   int i, hNb;

   // k1 = h * f(t_n, Y_n)
   // k2 = h * f(t_n + h / 2, Y_n + k1 / 2)
   // Y_n+1 = Y_n + k2

   // Note: the algorithm hereafter doesn't compute k1 and k2 as such and this
   //       simply for performance reasons...

   tStart = *at;

   hNb = 0;

   Realh = ah;
   Realh_2 = 0.5*Realh;

   while (*at != atEnd)
   {
      hNb++;

      // Check the time step is correct

      if (*at+Realh > atEnd)
      {
         Realh = atEnd-*at;
         Realh_2 = 0.5*Realh;
      }

      // Compute f(t_n, Y_n)

      aCallBack(*at, aY, adY, aData);

      // Compute k1 and therefore Yk2

      for (i = 0; i < anY; i++)
         aYk2[i] = aY[i]+Realh_2*adY[i];

      // Compute f(t_n + h / 2, Y_n + k1 / 2)

      aCallBack(*at+Realh_2, aYk2, adY, aData);

      // Compute Y_n+1

      for (i = 0; i < anY; i++)
         aY[i] += Realh*adY[i];

      // Advance through time

      if (Realh != ah)
         *at = atEnd;
      else
         *at = tStart+hNb*Realh;
   }
}

//==============================================================================
// End of file
//==============================================================================

