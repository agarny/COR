//==============================================================================
// ODE forward Euler integrator method
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 22/04/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

void __declspec(dllexport) __cdecl ODEForwardEuler(void (*aCallBack)(double, double*, double*, void*), int anY, double* aY, double* adY, double* at, double atEnd, double ah, void* aData)
{
   double tStart, Realh;
   int i, hNb;

   // Y_n+1 = Y_n + h * f(t_n, Y_n)

   tStart = *at;

   hNb = 0;

   Realh = ah;

   while (*at != atEnd)
   {
      hNb++;

      // Check the time step is correct

      if (*at+Realh > atEnd)
         Realh = atEnd-*at;

      // Compute f(t_n, Y_n)

      aCallBack(*at, aY, adY, aData);

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

