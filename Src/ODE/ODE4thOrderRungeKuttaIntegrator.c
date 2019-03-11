//==============================================================================
// ODE 4th-order Runge-Kutta integrator method
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

void __declspec(dllexport) __cdecl ODE4thOrderRungeKutta(void (*aCallBack)(double, double*, double*, void*), int anY, double* aY, double* adY, double* at, double atEnd, double ah, void* aData, double* ak1, double* ak2And3, double* aYk2Or3Or4)
{
   const double One_6 = 1.0/6.0;
   const double One_3 = 1.0/3.0;

   double tStart, Realh, Realh_2;
   int i, hNb;

   // k1 = h * f(t_n, Y_n)
   // k2 = h * f(t_n + h / 2, Y_n + k1 / 2)
   // k3 = h * f(t_n + h / 2, Y_n + k2 / 2)
   // k4 = h * f(t_n + h, Y_n + k3)
   // Y_n+1 = Y_n + k1 / 6 + k2 / 3 + k3 / 3 + k4 / 6

   // Note: the algorithm hereafter doesn't compute k1, k2, k3 and k4 as such
   //       and this simply for performance reasons...

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

      // Compute k1 and Yk2

      for (i = 0; i < anY; i++)
      {
         ak1[i] = adY[i];

         aYk2Or3Or4[i] = aY[i]+Realh_2*adY[i];
      }

      // Compute f(t_n + h / 2, Y_n + k1 / 2)

      aCallBack(*at+Realh_2, aYk2Or3Or4, adY, aData);

      // Compute k2 and Yk3

      for (i = 0; i < anY; i++)
      {
         ak2And3[i] = adY[i];

         aYk2Or3Or4[i] = aY[i]+Realh_2*adY[i];
      }

      // Compute f(t_n + h / 2, Y_n + k2 / 2)

      aCallBack(*at+Realh_2, aYk2Or3Or4, adY, aData);

      // Compute k3 and Yk4

      for (i = 0; i < anY; i++)
      {
         ak2And3[i] += adY[i];

         aYk2Or3Or4[i] = aY[i]+Realh*adY[i];
      }

      // Compute f(t_n + h, Y_n + k3)

      aCallBack(*at+Realh, aYk2Or3Or4, adY, aData);

      // Compute k4 and therefore Y_n+1

      for (i = 0; i < anY; i++)
         aY[i] += Realh*(One_6*(ak1[i]+adY[i])+One_3*ak2And3[i]);

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

