//==============================================================================
// Optimised mathematical functions
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 01/05/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

#include <math.h>

#define DLL_EXPORT __declspec(dllexport)

DLL_EXPORT double OptSin(double aNb)
{
   return sin(aNb);
}

DLL_EXPORT double OptCos(double aNb)
{
   return cos(aNb);
}

DLL_EXPORT double OptTan(double aNb)
{
   return tan(aNb);
}

DLL_EXPORT double OptSec(double aNb)
{
   return 1.0/cos(aNb);
}

DLL_EXPORT double OptCsc(double aNb)
{
   return 1.0/sin(aNb);
}

DLL_EXPORT double OptCot(double aNb)
{
   return 1.0/tan(aNb);
}

DLL_EXPORT double OptSinH(double aNb)
{
   return sinh(aNb);
}

DLL_EXPORT double OptCosH(double aNb)
{
   return cosh(aNb);
}

DLL_EXPORT double OptTanH(double aNb)
{
   return tanh(aNb);
}

DLL_EXPORT double OptSecH(double aNb)
{
   return 1.0/cosh(aNb);
}

DLL_EXPORT double OptCscH(double aNb)
{
   return 1.0/sinh(aNb);
}

DLL_EXPORT double OptCotH(double aNb)
{
   return 1.0/tanh(aNb);
}

DLL_EXPORT double OptArcSin(double aNb)
{
   return asin(aNb);
}

DLL_EXPORT double OptArcCos(double aNb)
{
   return acos(aNb);
}

DLL_EXPORT double OptArcTan(double aNb)
{
   return atan(aNb);
}

DLL_EXPORT double OptArcSec(double aNb)
{
   return acos(1.0/aNb);
}

DLL_EXPORT double OptArcCsc(double aNb)
{
   return asin(1.0/aNb);
}

DLL_EXPORT double OptArcCot(double aNb)
{
   return atan(1.0/aNb);
}

DLL_EXPORT double OptArcSinH(double aNb)
{
   return log(aNb+sqrt(aNb*aNb+1.0));
}

DLL_EXPORT double OptArcCosH(double aNb)
{
   return log(aNb+sqrt(aNb*aNb-1.0));
}

DLL_EXPORT double OptArcTanH(double aNb)
{
   return 0.5*log((1.0+aNb)/(1.0-aNb));
}

DLL_EXPORT double OptArcSecH(double aNb)
{
   double Nb = 1.0/aNb;

   return log(Nb+sqrt(Nb*Nb-1.0));
}

DLL_EXPORT double OptArcCscH(double aNb)
{
   double Nb = 1.0/aNb;

   return log(Nb+sqrt(Nb*Nb+1.0));
}

DLL_EXPORT double OptArcCotH(double aNb)
{
   double Nb = 1.0/aNb;

   return 0.5*log((1.0+Nb)/(1.0-Nb));
}

DLL_EXPORT int OptCeil(double aNb)
{
   return ceil(aNb);
}

DLL_EXPORT int OptFloor(double aNb)
{
   return floor(aNb);
}

DLL_EXPORT double OptExp(double aNb)
{
   return exp(aNb);
}

DLL_EXPORT double OptLN(double aNb)
{
   return log(aNb);
}

DLL_EXPORT double OptLog10(double aNb)
{
   return log10(aNb);
}

DLL_EXPORT int OptMaxI(int aNb1, int aNb2)
{
   return (aNb1 > aNb2)?aNb1:aNb2;
}

DLL_EXPORT double OptMaxD(double aNb1, double aNb2)
{
   return (aNb1 > aNb2)?aNb1:aNb2;
}

DLL_EXPORT int OptMinI(int aNb1, int aNb2)
{
   return (aNb1 < aNb2)?aNb1:aNb2;
}

DLL_EXPORT double OptMinD(double aNb1, double aNb2)
{
   return (aNb1 < aNb2)?aNb1:aNb2;
}

DLL_EXPORT double OptPower(double aBase, double aExponent)
{
   return pow(aBase, aExponent);
}

DLL_EXPORT double OptFact(double aNb)
{
   double Res = 1.0;

   while (aNb > 1.0)
      Res *= aNb--;

   return Res;
}

//==============================================================================
// End of file
//==============================================================================
