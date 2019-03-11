//==============================================================================
// CellML file:   D:\Programming\COR\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml
// CellML model:  hodgkin_huxley_squid_axon_model_1952_original
// Date and time: 10/04/2008 at 03:12:11
//------------------------------------------------------------------------------
// Conversion from CellML 1.0 to C was done using COR (0.9.31.909)
//    Copyright 2002-2008 Dr Alan Garny
//    http://COR.physiol.ox.ac.uk/ - COR@physiol.ox.ac.uk
//------------------------------------------------------------------------------
// http://www.CellML.org/
//==============================================================================

#include "test.h"

//------------------------------------------------------------------------------

#include <math.h>

//------------------------------------------------------------------------------
// State variables
//------------------------------------------------------------------------------

double Y[_NB_OF_STATE_VARIABLES_];
double dY[_NB_OF_STATE_VARIABLES_];
// 0: membrane___V (millivolt)
// 1: potassium_channel_n_gate___n (dimensionless)
// 2: sodium_channel_h_gate___h (dimensionless)
// 3: sodium_channel_m_gate___m (dimensionless)

//------------------------------------------------------------------------------
// Constants
//------------------------------------------------------------------------------

double leakage_current___g_L;   // milliS_per_cm2
double membrane___Cm;   // microF_per_cm2
double membrane___E_R;   // millivolt
double potassium_channel___g_K;   // milliS_per_cm2
double sodium_channel___g_Na;   // milliS_per_cm2

//------------------------------------------------------------------------------
// Computed variables
//------------------------------------------------------------------------------

double leakage_current___E_L;   // millivolt
double leakage_current___i_L;   // microA_per_cm2
double membrane___i_Stim;   // microA_per_cm2
double potassium_channel___E_K;   // millivolt
double potassium_channel___i_K;   // microA_per_cm2
double potassium_channel_n_gate___alpha_n;   // per_millisecond
double potassium_channel_n_gate___beta_n;   // per_millisecond
double sodium_channel___E_Na;   // millivolt
double sodium_channel___i_Na;   // microA_per_cm2
double sodium_channel_h_gate___alpha_h;   // per_millisecond
double sodium_channel_h_gate___beta_h;   // per_millisecond
double sodium_channel_m_gate___alpha_m;   // per_millisecond
double sodium_channel_m_gate___beta_m;   // per_millisecond

//------------------------------------------------------------------------------
// Initialisation
//------------------------------------------------------------------------------

void init()
{
   //---------------------------------------------------------------------------
   // State variables
   //---------------------------------------------------------------------------

   Y[0] = 0.0;   // membrane___V (millivolt)
   Y[1] = 0.325;   // potassium_channel_n_gate___n (dimensionless)
   Y[2] = 0.6;   // sodium_channel_h_gate___h (dimensionless)
   Y[3] = 0.05;   // sodium_channel_m_gate___m (dimensionless)

   //---------------------------------------------------------------------------
   // Constants
   //---------------------------------------------------------------------------

   leakage_current___g_L = 0.3;   // milliS_per_cm2
   membrane___Cm = 1.0;   // microF_per_cm2
   membrane___E_R = 0.0;   // millivolt
   potassium_channel___g_K = 36.0;   // milliS_per_cm2
   sodium_channel___g_Na = 120.0;   // milliS_per_cm2
}

//------------------------------------------------------------------------------
// Computation
//------------------------------------------------------------------------------

void compute(double t)
{
   // t: time (millisecond)

   leakage_current___E_L = membrane___E_R-10.613;
   leakage_current___i_L = leakage_current___g_L*(Y[0]-leakage_current___E_L);

   if ((t >= 10.0) && (t <= 10.5))
      membrane___i_Stim = -20.0;
   else
      membrane___i_Stim = 0.0;

   sodium_channel___E_Na = membrane___E_R-115.0;
   sodium_channel___i_Na = sodium_channel___g_Na*pow(Y[3], 3.0)*Y[2]*(Y[0]-sodium_channel___E_Na);
   potassium_channel___E_K = membrane___E_R+12.0;
   potassium_channel___i_K = potassium_channel___g_K*pow(Y[1], 4.0)*(Y[0]-potassium_channel___E_K);
   dY[0] = -(-membrane___i_Stim+sodium_channel___i_Na+potassium_channel___i_K+leakage_current___i_L)/membrane___Cm;
   potassium_channel_n_gate___alpha_n = 0.01*(Y[0]+10.0)/(exp((Y[0]+10.0)/10.0)-1.0);
   potassium_channel_n_gate___beta_n = 0.125*exp(Y[0]/80.0);
   dY[1] = potassium_channel_n_gate___alpha_n*(1.0-Y[1])-potassium_channel_n_gate___beta_n*Y[1];
   sodium_channel_h_gate___alpha_h = 0.07*exp(Y[0]/20.0);
   sodium_channel_h_gate___beta_h = 1.0/(exp((Y[0]+30.0)/10.0)+1.0);
   dY[2] = sodium_channel_h_gate___alpha_h*(1.0-Y[2])-sodium_channel_h_gate___beta_h*Y[2];
   sodium_channel_m_gate___alpha_m = 0.1*(Y[0]+25.0)/(exp((Y[0]+25.0)/10.0)-1.0);
   sodium_channel_m_gate___beta_m = 4.0*exp(Y[0]/18.0);
   dY[3] = sodium_channel_m_gate___alpha_m*(1.0-Y[3])-sodium_channel_m_gate___beta_m*Y[3];
}

//==============================================================================
// End of file
//==============================================================================
