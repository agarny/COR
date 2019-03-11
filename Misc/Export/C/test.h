//==============================================================================
// CellML file:   D:\Programming\COR\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml
// CellML model:  hodgkin_huxley_squid_axon_model_1952_original
// Date and time: 10/04/2008 at 03:12:11
//------------------------------------------------------------------------------
// Conversion from CellML 1.0 to C (header) was done using COR (0.9.31.909)
//    Copyright 2002-2008 Dr Alan Garny
//    http://COR.physiol.ox.ac.uk/ - COR@physiol.ox.ac.uk
//------------------------------------------------------------------------------
// http://www.CellML.org/
//==============================================================================

#ifndef __TEST_H__
#define __TEST_H__

//------------------------------------------------------------------------------
// State variables
//------------------------------------------------------------------------------

#define _NB_OF_STATE_VARIABLES_ 4

extern double Y[_NB_OF_STATE_VARIABLES_];
extern double dY[_NB_OF_STATE_VARIABLES_];
// 0: membrane___V (millivolt)
// 1: potassium_channel_n_gate___n (dimensionless)
// 2: sodium_channel_h_gate___h (dimensionless)
// 3: sodium_channel_m_gate___m (dimensionless)

//------------------------------------------------------------------------------
// Constants
//------------------------------------------------------------------------------

extern double leakage_current___g_L;   // milliS_per_cm2
extern double membrane___Cm;   // microF_per_cm2
extern double membrane___E_R;   // millivolt
extern double potassium_channel___g_K;   // milliS_per_cm2
extern double sodium_channel___g_Na;   // milliS_per_cm2

//------------------------------------------------------------------------------
// Computed variables
//------------------------------------------------------------------------------

extern double leakage_current___E_L;   // millivolt
extern double leakage_current___i_L;   // microA_per_cm2
extern double membrane___i_Stim;   // microA_per_cm2
extern double potassium_channel___E_K;   // millivolt
extern double potassium_channel___i_K;   // microA_per_cm2
extern double potassium_channel_n_gate___alpha_n;   // per_millisecond
extern double potassium_channel_n_gate___beta_n;   // per_millisecond
extern double sodium_channel___E_Na;   // millivolt
extern double sodium_channel___i_Na;   // microA_per_cm2
extern double sodium_channel_h_gate___alpha_h;   // per_millisecond
extern double sodium_channel_h_gate___beta_h;   // per_millisecond
extern double sodium_channel_m_gate___alpha_m;   // per_millisecond
extern double sodium_channel_m_gate___beta_m;   // per_millisecond

//------------------------------------------------------------------------------
// Procedures
//------------------------------------------------------------------------------

extern void init();
extern void compute(double t);

//------------------------------------------------------------------------------

#endif

//==============================================================================
// End of file
//==============================================================================
