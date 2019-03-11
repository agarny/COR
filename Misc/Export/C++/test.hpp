//==============================================================================
// CellML file:   D:\Programming\COR\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml
// CellML model:  hodgkin_huxley_squid_axon_model_1952_original
// Date and time: 10/04/2008 at 03:12:17
//------------------------------------------------------------------------------
// Conversion from CellML 1.0 to C++ (header) was done using COR (0.9.31.909)
//    Copyright 2002-2008 Dr Alan Garny
//    http://COR.physiol.ox.ac.uk/ - COR@physiol.ox.ac.uk
//------------------------------------------------------------------------------
// http://www.CellML.org/
//==============================================================================

#ifndef __TEST_HPP__
#define __TEST_HPP__

//------------------------------------------------------------------------------

#define _NB_OF_STATE_VARIABLES_ 4

//------------------------------------------------------------------------------

class test
{
   public:
      //------------------------------------------------------------------------
      // State variables
      //------------------------------------------------------------------------

      double Y[_NB_OF_STATE_VARIABLES_];
      double dY[_NB_OF_STATE_VARIABLES_];
      // 0: membrane___V (millivolt)
      // 1: potassium_channel_n_gate___n (dimensionless)
      // 2: sodium_channel_h_gate___h (dimensionless)
      // 3: sodium_channel_m_gate___m (dimensionless)

      //------------------------------------------------------------------------
      // Constants
      //------------------------------------------------------------------------

      double leakage_current___g_L;   // milliS_per_cm2
      double membrane___Cm;   // microF_per_cm2
      double membrane___E_R;   // millivolt
      double potassium_channel___g_K;   // milliS_per_cm2
      double sodium_channel___g_Na;   // milliS_per_cm2

      //------------------------------------------------------------------------
      // Computed variables
      //------------------------------------------------------------------------

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

      //------------------------------------------------------------------------
      // Procedures
      //------------------------------------------------------------------------

      void init();
      void compute(double t);
};

//------------------------------------------------------------------------------

#endif

//==============================================================================
// End of file
//==============================================================================
