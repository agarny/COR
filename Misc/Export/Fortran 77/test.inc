!=======================================================================
! CellML file:   D:\Programming\COR\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml
! CellML model:  hodgkin_huxley_squid_axon_model_1952_original
! Date and time: 10/04/2008 at 03:12:29
!-----------------------------------------------------------------------
! Conversion from CellML 1.0 to Fortran 77 (include) was done using COR (0.9.31.909)
!    Copyright 2002-2008 Dr Alan Garny
!    http://COR.physiol.ox.ac.uk/ - COR@physiol.ox.ac.uk
!-----------------------------------------------------------------------
! http://www.CellML.org/
!=======================================================================

      !-----------------------------------------------------------------
      ! State variables
      !-----------------------------------------------------------------

      INTEGER _NB_OF_STATE_VARIABLES_
      PARAMETER (_NB_OF_STATE_VARIABLES_ = 4)

      DOUBLE PRECISION Y(4)
      COMMON /Y/ Y
      DOUBLE PRECISION DY(4)
      COMMON /DY/ DY
      ! 1: membrane___V (millivolt)
      ! 2: potassium_channel_n_gate___n (dimensionless)
      ! 3: sodium_channel_h_gate___h (dimensionless)
      ! 4: sodium_channel_m_gate___m (dimensionless)

      !-----------------------------------------------------------------
      ! Constants
      !-----------------------------------------------------------------

      DOUBLE PRECISION leakage_current___g_L   ! milliS_per_cm2
      DOUBLE PRECISION membrane___Cm   ! microF_per_cm2
      DOUBLE PRECISION membrane___E_R   ! millivolt
      DOUBLE PRECISION potassium_channel___g_K   ! milliS_per_cm2
      DOUBLE PRECISION sodium_channel___g_Na   ! milliS_per_cm2

      COMMON /CONSTANTS001/ leakage_current___g_L, membrane___Cm, membra
     'ne___E_R, potassium_channel___g_K, sodium_channel___g_Na

      !-----------------------------------------------------------------
      ! Computed variables
      !-----------------------------------------------------------------

      DOUBLE PRECISION leakage_current___E_L   ! millivolt
      DOUBLE PRECISION leakage_current___i_L   ! microA_per_cm2
      DOUBLE PRECISION membrane___i_Stim   ! microA_per_cm2
      DOUBLE PRECISION potassium_channel___E_K   ! millivolt
      DOUBLE PRECISION potassium_channel___i_K   ! microA_per_cm2
      DOUBLE PRECISION potassium_channel_n_gate___alpha_n   ! per_millisecond
      DOUBLE PRECISION potassium_channel_n_gate___beta_n   ! per_millisecond
      DOUBLE PRECISION sodium_channel___E_Na   ! millivolt
      DOUBLE PRECISION sodium_channel___i_Na   ! microA_per_cm2
      DOUBLE PRECISION sodium_channel_h_gate___alpha_h   ! per_millisecond
      DOUBLE PRECISION sodium_channel_h_gate___beta_h   ! per_millisecond
      DOUBLE PRECISION sodium_channel_m_gate___alpha_m   ! per_millisecond
      DOUBLE PRECISION sodium_channel_m_gate___beta_m   ! per_millisecond

      COMMON /COMPVARS001/ leakage_current___E_L, leakage_current___i_L,
     ' membrane___i_Stim, potassium_channel___E_K, potassium_channel___i
     '_K, potassium_channel_n_gate___alpha_n, potassium_channel_n_gate__
     '_beta_n, sodium_channel___E_Na, sodium_channel___i_Na, sodium_chan
     'nel_h_gate___alpha_h, sodium_channel_h_gate___beta_h, sodium_chann
     'el_m_gate___alpha_m, sodium_channel_m_gate___beta_m

      !-----------------------------------------------------------------
      ! User's methods
      !-----------------------------------------------------------------

      EXTERNAL INIT
      EXTERNAL COMPUTE

!=======================================================================
! End of file
!=======================================================================
