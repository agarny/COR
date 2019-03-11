!=======================================================================
! CellML file:   D:\Programming\COR\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml
! CellML model:  hodgkin_huxley_squid_axon_model_1952_original
! Date and time: 10/04/2008 at 03:12:29
!-----------------------------------------------------------------------
! Conversion from CellML 1.0 to Fortran 77 was done using COR (0.9.31.909)
!    Copyright 2002-2008 Dr Alan Garny
!    http://COR.physiol.ox.ac.uk/ - COR@physiol.ox.ac.uk
!-----------------------------------------------------------------------
! http://www.CellML.org/
!=======================================================================

      !-----------------------------------------------------------------
      ! Initialisation
      !-----------------------------------------------------------------

      SUBROUTINE INIT
         IMPLICIT NONE

         DOUBLE PRECISION Y(4)
         COMMON /Y/ Y

         DOUBLE PRECISION leakage_current___g_L   ! milliS_per_cm2
         DOUBLE PRECISION membrane___Cm   ! microF_per_cm2
         DOUBLE PRECISION membrane___E_R   ! millivolt
         DOUBLE PRECISION potassium_channel___g_K   ! milliS_per_cm2
         DOUBLE PRECISION sodium_channel___g_Na   ! milliS_per_cm2

         COMMON /CONSTANTS001/ leakage_current___g_L, membrane___Cm, mem
     '   brane___E_R, potassium_channel___g_K, sodium_channel___g_Na

         !--------------------------------------------------------------
         ! State variables
         !--------------------------------------------------------------

         Y(1) = 0.0D0   ! membrane___V (millivolt)
         Y(2) = 0.325D0   ! potassium_channel_n_gate___n (dimensionless)
         Y(3) = 0.6D0   ! sodium_channel_h_gate___h (dimensionless)
         Y(4) = 0.05D0   ! sodium_channel_m_gate___m (dimensionless)

         !--------------------------------------------------------------
         ! Constants
         !--------------------------------------------------------------

         leakage_current___g_L = 0.3D0   ! milliS_per_cm2
         membrane___Cm = 1.0D0   ! microF_per_cm2
         membrane___E_R = 0.0D0   ! millivolt
         potassium_channel___g_K = 36.0D0   ! milliS_per_cm2
         sodium_channel___g_Na = 120.0D0   ! milliS_per_cm2

         RETURN
      END

      !-----------------------------------------------------------------
      ! Computation
      !-----------------------------------------------------------------

      SUBROUTINE COMPUTE(t)
         IMPLICIT NONE

         DOUBLE PRECISION t
         ! t: time (millisecond)

         DOUBLE PRECISION Y(4)
         COMMON /Y/ Y
         DOUBLE PRECISION DY(4)
         COMMON /DY/ DY

         DOUBLE PRECISION leakage_current___g_L   ! milliS_per_cm2
         DOUBLE PRECISION membrane___Cm   ! microF_per_cm2
         DOUBLE PRECISION membrane___E_R   ! millivolt
         DOUBLE PRECISION potassium_channel___g_K   ! milliS_per_cm2
         DOUBLE PRECISION sodium_channel___g_Na   ! milliS_per_cm2

         COMMON /CONSTANTS001/ leakage_current___g_L, membrane___Cm, mem
     '   brane___E_R, potassium_channel___g_K, sodium_channel___g_Na

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

         COMMON /COMPVARS001/ leakage_current___E_L, leakage_current___i
     '   _L, membrane___i_Stim, potassium_channel___E_K, potassium_chann
     '   el___i_K, potassium_channel_n_gate___alpha_n, potassium_channel
     '   _n_gate___beta_n, sodium_channel___E_Na, sodium_channel___i_Na,
     '    sodium_channel_h_gate___alpha_h, sodium_channel_h_gate___beta_
     '   h, sodium_channel_m_gate___alpha_m, sodium_channel_m_gate___bet
     '   a_m

         leakage_current___E_L = membrane___E_R-10.613D0
         leakage_current___i_L = leakage_current___g_L*(Y(1)-leakage_cur
     '   rent___E_L)

         IF ((t .GE. 10.0D0) .AND. (t .LE. 10.5D0)) THEN
            membrane___i_Stim = -20.0D0
         ELSE
            membrane___i_Stim = 0.0D0
         END IF

         sodium_channel___E_Na = membrane___E_R-115.0D0
         sodium_channel___i_Na = sodium_channel___g_Na*Y(4)**3.0D0*Y(3)*
     '   (Y(1)-sodium_channel___E_Na)
         potassium_channel___E_K = membrane___E_R+12.0D0
         potassium_channel___i_K = potassium_channel___g_K*Y(2)**4.0D0*(
     '   Y(1)-potassium_channel___E_K)
         DY(1) = -(-membrane___i_Stim+sodium_channel___i_Na+potassium_ch
     '   annel___i_K+leakage_current___i_L)/membrane___Cm
         potassium_channel_n_gate___alpha_n = 0.01D0*(Y(1)+10.0D0)/(DEXP
     '   ((Y(1)+10.0D0)/10.0D0)-1.0D0)
         potassium_channel_n_gate___beta_n = 0.125D0*DEXP(Y(1)/80.0D0)
         DY(2) = potassium_channel_n_gate___alpha_n*(1.0D0-Y(2))-potassi
     '   um_channel_n_gate___beta_n*Y(2)
         sodium_channel_h_gate___alpha_h = 0.07D0*DEXP(Y(1)/20.0D0)
         sodium_channel_h_gate___beta_h = 1.0D0/(DEXP((Y(1)+30.0D0)/10.0
     '   D0)+1.0D0)
         DY(3) = sodium_channel_h_gate___alpha_h*(1.0D0-Y(3))-sodium_cha
     '   nnel_h_gate___beta_h*Y(3)
         sodium_channel_m_gate___alpha_m = 0.1D0*(Y(1)+25.0D0)/(DEXP((Y(
     '   1)+25.0D0)/10.0D0)-1.0D0)
         sodium_channel_m_gate___beta_m = 4.0D0*DEXP(Y(1)/18.0D0)
         DY(4) = sodium_channel_m_gate___alpha_m*(1.0D0-Y(4))-sodium_cha
     '   nnel_m_gate___beta_m*Y(4)

         RETURN
      END

!=======================================================================
! End of file
!=======================================================================
