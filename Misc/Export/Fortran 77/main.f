      PROGRAM MAIN

      INCLUDE "test.inc"

      DOUBLE PRECISION TMAX
      DOUBLE PRECISION H, HH
      DOUBLE PRECISION T, TT

      INTEGER I

      TMAX = 30.0D0   ! Simulation duration (s)

      H = 0.001D0   ! Time step (s)
      HH = 0.1D0    ! Output time step (s)

      T = 0.0D0    ! Time (s)
      TT = 0.0D0   ! Output time (s)

      CALL INIT   ! Initialise the model

      DO WHILE (T .LE. TMAX+H/2.0D0)
         ! Output the data, if necessary

         IF (T .GE. TT-H/2.0D0) THEN
            WRITE(*,100) T

            DO 10 I = 1, _NB_OF_STATE_VARIABLES_
               WRITE(*,200) Y(I)
 10         CONTINUE

            WRITE(*,*)

            TT = TT+HH
         ENDIF

         CALL COMPUTE(T)   ! Compute the model

         ! Integrate the model

         DO 20 I = 1, _NB_OF_STATE_VARIABLES_
            Y(I) = Y(I)+H*DY(I)
 20      CONTINUE

         T = T+H   ! Advance in time
      ENDDO

 100  FORMAT (G, $)
 200  FORMAT ('	', G, $)

      STOP

      END
