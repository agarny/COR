@ECHO OFF

IF NOT EXIST "C:\Program Files (x86)\" GOTO x86

SET ProgFilesDir=C:\Program Files (x86)

GOTO CONTINUE

:x86

SET ProgFilesDir=C:\Program Files

:CONTINUE

CALL "%ProgFilesDir%\Intel\Compiler\C++\10.1.021\IA32\Bin\ICLVars.bat"

TITLE Compiling the ODE library using the Intel C++ compiler...

SET Files=..\Src\ODE\ODEForwardEulerIntegrator.c ..\Src\ODE\ODE2ndOrderRungeKuttaIntegrator.c ..\Src\ODE\ODE4thOrderRungeKuttaIntegrator.c ..\Src\Extra\CVODE\src\cvode\cvode.c ..\Src\Extra\CVODE\src\cvode\cvode_band.c ..\Src\Extra\CVODE\src\cvode\cvode_bandpre.c ..\Src\Extra\CVODE\src\cvode\cvode_dense.c ..\Src\Extra\CVODE\src\cvode\cvode_diag.c ..\Src\Extra\CVODE\src\cvode\cvode_direct.c ..\Src\Extra\CVODE\src\cvode\cvode_io.c ..\Src\Extra\CVODE\src\cvode\cvode_spbcgs.c ..\Src\Extra\CVODE\src\cvode\cvode_spgmr.c ..\Src\Extra\CVODE\src\cvode\cvode_spils.c ..\Src\Extra\CVODE\src\cvode\cvode_sptfqmr.c ..\Src\Extra\CVODE\src\nvec_ser\nvector_serial.c ..\Src\Extra\CVODE\src\sundials\sundials_band.c ..\Src\Extra\CVODE\src\sundials\sundials_dense.c ..\Src\Extra\CVODE\src\sundials\sundials_direct.c ..\Src\Extra\CVODE\src\sundials\sundials_iterative.c ..\Src\Extra\CVODE\src\sundials\sundials_math.c ..\Src\Extra\CVODE\src\sundials\sundials_nvector.c ..\Src\Extra\CVODE\src\sundials\sundials_spbcgs.c ..\Src\Extra\CVODE\src\sundials\sundials_spgmr.c ..\Src\Extra\CVODE\src\sundials\sundials_sptfqmr.c

SET DLLName=IntelODE.dll
SET Flags=/I..\Src\Extra\CVODE\include /w /c /Ox /QaxKWNPT /Qipo-c /DNO_FPRINTF_OUTPUT

ICL %Flags% %Files%

ICL /LD /Fe%DLLName% *.obj

DEL *.exp *.lib *.obj
