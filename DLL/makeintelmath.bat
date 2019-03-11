@ECHO OFF

IF NOT EXIST "C:\Program Files (x86)\" GOTO x86

SET ProgFilesDir=C:\Program Files (x86)

GOTO CONTINUE

:x86

SET ProgFilesDir=C:\Program Files

:CONTINUE

CALL "%ProgFilesDir%\Intel\Compiler\C++\10.1.021\IA32\Bin\ICLVars.bat"

TITLE Compiling the Math library using the Intel C++ compiler...

SET Files=..\Src\Common\OptMath.c

SET DLLName=IntelMath.dll
SET Flags=/I..\Src\Extra\CVODE\include /w /c /Ox /QaxKWNPT /Qipo-c

ICL %Flags% %Files%

ICL /LD /Fe%DLLName% *.obj

DEL *.exp *.lib *.obj
