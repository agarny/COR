@ECHO OFF

IF NOT EXIST "C:\Program Files (x86)\" GOTO x86

SET ProgFilesDir=C:\Program Files (x86)

GOTO CONTINUE

:x86

SET ProgFilesDir=C:\Program Files

:CONTINUE

CALL "%ProgFilesDir%\Microsoft Visual Studio 9.0\VC\Bin\Vcvars32.bat"

TITLE Compiling the Math library using the Microsoft C++ compiler...

SET Files=..\Src\Common\OptMath.c

SET DLLName=MicrosoftMath.dll
SET Flags=/I..\Src\Extra\CVODE\include /w /c /Ox

CL %Flags% %Files%

CL /LD /Fe%DLLName% *.obj

DEL *.exp *.lib *.obj
