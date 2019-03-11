@ECHO OFF

TITLE Running COR tests...

IF NOT EXIST "C:\Program Files (x86)\" GOTO x86

SET ProgFilesDir=C:\Program Files (x86)

GOTO CONTINUE

:x86

SET ProgFilesDir=C:\Program Files

:CONTINUE

CALL "%ProgFilesDir%\CodeGear\RAD Studio\5.0\bin\rsvars.bat"

SET MSBuildBinPath=%WinDir%\Microsoft.NET\Framework\v2.0.50727

CALL %MSBuildBinPath%\MSBuild Tests.dproj

IF %ERRORLEVEL% == 0 GOTO TESTS

:PROBLEM

COLOR 67

GOTO CLEAN

:TESTS

Tests.exe

IF %ERRORLEVEL% == 0 GOTO SUCCESS

COLOR 47

GOTO CLEAN

:SUCCESS

COLOR 27

:CLEAN

CALL "-= Clean Up =-"

PAUSE

:DONE
