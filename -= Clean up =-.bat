@TITLE Cleaning up the COR repository...

@DEL /F Tests.exe 1> NUL 2> NUL
@DEL /F /Q /S *.~* *.dcp *.dcu *.drc *.dsk *.identcache *.jdbg *.local *.log *.map 1> NUL 2> NUL
@DEL /F /Q /S /AH desktop.ini 1> NUL 2> NUL
@FOR /F "DELIMS=;" %%D IN ('DIR /AD /B /S ^| GREP __history') DO @DEL /F /Q /S "%%D" 1> NUL 2> NUL && @RD "%%D" 1> NUL 2> NUL

SET ERRORLEVEL=0
