#-------------------------------------------------------------------------------
# Script for installing COR
#
# by Alan Garny
#    alan.garny@dpag.ox.ac.uk
#    http://COR.physiol.ox.ac.uk/
#
# © Copyright 2002-2009
#-------------------------------------------------------------------------------
# Note: this installation script works with the free COR from Nullsoft, Inc.
#       (http://www.nullsoft.com/free/nsis/)
#-------------------------------------------------------------------------------
# Date of Creation: 27/04/2002
#
# Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Source code or not?
#-------------------------------------------------------------------------------

!define SOURCE_CODE

#-------------------------------------------------------------------------------
# Settings for the modern user interface
#-------------------------------------------------------------------------------

!include "MUI.nsh"

#-------------------------------------------------------------------------------
# General settings
#-------------------------------------------------------------------------------

Name "COR 0.9"
OutFile "COR09.exe"

SetCompressor /SOLID lzma

BrandingText "COR 0.9"

!define MUI_HEADERIMAGE
!define MUI_ABORTWARNING

!define MUI_COMPONENTSPAGE_SMALLDESC

!define SHCNE_ASSOCCHANGED 0x08000000
!define SHCNF_IDLIST 0

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "License.txt"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH

!insertmacro MUI_LANGUAGE "English"

#-------------------------------------------------------------------------------
# Default installation directory, as well as the registry key to check for the
# installation directory (so if the user installs COR again, it will overwrite
# the old one automatically)
#-------------------------------------------------------------------------------

InstallDir "$PROGRAMFILES\COR"
InstallDirRegKey HKLM "Software\COR" "InstallDir"

#-------------------------------------------------------------------------------
# Different types of installation
#-------------------------------------------------------------------------------

InstType "Normal"
InstType "Full"

#-------------------------------------------------------------------------------
# Some general settings
#-------------------------------------------------------------------------------

ShowInstDetails show
ShowUninstDetails show
SetOverwrite on
SetDateSave on

#-------------------------------------------------------------------------------
# On initialisation of the installer
#-------------------------------------------------------------------------------

Function .onInit
   # Check the user's privileges

   ClearErrors

   UserInfo::GetName

   IfErrors Win9x

   Pop $0

   UserInfo::GetAccountType

   Pop $1

   StrCmp $1 "Admin" Continue

   MessageBox MB_OK "Sorry, but the COR installer requires administrative privileges to run."
   Abort

Win9x:

   MessageBox MB_OK "Sorry, but COR is not supported on Win9x systems."
   Abort

Continue:

   SetShellVarContext all
FunctionEnd

#-------------------------------------------------------------------------------
# On initialisation of the uninstaller
#-------------------------------------------------------------------------------

Function un.onInit
   # Check the user's privileges

   ClearErrors

   UserInfo::GetName

   IfErrors Win9x

   Pop $0

   UserInfo::GetAccountType

   Pop $1

   StrCmp $1 "Admin" Continue

   MessageBox MB_OK "Sorry, but the COR uninstaller requires administrative privileges to run."
   Abort

Win9x:

   MessageBox MB_OK "Sorry, but COR is not supported on Win9x systems."
   Abort

Continue:

   SetShellVarContext all
FunctionEnd

#-------------------------------------------------------------------------------
# COR
#-------------------------------------------------------------------------------

Section "COR Environment (required)" COREnvironment
   SectionIn 1 2 RO

   # Delete the keys in the registry, just in case...

   DeleteRegKey HKLM "Software\COR"
   DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\COR"

   # Delete the user's registry keys, just in case...

   DeleteRegKey HKCU "Software\COR"

   # Install the COR application

   Delete "$INSTDIR\COR.exe"
   Delete "$INSTDIR\Uninstall.exe"

   SetOutPath "$INSTDIR"

   File "..\COR.exe"

   WriteRegStr HKLM "Software\COR" "InstallDir" "$INSTDIR"

   WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\COR" "DisplayName" "COR (remove only)"
   WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\COR" "UninstallString" '"$INSTDIR\uninstall.exe"'

   WriteUninstaller "Uninstall.exe"
SectionEnd

#-------------------------------------------------------------------------------
# Shell Extensions
#-------------------------------------------------------------------------------

Section "COR Shell Extensions" CORShellExtensions
   SectionIn 1 2

   # ".cellml" extension

   ReadRegStr $1 HKCR ".cellml" ""

   StrCmp $1 "" Continue
   StrCmp $1 "CellMLFile" Continue

   WriteRegStr HKCR ".cellml" "BackupVal" $1

Continue:

   WriteRegStr HKCR ".cellml" "" "CellMLFile"

   WriteRegStr HKCR "CellMLFile" "" "CellML File"

   WriteRegStr HKCR "CellMLFile\DefaultIcon" "" "$INSTDIR\COR.exe,1"

   WriteRegStr HKCR "CellMLFile\Shell\Open" "" "Open with COR"
   WriteRegStr HKCR "CellMLFile\Shell\Open\Command" "" '"$INSTDIR\COR.exe" /Open "%1"'
   WriteRegStr HKCR "CellMLFile\Shell\Open\DDEExec" "" '/Open "%1"'
   WriteRegStr HKCR "CellMLFile\Shell\Open\DDEExec\Application" "" "COR"
   WriteRegStr HKCR "CellMLFile\Shell\Open\DDEExec\Topic" "" "DDEServerConv"

   System::Call "Shell32::SHChangeNotify(i ${SHCNE_ASSOCCHANGED}, i ${SHCNF_IDLIST}, i 0, i 0)"
SectionEnd

#-------------------------------------------------------------------------------
# Start Menu Group
#-------------------------------------------------------------------------------

Section "Start Menu Group" StartMenuGroup
   SectionIn 1 2

   SetOutPath "$SMPROGRAMS\COR"

   CreateShortCut "$SMPROGRAMS\COR\Home Page.lnk" "http://COR.physiol.ox.ac.uk/"
   CreateShortCut "$SMPROGRAMS\COR\Uninstall.lnk" "$INSTDIR\Uninstall.exe" "" "$INSTDIR\Uninstall.exe" 0
   CreateShortCut "$SMPROGRAMS\COR\COR.lnk" "$INSTDIR\COR.exe" "" "$INSTDIR\COR.exe" 0
SectionEnd

#-------------------------------------------------------------------------------
# Desktop Shortcut
#-------------------------------------------------------------------------------

Section "Desktop Shortcut" DesktopShortcut
   SectionIn 2 

   SetOutPath $INSTDIR

   CreateShortCut "$DESKTOP\COR.lnk" "$INSTDIR\COR.exe"
SectionEnd

#-------------------------------------------------------------------------------
# Source Code
#-------------------------------------------------------------------------------

!ifdef SOURCE_CODE
   Section "Source Code" SourceCode
      SectionIn 2

      RMDir /r "$INSTDIR\Source"

      SetOutPath "$INSTDIR\Source"

      File "..\-= Clean up =-.bat"

      File "..\-= Run Tests =-.bat"

      File "..\COR.groupproj"

      File "..\COR.dpr"
      File "..\COR.dproj"
      File "..\COR.res"

      File "..\COR.xml"

      File "..\Tests.dpr"
      File "..\Tests.dproj"
      File "..\Tests.res"

      File "..\VCLs.dproj"
      File "..\VCLs.dpk"
      File "..\VCLs.res"

      File "..\ReadMe.txt"

      File "..\FastMM_FullDebugMode.dll"

      SetOutPath "$INSTDIR\Source\DLL"

      File "..\DLL\IntelMath.dll"
      File "..\DLL\IntelODE.dll"
      File "..\DLL\lmakeintelmath.bat"
      File "..\DLL\lmakeintelode.bat"
      File "..\DLL\lmakemicrosoftmath.bat"
      File "..\DLL\lmakemicrosoftode.bat"
      File "..\DLL\makeintelmath.bat"
      File "..\DLL\makeintelode.bat"
      File "..\DLL\makemicrosoftmath.bat"
      File "..\DLL\makemicrosoftode.bat"
      File "..\DLL\MicrosoftMath.dll"
      File "..\DLL\MicrosoftODE.dll"
      File "..\DLL\ReadMe.txt"

      SetOutPath "$INSTDIR\Source\Help"

      File "..\Help\COR.chm"
      File "..\Help\COR.hnd"
      File "..\Help\CORHelp.pas"

      SetOutPath "$INSTDIR\Source\Install"

      File "COR.nsi"
      File "License.txt"
      File "ReadMe.txt"

      SetOutPath "$INSTDIR\Source\Misc\Export\C"

      File "..\Misc\Export\C\main.c"
      File "..\Misc\Export\C\makec"
      File "..\Misc\Export\C\test.c"
      File "..\Misc\Export\C\test.h"

      SetOutPath "$INSTDIR\Source\Misc\Export\C++"

      File "..\Misc\Export\C++\main.cpp"
      File "..\Misc\Export\C++\makecpp"
      File "..\Misc\Export\C++\test.cpp"
      File "..\Misc\Export\C++\test.hpp"

      SetOutPath "$INSTDIR\Source\Misc\Export\Delphi for Win32"

      File "..\Misc\Export\Delphi for Win32\main.dpr"
      File "..\Misc\Export\Delphi for Win32\test.pas"

      SetOutPath "$INSTDIR\Source\Misc\Export\Fortran 77"

      File "..\Misc\Export\Fortran 77\main.f"
      File "..\Misc\Export\Fortran 77\makef"
      File "..\Misc\Export\Fortran 77\test.f"
      File "..\Misc\Export\Fortran 77\test.inc"

      SetOutPath "$INSTDIR\Source\Misc\Export\Java"

      File "..\Misc\Export\Java\main.java"
      File "..\Misc\Export\Java\makej"
      File "..\Misc\Export\Java\test.java"

      SetOutPath "$INSTDIR\Source\Misc\Export\MATLAB"

      File "..\Misc\Export\MATLAB\main.m"
      File "..\Misc\Export\MATLAB\test.m"

      SetOutPath "$INSTDIR\Source\Misc\Export\Pascal"

      File "..\Misc\Export\Pascal\main.dpr"
      File "..\Misc\Export\Pascal\test.pas"

      SetOutPath "$INSTDIR\Source\Misc\File Extensions Update"

      File "..\Misc\File Extensions Update\CORFileExtensionsUpdate.dpr"
      File "..\Misc\File Extensions Update\CORFileExtensionsUpdate.dproj"
      File "..\Misc\File Extensions Update\CORFileExtensionsUpdate.exe"
      File "..\Misc\File Extensions Update\CORFileExtensionsUpdate.res"
      File "..\Misc\File Extensions Update\Main.dfm"
      File "..\Misc\File Extensions Update\Main.pas"

      SetOutPath "$INSTDIR\Source\Models"

      File "..\Models\aslanidi_model_2009.cellml"
      File "..\Models\beeler_reuter_model_1977.cellml"
      File "..\Models\bondarenko_model_2004_apex.cellml"
      File "..\Models\bondarenko_model_2004_septum.cellml"
      File "..\Models\bueno_model_2007.cellml"
      File "..\Models\chay_model_1997.cellml"
      File "..\Models\courtemanche_ramirez_nattel_model_1998.cellml"
      File "..\Models\demir_model_1994.cellml"
      File "..\Models\demir_model_1999.cellml"
      File "..\Models\difrancesco_noble_model_1985.cellml"
      File "..\Models\dokos_model_1996.cellml"
      File "..\Models\earm_noble_model_1990.cellml"
      File "..\Models\endresen_model_1997.cellml"
      File "..\Models\espinosa_model_1998_hypertrophy.cellml"
      File "..\Models\espinosa_model_1998_normal.cellml"
      File "..\Models\fenton_karma_model_1998_BR.cellml"
      File "..\Models\fenton_karma_model_1998_GP.cellml"
      File "..\Models\fenton_karma_model_1998_MBR.cellml"
      File "..\Models\fenton_karma_model_1998_MLR_1.cellml"
      File "..\Models\fink_noble_giles_model_2008.cellml"
      File "..\Models\fitzhugh_nagumo_model_1961.cellml"
      File "..\Models\fox_model_2002.cellml"
      File "..\Models\grandi_pasqualini_bers_2010.cellml"
      File "..\Models\hilgemann_noble_model_1987.cellml"
      File "..\Models\hodgkin_huxley_squid_axon_model_1952_modified.cellml"
      File "..\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml"
      File "..\Models\hund_rudy_model_2004.cellml"
      File "..\Models\inada_AN_model_2009.cellml"
      File "..\Models\inada_N_model_2009.cellml"
      File "..\Models\inada_NH_model_2009.cellml"
      File "..\Models\iribe_model_2006.cellml"
      File "..\Models\iyer_model_2004.cellml"
      File "..\Models\iyer_model_2007.cellml"
      File "..\Models\jafri_rice_winslow_model_1998.cellml"
      File "..\Models\katsnelson_model_2004_dimensional.cellml"
      File "..\Models\katsnelson_model_2004_dimensionless.cellml"
      File "..\Models\kurata_model_2002.cellml"
      File "..\Models\lindblad_atrial_model_1996.cellml"
      File "..\Models\livshitz_rudy_model_2007.cellml"
      File "..\Models\LR_dynamic_model_2000.cellml"
      File "..\Models\luo_rudy_I_model_1991.cellml"
      File "..\Models\mahajan_shiferaw_model_2008.cellml"
      File "..\Models\maleckar_greenstein_trayanova_giles_model_2008.cellml"
      File "..\Models\matsuoka_model_2003.cellml"
      File "..\Models\mcallister_noble_tsien_model_1975_A.cellml"
      File "..\Models\mcallister_noble_tsien_model_1975_B.cellml"
      File "..\Models\noble_difrancesco_denyer_model_1989.cellml"
      File "..\Models\noble_model_1962.cellml"
      File "..\Models\noble_model_1991.cellml"
      File "..\Models\noble_model_1998.cellml"
      File "..\Models\noble_model_1998_extended.cellml"
      File "..\Models\noble_model_1998_stretch.cellml"
      File "..\Models\noble_model_2001.cellml"
      File "..\Models\noble_noble_SAN_model_1984.cellml"
      File "..\Models\noble_SAN_model_1989.cellml"
      File "..\Models\nygren_atrial_model_1998.cellml"
      File "..\Models\pandit_model_2001_endo.cellml"
      File "..\Models\pandit_model_2001_epi.cellml"
      File "..\Models\pasek_simurda_orchard_christe_model_2008.cellml"
      File "..\Models\priebe_beuckelmann_model_1998.cellml"
      File "..\Models\sakmann_model_2000_endo.cellml"
      File "..\Models\sakmann_model_2000_epi.cellml"
      File "..\Models\sakmann_model_2000_M.cellml"
      File "..\Models\sarai_model_2003.cellml"
      File "..\Models\shannon_wang_puglisi_weber_bers_model_2004.cellml"
      File "..\Models\stewart_zhang_model_2008.cellml"
      File "..\Models\ten_tusscher_model_2004_endo.cellml"
      File "..\Models\ten_tusscher_model_2004_epi.cellml"
      File "..\Models\ten_tusscher_model_2004_M.cellml"
      File "..\Models\ten_tusscher_model_2006_endo.cellml"
      File "..\Models\ten_tusscher_model_2006_epi.cellml"
      File "..\Models\ten_tusscher_model_2006_M.cellml"
      File "..\Models\van_der_pol_model_1928.cellml"
      File "..\Models\viswanathan_model_1999_endo.cellml"
      File "..\Models\viswanathan_model_1999_epi.cellml"
      File "..\Models\viswanathan_model_1999_M.cellml"
      File "..\Models\winslow_model_1999.cellml"
      File "..\Models\yanagihara_model_1980.cellml"
      File "..\Models\zhang_SAN_model_2000_0D_capable.cellml"
      File "..\Models\zhang_SAN_model_2000_1D_capable.cellml"
      File "..\Models\zhang_SAN_model_2000_all.cellml"
      File "..\Models\zhang_SAN_model_2000_published.cellml"

      SetFileAttributes "$INSTDIR\Source\Models\aslanidi_model_2009.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\beeler_reuter_model_1977.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\bondarenko_model_2004_apex.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\bondarenko_model_2004_septum.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\bueno_model_2007.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\chay_model_1997.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\courtemanche_ramirez_nattel_model_1998.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\demir_model_1994.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\demir_model_1999.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\difrancesco_noble_model_1985.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\dokos_model_1996.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\earm_noble_model_1990.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\endresen_model_1997.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\espinosa_model_1998_hypertrophy.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\espinosa_model_1998_normal.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\fenton_karma_model_1998_BR.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\fenton_karma_model_1998_GP.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\fenton_karma_model_1998_MBR.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\fenton_karma_model_1998_MLR_1.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\fink_noble_giles_model_2008.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\fitzhugh_nagumo_model_1961.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\fox_model_2002.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\grandi_pasqualini_bers_2010.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\hilgemann_noble_model_1987.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\hodgkin_huxley_squid_axon_model_1952_modified.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\hund_rudy_model_2004.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\inada_AN_model_2009.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\inada_N_model_2009.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\inada_NH_model_2009.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\iribe_model_2006.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\iyer_model_2004.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\iyer_model_2007.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\jafri_rice_winslow_model_1998.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\katsnelson_model_2004_dimensional.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\katsnelson_model_2004_dimensionless.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\kurata_model_2002.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\lindblad_atrial_model_1996.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\livshitz_rudy_model_2007.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\LR_dynamic_model_2000.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\luo_rudy_I_model_1991.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\mahajan_shiferaw_model_2008.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\maleckar_greenstein_trayanova_giles_model_2008.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\matsuoka_model_2003.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\mcallister_noble_tsien_model_1975_A.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\mcallister_noble_tsien_model_1975_B.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_difrancesco_denyer_model_1989.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_model_1962.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_model_1991.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_model_1998.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_model_1998_extended.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_model_1998_stretch.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_model_2001.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_noble_SAN_model_1984.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\noble_SAN_model_1989.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\nygren_atrial_model_1998.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\pandit_model_2001_endo.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\pandit_model_2001_epi.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\pasek_simurda_orchard_christe_model_2008.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\priebe_beuckelmann_model_1998.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\sakmann_model_2000_endo.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\sakmann_model_2000_epi.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\sakmann_model_2000_M.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\sarai_model_2003.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\shannon_wang_puglisi_weber_bers_model_2004.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\stewart_zhang_model_2008.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\ten_tusscher_model_2004_endo.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\ten_tusscher_model_2004_epi.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\ten_tusscher_model_2004_M.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\ten_tusscher_model_2006_endo.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\ten_tusscher_model_2006_epi.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\ten_tusscher_model_2006_M.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\van_der_pol_model_1928.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\viswanathan_model_1999_endo.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\viswanathan_model_1999_epi.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\viswanathan_model_1999_M.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\winslow_model_1999.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\yanagihara_model_1980.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\zhang_SAN_model_2000_0D_capable.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\zhang_SAN_model_2000_1D_capable.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\zhang_SAN_model_2000_all.cellml" READONLY
      SetFileAttributes "$INSTDIR\Source\Models\zhang_SAN_model_2000_published.cellml" READONLY

      SetOutPath "$INSTDIR\Source\Models\Tests"

      File "..\Models\Tests\both_state_variable_and_computed_variable_1.cellml"
      File "..\Models\Tests\both_state_variable_and_computed_variable_2.cellml"
      File "..\Models\Tests\both_state_variable_and_computed_variable_3.cellml"
      File "..\Models\Tests\both_state_variable_and_computed_variable_4.cellml"
      File "..\Models\Tests\exponent.cellml"
      File "..\Models\Tests\functions.cellml"
      File "..\Models\Tests\increment.cellml"
      File "..\Models\Tests\machine_code.cellml"
      File "..\Models\Tests\same_variable_name.cellml"
      File "..\Models\Tests\units_and_base_units.cellml"

      SetOutPath "$INSTDIR\Source\Res"

      File "..\Res\App.ico"
      File "..\Res\CellML.ico"
      File "..\Res\Logo.bmp"
      File "..\Res\Logo.pptx"
      File "..\Res\Resources.rc"
      File "..\Res\Resources.res"

      SetOutPath "$INSTDIR\Source\Src\CellML"

      File "..\Src\CellML\CellMLAPI.pas"
      File "..\Src\CellML\CellMLAPIEngine.pas"
      File "..\Src\CellML\CellMLAPIExportEngine.pas"
      File "..\Src\CellML\CellMLAPIToCellMLASCIIEngine.pas"
      File "..\Src\CellML\CellMLAPIToCellMLFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToCFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToCMCFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToCPPFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToDelphiForWin32FileEngine.pas"
      File "..\Src\CellML\CellMLAPIToFortran77FileEngine.pas"
      File "..\Src\CellML\CellMLAPIToJavaFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToMATLABFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToMCEngine.pas"
      File "..\Src\CellML\CellMLAPIToMSWordFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToPascalFileEngine.pas"
      File "..\Src\CellML\CellMLAPIToTeXFileEngine.pas"
      File "..\Src\CellML\CellMLASCIIToCellMLAPIEngine.pas"
      File "..\Src\CellML\CellMLFileToCellMLAPIEngine.pas"
      File "..\Src\CellML\CellMLScannerEngine.pas"

      SetOutPath "$INSTDIR\Source\Src\CellML\Tests"

      File "..\Src\CellML\Tests\CellMLAPITests.pas"

      SetOutPath "$INSTDIR\Source\Src\Common"

      File "..\Src\Common\Cell.pas"
      File "..\Src\Common\Common.pas"
      File "..\Src\Common\CORCommon.pas"
      File "..\Src\Common\Engine.pas"
      File "..\Src\Common\OptMath.c"
      File "..\Src\Common\OptMath.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\Delphi\VCL"

      File "..\Src\Extra\Delphi\VCL\ComCtrls.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\CVODE"

      File "..\Src\Extra\CVODE\LICENSE"
      File "..\Src\Extra\CVODE\README"

      SetOutPath "$INSTDIR\Source\Src\Extra\CVODE\include\cvode"

      File "..\Src\Extra\CVODE\include\cvode\cvode.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_band.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_bandpre.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_dense.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_diag.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_direct.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_spbcgs.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_spgmr.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_spils.h"
      File "..\Src\Extra\CVODE\include\cvode\cvode_sptfqmr.h"

      SetOutPath "$INSTDIR\Source\Src\Extra\CVODE\include\nvector"

      File "..\Src\Extra\CVODE\include\nvector\nvector_serial.h"

      SetOutPath "$INSTDIR\Source\Src\Extra\CVODE\include\sundials"

      File "..\Src\Extra\CVODE\include\sundials\sundials_band.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_config.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_dense.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_direct.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_iterative.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_math.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_nvector.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_spbcgs.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_spgmr.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_sptfqmr.h"
      File "..\Src\Extra\CVODE\include\sundials\sundials_types.h"

      SetOutPath "$INSTDIR\Source\Src\Extra\CVODE\src\cvode"

      File "..\Src\Extra\CVODE\src\cvode\cvode.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_band.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_bandpre.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_bandpre_impl.h"
      File "..\Src\Extra\CVODE\src\cvode\cvode_dense.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_diag.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_diag_impl.h"
      File "..\Src\Extra\CVODE\src\cvode\cvode_direct.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_direct_impl.h"
      File "..\Src\Extra\CVODE\src\cvode\cvode_impl.h"
      File "..\Src\Extra\CVODE\src\cvode\cvode_io.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_spbcgs.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_spgmr.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_spils.c"
      File "..\Src\Extra\CVODE\src\cvode\cvode_spils_impl.h"
      File "..\Src\Extra\CVODE\src\cvode\cvode_sptfqmr.c"
      File "..\Src\Extra\CVODE\src\cvode\LICENSE"
      File "..\Src\Extra\CVODE\src\cvode\README"

      SetOutPath "$INSTDIR\Source\Src\Extra\CVODE\src\nvec_ser"

      File "..\Src\Extra\CVODE\src\nvec_ser\nvector_serial.c"
      File "..\Src\Extra\CVODE\src\nvec_ser\README"

      SetOutPath "$INSTDIR\Source\Src\Extra\CVODE\src\sundials"

      File "..\Src\Extra\CVODE\src\sundials\README"
      File "..\Src\Extra\CVODE\src\sundials\sundials_band.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_dense.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_direct.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_iterative.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_math.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_nvector.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_spbcgs.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_spgmr.c"
      File "..\Src\Extra\CVODE\src\sundials\sundials_sptfqmr.c"

      SetOutPath "$INSTDIR\Source\Src\Extra\DeCAL"

      File "..\Src\Extra\DeCAL\DeCAL.pas"
      File "..\Src\Extra\DeCAL\mwFixedRecSort.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\FastCode"

      File "..\Src\Extra\FastCode\AnsiStringReplaceJOHIA32Unit12.pas"
      File "..\Src\Extra\FastCode\AnsiStringReplaceJOHPASUnit12.pas"
      File "..\Src\Extra\FastCode\D7PosEx.inc"
      File "..\Src\Extra\FastCode\FastCode.inc"
      File "..\Src\Extra\FastCode\FastCode.pas"
      File "..\Src\Extra\FastCode\FastcodeAnsiStringReplaceUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeCompareMemUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeCompareStrUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeCompareTextUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeCPUID.pas"
      File "..\Src\Extra\FastCode\FastcodeFillCharUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeLowerCaseUnit.pas"
      File "..\Src\Extra\FastCode\FastcodePatch.pas"
      File "..\Src\Extra\FastCode\FastcodePosExUnit.pas"
      File "..\Src\Extra\FastCode\FastcodePosUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeStrCompUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeStrCopyUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeStrICompUnit.pas"
      File "..\Src\Extra\FastCode\FastCodeStrLenUnit.pas"
      File "..\Src\Extra\FastCode\FastcodeStrToInt32Unit.pas"
      File "..\Src\Extra\FastCode\FastcodeUpperCaseUnit.pas"
      File "..\Src\Extra\FastCode\Readme.txt"
      File "..\Src\Extra\FastCode\Version.History.txt"

      SetOutPath "$INSTDIR\Source\Src\Extra\FastCode\Non.RTL"

      File "..\Src\Extra\FastCode\Non.RTL\FastCodeCharPosUnit.pas"
      File "..\Src\Extra\FastCode\Non.RTL\FastcodeGCDUnit.pas"
      File "..\Src\Extra\FastCode\Non.RTL\FastcodeMaxIntUnit.pas"
      File "..\Src\Extra\FastCode\Non.RTL\FastcodePosIExUnit.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\FastMM4"

      File "..\Src\Extra\FastMM4\FastMM4.pas"
      File "..\Src\Extra\FastMM4\FastMM4_FAQ.txt"
      File "..\Src\Extra\FastMM4\FastMM4_Readme.txt"
      File "..\Src\Extra\FastMM4\FastMM4Messages.pas"
      File "..\Src\Extra\FastMM4\FastMM4Options.inc"

      SetOutPath "$INSTDIR\Source\Src\Extra\FastMM4\FullDebugMode DLL"

      File "..\Src\Extra\FastMM4\FullDebugMode DLL\FastMM_FullDebugMode.dpr"
      File "..\Src\Extra\FastMM4\FullDebugMode DLL\FastMM_FullDebugMode.dproj"
      File "..\Src\Extra\FastMM4\FullDebugMode DLL\FastMM_FullDebugMode.res"

      SetOutPath "$INSTDIR\Source\Src\Extra\FastMove"

      File "..\Src\Extra\FastMove\FastMove.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\JCL\Docs"

      File "..\Src\Extra\JCL\Docs\MPL-1.1.txt"
      File "..\Src\Extra\JCL\Docs\MPL FAQ.html"
      File "..\Src\Extra\JCL\Docs\Readme.txt"

      SetOutPath "$INSTDIR\Source\Src\Extra\JCL\Experts\Common"

      File "..\Src\Extra\JCL\Experts\Common\JclImages.res"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaActionConfigureSheet.dfm"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaActionConfigureSheet.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaConfigurationForm.dfm"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaConfigurationForm.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaConsts.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaExceptionForm.dfm"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaExceptionForm.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaResources.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaUnitVersioningSheet.dfm"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaUnitVersioningSheet.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaUtils.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaWizardForm.dfm"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaWizardForm.pas"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaWizardFrame.dfm"
      File "..\Src\Extra\JCL\Experts\Common\JclOtaWizardFrame.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\JCL\Experts\Debug\Converter"

      File "..\Src\Extra\JCL\Experts\Debug\Converter\JclDebugIdeConfigFrame.dfm"
      File "..\Src\Extra\JCL\Experts\Debug\Converter\JclDebugIdeConfigFrame.pas"
      File "..\Src\Extra\JCL\Experts\Debug\Converter\JclDebugIdeIcon.res"
      File "..\Src\Extra\JCL\Experts\Debug\Converter\JclDebugIdeImpl.pas"
      File "..\Src\Extra\JCL\Experts\Debug\Converter\JclDebugIdeResult.dfm"
      File "..\Src\Extra\JCL\Experts\Debug\Converter\JclDebugIdeResult.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\JCL\Experts\Debug\Dialog"

      File "..\Src\Extra\JCL\Experts\Debug\Dialog\ExceptDlgMail.dfm"
      File "..\Src\Extra\JCL\Experts\Debug\Dialog\ExceptDlgMail.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\JCL\Source\Common"

      File "..\Src\Extra\JCL\Source\Common\Jcl8087.pas"
      File "..\Src\Extra\JCL\Source\Common\JclAnsiStrings.pas"
      File "..\Src\Extra\JCL\Source\Common\JclBase.pas"
      File "..\Src\Extra\JCL\Source\Common\JclBorlandTools.pas"
      File "..\Src\Extra\JCL\Source\Common\JclCharsets.pas"
      File "..\Src\Extra\JCL\Source\Common\JclDateTime.pas"
      File "..\Src\Extra\JCL\Source\Common\JclFileUtils.pas"
      File "..\Src\Extra\JCL\Source\Common\JclIniFiles.pas"
      File "..\Src\Extra\JCL\Source\Common\JclLogic.pas"
      File "..\Src\Extra\JCL\Source\Common\JclMath.pas"
      File "..\Src\Extra\JCL\Source\Common\JclMime.pas"
      File "..\Src\Extra\JCL\Source\Common\JclResources.pas"
      File "..\Src\Extra\JCL\Source\Common\JclRTTI.pas"
      File "..\Src\Extra\JCL\Source\Common\JclSimpleXml.pas"
      File "..\Src\Extra\JCL\Source\Common\JclStreams.pas"
      File "..\Src\Extra\JCL\Source\Common\JclStringConversions.pas"
      File "..\Src\Extra\JCL\Source\Common\JclStrings.pas"
      File "..\Src\Extra\JCL\Source\Common\JclSynch.pas"
      File "..\Src\Extra\JCL\Source\Common\JclSysInfo.pas"
      File "..\Src\Extra\JCL\Source\Common\JclSysUtils.pas"
      File "..\Src\Extra\JCL\Source\Common\JclUnicode.pas"
      File "..\Src\Extra\JCL\Source\Common\JclUnicode.res"
      File "..\Src\Extra\JCL\Source\Common\JclUnitVersioning.pas"
      File "..\Src\Extra\JCL\Source\Common\JclUnitVersioningProviders.pas"
      File "..\Src\Extra\JCL\Source\Common\JclWideStrings.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\JCL\Source\Include"

      File "..\Src\Extra\JCL\Source\Include\crossplatform.inc"
      File "..\Src\Extra\JCL\Source\Include\jcl.inc"
      File "..\Src\Extra\JCL\Source\Include\jcld11.inc"
      File "..\Src\Extra\JCL\Source\Include\jedi.inc"
      File "..\Src\Extra\JCL\Source\Include\windowsonly.inc"

      SetOutPath "$INSTDIR\Source\Src\Extra\JCL\Source\Windows"

      File "..\Src\Extra\JCL\Source\Windows\JclAppInst.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclConsole.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclDebug.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclHookExcept.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclMapi.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclPeImage.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclRegistry.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclSecurity.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclShell.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclTD32.pas"
      File "..\Src\Extra\JCL\Source\Windows\JclWin32.pas"
      File "..\Src\Extra\JCL\Source\Windows\MSHelpServices_TLB.pas"
      File "..\Src\Extra\JCL\Source\Windows\Snmp.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\JVCL\Common"

      File "..\Src\Extra\JVCL\Common\jvcl.inc"
      File "..\Src\Extra\JVCL\Common\jvcld11.inc"

      SetOutPath "$INSTDIR\Source\Src\Extra\JVCL\Help"

      File "..\Src\Extra\JVCL\Help\License.htm"
      File "..\Src\Extra\JVCL\Help\MPL-1.1.html"

      SetOutPath "$INSTDIR\Source\Src\Extra\JVCL\Resources"

      File "..\Src\Extra\JVCL\Resources\JvConsts.res"
      File "..\Src\Extra\JVCL\Resources\JvToolEdit.res"

      SetOutPath "$INSTDIR\Source\Src\Extra\JVCL\Run"

      File "..\Src\Extra\JVCL\Run\JvAppStorage.pas"
      File "..\Src\Extra\JVCL\Run\JvBaseDlg.pas"
      File "..\Src\Extra\JVCL\Run\JvBrowseFolder.pas"
      File "..\Src\Extra\JVCL\Run\JvButton.pas"
      File "..\Src\Extra\JVCL\Run\JvCaret.pas"
      File "..\Src\Extra\JVCL\Run\JVCLVer.pas"
      File "..\Src\Extra\JVCL\Run\JvComponent.pas"
      File "..\Src\Extra\JVCL\Run\JvComponentBase.pas"
      File "..\Src\Extra\JVCL\Run\JvConsts.pas"
      File "..\Src\Extra\JVCL\Run\JvDataSourceIntf.pas"
      File "..\Src\Extra\JVCL\Run\JvEdit.pas"
      File "..\Src\Extra\JVCL\Run\JVExControls.pas"
      File "..\Src\Extra\JVCL\Run\JvExForms.pas"
      File "..\Src\Extra\JVCL\Run\JvExGrids.pas"
      File "..\Src\Extra\JVCL\Run\JvExMask.pas"
      File "..\Src\Extra\JVCL\Run\JvExStdCtrls.pas"
      File "..\Src\Extra\JVCL\Run\JvFixedEditPopup.pas"
      File "..\Src\Extra\JVCL\Run\JvHotTrackPersistent.pas"
      File "..\Src\Extra\JVCL\Run\JvJCLUtils.pas"
      File "..\Src\Extra\JVCL\Run\JvJVCLUtils.pas"
      File "..\Src\Extra\JVCL\Run\JvLinkLabel.pas"
      File "..\Src\Extra\JVCL\Run\JvLinkLabelParser.pas"
      File "..\Src\Extra\JVCL\Run\JvLinkLabelRenderer.pas"
      File "..\Src\Extra\JVCL\Run\JvLinkLabelTextHandler.pas"
      File "..\Src\Extra\JVCL\Run\JvLinkLabelTools.pas"
      File "..\Src\Extra\JVCL\Run\JvLinkLabelTree.pas"
      File "..\Src\Extra\JVCL\Run\JvMaxPixel.pas"
      File "..\Src\Extra\JVCL\Run\JvPickDate.pas"
      File "..\Src\Extra\JVCL\Run\JvProgressUtils.pas"
      File "..\Src\Extra\JVCL\Run\JvPropertyStore.pas"
      File "..\Src\Extra\JVCL\Run\JvPropertyStoreEditorIntf.pas"
      File "..\Src\Extra\JVCL\Run\JvResources.pas"
      File "..\Src\Extra\JVCL\Run\JvSpeedButton.pas"
      File "..\Src\Extra\JVCL\Run\JvStrings.pas"
      File "..\Src\Extra\JVCL\Run\JvThemes.pas"
      File "..\Src\Extra\JVCL\Run\JvToolEdit.pas"
      File "..\Src\Extra\JVCL\Run\JvTranslateString.pas"
      File "..\Src\Extra\JVCL\Run\JvTypes.pas"
      File "..\Src\Extra\JVCL\Run\JvValidateEdit.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\OpenGL"

      File "..\Src\Extra\OpenGL\OpenGL_SG.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\SynEdit"

      File "..\Src\Extra\SynEdit\ReadMe.txt"

      SetOutPath "$INSTDIR\Source\Src\Extra\SynEdit\Source"

      File "..\Src\Extra\SynEdit\Source\SynAutoCorrect.pas"
      File "..\Src\Extra\SynEdit\Source\SynAutoCorrectEditor.dfm"
      File "..\Src\Extra\SynEdit\Source\SynAutoCorrectEditor.pas"
      File "..\Src\Extra\SynEdit\Source\SynCompletionProposal.pas"
      File "..\Src\Extra\SynEdit\Source\SynEdit.inc"
      File "..\Src\Extra\SynEdit\Source\SynEdit.pas"
      File "..\Src\Extra\SynEdit\Source\SynEdit.res"
      File "..\Src\Extra\SynEdit\Source\SynEditHighlighter.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditKbdHandler.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditKeyCmdEditor.dfm"
      File "..\Src\Extra\SynEdit\Source\SynEditKeyCmdEditor.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditKeyCmds.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditKeyCmdsEditor.dfm"
      File "..\Src\Extra\SynEdit\Source\SynEditKeyCmdsEditor.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditKeyConst.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditMiscClasses.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditMiscProcs.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPlugins.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPrint.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPrinterInfo.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPrintHeaderFooter.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPrintMargins.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPrintMarginsDialog.dfm"
      File "..\Src\Extra\SynEdit\Source\SynEditPrintMarginsDialog.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPrintTypes.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditPropertyReg.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditReg.dcr"
      File "..\Src\Extra\SynEdit\Source\SynEditReg.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditSearch.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditStrConst.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditTextBuffer.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditTypes.pas"
      File "..\Src\Extra\SynEdit\Source\SynEditWordWrap.pas"
      File "..\Src\Extra\SynEdit\Source\SynHighlighterMulti.pas"
      File "..\Src\Extra\SynEdit\Source\SynHighlighterXML.pas"
      File "..\Src\Extra\SynEdit\Source\SynMacroRecorder.pas"
      File "..\Src\Extra\SynEdit\Source\SynRegExpr.pas"
      File "..\Src\Extra\SynEdit\Source\SynTextDrawer.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\SynEdit\SynGen"

      File "..\Src\Extra\SynEdit\SynGen\GenLex.pas"
      File "..\Src\Extra\SynEdit\SynGen\LongIntList.pas"
      File "..\Src\Extra\SynEdit\SynGen\SynGen.dpr"
      File "..\Src\Extra\SynEdit\SynGen\SynGen.dproj"
      File "..\Src\Extra\SynEdit\SynGen\SynGen.exe"
      File "..\Src\Extra\SynEdit\SynGen\SynGen.res"
      File "..\Src\Extra\SynEdit\SynGen\SynGenUnit.dfm"
      File "..\Src\Extra\SynEdit\SynGen\SynGenUnit.pas"
      File "..\Src\Extra\SynEdit\SynGen\SynHighlighterCellML.msg"
      File "..\Src\Extra\SynEdit\SynGen\SynHighlighterCellML.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\Virtual Treeview\Design"

      File "..\Src\Extra\Virtual Treeview\Design\VirtualTreesReg.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\Virtual Treeview\Source"

      File "..\Src\Extra\Virtual Treeview\Source\Compilers.inc"
      File "..\Src\Extra\Virtual Treeview\Source\VirtualTrees.pas"
      File "..\Src\Extra\Virtual Treeview\Source\VirtualTrees.res"
      File "..\Src\Extra\Virtual Treeview\Source\VTAccessibilityFactory.pas"
      File "..\Src\Extra\Virtual Treeview\Source\VTConfig.inc"
      File "..\Src\Extra\Virtual Treeview\Source\VTHeaderPopup.pas"

      SetOutPath "$INSTDIR\Source\Src\Extra\WCrypt2"

      File "..\Src\Extra\WCrypt2\WCrypt2.pas"

      SetOutPath "$INSTDIR\Source\Src\GUI"

      File "..\Src\GUI\About.dfm"
      File "..\Src\GUI\About.pas"
      File "..\Src\GUI\CommandViewer.dfm"
      File "..\Src\GUI\CommandViewer.pas"
      File "..\Src\GUI\Computation.dfm"
      File "..\Src\GUI\Computation.pas"
      File "..\Src\GUI\Console.dfm"
      File "..\Src\GUI\Console.pas"
      File "..\Src\GUI\Contents.dfm"
      File "..\Src\GUI\Contents.pas"
      File "..\Src\GUI\Dockable.dfm"
      File "..\Src\GUI\Dockable.pas"
      File "..\Src\GUI\DockSites.pas"
      File "..\Src\GUI\Editor.dfm"
      File "..\Src\GUI\Editor.pas"
      File "..\Src\GUI\Generic.dfm"
      File "..\Src\GUI\Generic.pas"
      File "..\Src\GUI\GraphPanel.dfm"
      File "..\Src\GUI\GraphPanel.pas"
      File "..\Src\GUI\Main.dfm"
      File "..\Src\GUI\Main.pas"
      File "..\Src\GUI\Msg.dfm"
      File "..\Src\GUI\Msg.pas"
      File "..\Src\GUI\Msgs.dfm"
      File "..\Src\GUI\Msgs.pas"
      File "..\Src\GUI\Options.dfm"
      File "..\Src\GUI\Options.pas"
      File "..\Src\GUI\Password.dfm"
      File "..\Src\GUI\Password.pas"
      File "..\Src\GUI\Properties.dfm"
      File "..\Src\GUI\Properties.pas"
      File "..\Src\GUI\SplashScreen.dfm"
      File "..\Src\GUI\SplashScreen.pas"
      File "..\Src\GUI\Update.dfm"
      File "..\Src\GUI\Update.pas"
      File "..\Src\GUI\VersionInfo.dfm"
      File "..\Src\GUI\VersionInfo.pas"

      SetOutPath "$INSTDIR\Source\Src\ODE"

      File "..\Src\ODE\ODE2ndOrderRungeKuttaIntegrator.c"
      File "..\Src\ODE\ODE2ndOrderRungeKuttaIntegrator.pas"
      File "..\Src\ODE\ODE4thOrderRungeKuttaIntegrator.c"
      File "..\Src\ODE\ODE4thOrderRungeKuttaIntegrator.pas"
      File "..\Src\ODE\ODECVODEIntegrator.pas"
      File "..\Src\ODE\ODEFixedTimeStepIntegrator.pas"
      File "..\Src\ODE\ODEForwardEulerIntegrator.c"
      File "..\Src\ODE\ODEForwardEulerIntegrator.pas"
      File "..\Src\ODE\ODEIntegrator.pas"

      SetOutPath "$INSTDIR\Source\Src\VCL"

      File "..\Src\VCL\CmdGraph.pas"
      File "..\Src\VCL\OGL.pas"
      File "..\Src\VCL\OGLGraphPanel.pas"
      File "..\Src\VCL\RippledBitmap.pas"
      File "..\Src\VCL\SyntaxEdit.pas"
      File "..\Src\VCL\VSTListBox.pas"

      SetOutPath "$INSTDIR\Source\Temp"

      IfFileExists "$SMPROGRAMS\COR" 0 NoShortCuts

      CreateShortCut "$SMPROGRAMS\COR\Source Code.lnk" "$INSTDIR\Source"
   NoShortCuts:
   SectionEnd
!endif

#-------------------------------------------------------------------------------
# Examples
#-------------------------------------------------------------------------------

Section "Examples (recommended)" Examples
   SectionIn 1 2

   SetOutPath "$INSTDIR\Models"

   File "..\Models\aslanidi_model_2009.cellml"
   File "..\Models\beeler_reuter_model_1977.cellml"
   File "..\Models\bondarenko_model_2004_apex.cellml"
   File "..\Models\bondarenko_model_2004_septum.cellml"
   File "..\Models\bueno_model_2007.cellml"
   File "..\Models\chay_model_1997.cellml"
   File "..\Models\courtemanche_ramirez_nattel_model_1998.cellml"
   File "..\Models\demir_model_1994.cellml"
   File "..\Models\demir_model_1999.cellml"
   File "..\Models\difrancesco_noble_model_1985.cellml"
   File "..\Models\dokos_model_1996.cellml"
   File "..\Models\earm_noble_model_1990.cellml"
   File "..\Models\endresen_model_1997.cellml"
   File "..\Models\espinosa_model_1998_hypertrophy.cellml"
   File "..\Models\espinosa_model_1998_normal.cellml"
   File "..\Models\fenton_karma_model_1998_BR.cellml"
   File "..\Models\fenton_karma_model_1998_GP.cellml"
   File "..\Models\fenton_karma_model_1998_MBR.cellml"
   File "..\Models\fenton_karma_model_1998_MLR_1.cellml"
   File "..\Models\fink_noble_giles_model_2008.cellml"
   File "..\Models\fitzhugh_nagumo_model_1961.cellml"
   File "..\Models\fox_model_2002.cellml"
   File "..\Models\grandi_pasqualini_bers_2010.cellml"
   File "..\Models\hilgemann_noble_model_1987.cellml"
   File "..\Models\hodgkin_huxley_squid_axon_model_1952_modified.cellml"
   File "..\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml"
   File "..\Models\hund_rudy_model_2004.cellml"
   File "..\Models\inada_AN_model_2009.cellml"
   File "..\Models\inada_N_model_2009.cellml"
   File "..\Models\inada_NH_model_2009.cellml"
   File "..\Models\iribe_model_2006.cellml"
   File "..\Models\iyer_model_2004.cellml"
   File "..\Models\iyer_model_2007.cellml"
   File "..\Models\jafri_rice_winslow_model_1998.cellml"
   File "..\Models\katsnelson_model_2004_dimensional.cellml"
   File "..\Models\katsnelson_model_2004_dimensionless.cellml"
   File "..\Models\kurata_model_2002.cellml"
   File "..\Models\lindblad_atrial_model_1996.cellml"
   File "..\Models\livshitz_rudy_model_2007.cellml"
   File "..\Models\LR_dynamic_model_2000.cellml"
   File "..\Models\luo_rudy_I_model_1991.cellml"
   File "..\Models\mahajan_shiferaw_model_2008.cellml"
   File "..\Models\maleckar_greenstein_trayanova_giles_model_2008.cellml"
   File "..\Models\matsuoka_model_2003.cellml"
   File "..\Models\mcallister_noble_tsien_model_1975_A.cellml"
   File "..\Models\mcallister_noble_tsien_model_1975_B.cellml"
   File "..\Models\noble_difrancesco_denyer_model_1989.cellml"
   File "..\Models\noble_model_1962.cellml"
   File "..\Models\noble_model_1991.cellml"
   File "..\Models\noble_model_1998.cellml"
   File "..\Models\noble_model_1998_extended.cellml"
   File "..\Models\noble_model_1998_stretch.cellml"
   File "..\Models\noble_model_2001.cellml"
   File "..\Models\noble_noble_SAN_model_1984.cellml"
   File "..\Models\noble_SAN_model_1989.cellml"
   File "..\Models\nygren_atrial_model_1998.cellml"
   File "..\Models\pandit_model_2001_endo.cellml"
   File "..\Models\pandit_model_2001_epi.cellml"
   File "..\Models\pasek_simurda_orchard_christe_model_2008.cellml"
   File "..\Models\priebe_beuckelmann_model_1998.cellml"
   File "..\Models\sakmann_model_2000_endo.cellml"
   File "..\Models\sakmann_model_2000_epi.cellml"
   File "..\Models\sakmann_model_2000_M.cellml"
   File "..\Models\sarai_model_2003.cellml"
   File "..\Models\shannon_wang_puglisi_weber_bers_model_2004.cellml"
   File "..\Models\stewart_zhang_model_2008.cellml"
   File "..\Models\ten_tusscher_model_2004_endo.cellml"
   File "..\Models\ten_tusscher_model_2004_epi.cellml"
   File "..\Models\ten_tusscher_model_2004_M.cellml"
   File "..\Models\ten_tusscher_model_2006_endo.cellml"
   File "..\Models\ten_tusscher_model_2006_epi.cellml"
   File "..\Models\ten_tusscher_model_2006_M.cellml"
   File "..\Models\van_der_pol_model_1928.cellml"
   File "..\Models\viswanathan_model_1999_endo.cellml"
   File "..\Models\viswanathan_model_1999_epi.cellml"
   File "..\Models\viswanathan_model_1999_M.cellml"
   File "..\Models\winslow_model_1999.cellml"
   File "..\Models\yanagihara_model_1980.cellml"
   File "..\Models\zhang_SAN_model_2000_0D_capable.cellml"
   File "..\Models\zhang_SAN_model_2000_1D_capable.cellml"
   File "..\Models\zhang_SAN_model_2000_all.cellml"
   File "..\Models\zhang_SAN_model_2000_published.cellml"

   SetFileAttributes "$INSTDIR\Models\aslanidi_model_2009.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\beeler_reuter_model_1977.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\bondarenko_model_2004_apex.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\bondarenko_model_2004_septum.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\bueno_model_2007.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\chay_model_1997.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\courtemanche_ramirez_nattel_model_1998.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\demir_model_1994.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\demir_model_1999.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\difrancesco_noble_model_1985.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\dokos_model_1996.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\earm_noble_model_1990.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\endresen_model_1997.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\espinosa_model_1998_hypertrophy.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\espinosa_model_1998_normal.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\fenton_karma_model_1998_BR.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\fenton_karma_model_1998_GP.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\fenton_karma_model_1998_MBR.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\fenton_karma_model_1998_MLR_1.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\fink_noble_giles_model_2008.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\fitzhugh_nagumo_model_1961.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\fox_model_2002.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\grandi_pasqualini_bers_2010.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\hilgemann_noble_model_1987.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\hodgkin_huxley_squid_axon_model_1952_modified.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\hund_rudy_model_2004.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\inada_AN_model_2009.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\inada_N_model_2009.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\inada_NH_model_2009.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\iribe_model_2006.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\iyer_model_2004.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\iyer_model_2007.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\jafri_rice_winslow_model_1998.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\katsnelson_model_2004_dimensional.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\katsnelson_model_2004_dimensionless.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\kurata_model_2002.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\lindblad_atrial_model_1996.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\livshitz_rudy_model_2007.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\LR_dynamic_model_2000.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\luo_rudy_I_model_1991.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\mahajan_shiferaw_model_2008.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\maleckar_greenstein_trayanova_giles_model_2008.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\matsuoka_model_2003.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\mcallister_noble_tsien_model_1975_A.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\mcallister_noble_tsien_model_1975_B.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_difrancesco_denyer_model_1989.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_model_1962.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_model_1991.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_model_1998.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_model_1998_extended.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_model_1998_stretch.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_model_2001.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_noble_SAN_model_1984.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\noble_SAN_model_1989.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\nygren_atrial_model_1998.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\pandit_model_2001_endo.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\pandit_model_2001_epi.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\pasek_simurda_orchard_christe_model_2008.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\priebe_beuckelmann_model_1998.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\sakmann_model_2000_endo.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\sakmann_model_2000_epi.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\sakmann_model_2000_M.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\sarai_model_2003.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\shannon_wang_puglisi_weber_bers_model_2004.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\stewart_zhang_model_2008.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\ten_tusscher_model_2004_endo.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\ten_tusscher_model_2004_epi.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\ten_tusscher_model_2004_M.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\ten_tusscher_model_2006_endo.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\ten_tusscher_model_2006_epi.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\ten_tusscher_model_2006_M.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\van_der_pol_model_1928.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\viswanathan_model_1999_endo.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\viswanathan_model_1999_epi.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\viswanathan_model_1999_M.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\winslow_model_1999.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\yanagihara_model_1980.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\zhang_SAN_model_2000_0D_capable.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\zhang_SAN_model_2000_1D_capable.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\zhang_SAN_model_2000_all.cellml" READONLY
   SetFileAttributes "$INSTDIR\Models\zhang_SAN_model_2000_published.cellml" READONLY

   IfFileExists "$SMPROGRAMS\COR" 0 NoShortCuts

   CreateShortCut "$SMPROGRAMS\COR\Models.lnk" "$INSTDIR\Models"
NoShortCuts:
SectionEnd

#-------------------------------------------------------------------------------
# Things to do for the uninstall
#-------------------------------------------------------------------------------

Section "Uninstall"
   # Delete the keys in the registry

   DeleteRegKey HKLM "Software\COR"
   DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\COR"

   # Delete the user's registry keys

   DeleteRegKey HKCU "Software\COR"

   # Delete the files and directories

   # Delete the whole "Source" directory, no matter what it contains (normally
   # it should only contain the source code and maybe some backup, object, etc.
   # files)

   RMDir /r "$INSTDIR\Source"

   # Delete the models directory, but only the files that we created, in case
   # the user created new ones...

   Delete "$INSTDIR\Models\aslanidi_model_2009.cellml"
   Delete "$INSTDIR\Models\beeler_reuter_model_1977.cellml"
   Delete "$INSTDIR\Models\bondarenko_model_2004_apex.cellml"
   Delete "$INSTDIR\Models\bondarenko_model_2004_septum.cellml"
   Delete "$INSTDIR\Models\bueno_model_2007.cellml"
   Delete "$INSTDIR\Models\chay_model_1997.cellml"
   Delete "$INSTDIR\Models\courtemanche_ramirez_nattel_model_1998.cellml"
   Delete "$INSTDIR\Models\demir_model_1994.cellml"
   Delete "$INSTDIR\Models\demir_model_1999.cellml"
   Delete "$INSTDIR\Models\difrancesco_noble_model_1985.cellml"
   Delete "$INSTDIR\Models\dokos_model_1996.cellml"
   Delete "$INSTDIR\Models\earm_noble_model_1990.cellml"
   Delete "$INSTDIR\Models\endresen_model_1997.cellml"
   Delete "$INSTDIR\Models\espinosa_model_1998_hypertrophy.cellml"
   Delete "$INSTDIR\Models\espinosa_model_1998_normal.cellml"
   Delete "$INSTDIR\Models\fenton_karma_model_1998_BR.cellml"
   Delete "$INSTDIR\Models\fenton_karma_model_1998_GP.cellml"
   Delete "$INSTDIR\Models\fenton_karma_model_1998_MBR.cellml"
   Delete "$INSTDIR\Models\fenton_karma_model_1998_MLR_1.cellml"
   Delete "$INSTDIR\Models\fink_noble_giles_model_2008.cellml"
   Delete "$INSTDIR\Models\fitzhugh_nagumo_model_1961.cellml"
   Delete "$INSTDIR\Models\fox_model_2002.cellml"
   Delete "$INSTDIR\Models\grandi_pasqualini_bers_2010.cellml"
   Delete "$INSTDIR\Models\hilgemann_noble_model_1987.cellml"
   Delete "$INSTDIR\Models\hodgkin_huxley_squid_axon_model_1952_modified.cellml"
   Delete "$INSTDIR\Models\hodgkin_huxley_squid_axon_model_1952_original.cellml"
   Delete "$INSTDIR\Models\hund_rudy_model_2004.cellml"
   Delete "$INSTDIR\Models\inada_AN_model_2009.cellml"
   Delete "$INSTDIR\Models\inada_N_model_2009.cellml"
   Delete "$INSTDIR\Models\inada_NH_model_2009.cellml"
   Delete "$INSTDIR\Models\iribe_model_2006.cellml"
   Delete "$INSTDIR\Models\iyer_model_2004.cellml"
   Delete "$INSTDIR\Models\iyer_model_2007.cellml"
   Delete "$INSTDIR\Models\jafri_rice_winslow_model_1998.cellml"
   Delete "$INSTDIR\Models\katsnelson_model_2004_dimensional.cellml"
   Delete "$INSTDIR\Models\katsnelson_model_2004_dimensionless.cellml"
   Delete "$INSTDIR\Models\kurata_model_2002.cellml"
   Delete "$INSTDIR\Models\lindblad_atrial_model_1996.cellml"
   Delete "$INSTDIR\Models\livshitz_rudy_model_2007.cellml"
   Delete "$INSTDIR\Models\LR_dynamic_model_2000.cellml"
   Delete "$INSTDIR\Models\luo_rudy_I_model_1991.cellml"
   Delete "$INSTDIR\Models\mahajan_shiferaw_model_2008.cellml"
   Delete "$INSTDIR\Models\maleckar_greenstein_trayanova_giles_model_2008.cellml"
   Delete "$INSTDIR\Models\matsuoka_model_2003.cellml"
   Delete "$INSTDIR\Models\mcallister_noble_tsien_model_1975_A.cellml"
   Delete "$INSTDIR\Models\mcallister_noble_tsien_model_1975_B.cellml"
   Delete "$INSTDIR\Models\noble_difrancesco_denyer_model_1989.cellml"
   Delete "$INSTDIR\Models\noble_model_1962.cellml"
   Delete "$INSTDIR\Models\noble_model_1991.cellml"
   Delete "$INSTDIR\Models\noble_model_1998.cellml"
   Delete "$INSTDIR\Models\noble_model_1998_extended.cellml"
   Delete "$INSTDIR\Models\noble_model_1998_stretch.cellml"
   Delete "$INSTDIR\Models\noble_model_2001.cellml"
   Delete "$INSTDIR\Models\noble_noble_SAN_model_1984.cellml"
   Delete "$INSTDIR\Models\noble_SAN_model_1989.cellml"
   Delete "$INSTDIR\Models\nygren_atrial_model_1998.cellml"
   Delete "$INSTDIR\Models\pandit_model_2001_endo.cellml"
   Delete "$INSTDIR\Models\pandit_model_2001_epi.cellml"
   Delete "$INSTDIR\Models\pasek_simurda_orchard_christe_model_2008.cellml"
   Delete "$INSTDIR\Models\priebe_beuckelmann_model_1998.cellml"
   Delete "$INSTDIR\Models\sakmann_model_2000_endo.cellml"
   Delete "$INSTDIR\Models\sakmann_model_2000_epi.cellml"
   Delete "$INSTDIR\Models\sakmann_model_2000_M.cellml"
   Delete "$INSTDIR\Models\sarai_model_2003.cellml"
   Delete "$INSTDIR\Models\shannon_wang_puglisi_weber_bers_model_2004.cellml"
   Delete "$INSTDIR\Models\stewart_zhang_model_2008.cellml"
   Delete "$INSTDIR\Models\ten_tusscher_model_2004_endo.cellml"
   Delete "$INSTDIR\Models\ten_tusscher_model_2004_epi.cellml"
   Delete "$INSTDIR\Models\ten_tusscher_model_2004_M.cellml"
   Delete "$INSTDIR\Models\ten_tusscher_model_2006_endo.cellml"
   Delete "$INSTDIR\Models\ten_tusscher_model_2006_epi.cellml"
   Delete "$INSTDIR\Models\ten_tusscher_model_2006_M.cellml"
   Delete "$INSTDIR\Models\van_der_pol_model_1928.cellml"
   Delete "$INSTDIR\Models\viswanathan_model_1999_endo.cellml"
   Delete "$INSTDIR\Models\viswanathan_model_1999_epi.cellml"
   Delete "$INSTDIR\Models\viswanathan_model_1999_M.cellml"
   Delete "$INSTDIR\Models\winslow_model_1999.cellml"
   Delete "$INSTDIR\Models\yanagihara_model_1980.cellml"
   Delete "$INSTDIR\Models\zhang_SAN_model_2000_0D_capable.cellml"
   Delete "$INSTDIR\Models\zhang_SAN_model_2000_1D_capable.cellml"
   Delete "$INSTDIR\Models\zhang_SAN_model_2000_all.cellml"
   Delete "$INSTDIR\Models\zhang_SAN_model_2000_published.cellml"

   RMDir "$INSTDIR\Models\"

   # Delete the "main" files...

   Delete "$INSTDIR\COR.exe"
   Delete "$INSTDIR\Uninstall.exe"

   RMDir "$INSTDIR"

   IfFileExists $INSTDIR 0 Continue

   MessageBox MB_YESNO|MB_ICONQUESTION "The COR directory is not empty (you must have put some files in it). Do you want to keep its contents?" IDYES Continue

   Delete "$INSTDIR\*.*"

   RMDir /r $INSTDIR

Continue:

   # Delete the shell extensions

   ReadRegStr $1 HKCR ".cellml" ""

   StrCmp $1 "CellMLFile" 0 Continue2

   ReadRegStr $1 HKCR ".cellml" "BackupVal"

   StrCmp $1 "" 0 RestoreBackup

   DeleteRegKey HKCR ".cellml"

   Goto Continue2

RestoreBackup:

   WriteRegStr HKCR ".cellml" "" $1

   DeleteRegValue HKCR ".cellml" "BackupVal"

Continue2:

   DeleteRegKey HKCR "CellMLFile"

   System::Call "Shell32::SHChangeNotify(i ${SHCNE_ASSOCCHANGED}, i ${SHCNF_IDLIST}, i 0, i 0)"

   # Delete the start menu group

   Delete "$SMPROGRAMS\COR\*.*"

   RMDir "$SMPROGRAMS\COR"

   # Delete the desktop icon

   Delete "$DESKTOP\COR.lnk"
SectionEnd

#-------------------------------------------------------------------------------
# Descriptions
#-------------------------------------------------------------------------------

!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
   !insertmacro MUI_DESCRIPTION_TEXT ${COREnvironment} "Core files required by COR."
   !insertmacro MUI_DESCRIPTION_TEXT ${CORShellExtensions} "Support for CellML files extension."
   !insertmacro MUI_DESCRIPTION_TEXT ${StartMenuGroup} "Shortcuts, in the Start menu, to various COR resources."
   !insertmacro MUI_DESCRIPTION_TEXT ${DesktopShortcut} "Shortcut, on the desktop, to COR."

   !ifdef SOURCE_CODE
      !insertmacro MUI_DESCRIPTION_TEXT ${SourceCode} "Source code for the whole COR project."
   !endif
   
   !insertmacro MUI_DESCRIPTION_TEXT ${Examples} "COR examples (i.e. some CellML files)."
!insertmacro MUI_FUNCTION_DESCRIPTION_END

#-------------------------------------------------------------------------------
# End of file
#-------------------------------------------------------------------------------
