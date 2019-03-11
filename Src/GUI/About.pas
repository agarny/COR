//==============================================================================
// About form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 27/04/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit About;

//==============================================================================

Interface

//==============================================================================

Uses
   Controls, Forms, ExtCtrls, ComCtrls, StdCtrls, Classes, RippledBitmap,
   JvLinkLabel, Graphics, Generic, JvExControls;

//==============================================================================

Type
   TAboutForm = Class(TGenericForm)
    OKBtn: TButton;
    CORLogo: TRippledBitmap;
    Timer: TTimer;
    DummyLab: TLabel;
    CORPanel: TPanel;
    VersionVal: TLabel;
    CopyrightVal: TLabel;
    URLAndEMailVal: TJvLinkLabel;
    CellMLPanel: TPanel;
    CellMLVerVal: TJvLinkLabel;
    DevToolsPanel: TPanel;
    DevToolsVal: TJvLinkLabel;
    ThirdPartiesPanel: TPanel;
    ThirdPartiesVal: TJvLinkLabel;
    ComputerPanel: TPanel;
    NameAndWinVerVal: TLabel;
    CPUSpeedVal: TLabel;
    CPUSpeedLab: TLabel;
    PhysMemLab: TLabel;
    PhysMemVal: TLabel;
    PhysMemProgressBar: TProgressBar;
    CodeGearPanel: TPanel;
    CodeGearLogo: TImage;
    IntelMicrosoftPanel: TPanel;
    IntelLogo: TImage;
    MicrosoftLogo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure URLAndEMailValLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: String);
    procedure CellMLVerValLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: String);
    procedure ThirdPartiesValLinkClick(Sender: TObject;
      LinkNumber: Integer; LinkText, LinkParam: String);
    procedure DevToolsValLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: string);
    procedure CodeGearLogoClick(Sender: TObject);
    procedure IntelLogoClick(Sender: TObject);
    procedure MicrosoftLogoClick(Sender: TObject);
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   Windows, SysUtils, StrUtils, Consts, DeCAL, Common, CORCommon, CellMLAPI,
   CellMLAPIToMCEngine;

//==============================================================================

{$R *.dfm}

//==============================================================================

Procedure TAboutForm.FormCreate(Sender: TObject);
Begin
   Inherited;

   // GUI stuff

   PhysMemProgressBar.DoubleBuffered  := True;

   If (Not IsWinVistaOrAbove) Then
      OKBtn.Cancel := True;

   // Initialise a few things

   // COR details

   VersionVal.Caption     := 'Version '+COR_VERSION;
   CopyrightVal.Caption   := COR_FULL_COPYRIGHT;
   URLAndEMailVal.Caption := '<LINK>'+COR_URL+'</LINK>     <LINK>'+COR_EMAIL+'</LINK>';

   // CellML details

   CellMLVerVal.Caption := 'The models are stored using <LINK>'+CELLML_VERSION+'</LINK>';

   // Development tools details

   DevToolsVal.Caption := COR_NAME+' is developed using <LINK>CodeGear Delphi 2007</LINK>, as well as <LINK>'+String(Compilers.At(Integer(cIntelCPP)).VString)+' 10.1.021</LINK> and <LINK>'+String(Compilers.At(Integer(cMicrosoftCPP)).VString)+' 2008 SP1</LINK>';

   // Third parties details

   ThirdPartiesVal.Caption := 'Third-party components/libraries are also used. They are: <LINK>CVODE 2.6.0</LINK>, <LINK>DeCAL 1.0.0</LINK>, <LINK>FastCode 0.6.4</LINK> (including <LINK>FastMove 3.03</LINK>), <LINK>FastMM 4.94</LINK>, '+
                              'some components from <LINK>JCL 2.1.1.3536</LINK> and <LINK>JVCL 3.39</LINK> (part of <LINK>Project JEDI</LINK>), <LINK>OpenGL Soft Gems 2.0.1</LINK>, <LINK>SynEdit 2.0.6</LINK>, <LINK>Virtual Treeview 5.0.0</LINK> '+
                              'and <LINK>WCrypt2</LINK>';

   // Try to get URLAndEMailVal and CellMLVerVal in the right place, since
   // TJvLinkLabel cannot centre its caption. We use DummyLab to help us with
   // that...

   DummyLab.Font.Style := [fsUnderline];

   DummyLab.Caption := ReplaceStr(URLAndEMailVal.Caption, '<LINK>', '');
   DummyLab.Caption := ReplaceStr(DummyLab.Caption, '</LINK>', '');

   URLAndEMailVal.Left  := (ClientWidth-DummyLab.Width) Div 2;
   URLAndEMailVal.Width := DummyLab.Width+1;
   // Note: the "+1" is unfortunately necessary, as TJvLinkLabel will
   //       otherwise display the caption over two lines...

   DummyLab.Caption := ReplaceStr(CellMLVerVal.Caption, '<LINK>', '');
   DummyLab.Caption := ReplaceStr(DummyLab.Caption, '</LINK>', '');

   CellMLVerVal.Left  := (ClientWidth-DummyLab.Width) Div 2;
   CellMLVerVal.Width := DummyLab.Width+1;
   // Note: the "+1" is unfortunately necessary, as TJvLinkLabel will
   //       otherwise display the caption over two lines...

   // Update the about box straight away...

   TimerTimer(Sender);
End;

//==============================================================================

Procedure TAboutForm.TimerTimer(Sender: TObject);
Var
   ProgressBarLeft: Integer;
Begin
   // Disable the timer, so we have time to update the about box

   Timer.Enabled := False;

   // Get the name of the computer and the version of Windows on which COR is
   // running

   NameAndWinVerVal.Caption := CORComputer.Name+': '+CORComputer.WindowsVersion;

   // Get the information of the COR computer after updating it

   CORComputer.Update;

   // Get the CPU speed

   If (CORComputer.CPUSpeed < 1) Then
      CPUSpeedVal.Caption := ConvertSpeed(CORComputer.CPUSpeed, sfMHz)
   Else
      CPUSpeedVal.Caption := ConvertSpeed(CORComputer.CPUSpeed, sfGHz);

   // Get some information about the memory available

   PhysMemVal.Caption := ConvertBytes(CORComputer.PhysicalMemory.Available, bfMB)+' / '+ConvertBytes(CORComputer.PhysicalMemory.Total, bfMB);

   PhysMemProgressBar.Position := Round(100*(CORComputer.PhysicalMemory.Available/CORComputer.PhysicalMemory.Total));

   // Position some components

   If (CPUSpeedVal.Width > PhysMemVal.Width) Then
      ProgressBarLeft := CPUSpeedVal.Width
   Else
      ProgressBarLeft := PhysMemVal.Width;

   ProgressBarLeft := ProgressBarLeft+CPUSpeedVal.Left+GRID_SPACING;
   // Offset + some space...

   PhysMemProgressBar.Width := PhysMemProgressBar.Width+(PhysMemProgressBar.Left-ProgressBarLeft);
   PhysMemProgressBar.Left  := ProgressBarLeft;

   // Re-enable the timer

   Timer.Enabled := True;
End;

//==============================================================================

Procedure TAboutForm.URLAndEMailValLinkClick(Sender: TObject;
                                             LinkNumber: Integer;
                                             LinkText, LinkParam: String);
Begin
   If (LinkNumber = 0) Then
      CORShellExecute(COR_URL)
   Else
      CORShellExecute('MAILTO: '+COR_NAME+' <'+COR_EMAIL+'>?SUBJECT=About '+COR_NAME+' ('+COR_VERSION+')');
End;

//==============================================================================

Procedure TAboutForm.CellMLVerValLinkClick(Sender: TObject; LinkNumber: Integer;
                                           LinkText, LinkParam: String);
Begin
   CORShellExecute(CELLML_URL);
End;

//==============================================================================

Const
   CODEGEAR_DELPHI_URL = 'http://www.codegear.com/products/delphi/win32/';
   INTEL_CPP_URL       = 'http://www.intel.com/cd/software/products/asmo-na/eng/279578.htm';
   MICROSOFT_CPP_URL   = 'http://www.microsoft.com/express/vc/';

//==============================================================================

Procedure TAboutForm.DevToolsValLinkClick(Sender: TObject; LinkNumber: Integer;
                                          LinkText, LinkParam: String);
Begin
   Case LinkNumber Of
      0:  CORShellExecute(CODEGEAR_DELPHI_URL);
      1:  CORShellExecute(INTEL_CPP_URL);
      2:  CORShellExecute(MICROSOFT_CPP_URL);
   End;
End;

//==============================================================================

Procedure TAboutForm.CodeGearLogoClick(Sender: TObject);
Begin
   CORShellExecute(CODEGEAR_DELPHI_URL);
End;

//==============================================================================

Procedure TAboutForm.IntelLogoClick(Sender: TObject);
Begin
   CORShellExecute(INTEL_CPP_URL);
End;

//==============================================================================

Procedure TAboutForm.MicrosoftLogoClick(Sender: TObject);
Begin
   CORShellExecute(MICROSOFT_CPP_URL);
End;

//==============================================================================

Procedure TAboutForm.ThirdPartiesValLinkClick(Sender: TObject;
                                              LinkNumber: Integer;
                                              LinkText, LinkParam: String);
Begin
   Case LinkNumber Of
       0:  CORShellExecute('http://www.llnl.gov/CASC/sundials/description/description.html#descr_cvode');
       1:  CORShellExecute('http://sourceforge.net/projects/decal/');
       2:  CORShellExecute('http://www.sourceforge.net/projects/fastcode/');
       3:  CORShellExecute('http://fastcode.sourceforge.net/challenge_content/FastMove.html');
       4:  CORShellExecute('http://sourceforge.net/projects/fastmm/');
       5:  CORShellExecute('http://sourceforge.net/projects/jcl/');
       6:  CORShellExecute('http://sourceforge.net/projects/jvcl/');
       7:  CORShellExecute('http://www.sourceforge.net/projects/projectjedi/');
       8:  CORShellExecute('http://www.soft-gems.net/index.php?option=com_content&task=view&id=28&Itemid=33');
       9:  CORShellExecute('http://sourceforge.net/projects/synedit/');
      10:  CORShellExecute('http://www.soft-gems.net/index.php?option=com_content&task=view&id=12&Itemid=33');
      11:  CORShellExecute('http://www.delphi-jedi.org/apilibrary.html');
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

