//==============================================================================
// Dockable frame
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 10/06/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Dockable;

//==============================================================================

Interface

//==============================================================================

Uses
   Messages, Classes, Forms;

//==============================================================================

Type
   TDockableFrame = Class(TFrame)
      Protected
         // Properties used for internal purposes

         RealCaption: String;

         // Inherited methods

         Procedure Loaded; Override;

         // Methods handling different messages (from Windows or not)

         Procedure WMGetText(Var aMessage: TWMGetText); Message WM_GETTEXT;
         Procedure WMGetTextLength(Var aMessage: TWMGetTextLength); Message WM_GETTEXTLENGTH;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   SysUtils;

//==============================================================================

{$R *.dfm}

//==============================================================================

Procedure TDockableFrame.Loaded;
Begin
   Inherited;

   If (CompareStr(Name, 'CommandViewerFrame') = 0) Then
      RealCaption := 'Command Viewer'
   Else If (CompareStr(Name, 'PropertiesFrame') = 0) Then
      RealCaption := 'Properties Viewer'
   Else If (CompareStr(Name, 'MsgsFrame') = 0) Then
      RealCaption := 'Messages Viewer'
   Else If (CompareStr(Name, 'GraphPanelFrame') = 0) Then
      RealCaption := 'Graph Panel'
   Else If (CompareStr(Name, 'ConsoleFrame') = 0) Then
      RealCaption := 'Console Viewer'
   Else
      RealCaption := Name;   // Note: we should never reach that point...
End;

//==============================================================================

Procedure TDockableFrame.WMGetText(Var aMessage: TWMGetText);
Begin
   aMessage.Result := StrLen(StrPLCopy(aMessage.Text, RealCaption, aMessage.TextMax-1));
End;

//==============================================================================

Procedure TDockableFrame.WMGetTextLength(Var aMessage: TWMGetTextLength);
Begin
   aMessage.Result := Length(RealCaption);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
