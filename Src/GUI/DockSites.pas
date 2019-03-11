//==============================================================================
// Dock forms
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 15/05/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit DockSites;

//==============================================================================

Interface

//==============================================================================

Uses
   Classes, Forms, ToolWin;

//==============================================================================

Type
   TToolBarDockForm = Class(TToolDockForm)
      Public
         Constructor Create(aOwner: TComponent); Override;
   End;
   TDockForm = Class(TCustomDockForm)
      Public
         Constructor Create(aOwner: TComponent); Override;
   End;

//==============================================================================

Implementation

//==============================================================================

Constructor TToolBarDockForm.Create(aOwner: TComponent);
Begin
   Inherited;

   BorderStyle := bsToolWindow;   // Don't want to allow resizing...
End;

//==============================================================================

Constructor TDockForm.Create(aOwner: TComponent);
Begin
   Inherited;

   FormStyle := fsNormal;   // Since it is set to "fsStayOnTop" by default and
                            // that we don't want that at all...
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

