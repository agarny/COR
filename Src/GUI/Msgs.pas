//==============================================================================
// Messages frame
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 19/05/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Msgs;

//==============================================================================

Interface

//==============================================================================

Uses
   Classes, Controls, Dockable, DeCAL, VirtualTrees, VSTListBox;

//==============================================================================

Type
   TMsgsFrame = Class(TDockableFrame)
    Msgs: TVSTListBox;
      Public
         FileNames: DArray;
         LineNbs: DArray;
         ColNbs: DArray;

         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;
   End;

//==============================================================================

Implementation

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TMsgsFrame.Create(aOwner: TComponent);
Begin
   Inherited;

   FileNames := DArray.Create;
   LineNbs   := DArray.Create;
   ColNbs    := DArray.Create;
End;

//==============================================================================

Destructor TMsgsFrame.Destroy;
Begin
   Inherited;

   FileNames.Free;
   LineNbs.Free;
   ColNbs.Free;
   // Note: we must NOT free the objects held by those arrays, for they are not
   //       objects!
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

