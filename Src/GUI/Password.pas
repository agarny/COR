//==============================================================================
// Password form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 09/01/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Password;

//==============================================================================

Interface

//==============================================================================

Uses
   Classes, Controls, StdCtrls, JvExStdCtrls, JvEdit, JvValidateEdit, Generic;

//==============================================================================

Type
   TPasswordForm = Class(TGenericForm)
    PasswordLab: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    PasswordVal: TJvValidateEdit;
      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent; Const aCaption: String = '&Password'); Reintroduce;
   End;

//==============================================================================

Implementation

//==============================================================================

{$R *.dfm}

//==============================================================================

Uses
   CORCommon;

//==============================================================================

Constructor TPasswordForm.Create(aOwner: TComponent; Const aCaption: String);
Begin
   Inherited Create(aOwner);

   Caption := aCaption;

   PasswordLab.Caption := aCaption+':';

   EditFieldForPassword(PasswordVal);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
