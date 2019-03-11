//==============================================================================
// Splash screen form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 21/03/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit SplashScreen;

//==============================================================================

Interface

//==============================================================================

Uses
   Controls, Forms, Classes, ExtCtrls, Graphics, Messages, StdCtrls, Generic;

//==============================================================================

Type
   TSplashScreenForm = Class(TGenericForm)
    CORLogo: TImage;
    Shape: TShape;
    CORInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SplashScreenMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      Protected
         // Inherited methods

         Procedure WMEraseBkgnd(Var aMessage: TWMEraseBkgnd); Message WM_ERASEBKGND;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   Windows, CORCommon;

//==============================================================================

{$R *.dfm}

//==============================================================================

Procedure TSplashScreenForm.FormCreate(Sender: TObject);
Begin
   Inherited;

   CORInfo.Caption := COR_COPYRIGHT+' - Version '+COR_VERSION;
End;

//==============================================================================

Procedure TSplashScreenForm.WMEraseBkgnd(Var aMessage: TWMEraseBkgnd);
Begin
   // Prevent the background from being erased

   aMessage.Result := 1;
End;

//==============================================================================

Procedure TSplashScreenForm.SplashScreenMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
   Hide;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

