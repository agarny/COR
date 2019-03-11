//==============================================================================
// Message form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 05/10/2006
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Msg;

//==============================================================================

Interface

//==============================================================================

Uses
   Forms, Classes, Messages, StdCtrls, ExtCtrls, Controls, Generic;

//==============================================================================

Type
   TMsgForm = Class(TGenericForm)
    Shape: TShape;
    Msg: TLabel;
      Protected
         // Inherited methods

         Procedure WMEraseBkgnd(Var aMessage: TWMEraseBkgnd); Message WM_ERASEBKGND;

      Public
         // Constructor & Destructor

         Constructor Create(Const aMsg: String; Const aFontSize: Integer = -1); Reintroduce;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   Common, CORCommon;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TMsgForm.Create(Const aMsg: String; Const aFontSize: Integer);
   Procedure SetFontSize(aFontSize: Integer);
   Begin
      Inc(aFontSize);   // Just so that the algorithm below works fine...

      Repeat
         Dec(aFontSize);

         Msg.Font.Size   := aFontSize;

         Msg.Canvas.Font := Msg.Font;

         ClientWidth  := HALF_GRID_SPACING+Msg.Canvas.TextWidth(Msg.Caption)+HALF_GRID_SPACING;
         ClientHeight := HALF_GRID_SPACING+Msg.Canvas.TextHeight(Msg.Caption)+HALF_GRID_SPACING;
      Until ClientWidth <= Application.MainForm.ClientWidth;
   End;
Const
   NORMAL_FONT_SIZE = 11;
Begin
   Inherited Create(Application.MainForm);

   Msg.Caption := aMsg;

   If (aFontSize <> UNDEFINED) Then
      SetFontSize(aFontSize)
   Else
      SetFontSize(NORMAL_FONT_SIZE);
End;

//==============================================================================

Procedure TMsgForm.WMEraseBkgnd(Var aMessage: TWMEraseBkgnd);
Begin
   // Prevent the background from being erased

   aMessage.Result := 1;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

