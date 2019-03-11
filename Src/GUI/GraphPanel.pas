//==============================================================================
// Graph panel frame
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 06/09/2005
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit GraphPanel;

//==============================================================================

Interface

//==============================================================================

Uses
   Classes, Controls, Messages, Dockable, ExtCtrls, OGL, OGLGraphPanel;

//==============================================================================

Type
   TGraphPanelFrame = Class(TDockableFrame)
    OGLGraphPanel: TOGLGraphPanel;
    Panel: TPanel;
    procedure OGLGraphPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
      Private
         // Methods to modify the different published properties

         Procedure SetActive(Const aValue: Boolean); Inline;

         // Methods handling different messages (from Windows or not)

         Procedure CMVisibleChanged(Var aMessage: TMessage); Message CM_VISIBLECHANGED;
         Procedure WMClose(Var aMessage: TWMClose); Message WM_CLOSE;

      Protected
         // Private representation of published properties

         FActive: Boolean;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent); Override;

      Published
         // Published properties

         Property Active: Boolean Read FActive Write SetActive;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
   Windows, SysUtils, Graphics, Main;

//==============================================================================

Var
   TGraphPanelFrameCounter: Integer;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TGraphPanelFrame.Create(aOwner: TComponent);
Begin
   Inherited;

   Inc(TGraphPanelFrameCounter);

{$IFDEF OPT_MATH}
   Name := ClassName+StringOfChar('0', OptFloor(OptLog10(MaxInt))-OptFloor(OptLog10(TGraphPanelFrameCounter)))+IntToStr(TGraphPanelFrameCounter)
{$ELSE}
   Name := ClassName+StringOfChar('0', Floor(Log10(MaxInt))-Floor(Log10(TGraphPanelFrameCounter)))+IntToStr(TGraphPanelFrameCounter)
{$ENDIF}
End;

//==============================================================================

Procedure TGraphPanelFrame.SetActive(Const aValue: Boolean);
Begin
   If (aValue <> FActive) Then Begin
      FActive := aValue;

      If (aValue) Then 
         Panel.Color := clActiveCaption
      Else
         Panel.Color := clInactiveCaption;
   End;
End;

//==============================================================================

Procedure TGraphPanelFrame.CMVisibleChanged(Var aMessage: TMessage);
Begin
   Inherited;

   If (Not Visible) Then
      // The graph panel has been hidden, which means it has to be closed...

      PostMessage(Handle, WM_CLOSE, 0, 0);
End;

//==============================================================================

Procedure TGraphPanelFrame.WMClose(Var aMessage: TWMClose);
Begin
   // When closing a graph panel, another one has to be selected, so...
   // Note: we cannot do this within the handler for "CM_VISIBLECHANGED",
   //       because after that message is handled, Windows will try to do
   //       something with it, so...

   MainForm.ComputationFrame.RemoveGraphPanel(Self, False);

   Inherited;

   Self.Free;
   // Note: it is very important to release the graph panel, as it will at least
   //       update docking-related things...
End;

//==============================================================================

Procedure TGraphPanelFrame.OGLGraphPanelMouseDown(Sender: TObject;
                                                  Button: TMouseButton;
                                                  Shift: TShiftState;
                                                  X, Y: Integer);
Begin
   Inherited;

   MainForm.ComputationFrame.ActiveGraphPanel := Self;
End;

//==============================================================================

Initialization

//==============================================================================

TGraphPanelFrameCounter := 0;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

