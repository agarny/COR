//==============================================================================
// Version info form
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://COR.physiol.ox.ac.uk/
//
// Copyright 2002-2009
//------------------------------------------------------------------------------
// Date of Creation: 03/01/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit VersionInfo;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, Classes, Controls, Forms, StdCtrls, ComCtrls, Generic, ExtCtrls;

//==============================================================================

Type
   TVersionInfoForm = class(TGenericForm)
    MsgLab: TLabel;
    QuestionLab: TLabel;
    YesBtn: TButton;
    NoBtn: TButton;
    VersionInfoScrollBox: TScrollBox;
    VersionInfoPanel: TPanel;
    VersionInfoVal: TRichEdit;
    procedure VersionInfoValResizeRequest(Sender: TObject; Rect: TRect);
    procedure FormCreate(Sender: TObject);
      Public
         Constructor Create(aOwner: TComponent; Const aVersionInfo: TStrings; Const aNewFileSize: Integer); Reintroduce;
   End;

//==============================================================================

Implementation

//==============================================================================

Uses
   SysUtils, Graphics, RichEdit, Common, CORCommon;

//==============================================================================

{$R *.dfm}

//==============================================================================

Constructor TVersionInfoForm.Create(aOwner: TComponent;
                                    Const aVersionInfo: TStrings;
                                    Const aNewFileSize: Integer);
Var
   I: Integer;
   NbOfDescriptions: Integer;
Begin
   Inherited Create(aOwner);

   Caption := 'Newer version of '+COR_NAME+' available!';

   Width := VersionInfoScrollBox.Width+GRID_SPACING;
   // Note: the scrollbox for the version info component MUST be THE reference
   //       in terms of width! Otherwise, things get completely screwed up with
   //       the version info component, since it is a "TRichEdit" one and may
   //       therefore have to word wrap some text... so we must NOT resize the
   //       width of the scrollbox ever!

   MsgLab.AutoSize := False;

   MsgLab.Caption := 'A new version of '+COR_NAME+' is available!';

   If (aVersionInfo.Count <> 0) Then Begin
      MsgLab.Caption := MsgLab.Caption+' Below is a summary of the changes to '+COR_NAME+', since you have last installed/updated it:';

      MsgLab.Width := VersionInfoScrollBox.Width;

      MsgLab.AutoSize := True;

      VersionInfoScrollBox.Top := MsgLab.Top+MsgLab.Height+HALF_GRID_SPACING;

      VersionInfoScrollBox.Height := 3*VersionInfoPanel.Width Div 5;

      I := 0;

      With VersionInfoVal Do Begin
         // Set the location and width of the version info component

         Top  := 0;
         Left := 0;

         Width := VersionInfoScrollBox.ClientWidth;

         // Generate the version info

         Repeat
            If (NewVersion(aVersionInfo.Strings[I], COR_VERSION)) Then Begin
               If (I <> 0) Then
                  Lines.Add('');

               // Version info

               Paragraph.Alignment := taCenter;
               Paragraph.Numbering := nsNone;

               SelAttributes.Style := [fsBold];

               Lines.Add('Version '+aVersionInfo.Strings[I]);

               // Version description

               Lines.Add('');

               Paragraph.Alignment := taLeftJustify;
               Paragraph.Numbering := nsBullet;

               SelAttributes.Style := [];

               NbOfDescriptions := 0;

               Repeat
                  Inc(I);

                  If ((I <> aVersionInfo.Count) And
                      Not ValidVersion(aVersionInfo.Strings[I]) And
                      (CompareStr(aVersionInfo.Strings[I], '') <> 0)) Then Begin
                     Lines.Add(aVersionInfo.Strings[I]);

                     Inc(NbOfDescriptions);
                  End;
               Until (I = aVersionInfo.Count) Or ValidVersion(aVersionInfo.Strings[I]);

               Paragraph.Numbering := nsNone;
               // Note: this is required to avoid the trailing bullet... :(

               If (NbOfDescriptions = 0) Then Begin
                  Paragraph.Alignment := taCenter;

                  SelAttributes.Style := [fsItalic];

                  Lines.Add('No information available...');
               End;
            End Else
               Break;
         Until I = aVersionInfo.Count;

         // Determine the real height of the version info component
         // Note: to do something like "-Lines.Count*Font.Height" is,
         //       unfortunately, not good enough, so...

         // Set the height to something much bigger than it will ever be (this
         // is done via the scaling by a factor of 2)

         Height := -2*Lines.Count*Font.Height;

         // Request a resizing, in order to get the optimised height (done by
         // handling the "OnRequestResize" event)

         SendMessage(Handle, EM_REQUESTRESIZE, 0, 0);

         // Check the height of the version info component against that of the
         // client part of the scrollbox and adjust things if necessary...

         If (Height < VersionInfoScrollBox.ClientHeight) Then
            // The version info component doesn't take all the client space of
            // the scrollbox, so update the height of the scrollbox...

            VersionInfoScrollBox.ClientHeight := Height
         Else
            // The scrollbox has a scrollbar, so update the width of the version
            // info component...

            Width := VersionInfoScrollBox.ClientWidth;

         // Update the version info panel

         VersionInfoPanel.Top  := Top;
         VersionInfoPanel.Left := Left;

         VersionInfoPanel.Width := Width;
      End;

      QuestionLab.Top := VersionInfoScrollBox.Top+VersionInfoScrollBox.Height+HALF_GRID_SPACING;

      QuestionLab.Width := VersionInfoScrollBox.Width;

      QuestionLab.Caption := 'Do you want to download it (size: '+ConvertBytes(aNewFileSize, bfKB)+')?';

      YesBtn.Top := QuestionLab.Top+QuestionLab.Height+HALF_GRID_SPACING;
      NoBtn.Top  := QuestionLab.Top+QuestionLab.Height+HALF_GRID_SPACING;
   End Else Begin
      MsgLab.Caption := MsgLab.Caption+' Do you want to download it (size: '+ConvertBytes(aNewFileSize, bfKB)+')?';

      MsgLab.Width := VersionInfoScrollBox.Width;

      MsgLab.AutoSize := True;

      VersionInfoScrollBox.Visible := False;

      QuestionLab.Visible := False;

      YesBtn.Top := MsgLab.Top+MsgLab.Height+HALF_GRID_SPACING;
      NoBtn.Top  := MsgLab.Top+MsgLab.Height+HALF_GRID_SPACING;
   End;

   YesBtn.Left := (ClientWidth-YesBtn.Width-GRID_SPACING-NoBtn.Width) Div 2;
   NoBtn.Left  := YesBtn.Left+YesBtn.Width+GRID_SPACING;

   ClientHeight := YesBtn.Top+YesBtn.Height+HALF_GRID_SPACING;
End;

//==============================================================================

Procedure TVersionInfoForm.FormCreate(Sender: TObject);
Begin
   Inherited;

   If (Not IsWinVistaOrAbove) Then
      NoBtn.Cancel := True;
End;

//==============================================================================

Procedure TVersionInfoForm.VersionInfoValResizeRequest(Sender: TObject;
                                                       Rect: TRect);
Begin
   // Set the real height for the version info component

   With VersionInfoVal Do Begin
      Height := Rect.Bottom-Rect.Top+Font.Height;
      // Note: the "+Font.Height" is to get rid of the apparent trailing line
      //       that is automatically added by "TRichEdit", since to do something
      //       like "Lines.Delete(Lines.Count-1)" doesn't do the trick...

      // Update the version info panel

      VersionInfoPanel.Height := Height;
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
