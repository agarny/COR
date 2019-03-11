//==============================================================================
// Rippled bitmap component
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 04/06/2004
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================
// Note: based on TImage and some code found at:
//       http://www.darwin3d.com/gdm1999.htm
//       (source code and exe: http://www.darwin3d.com/gamedev/gdm1299.zip)
//==============================================================================

Unit RippledBitmap;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, SysUtils, Classes, Controls, Graphics;

//==============================================================================

Type
   TRBRipples = Class(TPersistent)
      Private
         // Methods to modify the different published properties

         Procedure SetRadius(Const aValue: Byte); Inline;
         Procedure SetDamping(Const aValue: Double); Inline;

      Protected
         // Private representation of published properties

         FRadius: Byte;
         FDamping: Double;

         // Methods used for internal purposes

         Procedure DefineProperties(aFiler: TFiler); Override;

         Procedure ReadDamping(aReader: TReader); Inline;
         Procedure WriteDamping(aWriter: TWriter); Inline;

      Public
         // Constructor & Destructor

         Constructor Create;

      Published
         // Published properties

         Property Radius: Byte Read FRadius Write SetRadius Default 12;
         Property Damping: Double Read FDamping Write SetDamping;
   End;
   TRippledBitmap = Class;
   TRBEngine = Class(TThread)
      Private
         // Properties used for internal purposes

         Owner: TRippledBitmap;

      Protected
         // Inherited methods

         Procedure Execute; Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TRippledBitmap);
   End;
   TRBBuffer = Array Of Array Of ShortInt;
   TRippledBitmap = Class(TGraphicControl)
      Private
         // Properties used for internal purposes

         Engine: TRBEngine;

         RealBmp, BuffBmp: TBitmap;

         RealBmpOrig, RealBmpPitch: Integer;
         BuffBmpOrig, BuffBmpPitch: Integer;

         Buff: Array[0..1] Of TRBBuffer;
         Buff0, Buff1: Integer;

         Dragging: Boolean;

         // Methods to modify the different published properties

         Procedure SetBitmap(Const aValue: TBitmap);
         Procedure SetCenter(Const aValue: Boolean);
         Procedure SetProportional(Const aValue: Boolean);
         Procedure SetStretch(Const aValue: Boolean);

         // Methods used for internal purposes

         Procedure ResetBuffs;

         Procedure UpdateRBProps;

         Procedure MakeDrip(Const aX, aY: Integer);

      Protected
         // Private representation of published properties

         FBitmap: TBitmap;
         FCenter: Boolean;
         FRipples: TRBRipples;
         FProportional: Boolean;
         FStretch: Boolean;

         // Inherited methods

         Procedure SetAutoSize(aValue: Boolean); Override;

         Procedure Paint; Override;
         Procedure Resize; Override;

         // Methods handling different messages

         Procedure MouseDown(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer); Override;
         Procedure MouseMove(aShift: TShiftState; aX, aY: Integer); Override;
         Procedure MouseUp(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer); Override;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;

      Published
         // Published properties

         Property Anchors;
         Property AutoSize;
         Property Bitmap: TBitmap Read FBitmap Write SetBitmap;
         Property Center: Boolean Read FCenter Write SetCenter Default False;
         Property Ripples: TRBRipples Read FRipples;
         Property Proportional: Boolean Read FProportional Write SetProportional Default False;
         Property Stretch: Boolean Read FStretch Write SetStretch Default False;
   End;

//==============================================================================

Procedure Register;

//==============================================================================

Implementation

//==============================================================================

Constructor TRBRipples.Create;
Begin
   FRadius  := 12;
   FDamping := 0.002;
End;

//==============================================================================

Procedure TRBRipples.DefineProperties(aFiler: TFiler);
Begin
   Inherited;

   aFiler.DefineProperty('Damping', ReadDamping, WriteDamping, True);
End;

//==============================================================================

Procedure TRBRipples.ReadDamping(aReader: TReader);
Begin
   Damping := aReader.ReadFloat;
End;

//==============================================================================

Procedure TRBRipples.WriteDamping(aWriter: TWriter);
Begin
   aWriter.WriteFloat(Damping);
End;

//==============================================================================

Procedure TRBRipples.SetRadius(Const aValue: Byte);
Begin
   If ((aValue <> FRadius) And (aValue > 0)) Then
      FRadius := aValue;
End;

//==============================================================================

Procedure TRBRipples.SetDamping(Const aValue: Double);
Begin
   If ((aValue <> FDamping) And (aValue > 0)) Then
      FDamping := aValue;
End;

//==============================================================================

Constructor TRBEngine.Create(aOwner: TRippledBitmap);
Begin
   Owner := aOwner;

   Inherited Create(True);

   Priority := tpIdle;
End;

//==============================================================================

Procedure TRBEngine.Execute;
Const
   One_6: Double = 1/6;
Var
   BuffBmpWidthPlusOne, BuffBmpHeightPlusOne: Integer;
   I, J: Integer;
   OneMinusDamping: Double;
   RealBmpPixels, BuffBmpPixels: PByteArray;
   XOff, YOff: Integer;
   R, G, B: Integer;
   BuffVal: ShortInt;
   ShouldSuspend: Boolean;
Begin
   With Owner Do Begin
      While Not Terminated Do Begin
         ShouldSuspend := True;

         BuffBmpWidthPlusOne  := BuffBmp.Width+1;
         BuffBmpHeightPlusOne := BuffBmp.Height+1;

         OneMinusDamping := 1-Ripples.Damping;

         For J := 2 To BuffBmpHeightPlusOne Do
            For I := 2 To BuffBmpWidthPlusOne Do
               Buff[Buff0][I, J] := ShortInt(Trunc(OneMinusDamping*
                                                   (One_6*(Buff[Buff1][I-2, J  ]+Buff[Buff1][I+2, J  ]+
                                                           Buff[Buff1][I,   J-2]+Buff[Buff1][I,   J+2]+
                                                           Buff[Buff1][I-1, J  ]+Buff[Buff1][I+1, J  ]+
                                                           Buff[Buff1][I,   J-1]+Buff[Buff1][I,   J+1]+
                                                           Buff[Buff1][I-1, J-1]+Buff[Buff1][I+1, J-1]+
                                                           Buff[Buff1][I-1, J+1]+Buff[Buff1][I+1, J+1])-Buff[Buff0][I, J])));

         // Swap the buffers

         If (Buff0 = 0) Then Begin
            Buff0 := 1;
            Buff1 := 0;
         End Else Begin
            Buff0 := 0;
            Buff1 := 1;
         End;

         For J := 2 To BuffBmpHeightPlusOne Do Begin
            BuffBmpPixels := Pointer(BuffBmpOrig+(J-2)*BuffBmpPitch);

            For I := 2 To BuffBmpWidthPlusOne Do Begin
               If ((I > 2) And (I < BuffBmpWidthPlusOne)) Then
                  XOff := I-Buff[Buff1][I-1, J]+Buff[Buff1][I+1, J]
               Else
                  XOff := I;

               If ((J > 2) And (J < BuffBmpHeightPlusOne)) Then
                  YOff := J-Buff[Buff1][I, J-1]+Buff[Buff1][I, J+1]
               Else
                  YOff := J;

               If (XOff < 2) Then
                  XOff := 2;

               If (YOff < 2) Then
                  YOff := 2;

               If (XOff > BuffBmpWidthPlusOne) Then
                  XOff := BuffBmpWidthPlusOne;

               If (YOff > BuffBmpHeightPlusOne) Then
                  YOff := BuffBmpHeightPlusOne;

               RealBmpPixels := Pointer(RealBmpOrig+(YOff-2)*RealBmpPitch);

               R := RealBmpPixels[3*XOff];
               G := RealBmpPixels[3*XOff-1];
               B := RealBmpPixels[3*XOff-2];

               BuffVal := Buff[Buff1][I, J];

               ShouldSuspend := ShouldSuspend And (BuffVal = 0);

               Inc(R, BuffVal);
               Inc(G, BuffVal);
               Inc(B, BuffVal);

               If (R < 0) Then
                  R := 0;

               If (G < 0) Then
                  G := 0;

               If (B < 0) Then
                  B := 0;

               If (R > 255) Then
                  R := 255;

               If (G > 255) Then
                  G := 255;

               If (B > 255) Then
                  B := 255;

               BuffBmpPixels[3*I]   := R;
               BuffBmpPixels[3*I-1] := G;
               BuffBmpPixels[3*I-2] := B;
            End;
         End;

         Synchronize(Paint);

         If (ShouldSuspend) Then
{$WARNINGS OFF}
            Suspend;
{$WARNINGS ON}
      End;
   End;
End;

//==============================================================================

Constructor TRippledBitmap.Create(aOwner: TComponent);
Begin
   Inherited;

   RealBmp := TBitmap.Create;
   BuffBmp := TBitmap.Create;

   With RealBmp Do Begin
      HandleType  := bmDIB;     // Just to be on
      PixelFormat := pf24Bit;   // the safe side...
   End;

   With BuffBmp Do Begin
      HandleType  := bmDIB;     // Just to be on
      PixelFormat := pf24Bit;   // the safe side...
   End;

   FBitmap  := TBitmap.Create;
   FRipples := TRBRipples.Create;

   Engine := TRBEngine.Create(Self);
End;

//==============================================================================

Destructor TRippledBitmap.Destroy;
Begin
   Engine.Free;

   FRipples.Free;
   FBitmap.Free;

   ResetBuffs;

   BuffBmp.Free;
   RealBmp.Free;

   Inherited;
End;

//==============================================================================

Procedure TRippledBitmap.SetAutoSize(aValue: Boolean);
Begin
   If (aValue <> AutoSize) Then Begin
      Inherited;

      UpdateRBProps;

      Repaint;
   End;
End;

//==============================================================================

Procedure TRippledBitmap.SetBitmap(Const aValue: TBitmap);
Begin
   FBitmap.Assign(aValue);

   UpdateRBProps;

   Repaint;
End;

//==============================================================================

Procedure TRippledBitmap.SetCenter(Const aValue: Boolean);
Begin
   If (aValue <> FCenter) Then Begin
      FCenter := aValue;

      UpdateRBProps;

      Repaint;
   End;
End;

//==============================================================================

Procedure TRippledBitmap.SetProportional(Const aValue: Boolean);
Begin
   If (aValue <> FProportional) Then Begin
      FProportional := aValue;

      UpdateRBProps;

      Repaint;
   End;
End;

//==============================================================================

Procedure TRippledBitmap.SetStretch(Const aValue: Boolean);
Begin
   If (aValue <> FStretch) Then Begin
      FStretch := aValue;

      UpdateRBProps;

      Repaint;
   End;
End;

//==============================================================================

Procedure TRippledBitmap.ResetBuffs;
Var
   I, K: Integer;
Begin
   For K := 0 To 1 Do Begin
      For I := 0 To High(Buff[K]) Do
         SetLength(Buff[K][I], 0);

      SetLength(Buff[K], 0);
   End;

   Buff0 := 0;
   Buff1 := 1;
End;

//==============================================================================

Procedure TRippledBitmap.UpdateRBProps;
   Function BmpDims: TRect;
   Var
      W, H: Integer;
      XYAspect: Double;
   begin
      W := FBitmap.Width;
      H := FBitmap.Height;

      If (FStretch Or (FProportional And ((W > Width) Or (H > Height)))) Then Begin
         If (FProportional And (W > 0) And (H > 0)) then Begin
            XYAspect := W/H;

            If (W > H) Then Begin
               W := Width;
               H := Trunc(Width/XYAspect);

               If (H > Height) Then Begin
                  H := Height;
                  W := Trunc(Height*XYAspect);
               End;
            End Else Begin
               H := Height;
               W := Trunc(Height*XYAspect);

               If (W > Width) Then Begin
                  W := Width;
                  H := Trunc(Width/XYAspect);
               End;
            End;
         End Else Begin
            W := Width;
            H := Height;
         End;
      End;

      With Result Do Begin
         Top   := 0;
         Right := W;

         Bottom := H;
         Left   := 0;
      End;
   End;

   Procedure BmpOrigAndPitch(aBmp: TBitmap; Var aOrig, aPitch: Integer);
   Begin
      aOrig  := Integer(aBmp.ScanLine[0]);
      aPitch := Integer(aBmp.ScanLine[1])-aOrig;
   End;
Var
   I, J, K: Integer;
   BD: TRect;
Begin
   // Do we really want to resize the bitmap?

   If (AutoSize And Not FBitmap.Empty) Then Begin
      Width  := FBitmap.Width;
      Height := FBitmap.Height;
   End;

   // Determine the real bitmap

   BD := BmpDims;

   With RealBmp Do Begin
      Width  := BD.Right;
      Height := BD.Bottom;

      Canvas.StretchDraw(BD, FBitmap);
   End;

   // Initialise the buffer bitmap

   BuffBmp.Assign(RealBmp);

   // Determine the origin and pitch of the bitmaps (avoids time consuming calls
   // to "ScanLine")

   If (RealBmp.Height <> 0) Then Begin
      BmpOrigAndPitch(RealBmp, RealBmpOrig, RealBmpPitch);
      BmpOrigAndPitch(BuffBmp, BuffBmpOrig, BuffBmpPitch);
   End;

   // Reset the buffers

   ResetBuffs;

   // Initialise the buffers

   If (Not BuffBmp.Empty) Then
      For K := 0 To 1 Do Begin
         SetLength(Buff[K], BuffBmp.Width+4);

         For I := 0 To BuffBmp.Width+3 Do Begin
            SetLength(Buff[K][I], BuffBmp.Height+4);

            For J := 0 To BuffBmp.Height+3 Do
               Buff[K][I, J] := 0;
         End;
      End;
End;

//==============================================================================

Procedure TRippledBitmap.MakeDrip(Const aX, aY: Integer);
Var
   X, Y: Integer;
   I, J: Integer;
   Mult, Diff: Double;
   RealBmpRect: TRect;
Begin
   RealBmpRect := Rect(0, 0, RealBmp.Width-1, RealBmp.Height-1);

   Mult := 16/FRipples.Radius;

   If (FCenter) Then Begin
      X := aX-(Width-RealBmp.Width) Div 2;
      Y := aY-(Height-RealBmp.Height) Div 2;
   End Else Begin
      X := aX;
      Y := aY;
   End;

   For J := Y-FRipples.Radius To Y+FRipples.Radius Do Begin
      For I := X-FRipples.Radius To X+FRipples.Radius Do Begin
         If (PtInRect(RealBmpRect, Point(I, J))) Then Begin
            Diff := FRipples.Radius-Sqrt(Sqr(I-X)+Sqr(J-Y));

            If (Diff >= 0) Then
               Buff[Buff0][I+2, J+2] := Round(Mult*Diff);
         End;
      End;
   End;

{$WARNINGS OFF}
   Engine.Resume;
{$WARNINGS ON}
End;

//==============================================================================

Procedure TRippledBitmap.Paint;
Var
   BmpLoc: TRect;
Begin
   If (csDestroying In ComponentState) Then
      Exit;

   If (BuffBmp.Empty) Then
      UpdateRBProps;

   If (BuffBmp.Empty) Then Begin
      If (csDesigning In ComponentState) Then
         With Inherited Canvas Do Begin
            Pen.Style   := psDash;
            Brush.Style := bsClear;

            Rectangle(0, 0, Width, Height);
         End;
   End Else
      With Inherited Canvas Do Begin
         BmpLoc := Rect(0, 0, RealBmp.Width, RealBmp.Height);

         If (FCenter) Then
            OffsetRect(BmpLoc, (Width-BmpLoc.Right) Div 2, (Height-BmpLoc.Bottom) Div 2);

         StretchDraw(BmpLoc, BuffBmp);
      End;
End;

//==============================================================================

Procedure TRippledBitmap.Resize;
Begin
   UpdateRBProps;
End;

//==============================================================================

Procedure TRippledBitmap.MouseDown(aButton: TMouseButton; aShift: TShiftState;
                                   aX, aY: Integer);
Begin
   If (Assigned(OnMouseDown)) Then
      OnMouseDown(Self, aButton, aShift, aX, aY);

   Dragging := True;

   MakeDrip(aX, aY);

   SetCaptureControl(Self);
End;

//==============================================================================

Procedure TRippledBitmap.MouseMove(aShift: TShiftState; aX, aY: Integer);
Begin
   If (Assigned(OnMouseMove)) Then
      OnMouseMove(Self, aShift, aX, aY);

   If (Dragging) Then
      MakeDrip(aX, aY);
End;

//==============================================================================

Procedure TRippledBitmap.MouseUp(aButton: TMouseButton; aShift: TShiftState;
                                 aX, aY: Integer);
Begin
   Dragging := False;

   SetCaptureControl(Nil);

   If (Assigned(OnMouseUp)) Then
      OnMouseUp(Self, aButton, aShift, aX, aY);
End;

//==============================================================================

Procedure Register;
Begin
   RegisterComponents('COR', [TRippledBitmap]);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

