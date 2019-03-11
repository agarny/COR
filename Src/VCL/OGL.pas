//==============================================================================
// OpenGL component
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 24/10/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit OGL;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, Messages, Classes, Controls, ExtCtrls, Clipbrd, Graphics, OpenGL_SG;

//==============================================================================

{$M+}

//==============================================================================

Type
   POGLBmpRGB = ^TOGLBmpRGB;
   TOGLBmpRGB = Packed Record
      B: Byte;
      G: Byte;
      R: Byte;
   End;
   TOGLBmp = Class(TBitmap)
      Public
         // Constructor & Destructor

         Constructor Create; Override;
   End;
   TOGL = Class;
   TOGL3DPt = Record
      X: Double;
      Y: Double;
      Z: Double;
   End;
   TOGLFontDimension = (fd2D, fd3D);
   TOGLFont = Class
      Private
         // Properties used for internal purposes

         Owner: TOGL;

      Protected
         // Private representation of published properties

         FBase: Integer;

      Public
         // Constructor & Destructor

         Constructor Create(aOGL: TOGL; Const aDimension: TOGLFontDimension; aFont: TFont; aDC: HDC);

      Published
         // Published properties

         Property Base: Integer Read FBase;
   End;
   TOGLQuaternion = Class(TPersistent)
      Protected
         // Private representation of published properties

         FW: Double;
         FX, FY, FZ: Double;

         FAxis: TOGL3DPt;
         FAngle: Double;

         // Methods used for internal purposes

         Procedure UpdateAxisAndAngle;

         // Methods to modify the different published properties

         Procedure SetW(Const aValue: Double); Virtual;

         Procedure SetX(Const aValue: Double); Virtual;
         Procedure SetY(Const aValue: Double); Virtual;
         Procedure SetZ(Const aValue: Double); Virtual;

      Public
         // Constructor & Destructor

         Constructor Create(Const aW: Double = 1; Const aX: Double = 0; Const aY: Double = 0; Const aZ: Double = 0);

         // User's methods

         Procedure EulerMult(Const aX, aY, aZ: Double);

      Published
         // Published properties

         Property W: Double Read FW Write SetW;

         Property X: Double Read FX Write SetX;
         Property Y: Double Read FY Write SetY;
         Property Z: Double Read FZ Write SetZ;

         Property Axis: TOGL3DPt Read FAxis;
         Property Angle: Double Read FAngle;
   End;
   TOGL = Class(TCustomPanel)
      Private
         // Properties used for internal purposes

         RC: HGLRC;

         Font2D, Font2DBmp: TOGLFont;
         Font3D, Font3DBmp: TOGLFont;

         OldColor: TColor;

         OldWidth, OldHeight: Integer;

         // Methods to modify the different published properties

         Procedure SetWinAPI(Const aValue: Boolean);

         // Methods used for internal purposes

         Procedure CreateDCAndRC;
         Procedure DestroyDCAndRC;

         Procedure RenderToBitmap; Overload;

      Protected
         // Private representation of published properties

         FWinAPI: Boolean;

         // Properties used for internal purposes

         DC: THandle;

         NeedOpenGLInit: Boolean;

         NeedWinAPIBackupUpdate: Boolean;

         Backup: TOGLBmp;
         BackupCanvas: TCanvas;

         Buffer: TOGLBmp;
         BufferCanvas: TCanvas;

         Bmp: TOGLBmp;
         BmpCanvas: TCanvas;

         DestCanvas: TCanvas;

         // Special internal events

         FOnInit: TNotifyEvent;    // User's routine for GL initialisation
         FOnPaint: TNotifyEvent;   // User's routine for GL paint

         // Inherited methods

         Procedure Paint; Override;
         Procedure Resize; Override;

         // Methods handling different messages (from Windows or not)

         Procedure WMCreate(Var aMessage: TWMCreate); Message WM_CREATE;
         Procedure WMDestroy(Var aMessage: TWMDestroy); Message WM_DESTROY;
         Procedure WMEraseBkgnd(Var aMessage: TWMEraseBkgnd); Message WM_ERASEBKGND;

         Procedure CMFontChanged(Var aMessage: TMessage); Message CM_FONTCHANGED;

      Public
         // Constructor & Destructor

         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;

         // User's methods

         Procedure ActivateRC; Inline;

         Procedure OGLRepaint; Inline;
         Procedure ForceOGLRepaint; Inline;

         Procedure Text2D(Const aX, aY: Double; Const aText: String; Const aRisky: Boolean = False; Const aXShift: Integer = 0; Const aYShift: Integer = 0); Overload;
         Procedure Text2D(Const aX, aY, aZ: Double; Const aText: String); Overload;
         Procedure Text3D(Const aX, aY, aZ: Double; Const aText: String);

         Procedure RenderToBitmap(Var aBitmap: TBitmap); Overload; Inline;
         Procedure SaveToBitmap(Const aFileName: String); Inline;
         Procedure CopyToClipboard; Inline;

      Published
         // Published properties

         Property WinAPI: Boolean Read FWinAPI Write SetWinAPI Default False;

         Property Align;
         Property Anchors;
         Property Color;
         Property DragCursor;
         Property DragMode;
         Property Enabled;
         Property Font;
         Property ParentBackground;
         Property ParentColor;
         Property ParentFont;
         Property ParentShowHint;
         Property PopupMenu;
         Property ShowHint;
         Property TabOrder;
         Property TabStop;
         Property Visible;

         // Available events

         Property OnClick;
         Property OnContextPopup;
         Property OnDblClick;
         Property OnDragDrop;
         Property OnDragOver;
         Property OnEndDrag;
         Property OnEnter;
         Property OnExit;
         Property OnKeyDown;
         Property OnKeyPress;
         Property OnKeyUp;
         Property OnMouseDown;
         Property OnMouseMove;
         Property OnMouseUp;
         Property OnMouseWheel;
         Property OnMouseWheelDown;
         Property OnMouseWheelUp;
         Property OnResize;
         Property OnStartDrag;

         // OpenGL events

         Property OnInit: TNotifyEvent Read FOnInit Write FOnInit Default Nil;
         Property OnPaint: TNotifyEvent Read FOnPaint Write FOnPaint Default Nil;
   End;

//==============================================================================

Var
   OGLCrtDC: THandle;   // Note: they allow to test the current DC and RC 
   OGLCrtRC: HGLRC;     //       without having to call OpenGL functions...

//==============================================================================

Procedure OGLDrawBuffer(Const aMode: glEnum); Inline;

Procedure ColorToRGBValues(Const aColor: TColor; Var aR, aG, aB: Single);

Procedure OGLClearColor(Const aColor: TColor; Const aAlpha: Single = 1); Inline;
Procedure OGLColor(Const aColor: TColor; Const aAlpha: Single = 1); Inline;
Procedure OGLMaterial(Const aColor: TColor; Const aAlpha: Single =1; Const aFace: glEnum = GL_FRONT; Const aMaterial: glEnum = GL_AMBIENT_AND_DIFFUSE); Inline;

Procedure OGLLineWidth(Const aLineWidth: Double); Inline;
Procedure OGLLineStyle(Const aLineStyle: TPenStyle); Inline;
Procedure OGLLineSmooth(Const aLineSmooth: Boolean); Inline;

Procedure OGLPointSize(Const aPointSize: Double); Inline;

//==============================================================================

Procedure Register;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
   SysUtils;

//==============================================================================

Const
   DegToHalfRad = PI/360;
   HalfRadToDeg = 360/PI;
   One_255      = 1/255;

//==============================================================================

Constructor TOGLBmp.Create;
Begin
   Inherited;

   PixelFormat := pf24Bit;
   // Use less memory and is faster, not to mention that alpha blending can, for
   // instance, easily be done with such a pixel format...
End;

//==============================================================================

Constructor TOGLFont.Create(aOGL: TOGL; Const aDimension: TOGLFontDimension;
                            aFont: TFont; aDC: HDC);
Var
   Font: HFONT;
   Bold, Italic, Underline, StrikeOut: Integer;
   CharacterSet: Integer;
   GlyphsMetrics: Array[0..127] Of GLYPHMETRICSFLOAT;
Begin
   Owner := aOGL;

   Owner.ActivateRC;

   FBase := glGenLists(128);   // Allow space for 128 characters
   // Note: to be completely clean, one would delete the list when desoroying
   //       the font, but that would involve an OpenGL call, which we cannot
   //       make, because of the device context not being available anymore.
   //       Having said that, it shouldn't matter, since we won't need to create
   //       another list...

   If (fsBold In aFont.Style) Then
      Bold := FW_BOLD
   Else
      Bold := 0;

   If (fsItalic In aFont.Style) Then
      Italic := 1
   Else
      Italic := 0;

   If (fsUnderline In aFont.Style) Then
      Underline := 1
   Else
      Underline := 0;

   If (fsStrikeOut In aFont.Style) Then
      StrikeOut := 1
   Else
      StrikeOut := 0;

   If ((CompareStr(aFont.Name, 'Webdings') = 0) Or
       (CompareStr(aFont.Name, 'Wingdings') = 0) Or
       (CompareStr(aFont.Name, 'Wingdings 2') = 0) Or
       (CompareStr(aFont.Name, 'Wingdings 3') = 0)) Then
      CharacterSet := SYMBOL_CHARSET
   Else
      CharacterSet := ANSI_CHARSET;

   Font := CreateFont(aFont.Height,                   // Height
                      0,                              // Width
                      0,                              // Escapement
                      0,                              // Orientation
                      Bold,                           // Bold or not
                      Italic,                         // Italic or not
                      Underline,                      // Underline or not
                      StrikeOut,                      // Strikeout or not
                      CharacterSet,                   // Character set
                      OUT_TT_PRECIS,                  // Output precision
                      CLIP_DEFAULT_PRECIS,            // Clipping precision
                      ANTIALIASED_QUALITY,            // Quality
                      DEFAULT_PITCH Or FF_DONTCARE,   // Pitch and family
                      PChar(aFont.Name));             // Face

   SelectObject(aDC, Font);

   If (aDimension = fd2D) Then
      wglUseFontBitmaps(aDC,     // Device context
                        0,       // First
                        128,     // Count
                        FBase)   // Base
   Else
      wglUseFontOutlines(aDC,                 // Device context
                         0,                   // First
                         128,                 // Count
                         FBase,               // Base
                         0,                   // Deviation
                         0.1,                 // Extrusion
                         WGL_FONT_POLYGONS,   // Format
                         @GlyphsMetrics);     // Glyphs metrics

   DeleteObject(Font);
End;

//==============================================================================

Constructor TOGLQuaternion.Create(Const aW, aX, aY, aZ: Double);
Begin
   FW := aW;

   FX := aX;
   FY := aY;
   FZ := aZ;

   UpdateAxisAndAngle;
End;

//==============================================================================

Procedure TOGLQuaternion.UpdateAxisAndAngle;
Var
   Scale, One_Scale: Double;
Begin
   // Update the axis and the angle

   Scale := Sqrt(Sqr(FX)+Sqr(FY)+Sqr(FZ));

   If (Scale <> 0) Then Begin
      One_Scale := 1/Sqrt(Sqr(FX)+Sqr(FY)+Sqr(FZ));

      FAxis.X := One_Scale*FX;
      FAxis.Y := One_Scale*FY;
      FAxis.Z := One_Scale*FZ;

{$IFDEF OPT_MATH}
      FAngle := HalfRadToDeg*OptArcCos(FW);
{$ELSE}
      FAngle := HalfRadToDeg*ArcCos(FW);
{$ENDIF}
   End Else Begin
      // No valid axis and angle can be determined, so create one around the X
      // axis of angle 0, just so that it doesn't do anything in the end...

      FAxis.X := 1;
      FAxis.Y := 0;
      FAxis.Z := 0;

      FAngle := 0;
   End;
End;

//==============================================================================

Procedure TOGLQuaternion.EulerMult(Const aX, aY, aZ: Double);
Var
   HalfX, HalfY, HalfZ: Double;
   C1, C2, C3: Double;
   S1, S2, S3: Double;
   C1C2, S1S2: Double;
   QuatX, QuatY, QuatZ, QuatW: Double;
   TempW, TempX, TempY, TempZ: Double;
Begin
   If ((aX = 0) And (aY = 0) And (aZ = 0)) Then
      Exit;

   HalfX := DegToHalfRad*aX;
   HalfY := DegToHalfRad*aY;
   HalfZ := DegToHalfRad*aZ;

   C1 := Cos(HalfX);
   C2 := Cos(HalfY);
   C3 := Cos(HalfZ);

   S1 := Sin(HalfX);
   S2 := Sin(HalfY);
   S3 := Sin(HalfZ);

   C1C2 := C1*C2;
   S1S2 := S1*S2;

   QuatX := S1*C2*C3-C1*S2*S3;
   QuatY := C1*S2*C3+S1*C2*S3;
   QuatZ := C1C2*S3-S1S2*C3;

   QuatW := C1C2*C3+S1S2*S3;

   // Note: no need to normalise the quaternion, as it already is...

   TempW := QuatW*FW-QuatX*FX-QuatY*FY-QuatZ*FZ;

   TempX := QuatW*FX+QuatX*FW+QuatY*FZ-QuatZ*FY;
   TempY := QuatW*FY+QuatY*FW+QuatZ*FX-QuatX*FZ;
   TempZ := QuatW*FZ+QuatZ*FW+QuatX*FY-QuatY*FX;

   FW := TempW;

   FX := TempX;
   FY := TempY;
   FZ := TempZ;

   UpdateAxisAndAngle;
End;

//==============================================================================

Procedure TOGLQuaternion.SetW(Const aValue: Double);
Begin
   If (aValue <> FW) Then Begin
      FW := aValue;

      UpdateAxisAndAngle;
   End;
End;

//==============================================================================

Procedure TOGLQuaternion.SetX(Const aValue: Double);
Begin
   If (aValue <> FX) Then Begin
      FX := aValue;

      UpdateAxisAndAngle;
   End;
End;

//==============================================================================

Procedure TOGLQuaternion.SetY(Const aValue: Double);
Begin
   If (aValue <> FY) Then Begin
      FY := aValue;

      UpdateAxisAndAngle;
   End;
End;

//==============================================================================

Procedure TOGLQuaternion.SetZ(Const aValue: Double);
Begin
   If (aValue <> FZ) Then Begin
      FZ := aValue;

      UpdateAxisAndAngle;
   End;
End;

//==============================================================================

Constructor TOGL.Create(aOwner: TComponent);
Begin
   Inherited;

   // Create the backup and buffer images

   Backup := TOGLBmp.Create;

   BackupCanvas := Backup.Canvas;

   Buffer := TOGLBmp.Create;

   BufferCanvas := Buffer.Canvas;

   Bmp := TOGLBmp.Create;

   BmpCanvas := Bmp.Canvas;

   DestCanvas := Canvas;   // Where the rendering is going by default...
End;

//==============================================================================

Destructor TOGL.Destroy;
Begin
   Backup.Free;
   Buffer.Free;
   Bmp.Free;

   Inherited;
End;

//==============================================================================

Procedure TOGL.Paint;
Var
   CR, CG, CB: Single;
Begin
   If (Enabled) Then Begin
      // Render the scene

      If (FWinAPI) Then Begin
         DestCanvas.Font := Font;          // Just in case we must
         DestCanvas.Font.Color := Color;   // use the canvas...

         If ((OldColor <> Color) Or
             (OldWidth <> Width) Or (OldHeight <> Height)) Then Begin
            // The component has a new colour or has been resized, so we need to
            // repaint it all...

            NeedWinAPIBackupUpdate := True;

            OldColor := Color;

            OldWidth  := Width;
            OldHeight := Height;
         End;

         // Dimensions of the buffer images

         Buffer.Width  := Width;
         Buffer.Height := Height;

         // Select the font and its colour, since it's the same throughout the
         // painting

         BufferCanvas.Font := Font;
         BufferCanvas.Font.Color := Color;

         If (Not Assigned(FOnPaint) Or
             ((csDesigning In ComponentState) And Not Assigned(FOnPaint))) Then Begin
            // Clear everything up

            BufferCanvas.Brush.Color := Color;
            BufferCanvas.Pen.Color   := Color;

            BufferCanvas.Rectangle(0, 0, Width+1, Height+1);

            DestCanvas.CopyRect(Rect(0, 0, Width, Height), BufferCanvas, Rect(0, 0, Width, Height));
         End Else If (Assigned(FOnPaint)) Then
            FOnPaint(Self);
      End Else Begin
         ActivateRC;

         // Initialise the OpenGL scene

         // Note: the handling of the init event would ideally be done within
         //       "WMCreate" or "Loaded", BUT in some cases (not quite sure
         //       as what is reponsible for those, but...) to have it in
         //       "WMCreate" results in the handler not being called, while
         //       having it in "Loaded" results in the "Resize" procedure not
         //       being called, so...

         If (NeedOpenGLInit) Then Begin
            NeedOpenGLInit := False;

            Resize;   // To get any viewport settings right, for instance...

            If (Assigned(FOnInit)) Then
               FOnInit(Self);
         End;

         // Render the scene

         If (Not Assigned(FOnPaint) Or
             ((csDesigning In ComponentState) And Not Assigned(FOnPaint))) Then Begin
            // Background colour

            ColorToRGBValues(Color, CR, CG, CB);

            glClearColor(CR, CG, CB, 0);

            // Clear everything up

            glClear(GL_COLOR_BUFFER_BIT);
         End Else If (Assigned(FOnPaint)) Then
            FOnPaint(Self);

         // Swap the buffers in the case of direct rendering (because of it
         // being double buffered)

         If (DestCanvas = Canvas) Then
            SwapBuffers(DC);
      End;
   End;
End;

//==============================================================================

Procedure TOGL.Resize;
Begin
   // Resize the scene

   If (Not FWinAPI) Then
      ActivateRC;

   Inherited;
End;

//==============================================================================

Procedure TOGL.CreateDCAndRC;
Var
   DummyPalette: HPALETTE;
Begin
   If (DestCanvas = Canvas) Then Begin
      // Get a device context

      DC := GetDC(Handle);

      // Create the main rendering context

      RC := CreateRenderingContext(DC, [opDoubleBuffered], 24, 0, 0, 0, 0, 0, DummyPalette);
   End Else Begin
      // Get a bitmap device context

      DC := BmpCanvas.Handle;

      // Create a bitmap rendering context

      RC := CreateRenderingContext(DC, [], 24, 0, 0, 0, 0, 0, DummyPalette);
   End;

   // Activate the rendering context

   ActivateRC;

   // Create the 2D and 3D fonts

   If (DestCanvas = Canvas) Then Begin
      Font2D := TOGLFont.Create(Self, fd2D, Font, DC);
      Font3D := TOGLFont.Create(Self, fd3D, Font, DC);
   End Else Begin
      Font2DBmp := TOGLFont.Create(Self, fd2D, Font, DC);
      Font3DBmp := TOGLFont.Create(Self, fd3D, Font, DC);
   End;

   // Get the OpenGL scene to initialise

   NeedOpenGLInit := True;
End;

//==============================================================================

Procedure TOGL.DestroyDCAndRC;
Begin
   If (HasActiveContext) Then
      DeactivateRenderingContext;

   // Destroy the rendering context

   DestroyRenderingContext(RC);

   // Release the device context

   ReleaseDC(Handle, DC);

   DC := 0;   // Just to be on the safe side...

   // Destroy the fonts

   If (DestCanvas = Canvas) Then Begin
      Font2D.Free;
      Font3D.Free;
   End Else Begin
      Font2DBmp.Free;
      Font3DBmp.Free;
   End;
End;

//==============================================================================

Procedure TOGL.WMCreate(Var aMessage: TWMCreate);
Begin
   Inherited;

   // Create the device and rendering contexts

   CreateDCAndRC;
End;

//==============================================================================

Procedure TOGL.WMDestroy(Var aMessage: TWMDestroy);
Begin
   // Destroy the device and rendering contexts

   DestroyDCAndRC;

   Inherited;
End;

//==============================================================================

Procedure TOGL.WMEraseBkgnd(Var aMessage: TWMEraseBkgnd);
Begin
   // Note: normally, we would call "Inherited" when overriding a message, but
   //       in the present case that's exactly what we don'twant to do, as this
   //       will prevent the background from being erased, so...

   aMessage.Result := 1;   // The message has been handled
End;

//==============================================================================

Procedure TOGL.CMFontChanged(Var aMessage: TMessage);
Begin
   Inherited;

   If (DC <> 0) Then Begin
      Font2D.Free;
      Font3D.Free;

      Font2D := TOGLFont.Create(Self, fd2D, Font, DC);
      Font3D := TOGLFont.Create(Self, fd3D, Font, DC);

      OGLRepaint;
   End;
End;

//==============================================================================

Procedure TOGL.SetWinAPI(Const aValue: Boolean);
Begin
   If (aValue <> FWinAPI) Then Begin
      FWinAPI := aValue;

      If (FWinAPI) Then
         NeedWinAPIBackupUpdate := True
         // Note: necessary, since we cannot assume what happened when we were
         //       in OpenGL mode...
      Else
         Resize;   // In case the dimensions of the component have changed in
                   // between...

      OGLRepaint;
   End;
End;

//==============================================================================

Procedure TOGL.ActivateRC;
Begin
   // Activate the rendering context

   If ((OGLCrtDC <> DC) Or (OGLCrtRC <> RC)) Then Begin
      If (HasActiveContext) Then
         DeactivateRenderingContext;

      ActivateRenderingContext(DC, RC);

      OGLCrtDC := DC;
      OGLCrtRC := RC;
   End;
End;

//==============================================================================

Procedure TOGL.OGLRepaint;
Begin
   // Repaint the scene. Note that in the case of off-screen rendering, we MUST
   // NOT call "Repaint", as this will have no effect whatsoever if the
   // application is minimised. So, instead, we MUST call "Paint" directly, just
   // to be 100% sure that the scene will be repainted...

   If (DestCanvas = Canvas) Then
      Repaint
   Else
      Paint;
End;

//==============================================================================

Procedure TOGL.ForceOGLRepaint;
Begin
   NeedWinAPIBackupUpdate := True;   // Just to ensure that the Win API version
                                     // will be fully rendered...

   OGLRepaint;
End;

//==============================================================================

Procedure TOGL.Text2D(Const aX, aY: Double; Const aText: String;
                      Const aRisky: Boolean; Const aXShift, aYShift: Integer);
Begin
   If (Not FWinAPI And (CompareStr(aText, '') <> 0)) Then Begin
      glRasterPos2f(aX, aY);

      If (aRisky) Then
         // In case we want to display some text that is partly outside the view

         glBitmap(0, 0, 0, 0, aXShift, aYShift, Nil);

      glPushAttrib(GL_LIST_BIT);
         If (DestCanvas = Canvas) Then
            glListBase(Font2D.Base)
         Else
            glListBase(Font2DBmp.Base);

         glCallLists(Length(aText), GL_UNSIGNED_BYTE, PChar(aText));
      glPopAttrib;
   End;
End;

//==============================================================================

Procedure TOGL.Text2D(Const aX, aY, aZ: Double; Const aText: String);
Begin
   If (Not FWinAPI And (CompareStr(aText, '') <> 0)) Then Begin
      glRasterPos3f(aX, aY, aZ);

      glPushAttrib(GL_LIST_BIT);
         If (DestCanvas = Canvas) Then
            glListBase(Font2D.Base)
         Else
            glListBase(Font2DBmp.Base);

         glCallLists(Length(aText), GL_UNSIGNED_BYTE, PChar(aText));
      glPopAttrib;
   End;
End;

//==============================================================================

Procedure TOGL.Text3D(Const aX, aY, aZ: Double; Const aText: String);
Begin
   If (Not FWinAPI And (CompareStr(aText, '') <> 0)) Then Begin
      glPushMatrix;
         glTranslatef(aX, aY, aZ);

         glPushAttrib(GL_LIST_BIT);
            If (DestCanvas = Canvas) Then
               glListBase(Font3D.Base)
            Else
               glListBase(Font3DBmp.Base);

            glCallLists(Length(aText), GL_UNSIGNED_BYTE, PChar(aText));
         glPopAttrib;
      glPopMatrix;
   End;
End;

//==============================================================================

Procedure TOGL.RenderToBitmap;
Begin
   // Make sure the dimensions of the bitmap are up to date

   Bmp.Width  := Width;
   Bmp.Height := Height;

   // Use the bitmap's canvas instead of the component's

   If (FWinAPI) Then Begin
      NeedWinAPIBackupUpdate := True;   // Just to ensure that the Win API
                                        // version will be fully rendered...

      DestCanvas := BmpCanvas;

      OGLRepaint;

      DestCanvas := Canvas;

      NeedWinAPIBackupUpdate := True;   // Just to ensure that the Win API
                                        // version will be fully rendered...
   End Else Begin
      // Destroy the current device and rendering contexts

      DestroyDCAndRC;

      // Set the new device and rendering contexts

      DestCanvas := BmpCanvas;

      CreateDCAndRC;

      // Render the scene to the bitmap

      OGLRepaint;

      // Destroy the temporary rendering context

      DestroyDCAndRC;

      // Recreate the original device and rendering contexts

      DestCanvas := Canvas;

      CreateDCAndRC;
   End;
End;

//==============================================================================

Procedure TOGL.RenderToBitmap(Var aBitmap: TBitmap);
Begin
   RenderToBitmap;

   aBitmap.Assign(Bmp);
End;

//==============================================================================

Procedure TOGL.SaveToBitmap(Const aFileName: String);
Begin
   RenderToBitmap;

   Bmp.SaveToFile(aFileName);
End;

//==============================================================================

Procedure TOGL.CopyToClipboard;
Begin
   RenderToBitmap;

   Clipboard.Assign(Bmp);
End;

//==============================================================================

Procedure OGLDrawBuffer(Const aMode: glEnum);
Begin
   glDrawBuffer(aMode);
End;

//==============================================================================

Procedure ColorToRGBValues(Const aColor: TColor; Var aR, aG, aB: Single);
Var
   RGBValues: Longint;
Begin
   If (aColor < 0) Then
      RGBValues := GetSysColor(aColor And $000000FF)
   Else
      RGBValues := aColor;

   aR := One_255*(RGBValues Mod 256);
   aG := One_255*((RGBValues ShR 8) Mod 256);
   aB := One_255*(RGBValues ShR 16);
End;

//==============================================================================

Procedure OGLClearColor(Const aColor: TColor; Const aAlpha: Single);
Var
   CR, CG, CB: Single;
Begin
   ColorToRGBValues(aColor, CR, CG, CB);

   glClearColor(CR, CG, CB, aAlpha);
End;

//==============================================================================

Procedure OGLColor(Const aColor: TColor; Const aAlpha: Single);
Var
   CR, CG, CB: Single;
Begin
   ColorToRGBValues(aColor, CR, CG, CB);

   glColor4f(CR, CG, CB, aAlpha);
End;

//==============================================================================

Procedure OGLMaterial(Const aColor: TColor; Const aAlpha: Single;
                      Const aFace, aMaterial: glEnum);
Var
   Material: Array[0..3] Of Single;
Begin
   ColorToRGBValues(aColor, Material[0], Material[1], Material[2]);

   Material[3] := aAlpha;

   glMaterialfv(aFace, aMaterial, @Material);
End;

//==============================================================================

Procedure OGLLineWidth(Const aLineWidth: Double);
Begin
   glLineWidth(aLineWidth);
End;

//==============================================================================

Procedure OGLLineStyle(Const aLineStyle: TPenStyle);
Begin
   Case aLineStyle Of
      psDash:
         glLineStipple(1, $0FFF);
      psDot:
         glLineStipple(1, $3333);
      psDashDot:
         glLineStipple(1, $0C3F);
      psDashDotDot:
         glLineStipple(1, $33FF);
      psClear:
         glLineStipple(1, $0000);
   Else   // psSolid
      glLineStipple(1, $FFFF);
   End;
End;

//==============================================================================

Procedure OGLLineSmooth(Const aLineSmooth: Boolean);
Begin
   If (aLineSmooth) Then
      glEnable(GL_LINE_SMOOTH)   // Enable antialiasing
   Else
      glDisable(GL_LINE_SMOOTH);   // Disable antialiasing
End;

//==============================================================================

Procedure OGLPointSize(Const aPointSize: Double);
Begin
   glPointSize(aPointSize);
End;

//==============================================================================

Procedure Register;
Begin
   RegisterComponents('COR', [TOGL]);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

