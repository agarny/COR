//==============================================================================
// Command graph component
//                
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 01/06/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CmdGraph;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, Classes, ExtCtrls, Graphics;

//==============================================================================

{$M+}

//==============================================================================

Type
   TCmdGraphOptions = Class
      Private
         // Methods to modify the different published properties

         Procedure SetOptimisedFontSize(Const aValue: Boolean); Inline;
         Procedure SetUnderscoreForSub(Const aValue: Boolean); Inline;
         Procedure SetGreekSymbols(Const aValue: Boolean); Inline;
         Procedure SetDigitGrouping(Const aValue: Boolean); Inline;
         Procedure SetFontName(Const aValue: String); Inline;
         Procedure SetFontSize(Const aValue: Integer); Inline;
         Procedure SetSubFont(Const aValue: Integer); Inline;
         Procedure SetBackgroundColor(Const aValue: TColor); Inline;
         Procedure SetForegroundColor(Const aValue: TColor); Inline;

      Protected
         // Private representation of published properties

         FOptimisedFontSize: Boolean;
         FUnderscoreForSub: Boolean;
         FGreekSymbols: Boolean;
         FDigitGrouping: Boolean;
         FFontName: String;
         FFontSize: Integer;
         FSubFont: Integer;
         FBackgroundColor: TColor;
         FForegroundColor: TColor;

      Public
         // User's methods

         Procedure Copy(Const aCommandViewerOptions: TCmdGraphOptions);

      Published
         // Published properties

         Property OptimisedFontSize: Boolean Read FOptimisedFontSize Write SetOptimisedFontSize Default True;
         Property UnderscoreForSub: Boolean Read FUnderscoreForSub Write SetUnderscoreForSub Default True;
         Property GreekSymbols: Boolean Read FGreekSymbols Write SetGreekSymbols Default True;
         Property DigitGrouping: Boolean Read FDigitGrouping Write SetDigitGrouping Default True;
         Property FontName: String Read FFontName Write SetFontName;
         Property FontSize: Integer Read FFontSize Write SetFontSize Default 10;
         Property SubFont: Integer Read FSubFont Write SetSubFont Default 50;
         Property BackgroundColor: TColor Read FBackgroundColor Write SetBackgroundColor;
         Property ForegroundColor: TColor Read FForegroundColor Write SetForegroundColor;
   End;
   TCmdBinTreeItemType = (itSym,
                          itConcatenate,
                          itUnit,
                          itProperties, itProperty,
                          itPiecewise, itPiece, itOtherwise,
                          itEq, itNEq, itGT, itLT, itGEq, itLEq,
                          itPlus, itMinus, itTimes, itDivide, itPow, itRoot, itAbs, itExp, itLN, itLog, itFloor, itCeil, itFact,
                          itAnd, itOr, itXOr, itNot,
                          itDiff,
                          itDegree, itLogBase,
                          itSin, itCos, itTan, itSec, itCsc, itCot, itSinH, itCosH, itTanH, itSecH, itCscH, itCotH, itASin, itACos, itATan, itASec, itACsc, itACot, itASinH, itACosH, itATanH, itASecH, itACscH, itACotH);
   TCmdBinTree = Class
      Public
         // Public properties

         ItemType: TCmdBinTreeItemType;

         Item: String;

         Left: TCmdBinTree;
         Right: TCmdBinTree;

         Baseline: Integer;

         // Constructor & Destructor

         Constructor Create(Const aItemType: TCmdBinTreeItemType; Const aItem: String = '');
         Destructor Destroy; Override;
   End;
   TCmdGraph = Class(TCustomPanel)
      Private
         // Methods to modify the different published properties

         Procedure SetCmdBinTree(Const aValue: TCmdBinTree); Inline;
         Procedure SetInvalidBitmap(aValue: TBitmap); Inline;
         Procedure SetInvalid(Const aValue: Boolean); Inline;
         Procedure SetOptimisedFontSize(Const aValue: Boolean); Inline;
         Procedure SetUnderscoreForSub(Const aValue: Boolean); Inline;
         Procedure SetGreekSymbols(Const aValue: Boolean); Inline;
         Procedure SetDigitGrouping(Const aValue: Boolean); Inline;
         Procedure SetSubFont(Const aValue: Byte); Inline;

         Procedure SetMonochrome(Const aValue: Boolean); Inline;

         Function GetIdealWidth: Integer;
         Function GetIdealHeight: Integer;

         // Methods used for internal purposes

         Function GetBarSize(Const aFontSize: Integer): Integer;

         Function CreateBmp(Const aFontSize: Integer = 0; Const aBarSize: Integer = 0): TBitmap;

         Function AddSym(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;

         Function CreateCurlyBracket(Const aHeight: Integer; Const aFontSize: Integer): TBitmap;
         Procedure AddCurlyBracket(Var aBuffer: TBitmap; Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer);

         Function CreateParenthesis(Const aHeight: Integer; Const aFontSize: Integer; Const aLeftParenthesis: Boolean): TBitmap;
         Procedure AddParentheses(Var aBuffer: TBitmap; Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer);

         Function AddOperand(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;

         Function AddFunc(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;

         Procedure AddDiffD(Var aBuffer: TBitmap; Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer);
         Procedure AddDivider(Var aBuffer: TBitmap; Const aLeft: TBitmap; Const aRight: TBitmap; Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer);

         Function AddUnit(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddConcatenateOrProperties(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer; Const aProperties: Boolean): TBitmap;
         Function AddPropertyOrPiecewise(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddPiece(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddOtherwise(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddDiff(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddTimes(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddDivide(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddPow(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddRoot(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddAbs(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddExp(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddFloor(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddCeil(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;
         Function AddFact(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;

      Protected
         // Private representation of published properties

         FCmdBinTree: TCmdBinTree;

         FInvalidBitmap: TBitmap;

         FInvalid: Boolean;
         FOptimisedFontSize: Boolean;
         FUnderscoreForSub: Boolean;
         FGreekSymbols: Boolean;
         FDigitGrouping: Boolean;
         FSubFont: Byte;

         FMonochrome: Boolean;
         // Note: this property must not be part of "TCmdGraphOptions", since it
         //       is only to be accessed by the programmer, not the user... The
         //       user should, indeed, always want the best quality possible,
         //       while the programmer may require a monochrome version for
         //       memory reasons...

         // Inherited methods

         Procedure Paint; Override;

      Public
         // Constructor & Destructor
                                                 
         Constructor Create(aOwner: TComponent); Override;
         Destructor Destroy; Override;

         // User's methods

         Procedure SetOptions(Const aValue: TCmdGraphOptions);

         Function Render(Var aCmdBinTree: TCmdBinTree; Const aFontSize: Integer): TBitmap;

         Function Bitmap: TBitmap;
         Procedure CopyToClipboard;

      Published
         // Published properties

         Property Align;
         Property Color;
         Property Font;
         Property PopupMenu;

         Property CmdBinTree: TCmdBinTree Read FCmdBinTree Write SetCmdBinTree;
         Property IdealWidth: Integer Read GetIdealWidth;
         Property IdealHeight: Integer Read GetIdealHeight;
         Property InvalidBitmap: TBitmap Read FInvalidBitmap Write SetInvalidBitmap;
         Property Invalid: Boolean Read FInvalid Write SetInvalid Default False;
         Property OptimisedFontSize : Boolean Read FOptimisedFontSize Write SetOptimisedFontSize Default True;
         Property UnderscoreForSub: Boolean Read FUnderscoreForSub Write SetUnderscoreForSub Default True;
         Property GreekSymbols : Boolean Read FGreekSymbols Write SetGreekSymbols Default True;
         Property DigitGrouping : Boolean Read FDigitGrouping Write SetDigitGrouping Default True;
         Property SubFont: Byte Read FSubFont Write SetSubFont Default 50;

         Property Monochrome: Boolean Read FMonochrome Write SetMonochrome Default False;

         Property OnCanResize;
         Property OnClick;
         Property OnConstrainedResize;
         Property OnContextPopup;
         Property OnDblClick;
         Property OnDockDrop;
         Property OnDockOver;
         Property OnDragDrop;
         Property OnDragOver;
         Property OnEndDock;
         Property OnEndDrag;
         Property OnEnter;
         Property OnExit;
         Property OnGetSiteInfo;
         Property OnMouseDown;
         Property OnMouseMove;
         Property OnMouseUp;
         Property OnResize;
         Property OnStartDock;
         Property OnStartDrag;
         Property OnUnDock;
   End;

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
   SysUtils, Controls, Clipbrd, StrUtils, Common;

//==============================================================================

Procedure TCmdGraphOptions.SetOptimisedFontSize(Const aValue: Boolean);
Begin
   If (aValue <> FOptimisedFontSize) Then
      FOptimisedFontSize := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetUnderscoreForSub(Const aValue: Boolean);
Begin
   If (aValue <> FUnderscoreForSub) Then
      FUnderscoreForSub := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetGreekSymbols(Const aValue: Boolean);
Begin
   If (aValue <> FGreekSymbols) Then
      FGreekSymbols := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetDigitGrouping(Const aValue: Boolean);
Begin
   If (aValue <> FDigitGrouping) Then
      FDigitGrouping := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetFontName(Const aValue: String);
Begin
   If (CompareStr(aValue, FFontName) <> 0) Then
      FFontName := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetFontSize(Const aValue: Integer);
Begin
   If (aValue <> FFontSize) Then
      FFontSize := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetSubFont(Const aValue: Integer);
Begin
   If ((aValue <> FSubFont) And
       (aValue >= 10) And (aValue <= 90)) Then 
      FSubFont := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetBackgroundColor(Const aValue: TColor);
Begin
   If (aValue <> FBackgroundColor) Then
      FBackgroundColor := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.SetForegroundColor(Const aValue: TColor);
Begin
   If (aValue <> FForegroundColor) Then
      FForegroundColor := aValue;
End;

//==============================================================================

Procedure TCmdGraphOptions.Copy(Const aCommandViewerOptions: TCmdGraphOptions);
Begin
   FOptimisedFontSize := aCommandViewerOptions.OptimisedFontSize;
   FUnderscoreForSub  := aCommandViewerOptions.UnderscoreForSub;
   FGreekSymbols      := aCommandViewerOptions.GreekSymbols;
   FDigitGrouping     := aCommandViewerOptions.DigitGrouping;
   FFontName          := aCommandViewerOptions.FontName;
   FFontSize          := aCommandViewerOptions.FontSize;
   FSubFont           := aCommandViewerOptions.SubFont;

   FBackgroundColor := aCommandViewerOptions.BackgroundColor;
   FForegroundColor := aCommandViewerOptions.ForegroundColor;
End;

//==============================================================================

Constructor TCmdBinTree.Create(Const aItemType: TCmdBinTreeItemType;
                               Const aItem: String);
Begin
   ItemType := aItemType;

   Case aItemType Of
      itEq:    Item := ' = ';
      itNEq:   Item := ' '+Chr(185)+' ';
      itGT:    Item := ' > ';
      itLT:    Item := ' < ';
      itGEq:   Item := ' '+Chr(179)+' ';
      itLEq:   Item := ' '+Chr(163)+' ';
      itPlus:  Item := '+';
      itMinus: Item := '-';
      itExp:   Item := 'e';
      itLN:    Item := 'ln';
      itLog:   Item := 'log';
      itFact:  Item := '!';
      itAnd:   Item := ' and ';
      itOr:    Item := ' or ';
      itXOr:   Item := ' xor ';
      itNot:   Item := 'not';
      itSin:   Item := 'sin';
      itCos:   Item := 'cos';
      itTan:   Item := 'tan';
      itSec:   Item := 'sec';
      itCsc:   Item := 'csc';
      itCot:   Item := 'cot';
      itSinH:  Item := 'sinh';
      itCosH:  Item := 'cosh';
      itTanH:  Item := 'tanh';
      itSecH:  Item := 'sech';
      itCscH:  Item := 'csch';
      itCotH:  Item := 'coth';
      itASin:  Item := 'asin';
      itACos:  Item := 'acos';
      itATan:  Item := 'atan';
      itASec:  Item := 'asec';
      itACsc:  Item := 'acsc';
      itACot:  Item := 'acot';
      itASinH: Item := 'asinh';
      itACosH: Item := 'acosh';
      itATanH: Item := 'atanh';
      itASecH: Item := 'asech';
      itACscH: Item := 'acsch';
      itACotH: Item := 'acoth';
   Else
      Item := aItem;
   End;

   Left  := Nil;
   Right := Nil;
End;

//==============================================================================

Destructor TCmdBinTree.Destroy;
Begin
   Left.Free;
   Right.Free;
End;

//==============================================================================

Constructor TCmdGraph.Create(aOwner: TComponent);
Begin
   Inherited;

   ParentBackground := False;   // Very important when using XP themes, as
                                // otherwise it will flicker

   DoubleBuffered := True;

   FCmdBinTree := Nil;

   FInvalidBitmap := TBitmap.Create;

   FInvalid := False;
   FOptimisedFontSize := True;
   FUnderscoreForSub := True;
   FGreekSymbols := True;
   FDigitGrouping := True;
   FSubFont := 50;   // Percentage of the default size

   FMonochrome := False;
End;

//==============================================================================

Destructor TCmdGraph.Destroy;
Begin
   FInvalidBitmap.Free;

   Inherited;
End;

//==============================================================================

Function TCmdGraph.GetBarSize(Const aFontSize: Integer): Integer;
Type
   PRGB = ^TRGB;
   TRGB = Packed Record
      B: Byte;
      G: Byte;
      R: Byte;
   End;
Const
   DivChar = '_';
Var
   SL: PByte;
   SLDelta: Integer;
   P: PRGB;
   ColorRGB, FontColorRGB: Longint;
Begin
   With TBitmap.Create Do Begin
      PixelFormat := pf24Bit;

      Canvas.Brush.Color := Color;

      Canvas.Font := Font;
      Canvas.Font.Size := aFontSize;

      Width  := Canvas.TextWidth(DivChar);
      Height := Canvas.TextHeight(DivChar);

      Canvas.TextOut(0, 0, DivChar);

      ColorRGB := ColorToRGB(Color);
      FontColorRGB := ColorToRGB(Font.Color);

      SL := ScanLine[0];
      SLDelta := Integer(ScanLine[1])-Integer(SL);

      Inc(SL, (Height-1)*SLDelta+3*(Width Div 2));

      P := PRGB(SL);

      // Look for the beginning of the underscore character

      While (P.R+P.G ShL 8+P.B ShL 16 = ColorRGB) Do Begin
         Dec(SL, SLDelta);

         P := PRGB(SL);
      End;

      // Look for the end of the end of the underscore character which will give
      // us its height and therefore the bar size

      Result := 0;

      While (P.R+P.G ShL 8+P.B ShL 16 = FontColorRGB) Do Begin
         Dec(SL, SLDelta);

         P := PRGB(SL);

         Inc(Result);
      End;

      If (Result = 0) Then
         Result := 1;

      Free;
   End;
End;

//==============================================================================

Function TCmdGraph.Render(Var aCmdBinTree: TCmdBinTree;
                          Const aFontSize: Integer): TBitmap;
Type
   TCmdBinTreeOpType = (otOperand, otFunc, otOwn, otNone);
Var
   OpType: TCmdBinTreeOpType;
Begin
   Result := Nil;   // Just to avoid the warning...

   // Determine the type of operator we are dealing with

   Case aCmdBinTree.ItemType Of
      itEq, itNEq, itGT, itLT, itGEq, itLEq,
      itAnd, itOr, itXOr:
         OpType := otOperand;
      itPlus, itMinus:
         If (aCmdBinTree.Right <> Nil) Then
            OpType := otOperand
         Else
            OpType := otFunc;
      itConcatenate,
      itUnit,
      itProperties, itProperty,
      itPiecewise, itPiece, itOtherwise,
      itTimes, itDivide, itPow, itRoot, itAbs, itExp, itFloor, itCeil, itFact,
      itDiff:
         OpType := otOwn;
      itSym:
         OpType := otNone;
   Else
      OpType := otFunc;
   End;

   // Combine the bitmap with the sub-bitmaps, if any, and this based on the 
   // type of the operator

   Case OpType Of
      otOperand:
         Result := AddOperand(aCmdBinTree, aFontSize);
      otFunc:
         Result := AddFunc(aCmdBinTree, aFontSize);
      otOwn:
         Case aCmdBinTree.ItemType Of
            itConcatenate:
               Result := AddConcatenateOrProperties(aCmdBinTree, aFontSize, False);
            itUnit:
               Result := AddUnit(aCmdBinTree, aFontSize);
            itProperties:
               Result := AddConcatenateOrProperties(aCmdBinTree, aFontSize, True);
            itProperty, itPiecewise:
               Result := AddPropertyOrPiecewise(aCmdBinTree, aFontSize);
            itPiece:
               Result := AddPiece(aCmdBinTree, aFontSize);
            itOtherwise:
               Result := AddOtherwise(aCmdBinTree, aFontSize);
            itDiff:
               Result := AddDiff(aCmdBinTree, aFontSize);
            itTimes:
               Result := AddTimes(aCmdBinTree, aFontSize);
            itDivide:
               Result := AddDivide(aCmdBinTree, aFontSize);
            itPow:
               Result := AddPow(aCmdBinTree, aFontSize);
            itRoot:
               Result := AddRoot(aCmdBinTree, aFontSize);
            itAbs:
               Result := AddAbs(aCmdBinTree, aFontSize);
            itExp:
               Result := AddExp(aCmdBinTree, aFontSize);
            itFloor:
               Result := AddFloor(aCmdBinTree, aFontSize);
            itCeil:
               Result := AddCeil(aCmdBinTree, aFontSize);
            itFact:
               Result := AddFact(aCmdBinTree, aFontSize);
         End;
   Else
      Result := AddSym(aCmdBinTree, aFontSize);
   End;
End;

//==============================================================================

Function TCmdGraph.CreateBmp(Const aFontSize: Integer;
                             Const aBarSize: Integer): TBitmap;
Begin
   Result := TBitmap.Create;

   With Result Do Begin
      If (FMonochrome) Then Begin
         PixelFormat := pf1Bit;

         Canvas.Brush.Color := clWhite;

         // Either the font or the pen is used, so...

         Canvas.Font := Font;
         Canvas.Font.Size  := aFontSize;
         Canvas.Font.Color := clBlack;

         Canvas.Pen.Color := clBlack;
         Canvas.Pen.Width := aBarSize;
      End Else Begin
         PixelFormat := pf24Bit;   // We don't need more than that and it's
                                   // bound to be a bit faster at least, so...

         Canvas.Brush.Color := Color;

         // Either the font or the pen is used, so...

         Canvas.Font := Font;
         Canvas.Font.Size := aFontSize;

         Canvas.Pen.Color := Font.Color;
         Canvas.Pen.Width := aBarSize;
      End;
   End;
End;

//==============================================================================

Function TCmdGraph.AddSym(Var aCmdBinTree: TCmdBinTree;
                          Const aFontSize: Integer): TBitmap;
   Function AddSubSym(Const aSym: String; Const aFontSize: Integer;
                      Const aItalics: Boolean = True): TBitmap;
   Const
      GREEK_ALPHABET: Array[0..23, 0..1] Of String = (('alpha',   Chr(Ord('a'))),
                                                      ('beta',    Chr(Ord('b'))),
                                                      ('gamma',   Chr(Ord('g'))),
                                                      ('delta',   Chr(Ord('d'))),
                                                      ('epsilon', Chr(Ord('e'))),
                                                      ('zeta',    Chr(Ord('z'))),
                                                      ('eta',     Chr(Ord('h'))),
                                                      ('theta',   Chr(Ord('q'))),
                                                      ('iota',    Chr(Ord('i'))),
                                                      ('kappa',   Chr(Ord('k'))),
                                                      ('lambda',  Chr(Ord('l'))),
                                                      ('mu',      Chr(Ord('m'))),
                                                      ('nu',      Chr(Ord('n'))),
                                                      ('xi',      Chr(Ord('x'))),
                                                      ('omicron', Chr(Ord('o'))),
                                                      ('pi',      Chr(Ord('p'))),
                                                      ('rho',     Chr(Ord('r'))),
                                                      ('sigma',   Chr(Ord('s'))),
                                                      ('tau',     Chr(Ord('t'))),
                                                      ('upsilon', Chr(Ord('u'))),
                                                      ('phi',     Chr(Ord('f'))),
                                                      ('chi',     Chr(Ord('c'))),
                                                      ('psi',     Chr(Ord('y'))),
                                                      ('omega',   Chr(Ord('w'))));
   Var
      ABC: Array[0..255] Of TABC;
      I, A, C, RelLeft: Integer;
      RealSym: String;
   Begin
      Result := CreateBmp(aFontSize);

      If (CompareStr(aSym, '') = 0) Then
         Exit;

      RealSym := aSym;

      If (FGreekSymbols And (CompareStr(LowerCase(RealSym), 'infinity') = 0)) Then Begin
         // We want Greek symbols and we are dealing with the special case of
         // the infinity character (which we don't want to be displayed in
         // italics!)

         Result.Canvas.Font.Name := 'Symbol';

         RealSym := Chr(165);
      End Else Begin
         If (FGreekSymbols) Then
            For I := 0 To High(GREEK_ALPHABET) Do
               If (CompareStr(LowerCase(RealSym), GREEK_ALPHABET[I, 0]) = 0) Then Begin
                  Result.Canvas.Font.Name := 'Symbol';

                  RealSym := GREEK_ALPHABET[I, 1];
               End;

         If (aItalics) Then
            Result.Canvas.Font.Style := [fsItalic];
      End;

      // New dimensions of the bitmap

      Result.Height := Result.Canvas.TextHeight(RealSym);
      Result.Width  := Result.Canvas.TextWidth(RealSym);

      RelLeft := 0;

      If (GetCharABCWidths(Result.Canvas.Handle, 0, 255, ABC)) Then Begin
         Result.Width := 0;

         For I := 1 To Length(RealSym) Do
            Result.Width := Result.Width+ABC[Ord(RealSym[I])].abcA+Integer(ABC[Ord(RealSym[I])].abcB)+ABC[Ord(RealSym[I])].abcC;

         A := ABC[Ord(RealSym[1])].abcA;

         If (A < 0) Then Begin
            RelLeft := -A;

            Result.Width := Result.Width-A;
         End;

         C := ABC[Ord(RealSym[Length(RealSym)])].abcC;

         If (C < 0) Then
            Result.Width := Result.Width-C;
      End;

      Result.Canvas.TextOut(RelLeft, 0, RealSym);
   End;
Var
   I: Integer;
   Sym, NewSym: String;
   Val: Double;
   SubSyms: TStringList;
   UnderscorePos: Integer;
Begin
   // Determine the sub-symbols to be added

   Sym := aCmdBinTree.Item;

   If (TryStrToFloat(Sym, Val)) Then Begin
      If (FDigitGrouping) Then
         Result := AddSubSym(GroupDigits(Sym), aFontSize, False)
      Else
         Result := AddSubSym(Sym, aFontSize, False);
   End Else Begin
      // Other type of symbol, so...

      SubSyms := TStringList.Create;

      // Use the underscore to specify subscripts, if required

      If (FUnderscoreForSub) Then Begin
         // Clean the symbol (i.e. no trailing "_" and no duplicates)

         NewSym := ReplaceStr(Sym, '__', '_');

         While CompareStr(NewSym, Sym) <> 0 Do Begin
            Sym := NewSym;

            NewSym := ReplaceStr(Sym, '__', '_');
         End;

         If (CompareStr(Sym, '') <> 0) Then Begin
            If (Sym[1] = '_') Then
               Sym := Copy(Sym, 2, Length(Sym)-1);

            If (Sym[Length(Sym)] = '_') Then
               Sym := Copy(Sym, 1, Length(Sym)-1);
         End;

         // Extract the sub-symbols

         If (CompareStr(Sym, '') <> 0) Then Begin
            UnderscorePos := Pos('_', Sym);

            While UnderscorePos <> 0 Do Begin
               SubSyms.Add(Copy(Sym, 1, UnderscorePos-1));

               Sym := Copy(Sym, UnderscorePos+1, Length(Sym)-UnderscorePos);

               UnderscorePos := Pos('_', Sym);
            End;
         End;
      End;

      SubSyms.Add(Sym);

      // Render the various sub-symbols

      Result := AddSubSym(SubSyms.Strings[0], aFontSize);

      If (SubSyms.Count > 1) Then
         For I := 1 To SubSyms.Count-1 Do
{$IFDEF OPT_MATH}
            With AddSubSym(SubSyms.Strings[I], Round(OptPower(0.01*FSubFont, I)*aFontSize)) Do Begin
{$ELSE}
            With AddSubSym(SubSyms.Strings[I], Round(Power(0.01*FSubFont, I)*aFontSize)) Do Begin
{$ENDIF}
               Result.Width := Result.Width+Width;

               Result.Canvas.CopyRect(Rect(Result.Width-Width, Result.Height-Height, Result.Width, Result.Height),
                                      Canvas,
                                      Rect(0, 0, Width, Height));

               Free;
            End;

      // Destroy the list of sub-symbols

      SubSyms.Free;
   End;
End;

//==============================================================================

Function TCmdGraph.CreateCurlyBracket(Const aHeight: Integer;
                                      Const aFontSize: Integer): TBitmap;
Var
   BarSize: Integer;
   L1, L2, L3: Integer;
Begin
   BarSize := GetBarSize(aFontSize);

   Result := CreateBmp(aFontSize, BarSize);

   // New dimensions of the bitmap

   Result.Width  := 7*BarSize;
   Result.Height := BarSize+aHeight+BarSize;

   L1 := Result.Height Div 50;
   L2 := Result.Height Div 15;
   L3 := Result.Height Div 2;

   // Render the curly bracket

   Result.Canvas.Polyline([Point(Result.Width-5*BarSize, BarSize),
                           Point(Result.Width-4*BarSize, BarSize+L1),
                           Point(Result.Width-3*BarSize, BarSize+L1+L2),
                           Point(Result.Width-3*BarSize, L3-L1),
                           Point(Result.Width-BarSize, L3),
                           Point(Result.Width-3*BarSize, L3+L1),
                           Point(Result.Width-3*BarSize, Result.Height-L2-L1-BarSize),
                           Point(Result.Width-4*BarSize, Result.Height-L1-BarSize),
                           Point(Result.Width-5*BarSize, Result.Height-BarSize)]);

   // Mirror it...
   // Note: to make it in a "mirrored" way does NOT look nice, for some reasons,
   //       so...

   Result.Canvas.CopyRect(Rect(Result.Width, 0, 0, Result.Height),
                          Result.Canvas,
                          Rect(0, 0, Result.Width, Result.Height));
End;

//==============================================================================

Procedure TCmdGraph.AddCurlyBracket(Var aBuffer: TBitmap;
                                    Var aCmdBinTree: TCmdBinTree;
                                    Const aFontSize: Integer);
Var
   Buffer: TBitmap;
   BarSize: Integer;
   CB: TBitmap;
Begin
   // Note: "TCanvas" drawing functions (e.g. "Polyline", "Arc", etc.) don't
   //       draw in the same way when used for drawing the left and right
   //       parentheses! Therefore, we create one parenthese and mirror it to
   //       get the other one (!!)...

   BarSize := GetBarSize(aFontSize);

   Buffer := CreateBmp(aFontSize, BarSize);

   // Render the parentheses

   CB := CreateCurlyBracket(aBuffer.Height, aFontSize);

   // New dimensions of the bitmap

   Inc(aCmdBinTree.Baseline, BarSize);

   Buffer.Width  := CB.Width+aBuffer.Width;
   Buffer.Height := BarSize+aBuffer.Height+BarSize;

   // {

   Buffer.Canvas.CopyRect(Rect(0, 0, CB.Width, CB.Height),
                          CB.Canvas,
                          Rect(0, 0, CB.Width, CB.Height));

   // {a

   Buffer.Canvas.CopyRect(Rect(CB.Width, BarSize, CB.Width+aBuffer.Width, BarSize+aBuffer.Height),
                          aBuffer.Canvas,
                          Rect(0, 0, aBuffer.Width, aBuffer.Height));

   // Update the buffer

   CB.Free;

   aBuffer.Free;

   aBuffer := Buffer;
End;

//==============================================================================

Function TCmdGraph.CreateParenthesis(Const aHeight: Integer;
                                     Const aFontSize: Integer;
                                     Const aLeftParenthesis: Boolean): TBitmap;
Var
   BarSize: Integer;
   L1, L2, L3: Integer;
Begin
   BarSize := GetBarSize(aFontSize);

   Result := CreateBmp(aFontSize, BarSize);

   // New dimensions of the bitmap

   Result.Width  := 6*BarSize;
   Result.Height := BarSize+aHeight+BarSize;

   L1 := Result.Height Div 50;
   L2 := Result.Height Div 15;
   L3 := Result.Height Div 5;

   // Render the parenthese

   Result.Canvas.Polyline([Point(Result.Width-4*BarSize, BarSize),
                           Point(Result.Width-3*BarSize, BarSize+L1),
                           Point(Result.Width-2*BarSize, BarSize+L1+L2),
                           Point(Result.Width-BarSize, BarSize+L1+L2+L3),
                           Point(Result.Width-BarSize, Result.Height-L3-L2-L1-BarSize),
                           Point(Result.Width-2*BarSize, Result.Height-L2-L1-BarSize),
                           Point(Result.Width-3*BarSize, Result.Height-L1-BarSize),
                           Point(Result.Width-4*BarSize, Result.Height-BarSize)]);

   // Mirror it, if necessary...

   If (aLeftParenthesis) Then
      Result.Canvas.CopyRect(Rect(Result.Width, 0, 0, Result.Height),
                             Result.Canvas,
                             Rect(0, 0, Result.Width, Result.Height));
End;


//==============================================================================

Procedure TCmdGraph.AddParentheses(Var aBuffer: TBitmap;
                                   Var aCmdBinTree: TCmdBinTree;
                                   Const aFontSize: Integer);
Var
   Buffer: TBitmap;
   BarSize: Integer;
   LP, RP: TBitmap;
Begin
   // Note: "TCanvas" drawing functions (e.g. "Polyline", "Arc", etc.) don't
   //       draw in the same way when used for drawing the left and right
   //       parentheses! Therefore, we create one parenthese and mirror it to
   //       get the other one (!!)...

   BarSize := GetBarSize(aFontSize);

   Buffer := CreateBmp(aFontSize, BarSize);

   // Render the parentheses

   LP := CreateParenthesis(aBuffer.Height, aFontSize, True);
   RP := CreateParenthesis(aBuffer.Height, aFontSize, False);

   // New dimensions of the bitmap

   Inc(aCmdBinTree.Baseline, BarSize);

   Buffer.Width  := LP.Width+aBuffer.Width+RP.Width;
   Buffer.Height := BarSize+aBuffer.Height+BarSize;

   // (

   Buffer.Canvas.CopyRect(Rect(0, 0, LP.Width, LP.Height),
                          LP.Canvas,
                          Rect(0, 0, LP.Width, LP.Height));

   // (a

   Buffer.Canvas.CopyRect(Rect(LP.Width, BarSize, LP.Width+aBuffer.Width, BarSize+aBuffer.Height),
                          aBuffer.Canvas,
                          Rect(0, 0, aBuffer.Width, aBuffer.Height));

   // (a)

   Buffer.Canvas.CopyRect(Rect(Buffer.Width-RP.Width, 0, Buffer.Width, RP.Height),
                          RP.Canvas,
                          Rect(0, 0, RP.Width, RP.Height));

   // Update the buffer

   LP.Free;
   RP.Free;

   aBuffer.Free;

   aBuffer := Buffer;
End;

//==============================================================================

Function TCmdGraph.AddOperand(Var aCmdBinTree: TCmdBinTree;
                              Const aFontSize: Integer): TBitmap;
Var
   BarSize: Integer;
   Left, Right: TBitmap;
   RealHeight: Integer;
   RelTop: Integer;
Begin
   BarSize := GetBarSize(aFontSize);

   Result := CreateBmp(aFontSize);

   Left  := Render(aCmdBinTree.Left, aFontSize);
   Right := Render(aCmdBinTree.Right, aFontSize);

   Case aCmdBinTree.ItemType Of
      itEq, itNEq, itGT, itLT, itGEq, itLEq: Begin
         // Some operands ("<>", ">=" and "<=") are not part of the default
         // ASCII table, but are present in the "Symbol" font, so...
         // Note #1: "=", for instance, is part of the default ASCII table, but
         //          it's better to have all those symbols using the same font
         //          for consistency... In that respect, we must make sure that
         //          all the characters have the same height...
         // Note #2: Ideally, one would use unicode (the mathematical operators
         //          subset of any unicode capable font), but as yet, Delphi's
         //          VCL does not support unicode (since Delphi targets Win9X
         //          and those platforms don't know about unicode...), so...

         RealHeight := Result.Canvas.TextHeight(' ');

         Result.Canvas.Font.Name := 'Symbol';

         Result.Canvas.Font.Size := Round(aFontSize*RealHeight/Result.Canvas.TextHeight(' '));
      End;
   End;

   // Check whether "Right" is is a piecewise element, if so then add a curly
   // bracket in front of it

   If (aCmdBinTree.Right.ItemType = itPiecewise) Then
      AddCurlyBracket(Right, aCmdBinTree.Right, aFontSize);

   // Determine whether "Left" and/or "Right" need parentheses around them

   Case aCmdBinTree.ItemType Of
      itPlus: Begin
         Case aCmdBinTree.Left.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itAnd, itOr, itXOr:
               AddParentheses(Left, aCmdBinTree.Left, aFontSize);
         End;

         Case aCmdBinTree.Right.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itAnd, itOr, itXOr:
               AddParentheses(Right, aCmdBinTree.Right, aFontSize);
         End;
      End;
      itMinus: Begin
         Case aCmdBinTree.Left.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itAnd, itOr, itXOr:
               AddParentheses(Left, aCmdBinTree.Left, aFontSize);
         End;

         Case aCmdBinTree.Right.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus,
            itAnd, itOr, itXOr:
               AddParentheses(Right, aCmdBinTree.Right, aFontSize);
         End;
      End;
      itAnd: Begin
         Case aCmdBinTree.Left.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus,
            itOr, itXOr:
               AddParentheses(Left, aCmdBinTree.Left, aFontSize);
         End;

         Case aCmdBinTree.Right.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus,
            itOr, itXOr:
               AddParentheses(Right, aCmdBinTree.Right, aFontSize);
         End;
      End;
      itOr: Begin
         Case aCmdBinTree.Left.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus,
            itAnd, itXOr:
               AddParentheses(Left, aCmdBinTree.Left, aFontSize);
         End;

         Case aCmdBinTree.Right.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus,
            itAnd, itXOr:
               AddParentheses(Right, aCmdBinTree.Right, aFontSize);
         End;
      End;
      itXOr: Begin
         Case aCmdBinTree.Left.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus,
            itAnd, itOr:
               AddParentheses(Left, aCmdBinTree.Left, aFontSize);
         End;

         Case aCmdBinTree.Right.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus,
            itAnd, itOr:
               AddParentheses(Right, aCmdBinTree.Right, aFontSize);
         End;
      End;
   End;

   // New dimensions of the bitmap

{$IFDEF OPT_MATH}
   aCmdBinTree.Baseline := OptMaxI(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ELSE}
   aCmdBinTree.Baseline := Max(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ENDIF}

   Result.Width  := Left.Width+BarSize+Result.Canvas.TextWidth(aCmdBinTree.Item)+BarSize+Right.Width;
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(aCmdBinTree.Baseline+Result.Canvas.TextHeight(aCmdBinTree.Item),
                            OptMaxI(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height,
                                    aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height));
{$ELSE}
   Result.Height := Max(aCmdBinTree.Baseline+Result.Canvas.TextHeight(aCmdBinTree.Item),
                        Max(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height,
                            aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height));
{$ENDIF}

   // a

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height);

   Result.Canvas.CopyRect(Rect(0, RelTop, Left.Width, RelTop+Left.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // a <operand>

   Result.Canvas.TextOut(Left.Width+BarSize, Result.Height-Result.Canvas.TextHeight(aCmdBinTree.Item)-aCmdBinTree.Baseline, aCmdBinTree.Item);

   // a <operand> b

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);

   Result.Canvas.CopyRect(Rect(Result.Width-Right.Width, RelTop, Result.Width, RelTop+Right.Height),
                          Right.Canvas,
                          Rect(0, 0, Right.Width, Right.Height));

   // Free some stuff

   Left.Free;
   Right.Free;
End;

//==============================================================================

Function TCmdGraph.AddFunc(Var aCmdBinTree: TCmdBinTree;
                           Const aFontSize: Integer): TBitmap;
Var
   LogBase, Left: TBitmap;
   AddedParentheses: Boolean;
   SpaceSize: Integer;
Begin
   Result := CreateBmp(aFontSize);

   AddedParentheses := False;

   If ((aCmdBinTree.ItemType = itLog) And (aCmdBinTree.Left.ItemType = itLogBase)) Then Begin
      LogBase := Render(aCmdBinTree.Left.Left, Round(0.01*FSubFont*aFontSize));
      Left    := Render(aCmdBinTree.Right, aFontSize);

      // Determine whether "Left" needs parentheses around it

      Case aCmdBinTree.Right.ItemType Of
         itEq, itNEq, itGT, itLT, itGEq, itLEq,
         itPlus, itMinus, itTimes,
         itExp, itLN, itLog, itFact,
         itAnd, itOr, itXOr, itNot,
         itSin, itCos, itTan, itSec, itCsc, itCot,
         itSinH, itCosH, itTanH, itSecH, itCscH, itCotH,
         itASin, itACos, itATan, itASec, itACsc, itACot,
         itASinH, itACosH, itATanH, itASecH, itACscH, itACotH: Begin
            AddParentheses(Left, aCmdBinTree.Right, aFontSize);

            AddedParentheses := True;
         End;
      End;

      aCmdBinTree.Baseline := aCmdBinTree.Right.Baseline;
   End Else Begin
      LogBase := CreateBmp(aFontSize);   // Dummy bitmap...
      Left    := Render(aCmdBinTree.Left, aFontSize);

      // Determine whether "Left" needs parentheses around it

      Case aCmdBinTree.ItemType Of
         itPlus:
            Case aCmdBinTree.Left.ItemType Of
               itEq, itNEq, itGT, itLT, itGEq, itLEq,
               itAnd, itOr, itXOr, itNot: Begin
                  AddParentheses(Left, aCmdBinTree.Left, aFontSize);

                  AddedParentheses := True;
               End;
            End;
         itMinus:
            Case aCmdBinTree.Left.ItemType Of
               itEq, itNEq, itGT, itLT, itGEq, itLEq,
               itPlus, itMinus,
               itAnd, itOr, itXOr, itNot: Begin
                  AddParentheses(Left, aCmdBinTree.Left, aFontSize);

                  AddedParentheses := True;
               End;
            End;
      Else
         Case aCmdBinTree.Left.ItemType Of
            itEq, itNEq, itGT, itLT, itGEq, itLEq,
            itPlus, itMinus, itTimes, 
            itExp, itLN, itLog, itFact,
            itAnd, itOr, itXOr, itNot,
            itSin, itCos, itTan, itSec, itCsc, itCot,
            itSinH, itCosH, itTanH, itSecH, itCscH, itCotH,
            itASin, itACos, itATan, itASec, itACsc, itACot,
            itASinH, itACosH, itATanH, itASecH, itACscH, itACotH: Begin
               AddParentheses(Left, aCmdBinTree.Left, aFontSize);

               AddedParentheses := True;
            End;
         End;
      End;

      aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline;
   End;

   // New dimensions of the bitmap

   SpaceSize := 0;

   If (aCmdBinTree.ItemType <> itPlus) Then Begin
      // Not the unirary plus, so can include the width required for the
      // function

      If (Not AddedParentheses And (aCmdBinTree.ItemType <> itMinus)) Then
         // No parentheses were added and it's not the unirary minus, so we need
         // some space between the function and its argument 

         SpaceSize := 7*GetBarSize(aFontSize);

      Result.Width := Result.Canvas.TextWidth(aCmdBinTree.Item)+LogBase.Width+SpaceSize+Left.Width
   End Else
      Result.Width := Left.Width;

{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(aCmdBinTree.Baseline+Result.Canvas.TextHeight(aCmdBinTree.Item), Left.Height);
{$ELSE}
   Result.Height := Max(aCmdBinTree.Baseline+Result.Canvas.TextHeight(aCmdBinTree.Item), Left.Height);
{$ENDIF}

   // func

   If (aCmdBinTree.ItemType <> itPlus) Then
      // Not the unirary plus, so it's fine...

      Result.Canvas.TextOut(0, Result.Height-Result.Canvas.TextHeight(aCmdBinTree.Item)-aCmdBinTree.Baseline, aCmdBinTree.Item);

   // func [logbase]

   Result.Canvas.CopyRect(Rect(Result.Width-LogBase.Width-SpaceSize-Left.Width, Result.Height-LogBase.Height, Result.Width-SpaceSize-Left.Width, Result.Height),
                          LogBase.Canvas,
                          Rect(0, 0, LogBase.Width, LogBase.Height));

   // func [logbase] a

   Result.Canvas.CopyRect(Rect(Result.Width-Left.Width, Result.Height-Left.Height, Result.Width, Result.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // Free some stuff

   LogBase.Free;
   Left.Free;
End;

//==============================================================================

Procedure TCmdGraph.AddDiffD(Var aBuffer: TBitmap;
                             Var aCmdBinTree: TCmdBinTree;
                             Const aFontSize: Integer);
Const
   DChar = 'd';
Var
   Buffer: TBitmap;
Begin
   Buffer := CreateBmp(aFontSize);

   // New dimensions of the bitmap

   Buffer.Width  := Buffer.Canvas.TextWidth(DChar)+aBuffer.Width;
{$IFDEF OPT_MATH}
   Buffer.Height := OptMaxI(aCmdBinTree.Baseline+Buffer.Canvas.TextHeight(DChar), aBuffer.Height);
{$ELSE}
   Buffer.Height := Max(aCmdBinTree.Baseline+Buffer.Canvas.TextHeight(DChar), aBuffer.Height);
{$ENDIF}

   // d

   Buffer.Canvas.TextOut(0, Buffer.Height-Buffer.Canvas.TextHeight(DChar)-aCmdBinTree.Baseline, DChar);

   // da

   Buffer.Canvas.CopyRect(Rect(Buffer.Width-aBuffer.Width, Buffer.Height-aBuffer.Height, Buffer.Width, Buffer.Height),
                          aBuffer.Canvas,
                          Rect(0, 0, aBuffer.Width, aBuffer.Height));

   // Update the buffer

   aBuffer.Free;

   aBuffer := Buffer;
End;

//==============================================================================

Procedure TCmdGraph.AddDivider(Var aBuffer: TBitmap;
                               Const aLeft: TBitmap; Const aRight: TBitmap;
                               Var aCmdBinTree: TCmdBinTree;
                               Const aFontSize: Integer);
Var
   Buffer: TBitmap;
   BarSize: Integer;
   RelLeft: Integer;
Begin
   BarSize := GetBarSize(aFontSize);

   Buffer := CreateBmp(aFontSize, BarSize);

   // New dimensions of the bitmap

   aCmdBinTree.Baseline := BarSize+aRight.Height-Buffer.Canvas.TextHeight(' ') Div 2;

{$IFDEF OPT_MATH}
   Buffer.Width  := OptMaxI(aLeft.Width, aRight.Width);
{$ELSE}
   Buffer.Width  := Max(aLeft.Width, aRight.Width);
{$ENDIF}
   Buffer.Height := aLeft.Height+2*BarSize+aRight.Height;

   // a

   RelLeft := (Buffer.Width-aLeft.Width) Div 2;

   Buffer.Canvas.CopyRect(Rect(RelLeft, 0, RelLeft+aLeft.Width, aLeft.Height),
                          aLeft.Canvas,
                          Rect(0, 0, aLeft.Width, aLeft.Height));

   // a
   // -

   Buffer.Canvas.Polyline([Point(0, aLeft.Height+BarSize),
                           Point(Buffer.Width, aLeft.Height+BarSize)]);

   // a
   // -
   // b

   RelLeft := (Buffer.Width-aRight.Width) Div 2;

   Buffer.Canvas.CopyRect(Rect(RelLeft, Buffer.Height-aRight.Height, RelLeft+aRight.Width, Buffer.Height),
                          aRight.Canvas,
                          Rect(0, 0, aRight.Width, aRight.Height));

   // Update the buffer

   aBuffer.Free;

   aBuffer := Buffer;
End;

//==============================================================================

Function TCmdGraph.AddUnit(Var aCmdBinTree: TCmdBinTree;
                           Const aFontSize: Integer): TBitmap;
Var
   OldUnderscoreForSub: Boolean;
   OldGreekSymbols: Boolean;
Begin
   OldUnderscoreForSub := FUnderscoreForSub;
   OldGreekSymbols     := FGreekSymbols;

   FUnderscoreForSub := False;
   FGreekSymbols     := False;

   Result := AddSym(aCmdBinTree, aFontSize);

   FUnderscoreForSub := OldUnderscoreForSub;
   FGreekSymbols     := OldGreekSymbols;
End;

//==============================================================================

Function TCmdGraph.AddConcatenateOrProperties(Var aCmdBinTree: TCmdBinTree;
                                              Const aFontSize: Integer;
                                              Const aProperties: Boolean): TBitmap;
Var
   Left, Right: TBitmap;
   RelTop: Integer;
Begin
   Result := CreateBmp(aFontSize);

   Left  := Render(aCmdBinTree.Left, aFontSize);
   Right := Render(aCmdBinTree.Right, aFontSize);

   If (aProperties) Then
      AddCurlyBracket(Right, aCmdBinTree.Right, aFontSize);

   // New dimensions of the bitmap

{$IFDEF OPT_MATH}
   aCmdBinTree.Baseline := OptMaxI(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ELSE}
   aCmdBinTree.Baseline := Max(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ENDIF}

   Result.Width  := Left.Width+1+Right.Width;
   // Note: the "+1" seems necessary in some cases (when the equation graph is
   //       "small") for when "Left" is surrounded by parentheses, so...
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height,
                            aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);
{$ELSE}
   Result.Height := Max(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height,
                        aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);
{$ENDIF}

   // a

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height);

   Result.Canvas.CopyRect(Rect(0, RelTop, Left.Width, RelTop+Left.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // ab or a{b

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);

   Result.Canvas.CopyRect(Rect(Result.Width-Right.Width, RelTop, Result.Width, RelTop+Right.Height),
                          Right.Canvas,
                          Rect(0, 0, Right.Width, Right.Height));

   // Free some stuff

   Left.Free;
   Right.Free;
End;

//==============================================================================

Function TCmdGraph.AddPropertyOrPiecewise(Var aCmdBinTree: TCmdBinTree;
                                          Const aFontSize: Integer): TBitmap;
Var
   Left, Right: TBitmap;
   BarSize: Integer;
Begin
   BarSize := GetBarSize(aFontSize);

   If (aCmdBinTree.Right <> Nil) Then Begin
      Result := CreateBmp(aFontSize);

      Left  := Render(aCmdBinTree.Left, aFontSize);
      Right := Render(aCmdBinTree.Right, aFontSize);

      // New dimensions of the bitmap

      aCmdBinTree.Baseline := (Left.Height+BarSize+Right.Height) Div 2-Result.Canvas.TextHeight(' ') Div 2;

{$IFDEF OPT_MATH}
      Result.Width  := OptMaxI(Left.Width, Right.Width);
{$ELSE}
      Result.Width  := Max(Left.Width, Right.Width);
{$ENDIF}
      Result.Height := Left.Height+BarSize+Right.Height;

      // a

      Result.Canvas.CopyRect(Rect(0, 0, Left.Width, Left.Height),
                             Left.Canvas,
                             Rect(0, 0, Left.Width, Left.Height));

      // a
      // b

      Result.Canvas.CopyRect(Rect(0, Left.Height+BarSize, Right.Width, Result.Height),
                             Right.Canvas,
                             Rect(0, 0, Right.Width, Right.Height));

      // Free some stuff

      Left.Free;
      Right.Free;
   End Else Begin
      Result := Render(aCmdBinTree.Left, aFontSize);

      // New dimensions of the bitmap

      aCmdBinTree.Baseline := Result.Height Div 2-Result.Canvas.TextHeight(' ') Div 2;
   End;
End;

//==============================================================================

Function TCmdGraph.AddPiece(Var aCmdBinTree: TCmdBinTree;
                            Const aFontSize: Integer): TBitmap;
Const
   IfChars = ', if ';
Var
   Left, Right: TBitmap;
   RelTop: Integer;
Begin
   Result := CreateBmp(aFontSize);

   Left  := Render(aCmdBinTree.Left, aFontSize);
   Right := Render(aCmdBinTree.Right, aFontSize);

   // New dimensions of the bitmap

{$IFDEF OPT_MATH}
   aCmdBinTree.Baseline := OptMaxI(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ELSE}
   aCmdBinTree.Baseline := Max(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ENDIF}

   Result.Width  := Left.Width+1+Result.Canvas.TextWidth(IfChars)+Right.Width;
   // Note: the "+1" is because if "Left" ends with a closing parenthesis, then
   //       the "if" part may slightly overwrite it, so...
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(Left.Height, OptMaxI(Result.Canvas.TextHeight(IfChars), Right.Height));
{$ELSE}
   Result.Height := Max(Left.Height, Max(Result.Canvas.TextHeight(IfChars), Right.Height));
{$ENDIF}

   // a

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height);

   Result.Canvas.CopyRect(Rect(0, RelTop, Left.Width, RelTop+Left.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // a if

   Result.Canvas.TextOut(Left.Width+1, Result.Height-Result.Canvas.TextHeight(IfChars)-aCmdBinTree.Baseline, IfChars);
   // Note: the "+1" is because if "Left" ends with a closing parenthesis, then
   //       the "if" part may slightly overwrite it, so...

   // a if b

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);

   Result.Canvas.CopyRect(Rect(Result.Width-Right.Width, RelTop, Result.Width, RelTop+Right.Height),
                          Right.Canvas,
                          Rect(0, 0, Right.Width, Right.Height));

   // Free some stuff

   Left.Free;
   Right.Free;
End;

//==============================================================================

Function TCmdGraph.AddOtherwise(Var aCmdBinTree: TCmdBinTree;
                                Const aFontSize: Integer): TBitmap;
Const
   OtherwiseChars = ', otherwise';
Var
   Left: TBitmap;
   RelTop: Integer;
Begin
   Result := CreateBmp(aFontSize);

   Left  := Render(aCmdBinTree.Left, aFontSize);

   // New dimensions of the bitmap

   aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline;

   Result.Width  := Left.Width+1+Result.Canvas.TextWidth(OtherwiseChars);
   // Note: the "+1" is because if "Left" ends with a closing parenthesis, then
   //       the "otherwise" part may slightly overwrite it, so...
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(Left.Height, Result.Canvas.TextHeight(OtherwiseChars));
{$ELSE}
   Result.Height := Max(Left.Height, Result.Canvas.TextHeight(OtherwiseChars));
{$ENDIF}

   // a

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height);

   Result.Canvas.CopyRect(Rect(0, RelTop, Left.Width, RelTop+Left.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // a otherwise

   Result.Canvas.TextOut(Left.Width+1, Result.Height-Result.Canvas.TextHeight(OtherwiseChars)-aCmdBinTree.Baseline, OtherwiseChars);
   // Note: the "+1" is because if "Left" ends with a closing parenthesis, then
   //       the "otherwise" part may slightly overwrite it, so...

   // Free some stuff

   Left.Free;
End;

//==============================================================================

Function TCmdGraph.AddDiff(Var aCmdBinTree: TCmdBinTree;
                           Const aFontSize: Integer): TBitmap;
Var
   Left, Right: TBitmap;
Begin
   Result := Nil;

   Left  := Render(aCmdBinTree.Right, aFontSize);
   Right := Render(aCmdBinTree.Left, aFontSize);

   AddDiffD(Left, aCmdBinTree.Left, aFontSize);
   AddDiffD(Right, aCmdBinTree.Right, aFontSize);

   AddDivider(Result, Left, Right, aCmdBinTree, aFontSize);

   // Free some stuff

   Left.Free;
   Right.Free;
End;

//==============================================================================

Function TCmdGraph.AddTimes(Var aCmdBinTree: TCmdBinTree;
                            Const aFontSize: Integer): TBitmap;
Const
   TimesChar = '.';
Var
   Left, Right: TBitmap;
   RelTop, RelLeft: Integer;
Begin
   Result := CreateBmp(aFontSize);

   Left  := Render(aCmdBinTree.Left, aFontSize);
   Right := Render(aCmdBinTree.Right, aFontSize);

   // Determine whether "Left" and/or "Right" need parentheses around them

   Case aCmdBinTree.Left.ItemType Of
      itEq, itNEq, itGT, itLT, itGEq, itLEq,
      itAnd, itOr, itXOr:
         AddParentheses(Left, aCmdBinTree.Left, aFontSize);
      itLN, itLog,
      itSin, itCos, itTan, itSec, itCsc, itCot,
      itSinH, itCosH, itTanH, itSecH, itCscH, itCotH,
      itASin, itACos, itATan, itASec, itACsc, itACot,
      itASinH, itACosH, itATanH, itASecH, itACscH, itACotH:
         If (aCmdBinTree.Left.Left.ItemType = itSym) Then
            // Only add parentheses when we have something like:
            //    (log x) * y
            // but not when we have something like:
            //    log(x+y) * z

            AddParentheses(Left, aCmdBinTree.Left, aFontSize);
      itPlus, itMinus:
         If (aCmdBinTree.Left.Right <> Nil) Then
            AddParentheses(Left, aCmdBinTree.Left, aFontSize);
   End;

   Case aCmdBinTree.Right.ItemType Of
      itEq, itNEq, itGT, itLT, itGEq, itLEq,
      itAnd, itOr, itXOr:
         AddParentheses(Right, aCmdBinTree.Right, aFontSize);
      itPlus, itMinus:
         If (aCmdBinTree.Right.Right <> Nil) Then
            AddParentheses(Right, aCmdBinTree.Right, aFontSize);
   End;

   // New dimensions of the bitmap

{$IFDEF OPT_MATH}
   aCmdBinTree.Baseline := OptMaxI(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ELSE}
   aCmdBinTree.Baseline := Max(aCmdBinTree.Left.Baseline, aCmdBinTree.Right.Baseline);
{$ENDIF}

   Result.Canvas.Font.Size := 2*(aFontSize Div 3);
   // Note: ONLY because of the separator

   Result.Width  := Left.Width+1+Result.Canvas.TextWidth(TimesChar)+Right.Width;
   // Note: the "+1" seems necessary in some cases (when the equation graph is
   //       "small") for when "Left" is surrounded by parentheses, so...
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height,
                            aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);
{$ELSE}
   Result.Height := Max(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height,
                        aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);
{$ENDIF}

   // a

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Left.Baseline+Left.Height);

   Result.Canvas.CopyRect(Rect(0, RelTop, Left.Width, RelTop+Left.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // a.

   RelTop := Result.Height-(Round(1.4*Result.Canvas.TextHeight(TimesChar))+aCmdBinTree.Baseline);
   RelLeft := Left.Width+1;

   Result.Canvas.TextOut(RelLeft, RelTop, TimesChar);

   // a.b

   RelTop := Result.Height-(aCmdBinTree.Baseline-aCmdBinTree.Right.Baseline+Right.Height);

   Result.Canvas.CopyRect(Rect(Result.Width-Right.Width, RelTop, Result.Width, RelTop+Right.Height),
                          Right.Canvas,
                          Rect(0, 0, Right.Width, Right.Height));

   // Free some stuff

   Left.Free;
   Right.Free;
End;

//==============================================================================

Function TCmdGraph.AddDivide(Var aCmdBinTree: TCmdBinTree;
                             Const aFontSize: Integer): TBitmap;
Var
   Left, Right: TBitmap;
Begin
   Result := Nil;

   Left  := Render(aCmdBinTree.Left, aFontSize);
   Right := Render(aCmdBinTree.Right, aFontSize);

   AddDivider(Result, Left, Right, aCmdBinTree, aFontSize);

   // Free some stuff

   Left.Free;
   Right.Free;
End;

//==============================================================================

Function TCmdGraph.AddPow(Var aCmdBinTree: TCmdBinTree;
                          Const aFontSize: Integer): TBitmap;
Var
   Left, Right: TBitmap;
Begin
   Result := CreateBmp(aFontSize);

   Left  := Render(aCmdBinTree.Left, aFontSize);
   Right := Render(aCmdBinTree.Right, Round(0.01*FSubFont*aFontSize));

   // Determine whether "Left" needs parentheses around it

   Case aCmdBinTree.Left.ItemType Of
      itEq, itNEq, itGT, itLT, itGEq, itLEq,
      itTimes, itDivide, itPow, itRoot,
      itExp, itLN, itLog, itFact,
      itAnd, itOr, itXOr, itNot,
      itDiff,
      itSin, itCos, itTan, itSec, itCsc, itCot,
      itSinH, itCosH, itTanH, itSecH, itCscH, itCotH,
      itASin, itACos, itATan, itASec, itACsc, itACot,
      itASinH, itACosH, itATanH, itASecH, itACscH, itACotH:
         AddParentheses(Left, aCmdBinTree.Left, aFontSize);
      itPlus, itMinus:
         If (aCmdBinTree.Left.Right <> Nil) Then
            AddParentheses(Left, aCmdBinTree.Left, aFontSize);
   End;

   // New dimensions of the bitmap

   aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline;

   Result.Width  := Left.Width+Right.Width;
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(Left.Height, 2*(Left.Height Div 3)+Right.Height);
{$ELSE}
   Result.Height := Max(Left.Height, 2*(Left.Height Div 3)+Right.Height);
{$ENDIF}

   // a

   Result.Canvas.CopyRect(Rect(0, Result.Height-Left.Height, Left.Width, Result.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   //  b
   // a

   Result.Canvas.CopyRect(Rect(Left.Width, 0, Result.Width, Right.Height),
                          Right.Canvas,
                          Rect(0, 0, Right.Width, Right.Height));

   // Free some stuff

   Left.Free;
   Right.Free;
End;

//==============================================================================
Function TCmdGraph.AddRoot(Var aCmdBinTree: TCmdBinTree;
                           Const aFontSize: Integer): TBitmap;
Var
   BarSize: Integer;
   Left, Degree: TBitmap;
   RelLeft, RelTop: Integer;
   RootWithDegree: Boolean;
Begin
   BarSize := GetBarSize(aFontSize);

   Result := CreateBmp(aFontSize, BarSize);

   RootWithDegree := aCmdBinTree.Right <> Nil;

   If (RootWithDegree) Then Begin
      Left   := Render(aCmdBinTree.Right, aFontSize);
      Degree := Render(aCmdBinTree.Left.Left, Round(0.01*FSubFont*aFontSize));
   End Else Begin
      Left   := Render(aCmdBinTree.Left, aFontSize);
      Degree := CreateBmp;
   End;

   // New dimensions of the bitmap

   If (RootWithDegree) Then
      aCmdBinTree.Baseline := aCmdBinTree.Right.Baseline
   Else
      aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline;

   Result.Width  := BarSize+BarSize+Degree.Width+BarSize+3*BarSize+BarSize+Left.Width+BarSize+BarSize;
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(BarSize+Left.Height+BarSize+BarSize,
                            BarSize+Left.Height Div 3+BarSize+Degree.Height);
{$ELSE}
   Result.Height := Max(BarSize+Left.Height+BarSize+BarSize,
                        BarSize+Left.Height Div 3+BarSize+Degree.Height);
{$ENDIF}

   // a

   RelLeft := Result.Width-Left.Width-BarSize-BarSize;
   RelTop  := Result.Height-BarSize-Left.Height;

   Result.Canvas.CopyRect(Rect(RelLeft, RelTop, RelLeft+Left.Width, RelTop+Left.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   //   --
   // \/a

   RelLeft := Result.Width-BarSize-Degree.Width-BarSize-3*BarSize-BarSize-Left.Width-BarSize-BarSize;
   RelTop  := Result.Height-BarSize-Left.Height Div 3;

   Result.Canvas.Polyline([Point(RelLeft, RelTop),
                           Point(RelLeft+BarSize+Degree.Width+BarSize, RelTop+Left.Height Div 3),
                           Point(RelLeft+BarSize+Degree.Width+BarSize+3*BarSize, RelTop+Left.Height Div 3-Left.Height-BarSize),
                           Point(RelLeft+BarSize+Degree.Width+BarSize+3*BarSize+BarSize+Left.Width+BarSize, RelTop+Left.Height Div 3-Left.Height-BarSize)]);

   // b --
   // \/a

   If (RootWithDegree And (CompareStr(aCmdBinTree.Left.Item, '2') <> 0)) Then Begin
      RelLeft := Result.Width-Degree.Width-BarSize-3*BarSize-BarSize-Left.Width-BarSize-BarSize;
      RelTop  := Result.Height-BarSize-Left.Height Div 3-BarSize-Degree.Height;

      Result.Canvas.CopyRect(Rect(RelLeft, RelTop, RelLeft+Degree.Width, RelTop+Degree.Height),
                             Degree.Canvas,
                             Rect(0, 0, Degree.Width, Degree.Height));
   End;

   // Free some stuff

   Left.Free;
   Degree.Free;
End;

//==============================================================================

Function TCmdGraph.AddAbs(Var aCmdBinTree: TCmdBinTree;
                          Const aFontSize: Integer): TBitmap;
Var
   BarSize: Integer;
   Left: TBitmap;
Begin
   BarSize := GetBarSize(aFontSize);

   Result := CreateBmp(aFontSize, BarSize);

   Left := Render(aCmdBinTree.Left, aFontSize);

   // New dimensions of the bitmap

   aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline+BarSize;

   Result.Width  := 2*BarSize+Left.Width+2*BarSize;
   Result.Height := BarSize+Left.Height+BarSize;

   // |

   Result.Canvas.Polyline([Point(BarSize, BarSize),
                           Point(BarSize, Result.Height-BarSize)]);

   // |a

   Result.Canvas.CopyRect(Rect(2*BarSize, Result.Height-Left.Height-BarSize, 2*BarSize+Left.Width, Result.Height-BarSize),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // |a|

   Result.Canvas.Polyline([Point(Result.Width-BarSize, BarSize),
                           Point(Result.Width-BarSize, Result.Height-BarSize)]);

   // Free some stuff

   Left.Free;
End;

//==============================================================================

Function TCmdGraph.AddExp(Var aCmdBinTree: TCmdBinTree;
                          Const aFontSize: Integer): TBitmap;
Var
   Left: TBitmap;
   ExpWidth, ExpHeight: Integer;
Begin
   Result := CreateBmp(aFontSize);

   Left := Render(aCmdBinTree.Left, Round(0.01*FSubFont*aFontSize));

   // New dimensions of the bitmap

   ExpWidth  := Result.Canvas.TextWidth(aCmdBinTree.Item);
   ExpHeight := Result.Canvas.TextHeight(aCmdBinTree.Item);

   Result.Width  := ExpWidth+Left.Width;
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(ExpHeight, 2*(ExpHeight Div 3)+Left.Height);
{$ELSE}
   Result.Height := Max(ExpHeight, 2*(ExpHeight Div 3)+Left.Height);
{$ENDIF}

   // e

   Result.Canvas.TextOut(0, Result.Height-ExpHeight, aCmdBinTree.Item);

   //  b
   // e

   Result.Canvas.CopyRect(Rect(ExpWidth, 0, Result.Width, Left.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // Free some stuff

   Left.Free;
End;

//==============================================================================

Function TCmdGraph.AddFloor(Var aCmdBinTree: TCmdBinTree;
                            Const aFontSize: Integer): TBitmap;
Var
   BarSize: Integer;
   Left: TBitmap;
Begin
   BarSize := GetBarSize(aFontSize);

   Result := CreateBmp(aFontSize, BarSize);

   Left := Render(aCmdBinTree.Left, aFontSize);

   // New dimensions of the bitmap

   aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline+2*BarSize;

   Result.Width  := 6*BarSize+Left.Width+6*BarSize;
   Result.Height := BarSize+Left.Height+2*BarSize;

   // |
   // +-

   Result.Canvas.Polyline([Point(BarSize, BarSize),
                           Point(BarSize, Result.Height-BarSize), Point(5*BarSize, Result.Height-BarSize)]);

   // | a
   // +-

   Result.Canvas.CopyRect(Rect(6*BarSize, BarSize, 6*BarSize+Left.Width, Result.Height-2*BarSize),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // | a |
   // +- -+

   Result.Canvas.Polyline([Point(Result.Width-BarSize, BarSize),
                           Point(Result.Width-BarSize, Result.Height-BarSize), Point(Result.Width-5*BarSize, Result.Height-BarSize)]);

   // Free some stuff

   Left.Free;
End;

//==============================================================================

Function TCmdGraph.AddCeil(Var aCmdBinTree: TCmdBinTree;
                           Const aFontSize: Integer): TBitmap;
Var
   BarSize: Integer;
   Left: TBitmap;
Begin
   BarSize := GetBarSize(aFontSize);

   Result := CreateBmp(aFontSize, BarSize);

   Left := Render(aCmdBinTree.Left, aFontSize);

   // New dimensions of the bitmap

   aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline+BarSize;

   Result.Width  := 6*BarSize+Left.Width+6*BarSize;
   Result.Height := 2*BarSize+Left.Height+BarSize;

   // +-
   // |

   Result.Canvas.Polyline([Point(BarSize, Result.Height-BarSize),
                           Point(BarSize, BarSize), Point(5*BarSize, BarSize)]);

   // +-
   // | a

   Result.Canvas.CopyRect(Rect(6*BarSize, 2*BarSize, 6*BarSize+Left.Width, Result.Height-BarSize),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // +- -+
   // | a |

   Result.Canvas.Polyline([Point(Result.Width-BarSize, Result.Height-BarSize),
                           Point(Result.Width-BarSize, BarSize), Point(Result.Width-7*BarSize, BarSize)]);

   // Free some stuff

   Left.Free;
End;

//==============================================================================

Function TCmdGraph.AddFact(Var aCmdBinTree: TCmdBinTree;
                           Const aFontSize: Integer): TBitmap;
Var
   Left: TBitmap;
   FactHeight: Integer;
Begin
   Result := CreateBmp(aFontSize);

   Left := Render(aCmdBinTree.Left, aFontSize);

   // Determine whether "Left" needs parentheses around it

   Case aCmdBinTree.Left.ItemType Of
      itEq, itNEq, itGT, itLT, itGEq, itLEq,
      itPlus, itMinus, itTimes, itPow, itRoot,
      itExp, itLN, itLog,
      itAnd, itOr, itXOr, itNot,
      itSin, itCos, itTan, itSec, itCsc, itCot,
      itSinH, itCosH, itTanH, itSecH, itCscH, itCotH,
      itASin, itACos, itATan, itASec, itACsc, itACot,
      itASinH, itACosH, itATanH, itASecH, itACscH, itACotH:
         AddParentheses(Left, aCmdBinTree.Left, aFontSize);
   End;

   // New dimensions of the bitmap

   aCmdBinTree.Baseline := aCmdBinTree.Left.Baseline; 

   FactHeight := Result.Canvas.TextHeight(aCmdBinTree.Item);

   Result.Width  := Left.Width+Result.Canvas.TextWidth(aCmdBinTree.Item);
{$IFDEF OPT_MATH}
   Result.Height := OptMaxI(Left.Height, aCmdBinTree.Baseline+FactHeight);
{$ELSE}
   Result.Height := Max(Left.Height, aCmdBinTree.Baseline+FactHeight);
{$ENDIF}

   // a

   Result.Canvas.CopyRect(Rect(0, Result.Height-Left.Height, Left.Width, Result.Height),
                          Left.Canvas,
                          Rect(0, 0, Left.Width, Left.Height));

   // a!

   Result.Canvas.TextOut(Left.Width, Result.Height-FactHeight-aCmdBinTree.Baseline, aCmdBinTree.Item);

   // Free some stuff

   Left.Free;
End;

//==============================================================================

Procedure TCmdGraph.Paint;
Var
   Buffer: TBitmap;
   RelLeft, RelTop: Integer;
   Mult, Mult2: Double;
Begin
   If (FInvalid) Then Begin
      If (Not FInvalidBitmap.Empty) Then Begin
         Buffer := TBitmap.Create;

         Buffer.Canvas.Brush.Color := FInvalidBitmap.Canvas.Pixels[0, 0];

         Buffer.Width  := Width;
         Buffer.Height := Height;

         Buffer.Canvas.FillRect(Rect(0, 0, Width, Height));

         Mult  := Buffer.Width/FInvalidBitmap.Width;
         Mult2 := Buffer.Height/FInvalidBitmap.Height;

         If (Mult2 < Mult) Then
            Mult := Mult2;

         Mult := 0.95*Mult;

         RelLeft := (Buffer.Width-Round(Mult*FInvalidBitmap.Width)) Div 2;
         RelTop  := (Buffer.Height-Round(Mult*FInvalidBitmap.Height)) Div 2;

         Buffer.Canvas.CopyRect(Rect(RelLeft, RelTop, RelLeft+Round(Mult*FInvalidBitmap.Width), RelTop+Round(Mult*FInvalidBitmap.Height)),
                                FInvalidBitmap.Canvas,
                                Rect(0, 0, FInvalidBitmap.Width, FInvalidBitmap.Height));

         Canvas.CopyRect(Rect(0, 0, Width, Height),
                         Buffer.Canvas,
                         Rect(0, 0, Buffer.Width, Buffer.Height));

         Buffer.Free;
      End;
   End Else Begin
      Buffer := Bitmap;

      If (Buffer <> Nil) Then Begin
         RelLeft := (Width-Buffer.Width) Div 2;
         RelTop  := (Height-Buffer.Height) Div 2;

         Canvas.CopyRect(Rect(RelLeft, RelTop, RelLeft+Buffer.Width, RelTop+Buffer.Height),
                         Buffer.Canvas,
                         Rect(0, 0, Buffer.Width, Buffer.Height));

         Buffer.Free;
      End;
   End;
End;

//==============================================================================

Procedure TCmdGraph.SetOptions(Const aValue: TCmdGraphOptions);
Begin
   FOptimisedFontSize := aValue.OptimisedFontSize;
   FUnderscoreForSub  := aValue.UnderscoreForSub;
   FGreekSymbols      := aValue.GreekSymbols;
   FDigitGrouping     := aValue.DigitGrouping;
   Font.Name          := aValue.FontName;
   Font.Size          := aValue.FontSize;
   FSubFont           := aValue.SubFont;

   Color      := aValue.BackgroundColor;
   Font.Color := aValue.ForegroundColor;

   Repaint;
End;

//==============================================================================

Function TCmdGraph.Bitmap: TBitmap;
   Procedure ResetBaselines(Var aCmdBinTree: TCmdBinTree);
   Begin
      If (aCmdBinTree.Left <> Nil) Then
         ResetBaselines(aCmdBinTree.Left);

      If (aCmdBinTree.Right <> Nil) Then
         ResetBaselines(aCmdBinTree.Right);

      aCmdBinTree.Baseline := 0;
   End;
Var
   Buffer: TBitmap;
Begin
   If (Not FInvalid And (FCmdBinTree <> Nil)) Then Begin
      Font.Charset := GREEK_CHARSET;   // Just to be 100% sure that we can
                                       // render Greek characters...

      If (FOptimisedFontSize) Then Begin
         // Render the equation in two stages:
         //    1) Render it once to get the default dimensions
         //    2) Render it a second time to get a near to optimal dimensions
         //       based on the default ones (note that the "0.95" is to allow
         //       for a margin error of 5%)
         // Note: the above two renderings don't necessarily yield a perfect
         //       rendering, so in order to get one, we should compute a scaling
         //       factor to update the width and height of the equation so it
         //       perfectly fits the dimensions for the component. Because of
         //       the two previous renderings, the scaling factor would be very
         //       optimised, i.e. close to 1... However, there is one major
         //       issue with this solution, which is that "CopyRect" (or
         //       "StretchDraw") would have quite a bit of work to rescale the
         //       equation. This would therefore take some time and make the
         //       overall rendering rather slow, hence we don't do that and
         //       simply accept the fact that the final rendering is can only be
         //       almost optimal, but not optimal...

         ResetBaselines(FCmdBinTree);

         Buffer := Render(FCmdBinTree, Font.Size);

         ResetBaselines(FCmdBinTree);

{$IFDEF OPT_MATH}
         Result := Render(FCmdBinTree, Round(0.95*Font.Size*OptMinD(Width/Buffer.Width, Height/Buffer.Height)));
{$ELSE}
         Result := Render(FCmdBinTree, Round(0.95*Font.Size*Min(Width/Buffer.Width, Height/Buffer.Height)));
{$ENDIF}

         Buffer.Free;
      End Else Begin
         ResetBaselines(FCmdBinTree);

         Result := Render(FCmdBinTree, Font.Size);
      End
   End Else
      Result := Nil;
End;

//==============================================================================

Procedure TCmdGraph.CopyToClipboard;
Var
   Buffer: TBitmap;
Begin
   Buffer := Bitmap;

   If (Buffer <> Nil) Then
      Clipboard.Assign(Buffer);

   Buffer.Free;
End;

//==============================================================================

Procedure TCmdGraph.SetCmdBinTree(Const aValue: TCmdBinTree);
Begin
   FInvalid := False;

   FCmdBinTree := aValue;

   Repaint;
End;

//==============================================================================

Procedure TCmdGraph.SetInvalidBitmap(aValue: TBitmap);
Begin
   FInvalidBitmap.Assign(aValue);

   Repaint;
End;

//==============================================================================

Procedure TCmdGraph.SetInvalid(Const aValue: Boolean);
Begin
   If (aValue <> FInvalid) Then Begin
      FInvalid := aValue;

      Repaint;
   End;
End;

//==============================================================================

Procedure TCmdGraph.SetOptimisedFontSize(Const aValue: Boolean);
Begin
   If (aValue <> FOptimisedFontSize) Then Begin
      FOptimisedFontSize := aValue;

      Repaint;
   End;
End;

//==============================================================================

Procedure TCmdGraph.SetUnderscoreForSub(Const aValue: Boolean);
Begin
   If (aValue <> FUnderscoreForSub) Then Begin
      FUnderscoreForSub := aValue;

      Repaint;
   End;
End;

//==============================================================================

Procedure TCmdGraph.SetGreekSymbols(Const aValue: Boolean);
Begin
   If (aValue <> FGreekSymbols) Then Begin
      FGreekSymbols := aValue;

      Repaint;
   End;
End;

//==============================================================================

Procedure TCmdGraph.SetDigitGrouping(Const aValue: Boolean);
Begin
   If (aValue <> FDigitGrouping) Then Begin
      FDigitGrouping := aValue;

      Repaint;
   End;
End;

//==============================================================================

Procedure TCmdGraph.SetMonochrome(Const aValue: Boolean);
Begin
   If (aValue <> FMonochrome) Then Begin
      FMonochrome := aValue;

      Repaint;
   End;
End;

//==============================================================================

Procedure TCmdGraph.SetSubFont(Const aValue: Byte);
Begin
   If ((aValue <> FSubFont) And
       (aValue >= 10) And (aValue <= 90)) Then Begin
      FSubFont := aValue;

      Repaint;
   End;
End;

//==============================================================================

Function TCmdGraph.GetIdealWidth: Integer;
Var
   Buffer: TBitmap;
Begin
   If ((FCmdBinTree <> Nil) And Not FOptimisedFontSize) Then Begin
      Buffer := Render(FCmdBinTree, Font.Size);

      Result := Buffer.Width;

      Buffer.Free;
   End Else
      Result := Width;
End;

//==============================================================================

Function TCmdGraph.GetIdealHeight: Integer;
Var
   Buffer: TBitmap;
Begin
   If ((FCmdBinTree <> Nil) And Not FOptimisedFontSize) Then Begin
      Buffer := Render(FCmdBinTree, Font.Size);

      Result := Buffer.Height;

      Buffer.Free;
   End Else
      Result := Height;
End;

//==============================================================================

Procedure Register;
Begin
   RegisterComponents('COR', [TCmdGraph]);
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

