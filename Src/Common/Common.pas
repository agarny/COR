//==============================================================================
// Common unit
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 27/04/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit Common;

//==============================================================================

Interface

//==============================================================================

Uses
   Windows, Dialogs, SysUtils, IdGlobal, ComCtrls, ShlObj, Graphics;

//==============================================================================

Type
   TCPUVendor = (cvUnknown, cvAMD, cvCentaur, cvCyrix, cvIntel, cvTransmeta,
                 cvNexGen, cvRise, cvUMC, cvNSC, cvSiS);
   TSpeedFormat = (sfUndefined, sfHz, sfKHz, sfMHz, sfGHz);
   TBytesFormat = (bfUndefined, bfB, bfKB, bfMB, bfGB);
   TRGB = Record
      R: Byte;
      G: Byte;
      B: Byte;
   End;
   THSL = Record
      H: Byte;
      S: Byte;
      L: Byte;
   End;

//==============================================================================

Const
   UNDEFINED = -1;

   TAB: Char = Chr(9);
   SPACE: Char = ' ';
   CR: Char = Chr(13);
   LF: Char = Chr(10);

//==============================================================================

Var
   CPUVendor: TCPUVendor;

   TempDir: String;

   OpenDialog: TOpenDialog;
   SaveDialog: TSaveDialog;

   QPCFrequency: Int64;
   One_QPCFrequency: Double;

   CRLF: String;

//==============================================================================

Function TimerVal: Int64; Inline;
Function TimerValToSec(Const aTimerVal: Int64): Double; Inline;
Function TimerValToMSec(Const aTimerVal: Int64): Double; Inline;

Function Encrypt(Const aString: String; Const aPassword: String = ''): String;
Function Decrypt(Const aString: String; Const aPassword: String = ''): String;

Function FullFileName(Const aFileName: String; Const aRelativeTo: String = ''): String;
Function RelativeFileName(Const aFileName: String; Const aRelativeTo: String = ''): String;

Function DirectoryIsWritable(Const aDirName: String): Boolean;

Function RGBToHSL(Const aRGB: TRGB): THSL;
Function HSLToRGB(Const aHSL: THSL): TRGB;

Function ColorToHSL(Const aColor: TColor): THSL; Inline;
Function HSLToColor(Const aHSL: THSL): TColor; Inline;

Function RGBToRGBValues(Const aColor: LongInt): TRGB;

Function ToBytes(Const aValue: Double): TBytes; Overload;

Function BytesToString(Const aValue: TBytes; Const aIndex, aSize: Integer): String;
Function BytesToInteger(Const aValue: TBytes; Const aIndex: Integer): Integer;
Function BytesToInt64(Const aValue: TBytes; Const aIndex: Integer): Int64;
Function BytesToDouble(Const aValue: TBytes; Const aIndex: Integer): Double;
// Note: for some reasons, Indy doesn't handle doubles at all...

Function GroupDigits(Const aNbStr: String): String;

Function ConvertSpeed(Const aSpeed: Int64; Const aConvertFormat: TSpeedFormat = sfUndefined): String;
Function ConvertBytes(Const aNbOfBytes: Int64; Const aConvertFormat: TBytesFormat = bfUndefined): String;

Procedure ExtractResource(Const aResName, aResFileName: String); Inline;

Function SimplifyNbStr(Const aNbStr: String): String;
Function SimplifyNb(Const aNb: Integer): String; Overload; Inline;
Function SimplifyNb(Const aNb: Double): String; Overload; Inline;

Procedure SetToolBarWidth(aToolBar: TToolBar);

Function SelectFolderDialog(Const aCaption: String; Var aFolder: String; Const aFlags: UINT = BIF_USENEWUI): Boolean;

Function DblEquiv(Const aDblNb1, aDblNb2: Double): Boolean; Inline;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
   Forms, Classes;

//==============================================================================

Var
   TempDirArr: Array[0..MAX_PATH] Of Char;

//==============================================================================

Function GetCPUVendor: TCPUVendor;
   // Note: this code comes from the FastCode project
   //       (see http://sourceforge.net/projects/fastcode/)

   Type TRegisters = Packed Record
      EAX: Cardinal;
      EBX: Cardinal;
      ECX: Cardinal;
      EDX: Cardinal;
   End;
   Function IsCPUIDAvailable: Boolean; Register;
   Asm
      PUSHFD                 // Save EFLAGS to stack
      POP     EAX            // Store EFLAGS in EAX
      MOV     EDX, EAX       // Save in EDX for later testing
      XOR     EAX, $200000   // Flip ID bit in EFLAGS
      PUSH    EAX            // Save new EFLAGS value on stack
      POPFD                  // Replace current EFLAGS value
      PUSHFD                 // Get new EFLAGS
      POP     EAX            // Store new EFLAGS in EAX
      XOR     EAX, EDX       // Check if ID bit changed
      JZ      @Exit          // No, CPUID not available
      MOV     EAX, True      // Yes, CPUID is available
      @Exit:
   End;
   Procedure GetCPUID(Const aParam: Cardinal; Var aRegisters: TRegisters);
   Asm
     PUSH    EBX                         {save affected registers}
     PUSH    EDI
     MOV     EDI, aRegisters
     XOR     EBX, EBX                    {clear EBX register}
     XOR     ECX, ECX                    {clear ECX register}
     XOR     EDX, EDX                    {clear EDX register}
     DB $0F, $A2                         {CPUID opcode}
     MOV     TRegisters(EDI).&EAX, EAX   {save EAX register}
     MOV     TRegisters(EDI).&EBX, EBX   {save EBX register}
     MOV     TRegisters(EDI).&ECX, ECX   {save ECX register}
     MOV     TRegisters(EDI).&EDX, EDX   {save EDX register}
     POP     EDI                         {restore registers}
     POP     EBX
   end;
Type
   TCPUVendorStr = String[12];
Const
  CPUVendorStr : Array[Low(TCPUVendor)..High(TCPUVendor)] Of TCPUVendorStr =
                 ('', 'AuthenticAMD', 'CentaurHauls', 'CyrixInstead',
                  'GenuineIntel', 'GenuineTMx86', 'NexGenDriven',
                  'RiseRiseRise', 'UMC UMC UMC ', 'Geode by NSC',
                  'SiS SiS SiS');
Var
   VendorStr: TCPUVendorStr;
   Registers: TRegisters;
Begin
   If (IsCPUIDAvailable) Then Begin
      // Call CPUID function 0

      GetCPUID(0, Registers);

      // Get CPU vendor

      SetLength(VendorStr, 12);

      Move(Registers.EBX, VendorStr[1], 4);
      Move(Registers.EDX, VendorStr[5], 4);
      Move(Registers.ECX, VendorStr[9], 4);

      // Gget the CPU vendor from the CPU vendor string

      Result := High(TCPUVendor);

      While ((VendorStr <> CPUVendorStr[Result]) And (Result > Low(TCPUVendor))) Do
         Dec(Result);
   End Else
      Result := cvUnknown;
End;

//==============================================================================

Function TimerVal: Int64;
Begin
   QueryPerformanceCounter(Result);
End;

//==============================================================================

Function TimerValToSec(Const aTimerVal: Int64): Double;
Begin
   Result := aTimerVal*One_QPCFrequency;
End;

//==============================================================================

Function TimerValToMSec(Const aTimerVal: Int64): Double;
Begin
   Result := 1000*aTimerVal*One_QPCFrequency;
End;

//==============================================================================

Function EncryptDecrypt(Const aEncrypt: Boolean;
                        Const aString, aPassword: String): String;
   Function RotateBits(Const aChar: Char; aBits: Integer): Char; Inline;
   Var
      RotatedChar: Word;
   Begin
      aBits := aBits Mod 8;

      // Are we shifting left?

      If (aBits < 0) Then Begin
         // Put the data to the right half of a Word (2 bytes)

         RotatedChar := MakeWord(Byte(aChar), 0);

         // Now shift it to the left by the appropriate number of bits

         RotatedChar := RotatedChar ShL Abs(aBits);
      End Else Begin
         // Put the data to the left half of a Word (2 bytes)

         RotatedChar := MakeWord(0, Byte(aChar));

         // Now shift it to the right by the appropriate number of bits

         RotatedChar := RotatedChar ShR Abs(aBits);
      End;

      // Finally, swap the bytes

      RotatedChar := System.Swap(RotatedChar);

      // And OR the two halves together

      RotatedChar := Hi(RotatedChar) Or Lo(RotatedChar);

      Result := Chr(RotatedChar);
   End;
Var
   I, PasswordCheck, PasswordDigit, Direction, ShiftVal: Integer;
Begin
   // Note: this code is adapted from Robert Vivrette's original code
   //       (RobertV@Mail.com, http://www.undu.com/Articles/990413a.html)

  PasswordDigit := 0;
  PasswordCheck := 0;

   For I := 1 To Length(aPassword) Do
      Inc(PasswordCheck, Ord(aPassword[I]));

   Result := aString;

   If (aEncrypt) Then
      Direction := 1
   Else
      Direction := -1;

   For I := 1 To Length(Result) Do Begin
      If (Length(aPassword) = 0) Then
         ShiftVal := I
      Else
         ShiftVal := Ord(aPassword[PasswordDigit]);

      If (Not Odd(I)) Then
         Result[I] := RotateBits(Result[I], Direction*(ShiftVal+PasswordCheck))
      Else
         Result[I] := RotateBits(Result[I], -Direction*(ShiftVal+PasswordCheck));

      Inc(PasswordDigit);

      If (PasswordDigit > Length(aPassword)) Then
         PasswordDigit := 0;
   End;
End;

//==============================================================================

Function Encrypt(Const aString, aPassword: String): String;
Begin
   Result := EncryptDecrypt(True, aString, aPassword);
End;

//==============================================================================

Function Decrypt(Const aString, aPassword: String): String;
Begin
   Result := EncryptDecrypt(False, aString, aPassword);
End;

//==============================================================================

Function FullFileName(Const aFileName, aRelativeTo: String): String;
Var
   RelativeTo: String;
Begin
   If (CompareStr(aRelativeTo, '') = 0) Then
      // No relative information has been provided, so...

      RelativeTo := Application.ExeName
   Else
      RelativeTo := aRelativeTo;

   Result := aFileName;   // By default, we assume we have a different drive or
                          // that it's the same file

   If (CompareStr(ExtractFileDrive(aFileName), '') = 0) Then
      Result := IncludeTrailingPathDelimiter(ExtractFileDir(aRelativeTo))+aFileName;
End;

//==============================================================================

Function RelativeFileName(Const aFileName, aRelativeTo: String): String;
Var
   RelativeTo: String;
   PosVal, BackslashPosVal: Integer;
   Rem1, Rel1, Rel2: String;
Begin
   If (CompareStr(aRelativeTo, '') = 0) Then
      // No relative information has been provided, so...

      RelativeTo := Application.ExeName
   Else
      RelativeTo := aRelativeTo;

   Result := aFileName;   // By default, we assume we have a different drive or
                          // that it's the same file

   BackslashPosVal := 0;

   For PosVal := 1 To Length(RelativeTo) Do Begin
      If (RelativeTo[PosVal] <> aFileName[PosVal]) Then
         Break;

      If (RelativeTo[PosVal] = PathDelim) Then
         BackslashPosVal := PosVal;
   End;

   If ((PosVal <> 1) And (PosVal <> Length(RelativeTo))) Then Begin
      PosVal := BackslashPosVal+1;

      Rem1 := Copy(RelativeTo, PosVal, Length(RelativeTo)-PosVal+1);
      Rel1 := '';
      Rel2 := Copy(aFileName, PosVal, Length(aFileName)-PosVal+1);

      PosVal := Pos(PathDelim, Rem1);

      While (PosVal <> 0) Do Begin
         Rem1 := Copy(Rem1, PosVal+1, Length(Rem1)-PosVal);

         Rel1 := Rel1+'..'+PathDelim;

         PosVal := Pos(PathDelim, Rem1);
      End;

      Result := Rel1+Rel2;   // Relative file name
   End;
End;

//==============================================================================

Function DirectoryIsWritable(Const aDirName: String): Boolean;
Var
   TempFileName: String;
   TempFileID: TextFile;
Begin
   TempFileName := IncludeTrailingPathDelimiter(aDirName)+'~Temp~';

   AssignFile(TempFileID, TempFileName);

{$I-}
   Rewrite(TempFileID);

   Result := IOResult = 0;

   CloseFile(TempFileID);
{$I+}

   DeleteFile(TempFileName);
End;

//==============================================================================

Function RGBToHSL(Const aRGB: TRGB): THSL;
Const
   ONE_255 = 1/255;
   ONE_6   = 1/6;
   ONE_3   = 1/3;
   TWO_3   = 2/3;
Var
   RealR, RealG, RealB: Double;
   RealH, RealS, RealL: Double;
   RGBMin, RGBMax, RGBDiff: Double;
   RComp, GComp, BComp: Double;
Begin
   // Personal implementation of the algorithm that can be found at
   // http://www.easyrgb.com/index.php?X=MATH&H=18#text18

   RealR := ONE_255*aRGB.R;
   RealG := ONE_255*aRGB.G;
   RealB := ONE_255*aRGB.B;

{$IFDEF OPT_MATH}
   RGBMin := OptMinD(RealR, OptMinD(RealG, RealB));
   RGBMax := OptMaxD(RealR, OptMaxD(RealG, RealB));
{$ELSE}
   RGBMin := Min(RealR, Min(RealG, RealB));
   RGBMax := Max(RealR, Max(RealG, RealB));
{$ENDIF}

   RGBDiff := RGBMax-RGBMin;

   RealL := 0.5*(RGBMax+RGBMin);

   If (RGBDiff = 0) Then Begin
      RealH := 0;
      RealS := 0;
   End Else Begin
      If (RealL < 0.5) Then
         RealS := RGBDiff/(RGBMax+RGBMin)
      Else
         RealS := RGBDiff/(2-RGBMax-RGBMin);

      RComp := ((ONE_6*(RGBMax-RealR))+(0.5*RGBDiff))/RGBDiff;
      GComp := ((ONE_6*(RGBMax-RealG))+(0.5*RGBDiff))/RGBDiff;
      BComp := ((ONE_6*(RGBMax-RealB))+(0.5*RGBDiff))/RGBDiff;

      If (RealR = RGBMax) Then
         RealH := BComp-GComp
      Else If (RealG = RGBMax) Then
         RealH := ONE_3+RComp-BComp
      Else If (RealB = RGBMax) Then
         RealH := TWO_3+GComp-RComp
      Else
         RealH := 0;

      If (RealH < 0) Then
         RealH := RealH+1
      Else If (RealH > 1) Then
         RealH := RealH-1;
   End;

   Result.H := Round(240*RealH);
   Result.S := Round(240*RealS);
   Result.L := Round(240*RealL);
End;

//==============================================================================

Function HSLToRGB(Const aHSL: THSL): TRGB;
   Function HueToRGB(aX, aY, aH: Double): Double;
   Const
      TWO_3 = 2/3;
   Begin
      If (aH < 0) Then
         aH := aH+1
      Else If (aH > 1) Then
         aH := aH-1;

      If ((6*aH) < 1) Then
         Result := aX+(aY-aX)*6*aH
      Else If ((2*aH) < 1) Then
         Result := aY
      Else If ((3*aH) < 2) Then
         Result := aX+(aY-aX)*(TWO_3-aH)*6
      Else
         Result := aX;
   End;
Const
   ONE_240 = 1/240;
   ONE_3   = 1/3;
Var
   RealH, RealS, RealL: Double;
   X, Y: Double;
Begin
   // Personal implementation of the algorithm that can be found at
   // http://www.easyrgb.com/index.php?X=MATH&H=18#text19

   RealH := ONE_240*aHSL.H;
   RealS := ONE_240*aHSL.S;
   RealL := ONE_240*aHSL.L;

   If (RealS = 0)   Then Begin
      Result.R := Round(255*RealL);
      Result.G := Round(255*RealL);
      Result.B := Round(255*RealL);
   End Else Begin
      If (RealL < 0.5) Then
         Y := RealL*(1+RealS)
      Else
         Y := RealL+RealS-(RealS*RealL);

      X := 2*RealL-Y;

      Result.R := Round(255*HueToRGB(X, Y, RealH+ONE_3));
      Result.G := Round(255*HueToRGB(X, Y, RealH));
      Result.B := Round(255*HueToRGB(X, Y, RealH-ONE_3));
   End;
End;

//==============================================================================

Function ColorToHSL(Const aColor: TColor): THSL;
Begin
   Result := RGBToHSL(RGBToRGBValues(aColor));
End;

//==============================================================================

Function HSLToColor(Const aHSL: THSL): TColor;
Var
   RGBVal: TRGB;
Begin
   RGBVal := HSLToRGB(aHSL);

   Result := RGB(RGBVal.R, RGBVal.G, RGBVal.B);
End;

//==============================================================================

Function RGBToRGBValues(Const aColor: LongInt): TRGB;
Begin
   Result.R := aColor Mod 256;
   Result.G := (aColor ShR 8) Mod 256;
   Result.B := aColor ShR 16;
End;

//==============================================================================

Function ToBytes(Const aValue: Double): TBytes;
Begin
   SetLength(Result, SizeOf(Double));

   PDouble(@Result[0])^ := aValue;
End;

//==============================================================================

Function BytesToString(Const aValue: TBytes;
                       Const aIndex, aSize: Integer): String;
Begin
   If (aIndex+aSize > Length(aValue)) Then
      Result := '???'
   Else
      Result := IdGlobal.BytesToString(aValue, aIndex, aSize);
End;

//==============================================================================

Function BytesToInteger(Const aValue: TBytes;
                        Const aIndex: Integer): Integer;
Begin
   If (aIndex+SizeOf(Integer) > Length(aValue)) Then
      Result := 0
   Else
      Result := IdGlobal.BytesToInteger(aValue, aIndex);
End;

//==============================================================================

Function BytesToInt64(Const aValue: TBytes;
                      Const aIndex: Integer): Int64;
Begin
   If (aIndex+SizeOf(Int64) > Length(aValue)) Then
      Result := 0
   Else
      Result := IdGlobal.BytesToInt64(aValue, aIndex);
End;

//==============================================================================

Function BytesToDouble(Const aValue: TBytes;
                       Const aIndex: Integer): Double;
Begin
   If (aIndex+SizeOf(Double) > Length(aValue)) Then
      Result := 0
   Else
      Result := PDouble(@AValue[aIndex])^;
End;

//==============================================================================

Function GroupDigits(Const aNbStr: String): String;
Const
   GROUP_SIZE = 3;
Var
   DecPos, I: Integer;
Begin
   If (aNbStr[1] = '-') Then
      Result := Copy(aNbStr, 2, Length(aNbStr)-1)
   Else
      Result := aNbStr;

   DecPos := Pos(DecimalSeparator, Result);

   If (DecPos = 0) Then
      DecPos := Length(Result)+1;

   // Group the digits that are in front of the decimal separator

   I := DecPos-GROUP_SIZE;

   While I > 1 Do Begin
      Result := Copy(Result, 1, I-1)+ThousandSeparator+Copy(Result, I, Length(Result)-I+1);

      Dec(I, GROUP_SIZE);

      Inc(DecPos);
   End;

   // Group the digits that are after the decimal separator

   I := DecPos+GROUP_SIZE+1;

   While I <= Length(Result) Do Begin
      Result := Copy(Result, 1, I-1)+ThousandSeparator+Copy(Result, I, Length(Result)-I+1);

      Inc(I, GROUP_SIZE+1);
   End;

   // Re-insert the minus sign, if necessary

   If (aNbStr[1] = '-') Then
      Result := '-'+Result;
End;

//==============================================================================

Function ConvertSpeed(Const aSpeed: Int64;
                      Const aConvertFormat: TSpeedFormat): String;
Var
   ConvFormat: TSpeedFormat;
   Speed: Double;
   UnitStr: String;
Begin
   If (((aSpeed < 1000) And (aConvertFormat = sfUndefined)) Or (aConvertFormat = sfHz)) Then
      ConvFormat := sfHz
   Else If (((aSpeed < 1000000) And (aConvertFormat = sfUndefined)) Or (aConvertFormat = sfKHz)) Then
      ConvFormat := sfKHz
   Else If (((aSpeed < 1000000000) And (aConvertFormat = sfUndefined)) Or (aConvertFormat = sfMHz)) Then
      ConvFormat := sfMHz
   Else If (((aSpeed >= 1000000000) And (aConvertFormat = sfUndefined)) Or (aConvertFormat = sfGHz)) Then
      ConvFormat := sfGHz
   Else
      ConvFormat := sfHz;

   Case ConvFormat Of
      sfKHz: Begin
         Speed := 0.001*aSpeed;

         UnitStr := 'KHz';
      End;
      sfMHz: Begin
         Speed := 0.001*Round(0.001*aSpeed);

         UnitStr := 'MHz';
      End;
      sfGHz: Begin
         Speed := 0.001*Round(0.000001*aSpeed);

         UnitStr := 'GHz';
      End;
   Else
      Speed := aSpeed;

      UnitStr := 'Hz';
   End;

   Result := GroupDigits(FloatToStr(Speed))+' '+UnitStr;
End;

//==============================================================================

Function ConvertBytes(Const aNbOfBytes: Int64;
                      Const aConvertFormat: TBytesFormat): String;
Var
   ConvFormat: TBytesFormat;
   NbOfxBytes: Int64;
   UnitStr: String;
Begin
   If (((aNbOfBytes < 1 ShL 10) And (aConvertFormat = bfUndefined)) Or (aConvertFormat = bfB)) Then
      ConvFormat := bfB
   Else If (((aNbOfBytes < 1 ShL 20) And (aConvertFormat = bfUndefined)) Or (aConvertFormat = bfKB)) Then
      ConvFormat := bfKB
   Else If (((aNbOfBytes < 1 ShL 30) And (aConvertFormat = bfUndefined)) Or (aConvertFormat = bfMB)) Then
      ConvFormat := bfMB
   Else If (((aNbOfBytes >= 1 ShL 30) And (aConvertFormat = bfUndefined)) Or (aConvertFormat = bfGB)) Then
      ConvFormat := bfGB
   Else
      ConvFormat := bfB;

   Case ConvFormat Of
      bfKB: Begin
         NbOfxBytes := aNbOfBytes Div (1 ShL 10);

         UnitStr := 'KB';
      End;
      bfMB: Begin
         NbOfxBytes := aNbOfBytes Div (1 ShL 20);

         UnitStr := 'MB';
      End;
      bfGB: Begin
         NbOfxBytes := aNbOfBytes Div (1 ShL 30);

         UnitStr := 'GB';
      End;
   Else
      NbOfxBytes := aNbOfBytes;

      UnitStr := 'B';
   End;

   Result := GroupDigits(IntToStr(NbOfxBytes))+' '+UnitStr;
End;

//==============================================================================

Procedure ExtractResource(Const aResName, aResFileName: String);
Begin
   With TResourceStream.Create(HInstance, aResName, RT_RCDATA) Do Begin
      SaveToFile(aResFileName);

      Free;
   End;
End;

//==============================================================================

Function SimplifyNbStr(Const aNbStr: String): String;
   Function SimpNbStr(Const aNbStr: String): String;
   Var
      DummyNb: Double;
      SplitPos: Integer;
      NewResult: String;
   Begin
      Result := aNbStr;

      If (TryStrToFloat(Result, DummyNb)) Then Begin
         // Remove the "-" sign, if necessary

         If (aNbStr[1] = '-') Then
            Result := Copy(aNbStr, 2, Length(aNbStr)-1)
         Else
            Result := aNbStr;

         // First: "000123.456" to "123.456"

         If (Length(Result) > 1) Then Begin
            SplitPos := 1;

            While ((SplitPos < Length(Result)) And (Result[SplitPos] = '0')) Do
               Inc(SplitPos);

            Result := Copy(Result, SplitPos, Length(Result)-SplitPos+1);
         End;

         // Second: ".123" to "0.123"

         If (Result[1] = DecimalSeparator) Then
            Result := '0'+Result;

         // Third: "123.456000" to "123.456"

         If (Length(Result) > 1) Then Begin
            SplitPos := Length(Result);

            While (Result[SplitPos] = '0') Do
               Dec(SplitPos);

            NewResult := Copy(Result, 1, SplitPos);

            If (Pos(DecimalSeparator, NewResult) <> 0) Then
               Result := NewResult;
         End;

         // Fourth: "123." to "123"

         If (Result[Length(Result)] = DecimalSeparator) Then
            Result := Copy(Result, 1, Length(Result)-1);

         // Remove any leading "+" sign, if necessary

         If (Result[1] = '+') Then
            Result := Copy(Result, 2, Length(Result)-1);

         // Put the leading "-" sign back in, if necessary

         If (aNbStr[1] = '-') Then
            Result := '-'+Result;
      End;
   End;
Var
   ePos: Integer;
Begin
   ePos := Pos('E', UpperCase(aNbStr));

   If (ePos <> 0) Then
      Result := SimpNbStr(Copy(aNbStr, 1, ePos-1))+'e'+SimpNbStr(Copy(aNbStr, ePos+1, Length(aNbStr)-ePos))
   Else
      Result := SimpNbStr(aNbStr);
End;

//==============================================================================

Function SimplifyNb(Const aNb: Integer): String;
Begin
   Result := SimplifyNbStr(IntToStr(aNb));
End;

//==============================================================================

Function SimplifyNb(Const aNb: Double): String;
Begin
   Result := SimplifyNbStr(FloatToStr(aNb));
End;

//==============================================================================

Procedure SetToolBarWidth(aToolBar: TToolBar);
Var
   I: Integer;
Begin
   aToolBar.ClientWidth := aToolBar.Indent;

   For I := 0 To aToolBar.ButtonCount-1 Do
      If (aToolBar.Buttons[I].Visible) Then
         aToolBar.ClientWidth := aToolBar.Width+aToolBar.Buttons[I].ClientWidth;
End;

//==============================================================================

Function SelectFolderDialog(Const aCaption: String; Var aFolder: String;
                            Const aFlags: UINT): Boolean;
   Function BrowseCallbackProc(aWnd: HWND; aMsg: UINT; aParam, aData: LPARAM): Integer; StdCall;
   Begin
      If (aMsg = BFFM_INITIALIZED) Then
         SendMessage(aWnd, BFFM_SETSELECTION, 1, aData);

     Result := 0;
   End;
Var
   BrowseInfo: TBrowseInfo;
   ItemIDList: PItemIDList;
   Path: PChar;
begin
   // Adapted from http://www.scalabium.com/faq/dct0157.htm

   Result := False;

   SHGetSpecialFolderLocation(Application.ActiveFormHandle, CSIDL_DRIVES, ItemIDList);

   With BrowseInfo Do Begin
      hwndOwner := GetActiveWindow;
      pidlRoot  := ItemIDList;

      SHGetSpecialFolderLocation(hwndOwner, CSIDL_DRIVES, ItemIDList);

      pszDisplayName := StrAlloc(MAX_PATH);
      lpszTitle      := PChar(aCaption);
      ulFlags        := aFlags;
      lpfn           := @BrowseCallbackProc;
      lParam         := LongInt(PChar(aFolder));
   End;

   ItemIDList := SHBrowseForFolder(BrowseInfo);

   StrDispose(BrowseInfo.pszDisplayName);

   Path := StrAlloc(MAX_PATH);

   If ((ItemIDList <> Nil) And SHGetPathFromIDList(ItemIDList, Path)) Then Begin
      aFolder := IncludeTrailingPathDelimiter(Path);

      Result := True;
   End;

   StrDispose(Path);
End;

//==============================================================================

Function DblEquiv(Const aDblNb1, aDblNb2: Double): Boolean;
Begin
   Result := Abs(aDblNb1-aDblNb2) < 2.2204460492503131E-16 {DBL_EPSILON};
   // Note: 2.2204460492503131E-16 is the epsilon to be used for double
   //       precision numbers
   //       http://threads.codegear.com/threads/threads.exe/view?commentid=27245
End;

//==============================================================================

Initialization

//==============================================================================

// Determine the CPU vendor

CPUVendor := GetCPUVendor;

// Determine the location of the temporary directory

If (GetTempPath(Length(TempDirArr), TempDirArr) = 0) Then
   // Didn't work, so use the directory where the executable is...

   TempDir := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName))
Else
   TempDir := TempDirArr;

// Open/save dialog box

OpenDialog := TOpenDialog.Create(Application);
SaveDialog := TSaveDialog.Create(Application);

OpenDialog.Options := [ofAllowMultiSelect,ofPathMustExist,ofEnableSizing];
SaveDialog.Options := [ofHideReadOnly,ofPathMustExist,ofEnableSizing];

// Miscellaneous

CRLF := CR+LF;

QueryPerformanceFrequency(QPCFrequency);

One_QPCFrequency := 1/QPCFrequency;

//==============================================================================

Finalization

//==============================================================================

//OpenDialog.Free;
//SaveDialog.Free;
// Note: no need to free them, since they belong to Application, which got rid
//       of them, so...

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

