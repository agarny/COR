//==============================================================================
// CellML scanner engine class
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 28/09/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLScannerEngine;

//==============================================================================

Interface

//==============================================================================

Uses
   Classes, CellMLAPI, DeCAL;

//==============================================================================

{$M+}

//==============================================================================

Type
   TCellMLScannerEngineSymbol = (sNil,   // Not really used (see
                                         // "TKeyword.Create")

                                 // Different CellML keywords

                                 sModel, sAs, sUnit, sFrom, sBase, sEndDef,
                                 sComp, sEndComp, sODE, sSel, sEndSel, sCase,
                                 sOtherwise, sGroup, sContainment,
                                 sEncapsulation, sFor, sIncl, sMap, sBetween,
                                 sVars, sOpeningBracket, sClosingBracket,
                                 sOpeningCurlyBracket, sClosingCurlyBracket,

                                 // Definition, variable, identifier or
                                 // derivative

                                 sDef, sVar, sIdentifier,

                                 // Parameters of a unit's definition

                                 sPref, sExpo, sMult, sOff,

                                 // Parameters of a variable's definition

                                 sInit, sPub, sPriv,

                                 // Number

                                 sNumber, sTooBigANumber,

                                 // Prefixes

                                 sYotta, sZetta, sExa, sPeta, sTera, sGiga,
                                 sMega, sKilo, sHecto, sDeka, sDeci, sCenti,
                                 sMilli, sMicro, sNano, sPico, sFemto, sAtto,
                                 sZepto, sYocto,

                                 // Public/private interfaces

                                 sNone, sIn, sOut,

                                 // Mathematical operators

                                 sEq, sEqEq, sNEq, sLT, sGT, sLEq, sGEq,
                                 sPlus, sMinus, sTimes, sDivide,
                                 sAnd, sOr, sXOr, sNot,

                                 // Mathematical functions with 1 argument

                                 sSqr, sSqrt, sAbs, sExp, sLN, sFloor, sCeil,
                                 sFact, sSin, sCos, sTan, sSec, sCsc, sCot,
                                 sSinH, sCosH, sTanH, sSecH, sCscH, sCotH,
                                 sASin, sACos, sATan, sASec, sACsc, sACot,
                                 sASinH, sACosH, sATanH, sASecH, sACscH, sACotH,

                                 // Mathematical functions with 1 or 2 arguments

                                 sLog,

                                 // Mathematical functions with 2 arguments

                                 sPow, sRoot,

                                 // Mathematical constants

                                 sTrue, sFalse, sPI, sExponentiale,

                                 // Miscellaneous

                                 sQuote, sComma, sColon, sSemiColon,
                                 sString, sUnknown, sEOF);
   TCellMLScannerEngineSymbolSet = Set Of TCellMLScannerEngineSymbol;
   TCellMLScannerEngineToken = Class
      Public
         // User's properties

         Sym: TCellMLScannerEngineSymbol;   // Symbol

         Line: Integer;   // Line where the token is located
         Col: Integer;    // Column where the token is located

         Str: String;   // String argument
         Nb: Double;    // Number argument

         Comment: String;   // Comment on the token, if any

         // Constructor & Destructor

         Constructor Create;

         // User's methods

         Procedure Initialise(Const aSym: TCellMLScannerEngineSymbol; Const aLine: Integer; Const aCol: Integer);
         Function  CellMLSym: TMathMLCommandBinTreeItemType;
   End;
   TCellMLScannerEngineCharType = (ctOther,
                                   ctLetter, ctDigit, ctUnderscore,
                                   ctDoubleQuote, ctQuote, ctComma, ctEq, ctLT,
                                   ctGT, ctPlus, ctMinus, ctTimes, ctDivide,
                                   ctColon, ctSemiColon, ctOpeningBracket,
                                   ctClosingBracket, ctOpeningCurlyBracket,
                                   ctClosingCurlyBracket, ctEOF);
   TCellMLScannerEngineKeyword = Class
      Public
         Str: String;
         Sym: TCellMLScannerEngineSymbol;

         Constructor Create(Const aStr: String; Const aSym: TCellMLScannerEngineSymbol = sNil);
   End;
   TCellMLScannerEngine = Class
      Private
         // Private representation of published properties

         FToken: TCellMLScannerEngineToken;   // Current token

         // Properties used for internal purposes

         LF: Char;    // Required as local constants to allow inlining of
         EOF: Char;   // methods that make use of them

         Stream: String;   // Stream, which contents is to be scanned
         StreamPos, StreamEOF: Integer;

         CrtChar: Char;   // Current character being scanned

         CharType: Array[0..65535] Of TCellMLScannerEngineCharType;
         // Lookup list for determining the type of a character

         // Methods used for internal purposes

         Procedure GetNextChar;

         Procedure GetWord;
         Procedure GetNumber;
         Procedure GetString;

      Public
         // Constructor & Destructor

         Constructor Create(Const aContents: String);
         Destructor Destroy; Override;

         Function GetNextToken: TCellMLScannerEngineSymbol;

      Published
         // Published properties

         Property Token: TCellMLScannerEngineToken Read FToken;
   End;

//==============================================================================

Var
   Keywords: DArray;   // List of keywords

//==============================================================================

Function KeywordsCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer;

//==============================================================================

Implementation

//==============================================================================

Uses
   SysUtils, Common;

//==============================================================================

Function KeywordsCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer;
Begin
   Result := CompareStr(TCellMLScannerEngineKeyword(aObj1.VObject).Str,
                        TCellMLScannerEngineKeyword(aObj2.VObject).Str);
End;

//==============================================================================

Constructor TCellMLScannerEngineToken.Create;
Begin
   Line := UNDEFINED;
   Col  := UNDEFINED;
End;

//==============================================================================

Procedure TCellMLScannerEngineToken.Initialise(Const aSym: TCellMLScannerEngineSymbol;
                                               Const aLine: Integer;
                                               Const aCol: Integer);
Begin
   Sym     := aSym;
   Line    := aLine;
   Col     := aCol;
   Str     := '';
   Nb      := 0;
   Comment := '';
End;

//==============================================================================

Function  TCellMLScannerEngineToken.CellMLSym: TMathMLCommandBinTreeItemType;
Begin
   Case Sym Of
      // Mathematical operators

      sEqEq:   Result := mitEqEq;
      sNEq:    Result := mitNEq;
      sLT:     Result := mitLT;
      sGT:     Result := mitGT;
      sLEq:    Result := mitLEq;
      sGEq:    Result := mitGEq;
      sPlus:   Result := mitPlus;
      sMinus:  Result := mitMinus;
      sTimes:  Result := mitTimes;
      sDivide: Result := mitDivide;
      sAnd:    Result := mitAnd;
      sOr:     Result := mitOr;
      sXOr:    Result := mitXOr;
      sNot:    Result := mitNot;

      // Mathematical functions with 1 argument

      sSqr:     Result := mitPow;
      sSqrt:    Result := mitRoot;
      sAbs:     Result := mitAbs;
      sExp:     Result := mitExp;
      sLN:      Result := mitLN;
      sCeil:    Result := mitCeil;
      sFloor:   Result := mitFloor;
      sFact:    Result := mitFact;
      sSin:     Result := mitSin;
      sCos:     Result := mitCos;
      sTan:     Result := mitTan;
      sSec:     Result := mitSec;
      sCsc:     Result := mitCsc;
      sCot:     Result := mitCot;
      sSinH:    Result := mitSinH;
      sCosH:    Result := mitCosH;
      sTanH:    Result := mitTanH;
      sSecH:    Result := mitSecH;
      sCscH:    Result := mitCscH;
      sCotH:    Result := mitCotH;
      sASin:    Result := mitASin;
      sACos:    Result := mitACos;
      sATan:    Result := mitATan;
      sASec:    Result := mitASec;
      sACsc:    Result := mitACsc;
      sACot:    Result := mitACot;
      sASinH:   Result := mitASinH;
      sACosH:   Result := mitACosH;
      sATanH:   Result := mitATanH;
      sASecH:   Result := mitASecH;
      sACscH:   Result := mitACscH;
      sACotH:   Result := mitACotH;

      // Mathematical functions with 1 or 2 arguments

      sLog: Result := mitLog;

      // Mathematical functions with 2 arguments

      sPow:  Result := mitPow;
      sRoot: Result := mitRoot;

      // Mathematical constants

      sTrue:         Result := mitTrue;
      sFalse:        Result := mitFalse;
      sPI:           Result := mitPI;
      sExponentiale: Result := mitExponentiale;
   Else
      Result := mitNil;
   End;
End;

//==============================================================================

Constructor TCellMLScannerEngineKeyword.Create(Const aStr: String;
                                               Const aSym: TCellMLScannerEngineSymbol);
Begin
   Str := aStr;
   Sym := aSym;
End;

//==============================================================================

Constructor TCellMLScannerEngine.Create(Const aContents: String);
Var
   I: Integer;
Begin
   FToken := TCellMLScannerEngineToken.Create;

   // Some constants

   LF  := Chr(10);
   EOF := Chr(0);

   // Initialise the character's type list
   // Note: by default all the characters are of "ctOther" type

   // Letters

   For I := Ord('A') To Ord('Z') Do
      CharType[I] := ctLetter;

   For I := Ord('a') To Ord('z') Do
      CharType[I] := ctLetter;

   // Digit

   For I := Ord('0') To Ord('9') Do
      CharType[I] := ctDigit;

   // Special characters

   CharType[Ord('_')]  := ctUnderscore;
   CharType[Ord('"')]  := ctDoubleQuote;
   CharType[Ord('''')] := ctQuote;
   CharType[Ord(',')]  := ctComma;
   CharType[Ord('=')]  := ctEq;
   CharType[Ord('<')]  := ctLT;
   CharType[Ord('>')]  := ctGT;
   CharType[Ord('+')]  := ctPlus;
   CharType[Ord('-')]  := ctMinus;
   CharType[Ord('*')]  := ctTimes;
   CharType[Ord('/')]  := ctDivide;
   CharType[Ord(':')]  := ctColon;
   CharType[Ord(';')]  := ctSemiColon;
   CharType[Ord('(')]  := ctOpeningBracket;
   CharType[Ord(')')]  := ctClosingBracket;
   CharType[Ord('{')]  := ctOpeningCurlyBracket;
   CharType[Ord('}')]  := ctClosingCurlyBracket;
   CharType[Ord(EOF)]  := ctEOF;

   // Put the contents of the file/equation into the stream

   Stream := aContents;

   StreamPos := 1;
   StreamEOF := Length(Stream)+1;

   // Initialise the line and column numbers

   FToken.Line := 1;
   Token.Col   := 0;

   // Get ready to scan the data

   GetNextChar;

   // Get the first token

   GetNextToken;
End;

//==============================================================================

Destructor TCellMLScannerEngine.Destroy;
Begin
   FToken.Free;
End;

//==============================================================================

Procedure TCellMLScannerEngine.GetNextChar;
Begin
   If (StreamPos = StreamEOF) Then
      // End of the file/equation, so simulate the EOF

      CrtChar := EOF
   Else Begin
      CrtChar := Stream[StreamPos];

      Inc(StreamPos);

      If (CrtChar = LF) Then Begin
         // Go to the next line

         Inc(FToken.Line);

         Token.Col := UNDEFINED;
      End;
   End;

   Inc(Token.Col);
End;

//==============================================================================

Procedure TCellMLScannerEngine.GetWord;
Var
   Keyword: TCellMLScannerEngineKeyword;
   KeywordIter: DIterator;
   I: Integer;
Begin
   // Get the word

   // EBNF: Letter { Letter } .

   FToken.Str := '';

   Repeat
      FToken.Str := FToken.Str+CrtChar;

      GetNextChar;
   Until Not (CharType[Ord(CrtChar)] In [ctLetter..ctUnderscore]);

   // Find out what kind of keyword it is, if any

   Keyword := TCellMLScannerEngineKeyword.Create(FToken.Str);

   KeywordIter := BinarySearch(Keywords, [Keyword]);

   If (Not AtEnd(KeywordIter)) Then
      FToken.Sym := TCellMLScannerEngineKeyword(DeCAL.GetObject(KeywordIter)).Sym
   Else Begin
      // Couldn't find a match for the string, so it has to be an identifier,
      // unless it's only made of underscores...

      FToken.Sym := sUnknown;

      For I := 1 To Length(FToken.Str) Do
         If (CharType[Ord(FToken.Str[I])] <> ctUnderscore) Then Begin
            FToken.Sym := sIdentifier;

            Break;
         End;
   End;

   Keyword.Free;
End;

//==============================================================================

Procedure TCellMLScannerEngine.GetNumber;
Begin
   // Get the number, which can only be a positive one, as we want to be able to
   // handle unary minuses (like in "-(3)" for instance). This means that "-3"
   // for instance would be scanned as being "sMinus" followed by "sNumber"

   // Note: to make the algorithm more efficient, we store each character
   //       directly after getting a new one (note that we enter this procedure
   //       with already one character stored, cf. "GetNextToken"), this means
   //       that we end up storing one character too many (i.e. the first
   //       character following the number), which we have to get rid of before
   //       converting the string into a proper number

   // EBNF: ctDigit { ctDigit } [ "." [ ctDigit { ctDigit } ] ] [ "e" [ "+" | "-" ] Digit { Digit } ]

   Repeat
      GetNextChar;

      FToken.Str := FToken.Str+CrtChar;
   Until CharType[Ord(CrtChar)] <> ctDigit;

   // Look for the fractional part if any...

   If (CrtChar = DecimalSeparator) Then
      Repeat
         GetNextChar;

         FToken.Str := FToken.Str+CrtChar;
      Until (CharType[Ord(CrtChar)] <> ctDigit);

   // Look for the exponent part if any...

   If (CrtChar = 'e') Then Begin
      GetNextChar;

      FToken.Str := FToken.Str+CrtChar;

      // Look for the exponent's sign, if any

      If ((CrtChar = '+') Or (CrtChar = '-')) Then Begin
         GetNextChar;

         FToken.Str := FToken.Str+CrtChar;
      End;

      // Get the exponent itself...

      If (CharType[Ord(CrtChar)] <> ctDigit) Then Begin
         // A digit was expected, so...

         FToken.Sym := sUnknown;

         FToken.Comment := 'a digit was expected after the exponent';

         FToken.Str := Copy(FToken.Str, 1, Length(FToken.Str)-1);

         Exit;
      End;

      Repeat
         GetNextChar;

         FToken.Str := FToken.Str+CrtChar;
      Until CharType[Ord(CrtChar)] <> ctDigit;
   End;

   // All done, so we have a number!

   FToken.Str := Copy(FToken.Str, 1, Length(FToken.Str)-1);
   // Note: because we stored one more character to make the algorithm faster

   If (TryStrToFloat(FToken.Str, FToken.Nb)) Then
      FToken.Sym := sNumber
   Else
      // Cannot properly get the number, this can be because there is a power
      // overflow for instance, so...

      FToken.Sym := sTooBigANumber;
End;

//==============================================================================

Procedure TCellMLScannerEngine.GetString;
Begin
   // EBNF: '"' { Character } '"'

   GetNextChar;

   While (CrtChar <> EOF) Do Begin
      FToken.Str := FToken.Str+char(CrtChar);

      If (CrtChar = '"') Then
         Exit;

      GetNextChar;
   End;

   If (CrtChar = EOF) Then Begin
      FToken.Sym := sUnknown;

      FToken.Comment := 'end of file found while retrieving a string';
   End Else Begin
      GetNextChar;

      FToken.Sym := sString;
   End;
End;

//==============================================================================

Function TCellMLScannerEngine.GetNextToken: TCellMLScannerEngineSymbol;
Begin
   // Skip all the spaces and special characters (includes TAB, LF, CR, etc.),
   // as well as comments

   While (((Ord(CrtChar) >= 9) And (Ord(CrtChar) <= 13)) Or (CrtChar = ' ')) Do
      GetNextChar;

   // Update the current token with the next one and get ready to fetch the one
   // after

   FToken.Initialise(sEOF, FToken.Line, FToken.Col);

   // No need to go further if we are at the end of the file

   FToken.Str := CrtChar;   // Default value of "FToken.Str"

   Case CharType[Ord(CrtChar)] Of
      ctLetter, ctUnderscore:
         GetWord;
      ctDigit: GetNumber;
      ctDoubleQuote: GetString;
      ctQuote: Begin
         FToken.Sym := sQuote;

         GetNextChar;
      End;
      ctComma: Begin
         FToken.Sym := sComma;

         GetNextChar;
      End;
      ctEq: Begin
         FToken.Sym := sEq;

         GetNextChar;

         If (CharType[Ord(CrtChar)] = ctEq) Then Begin
            FToken.Str := FToken.Str+CrtChar;

            FToken.Sym := sEqEq;

            GetNextChar;
         End;
      End;
      ctLT: Begin
         FToken.Sym := sLT;

         GetNextChar;

         Case CharType[Ord(CrtChar)] Of
            ctEq: Begin
               FToken.Str := FToken.Str+CrtChar;

               FToken.Sym := sLEq;

               GetNextChar;
            End;
            ctGT: Begin
               FToken.Str := FToken.Str+CrtChar;

               FToken.Sym := sNEq;

               GetNextChar;
            End;
         End;
      End;
      ctGT: Begin
         FToken.Sym := sGT;

         GetNextChar;

         If (CharType[Ord(CrtChar)] = ctEq) Then Begin
            FToken.Str := FToken.Str+CrtChar;

            FToken.Sym := sGEq;

            GetNextChar;
         End;
      End;
      ctPlus: Begin
         FToken.Sym := sPlus;

         GetNextChar;
      End;
      ctMinus: Begin
         FToken.Sym := sMinus;

         GetNextChar;
      End;
      ctTimes: Begin
         FToken.Sym := sTimes;

         GetNextChar;
      End;
      ctDivide: Begin
         FToken.Sym := sDivide;

         GetNextChar;
      End;
      ctColon: Begin
         FToken.Sym := sColon;

         GetNextChar;
      End;
      ctSemiColon: Begin
         FToken.Sym := sSemiColon;

         GetNextChar;
      End;
      ctOpeningBracket: Begin
         FToken.Sym := sOpeningBracket;

         GetNextChar;
      End;
      ctClosingBracket: Begin
         FToken.Sym := sClosingBracket;

         GetNextChar;
      End;
      ctOpeningCurlyBracket: Begin
         FToken.Sym := sOpeningCurlyBracket;

         GetNextChar;
      End;
      ctClosingCurlyBracket: Begin
         FToken.Sym := sClosingCurlyBracket;

         GetNextChar;
      End;
      ctEOF: FToken.Sym := sEOF;
      ctOther: Begin
         FToken.Sym := sUnknown;

         GetNextChar;
      End;
   End;

   Result := FToken.Sym;
End;

//==============================================================================

Initialization

//==============================================================================

Keywords := DArray.CreateWith(MakeComparator(KeywordsCompare));

With Keywords Do Begin
   // Different CellML keywords

   Add([TCellMLScannerEngineKeyword.Create('model', sModel)]);
   Add([TCellMLScannerEngineKeyword.Create('as', sAs)]);
   Add([TCellMLScannerEngineKeyword.Create('unit', sUnit)]);
   Add([TCellMLScannerEngineKeyword.Create('from', sFrom)]);
   Add([TCellMLScannerEngineKeyword.Create('base', sBase)]);
   Add([TCellMLScannerEngineKeyword.Create('enddef', sEndDef)]);
   Add([TCellMLScannerEngineKeyword.Create('comp', sComp)]);
   Add([TCellMLScannerEngineKeyword.Create('endcomp', sEndComp)]);
   Add([TCellMLScannerEngineKeyword.Create('ode', sODE)]);
   Add([TCellMLScannerEngineKeyword.Create('sel', sSel)]);
   Add([TCellMLScannerEngineKeyword.Create('endsel', sEndSel)]);
   Add([TCellMLScannerEngineKeyword.Create('case', sCase)]);
   Add([TCellMLScannerEngineKeyword.Create('otherwise', sOtherwise)]);
   Add([TCellMLScannerEngineKeyword.Create('group', sGroup)]);
   Add([TCellMLScannerEngineKeyword.Create('containment', sContainment)]);
   Add([TCellMLScannerEngineKeyword.Create('encapsulation', sEncapsulation)]);
   Add([TCellMLScannerEngineKeyword.Create('for', sFor)]);
   Add([TCellMLScannerEngineKeyword.Create('incl', sIncl)]);
   Add([TCellMLScannerEngineKeyword.Create('map', sMap)]);
   Add([TCellMLScannerEngineKeyword.Create('between', sBetween)]);
   Add([TCellMLScannerEngineKeyword.Create('vars', sVars)]);

   // Definition, variable or identifier

   Add([TCellMLScannerEngineKeyword.Create('def', sDef)]);
   Add([TCellMLScannerEngineKeyword.Create('var', sVar)]);

   // Parameters of a unit's definition

   Add([TCellMLScannerEngineKeyword.Create('pref', sPref)]);
   Add([TCellMLScannerEngineKeyword.Create('expo', sExpo)]);
   Add([TCellMLScannerEngineKeyword.Create('mult', sMult)]);
   Add([TCellMLScannerEngineKeyword.Create('off', sOff)]);

   // Parameters of a variable's definition

   Add([TCellMLScannerEngineKeyword.Create('init', sInit)]);
   Add([TCellMLScannerEngineKeyword.Create('pub', sPub)]);
   Add([TCellMLScannerEngineKeyword.Create('priv', sPriv)]);

   // Prefixes

   Add([TCellMLScannerEngineKeyword.Create('yotta', sYotta)]);
   Add([TCellMLScannerEngineKeyword.Create('zetta', sZetta)]);
   Add([TCellMLScannerEngineKeyword.Create('exa', sExa)]);
   Add([TCellMLScannerEngineKeyword.Create('peta', sPeta)]);
   Add([TCellMLScannerEngineKeyword.Create('tera', sTera)]);
   Add([TCellMLScannerEngineKeyword.Create('giga', sGiga)]);
   Add([TCellMLScannerEngineKeyword.Create('mega', sMega)]);
   Add([TCellMLScannerEngineKeyword.Create('kilo', sKilo)]);
   Add([TCellMLScannerEngineKeyword.Create('hecto', sHecto)]);
   Add([TCellMLScannerEngineKeyword.Create('deka', sDeka)]);
   Add([TCellMLScannerEngineKeyword.Create('deci', sDeci)]);
   Add([TCellMLScannerEngineKeyword.Create('centi', sCenti)]);
   Add([TCellMLScannerEngineKeyword.Create('milli', sMilli)]);
   Add([TCellMLScannerEngineKeyword.Create('micro', sMicro)]);
   Add([TCellMLScannerEngineKeyword.Create('nano', sNano)]);
   Add([TCellMLScannerEngineKeyword.Create('pico', sPico)]);
   Add([TCellMLScannerEngineKeyword.Create('femto', sFemto)]);
   Add([TCellMLScannerEngineKeyword.Create('atto', sAtto)]);
   Add([TCellMLScannerEngineKeyword.Create('zepto', sZepto)]);
   Add([TCellMLScannerEngineKeyword.Create('yocto', sYocto)]);

   // Public/private interfaces

   Add([TCellMLScannerEngineKeyword.Create('none', sNone)]);
   Add([TCellMLScannerEngineKeyword.Create('in', sIn)]);
   Add([TCellMLScannerEngineKeyword.Create('out', sOut)]);

   // Mathematical operators

   Add([TCellMLScannerEngineKeyword.Create('and', sAnd)]);
   Add([TCellMLScannerEngineKeyword.Create('or', sOr)]);
   Add([TCellMLScannerEngineKeyword.Create('xor', sXOr)]);
   Add([TCellMLScannerEngineKeyword.Create('not', sNot)]);

   // Mathematical functions with 1 argument

   Add([TCellMLScannerEngineKeyword.Create('sqr', sSqr)]);
   Add([TCellMLScannerEngineKeyword.Create('sqrt', sSqrt)]);
   Add([TCellMLScannerEngineKeyword.Create('abs', sAbs)]);
   Add([TCellMLScannerEngineKeyword.Create('exp', sExp)]);
   Add([TCellMLScannerEngineKeyword.Create('ln', sLN)]);
   Add([TCellMLScannerEngineKeyword.Create('floor', sFloor)]);
   Add([TCellMLScannerEngineKeyword.Create('ceil', sCeil)]);
   Add([TCellMLScannerEngineKeyword.Create('fact', sFact)]);
   Add([TCellMLScannerEngineKeyword.Create('sin', sSin)]);
   Add([TCellMLScannerEngineKeyword.Create('cos', sCos)]);
   Add([TCellMLScannerEngineKeyword.Create('tan', sTan)]);
   Add([TCellMLScannerEngineKeyword.Create('sec', sSec)]);
   Add([TCellMLScannerEngineKeyword.Create('csc', sCsc)]);
   Add([TCellMLScannerEngineKeyword.Create('cot', sCot)]);
   Add([TCellMLScannerEngineKeyword.Create('sinh', sSinH)]);
   Add([TCellMLScannerEngineKeyword.Create('cosh', sCosH)]);
   Add([TCellMLScannerEngineKeyword.Create('tanh', sTanH)]);
   Add([TCellMLScannerEngineKeyword.Create('sech', sSecH)]);
   Add([TCellMLScannerEngineKeyword.Create('csch', sCscH)]);
   Add([TCellMLScannerEngineKeyword.Create('coth', sCotH)]);
   Add([TCellMLScannerEngineKeyword.Create('asin', sASin)]);
   Add([TCellMLScannerEngineKeyword.Create('acos', sACos)]);
   Add([TCellMLScannerEngineKeyword.Create('atan', sATan)]);
   Add([TCellMLScannerEngineKeyword.Create('asec', sASec)]);
   Add([TCellMLScannerEngineKeyword.Create('acsc', sACsc)]);
   Add([TCellMLScannerEngineKeyword.Create('acot', sACot)]);
   Add([TCellMLScannerEngineKeyword.Create('asinh', sASinH)]);
   Add([TCellMLScannerEngineKeyword.Create('acosh', sACosH)]);
   Add([TCellMLScannerEngineKeyword.Create('atanh', sATanH)]);
   Add([TCellMLScannerEngineKeyword.Create('asech', sASecH)]);
   Add([TCellMLScannerEngineKeyword.Create('acsch', sACscH)]);
   Add([TCellMLScannerEngineKeyword.Create('acoth', sACotH)]);

   // Mathematical functions with 1 or 2 arguments

   Add([TCellMLScannerEngineKeyword.Create('log', sLog)]);

   // Mathematical functions with 2 arguments

   Add([TCellMLScannerEngineKeyword.Create('pow', sPow)]);
   Add([TCellMLScannerEngineKeyword.Create('root', sRoot)]);

   // Mathematical constants

   Add([TCellMLScannerEngineKeyword.Create('true', sTrue)]);
   Add([TCellMLScannerEngineKeyword.Create('false', sFalse)]);
   Add([TCellMLScannerEngineKeyword.Create('pi', sPI)]);
   Add([TCellMLScannerEngineKeyword.Create('e', sExponentiale)]);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

// Sort all the keywords

Sort(Keywords);

//==============================================================================

Finalization

//==============================================================================

FreeAndClear(Keywords);

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
