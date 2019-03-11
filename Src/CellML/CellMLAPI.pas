//==============================================================================
// CellML classes
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 17/07/2002
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPI;

//==============================================================================

Interface

//==============================================================================

Uses
   SysUtils, DeCAL, Dialogs, XMLIntf, XMLDoc, Classes, Engine;

//==============================================================================

{$M+}

//==============================================================================

Type
   TCellMLList = Class(DArray)
      Private
         // Properties used for internal purposes

         Managed: Boolean;

      Public
         // Constructor & Destructor

         Constructor Create(Const aManaged: Boolean = True); Reintroduce;
         Destructor Destroy; Override;
   End;
   TCellMLSortedList = Class(TCellMLList)
      Private
         // Properties used for internal purposes

         NeedSorting: Boolean;

      Public
         // Constructor & Destructor

         Constructor Create(Const aManaged: Boolean = True);

         // User's methods

         Function Compare(Const aObj1, aObj2: DObject): Integer; Inline;

         Procedure Add(Const aObjs: Array Of Const); Override;
         Function Find(Const aName: String): DIterator;
   End;
   TCellMLUnits = Class;
   TCellMLComponent = Class;
   TCellMLVariable = Class;
   TMathMLEquation = Class;
   TCellMLObject = Class
      Private
         Function AddMsg(Const aMsgType: TMsgDlgType; Const aFileName: String; Const aLine: Integer; Const aMsg: String): TEngineMsg;
         Procedure RemoveMsg(Const aMsg: TEngineMsg);

      Public
         // Published properties

         FileName: String;
         Line: Integer;

         // Constructor & Destructor

         Constructor Create(Const aFileName: String);

         // User's methods

         Function IsValid: Boolean; Virtual;
   End;
   TCellMLNamedObject = Class(TCellMLObject)
      Public
         // Published properties

         Name: String;

         // Constructor & Destructor

         Constructor Create(Const aFileName: String; Const aName: String = '');
   End;
   TMathMLCommandBinTree = Class;
   TCellMLModelVariableState = (vsUnknown, vsFree, vsState, vsConstant, vsComputed, vsMapped, vsMaybeFree, vsMaybeConstant);
   TCellMLModelEquationState = (esUnvisited, esVisited, esCompleted);
   TCellMLModelVariable = Class(TCellMLNamedObject)
      Public
         // Published properties

         Component: String;
         Variable: String;

         State: TCellMLModelVariableState;

         MappedComponent: String;
         MappedVariable: String;

         Units: String;

         InitialValue: String;

         VarName: String;
         VarIndex: Integer;

         GenName: Boolean;

         EqnState: Array[0..1] Of TCellMLModelEquationState;
         EqnBinTree: Array[0..1] Of TMathMLCommandBinTree;
         // Note: we need two states and trees. The first ones are for an
         //       algebraic equation while the second ones for an ODE. This is
         //       required since the first tree is tested within "CheckEquation"
         //       (under the name of "aMathMLEquationBinTree"), so...

         ComputeOnce: Boolean;

         PrevMsg: TEngineMsg; 

         // Constructor & Destructor

         Constructor Create(Const aFileName: String; Const aLine: Integer; Const aComponent, aVariable: String);

         // User's methods

         Function RealName: String; Inline;
         Function GenInfo: String;
   End;
   TCellMLModelEquation = Class
      Public
         // Published properties

         Component: String;

         EqnBinTree: TMathMLCommandBinTree;

         ComputeOnce: Boolean;

         // Constructor & Destructor

         Constructor Create(Const aComponent: String; Const aEqnBinTree: TMathMLCommandBinTree);
   End;
   TCellMLComponentRef = Class;
   TCellMLModelExtra = Class
      Public
         // Published properties

         Metadata: TXMLDocument;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TComponent);
         Destructor Destroy; Override;

         // User's methods

         Procedure Init(Const aExtra: TCellMLModelExtra);
   End;
   TCellMLModel = Class(TCellMLNamedObject)
      Private
         // Properties used for internal purposes

         DoUnitsChecking: Boolean;
         UnitsAreValid: Boolean;

         // Methods used for internal purposes

         Function GlobalVariable(Const aComponent, aVariable: String): TCellMLModelVariable;

      Protected
         // Private representation of published properties

         FExtra: TCellMLModelExtra;

      Public
         // Published properties

         FreeVariable: String;   // Used for deriving
         FreeVariableUnits: TCellMLUnits;

         UnitsList: TCellMLSortedList;
         ComponentList: TCellMLSortedList;
         GroupList: TCellMLList;
         ConnectionList: TCellMLList;

         GlobalVariableList: TCellMLSortedList;
         EquationList: TCellMLList;

         NbOfStateVariables: Integer;

         RequiredFunctions: DArray;

         // Constructor & Destructor

         Constructor Create(Const aFileName: String);
         Destructor Destroy; Override;

         // User's methods

         Function IsValid(Const aMessages: DArray; Const aReset: Boolean = False): Boolean; Reintroduce;

         Function CompareByVarState(Const aObj1, aObj2: DObject): Integer; Inline;

         Function MapVariable(Const aComponent, aVariable, aOrigComponent, aOrigVariable: String; Const aPrevComponent: String = ''; Const aPrevVariable: String = ''): TCellMLModelVariable;

         Function ReservedUnit(Const aUnit: String): Boolean;

         Function NbOfUnits(Const aUnit: String): Integer;
         Function FindUnit(Const aUnit: String): TCellMLUnits;

         Function NbOfComponents(Const aComponent: String): Integer;
         Function FindComponent(Const aComponent: String): TCellMLComponent;

         Function NbOfEncapsulations: Integer;
         Function NbOfContainments(Const aContainment: String): Integer;

         Function FindComponentRef(Const aComponent: String; aComponentRef: TCellMLComponentRef): TCellMLComponentRef;
         Procedure EncapsulationSets(Const aComponent: String; Var aParent: TCellMLComponent; Var aEncapsulatedSet, aSiblingSet: TCellMLSortedList);

         Function HasAtLeastOneComputeOnceEquation: Boolean;

      Published
         // Published properties

         Property Extra: TCellMLModelExtra Read FExtra;
   End;
   TCellMLComponent = Class(TCellMLNamedObject)
      Private
         // Properties used for internal purposes

         UnitsAreValid: Boolean;

      Protected
         // Private representation of published properties

         FOwner: TCellMLModel;

      Public
         // Published properties

         UnitsList: TCellMLSortedList;
         VariableList: TCellMLSortedList;
         EquationList: TCellMLList;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLModel);
         Destructor Destroy; Override;

         // User's methods

         Function IsValid: Boolean; Override;

         Function NbOfUnits(Const aUnit: String): Integer;
         Function FindUnit(Const aUnit: String): TCellMLUnits;

         Function NbOfVariables(Const aVariable: String): Integer;
         Function FindVariable(Const aVariable: String): TCellMLVariable;

      Published
         // Published properties

         Property Owner: TCellMLModel Read FOwner;
   End;
   TCellMLUnit = Class;
   TCellMLUnitsType = (utBase, utSimple, utComplex);
   TCellMLUnitsEquivalenceType = (etNone, etDimensional, etExact);
   TCellMLUnits = Class(TCellMLNamedObject)
      Private
         // Properties used for internal purposes

         HasCanonicalForm: Boolean;

         OrigUnitList: TCellMLList;
         UnitList: TCellMLList;

         // Methods to modify the different published properties

         Function GetUnitsType: TCellMLUnitsType;

         // Methods used for internal purposes

         Procedure SortUnitElements;

         Class Function EmptyUnits(Const aCellMLUnits: TCellMLUnits): TCellMLUnits;
         Class Function DuplicateUnits(Const aCellMLUnits: TCellMLUnits; Const aResetBaseUnits: Boolean = False): TCellMLUnits;

         Procedure EmptyUnitList(Const aUnitList: TCellMLList; Const aFree: Boolean = True);

      Protected
         // Private representation of published properties

         FOwnerModel: TCellMLModel;
         FOwnerComponent: TCellMLComponent;

      Public
         // Published properties

         BaseUnits: Boolean;

         // Constructor & Destructor

         Constructor Create(Const aOwnerModel: TCellMLModel; Const aOwnerComponent: TCellMLComponent; Const aName: String; Const aBaseUnits: Boolean = False); Overload;
         Constructor Create(Const aCellMLUnits: TCellMLUnits); Overload;
         Destructor Destroy; Override;

         // User's methods

         Function IsValid: Boolean; Override;

         Function NbOfUnitElements: Integer; Inline;

         Procedure AddUnitElement(Const aUnitElements: Array Of Const);
         Procedure RemoveUnitElement(Const aIter: Integer); Inline;

         Function GetUnitElement(Const aIter: Integer): TCellMLUnit; Inline;

         Procedure CanonicalForm;

         Function EquivalenceType(Const aCellMLUnits: TCellMLUnits): TCellMLUnitsEquivalenceType;

         Function Times(Const aCellMLUnits: TCellMLUnits): TCellMLUnits;
         Function Exponentiates(Const aExponent: Double): TCellMLUnits;

      Published
         // Published properties

         Property OwnerModel: TCellMLModel Read FOwnerModel;
         Property OwnerComponent: TCellMLComponent Read FOwnerComponent;

         Property UnitsType: TCellMLUnitsType Read GetUnitsType;
   End;
   TCellMLUnitPrefix = Class
      Public
         // Published properties

         Name: String;
         Factor: Integer;

         // Constructor & Destructor

         Constructor Create(Const aName: String; Const aFactor: Integer = 0);
   End;
   TCellMLUnit = Class(TCellMLNamedObject)
      Private
         // Private representation of published properties

         FOwner: TCellMLUnits;

         // Methods used for internal purposes

         Function CircularUnits(Const aCellMLModel: TCellMLModel; Const aCellMLComponent: TCellMLComponent; Const aCrtUnit: String; Const aUnit: String; Const aVisitedUnits: TCellMLList): Boolean; Overload;

      Public
         // Published properties

         Prefix: String;
         Exponent: String;
         Multiplier: String;
         Offset: String;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLUnits; Const aName: String; Const aPrefix: String = ''; Const aExponent: String = ''; Const aMultiplier: String = ''; Const aOffset: String = ''); Overload;
         Constructor Create(Const aCellMLUnit: TCellMLUnit); Overload;

         // User's methods

         Function IsValid: Boolean; Override;

         Procedure ConvUnitElemAttr(Var aPrefix, aExponent, aMultiplier, aOffset: Double);

      Published
         // Published properties

         Property Owner: TCellMLUnits Read FOwner;
   End;
   TCellMLVariable = Class(TCellMLNamedObject)
      Private
         // Private representation of published properties

         FOwner: TCellMLComponent;

      Public
         // Published properties

         Units: String;
         InitialValue: String;
         PublicInterface: String;
         PrivateInterface: String;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLComponent);

         // User's methods

         Function IsValid: Boolean; Override;

      Published
         // Published properties

         Property Owner: TCellMLComponent Read FOwner;
   End;
   TMathMLCommandBinTreeItemType = (mitNil,   // Not used, but necessary for
                                              // "TScannerToken.CellMLSym"
                                    mitConcatenate,
                                    mitUnit,
                                    mitProperties, mitProperty,
                                    // The above are necessary variable
                                    // declarations
                                    mitCI, mitCN,
                                    mitPiecewise, mitPiece, mitOtherwise,
                                    mitEq, mitEqEq, mitNEq, mitLT, mitGT, mitLEq, mitGEq,
                                    mitPlus, mitMinus, mitTimes, mitDivide, mitPow, mitRoot, mitAbs, mitExp, mitLN, mitLog, mitFloor, mitCeil, mitFact,
                                    mitAnd, mitOr, mitXOr, mitNot,
                                    mitDiff,
                                    mitDegree, mitLogBase,
                                    mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot, mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH, mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot, mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH,
                                    mitTrue, mitFalse, mitPI, mitExponentiale);
   TMathMLCommandBinTree = Class(TCellMLObject)
      Private
         // Properties used for internal purposes

         PossibleUnits: TCellMLSortedList;

         // Methods used for internal purposes

         Procedure CommonCreate;

         Procedure AddPossibleUnits(Const aCellMLUnits: TCellMLUnits); Overload;
         Procedure AddPossibleUnits(Const aSetOfUnits: TCellMLSortedList); Overload;
      Public
         // Public properties

         ItemType: TMathMLCommandBinTreeItemType;

         Str: String;
         Units: String;

         Left: TMathMLCommandBinTree;
         Right: TMathMLCommandBinTree;

         // Constructor & Destructor

         Constructor Create(Const aFileName: String; Const aItemType: TMathMLCommandBinTreeItemType; Const aStr: String = ''); Overload;
         Constructor Create(Const aFileName: String; Const aStr: String); Overload;
         Constructor Create(Const aFileName: String; Const aNb: String; Const aUnits: String); Overload;

         Destructor Destroy; Override;

         // User's methods

         Function IsValid: Boolean; Override;
   End;
   PMathMLCommandBinTree = ^TMathMLCommandBinTree;
   TMathMLEquation = Class(TCellMLObject)
      Private
         // Private representation of published properties

         FOwner: TCellMLComponent;

      Public
         // Published properties

         MathMLEquationBinTree: TMathMLCommandBinTree;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLComponent);
         Destructor Destroy; Override;

         // User's methods

         Function IsValid: Boolean; Override;

      Published
         // Published properties

         Property Owner: TCellMLComponent Read FOwner;
   End;
   TCellMLGroup = Class(TCellMLNamedObject)
      Private
         // Private representation of published properties

         FOwner: TCellMLModel;

      Public
         // Published properties

         Encapsulation: Boolean;
         Containment: Boolean;

         ComponentRefList: TCellMLList;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLModel);
         Destructor Destroy; Override;

         // User's methods

         Function IsValid: Boolean; Override;

         Function NbOfComponentRefs(Const aComponentRef: String): Integer;

      Published
         // Published properties

         Property Owner: TCellMLModel Read FOwner;
   End;
   TCellMLComponentRef = Class(TCellMLNamedObject)
      Private
         // Private representation of published properties

         FOwnerGroup: TCellMLGroup;
         FOwnerComponentRef: TCellMLComponentRef;

         // Methods to modify the different published properties

         Function GetTopOwnerGroup: TCellMLGroup; Inline;

      Public
         // Published properties

         ComponentRefList: TCellMLList;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLGroup); Overload;
         Constructor Create(Const aOwner: TCellMLComponentRef); Overload;
         Destructor Destroy; Override;

         // User's methods

         Function IsValid: Boolean; Override;

         Function NbOfComponents(Const aComponent: String): Integer;
         Function NbOfComponentRefs(Const aComponentRef: String): Integer;

      Published
         // Published properties

         Property TopOwnerGroup: TCellMLGroup Read GetTopOwnerGroup;

         Property OwnerGroupMin: TCellMLGroup Read FOwnerGroup;
         Property OwnerComponentRef: TCellMLComponentRef Read FOwnerComponentRef;
   End;
   TCellMLConnection = Class(TCellMLObject)
      Private
         // Properties used for internal purposes

         FOwner: TCellMLModel;

      Public
         // Published properties

         Component1: String;
         Component2: String;

         MapVariablesList: TCellMLList;

         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLModel);
         Destructor Destroy; Override;

         // User's methods

         Function IsValid: Boolean; Override;

      Published
         // Published properties

         Property Owner: TCellMLModel Read FOwner;
   End;
   TCellMLMapVariables = Class(TCellMLObject)
      Private
         // Private representation of published properties

         FOwner: TCellMLConnection;

         FVariable1: String;
         FVariable2: String;

      Public
         // Constructor & Destructor

         Constructor Create(Const aOwner: TCellMLConnection; Const aVariable1, aVariable2: String);

         // User's methods

         Function IsValid: Boolean; Override;

      Published
         // Published properties

         Property Owner: TCellMLConnection Read FOwner;

         Property Variable1: String Read FVariable1;
         Property Variable2: String Read FVariable2;
   End;
   TCellMLExportFormat = (efCDLL, efC, efCPP, efDelphiForWin32, efFortran77,
                          efJava, efMATLAB, efMSWord, efPascal, efTeX);

//==============================================================================

Const
   CELLML_VERSION = 'CellML 1.0';
   CELLML_URL     = 'http://www.cellml.org/';

   INDENT = 3;
   FORTRAN77_INDENT = 6;

//==============================================================================

Var
   CellMLIndent: Array Of String;

//==============================================================================

Procedure InitXMLDocument(Const aXMLDocument: IXMLDocument; Const aFullInit: Boolean = True);
Procedure CloneXMLDocument(Const aDestXMLDocument, aOrigXMLDocument: IXMLDocument);

Procedure IncIndent(Const aIndex: Integer; Const aSyntaxEditExportFormat: TCellMLExportFormat = efCPP);
Procedure DecIndent(Const aIndex: Integer; Const aSyntaxEditExportFormat: TCellMLExportFormat = efCPP);

Function PrefixVal(Const aPrefix: String): Integer;

Function FindStandardUnit(Const aUnit: String): TCellMLUnits;

Function LineCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ENDIF}
   Math, Forms, Windows, Common, CellMLScannerEngine;

//==============================================================================

Const
   // Units

   _DIMENSIONLESS_ = 'dimensionless';
   _BOOLEAN_       = 'boolean';

   AMPERE    = 'ampere';
   CANDELA   = 'candela';
   COULOMB   = 'coulomb';
   JOULE     = 'joule';
   KELVIN    = 'kelvin';
   KILOGRAM  = 'kilogram';
   LITER     = 'liter';
   LITRE     = 'litre';
   LUMEN     = 'lumen';
   METER     = 'meter';
   METRE     = 'metre';
   MOLE      = 'mole';
   NEWTON    = 'newton';
   SECOND    = 'second';
   STERADIAN = 'steradian';
   VOLT      = 'volt';
   WATT      = 'watt';
   WEBER     = 'weber';

   // Prefixes

   CENTI = 'centi';

//==============================================================================

Var
   Messages: DArray;                   // List of messages
   StandardUnits: TCellMLSortedList;   // List of standard units
   StandardUnitsUnits: TCellMLUnits;   // Used to create some standard units
   Prefixes: DArray;                   // List of prefixes
   DimensionlessUnit, BooleanUnit, SecondUnit: TCellMLUnits;

//==============================================================================

Function AddStandardUnit(Const aName: String; Const aBaseUnits: Boolean): TCellMLUnits;
Begin
   Result := TCellMLUnits.Create(Nil, Nil, aName, aBaseUnits);

   If (aBaseUnits) Then
      // In the case of a base units, we add one unit element, so that all the
      // units-based algorithms can work fine

      Result.AddUnitElement([TCellMLUnit.Create(Nil, aName, '0', '1', '1', '0')]);

   StandardUnits.Add([Result]);
End;

//==============================================================================

Function FindStandardUnit(Const aUnit: String): TCellMLUnits;
Var
   RealUnit: String;
   UnitIter: DIterator;
Begin
   // CellML allows for the reciprocal use of 'meter' and 'metre', and 'liter'
   // and 'litre', but that can cause problems when needing the canonical form
   // of a units definition, so we should, internally, only one or the other,
   // so... 

   If (CompareStr(aUnit, LITER) = 0) Then
      RealUnit := LITRE
   Else If (CompareStr(aUnit, METER) = 0) Then
      RealUnit := METRE
   Else
      RealUnit := aUnit;

   // Search for the unit in the list of standard units

   UnitIter := StandardUnits.Find(RealUnit);

   If (Not AtEnd(UnitIter)) Then
      Result := TCellMLUnits(DeCAL.GetObject(UnitIter))
   Else
      Result := Nil;
End;

//==============================================================================

Function UnitCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer;
Var
   UnitElement1, UnitElement2: TCellMLUnit;
Begin
   // Sort the unit elements in alphabetical order, though dimensionless
   // elements are always to be at the end

   UnitElement1 := TCellMLUnit(aObj1.VObject);
   UnitElement2 := TCellMLUnit(aObj2.VObject);

   If (CompareStr(UnitElement1.Name, _DIMENSIONLESS_) = 0) Then Begin
      If (CompareStr(UnitElement2.Name, _DIMENSIONLESS_) = 0) Then
         Result := 0
      Else
         Result := 1;
   End Else If (CompareStr(UnitElement2.Name, _DIMENSIONLESS_) = 0) Then
      Result := -1
   Else
      Result := CompareStr(UnitElement1.Name, UnitElement2.Name);
End;

//==============================================================================

Function StringCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer; //Inline;
Begin
   Result := CompareStr(String(aObj1.VObject), String(aObj2.VObject));
End;

//==============================================================================

Function PrefixCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer; Inline;
Begin
   Result := CompareStr(TCellMLUnitPrefix(aObj1.VObject).Name,
                        TCellMLUnitPrefix(aObj2.VObject).Name);
End;

//==============================================================================

Function LineCompare(aPtr: Pointer; Const aObj1, aObj2: DObject): Integer;
Var
   Obj1, Obj2: TCellMLNamedObject;
Begin
   Obj1 := TCellMLNamedObject(aObj1.VObject);
   Obj2 := TCellMLNamedObject(aObj2.VObject);

   If (Obj1.Line < Obj2.Line) Then
      Result := -1
   Else If (Obj1.Line > Obj2.Line) Then
      Result := 1
   Else
      Result := CompareStr(Obj1.Name, Obj2.Name);
End;

//==============================================================================

Procedure InitXMLDocument(Const aXMLDocument: IXMLDocument;
                          Const aFullInit: Boolean);
Begin
   // Some options for the CellML document

   aXMLDocument.NodeIndentStr := '   ';
   aXMLDocument.Options       := aXMLDocument.Options+[doNodeAutoIndent];

   If (aFullInit) Then Begin
      // The version

      aXMLDocument.Version := '1.0';

      // The encoding

      aXMLDocument.Encoding := 'utf-8';   // Corresponds to the 8 bit version of
                                          // Unicode
     // Note: this is fine even with, for example, a Japanese system, because
     //       the first 256 characters of UTF-16 are kind of mapped to a 8-bit
     //       encoding...
   End;
End;

//==============================================================================

Procedure CloneXMLDocument(Const aDestXMLDocument, aOrigXMLDocument: IXMLDocument);
Var
   MemoryStream: TMemoryStream;
Begin
   MemoryStream := TMemoryStream.Create;

   aOrigXMLDocument.SaveToStream(MemoryStream);
   aDestXMLDocument.LoadFromStream(MemoryStream);

   MemoryStream.Free;
End;

//==============================================================================

Procedure IncIndent(Const aIndex: Integer;
                    Const aSyntaxEditExportFormat: TCellMLExportFormat);
Begin
   Case aSyntaxEditExportFormat Of
      efFortran77:
         If (CellMLIndent[aIndex] = '') Then
            CellMLIndent[aIndex] := CellMLIndent[aIndex]+StringOfChar(' ', FORTRAN77_INDENT)
         Else
            CellMLIndent[aIndex] := CellMLIndent[aIndex]+StringOfChar(' ', INDENT);
   Else
      CellMLIndent[aIndex] := CellMLIndent[aIndex]+StringOfChar(' ', INDENT);
   End;
End;

//==============================================================================

Procedure DecIndent(Const aIndex: Integer;
                    Const aSyntaxEditExportFormat: TCellMLExportFormat);
Begin
   Case aSyntaxEditExportFormat Of
      efFortran77:
         If (Length(CellMLIndent[aIndex]) = 6) Then
            CellMLIndent[aIndex] := ''
         Else
            CellMLIndent[aIndex] := Copy(CellMLIndent[aIndex], 1, Length(CellMLIndent[aIndex])-INDENT);
   Else
      CellMLIndent[aIndex] := Copy(CellMLIndent[aIndex], 1, Length(CellMLIndent[aIndex])-INDENT);
   End;
End;

//==============================================================================

Function PrefixVal(Const aPrefix: String): Integer;
Var
   UnitPrefix: TCellMLUnitPrefix;
   UnitPrefixIter: DIterator;
Begin
   UnitPrefix := TCellMLUnitPrefix.Create(aPrefix);

   UnitPrefixIter := BinarySearch(Prefixes, [UnitPrefix]);

   If (Not AtEnd(UnitPrefixIter)) Then
      Result := TCellMLUnitPrefix(DeCAL.GetObject(UnitPrefixIter)).Factor
   Else
      Result := 0;

   UnitPrefix.Free;
End;

//==============================================================================

Function IsCellMLIdentifier(Const aCellMLIdentifier: String): Boolean;
   Function IsLetter(Const aLetter: Char): Boolean; Inline;
   Begin
      Result := ((Ord(aLetter) >= 65) And (Ord(aLetter) <= 90)) Or
                ((Ord(aLetter) >= 97) And (Ord(aLetter) <= 122));
   End;
   Function IsDigit(Const aDigit: Char): Boolean; Inline;
   Begin
      Result := (Ord(aDigit) >= 48) And (Ord(aDigit) <= 57);
   End;
Var
   Crt, I: Integer;
   Keyword: TCellMLScannerEngineKeyword;
   KeywordIter: DIterator;
Begin
   Result := False;

   Crt := 1;

   While ((Crt <= Length(aCellMLIdentifier)) And (aCellMLIdentifier[Crt] = '_')) Do
      Inc(Crt);

   If (Crt > Length(aCellMLIdentifier)) Then
      Exit;   // Cannot have only underscores, so...

   If (Not IsLetter(aCellMLIdentifier[Crt])) Then
      Exit;   // The next character following 0 or several underscores must be a
              // letter, so...

   Inc(Crt);

   For I := Crt To Length(aCellMLIdentifier) Do
      If (Not (IsLetter(aCellMLIdentifier[I]) Or IsDigit(aCellMLIdentifier[I]) Or (aCellMLIdentifier[I] = '_'))) Then
         Exit;   // The character must be either a letter, digit or underscore,
                 // so...

   Keyword := TCellMLScannerEngineKeyword.Create(aCellMLIdentifier);

   KeywordIter := BinarySearch(Keywords, [Keyword]);

   Keyword.Free;

   If (Not AtEnd(KeywordIter)) Then
      Exit;   // The identifier cannot be a keyword, so...

   Result := True;
End;

//==============================================================================

Constructor TCellMLList.Create(Const aManaged: Boolean);
Begin
   Inherited Create;

   Managed := aManaged;
End;

//==============================================================================

Destructor TCellMLList.Destroy;
Begin
   If (Managed) Then
      ObjFree(Self);

   Inherited;
End;

//==============================================================================

Constructor TCellMLSortedList.Create(Const aManaged: Boolean);
Begin
   Inherited;

   Comparator := Compare;
End;

//==============================================================================

Function TCellMLSortedList.Compare(Const aObj1, aObj2: DObject): Integer;
Begin
   Result := CompareStr(TCellMLNamedObject(aObj1.VObject).Name,
                        TCellMLNamedObject(aObj2.VObject).Name);
End;

//==============================================================================

Procedure TCellMLSortedList.Add(Const aObjs: Array Of Const);
Begin
   Inherited;

   NeedSorting := NeedSorting Or ((Size >= 2) And
                  (Compare(GetRef(AdvanceByF(Finish, -2))^, GetRef(AdvanceByF(Finish, -1))^) > 0));
End;

//==============================================================================

Function TCellMLSortedList.Find(Const aName: String): DIterator;
Var
   NamedObject: TCellMLNamedObject;
Begin
   If (NeedSorting) Then Begin
      Sort(Self);

      NeedSorting := False;
   End;

   NamedObject := TCellMLNamedObject.Create('', aName);

   Result := BinarySearch(Self, [NamedObject]);

   NamedObject.Free;
End;

//==============================================================================

Constructor TCellMLObject.Create(Const aFileName: String);
Begin
   FileName := aFileName;

   Line := UNDEFINED;
End;

//==============================================================================

Function TCellMLObject.IsValid: Boolean;
Begin
   // Note: the only reason that this function is defined (as opposed to being
   //       abstract) is to allow for searches (see "TCellMLSortedList.Find").
   //       In other words, any class that inherits from "TCellMLObject" should
   //       override this function!

   Result := True;
End;

//==============================================================================

Function TCellMLObject.AddMsg(Const aMsgType: TMsgDlgType;
                               Const aFileName: String; Const aLine: Integer;
                               Const aMsg: String): TEngineMsg;
Begin
   Result := TEngineMsg.Create(aMsgType, aFileName, aLine, UNDEFINED, aMsg);

   Messages.Add([Result]);
End;

//==============================================================================

Procedure TCellMLObject.RemoveMsg(Const aMsg: TEngineMsg);
Var
   Iter: Integer;
   EngineMsg: TEngineMsg;
Begin
   // Remove a given message
   // Note: it's far from being efficient, but it shouldn't matter as we
   //       shouldn't need to call that function too often... hopefully!

   If (aMsg = Nil) Then
      Exit;

   For Iter := 0 To Messages.Size-1 Do Begin
      EngineMsg := TEngineMsg(Messages.At(Iter).VObject);

      If (aMsg.Equals(EngineMsg)) Then Begin
         Messages.RemoveAt(Iter);

         EngineMsg.Free;

         Break;
      End;
   End;
End;

//==============================================================================

Constructor TCellMLNamedObject.Create(Const aFileName, aName: String);
Begin
   Inherited Create(aFileName);

   Name := aName;
End;

//==============================================================================

Function GlobalVariableName(Const aComponent, aVariable: String): String;
Begin
   Result := aComponent+'|'+aVariable;
End;

//==============================================================================

Constructor TCellMLModelVariable.Create(Const aFileName: String;
                                        Const aLine: Integer;
                                        Const aComponent, aVariable: String);
Begin
   Inherited Create(aFileName, GlobalVariableName(aComponent, aVariable));

   Component := aComponent;
   Variable  := aVariable;

   Line := aLine;

   ComputeOnce := True;
End;

//==============================================================================

Function TCellMLModelVariable.RealName: String;
Begin
   If (CompareStr(VarName, '') <> 0) Then
      Result := VarName
   Else
      Result := Name;
End;

//==============================================================================

Function TCellMLModelVariable.GenInfo: String;
Begin
   If (GenName) Then
      Result := ' ('+Variable+' in '+Component+')'
   Else
      Result := ' (in '+Component+')';
End;

//==============================================================================

Constructor TCellMLModelEquation.Create(Const aComponent: String;
                                        Const aEqnBinTree: TMathMLCommandBinTree);
Begin
   Component  := aComponent;

   EqnBinTree := aEqnBinTree;
End;

//==============================================================================

Constructor TCellMLModelExtra.Create(Const aOwner: TComponent);
Begin
   Inherited Create;

   Metadata := TXMLDocument.Create(aOwner);

   Metadata.Active := True;

   InitXMLDocument(Metadata);

   Metadata.DocumentElement := Metadata.CreateElement('model', '');
End;

//==============================================================================

Destructor TCellMLModelExtra.Destroy;
Begin
   With Metadata Do Begin
      Active := False;

      Free;
   End;

   Inherited;
End;

//==============================================================================

Procedure TCellMLModelExtra.Init(Const aExtra: TCellMLModelExtra);
Begin
   CloneXMLDocument(Metadata, aExtra.Metadata);
End;

//==============================================================================

Constructor TCellMLModel.Create(Const aFileName: String);
Begin
   Inherited Create(aFileName);

   FExtra := TCellMLModelExtra.Create(Application);

   UnitsList      := TCellMLSortedList.Create;
   ComponentList  := TCellMLSortedList.Create;
   GroupList      := TCellMLList.Create;
   ConnectionList := TCellMLList.Create;

   GlobalVariableList := TCellMLSortedList.Create;
   EquationList       := TCellMLList.Create;

   RequiredFunctions := DArray.CreateWith(MakeComparator(StringCompare));
End;

//==============================================================================

Destructor TCellMLModel.Destroy;
Begin
   FExtra.Free;

   UnitsList.Free;
   ComponentList.Free;
   GroupList.Free;
   ConnectionList.Free;

   GlobalVariableList.Free;
   EquationList.Free;

   RequiredFunctions.Free;
   // Note: we must NOT free the objects held by "RequiredFunctions", for they
   //       are strings!
End;

//==============================================================================

Function TCellMLModel.GlobalVariable(Const aComponent, aVariable: String): TCellMLModelVariable;
Var
   VariableIter: DIterator;
Begin
   VariableIter := GlobalVariableList.Find(GlobalVariableName(aComponent, aVariable));

   If (Not AtEnd(VariableIter)) Then
      Result := TCellMLModelVariable(DeCAL.GetObject(VariableIter))
   Else
      Result := Nil;
End;

//==============================================================================

Function TCellMLModel.IsValid(Const aMessages: DArray;
                              Const aReset: Boolean): Boolean;
Const
   AL_EQN = 0;
   OD_EQN = 1;
   BOTH_SV_AND_CV = 'is used both as a state variable and a computed variable';
   Function CheckMapping(Const aComponent1, aVariable1: String;
                         Const aVar1: TCellMLVariable;
                         Const aComponent2, aVariable2: String;
                         Const aVar2: TCellMLVariable): Boolean;
      Function CheckEncapsulation(Const aParentComponent, aEncapsulatedComponent: String): Boolean;
      Var
         Parent: TCellMLComponent;
         EncapsulatedSet, SiblingSet: TCellMLSortedList;
      Begin
         Parent := Nil;

         EncapsulatedSet := TCellMLSortedList.Create(False);
         SiblingSet      := TCellMLSortedList.Create(False);

         EncapsulationSets(aEncapsulatedComponent, Parent, EncapsulatedSet, SiblingSet);

         // The encapsulation is only valid if the encapsulated component's
         // parent's name matches that of "aParentComponent"

         Result := (Parent <> Nil) And (CompareStr(Parent.Name, aParentComponent) = 0);

         EncapsulatedSet.Free;
         SiblingSet.Free;
      End;
   Begin
      // Mapping between two variables can occur as follows:
      //    pub: out ---> pub: in OR priv: in
      //    pub: out ---> priv: in (requires the left component to be
      //                            encapsulated by the right one)
      //    priv: out ---> pub: in (requires the right component to be
      //                            encapsulated by the left one)

      Result := (CompareStr(aVar1.PublicInterface, 'out') = 0) And (CompareStr(aVar2.PublicInterface, 'in') = 0);

      If (Not Result And
          ((CompareStr(aVar1.PublicInterface, 'out') = 0) And (CompareStr(aVar2.PrivateInterface, 'in') = 0))) Then
         // The only way for the mapping to be valid is if Component1 is
         // encapsulated by Component2

         Result := CheckEncapsulation(aComponent2, aComponent1);

      If (Not Result And
          ((CompareStr(aVar1.PrivateInterface, 'out') = 0) And (CompareStr(aVar2.PublicInterface, 'in') = 0))) Then
         // The only way for the mapping to be valid is if Component2 is
         // encapsulated by Component1

         Result := CheckEncapsulation(aComponent1, aComponent2);

      If (Result) Then
         With GlobalVariable(aComponent2, aVariable2) Do Begin
            State := vsMapped;

            MappedComponent := aComponent1;
            MappedVariable  := aVariable1;
         End;
   End;
   Function CheckEquationPhase1(aCellMLComponent: TCellMLComponent;
                                aMathMLEquationBinTree: TMathMLCommandBinTree;
                                Const aTypeOfEqn: Integer;
                                Var aVariable: TCellMLModelVariable;
                                Var aMsgType: TMsgDlgType;
                                Var aMsg: String): Boolean;
   Var
      LookedupVar, VarToLookup: TCellMLModelVariable;
   Begin
      Result := True;

      If (aMathMLEquationBinTree = Nil) Then
         Exit;

      Case aMathMLEquationBinTree.ItemType Of
         mitEq: Begin
            // Deal with the left hand side of the equation

            If (aTypeOfEqn = AL_EQN) Then
               // Computed variable

               LookedupVar := GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Left.Str)
            Else
               // State variable

               LookedupVar := GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Left.Right.Str);

            // Deal with the right hand side of the equation

            Case LookedupVar.EqnState[aTypeOfEqn] Of
               esUnvisited: Begin
                  // We have never seen that computed variable before

                  LookedupVar.EqnState[aTypeOfEqn] := esVisited;

                  Result := CheckEquationPhase1(aCellMLComponent, aMathMLEquationBinTree.Right, aTypeOfEqn, aVariable, aMsgType, aMsg);

                  If (Result) Then Begin
                     // Everything went fine

                     EquationList.Add([TCellMLModelEquation.Create(aCellMLComponent.Name, aMathMLEquationBinTree)]);

                     LookedupVar.EqnState[aTypeOfEqn] := esCompleted;

                     // Check that the variable is not used as both a state
                     // variable and a computed variable

                     If ((LookedupVar.EqnState[AL_EQN] = esCompleted) And
                         (LookedupVar.EqnState[OD_EQN] = esCompleted)) Then Begin
                        LookedupVar.Line := LookedupVar.Line;

                        aVariable := LookedupVar;

                        aMsgType := mtError;
                        aMsg     := BOTH_SV_AND_CV;

                        Result := False;
                     End;
                  End;
               End;
               esVisited: Begin
                  // We have already seen that variable before, which means that
                  // we are going in circles

                  LookedupVar.Line := aMathMLEquationBinTree.Line;

                  aVariable := LookedupVar;

                  aMsgType := mtError;
                  aMsg     := 'is involved in a circular equation';

                  Result := False;
               End;
               esCompleted: Begin
                  // Need to check that the equation is exactly the same as the
                  // one we know about

                  Result := aMathMLEquationBinTree = LookedupVar.EqnBinTree[aTypeOfEqn];

                  If (Not Result) Then Begin
                     LookedupVar.Line := aMathMLEquationBinTree.Line;

                     aVariable := LookedupVar;

                     aMsgType := mtError;
                     aMsg     := 'is computed more than once';
                  End;
               End;
            End;
         End;
         mitCI: Begin
            // Deal with a computed variable used on the right hand side of an
            // equation

            LookedupVar := GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Str);
            VarToLookup := LookedupVar;

            Result := True;

            Case LookedupVar.State Of
               vsMaybeConstant:
                  LookedupVar.State := vsConstant;
               vsMapped: Begin
                  VarToLookup := MapVariable(LookedupVar.Component, LookedupVar.Variable, LookedupVar.Component, LookedupVar.Variable);

                  If (VarToLookup <> Nil) Then Begin
                     aCellMLComponent := FindComponent(VarToLookup.Component);

                     If (VarToLookup.State = vsMaybeConstant) Then
                        VarToLookup.State := vsConstant;
                  End Else Begin
                     aVariable := LookedupVar;

                     aMsgType := mtError;
                     aMsg     := 'is involved in a circular mapping definition';

                     Result := False;
                  End;
               End;
            End;

            If (Result) Then
               Result := CheckEquationPhase1(aCellMLComponent, VarToLookup.EqnBinTree[AL_EQN], AL_EQN, aVariable, aMsgType, aMsg);
         End;
         mitCN:
            // It's a constant number, so...

            Result := True;
         mitDiff: Begin
            LookedupVar := GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Right.Str);

            If (LookedupVar.State = vsState) Then 
               // We are dealing with a state variable used on the right hand
               // side of an equation

               Result := CheckEquationPhase1(aCellMLComponent, GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Right.Str).EqnBinTree[OD_EQN], OD_EQN, aVariable, aMsgType, aMsg)
            Else Begin
               LookedupVar.Line := aMathMLEquationBinTree.Line;

               aVariable := LookedupVar;

               aMsgType := mtError;
               aMsg     := 'is not a state variable and therefore cannot be used as one';

               Result := False;
            End;
         End;
      Else
         // Neither the root of an equation or a variable, so check the left and
         // right sub-nodes

         If (aMathMLEquationBinTree.ItemType In [mitLog, mitFloor, mitCeil, mitFact,
                                                 mitSec, mitCsc, mitCot,
                                                 mitSinH, mitCosH, mitTanH,
                                                 mitSecH, mitCscH, mitCotH,
                                                 mitASec, mitACsc, mitACot,
                                                 mitASinH, mitACosH, mitATanH,
                                                 mitASecH, mitACscH, mitACotH]) Then
            RequiredFunctions.Add([aMathMLEquationBinTree.Str]);

         Result := CheckEquationPhase1(aCellMLComponent, aMathMLEquationBinTree.Left, aTypeOfEqn, aVariable, aMsgType, aMsg) And
                   CheckEquationPhase1(aCellMLComponent, aMathMLEquationBinTree.Right, aTypeOfEqn, aVariable, aMsgType, aMsg);
      End;
   End;
   Function CheckEquationPhase2(aCellMLComponent: TCellMLComponent;
                                aMathMLEquationBinTree: TMathMLCommandBinTree;
                                Const aTypeOfEqn: Integer): Boolean;
   Var
      LookedupVar, VarToLookup: TCellMLModelVariable;
   Begin
      // Note: the idea behind this algorithm is that all the equations can, by
      //       default, be computed once (hence the "ComputeOnce" property of
      //       the model's parameters, variables, etc. is set to "True" in the
      //       first place). Now, because all of the equations are sorted by
      //       order of execution, we can easily determine which equation can be
      //       computed once based on whether the equation relies on something
      //       that is directly/indirectly based on the free variable or a state
      //       variable

      Result := True;   // By default, the equation can be computed once

      If (aMathMLEquationBinTree = Nil) Then
         Exit;

      Case aMathMLEquationBinTree.ItemType Of
         mitEq: Begin
            If (aTypeOfEqn = AL_EQN) Then Begin
               // Computed variable

               Result := CheckEquationPhase2(aCellMLComponent, aMathMLEquationBinTree.Right, aTypeOfEqn);

               GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Left.Str).ComputeOnce := Result;
            End Else Begin
               // State variable

               Result := False;
               // Note: even if some ODEs could be computed once, we want all
               //       of them to be computed every time, as this will
               //       otherwise require "CVODECompute" in the
               //       "ODECVODEIntegrator" unit to copy the contents of the
               //       cell's "dY" vector to that of the CVODE integrator and
               //       this will more often than not be more time consuming
               //       than computing all the state variables, so...

               GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Left.Right.Str).ComputeOnce := Result;
            End;
         End;
         mitCI: Begin
            // Deal with a computed variable used on the right hand side of an
            // equation

            LookedupVar := GlobalVariable(aCellMLComponent.Name, aMathMLEquationBinTree.Str);

            Case LookedupVar.State Of
               vsMapped: Begin
                  VarToLookup := MapVariable(LookedupVar.Component, LookedupVar.Variable, LookedupVar.Component, LookedupVar.Variable);

                  Case VarToLookup.State of
                     vsFree, vsState:
                        // We are dealing with either the free variable or a
                        // state variable, so...

                        Result := False;
                     vsComputed:
                        // We are dealing with a computed variable, so check
                        // whether it needs to be computed once or not

                        Result := Result And VarToLookup.ComputeOnce;
                  End;
               End;
               vsFree, vsState:
                  // We are dealing with either the free variable or a state
                  // variable, so...

                  Result := False;
               vsComputed:
                  // We are dealing with a computed variable, so check whether
                  // it needs to be computed once or not

                  Result := Result And LookedupVar.ComputeOnce;
            End;
         End;
         mitDiff:
            // We are dealing with the result of an ODE, so...

            Result := False;
      Else
         // Neither the root of an equation or a variable, so check the left and
         // right sub-nodes

         Result := Result And
                   CheckEquationPhase2(aCellMLComponent, aMathMLEquationBinTree.Left, aTypeOfEqn) And
                   CheckEquationPhase2(aCellMLComponent, aMathMLEquationBinTree.Right, aTypeOfEqn);
      End;
   End;
   Procedure RevertUnitList(Const aCellMLUnits: TCellMLUnits);
   Begin
      If (aCellMLUnits.OrigUnitList.Size <> 0) Then Begin
         aCellMLUnits.EmptyUnitList(aCellMLUnits.UnitList);

         aCellMLUnits.OrigUnitList.CloneTo(aCellMLUnits.UnitList);

         aCellMLUnits.EmptyUnitList(aCellMLUnits.OrigUnitList, False);
         // Note: we don't free the unit elements, since they are now owned by
         //       "aCellMLUnits.UnitList"
      End;
   End;
Var
   Iter, Iter2: Integer;
   SubIter, SubIter2: Integer;
   CrtComponent, Component: TCellMLComponent;
   CrtVariable, Variable: TCellMLVariable;
   EquationBinTree: TMathMLCommandBinTree;
   Units: TCellMLUnits;
   UnitElement: TCellMLUnit;
   Group: TCellMLGroup;
   ComponentRef: TCellMLComponentRef;
   Var1, Var2: TCellMLVariable;
   LookedupVar: TCellMLModelVariable;
   MsgType: TMsgDlgType;
   Msg: String;
   TypeOfEqn: Integer;
   Eqn: TCellMLModelEquation;
Begin
   Result := True;

   Messages := aMessages;

   // Empty the variable and equation lists, just in case...

   ObjFree(GlobalVariableList);

   GlobalVariableList.Clear;

   ObjFree(EquationList);

   EquationList.Clear;

   ObjFree(RequiredFunctions);

   RequiredFunctions.Clear;

   // The model must have a valid name

   If (Not IsCellMLIdentifier(Name)) Then Begin
      If (CompareStr(Name, '') = 0) Then
         AddMsg(mtError, FileName, Line, 'a name is required for the model')
      Else
         AddMsg(mtError, FileName, Line, ''''+Name+''' is not a valid name for a model');

      Result := False;
   End;

   // Sort a few lists that may not be properly sorted at this stage

   ComponentList.Comparator := ComponentList.Compare;

   Sort(UnitsList);
   Sort(ComponentList);

   For Iter := 0 To ComponentList.Size-1 Do
      With TCellMLComponent(ComponentList.At(Iter).VObject) Do Begin
         VariableList.Comparator := VariableList.Compare;

         Sort(UnitsList);
         Sort(VariableList);
      End;

   // A name for the model exists and is valid, so check the units, components,
   // groups and connections

   // Check the units

   UnitsAreValid := True;

   For Iter := 0 To UnitsList.Size-1 Do
      If (Not TCellMLUnits(UnitsList.At(Iter).VObject).IsValid) Then Begin
         Result := False;

         UnitsAreValid := False;
      End;

   // Check the components

   DoUnitsChecking := False;

   For Iter := 0 To ComponentList.Size-1 Do
      If (Not TCellMLComponent(ComponentList.At(Iter).VObject).IsValid) Then
         Result := False;

   // Check the groups

   For Iter := 0 To GroupList.Size-1 Do
      If (Not TCellMLGroup(GroupList.At(Iter).VObject).IsValid) Then
         Result := False;

   // Check the connections

   For Iter := 0 To ConnectionList.Size-1 Do
      If (Not TCellMLConnection(ConnectionList.At(Iter).VObject).IsValid) Then
         Result := False;

   // Check that all the variables have a proper function, that is are a free
   // variable, a state variable, a constant, a computed variable or a mapped
   // variable
   // Note: this is NOT an official rule, but it is important for COR, so...

   If (Result) Then Begin
      // First: include all the variables that are defined in the various
      //        components

      For Iter := 0 To ComponentList.Size-1 Do
         With TCellMLComponent(ComponentList.At(Iter).VObject) Do 
            For Iter2 := 0 To VariableList.Size-1 Do
               With TCellMLVariable(VariableList.At(Iter2).VObject) Do Begin
                  LookedupVar := TCellMLModelVariable.Create(FileName, Line, Owner.Name, Name);

                  Self.GlobalVariableList.Add([LookedupVar]);

                  If (CompareStr(Name, FreeVariable) = 0) Then
                     LookedupVar.State := vsMaybeFree;

                  LookedupVar.Units := Units;

                  If (CompareStr(InitialValue, '') <> 0) Then Begin
                     LookedupVar.State := vsMaybeConstant;

                     LookedupVar.InitialValue := InitialValue;
                  End;
               End;

      // Second: update the variables with mapping information, if necessary

      For Iter := 0 To ConnectionList.Size-1 Do
         With TCellMLConnection(ConnectionList.At(Iter).VObject) Do
            For Iter2 := 0 To MapVariablesList.Size-1 Do
               With TCellMLMapVariables(MapVariablesList.At(Iter2).VObject) Do Begin
                  Var1 := FindComponent(Component1).FindVariable(Variable1);
                  Var2 := FindComponent(Component2).FindVariable(Variable2);

                  // Component 1 -> Component 2?

                  If (Not CheckMapping(Component1, Variable1, Var1, Component2, Variable2, Var2)) Then
                     // Component 2 -> Component 1?

                     CheckMapping(Component2, Variable2, Var2, Component1, Variable1, Var1);
               End;

      // Third: find out whether the remaining undefined variables are actually
      //        state variables or whether they are computed ones

      NbOfStateVariables := 0;

      For Iter := 0 To ComponentList.Size-1 Do
         With TCellMLComponent(ComponentList.At(Iter).VObject) Do
            For Iter2 := 0 To EquationList.Size-1 Do Begin
               EquationBinTree := TMathMLEquation(EquationList.At(Iter2).VObject).MathMLEquationBinTree;

               With EquationBinTree.Left Do
                  If (ItemType = mitCI) Then Begin
                     // Computed variable

                     With Owner.GlobalVariable(Name, Str) Do Begin
                        If (State = vsMaybeConstant) Then Begin
                           PrevMsg := AddMsg(mtError, FileName, Line, 'the variable '''+Variable+''' is both initialised and computed');
                           // Note: check the case where State is equal to
                           //       vsComputed below...

                           Result := False;
                        End;

                        State := vsComputed;

                        If (EqnBinTree[AL_EQN] = Nil) Then
                           EqnBinTree[AL_EQN] := EquationBinTree;
                           // Note: this is not useful for the present check,
                           //       but will be for the next one...
                     End
                  End Else
                     // State variable

                     With Owner.GlobalVariable(Name, Right.Str) Do Begin
                        If (State = vsComputed) Then Begin
                           // We need to let the user know that the variable
                           // cannot be used both as a state variable and a
                           // computed variable, but first we must remove the
                           // message about the message about the variable being
                           // both initialised and computed (in case it was
                           // generated, see the case where State is equal to
                           // vsMaybeConstant above)

                           RemoveMsg(PrevMsg);

                           AddMsg(mtError, FileName, Line, 'the variable '''+Variable+''' '+BOTH_SV_AND_CV);

                           Result := False;
                        End Else Begin
                           State := vsState;

                           If (EqnBinTree[OD_EQN] = Nil) Then Begin
                              Inc(NbOfStateVariables);

                              EqnBinTree[OD_EQN] := EquationBinTree;
                              // Note: this is not useful for the present check,
                              //       but will be for the next one...
                           End;
                        End;
                     End;
            End;

      // Fourth: determine whether there is any undefined variable left
      // Note: the variable list is to be resorted by line number, since it is
      //       currently sorted by "<Component>_<Variable>", which is convenient
      //       most of the time, except when we want to report errors to the
      //       user...

      GlobalVariableList.Comparator := MakeComparator(LineCompare);
                        
      Sort(GlobalVariableList);

      For Iter := 0 To GlobalVariableList.Size-1 Do
         With TCellMLModelVariable(GlobalVariableList.At(Iter).VObject) Do
            Case State Of
               vsUnknown: Begin
                  AddMsg(mtError, FileName, Line, 'the variable '''+Variable+''' is either not initialised, not computed, not used or not properly mapped');

                  Result := False;
               End;
               vsState:
                  If (CompareStr(InitialValue, '') = 0) Then Begin
                     AddMsg(mtError, FileName, Line, 'the state variable '''+Variable+''' has not been initialised');

                     Result := False;
                  End;
            End;
   End;

   // We should, at this stage, have a free variable, but we never know, so...

   If (Result And (CompareStr(FreeVariable, '') = 0)) Then Begin
      AddMsg(mtError, FileName, Line, 'no free variable has been defined');

      Result := False;
   End;

   // Check that the free variable is of temporal type and that it is declared
   // in a top component
   // Note: this is NOT an official rule, but it is important for COR, since we
   //       integrate against time...

   If (Result) Then Begin
      // Look up the free variable's structure

      GlobalVariableList.Comparator := GlobalVariableList.Compare;

      Sort(GlobalVariableList);

      Component := Nil;
      Variable  := Nil;

      For Iter := 0 To ComponentList.Size-1 Do Begin
         CrtComponent := TCellMLComponent(ComponentList.At(Iter).VObject);

         For Iter2 := 0 To CrtComponent.VariableList.Size-1 Do Begin
            CrtVariable := TCellMLVariable(CrtComponent.VariableList.At(Iter2).VObject);
            LookedupVar := GlobalVariable(CrtComponent.Name, CrtVariable.Name);

            If (LookedupVar.State = vsMaybeFree) Then Begin
               If ((CompareStr(CrtVariable.Name, FreeVariable) = 0)     And
                   (CompareStr(CrtVariable.PublicInterface, 'in') <> 0) And
                   (CompareStr(CrtVariable.PrivateInterface, 'in') <> 0)) Then Begin
                  // The current definition might be that of the free variable,
                  // but for that the variable's component has to be one of the
                  // top components

                  ComponentRef := Nil;

                  For SubIter := 0 To GroupList.Size-1 Do Begin
                     Group := TCellMLGroup(GroupList.At(SubIter).VObject);

                     If (Group.Encapsulation) Then
                        For SubIter2 := 0 To Group.ComponentRefList.Size-1 Do Begin
                           ComponentRef := FindComponentRef(CrtComponent.Name, TCellMLComponentRef(Group.ComponentRefList.At(SubIter2).VObject));

                           If (ComponentRef <> Nil) Then
                              // The scanning is successful, so get out of
                              // here...

                              Break;
                        End;

                     If (ComponentRef <> Nil) Then
                        Break;
                  End;

                  // If we haven't found a reference to the component, then it
                  // means that it is a top component, so it's fine. However, if
                  // it has been found, then it must not have any parent

                  If ((Variable = Nil) And
                      ((ComponentRef = Nil) Or
                       ((ComponentRef <> Nil) And (ComponentRef.OwnerComponentRef = Nil)))) Then Begin
                     // The variable's component is a top component, so it has
                     // to be the definition we are looking for

                     LookedupVar.State := vsFree;

                     Component := CrtComponent;
                     Variable  := CrtVariable;
                  End;
               End;

               // If the current variable is the free variable, then its state
               // shouldn't be "vsMaybeFree" anymore. If it is, then the current
               // variable isn't the free variable and there is something wrong
               // with it

               If (LookedupVar.State = vsMaybeFree) Then Begin
                  With LookedupVar Do
                     AddMsg(mtError, FileName, Line, 'the variable '''+Variable+''' has been mistaken for the free variable, but it is not the free variable');

                  Result := False;
               End;
            End;
         End;
      End;

      If (Result) Then Begin
         If (Variable = Nil) Then Begin
            // Couldn't find the declaration to the free variable, so...

            AddMsg(mtError, FileName, UNDEFINED, 'the free variable '''+FreeVariable+''' is not defined anywhere');

            Result := False;
         End Else Begin
            // Check that the free variable's unit is dimensionally equivalent
            // to the base unit second

            Units := Component.FindUnit(Variable.Units);

            Assert(Units <> Nil);
            // Note: we should always have a units, so if it is not the case it
            //       means that there is something fundamentally wrong with our
            //       code...

            If (Units.EquivalenceType(SecondUnit) = etNone) Then Begin
               AddMsg(mtError, FileName, Variable.Line, 'the unit of the free variable '''+FreeVariable+''' must be dimensionally equivalent to the base unit '''+SECOND+'''');

               Result := False;
            End;

            If (Result) Then
               // Keep track of the units definition

               FreeVariableUnits := Units;
         End;
      End;
   End;

   // Check that there are no circular equations, as well as determine which
   // equations can be computed once
   // Note: this has to be done AFTER we have checked for the free variable!

   If (Result) Then Begin
      GlobalVariableList.Comparator := GlobalVariableList.Compare;

      Sort(GlobalVariableList);

      // Phase 1

      For Iter := 0 To ComponentList.Size-1 Do Begin
         Component := TCellMLComponent(ComponentList.At(Iter).VObject);

         For Iter2 := 0 To Component.EquationList.Size-1 Do Begin
            With TMathMLEquation(Component.EquationList.At(Iter2).VObject) Do Begin
               If (MathMLEquationBinTree.Left.ItemType = mitCI) Then
                  TypeOfEqn := AL_EQN
               Else
                  TypeOfEqn := OD_EQN;

               If (Not CheckEquationPhase1(Component, MathMLEquationBinTree, TypeOfEqn, LookedupVar, MsgType, Msg)) Then Begin
                  With LookedupVar Do
                     AddMsg(MsgType, FileName, Line, 'the variable '''+Variable+''' '+Msg);

                  If (MsgType = mtError) Then
                     Result := False;
               End;
            End;
         End;
      End;

      // Phase 2
      // Note: this has to be done separately from phase 1, since we need to
      //       have gone through all the equations first and have them in the
      //       order in which they should be computed before going ahead with
      //       phase 2, so...

      If (Result) Then
         For Iter := 0 To EquationList.Size-1 Do Begin
            Eqn := TCellMLModelEquation(EquationList.At(Iter).VObject);

            If (Eqn.EqnBinTree.Left.ItemType = mitCI) Then
               TypeOfEqn := AL_EQN
            Else
               TypeOfEqn := OD_EQN;

            Eqn.ComputeOnce := CheckEquationPhase2(FindComponent(Eqn.Component), Eqn.EqnBinTree, TypeOfEqn);
         End;
   End;

   // Check that there are no unused constants

   If (Result) Then Begin
      GlobalVariableList.Comparator := MakeComparator(LineCompare);

      Sort(GlobalVariableList);

      For Iter := 0 To GlobalVariableList.Size-1 Do
         With TCellMLModelVariable(GlobalVariableList.At(Iter).VObject) Do
            If (State = vsMaybeConstant) Then
               AddMsg(mtWarning, FileName, Line, 'the variable '''+Variable+''' is defined as a constant, but is never used');
   End;

   // Check that there is at least one state variable, as we otherwise cannot
   // run the model

   If (Result And (NbOfStateVariables = 0)) Then Begin
      AddMsg(mtError, FileName, UNDEFINED, 'the model must contain at least one state variable');

      Result := False;
   End;

   // Do units checking
   // Note: the only reason we are doing the units checking at this stage is
   //       that we may need access to the state of the different variables,
   //       something that we don't know when checking the components, so...

   If (Result And UnitsAreValid) Then Begin
      // Resort the list of variables, so that we can look things up during
      // units checking

      GlobalVariableList.Comparator := GlobalVariableList.Compare;

      Sort(GlobalVariableList);

      // Do units checking itself

      DoUnitsChecking := True;

      For Iter := 0 To ComponentList.Size-1 Do Begin
         Component := TCellMLComponent(ComponentList.At(Iter).VObject);

         For Iter2 := 0 To Component.EquationList.Size-1 Do
            TMathMLEquation(Component.EquationList.At(Iter2).VObject).IsValid;
      End;

      // Revert the units definitions to their original definitions (since they
      // have been converted into their canonical form for the purpose of
      // checking), if requested

      If (aReset) Then Begin
         For Iter := 0 To UnitsList.Size-1 Do
            RevertUnitList(TCellMLUnits(UnitsList.At(Iter).VObject));

         For Iter := 0 To ComponentList.Size-1 Do Begin
            Component := TCellMLComponent(ComponentList.At(Iter).VObject);

            For Iter2 := 0 To Component.UnitsList.Size-1 Do
               RevertUnitList(TCellMLUnits(Component.UnitsList.At(Iter2).VObject));
         End;
      End;
   End;

   // Resort a few lists, if required

   If (aReset) Then Begin
      UnitsList.Comparator     := MakeComparator(LineCompare);
      ComponentList.Comparator := MakeComparator(LineCompare);

      Sort(UnitsList);
      Sort(ComponentList);

      For Iter := 0 To ComponentList.Size-1 Do
         With TCellMLComponent(ComponentList.At(Iter).VObject) Do Begin
            UnitsList.Comparator    := MakeComparator(LineCompare);
            VariableList.Comparator := MakeComparator(LineCompare);

            Sort(UnitsList);
            Sort(VariableList);
         End;
   End;

   // Miscellaneous

   RequiredFunctions.TrimToSize;

   Sort(RequiredFunctions);
   // Necessary, since we may have added functions in a non-alphabetical order
End;

//==============================================================================

Function TCellMLModel.CompareByVarState(Const aObj1, aObj2: DObject): Integer;
Var
   Obj1, Obj2: TCellMLModelVariable;
Begin
   Obj1 := TCellMLModelVariable(aObj1.VObject);
   Obj2 := TCellMLModelVariable(aObj2.VObject);

   If (Obj1.State < Obj2.State) Then
      Result := -1
   Else If (Obj1.State > Obj2.State) Then
      Result := 1
   Else
      Result := CompareStr(Obj1.Name, Obj2.Name);
End;

//==============================================================================

Function TCellMLModel.MapVariable(Const aComponent, aVariable,
                                        aOrigComponent, aOrigVariable,
                                        aPrevComponent, aPrevVariable: String): TCellMLModelVariable;
Begin
   Result := GlobalVariable(aComponent, aVariable);

   If ((Result <> Nil) And (Result.State = vsMapped)) Then Begin
      If (((CompareStr(Result.MappedComponent, aOrigComponent) <> 0) Or
           (CompareStr(Result.MappedVariable, aOrigVariable) <> 0)) And
          ((CompareStr(Result.MappedComponent, aPrevComponent) <> 0) Or
           (CompareStr(Result.MappedVariable, aPrevVariable) <> 0))) Then
         Result := MapVariable(Result.MappedComponent, Result.MappedVariable, aOrigComponent, aOrigVariable, aComponent, aVariable)
      Else
         Result := Nil;
   End;
End;

//==============================================================================

Function TCellMLModel.ReservedUnit(Const aUnit: String): Boolean;
Begin
   // There is (currently, at least) only one reserved unit, which cannot be
   // explicitly used: boolean

   Result := aUnit = 'boolean';
End;

//==============================================================================

Function TCellMLModel.NbOfUnits(Const aUnit: String): Integer;
Var
   Iter: Integer;
Begin
   If (FindStandardUnit(aUnit) <> Nil) Then
      Result := 1
   Else
      Result := 0;

   // Is the unit defined in the model?

   For Iter := 0 To UnitsList.Size-1 Do
      If (CompareStr(TCellMLUnits(UnitsList.At(Iter).VObject).Name, aUnit) = 0) Then
         Inc(Result);
End;

//==============================================================================

Function TCellMLModel.FindUnit(Const aUnit: String): TCellMLUnits;
Var
   UnitIter: DIterator;
Begin
   UnitIter := UnitsList.Find(aUnit);

   If (Not AtEnd(UnitIter)) Then
      Result := TCellMLUnits(DeCAL.GetObject(UnitIter))
   Else
      // Not a unit defined in the model, so maybe a standard unit?

      Result := FindStandardUnit(aUnit);
End;

//==============================================================================

Function TCellMLModel.NbOfComponents(Const aComponent: String): Integer;
Var
   Iter: Integer;
Begin
   Result := 0;

   // Is the component defined in the model?

   For Iter := 0 To ComponentList.Size-1 Do
      If (CompareStr(TCellMLComponent(ComponentList.At(Iter).VObject).Name, aComponent) = 0) Then
         Inc(Result);
End;

//==============================================================================

Function TCellMLModel.FindComponent(Const aComponent: String): TCellMLComponent;
Var
   ComponentIter: DIterator;
Begin
   ComponentIter := ComponentList.Find(aComponent);

   If (Not AtEnd(ComponentIter)) Then
      Result := TCellMLComponent(DeCAL.GetObject(ComponentIter))
   Else
      Result := Nil;
End;

//==============================================================================

Function TCellMLModel.NbOfEncapsulations: Integer;
Var
   Iter: Integer;
Begin
   Result := 0;

   // Is the encapsulation defined in the model?

   For Iter := 0 To GroupList.Size-1 Do
      If (TCellMLGroup(GroupList.At(Iter).VObject).Encapsulation) Then
         Inc(Result);
End;

//==============================================================================

Function TCellMLModel.NbOfContainments(Const aContainment: String): Integer;
Var
   Iter: Integer;
Begin
   Result := 0;

   // Is the component defined in the model?

   For Iter := 0 To GroupList.Size-1 Do
      With TCellMLGroup(GroupList.At(Iter).VObject) Do
         If (Containment And (CompareStr(Name, aContainment) = 0)) Then
            Inc(Result);
End;

//==============================================================================

Function TCellMLModel.FindComponentRef(Const aComponent: String;
                                       aComponentRef: TCellMLComponentRef): TCellMLComponentRef;
Var
   Iter: Integer;
Begin
   Result := Nil;

   If (CompareStr(aComponentRef.Name, aComponent) = 0) Then
      // Found it!!

      Result := aComponentRef
   Else
      // Not found, is it present in its children?

      For Iter := 0 To aComponentRef.ComponentRefList.Size-1 Do Begin
         Result := FindComponentRef(aComponent, TCellMLComponentRef(aComponentRef.ComponentRefList.At(Iter).VObject));

         If (Result <> Nil) Then
            Break;
      End;
End;

//==============================================================================

Procedure TCellMLModel.EncapsulationSets(Const aComponent: String;
                                         Var aParent: TCellMLComponent;
                                         Var aEncapsulatedSet, aSiblingSet: TCellMLSortedList);
Var
   Iter, Iter2: Integer;
   Group: TCellMLGroup;
   Component: TCellMLComponent;
   ComponentRef, Parent: TCellMLComponentRef;
   NeedSiblingSetForTopComponent: Boolean;
   SiblingComponent: String;
Begin
   //---GRY--- KEEP IN MIND THAT AT THIS STAGE, AN ENCAPSULATION HIERARCHY CAN
   //          ONLY BE DEFINED IN ONE AND ONLY ONE GROUP!!

   Group := Nil;
   ComponentRef := Nil;

   For Iter := 0 To GroupList.Size-1 Do Begin
      Group := TCellMLGroup(GroupList.At(Iter).VObject);

      If (Group.Encapsulation) Then Begin
         For Iter2 := 0 To Group.ComponentRefList.Size-1 Do Begin
            ComponentRef := FindComponentRef(aComponent, TCellMLComponentRef(Group.ComponentRefList.At(Iter2).VObject));

            If (ComponentRef <> Nil) Then
               // The scanning is successful, so get out of here...

               Break;
         End;
      End;

      If (ComponentRef <> Nil) Then
         Break;
   End;

   // Get the parent, encapsulated set and sibling set if there is a valid
   // component reference, otherwise get the sibling set if the component
   // couldn't be found in the encapsulation hierarchy

   NeedSiblingSetForTopComponent := True;

   If (ComponentRef <> Nil) Then Begin
      // Parent

      Parent := ComponentRef.OwnerComponentRef;

      If (Parent <> Nil) Then
         aParent := FindComponent(Parent.Name);

      // Encapsulated set

      For Iter := 0 To ComponentRef.ComponentRefList.Size-1 Do Begin
         Component := FindComponent(TCellMLComponentRef(ComponentRef.ComponentRefList.At(Iter).VObject).Name);

         If (Component <> Nil) Then
            aEncapsulatedSet.Add([Component]);
      End;

      // Sibling set

      If (Parent <> Nil) Then Begin
         NeedSiblingSetForTopComponent := False;

         For Iter := 0 To Parent.ComponentRefList.Size-1 Do Begin
            SiblingComponent := TCellMLComponentRef(Parent.ComponentRefList.At(Iter).VObject).Name;

            If (CompareStr(SiblingComponent, aComponent) <> 0) Then Begin
               Component := FindComponent(SiblingComponent);

               If (Component <> Nil) Then
                  aSiblingSet.Add([Component]);
            End;
         End
      End;
   End;

   If (NeedSiblingSetForTopComponent) Then Begin
      // The component is either a "top" component in the encapsulation
      // hierarchy or couldn't be found anywhere (i.e. no parent or encapsulated
      // set, just a sibling set), so its sibling set consists of the "top"
      // components of the encapsulation hierarchy, as well as any other
      // component which is not part of the the encapsulation hierarchy
      // Note: this is NOT a rule, but it is mentioned in the text (1st
      //       paragraph of page 51), so...

      If ((Group <> Nil) And Not Group.Encapsulation) Then
         // Note: we can only work with "Group" if and only if it's an
         //       encapsulation group, so...

         Group := Nil;      

      // First the "top" components

      If (Group <> Nil) Then 
         For Iter := 0 To Group.ComponentRefList.Size-1 Do Begin
            SiblingComponent := TCellMLComponentRef(Group.ComponentRefList.At(Iter).VObject).Name;

            If (CompareStr(SiblingComponent, aComponent) <> 0) Then Begin
               Component := FindComponent(SiblingComponent);

               If (Component <> Nil) Then
                  aSiblingSet.Add([Component]);
            End;
         End;

      // Then the "isolated" components

      For Iter := 0 To ComponentList.Size-1 Do Begin
         SiblingComponent := TCellMLComponent(ComponentList.At(Iter).VObject).Name;

         If (((Group = Nil) Or ((Group <> Nil) And (Group.NbOfComponentRefs(SiblingComponent) = 0))) And
             (CompareStr(SiblingComponent, aComponent) <> 0)) Then Begin
            Component := FindComponent(SiblingComponent);

            If (Component <> Nil) Then
               aSiblingSet.Add([Component]);
         End;
      End;
   End;
End;

//==============================================================================

Function TCellMLModel.HasAtLeastOneComputeOnceEquation: Boolean;
Var
   Iter: Integer;
Begin
   Result := False;

   For Iter := 0 To EquationList.Size-1 Do
      With TCellMLModelEquation(EquationList.At(Iter).VObject) Do
         If (ComputeOnce) Then Begin
            Result := True;

            Exit;
         End;
End;

//==============================================================================

Constructor TCellMLComponent.Create(Const aOwner: TCellMLModel);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwner := aOwner;

   UnitsList    := TCellMLSortedList.Create;
   VariableList := TCellMLSortedList.Create;
   EquationList := TCellMLList.Create;
End;

//==============================================================================

Destructor TCellMLComponent.Destroy;
Begin
   UnitsList.Free;
   VariableList.Free;
   EquationList.Free;
End;

//==============================================================================

Function TCellMLComponent.IsValid: Boolean;
Var
   Iter: Integer;
Begin
   Result := True;

   // The component must have a valid and unique name

   If (Not IsCellMLIdentifier(Name)) Then Begin
      If (CompareStr(Name, '') = 0) Then
         AddMsg(mtError, FileName, Line, 'a name is required for the component')
      Else
         AddMsg(mtError, FileName, Line, ''''+Name+''' is not a valid name for a component');

      Result := False;
   End Else If (FOwner.NbOfComponents(Name) <> 1) Then Begin
      // The name exists and is valid, but is not unique across the different
      // components

      AddMsg(mtError, FileName, Line, ''''+Name+''' is used to name more than one component');

      Result := False;
   End;

   // Check the units

   UnitsAreValid := True;

   For Iter := 0 To UnitsList.Size-1 Do
      If (Not TCellMLUnits(UnitsList.At(Iter).VObject).IsValid) Then Begin
         Result := False;

         UnitsAreValid := False;
      End;

   // Check the variables

   For Iter := 0 To VariableList.Size-1 Do
      If (Not TCellMLVariable(VariableList.At(Iter).VObject).IsValid) Then
         Result := False;

   // Check the equations

   For Iter := 0 To EquationList.Size-1 Do
      If (Not TMathMLEquation(EquationList.At(Iter).VObject).IsValid) Then
         Result := False;
End;

//==============================================================================

Function TCellMLComponent.NbOfUnits(Const aUnit: String): Integer;
Var
   Iter: Integer;
Begin
   // Is the unit defined in the standard set of units or the model?

   Result := FOwner.NbOfUnits(aUnit);

   // Is the unit defined in the current component?

   For Iter := 0 To UnitsList.Size-1 Do
      If (CompareStr(TCellMLUnits(UnitsList.At(Iter).VObject).Name, aUnit) = 0) Then
         Inc(Result);
End;

//==============================================================================

Function TCellMLComponent.FindUnit(Const aUnit: String): TCellMLUnits;
Var
   UnitIter: DIterator;
Begin
   UnitIter := UnitsList.Find(aUnit);

   If (Not AtEnd(UnitIter)) Then
      Result := TCellMLUnits(DeCAL.GetObject(UnitIter))
   Else
      // Not present in the current component, so what about the model itself?

      Result := FOwner.FindUnit(aUnit);
End;

//==============================================================================

Function TCellMLComponent.NbOfVariables(Const aVariable: String): Integer;
Var
   Iter: Integer;
Begin
   Result := 0;

   // Is the variable defined in the component?

   For Iter := 0 To VariableList.Size-1 Do
      If (CompareStr(TCellMLVariable(VariableList.At(Iter).VObject).Name, aVariable) = 0) Then
         Inc(Result);
End;

//==============================================================================

Function TCellMLComponent.FindVariable(Const aVariable: String): TCellMLVariable;
Var
   VariableIter: DIterator;
Begin
   VariableIter := VariableList.Find(aVariable);

   If (Not AtEnd(VariableIter)) Then
      Result := TCellMLVariable(DeCAL.GetObject(VariableIter))
   Else
      Result := Nil;
End;

//==============================================================================

Constructor TCellMLUnits.Create(Const aOwnerModel: TCellMLModel;
                                Const aOwnerComponent: TCellMLComponent;
                                Const aName: String; Const aBaseUnits: Boolean);
Begin
   If (aOwnerModel <> Nil) Then
      Inherited Create(aOwnerModel.FileName, aName)
   Else If (aOwnerComponent <> Nil) Then
      Inherited Create(aOwnerComponent.FileName, aName)
   Else
      Inherited Create('', aName);

   FOwnerModel     := aOwnerModel;
   FOwnerComponent := aOwnerComponent;

   BaseUnits := aBaseUnits;

   OrigUnitList := TCellMLList.Create;
   UnitList     := TCellMLList.Create;
End;

//==============================================================================

Constructor TCellMLUnits.Create(Const aCellMLUnits: TCellMLUnits);
Begin
   Create(aCellMLUnits.OwnerModel, aCellMLUnits.OwnerComponent, aCellMLUnits.Name, aCellMLUnits.BaseUnits);
End;

//==============================================================================

Destructor TCellMLUnits.Destroy;
Begin
   EmptyUnitList(OrigUnitList);   // In case no reverting has been done...

   OrigUnitList.Free;
   UnitList.Free;
End;

//==============================================================================

Function TCellMLUnits.GetUnitsType: TCellMLUnitsType;
Var
   UnitElement: TCellMLUnit;
   Val: Double;
   Units: TCellMLUnits;
Begin
   If (BaseUnits) Then
      // A user-defined base unit, so...

      Result := utBase
   Else If (NbOfUnitElements = 1) Then Begin
      // Only based on one unit, so maybe a simple units, but for that the unit
      // element must have an exponent attribute value of 1 and the units
      // definition referenced by its units attribute must be either a standard
      // unit, a user-defined base unit or a simple units definition

      UnitElement := GetUnitElement(0);

      If (CompareStr(UnitElement.Exponent, '') <> 0) Then
         Val := StrToFloat(UnitElement.Exponent)
         // Note: when reaching this point, we know that if there is an exponent
         //       then it is a valid one, so no need to use "TryStrToFloat"...
      Else
         Val := 1;

      If (Val = 1) Then Begin
         // The exponent attribute value is 1, so still on track for being a
         // simple units definition...

         If (FOwnerModel <> Nil) Then
            Units := FOwnerModel.FindUnit(UnitElement.Name)
         Else If (FOwnerComponent <> Nil) Then
            Units := FOwnerComponent.FindUnit(UnitElement.Name)
         Else
            // No owner model or component, so that means we are dealing with a
            // standard unit, so...

            Units := FindStandardUnit(UnitElement.Name);

         Assert(Units <> Nil);
         // Note: we should always have a units, so if it is not the case it
         //       means that there is something fundamentally wrong with our
         //       code...

         If (Units.UnitsType <> utComplex) Then Begin
            // Units is either a user-defined base unit or a simple units
            // definition, so...

            If (CompareStr(UnitElement.Offset, '') <> 0) Then
               Val := StrToFloat(UnitElement.Offset)
               // Note: when reaching this point, we know that if there is an
               //       offset then it is a valid one, so no need to use
               //       "TryStrToFloat"...
            Else
               Val := 0;

            If (Val <> 0) Then
               Result := utSimple
            Else
               Result := utComplex;
         End Else
            // None of the above, so...

            Result := utComplex;
      End Else
         // The exponent attribute value of the unit element is not 1, so...

         Result := utComplex;
   End Else
      // The units definition is made of more than one unit element, so...

      Result := utComplex;
End;

//==============================================================================

Function TCellMLUnits.IsValid: Boolean;
Var
   Iter: Integer;
Begin
   Result := True;

   // The unit must have a valid name and neither be a reserved unit nor already
   // be defined

   If (Not IsCellMLIdentifier(Name)) Then Begin
      If (CompareStr(Name, '') = 0) Then
         AddMsg(mtError, FileName, Line, 'a name is required for the unit')
      Else
         AddMsg(mtError, FileName, Line, ''''+Name+''' is not a valid name for a unit');

      Result := False;
   End Else If (((FOwnerModel <> Nil) And (FOwnerModel.ReservedUnit(Name) Or (FOwnerModel.NbOfUnits(Name) > 1))) Or
                ((FOwnerComponent <> Nil) And (FOwnerComponent.Owner.ReservedUnit(Name) Or (FOwnerComponent.NbOfUnits(Name) > 1)))) Then Begin
      // The unit is a reserved unit or is already defined somewhere

      AddMsg(mtError, FileName, Line, ''''+Name+''' is already defined');

      Result := False;
   End;

   If (BaseUnits) Then Begin
      If (NbOfUnitElements <> 1) Then Begin
         // Note: a base unit should have no unit element, but because to get
         //       units checking work we actually need one, hence the test
         //       against "1" rather than "0"...

         AddMsg(mtError, FileName, Line, ''''+Name+''' must be either a base unit or based on other units');

         Result := False;
      End;
   End Else If (NbOfUnitElements = 0) Then Begin
      AddMsg(mtError, FileName, Line, ''''+Name+''' must be based on at least one other unit');

      Result := False;
   End Else
      // A name for the unit exists and is valid, so check the units it is based
      // on

      For Iter := 0 To NbOfUnitElements-1 Do
         If (Not GetUnitElement(Iter).IsValid) Then
            Result := False;
End;

//==============================================================================

Procedure TCellMLUnits.SortUnitElements;
Begin
   UnitList.Comparator := MakeComparator(UnitCompare);

   DeCAL.Sort(UnitList);
End;

//==============================================================================

Class Function TCellMLUnits.EmptyUnits(Const aCellMLUnits: TCellMLUnits): TCellMLUnits;
Begin
   Result := TCellMLUnits.Create(aCellMLUnits);
End;

//==============================================================================

Class Function TCellMLUnits.DuplicateUnits(Const aCellMLUnits: TCellMLUnits;
                                           Const aResetBaseUnits: Boolean): TCellMLUnits;
Var
   Iter: Integer;
Begin
   Result := TCellMLUnits.EmptyUnits(aCellMLUnits);

   // Populate the units definition

   For Iter := 0 To aCellMLUnits.NbOfUnitElements-1 Do
      Result.AddUnitElement([TCellMLUnit.Create(aCellMLUnits.GetUnitElement(Iter))]);

   If (aResetBaseUnits) Then
      Result.BaseUnits := False;
End;

//==============================================================================

Function TCellMLUnits.NbOfUnitElements: Integer;
Begin
   Result := UnitList.Size;
End;

//==============================================================================

Procedure TCellMLUnits.EmptyUnitList(Const aUnitList: TCellMLList;
                                     Const aFree: Boolean);
Begin
   If (aFree) Then
      ObjFree(aUnitList);

   aUnitList.Clear;
End;

//==============================================================================

Procedure TCellMLUnits.AddUnitElement(Const aUnitElements: Array Of Const);
Begin
   UnitList.Add(aUnitElements);

   HasCanonicalForm := False;
End;

//==============================================================================

Procedure TCellMLUnits.RemoveUnitElement(Const aIter: Integer);
Begin
   UnitList.RemoveAt(aIter);

   HasCanonicalForm := False;
End;

//==============================================================================

Function TCellMLUnits.GetUnitElement(Const aIter: Integer): TCellMLUnit;
Begin
   Result := TCellMLUnit(UnitList.At(aIter).VObject);
End;

//==============================================================================

Procedure TCellMLUnits.CanonicalForm;
   Function Expand(Const aCellMLUnits: TCellMLUnits): TCellMLUnits;
      Function RetrieveUEInfoAndUnits(Const aIter: Integer;
                                      Var aName: String;
                                      Var aPrefixStr, aExponentStr, aMultiplierStr, aOffsetStr: String;
                                      Var aPrefixDbl, aExponentDbl, aMultiplierDbl, aOffsetDbl: Double;
                                      Var aUnits: TCellMLUnits): Boolean;
      Var
         UnitElement: TCellMLUnit;
      Begin
         UnitElement := aCellMLUnits.GetUnitElement(aIter);

         aPrefixStr     := UnitElement.Prefix;
         aExponentStr   := UnitElement.Exponent;
         aMultiplierStr := UnitElement.Multiplier;
         aOffsetStr     := UnitElement.Offset;
         
         UnitElement.ConvUnitElemAttr(aPrefixDbl, aExponentDbl, aMultiplierDbl, aOffsetDbl);
         
         // Retrieve the units definition associated to the unit element

         If (aCellMLUnits.OwnerComponent <> Nil) Then
            aUnits := aCellMLUnits.OwnerComponent.FindUnit(UnitElement.Name)
         Else If (aCellMLUnits.OwnerModel <> Nil) Then
            aUnits := aCellMLUnits.OwnerModel.FindUnit(UnitElement.Name)
         Else
            // No owner model or component, so that means we are dealing with
            // a standard unit, so...

            aUnits := FindStandardUnit(UnitElement.Name);

         Assert(aUnits <> Nil);
         // Note: we should always have a units, so if it is not the case it
         //       means that there is something fundamentally wrong with our
         //       code...

         // Recursively expand the units definition, if needed

         If (aUnits.UnitsType <> utBase) Then Begin
            aUnits := Expand(aUnits);

            Result := True;
         End Else
            Result := False;

         aName := aUnits.Name;
         // Note: one would normally set "aName" with "UnitElement.Name", but
         //       there are cases where that wouldn't work. Indeed, a reference
         //       to "liter" or "meter" will be automatically converted to a
         //       reference to "litre" or "metre", respectively. "aUnits.Name"
         //       will always contain the right information, hence we use it
         //       instead of "UnitElement.Name", which only contains the unit
         //       element name before the automatic conversion, if required, has
         //       taken place, so...
      End;
      Procedure RetrieveUnitsUEInfo(Const aUnits: TCellMLUnits;
                                    Const aIter: Integer;
                                    Var aName: String;
                                    Var aPrefixStr, aExponentStr, aMultiplierStr, aOffsetStr: String;
                                    Var aPrefixDbl, aExponentDbl, aMultiplierDbl, aOffsetDbl: Double);
      Var
         UnitsUnitElement: TCellMLUnit;
      Begin
         UnitsUnitElement := aUnits.GetUnitElement(aIter);

         aName := UnitsUnitElement.Name;

         aPrefixStr     := UnitsUnitElement.Prefix;
         aExponentStr   := UnitsUnitElement.Exponent;
         aMultiplierStr := UnitsUnitElement.Multiplier;
         aOffsetStr     := UnitsUnitElement.Offset;
         
         UnitsUnitElement.ConvUnitElemAttr(aPrefixDbl, aExponentDbl, aMultiplierDbl, aOffsetDbl);
      End;
   Var
      Units: TCellMLUnits;
      FreeUnits: Boolean;
      UEName, UUEName: String;
      UEPrefixStr, UEExponentStr, UEMultiplierStr, UEOffsetStr: String;
      UEPrefixDbl, UEExponentDbl, UEMultiplierDbl, UEOffsetDbl: Double;
      UUEPrefixStr, UUEExponentStr, UUEMultiplierStr, UUEOffsetStr: String;
      UUEPrefixDbl, UUEExponentDbl, UUEMultiplierDbl, UUEOffsetDbl: Double;
      Iter, UnitsIter: Integer;
      UEMPE: Double;
   Begin
      // Expand the passed units definition based on the algorithm developed by
      // Jonathan Cooper in DOI: 10.1002/spe.828
      // Note: we have, based on Jonathan Cooper's paper, the following:
      //       BaseUnits: [n], with n the name of the base unit
      //       SimpleUnits: [m, p, u, o], with m the multiplier, p the prefix, u
      //                    the units and o the offset
      //       ComplexUnits: [m, p, u, e], with m the multiplier, p the prefix,
      //                     u the units and e the exponent

      Case aCellMLUnits.UnitsType Of
         utBase:
            // Base unit
            // [n] -> [n]

            Result := TCellMLUnits.DuplicateUnits(aCellMLUnits);
         utSimple: Begin
            // Simple units definition

            Result := TCellMLUnits.EmptyUnits(aCellMLUnits);

            // Retrieve information about the unit element

            FreeUnits := RetrieveUEInfoAndUnits(0, UEName, 
                                                   UEPrefixStr, UEExponentStr, UEMultiplierStr, UEOffsetStr,
                                                   UEPrefixDbl, UEExponentDbl, UEMultiplierDbl, UEOffsetDbl,
                                                   Units);

            // Expand the units definition

            If (Units.UnitsType = utBase) Then
               // Either a standard unit or a user-defined base unit, so...
               // [m, p, u, o] with u = n, hence we must expand to:
               // [m, p, n, o]

               Result.AddUnitElement([TCellMLUnit.Create(Nil, UEName,
                                                         // Prefix
                                                         UEPrefixStr,
                                                         // Exponent
                                                         '1',
                                                         // Multiplier
                                                         UEMultiplierStr,
                                                         // Offset
                                                         UEOffsetStr)])
            Else Begin
               // If it is not a standard unit or a user-definied base unit,
               // then it has to be a simple units definition, since we are
               // originally dealing with such a definition...
               // [m, p, u, o] with u = [m', p', u', o'], hence we must expand
               // to:
               // [m*10^p*m'*10^p', 0, u', o+o'/(m*10^p)], i.e.
               // [m*m'*10^(p+p'), 0, u', o+o'/(m*10^p)]

               RetrieveUnitsUEInfo(Units, 0, UUEName,
                                             UUEPrefixStr, UUEExponentStr, UUEMultiplierStr, UUEOffsetStr,
                                             UUEPrefixDbl, UUEExponentDbl, UUEMultiplierDbl, UUEOffsetDbl);

               Result.AddUnitElement([TCellMLUnit.Create(Nil, UUEName,
                                                         // Prefix
                                                         '0',
                                                         // Exponent
                                                         '1',
                                                         // Multiplier
{$IFDEF OPT_MATH}
                                                         SimplifyNb(UEMultiplierDbl*UUEMultiplierDbl*OptPower(10, UEPrefixDbl+UUEPrefixDbl)),
{$ELSE}
                                                         SimplifyNb(UEMultiplierDbl*UUEMultiplierDbl*Power(10, UEPrefixDbl+UUEPrefixDbl)),
{$ENDIF}
                                                         // Offset
{$IFDEF OPT_MATH}
                                                         SimplifyNb(UEOffsetDbl+UUEOffsetDbl/(UEMultiplierDbl*OptPower(10, UEPrefixDbl))))]);
{$ELSE}
                                                         SimplifyNb(UEOffsetDbl+UUEOffsetDbl/(UEMultiplierDbl*Power(10, UEPrefixDbl))))]);
{$ENDIF}
            End;

            // Release the memory used by the units definition, should some
            // having been allocated

            If (FreeUnits) Then
               Units.Free;
         End;
      Else   // utComplex
         // Complex units definition

         Result := TCellMLUnits.EmptyUnits(aCellMLUnits);

         For Iter := 0 To aCellMLUnits.NbOfUnitElements-1 Do Begin
            // Retrieve information about the unit element

            FreeUnits := RetrieveUEInfoAndUnits(Iter, UEName, 
                                                      UEPrefixStr, UEExponentStr, UEMultiplierStr, UEOffsetStr,
                                                      UEPrefixDbl, UEExponentDbl, UEMultiplierDbl, UEOffsetDbl,
                                                      Units);

            // Expand the units definition

            If (Units.UnitsType = utBase) Then
               // Either a standard unit or a user-defined base unit, so...
               // [m, p, u, e] with u = n, hence we must expand to:
               // [m, p, n, e]

               Result.AddUnitElement([TCellMLUnit.Create(Nil, UEName,
                                                         // Prefix
                                                         UEPrefixStr,
                                                         // Exponent
                                                         UEExponentStr,
                                                         // Multiplier
                                                         UEMultiplierStr,
                                                         // Offset
                                                         UEOffsetStr)])
            Else If (Units.UnitsType = utSimple) Then Begin
               // A simple units definition
               // [m, p, u, e] with u = [m', p', u', o'], hence we must expand
               // to:
               // [m*10^p^e*m'^e, p', u', e], i.e.
               // [m*(m'*10^p)^e, p', u', e]

               RetrieveUnitsUEInfo(Units, 0, UUEName,
                                             UUEPrefixStr, UUEExponentStr, UUEMultiplierStr, UUEOffsetStr,
                                             UUEPrefixDbl, UUEExponentDbl, UUEMultiplierDbl, UUEOffsetDbl);

               Result.AddUnitElement([TCellMLUnit.Create(Nil, UUEName,
                                                         // Prefix
                                                         UUEPrefixStr,
                                                         // Exponent
                                                         UEExponentStr,
                                                         // Multiplier
{$IFDEF OPT_MATH}
                                                         SimplifyNb(UEMultiplierDbl*OptPower(UUEMultiplierDbl*OptPower(10, UEPrefixDbl), UEExponentDbl)),
{$ELSE}
                                                         SimplifyNb(UEMultiplierDbl*Power(UUEMultiplierDbl*Power(10, UEPrefixDbl), UEExponentDbl)),
{$ENDIF}
                                                         // Offset
                                                         '0')]);
            End Else Begin
               // A complex units definition
               // [m, p, u, e] with u = [m_1', p_1', u_1', e_1'].
               //                       [m_2', p_2', u_2', e_2']. ... .
               //                       [m_n', p_n', u_n', e_n'], hence we must
               // expand to:
               // [m*10^p^e*(m_1'*10^p_1'^e_1')^e, 0, u_1', e*e_1'], i.e.
               // [m*10^(p*e)*(m_1'*10^(p_1'*e_1'))^e, 0, u_1', e*e_1'], i.e.
               // [(m_x'*10^p_x'^e_x')^e, 0, u_x', e*e_x'] for x in [2..n], i.e.
               // [(m_x'*10^(p_x'*e_x'))^e, 0, u_x', e*e_x']

               If (Units.NbOfUnitElements > 1) Then
                  Units.SortUnitElements;

               For UnitsIter := 0 To Units.NbOfUnitElements-1 Do Begin
                  RetrieveUnitsUEInfo(Units, UnitsIter, UUEName,
                                                        UUEPrefixStr, UUEExponentStr, UUEMultiplierStr, UUEOffsetStr,
                                                        UUEPrefixDbl, UUEExponentDbl, UUEMultiplierDbl, UUEOffsetDbl);

                  If (UnitsIter = 0) Then
{$IFDEF OPT_MATH}
                     UEMPE := UEMultiplierDbl*OptPower(10, UEPrefixDbl*UEExponentDbl)
{$ELSE}
                     UEMPE := UEMultiplierDbl*Power(10, UEPrefixDbl*UEExponentDbl)
{$ENDIF}
                  Else
                     UEMPE := 1;

                  Result.AddUnitElement([TCellMLUnit.Create(Nil, UUEName,
                                                            // Prefix
                                                            '0',
                                                            // Exponent
                                                            SimplifyNb(UEExponentDbl*UUEExponentDbl),
                                                            // Multiplier
{$IFDEF OPT_MATH}
                                                            SimplifyNb(UEMPE*OptPower(UUEMultiplierDbl*OptPower(10, UUEPrefixDbl*UUEExponentDbl), UEExponentDbl)),
{$ELSE}
                                                            SimplifyNb(UEMPE*Power(UUEMultiplierDbl*Power(10, UUEPrefixDbl*UUEExponentDbl), UEExponentDbl)),
{$ENDIF}
                                                            // Offset
                                                            '0')]);
               End;
            End;

            // Release the memory used by the units definition, should some
            // having been allocated

            If (FreeUnits) Then
               Units.Free;
         End;
      End;
   End;
   Procedure Simplify(Const aCellMLUnits: TCellMLUnits);
   Var
      CrtUnitElement, NextUnitElement: TCellMLUnit;
      CrtUEPrefix, CrtUEExponent, CrtUEMultiplier, CrtUEOffset: Double;
      NextUEPrefix, NextUEExponent, NextUEMultiplier, NextUEOffset: Double;
      NewUEExponent: Double;
      Iter, IterStep: Integer;
      RemoveNextUnitElement: Boolean;
   Begin
      // Simplify the passed units definition based on the algorithm developed
      // by Jonathan Cooper in DOI: 10.1002/spe.828

      If (aCellMLUnits.UnitsType <> utBase) Then Begin
         // Simple or complex units definition with more than one unit element

         // Step #0: sort the list in alphabetical order, but with dimensionless
         //          at the very end, if it exists

         If (aCellMLUnits.NbOfUnitElements > 1) Then
            aCellMLUnits.SortUnitElements;

         // Step #1: for each unique units reference, replace all the references
         //          by a single reference (treating dimensionless last)
         //          Note: to have sorted the list ensures that dimensionless is
         //                indeed treated last

         Iter := 0;

         While Iter <= aCellMLUnits.NbOfUnitElements-1 Do Begin
            CrtUnitElement := aCellMLUnits.GetUnitElement(Iter);

            CrtUnitElement.ConvUnitElemAttr(CrtUEPrefix, CrtUEExponent, CrtUEMultiplier, CrtUEOffset);

            If (Iter < aCellMLUnits.NbOfUnitElements-1) Then Begin
               NextUnitElement := aCellMLUnits.GetUnitElement(Iter+1);

               RemoveNextUnitElement := CompareStr(CrtUnitElement.Name, NextUnitElement.Name) = 0;
            End Else Begin
               NextUnitElement := Nil;   // Just to avoid a warning...

               RemoveNextUnitElement := False;
            End;

            If (RemoveNextUnitElement) Then Begin
               // Unit elements with the same unit, so remove "NextUnitElement"

               aCellMLUnits.RemoveUnitElement(Iter+1);

               NextUnitElement.ConvUnitElemAttr(NextUEPrefix, NextUEExponent, NextUEMultiplier, NextUEOffset);

               NextUnitElement.Free;

               IterStep := 0;

               If (CompareStr(CrtUnitElement.Name, _DIMENSIONLESS_) = 0) Then
                  // We are dealing with two dimensionless unit elements, so...

                  NextUEExponent := 0;   // To make sure we still have an
                                         // exponent of 1
            End Else Begin
               NextUEPrefix     := 0;
               NextUEExponent   := 0;   // Note: not 1, as otherwise the new
                                        //       exponent value will be wrong!
               NextUEMultiplier := 1;
               NextUEOffset     := 0;

               IterStep := 1;
            End;

            // Step #1A: sum the exponents

            NewUEExponent := CrtUEExponent+NextUEExponent;

            // Step #1B: replace with dimensionless if "NewUEExponent" is 0,
            //           otherwise replace the original the original exponent
            //           with "NewUEExponent"

            If (NewUEExponent = 0) Then Begin
               // Remove "CrtUnitElement"

               aCellMLUnits.RemoveUnitElement(Iter);

               CrtUnitElement.Free;

               // Create a new unit element of type dimensionless

               CrtUnitElement := TCellMLUnit.Create(Nil, _DIMENSIONLESS_, '0', '1', '1', '0');

               aCellMLUnits.AddUnitElement([CrtUnitElement]);
            End Else
               CrtUnitElement.Exponent := SimplifyNb(NewUEExponent);

            // Step #1C: the new multiplier is the product of all the
            //           multiplicative factors on the original references

{$IFDEF OPT_MATH}
            CrtUnitElement.Multiplier := SimplifyNb(CrtUEMultiplier*NextUEMultiplier*OptPower(10, CrtUEPrefix*CrtUEExponent)*OptPower(10, NextUEPrefix*NextUEExponent));
{$ELSE}
            CrtUnitElement.Multiplier := SimplifyNb(CrtUEMultiplier*NextUEMultiplier*Power(10, CrtUEPrefix*CrtUEExponent)*Power(10, NextUEPrefix*NextUEExponent));
{$ENDIF}

            // Step #1D: the new prefix is 0

            CrtUnitElement.Prefix := '0';

            // Go to the next unit element, if required

            Inc(Iter, IterStep);
         End;

         // Step #2: remove the dimensionless reference if it exists and it has
         //          a multiplier of 1

         CrtUnitElement := aCellMLUnits.GetUnitElement(aCellMLUnits.NbOfUnitElements-1);

         If (CompareStr(CrtUnitElement.Name, _DIMENSIONLESS_) = 0) Then Begin
            CrtUnitElement.ConvUnitElemAttr(CrtUEPrefix, CrtUEExponent, CrtUEMultiplier, CrtUEOffset);

            If (CrtUEMultiplier = 1) Then Begin
               // The dimensionless element has a multiplier of 1, so remove it

               aCellMLUnits.RemoveUnitElement(aCellMLUnits.NbOfUnitElements-1);

               CrtUnitElement.Free;
            End Else If (aCellMLUnits.NbOfUnitElements > 1) Then Begin
               // There is more than one unit element, so update the multiplier
               // of the first element by multiplying it with that of the
               // dimensionless element, and then remove the dimensionless
               // element, since it isn't needed anymore...
               // Note: this is not in Jonathan Cooper's original algorithm, but
               //       this avoids having to sort things during the expansion,
               //       so...

               NextUnitElement := aCellMLUnits.GetUnitElement(0);

               NextUnitElement.ConvUnitElemAttr(NextUEPrefix, NextUEExponent, NextUEMultiplier, NextUEOffset);

               NextUnitElement.Multiplier := SimplifyNb(CrtUEMultiplier*NextUEMultiplier);

               aCellMLUnits.RemoveUnitElement(aCellMLUnits.NbOfUnitElements-1);

               CrtUnitElement.Free;
            End;
         End;

         // Step #3: if the list is empty, then the result is dimensionless,
         //          otherwise the result is a complex units definition with the
         //          generated list of references, one for each uniques units,
         //          sorted by unit name (which would already be the case, since
         //          the original list is assumed to be sorted)

         If (aCellMLUnits.NbOfUnitElements = 0) Then
            aCellMLUnits.AddUnitElement([TCellMLUnit.Create(Nil, _DIMENSIONLESS_, '0', '1', '1', '0')]);
      End;
   End;
Var
   Units: TCellMLUnits;
Begin
   If (Not (BaseUnits Or HasCanonicalForm)) Then Begin
      // Not a base units AND doesn't have a canonical form, so... 

      // Expland the current units definition

      Units := Expand(Self);

      // Simplify the current units definition

      Simplify(Units);

      // Update the current units definition with its expanded and simplified
      // version, but first back it up (though only if necessary)

      If (CompareStr(FileName, '') <> 0) Then Begin
         // This is a 'proper' units definition, so back it up indeed (so that
         // we can 'properly' refer to it later, if needed)

         EmptyUnitList(OrigUnitList);

         UnitList.CloneTo(OrigUnitList);

         EmptyUnitList(UnitList, False);
         // Note: we don't free the unit elements, since they are now owned by
         //       "OrigUnitList"
      End Else
         EmptyUnitList(UnitList);

      Units.UnitList.CloneTo(UnitList);

      // No need for "Units", so...

      EmptyUnitList(Units.UnitList, False);
      // Note: we don't free the unit elements, since they are now owned by
      //       "UnitList"

      Units.Free;

      // Now we have a canonical form

      HasCanonicalForm := True;
   End;
End;

//==============================================================================

Function TCellMLUnits.EquivalenceType(Const aCellMLUnits: TCellMLUnits): TCellMLUnitsEquivalenceType;
Var
   Iter: Integer;
   UnitElementLHS, UnitElementRHS: TCellMLUnit;
   UELHSPrefix, UELHSExponent, UELHSMultiplier, UELHSOffset: Double;
   UERHSPrefix, UERHSExponent, UERHSMultiplier, UERHSOffset: Double;
   LHSMultExpo, RHSMultExpo: Double;
Begin
   // Check that the passed units definition is dimensionally equivalent to the
   // current one based on the algorithm developed by Jonathan Cooper in DOI:
   // 10.1002/spe.828

   CanonicalForm;
   aCellMLUnits.CanonicalForm;

   // The two units definitions can only be considered dimensionally equivalent
   // if the two consist of the same number of unit elements and if each of
   // these unit elements have the same name and same exponent value

   If (NbOfUnitElements <> aCellMLUnits.NbOfUnitElements) Then
      Result := etNone
   Else Begin
      // Same number of unit elements, so make sure that the name and exponent
      // of each of them are the same

      For Iter := 0 To NbOfUnitElements-1 Do Begin
         UnitElementLHS := GetUnitElement(Iter);
         UnitElementRHS := aCellMLUnits.GetUnitElement(Iter);

         UnitElementLHS.ConvUnitElemAttr(UELHSPrefix, UELHSExponent, UELHSMultiplier, UELHSOffset);
         UnitElementRHS.ConvUnitElemAttr(UERHSPrefix, UERHSExponent, UERHSMultiplier, UERHSOffset);

         If ((CompareStr(UnitElementLHS.Name, UnitElementRHS.Name) <> 0) Or
             (UELHSExponent <> UERHSExponent)) Then Begin
            // The names and/or exponents are different, so...

            Result := etNone;

            Exit;
         End;
      End;

      // At this stage, the units are dimensionally equivalent, but are they
      // exactly the same? To find out, we need to check the multipliers
      // (combined with the exponents) and the offsets
      // Note: the prefixes ares always equal to zero, so...

      LHSMultExpo := 1;
      RHSMultExpo := 1;

      For Iter := 0 To NbOfUnitElements-1 Do Begin
         GetUnitElement(Iter).ConvUnitElemAttr(UELHSPrefix, UELHSExponent, UELHSMultiplier, UELHSOffset);
         aCellMLUnits.GetUnitElement(Iter).ConvUnitElemAttr(UERHSPrefix, UERHSExponent, UERHSMultiplier, UERHSOffset);

{$IFDEF OPT_MATH}
         LHSMultExpo := LHSMultExpo*UELHSMultiplier*OptPower(10, UELHSExponent);
         RHSMultExpo := RHSMultExpo*UERHSMultiplier*OptPower(10, UERHSExponent);
{$ELSE}
         LHSMultExpo := LHSMultExpo*UELHSMultiplier*Power(10, UELHSExponent);
         RHSMultExpo := RHSMultExpo*UERHSMultiplier*Power(10, UERHSExponent);
{$ENDIF}

         If (Not DblEquiv(UELHSOffset, UERHSOffset)) Then Begin
            // The offset differ, so...

            Result := etDimensional;

            Exit;
         End;
      End;

      // All the offsets are the same, but what about the multipliers (combined
      // with the exponents)?

      If (Not DblEquiv(LHSMultExpo, RHSMultExpo)) Then
         // They are different, so...

         Result := etDimensional
      Else
         // Exactly the same, so...

         Result := etExact;
   End;
End;

//==============================================================================

Function TCellMLUnits.Times(Const aCellMLUnits: TCellMLUnits): TCellMLUnits;
Var
   Iter: Integer;
Begin
   // Multiply the current and passed units definitions based on the algorithm
   // developed by Jonathan Cooper in DOI: 10.1002/spe.828

   // Note: we would normally convert any base or simple units definition to a
   //       complex, but as it happens our data structure is such that this is
   //       not required, since all units definitions will contain information
   //       that can be used for either a base, simple or complex units
   //       definition, so...

   Result := TCellMLUnits.DuplicateUnits(Self, True);

   For Iter := 0 To aCellMLUnits.NbOfUnitElements-1 Do
      Result.AddUnitElement([TCellMLUnit.Create(aCellMLUnits.GetUnitElement(Iter))]);

   // Check whether we can retrieve some owner information, should that be
   // required

   If ((Result.OwnerModel = Nil) And (aCellMLUnits.OwnerModel <> Nil)) Then
      Result.FOwnerModel := aCellMLUnits.OwnerModel;
      // Note: "FOwnerModel" and not "OwnerModel", since this is a read-only
      //       property...

   If ((Result.OwnerComponent = Nil) And (aCellMLUnits.OwnerComponent <> Nil)) Then
      Result.FOwnerComponent := aCellMLUnits.OwnerComponent;
      // Note: "FOwnerComponent" and not "OwnerComponent", since this is a
      //       read-only property...

   // Get the canonical form

   Result.CanonicalForm;

   // New name for the units

   Result.Name := Name+'_times_'+aCellMLUnits.Name;
End;

//==============================================================================

Function TCellMLUnits.Exponentiates(Const aExponent: Double): TCellMLUnits;
Var
   Iter: Integer;
   UnitElement: TCellMLUnit;
   UEPrefix, UEExponent, UEMultiplier, UEOffset: Double;
Begin
   // Exponentiate the current units definition by the passed exponent based on
   // the algorithm developed by Jonathan Cooper in DOI: 10.1002/spe.828

   Result := TCellMLUnits.DuplicateUnits(Self, True);

   Case UnitsType Of
      utBase:
         Result.GetUnitElement(0).Exponent := SimplifyNb(aExponent);
      utSimple: Begin
         UnitElement := Result.GetUnitElement(0);

         UnitElement.Exponent := SimplifyNb(aExponent);
         UnitElement.Offset   := '0';
      End;
   Else   // utComplex
      For Iter := 0 To Result.NbOfUnitElements-1 Do Begin
         UnitElement := Result.GetUnitElement(Iter);

         UnitElement.ConvUnitElemAttr(UEPrefix, UEExponent, UEMultiplier, UEOffset);

{$IFDEF OPT_MATH}
         UnitElement.Multiplier := SimplifyNb(OptPower(UEMultiplier, aExponent));
{$ELSE}
         UnitElement.Multiplier := SimplifyNb(Power(UEMultiplier, aExponent));
{$ENDIF}

         UnitElement.Exponent := SimplifyNb(UEExponent*aExponent);
      End;
   End;

   // Get the canonical form

   Result.CanonicalForm;

   // New name for the units

   Result.Name := Name+'_to_the_power_of_'+SimplifyNb(aExponent);
End;

//==============================================================================

Constructor TCellMLUnitPrefix.Create(Const aName: String;
                                     Const aFactor: Integer);
Begin
   Name   := aName;
   Factor := aFactor;
End;

//==============================================================================

Constructor TCellMLUnit.Create(Const aOwner: TCellMLUnits;
                               Const aName, aPrefix, aExponent, aMultiplier, aOffset: String);
Begin
   If (aOwner = Nil) Then
      Inherited Create('', aName)
   Else
      Inherited Create(aOwner.FileName, aName);

   FOwner := aOwner;

   Prefix     := aPrefix;
   Exponent   := aExponent;
   Multiplier := aMultiplier;
   Offset     := aOffset;
End;

//==============================================================================

Constructor TCellMLUnit.Create(Const aCellMLUnit: TCellMLUnit);
Begin
   Create(aCellMLUnit.Owner, aCellMLUnit.Name, aCellMLUnit.Prefix, aCellMLUnit.Exponent, aCellMLUnit.Multiplier, aCellMLUnit.Offset);
End;

//==============================================================================

Function TCellMLUnit.CircularUnits(Const aCellMLModel: TCellMLModel;
                                   Const aCellMLComponent: TCellMLComponent;
                                   Const aCrtUnit: String; Const aUnit: String;
                                   Const aVisitedUnits: TCellMLList): Boolean;
Var
   Iter: Integer;
   Units: TCellMLUnits;
   UnitElement: TCellMLUnit;
Begin
   Result := False;

   // Has the current unit already been visited?

   For Iter := 0 To aVisitedUnits.Size-1 Do
      If (CompareStr(String(aVisitedUnits.At(Iter).VString), aCrtUnit) = 0) Then
         Exit;   // Circular reference for another unit that we are not
                 // concerned with, so ignore it

   aVisitedUnits.Add([aCrtUnit]);

   // Is the current unit part of the set of standard units?

   If (FindStandardUnit(aCrtUnit) <> Nil) Then
      Exit   // Yes, so...
   Else Begin
      // Look for the definition of the current unit within the model

      If (aCellMLModel <> Nil) Then
         Units := aCellMLModel.FindUnit(aCrtUnit)
      Else
         Units := aCellMLComponent.FindUnit(aCrtUnit);

      If (Units <> Nil) Then Begin
         // Is the defined unit a base unit?

         If (Units.BaseUnits) Then
            Exit   // Yes, so...
         Else
            // The defined unit is not a base one, so scan the units it is made
            // of...

            For Iter := 0 To Units.NbOfUnitElements-1 Do Begin
               UnitElement := Units.GetUnitElement(Iter);

               If ((CompareStr(UnitElement.Name, aUnit) = 0) Or CircularUnits(aCellMLModel, aCellMLComponent, UnitElement.Name, aUnit, aVisitedUnits)) Then Begin
                  // Circular reference found!

                  Result := True;

                  Exit;
               End;
            End;
      End;
   End;
End;

//==============================================================================

Function TCellMLUnit.IsValid: Boolean;
Var
   VisitedUnits: TCellMLList;
   IntVal: Integer;
   Val: Double;
Begin
   // The unit from which the new unit is to be defined must exist

   If (FOwner.OwnerModel <> Nil) Then
      Result := FOwner.OwnerModel.NbOfUnits(Name) <> 0
   Else
      Result := FOwner.OwnerComponent.NbOfUnits(Name) <> 0;

   If (Not Result) Then
      AddMsg(mtError, FileName, Line, ''''+Name+''' is not defined anywhere')
   Else Begin
      // The unit from which the new unit is to be defined exists, but it must
      // not be referenced (in)directly by another unit definition

      VisitedUnits := TCellMLList.Create(False);
      // Note: no need to be managed, since we are storing strings...

      If (CircularUnits(FOwner.OwnerModel, FOwner.OwnerComponent, Name, Name, VisitedUnits)) Then Begin
         AddMsg(mtError, FileName, Line, ''''+Name+''' is involved in a circular unit definition');

         Result := False;
      End;

      VisitedUnits.Free;

      // Is the prefix a known prefix, if it exists?

      If ((CompareStr(Prefix, '') <> 0) And (PrefixVal(Prefix) = 0)) Then
         // Couldn't recognise the prefix, so it has to be a number...

         If (Not TryStrToInt(Prefix, IntVal)) Then Begin
            AddMsg(mtError, FileName, Line, 'the prefix must be either a name of a prefix (e.g. '''+CENTI+''') or an integer');

            Result := False;
         End;

      // Is the exponent a real number, if it exists?

      If ((CompareStr(Exponent, '') <> 0) And Not TryStrToFloat(Exponent, Val)) Then Begin
         AddMsg(mtError, FileName, Line, 'the exponent must be a real number');

         Result := False;
      End;

      // Is the multiplier a real number, if it exists?

      If ((CompareStr(Multiplier, '') <> 0) And Not TryStrToFloat(Multiplier, Val)) Then Begin
         AddMsg(mtError, FileName, Line, 'the multiplier must be a real number');
      
         Result := False;
      End;

      // Is the offset a real number, if it exists?

      If (CompareStr(Offset, '') <> 0) Then Begin
         If (Not TryStrToFloat(Offset, Val)) Then Begin
            AddMsg(mtError, FileName, Line, 'the offset must be a real number');

            Result := False;
         End Else If (Val <> 0) Then Begin
            // The offset exists and is a real number different from zero. In
            // that case, the unit, which is based on the current one, cannot be
            // based on more than one unit (so as to allow for simple unit
            // definitions)

            If (FOwner.NbOfUnitElements <> 1) Then Begin
               AddMsg(mtError, FileName, Line, 'the offset has a value other than ''0'' and therefore prevents '''+FOwner.Name+''' from being based on more than one unit');

               Result := False;
            End;

            // Because the offset has a value different from zero, the exponent
            // cannot have a value other than one...

            If ((CompareStr(Exponent, '') <> 0) And TryStrToFloat(Exponent, Val) And (Val <> 1)) Then Begin
               AddMsg(mtError, FileName, Line, 'the offset has a value other than ''0'' and therefore prevents the exponent from having a value other than ''1''');

               Result := False;
            End;
         End;
      End;
   End;
End;

//==============================================================================

Procedure TCellMLUnit.ConvUnitElemAttr(Var aPrefix, aExponent, aMultiplier, aOffset: Double);
Begin
   aPrefix := PrefixVal(Prefix);

   If ((CompareStr(Prefix, '') <> 0) And (aPrefix = 0)) Then
      // Either no prefix or couldn't recognise it, so it has to be a
      // number...

      aPrefix := StrToFloat(Prefix);

   If (CompareStr(Exponent, '') <> 0) Then
      aExponent := StrToFloat(Exponent)
   Else
      aExponent := 1;

   If (CompareStr(Multiplier, '') <> 0) Then
      aMultiplier := StrToFloat(Multiplier)
   Else
      aMultiplier := 1;

   If (CompareStr(Offset, '') <> 0) Then
      aOffset := StrToFloat(Offset)
   Else
      aOffset := 0;
End;

//==============================================================================

Constructor TCellMLVariable.Create(Const aOwner: TCellMLComponent);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwner := aOwner;

   InitialValue := '';

   PublicInterface  := '';
   PrivateInterface := '';
End;

//==============================================================================

Function TCellMLVariable.IsValid: Boolean;
Var
   Val: Double;
   Mapped: Boolean;
   Iter, Iter2: Integer;
   Connection: TCellMLConnection;
   MatchComponent1, MatchComponent2: Boolean;
   MapVariables: TCellMLMapVariables;
Begin
   Result := True;

   // The variable must have a valid and unique name

   If (Not IsCellMLIdentifier(Name)) Then Begin
      If (CompareStr(Name, '') = 0) Then
         AddMsg(mtError, FileName, Line, 'a name is required for the variable')
      Else
         AddMsg(mtError, FileName, Line, ''''+Name+''' is not a valid name for a variable');

      Result := False;
   End Else If (FOwner.NbOfVariables(Name) <> 1) Then Begin
      // The name exists and is valid, but is not unique across the different
      // variables

      AddMsg(mtError, FileName, Line, ''''+Name+''' is used to name more than one variable');

      Result := False;
   End;

   // The units attribute must not be a reserved unit, but one of the standard
   // units or a unit defined in the current component or model

   If (FOwner.Owner.ReservedUnit(Units)) Then Begin
      AddMsg(mtError, FileName, Line, ''''+Units+''' is a reserved unit that cannot be explicitly used');

      Result := False;

      Owner.UnitsAreValid := False;
   End Else If (FOwner.NbOfUnits(Units) = 0) Then Begin
      AddMsg(mtError, FileName, Line, ''''+Units+''' is not defined anywhere');

      Result := False;

      Owner.UnitsAreValid := False;
   End;

   // The public interface must be "in", "out" or "none"

   If ((CompareStr(PublicInterface, '') <> 0)    And (CompareStr(PublicInterface, 'in') <> 0) And
       (CompareStr(PublicInterface, 'out') <> 0) And (CompareStr(PublicInterface, 'none') <> 0)) Then Begin
      AddMsg(mtError, FileName, Line, 'the public interface must have a value equal to ''in'', ''out'' or ''none''');

      Result := False;
   End;

   // The private interface must be "in", "out" or "none"

   If ((CompareStr(PrivateInterface, '') <> 0)    And (CompareStr(PrivateInterface, 'in') <> 0) And
       (CompareStr(PrivateInterface, 'out') <> 0) And (CompareStr(PrivateInterface, 'none') <> 0)) Then Begin
      AddMsg(mtError, FileName, Line, 'the private interface must have a value equal to ''in'', ''out'' or ''none''');

      Result := False;
   End;

   // Are the public/private interfaces properly used? I.e. "in" cannot be used
   // both for the public AND the private interfaces

   If (Result And (CompareStr(PublicInterface, 'in') = 0) And (CompareStr(PrivateInterface, 'in') = 0)) Then Begin
      AddMsg(mtError, FileName, Line, 'the public and private interfaces cannot both have a value equal to ''in''');

      Result := False;
   End;

   // Valid initial value (if any)?

   If (CompareStr(InitialValue, '') <> 0) Then Begin
      If (TryStrToFloat(InitialValue, Val)) Then Begin
         // The initial value itself is valid, but must check that the
         // public/private interfaces are not of type "in"...

         If (CompareStr(PublicInterface, 'in') = 0) Then Begin
            AddMsg(mtError, FileName, Line, 'an initial value cannot be defined when the public interface has a value equal to ''in''');

            Result := False;
         End Else If (CompareStr(PrivateInterface, 'in') = 0) Then Begin
            AddMsg(mtError, FileName, Line, 'an initial value cannot be defined when the private interface has a value equal to ''in''');

            Result := False;
         End;
      End Else Begin
         AddMsg(mtError, FileName, Line, 'the initial value must be a real number');

         Result := False;
      End;
   End;

   // Is the variable part of a connection (in case it has a public/private
   // interface of "in")?

   If ((CompareStr(PublicInterface, 'in') = 0) Or (CompareStr(PrivateInterface, 'in') = 0)) Then Begin
      Mapped := False;   // By default...

      For Iter := 0 To FOwner.Owner.ConnectionList.Size-1 Do Begin
         Connection := TCellMLConnection(FOwner.Owner.ConnectionList.At(Iter).VObject);

         MatchComponent1 := CompareStr(Connection.Component1, FOwner.Name) = 0;
         MatchComponent2 := CompareStr(Connection.Component2, FOwner.Name) = 0;

         If (MatchComponent1 Or MatchComponent2) Then Begin
            // Found the component we are interested in the current connection, so
            // check whether one of the mapped variables is the one we are
            // interested in and, if so, exit, since it would mean that the
            // variable is mapped...

            // Note: at this stage, we don't care whether it is properly mapped or
            //       not, since this is done when validating the connections...

            For Iter2 := 0 To Connection.MapVariablesList.Size-1 Do Begin
               MapVariables := TCellMLMapVariables(Connection.MapVariablesList.At(Iter2).VObject);

               If ((MatchComponent1 And (CompareStr(MapVariables.Variable1, Name) = 0)) Or
                   (MatchComponent2 And (CompareStr(MapVariables.Variable2, Name) = 0))) Then Begin
                  Mapped := True;

                  Break;
               End;
            End;
         End;

         If (Mapped) Then
            Break;
      End;

      If (Not Mapped) Then Begin
         AddMsg(mtError, FileName, Line, 'the public/private interface attribute being equal to ''in'', the variable must be mapped');

         Result := False;
      End;
   End;
End;

//==============================================================================

Procedure TMathMLCommandBinTree.CommonCreate;
Begin
   Left  := Nil;
   Right := Nil;

   PossibleUnits := TCellMLSortedList.Create;
End;

//==============================================================================

Constructor TMathMLCommandBinTree.Create(Const aFileName: String;
                                         Const aItemType: TMathMLCommandBinTreeItemType;
                                         Const aStr: String);
Begin
   Inherited Create(aFileName);

   ItemType := aItemType;

   If (CompareStr(aStr, '') <> 0) Then
      Str := aStr
   Else
      Case aItemType Of
         mitEq:           Str := ' = ';
         mitEqEq:         Str := ' == ';
         mitNEq:          Str := ' <> ';
         mitGT:           Str := ' > ';
         mitLT:           Str := ' < ';
         mitGEq:          Str := ' >= ';
         mitLEq:          Str := ' <= ';
         mitPlus:         Str := '+';
         mitMinus:        Str := '-';
         mitTimes:        Str := '*';
         mitDivide:       Str := '/';
         mitPow:          Str := 'pow';
         mitRoot:         Str := 'root';
         mitAbs:          Str := 'abs';
         mitExp:          Str := 'exp';
         mitLN:           Str := 'ln';
         mitLog:          Str := 'log';
         mitCeil:         Str := 'ceil';
         mitFloor:        Str := 'floor';
         mitFact:         Str := 'fact';
         mitAnd:          Str := ' and ';
         mitOr:           Str := ' or ';
         mitXOr:          Str := ' xor ';
         mitNot:          Str := 'not';
         mitSin:          Str := 'sin';
         mitCos:          Str := 'cos';
         mitTan:          Str := 'tan';
         mitSec:          Str := 'sec';
         mitCsc:          Str := 'csc';
         mitCot:          Str := 'cot';
         mitSinH:         Str := 'sinh';
         mitCosH:         Str := 'cosh';
         mitTanH:         Str := 'tanh';
         mitSecH:         Str := 'sech';
         mitCscH:         Str := 'csch';
         mitCotH:         Str := 'coth';
         mitASin:         Str := 'asin';
         mitACos:         Str := 'acos';
         mitATan:         Str := 'atan';
         mitASec:         Str := 'asec';
         mitACsc:         Str := 'acsc';
         mitACot:         Str := 'acot';
         mitASinH:        Str := 'asinh';
         mitACosH:        Str := 'acosh';
         mitATanH:        Str := 'atanh';
         mitASecH:        Str := 'asech';
         mitACscH:        Str := 'acsch';
         mitACotH:        Str := 'acoth';
         mitTrue:         Str := 'true';
         mitFalse:        Str := 'false';
         mitPI:           Str := 'pi';
         mitExponentiale: Str := 'e';
      End;

   CommonCreate;
End;

//==============================================================================

Constructor TMathMLCommandBinTree.Create(Const aFileName: String;
                                         Const aStr: String);
Begin
   Inherited Create(aFileName);

   ItemType := mitCI;
   Str      := aStr;

   CommonCreate;
End;

//==============================================================================

Constructor TMathMLCommandBinTree.Create(Const aFileName: String;
                                         Const aNb: String;
                                         Const aUnits: String);
Begin
   Inherited Create(aFileName);

   ItemType := mitCN;
   Str      := aNb;
   Units    := aUnits;

   CommonCreate;
End;

//==============================================================================

Destructor TMathMLCommandBinTree.Destroy;
Begin
   Left.Free;
   Right.Free;

   PossibleUnits.Free;
End;

//==============================================================================

Function TMathMLCommandBinTree.IsValid: Boolean;
Begin
   Result := True;   // Always true...
                     // Note: we NEVER come here, but the parent class declares
                     //       this function as abstract, so...
End;

//==============================================================================

Procedure TMathMLCommandBinTree.AddPossibleUnits(Const aCellMLUnits: TCellMLUnits);
Begin
   If (AtEnd(PossibleUnits.Find(aCellMLUnits.Name))) Then
      // The units has not yet been registered, so...

      PossibleUnits.Add([aCellMLUnits])
   Else
      // The units has already been registered, so...

      aCellMLUnits.Free;
End;

//==============================================================================

Procedure TMathMLCommandBinTree.AddPossibleUnits(Const aSetOfUnits: TCellMLSortedList);
Var
   Iter: Integer;
Begin
   For Iter := 0 To aSetOfUnits.Size-1 Do
      AddPossibleUnits(TCellMLUnits.DuplicateUnits(TCellMLUnits(aSetOfUnits.At(Iter).VObject)));
      // Note: we do indeed duplicate the units, since we want "PossibleUnits"
      //       to manage all the units it contains...
End;

//==============================================================================

Constructor TMathMLEquation.Create(Const aOwner: TCellMLComponent);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwner := aOwner;
End;

//==============================================================================

Destructor TMathMLEquation.Destroy;
Begin
   MathMLEquationBinTree.Free;
End;

//==============================================================================

Function TMathMLEquation.IsValid: Boolean;
   Function IsNodeValid(Const aMathMLEquationBinTree: TMathMLCommandBinTree): Boolean;
      Function IsCrtNodeValid(aMathMLEquationBinTree: TMathMLCommandBinTree): Boolean;
      Var
         Variable: TCellMLVariable;
         VarOrDeriv: String;
         Found: Boolean;
         Iter: Integer;
      Begin
         Result := True;

         Case aMathMLEquationBinTree.ItemType Of
            mitCI: Begin
               // Check that the variable is defined within the current
               // component

               Variable := FOwner.FindVariable(aMathMLEquationBinTree.Str);

               If (Variable = Nil) Then Begin
                  AddMsg(mtError, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, ''''+aMathMLEquationBinTree.Str+''' is not defined anywhere');

                  Result := False;
               End Else If (Variable.Owner.NbOfUnits(Variable.Units) = 0) Then
                  // Note: the variable's units doesn't exist, but that problem
                  //       will have already been reported in the declaration of
                  //       the variable, so no need to report it again, so...

                  Result := False
               Else
                  // Keep track of the variable's units information (useful for
                  // units conversion)

                  aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(Variable.Owner.FindUnit(Variable.Units)));
            End;
            mitCN: Begin
               // Check that a unit has been defined

               If (CompareStr(aMathMLEquationBinTree.Units, '') = 0) Then Begin
                  AddMsg(mtError, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, ''''+aMathMLEquationBinTree.Str+''' does not have a unit');

                  Result := False;
               End Else If (FOwner.NbOfUnits(aMathMLEquationBinTree.Units) = 0) Then Begin
                  AddMsg(mtError, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, ''''+aMathMLEquationBinTree.Units+''' is not defined anywhere');

                  Result := False;
               End Else
                  // Keep track of the constant's units information (useful for
                  // units conversion)

                  aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(FOwner.FindUnit(aMathMLEquationBinTree.Units)));
            End;
            mitEq: Begin
               // The left hand side of the equation must be a variable defined
               // in the component and which public/private interface is set to
               // "in"

               If ((aMathMLEquationBinTree.Left.ItemType <> mitCI) And
                   (aMathMLEquationBinTree.Left.ItemType <> mitDiff)) Then Begin
                  AddMsg(mtError, aMathMLEquationBinTree.Left.FileName, aMathMLEquationBinTree.Left.Line, 'the left hand side of an equation must be a variable or the derivative of a variable');

                  Result := False;
               End Else Begin
                  // Either a variable or a derivative of a variable, so just
                  // check that it is one that can be modified

                  If (aMathMLEquationBinTree.Left.ItemType = mitCI) Then Begin
                     // Variable

                     FileName := aMathMLEquationBinTree.Left.FileName;
                     Line     := aMathMLEquationBinTree.Left.Line;

                     VarOrDeriv := aMathMLEquationBinTree.Left.Str;
                  End Else Begin
                     // Derivative of a variable

                     FileName := aMathMLEquationBinTree.Left.Right.FileName;
                     Line     := aMathMLEquationBinTree.Left.Right.Line;

                     VarOrDeriv := aMathMLEquationBinTree.Left.Right.Str;

                     // Check that the free variable needs to be initialised or
                     // is of the right name
                     // Note: this is a specific rule for COR to make sure that
                     //       there is only ONE free variable

                     If (CompareStr(Owner.Owner.FreeVariable, '') = 0) Then
                        Owner.Owner.FreeVariable := aMathMLEquationBinTree.Left.Left.Str
                     Else If (CompareStr(aMathMLEquationBinTree.Left.Left.Str, Owner.Owner.FreeVariable) <> 0) Then Begin
                        AddMsg(mtError, FileName, Line, 'only one free variable ('''+Owner.Owner.FreeVariable+''' and not '''+aMathMLEquationBinTree.Left.Left.Str+''') can be used to derive a variable.');

                        Result := False;

                        Exit;
                     End;
                  End;

                  Found := False;

                  For Iter := 0 To FOwner.VariableList.Size-1 Do
                     With TCellMLVariable(FOwner.VariableList.At(Iter).VObject) Do
                        If (CompareStr(Name, VarOrDeriv) = 0) Then Begin
                           Found := (CompareStr(PublicInterface, 'in') <> 0) And (CompareStr(PrivateInterface, 'in') <> 0);

                           Break;
                        End;

                  If (Not Found) Then Begin
                     AddMsg(mtError, FileName, Line, ''''+VarOrDeriv+''' does not belong to the current component and therefore cannot be on the left hand side of an equation');

                     Result := False;
                  End;
               End;
            End;
         End;
      End;
   Begin
      // Note: notice the order of the "And" tests. They are very important!
      //       That is because of the compiler's settings that are such that if
      //       we want to evaluate "A And B" with "A" being "False", then "B"
      //       won't be evaluated, since there is no need for it. Here, we want
      //       "IsNodeValid" and "IsCrtNodeValid" to be evaluated, no matter the
      //       value of "Result", so...

      Result := True;

      // Check the validity of the left branch, if any...

      If (aMathMLEquationBinTree.Left <> Nil) Then
         Result := IsNodeValid(aMathMLEquationBinTree.Left) And Result;

      // Check the validity of the right branch, if any...

      If (aMathMLEquationBinTree.Right <> Nil) Then
         Result := IsNodeValid(aMathMLEquationBinTree.Right) And Result;

      // Check the validity of the current node

      Result := IsCrtNodeValid(aMathMLEquationBinTree) And Result;
   End;
   Procedure CheckUnits(aMathMLEquationBinTree: TMathMLCommandBinTree;
                        Var aPiecewiseStatement: Boolean);
      Const
         UNITS_MESSAGE = 'there is one or several problems with the units used in this equation';
         UNITS_DIMENSIONAL_EQUIVALENCE_MESSAGE = 'some units used in this equation are not exactly equivalent, but dimensionally equivalent and might therefore have to be converted';
      Procedure CheckUnitsForCrtNode(aMathMLEquationBinTree: TMathMLCommandBinTree;
                                     Var aPiecewiseStatement: Boolean);
         Procedure CheckEquivalenceType(Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                        Const aLHS: Boolean = False;
                                        Const aBoolean: Boolean = False);
         Var
            AtLeastOneNonEquivalence: Boolean;
            AtLeastOneDimensionalEquivalence: Boolean;
            LHSUnits: TCellMLUnits;
            Iter: Integer;
         Begin
            // Note: all the possible units of the LHS are dimensionally
            //       equivalent (otherwise, we wouldn't have reached this point)
            //       and the same is true for the RHS, as long as it is not a
            //       piecewise statement, so we only need to check that the
            //       first possible units of the LHS and RHS are indeed
            //       dimensionally equivalent. In the case of a piecewise
            //       statement, we need to check that the first possible units
            //       of the LHS is indeed dimensioanlly equivalent to all the
            //       possible units of the and RHS. Indeed, the RHS of a
            //       piecewise should be considered as several possible RHS, but
            //       because of our data structure (which we want to keep as
            //       simple as possible) we don't know which possible units
            //       belong to which sub-RHS, so we are left with no choice (if
            //       we want to keep our data structure and, as a consequence,
            //       our algorithm as simple as possible), but to test against
            //       all possible units of the RHS...

            Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
            Assert(aMathMLEquationBinTree.Right.PossibleUnits.Size <> 0);
            // Note: both sets of possible units should contain at least one
            //       units definition, so if it is not the case it means that
            //       there is something fundamentally wrong with our code...

            AtLeastOneNonEquivalence := False;
            AtLeastOneDimensionalEquivalence := False;

            If (aPiecewiseStatement) Then Begin
               LHSUnits := TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start));

               For Iter := 0 To aMathMLEquationBinTree.Right.PossibleUnits.Size-1 Do
                  Case LHSUnits.EquivalenceType(TCellMLUnits(aMathMLEquationBinTree.Right.PossibleUnits.At(Iter).VObject)) Of
                     etNone: Begin
                        AtLeastOneNonEquivalence := True;

                        Break;   // No need to carry on, so...
                     End;
                     etDimensional:
                        AtLeastOneDimensionalEquivalence := True;
                        // Note: don't "break", because we could get worse, i.e.
                        //       no equivalence at all, so...
                  End;
            End Else
               Case TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start)).EquivalenceType(TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Right.PossibleUnits.Start))) Of
                  etNone:
                     AtLeastOneNonEquivalence := True;
                  etDimensional:
                     AtLeastOneDimensionalEquivalence := True;
               End;

            If (AtLeastOneNonEquivalence) Then 
               AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE)
            Else If (AtLeastOneDimensionalEquivalence) Then
               AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_DIMENSIONAL_EQUIVALENCE_MESSAGE);

            If (aLHS) Then
               // The outcome is that of the LHS, so...

               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits)
            Else If (aBoolean) Then
               // The outcome is to be a boolean, so...

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(BooleanUnit))
            Else Begin
               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);
               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Right.PossibleUnits);
            End;
         End;
         Procedure PowerOrRootFunction(Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                       Const aPower: Boolean);
            Function RetrieveExponent(Const aMathMLEquationBinTree: TMathMLCommandBinTree): Double;
               Function CheckExponentOperands(Const aMathMLEquationBinTree: TMathMLCommandBinTree;
                                              Var aExponent: Double): Boolean;
{$IFNDEF OPT_MATH}
                  Function Fact(aNb: Double): Double;
                  Begin
                     Result := 1;

                     While (aNb > 1) Do Begin
                        Result := aNb*Result;

                        aNb := aNb-1;
                     End;
                  End;
{$ENDIF}
               Var
                  LookedupVar: TCellMLModelVariable;
                  LeftCst, RightCst: Double;
                  HasRightCst: Boolean;
               Begin
                  Result := True;   // Default...

                  If (aMathMLEquationBinTree.Left <> Nil) Then
                     Result := CheckExponentOperands(aMathMLEquationBinTree.Left, LeftCst);

                  If (Result) Then Begin
                     If (aMathMLEquationBinTree.Right <> Nil) Then Begin
                        Result := CheckExponentOperands(aMathMLEquationBinTree.Right, RightCst);

                        HasRightCst := True;
                     End Else
                        HasRightCst := False;

                     If (Result) Then
                        Case aMathMLEquationBinTree.ItemType Of
                           mitCI: Begin
                              // A variable, so it is fine only if it is a
                              // constant (be it defined in the current
                              // component or mapped)

                              LookedupVar := Owner.Owner.GlobalVariable(Owner.Name, aMathMLEquationBinTree.Str);

                              If (LookedupVar.State = vsMapped) Then
                                 // A mapped variable, so...

                                 LookedupVar := Owner.Owner.MapVariable(LookedupVar.Component, LookedupVar.Variable, LookedupVar.Component, LookedupVar.Variable);

                              Result := LookedupVar.State = vsConstant;

                              If (Result) Then
                                 aExponent := StrToFloat(LookedupVar.InitialValue);
                           End;
                           mitCN:
                              // A number, so it is fine

                              aExponent := StrToFloat(aMathMLEquationBinTree.Str);
                           mitEqEq:
                              If (LeftCst = RightCst) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitNEq:
                              If (LeftCst <> RightCst) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitLT:
                              If (LeftCst < RightCst) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitGT:
                              If (LeftCst > RightCst) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitLEq:
                              If (LeftCst <= RightCst) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitGEq:
                              If (LeftCst >= RightCst) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitPlus:
                              If (HasRightCst) Then
                                 aExponent := LeftCst+RightCst
                              Else
                                 aExponent := LeftCst;
                           mitMinus:
                              If (HasRightCst) Then
                                 aExponent := LeftCst-RightCst
                              Else
                                 aExponent := -LeftCst;
                           mitTimes:
                              aExponent := LeftCst*RightCst;
                           mitDivide:
                              aExponent := LeftCst/RightCst;
                           mitPow:
                              If (RightCst = 2) Then
                                 aExponent := LeftCst*LeftCst
                              Else
{$IFDEF OPT_MATH}
                                 aExponent := OptPower(LeftCst, RightCst);
{$ELSE}
                                 aExponent := Power(LeftCst, RightCst);
{$ENDIF}
                           mitRoot:
                              If (HasRightCst And (LeftCst <> 2)) Then
{$IFDEF OPT_MATH}
                                 aExponent := OptPower(RightCst, 1/LeftCst)
{$ELSE}
                                 aExponent := Power(RightCst, 1/LeftCst)
{$ENDIF}
                              Else
                                 aExponent := Sqrt(LeftCst);
                           mitAbs:
                              aExponent := Abs(LeftCst);
                           mitExp:
{$IFDEF OPT_MATH}
                              aExponent := OptExp(LeftCst);
{$ELSE}
                              aExponent := Exp(LeftCst);
{$ENDIF}
                           mitLN:
{$IFDEF OPT_MATH}
                              aExponent := OptLN(LeftCst);
{$ELSE}
                              aExponent := LN(LeftCst);
{$ENDIF}
                           mitLog:
                              If (HasRightCst) Then
{$IFDEF OPT_MATH}
                                 aExponent := OptLN(RightCst)/OptLN(LeftCst)
{$ELSE}
                                 aExponent := LN(RightCst)/LN(LeftCst)
{$ENDIF}
                              Else
{$IFDEF OPT_MATH}
                                 aExponent := OptLog10(LeftCst);
{$ELSE}
                                 aExponent := Log10(LeftCst);
{$ENDIF}
                           mitCeil:
{$IFDEF OPT_MATH}
                              aExponent := OptCeil(LeftCst);
{$ELSE}
                              aExponent := Ceil(LeftCst);
{$ENDIF}
                           mitFloor:
{$IFDEF OPT_MATH}
                              aExponent := OptFloor(LeftCst);
{$ELSE}
                              aExponent := Floor(LeftCst);
{$ENDIF}
                           mitFact:
{$IFDEF OPT_MATH}
                              aExponent := OptFact(LeftCst);
{$ELSE}
                              aExponent := Fact(LeftCst);
{$ENDIF}
                           mitAnd:
                              If ((LeftCst <> 0) And (RightCst <> 0)) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitOr:
                              If ((LeftCst <> 0) Or (RightCst <> 0)) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitXOr:
                              If ((LeftCst <> 0) XOr (RightCst <> 0)) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitNot:
                              If (LeftCst = 0) Then
                                 aExponent := 1
                              Else
                                 aExponent := 0;
                           mitDegree,
                           mitLogBase:
                              aExponent := LeftCst;
                           mitSin:
{$IFDEF OPT_MATH}
                              aExponent := OptSin(LeftCst);
{$ELSE}
                              aExponent := System.Sin(LeftCst);
{$ENDIF}
                           mitCos:
{$IFDEF OPT_MATH}
                              aExponent := OptCos(LeftCst);
{$ELSE}
                              aExponent := Cos(LeftCst);
{$ENDIF}
                           mitTan:
{$IFDEF OPT_MATH}
                              aExponent := OptTan(LeftCst);
{$ELSE}
                              aExponent := Tan(LeftCst);
{$ENDIF}
                           mitSec:
{$IFDEF OPT_MATH}
                              aExponent := OptSec(LeftCst);
{$ELSE}
                              aExponent := Sec(LeftCst);
{$ENDIF}
                           mitCsc:
{$IFDEF OPT_MATH}
                              aExponent := OptCsc(LeftCst);
{$ELSE}
                              aExponent := Csc(LeftCst);
{$ENDIF}
                           mitCot:
{$IFDEF OPT_MATH}
                              aExponent := OptCot(LeftCst);
{$ELSE}
                              aExponent := Cot(LeftCst);
{$ENDIF}
                           mitSinH:
{$IFDEF OPT_MATH}
                              aExponent := OptSinH(LeftCst);
{$ELSE}
                              aExponent := SinH(LeftCst);
{$ENDIF}
                           mitCosH:
{$IFDEF OPT_MATH}
                              aExponent := OptCosH(LeftCst);
{$ELSE}
                              aExponent := CosH(LeftCst);
{$ENDIF}
                           mitTanH:
{$IFDEF OPT_MATH}
                              aExponent := OptTanH(LeftCst);
{$ELSE}
                              aExponent := TanH(LeftCst);
{$ENDIF}
                           mitSecH:
{$IFDEF OPT_MATH}
                              aExponent := OptSecH(LeftCst);
{$ELSE}
                              aExponent := SecH(LeftCst);
{$ENDIF}
                           mitCscH:
{$IFDEF OPT_MATH}
                              aExponent := OptCsCH(LeftCst);
{$ELSE}
                              aExponent := CsCH(LeftCst);
{$ENDIF}
                           mitCotH:
{$IFDEF OPT_MATH}
                              aExponent := OptCotH(LeftCst);
{$ELSE}
                              aExponent := CotH(LeftCst);
{$ENDIF}
                           mitASin:
{$IFDEF OPT_MATH}
                              aExponent := OptArcSin(LeftCst);
{$ELSE}
                              aExponent := ArcSin(LeftCst);
{$ENDIF}
                           mitACos:
{$IFDEF OPT_MATH}
                              aExponent := OptArcCos(LeftCst);
{$ELSE}
                              aExponent := ArcCos(LeftCst);
{$ENDIF}
                           mitATan:
{$IFDEF OPT_MATH}
                              aExponent := OptArcTan(LeftCst);
{$ELSE}
                              aExponent := ArcTan(LeftCst);
{$ENDIF}
                           mitASec:
{$IFDEF OPT_MATH}
                              aExponent := OptArcSec(LeftCst);
{$ELSE}
                              aExponent := ArcSec(LeftCst);
{$ENDIF}
                           mitACsc:
{$IFDEF OPT_MATH}
                              aExponent := OptArcCsc(LeftCst);
{$ELSE}
                              aExponent := ArcCsc(LeftCst);
{$ENDIF}
                           mitACot:
{$IFDEF OPT_MATH}
                              aExponent := OptArcCot(LeftCst);
{$ELSE}
                              aExponent := ArcCot(LeftCst);
{$ENDIF}
                           mitASinH:
{$IFDEF OPT_MATH}
                              aExponent := OptArcSinH(LeftCst);
{$ELSE}
                              aExponent := ArcSinH(LeftCst);
{$ENDIF}
                           mitACosH:
{$IFDEF OPT_MATH}
                              aExponent := OptArcCosH(LeftCst);
{$ELSE}
                              aExponent := ArcCosH(LeftCst);
{$ENDIF}
                           mitATanH:
{$IFDEF OPT_MATH}
                              aExponent := OptArcTanH(LeftCst);
{$ELSE}
                              aExponent := ArcTanH(LeftCst);
{$ENDIF}
                           mitASecH:
{$IFDEF OPT_MATH}
                              aExponent := OptArcSecH(LeftCst);
{$ELSE}
                              aExponent := ArcSecH(LeftCst);
{$ENDIF}
                           mitACscH:
{$IFDEF OPT_MATH}
                              aExponent := OptArcCscH(LeftCst);
{$ELSE}
                              aExponent := ArcCscH(LeftCst);
{$ENDIF}
                           mitACotH:
{$IFDEF OPT_MATH}
                              aExponent := OptArcCotH(LeftCst);
{$ELSE}
                              aExponent := ArcCotH(LeftCst);
{$ENDIF}
                           mitTrue:
                              aExponent := 1;
                           mitFalse:
                              aExponent := 0;
                           mitPI:
                              aExponent := PI;
                           mitExponentiale:
{$IFDEF OPT_MATH}
                              aExponent := OptExp(1);
{$ELSE}
                              aExponent := Exp(1);
{$ENDIF}
                        End;
                  End;
               End;
            Begin
               // The exponent can be simple (i.e. a constant viariable (be it
               // defined in the current component or mapped) or a number) or
               // complex (e.g. 3+5). It may happen that a complex exponent may
               // consist of non-constant operands and yet yield a constant
               // result, but it is impossible to be 100% certain, hence we
               // only accept operands of constant type

               If (Not CheckExponentOperands(aMathMLEquationBinTree, Result)) Then
                  AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, 'the equation uses an exponent which value is not or may not be constant');
            End;
            Procedure CheckExponentForNaNAndInfinite(Var aExponent: Double);
            Begin
               If (IsNaN(aExponent) Or IsInfinite(aExponent)) Then Begin
                  AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, 'the equation uses a root function with an invalid exponent value');

                  aExponent := 1;   // Reset it to something that we know will
                                    // work
               End;
            End;
         Var
            ExponentExists: Boolean;
            CrtMathMLEquationBinTree: TMathMLCommandBinTree;
            Exponent: Double;
            Iter: Integer;
         Begin
            Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
            // Note: the left set of possible units should contain at least one
            //       units definition, so if it is not the case it means that
            //       there is something fundamentally wrong with our code...

            Exponent := 1;   // Just a value that "works" in case we cannot
                             // retrieve the exponent

            If (aPower) Then Begin
               Assert(aMathMLEquationBinTree.Right.PossibleUnits.Size <> 0);
               // Note: the right set of possible units should contain at least
               //       one units definition, so if it is not the case it means
               //       that there is something fundamentally wrong with our
               //       code...

               ExponentExists := True;
            End Else Begin
               Assert((aMathMLEquationBinTree.Right = Nil) Or
                      ((aMathMLEquationBinTree.Right <> Nil) And (aMathMLEquationBinTree.Right.PossibleUnits.Size <> 0)));
               // Note: the right set of possible units may contain at least one
               //       units definition, so if it is not the case it means that
               //       there is something fundamentally wrong with our code...

               ExponentExists := aMathMLEquationBinTree.Right <> Nil;
            End;

            If (ExponentExists) Then Begin
               // Extract the exponent

               If (aPower) Then
                  // Exponent for a power function, so it is located in the
                  // right branch of the tree

                  CrtMathMLEquationBinTree := aMathMLEquationBinTree.Right
               Else
                  // Exponent for a root function, so it is located in the left
                  // branch of the left branch of the tree

                  CrtMathMLEquationBinTree := aMathMLEquationBinTree.Left.Left;

               // The exponent must be of a dimensionless type, unless A in A^B
               // is also dimensionless, in which case it doesn't matter of what
               // type the exponent is

               If ((TCellMLUnits(DeCAL.GetObject(CrtMathMLEquationBinTree.PossibleUnits.Start)).EquivalenceType(DimensionlessUnit) <> etExact) And
                   (TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start)).EquivalenceType(DimensionlessUnit) <> etExact)) Then
                     AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE);

               // Determine the value of the exponent

               Exponent := RetrieveExponent(CrtMathMLEquationBinTree);

               CheckExponentForNaNAndInfinite(Exponent);

               If (Not aPower) Then Begin
                  If (Exponent <> 0) Then
                     Exponent := 1/Exponent
                  Else
                     AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, 'the equation uses a root function with an exponent value of zero');
               End;

               CheckExponentForNaNAndInfinite(Exponent);
            End Else
               // The exponent doesn't exist, so we are dealing with a square
               // root, hence...

               Exponent := 0.5;

            // Exponentiate any possible units definition

            If (aPower) Then
               // We are dealing with X to the power of something, so X is
               // located in the left branch of the tree

               CrtMathMLEquationBinTree := aMathMLEquationBinTree.Left
            Else Begin
               // We are dealing with X to the root of something, so X is either
               // located in the left or right branch of the tree, depending on
               // whether an exponent is available or not

               If (ExponentExists) Then
                  CrtMathMLEquationBinTree := aMathMLEquationBinTree.Right
               Else
                  CrtMathMLEquationBinTree := aMathMLEquationBinTree.Left;
            End;

            For Iter := 0 To CrtMathMLEquationBinTree.PossibleUnits.Size-1 Do
               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits(CrtMathMLEquationBinTree.PossibleUnits.At(Iter).VObject).Exponentiates(Exponent));
         End;
      Var
         Iter1, Iter2: Integer;
         TempUnits: TCellMLUnits;
      Begin
         Case aMathMLEquationBinTree.ItemType Of
            mitEq:
               // In: LHS and RHS must have dimensionally equivalent units
               // Out: units of the LHS

               CheckEquivalenceType(aMathMLEquationBinTree, True);
            mitPlus, mitMinus:
               // In: LHS and RHS must have dimensionally equivalent units,
               //     assuming that there is a RHS, otherwise we accept any kind
               //     of units
               // Out: units of the LHS (or RHS)

               If (aMathMLEquationBinTree.Right <> Nil) Then
                  CheckEquivalenceType(aMathMLEquationBinTree)
               Else
                  aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);
            mitTimes:
               // In: LHS and RHS can have any units
               // Out: product of the LHS and RHS units

               For Iter1 := 0 To aMathMLEquationBinTree.Left.PossibleUnits.Size-1 Do
                  For Iter2 := 0 To aMathMLEquationBinTree.Right.PossibleUnits.Size-1 Do
                     aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits(aMathMLEquationBinTree.Left.PossibleUnits.At(Iter1).VObject).Times(TCellMLUnits(aMathMLEquationBinTree.Right.PossibleUnits.At(Iter2).VObject)));
            mitDivide:
               // In: LHS and RHS can have any units
               // Out: product of the LHS and RHS units, with the RHS units
               //      exponentiated to -1

               For Iter1 := 0 To aMathMLEquationBinTree.Left.PossibleUnits.Size-1 Do
                  For Iter2 := 0 To aMathMLEquationBinTree.Right.PossibleUnits.Size-1 Do Begin
                     TempUnits := TCellMLUnits(aMathMLEquationBinTree.Right.PossibleUnits.At(Iter2).VObject).Exponentiates(-1);

                     aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits(aMathMLEquationBinTree.Left.PossibleUnits.At(Iter1).VObject).Times(TempUnits));

                     TempUnits.Free;
                  End;
            mitAnd, mitOr, mitXOr: Begin
               // In: LHS and RHS must be of boolean type
               // Out: boolean type

               Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
               Assert(aMathMLEquationBinTree.Right.PossibleUnits.Size <> 0);
               // Note: both sets of possible units should contain at least one
               //       units definition, so if it is not the case it means that
               //       there is something fundamentally wrong with our code...

               If ((TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start)).Name <> _BOOLEAN_) Or
                   (TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Right.PossibleUnits.Start)).Name <> _BOOLEAN_)) Then
                  AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE);

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(BooleanUnit));
            End;
            mitNot: Begin
               // In: LHS must be a boolean
               // Out: boolean

               Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
               // Note: the left set of possible units should contain at least
               //       one units definition, so if it is not the case it means
               //       that there is something fundamentally wrong with our
               //       code...

               If (TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start)).Name <> _BOOLEAN_) Then
                  AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE);

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(BooleanUnit));
            End;
            mitEqEq, mitNEq, mitLT, mitGT, mitLEq, mitGEq:
               // In: LHS and RHS must have dimensionally equivalent units
               // Out: boolean

               CheckEquivalenceType(aMathMLEquationBinTree, False, True);
            mitAbs, mitCeil, mitFloor: Begin
               // In: LHS can have any units
               // Out: units of the LHS

               Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
               // Note: the left set of possible units should contain at least
               //       one units definition, so if it is not the case it means
               //       that there is something fundamentally wrong with our
               //       code...

               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);
            End;
            mitExp, mitLN, mitFact,
            mitSin, mitCos, mitTan, mitSec, mitCsc, mitCot,
            mitSinH, mitCosH, mitTanH, mitSecH, mitCscH, mitCotH,
            mitASin, mitACos, mitATan, mitASec, mitACsc, mitACot,
            mitASinH, mitACosH, mitATanH, mitASecH, mitACscH, mitACotH: Begin
               // In: LHS must be dimensionless
               // Out: dimensionless

               Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
               // Note: the left set of possible units should contain at least
               //       one units definition, so if it is not the case it means
               //       that there is something fundamentally wrong with our
               //       code...

               If (TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start)).EquivalenceType(DimensionlessUnit) <> etExact) Then
                  AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE);

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(DimensionlessUnit));
            End;
            mitLog: Begin
               // In: LHS (and RHS) must be dimensionless
               // Out: dimensionless

               Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
               // Note: the left set of possible units should contain at least
               //       one units definition, so if it is not the case it means
               //       that there is something fundamentally wrong with our
               //       code...

               If (TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start)).EquivalenceType(DimensionlessUnit) <> etExact) Then
                  AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE)
               Else If (aMathMLEquationBinTree.Left.ItemType = mitLogBase) Then Begin
                  // Logarithm of a particular base, so also check the RHS

                  Assert(aMathMLEquationBinTree.Right.PossibleUnits.Size <> 0);
                  // Note: the right set of possible units should contain at
                  //       least one units definition, so if it is not the case
                  //       it means that there is something fundamentally wrong
                  //       with our code...

                  If (TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Right.PossibleUnits.Start)).EquivalenceType(DimensionlessUnit) <> etExact) Then
                     AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE);
               End;

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(DimensionlessUnit));
            End;
            mitLogBase:
               // In: LHS can have any units
               // Out: units of LHS

               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);
            mitPow:
               // In: LHS can have any units, while RHS must be dimensionless
               // Out: units of LHS to the power of RHS

               PowerOrRootFunction(aMathMLEquationBinTree, True);
            mitRoot:
               // In: LHS can have any units, while RHS must be dimensionless
               // Out: units of LHS to the power of the inverse of RHS

               PowerOrRootFunction(aMathMLEquationBinTree, False);
            mitDegree:
               // In: LHS can have any units
               // Out: units of LHS

               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);
            mitDiff: Begin
               // In: LHS and RHS can have any units
               // Out: product of the LHS and RHS units, with the RHS units
               //      exponentiated to -1
               // Note: we only deal with first order differential equations...

               TempUnits := TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Left.PossibleUnits.Start)).Exponentiates(-1);

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Right.PossibleUnits.Start)).Times(TempUnits));

               TempUnits.Free;
            End;
            mitPiecewise: Begin
               // In: LHS (and RHS) can have any units
               // Out: units of LHS (and RHS)

               Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
               // Note: the left set of possible units should contain at least
               //       one units definition, so if it is not the case it means
               //       that there is something fundamentally wrong with our
               //       code...

               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);

               If (aMathMLEquationBinTree.Right <> Nil) Then Begin
                  Assert(aMathMLEquationBinTree.Right.PossibleUnits.Size <> 0);
                  // Note: the right set of possible units should contain at
                  //       least one units definition, so if it is not the case
                  //       it means that there is something fundamentally wrong
                  //       with our code...

                  aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Right.PossibleUnits);
               End;

               aPiecewiseStatement := True;   // Keep track of the fact that the
                                              // current equation is a piecewise
                                              // statement...
            End;
            mitPiece: Begin
               // In: LHS can have any units, while LHS must be of boolean type
               // Out: units of LHS

               Assert(aMathMLEquationBinTree.Left.PossibleUnits.Size <> 0);
               Assert(aMathMLEquationBinTree.Right.PossibleUnits.Size <> 0);
               // Note: both sets of possible units should contain at least one
               //       units definition, so if it is not the case it means that
               //       there is something fundamentally wrong with our code...

               If (TCellMLUnits(DeCAL.GetObject(aMathMLEquationBinTree.Right.PossibleUnits.Start)).Name <> _BOOLEAN_) Then
                  AddMsg(mtWarning, aMathMLEquationBinTree.FileName, aMathMLEquationBinTree.Line, UNITS_MESSAGE);

               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);
            End;
            mitOtherwise:
               // In: LHS can have any units
               // Out: units of LHS

               aMathMLEquationBinTree.AddPossibleUnits(aMathMLEquationBinTree.Left.PossibleUnits);
            mitTrue, mitFalse:
               // In: boolean type
               // Out: boolean type

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(BooleanUnit));
            mitPI, mitExponentiale:
               // In: dimensionless
               // Out: dimensionless

               aMathMLEquationBinTree.AddPossibleUnits(TCellMLUnits.DuplicateUnits(DimensionlessUnit));
         End;
      End;
   Begin
      // Check the validity of the left branch, if any...

      If (aMathMLEquationBinTree.Left <> Nil) Then
         CheckUnits(aMathMLEquationBinTree.Left, aPiecewiseStatement);

      // Check the validity of the right branch, if any...

      If (aMathMLEquationBinTree.Right <> Nil) Then
         CheckUnits(aMathMLEquationBinTree.Right, aPiecewiseStatement);

      // Check the validity of the current node

      If ((aMathMLEquationBinTree.ItemType <> mitCI) And
          (aMathMLEquationBinTree.ItemType <> mitCN)) Then
         CheckUnitsForCrtNode(aMathMLEquationBinTree, aPiecewiseStatement);
   End;
Var
   PiecewiseStatement: Boolean;
Begin
   If (Not FOwner.Owner.DoUnitsChecking) Then
      // Just check the node, not whether the units are consistent

      Result := IsNodeValid(MathMLEquationBinTree)
   Else Begin
      // Check whether the units are consistent

      Result := True;   // Always is in this case

      If (FOwner.UnitsAreValid) Then Begin
         // Note: no need to test for "FOwner.Owner.UnitsAreValid", since we
         //       know it's equal to "True" otherwise we wouldn't be here...

         // The equation is valid and so are the units defined both at the model
         // and component levels, so check units by trying to deal with any
         // required units conversion

         PiecewiseStatement := False;

         CheckUnits(MathMLEquationBinTree, PiecewiseStatement);
      End;
   End;
End;

//==============================================================================

Constructor TCellMLGroup.Create(Const aOwner: TCellMLModel);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwner := aOwner;

   Encapsulation := False;
   Containment   := False;

   ComponentRefList := TCellMLList.Create;
End;

//==============================================================================

Destructor TCellMLGroup.Destroy;
Begin
   ComponentRefList.Free;
End;

//==============================================================================

Function TCellMLGroup.IsValid: Boolean;
Var
   Iter: Integer;
Begin
   Result := True;

   // Allow at most one encapsulation and one containment (of a particular name)
   // throughout the whole model's definition
   //---GRY--- THIS IS AN ADDED RULE TO MAKE THINGS MUCH SIMPLER TO HANDLE

   If (Encapsulation And (FOwner.NbOfEncapsulations <> 1)) Then Begin
      AddMsg(mtError, FileName, Line, 'only one group of ''encapsulation'' type is allowed');

      Result := False;
   End;

   If (Containment And (FOwner.NbOfContainments(Name) <> 1)) Then Begin
      If (CompareStr(Name, '') <> 0) Then
         AddMsg(mtError, FileName, Line, 'only one group of ''containment'' type with the name '''+Name+''' is allowed')
      Else
         AddMsg(mtError, FileName, Line, 'only one group of ''containment'' type is allowed');

      Result := False;
   End;

   // A group element must contain at least one relationship

   If (Not Encapsulation And Not Containment) Then Begin
      AddMsg(mtError, FileName, Line, 'a group must be of an ''encapsulation'' and/or ''containment'' type');

      Result := False;
   End;

   // A group element must contain at least one reference to a component

   If (ComponentRefList.Size = 0) Then Begin
      AddMsg(mtError, FileName, Line, 'a group must refer at least one component');

      Result := False;
   End;

   // A containment's name, if any, must be valid

   If (Containment And (CompareStr(Name, '') <> 0) And Not IsCellMLIdentifier(Name)) Then Begin
      AddMsg(mtError, FileName, Line, ''''+Name+''' is not a valid name for a containment');

      Result := False;
   End;

   // Check the references to the components

   For Iter := 0 To ComponentRefList.Size-1 Do
      If (Not TCellMLComponentRef(ComponentRefList.At(Iter).VObject).IsValid) Then
         Result := False;
End;

//==============================================================================

Function TCellMLGroup.NbOfComponentRefs(Const aComponentRef: String): Integer;
Var
   Iter: Integer;
Begin
   Result := 0;

   // Go through the different component references to find out whether they
   // have instances of the component reference

   For Iter := 0 To ComponentRefList.Size-1 Do
      Inc(Result, TCellMLComponentRef(ComponentRefList.At(Iter).VObject).NbOfComponentRefs(aComponentRef));
End;

//==============================================================================

Constructor TCellMLComponentRef.Create(Const aOwner: TCellMLGroup);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwnerGroup        := aOwner;
   FOwnerComponentRef := Nil;

   ComponentRefList := TCellMLList.Create;
End;

//==============================================================================

Constructor TCellMLComponentRef.Create(Const aOwner: TCellMLComponentRef);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwnerGroup        := Nil;
   FOwnerComponentRef := aOwner;

   ComponentRefList := TCellMLList.Create;
End;

//==============================================================================

Destructor TCellMLComponentRef.Destroy;
Begin
   ComponentRefList.Free;
End;

//==============================================================================

Function TCellMLComponentRef.IsValid: Boolean;
Var
   Iter: Integer;
Begin
   Result := True;

   // A component attribute must be defined

   If (Not IsCellMLIdentifier(Name)) Then Begin
      If (CompareStr(Name, '') = 0) Then
         AddMsg(mtError, FileName, Line, 'the name of a component is required for the reference')
      Else
         AddMsg(mtError, FileName, Line, ''''+Name+''' is not a valid name of component for a reference');

      Result := False;
   End Else Begin
      // The component must exist within the model

      If (((FOwnerGroup <> Nil) And (FOwnerGroup.Owner.NbOfComponents(Name) <> 1)) Or
          ((FOwnerComponentRef <> Nil) And (FOwnerComponentRef.NbOfComponents(Name) <> 1))) Then Begin
         AddMsg(mtError, FileName, Line, ''''+Name+''' is not defined anywhere');

         Result := False;
      End;

      // Each major reference must contain at least one reference to a component

      If ((FOwnerGroup <> Nil) And (ComponentRefList.Size = 0)) Then Begin
         AddMsg(mtError, FileName, Line, ''''+Name+''' is a major component and must therefore include at least one reference to another component');

         Result := False;
      End;

      // A component cannot be referenced more than once

      If (TopOwnerGroup.NbOfComponentRefs(Name) <> 1) Then Begin
         AddMsg(mtError, FileName, Line, ''''+Name+''' cannot be referenced more than once');

         Result := False;
      End;

      // Check the references to the components

      For Iter := 0 To ComponentRefList.Size-1 Do
         If (Not TCellMLComponentRef(ComponentRefList.At(Iter).VObject).IsValid) Then
            Result := False;
   End;
End;

//==============================================================================

Function TCellMLComponentRef.NbOfComponents(Const aComponent: String): Integer;
Begin
   If (FOwnerGroup <> Nil) Then
      Result := FOwnerGroup.Owner.NbOfComponents(aComponent)
   Else
      Result := FOwnerComponentRef.NbOfComponents(aComponent);
End;

//==============================================================================

Function TCellMLComponentRef.NbOfComponentRefs(Const aComponentRef: String): Integer;
Var
   Iter: Integer;
Begin
   Result := 0;

   // Is the component reference defined in the current component reference or
   // one of its children?

   If (CompareStr(Name, aComponentRef) = 0) Then
      Inc(Result);

   For Iter := 0 To ComponentRefList.Size-1 Do
      Inc(Result, TCellMLComponentRef(ComponentRefList.At(Iter).VObject).NbOfComponentRefs(aComponentRef));
End;

//==============================================================================

Function TCellMLComponentRef.GetTopOwnerGroup: TCellMLGroup;
Begin
   If (FOwnerComponentRef <> Nil) Then
      Result := FOwnerComponentRef.TopOwnerGroup
   Else
      Result := FOwnerGroup;
End;

//==============================================================================

Constructor TCellMLConnection.Create(Const aOwner: TCellMLModel);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwner := aOwner;

   MapVariablesList := TCellMLList.Create;
End;

//==============================================================================

Destructor TCellMLConnection.Destroy;
Begin
   MapVariablesList.Free;
End;

//==============================================================================

Function TCellMLConnection.IsValid: Boolean;
Var
   Iter: Integer;
   Found: Boolean;
   Comp1, Comp2: String;
   Parent1: TCellMLComponent;
   EncapsulatedSet1, SiblingSet1: TCellMLSortedList;
Begin
   Result := True;

   // A connection must be between two components

   If ((CompareStr(Component1, '') = 0) Or (CompareStr(Component2, '') = 0)) Then Begin
      AddMsg(mtError, FileName, Line, 'two components are required for a mapping');

      Result := False;
   End Else Begin
      // The first component must exist

      If (FOwner.NbOfComponents(Component1) <> 1) Then Begin
         AddMsg(mtError, FileName, Line, ''''+Component1+''' is not defined anywhere');

         Result := False;
      End;

      // The second component must exist

      If (FOwner.NbOfComponents(Component2) <> 1) Then Begin
         AddMsg(mtError, FileName, Line, ''''+Component2+''' is not defined anywhere');

         Result := False;
      End;

      If (Result) Then Begin
         // The two components cannot be the same

         If (CompareStr(Component1, Component2) = 0) Then Begin
            AddMsg(mtError, FileName, Line, ''''+Component1+''' cannot be mapped to itself');

            Result := False;
         End Else Begin
            // Added rule: only one connection between parent and encapsulated
            // components, encapsulated and parent component, or sibling
            // components is allowed

            // Retrieve the sets of the encapsulated component

            Parent1 := Nil;

            EncapsulatedSet1 := TCellMLSortedList.Create(False);
            SiblingSet1      := TCellMLSortedList.Create(False);

            Owner.EncapsulationSets(Component1, Parent1, EncapsulatedSet1, SiblingSet1);

            If (Not (((Parent1 <> Nil) And (CompareStr(Parent1.Name, Component2) = 0)) Or
                     ((EncapsulatedSet1 <> Nil) And Not AtEnd(EncapsulatedSet1.Find(Component2))) Or
                     ((SiblingSet1 <> Nil) And Not AtEnd(SiblingSet1.Find(Component2))))) Then Begin
               AddMsg(mtError, FileName, Line, 'the encapsulation relationship prevents '''+Component1+''' from being mapped to '''+Component2+'''');

               Result := False;
            End;

            // Free up the encapsulated and sibling sets

            EncapsulatedSet1.Free;
            SiblingSet1.Free;

            // Carry on with the last test

            If (Result) Then Begin
               // Only one unique pair of components can be mapped

               Found := False;

               For Iter := 0 To FOwner.ConnectionList.Size-1 Do Begin
                  With TCellMLConnection(FOwner.ConnectionList.At(Iter).VObject) Do Begin
                     Comp1 := Component1;
                     Comp2 := Component2;
                  End;

                  If (((CompareStr(Comp1, Component1) = 0) And (CompareStr(Comp2, Component2) = 0)) Or
                      ((CompareStr(Comp1, Component2) = 0) And (CompareStr(Comp2, Component1) = 0))) Then Begin
                     If (Found) Then Begin
                        AddMsg(mtError, FileName, Line, 'only one mapping between '''+Component1+''' and '''+Component2+''' is allowed');

                        Result := False;

                        Break;
                     End Else
                        Found := True;
                  End;
               End;
            End;
         End;
      End;
   End;

   // A connection must contain at least one set of variables to be mapped

   If (MapVariablesList.Size = 0) Then Begin
      AddMsg(mtError, FileName, Line, 'at least one set of variables must be mapped');

      Result := False;
   End Else
      // There is at least one set of variables, so check it/them

      For Iter := 0 To MapVariablesList.Size-1 Do
         If (Not TCellMLMapVariables(MapVariablesList.At(Iter).VObject).IsValid) Then
            Result := False;
End;

//==============================================================================

Constructor TCellMLMapVariables.Create(Const aOwner: TCellMLConnection;
                                       Const aVariable1, aVariable2: String);
Begin
   If (aOwner = Nil) Then
      Inherited Create('')
   Else
      Inherited Create(aOwner.FileName);

   FOwner := aOwner;

   FVariable1 := aVariable1;
   FVariable2 := aVariable2;
End;

//==============================================================================

Function TCellMLMapVariables.IsValid: Boolean;
   Function NbOfProperMappings(Const aModel: TCellMLModel;
                               Const aComponent: String;
                               Const aVariable: TCellMLVariable;
                               Const aParent: TCellMLComponent): Integer; Inline;
      Function IsProperlyMapped(Const aVariable1: TCellMLVariable;
                                Const aParent1: TCellMLComponent;
                                Const aComponent2, aVariable2: String): Boolean; Inline;
      Var
         Comp2: TCellMLComponent;
         Var2: TCellMLVariable;
         Res2: Boolean;
      Begin
         Result := False;

         // We consider the two variables as being mapped once if the first
         // variable has a public/private interface of "in" and the second
         // variable has one of "out". The tricky part is when the two variables
         // define both a public and a private interface. In this particular
         // case, we have to check the hierarchy between the two variables...

         Comp2 := aVariable1.Owner.Owner.FindComponent(aComponent2);

         If (Comp2 <> Nil) Then
            Var2 := Comp2.FindVariable(aVariable2)
         Else
            Var2 := Nil;

         // Check that we have a valid second variable. If it is not the case,
         // then just ignore the whole thing by returning false. It is ok to do
         // so, because the error will be found in "TCellMLMapVariables.IsValid"

         // Remember that we know that the first variable has either a public or
         // a private interface of "in"

         If (Var2 <> Nil) Then Begin
            If (CompareStr(aVariable1.PublicInterface, 'in') = 0) Then
               Result := (CompareStr(Var2.PublicInterface, 'out') = 0) Or (CompareStr(Var2.PrivateInterface, 'out') = 0)
            Else If (CompareStr(aVariable1.PrivateInterface, 'in') = 0) Then
               Result := CompareStr(Var2.PublicInterface, 'out') = 0;

            If (Result) Then Begin
               Res2 := False;

               If (CompareStr(Var2.PublicInterface, 'in') = 0) Then
                  Res2 := (CompareStr(aVariable1.PublicInterface, 'out') = 0) Or (CompareStr(aVariable1.PrivateInterface, 'out') = 0)
               Else If (CompareStr(Var2.PrivateInterface, 'in') = 0) Then
                  Res2 := CompareStr(aVariable1.PublicInterface, 'out') = 0;

               If (Res2) Then Begin
                  // The variable could pass either from the first to the second
                  // component or from the second to the first component. We
                  // are, however, here expecting the variable to pass from the
                  // first to the second component, which means that the first
                  // component should be encapsulated by the second one...

                  If (aParent1 = Nil) Then
                     Result := False
                  Else
                     Result := CompareStr(aParent1.Name, Comp2.Name) = 0;
               End;
            End;
         End;
      End;
   Var
      Iter, Iter2: Integer;
      Connection: TCellMLConnection;
      MapVariables: TCellMLMapVariables;
   Begin
      Result := 0;

      // Scan the different connection in search of one that includes the
      // component

      For Iter := 0 To aModel.ConnectionList.Size-1 Do Begin
         Connection := TCellMLConnection(aModel.ConnectionList.At(Iter).VObject);

         If (CompareStr(Connection.Component1, aComponent) = 0) Then Begin
            // Found the component we are interested in in the current
            // connection, so check whether one of the mapped variables is the
            // one we are interested in and, if so, how many times it is mapped

            For Iter2 := 0 To Connection.MapVariablesList.Size-1 Do Begin
               MapVariables := TCellMLMapVariables(Connection.MapVariablesList.At(Iter2).VObject);

               If ((CompareStr(MapVariables.Variable1, aVariable.Name) = 0) And
                   IsProperlyMapped(aVariable, aParent, Connection.Component2, MapVariables.Variable2)) Then
                  Inc(Result);
            End;
         End Else If (CompareStr(Connection.Component2, aComponent) = 0) Then Begin
            // Found the component we are interested in in the current
            // connection, so check whether one of the mapped variables is the
            // one we are interested in and, if so, how many times it is mapped

            For Iter2 := 0 To Connection.MapVariablesList.Size-1 Do Begin
               MapVariables := TCellMLMapVariables(Connection.MapVariablesList.At(Iter2).VObject);

               If ((CompareStr(MapVariables.Variable2, aVariable.Name) = 0) And
                   IsProperlyMapped(aVariable, aParent, Connection.Component1, MapVariables.Variable1)) Then
                  Inc(Result);
            End;
         End;
      End;
   End;
   Function IsMappedAtMostOnceOnly(Const aModel: TCellMLModel;
                                   Const aComponent: String;
                                   Const aVariable: TCellMLVariable;
                                   Const aParent: TCellMLComponent): Boolean; Inline;
   Begin
      Result := NbOfProperMappings(aModel, aComponent, aVariable, aParent) <= 1;
   End;
   Function IsValidPublicInterfaceOfIn(Const aModel: TCellMLModel;
                                       Const aComponent1: String;
                                       Const aVariable1: TCellMLVariable;
                                       Const aParent1: TCellMLComponent;
                                       Const aEncapsulatedSet1, aSiblingSet1: TCellMLSortedList;
                                       Const aVariable2: TCellMLVariable): Boolean; Inline;
   Begin
      Result := False;   // By default, it's not the case...

      If ((aSiblingSet1 <> Nil) And Not AtEnd(aSiblingSet1.Find(aVariable2.Owner.Name))) Then
         // The variable is declared in a component that belongs to the sibling
         // set of the first variable, so the mapping must be unique and the
         // public interface of the variable must be of "out"

         Result := (NbOfProperMappings(aModel, aComponent1, aVariable1, aParent1) = 1) And
                   (CompareStr(aVariable2.PublicInterface, 'out') = 0);

      If (Not Result And
          (aParent1 <> Nil) And (aParent1.FindVariable(aVariable2.Name) <> Nil)) Then
         // The variable is declared in a component that belongs to the the
         // parent of the first variable, so the mapping must be unique and the
         // private interface of the variable must be of "out"

         Result := (NbOfProperMappings(aModel, aComponent1, aVariable1, aParent1) = 1) And
                   (CompareStr(aVariable2.PrivateInterface, 'out') = 0);
   End;
   Function IsValidPublicInterfaceOfOut(Const aParent1: TCellMLComponent;
                                        Const aSiblingSet1: TCellMLSortedList;
                                        Const aVariable2: TCellMLVariable): Boolean; Inline;
   Begin
      Result := True;   // By default, it's the case...

      If ((aSiblingSet1 <> Nil) And Not AtEnd(aSiblingSet1.Find(aVariable2.Owner.Name))) Then
         // The variable is declared in a component that belongs to the sibling
         // set of the first variable, so its public interface must be of "in"

         Result := CompareStr(aVariable2.PublicInterface, 'in') = 0;

      If (Not Result And
          (aParent1 <> Nil) And (aParent1.FindVariable(aVariable2.Name) <> Nil)) Then
         // The variable is declared in a component that belongs to the parent
         // of the first variable, so its private interface must be of "in"

         Result := CompareStr(aVariable2.PrivateInterface, 'in') = 0;
   End;
   Function IsValidPrivateInterfaceOfIn(Const aModel: TCellMLModel;
                                        Const aComponent1: String;
                                        Const aVariable1: TCellMLVariable;
                                        Const aParent1: TCellMLComponent;
                                        Const aEncapsulatedSet1, aSiblingSet1: TCellMLSortedList;
                                        Const aVariable2: TCellMLVariable): Boolean; Inline;
   Begin
      Result := True;   // By default, it's the case...

      If ((aEncapsulatedSet1 <> Nil) And Not AtEnd(aEncapsulatedSet1.Find(aVariable2.Owner.Name))) Then
         // The variable is declared in a component that belongs to the
         // encapsulated set of the first variable, so the mapping must be
         // unique and the public interface of the variable must be of "out"

         Result := (NbOfProperMappings(aModel, aComponent1, aVariable1, aParent1) = 1) And
                   (CompareStr(aVariable2.PublicInterface, 'out') = 0);
   End;
   Function IsValidPrivateInterfaceOfOut(Const aEncapsulatedSet1: TCellMLSortedList;
                                         Const aVariable2: TCellMLVariable): Boolean; Inline;
   Begin
      Result := True;   // By default, it's the case...

      If ((aEncapsulatedSet1 <> Nil) And Not AtEnd(aEncapsulatedSet1.Find(aVariable2.Owner.Name))) Then
         // The variable is declared in a component that belongs to the
         // encapsulated set of the first variable, so its public interface must
         // be of "in"

         Result := CompareStr(aVariable2.PublicInterface, 'in') = 0;
   End;
Var
   Comp1, Comp2: TCellMLComponent;
   Var1, Var2: TCellMLVariable;
   Parent1, Parent2: TCellMLComponent;
   EncapsulatedSet1, EncapsulatedSet2: TCellMLSortedList;
   SiblingSet1, SiblingSet2: TCellMLSortedList;
   Res1, Res2: Boolean;
Begin
   Result := True;

   // A connection must be between two variables

   If ((CompareStr(Variable1, '') = 0) Or (CompareStr(Variable2, '') = 0)) Then Begin
      AddMsg(mtError, FileName, Line, 'two variables are required for a mapping');

      Result := False;
   End Else Begin
      // The first variable must exist within the first component

      Comp1 := FOwner.Owner.FindComponent(FOwner.Component1);
      Var1  := Nil;

      If (Comp1 <> Nil) Then Begin
         Var1 := Comp1.FindVariable(Variable1);

         If (Var1 = Nil) Then Begin
            AddMsg(mtError, FileName, Line, ''''+Variable1+''' is not defined in '''+FOwner.Component1+'''');

            Result := False;
         End;
      End Else
         Result := False;

      // The second variable must exist within the second component

      Comp2 := FOwner.Owner.FindComponent(FOwner.Component2);
      Var2  := Nil;

      If (Comp2 <> Nil) Then Begin
         Var2 := Comp2.FindVariable(Variable2);

         If (Var2 = Nil) Then Begin
            AddMsg(mtError, FileName, Line, ''''+Variable2+''' is not defined in '''+FOwner.Component2+'''');

            Result := False;
         End;
      End Else
         Result := False;

      If (Result) Then Begin
         // Get the parent, encapsulated and sibling sets for the two variables

         Parent1 := Nil;

         EncapsulatedSet1 := TCellMLSortedList.Create(False);
         SiblingSet1      := TCellMLSortedList.Create(False);

         FOwner.Owner.EncapsulationSets(Comp1.Name, Parent1, EncapsulatedSet1, SiblingSet1);

         Parent2 := Nil;

         EncapsulatedSet2 := TCellMLSortedList.Create(False);
         SiblingSet2      := TCellMLSortedList.Create(False);

         FOwner.Owner.EncapsulationSets(Comp2.Name, Parent2, EncapsulatedSet2, SiblingSet2);

         // If one of the variables has a public/private interface of "in", then
         // the other variable must have a public/private interface of "out"

         If ((CompareStr(Var1.PublicInterface, 'in') = 0) Or (CompareStr(Var1.PrivateInterface, 'in') = 0)) Then Begin
            Res1 := (CompareStr(Var2.PublicInterface, 'out') = 0) Or (CompareStr(Var2.PrivateInterface, 'out') = 0);

            If ((CompareStr(Var2.PublicInterface, 'in') = 0) Or (CompareStr(Var2.PrivateInterface, 'in') = 0)) Then Begin
               // Both variables have a public/private interface of "in", so
               // determine which one is the "valid" one, if any...

               Res2 := (CompareStr(Var1.PublicInterface, 'out') = 0) Or (CompareStr(Var1.PrivateInterface, 'out') = 0);

               If (Not Res1 And Not Res2) Then Begin
                  // Neither of the mappings is valid, so...

                  AddMsg(mtError, FileName, Line, 'the public/private interface attribute of '''+Comp1.Name+'.'+Variable1+''' and '''+Comp2.Name+'.'+Variable2+''' being equal to ''in'', the public/private interface attribute of '''+Comp2.Name+'.'+Variable2+''' or '''+Comp1.Name+'.'+Variable1+''', respectively, must be equal to ''out''');

                  Result := False;
               End;
            End Else If (Not Res1) Then Begin
               // Only the first variable has a public/private interface of "in"
               // and the mapping is not valid, so...

               AddMsg(mtError, FileName, Line, 'the public/private interface attribute of '''+Comp1.Name+'.'+Variable1+''' being equal to ''in'', the public/private interface attribute of '''+Comp2.Name+'.'+Variable2+''' must be equal to ''out''');

               Result := False;
            End;
         End Else If (((CompareStr(Var2.PublicInterface, 'in') = 0) Or (CompareStr(Var2.PrivateInterface, 'in') = 0)) And
                      (CompareStr(Var1.PublicInterface, 'out') <> 0) And (CompareStr(Var1.PrivateInterface, 'out') <> 0)) Then Begin
            // Only the second variable has a public/private interface of "in"
            // and the mapping is not valid, so...

            AddMsg(mtError, FileName, Line, 'the public/private interface attribute of '''+Comp2.Name+'.'+Variable2+''' being equal to ''in'', the public/private interface attribute of '''+Comp1.Name+'.'+Variable1+''' must be equal to ''out''');

            Result := False;
         End;

         // If one of the variables has a public/private interface of "in", then
         // it must be mapped to no more than one other variable in the model

         If (((CompareStr(Var1.PublicInterface, 'in') = 0) Or (CompareStr(Var1.PrivateInterface, 'in') = 0)) And
             Not IsMappedAtMostOnceOnly(FOwner.Owner, Comp1.Name, Var1, Parent1)) Then Begin
            AddMsg(mtError, FileName, Line, 'the public/private interface attribute of '''+Comp1.Name+'.'+Variable1+''' being equal to ''in'', it can only be mapped to one other variable in the model');

            Result := False;
         End;

         If (((CompareStr(Var2.PublicInterface, 'in') = 0) Or (CompareStr(Var2.PrivateInterface, 'in') = 0)) And
             Not IsMappedAtMostOnceOnly(FOwner.Owner, Comp2.Name, Var2, Parent2)) Then Begin
            AddMsg(mtError, FileName, Line, 'the public/private interface attribute of '''+Comp2.Name+'.'+Variable2+''' being equal to ''in'', it can only be mapped to one other variable in the model');

            Result := False;
         End;

         // If one of the variables has a public interface of "in", then it must
         // be mapped to a single variable owned by a component in the sibling
         // set, provided the target variable has a public interface of "out",
         // or to a single variable owned by the parent component, provided the
         // target variable has a private interface of "out"

         If (CompareStr(Var1.PublicInterface, 'in') = 0) Then Begin
            Res1 := IsValidPublicInterfaceOfIn(FOwner.Owner, Comp1.Name, Var1, Parent1, EncapsulatedSet1, SiblingSet1, Var2);

            If (CompareStr(Var2.PublicInterface, 'in') = 0) Then Begin
               // Both variables have a public interface of "in", so determine
               // which one is the "valid" one, if any...

               Res2 := IsValidPublicInterfaceOfIn(FOwner.Owner, Comp2.Name, Var2, Parent2, EncapsulatedSet2, SiblingSet2, Var1);

               If (Not Res1 And Not Res2) Then Begin
                  // Neither of the mappings is valid, so...

                  AddMsg(mtError, FileName, Line, 'the public interface attribute of '''+Comp1.Name+'.'+Variable1+''' and '''+Comp2.Name+'.'+Variable2+''' being equal to ''in'', one of the two variables must be mapped to a single variable that is owned by a component in the sibling set (or by the parent component) and that has a public (or private) interface attribute equal to ''out''');

                  Result := False;
               End;
            End Else If (Not Res1) Then Begin
               // Only the first variable has a public interface of "in" and the
               // mapping is not valid, so...

               AddMsg(mtError, FileName, Line, 'the public interface attribute of '''+Comp1.Name+'.'+Variable1+''' being equal to ''in'', '''+Comp2.Name+'.'+Variable2+''' must be owned by a component in the sibling set (or by the parent component) and have a public (or private) interface attribute equal to ''out''');

               Result := False;
            End;
         End Else If ((CompareStr(Var2.PublicInterface, 'in') = 0) And
                      Not IsValidPublicInterfaceOfIn(FOwner.Owner, Comp2.Name, Var2, Parent2, EncapsulatedSet2, SiblingSet2, Var1)) Then Begin

            // Only the second variable has a public interface of "in" and the
            // mapping is not valid, so...

            AddMsg(mtError, FileName, Line, 'the public interface attribute of '''+Comp2.Name+'.'+Variable2+''' being equal to ''in'', '''+Comp1.Name+'.'+Variable1+''' must be owned by a component in the sibling set (or by the parent component) and have a public (or private) interface attribute equal to ''out''');

            Result := False;
         End;

         // If one of the variables has a public interface of "out", then it may
         // be mapped to variables owned by components in the sibling set,
         // provided the target variables have a public interface of "in". It
         // may also be mapped to variables owned by the parent component,
         // provided the target variables have a private interface of "in"

         If (CompareStr(Var1.PublicInterface, 'out') = 0) Then Begin
            Res1 := IsValidPublicInterfaceOfOut(Parent1, SiblingSet1, Var2);

            If (CompareStr(Var2.PublicInterface, 'out') = 0) Then Begin
               // Both variables have a private interface of "out", so determine
               // which one is the valid one, if any...

               Res2 := IsValidPublicInterfaceOfOut(Parent2, SiblingSet2, Var1);

               If (Not Res1 Or Not Res2) Then Begin
                  // One of the mappings is not valid, so...

                  AddMsg(mtError, FileName, Line, 'the public interface attribute of '''+Comp1.Name+'.'+Variable1+''' and '''+Comp2.Name+'.'+Variable2+''' being equal to ''out'', and being mapped to a variable that is owned by a component in the sibling set (or by the parent component), the target variable must have a public (or private) interface attribute equal to ''in''');

                  Result := False;
               End;
            End Else If (Not Res1) Then Begin
               // Only the first variable has a private interface of "out" and
               // the mapping is not valid, so...

               AddMsg(mtError, FileName, Line, 'the public interface attribute of '''+Comp1.Name+'.'+Variable1+''' being equal to ''out'' and '''+Comp2.Name+'.'+Variable2+''' being owned by a component in the sibling set (or by the parent component), '''+Comp2.Name+'.'+Variable2+''' must have a public (or private) interface attribute equal to ''in''');

               Result := False;
            End;
         End Else If ((CompareStr(Var2.PublicInterface, 'out') = 0) And
                      Not IsValidPublicInterfaceOfOut(Parent2, SiblingSet2, Var1)) Then Begin
            // Only the second variable has a private interface of "out" and the
            // mapping is not valid, so...

            AddMsg(mtError, FileName, Line, 'the public interface attribute of '''+Comp2.Name+'.'+Variable2+''' being equal to ''out'' and '''+Comp1.Name+'.'+Variable1+''' being owned by a component in the sibling set (or by the parent component), '''+Comp1.Name+'.'+Variable1+''' must have a public (or private) interface attribute equal to ''in''');

            Result := False;
         End;

         // If one of the variables has a private interface of "in", then it
         // may be mapped to a single variable owned by a component in the
         // encapsulated set, provided the target variable has a public
         // interface of "out"

         If (CompareStr(Var1.PrivateInterface, 'in') = 0) Then Begin
            Res1 := IsValidPrivateInterfaceOfIn(FOwner.Owner, Comp1.Name, Var1, Parent1, EncapsulatedSet1, SiblingSet1, Var2);

            If (CompareStr(Var2.PrivateInterface, 'in') = 0) Then Begin
               // Both variables have a private interface of "in", so determine
               // which one is the valid one, if any...

               Res2 := IsValidPrivateInterfaceOfIn(FOwner.Owner, Comp2.Name, Var2, Parent2, EncapsulatedSet2, SiblingSet2, Var1);

               If (Not Res1 Or Not Res2) Then Begin
                  // One of the mappings is not valid, so...

                  AddMsg(mtError, FileName, Line, 'the private interface attribute of '''+Comp1.Name+'.'+Variable1+''' and '''+Comp2.Name+'.'+Variable2+''' being equal to ''in'', and being mapped to a single variable that is owned by a component in the encapsulated set, the target variable must have a public interface attribute equal to ''out''');

                  Result := False;
               End;
            End Else If (Not Res1) Then Begin
               // Only the first variable has a private interface of "in" and
               // the mapping is not valid, so...

               AddMsg(mtError, FileName, Line, 'the private interface attribute of '''+Comp1.Name+'.'+Variable1+''' being equal to ''in'', and being mapped to '''+Comp2.Name+'.'+Variable2+''' that is owned by a component in the encapsulated set, '''+Comp2.Name+'.'+Variable2+''' must have a public interface attribute equal to ''out''');

               Result := False;
            End;
         End Else If ((CompareStr(Var2.PrivateInterface, 'in') = 0) And
                      Not IsValidPrivateInterfaceOfIn(FOwner.Owner, Comp2.Name, Var2, Parent2, EncapsulatedSet2, SiblingSet2, Var1)) Then Begin
            // Only the second variable has a private interface of "in" and the
            // mapping is not valid, so...

            AddMsg(mtError, FileName, Line, 'the private interface attribute of '''+Comp2.Name+'.'+Variable2+''' being equal to ''in'', and being mapped to '''+Comp1.Name+'.'+Variable1+''' that is owned by a component in the encapsulated set, '''+Comp1.Name+'.'+Variable1+''' must have a public interface attribute equal to ''out''');

            Result := False;
         End;

         // If one of the variables has a private interface of "out", then it
         // may be mapped to variables owned by components in the encapsulated
         // set, provided the target variables have a public interface of "in"

         If (CompareStr(Var1.PrivateInterface, 'out') = 0) Then Begin
            Res1 := IsValidPrivateInterfaceOfOut(EncapsulatedSet1, Var2);

            If (CompareStr(Var2.PrivateInterface, 'out') = 0) Then Begin
               // Both variables have a private interface of "out", so determine
               // which one is the valid one, if any...

               Res2 := IsValidPrivateInterfaceOfOut(EncapsulatedSet2, Var1);

               If (Not Res1 Or Not Res2) Then Begin
                  // One of the mappings is not valid, so...

                  AddMsg(mtError, FileName, Line, 'the private interface attribute of '''+Comp1.Name+'.'+Variable1+''' and '''+Comp2.Name+'.'+Variable2+''' being equal to ''out'', and being mapped to a variable that is owned by a component in the encapsulated set, the target variable must have a public interface attribute equal to ''in''');

                  Result := False;
               End;
            End Else If (Not Res1) Then Begin
               // Only the first variable has a private interface of "out" and
               // the mapping is not valid, so...

               AddMsg(mtError, FileName, Line, 'the private interface attribute of '''+Comp1.Name+'.'+Variable1+''' being equal to ''out'', and being mapped to '''+Comp2.Name+'.'+Variable2+''' that is owned by a component in the encapsulated set, '''+Comp2.Name+'.'+Variable2+''' must have a public interface attribute equal to ''in''');

               Result := False;
            End;
         End Else If ((CompareStr(Var2.PrivateInterface, 'out') = 0) And
                      Not IsValidPrivateInterfaceOfOut(EncapsulatedSet2, Var1)) Then Begin
            // Only the second variable has a private interface of "out" and the
            // mapping is not valid, so...

            AddMsg(mtError, FileName, Line, 'the private interface attribute of '''+Comp2.Name+'.'+Variable2+''' being equal to ''out'', and being mapped to '''+Comp1.Name+'.'+Variable1+''' that is owned by a component in the encapsulated set, '''+Comp1.Name+'.'+Variable1+''' must have a public interface attribute equal to ''in''');

            Result := False;
         End;

         // Check that the two variables have dimensionally equivalent units,
         // but only if there is no problem with units definitions
         //---GRY--- IF THE UNITS ARE NOT THE SAME, BUT ARE NONETHELESS
         //          DIMENSIONALLY EQUIVALENT, THEN A CONVERSION BETWEEN THEM IS
         //          REQUIRED...

         If (Owner.Owner.UnitsAreValid And
             Var1.Owner.UnitsAreValid And Var2.Owner.UnitsAreValid) Then Begin
            Case Var1.Owner.FindUnit(Var1.Units).EquivalenceType(Var2.Owner.FindUnit(Var2.Units)) Of
               etNone:
                  AddMsg(mtWarning, FileName, Line, ''''+Comp1.Name+'.'+Variable1+''' ('''+Var1.Units+''') and '''+Comp2.Name+'.'+Variable2+''' ('''+Var2.Units+''') are not dimensionally equivalent');
               etDimensional:
                  AddMsg(mtWarning, FileName, Line, ''''+Comp1.Name+'.'+Variable1+''' ('''+Var1.Units+''') and '''+Comp2.Name+'.'+Variable2+''' ('''+Var2.Units+''') are dimensionally equivalent and should therefore be converted');
            End;
         End;

         // Free up the encapsulated and sibling sets

         EncapsulatedSet1.Free;
         SiblingSet1.Free;

         EncapsulatedSet2.Free;
         SiblingSet2.Free;
      End;
   End;
End;

//==============================================================================

Initialization

//==============================================================================

// Standard units

StandardUnits := TCellMLSortedList.Create;

// Add the different base SI units

AddStandardUnit(AMPERE, True);
AddStandardUnit(CANDELA, True);
AddStandardUnit(KELVIN, True);
AddStandardUnit(KILOGRAM, True);
AddStandardUnit(METRE, True);
AddStandardUnit(MOLE, True);

SecondUnit := AddStandardUnit(SECOND, True);

// Add the different derived SI units

StandardUnitsUnits := AddStandardUnit('becquerel', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, SECOND, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('celsius', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, KELVIN, '0', '1', '1', '-273.15')]);

StandardUnitsUnits := AddStandardUnit(COULOMB, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, AMPERE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, SECOND, '0', '1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('farad', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, COULOMB, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, VOLT, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('gray', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, JOULE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, KILOGRAM, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('henry', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, WEBER, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, AMPERE, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('hertz', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, SECOND, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit(JOULE, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, NEWTON, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('katal', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, MOLE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, SECOND, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit(LUMEN, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, CANDELA, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, STERADIAN, '0', '1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('lux', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, LUMEN, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '-2', '1', '0')]);

StandardUnitsUnits := AddStandardUnit(NEWTON, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, KILOGRAM, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, SECOND, '0', '-2', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('ohm', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, VOLT, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, AMPERE, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('pascal', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, NEWTON, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '-2', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('radian', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('siemens', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, AMPERE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, VOLT, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('sievert', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, JOULE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, KILOGRAM, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit(STERADIAN, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '2', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '-2', '1', '0')]);

StandardUnitsUnits := AddStandardUnit('tesla', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, WEBER, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, '0', '-2', '1', '0')]);

StandardUnitsUnits := AddStandardUnit(VOLT, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, WATT, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, AMPERE, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit(WATT, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, JOULE, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, SECOND, '0', '-1', '1', '0')]);

StandardUnitsUnits := AddStandardUnit(WEBER, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, VOLT, '0', '1', '1', '0')]);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, SECOND, '0', '1', '1', '0')]);

// Add the different units for convenience

DimensionlessUnit := AddStandardUnit(_DIMENSIONLESS_, True);
BooleanUnit       := AddStandardUnit(_BOOLEAN_, True);

StandardUnitsUnits := AddStandardUnit('gram', False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, KILOGRAM, '0', '1', '0.001', '0')]);

StandardUnitsUnits := AddStandardUnit(LITRE, False);
StandardUnitsUnits.AddUnitElement([TCellMLUnit.Create(StandardUnitsUnits, METRE, CENTI, '3', '1000', '0')]);

// Only use the memory that is required (no waste!)

StandardUnits.TrimToSize;

// Prefixes

Prefixes := DArray.CreateWith(MakeComparator(PrefixCompare));

With Prefixes Do Begin
   Add([TCellMLUnitPrefix.Create('yotta',  24)]);
   Add([TCellMLUnitPrefix.Create('zetta',  21)]);
   Add([TCellMLUnitPrefix.Create('exa',    18)]);
   Add([TCellMLUnitPrefix.Create('peta',   15)]);
   Add([TCellMLUnitPrefix.Create('tera',   12)]);
   Add([TCellMLUnitPrefix.Create('giga',    9)]);
   Add([TCellMLUnitPrefix.Create('mega',    6)]);
   Add([TCellMLUnitPrefix.Create('kilo',    3)]);
   Add([TCellMLUnitPrefix.Create('hecto',   2)]);
   Add([TCellMLUnitPrefix.Create('deka',    1)]);
   Add([TCellMLUnitPrefix.Create('deci',   -1)]);
   Add([TCellMLUnitPrefix.Create(CENTI,    -2)]);
   Add([TCellMLUnitPrefix.Create('milli',  -3)]);
   Add([TCellMLUnitPrefix.Create('micro',  -6)]);
   Add([TCellMLUnitPrefix.Create('nano',   -9)]);
   Add([TCellMLUnitPrefix.Create('pico',  -12)]);
   Add([TCellMLUnitPrefix.Create('femto', -15)]);
   Add([TCellMLUnitPrefix.Create('atto',  -18)]);
   Add([TCellMLUnitPrefix.Create('zepto', -21)]);
   Add([TCellMLUnitPrefix.Create('yocto', -24)]);

   // Only use the memory that is required (no waste!)

   TrimToSize;
End;

Sort(Prefixes);   // Shouldn't be necessary, but this is just in case we add a
                  // new prefix one day and don't insert it at the right
                  // place...

//==============================================================================

Finalization

//==============================================================================

StandardUnits.Free;   // Note: we must NOT free the objects held by
                      //       "StandardUnits", since "StandardUnits" is a
                      //       managed sorted list...

FreeAndClear(Prefixes);

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================
