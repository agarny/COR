//==============================================================================
// Tests for CellML classes
//
// by Alan Garny
//    alan.garny@dpag.ox.ac.uk
//    http://cor.physiol.ox.ac.uk/
//
// Copyright 2002-2010
//------------------------------------------------------------------------------
// Date of Creation: 20/06/2007
//
// Modifications: (model: [<initials>, dd/mm/yy] <what has been done>)
//==============================================================================

Unit CellMLAPITests;

//==============================================================================

Interface

//==============================================================================

Uses
   TestFramework, CellMLAPI, SysUtils, Dialogs, DeCAL;

//==============================================================================

Type
   TCellMLAPIUnitsUnitElement = Record
      Name: String;
      Prefix, Exponent, Multiplier, Offset: Double;
   End;
   TCellMLAPIUnitsTests = Class(TTestCase)
      Private
         // Properties used for internal purposes

         Units: TCellMLUnits;
         UEPrefix, UEExponent, UEMultiplier, UEOffset: Double;

         // Methods used for internal purposes

         Procedure CheckUnitsDefinitionAgainstAnother(Const aHeader, aName: String; Const aUnitElements: Array Of TCellMLAPIUnitsUnitElement);
   End;
   TCellMLAPIStandardUnitsTests = Class(TCellMLAPIUnitsTests)
      Private
         // Methods used for internal purposes

         Procedure CheckUnitsDefinition(Const aHeader, aName: String; Const aUnitElements: Array Of TCellMLAPIUnitsUnitElement); Overload;
         Procedure CheckUnitsDefinition(Const aHeader, aName: String); Overload; Inline;

      Published
         // Standard units

         Procedure AmpereTest;
         Procedure BecquerelTest;
         Procedure CandelaTest;
         Procedure CelsiusTest;
         Procedure CoulombTest;
         Procedure DimensionlessTest;
         Procedure FaradTest;
         Procedure GramTest;
         Procedure GrayTest;
         Procedure HenryTest;
         Procedure HertzTest;
         Procedure JouleTest;
         Procedure KatalTest;
         Procedure KelvinTest;
         Procedure KilogramTest;
         Procedure LitreTest;
         Procedure LumenTest;
         Procedure LuxTest;
         Procedure MetreTest;
         Procedure MoleTest;
         Procedure NewtonTest;
         Procedure OhmTest;
         Procedure PascalTest;
         Procedure RadianTest;
         Procedure SecondTest;
         Procedure SiemensTest;
         Procedure SievertTest;
         Procedure SteradianTest;
         Procedure TeslaTest;
         Procedure VoltTest;
         Procedure WattTest;
         Procedure WeberTest;
   End;
   TCellMLAPIJCUnitsTests = Class(TCellMLAPIUnitsTests)
      Private
         // Properties used for internal purposes

         OrigUnits: TCellMLUnits;

      Published
         Procedure Test01;
         Procedure Test02;
         Procedure Test03;
         Procedure Test04;
         Procedure Test05;
         Procedure Test06;
         Procedure Test07;
         Procedure Test08;
         Procedure Test09;
         Procedure Test10;
         Procedure Test11;
         Procedure Test12;
   End;
   TCellMLAPICanonicalFormUnitsTests = Class(TCellMLAPIUnitsTests)
      Published
         Procedure Test01;
         Procedure Test02;
         Procedure Test03;
         Procedure Test04;
         Procedure Test05;
         Procedure Test06;
   End;
   TCellMLAPIEquivalenceTypeUnitsTests = Class(TCellMLAPIUnitsTests)
      Private
         // Properties used for internal purposes

         OrigUnits: TCellMLUnits;

      Published
         Procedure Test01;
         Procedure Test02;
         Procedure Test03;
         Procedure Test04;
         Procedure Test05;
         Procedure Test06;
         Procedure Test07;
   End;
   TCellMLAPIMultiplicationUnitsTests = Class(TCellMLAPIUnitsTests)
      Published
         Procedure Test01;
         Procedure Test02;
         Procedure Test03;
         Procedure Test04;
         Procedure Test05;
         Procedure Test06;
   End;
   TCellMLAPIExponentiationUnitsTests = Class(TCellMLAPIUnitsTests)
      Private
         // Properties used for internal purposes

         OrigUnits: TCellMLUnits;

      Published
         Procedure Test01;
         Procedure Test02;
         Procedure Test03;
   End;
   TCellMLAPIMiscellaneousTests = Class(TCellMLAPIUnitsTests)
      Private
         // Properties used for internal purposes

         OrigUnits: TCellMLUnits;

      Published
         Procedure Test01;
         Procedure Test02;
         Procedure Test03;
   End;

//==============================================================================

Function Suite: ITestSuite;

//==============================================================================

Implementation

//==============================================================================

Uses
{$IFDEF OPT_MATH}
   OptMath,
{$ELSE}
   Math,
{$ENDIF}
   Common;

//==============================================================================

Function Suite: ITestSuite;
Begin
   Result := TestSuite('CellMLAPITests',
                       [TCellMLAPIStandardUnitsTests.Suite,
                        TCellMLAPIJCUnitsTests.Suite,
                        TCellMLAPICanonicalFormUnitsTests.Suite,
                        TCellMLAPIEquivalenceTypeUnitsTests.Suite,
                        TCellMLAPIMultiplicationUnitsTests.Suite,
                        TCellMLAPIExponentiationUnitsTests.Suite,
                        TCellMLAPIMiscellaneousTests.Suite]);
End;

//==============================================================================

Function RCellMLAPIUnitsUnitElement(Const aName: String;
                                    Const aPrefix, aExponent, aMultiplier, aOffset: Double): TCellMLAPIUnitsUnitElement; Inline;
Begin
   Result.Name := aName;

   Result.Prefix     := aPrefix;
   Result.Exponent   := aExponent;
   Result.Multiplier := aMultiplier;
   Result.Offset     := aOffset;
End;

//==============================================================================

Function Capitalise(Const aString: String): String; Inline;
Begin
   If (Length(aString) >= 1) Then
      Result := UpperCase(aString[1])+Copy(aString, 2, Length(aString)-1)
   Else
      Result := aString;
End;

//==============================================================================

Procedure TCellMLAPIUnitsTests.CheckUnitsDefinitionAgainstAnother(Const aHeader, aName: String;
                                                                  Const aUnitElements: Array Of TCellMLAPIUnitsUnitElement);
Var
   Iter: Integer;
   UEName: String;
   UnitElementName: String;
Begin
   CheckEquals(Length(aUnitElements), Units.NbOfUnitElements, aHeader+' - '+aName+' - Wrong number of unit elements');

   For Iter := Low(aUnitElements) To High(aUnitElements) Do Begin
      Units.GetUnitElement(Iter).ConvUnitElemAttr(UEPrefix, UEExponent, UEMultiplier, UEOffset);

      UEName := Units.GetUnitElement(Iter).Name;

      UnitElementName := Capitalise(UEName);

      CheckEquals(aUnitElements[Iter].Name, UEName, aHeader+' - '+aName+' - Wrong name');
      CheckEquals(SimplifyNb(aUnitElements[Iter].Prefix), SimplifyNb(UEPrefix), aHeader+' - '+aName+' - '+UnitElementName+' - Wrong prefix');
      CheckEquals(SimplifyNb(aUnitElements[Iter].Exponent), SimplifyNb(UEExponent), aHeader+' - '+aName+' - '+UnitElementName+' - Wrong exponent');
      CheckEquals(SimplifyNb(aUnitElements[Iter].Multiplier), SimplifyNb(UEMultiplier), aHeader+' - '+aName+' - '+UnitElementName+' - Wrong multiplier');
      CheckEquals(SimplifyNb(aUnitElements[Iter].Offset), SimplifyNb(UEOffset), aHeader+' - '+aName+' - '+UnitElementName+' - Wrong offset');
      // Note: for some reasons, the multiplier test fails for
      //       "TCellMLAPIExponentiationUnitsTests.Test03". The multiplier has
      //       a value of 1.25e65 in both cases, but still the test fails, hence
      //       our use of "SimplifyNb" instead... not neat, but eh!
   End;
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.CheckUnitsDefinition(Const aHeader, aName: String;
                                                            Const aUnitElements: Array Of TCellMLAPIUnitsUnitElement);
Var
   UnitName: String;
Begin
   Units := FindStandardUnit(aName);

   UnitName := Capitalise(aName);

   CheckNotEquals(Integer(Units), Integer(Nil), aHeader+' - '+UnitName+' - Cannot find the units definition');

   Units.CanonicalForm;

   CheckUnitsDefinitionAgainstAnother(aHeader, UnitName, aUnitElements);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.CheckUnitsDefinition(Const aHeader, aName: String);
Begin
   CheckUnitsDefinition(aHeader, aName, [RCellMLAPIUnitsUnitElement(aName, 0, 1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.AmpereTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.AmpereTest', 'ampere');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.BecquerelTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.BecquerelTest', 'becquerel', [RCellMLAPIUnitsUnitElement('second', 0, -1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.CandelaTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.CandelaTest', 'candela');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.CelsiusTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.CelsiusTest', 'celsius', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1, 1, -273.15)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.CoulombTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.CoulombTest', 'coulomb', [RCellMLAPIUnitsUnitElement('ampere', 0, 1, 1, 0),
                                                                                RCellMLAPIUnitsUnitElement('second', 0, 1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.DimensionlessTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.DimensionlessTest', 'dimensionless');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.FaradTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.FaradTest', 'farad', [RCellMLAPIUnitsUnitElement('ampere',   0,  2, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('kilogram', 0, -1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('metre',    0, -2, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('second',   0,  4, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.GramTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.GramTest', 'gram', [RCellMLAPIUnitsUnitElement('kilogram', 0, 1, 0.001, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.GrayTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.GrayTest', 'gray', [RCellMLAPIUnitsUnitElement('metre',  0,  2, 1, 0),
                                                                          RCellMLAPIUnitsUnitElement('second', 0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.HenryTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.HenryTest', 'henry', [RCellMLAPIUnitsUnitElement('ampere',   0, -2, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('metre',    0,  2, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('second',   0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.HertzTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.HertzTest', 'hertz', [RCellMLAPIUnitsUnitElement('second', 0, -1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.JouleTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.JouleTest', 'joule', [RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('metre',    0,  2, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('second',   0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.KatalTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.KatalTest', 'katal', [RCellMLAPIUnitsUnitElement('mole',   0,  1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('second', 0, -1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.KelvinTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.KelvinTest', 'kelvin');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.KilogramTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.KilogramTest', 'kilogram');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.LitreTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.LitreTest', 'litre', [RCellMLAPIUnitsUnitElement('metre', 0, 3, 0.001, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.LumenTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.LumenTest', 'lumen', [RCellMLAPIUnitsUnitElement('candela', 0, 1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.LuxTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.LuxTest', 'lux', [RCellMLAPIUnitsUnitElement('candela', 0,  1, 1, 0),
                                                                        RCellMLAPIUnitsUnitElement('metre',   0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.MetreTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.MetreTest', 'metre');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.MoleTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.MoleTest', 'mole');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.NewtonTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.NewtonTest', 'newton', [RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                              RCellMLAPIUnitsUnitElement('metre',    0,  1, 1, 0),
                                                                              RCellMLAPIUnitsUnitElement('second',   0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.OhmTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.OhmTest', 'ohm', [RCellMLAPIUnitsUnitElement('ampere',   0, -2, 1, 0),
                                                                        RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                        RCellMLAPIUnitsUnitElement('metre',    0,  2, 1, 0),
                                                                        RCellMLAPIUnitsUnitElement('second',   0, -3, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.PascalTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.PascalTest', 'pascal', [RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                              RCellMLAPIUnitsUnitElement('metre',    0, -1, 1, 0),
                                                                              RCellMLAPIUnitsUnitElement('second',   0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.RadianTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.RadianTest', 'radian', [RCellMLAPIUnitsUnitElement('dimensionless', 0, 1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.SecondTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.SecondTest', 'second');
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.SiemensTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.SiemensTest', 'siemens', [RCellMLAPIUnitsUnitElement('ampere',   0,  2, 1, 0),
                                                                                RCellMLAPIUnitsUnitElement('kilogram', 0, -1, 1, 0),
                                                                                RCellMLAPIUnitsUnitElement('metre',    0, -2, 1, 0),
                                                                                RCellMLAPIUnitsUnitElement('second',   0,  3, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.SievertTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.SievertTest', 'sievert', [RCellMLAPIUnitsUnitElement('metre',  0,  2, 1, 0),
                                                                                RCellMLAPIUnitsUnitElement('second', 0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.SteradianTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.SteradianTest', 'steradian', [RCellMLAPIUnitsUnitElement('dimensionless', 0, 1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.TeslaTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.TeslaTest', 'tesla', [RCellMLAPIUnitsUnitElement('ampere',   0, -1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('second',   0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.VoltTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.VoltTest', 'volt', [RCellMLAPIUnitsUnitElement('ampere',   0, -1, 1, 0),
                                                                          RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                          RCellMLAPIUnitsUnitElement('metre',    0,  2, 1, 0),
                                                                          RCellMLAPIUnitsUnitElement('second',   0, -3, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.WattTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.WattTest', 'watt', [RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                          RCellMLAPIUnitsUnitElement('metre',    0,  2, 1, 0),
                                                                          RCellMLAPIUnitsUnitElement('second',   0, -3, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIStandardUnitsTests.WeberTest;
Begin
   CheckUnitsDefinition('TCellMLAPIStandardUnitsTests.WeberTest', 'weber', [RCellMLAPIUnitsUnitElement('ampere',   0, -1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('kilogram', 0,  1, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('metre',    0,  2, 1, 0),
                                                                            RCellMLAPIUnitsUnitElement('second',   0, -2, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test01;
Begin
   //    ComplexUnits [Unit 10 0 second 2, Unit 2 0 gram (-1)]
   // == ComplexUnits [Unit 10.0 0.0 (BaseUnits "second") 2.0,
   //                  Unit 2000.0 0.0 (BaseUnits "kilogram") (-1.0)]

   Units := TCellMLUnits.Create(Nil, Nil, 'Test01');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'second', SimplifyNb(0), SimplifyNb( 2), SimplifyNb(10), SimplifyNb(0))]);
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'gram',   SimplifyNb(0), SimplifyNb(-1), SimplifyNb( 2), SimplifyNb(0))]);

      Units.CanonicalForm;

      CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test01', 'Test01', [RCellMLAPIUnitsUnitElement('kilogram', 0, -1, 2000, 0),
                                                                                     RCellMLAPIUnitsUnitElement('second',   0,  2,   10, 0)]);
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test02;
Begin
   //    fahrenheit
   // == SimpleUnits (5/9) 0.0 (BaseUnits "kelvin") (32 - 273.15 * (9/5))

   Units := TCellMLUnits.Create(Nil, Nil, 'Test02');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(5/9), SimplifyNb(32))]);

      Units.CanonicalForm;

      CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test02', 'Test02', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1, 5/9, 32-273.15*(9/5))]);
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test03;
Var
   U1, U2: TCellMLUnits;
Begin
   //    otimes (ComplexUnits [Unit 1 0 coulomb 1, Unit 1 0 volt (-1)])
   //           (ComplexUnits [Unit 1 0 volt 1])
   // == ComplexUnits [Unit 1 0 coulomb 1]

   // Note: we use different units, since "coulomb" is not a base unit and our
   //       implementation of "otimes" will generate a canonical form, so...

   U1 := TCellMLUnits.Create(Nil, Nil, 'Test03');

   Try
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'second',    SimplifyNb(0), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

      U2 := TCellMLUnits.Create(Nil, Nil, 'Test03b');

      Try
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'second', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

         Units := U1.Times(U2);

         Try
            CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test03', 'Test03', [RCellMLAPIUnitsUnitElement('metre', 0, 1, 1, 0)]);
         Finally
            Units.Free;
         End;
      Finally
         U2.Free;
      End;
   Finally
      U1.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test04;
Begin
   // radian == dimensionless

   Units := TCellMLUnits.Create(Nil, Nil, 'Test04');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'radian', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      Units.CanonicalForm;

      CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test04', 'Test04', [RCellMLAPIUnitsUnitElement('dimensionless', 0, 1, 1, 0)]);
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test05;
Begin
   // dim_equiv liter (ComplexUnits [Unit 1 0 metre 3])

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test05LHS');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'liter', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      Units := TCellMLUnits.Create(Nil, Nil, 'Test05RHS');

      Try
         Units.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', SimplifyNb(0), SimplifyNb(3), SimplifyNb(1), SimplifyNb(0))]);

         CheckEquals(Integer(etDimensional), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIJCUnitsTests.Test05 - Test05 - Units definitions are not dimensionally equivalent');
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test06;
Var
   U1, U2: TCellMLUnits;
Begin
   //    otimes kilogram (ComplexUnits [Unit 1 0 gram (-1)])
   // == ComplexUnits [Unit 1000.0 0.0 (BaseUnits "dimensionless") 1.0]

   U1 := TCellMLUnits.Create(Nil, Nil, 'Test06');

   Try
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'kilogram', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);

      U2 := TCellMLUnits.Create(Nil, Nil, 'Test06b');

      Try
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'gram', SimplifyNb(0), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

         Units := U1.Times(U2);

         Try
            CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test06', 'Test06', [RCellMLAPIUnitsUnitElement('dimensionless', 0, 1, 1000, 0)]);
         Finally
            Units.Free;
         End;
      Finally
         U2.Free;
      End;
   Finally
      U1.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test07;
Begin
   //    ComplexUnits [Unit 1 1 litre 2]
   // == ComplexUnits [Unit ((mfac 1000 (-2) 3)**2 * mfac 1 1 2) 0 meter 6.0]

   Units := TCellMLUnits.Create(Nil, Nil, 'Test07');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'litre', SimplifyNb(1), SimplifyNb(2), SimplifyNb(1), SimplifyNb(0))]);

      Units.CanonicalForm;

{$IFDEF OPT_MATH}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test07', 'Test07', [RCellMLAPIUnitsUnitElement('metre', 0, 6, OptPower(1000*OptPower(10, -2*3), 2)*OptPower(10, 2), 0)]);
{$ELSE}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test07', 'Test07', [RCellMLAPIUnitsUnitElement('metre', 0, 6, Power(1000*Power(10, -2*3), 2)*Power(10, 2), 0)]);
{$ENDIF}
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test08;
Begin
   //    ComplexUnits [Unit 1 1 litre 2]
   // == ComplexUnits [Unit 1e-4 0 meter 6.0]

   Units := TCellMLUnits.Create(Nil, Nil, 'Test08');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'litre', SimplifyNb(1), SimplifyNb(2), SimplifyNb(1), SimplifyNb(0))]);

      Units.CanonicalForm;

      CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test08', 'Test08', [RCellMLAPIUnitsUnitElement('metre', 0, 6, 0.0001, 0)]);
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test09;
Var
   U1, U2: TCellMLUnits;
Begin
   //    otimes (ComplexUnits [Unit 1 0 meter (-1)]) metre
   // == dimensionless

   U1 := TCellMLUnits.Create(Nil, Nil, 'Test09');

   Try
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'meter', SimplifyNb(0), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

      U2 := TCellMLUnits.Create(Nil, Nil, 'Test09b');

      Try
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

         Units := U1.Times(U2);

         Try
            CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test09', 'Test09', [RCellMLAPIUnitsUnitElement('dimensionless', 0, 1, 1, 0)]);
         Finally
            Units.Free;
         End;
      Finally
         U2.Free;
      End;
   Finally
      U1.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test10;
Var
   CellMLModel: TCellMLModel;
   Test10LHS, Test10RHS: TCellMLUnits;
   Iter: Integer;
   UnitElementLHS, UnitElementRHS: TCellMLUnit;
Begin
   //    ComplexUnits [Unit 1 kilo (ComplexUnits [Unit 1 0 metre 1,
   //                                             Unit 1 0 kilogram 1,
   //                                             Unit 1 0 second (-2)]) 1,
   //                  Unit 1 0 metre (-2)]
   // == ComplexUnits [Unit 1 kilo (ComplexUnits [Unit 1 0 kilogram 1,
   //                                             Unit 1 0 metre 1,
   //                                             Unit 1 0 second (-2)]) 1,
   //                  Unit 1 0 metre (-2)]

   CellMLModel := TCellMLModel.Create('');

   Try
      Test10LHS := TCellMLUnits.Create(CellMLModel, Nil, 'Test10LHS');

      Try
         Test10LHS.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',    SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
         Test10LHS.AddUnitElement([TCellMLUnit.Create(Nil, 'kilogram', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
         Test10LHS.AddUnitElement([TCellMLUnit.Create(Nil, 'second',   SimplifyNb(0), SimplifyNb(-2), SimplifyNb(1), SimplifyNb(0))]);

         CellMLModel.UnitsList.Add([Test10LHS]);

         OrigUnits := TCellMLUnits.Create(CellMLModel, Nil, 'Test10LHS');

         Try
            OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'Test10LHS', 'kilo', SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
            OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', SimplifyNb(0), SimplifyNb(-2), SimplifyNb(1), SimplifyNb(0))]);

            Test10RHS := TCellMLUnits.Create(CellMLModel, Nil, 'Test10RHS');

            Try
               Test10RHS.AddUnitElement([TCellMLUnit.Create(Nil, 'kilogram', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
               Test10RHS.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',    SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
               Test10RHS.AddUnitElement([TCellMLUnit.Create(Nil, 'second',   SimplifyNb(0), SimplifyNb(-2), SimplifyNb(1), SimplifyNb(0))]);

               CellMLModel.UnitsList.Add([Test10RHS]);

               Units := TCellMLUnits.Create(CellMLModel, Nil, 'Test10RHS');

               Try
                  Units.AddUnitElement([TCellMLUnit.Create(Nil, 'Test10RHS', 'kilo', SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
                  Units.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', SimplifyNb(0), SimplifyNb(-2), SimplifyNb(1), SimplifyNb(0))]);

                  // Get the canonical form of both units definitions

                  OrigUnits.CanonicalForm;
                  Units.CanonicalForm;

                  // Check that the two units definitions are 100% the same

                  CheckEquals(Units.NbOfUnitElements, OrigUnits.NbOfUnitElements, 'TCellMLAPIJCUnitsTests.Test10 - Test10 - Wrong number of unit elements');

                  For Iter := 0 To OrigUnits.NbOfUnitElements-1 Do Begin
                     UnitElementLHS := OrigUnits.GetUnitElement(Iter);
                     UnitElementRHS := Units.GetUnitElement(Iter);

                     If (CompareStr(UnitElementLHS.Name, 'Test10LHS') <> 0) Then
                        // Not testing the sub-units definition, so...

                        CheckEquals(UnitElementRHS.Name, UnitElementLHS.Name, 'TCellMLAPIJCUnitsTests.Test10 - Test10 - Wrong name');

                     CheckEquals(UnitElementRHS.Prefix, UnitElementLHS.Prefix, 'TCellMLAPIJCUnitsTests.Test10 - Test10 - '+UnitElementLHS.Name+' - Wrong prefix');
                     CheckEquals(UnitElementRHS.Exponent, UnitElementLHS.Exponent, 'TCellMLAPIJCUnitsTests.Test10 - Test10 - '+UnitElementLHS.Name+' - Wrong exponent');
                     CheckEquals(UnitElementRHS.Multiplier, UnitElementLHS.Multiplier, 'TCellMLAPIJCUnitsTests.Test10 - Test10 - '+UnitElementLHS.Name+' - Wrong multiplier');
                     CheckEquals(UnitElementRHS.Offset, UnitElementLHS.Offset, 'TCellMLAPIJCUnitsTests.Test10 - Test10 - '+UnitElementLHS.Name+' - Wrong offset');
                  End;
               Finally
                  Units.Free;
               End;
            Finally
               // Test10RHS.Free;
               // Note: it is indeed managed by CellMLModel, so...
            End;
         Finally
            OrigUnits.Free;
         End;
      Finally
         // Test10LHS.Free;
         // Note: it is indeed managed by CellMLModel, so...
      End;
   Finally
      CellMLModel.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test11;
Begin
   //    ComplexUnits [Unit 1 centi metre 2, Unit 10 0 metre 1]
   // == ComplexUnits [Unit 0.001 0 metre 3]

   Units := TCellMLUnits.Create(Nil, Nil, 'Test11');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', 'centi', SimplifyNb(2), SimplifyNb(1), SimplifyNb(0))]);
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', SimplifyNb(0), SimplifyNb(1), SimplifyNb(10), SimplifyNb(0))]);

      Units.CanonicalForm;

      CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test11', 'Test11', [RCellMLAPIUnitsUnitElement('metre', 0, 3, 0.001, 0)]);
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIJCUnitsTests.Test12;
Var
   U1, U2: TCellMLUnits;
Begin
   //    otimes (ComplexUnits [Unit 1 centi meter 2, Unit 1 0 kilogram 1])
   //           (ComplexUnits [Unit 10 0 metre (-1)])
   // == ComplexUnits [Unit 1 0 kilogram 1, Unit 0.001 0 metre 1]

   U1 := TCellMLUnits.Create(Nil, Nil, 'Test12');

   Try
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'meter', 'centi', SimplifyNb(2), SimplifyNb(1), SimplifyNb(0))]);
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'kilogram', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);

      U2 := TCellMLUnits.Create(Nil, Nil, 'Test12b');

      Try
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', SimplifyNb(0), SimplifyNb(-1), SimplifyNb(10), SimplifyNb(0))]);

         Units := U1.Times(U2);

         Try
            CheckUnitsDefinitionAgainstAnother('TCellMLAPIJCUnitsTests.Test12', 'Test12', [RCellMLAPIUnitsUnitElement('kilogram', 0, 1, 1,     0),
                                                                                           RCellMLAPIUnitsUnitElement('metre',    0, 1, 0.001, 0)]);
         Finally
            Units.Free;
         End;
      Finally
         U2.Free;
      End;
   Finally
      U1.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPICanonicalFormUnitsTests.Test01;
Begin
   // Canonical form of a base unit

   Units := FindStandardUnit('second');

   Units.CanonicalForm;

   CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test01', 'Test01', [RCellMLAPIUnitsUnitElement('second', 0, 1, 1, 0)]);
End;

//==============================================================================

Procedure TCellMLAPICanonicalFormUnitsTests.Test02;
Begin
   // Canonical form of a simple units definition based on a base unit

   Units := TCellMLUnits.Create(Nil, Nil, 'Test02');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'second', SimplifyNb(3), SimplifyNb( 1), SimplifyNb(5), SimplifyNb(7))]);

      Units.CanonicalForm;

{$IFDEF OPT_MATH}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test02', 'Test02', [RCellMLAPIUnitsUnitElement('second', 0, 1, 5*OptPower(10, 3), 7)]);
{$ELSE}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test02', 'Test02', [RCellMLAPIUnitsUnitElement('second', 0, 1, 5*Power(10, 3), 7)]);
{$ENDIF}
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPICanonicalFormUnitsTests.Test03;
Begin
   // Canonical form of a simple units definition based on a simple units
   // definition

   Units := TCellMLUnits.Create(Nil, Nil, 'Test03');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(3), SimplifyNb( 1), SimplifyNb(5), SimplifyNb(7))]);

      Units.CanonicalForm;

{$IFDEF OPT_MATH}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test03', 'Test03', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1, 5*OptPower(10, 3)*1*OptPower(10, 0), 7+(-273.15)/(5*OptPower(10, 3)))]);
{$ELSE}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test03', 'Test03', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1, 5*Power(10, 3)*1*Power(10, 0), 7+(-273.15)/(5*Power(10, 3)))]);
{$ENDIF}
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPICanonicalFormUnitsTests.Test04;
Begin
   // Canonical form of a complex units definition based on a base unit

   Units := TCellMLUnits.Create(Nil, Nil, 'Test04');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'second', SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);

      Units.CanonicalForm;

{$IFDEF OPT_MATH}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test04', 'Test04', [RCellMLAPIUnitsUnitElement('second', 0, 5, 7*OptPower(10, 3*5), 0)]);
{$ELSE}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test04', 'Test04', [RCellMLAPIUnitsUnitElement('second', 0, 5, 7*Power(10, 3*5), 0)]);
{$ENDIF}
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPICanonicalFormUnitsTests.Test05;
Begin
   // Canonical form of a complex units definition based on a simple units
   // definition

   Units := TCellMLUnits.Create(Nil, Nil, 'Test05');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);

      Units.CanonicalForm;

{$IFDEF OPT_MATH}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test05', 'Test05', [RCellMLAPIUnitsUnitElement('kelvin', 0, 5, 7*OptPower(1, 5)*OptPower(10, 3*5), 0)]);
{$ELSE}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test05', 'Test05', [RCellMLAPIUnitsUnitElement('kelvin', 0, 5, 7*Power(1, 5)*Power(10, 3*5), 0)]);
{$ENDIF}
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPICanonicalFormUnitsTests.Test06;
Begin
   // Canonical form of a complex units definition based on a complex units
   // definition

   Units := TCellMLUnits.Create(Nil, Nil, 'Test06');

   Try
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);
      Units.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(7), SimplifyNb(3), SimplifyNb(5), SimplifyNb(0))]);

      Units.CanonicalForm;

{$IFDEF OPT_MATH}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test06', 'Test06', [RCellMLAPIUnitsUnitElement('kelvin', 0, 3, 5*OptPower(1, 3)*OptPower(10, 7*3), 0),
                                                                                                RCellMLAPIUnitsUnitElement('second', 0, 5, 7*OptPower(10, 3*5), 0)]);
{$ELSE}
      CheckUnitsDefinitionAgainstAnother('TCellMLAPICanonicalFormUnitsTests.Test06', 'Test06', [RCellMLAPIUnitsUnitElement('kelvin', 0, 3, 5*Power(1, 3)*Power(10, 7*3), 0),
                                                                                                RCellMLAPIUnitsUnitElement('second', 0, 5, 7*Power(10, 3*5), 0)]);
{$ENDIF}
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIEquivalenceTypeUnitsTests.Test01;
Begin
   // Check that a base unit is exactly equivalent to a base unit

   OrigUnits := FindStandardUnit('second');
   Units     := FindStandardUnit('second');

   CheckEquals(Integer(etExact), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIEquivalenceTypeUnitsTests.Test01 - Test01 - Units definitions are not dimensionally equivalent');
End;

//==============================================================================

Procedure TCellMLAPIEquivalenceTypeUnitsTests.Test02;
Begin
   // Check that a simple units definition is dimensionally equivalent to a base
   // unit

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test02LHS');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', 'milli', SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      Units := FindStandardUnit('kelvin');

      CheckEquals(Integer(etDimensional), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIEquivalenceTypeUnitsTests.Test02 - Test02 - Units definitions are not dimensionally equivalent');
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIEquivalenceTypeUnitsTests.Test03;
Begin
   // Check that a complex units definition is dimensionally equivalent to a
   // base unit

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test03LHS');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'litre', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(  1), SimplifyNb(0))]);
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', 'deci',        SimplifyNb(-3), SimplifyNb(100), SimplifyNb(0))]);

      Units := FindStandardUnit('dimensionless');

      CheckEquals(Integer(etDimensional), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIEquivalenceTypeUnitsTests.Test03 - Test03 - Units definitions are not dimensionally equivalent');
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIEquivalenceTypeUnitsTests.Test04;
Begin
   // Check that a simple units definition is dimensionally equivalent to a
   // simple units definition

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test04LHS');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', 'milli', SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      Units := TCellMLUnits.Create(Nil, Nil, 'Test04RHS');

      Try
         Units.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', 'kilo', SimplifyNb(1), SimplifyNb(123), SimplifyNb(0.123))]);

         CheckEquals(Integer(etDimensional), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIEquivalenceTypeUnitsTests.Test04 - Test04 - Units definitions are not dimensionally equivalent');
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIEquivalenceTypeUnitsTests.Test05;
Begin
   // Check that a complex units definition is dimensionally equivalent to a
   // simple units definition

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test05LHS');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', 'deci', SimplifyNb(-3), SimplifyNb(100), SimplifyNb(0))]);
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', 'kilo', SimplifyNb( 6), SimplifyNb(  1), SimplifyNb(0))]);

      Units := TCellMLUnits.Create(Nil, Nil, 'Test05RHS');

      Try
         Units.AddUnitElement([TCellMLUnit.Create(Nil, 'litre', 'milli', SimplifyNb(1), SimplifyNb(123), SimplifyNb(0.123))]);

         CheckEquals(Integer(etDimensional), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIEquivalenceTypeUnitsTests.Test05 - Test05 - Units definitions are not dimensionally equivalent');
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIEquivalenceTypeUnitsTests.Test06;
Begin
   // Check that a complex units definition is dimensionally equivalent to a
   // complex units definition

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test06LHS');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'litre', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(  1), SimplifyNb(0))]);
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'second', 'milli',      SimplifyNb(-3), SimplifyNb(100), SimplifyNb(0))]);

      Units := TCellMLUnits.Create(Nil, Nil, 'Test06RHS');

      Try
         Units.AddUnitElement([TCellMLUnit.Create(Nil, 'second', SimplifyNb(0), SimplifyNb(-3), SimplifyNb(  1), SimplifyNb(0))]);
         Units.AddUnitElement([TCellMLUnit.Create(Nil, 'metre', 'deci',         SimplifyNb( 3), SimplifyNb(100), SimplifyNb(0))]);

         CheckEquals(Integer(etDimensional), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIEquivalenceTypeUnitsTests.Test06 - Test06 - Units definitions are not dimensionally equivalent');
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIEquivalenceTypeUnitsTests.Test07;
Begin
   // Check that mV/ms is exactly equivalent to V/s

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test07LHS');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'volt',   'milli', SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'second', 'milli', SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

      Units := TCellMLUnits.Create(Nil, Nil, 'Test07RHS');

      Try
         Units.AddUnitElement([TCellMLUnit.Create(Nil, 'volt',   SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);
         Units.AddUnitElement([TCellMLUnit.Create(Nil, 'second', SimplifyNb(0), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

         CheckEquals(Integer(etExact), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIEquivalenceTypeUnitsTests.Test07 - Test07 - Units definitions are not dimensionally equivalent');
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMultiplicationUnitsTests.Test01;
Var
   U1, U2: TCellMLUnits;
Begin
   // Multiplication of a base unit by another base unit

   U1 := FindStandardUnit('metre');
   U2 := FindStandardUnit('second');

   Units := U1.Times(U2);

   Try
      CheckUnitsDefinitionAgainstAnother('TCellMLAPIMultiplicationUnitsTests.Test01', 'Test01', [RCellMLAPIUnitsUnitElement('metre', 0, 1, 1, 0),
                                                                                                 RCellMLAPIUnitsUnitElement('second', 0, 1, 1, 0)]);
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMultiplicationUnitsTests.Test02;
Var
   U1, U2: TCellMLUnits;
Begin
   // Multiplication of a base unit by a simple units definition

   U1 := FindStandardUnit('metre');
   U2 := TCellMLUnits.Create(Nil, Nil, 'Test02b');

   Try
      U2.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      Units := U1.Times(U2);

      Try
         CheckUnitsDefinitionAgainstAnother('TCellMLAPIMultiplicationUnitsTests.Test02', 'Test02', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1, 1, 0),
                                                                                                    RCellMLAPIUnitsUnitElement('metre', 0, 1, 1, 0)]);
      Finally
         Units.Free;
      End;
   Finally
      U2.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMultiplicationUnitsTests.Test03;
Var
   U1, U2: TCellMLUnits;
Begin
   // Multiplication of a base unit by a complex units definition

   U1 := FindStandardUnit('metre');
   U2 := TCellMLUnits.Create(Nil, Nil, 'Test03b');

   Try
      U2.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);
      U2.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      Units := U1.Times(U2);

      Try
         CheckUnitsDefinitionAgainstAnother('TCellMLAPIMultiplicationUnitsTests.Test03', 'Test03', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1, 1, 0),
                                                                                                    RCellMLAPIUnitsUnitElement('metre', 0, 1, 1, 0),
{$IFDEF OPT_MATH}
                                                                                                    RCellMLAPIUnitsUnitElement('second', 0, 5, 7*OptPower(10, 3*5), 0)]);
{$ELSE}
                                                                                                    RCellMLAPIUnitsUnitElement('second', 0, 5, 7*Power(10, 3*5), 0)]);
{$ENDIF}
      Finally
         Units.Free;
      End;
   Finally
      U2.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMultiplicationUnitsTests.Test04;
Var
   U1, U2: TCellMLUnits;
Begin
   // Multiplication of a simple units definition by a simple units definition

   U1 := TCellMLUnits.Create(Nil, Nil, 'Test04');

   Try
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);

      U2 := TCellMLUnits.Create(Nil, Nil, 'Test04b');

      Try
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

         Units := U1.Times(U2);

         Try
            CheckUnitsDefinitionAgainstAnother('TCellMLAPIMultiplicationUnitsTests.Test04', 'Test04', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1+1, 1, 0)]);
         Finally
            Units.Free;
         End;
      Finally
         U2.Free;
      End;
   Finally
      U1.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMultiplicationUnitsTests.Test05;
Var
   U1, U2: TCellMLUnits;
Begin
   // Multiplication of a simple units definition by a complex units definition

   U1 := TCellMLUnits.Create(Nil, Nil, 'Test05');

   Try
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb( 1), SimplifyNb(1), SimplifyNb(0))]);

      U2 := TCellMLUnits.Create(Nil, Nil, 'Test05b');

      Try
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

         Units := U1.Times(U2);

         Try
            CheckUnitsDefinitionAgainstAnother('TCellMLAPIMultiplicationUnitsTests.Test05', 'Test05', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1+1, 1, 0),
{$IFDEF OPT_MATH}
                                                                                                       RCellMLAPIUnitsUnitElement('second', 0, 5, 7*OptPower(10, 3*5), 0)]);
{$ELSE}
                                                                                                       RCellMLAPIUnitsUnitElement('second', 0, 5, 7*Power(10, 3*5), 0)]);
{$ENDIF}
         Finally
            Units.Free;
         End;
      Finally
         U2.Free;
      End;
   Finally
      U1.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMultiplicationUnitsTests.Test06;
Var
   U1, U2: TCellMLUnits;
Begin
   // Multiplication of a complex units definition by a complex units definition

   U1 := TCellMLUnits.Create(Nil, Nil, 'Test06');

   Try
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);
      U1.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      U2 := TCellMLUnits.Create(Nil, Nil, 'Test06b');

      Try
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);
         U2.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

         Units := U1.Times(U2);

         Try
            CheckUnitsDefinitionAgainstAnother('TCellMLAPIMultiplicationUnitsTests.Test06', 'Test06', [RCellMLAPIUnitsUnitElement('kelvin', 0, 1+1, 1, 0),
{$IFDEF OPT_MATH}
                                                                                                       RCellMLAPIUnitsUnitElement('second', 0, 5+5, 7*OptPower(10, 3*5)*7*OptPower(10, 3*5), 0)]);
{$ELSE}
                                                                                                       RCellMLAPIUnitsUnitElement('second', 0, 5+5, 7*Power(10, 3*5)*7*Power(10, 3*5), 0)]);
{$ENDIF}
         Finally
            Units.Free;
         End;
      Finally
         U2.Free;
      End;
   Finally
      U1.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIExponentiationUnitsTests.Test01;
Begin
   // Exponentiation of a base unit

   OrigUnits := FindStandardUnit('second');

   Units := OrigUnits.Exponentiates(3);

   Try
      CheckUnitsDefinitionAgainstAnother('TCellMLAPIExponentiationUnitsTests.Test01', 'Test01', [RCellMLAPIUnitsUnitElement('second', 0, 3, 1, 0)]);
   Finally
      Units.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIExponentiationUnitsTests.Test02;
Begin
   // Exponentiation of a simple units defition

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test02');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

      Units := OrigUnits.Exponentiates(3);

      Try
         CheckUnitsDefinitionAgainstAnother('TCellMLAPIExponentiationUnitsTests.Test02', 'Test02', [RCellMLAPIUnitsUnitElement('kelvin', 0, 3, 1, 0)]);
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIExponentiationUnitsTests.Test03;
Begin
   // Exponentiation of a complex units defition

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test03');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(3), SimplifyNb(5), SimplifyNb(7), SimplifyNb(0))]);
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'celsius', SimplifyNb(7), SimplifyNb(3), SimplifyNb(5), SimplifyNb(0))]);

      Units := OrigUnits.Exponentiates(3);

      Try
{$IFDEF OPT_MATH}
         CheckUnitsDefinitionAgainstAnother('TCellMLAPIExponentiationUnitsTests.Test03', 'Test03', [RCellMLAPIUnitsUnitElement('kelvin', 0, 3*3, OptPower(5*OptPower(1, 3)*OptPower(10, 7*3), 3), 0),
                                                                                                    RCellMLAPIUnitsUnitElement('second', 0, 5*3, OptPower(7*OptPower(10, 3*5), 3), 0)]);
{$ELSE}
         CheckUnitsDefinitionAgainstAnother('TCellMLAPIExponentiationUnitsTests.Test03', 'Test03', [RCellMLAPIUnitsUnitElement('kelvin', 0, 3*3, Power(5*Power(1, 3)*Power(10, 7*3), 3), 0),
                                                                                                    RCellMLAPIUnitsUnitElement('second', 0, 5*3, Power(7*Power(10, 3*5), 3), 0)]);
{$ENDIF}
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMiscellaneousTests.Test01;
Var
   CellMLModel: TCellMLModel;
   MinuteUnits: TCellMLUnits;
   U1, U2, UTemp: TcellMLUnits;
Begin
   // metre_minute == metre/minute

   CellMLModel := TCellMLModel.Create('UNDEFINED');

   Try
      MinuteUnits := TCellMLUnits.Create(CellMLModel, Nil, 'minute');

      Try
         MinuteUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(60), SimplifyNb(0))]);

         CellMLModel.UnitsList.Add([MinuteUnits]);

         OrigUnits := TCellMLUnits.Create(CellMLModel, Nil, 'Test01');

         Try
            OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);
            OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'minute', SimplifyNb(0), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

            U1 := TCellMLUnits.Create(CellMLModel, Nil, 'U1');
            U2 := TCellMLUnits.Create(CellMLModel, Nil, 'U2');

            Try
               U1.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);
               U2.AddUnitElement([TCellMLUnit.Create(Nil, 'minute',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

               UTemp := U2.Exponentiates(-1);

               Units := U1.Times(UTemp);

               Try
                  CheckEquals(Integer(etExact), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIMiscellaneousTests.Test01 - Test01 - Units definitions are not exactly equivalent');
               Finally
                  UTemp.Free;
                  Units.Free;
               End;
            Finally
               U1.Free;
               U2.Free;
            End;
         Finally
            OrigUnits.Free;
         End;
      Finally
//         MinuteUnits.Free;
// Note: dealt with when freeing up "CellMLModel"... 
      End;
   Finally
      CellMLModel.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMiscellaneousTests.Test02;
Begin
   // centimetre_millimetre == centimetre_millimetre*dimensionless

   OrigUnits := TCellMLUnits.Create(Nil, Nil, 'Test02');

   Try
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',  SimplifyNb(-2), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);
      OrigUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',  SimplifyNb(-3), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

      Units := OrigUnits.Times(FindStandardUnit('dimensionless'));

      Try
         CheckEquals(Integer(etExact), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIMiscellaneousTests.Test02 - Test02 - Units definitions are not exactly equivalent');
      Finally
         Units.Free;
      End;
   Finally
      OrigUnits.Free;
   End;
End;

//==============================================================================

Procedure TCellMLAPIMiscellaneousTests.Test03;
Var
   CellMLModel: TCellMLModel;
   MinuteUnits: TCellMLUnits;
   OU1, OU2, OUTemp: TcellMLUnits;
   U1, U2, U3, UTemp1, UTemp2, UTemp3: TCellMLUnits;
Begin
   // metre/minute == metre_minute*centigram*per_milligram/(centigram*per_milligram)

   CellMLModel := TCellMLModel.Create('UNDEFINED');

   Try
      MinuteUnits := TCellMLUnits.Create(CellMLModel, Nil, 'minute');

      OU1 := TCellMLUnits.Create(CellMLModel, Nil, 'OU1');
      OU2 := TCellMLUnits.Create(CellMLModel, Nil, 'OU2');

      Try
         MinuteUnits.AddUnitElement([TCellMLUnit.Create(Nil, 'second',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(60), SimplifyNb(0))]);

         CellMLModel.UnitsList.Add([MinuteUnits]);

         OU1.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);
         OU2.AddUnitElement([TCellMLUnit.Create(Nil, 'minute',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

         OUTemp := OU2.Exponentiates(-1);

         OrigUnits := OU1.Times(OUTemp);

         U1 := TcellMLUnits.Create(CellMLModel, Nil, 'U1');
         U2 := TcellMLUnits.Create(CellMLModel, Nil, 'U2');
         U3 := TcellMLUnits.Create(CellMLModel, Nil, 'U3');

         Try
            U1.AddUnitElement([TCellMLUnit.Create(Nil, 'metre',  SimplifyNb(0), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);
            U1.AddUnitElement([TCellMLUnit.Create(Nil, 'minute',  SimplifyNb(0), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

            U2.AddUnitElement([TCellMLUnit.Create(Nil, 'kilogram',  SimplifyNb(-5), SimplifyNb(1), SimplifyNb(1), SimplifyNb(0))]);

            U3.AddUnitElement([TCellMLUnit.Create(Nil, 'kilogram',  SimplifyNb(-6), SimplifyNb(-1), SimplifyNb(1), SimplifyNb(0))]);

            UTemp1 := U2.Times(U3);
            UTemp2 := UTemp1.Exponentiates(-1);
            UTemp3 := U1.Times(UTemp1);

            Units := UTemp3.Times(UTemp2);

            Try
               CheckEquals(Integer(etExact), Integer(OrigUnits.EquivalenceType(Units)), 'TCellMLAPIMiscellaneousTests.Test03 - Test03 - Units definitions are not exactly equivalent');
            Finally
               Units.Free;
            End;
         Finally
            OrigUnits.Free;
            OUTemp.Free;
         End;
      Finally
//         MinuteUnits.Free;
// Note: dealt with when freeing up "CellMLModel"...
         OU1.Free;
         OU2.Free;
      End;
   Finally
      CellMLModel.Free;
   End;
End;

//==============================================================================

End.

//==============================================================================
// End of file
//==============================================================================

