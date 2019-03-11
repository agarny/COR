{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: SynHighlighterCellML.pas, released 2008-11-04.
Description: Syntax Parser/Highlighter
The initial author of this file is Alan Garny.
Copyright (c) 2008, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit SynHighlighterCellML;

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkExtraInfo,
    tkIdentifier,
    tkKey,
    tkNull,
    tkUnknown);

  TRangeState = (rsUnKnown, rsExtraInfo);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 163;

type
  TSynCellMLSyn = class(TSynCustomHighlighter)
  private
    fLineRef: string;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fRange: TRangeState;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0 .. MaxKey] of TIdentFuncTableFunc;
    fExtraInfoAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func6: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func163: TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure CRProc;
    procedure LFProc;
    procedure ExtraInfoOpenProc;
    procedure ExtraInfoProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property ExtraInfoAttri: TSynHighlighterAttributes read fExtraInfoAttri write fExtraInfoAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SYNS_FilterCellML = 'CellML files (*.cellml)|*.cellml';
  SYNS_LangCellML = 'CellML';
  SYNS_AttrExtraInfo = 'ExtraInfo';

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then
            mHashTable[I] := Ord(I) - 64
          else if (I > #96) then
            mHashTable[I] := Ord(I) - 95;
        end;
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynCellMLSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[6] := Func6;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[46] := Func46;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[139] := Func139;
  fIdentFuncTable[163] := Func163;
end;

function TSynCellMLSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynCellMLSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;

function TSynCellMLSyn.Func6: TtkTokenKind;
begin
  if KeyComp('e') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func18: TtkTokenKind;
begin
  if KeyComp('def') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func22: TtkTokenKind;
begin
  if KeyComp('as') then Result := tkKey else
    if KeyComp('and') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func25: TtkTokenKind;
begin
  if KeyComp('deci') then Result := tkKey else
    if KeyComp('deka') then Result := tkKey else
      if KeyComp('in') then Result := tkKey else
        if KeyComp('abs') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func27: TtkTokenKind;
begin
  if KeyComp('pi') then Result := tkKey else
    if KeyComp('ode') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func28: TtkTokenKind;
begin
  if KeyComp('csc') then Result := tkKey else
    if KeyComp('giga') then Result := tkKey else
      if KeyComp('ln') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func30: TtkTokenKind;
begin
  if KeyComp('off') then Result := tkKey else
    if KeyComp('sec') then Result := tkKey else
      if KeyComp('mega') then Result := tkKey else
        if KeyComp('acsc') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func31: TtkTokenKind;
begin
  if KeyComp('base') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func32: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else
    if KeyComp('asec') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func33: TtkTokenKind;
begin
  if KeyComp('exa') then Result := tkKey else
    if KeyComp('map') then Result := tkKey else
      if KeyComp('ceil') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func34: TtkTokenKind;
begin
  if KeyComp('fact') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func35: TtkTokenKind;
begin
  if KeyComp('or') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func37: TtkTokenKind;
begin
  if KeyComp('csch') then Result := tkKey else
    if KeyComp('log') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func38: TtkTokenKind;
begin
  if KeyComp('tan') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func39: TtkTokenKind;
begin
  if KeyComp('acsch') then Result := tkKey else
    if KeyComp('sech') then Result := tkKey else
      if KeyComp('sel') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func40: TtkTokenKind;
begin
  if KeyComp('cos') then Result := tkKey else
    if KeyComp('atan') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func41: TtkTokenKind;
begin
  if KeyComp('cot') then Result := tkKey else
    if KeyComp('asech') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func42: TtkTokenKind;
begin
  if KeyComp('for') then Result := tkKey else
    if KeyComp('incl') then Result := tkKey else
      if KeyComp('pub') then Result := tkKey else
        if KeyComp('acos') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func43: TtkTokenKind;
begin
  if KeyComp('acot') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func44: TtkTokenKind;
begin
  if KeyComp('enddef') then Result := tkKey else
    if KeyComp('var') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func45: TtkTokenKind;
begin
  if KeyComp('sin') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func46: TtkTokenKind;
begin
  if KeyComp('peta') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func47: TtkTokenKind;
begin
  if KeyComp('asin') then Result := tkKey else
    if KeyComp('tanh') then Result := tkKey else
      if KeyComp('pico') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func48: TtkTokenKind;
begin
  if KeyComp('exp') then Result := tkKey else
    if KeyComp('tera') then Result := tkKey else
      if KeyComp('false') then Result := tkKey else
        if KeyComp('nano') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func49: TtkTokenKind;
begin
  if KeyComp('pref') then Result := tkKey else
    if KeyComp('atanh') then Result := tkKey else
      if KeyComp('cosh') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func50: TtkTokenKind;
begin
  if KeyComp('coth') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func51: TtkTokenKind;
begin
  if KeyComp('comp') then Result := tkKey else
    if KeyComp('kilo') then Result := tkKey else
      if KeyComp('acosh') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func52: TtkTokenKind;
begin
  if KeyComp('acoth') then Result := tkKey else
    if KeyComp('not') then Result := tkKey else
      if KeyComp('none') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func54: TtkTokenKind;
begin
  if KeyComp('sinh') then Result := tkKey else
    if KeyComp('model') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func56: TtkTokenKind;
begin
  if KeyComp('centi') then Result := tkKey else
    if KeyComp('hecto') then Result := tkKey else
      if KeyComp('init') then Result := tkKey else
        if KeyComp('asinh') then Result := tkKey else
          if KeyComp('from') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func57: TtkTokenKind;
begin
  if KeyComp('sqr') then Result := tkKey else
    if KeyComp('pow') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func59: TtkTokenKind;
begin
  if KeyComp('out') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func60: TtkTokenKind;
begin
  if KeyComp('atto') then Result := tkKey else
    if KeyComp('milli') then Result := tkKey else
      if KeyComp('xor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func63: TtkTokenKind;
begin
  if KeyComp('micro') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func64: TtkTokenKind;
begin
  if KeyComp('femto') then Result := tkKey else
    if KeyComp('expo') then Result := tkKey else
      if KeyComp('vars') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func65: TtkTokenKind;
begin
  if KeyComp('endsel') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func68: TtkTokenKind;
begin
  if KeyComp('unit') then Result := tkKey else
    if KeyComp('true') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func69: TtkTokenKind;
begin
  if KeyComp('priv') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func70: TtkTokenKind;
begin
  if KeyComp('mult') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func71: TtkTokenKind;
begin
  if KeyComp('floor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func72: TtkTokenKind;
begin
  if KeyComp('root') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func77: TtkTokenKind;
begin
  if KeyComp('endcomp') then Result := tkKey else
    if KeyComp('zetta') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func78: TtkTokenKind;
begin
  if KeyComp('sqrt') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func81: TtkTokenKind;
begin
  if KeyComp('between') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func82: TtkTokenKind;
begin
  if KeyComp('group') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func83: TtkTokenKind;
begin
  if KeyComp('yocto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func86: TtkTokenKind;
begin
  if KeyComp('yotta') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func87: TtkTokenKind;
begin
  if KeyComp('zepto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func131: TtkTokenKind;
begin
  if KeyComp('otherwise') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func139: TtkTokenKind;
begin
  if KeyComp('containment') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.Func163: TtkTokenKind;
begin
  if KeyComp('encapsulation') then Result := tkKey else Result := tkIdentifier;
end;

function TSynCellMLSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynCellMLSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynCellMLSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      '{': fProcTable[I] := ExtraInfoOpenProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynCellMLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynCellMLSyn.CRProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynCellMLSyn.LFProc;
begin
  fTokenID := tkUnknown;
  inc(Run);
end;

procedure TSynCellMLSyn.ExtraInfoOpenProc;
begin
  Inc(Run);
  fRange := rsExtraInfo;
  ExtraInfoProc;
  fTokenID := tkExtraInfo;
end;

procedure TSynCellMLSyn.ExtraInfoProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkExtraInfo;
      repeat
        if (fLine[Run] = '}') then
        begin
          Inc(Run, 1);
          fRange := rsUnKnown;
          Break;
        end;
        if not (fLine[Run] in [#0, #10, #13]) then
          Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

constructor TSynCellMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fExtraInfoAttri := TSynHighLighterAttributes.Create(SYNS_AttrExtraInfo);
  fExtraInfoAttri.Style := [fsItalic];
  fExtraInfoAttri.Foreground := clNavy;
  AddAttribute(fExtraInfoAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterCellML;
  fRange := rsUnknown;
end;

procedure TSynCellMLSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynCellMLSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine+Run));

   Inc(Run, fStringLen);

   While Identifiers[fLine[Run]] Do
      Inc(Run);
end;

procedure TSynCellMLSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynCellMLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsExtraInfo: ExtraInfoProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynCellMLSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
  else
    Result := nil;
  end;
end;

function TSynCellMLSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynCellMLSyn.GetKeyWords: string;
begin
  Result := 
    'abs,acos,acosh,acot,acoth,acsc,acsch,and,as,asec,asech,asin,asinh,ata' +
    'n,atanh,atto,base,between,case,ceil,centi,comp,containment,cos,cosh,co' +
    't,coth,csc,csch,deci,def,deka,e,encapsulation,endcomp,enddef,endsel,ex' +
    'a,exp,expo,fact,false,femto,floor,for,from,giga,group,hecto,in,incl,in' +
    'it,kilo,ln,log,map,mega,micro,milli,model,mult,nano,none,not,ode,off,o' +
    'r,otherwise,out,peta,pi,pico,pow,pref,priv,pub,root,sec,sech,sel,sin,s' +
    'inh,sqr,sqrt,tan,tanh,tera,true,unit,var,vars,xor,yocto,yotta,zepto,ze' +
    'tta';
end;

function TSynCellMLSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynCellMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynCellMLSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkExtraInfo: Result := fExtraInfoAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynCellMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynCellMLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynCellMLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z'];
end;

function TSynCellMLSyn.GetSampleSource: string;
begin
  Result := 'Sample source for: '#13#10 +
            'Syntax Parser/Highlighter';
end;

function TSynCellMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterCellML;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynCellMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangCellML;
end;

procedure TSynCellMLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynCellMLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynCellMLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynCellMLSyn);
{$ENDIF}
end.
