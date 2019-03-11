TSynCellMLSyn   { Name of the class }
tk              { Identifier prefix }
Sensitive '_', 'a'..'z', 'A'..'Z':: '_', '0'..'9', 'a'..'z', 'A'..'Z'::

TOKENTYPES
Identifier
ExtraInfo Style=[fsItalic]|Foreground=clNavy
Key Style=[fsBold]
|><|

KEYS
model 
as
unit
from
base
enddef
comp
endcomp
ode
sel
endsel
case
otherwise
group
containment
encapsulation
for
incl
map
between
vars
def
var
pref
expo
mult
off
init
pub
priv
yotta
zetta
exa
peta
tera
giga
mega
kilo
hecto
deka
deci
centi
milli
micro
nano
pico
femto
atto
zepto
yocto
none
in
out
pow
sqr
root
sqrt
abs
exp
ln
log
floor
ceil
fact
and
or
xor
not
sin
cos
tan
sec
csc
cot
sinh
cosh
tanh
sech
csch
coth
asin
acos
atan
asec
acsc
acot
asinh
acosh
atanh
asech
acsch
acoth
true
false
pi
e
|><|

CHARS
'A'..'Z', 'a'..'z', '_':: Ident
BeginProc
   fTokenID := IdentKind((fLine+Run));

   Inc(Run, fStringLen);

   While Identifiers[fLine[Run]] Do
      Inc(Run);
EndProc
|><|

ENCLOSEDBY
ExtraInfo,ExtraInfo,{,},MultiLine
|><|

