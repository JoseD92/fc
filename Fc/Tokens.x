{
module Fc.Lexer where
}

%wrapper "posn"
$digit = [0-9]
$octdig = [0-7]
$hexdig = [0-9A-Fa-f]
$alpha = [a-zA-Z]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\\\ ]
$graphic = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/\\\ 0-9a-zA-Z]

@string = \" (([^\"])* (\\ \")* )* \"
@nostring = \" ([^\"])*
@entero10 = [0-9][0-9]*
@float1 = @entero10 "." @entero10 [eE] [\-\+] @entero10 | @entero10 [eE] [\-\+] @entero10
@float2 = @entero10 "." @entero10
@float3 = @entero10 "f"

@reservedid = 
	if|while|for|else|union|struct|unsigned|continue|break|return|
    function|int|float|bool|void|char|read|write|True|False|ref

@operators =
    "=="|">="|"<="|"!="|"||"|"&&"|">>"|"<<"|"="|"+"|"-"|"*"|"/"|
    "("|")"|"{"|"}"|";"|"%"|"<"|">"|"&"|"|"|"["|"]"|","|"."

@variable = $alpha [$alpha $digit \_]*

tokens :-

$white+ ; --espacios
"//".* ; --comentarios de resto de linea
--Palabras reservadas
@reservedid { \(AlexPn _ l c) s -> case s of
  "ref" ->  Ref (s,l,c)
  "if" ->  If (s,l,c)
  "while" ->  While (s,l,c)
  "for" ->  For (s,l,c)
  "else" ->  Else (s,l,c)
  "union" ->  Union (s,l,c)
  "struct" ->  Struct (s,l,c)
  "unsigned" ->  Unsigned (s,l,c)
  "continue" ->  Continue (s,l,c)
  "break" ->  Break (s,l,c)
  "return" ->  Return (s,l,c)
  "function" ->  Function (s,l,c)
  "int" ->  Type (s,l,c)
  "float" ->  Type (s,l,c)
  "bool" ->  Type (s,l,c)
  "void" ->  Type (s,l,c)
  "char" ->  Type (s,l,c)
  "read" ->  Read (s,l,c)
  "write" ->  Write (s,l,c)
  "True" ->  Bool (s,l,c)
  "False" ->  Bool (s,l,c)
}
@float1 { \(AlexPn _ l c) s -> let val = (read s :: Float) in
  if (val /= 1/0) then if (val /= 0) then Float (val,l,c)
    else let char = head $ dropWhile (\c->c=='0'||c=='.') s in
      if (char == 'E' || char == 'e') then Float (0,l,c)
      else Error (s,l,c) "Underflow"
  else Error (s,l,c) "Overflow"
}
@float2 { \(AlexPn _ l c) s -> let val = (read s :: Float) in
  if (val /= 1/0) then if (val /= 0) then Float (val,l,c)
    else let char = dropWhile (\c->c=='0'||c=='.') s in
      if (char == []) then Float (0,l,c)
      else Error (s,l,c) "Underflow"
  else Error (s,l,c) "Overflow"
}
@float3 { \(AlexPn _ l c) ss -> let s = init ss in let val = (read s :: Float) in
  if (val /= 1/0) then Float (val,l,c)
  else Error (s,l,c) "Overflow"
}

@entero10 { \(AlexPn _ l c) s -> let val = (read s :: Integer) in
  if (val <= 2147483647 && val >= -2147483648) then Int ((read s)::Int,l,c)
  else Error (s,l,c) "Overflow"
} -- numeros base 10

@operators { \(AlexPn _ l c) s -> case s of
  "==" ->  Igual (s,l,c)
  ">=" ->  MayorIgual (s,l,c)
  "<=" ->  MenorIgual (s,l,c)
  "!=" ->  Diferente (s,l,c)
  "||" ->  BoolOr (s,l,c)
  "&&" ->  BoolAnd (s,l,c)
  "<<" ->  ShiftL (s,l,c)
  ">>" ->  ShiftR (s,l,c)
  "." ->  Punto (s,l,c)
  "=" ->  Asignacion (s,l,c)
  "+" ->  Mas (s,l,c)
  "-" ->  Menos (s,l,c)
  "*" ->  Por (s,l,c)
  "/" ->  Entre (s,l,c)
  "(" ->  OpenParen (s,l,c)
  ")" ->  CloseParen (s,l,c)
  "{" ->  OpenCurly (s,l,c)
  "}" ->  CloseCurly (s,l,c)
  ";" ->  PuntoComa (s,l,c)
  --":" ->  DosPuntos (s,l,c)
  "%" ->  Modulo (s,l,c)
  "<" ->  Menor (s,l,c)
  ">" ->  Mayor (s,l,c)
  "&" ->  BitAnd (s,l,c)
  "|" ->  BitOr (s,l,c)
  "[" ->  OpenBrackets (s,l,c)
  "]" ->  CloseBrackets (s,l,c)
  "," ->  Coma (s,l,c)
}
@variable { \(AlexPn _ l c) s -> Var (s,l,c) }
@nostring { \(AlexPn _ l c) s -> Error (s,l,c) "String no cerrado" }
@string { \(AlexPn _ l c) s -> Str (s,l,c) }
. { \(AlexPn _ l c) s -> Error (s,l,c) "Simbolo no reconocido" }


{
-- Each action has type :: String -> Token
-- The token type:

type Lc = ([Char],Int,Int)
giveLC (_,l,c) = "Linea: " ++ (show l) ++ " Columna: " ++ (show c)

data Token = Sym Lc | 
    Var Lc | 
    Int (Int,Int,Int) |
    Float (Float,Int,Int) |
    Type Lc |
    Function Lc |
    Bool Lc |
    Ref Lc |
    If Lc |
    While Lc |
    For Lc |
    Else Lc |
    Union Lc |
    Struct Lc |
    Unsigned Lc |
    Continue Lc |
    Break Lc |
    Return Lc |
    Read Lc |
    Write Lc |
    Str Lc |
    Igual Lc |
    MayorIgual Lc |
    MenorIgual Lc |
    Diferente Lc |
    BoolOr Lc |
    BoolAnd Lc |
    ShiftR Lc |
    ShiftL Lc |
    Punto Lc |
    Asignacion Lc |
    Mas Lc |
    Menos Lc |
    Por Lc |
    Entre Lc |
    OpenParen Lc |
    CloseParen Lc |
    OpenCurly Lc |
    CloseCurly Lc |
    PuntoComa Lc |
    --DosPuntos Lc |
    Modulo Lc |
    Menor Lc |
    Mayor Lc |
    BitAnd Lc |
    BitOr Lc |
    OpenBrackets Lc |
    CloseBrackets Lc |
    Coma Lc |
    Error Lc String
  deriving (Eq)

lineCol :: Token -> String
lineCol (Sym lc) =  giveLC lc
lineCol (Var lc) =  giveLC lc
lineCol (Int (_,l,c)) = "Linea: " ++ (show l) ++ " Columna: " ++ (show c)
lineCol (Float (_,l,c)) = "Linea: " ++ (show l) ++ " Columna: " ++ (show c)
lineCol (Type lc) =  giveLC lc
lineCol (Function lc) =  giveLC lc
lineCol (Bool lc) =  giveLC lc
lineCol (Ref lc) =  giveLC lc
lineCol (If lc) =  giveLC lc
lineCol (While lc) =  giveLC lc
lineCol (For lc) =  giveLC lc
lineCol (Else lc) =  giveLC lc
lineCol (Union lc) =  giveLC lc
lineCol (Struct lc) =  giveLC lc
lineCol (Unsigned lc) =  giveLC lc
lineCol (Continue lc) =  giveLC lc
lineCol (Break lc) =  giveLC lc
lineCol (Return lc) =  giveLC lc
lineCol (Read lc) =  giveLC lc
lineCol (Write lc) =  giveLC lc
lineCol (Str lc) =  giveLC lc
lineCol (Igual lc) =  giveLC lc
lineCol (MayorIgual lc) =  giveLC lc
lineCol (MenorIgual lc) =  giveLC lc
lineCol (Diferente lc) =  giveLC lc
lineCol (BoolOr lc) =  giveLC lc
lineCol (BoolAnd lc) =  giveLC lc
lineCol (Asignacion lc) =  giveLC lc
lineCol (Mas lc) =  giveLC lc
lineCol (Menos lc) =  giveLC lc
lineCol (Por lc) =  giveLC lc
lineCol (Entre lc) =  giveLC lc
lineCol (OpenParen lc) =  giveLC lc
lineCol (CloseParen lc) =  giveLC lc
lineCol (OpenCurly lc) =  giveLC lc
lineCol (CloseCurly lc) =  giveLC lc
lineCol (PuntoComa lc) =  giveLC lc
lineCol (Modulo lc) =  giveLC lc
lineCol (Menor lc) =  giveLC lc
lineCol (Mayor lc) =  giveLC lc
lineCol (BitAnd lc) =  giveLC lc
lineCol (BitOr lc) =  giveLC lc
lineCol (OpenBrackets lc) =  giveLC lc
lineCol (CloseBrackets lc) =  giveLC lc
lineCol (Coma lc) =  giveLC lc
lineCol (ShiftR lc) =  giveLC lc
lineCol (ShiftL lc) =  giveLC lc
lineCol (Punto lc) =  giveLC lc

instance Show Token where
  show (Sym (s,l,c)) = "Token: Simbolo"++"\n"++"String: "++s++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Var (s,l,c)) = "Token: variable"++"\n"++"String: "++s++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Int (s,l,c)) = "Token: numero"++"\n"++"Value: "++(show s)++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Float (s,l,c)) = "Token: flotante"++"\n"++"Value: "++(show s)++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Type (s,l,c)) = "Token: tipo"++"\n"++"String: "++s++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Function (s,l,c)) = "Token: Function"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Str (s,l,c)) = "Token: String"++"\n"++"String: "++s++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Error (s,l,c) d) = "Error - No es parte del lenguaje"++"\n"++"Descripcion: "++d++"\n"++"String: "++s++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Bool (s,l,c)) = "Token: Bool"++"\n"++"String: "++s++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Ref (s,l,c)) = "Token: Ref"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (ShiftR (s,l,c)) = "Token: ShiftR"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (ShiftL (s,l,c)) = "Token: ShiftL"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Punto (s,l,c)) = "Token: Punto"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (If (s,l,c)) = "Token: If"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (While (s,l,c)) = "Token: While"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (For (s,l,c)) = "Token: For"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Else (s,l,c)) = "Token: Else"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Union (s,l,c)) = "Token: Union"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Struct (s,l,c)) = "Token: Struct"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Unsigned (s,l,c)) = "Token: Unsigned"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Continue (s,l,c)) = "Token: Continue"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Break (s,l,c)) = "Token: Break"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Return (s,l,c)) = "Token: Return"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Read (s,l,c)) = "Token: Read"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Write (s,l,c)) = "Token: Write"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Igual (s,l,c)) = "Token: Igual"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (MayorIgual (s,l,c)) = "Token: MayorIgual"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (MenorIgual (s,l,c)) = "Token: MenorIgual"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Diferente (s,l,c)) = "Token: Diferente"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (BoolOr (s,l,c)) = "Token: BoolOr"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (BoolAnd (s,l,c)) = "Token: BoolAnd"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Asignacion (s,l,c)) = "Token: Asignacion"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Mas (s,l,c)) = "Token: Mas"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Menos (s,l,c)) = "Token: Menos"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Por (s,l,c)) = "Token: Por"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Entre (s,l,c)) = "Token: Entre"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (OpenParen (s,l,c)) = "Token: OpenParen"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (CloseParen (s,l,c)) = "Token: CloseParen"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (OpenCurly (s,l,c)) = "Token: OpenCurly"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (CloseCurly (s,l,c)) = "Token: CloseCurly"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (PuntoComa (s,l,c)) = "Token: PuntoComa"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  --show (DosPuntos (s,l,c)) = "Token: DosPuntos"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Modulo (s,l,c)) = "Token: Modulo"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Menor (s,l,c)) = "Token: Menor"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Mayor (s,l,c)) = "Token: Mayor"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (BitAnd (s,l,c)) = "Token: BitAnd"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (BitOr (s,l,c)) = "Token: BitOr"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (OpenBrackets (s,l,c)) = "OpenBrackets: Read"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (CloseBrackets (s,l,c)) = "Token: CloseBrackets"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)
  show (Coma (s,l,c)) = "Token: Coma"++"\n"++"Lugar: linea "++(show l)++", columna "++(show c)

}