{
module Fc.Grammar where
import Fc.Lexer
import Control.Monad.Writer
import Control.Monad.State
import qualified Fc.Tabla as T
}

%monad { State (T.Tabla String TypeData,[String],[String]) } { (>>=) } { return }
%name parsefc
%tokentype { Token }
%error { parseError }

%token
    --sym { Sym $$ } 
    var { Var $$ } 
    int { Int $$ }
    float { Float $$ }
    type { Type $$ }
    functionType { Function $$ }
    bool { Bool $$ }
    ref { Ref $$ }
    if { If $$ }
    while { While $$ }
    for { For $$ }
    else { Else $$ }
    union { Union $$ }
    struct { Struct $$ }
    unsigned { Unsigned $$ }
    continue { Continue $$ }
    break { Break $$ }
    return { Return $$ }
    read { Read $$ }
    write { Write $$ }
    str { Str $$ }
    '<<' { ShiftL $$ }
    '>>' { ShiftR $$ }
    '.' { Punto $$ }
    '==' { Igual $$ }
    '>=' { MayorIgual $$ }
    '<=' { MenorIgual $$ }
    '!=' { Diferente $$ }
    '||' { BoolOr $$ }
    '&&' { BoolAnd $$ }
    '=' { Asignacion $$ }
    '+' { Mas $$ }
    '-' { Menos $$ }
    '*' { Por $$ }
    '/' { Entre $$ }
    '(' { OpenParen $$ }
    ')' { CloseParen $$ }
    '{' { OpenCurly $$ }
    '}' { CloseCurly $$ }
    ';' { PuntoComa $$ }
    '%' { Modulo $$ }
    '<' { Menor $$ }
    '>' { Mayor $$ }
    '&' { BitAnd $$ }
    '|' { BitOr $$ }
    '[' { OpenBrackets $$ }
    ']' { CloseBrackets $$ }
    ',' { Coma $$ }

%left '||'
%left '&&'
%left '|'
%left '&'
%nonassoc '==' '!=' 
%nonassoc '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%left NEG
%left Deref
%right '.'

%%

All : All Funcion                  { }
    | Funcion                      { }
    | All Struct                   { }
    | Struct                       { }
    | All Union                    { }
    | Union                        { }
    | All VarDeclaration           { }
    | VarDeclaration               { }

VarDeclarations : VarDeclarations VarDeclaration  { }
    | VarDeclaration  { }

ManyTypes : ManyTypes ',' Types    { $3:$1 }
    | Types { [$1] }

Types : type { }
    | unsigned type { {-TUnsigned $2-} }
    | struct var {% modify $ checkExist $2 }
    | union var {% modify $ checkExist $2 }
    | functionType '(' Types ')' '(' ManyTypes ')' { }
    | functionType '(' Types ')' '(' ')' { }
    | Types '*' { {-TRef $1-} }

VarDeclaration : Types var ';'  {% modify $ modifTabla $2 TInt }
    | Types var '[' Exp ']' ';'  {% modify $ modifTabla $2 TInt }

Struct : struct var Block2 VarDeclarations Block3 ';' {% modify $ modifTabla $2 (TStruct $ (\(s,_,_)->s) $2) }

Union : union var Block2 VarDeclarations Block3 ';' {% modify $ modifTabla $2 (TUnion $ (\(s,_,_)->s) $2)  }

Funcion : Types VarVar FuncionP2 Parametros ')' Block FuncionP3   {  }
    | Types VarVar '(' ')' Block  {  }

VarVar : var {% modify $ modifTabla $1 FunGlob}
 
FuncionP2 : '(' {% modify (\(t,s,s2)->(T.enterScope t,s,s2))}
FuncionP3 : {% modify (\(t,s,s2)->(T.exitScope t,s,s2))}

Block : Block2 Instrucciones Block3 {}
Block2 : '{' {% modify (\(t,s,s2)->(T.enterScope t,s,s2))}
Block3 : '}' {% modify (\(t,s,s2)->(T.exitScope t,s,s2))}

Instruccion : Block { }
    | For { }
    | While { }
    | If { }
    | Asignacion ';' { }
    | Exp ';' { }
    | break ';' { }
    | continue ';' { }
    | return ';' { }
    | return Exp ';' { }
    | read var ';' {% modify $ checkExist $2 }
    | write Exp ';' { }
    | ';' { }

For : for '(' Instruccion ';' Exp ';' Instruccion ')' Instruccion { }
While : while '(' Exp ')' Instruccion { }

If : if '(' Exp ')' Block { }
    | if '(' Exp ')' Block else Instruccion { }

Asignacion : var '=' Exp {% modify $ checkExist $1 }
    | var '[' Exp ']' '=' Exp {% modify $ checkExist $1 }

LLamada : var '(' ParametrosIn ')' {% modify $ checkExist $1 }
    | var '(' ')' {% modify $ checkExist $1 }

ParametrosIn : ParametrosIn ',' Exp { }
    | Exp { }

Instrucciones : Instrucciones Instruccion { }
    | Instruccion { }
    | Instrucciones VarDeclaration { }
    | VarDeclaration { }

Parametro : ref Types var      {% modify $ modifTabla $3 TInt }
    | Types var                {% modify $ modifTabla $2 TInt }
Parametros : Parametros ',' Parametro { }
    | Parametro              { }

AnomFun : '(' Types ')' '(' Parametros ')' '{' Instrucciones '}'  { }
    | '(' Types ')' '(' ')' '{' Instrucciones '}'  { }

Atom : LLamada { }
    | int { }
    | float { }
    | bool { }
    | str {% modify $ (\ins (t,s,s2)->if(not $ elem ins s2)then (t,s,ins:s2) else (t,s,s2)) $ (\(s,_,_)->s) $1}
    | var {% modify $ checkExist $1 }
    | AnomFun { }

Exp : Exp '+' Exp            { }
    | Exp '-' Exp            { }
    | Exp '*' Exp            { }
    | Exp '/' Exp            { }
    | Exp '<<' Exp            { }
    | Exp '>>' Exp            { }
    | Exp '<' Exp            { }
    | Exp '<=' Exp            { }
    | Exp '>' Exp            { }
    | Exp '>=' Exp            { }
    | Exp '==' Exp            { }
    | Exp '!=' Exp            { }
    | Exp '&' Exp            { }
    | Exp '%' Exp            { }
    | Exp '|' Exp            { }
    | Exp '&&' Exp            { }
    | Exp '||' Exp            { }
    | Exp '.' var            { }
    | '(' Exp ')'            { }
    | '-' Exp %prec NEG      { }
    | '*' Exp %prec Deref      { }
    | Atom                    { }

{

parseError :: [Token] -> a
parseError s = error ("Parse error in " ++ (lineCol $ head s))

data TypeData = FunGlob | FunLoc [TypeData] [TypeData] | TInt | TFloat| TBool | 
  TVoid | TChar | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Show,Eq)

modifTabla (s,l,c) tipo (tabla,errores,strings) = if ((T.localLookup s tabla) == Nothing)
  then
    if ((T.lookup s tabla) /= Just FunGlob)
      then
        (T.insert s tipo tabla ,errores,strings)
      else
        (tabla,("Existe una funcion global de nombre "++s++", no se puede declarar de nuevo. Linea: "++(show l)++" Columna: "++(show c)):errores,strings)
  else 
    (tabla,("Ya se encuentra declarada la variable "++s++" en este Alcance. Linea: "++(show l)++" Columna: "++(show c)):errores,strings)

checkExist (s,l,c) (tabla,errores,strings) = if ((T.lookup s tabla) == Nothing)
  then
    (tabla,("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c)):errores,strings)
  else 
    (tabla,errores,strings)

{-
{ Plus $1 $3 }
{ Minus $1 $3 }
{ Times $1 $3 }
{ Div $1 $3 }
{ $2 }
{ Negate $2 }
 { (\(n,_,_)->Val n) $1 }

data Exp = Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Brack Exp
         | Val Int
         deriving Show
-}

}