{
module Fc.Grammar where
import Fc.Lexer
import Control.Monad.Writer
import Control.Monad.State
import qualified Fc.Tabla as T
import qualified Fc.MyState as MyState
import Fc.Datas (TypeData(..))
}

%monad { State MyState.ParseState } { (>>=) } { return }
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

asteriscos : asteriscos '*' { $1 + 1 }
    | '*' { 1 }

Types : type { }
    | unsigned type { {-TUnsigned $2-} }
    | struct var {% modify $ checkExist $2 }
    | union var {% modify $ checkExist $2 }
    | functionType '(' Types ')' '(' ManyTypes ')' { }
    | functionType '(' Types ')' '(' ')' { }
    | '(' Types asteriscos ')' { {-TRef $1-} }

variables : variables ',' var          {% modify $ modifTabla $3 TInt }
    | variables ',' var '[' int ']'    {% modify $ modifTabla $3 TInt }
    | variables ',' '*' var            {% modify $ modifTabla $4 TInt }
    | var                              {% modify $ modifTabla $1 TInt }
    | '*' var                          {% modify $ modifTabla $2 TInt }
    | var '[' int ']'                  {% modify $ modifTabla $1 TInt }

VarDeclaration : Types variables ';'  { }

Struct : struct var Block2 VarDeclarations Block3 ';' {% modify $ modifTabla $2 (TStruct $ (\(s,_,_)->s) $2) }

Union : union var Block2 VarDeclarations Block3 ';' {% modify $ modifTabla $2 (TUnion $ (\(s,_,_)->s) $2)  }

Funcion : Types VarVar FuncionP2 Parametros ')' Block FuncionP3   {  }
    | Types VarVar '(' ')' Block  {  }

VarVar : var {% modify $ modifTabla $1 FunGlob}
 
FuncionP2 : '(' {% modify $ MyState.alterSimT T.enterScope}
FuncionP3 : {% modify $ MyState.alterSimT T.exitScope}

Block : Block2 Instrucciones Block3 {}
Block2 : '{' {% modify $ MyState.alterSimT T.enterScope}
Block3 : '}' {% modify $ MyState.alterSimT T.exitScope}

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
    | str {% modify $ MyState.addString $ (\(s,_,_)->s) $1}
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

modifTabla (s,l,c) tipo estado = if ((MyState.querrySimT (T.localLookup s) estado) == Nothing)
  then
    if ((MyState.querrySimT (T.lookup s) estado) /= Just FunGlob)
      then
        MyState.alterSimT (T.insert s tipo) estado
      else
        MyState.addError estado ("Existe una funcion global de nombre "++s++", no se puede declarar de nuevo. Linea: "++(show l)++" Columna: "++(show c))
  else 
    MyState.addError estado ("Ya se encuentra declarada la variable "++s++" en este Alcance. Linea: "++(show l)++" Columna: "++(show c))

checkExist (s,l,c) estado = if ((MyState.querrySimT (T.lookup s) estado) == Nothing)
  then
    MyState.addError estado ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))
  else 
    estado

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