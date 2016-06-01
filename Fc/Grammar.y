{
module Fc.Grammar where
import Fc.Lexer
import Control.Monad.Writer
import Control.Monad.State
import qualified Fc.Tabla as T
import qualified Fc.MyState as MyState
import Fc.Datas (TypeData(..))
import Data.Maybe
}

%monad { State MyState.ParseState } { (>>=) } { return }
%name parsefc All
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

ManyTypes : ManyTypes ',' Types    {% modify $ MyState.empilaAT }
    | pretype Types {% modify $ MyState.empilaAT}

pretype : {% modify $ MyState.ponAT}

asteriscos : asteriscos '*' { $1 + 1 }
    | '*' { 1 }

basicType : type {% modify $ MyState.modifAType $ const $ case ((\(s,_,_)->s) $1) of
  "int" ->  TInt
  "float" ->  TFloat
  "bool" ->  TBool
  "void" ->  TVoid
  "char" ->  TChar
  }

Types : basicType                                      {}
    | unsigned basicType                               {% modify $ MyState.modifAType TUnsigned }
    | struct var                                       {% modify $ checkExist2 $2 }
    | union var                                        {% modify $ checkExist2 $2 }
    | functionType '(' Types ')' '(' ManyTypes ')'     {%
  modify $ (\s -> MyState.modifAType (const $ FunLoc (last $ MyState.pilaAT s) (init $ MyState.pilaAT s)) s)
  }
    | functionType '(' Types ')' '(' ')'               {%
  modify $ (\s -> MyState.modifAType (const $ FunLoc (MyState.getAType s) []) s)
  }
    | '(' Types asteriscos ')'                         {% modify $ MyState.modifAType $ unasteriscos $3 }

arrays : arrays '[' int ']' { (.) $1 (TArray ((\(i,_,_)->i) $3)) }
  | '[' int ']' { TArray ((\(i,_,_)->i) $2) }

variables : variables ',' var          {% modify $ (\s -> modifTabla $3 (MyState.getAType s) s) }
    | variables ',' var arrays    {% modify $ (\s -> modifTabla $3 ($4 $ MyState.getAType s) s) }
    | variables ',' asteriscos var     {% modify $ (\s -> modifTabla $4 (unasteriscos $3 $ MyState.getAType s) s) }
    | var                              {% modify $ (\s -> modifTabla $1 (MyState.getAType s) s) }
    | asteriscos var                   {% modify $ (\s -> modifTabla $2 (unasteriscos $1 $ MyState.getAType s) s) }
    | var arrays                  {% modify $ (\s -> modifTabla $1 ($2 $ MyState.getAType s) s) }

VarDeclaration : Types variables ';'  { }

StructVar : var {% modify $ modifTabla $1 (TStruct $ (\(s,_,_)->s) $1) }
Struct : struct StructVar Block2 VarDeclarations Block3 ';' {}

UnionVar : var {% modify $ modifTabla $1 (TUnion $ (\(s,_,_)->s) $1) }
Union : union UnionVar Block2 VarDeclarations Block3 ';' {}

Funcion : Types VarVar FuncionP2 Parametros ')' registra2 Block FuncionP3   {  }
    | Types VarVar registra Block  {  }

registra : '(' ')' {% modify $ (\s -> modifTabla (MyState.auxS s) (FunGlob (last $ MyState.pilaAT s) (init $ MyState.pilaAT s)) s) }

registra2 : {% do
  modify $ MyState.alterSimT T.exitScope
  modify $ (\s -> modifTabla (MyState.auxS s) (FunGlob (last $ MyState.pilaAT s) (init $ MyState.pilaAT s)) s) 
  modify $ MyState.alterSimT $ T.enterN 0
  }

VarVar : var {% do modify $ MyState.ponAT; modify $ MyState.newAusS $1}
 
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

Parametro : ref Types var      {% do
  modify $ (\s -> modifTabla $3 (TRef $ MyState.getAType s) s)
  modify $ MyState.modifAType TRef
  modify $ MyState.empilaAT }
    | Types var                {% do
  modify $ (\s -> modifTabla $2 (MyState.getAType s) s)
  modify $ MyState.empilaAT }
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

unasteriscos 1 = TRef
unasteriscos i = TRef.(unasteriscos (i-1))

parseError :: [Token] -> a
parseError s = error ("Parse error in " ++ (lineCol $ head s))

modifTabla (s,l,c) tipo estado = if ((MyState.querrySimT (T.localLookup s) estado) == Nothing)
  then
    --if ((MyState.querrySimT (T.lookup s) estado) /= Just FunGlob)
      --then
        MyState.alterSimT (T.insert s tipo) estado
      --else
        --MyState.addError estado ("Existe una funcion global de nombre "++s++", no se puede declarar de nuevo. Linea: "++(show l)++" Columna: "++(show c))
  else 
    MyState.addError estado ("Ya se encuentra declarada la variable "++s++" en este Alcance. Linea: "++(show l)++" Columna: "++(show c))

checkExist (s,l,c) estado = if (isNothing $ MyState.querrySimT (T.lookup s) estado)
  then
    MyState.addError estado ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))
  else 
    estado

checkExist2 (s,l,c) estado = if (isNothing resul)
  then
    MyState.addError estado ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))
  else 
    MyState.modifAType (const (fromJust resul)) estado
  where
    resul = MyState.querrySimT (T.lookup s) estado

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