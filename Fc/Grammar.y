{
module Fc.Grammar where
import Fc.Lexer
import Control.Monad.State
import qualified Fc.Tabla as T
import qualified Fc.MyState as MyState
import Fc.Datas (TypeData(..),operAcc,operAccMono)
import Data.Maybe
import System.IO
import Control.Monad (when)
}

%monad { StateT MyState.ParseState IO } { (>>=) } { return }
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

All : All Funcion                  {% do
  [x,y] <- pop 2
  modify $ MyState.push (if x==TVoid && y==TVoid then TVoid else TError)
  }
    | Funcion                      { }
    | All Struct                   {% do
  [x] <- pop 1
  modify $ MyState.push (if x==TVoid then TVoid else TError)
  }
    | Struct                       {%modify $ MyState.push TVoid }
    | All Union                    {% do
  [x,y] <- pop 2
  modify $ MyState.push (if x==TVoid && y==TVoid then TVoid else TError)
  }
    | Union                        {%modify $ MyState.push TVoid }
    | All VarDeclaration           {% do
  [x,y] <- pop 2
  modify $ MyState.push (if x==TVoid && y==TVoid then TVoid else TError)
  }
    | VarDeclaration               {%modify $ MyState.push TVoid }

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
    | struct var                                       {% checkExist2 $2 }
    | union var                                        {% checkExist2 $2 }
    | functionType '(' Types ')' '(' ManyTypes ')'     {%
  modify $ (\s -> MyState.modifAType (const $ FunLoc (last $ MyState.pilaAT s) (init $ MyState.pilaAT s)) s)
  }
    | functionType '(' Types ')' '(' ')'               {%
  modify $ (\s -> MyState.modifAType (const $ FunLoc (MyState.getAType s) []) s)
  }
    | '(' Types asteriscos ')'                         {% modify $ MyState.modifAType $ unasteriscos $3 }

arrays : arrays '[' int ']' { (.) $1 (TArray ((\(i,_,_)->i) $3)) }
  | '[' int ']' { TArray ((\(i,_,_)->i) $2) }

variables : variables ',' var          {% do
  s <- get
  modifTabla $3 (MyState.getAType s) }
    | variables ',' var arrays    {% do
  s <- get
  modifTabla $3 ($4 $ MyState.getAType s) }
    | variables ',' asteriscos var     {% do
  s <- get
  modifTabla $4 (unasteriscos $3 $ MyState.getAType s) }
    | var                              {% do
  s <- get
  modifTabla $1 (MyState.getAType s) }
    | asteriscos var                   {% do
  s <- get
  modifTabla $2 (unasteriscos $1 $ MyState.getAType s) }
    | var arrays                  {% do
  s <- get
  modifTabla $1 ($2 $ MyState.getAType s) }

VarDeclaration : Types variables ';'  { }

StructVar : var {% do
  modifTabla $1 (TStruct $ (\(s,_,_)->s) $1) 
  modify $ MyState.push (TStruct $ (\(s,_,_)->s) $1) 
  }
Struct : struct StructVar Block2 VarDeclarations Block3 ';' {% do
  [x] <- pop 1
  modify $ (\s-> MyState.addus x (MyState.querrySimT (T.enterN 0) s) s) 
  }

UnionVar : var {% do
  modifTabla $1 (TUnion $ (\(s,_,_)->s) $1) 
  modify $ MyState.push (TUnion $ (\(s,_,_)->s) $1) 
  }
Union : union UnionVar Block2 VarDeclarations Block3 ';' {% do
  [x] <- pop 1
  modify $ (\s-> MyState.addus x (MyState.querrySimT (T.enterN 0) s) s) 
  }

Funcion : Types VarVar FuncionP2 Parametros ')' registra2 Block FuncionP3   {  }
    | Types VarVar registra Block  {  }

registra : '(' ')' {% do
  s <- get
  modifTabla (MyState.auxS s) (FunGlob (last $ MyState.pilaAT s) (init $ MyState.pilaAT s)) }

registra2 : {% do
  modify $ MyState.alterSimT T.exitScope
  s <- get
  modifTabla (MyState.auxS s) (FunGlob (last $ MyState.pilaAT s) (init $ MyState.pilaAT s))
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
    | Exp ';' {% do
  [x] <- pop 1
  modify $ MyState.push (if x==TError then TError else TVoid)
 }
    | break ';' {% modify $ MyState.push TVoid}
    | continue ';' {% modify $ MyState.push TVoid}
    | return ';' {% modify $ MyState.push TVoid}
    | return Exp ';' {% do
  [x] <- pop 1
  modify $ MyState.push (if x==TError then TError else TVoid)
 }
    | read var ';' {% do 
  checkExist $2 
  modify $ MyState.push TVoid
  }
    | write Exp ';' {% modify $ MyState.push TVoid} --------------------------arreglar, chequear expresion
    | ';' {% modify $ MyState.push TVoid}

For : for '(' Instruccion ';' Exp ';' Instruccion ')' Instruccion {% do
  [x,y,z,w] <- pop 4
  modify $ MyState.push (if y==TBool && x==TVoid && z==TVoid && w==TVoid then TVoid else TError)
 }
While : while '(' Exp ')' Instruccion {% do
  [x,y] <- pop 2
  modify $ MyState.push (if x==TVoid && y==TBool then TVoid else TError)
 }

If : if '(' Exp ')' Block {% do
  [x,y] <- pop 2
  modify $ MyState.push (if x==TVoid && y==TBool then TVoid else TError)
 }
    | if '(' Exp ')' Block else Instruccion {% do
  [x,y,z] <- pop 3
  modify $ MyState.push (if y==TVoid && x==TVoid && z==TBool then TVoid else TError)
 }

Asignacion : var '=' Exp {% do
  checkExist $1 
  [x] <- pop 1
  let s = (\(x,_,_)->x) $1
  modify $ (\estado->MyState.push (if x==(maybe TError id (MyState.querrySimT (T.lookup s) estado)) then TVoid else TError) estado)
  }
    | var '[' Exp ']' '=' Exp {% do
  checkExist $1 
  [x,y] <- pop 1
  let s = (\(x,_,_)->x) $1
  modify $ (\estado->MyState.push (if y==TInt && x==(maybe TError id (MyState.querrySimT (T.lookup s) estado)) then TVoid else TError) estado)
  }

LLamada : var '(' ParametrosIn ')' {% do
  l <- pop $3
  evalLlamada $1 l
    }
    | var '(' ')' {% evalLlamada $1 []}

ParametrosIn : ParametrosIn ',' Exp { 1 + $1 }
    | Exp { 1 } 

Instrucciones : Instrucciones Instruccion {%do
  [x,y] <- pop 2
  modify $ MyState.push (if x==TVoid && y==TVoid then TVoid else TError)
 }
    | Instruccion { }
    | Instrucciones VarDeclaration { }
    | VarDeclaration {% modify $ MyState.push TVoid }

Parametro : ref Types var      {% do
  s <- get
  modifTabla $3 (TRef $ MyState.getAType s)
  modify $ MyState.modifAType TRef
  modify $ MyState.empilaAT }
    | Types var                {% do
  s <- get
  modifTabla $2 (MyState.getAType s)
  modify $ MyState.empilaAT }
Parametros : Parametros ',' Parametro { }
    | Parametro              { }

AnomFun : '(' Types ')' '(' Parametros ')' '{' Instrucciones '}'  { }
    | '(' Types ')' '(' ')' '{' Instrucciones '}'  { }

arr : arr '[' Exp ']' {%do
  [x,y] <- pop 2
  modify $ MyState.push (if y==TInt&&x==TInt then TInt else TError)
  modify $ (\estado->MyState.asingFun (ayudaArreglo.(MyState.getFun estado)) estado)
  }
    | '[' Exp ']' {%do
  [x] <- pop 1
  modify $ MyState.push (if x==TInt then TInt else TError)
  modify $ MyState.asingFun ayudaArreglo
  }

Atom : LLamada { }
    | int { % modify $ MyState.push TInt}
    | float { % modify $ MyState.push TFloat}
    | bool { % modify $ MyState.push TBool}
    | str {% do
  let s = (\(x,_,_)->x) $1
  modify $ MyState.addString s 
  modify $ MyState.push (TArray (length s) TChar)
          }
    | var {% do
  let s = (\(x,_,_)->x) $1
  checkExist $1 
  modify $ (\estado->MyState.push (maybe TError id (MyState.querrySimT (T.lookup s) estado)) estado)
          }
    | var arr {% do
  let s = (\(x,_,_)->x) $1
  checkExist $1 
  [x] <- pop 1
  modify $ (\estado->MyState.push (if x==TError then TError else (maybe TError (MyState.getFun estado) (MyState.querrySimT (T.lookup s) estado))) estado)
          }
    | AnomFun { }

Exp : Exp '+' Exp            {% twoOperators "+" }
    | Exp '-' Exp            {% twoOperators "-" }
    | Exp '*' Exp            {% twoOperators "*" }
    | Exp '/' Exp            {% twoOperators "/" }
    | Exp '<<' Exp           {% twoOperators "<<" }
    | Exp '>>' Exp           {% twoOperators ">>" }
    | Exp '<' Exp            {% twoOperators "<" }
    | Exp '<=' Exp           {% twoOperators "<=" }
    | Exp '>' Exp            {% twoOperators ">" }
    | Exp '>=' Exp           {% twoOperators ">=" }
    | Exp '==' Exp           {% twoOperators "==" }
    | Exp '!=' Exp           {% twoOperators "!=" }
    | Exp '&' Exp            {% twoOperators "&" }
    | Exp '%' Exp            {% twoOperators "%" }
    | Exp '|' Exp            {% twoOperators "|" }
    | Exp '&&' Exp           {% twoOperators "&&" }
    | Exp '||' Exp           {% twoOperators "||" }
    | Exp '.' var            {% do
  [x] <- pop 1
  let s = (\(x,_,_)->x) $3
  if (ayudasu x) then modify $ MyState.push TError
  else do
    estado <- get
    let maybeField = maybe Nothing (T.localLookup s) (MyState.buscasusymt x estado)
    modify $ MyState.push $ maybe TError id maybeField
 }
    | '(' Exp ')'            { }
    | '-' Exp %prec NEG      {% oneOperators "||" }
    | '*' Exp %prec Deref    {% do
  [x] <- pop 1
  modify $ MyState.push (ayudaApuntador x)
 }
    | Atom                    { }

{

errStrPut = hPutStrLn stderr

printError = lift.errStrPut

sprint = lift.putStrLn

unasteriscos 1 = TRef
unasteriscos i = TRef.(unasteriscos (i-1))

parseError :: [Token] -> a
parseError s = error ("Parse error in " ++ (lineCol $ head s))

ayudaApuntador (TRef x) = x
ayudaApuntador _ = TError

ayudaArreglo (TArray _ x) = x
ayudaArreglo _ = TError

ayudasu (TStruct _) = False
ayudasu (TUnion _) = False
ayudasu _ = True

pop :: Int -> StateT MyState.ParseState IO [TypeData]
pop x = do
  estado <- get
  modify $ MyState.typePilaOperate (drop x)
  return (take x $ MyState.typePila estado)

twoOperators :: String -> StateT MyState.ParseState IO ()
twoOperators o = do
  [x,y] <- pop 2
  modify $ MyState.push (operAcc x o y)

oneOperators :: String -> StateT MyState.ParseState IO ()
oneOperators o = do
  [x] <- pop 1
  modify $ MyState.push (operAccMono o x)

evalLlamada :: (String,Int,Int) -> [TypeData] -> StateT MyState.ParseState IO ()
evalLlamada inTok param = do 
  let s = (\(x,_,_)->x) inTok
  checkExist inTok
  modify $ (\estado->MyState.push (maybe TError help (MyState.querrySimT (T.lookup s) estado)) estado)
  where
    help (FunGlob returnT tl) = if tl==param then returnT else TError
    help (FunLoc returnT tl) = if tl==param then returnT else TError

modifTabla (s,l,c) tipo = do
  estado <- get
  if ((MyState.querrySimT (T.localLookup s) estado) == Nothing)
  then
    --if ((MyState.querrySimT (T.lookup s) estado) /= Just FunGlob)
      --then
        modify $ MyState.alterSimT (T.insert s tipo)
      --else
        --MyState.addError estado ("Existe una funcion global de nombre "++s++", no se puede declarar de nuevo. Linea: "++(show l)++" Columna: "++(show c))
  else 
    printError ("Ya se encuentra declarada la variable "++s++" en este Alcance. Linea: "++(show l)++" Columna: "++(show c))

checkExist (s,l,c) = do
  estado <- get
  when (isNothing $ MyState.querrySimT (T.lookup s) estado) $ do
    printError ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))

checkExist2 (s,l,c) = do
  estado <- get
  if (isNothing $ resul estado)
  then
    printError ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))
  else 
    modify $ MyState.modifAType (const (fromJust (resul estado)))
  where
    resul estado = MyState.querrySimT (T.lookup s) estado

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