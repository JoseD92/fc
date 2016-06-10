{
module Fc.Grammar where
import Fc.Lexer
import Control.Monad.State
import qualified Fc.Tabla as T
import Fc.MyState
import Fc.Datas (TypeData(..),operAcc,operAccMono)
import Data.Maybe
import System.IO
import Control.Monad (when)
}

%monad { StateT ParseState IO } { (>>=) } { return }
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
  modify $ push (if x==TVoid && y==TVoid then TVoid else TError)
  }
    | Funcion                      { }
    | All Struct                   {% do
  [x] <- pop 1
  modify $ push (if x==TVoid then TVoid else TError)
  }
    | Struct                       {%modify $ push TVoid }
    | All Union                    {% do
  [x,y] <- pop 2
  modify $ push (if x==TVoid && y==TVoid then TVoid else TError)
  }
    | Union                        {%modify $ push TVoid }
    | All VarDeclaration           {% do
  [x,y] <- pop 2
  modify $ push (if x==TVoid && y==TVoid then TVoid else TError)
  }
    | VarDeclaration               {%modify $ push TVoid }

VarDeclarations : VarDeclarations VarDeclaration  { }
    | VarDeclaration  { }

ManyTypes : ManyTypes ',' Types    {% modify $ empilaAT }
    | pretype Types {% modify $ empilaAT}

pretype : {% modify $ ponAT}

asteriscos : asteriscos '*' { $1 + 1 }
    | '*' { 1 }

basicType : type {% modify $ modifAType $ const $ case ((\(s,_,_)->s) $1) of
  "int" ->  TInt
  "float" ->  TFloat
  "bool" ->  TBool
  "void" ->  TVoid
  "char" ->  TChar
  }

Types : basicType                                      {}
    | unsigned basicType                               {% modify $ modifAType TUnsigned }
    | struct var                                       {% checkExist2 $2 }
    | union var                                        {% checkExist2 $2 }
    | functionType '(' Types ')' '(' ManyTypes ')'     {%
  modify $ (\s -> modifAType (const $ FunLoc (last $ pilaAT s) (init $ pilaAT s)) s)
  }
    | functionType '(' Types ')' '(' ')'               {%
  modify $ (\s -> modifAType (const $ FunLoc (getAType s) []) s)
  }
    | '(' Types asteriscos ')'                         {% modify $ modifAType $ unasteriscos $3 }

arrays : arrays '[' int ']' { (.) $1 (TArray ((\(i,_,_)->i) $3)) }
  | '[' int ']' { TArray ((\(i,_,_)->i) $2) }

variables : variables ',' var          {% do
  s <- get
  modifTabla $3 (getAType s) }
    | variables ',' var arrays    {% do
  s <- get
  modifTabla $3 ($4 $ getAType s) }
    | variables ',' asteriscos var     {% do
  s <- get
  modifTabla $4 (unasteriscos $3 $ getAType s) }
    | var                              {% do
  s <- get
  modifTabla $1 (getAType s) }
    | asteriscos var                   {% do
  s <- get
  modifTabla $2 (unasteriscos $1 $ getAType s) }
    | var arrays                  {% do
  s <- get
  modifTabla $1 ($2 $ getAType s) }

VarDeclaration : Types variables ';'  { }

StructVar : var {% do
  modifTabla $1 (TStruct $ (\(s,_,_)->s) $1) 
  modify $ push (TStruct $ (\(s,_,_)->s) $1) 
  }
Struct : struct StructVar Block2 VarDeclarations Block3 ';' {% do
  [x] <- pop 1
  modify $ (\s-> addus x (querrySimT (T.enterN 0) s) s) 
  }

UnionVar : var {% do
  modifTabla $1 (TUnion $ (\(s,_,_)->s) $1) 
  modify $ push (TUnion $ (\(s,_,_)->s) $1) 
  }
Union : union UnionVar Block2 VarDeclarations Block3 ';' {% do
  [x] <- pop 1
  modify $ (\s-> addus x (querrySimT (T.enterN 0) s) s) 
  }

Funcion : Types VarVar FuncionP2 Parametros ')' registra2 Block FuncionP3   {  }
    | Types VarVar registra Block  {  }

registra : '(' ')' {% do
  s <- get
  modifTabla (auxS s) (FunGlob (last $ pilaAT s) (init $ pilaAT s)) }

registra2 : {% do
  modify $ alterSimT T.exitScope
  s <- get
  modifTabla (auxS s) (FunGlob (last $ pilaAT s) (init $ pilaAT s))
  modify $ alterSimT $ T.enterN 0
  }

VarVar : var {% do modify $ ponAT; modify $ newAusS $1}
 
FuncionP2 : '(' {% modify $ alterSimT T.enterScope}
FuncionP3 : {% modify $ alterSimT T.exitScope}

Block : Block2 Instrucciones Block3 {}
Block2 : '{' {% modify $ alterSimT T.enterScope}
Block3 : '}' {% modify $ alterSimT T.exitScope}

Instruccion : Block { }
    | For { }
    | While { }
    | If { }
    | Asignacion ';' { }
    | Exp ';' {% do
  [x] <- pop 1
  modify $ push (if x==TError then TError else TVoid)
 }
    | break ';' {% modify $ push TVoid}
    | continue ';' {% modify $ push TVoid}
    | return ';' {% modify $ push TVoid}
    | return Exp ';' {% do
  [x] <- pop 1
  modify $ push (if x==TError then TError else TVoid)
 }
    | read var ';' {% do 
  checkExist $2 
  modify $ push TVoid
  }
    | write Exp ';' {% modify $ push TVoid} --------------------------arreglar, chequear expresion
    | ';' {% modify $ push TVoid}

For : for '(' Instruccion ';' Exp ';' Instruccion ')' Instruccion {% do
  [x,y,z,w] <- pop 4
  modify $ push (if y==TBool && x==TVoid && z==TVoid && w==TVoid then TVoid else TError)
 }
While : while '(' Exp ')' Instruccion {% do
  [x,y] <- pop 2
  modify $ push (if x==TVoid && y==TBool then TVoid else TError)
 }

If : if '(' Exp ')' Block {% do
  [x,y] <- pop 2
  modify $ push (if x==TVoid && y==TBool then TVoid else TError)
 }
    | if '(' Exp ')' Block else Instruccion {% do
  [x,y,z] <- pop 3
  modify $ push (if y==TVoid && x==TVoid && z==TBool then TVoid else TError)
 }

Asignacion : var '=' Exp {% do
  checkExist $1 
  [x] <- pop 1
  let s = (\(x,_,_)->x) $1
  modify $ (\estado->push (if x==(maybe TError id (querrySimT (T.lookup s) estado)) then TVoid else TError) estado)
  }
    | var '[' Exp ']' '=' Exp {% do
  checkExist $1 
  [x,y] <- pop 1
  let s = (\(x,_,_)->x) $1
  modify $ (\estado->push (if y==TInt && x==(maybe TError id (querrySimT (T.lookup s) estado)) then TVoid else TError) estado)
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
  modify $ push (if x==TVoid && y==TVoid then TVoid else TError)
 }
    | Instruccion { }
    | Instrucciones VarDeclaration { }
    | VarDeclaration {% modify $ push TVoid }

Parametro : ref Types var      {% do
  s <- get
  modifTabla $3 (TRef $ getAType s)
  modify $ modifAType TRef
  modify $ empilaAT }
    | Types var                {% do
  s <- get
  modifTabla $2 (getAType s)
  modify $ empilaAT }
Parametros : Parametros ',' Parametro { }
    | Parametro              { }

AnomFun : '(' Types ')' '(' Parametros ')' '{' Instrucciones '}'  { }
    | '(' Types ')' '(' ')' '{' Instrucciones '}'  { }

arr : arr '[' Exp ']' {%do
  [x] <- pop 1
  return $ sReturnEmpty {tipo=if (tipo $1)==TInt&&x==TInt then TInt else TError,number=1+(number $1)}
  }
    | '[' Exp ']' {%do
  [x] <- pop 1
  return $ sReturnEmpty {tipo=if x==TInt then TInt else TError,number=1}
  }

Atom : LLamada { }
    | int { % modify $ push TInt}
    | float { % modify $ push TFloat}
    | bool { % modify $ push TBool}
    | str {% do
  let s = (\(x,_,_)->x) $1
  modify $ addString s 
  modify $ push (TArray (length s) TChar)
          }
    | var {% do
  let s = (\(x,_,_)->x) $1
  checkExist $1 
  modify $ (\estado->push (maybe TError id (querrySimT (T.lookup s) estado)) estado)
          }
    | var arr {% do
  let s = (\(x,_,_)->x) $1
  checkExist $1 
  modify $ (\estado->push (if (tipo $2)==TError then TError else (maybe TError (ayudaArreglo (number $2)) (querrySimT (T.lookup s) estado))) estado)
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
  if (ayudasu x) then modify $ push TError
  else do
    estado <- get
    let maybeField = maybe Nothing (T.localLookup s) (buscasusymt x estado)
    modify $ push $ maybe TError id maybeField
 }
    | '(' Exp ')'            { }
    | '-' Exp %prec NEG      {% oneOperators "||" }
    | '*' Exp %prec Deref    {% do
  [x] <- pop 1
  modify $ push (ayudaApuntador x)
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

ayudaArreglo 0 x = x
ayudaArreglo n (TArray _ x) = ayudaArreglo (n-1) x
ayudaArreglo _ _ = TError

ayudaApuntador (TRef x) = x
ayudaApuntador _ = TError

ayudasu (TStruct _) = False
ayudasu (TUnion _) = False
ayudasu _ = True

pop :: Int -> StateT ParseState IO [TypeData]
pop x = do
  estado <- get
  modify $ typePilaOperate (drop x)
  return (take x $ typePila estado)

twoOperators :: String -> StateT ParseState IO ()
twoOperators o = do
  [x,y] <- pop 2
  modify $ push (operAcc x o y)

oneOperators :: String -> StateT ParseState IO ()
oneOperators o = do
  [x] <- pop 1
  modify $ push (operAccMono o x)

evalLlamada :: (String,Int,Int) -> [TypeData] -> StateT ParseState IO ()
evalLlamada inTok param = do 
  let s = (\(x,_,_)->x) inTok
  checkExist inTok
  modify $ (\estado->push (maybe TError help (querrySimT (T.lookup s) estado)) estado)
  where
    help (FunGlob returnT tl) = if tl==param then returnT else TError
    help (FunLoc returnT tl) = if tl==param then returnT else TError

modifTabla (s,l,c) tipo = do
  estado <- get
  if ((querrySimT (T.localLookup s) estado) == Nothing)
  then
    --if ((querrySimT (T.lookup s) estado) /= Just FunGlob)
      --then
        modify $ alterSimT (T.insert s tipo)
      --else
        --addError estado ("Existe una funcion global de nombre "++s++", no se puede declarar de nuevo. Linea: "++(show l)++" Columna: "++(show c))
  else 
    printError ("Ya se encuentra declarada la variable "++s++" en este Alcance. Linea: "++(show l)++" Columna: "++(show c))

checkExist (s,l,c) = do
  estado <- get
  when (isNothing $ querrySimT (T.lookup s) estado) $ do
    printError ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))

checkExist2 (s,l,c) = do
  estado <- get
  if (isNothing $ resul estado)
  then
    printError ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))
  else 
    modify $ modifAType (const (fromJust (resul estado)))
  where
    resul estado = querrySimT (T.lookup s) estado

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