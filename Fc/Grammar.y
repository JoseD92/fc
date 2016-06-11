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

All : All Funcion                  {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)} }
    | Funcion                      {% return $1 }
    | All Struct                   {% return $1 }
    | Struct                       {% return $ sReturnEmpty {tipo=TVoid} }
    | All Union                    {% return $1 }
    | Union                        {% return $ sReturnEmpty {tipo=TVoid} }
    | All VarDeclaration           {% return $1 }
    | VarDeclaration               {% return $ sReturnEmpty {tipo=TVoid} }

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
  return $ sReturnEmpty {tipo=(TStruct $ (\(s,_,_)->s) $1)}
  }
Struct : struct StructVar Block2 VarDeclarations Block3 ';' {% modify $ (\s-> addus (tipo $2) (querrySimT (T.enterN 0) s) s) }

UnionVar : var {% do
  modifTabla $1 (TUnion $ (\(s,_,_)->s) $1) 
  return $ sReturnEmpty {tipo=(TUnion $ (\(s,_,_)->s) $1)}
  }
Union : union UnionVar Block2 VarDeclarations Block3 ';' {% modify $ (\s-> addus (tipo $2) (querrySimT (T.enterN 0) s) s) }

Funcion : Types VarVar FuncionP2 Parametros ')' registra2 Block FuncionP3   {% return $7 }
    | Types VarVar registra Block  {% return $4 }

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

Block : Block2 Instrucciones Block3 {% return $2 }
Block2 : '{' {% modify $ alterSimT T.enterScope}
Block3 : '}' {% modify $ alterSimT T.exitScope}

Instruccion : Block  {% return $1 }
    | For            {% return $1 }
    | While          {% return $1 }
    | If             {% return $1 }
    | Asignacion ';' {% return $1 }
    | Exp ';'        {% return $ sReturnEmpty {tipo=(if (tipo $1)==TError then TError else TVoid)} }
    | break ';'      {% return $ sReturnEmpty {tipo=TVoid} }
    | continue ';'   {% return $ sReturnEmpty {tipo=TVoid} }
    | return ';'     {% return $ sReturnEmpty {tipo=TVoid} }
    | return Exp ';' {% return $ sReturnEmpty {tipo=(if (tipo $2)==TError then TError else TVoid)} }
    | read var ';'   {% do 
                          querry <- checkExist $2
                          return $ sReturnEmpty {tipo=(maybe TError (const TVoid) querry)}
                     }
    | write Exp ';'  {% return $ sReturnEmpty {tipo=(if (tipo $2)==TError then TError else TVoid)} }
    | ';'            {% return $ sReturnEmpty {tipo=TVoid} }

For : for '(' Instruccion ';' Exp ';' Instruccion ')' Instruccion {% return $ sReturnEmpty {tipo=(if (tipo $5)==TBool && (tipo $3)==TVoid && (tipo $7)==TVoid && (tipo $9)==TVoid then TVoid else TError)} }

While : while '(' Exp ')' Instruccion {% return $ sReturnEmpty {tipo=(if (tipo $5)==TVoid && (tipo $3)==TBool then TVoid else TError)} }

If : if '(' Exp ')' Block                   {% return $ sReturnEmpty {tipo=(if (tipo $5)==TVoid && (tipo $3)==TBool then TVoid else TError)} }
    | if '(' Exp ')' Block else Instruccion {% return $ sReturnEmpty {tipo=(if (tipo $7)==TVoid && (tipo $5)==TVoid && (tipo $3)==TBool then TVoid else TError)} }

Asignacion : var '=' Exp {% do
                              querry <- checkExist $1
                              return $ sReturnEmpty {tipo=(if (tipo $3)/=TError && (tipo $3)==(maybe TError id querry) then TVoid else TError)}
                         }
    | var arr '=' Exp {% do
                           querry <- checkExist $1
                           return $ sReturnEmpty {tipo=(if (tipo $4)/=TError &&(tipo $2)==TInt && (tipo $4)==(maybe TError (ayudaArreglo (number $2)) querry) then TVoid else TError)}
                      }

LLamada : var '(' ParametrosIn ')'   {% evalLlamada $1 $3}
    | var '(' ')'                    {% evalLlamada $1 []}

ParametrosIn : ParametrosIn ',' Exp { (tipo $3):$1 }
    | Exp { [tipo $1] } 

Instrucciones : Instrucciones Instruccion {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)}}
    | Instruccion                         {% return $1 }
    | Instrucciones VarDeclaration        {% return $1 }
    | VarDeclaration                      {% return $ sReturnEmpty {tipo=TVoid} }

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

arr : arr '[' Exp ']' {% return $ sReturnEmpty {tipo=if (tipo $1)==TInt&&(tipo $3)==TInt then TInt else TError,number=1+(number $1)} }
    | '[' Exp ']' {% return $ sReturnEmpty {tipo=if (tipo $2)==TInt then TInt else TError,number=1} }

Atom : LLamada {% return $1 }
    | int { % return $ sReturnEmpty {tipo=TInt}}
    | float { % return $ sReturnEmpty {tipo=TFloat}}
    | bool { % return $ sReturnEmpty {tipo=TBool}}
    | str {% do
  let s = (\(x,_,_)->x) $1
  modify $ addString s 
  return $ sReturnEmpty {tipo=(TArray (length s) TChar)}
          }
    | var {% do
  querry <- checkExist $1
  return $ sReturnEmpty {tipo=(maybe TError id querry)}
          }
    | var arr {% do
  querry <- checkExist $1
  return $ sReturnEmpty {tipo=(if (tipo $2)==TError then TError else (maybe TError (ayudaArreglo (number $2)) querry))}
          }
    | AnomFun { % return $ sReturnEmpty {tipo=TAny}}

Exp : Exp '+' Exp            {% twoOperators $2 $1 $3 }
    | Exp '-' Exp            {% twoOperators $2 $1 $3 }
    | Exp '*' Exp            {% twoOperators $2 $1 $3 }
    | Exp '/' Exp            {% twoOperators $2 $1 $3 }
    | Exp '<<' Exp           {% twoOperators $2 $1 $3 }
    | Exp '>>' Exp           {% twoOperators $2 $1 $3 }
    | Exp '<' Exp            {% twoOperators $2 $1 $3 }
    | Exp '<=' Exp           {% twoOperators $2 $1 $3 }
    | Exp '>' Exp            {% twoOperators $2 $1 $3 }
    | Exp '>=' Exp           {% twoOperators $2 $1 $3 }
    | Exp '==' Exp           {% twoOperators $2 $1 $3 }
    | Exp '!=' Exp           {% twoOperators $2 $1 $3 }
    | Exp '&' Exp            {% twoOperators $2 $1 $3 }
    | Exp '%' Exp            {% twoOperators $2 $1 $3 }
    | Exp '|' Exp            {% twoOperators $2 $1 $3 }
    | Exp '&&' Exp           {% twoOperators $2 $1 $3 }
    | Exp '||' Exp           {% twoOperators $2 $1 $3 }
    | Exp '.' var            {% do
  let s = (\(x,_,_)->x) $3
  if (ayudasu (tipo $1)) then return $ sReturnEmpty {tipo=TError}
  else do
    estado <- get
    let maybeField = maybe Nothing (T.localLookup s) (buscasusymt (tipo $1) estado)
    return $ sReturnEmpty {tipo=maybe TError id maybeField}
 }
    | '(' Exp ')'            {% return $2 }
    | '-' Exp %prec NEG      {% oneOperators $1 $2 }
    | '*' Exp %prec Deref    {% return $ sReturnEmpty {tipo=(ayudaApuntador (tipo $2))}}
    | Atom                   {% return $1 }

{

errStrPut = hPutStrLn stderr

printError = lift.errStrPut

--sprint = lift.putStrLn

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

twoOperators :: (String,Int,Int) -> SReturn -> SReturn -> StateT ParseState IO SReturn
twoOperators (o,l,c) r1 r2 = do
  return $ sReturnEmpty {tipo=(operAcc (tipo r1) o (tipo r2))}

oneOperators :: (String,Int,Int) -> SReturn -> StateT ParseState IO SReturn
oneOperators (o,l,c) r1 = do
  return $ sReturnEmpty {tipo=(operAccMono o (tipo r1))}

evalLlamada :: (String,Int,Int) -> [TypeData] -> StateT ParseState IO SReturn
evalLlamada inTok param = do 
  querry <- checkExist inTok
  return $ sReturnEmpty {tipo=(maybe TError help querry)}
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

checkExist :: (String,Int,Int) -> StateT ParseState IO (Maybe TypeData)
checkExist (s,l,c) = do
  estado <- get
  let resul = querrySimT (T.lookup s) estado
  when (isNothing resul) $ do
    printError ("No existe el simbolo "++s++". Linea: "++(show l)++" Columna: "++(show c))
  return resul

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