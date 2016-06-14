
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
    | All Struct                   {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)} }
    | Struct                       {% return $1 }
    | All Union                    {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)} }
    | Union                        {% return $1 }
    | All VarDeclaration           {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)} }
    | VarDeclaration               {% return $1 }

VarDeclarations : VarDeclarations VarDeclaration  {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)} }
    | VarDeclaration  {% return $1 }

ManyTypes : ManyTypes ',' Types    { (tipo $3):$1 }
    | Types { [tipo $1] }

asteriscos : asteriscos '*' { $1 + 1 }
    | '*' { 1 }

basicType : type {% return $ sReturnEmpty {tipo=case ((\(s,_,_)->s) $1) of
  "int" ->  TInt
  "float" ->  TFloat
  "bool" ->  TBool
  "void" ->  TVoid
  "char" ->  TChar}
  }

Types : basicType                                      {% return $1 }
    | unsigned basicType                               {% return $ sReturnEmpty {tipo=TUnsigned (tipo $2)} }
    | struct var                                       {% checkExist2 $2 }
    | union var                                        {% checkExist2 $2 }
    | functionType '(' Types ')' '(' ManyTypes ')'     {% return $ sReturnEmpty {tipo= FunLoc (tipo $3) $6} }
    | functionType '(' Types ')' '(' ')'               {% return $ sReturnEmpty {tipo= FunLoc (tipo $3) []} }
    | '(' Types asteriscos ')'                         {% return $ sReturnEmpty {tipo= unasteriscos $3 (tipo $2) } }

arrays : arrays '[' int ']' { (.) $1 (TArray ((\(i,_,_)->i) $3)) }
  | '[' int ']' { TArray ((\(i,_,_)->i) $2) }

variables : variables ',' var                {% do
  modifTabla $3 (tipo $1) 
  return $1}
    | variables ',' var arrays               {% do
  modifTabla $3 ($4 $ tipo $1) 
  return $1}
    | variables ',' asteriscos var           {% do
  modifTabla $4 (unasteriscos $3 $ tipo $1) 
  return $1}
    | Types var                              {% do
  modifTabla $2 (tipo $1) 
  return $1}
    | Types asteriscos var                   {% do
  modifTabla $3 (unasteriscos $2 $ tipo $1) 
  return $1}
    | Types var arrays                       {% do
  modifTabla $2 ($3 $ tipo $1) 
  return $1}

VarDeclaration : variables ';'  {% if (tipo $1 == TError) then return $1 else return $ sReturnEmpty {tipo=TVoid} }

StructVar : var {% do
  modifTabla $1 (TStruct $ (\(s,_,_)->s) $1) 
  return $ sReturnEmpty {tipo=(TStruct $ (\(s,_,_)->s) $1)}
  }
Struct : struct StructVar Block2 VarDeclarations Block3 ';' {% do
  s <- get
  let miTabla = querrySimT (T.enterN 0) s
  let (_,l,c) = $1
  modify $ addus (tipo $2) miTabla
  --aqui podria agregar un comando para quitar la tabla de la tabla general
  if (T.pertenece (tipo $2) miTabla) then do
    printError $ "Error en la linea "++(show l)++" columna "++(show (c+1))++
      ". La estructura se contiene a si misma."
    return $ sReturnEmpty {tipo=TError}
  else return $ sReturnEmpty {tipo=if ((tipo $4) == TError) then TError else TVoid}
}

UnionVar : var {% do
  modifTabla $1 (TUnion $ (\(s,_,_)->s) $1) 
  return $ sReturnEmpty {tipo=(TUnion $ (\(s,_,_)->s) $1)}
  }
Union : union UnionVar Block2 VarDeclarations Block3 ';' {% do
  s <- get
  let miTabla = querrySimT (T.enterN 0) s
  let (_,l,c) = $1
  modify $ addus (tipo $2) miTabla
  --aqui podria agregar un comando para quitar la tabla de la tabla general
  if (T.pertenece (tipo $2) miTabla) then do
    printError $ "Error en la linea "++(show l)++" columna "++(show (c+1))++
      ". La union se contiene a si misma."
    return $ sReturnEmpty {tipo=TError}
  else return $ sReturnEmpty {tipo=if ((tipo $4) == TError) then TError else TVoid}
}

Funcion : FuncionConArgs Block FuncionP3   {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)} }
    | FuncionNoArgs Block  {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)} }

FuncionNoArgs : Types var '(' ')' {% do 
  modifTabla $2 (FunGlob (tipo $1) [])
  return $ sReturnEmpty {tipo=if ((tipo $1) == TError) then TError else TVoid}
 }

FuncionConArgs : Types var FuncionP2 Parametros ')'  {% do
  modify $ alterSimT T.exitScope
  modifTabla $2 (FunGlob (tipo $1) $4)
  modify $ alterSimT $ T.enterN 0
  return $ sReturnEmpty {tipo=if ((tipo $1) == TError || (elem TError $4)) then TError else TVoid}
}
 
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

For : for '(' Instruccion BoolE2 Instruccion ')' Instruccion 
  {% return $ sReturnEmpty {tipo=(if (tipo $5)==TBool && (tipo $3)==TVoid && (tipo $5)==TVoid && (tipo $7)==TVoid then TVoid else TError)} }

While : while BoolE Instruccion {% return $ sReturnEmpty {tipo=(if (tipo $3)==TVoid && (tipo $2)==TBool then TVoid else TError)} }

If : if BoolE Block             {% return $ sReturnEmpty {tipo=(if (tipo $3)==TVoid && (tipo $2)==TBool then TVoid else TError)} }
    | if BoolE Block else Block {% return $ sReturnEmpty {tipo=(if (tipo $3)==TVoid && (tipo $5)==TVoid && (tipo $2)==TBool then TVoid else TError)} }

BoolE2 : ';' Exp ';' {% do
  if ((tipo $2)==TError) then return $ sReturnEmpty {tipo=TError}
  else
    if ((tipo $2)==TBool) then return $ sReturnEmpty {tipo=TBool}
    else do
      let (_,l,c) = $1
      printError $ "Error en la linea "++(show l)++" columna "++(show (c+1))++
        ". Se esperaba un valor de tipo Bool, pero se encontro: "++(show (tipo $2))++"."
      return $ sReturnEmpty {tipo=TError}
  }

BoolE : '(' Exp ')' {% do
  if ((tipo $2)==TError) then return $ sReturnEmpty {tipo=TError}
  else
    if ((tipo $2)==TBool) then return $ sReturnEmpty {tipo=TBool}
    else do
      let (_,l,c) = $1
      printError $ "Error en la linea "++(show l)++" columna "++(show (c+1))++
        ". Se esperaba un valor de tipo Bool, pero se encontro: "++(show (tipo $2))++"."
      return $ sReturnEmpty {tipo=TError}
  }

--If : B  {% return $ sReturnEmpty {tipo=TVoid} }
--  | U   {% return $ sReturnEmpty {tipo=TVoid} }

--B : I B else B {% return $ sReturnEmpty {tipo=TVoid} }
--  | Instruccion             {% return $ sReturnEmpty {tipo=TVoid} }

--U : I If       {% return $ sReturnEmpty {tipo=TVoid} }
--  | I B else U {% return $ sReturnEmpty {tipo=TVoid} }

--I : if '(' Exp ')' {% return $ sReturnEmpty {tipo=TVoid} }

--mases : mases '+' {ayudaApuntador.$1}
--    | '+' {ayudaApuntador}

Asignacion : Atom2 '=' Exp {% do
  if ((tipo $3)==TError || (tipo $1)==TError) then return $ sReturnEmpty {tipo=TError}
  else
    if ((tipo $1)==(tipo $3)) then return $ sReturnEmpty {tipo=TVoid}
    else do
      let (_,l,c) = $2
      printError $ "Error en la linea "++(show l)++" columna "++(show c)++
        ". Los lados de una asignacion deben de ser del mismo tipo, pero se encontro: "++(show (tipo $1))++" y "++(show (tipo $3))++"."
      return $ sReturnEmpty {tipo=TError}
 }

LLamada : var '(' ParametrosIn ')'   {% evalLlamada $1 $3}
    | var '(' ')'                    {% evalLlamada $1 []}

ParametrosIn : ParametrosIn ',' Exp { (tipo $3):$1 }
    | Exp { [tipo $1] } 

Instrucciones : Instrucciones Instruccion {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)}}
    | Instruccion                         {% return $1 }
    | Instrucciones VarDeclaration        {% return $ sReturnEmpty {tipo=(if (tipo $1)==TVoid && (tipo $2)==TVoid then TVoid else TError)}}
    | VarDeclaration                      {% return $1 }

Parametro : ref Types var      {% do
  modifTabla $3 (TRef $ (tipo $2))
  return $ sReturnEmpty {tipo=(TRef $ (tipo $2)) }
  }
    | Types var                {% do
  modifTabla $2 (tipo $1)
  return $ sReturnEmpty {tipo=(tipo $1)}
  }
Parametros : Parametros ',' Parametro { (tipo $3):$1 }
    | Parametro              { [(tipo $1)] }

AnomFun : '(' Types ')' '(' Parametros ')' '{' Instrucciones '}'  { }
    | '(' Types ')' '(' ')' '{' Instrucciones '}'  { }

arr : arr '[' Exp ']' {% do
  if ((tipo $1)==TError) then return $ sReturnEmpty {tipo=TError,number=0}
  else
    if ((tipo $3)==TInt) then return $ sReturnEmpty {tipo=TInt,number=1+(number $1)}
    else do
      let (_,l,c)=$2
      when ((tipo $3)/=TError) $ printError $ "Error en la linea "++(show l)++" columna "++(show (c+1))++". Se esperaba un valor de tipo entero."
      return $ sReturnEmpty {tipo=TError,number=0}
 }
    | {% return $ sReturnEmpty {tipo=TInt,number=0} }

Atom : LLamada {% return $1 }
    | int { % return $ sReturnEmpty {tipo=TInt}}
    | float { % return $ sReturnEmpty {tipo=TFloat}}
    | bool { % return $ sReturnEmpty {tipo=TBool}}
    | str {% do
  let s = (\(x,_,_)->x) $1
  modify $ addString s 
  return $ sReturnEmpty {tipo=(TArray (length s) TChar)}
          }
    | AnomFun { % return $ sReturnEmpty {tipo=TAny}}

Atom2 : var arr {% arrCheck $2 $1 }
    | Exp '.' var            {% campos $1 $2 $3}
    | '*' Exp %prec Deref    {% do
  let (_,l,c) = $1
  if ((tipo $2)==TError) then return $ sReturnEmpty {tipo=TError}
  else do
    when ((ayudaApuntador (tipo $2))==TError) $ printError $ "Error en la linea "++(show l)++" columna "++(show c)++
      ". El tipo "++(show (tipo $2))++" no es una referencia."
    return $ sReturnEmpty {tipo=(ayudaApuntador (tipo $2))}}

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
    | '(' Exp ')'            {% return $2 }
    | '-' Exp %prec NEG      {% oneOperators $1 $2 }
    | Atom2                  {% return $1 }
    | Atom                   {% return $1 }

{

errStrPut = hPutStrLn stderr

printError = lift.errStrPut

--sprint = lift.putStrLn

unasteriscos 1 = TRef
unasteriscos i = TRef.(unasteriscos (i-1))

deasterisco 1 = ayudaApuntador
deasterisco n = ayudaApuntador.(deasterisco (n-1))

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

arrCheck expIn tok@(s,l,c) = do
  if ((tipo expIn)==TError) then return $ sReturnEmpty {tipo=TError}
  else do
    querry <- checkExist tok
    if (isNothing querry) then return $ sReturnEmpty {tipo=TError}
    else do
      let resultype=(ayudaArreglo (number expIn)) $ fromJust querry
      when (resultype == TError) $ printError $ "Error en la linea "++(show l)++" columna "++(show c)++
        ". Mala dimencion para la variable "++s++"."
      return $ sReturnEmpty {tipo=resultype}

twoOperators :: (String,Int,Int) -> SReturn -> SReturn -> StateT ParseState IO SReturn
twoOperators (o,l,c) r1 r2 = do
  if ((tipo r1)==TError || (tipo r2)==TError) then return $ sReturnEmpty {tipo=TError}
  else
    if resultype == TError then do
      printError $ "Error de tipos en la linea "++(show l)++" columna "++(show c)++
        ". El operador \""++o++"\" recibio argumentos de tipo: "++(show (tipo r1))++" y "++(show (tipo r2))++"."
      return $ sReturnEmpty {tipo=TError}
    else
      return $ sReturnEmpty {tipo=resultype}
  where resultype = (operAcc (tipo r1) o (tipo r2))
  
campos expIn (_,l,c) (s,_,_)  = do
  if ((tipo expIn)==TError) then return $ sReturnEmpty {tipo=TError}
  else
    if (ayudasu (tipo expIn)) then do
      printError $ "Error en la linea "++(show l)++" columna "++(show c)++". El tipo "++(show (tipo expIn))++" no posee campos."
      return $ sReturnEmpty {tipo=TError}
    else do
      estado <- get
      let maybeField = maybe Nothing (T.localLookup s) (buscasusymt (tipo expIn) estado)
      when (isNothing maybeField) $ printError $ "Error en la linea "++(show l)++" columna "++(show c)++
        ". El tipo "++(show (tipo expIn))++" no posee el campo "++s++"."
      return $ sReturnEmpty {tipo=maybe TError id maybeField}

oneOperators :: (String,Int,Int) -> SReturn -> StateT ParseState IO SReturn
oneOperators (o,l,c) r1 = do
  if ((tipo r1)==TError) then return $ sReturnEmpty {tipo=TError}
  else
    if resultype == TError then do
      printError $ "Error de tipos en la linea "++(show l)++" columna "++(show c)++
        ". El operador \""++o++"\" recibio un argumento de tipo: "++(show (tipo r1))++"."
      return $ sReturnEmpty {tipo=TError}
    else
      return $ sReturnEmpty {tipo=resultype}
  where  resultype = (operAccMono o (tipo r1))

evalLlamada :: (String,Int,Int) -> [TypeData] -> StateT ParseState IO SReturn
evalLlamada inTok@(s,l,c) param = do 
  if (elem TError param) then return $ sReturnEmpty {tipo=TError}
  else do
    querry <- checkExist inTok
    if (isNothing querry) then return $ sReturnEmpty {tipo=TError}
    else do
      if(resultype querry == TError) then do
        printError $ "Error en la linea "++(show l)++" columna "++(show c)++". Argumentos no son validos para la funcion "++s++"."
        return $ sReturnEmpty {tipo=TError}
      else
        return $ sReturnEmpty {tipo=resultype querry}
  where
    resultype = help.fromJust
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
  then do
    printError ("No existe la estructura o union "++s++". Linea: "++(show l)++" Columna: "++(show c))
    return $ sReturnEmpty {tipo=TError}
  else 
    if (ayudasu $ fromJust (resul estado)) then do
      printError (s++" no es una estructura o union. Linea: "++(show l)++" Columna: "++(show c))
      return $ sReturnEmpty {tipo=TError}
    else
      return $ sReturnEmpty {tipo=fromJust (resul estado)}
  where
    resul estado = querrySimT (T.lookup s) estado


}