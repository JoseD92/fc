{-# LANGUAGE DeriveGeneric #-}
module Fc.Tac.Tac (
compaq2file,decodeFromFile,Value(..),TacIns(..),Tac(..),Registro(..),Etiqueta(..),regGen,etiGen,BiOperators(..),toTac
) where

import qualified Data.ByteString.Lazy as B
import           Data.Sequence
import      qualified     Data.Sequence as Seq
import           Data.Serialize (Serialize,encodeLazy,decodeLazy)
import           Data.Word
import           GHC.Generics
import qualified Prelude as P
import qualified Fc.Tabla as T
import Prelude hiding (GT,length)
import Data.Foldable (toList)
import Control.Monad.State
import Fc.Datas
import Control.Monad (mapM)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)
import qualified Prelude as P
import Debug.Trace

type DataSize = Word32 --tipo para los registros y etiquetas
-- 64 bits porque correra en maquinas de 64bits,
-- si se sabe correra en 32bits se puede cambiar a word32

--      Registros o temporales
newtype Registro = Reg DataSize deriving (Generic)
instance Serialize Registro
instance Show Registro where
  show (Reg x) = "t"++ show x

regGen :: [Registro]
regGen = map Reg [0 ..]

--      Etiquetas
data Etiqueta = Eti DataSize | EtiErr deriving (Generic)
instance Serialize Etiqueta
instance Show Etiqueta where
  show (Eti x) = show x
  show EtiErr = "No Eti"

etiGen :: [Etiqueta]
etiGen = map Eti [0 ..]

-- instrucciones de Tac

data BiOperators = Mod | Mas | Menos | Por | Entre | GT | Lt | Eq | EGT | ELT
  | And | Or | NEq | RShift | LShift | BitAnd | BitOr
  deriving (Generic)
instance Serialize BiOperators
instance Show BiOperators where
  show Mod = "%"
  show Mas = "+"
  show Menos = "-"
  show Por = "*"
  show Entre = "/"
  show GT = ">"
  show Lt = "<"
  show Eq = "=="
  show EGT = ">="
  show ELT = "<="
  show And = "&&"
  show Or = "||"
  show NEq = "!="
  show RShift = ">>"
  show LShift = "<<"
  show BitAnd = "&"
  show BitOr = "|"

data Value = Val Registro |
  VarGlobal String | -- si es glogal solo nesecito el string para buscar en la tabla de simbolos y saber el offset
  VarLoc String DataSize | --si es local, solo necesito saber el offset con el frame pointer, se debe crear usando la tabla de simbolos, el string es para debug e imprimir
  Cons DataSize |
  BasePtr Int |
  FramePtr Int |
  BasicInt Int |
  BasicFloat Float |
  BasicBool Bool |
  DirOf Etiqueta
  deriving (Generic)
instance Serialize Value
instance Show Value where
  show (Val x) = show x
  show (VarGlobal s) = s
  show (VarLoc s _) = s
  show (Cons x) = show x
  show (BasePtr x) = "BasePtr " ++ show x
  show (FramePtr x) = "FramePtr " ++ show x
  show (BasicInt x) = show x
  show (BasicFloat x) = show x
  show (BasicBool x) = show x
  show (DirOf x) = show x

data Tac = IntBiOp Etiqueta BiOperators Value Value Value
  | UIntBiOp Etiqueta BiOperators Value Value Value
  | FloatBiOp Etiqueta BiOperators Value Value Value
  | BoolBiOp Etiqueta BiOperators Value Value Value
  | UnaryOp Etiqueta BiOperators Value Value
  | Copy Etiqueta Value Value
  | Goto Etiqueta Etiqueta
  | Gotoz Etiqueta Etiqueta Value --goto if zero
  | Gotonz Etiqueta Etiqueta Value --goto if not zero
  | Parametro Etiqueta Value
  | Call Etiqueta String Int Value -- llama a una funcion con n parametros
  | Copy1 Etiqueta Value Value Value -- x:=y[i,j,...]   el segundo value debe ser VarGlobal o VarLoc
  | Copy2 Etiqueta Value Value Value -- y[i,j,...]:=x   el segundo value debe ser VarGlobal o VarLoc
  | Copy3 Etiqueta Value Value -- x:=&y   el segundo value debe ser VarGlobal o VarLoc
  | Copy4 Etiqueta Value Value -- x:=*y
  | Copy5 Etiqueta Value Value -- *x:=y
  | Comentario String
  | Fall Etiqueta
  | Prologo Etiqueta String
  | Epilogo Etiqueta String
  | Return Etiqueta
  | ReturnW Etiqueta Value
  | TacParam Etiqueta Value
  | Clean Etiqueta Int
  deriving (Generic)
instance Serialize Tac
instance Show Tac where
  show (IntBiOp eti oper r1 r2 resul) = show eti ++":\t"++ show resul ++" := "++ show r1 ++" "++ show oper ++" "++ show r2
  show (UIntBiOp eti oper r1 r2 resul) = show eti ++":\t"++ show resul ++" := "++ show r1 ++" "++ show oper ++" "++ show r2
  show (FloatBiOp eti oper r1 r2 resul) = show eti ++":\t"++ show resul ++" := "++ show r1 ++" "++ show oper ++" "++ show r2
  show (BoolBiOp eti oper r1 r2 resul) = show eti ++":\t"++ show resul ++" := "++ show r1 ++" "++ show oper ++" "++ show r2
  show (UnaryOp eti oper r1 resul) = show eti ++":\t"++ show resul ++" := "++ show oper ++" "++ show r1
  show (Copy eti r1 r2) = show eti ++":\t"++ show r1 ++" := "++ show r2
  show (Goto eti1 eti2) = show eti1 ++ ":\tGoto " ++ show eti2
  show (Gotoz eti1 eti2 r) = show eti1 ++":\tifZero "++ show r ++" Goto "++show eti2
  show (Gotonz eti1 eti2 r) = show eti1 ++":\tifnotZero "++ show r ++" Goto "++show eti2
  show (Parametro eti val) = show eti ++ ":\tParam " ++ show val
  show (Call eti s n v) = show eti ++":\t"++show v ++" := "++"Call " ++ s ++" "++ show n
  show (Copy1 eti r1 r2 l) = show eti ++":\t"++ show r1 ++" := "++ show r2 ++ "["++ show l ++ "]"
  show (Copy2 eti r1 r2 l) = show eti ++":\t" ++ show r2 ++ "["++ show l ++ "]" ++" := " ++ show r1
  show (Copy3 eti r1 r2) = show eti ++":\t"++ show r1 ++" := &"++ show r2
  show (Copy4 eti r1 r2) = show eti ++":\t"++ show r1 ++" := *"++ show r2
  show (Copy5 eti r1 r2) = show eti ++":\t*"++ show r1 ++" := "++ show r2
  show (Comentario s) = s
  show (Fall eti) = show eti ++":\tFall"
  show (Prologo eti s) = show eti ++":\tPrologo de "++s
  show (Epilogo eti s) = show eti ++":\tEpilogo de "++s
  show (Return eti) = show eti ++":\tReturn"
  show (ReturnW eti v) = show eti ++":\tReturn "++show v
  show (TacParam eti v) = show eti ++":\tParam "++show v
  show (Clean eti v) = show eti ++":\tClean "++show v

help l = concatMap (\x->"["++ show x ++"]") l

getEti :: Tac -> Etiqueta
getEti (IntBiOp eti _ _ _ _) = eti
getEti (UIntBiOp eti _ _ _ _) = eti
getEti (FloatBiOp eti _ _ _ _) = eti
getEti (BoolBiOp eti _ _ _ _) = eti
getEti (UnaryOp eti _ _ _) = eti
getEti (Copy eti _ _) = eti
getEti (Goto eti1 _) = eti1
getEti (Gotoz eti1 _ _) = eti1
getEti (Gotonz eti1 _ _) = eti1
getEti (Parametro eti _) = eti
getEti (Call eti _ _ _) = eti
getEti (Copy1 eti _ _ _) = eti
getEti (Copy2 eti _ _ _) = eti
getEti (Copy3 eti _ _) = eti
getEti (Copy4 eti _ _) = eti
getEti (Copy5 eti _ _) = eti
getEti (Comentario _) = EtiErr
getEti (Fall eti) = eti
getEti (Prologo eti _) = eti
getEti (Epilogo eti _) = eti
getEti (Return eti) = eti
getEti (ReturnW eti _) = eti
getEti (TacParam eti _) = eti
getEti (Clean eti _) = eti

-- conjunto de Tac

newtype TacIns = TacIns (Seq Tac) deriving (Generic)
instance Serialize TacIns
instance Show TacIns where
  show (TacIns s) = unlines $ map show $ toList s

fstEti :: TacIns -> Etiqueta
fstEti (TacIns s) = getEti $ index s 0

tacInsEmpty = TacIns empty

insJoin (TacIns t1) (TacIns t2) = TacIns (t1 >< t2)

tacInsSeqOp f (TacIns t1) a = TacIns (f t1 a)
tiso = tacInsSeqOp

compaq2file :: TacIns -> String -> IO ()
compaq2file tac file = B.writeFile file $ encodeLazy tac

decodeFromFile :: String -> IO (Either String TacIns)
decodeFromFile file = fmap decodeLazy (B.readFile file)


-- state para correr el tac

data TacState = TacState {
  susymt :: Map.Map TypeData (T.Tabla String (TypeData,Offset)),
  registros :: [Registro],
  etiquetas :: [Etiqueta],
  padre :: [(T.Tabla String (TypeData,Offset),Int)],
  simT :: T.Tabla String (TypeData,Offset),
  actual :: Int,
  tamF :: (TypeData -> Int),
  continueEti :: Etiqueta,
  breakEti :: Etiqueta,
  strings :: Map.Map String Etiqueta
}
instance Show TacState where
  show s = "TacState "++(show.head.registros $ s) ++ " " ++ (show.head.etiquetas $ s)

stateInit :: T.Tabla String (TypeData,Offset) -> Map.Map TypeData (T.Tabla String (TypeData,Offset)) -> Map.Map TypeData Int -> TacState
--stateInit t su tamt = TacState su regGen etiGen [] (index (T.hijos t) 0) 0 ((flip tam) tamt) EtiErr EtiErr Map.empty
stateInit t su tamt = TacState su regGen etiGen [] t 0 ((flip tam) tamt) EtiErr EtiErr Map.empty

enCliclo :: Etiqueta -> Etiqueta -> State TacState TacIns -> State TacState TacIns
enCliclo conEti bEti action = do
  s <- get
  let prevCon = continueEti s
  let prevBreak = breakEti s
  modify (\s-> s{continueEti = conEti, breakEti = bEti })
  resul <- action
  modify (\s-> s{continueEti = prevCon, breakEti = prevBreak })
  return resul

nextReg :: State TacState Registro
nextReg = state (\s -> (head.registros $ s,s {registros=tail.registros $ s}))

nextEti :: State TacState Etiqueta
nextEti = state (\s -> (head.etiquetas $ s,s {etiquetas=tail.etiquetas $ s}))

godown :: State TacState ()
godown = do
  s <- get
  trace ((show $ actual s)++" "++(show $ length $ T.hijos $ simT s)) (return ())
  modify $ (\s-> let h = index (T.hijos $ simT s) (actual s) in s{padre= (simT s,actual s) : padre s,simT= h,actual= 0})

goup :: State TacState ()
goup = modify $ (\s-> let h = head.padre $ s in s{padre=tail.padre $ s,actual=(snd h) + 1, simT=fst h})

querryST :: String -> State TacState (Maybe (TypeData,Offset))
querryST s = get >>= return.(T.lookup s).simT

querrySU :: TypeData -> String -> State TacState (Maybe (TypeData,Offset))
querrySU strUni s = get >>= return.(maybe (Nothing) (T.lookup s)).(Map.lookup strUni).susymt

tamf :: State TacState (TypeData -> Int)
tamf = get >>= return.tamF

toTac :: [Instruc] -> T.Tabla String (TypeData,Offset) -> Map.Map TypeData (T.Tabla String (TypeData,Offset)) -> Map.Map TypeData Int -> TacIns
toTac i t su tamt = evalState (fctac i) (stateInit t su tamt)

fctac :: [Instruc] -> State TacState TacIns
fctac i = do
  evaluados <- mapM fcIns2Tac i
  return (foldl1 insJoin evaluados)

gg s = return $ tiso (|>) tacInsEmpty (Comentario s)

fcIns2Tac :: Instruc -> State TacState TacIns
fcIns2Tac (InstrucFun s i) = do
  trace (s) (return ())
  eti1 <- nextEti
  eti2 <- nextEti
  let pro = tiso (|>) tacInsEmpty $ Prologo eti1 s
  let epi = tiso (|>) tacInsEmpty $ Epilogo eti2 s
  godown
  r<-fcIns2Tac i
  goup
  return $ insJoin pro $ insJoin r epi
fcIns2Tac (InstrucBlock i) = do
  trace ("Block") (return ())
  godown
  r <- fctac i
  goup
  return r
fcIns2Tac (InstrucExpre e _ _) = do
  t <- nextReg
  fcExpre2Tac t e
fcIns2Tac (InstrucAsing e1 e2 _ _) = do
  temp1 <- nextReg
  asignado <- case e1 of
    ExpreArr{} -> buscaLugar temp1 e1
    ExpreDeRef{} -> fcExpre2Tac temp1 e1
  if isBoolExpre e2 then do
    etiTrue <- nextEti
    etiFalse <- nextEti
    let codeTrue = tiso (|>) tacInsEmpty $ Copy5 etiTrue (Val temp1) (BasicBool True)
    let codeFalse = tiso (|>) tacInsEmpty $ Copy5 etiFalse (Val temp1) (BasicBool False)
    code <- boolExp e2 etiTrue etiFalse
    return $ insJoin asignado $ insJoin code $ insJoin codeTrue codeFalse
  else do
    temp2 <- nextReg
    expre <- fcExpre2Tac temp2 e2
    eti <- nextEti
    return $ insJoin (insJoin asignado expre) $ tiso (|>) tacInsEmpty $ Copy5 eti (Val temp1) (Val temp2)
fcIns2Tac (InstrucIf bool block _ _) = do
  blockIns <- fcIns2Tac block
  let fstBlock = fstEti blockIns
  eti <- nextEti
  let fall = tiso (|>) tacInsEmpty $ Fall eti
  boolins <- boolExp bool fstBlock eti
  return $ insJoin boolins $ insJoin blockIns fall
fcIns2Tac (InstrucIfElse bool block1 block2 _ _) = do
  blockIns1 <- fcIns2Tac block1
  let fstBlock1 = fstEti blockIns1
  blockIns2 <- fcIns2Tac block2
  let fstBlock2 = fstEti blockIns2
  eti <- nextEti
  let fall = tiso (|>) tacInsEmpty $ Fall eti
  eti2 <- nextEti
  let go = tiso (|>) tacInsEmpty $ Goto eti2 eti
  boolins <- boolExp bool fstBlock1 fstBlock2
  return $ insJoin boolins $ insJoin blockIns1 $ insJoin go $ insJoin blockIns2 fall
fcIns2Tac (InstrucWhile bool block _ _) = do
  eti1 <- nextEti
  eti2 <- nextEti
  enCliclo eti2 eti1 $ do
    blockIns <- fcIns2Tac block
    let fstBlock = fstEti blockIns
    let fall = tiso (|>) tacInsEmpty $ Fall eti1
    boolins <- boolExp bool fstBlock eti1
    let boolfst = fstEti boolins
    let go = tiso (|>) tacInsEmpty $ Goto eti2 boolfst
    return $ insJoin boolins $ insJoin blockIns $ insJoin go fall
fcIns2Tac (InstrucFor init condition increment codeBlock _ _) = do
  initIns <- fcIns2Tac init
  incIns <- fcIns2Tac increment
  eti1 <- nextEti
  eti2 <- nextEti
  enCliclo (fstEti incIns) eti1 $ do
    codeIns <- fcIns2Tac codeBlock
    let codefst = fstEti codeIns
    let fall = tiso (|>) tacInsEmpty $ Fall eti1
    boolins <- boolExp condition codefst eti1
    let go = tiso (|>) tacInsEmpty $ Goto eti2 (fstEti boolins)
    return $ insJoin initIns $ insJoin boolins $ insJoin codeIns $ insJoin incIns $ insJoin go fall
fcIns2Tac (InstrucBreak _ _) = do
  eti <- nextEti
  s <- get
  let code = tiso (|>) tacInsEmpty $ Goto eti (breakEti s)
  return code
fcIns2Tac (InstrucCon _ _) = do
  eti <- nextEti
  s <- get
  let code = tiso (|>) tacInsEmpty $ Goto eti (continueEti s)
  return code
fcIns2Tac (InstrucRet _ _) = do
  eti <- nextEti
  let code = tiso (|>) tacInsEmpty $ Return eti
  return code
fcIns2Tac (InstrucRetW e _ _) = do
  t <- nextReg
  expre <- fcExpre2Tac t e
  eti <- nextEti
  let code = tiso (|>) tacInsEmpty $ ReturnW eti (Val t)
  return $ insJoin expre code
fcIns2Tac i = gg ("Una Instruccion\n"++show i)

ayudaConArr :: TypeData -> Registro -> Expre -> State TacState TacIns
ayudaConArr (TArray _ t) r (ExpreArr s (e:ex) i j ty) = do
  f<-tamf
  let tam = f t
  t1 <- nextReg
  i1 <- fcExpre2Tac t1 e
  t2 <- nextReg
  i2 <- ayudaConArr t t2 (ExpreArr s ex i j t)
  let pre = insJoin i1 i2
  eti <- nextEti
  eti2 <- nextEti
  t3 <- nextReg
  let post = insJoin pre $ tiso (|>) tacInsEmpty $ IntBiOp eti (string2operator "*") (Val t1) (BasicInt tam) (Val t3)
  let post2 = insJoin post $ tiso (|>) tacInsEmpty $ IntBiOp eti2 (string2operator "+") (Val t3) (Val t2) (Val r)
  return post2
ayudaConArr _ r e = buscaLugar r e


buscaLugar :: Registro -> Expre -> State TacState TacIns
buscaLugar r (ExpreArr s [] _ _ _) = do
  (tipo,offs) <- querryST s >>= return.fromJust
  f<-tamf
  re <- case offs of
    Param _ i -> return $ FramePtr (i - (f tipo))
    Local _ i -> return $ FramePtr i
    Global i -> return $ BasePtr i
  eti <- nextEti
  return $ tiso (|>) tacInsEmpty $ Copy eti (Val r) re
buscaLugar r (ExpreArr s l i c t@(TArray _ _)) = ayudaConArr t r (ExpreArr s (P.reverse l) i c t)
buscaLugar r e = gg $ " mal "++ show e

type2binop TInt = IntBiOp
type2binop (TUnsigned TInt) = UIntBiOp
type2binop TFloat = FloatBiOp
type2binop TBool = BoolBiOp

string2operator "%" = Mod
string2operator "+" = Mas
string2operator "-" = Menos
string2operator "*" = Por
string2operator "/" = Entre
string2operator ">" = GT
string2operator "<" = Lt
string2operator "==" = Eq
string2operator ">=" = EGT
string2operator "<=" = ELT
string2operator "&&" = And
string2operator "||" = Or
string2operator "!=" = NEq
string2operator ">>" = RShift
string2operator "<<" = LShift
string2operator "&" = BitAnd
string2operator "|" = BitOr

isBoolOper "%" = False
isBoolOper "+" = False
isBoolOper "-" = False
isBoolOper "*" = False
isBoolOper "/" = False
isBoolOper ">" = True
isBoolOper "<" = True
isBoolOper "==" = True
isBoolOper ">=" = True
isBoolOper "<=" = True
isBoolOper "&&" = True
isBoolOper "||" = True
isBoolOper "!=" = True
isBoolOper ">>" = False
isBoolOper "<<" = False
isBoolOper "&" = False
isBoolOper "|" = False

isBoolExpre (ExpreBin oper _ _ _ _ _) = isBoolOper oper
isBoolExpre _ = False

boolExp :: Expre -> Etiqueta -> Etiqueta -> State TacState TacIns
boolExp (ExpreBin "&&" e1 e2 _ _ _) trueEti falseEti = do
  b2 <- boolExp e2 trueEti falseEti
  let fstb2 = fstEti b2
  b1 <- boolExp e1 fstb2 falseEti
  return $ insJoin b1 b2
boolExp (ExpreBin "||" e1 e2 _ _ _) trueEti falseEti = do
  b2 <- boolExp e2 trueEti falseEti
  let fstb2 = fstEti b2
  b1 <- boolExp e1 trueEti fstb2
  return $ insJoin b1 b2
boolExp (ExpreBin oper e1 e2 _ _ _) trueEti falseEti = do
  t1 <- nextReg
  i1 <- fcExpre2Tac t1 e1
  t2 <- nextReg
  i2 <- fcExpre2Tac t2 e2
  let pre = insJoin i1 i2
  eti <- nextEti
  t3 <- nextReg
  let code = tiso (|>) tacInsEmpty $ BoolBiOp eti (string2operator oper) (Val t1) (Val t2) (Val t3)
  eti <- nextEti
  let code2 = tiso (|>) tacInsEmpty $ Gotonz eti trueEti (Val t3)
  eti <- nextEti
  let code3 = tiso (|>) tacInsEmpty $ Goto eti falseEti
  return $ insJoin pre $ insJoin code $ insJoin code2 code3
boolExp (ExpreBasicBool b _ _) trueEti falseEti = do
  eti <- nextEti
  let code = tiso (|>) tacInsEmpty $ if b then Goto eti trueEti else Goto eti falseEti
  return code
boolExp e _ _ = gg ("Como llego aki?\n"++show e)

fcExpre2Tac :: Registro -> Expre -> State TacState TacIns
fcExpre2Tac r (ExpreBin s e1 e2 _ _ tipo) = do
  t1 <- nextReg
  i1 <- fcExpre2Tac t1 e1
  t2 <- nextReg
  i2 <- fcExpre2Tac t2 e2
  let pre = insJoin i1 i2
  eti <- nextEti
  let f = type2binop tipo
  return $ insJoin pre $ tiso (|>) tacInsEmpty $ f eti (string2operator s) (Val t1) (Val t2) (Val r)

fcExpre2Tac r (ExpreBasicInt v _ _) = do
  eti <- nextEti
  return $ tiso (|>) tacInsEmpty $ Copy eti (Val r) (BasicInt v)
fcExpre2Tac r (ExpreBasicFloat v _ _) = do
  eti <- nextEti
  return $ tiso (|>) tacInsEmpty $ Copy eti (Val r) (BasicFloat v)
fcExpre2Tac r (ExpreBasicBool v _ _) = do
  eti <- nextEti
  return $ tiso (|>) tacInsEmpty $ Copy eti (Val r) (BasicBool v)

fcExpre2Tac r (ExpreBasicStr s _ _) = do
  estado <- get
  eti <- if Map.notMember  s (strings estado) then do
    eti <- nextEti
    modify (\es -> es{strings = Map.insert s eti $ strings es})
    return eti
  else do
    return $ fromJust $ Map.lookup s (strings estado)
  eti2 <- nextEti
  return $ tiso (|>) tacInsEmpty $ Copy eti2 (Val r) (DirOf eti)

fcExpre2Tac r (ExpreMono s e _ _ tipo) = do
  t1 <- nextReg
  i1 <- fcExpre2Tac t1 e
  eti <- nextEti
  return $ insJoin i1 $ tiso (|>) tacInsEmpty $ UnaryOp eti (string2operator s) (Val t1) (Val r)

fcExpre2Tac r e@(ExpreArr _ _ _ _ _) = do
  t1 <- nextReg
  i <- buscaLugar t1 e
  eti <- nextEti
  return $ insJoin i $ tiso (|>) tacInsEmpty $ Copy4 eti (Val r) (Val t1)

fcExpre2Tac r (ExpreDeRef e _ _ ) = do
  t1 <- nextReg
  i <- fcExpre2Tac t1 e
  eti <- nextEti
  return $ insJoin i $ tiso (|>) tacInsEmpty $ Copy4 eti (Val r) (Val t1)
{-
fcExpre2Tac r (ExpreField s e _ _) = do
  t1 <- nextReg
  i <- fcExpre2Tac t1 e

  eti <- nextEti
  return $ insJoin i $ tiso (|>) tacInsEmpty $ Copy4 eti (Val r) (Val t1)
-}
fcExpre2Tac r (ExpreLLamada s le _ _ ) = do
  let tam = P.length le
  code <- expreList2Param $ P.reverse le
  eti <- nextEti
  let code2 = tiso (|>) tacInsEmpty $ Call eti s tam (Val r)
  eti <- nextEti
  let code3 = tiso (|>) tacInsEmpty $ Clean eti tam
  return $ insJoin code $ insJoin code2 code3

fcExpre2Tac _ e = gg $ "Una expresion\n"++show e

expreList2Param :: [Expre] -> State TacState TacIns
expreList2Param [] = return tacInsEmpty
expreList2Param (e:xe) = do
  resto <- expreList2Param xe
  t <- nextReg
  i <- fcExpre2Tac t e
  eti <- nextEti
  let code = tiso (|>) tacInsEmpty $ TacParam eti (Val t)
  return $ insJoin resto $ insJoin i code








--
