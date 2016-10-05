{-# LANGUAGE DeriveGeneric #-}
module Fc.Tac.Tac (
compaq2file,decodeFromFile,Value(..),TacIns(..),Tac(..),Registro(..),Etiqueta(..),regGen,etiGen,BiOperators(..),toTac
) where

import qualified Data.ByteString.Lazy as B
import           Data.Sequence
import           Data.Serialize (Serialize,encodeLazy,decodeLazy)
import           Data.Word
import           GHC.Generics
import Prelude hiding (GT)
import Data.Foldable (toList)
import Control.Monad.State
import Fc.Datas
import Control.Monad (mapM)

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
newtype Etiqueta = Eti DataSize deriving (Generic)
instance Serialize Etiqueta
instance Show Etiqueta where
  show (Eti x) = show x

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
  Cons DataSize
  deriving (Generic)
instance Serialize Value
instance Show Value where
  show (Val x) = show x
  show (VarGlobal s) = s
  show (VarLoc s _) = s
  show (Cons x) = show x

data Tac = IntBiOp Etiqueta BiOperators Value Value Value
  | UIntBiOp Etiqueta BiOperators Value Value Value
  | FloatBiOp Etiqueta BiOperators Value Value Value
  | UnaryOp Etiqueta BiOperators Value Value
  | Copy Etiqueta Value Value
  | Goto Etiqueta Etiqueta
  | Gotoz Etiqueta Etiqueta Value --goto if zero
  | Gotonz Etiqueta Etiqueta Value --goto if not zero
  | Parametro Etiqueta Value
  | Call Etiqueta String Int -- llama a una funcion con n parametros
  | Copy1 Etiqueta Value Value [Value] -- x:=y[i,j,...]   el segundo value debe ser VarGlobal o VarLoc
  | Copy2 Etiqueta Value Value [Value] -- y[i,j,...]:=x   el segundo value debe ser VarGlobal o VarLoc
  | Copy3 Etiqueta Value Value -- x:=&y   el segundo value debe ser VarGlobal o VarLoc
  | Copy4 Etiqueta Value Value -- x:=*y
  | Copy5 Etiqueta Value Value -- *x:=y
  | Comentario String
  deriving (Generic)
instance Serialize Tac
instance Show Tac where
  show (IntBiOp eti oper r1 r2 resul) = show eti ++":\t"++ show resul ++" := "++ show r1 ++" "++ show oper ++" "++ show r2
  show (UIntBiOp eti oper r1 r2 resul) = show eti ++":\t"++ show resul ++" := "++ show r1 ++" "++ show oper ++" "++ show r2
  show (FloatBiOp eti oper r1 r2 resul) = show eti ++":\t"++ show resul ++" := "++ show r1 ++" "++ show oper ++" "++ show r2
  show (UnaryOp eti oper r1 resul) = show eti ++":\t"++ show resul ++" := "++ show oper ++" "++ show r1
  show (Copy eti r1 r2) = show eti ++":\t"++ show r1 ++" := "++ show r2
  show (Goto eti1 eti2) = show eti1 ++ ":\tGoto " ++ show eti2
  show (Gotoz eti1 eti2 r) = show eti1 ++":\tif "++ show r ++" Goto "++show eti2
  show (Gotonz eti1 eti2 r) = show eti1 ++":\tifnot "++ show r ++" Goto "++show eti2
  show (Parametro eti val) = show eti ++ ":\tParam " ++ show val
  show (Call eti s n) = show eti ++ ":\tCall " ++ s ++" "++ show n
  show (Copy1 eti r1 r2 l) = show eti ++":\t"++ show r1 ++" := "++ show r2 ++ help l
  show (Copy2 eti r1 r2 l) = show eti ++":\t" ++ show r2 ++ help l ++" := " ++ show r1
  show (Copy3 eti r1 r2) = show eti ++":\t"++ show r1 ++" := &"++ show r2
  show (Copy4 eti r1 r2) = show eti ++":\t"++ show r1 ++" := *"++ show r2
  show (Copy5 eti r1 r2) = show eti ++":\t*"++ show r1 ++" := "++ show r2
  show (Comentario s) = s

help l = concatMap (\x->"["++ show x ++"]") l

-- conjunto de Tac

newtype TacIns = TacIns (Seq Tac) deriving (Generic)
instance Serialize TacIns
instance Show TacIns where
  show (TacIns s) = unlines $ map show $ toList s

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
  registros :: [Registro],
  etiquetas :: [Etiqueta]
}
instance Show TacState where
  show s = "TacState "++(show.head.registros $ s) ++ " " ++ (show.head.etiquetas $ s)

stateEmpty :: TacState
stateEmpty = TacState regGen etiGen

nextReg :: State TacState Registro
nextReg = state (\s -> (head.registros $ s,s {registros=tail.registros $ s}))

nextEti :: State TacState Etiqueta
nextEti = state (\s -> (head.etiquetas $ s,s {etiquetas=tail.etiquetas $ s}))

toTac :: [Instruc] -> TacIns
toTac i = evalState (fctac i) stateEmpty

fctac :: [Instruc] -> State TacState TacIns
fctac i = do
  evaluados <- mapM fcIns2Tac i
  return (foldl1 insJoin evaluados)

gg s = return $ tiso (|>) tacInsEmpty (Comentario s)

fcIns2Tac :: Instruc -> State TacState TacIns
fcIns2Tac (InstrucFun "main" i) = fcIns2Tac i
fcIns2Tac (InstrucBlock i) = fctac i
fcIns2Tac (InstrucExpre e l c) = do
  i2 <- gg $ "Expresion en "++show l ++" "++show c
  i1 <- fcExpre2Tac e
  return $ insJoin i1 i2
fcIns2Tac (InstrucAsing e1 e2 l c) = gg $ "Asingnacion en "++show l ++" "++show c

fcIns2Tac _ = gg "Una Instruccion"


fcExpre2Tac :: Expre -> State TacState TacIns

{-
ExpreBin String Expre Expre Int Int
ExpreMono String Expre Int Int
ExpreBasicInt Int Int Int
ExpreBasicFloat Float Int Int
ExpreBasicBool Bool Int Int
ExpreArr String [Expre] Int Int
-}
--ExpreDeRef Expre Int Int
--ExpreField String Expre Int Int
--ExpreArr String [Expre] Int Int

fcExpre2Tac _ = gg "Una expresion"











--
