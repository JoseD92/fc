{-# LANGUAGE DeriveGeneric #-}
module Fc.Tac.Tac (
compaq2file,decodeFromFile,Value(..),TacIns(..),Tac(..),Registro(..),Etiqueta(..),regGen,etiGen,BiOperators(..)
) where

import qualified Data.ByteString.Lazy as B
import           Data.Sequence
import           Data.Serialize
import           Data.Word
import           GHC.Generics
import Prelude hiding (GT)
import Data.Foldable (toList)

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
  | Param Etiqueta Value
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
  show (Param eti val) = show eti ++ ":\tParam " ++ show val
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

compaq2file :: TacIns -> String -> IO ()
compaq2file tac file = B.writeFile file $ encodeLazy tac

decodeFromFile :: String -> IO (Either String TacIns)
decodeFromFile file = fmap decodeLazy (B.readFile file)
