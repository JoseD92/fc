{-# LANGUAGE DeriveGeneric #-}
module Fc.Tac.Tac (
compaq2file,decodeFromFile,TacIns,Tac(..),Registro(..),Etiqueta(..),regGen,etiGen,BiOperators(..)
) where

import qualified Data.ByteString.Lazy as B
import           Data.Sequence
import           Data.Serialize
import           Data.Word
import           GHC.Generics

type DataSize = Word64 --tipo para los registros y etiquetas
-- 64 bits porque correra en maquinas de 64bits,
-- si se sabe correra en 32bits se puede cambiar a word32

--      Registros o temporales
newtype Registro = Reg DataSize deriving (Show,Generic)
instance Serialize Registro

regGen :: [Registro]
regGen = map Reg [0 ..]

--      Etiquetas
newtype Etiqueta = Eti DataSize deriving (Show,Generic)
instance Serialize Etiqueta

etiGen :: [Etiqueta]
etiGen = map Eti [0 ..]

-- instrucciones de Tac

data BiOperators = Mod | Mas | Menos | Por | Entre | GT | Lt | Eq | EGT | ELT
  | And | Or | NEq | RShift | LShift deriving (Show,Generic)
instance Serialize BiOperators

data Tac = IntOp Etiqueta BiOperators Registro Registro Registro
  | FloatOp Etiqueta BiOperators Registro Registro Registro
  | Comentario String
  deriving (Show,Generic)
instance Serialize Tac

-- conjunto de Tac

type TacIns = Seq Tac

compaq2file :: TacIns -> String -> IO ()
compaq2file tac file = B.writeFile file $ encodeLazy tac

decodeFromFile :: String -> IO (Either String TacIns)
decodeFromFile file = fmap decodeLazy (B.readFile file)
