module Fc.Datas (TypeData(..),operAcc,operAccMono)
where

import qualified Fc.Tabla as T(Tabla(info,hijos))
import qualified Data.Map.Strict as Map
import Data.Maybe

data TypeData = FunGlob TypeData [TypeData] | FunLoc TypeData [TypeData] | TInt | TFloat| TBool | TAny | TError |
  TVoid | TChar | TArray Int TypeData | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Show,Eq,Ord)

operAcc :: TypeData -> String -> TypeData -> TypeData
operAcc TError _ _ = TError
operAcc _ _ TError = TError
operAcc TAny _ x = x
operAcc x _ TAny = x
operAcc x _ y = if (x==y) then x else TError
operAcc _ _ _ = TError

operAccMono :: String -> TypeData -> TypeData
operAccMono _ TError = TError
operAccMono _ TAny = TAny
operAccMono _ x = x
operAccMono _ _ = TError

tam :: TypeData -> Map.Map TypeData (T.Tabla String TypeData) -> Int
tam TInt _ = 4
tam TFloat _ = 4
tam (TRef _) _ = 4
tam TBool _ = 1
tam TChar _ = 1
tam (TArray i x) m = i*(tam x m)
tam (TUnsigned x) m = tam x m
--tam (TStruct s) susymt = (sum (map tam (T.info laTabla)))+
--  where laTabla = Map.lookup s susymt
--tam (TUnion s) susymt =
--  where laTabla = Map.lookup s susymt
tam _ _ = 0