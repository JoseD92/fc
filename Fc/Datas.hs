module Fc.Datas (TypeData(..),operAcc,operAccMono)
where

import qualified Fc.Tabla as T

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