module Fc.Datas (TypeData(..))
where

import qualified Fc.Tabla as T

data TypeData = FunGlob TypeData [TypeData] | FunLoc TypeData [TypeData] | TInt | TFloat| TBool | TAny |
  TVoid | TChar | TArray Int TypeData | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Show,Eq)