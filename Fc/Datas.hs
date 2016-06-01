module Fc.Datas (TypeData(..))
where

import qualified Fc.Tabla as T

data TypeData = FunGlob | FunLoc [TypeData] [TypeData] | TInt | TFloat| TBool | 
  TVoid | TChar | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Show,Eq)