module Fc.Datas (TypeData(..))
where

data TypeData = FunGlob | FunLoc [TypeData] [TypeData] | TInt | TFloat| TBool | 
  TVoid | TChar | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Show,Eq)