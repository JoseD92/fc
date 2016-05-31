module Fc.MyState (
  ParseState,
  empty,
  alterSimT,
  addError,
  addString,
  querrySimT
)where
import qualified Fc.Tabla as T
import Fc.Datas (TypeData)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

data ParseState = ParseState {
    simT :: T.Tabla String TypeData,
    errores :: Seq.Seq String,
    stringT :: Map.Map String Bool
  }

empty = ParseState T.empty Seq.empty Map.empty

alterSimT f parS = parS {simT=f (simT parS)}

querrySimT f parS = f (simT parS)

addError parS e = parS {errores=(Seq.|>) (errores parS) e }

addString s parS = parS {stringT=(Map.insert) s True (stringT parS) } 

instance Show ParseState where
  show parS = unlines $ [ unlines.toList $ errores parS , "" , (T.dump $ simT parS) , "" , unlines.(map fst).(Map.toList) $ stringT parS ]