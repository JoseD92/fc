module Fc.MyState (
  ParseState(pilaAT),
  empty,
  alterSimT,
  addError,
  addString,
  querrySimT,
  empilaAT,
  ponAT,
  modifAType,
  getAType,
  newAusS
)where
import qualified Fc.Tabla as T
import Fc.Datas (TypeData)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Fc.Datas (TypeData(..))

data ParseState = ParseState {
    simT :: T.Tabla String TypeData,
    errores :: Seq.Seq String,
    stringT :: Map.Map String Bool,
    activeType :: TypeData,
    pilaAT :: [TypeData],
    auxS :: String
  }

newAusS s parS = parS {auxS=s}

empilaAT parS = parS {pilaAT=(activeType parS):(pilaAT parS)}

ponAT parS = parS {pilaAT=(activeType parS):[]}

empty = ParseState T.empty Seq.empty Map.empty TVoid [] ""

modifAType f parS = parS {activeType=f (activeType parS)}

getAType = activeType

alterSimT f parS = parS {simT=f (simT parS)}

querrySimT f parS = f (simT parS)

addError parS e = parS {errores=(Seq.|>) (errores parS) e }

addString s parS = parS {stringT=(Map.insert) s True (stringT parS) } 

instance Show ParseState where
  show parS = unlines $ [ unlines.toList $ errores parS , "" , (T.dump $ simT parS) , "" , unlines.(map fst).(Map.toList) $ stringT parS ]