module Fc.MyState (
  ParseState(..),
  SReturn(..),
  sReturnEmpty,
  parseStateEmpty,
  alterSimT,
  addString,
  querrySimT,
  addus,
  buscasusymt
)where
import qualified Fc.Tabla as T
import Fc.Datas (TypeData)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Fc.Datas (TypeData(..))

data ParseState = ParseState {
    simT :: T.Tabla String TypeData,
    stringT :: Map.Map String Bool, --tabla de string
    susymt :: Map.Map TypeData (T.Tabla String TypeData) --tabla de simbolos de cada union/struct
  }

addus k v parS = parS {susymt=(Map.insert) k v (susymt parS) } 

buscasusymt k parS = Map.lookup k $ susymt parS

parseStateEmpty = ParseState T.empty Map.empty Map.empty

alterSimT f parS = parS {simT=f (simT parS)}

querrySimT f parS = f (simT parS)

addString s parS = parS {stringT=(Map.insert) s True (stringT parS) } 

instance Show ParseState where
  show parS = unlines $ [ (T.dump $ simT parS) , "" , unlines.(map fst).(Map.toList) $ stringT parS ,
    "", unlines.(map (\(x,y)->unlines [show x,T.dump y])).(Map.toList) $ susymt parS]



----------------------------------return del monad state

data SReturn = SReturn {
  tipo :: !TypeData,
  number :: !Int
} deriving (Show)

sReturnEmpty = SReturn TAny 0
