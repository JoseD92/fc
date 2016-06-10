module Fc.MyState (
  ParseState(pilaAT,auxS,typePila),
  SReturn(..),
  sReturnEmpty,
  parseStateEmpty,
  alterSimT,
  addString,
  querrySimT,
  empilaAT,
  ponAT,
  modifAType,
  getAType,
  newAusS,
  push,
  typePilaOperate,
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
    activeType :: TypeData,
    pilaAT :: [TypeData],
    auxS :: (String,Int,Int),
    susymt :: Map.Map TypeData (T.Tabla String TypeData), --tabla de simbolos de cada union/struct
    typePila :: [TypeData]
  }

addus k v parS = parS {susymt=(Map.insert) k v (susymt parS) } 

buscasusymt k parS = Map.lookup k $ susymt parS

push t parS = parS {typePila=t:(typePila parS)}

typePilaOperate f parS = parS {typePila=f (typePila parS)}

newAusS s parS = parS {auxS=s}

empilaAT parS = parS {pilaAT=(activeType parS):(pilaAT parS)}

ponAT parS = parS {pilaAT=(activeType parS):[]}

parseStateEmpty = ParseState T.empty Map.empty TVoid [] ("",0,0) Map.empty []

modifAType f parS = parS {activeType=f (activeType parS)}

getAType = activeType

alterSimT f parS = parS {simT=f (simT parS)}

querrySimT f parS = f (simT parS)

addString s parS = parS {stringT=(Map.insert) s True (stringT parS) } 

instance Show ParseState where
  show parS = unlines $ [ (T.dump $ simT parS) , "" , unlines.(map fst).(Map.toList) $ stringT parS ,
    "", unlines.(map (\(x,y)->unlines [show x,T.dump y])).(Map.toList) $ susymt parS ,"" , show.head $ typePila parS]



----------------------------------return del monad state

data SReturn = SReturn {
  tipo :: !TypeData,
  number :: !Int
} deriving (Show)

sReturnEmpty = SReturn TAny 0
