module Fc.MyState (
  ParseState(..),
  SReturn(..),
  sReturnEmpty,
  parseStateEmpty,
  alterSimT,
  addString,
  querrySimT,
  addus,
  buscasusymt,
  addTam
)where
import qualified Fc.Tabla as T
import Fc.Datas (TypeData)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Fc.Datas (TypeData(..),tam)
import Data.Maybe

data ParseState = ParseState {
    simT :: T.Tabla String TypeData,
    stringT :: Map.Map String Bool, --tabla de string
    susymt :: Map.Map TypeData (T.Tabla String TypeData), --tabla de simbolos de cada union/struct
    tamTable :: Map.Map TypeData Int
  }

addus k v parS = parS {susymt=(Map.insert) k v (susymt parS) } 

addTam k v parS = parS {tamTable=(Map.insert) k v (tamTable parS) } 

buscasusymt k parS = Map.lookup k $ susymt parS

parseStateEmpty = ParseState T.empty Map.empty Map.empty Map.empty

alterSimT f parS = parS {simT=f (simT parS)}

querrySimT f parS = f (simT parS)

addString s parS = parS {stringT=(Map.insert) s True (stringT parS) } 

--instance Show ParseState where
--  show parS = unlines $ [ (T.dump $ simT parS) , "" , unlines.(map fst).(Map.toList) $ stringT parS ,
--    "", unlines.(map (\(x,y)->unlines [show x,T.dump y])).(Map.toList) $ susymt parS]


instance Show ParseState where
  show parS = (print3 (tamTable parS) "" 0 (simT parS)) ++ 
    (unlines.(map (\(x,y)->unlines [(show x) ++ " - tam: " ++(show.fromJust $ Map.lookup x (tamTable parS))
    ,print2 (tamTable parS) y])).(Map.toList) $ susymt parS)

print2 :: Map.Map TypeData Int -> T.Tabla String TypeData -> String
print2 tamTable t = unlines kvo
  where
    kv = (Map.toList) $ T.info t
    off = offset 0 $ map (\(k,v) -> tam v tamTable) kv
    kvo = zipWith (\(k,v) o-> "  "++k++": "++(show v)++" "++(show $ tam v tamTable)++" "++(show o)) kv off
  

print3 :: Map.Map TypeData Int -> String -> Int -> T.Tabla String TypeData -> String
print3 tamTable s i t = (unlines kvo) ++ paso2
  where
    kv = (Map.toList) $ T.info t
    off = offset i $ map (\(k,v) -> tam v tamTable) kv
    kvo = zipWith (\(k,v) o-> s++k++": "++(show v)++" "++(show $ tam v tamTable)++" "++(show o)) kv off
    seqs = (fmap (print3 tamTable (s++"  ") (last off)) (T.hijos t))
    paso2 = foldl (++) [] seqs

offset :: Int -> [Int] -> [Int]
offset i l = reverse $ foldl (\l x -> (x+(head l)):l) [i] l


----------------------------------return del monad state

data SReturn = SReturn {
  tipo :: !TypeData,
  number :: !Int
} deriving (Show)

sReturnEmpty = SReturn TAny 0
