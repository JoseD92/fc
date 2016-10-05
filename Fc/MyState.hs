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
  addTam,
  plusOffset,
  changeOff,
  restoreOff
)where
import qualified Fc.Tabla as T
import Fc.Datas (TypeData)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Fc.Datas (TypeData(..),tam,Expre(..),Instruc(..),Offset(..),tam)
import Data.Maybe

data ParseState = ParseState {
    simT :: T.Tabla String (TypeData,Offset),
    stringT :: Map.Map String Bool, --tabla de string
    susymt :: Map.Map TypeData (T.Tabla String (TypeData,Offset)), --tabla de simbolos de cada union/struct
    tamTable :: Map.Map TypeData Int,
    offsetFun :: (Int -> Offset),
    currentOff :: Int,
    globalOff :: Int,
    isglobal :: Bool
  }

addus k v parS = parS {susymt=(Map.insert) k v (susymt parS) }

addTam k v parS = parS {tamTable=(Map.insert) k v (tamTable parS) }

buscasusymt k parS = Map.lookup k $ susymt parS

parseStateEmpty = ParseState T.empty Map.empty Map.empty Map.empty Global 0 0 True

alterSimT f parS = parS {simT=f (simT parS)}

querrySimT f parS = f (simT parS)

plusOffset tipo parS = if isglobal parS then parS {globalOff=globalOff parS + tam tipo (tamTable parS)} else parS {currentOff=currentOff parS + tam tipo (tamTable parS)}

addString s parS = parS {stringT=(Map.insert) s True (stringT parS) }

changeOff f s parS = parS {offsetFun=f s,currentOff=0,isglobal=False}

restoreOff parS = parS {offsetFun=Global,isglobal=True}

--instance Show ParseState where
--  show parS = unlines $ [ (T.dump $ simT parS) , "" , unlines.(map fst).(Map.toList) $ stringT parS ,
--    "", unlines.(map (\(x,y)->unlines [show x,T.dump y])).(Map.toList) $ susymt parS]


instance Show ParseState where
  show parS = (print3 (tamTable parS) "" 0 (simT parS)) ++
    (unlines.(map (\(x,y)->unlines [(show x) ++ " - tam: " ++(show.fromJust $ Map.lookup x (tamTable parS))
    ,print2 (tamTable parS) y])).(Map.toList) $ susymt parS)

print2 :: Map.Map TypeData Int -> T.Tabla String (TypeData,Offset) -> String
print2 tamTable t = unlines kvo
  where
    kv = (Map.toList) $ T.info t
    off = offset 0 $ map (\(k,v) -> tam (fst v) tamTable) kv
    kvo = zipWith (\(k,v) o-> "  "++k++": "++(show v)++" "++(show $ tam (fst v) tamTable)++" "++(show o)) kv off


print3 :: Map.Map TypeData Int -> String -> Int -> T.Tabla String (TypeData,Offset) -> String
print3 tamTable s i t = (unlines kvo) ++ paso2
  where
    kv = (Map.toList) $ T.info t
    off = offset i $ map (\(k,v) -> tam (fst v) tamTable) kv
    kvo = zipWith (\(k,v) o-> s++k++": "++(show v)++" "++(show $ tam (fst v) tamTable)++" "++(show o)) kv off
    seqs = (fmap (print3 tamTable (s++"  ") (last off)) (T.hijos t))
    paso2 = foldl (++) [] seqs

offset :: Int -> [Int] -> [Int]
offset i l = reverse $ foldl (\l x -> (mimod (x+(head l))):l) [i] l

mimod x = 4-(mod x 4) + x

----------------------------------return del monad state

data SReturn = SReturn {
  tipo :: !TypeData,
  number :: !Int,
  expre :: Expre,
  expreList :: [Expre],
  ins :: Instruc,
  insList :: [Instruc],
  name :: String
}

sReturnEmpty = SReturn TAny 0 ExpreNull [] InstrucNull [] ""
