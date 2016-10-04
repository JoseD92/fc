import Fc.Tac.Tac
import Data.Sequence
import System.Environment
import Data.Word
import qualified Fc.Tabla as T

main = do
  args <- getArgs
  let anop = (IntBiOp (Eti 0) Menos (Val $ Reg 1) (Val $ Reg 1) (Val $ Reg 1))
  let anop2 = Copy1 (Eti 3) (Val $ Reg 4) (VarLoc "i" 4) [Cons 5,(Val $ Reg 6)]
  let mytac = TacIns $ empty |> anop |> anop |> anop |> anop2
  putStrLn $ show mytac
  compaq2file mytac (head args)
  tac2 <- decodeFromFile (head args)
  either putStrLn (putStrLn.show) tac2
