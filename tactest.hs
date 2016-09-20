import Fc.Tac.Tac
import Data.Sequence
import System.Environment
import Data.Word

main = do
  args <- getArgs
  let anop = (IntOp (Eti 0) Menos (Reg 1) (Reg 1) (Reg 1))
  let mytac = empty |> anop |> anop |> anop
  putStrLn $ show mytac
  compaq2file mytac (head args)
  tac2 <- decodeFromFile "55"--(head args)
  either putStrLn (putStrLn.show) tac2
