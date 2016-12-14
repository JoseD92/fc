import System.IO
import System.Environment
import Fc.Lexer
import Data.List
import Fc.Grammar
import Data.Char (ord)
import Control.Monad.Writer
import Control.Monad.State
import qualified Fc.Tabla as T
import Fc.MyState
import Fc.Datas
import Fc.Tac.Tac
import Fc.Tac.TacOp
import Fc.Tac.Tac2Mips
--getopt -- buscar

filfun (Error _ _) = False
filfun _ = True

run t = do
  r <- parsefc t
  modify (alterSimT T.goToRoot)
  return r

main :: IO ()
main = do
  args <- getArgs
  s <- if (or $ map (\s->"--"/=(take 2 s)) args) then readFile (head $ filter (\s->"--"/=(take 2 s)) args)
  else getContents

  let (tokens,errores) = partition filfun (alexScanTokens s)
  when (elem "--lexer" args) $ do
    putStr $ (unlines $ map show $ tokens) ++ "\n"
    errStrPut $ (unlines $ map show $ errores) ++ "\n"

  (out,estado) <- runStateT (run tokens) parseStateEmpty

  --let arbol = parsefc tokens
  if (elem "--parser" args) then do
    putStr $ show estado
    putStr $ (show $ tipo out) ++ "-----------------"
  else return ()

  if (elem "--arbol" args) then do
    mapM_ (putStrLn.(insToStr 0)) $ reverse.insList $ out
  else return ()

  let (pretac,strings) = toTac (reverse.insList $ out) (T.tablaReverse $ T.goToRoot (simT estado)) (susymt estado) (tamTable estado)
  let tac = quitaFalls $ pretac
  --print tac
  let textLast = basics tac strings
  let datazone = ".data\n"++strings2code strings
  textHeader <- readFile "mipsTextHeader.asm"
  writeFile "a.s" $ datazone++textHeader++"\n"++textLast
