import System.IO
import System.Environment
import Fc.Lexer
import Data.List
import Fc.Grammar
import Data.Char (ord)
import Control.Monad.Writer
import Control.Monad.State
import qualified Fc.Tabla as T
import qualified Fc.MyState as MyState
import Fc.Datas (TypeData(..))
--getopt -- buscar
errStrPut = hPutStr stderr

filfun (Error _ _) = False
filfun _ = True

inicializaStado = (\s-> T.enterScope $ T.insert "write" (FunGlob TVoid [TAny]) $ T.insert "read" (FunGlob TAny []) s)

run t = parsefc t >> modify (MyState.alterSimT T.goToRoot)

main = do
  args <- getArgs
  s <- if (or $ map (\s->"--"/=(take 2 s)) args) then readFile (head $ filter (\s->"--"/=(take 2 s)) args)
  else getContents

  let (tokens,errores) = partition filfun (alexScanTokens s)
  if (elem "--lexer" args) then do
    putStr $ (unlines $ map show $ tokens) ++ "\n"
    errStrPut $ (unlines $ map show $ errores) ++ "\n"
  else return ()

  --let arbol = parsefc tokens
  if (elem "--parser" args) then do
    putStr $ show $ execState (run tokens) $ (MyState.alterSimT inicializaStado $ MyState.empty)
  else return ()