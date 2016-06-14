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
--getopt -- buscar

filfun (Error _ _) = False
filfun _ = True

inicializaStado = (\s-> T.enterScope $ T.insert "write" (FunGlob TVoid [TAny]) $ T.insert "read" (FunGlob TAny []) s)

run t = do
  r <- parsefc t
  modify (alterSimT T.goToRoot)
  return r

main = do
  args <- getArgs
  s <- if (or $ map (\s->"--"/=(take 2 s)) args) then readFile (head $ filter (\s->"--"/=(take 2 s)) args)
  else getContents

  let (tokens,errores) = partition filfun (alexScanTokens s)
  if (elem "--lexer" args) then do
    putStr $ (unlines $ map show $ tokens) ++ "\n"
    errStrPut $ (unlines $ map show $ errores) ++ "\n"
  else return ()

  (out,estado) <- runStateT (run tokens) $ (alterSimT inicializaStado $ parseStateEmpty)

  --let arbol = parsefc tokens
  if (elem "--parser" args) then do
    putStr $ show estado 
    putStr $ (show out) ++ "-----------------"
  else return ()
