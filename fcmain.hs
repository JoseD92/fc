import System.IO
import System.Environment
import Fc.Lexer
import Data.List
import Fc.Grammar
import Data.Char (ord)
import Control.Monad.Writer
import Control.Monad.State
import qualified Fc.Tabla as T
--getopt -- buscar
errStrPut = hPutStr stderr

filfun (Error _ _) = False
filfun _ = True

inicializaStado = (\s-> T.enterScope $ T.insert "write" FunGlob $ T.insert "read" FunGlob s)

gg t = do
  parsefc t
  modify (\(t,s,s2)->(T.goToRoot t,s,s2))

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
    putStrLn $ unlines.reverse.(\(_,s,_)->s) $ execState (gg tokens) $ (inicializaStado T.empty,[],[])
    putStrLn $ (T.dump).(\(s,_,_)->s) $ execState (gg tokens) $ (inicializaStado T.empty,[],[])
    putStrLn $ unlines.(\(_,_,s)->s) $ execState (gg tokens) $ (inicializaStado T.empty,[],[])
  else return ()