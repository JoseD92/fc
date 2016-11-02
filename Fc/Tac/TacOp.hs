module Fc.Tac.TacOp (
quitaFalls
) where

import      qualified     Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Fc.Tac.Tac
import Data.Foldable (toList)
import Data.Maybe (maybe)

nextNonFall ((Fall _):xs) = nextNonFall xs
nextNonFall (x:_) = x

buscaFalls [] m = m
buscaFalls ((Fall e):xs) m = buscaFalls xs (Map.insert e (getEti.nextNonFall $ xs) m)
buscaFalls (_:xs) m = buscaFalls xs m

subGotoFall m [] = []
subGotoFall m ((Fall _):xs) = subGotoFall m xs
subGotoFall m (g@(Goto e1 e2):xs) = (maybe g (\e->Goto e1 e) (Map.lookup e2 m)):subGotoFall m xs
subGotoFall m (g@(Gotoz e1 e2 v):xs) = (maybe g (\e->Gotoz e1 e v) (Map.lookup e2 m)):subGotoFall m xs
subGotoFall m (g@(Gotonz e1 e2 v):xs) = (maybe g (\e->Gotonz e1 e v) (Map.lookup e2 m)):subGotoFall m xs
subGotoFall m (x:xs) = x:subGotoFall m xs

quitaFalls (TacIns s) = TacIns $ Seq.fromList l2
  where
    l = toList s
    m = buscaFalls l Map.empty
    l2 = subGotoFall m l
