{-# LANGUAGE DeriveGeneric #-}
module Fc.Tabla (
  empty,
  insert,
  lookup,
  destroyLocal,
  dumpLocal,
  enterScope,
  exitScope,
  dump,
  Tabla(info,hijos),
  enterN,
  goToRoot,
  localLookup,
  pertenece,
  eliminaPrimero
) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Prelude hiding (lookup)
import Data.Tree
import Data.Tree.Pretty

data Tabla k a = Tabla {
    info :: Map.Map k a,
    padre :: Tabla k a,
    hijos :: Seq.Seq (Tabla k a)
  } | Null deriving (Show)

isNull :: Tabla k a -> Bool
isNull Null = True
isNull _ = False

empty = Tabla (Map.empty) Null Seq.empty

insert k v t = actualizaHijos (padre t) $ t {info= Map.insert k v (info t) }

lookup :: (Ord k) => k -> Tabla k a -> Maybe a
lookup k t = help ans
  where
    ans = Map.lookup k $ info t
    help Nothing = if (isNull $ padre t) then Nothing
                   else lookup k (padre t)
    help a = a
--buscar funcion maybe

localLookup :: (Ord k) => k -> Tabla k a -> Maybe a
localLookup k t = Map.lookup k $ info t

destroyLocal t = actualizaHijos (padre t) $ t {info=Map.empty}

dumpLocal :: (Show a) => Tabla k a -> String
dumpLocal = unwords.(map (\s->"{"++s++"}")).(map show).(foldl (flip (:)) []).info

enterScope t = x
  where
    x = empty {padre=y}
    y = t {hijos=(Seq.<|) x (hijos t)}

exitScope t@(Tabla _ Null _) = error "esto es malo"
exitScope t@(Tabla _ p _) = actualizaHijos (padre y) y
  where
    x = t {padre=y}
    y = p {hijos= (Seq.<|) x $ Seq.drop 1 (hijos p)}

actualizaHijos padre t = y
  where
    x = fmap (actualizaHijos y) (hijos t)
    y = t {hijos= x,padre=padre}

toTree x = Node (dumpLocal x) ((map toTree).(foldl (flip (:)) []).hijos $ x)

dump t = drawTree $ toTree t

enterN i t = Seq.index (hijos t) i

goToRoot t@(Tabla _ Null _) = t
goToRoot t@(Tabla _ p _) = goToRoot $ exitScope t

pertenece :: (Eq a) => a -> Tabla b a -> Bool
pertenece s t = (elem s (Map.elems (info t))) || or (fmap (pertenece s) (hijos t))

eliminaPrimero t = t{hijos=Seq.drop 1 (hijos t)}
