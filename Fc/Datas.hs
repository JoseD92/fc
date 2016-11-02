{-# LANGUAGE DeriveGeneric #-}
module Fc.Datas (TypeData(..),operAcc,operAccMono,tam,Expre(..),Instruc(..),insToStr,Offset(..))
where

import qualified Fc.Tabla as T(Tabla(info,hijos))
import qualified Data.Map.Strict as Map
import Data.Maybe
import           Data.Serialize (Serialize)
import           GHC.Generics

data Offset = Param String Int | Local String Int | Global Int | SUVar String Int deriving (Show,Generic)
instance Serialize Offset

data TypeData = FunGlob TypeData [TypeData] | TInt | TFloat| TBool | TAny | TError |
  TVoid | TChar | TArray Int TypeData | TUnion String | TStruct String | TUnsigned TypeData | TRef TypeData deriving (Eq,Ord)

operAcc :: TypeData -> String -> TypeData -> TypeData
operAcc TError _ _ = TError
operAcc _ _ TError = TError
operAcc TAny _ x = x
operAcc x _ TAny = x
operAcc (TArray _ x) _ (TArray _ y) = if (x==y) then (TArray 0 x) else TError
operAcc TInt "||" TInt = TBool
operAcc TInt "&&" TInt = TBool
operAcc TInt ">" TInt = TBool
operAcc TInt "<" TInt = TBool
operAcc TInt ">=" TInt = TBool
operAcc TInt "<=" TInt = TBool
operAcc TInt "==" TInt = TBool
operAcc TInt "!=" TInt = TBool
operAcc (TUnsigned r) "||" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc (TUnsigned r) "&&" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc (TUnsigned r) ">" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc (TUnsigned r) "<" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc (TUnsigned r) ">=" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc (TUnsigned r) "<=" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc (TUnsigned r) "==" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc (TUnsigned r) "!=" (TUnsigned r2) = if r==r2 then TBool else TError
operAcc TFloat "||" TFloat = TBool
operAcc TFloat "&&" TFloat = TBool
operAcc TFloat ">" TFloat = TBool
operAcc TFloat "<" TFloat = TBool
operAcc TFloat ">=" TFloat = TBool
operAcc TFloat "<=" TFloat = TBool
operAcc TFloat "==" TFloat = TBool
operAcc TFloat "!=" TFloat = TBool
operAcc TChar "||" TChar = TBool
operAcc TChar "&&" TChar = TBool
operAcc TChar ">" TChar = TBool
operAcc TChar "<" TChar = TBool
operAcc TChar ">=" TChar = TBool
operAcc TChar "<=" TChar = TBool
operAcc TChar "==" TChar = TBool
operAcc TChar "!=" TChar = TBool
operAcc x _ y = if (x==y) then x else TError
operAcc _ _ _ = TError

operAccMono :: String -> TypeData -> TypeData
operAccMono _ TError = TError
operAccMono _ TAny = TAny
operAccMono _ x = x
operAccMono _ _ = TError

tam :: TypeData -> Map.Map TypeData Int -> Int
tam TInt _ = 4
tam TFloat _ = 4
tam (TRef _) _ = 4
tam TBool _ = 1
tam TChar _ = 1
tam (TArray i x) m = i*(tam x m)
tam (TUnsigned x) m = tam x m
tam s@(TStruct _) tamTable = fromJust $ Map.lookup s tamTable
tam s@(TUnion _) tamTable = fromJust $ Map.lookup s tamTable
tam _ _ = 0

instance Show TypeData where
  show (FunGlob _ _) = "Function"
  show (TInt) = "Int"
  show (TFloat) = "Float"
  show (TBool) = "Bool"
  show (TAny) = "Any"
  show (TError) = "Error"
  show (TVoid) = "Void"
  show (TChar) = "Char"
  show (TArray i x) = "Array("++(show i)++") of "++(show x)
  show (TUnion s) = "Union "++s
  show (TStruct s) = "Struct "++s
  show (TUnsigned x) = "Unsigned "++(show x)
  show (TRef x) = "Pointer to "++(show x)


----------------------tipos del ast

data Expre = ExpreBin String Expre Expre Int Int TypeData
  | ExpreMono String Expre Int Int TypeData
  | ExpreDeRef Expre Int Int
  | ExpreField String Expre Int Int
  | ExpreArr String [Expre] Int Int TypeData
  | ExpreLLamada String [Expre] Int Int
  | ExpreBasicInt Int Int Int
  | ExpreBasicFloat Float Int Int
  | ExpreBasicBool Bool Int Int
  | ExpreBasicStr String Int Int
  | ExpreNull --falta basicchar para representar las instrucciones que son un caracter
  deriving (Show)

expreToStr i (ExpreBin s e1 e2 _ _ _) = init.unlines $ ((replicate i ' ')++s):k
  where k = ((expreToStr (i+2)) e1):((expreToStr (i+2)) e2):[]
expreToStr i (ExpreMono s e1 _ _ _) = init.unlines $ ((replicate i ' ')++s):k
  where k = ((expreToStr (i+2)) e1):[]
expreToStr i (ExpreField s e1 _ _) = init.unlines $ ((replicate i ' ')++"Field: "++s):k
  where k = ((expreToStr (i+2)) e1):[]
expreToStr i (ExpreDeRef e1 _ _) = init.unlines $ ((replicate i ' ')++"Deref"):k
  where k = ((expreToStr (i+2)) e1):[]
expreToStr i (ExpreArr s e1 _ _ _) = init.unlines $ ((replicate i ' ')++"Var: "++s):k
  where k = map (expreToStr (i+2)) e1
expreToStr i (ExpreLLamada s e1 _ _) = init.unlines $ ((replicate i ' ')++"Llamada: "++s):k
  where k = map (expreToStr (i+2)) e1
expreToStr i (ExpreBasicInt i1 _ _) = init.unlines $ ((replicate i ' ')++"Int: "++(show i1)):[]
expreToStr i (ExpreBasicFloat i1 _ _) = init.unlines $ ((replicate i ' ')++"Float: "++(show i1)):[]
expreToStr i (ExpreBasicBool i1 _ _) = init.unlines $ ((replicate i ' ')++"Bool: "++(show i1)):[]
expreToStr i (ExpreBasicStr s _ _) = init.unlines $ ((replicate i ' ')++"String: "++s):[]
expreToStr i (ExpreNull) = init.unlines $ ((replicate i ' ')++"ExpreNull"):[]

data Instruc = InstrucBlock [Instruc]
  | InstrucFor Instruc Expre Instruc Instruc Int Int
  | InstrucWhile Expre Instruc Int Int
  | InstrucIf Expre Instruc Int Int
  | InstrucIfElse Expre Instruc Instruc Int Int
  | InstrucAsing Expre Expre Int Int
  | InstrucExpre Expre Int Int
  | InstrucBreak Int Int
  | InstrucCon Int Int
  | InstrucRet Int Int
  | InstrucRetW Expre Int Int
  | InstrucFun String Instruc
  | InstrucRead TypeData String
  | InstrucWrite TypeData String
  | InstrucNull

instance Show Instruc where
  show (InstrucBlock _) = "InstrucBlock"
  show (InstrucFor _ _ _ _ _ _) = "InstrucFor"
  show (InstrucWhile _ _ _ _) = "InstrucWhile"
  show (InstrucIf _ _ _ _) = "InstrucIf"
  show (InstrucIfElse _ _ _ _ _) = "InstrucIfElse"
  show (InstrucAsing _ _ _ _) = "InstrucAsing"
  show (InstrucExpre _ _ _) = "InstrucExpre"
  show (InstrucBreak _ _) = "InstrucBreak"
  show (InstrucCon _ _) = "InstrucCon"
  show (InstrucRet _ _) = "InstrucRet"
  show (InstrucRetW _ _ _) = "InstrucRetW"
  show (InstrucFun s _) = "InstrucFun " ++ s
  show (InstrucRead t _) = "InstrucRead " ++ show t
  show (InstrucWrite t _) = "InstrucWrite " ++ show t
  show InstrucNull = "InstrucNull"

insToStr i (InstrucBlock l) = init.unlines $ ((replicate i ' ')++"Bloque"):k
  where k = map (insToStr (i+2)) l
insToStr i (InstrucFor i1 e i2 i3 _ _) = init.unlines $ ((replicate i ' ')++"For"):k
  where k = ((insToStr (i+2)) i1):((expreToStr (i+2)) e):((insToStr (i+2)) i2):((insToStr (i+2)) i3):[]
insToStr i (InstrucWhile e i1 _ _) = init.unlines $ ((replicate i ' ')++"While"):k
  where k = ((expreToStr (i+2)) e):((insToStr (i+2)) i1):[]
insToStr i (InstrucIf e i1 _ _) = init.unlines $ ((replicate i ' ')++"If"):k
  where k = ((expreToStr (i+2)) e):((insToStr (i+2)) i1):[]
insToStr i (InstrucIfElse e i1 i2 _ _) = init.unlines $ ((replicate i ' ')++"If"):k
  where k = ((expreToStr (i+2)) e):((insToStr (i+2)) i1):((insToStr (i+2)) i2):[]
insToStr i (InstrucAsing e1 e2 _ _) = init.unlines $ ((replicate i ' ')++"Asignacion"):k
  where k = ((expreToStr (i+2)) e1):((expreToStr (i+2)) e2):[]
insToStr i (InstrucExpre e _ _) = init.unlines $ ((replicate i ' ')++"Expre"):k
  where k = ((expreToStr (i+2)) e):[]
insToStr i (InstrucBreak  _ _) = init.unlines $ ((replicate i ' ')++"Break"):[]
insToStr i (InstrucCon  _ _) = init.unlines $ ((replicate i ' ')++"Continue"):[]
insToStr i (InstrucRet  _ _) = init.unlines $ ((replicate i ' ')++"Return"):[]
insToStr i (InstrucRetW e _ _) = init.unlines $ ((replicate i ' ')++"Return"):k
  where k = ((expreToStr (i+2)) e):[]
insToStr i (InstrucFun s i1) = init.unlines $ ((replicate i ' ')++"Funcion --"++s++"--"):k
  where k = ((insToStr (i+2)) i1):[]
insToStr i (InstrucRead t _) = (replicate i ' ')++"Read --"++show t++"--\n"
insToStr i (InstrucWrite t _) = (replicate i ' ')++"Write --"++show t++"--\n"
insToStr _ InstrucNull = ""
